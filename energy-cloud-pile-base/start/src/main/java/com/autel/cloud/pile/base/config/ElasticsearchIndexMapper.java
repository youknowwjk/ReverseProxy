package com.autel.cloud.pile.base.config;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.client.indices.GetMappingsRequest;
import org.elasticsearch.client.indices.GetMappingsResponse;
import org.elasticsearch.client.indices.PutMappingRequest;
import org.elasticsearch.common.xcontent.XContentType;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.event.EventListener;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.core.mapping.ElasticsearchPersistentEntity;
import org.springframework.data.elasticsearch.core.mapping.ElasticsearchPersistentProperty;
import org.springframework.data.mapping.PropertyHandler;
import org.springframework.data.mapping.context.MappingContext;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * ES索引校验更新
 *
 * @Author A22282
 * @Date 2024/6/4 9:16
 */
@Component
@Slf4j
public class ElasticsearchIndexMapper {
    @Resource
    private RestHighLevelClient restHighLevelClient;
    @Resource
    private MappingContext<? extends ElasticsearchPersistentEntity<?>, ElasticsearchPersistentProperty> mappingContext;

    private static final String BASE_PACKAGE = "com.autel.cloud.pile.base.infrastructure.elastic.entity";

    private final ObjectMapper objectMapper = new ObjectMapper();

    @EventListener(ApplicationReadyEvent.class)
    public void updateIndicesMapping(){
        // 扫描指定包下所有的类
        ClassPathScanningCandidateComponentProvider scanner =
                new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Document.class));

        Set<BeanDefinition> beanDefinitions = scanner.findCandidateComponents(BASE_PACKAGE);
        for (BeanDefinition beanDefinition : beanDefinitions) {
            try {
                Class<?> entityClass = Class.forName(beanDefinition.getBeanClassName());
                updateIndexMapping(entityClass);
            } catch (Exception e) {
                log.error("updateIndicesMapping,Exception: ", e);
            }
        }
    }

    private void updateIndexMapping(Class<?> entityClass) throws JsonProcessingException {
        // 获取实体类的索引名称
        ElasticsearchPersistentEntity<?> entity = mappingContext.getPersistentEntity(entityClass);
        if (entity == null || entity.getIndexCoordinates() == null) {
            log.error("Failed to retrieve mapping for entity: {}", entityClass.getSimpleName());
            return;
        }
        String indexName = entity.getIndexCoordinates().getIndexName();

        // 获取现有的索引映射
        Map<String, Object> currentMappings = getCurrentMappings(indexName);

        // 获取实体类的映射
        Map<String, Object> entityMappings = getEntityMappings(entity, currentMappings);

        // 更新索引的映射
        if (!entityMappings.isEmpty()) {
            String jsonMapping = "{ \"properties\": " + objectMapper.writeValueAsString(entityMappings) + " }";
            PutMappingRequest putMappingRequest = new PutMappingRequest(indexName);
            putMappingRequest.source(jsonMapping, XContentType.JSON);

            try {
                restHighLevelClient.indices().putMapping(putMappingRequest, RequestOptions.DEFAULT);
                log.error("Mappings updated successfully for index: {}", indexName);
            } catch (IOException e) {
                log.error("Failed to update mapping for index: ", e);
            }
        } else {
            log.error("No new fields to update for index: {}", indexName);
        }
    }

    private Map<String, Object> getCurrentMappings(String indexName) {
        GetMappingsRequest getMappingsRequest = new GetMappingsRequest().indices(indexName);
        try {
            GetMappingsResponse getMappingResponse = restHighLevelClient.indices().getMapping(getMappingsRequest, RequestOptions.DEFAULT);
            if (getMappingResponse.mappings().containsKey(indexName)) {
                Map<String, Object> mapping = getMappingResponse.mappings().get(indexName).sourceAsMap();
                Object properties = mapping.get("properties");
                if (properties instanceof Map) {
                    return (Map<String, Object>) properties;
                }
            }
        } catch (IOException e) {
            log.error("getCurrentMappings,IOException: ", e);
        }
        return new HashMap<>();
    }

    private Map<String, Object> getEntityMappings(ElasticsearchPersistentEntity<?> entity, Map<String, Object> currentMappings) {
        Map<String, Object> entityMappings = new HashMap<>();
        entity.doWithProperties((PropertyHandler<ElasticsearchPersistentProperty>) property -> {
            String fieldName = property.getFieldName();
            if (!currentMappings.containsKey(fieldName)) {
                Map<String, Object> fieldMapping = new HashMap<>();
                String fieldType = getFieldType(property);
                if (fieldType != null) {
                    fieldMapping.put("type", getFieldType(property));
                    entityMappings.put(fieldName, fieldMapping);
                }
            } else {
                // 检查现有字段类型与实体类中的字段类型是否一致
                Map<String, Object> currentFieldMapping = (Map<String, Object>) currentMappings.get(fieldName);
                if (CollectionUtils.isEmpty(currentFieldMapping)) {
                    return;
                }
                String currentFieldType = (String) currentFieldMapping.get("type");
                if (!StringUtils.hasText(currentFieldType)) {
                    return;
                }
                String fieldType = getFieldType(property);
                if (!currentFieldType.equals(fieldType)) {
                    log.error("Field type mismatch for field {} in index {}: current type is {}, expected type is {}. Skipping update for this field.", fieldName, entity.getIndexCoordinates().getIndexName(), currentFieldType, fieldType);
                }
            }
        });
        return entityMappings;
    }

    private String getFieldType(ElasticsearchPersistentProperty property) {
        Field field = property.findAnnotation(Field.class);
        if (field != null) {
            return field.type().name().toLowerCase();
        }
        return null;
    }
}
