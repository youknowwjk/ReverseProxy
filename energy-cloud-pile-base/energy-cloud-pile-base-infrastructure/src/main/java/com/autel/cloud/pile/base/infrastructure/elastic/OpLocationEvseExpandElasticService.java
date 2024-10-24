package com.autel.cloud.pile.base.infrastructure.elastic;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.action.bulk.BulkRequest;
import org.elasticsearch.action.delete.DeleteRequest;
import org.elasticsearch.action.delete.DeleteResponse;
import org.elasticsearch.action.index.IndexRequest;
import org.elasticsearch.action.index.IndexResponse;
import org.elasticsearch.action.update.UpdateRequest;
import org.elasticsearch.action.update.UpdateResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.common.xcontent.XContentType;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.reindex.BulkByScrollResponse;
import org.elasticsearch.index.reindex.DeleteByQueryRequest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.SearchHitsIterator;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2022/11/11 8:51
 */
@Component
@Slf4j
public class OpLocationEvseExpandElasticService {

    @Resource
    RestHighLevelClient restHighLevelClient;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private OpLocationEvseExpandElastic opLocationEvseExpandElastic;

    public void deleteAll() {
        DeleteByQueryRequest request = new DeleteByQueryRequest(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX);
        request.setQuery(QueryBuilders.matchAllQuery());
        request.types(BaseConstant.ES_TYPE);
        try {
            BulkByScrollResponse response = this.getClient().deleteByQuery(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("deleteAll,exception={}", e.getMessage());
        }
    }

    public void saveAll(List<OpLocationEvseExpandElasticDTO> list) {
        if (CollectionUtils.isEmpty(list)) {
            return;
        }
        BulkRequest bulkRequest = new BulkRequest();
        list.stream().forEach(dto -> {
            IndexRequest request = new IndexRequest(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX);
            request.id(dto.getId().toString());
            request.source(JSON.toJSONString(dto, SerializerFeature.WriteMapNullValue), XContentType.JSON);
            request.type(BaseConstant.ES_TYPE);
            bulkRequest.add(request);
        });
        try {
            this.getClient().bulk(bulkRequest, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("saveAll,exception={}", e.getMessage());
        }
    }

    public void save(OpLocationEvseExpandElasticDTO dto) {
        // 定义请求对象
        IndexRequest request = new IndexRequest(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX);
        // 设置文档id
        request.id(dto.getId().toString());
        // 将json格式字符串放在请求中
        request.source(JSON.toJSONString(dto, SerializerFeature.WriteMapNullValue), XContentType.JSON);
        request.type(BaseConstant.ES_TYPE);
        // 3、发送请求到ES
        try {
            IndexResponse response = this.getClient().index(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("save,exception={}", e.getMessage());
        }
    }

    /**
     * 更新索引数据
     */
    public void update(OpLocationEvseExpandElasticDTO dto) {
        UpdateRequest request = new UpdateRequest();
        request.index(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX).id(dto.getId().toString());
        // 拓展：局部更新也可以这样写：request.doc(XContentType.JSON, "name", "李四", "age", 25);，其中"name"和"age"是User对象中的字段名称，而"小美"和20是对应的字段值
        request.doc(JSON.toJSONString(dto, SerializerFeature.WriteMapNullValue), XContentType.JSON);
        request.type(BaseConstant.ES_TYPE);
        // 3、发送请求到ES
        try {
            UpdateResponse response = this.getClient().update(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("update,exception={}", e.getMessage());
        }
    }

    /**
     * 删除索引数据
     *
     * @param id
     * @throws Exception
     */
    public void delete(Long id) {
        // 2、定义请求对象
        DeleteRequest request = new DeleteRequest(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX);
        request.id(id.toString());
        request.type(BaseConstant.ES_TYPE);
        // 3、发送请求到ES
        try {
            DeleteResponse response = this.getClient().delete(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("delete,exception={}", e.getMessage());
        }
    }

    public void deleteByLocationIds(List<Long> locationIds) {
        if (CollectionUtils.isEmpty(locationIds)) {
            return;
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));
        DeleteByQueryRequest request = new DeleteByQueryRequest(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX);
        request.setQuery(queryBuilder);
        request.types(BaseConstant.ES_TYPE);
        try {
            BulkByScrollResponse response = this.getClient().deleteByQuery(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("deleteByLocationIds,exception={}", e.getMessage());
        }
    }

    public void deleteByIds(List<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return;
        }
        BulkRequest bulkRequest = new BulkRequest();
        ids.stream().forEach(id -> {
            DeleteRequest request = new DeleteRequest(BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX);
            request.id(id.toString());
            request.type(BaseConstant.ES_TYPE);
            bulkRequest.add(request);
        });
        try {
            this.getClient().bulk(bulkRequest, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("deleteByIds,exception={}", e.getMessage());
        }
    }

    public Boolean checkUpdate(Long id, String state, Long updatedAt) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("id", id));
        queryBuilder.must(QueryBuilders.rangeQuery("updatedAt").lt(updatedAt));

//        Iterable<OpLocationEvseExpandElasticDTO> search = opLocationEvseExpandElastic.search(queryBuilder);
//        Iterator<OpLocationEvseExpandElasticDTO> it = search.iterator();
        SearchHits<OpLocationEvseExpandElasticDTO> searchHits =
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseExpandElasticDTO.class);
        if(searchHits.getTotalHits() == 0) {
            return false;
        }
        Iterator<OpLocationEvseExpandElasticDTO> it = searchHits.stream().map(SearchHit::getContent).iterator();

        if (it.hasNext()) {
            OpLocationEvseExpandElasticDTO dto = it.next();
            dto.setGunState(state);
            dto.setUpdatedAt(updatedAt);
            opLocationEvseExpandElastic.save(dto);
        }
        return true;
    }

    private RestHighLevelClient getClient(){
        return restHighLevelClient;
    }

    /**
     * 批量更新
     *
     * @param collections
     * @return
     */
    public Boolean batchCheckUpdate(Collection<OpLocationEvseElasticDTO> collections) {
        if (CollectionUtils.isEmpty(collections)) {
            return Boolean.TRUE;
        }
        List<Long> ids = collections.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
        Iterable<OpLocationEvseExpandElasticDTO> iterable = opLocationEvseExpandElastic.findAllById(ids);
        Iterator<OpLocationEvseExpandElasticDTO> iterator = iterable.iterator();
        Map<Long, OpLocationEvseExpandElasticDTO> collect = new HashMap<>();
        while (iterator.hasNext()) {
            OpLocationEvseExpandElasticDTO next = iterator.next();
            collect.put(next.getId(), next);
        }
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : collections) {  // 要更新数据
            if (collect.containsKey(opLocationEvseElasticDTO.getId())) {
                OpLocationEvseExpandElasticDTO opLocationEvseExpandElasticDTO = collect.get(opLocationEvseElasticDTO.getId());// ES DB里面的数据
                if (Objects.isNull(opLocationEvseExpandElasticDTO.getUpdatedAt()) || opLocationEvseElasticDTO.getUpdatedAt().compareTo(opLocationEvseExpandElasticDTO.getUpdatedAt()) < 0) {
                    opLocationEvseExpandElasticDTO.setGunState(opLocationEvseElasticDTO.getState());
                    opLocationEvseExpandElasticDTO.setUpdatedAt(opLocationEvseElasticDTO.getUpdatedAt());
                }
            }
        }
        if (!CollectionUtils.isEmpty(collect)) {
            opLocationEvseExpandElastic.saveAll(iterable);
        }
        return true;
    }

    public boolean updateBatch(List<OpLocationEvseElasticDTO> evseDtoList) {
        if (CollectionUtils.isEmpty(evseDtoList)) {
            return true;
        }
        List<Long> ids = evseDtoList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
        Map<Long, OpLocationEvseElasticDTO> evseDtoMap = evseDtoList.stream().collect(Collectors.toMap(OpLocationEvseElasticDTO::getId, e -> e, (f, s) -> f));
        Iterable<OpLocationEvseExpandElasticDTO> iterable = opLocationEvseExpandElastic.findAllById(ids);
        Iterator<OpLocationEvseExpandElasticDTO> iterator = iterable.iterator();
        while (iterator.hasNext()) {
            OpLocationEvseExpandElasticDTO next = iterator.next();
            next.setGunType(evseDtoMap.get(next.getId()).getGunType());
        }
        opLocationEvseExpandElastic.saveAll(iterable);
        return true;
    }

    public List<OpLocationEvseExpandElasticDTO> findAll() {
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withPageable(PageRequest.of(0, 100))
                .build();
        SearchHitsIterator<OpLocationEvseExpandElasticDTO> evseDtoResult
                = elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseExpandElasticDTO.class);
        List<OpLocationEvseExpandElasticDTO> resultList = new ArrayList<>();
        evseDtoResult.forEachRemaining(one -> resultList.add(one.getContent()));
        return resultList;
    }
}
