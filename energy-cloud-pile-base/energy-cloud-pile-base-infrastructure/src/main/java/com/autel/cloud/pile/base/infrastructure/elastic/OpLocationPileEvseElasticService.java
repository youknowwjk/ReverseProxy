package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.dto.pile.QueryPilePageDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.vo.PileBaseVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2022/11/22 16:04
 */
@Component
@Slf4j
public class OpLocationPileEvseElasticService {
    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    public OpLocationPileEvseElasticDTO findOne(String pileSn) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("pileSn", pileSn));
        NativeSearchQuery query = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(0, 1))
                .build();
        log.info("findOne,query={}", query.getQuery().toString());
        SearchHits<OpLocationPileEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(query, OpLocationPileEvseElasticDTO.class);
        if(searchHits.getTotalHits() == 0) {
            return null;
        }
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOS = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(opLocationPileEvseElasticDTOS)) {
            return opLocationPileEvseElasticDTOS.get(0);
        }
        return null;
    }

    public Page<PileBaseVO> findByPage(QueryPilePageDTO paramDTO, List<Long> locationIdList) {
        Page<PileBaseVO> page=new Page<>(paramDTO.getPage(),paramDTO.getPageSize());

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", paramDTO.getOperatorId()));
        if (StringUtils.isNotBlank(paramDTO.getLocationId())) {
            queryBuilder.must(QueryBuilders.termQuery("locationId", Long.valueOf(paramDTO.getLocationId())));
        } else {
            queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIdList));
        }
        if(StringUtils.isNotBlank(paramDTO.getSearchValue())){
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", QueryParserBase.escape(paramDTO.getSearchValue()))));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", QueryParserBase.escape(paramDTO.getSearchValue()))));
            queryBuilder.must(queryBuilder2);
        }
        NativeSearchQuery query = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("id").order(SortOrder.DESC))
                .withPageable(PageRequest.of(paramDTO.getPage()-1, paramDTO.getPageSize()))
                .build();
        log.info("findByPage,query={}", query.getQuery().toString());
        SearchHits<OpLocationPileEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(query, OpLocationPileEvseElasticDTO.class);
        if(searchHits.getTotalHits() == 0) {
            page.setTotal(0);
            return page;
        }
        List<PileBaseVO> records = searchHits.stream().map(e -> {
            OpLocationPileEvseElasticDTO content = e.getContent();
            PileBaseVO d = new PileBaseVO();
            d.setId(content.getId());
            d.setLocationId(content.getLocationId());
            d.setName(content.getName());
            d.setSn(content.getPileSn());
            d.setLocationName(content.getLocationName());
            d.setLocationId(content.getLocationId());
            d.setPowerType(StringUtils.startsWith(content.getPowerType(), "DC") ? "DC" : "AC");
            d.setPhases(content.getPowerType());
            return d;
        }).collect(Collectors.toList());
        page.setRecords(records);
        page.setTotal(searchHits.getTotalHits());
        return page;
    }
}
