package com.autel.cloud.pile.base.controller;

import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.IndexBoost;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@RestController
@Slf4j
@Validated
public class ElasticsearchTransferController {


    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Resource
    private OpLocationElastic opLocationElastic;

    @RequestMapping("transferLocation")
    public void transfer(@RequestParam("source") String source, @RequestParam("locationIdOffset") int locationIdOffset, @RequestParam("sellerIdOffset") int sellerIdOffset) {

        NativeSearchQueryBuilder nativeSearchQueryBuilder = new NativeSearchQueryBuilder().withIndicesBoost(new IndexBoost(source, 1.0f));
        NativeSearchQuery nativeSearchQuery = nativeSearchQueryBuilder.withQuery(QueryBuilders.matchAllQuery()).build();
        log.info("statReport {}", nativeSearchQuery.getQuery().toString());
        List<OpLocationElasticDTO> opLocationElasticDTOS =
//                elasticsearchRestTemplate.queryForList(nativeSearchQuery, OpLocationElasticDTO.class);
        elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());

        for (OpLocationElasticDTO op : opLocationElasticDTOS) {
            try {
                if (Objects.nonNull(op.getId()) && op.getId() > 0 && op.getId() < locationIdOffset) {
                    op.setId(op.getId() + locationIdOffset);
                }
                if (Objects.nonNull(op.getOperatorId()) && op.getOperatorId() > 0 && op.getOperatorId() < sellerIdOffset) {
                    op.setOperatorId(op.getOperatorId() + sellerIdOffset);
                }

                if (Objects.nonNull(op.getSubOperatorId()) && op.getSubOperatorId() > 0 && op.getSubOperatorId() < sellerIdOffset) {
                    op.setSubOperatorId(op.getSubOperatorId() + sellerIdOffset);
                }
                opLocationElastic.save(op);
            } catch (Exception e) {
                log.error(op.getName(), e);
            }
        }

    }

}
