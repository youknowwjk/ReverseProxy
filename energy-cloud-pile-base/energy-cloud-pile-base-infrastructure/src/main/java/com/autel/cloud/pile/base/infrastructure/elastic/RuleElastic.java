package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.RuleElasticDTO;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

/**
 * @Author temp
 * @Date 2022/7/19 21:53
 */
@Repository
public interface RuleElastic extends ElasticsearchRepository<RuleElasticDTO,Long> {
}
