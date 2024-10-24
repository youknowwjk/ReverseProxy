package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO2;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

/**
 * @ClassName OpLocationPileEvseElastic
 * @Author A22121
 * @Description
 * @Date 2022/5/17 20:37
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
public interface OpLocationPileEvseElastic2 extends ElasticsearchRepository<OpLocationPileEvseElasticDTO2, String> {

    List<OpLocationPileEvseElasticDTO2> findAllByLocationId(Long stationId);
    List<OpLocationPileEvseElasticDTO2> findAllByLocationIdIn(Set<Long> stationId);
    OpLocationPileEvseElasticDTO2 findByPileSn(String pileSn);

}
