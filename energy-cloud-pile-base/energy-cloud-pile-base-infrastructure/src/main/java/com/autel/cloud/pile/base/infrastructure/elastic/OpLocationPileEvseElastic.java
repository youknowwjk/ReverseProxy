package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
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
public interface OpLocationPileEvseElastic extends ElasticsearchRepository<OpLocationPileEvseElasticDTO, Long> {
    List<OpLocationPileEvseElasticDTO> findByPileSnLike(String pileSn);

    List<OpLocationPileEvseElasticDTO> findByPileSnNotAndLocationId(String pileSn,Long locationId);

    OpLocationPileEvseElasticDTO findByPileSn(String pileSn);

    List<OpLocationPileEvseElasticDTO> findByPileSnIn(List<String> pileSnList);

    List<OpLocationPileEvseElasticDTO> findByOperatorId(Long operatorId);

    List<OpLocationPileEvseElasticDTO> findByOperatorIdIn(List<Long> operatorIdList);

    List<OpLocationPileEvseElasticDTO> findByPileSnInAndLocationId(List<String> pileSnList, Long locationId);

    List<OpLocationPileEvseElasticDTO> findByPileSnInAndLocationIdNot(List<String> pileSnList, Long locationId);

    List<OpLocationPileEvseElasticDTO> findAllByLocationId(Long stationId);

    List<OpLocationPileEvseElasticDTO> findAllByLocationIdAndPileSn(Long stationId, String pileSn);

    List<OpLocationPileEvseElasticDTO> findAllByLocationIdIn(Set<Long> stationId);

    List<OpLocationPileEvseElasticDTO> findAllByLocationIdInOrderByCreatedAtAsc(Set<Long> stationId);

    Page<OpLocationPileEvseElasticDTO> findByLocationIdOrderByCreatedAtDesc(Long stationId, Pageable pageable);

    Page<OpLocationPileEvseElasticDTO> findByLocationIdAndPileSnLike(Long stationId, String pileSn, Pageable pageable);

    List<OpLocationPileEvseElasticDTO> findAllByRuleId(Long ruleId);

    List<OpLocationPileEvseElasticDTO> findAllByEroamingEnable(Integer eroamingEnable);

    List<OpLocationPileEvseElasticDTO> findByLocationIdAndIdNotIn(Long locationId, Collection<Long> ids);
}
