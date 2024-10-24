package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * @ClassName OpLocationEvseElastic
 * @Author A22121
 * @Description
 * @Date 2022/4/19 15:04
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
public interface OpLocationEvseElastic extends ElasticsearchRepository<OpLocationEvseElasticDTO, Long> {
    List<OpLocationEvseElasticDTO> findAllByLocationIdIn(Set<Long> stationIds);

    List<OpLocationEvseElasticDTO> findAllByLocationId(Long stationId);

    List<OpLocationEvseElasticDTO> findAllByLocationIdAndPileSn(Long stationId, String pileSn);

    List<OpLocationEvseElasticDTO> findAllByPileSnIn(Set<String> pileSnSet);

    List<OpLocationEvseElasticDTO> findAllByPileSn(String pileSn);

    List<OpLocationEvseElasticDTO> findAllByIdIn(List<Long> idList);

    List<OpLocationEvseElasticDTO> findAllByOperatorIdAndLocationIdIn(Long operatorId, List<String> pileSnList);

    List<OpLocationEvseElasticDTO> findAllByEvseSnIn(List<String> evseSnList);

    OpLocationEvseElasticDTO findByEvseSn(String evseSn);

    /**
     * 查询拥有指定计费规则的枪
     */
    OpLocationEvseElasticDTO findByTariffIdAndId(Long tariffId, Long id);

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的充电枪数据
     */
    List<OpLocationEvseElasticDTO> findAllByOperatorId(Long sellerId);

    List<OpLocationEvseElasticDTO> findAllByRuleId(Long ruleId);

    List<OpLocationEvseElasticDTO> findByLocationIdAndIdNotIn(Long locationId, Collection<Long> ids);

    OpLocationEvseElasticDTO findByOperatorIdAndEvseSn(Long operatorId, String evseSn);

    List<OpLocationEvseElasticDTO> findByOperatorIdAndLocationIdIn(Long operatorId, List<Long> locationIds);

    List<OpLocationEvseElasticDTO> findAllByIdInOrderByEvseSn(List<Long> idList);
}
