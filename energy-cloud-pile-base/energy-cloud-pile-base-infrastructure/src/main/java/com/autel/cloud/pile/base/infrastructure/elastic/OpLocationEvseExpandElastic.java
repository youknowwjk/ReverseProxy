package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

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
public interface OpLocationEvseExpandElastic extends ElasticsearchRepository<OpLocationEvseExpandElasticDTO, Long> {

    void deleteByIdIn(List<Long> ids);

    void deleteAllByPlatformIs(Integer platform);

    List<OpLocationEvseExpandElasticDTO> findAllByLocationIdIn(Set<Long> collect);


    List<OpLocationEvseExpandElasticDTO> findAllByLocationId(Long locationId);

    List<OpLocationEvseExpandElasticDTO> findAllByIdIn(List<Long> collect);
}
