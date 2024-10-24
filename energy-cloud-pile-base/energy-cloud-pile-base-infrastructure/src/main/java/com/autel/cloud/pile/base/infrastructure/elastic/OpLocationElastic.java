package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * @ClassName OpLocationEs
 * @Author A22121
 * @Description
 * @Date 2022/4/15 10:04
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
public interface OpLocationElastic extends ElasticsearchRepository<OpLocationElasticDTO, Long> {

    List<OpLocationElasticDTO> findAllByOperatorId(Long operatorId);

    List<OpLocationElasticDTO> findAllByPlatform(Integer platForm);

    List<OpLocationElasticDTO> findAllByOperatorIdAndPlatform(Long operatorId,Integer platForm);

    List<OpLocationElasticDTO> findAllByOperatorIdInAndPlatform(List<Long> operatorId,Integer platForm);

    Page<OpLocationElasticDTO> findAllByOperatorId(Long operatorId, Pageable pageable);

    Page<OpLocationElasticDTO> findByIdInOrNameLike(Set<Long> ids, String name, Pageable pageable);

    Page<OpLocationElasticDTO> findByOperatorIdAndIdInOrNameLike(Long operatorId, Set<Long> ids, String name, Pageable pageable);

    Page<OpLocationElasticDTO> findByNameLike(String name, Pageable pageable);

    Page<OpLocationElasticDTO> findByOperatorIdAndNameLike(Long operatorId, String name, Pageable pageable);

    List<OpLocationElasticDTO> findByGroupIdIn(List<Long> groupgIds);

    List<OpLocationElasticDTO> findAllByIdIn(Set<Long> locationIds);

    void deleteAllByIdIn(Collection<Long> locationIds);

    List<OpLocationElasticDTO> findAllByOperatorIdIn(List<Long> operatorIdList);

    void deleteAllByPlatformIs(Integer platform);
}
