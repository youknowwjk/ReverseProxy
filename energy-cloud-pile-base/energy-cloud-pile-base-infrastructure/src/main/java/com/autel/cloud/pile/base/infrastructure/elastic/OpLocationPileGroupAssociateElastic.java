package com.autel.cloud.pile.base.infrastructure.elastic;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileGroupAssociateElasticDTO;
import org.springframework.data.elasticsearch.repository.ElasticsearchRepository;
import org.springframework.stereotype.Repository;

/**
 * @ClassName OpLocationGroupElastic
 * @Author A22121
 * @Description
 * @Date 2022/7/15 14:46
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
public interface OpLocationPileGroupAssociateElastic extends ElasticsearchRepository<OpLocationPileGroupAssociateElasticDTO, Long> {
    OpLocationPileGroupAssociateElasticDTO findByPileId(Long pileId);
}
