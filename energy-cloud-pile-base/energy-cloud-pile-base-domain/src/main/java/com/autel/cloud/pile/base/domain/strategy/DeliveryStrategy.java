package com.autel.cloud.pile.base.domain.strategy;

import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;

import java.util.List;

/**
 * 配置下发策略
 *
 * @Author temp
 * @Date 2023/2/7 10:22
 */
public interface DeliveryStrategy {
    void dealSafeStragety(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList);
    void delivery(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList);
    void updateDeliverCache(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList);
}
