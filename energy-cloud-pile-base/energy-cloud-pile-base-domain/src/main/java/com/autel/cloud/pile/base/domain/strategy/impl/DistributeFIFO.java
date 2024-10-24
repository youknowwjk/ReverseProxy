package com.autel.cloud.pile.base.domain.strategy.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;
import com.autel.cloud.pile.base.domain.strategy.AbstractDistributeStrategy;
import com.autel.cloud.pile.base.domain.strategy.DistributeStrategy;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * 先进先出策略
 *
 * @Author temp
 * @Date 2022/12/14 15:33
 */
@Component(value = BaseConstant.DISTRIBUTE_FIFO)
@Slf4j
public class DistributeFIFO extends AbstractDistributeStrategy implements DistributeStrategy {

    @Override
    public void execute(OncePerRequestContext context) {
        this.handleUseStrategy(context);
        Integer energyUseStrategy = context.getRootDto().getEnergyUseStrategy();
        if (energyUseStrategy != null && energyUseStrategy == 1) {
            return;
        }
        //最低分配
        List<OpLocationPileGroupDeliveryVO> deliveryVOList = this.getMinDelivery(context);
        if (CollectionUtils.isEmpty(deliveryVOList)) {
            //分配策略计算
            deliveryVOList = this.doFiFo(context, null);
        }
        log.info("DistributeFIFO,计算结果,execute deliveryVOList={}", JSON.toJSONString(deliveryVOList));
        context.setDeliveryList(deliveryVOList);
        this.handleDelivery(context);
    }
}
