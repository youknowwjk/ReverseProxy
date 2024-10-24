package com.autel.cloud.pile.base.domain.strategy.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;
import com.autel.cloud.pile.base.domain.strategy.AbstractDistributeStrategy;
import com.autel.cloud.pile.base.domain.strategy.DistributeStrategy;
import com.autel.cloud.pile.base.vo.DeliveryEvseInfoTreeVO;
import com.autel.cloud.pile.base.vo.DeliveryGroupTreeVO;
import com.autel.cloud.pile.base.vo.DeliveryPileInfoTreeVO;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 加权平均策略
 *
 * @Author temp
 * @Date 2022/12/14 15:33
 */
@Component(value = BaseConstant.DISTRIBUTE_EVENLY)
@Slf4j
public class DistributeEvenly extends AbstractDistributeStrategy implements DistributeStrategy {

    private List<OpLocationPileGroupDeliveryVO> doExecute(OncePerRequestContext context) {
        List<OpLocationPileGroupDeliveryVO> resultVoList = new ArrayList<>();
        //过滤离线桩
        this.handleOffline(resultVoList, context,context.getDeliveryTreeVo());
        DeliveryGroupTreeVO deliveryTreeVo = context.getDeliveryTreeVo();
        //过滤VIP
        AtomicInteger online = new AtomicInteger(deliveryTreeVo.getEvseNumber());
        BigDecimal usable = this.handleVip(resultVoList, deliveryTreeVo, deliveryTreeVo.getUsableChargingUp(), online);
        if (deliveryTreeVo == null || BigDecimal.ZERO.compareTo(deliveryTreeVo.getTotal()) == 0) {
            log.info("DistributeEvenly,doExecute,total=0");
            return resultVoList;
        }
        setOpLocationPileGroupDeliveryVO(resultVoList, deliveryTreeVo, usable.min(deliveryTreeVo.getUsableTotal()));
        return resultVoList;
    }

    private void setOpLocationPileGroupDeliveryVO(List<OpLocationPileGroupDeliveryVO> resultVoList, DeliveryGroupTreeVO treeVO, BigDecimal usable) {
        if (BigDecimal.ZERO.compareTo(usable) == 0) {
            log.info("DistributeEvenly,setOpLocationPileGroupDeliveryVO,usable={}", usable);
            return;
        }
        List<DeliveryGroupTreeVO> childrenList = treeVO.getChildrenList();
        List<DeliveryPileInfoTreeVO> pileVoList = treeVO.getPileInfoList();
        BigDecimal parent = treeVO.getTotal();
        //计算子群组
        if (!CollectionUtils.isEmpty(childrenList)) {
            childrenList.stream().forEach(vo -> {
                BigDecimal child = vo.getUsableTotal();
                BigDecimal tmp = weightCalc(parent, usable, child);
                this.setOpLocationPileGroupDeliveryVO(resultVoList, vo, tmp);
            });
        }
        //先计算当前群组各个桩分配电流
        if (!CollectionUtils.isEmpty(pileVoList)) {
            for (DeliveryPileInfoTreeVO pileInfoVO : pileVoList) {
                BigDecimal usablePileLimit = pileInfoVO.getUsablePileLimit();
                BigDecimal value = weightCalc(parent, usable, usablePileLimit).divide(BigDecimal.valueOf(pileInfoVO.getEvseInfoList().size()), 1, BigDecimal.ROUND_DOWN);
                List<DeliveryEvseInfoTreeVO> evseVoList = pileInfoVO.getEvseInfoList();
                for (DeliveryEvseInfoTreeVO evseInfoVO : evseVoList) {
                    resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, value));
                }
            }
        }
    }

    /**
     * @param total      群组总电流
     * @param chargingUp 群组可分配上限
     * @param current    当前电流
     * @return
     */
    private BigDecimal weightCalc(BigDecimal total, BigDecimal chargingUp, BigDecimal current) {
        return current.multiply(chargingUp).divide(total, 1, BigDecimal.ROUND_DOWN);
    }

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
            deliveryVOList = this.doExecute(context);
        }
        log.info("DistributeEvenly,计算结果,execute deliveryVOList={}", JSON.toJSONString(deliveryVOList));
        context.setDeliveryList(deliveryVOList);
        this.handleDelivery(context);
    }
}
