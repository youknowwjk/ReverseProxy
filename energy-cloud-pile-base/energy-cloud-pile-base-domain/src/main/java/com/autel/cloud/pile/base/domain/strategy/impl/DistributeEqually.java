package com.autel.cloud.pile.base.domain.strategy.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;
import com.autel.cloud.pile.base.domain.strategy.AbstractDistributeStrategy;
import com.autel.cloud.pile.base.domain.strategy.DistributeStrategy;
import com.autel.cloud.pile.base.vo.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * 桩数平均策略
 *
 * @Author temp
 * @Date 2022/12/14 15:33
 */
@Component(value = BaseConstant.DISTRIBUTE_EQUALLY)
@Slf4j
public class DistributeEqually extends AbstractDistributeStrategy implements DistributeStrategy {

    private List<OpLocationPileGroupDeliveryVO> doExecute(OncePerRequestContext context) {
        DeliveryGroupTreeVO deliveryTreeVo = context.getDeliveryTreeVo();
        if (null==deliveryTreeVo){
            return new ArrayList<>();
        }
        List<OpLocationPileGroupDeliveryVO> resultVoList = new ArrayList<>();
        //过滤离线桩
        this.handleOffline(resultVoList, context,deliveryTreeVo);
        //过滤VIP
        AtomicInteger online = new AtomicInteger(deliveryTreeVo.getEvseNumber());
        BigDecimal usable = this.handleVip(resultVoList, deliveryTreeVo, deliveryTreeVo.getUsableChargingUp(), online);
        if (deliveryTreeVo == null || BigDecimal.ZERO.compareTo(deliveryTreeVo.getTotal()) == 0) {
            log.info("DistributeEqually,doExecute,total=0");
            return resultVoList;
        }
        setOpLocationPileGroupDeliveryVO(resultVoList, deliveryTreeVo, usable.min(deliveryTreeVo.getUsableTotal()));
        return resultVoList;
    }
    /**
     * 这里getPileGroupMixList是一种加权平均  权值是桩数
     * 1. 初始化和预处理
     * 计算总量：首先，该方法会计算整个树结构的资源总量 total。如果 total 为零，直接返回，因为没有可分配的资源。
     * 获取桩群组列表：通过 getPileGroupMixList 方法获取混合列表 pileGroupMixList，这个列表包括了所有需要进行资源分配的群组和单个桩，并按权重排序（从小到大）。
     * 2. 循环分配
     * 遍历混合列表：方法遍历 pileGroupMixList 中的每个 PileGroupDeliveryMixVO 对象，这个对象包含了具体的充电桩信息或子群组信息。
     * 判断资源需求：
     * 如果当前对象的资源需求 (mixVO.getValue()) 小于可用资源 (usable)，则完全满足该对象的需求，并将该对象从可用资源中扣除。
     * 如果资源需求大于或等于可用资源，则对当前对象进行部分或全部资源分配。
     * 3. 具体分配逻辑
     * 对单个桩的分配：如果 mixVO 是一个单个桩（isPile() 返回 true），则方法会遍历这个桩的所有 evseInfoList（每个充电桩可能有多个连接点），并根据计算出的分配值（通过 equallyCalc 方法）分配资源。
     * 对子群组的分配：如果 mixVO 是一个子群组（isPile() 返回 false），则递归调用 setOpLocationPileGroupDeliveryVO 方法，将资源分配给子群组。
     * 4. 资源分配计算
     * equallyCalc 方法：这个方法通过传入的群组桩数、可用资源和子群组桩数来计算平均分配的资源量。它的作用是确保资源在每个桩之间进行公平分配。
     * 5. 递归分配
     * 当资源需求超过当前可用资源时，方法会尝试分配部分资源并将剩余的资源递归地分配给其他桩或子群组。这种递归方式确保了资源在整个树结构中的公平分配。
     * 6. 记录结果
     * 每次分配后，方法会构建 OpLocationPileGroupDeliveryVO 对象，并将其添加到 resultVoList 列表中，最终这个列表将包含所有的分配结果。
     * 总结：
     * setOpLocationPileGroupDeliveryVO 方法实现了在充电桩或群组之间基于权重的资源分配。它的核心逻辑是按需分配资源，
     * 优先满足资源需求较小的桩或群组，然后将剩余资源按比例分配给其他桩或群组。方法还考虑了递归分配，以确保即使在层次结构复杂的群组中，也能公平地分配资源。
    */

    private void setOpLocationPileGroupDeliveryVO(List<OpLocationPileGroupDeliveryVO> resultVoList, DeliveryGroupTreeVO treeVO, BigDecimal usable) {
        BigDecimal total = treeVO.getTotal();
        if (BigDecimal.ZERO.compareTo(total) == 0) {
            log.info("DistributeEqually,setOpLocationPileGroupDeliveryVO,total={}", total);
            return;
        }
        Integer pileNumber = treeVO.getPileNumber();
        List<PileGroupDeliveryMixVO> pileGroupMixListTmp = this.getPileGroupMixList(treeVO);
        List<PileGroupDeliveryMixVO> pileGroupMixList = pileGroupMixListTmp.stream().sorted((f, s) -> (int) f.getValue().subtract(s.getValue()).intValue()).collect(Collectors.toList());
        if (!pileGroupMixList.isEmpty()) {
            for (PileGroupDeliveryMixVO mixVO : pileGroupMixList) {
                if (mixVO.getValue().compareTo(usable) < 0) {
                    if (mixVO.isPile()) {
                        DeliveryPileInfoTreeVO pileInfoVO = mixVO.getPileInfoVO();
                        BigDecimal usablePileLimit = pileInfoVO.getUsablePileLimit().min(usable);
                        usable = usable.subtract(usablePileLimit);
                        pileNumber -= 1;
                        List<DeliveryEvseInfoTreeVO> evseVoList = pileInfoVO.getEvseInfoList();
                        BigDecimal value = usablePileLimit.divide(BigDecimal.valueOf(pileInfoVO.getEvseInfoList().size()), 1, BigDecimal.ROUND_DOWN);
                        for (DeliveryEvseInfoTreeVO evseInfoVO : evseVoList) {
                            resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, value));
                        }
                    } else {
                        DeliveryGroupTreeVO treeVO1 = mixVO.getTreeVO();
                        BigDecimal total1 = treeVO1.getUsableTotal().min(usable);
                        pileNumber -= treeVO1.getPileNumber();
                        usable = usable.subtract(total1);
                        this.setOpLocationPileGroupDeliveryVO(resultVoList, treeVO1, total1);
                    }
                } else {
                    if (mixVO.isPile()) {
                        DeliveryPileInfoTreeVO pileInfoVO = mixVO.getPileInfoVO();
                        List<DeliveryEvseInfoTreeVO> evseVoList = pileInfoVO.getEvseInfoList();
                        BigDecimal value = equallyCalc(BigDecimal.valueOf(pileNumber), usable, BigDecimal.ONE).divide(BigDecimal.valueOf(pileInfoVO.getEvseInfoList().size()), 1, BigDecimal.ROUND_DOWN);
                        for (DeliveryEvseInfoTreeVO evseInfoVO : evseVoList) {
                            resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, value));
                        }
                    } else {
                        DeliveryGroupTreeVO treeVO1 = mixVO.getTreeVO();
                        BigDecimal total1 = equallyCalc(BigDecimal.valueOf(pileNumber), usable, BigDecimal.valueOf(treeVO1.getPileNumber()));
                        this.setOpLocationPileGroupDeliveryVO(resultVoList, treeVO1, total1.min(treeVO1.getUsableTotal()));
                    }
                }
            }
        }
    }

    private List<PileGroupDeliveryMixVO> getPileGroupMixList(DeliveryGroupTreeVO treeVO) {
        List<DeliveryPileInfoTreeVO> pileVoList = treeVO.getPileInfoList();
        List<DeliveryGroupTreeVO> childrenList = treeVO.getChildrenList();
        List<PileGroupDeliveryMixVO> resultList = new ArrayList<>();
        Integer pileNumber = treeVO.getPileNumber();
        if (!CollectionUtils.isEmpty(pileVoList)) {
            for (DeliveryPileInfoTreeVO pileInfoVO : pileVoList) {
                BigDecimal usablePileLimit = pileInfoVO.getUsablePileLimit();
                PileGroupDeliveryMixVO mixVO = new PileGroupDeliveryMixVO();
                mixVO.setPile(true);
                mixVO.setPileInfoVO(pileInfoVO);
                mixVO.setValue(usablePileLimit.multiply(BigDecimal.valueOf(pileNumber)));
                resultList.add(mixVO);
            }
        }
        if (!CollectionUtils.isEmpty(childrenList)) {
            for (DeliveryGroupTreeVO treeVo : childrenList) {
                if (treeVo.getPileInfoList().isEmpty() && treeVo.getChildrenList().isEmpty()){
                    continue;
                }
                BigDecimal total = treeVo.getUsableTotal();
                PileGroupDeliveryMixVO mixVO = new PileGroupDeliveryMixVO();
                mixVO.setPile(false);
                mixVO.setTreeVO(treeVo);
                mixVO.setValue(total.multiply(BigDecimal.valueOf(pileNumber)).divide(BigDecimal.valueOf(treeVo.getPileNumber()).setScale(1, BigDecimal.ROUND_DOWN)));
                resultList.add(mixVO);
            }
        }
        return resultList;
    }

    /**
     * @param total      群组桩在线个数
     * @param chargingUp 群组可分配上限
     * @param number     桩数
     * @return
     */
    private BigDecimal equallyCalc(BigDecimal total, BigDecimal chargingUp, BigDecimal number) {
        return number.multiply(chargingUp).divide(total, 1, BigDecimal.ROUND_DOWN);
    }

    @Override
    public void execute(OncePerRequestContext context) {
        if(context==null){
            return;
        }
        //成本最优走这里
        this.handleUseStrategy(context);
        Integer energyUseStrategy = context.getRootDto().getEnergyUseStrategy();
        if (energyUseStrategy != null && energyUseStrategy == 1) {
            return;
        }
        if (!context.getDeliveryFlag()){
            return;
        }
        //最低分配
        List<OpLocationPileGroupDeliveryVO> deliveryVOList = this.getMinDelivery(context);
        if (CollectionUtils.isEmpty(deliveryVOList)) {
            //分配策略计算
            deliveryVOList = this.doExecute(context);
        }
        log.info("DistributeEqually,计算结果,execute deliveryVOList={}", JSON.toJSONString(deliveryVOList));
        context.setDeliveryList(deliveryVOList);
        this.handleDelivery(context);
    }

}
