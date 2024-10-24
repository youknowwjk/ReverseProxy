package com.autel.cloud.pile.base.domain.facade;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.enums.MerchantChargePointRelationEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargePointMerchantRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargePointMerchantTerminalMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantRelationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantTerminalEntity;
import com.autel.cloud.pile.base.vo.Connector;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.enums.PayStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

@Component
@Slf4j
public class ChargePointMerchantRelationFacade {

    @Resource
    private ChargePointMerchantRelationMapper chargePointMerchantRelationMapper;

    @Autowired
    private ChargePointMerchantTerminalMapper chargePointMerchantTerminalMapper;

    @Resource
    private TransactionTemplate transactionTemplate;

    public Map<String, Integer> getGunSubStatus(Long sellerId) {

        log.info("===>>> ChargePointMerchantRelationFacade.getGunSubStatus sellerId : {}",
                JSON.toJSONString(sellerId));

        if (sellerId == null) {
            return null;
        }

        Map<String, Integer> evseSnAndSubStatusMap = new HashMap<>();

        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntityList = chargePointMerchantRelationMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .eq(ChargePointMerchantRelationEntity::getMerchantId, sellerId)
                .ne(ChargePointMerchantRelationEntity::getOverchargingPileFlag, 1));

        if (ObjectUtils.isNotEmpty(chargePointMerchantRelationEntityList)) {
            for (ChargePointMerchantRelationEntity chargePointMerchantRelationEntity : chargePointMerchantRelationEntityList) {
                int subStatus = chargePointMerchantRelationEntity.getSubStatus();
                String sn = chargePointMerchantRelationEntity.getSn();
                List<Connector> connectors = chargePointMerchantRelationEntity.getConnectors();
                if (ObjectUtils.isNotEmpty(connectors)) {
                    List<Connector> connectorList = JSON.parseArray(connectors.toString(), Connector.class);
                    for (Connector connector : connectorList) {
                        Integer connectorId = connector.getConnectorId();
                        evseSnAndSubStatusMap.put(sn + "_" + connectorId, subStatus);
                    }
                }
            }
        }

        List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntityList = chargePointMerchantTerminalMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantTerminalEntity>()
                .eq(ChargePointMerchantTerminalEntity::getMerchantId, sellerId));

        if (ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntityList)) {
            for (ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity : chargePointMerchantTerminalEntityList) {
                String hostSn = chargePointMerchantTerminalEntity.getHostSn();
                Integer subStatus = chargePointMerchantTerminalEntity.getSubStatus();
                List<Connector> connectorsList = chargePointMerchantTerminalEntity.getConnectorsList();
                if (ObjectUtils.isNotEmpty(connectorsList)) {
                    for (Connector connector : connectorsList) {
                        Integer connectorId = connector.getConnectorId();
                        evseSnAndSubStatusMap.put(hostSn + "_" + connectorId, subStatus);
                    }
                }
            }
        }

        return ObjectUtils.isEmpty(evseSnAndSubStatusMap)
                ? null
                : evseSnAndSubStatusMap;
    }

    public int bindOwnerRelation(Long merchantId, Collection<String> sns) {
        log.info("bindOwnerRelation merchantId={} sns= {}", merchantId, JSON.toJSONString(sns));
        if (CollectionUtils.isEmpty(sns)) {
            return 0;
        }
        AtomicInteger count = new AtomicInteger();
        for (String sn : sns) {
            log.info("chargePointMerchantRelationEntity={}", sn);
            ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = bindOwnerRelation(merchantId, sn);
            log.info("chargePointMerchantRelationEntity={}", JSON.toJSONString(chargePointMerchantRelationEntity));
            Optional.ofNullable(chargePointMerchantRelationEntity).ifPresent((t) -> count.incrementAndGet());
        }
        return count.get();
    }

    public ChargePointMerchantRelationEntity bindOwnerRelation(Long merchantId, String sn) {
        return transactionTemplate.execute(transactionStatus -> {
            try {
                LambdaQueryWrapper<ChargePointMerchantRelationEntity> lambdaQuery = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
                lambdaQuery.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId).eq(ChargePointMerchantRelationEntity::getSn, sn);
                ChargePointMerchantRelationEntity newOwner = chargePointMerchantRelationMapper.selectOne(lambdaQuery);
                Assert.isTrue(Objects.nonNull(newOwner), sn + "不属于" + merchantId);
                if (MerchantChargePointRelationEnum.OWNER.getKey() == newOwner.getRelation()) {
                    log.info("不用修改 {} {}", merchantId, sn);
                    return newOwner;
                } else {
                    LambdaQueryWrapper<ChargePointMerchantRelationEntity> lambdaOriginOwnerQuery = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
                    lambdaOriginOwnerQuery.eq(ChargePointMerchantRelationEntity::getRelation, MerchantChargePointRelationEnum.OWNER.getKey()).eq(ChargePointMerchantRelationEntity::getSn, sn);
                    ChargePointMerchantRelationEntity originOwner = chargePointMerchantRelationMapper.selectOne(lambdaOriginOwnerQuery);
                    if (Objects.isNull(originOwner)) {
                        log.info("直接修改 {} {}", merchantId, sn);
                        newOwner.setRelation(MerchantChargePointRelationEnum.OWNER.getKey());
                        chargePointMerchantRelationMapper.updateById(newOwner);
                    } else {
                        log.info("交换修改 {} {}", merchantId, sn);
                        originOwner.setRelation(MerchantChargePointRelationEnum.MAINTENANCE.getKey());
                        newOwner.setRelation(MerchantChargePointRelationEnum.OWNER.getKey());
                        chargePointMerchantRelationMapper.delete(lambdaOriginOwnerQuery);
                        chargePointMerchantRelationMapper.updateById(newOwner);
                        chargePointMerchantRelationMapper.insert(originOwner);
                    }
                }
                return newOwner;
            } catch (Exception e) {
                //回滚
                log.error("update bindOwnerRelation failed: {}", sn);
                log.error("update bindOwnerRelation failed because:", e);
                transactionStatus.setRollbackOnly();
                return null;
            }
        });
    }

}
