package com.autel.cloud.pile.base.domain.convert;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.dto.DeliveryEvseInfoDTO;
import com.autel.cloud.pile.base.dto.DeliveryGroupDTO;
import com.autel.cloud.pile.base.dto.DeliveryPileInfoDTO;
import com.autel.cloud.pile.base.dto.TimeSettingDetailDTO;
import com.autel.cloud.pile.base.enums.AllocationStrategyEnums;
import com.autel.cloud.pile.base.enums.UsageScenarioEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.vo.*;
import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;

import java.math.BigDecimal;

/**
 * @Author temp
 * @Date 2023/8/31 11:37
 * @Since 1.0.0
 */
@Mapper(componentModel = "spring")
public interface OncePerRequestConvert {

    @Mapping(target = "sellerId", source = "merchantId")
    @Mapping(target = "allocationStrategy", source = "allocationStrategy", ignore = true)
    @Mapping(target = "timeSettingDetail", source = "timeSettingDetail", ignore = true)
    DeliveryGroupDTO toDeliveryDto(OpLocationPileGroupEntity source);

    @Mapping(target = "sellerId", source = "merchantId")
    @Mapping(target = "allocationStrategy", source = "allocationStrategy", ignore = true)
    OpLocationPileGroupVO toGroupVo(OpLocationPileGroupEntity source);

    @Mapping(target = "id", source = "id", ignore = true)
    @Mapping(target = "associateId", source = "id")
    OpLocationPileGroupVO.GroupPileDTO toPileVo(OpLocationPileGroupAssociateEntity source);

    OpLocationPileGroupVO.GroupPileEvseDTO toEvseVo(OpLocationEvseElasticDTO source);

    DeliveryPileInfoTreeVO toTreeVo(DeliveryPileInfoDTO source);

    DeliveryEvseInfoTreeVO toTreeVo(DeliveryEvseInfoDTO source);

    @Mapping(target = "groupId", source = "dto.id")
    @Mapping(target = "groupName", source = "dto.name")
    @Mapping(target = "groupLimit", source = "dto.chargingUp")
    @Mapping(target = "chargingUp", source = "dto.chargingUp")
    @Mapping(target = "phaseNum", source = "evseDto.phaseNum")
    @Mapping(target = "tariffId", source = "evseDto.tariffId")
    @Mapping(target = "upload", source = "evseDto.timeValue")
    @Mapping(target = "pileSn", source = "pileDto.pileSn")
    @Mapping(target = "associateId", source = "pileDto.associateId")
    OpLocationPileGroupDeliveryVO toDeliveryVo(DeliveryGroupDTO dto, DeliveryPileInfoDTO pileDto, DeliveryEvseInfoDTO evseDto);

    @Mapping(target = "groupId", source = "dto.id")
    @Mapping(target = "groupName", source = "dto.name")
    @Mapping(target = "groupLimit", source = "dto.chargingUp")
    @Mapping(target = "chargingUp", source = "dto.chargingUp")
    @Mapping(target = "phaseNum", source = "evseDto.phaseNum")
    @Mapping(target = "tariffId", source = "evseDto.tariffId")
    @Mapping(target = "upload", source = "evseDto.timeValue")
    @Mapping(target = "pileSn", source = "pileDto.pileSn")
    @Mapping(target = "associateId", source = "pileDto.associateId")
    OpLocationPileGroupDeliveryVO toDeliveryVo(DeliveryGroupTreeVO dto, DeliveryPileInfoTreeVO pileDto, DeliveryEvseInfoTreeVO evseDto);

    OpLocationPileGroupDeliveryVO copy(OpLocationPileGroupDeliveryVO source);

    DeliveryEvseInfoDTO toDeliveryDto(OpLocationEvseElasticDTO source);

    @Mapping(target = "pileName", source = "name")
    DeliveryPileInfoDTO toDeliveryDto(OpLocationPileEvseElasticDTO source);

    @Mapping(target = "pileInfoList", source = "pileInfoList")
    @Mapping(target = "usableChargingUp", source = "chargingUp")
    DeliveryGroupTreeVO toTreeVo(DeliveryGroupDTO source);

    @AfterMapping
    default void handleRelation(@MappingTarget OpLocationPileGroupVO target, OpLocationPileGroupEntity source) {
        target.setAllocationStrategy(AllocationStrategyEnums.getCodeByValue(source.getAllocationStrategy()));
    }

    @AfterMapping
    default void handleRelation(@MappingTarget OpLocationPileGroupDeliveryVO target, DeliveryGroupTreeVO dto, DeliveryPileInfoTreeVO pileDto, DeliveryEvseInfoTreeVO evseDto) {
        target.setUsageScenario(UsageScenarioEnum.BUSINESS_PILE.getCode());
        target.setOfflineCurrent(BigDecimal.ZERO);
        target.setFirstDownThenUp(0);
    }

    @AfterMapping
    default void handleRelation(@MappingTarget OpLocationPileGroupDeliveryVO target, DeliveryGroupDTO dto, DeliveryPileInfoDTO pileDto, DeliveryEvseInfoDTO evseDto) {
        target.setUsageScenario(UsageScenarioEnum.BUSINESS_PILE.getCode());
        target.setOfflineCurrent(BigDecimal.ZERO);
        target.setFirstDownThenUp(0);
    }

    @AfterMapping
    default void handleRelation(@MappingTarget DeliveryGroupTreeVO target, DeliveryGroupDTO source) {
        target.setPileNumber(0);
        target.setTotal(BigDecimal.ZERO);
        target.setUsableTotal(BigDecimal.ZERO);
        target.setRemove(BigDecimal.ZERO);
    }


    @AfterMapping
    default void handleRelation(@MappingTarget DeliveryGroupDTO target, OpLocationPileGroupEntity source) {
        target.setAllocationStrategy(AllocationStrategyEnums.getCodeByValue(source.getAllocationStrategy()));
        Integer timeSettingMode = source.getTimeSettingMode();
        String timeSettingDetail = source.getTimeSettingDetail();
        if (timeSettingMode != null && timeSettingMode == 1) {
            target.setTimeSettingDetail(JSON.parseArray(timeSettingDetail, TimeSettingDetailDTO.class));
        }
    }

    @AfterMapping
    default void handleRelation(@MappingTarget DeliveryPileInfoDTO target, OpLocationPileEvseElasticDTO source) {

    }

    @AfterMapping
    default void handleRelation(@MappingTarget DeliveryEvseInfoDTO target, OpLocationEvseElasticDTO source) {
        target.setIsVip(false);
        target.setLast(false);
        target.setTotalElectricalPower(BigDecimal.ZERO);
    }
}
