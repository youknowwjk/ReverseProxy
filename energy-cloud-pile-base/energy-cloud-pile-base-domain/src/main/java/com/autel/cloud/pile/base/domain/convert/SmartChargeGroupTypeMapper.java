package com.autel.cloud.pile.base.domain.convert;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.dto.LoadLimitPolicy;
import com.autel.cloud.pile.base.dto.SmartChargeGroupConfigAddParamDTOcopy;
import com.autel.cloud.pile.base.dto.SmartChargeGroupConfigUpdateParamDTOcopy;
import com.autel.cloud.pile.base.dto.TimeSettingDetailDTO;
import com.autel.cloud.pile.base.enums.UnitEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.vo.PileGroupDetailV3VOcopy;
import com.autel.cloud.pile.base.vo.PileGroupTreeVOcopy;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.mapstruct.factory.Mappers;
import org.springframework.util.StringUtils;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;


@Mapper
public interface SmartChargeGroupTypeMapper {


    SmartChargeGroupTypeMapper INSTANCE = Mappers.getMapper(SmartChargeGroupTypeMapper.class);

    @Mapping(target = "timeSettingDetail", source = "timeSettingDetail", ignore = true)
    OpLocationPileGroupEntity dto2Entity(SmartChargeGroupConfigAddParamDTOcopy smartChargeGroupConfigParamDTO);


    @Mapping(target = "timeSettingDetail", source = "timeSettingDetail", ignore = true)
    OpLocationPileGroupEntity dto2Entity(SmartChargeGroupConfigUpdateParamDTOcopy smartChargeGroupConfigParamDTO);


    @Mapping(target = "timeSettingDetail", source = "timeSettingDetail", ignore = true)
    PileGroupDetailV3VOcopy entity2VO(OpLocationPileGroupEntity entity);

    SmartChargeGroupConfigAddParamDTOcopy update2Add(SmartChargeGroupConfigUpdateParamDTOcopy entity);

    @AfterMapping
    default void afterMapping(@MappingTarget PileGroupDetailV3VOcopy target, OpLocationPileGroupEntity source) {
        target.setGroupId(source.getId());

        String detail = source.getTimeSettingDetail();
        if (StringUtils.hasText(detail)) {
            List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
            target.setTimeSettingDetail(timeSettingDetailDTOS.stream().sorted((f, s) -> (int) (f.getId() - s.getId())).collect(Collectors.toList()));
        }
        if (target.getSecurityEnabled() == null) {
            target.setSecurityEnabled(false);
        }
    }

    @AfterMapping
    default void handleRelation(@MappingTarget OpLocationPileGroupEntity target, SmartChargeGroupConfigAddParamDTOcopy source) {
        if (UnitEnum.POWER.getCode().equals(source.getChargingUpUnit())) {
            target.setChargingUpType("Power");
        }
        if (UnitEnum.CURRENT.getCode().equals(source.getChargingUpUnit())) {
            target.setChargingUpType("Current");
        }
        if (!CollectionUtils.isEmpty(source.getTimeSettingDetail())) {
            target.setTimeSettingMode(1);
            target.setTimeSettingDetail(JSON.toJSONString(source.getTimeSettingDetail()));
        }
        //取第一个场站ID
        List<PileGroupTreeVOcopy> list = source.getPileGroupList();
        if (!CollectionUtils.isEmpty(list)) {
            target.setLocationId(list.get(0).getLocationId());
        }
        String chargingUpUnit = target.getChargingUpUnit();
        if (StringUtils.hasText(chargingUpUnit)) {
            target.setChargingUpUnit(chargingUpUnit.toUpperCase());
        }
        if (Objects.isNull(target.getStatus())) {
            target.setStatus(1);
        }
        target.setUseSmartCharging(1);
        target.setSmartChargingMode(0);
    }

    @AfterMapping
    default void handleRelation(@MappingTarget OpLocationPileGroupEntity target, SmartChargeGroupConfigUpdateParamDTOcopy source) {
        if (UnitEnum.POWER.getCode().equals(source.getChargingUpUnit())) {
            target.setChargingUpType("Power");
        }
        if (UnitEnum.CURRENT.getCode().equals(source.getChargingUpUnit())) {
            target.setChargingUpType("Current");
        }
        String chargingUpUnit = target.getChargingUpUnit();
        if (StringUtils.hasText(chargingUpUnit)) {
            target.setChargingUpUnit(chargingUpUnit.toUpperCase());
        }
        if (!ObjectUtils.isEmpty(source.getLoadType())) {
            target.setLoadType(source.getLoadType());
            if (source.getLoadType() == 0) {
                target.setMeterId(null);
                target.setMeterLocation(null);
            }
            if (source.getLoadType() == 1) {
                target.setMeterId(source.getMeterId());
                target.setMeterLocation(source.getMeterLocation());
            }
        }
        if (!ObjectUtils.isEmpty(source.getEnergyUseStrategy())) {
            target.setEnergyUseStrategy(source.getEnergyUseStrategy());
            if (source.getEnergyUseStrategy() == 0) {
                target.setTariffId(null);
                target.setFavor(null);
                target.setPlanTime(null);
            }
            if (source.getEnergyUseStrategy() == 1) {
                target.setTariffId(source.getTariffId());
                target.setFavor(source.getFavor());
                target.setPlanTime(source.getPlanTime());
            }
        }
        target.setSecurityEnabled(false);
        if (!ObjectUtils.isEmpty(source.getSecurityEnabled())) {
            target.setSecurityEnabled(source.getSecurityEnabled());
        }
        List<LoadLimitPolicy> timeSettingDetails = source.getTimeSettingDetail();
        if (!CollectionUtils.isEmpty(timeSettingDetails)) {
            target.setTimeSettingMode(1);
            target.setTimeSettingDetail(JSON.toJSONString(timeSettingDetails));
        } else {
            target.setTimeSettingMode(0);
            target.setTimeSettingDetail(null);
        }
        target.setUseSmartCharging(1);
        target.setSmartChargingMode(0);
    }

}
