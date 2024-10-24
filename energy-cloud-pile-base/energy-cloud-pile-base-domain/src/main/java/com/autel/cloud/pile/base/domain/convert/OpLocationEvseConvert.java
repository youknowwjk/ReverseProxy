package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.vo.OpLocationEvseElasticVO;
import org.springframework.beans.BeanUtils;

import java.math.BigDecimal;
import java.util.Map;
import java.util.Optional;

/**
 * @ClassName OpLocationEvseConvert
 * @Author A22121
 * @Description
 * @Date 2022/4/15 11:18
 * @Version 0.0.1-SNAPSHOT
 */
public class OpLocationEvseConvert {

    private OpLocationEvseConvert() {

    }

    /**
     * 转换
     *
     * @param opLocationEvseEntity
     * @return
     */
    public static OpLocationEvseDTO toOpLocationEvseDTO(OpLocationEvseEntity opLocationEvseEntity) {
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        BeanUtils.copyProperties(opLocationEvseEntity, opLocationEvseDTO);
        return opLocationEvseDTO;
    }

    /**
     * 转换
     *
     * @param opLocationEvseElasticDTO
     * @return
     */
    public static OpLocationEvseDTO toOpLocationEvseDTO(OpLocationEvseElasticDTO opLocationEvseElasticDTO) {
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        BeanUtils.copyProperties(opLocationEvseElasticDTO, opLocationEvseDTO);
        opLocationEvseDTO.setPileSN(opLocationEvseElasticDTO.getPileSn());
        return opLocationEvseDTO;
    }

    /**
     * 转换
     *
     * @param opLocationEvseElasticDTO
     * @return
     */
    public static OpLocationEvseElasticVO toOpLocationEvseElasticVO(OpLocationEvseElasticDTO opLocationEvseElasticDTO) {
        OpLocationEvseElasticVO vo = new OpLocationEvseElasticVO();
        BeanUtils.copyProperties(opLocationEvseElasticDTO, vo);
        return vo;
    }

    /**
     * 转换
     *
     * @param opLocationEvseElasticDTO
     * @return
     */
    public static OpLocationEvseRealTimeDTO toOpLocationEvseRealTimeDTO(OpLocationEvseElasticDTO opLocationEvseElasticDTO) {
        OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = new OpLocationEvseRealTimeDTO();
        BeanUtils.copyProperties(opLocationEvseElasticDTO, opLocationEvseRealTimeDTO);
        return opLocationEvseRealTimeDTO;
    }

    /**
     * 转换
     *
     * @param opLocationEvseEntity
     * @return
     */
    public static OpLocationEvseRealTimeDTO toOpLocationEvseRealTimeDTO(OpLocationEvseEntity opLocationEvseEntity) {
        OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = new OpLocationEvseRealTimeDTO();
        BeanUtils.copyProperties(opLocationEvseEntity, opLocationEvseRealTimeDTO);
        return opLocationEvseRealTimeDTO;
    }

    /**
     * 转换
     *
     * @param opLocationEvseEntity
     * @return
     */
    public static OpLocationEvseElasticDTO toOpLocationEvseElastic(OpLocationEvseElasticDTO opLocationEvseElasticDTO,OpLocationEvseEntity opLocationEvseEntity,
                                                                   OpLocationEntity opLocationEntity,
                                                                   OpLocationConnectorEntity opLocationConnectorEntity) {
        if (opLocationEvseElasticDTO == null) {
            opLocationEvseElasticDTO = new OpLocationEvseElasticDTO();
        }
        BeanUtils.copyProperties(opLocationEvseEntity, opLocationEvseElasticDTO);
        if (opLocationEntity != null) {
            opLocationEvseElasticDTO.setLocationName(opLocationEntity.getName());
            opLocationEvseElasticDTO.setOperatorId(opLocationEntity.getOperatorId());
        }
        if (opLocationConnectorEntity != null) {
            opLocationEvseElasticDTO.setGunType(opLocationConnectorEntity.getGunType());
            opLocationEvseElasticDTO.setPowerType(opLocationConnectorEntity.getPowerType());
            opLocationEvseElasticDTO.setAmperage(opLocationConnectorEntity.getAmperage());
            opLocationEvseElasticDTO.setVoltage(opLocationConnectorEntity.getVoltage());
            opLocationEvseElasticDTO.setPower(opLocationConnectorEntity.getPower());
            opLocationEvseElasticDTO.setStatus(opLocationConnectorEntity.getStatus());
        }
        return opLocationEvseElasticDTO;
    }

    /**
     * 转换
     *
     * @param opLocationEvseEntity
     * @return
     */
    public static OpLocationEvseElasticDTO toOpLocationEvseElastic(OpLocationEvseEntity opLocationEvseEntity,
                                                                   OpLocationPileEvseEntity opLocationPileEvseEntity,
                                                                   OpLocationEntity opLocationEntity,
                                                                   OpLocationConnectorEntity opLocationConnectorEntity) {
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = new OpLocationEvseElasticDTO();
        BeanUtils.copyProperties(opLocationEvseEntity, opLocationEvseElasticDTO);
        if (opLocationPileEvseEntity != null) {
            opLocationEvseElasticDTO.setPileName(opLocationPileEvseEntity.getName());
            opLocationEvseElasticDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
            opLocationEvseElasticDTO.setBrandId(opLocationPileEvseEntity.getBrandId());
        }
        if (opLocationEntity != null) {
            opLocationEvseElasticDTO.setLocationName(opLocationEntity.getName());
            opLocationEvseElasticDTO.setOperatorId(opLocationEntity.getOperatorId());
        }
        if (opLocationConnectorEntity != null) {
            opLocationEvseElasticDTO.setGunType(opLocationConnectorEntity.getGunType());
            opLocationEvseElasticDTO.setPowerType(opLocationConnectorEntity.getPowerType());
            opLocationEvseElasticDTO.setAmperage(opLocationConnectorEntity.getAmperage());
            opLocationEvseElasticDTO.setVoltage(opLocationConnectorEntity.getVoltage());
            opLocationEvseElasticDTO.setPower(opLocationConnectorEntity.getPower());
            opLocationEvseElasticDTO.setStatus(opLocationConnectorEntity.getStatus());
        }
        return opLocationEvseElasticDTO;
    }

    public static void toOpLocationEvseElasticDto(OpLocationEvseElasticDTO target,OpLocationEvseEntity evseSource,OpLocationPileEvseEntity pileSource,OpLocationEntity locationSource,OpLocationConnectorEntity connectorSource){
        target.setId(evseSource.getId());
        target.setCreatedAt(evseSource.getCreatedAt());
        target.setUpdatedAt(evseSource.getUpdatedAt());
        target.setStatus(evseSource.getStatus());
        target.setLocationId(locationSource.getId());
        target.setLocationName(locationSource.getName());
        if (target.getState() == null) {
            target.setState(evseSource.getState());
        }
        target.setEvseId(evseSource.getEvseId());
        target.setFloorLevel(evseSource.getFloorLevel());
        target.setLatitude(evseSource.getLatitude());
        target.setLongitude(evseSource.getLongitude());
        target.setPhysicalReference(evseSource.getPhysicalReference());
        target.setParkingRestrictions(evseSource.getParkingRestrictions());
        target.setPinCode(evseSource.getPinCode());
        target.setTariffId(evseSource.getTariffId());
        target.setEvseSn(evseSource.getEvseSn());
        target.setGunType(connectorSource.getGunType());
        target.setPowerType(connectorSource.getPowerType());
        target.setVoltage(connectorSource.getVoltage());
        target.setAmperage(connectorSource.getAmperage());
        target.setPower(connectorSource.getPower());
        target.setBrandId(pileSource.getBrandId());
        target.setPileSn(pileSource.getPileSn());
        target.setPileName(pileSource.getName());
        target.setOperatorId(locationSource.getOperatorId());
        target.setReservationEnabled(locationSource.getReservationEnabled());
    }

    /**
     * 转换
     *
     * @param opLocationEvseDTO
     * @return
     */
    public static OpLocationEvseEntity toOpLocationEvseEntity(OpLocationEvseDTO opLocationEvseDTO) {
        OpLocationEvseEntity opLocationEvseEntity = new OpLocationEvseEntity();
        BeanUtils.copyProperties(opLocationEvseDTO, opLocationEvseEntity);
        return opLocationEvseEntity;
    }

    /**
     * 从map中获取数据
     *
     * @param opLocationEvseRealTimeDTO
     * @param monitorMap
     */
    public static void setDataFromMap(OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO, Map<String, String> monitorMap) {
        if (monitorMap.containsKey(BaseConstant.CREATE_TIME) && monitorMap.get(BaseConstant.CREATE_TIME) != null) {
            opLocationEvseRealTimeDTO.setCreateTime(monitorMap.get(BaseConstant.CREATE_TIME));
        }
        if (monitorMap.containsKey(BaseConstant.CURRENT) && monitorMap.get(BaseConstant.CURRENT) != null) {
            opLocationEvseRealTimeDTO.setCurrent(Double.valueOf(monitorMap.get(BaseConstant.CURRENT)));
        }
        if (monitorMap.containsKey(BaseConstant.VOLTAGE) && monitorMap.get(BaseConstant.VOLTAGE) != null) {
            opLocationEvseRealTimeDTO.setVoltage(Double.valueOf(monitorMap.get(BaseConstant.VOLTAGE)));
        }
        if (monitorMap.containsKey(BaseConstant.POWER) && monitorMap.get(BaseConstant.POWER) != null) {
            opLocationEvseRealTimeDTO.setPower(Double.valueOf(monitorMap.get(BaseConstant.POWER)));
        }
        if (monitorMap.containsKey(BaseConstant.BUS_ID) && monitorMap.get(BaseConstant.BUS_ID) != null) {
            opLocationEvseRealTimeDTO.setBusId(monitorMap.get(BaseConstant.BUS_ID));
        }
        if (monitorMap.containsKey(BaseConstant.BATTERY_SOC) && monitorMap.get(BaseConstant.BATTERY_SOC) != null) {
            opLocationEvseRealTimeDTO.setBatterySoc(Double.valueOf(monitorMap.get(BaseConstant.BATTERY_SOC)));
        }
        if (monitorMap.containsKey(BaseConstant.TOTAL_ELECTRIVAL_POWER) && monitorMap.get(BaseConstant.TOTAL_ELECTRIVAL_POWER) != null) {
            opLocationEvseRealTimeDTO.setTotalElectricalPower(Double.valueOf(monitorMap.get(BaseConstant.TOTAL_ELECTRIVAL_POWER)));
        }
        if (monitorMap.containsKey(BaseConstant.POSITIVE_TEMPERATURE) && monitorMap.get(BaseConstant.POSITIVE_TEMPERATURE) != null) {
            opLocationEvseRealTimeDTO.setPositiveTemperature(Double.valueOf(monitorMap.get(BaseConstant.POSITIVE_TEMPERATURE)));
        }
        if (monitorMap.containsKey(BaseConstant.NEGATIVE_TEMPERATURE) && monitorMap.get(BaseConstant.NEGATIVE_TEMPERATURE) != null) {
            opLocationEvseRealTimeDTO.setNegativeTemperature(Double.valueOf(monitorMap.get(BaseConstant.NEGATIVE_TEMPERATURE)));
        }
        if (monitorMap.containsKey(BaseConstant.RD_TIME_LEFT) && monitorMap.get(BaseConstant.RD_TIME_LEFT) != null) {
            opLocationEvseRealTimeDTO.setRdTimeLeft(Integer.valueOf(monitorMap.get(BaseConstant.RD_TIME_LEFT)));
        }
    }

    /**
     * 转换
     *
     * @param opEvseChargingThresholdDTO
     * @return
     */
    public static OpEvseChargingThresholdEntity toOpEvseChargingThresholdEntity(OpEvseChargingThresholdDTO opEvseChargingThresholdDTO) {
        OpEvseChargingThresholdEntity opEvseChargingThresholdEntity = new OpEvseChargingThresholdEntity();
        BeanUtils.copyProperties(opEvseChargingThresholdDTO, opEvseChargingThresholdEntity);
        return opEvseChargingThresholdEntity;
    }

    /**
     * 转换
     *
     * @param opEvseChargingThresholdDTO
     * @return
     */
    public static void toOpEvseChargingThresholdEntity(OpEvseChargingThresholdEntity entity, OpEvseChargingThresholdDTO opEvseChargingThresholdDTO) {
        opEvseChargingThresholdDTO.setId(entity.getId());
        opEvseChargingThresholdDTO.setCreatedAt(entity.getCreatedAt());
        opEvseChargingThresholdDTO.setUpdatedAt(System.currentTimeMillis());
        opEvseChargingThresholdDTO.setDeleted(entity.getDeleted());
        opEvseChargingThresholdDTO.setStatus(entity.getStatus());
        BeanUtils.copyProperties(opEvseChargingThresholdDTO, entity);
    }

    /**
     * 转换
     *
     * @param opEvseDirectionsDTO
     * @return
     */
    public static OpEvseDirectionsEntity toOpEvseDirectionsEntity(OpEvseDirectionsDTO opEvseDirectionsDTO) {
        OpEvseDirectionsEntity opEvseDirectionsEntity = new OpEvseDirectionsEntity();
        BeanUtils.copyProperties(opEvseDirectionsDTO, opEvseDirectionsEntity);
        return opEvseDirectionsEntity;
    }

    /**
     * 转换
     *
     * @param opEvseImageDTO
     * @return
     */
    public static OpEvseImageEntity toOpEvseImageEntity(OpEvseImageDTO opEvseImageDTO) {
        OpEvseImageEntity opEvseImageEntity = new OpEvseImageEntity();
        BeanUtils.copyProperties(opEvseImageDTO, opEvseImageEntity);
        return opEvseImageEntity;
    }

    /**
     * 转换
     *
     * @param opEvseSocThresholdDTO
     * @return
     */
    public static OpEvseSocThresholdEntity toOpEvseSocThresholdEntity(OpEvseSocThresholdDTO opEvseSocThresholdDTO) {
        OpEvseSocThresholdEntity opEvseSocThresholdEntity = new OpEvseSocThresholdEntity();
        BeanUtils.copyProperties(opEvseSocThresholdDTO, opEvseSocThresholdEntity);
        return opEvseSocThresholdEntity;
    }

    /**
     * 转换
     *
     * @param opEvseSocThresholdDTO
     * @return
     */
    public static void toOpEvseSocThresholdEntity(OpEvseSocThresholdEntity entity, OpEvseSocThresholdDTO opEvseSocThresholdDTO) {
        opEvseSocThresholdDTO.setId(entity.getId());
        opEvseSocThresholdDTO.setCreatedAt(entity.getCreatedAt());
        opEvseSocThresholdDTO.setUpdatedAt(System.currentTimeMillis());
        opEvseSocThresholdDTO.setDeleted(entity.getDeleted());
        opEvseSocThresholdDTO.setStatus(entity.getStatus());
        BeanUtils.copyProperties(opEvseSocThresholdDTO, entity);
    }

    /**
     * 转换
     *
     * @param opLocationConnectorDTO
     * @return
     */
    public static OpLocationConnectorEntity toOpLocationConnectorEntity(OpLocationConnectorDTO opLocationConnectorDTO) {
        OpLocationConnectorEntity opLocationConnectorEntity = new OpLocationConnectorEntity();
        BeanUtils.copyProperties(opLocationConnectorDTO, opLocationConnectorEntity);
        return opLocationConnectorEntity;
    }

    /**
     * 转换
     *
     * @param opLocationEvseElasticDTO
     * @return
     */
    public static OpEvseInfoDTO toOpEvseInfoDTO(OpLocationEvseElasticDTO opLocationEvseElasticDTO) {
        OpEvseInfoDTO opEvseInfoDTO = new OpEvseInfoDTO();
        BeanUtils.copyProperties(opLocationEvseElasticDTO, opEvseInfoDTO);
        return opEvseInfoDTO;
    }

    /**
     * 转换
     *
     * @param opLocationEvseEntity
     * @return
     */
    public static OpEvseInfoDTO toOpEvseInfoDTO(OpLocationEvseEntity opLocationEvseEntity) {
        OpEvseInfoDTO opEvseInfoDTO = new OpEvseInfoDTO();
        BeanUtils.copyProperties(opLocationEvseEntity, opEvseInfoDTO);
        return opEvseInfoDTO;
    }

    /**
     * 转换
     *
     * @param opEvseChargingThresholdEntity
     * @return
     */
    public static OpEvseChargingThresholdDTO toOpEvseChargingThresholdDTO(OpEvseChargingThresholdEntity opEvseChargingThresholdEntity) {
        OpEvseChargingThresholdDTO opEvseChargingThresholdDTO = new OpEvseChargingThresholdDTO();
        BeanUtils.copyProperties(opEvseChargingThresholdEntity, opEvseChargingThresholdDTO);
        return opEvseChargingThresholdDTO;
    }

    /**
     * 转换
     *
     * @param opEvseSocThresholdEntity
     * @return
     */
    public static OpEvseSocThresholdDTO toOpEvseSocThresholdDTO(OpEvseSocThresholdEntity opEvseSocThresholdEntity) {
        OpEvseSocThresholdDTO op = new OpEvseSocThresholdDTO();
        BeanUtils.copyProperties(opEvseSocThresholdEntity, op);
        return op;
    }

    /**
     * 转换
     *
     * @param op
     * @return
     */
    public static OpEvseDirectionsDTO toOpEvseDirectionsDTO(OpEvseDirectionsEntity op) {
        OpEvseDirectionsDTO opEvseDirectionsDTO = new OpEvseDirectionsDTO();
        BeanUtils.copyProperties(op, opEvseDirectionsDTO);
        return opEvseDirectionsDTO;
    }

    /**
     * 转换
     *
     * @param op
     * @return
     */
    public static OpEvseImageDTO toOpEvseImageDTO(OpEvseImageEntity op) {
        OpEvseImageDTO opEvseImageDTO = new OpEvseImageDTO();
        BeanUtils.copyProperties(op, opEvseImageDTO);
        return opEvseImageDTO;
    }

    /**
     * 转换
     *
     * @param op
     * @return
     */
    public static OpLocationConnectorDTO toOpLocationConnectorDTO(OpLocationConnectorEntity op) {
        OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();

        if (op != null) {
            BeanUtils.copyProperties(op, opLocationConnectorDTO);
        }

        return opLocationConnectorDTO;
    }

    /**
     * 转化
     *
     * @param opLocationEvseElasticDTO
     * @return
     */
    public static OpEvseAssociatedRuleDTO toOpEvseAssociatedRuleDTO(OpLocationEvseElasticDTO opLocationEvseElasticDTO) {
        return OpEvseAssociatedRuleDTO.builder()
                .createdAt(opLocationEvseElasticDTO.getCreatedAt())
                .evseId(opLocationEvseElasticDTO.getId())
                .tariffId(opLocationEvseElasticDTO.getTariffId())
                .evseSn(opLocationEvseElasticDTO.getEvseSn())
                .powerType(opLocationEvseElasticDTO.getPowerType())
                .gunType(opLocationEvseElasticDTO.getGunType())
                .locationId(opLocationEvseElasticDTO.getLocationId())
                .locationName(opLocationEvseElasticDTO.getLocationName())
                .physicalReference(opLocationEvseElasticDTO.getPhysicalReference())
                .voltage(opLocationEvseElasticDTO.getVoltage())
                .amperage(opLocationEvseElasticDTO.getAmperage())
                .power(BigDecimal.valueOf(Optional.ofNullable(opLocationEvseElasticDTO.getPower()).orElse(0D)))
                .connector(opLocationEvseElasticDTO.getEvseSn().split("_")[1])
                .build();
    }

    /**
     * 转化
     *
     * @param opLocationEntity
     * @param opLocationOperationEntity
     * @return
     */
    public static OpLocationsAndEvsesDTO toOpLocationsAndEvsesDTO(OpLocationEntity opLocationEntity, OpLocationOperationEntity opLocationOperationEntity) {
        OpLocationsAndEvsesDTO opLocationsAndEvsesDTO = new OpLocationsAndEvsesDTO();
        if (opLocationEntity != null) {
            opLocationsAndEvsesDTO.setId(opLocationEntity.getId());
            opLocationsAndEvsesDTO.setAddress(opLocationEntity.getAddress());
            opLocationsAndEvsesDTO.setName(opLocationEntity.getName());
        }
        if (opLocationOperationEntity != null) {
            opLocationsAndEvsesDTO.setOperationStatus(opLocationOperationEntity.getStatus());
        }
        return opLocationsAndEvsesDTO;
    }

    public static EvseInfoDTO toEvseInfoDTO(OpLocationEvseEntity opLocationEvseEntity, OpLocationConnectorEntity opLocationConnectorEntity) {
        EvseInfoDTO evseInfoDTO = new EvseInfoDTO();
        if (opLocationEvseEntity != null) {
            evseInfoDTO.setEvse_sn(opLocationEvseEntity.getEvseSn());
        }
        if (opLocationConnectorEntity != null) {
            evseInfoDTO.setPower(opLocationConnectorEntity.getPower());
        }
        return evseInfoDTO;
    }
}
















