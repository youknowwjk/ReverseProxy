package com.autel.cloud.pile.base.domain.convert;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.pile.base.dto.OpEvseInfoDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.dto.OpPileAssociatedRuleDTO;
import com.autel.cloud.pile.base.enums.BrandEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.vo.OpPileEvseInfoVO;
import com.autel.cloud.pile.base.vo.britishAct.BritishActVO;
import com.autel.cloud.pile.base.vo.britishAct.DefaultChargingTimeVO;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;

import java.util.List;

/**
 * @ClassName OpLocationPileEvseConvert
 * @Author A22121
 * @Description
 * @Date 2022/5/12 20:50
 * @Version 0.0.1-SNAPSHOT
 */
public class OpLocationPileEvseConvert {

    private OpLocationPileEvseConvert() {

    }

    /**
     * to OpLocationPileEvseEntity
     *
     * @param opLocationEvseDTO
     * @return
     */
    public static OpLocationPileEvseEntity toOpLocationPileEvseEntity(OpLocationEvseDTO opLocationEvseDTO) {
        OpLocationPileEvseEntity opLocationPileEvseEntity = new OpLocationPileEvseEntity();
        opLocationPileEvseEntity.setLocationId(opLocationEvseDTO.getLocationId());
        opLocationPileEvseEntity.setPileSn(opLocationEvseDTO.getPileSN());
        opLocationPileEvseEntity.setName(opLocationEvseDTO.getPileName());
        opLocationPileEvseEntity.setBrandId(opLocationEvseDTO.getBrandId());
        opLocationPileEvseEntity.setProductModel(opLocationEvseDTO.getProductModel());
        opLocationPileEvseEntity.setVendor(opLocationEvseDTO.getVendor());
        opLocationPileEvseEntity.setPublicMark(opLocationEvseDTO.getPublicMark());
        opLocationPileEvseEntity.setCreatedAt(System.currentTimeMillis());
        opLocationPileEvseEntity.setUpdatedAt(System.currentTimeMillis());
        return opLocationPileEvseEntity;
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseElasticDTO
     * @return
     */
    public static OpLocationPileEvseDTO toOpLocationPileEvseDTO(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO) {
        OpLocationPileEvseDTO opLocationPileEvseDTO = new OpLocationPileEvseDTO();
        BeanUtils.copyProperties(opLocationPileEvseElasticDTO, opLocationPileEvseDTO);
        return opLocationPileEvseDTO;
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseDTO
     * @param opEvseInfoDTO
     * @param status
     * @return
     */
    public static OpPileEvseInfoVO toOpPileEvseInfoVO(OpLocationPileEvseDTO opLocationPileEvseDTO,
                                                      OpEvseInfoDTO opEvseInfoDTO,
                                                      String status,
                                                      OpEvseBrandEntity opEvseBrandEntity,
                                                      int evseSize,
                                                      boolean hubject,
                                                      List<String> connectType,
                                                      OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO,
                                                      boolean eroamingEnable) {
        OpPileEvseInfoVO opPileEvseInfoVO = new OpPileEvseInfoVO();
        opPileEvseInfoVO.setPileSn(opLocationPileEvseDTO.getPileSn());
        opPileEvseInfoVO.setPileId(opLocationPileEvseDTO.getId());
        opPileEvseInfoVO.setPileName(opLocationPileEvseDTO.getName());
        opPileEvseInfoVO.setLocationId(opLocationPileEvseDTO.getLocationId());
        opPileEvseInfoVO.setLocationName(opEvseInfoDTO.getLocationName());
        opPileEvseInfoVO.setBrandId(opLocationPileEvseDTO.getBrandId());
        opPileEvseInfoVO.setBrandName("");
        if (!opLocationPileEvseDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
            opPileEvseInfoVO.setDlbEnable(Boolean.FALSE);
        }
        opPileEvseInfoVO.setPowerType(opEvseInfoDTO.getPowerType());
        opPileEvseInfoVO.setTariffId(opEvseInfoDTO.getTariffId());
        opPileEvseInfoVO.setVendor(opLocationPileEvseDTO.getVendor());
        opPileEvseInfoVO.setProductModel(opLocationPileEvseDTO.getProductModel());
        if (StringUtils.isNotBlank(status)) {
            opPileEvseInfoVO.setCanReset(
                    status.equals(EvseDeviceStatusEnum.FAULTED.getName()) ||
                            status.equals(EvseDeviceStatusEnum.UNAVAILABLE.getName()) ||
                            status.equals(EvseDeviceStatusEnum.AVAILABLE.getName()));
            opPileEvseInfoVO.setOnLine(!status.equals(EvseDeviceStatusEnum.DEFAULT.getName()));
        } else {
            opPileEvseInfoVO.setCanReset(Boolean.FALSE);
            opPileEvseInfoVO.setOnLine(Boolean.FALSE);
        }

        opPileEvseInfoVO.setPower(opEvseInfoDTO.getPower());
        opPileEvseInfoVO.setVoltage(opEvseInfoDTO.getVoltage());
        opPileEvseInfoVO.setAmperage(opEvseInfoDTO.getAmperage());
        if (opEvseInfoDTO.getPowerType() != null && opEvseInfoDTO.getPowerType().startsWith("AC")) {
            if ("AC_1_PHASE".equalsIgnoreCase(opEvseInfoDTO.getPowerType())) {
                opPileEvseInfoVO.setPhase("1");
            }else if ("AC_3_PHASE".equalsIgnoreCase(opEvseInfoDTO.getPowerType())) {
                opPileEvseInfoVO.setPhase("3");
            }

        }
        if (opLocationPileEvseDTO.getBrandId() != null) {
            opPileEvseInfoVO.setBrandName(opEvseBrandEntity.getName());
        }
        if (StringUtils.isNotBlank(status)) {
            if (status.equals(EvseDeviceStatusEnum.AVAILABLE.getName())) {
                opPileEvseInfoVO.setPileStatus(EvseDeviceStatusEnum.AVAILABLE.getName());
                opPileEvseInfoVO.setPileStatusCode(EvseDeviceStatusEnum.AVAILABLE.getCode());
            } else if (status.equals(EvseDeviceStatusEnum.FAULTED.getName())) {
                opPileEvseInfoVO.setPileStatus(EvseDeviceStatusEnum.FAULTED.getName());
                opPileEvseInfoVO.setPileStatusCode(EvseDeviceStatusEnum.FAULTED.getCode());
            } else if (status.equals(EvseDeviceStatusEnum.UNAVAILABLE.getName())) {
                opPileEvseInfoVO.setPileStatus(EvseDeviceStatusEnum.UNAVAILABLE.getName());
                opPileEvseInfoVO.setPileStatusCode(EvseDeviceStatusEnum.UNAVAILABLE.getCode());
            } else if (status.equals(EvseDeviceStatusEnum.DEFAULT.getName())) {
                opPileEvseInfoVO.setPileStatus(EvseDeviceStatusEnum.DEFAULT.getName());
                opPileEvseInfoVO.setPileStatusCode(EvseDeviceStatusEnum.DEFAULT.getCode());
            }
        }
        //12月份新版桩详情增加枪数统计
        opPileEvseInfoVO.setEvseNum(evseSize);
        opPileEvseInfoVO.setHubjectChecked(hubject);
        //网络类型 BT:蓝牙; WIFI:wifi; 4G:4G;
        opPileEvseInfoVO.setConnectType(connectType);
        opPileEvseInfoVO.setPublicMark(opLocationPileEvseElasticDTO.getPublicMark());
        if (eroamingEnable
                && hubject
                && Integer.valueOf(1).equals(opLocationPileEvseElasticDTO.getPublicMark())) {
            opPileEvseInfoVO.setSupportEroamingMark(true);
            opPileEvseInfoVO.setHubjectEnable(opLocationPileEvseElasticDTO.getEroamingEnable() == null ? 0 : opLocationPileEvseElasticDTO.getEroamingEnable());
        } else {
            opPileEvseInfoVO.setSupportEroamingMark(false);
        }
        return opPileEvseInfoVO;
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseDTO
     * @return
     */
    public static OpPileEvseInfoVO toOpPileEvseInfoVO(OpLocationPileEvseDTO opLocationPileEvseDTO) {
        OpPileEvseInfoVO opPileEvseInfoVO = new OpPileEvseInfoVO();
        opPileEvseInfoVO.setPileSn(opLocationPileEvseDTO.getPileSn());
        opPileEvseInfoVO.setPileId(opLocationPileEvseDTO.getId());
        opPileEvseInfoVO.setPileName(opLocationPileEvseDTO.getName());
        opPileEvseInfoVO.setLocationName(opLocationPileEvseDTO.getLocationName());
        opPileEvseInfoVO.setLocationId(opLocationPileEvseDTO.getLocationId());
        opPileEvseInfoVO.setBrandId(opLocationPileEvseDTO.getBrandId());
        opPileEvseInfoVO.setBrandName("");
        opPileEvseInfoVO.setVendor(opLocationPileEvseDTO.getVendor());
        opPileEvseInfoVO.setProductModel(opLocationPileEvseDTO.getProductModel());
        opPileEvseInfoVO.setEvseList(opLocationPileEvseDTO.getEvseList());
        return opPileEvseInfoVO;
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseEntity
     * @return
     */
    public static OpLocationPileEvseElasticDTO toOpLocationPileEvseElastic(OpLocationPileEvseEntity opLocationPileEvseEntity,
                                                                           OpLocationEvseEntity opLocationEvseEntity,
                                                                           OpLocationEntity opLocationEntity) {
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = new OpLocationPileEvseElasticDTO();
        opLocationPileEvseElasticDTO.setId(opLocationPileEvseEntity.getId());
        opLocationPileEvseElasticDTO.setCreatedAt(opLocationPileEvseEntity.getCreatedAt());
        opLocationPileEvseElasticDTO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
        opLocationPileEvseElasticDTO.setLocationId(opLocationPileEvseEntity.getLocationId());
        opLocationPileEvseElasticDTO.setEvseList(opLocationPileEvseEntity.getEvseList());
        opLocationPileEvseElasticDTO.setName(opLocationPileEvseEntity.getName());
        opLocationPileEvseElasticDTO.setBrandId(opLocationPileEvseEntity.getBrandId());
        opLocationPileEvseElasticDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
        opLocationPileEvseElasticDTO.setProductModel(opLocationPileEvseEntity.getProductModel());
        opLocationPileEvseElasticDTO.setVendor(opLocationPileEvseEntity.getVendor());
        opLocationPileEvseElasticDTO.setOperatorId(opLocationEntity.getOperatorId());
        opLocationPileEvseElasticDTO.setLocationName(opLocationEntity.getName());
        if (opLocationEvseEntity != null) {
            opLocationPileEvseElasticDTO.setTariffId(opLocationEvseEntity.getTariffId());
        }
        return opLocationPileEvseElasticDTO;
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseEntity
     * @return
     */
    public static OpLocationPileEvseElasticDTO toOpLocationPileEvseElastic(OpLocationPileEvseEntity opLocationPileEvseEntity,
                                                                           OpLocationEntity opLocationEntity) {
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = new OpLocationPileEvseElasticDTO();
        opLocationPileEvseElasticDTO.setId(opLocationPileEvseEntity.getId());
        opLocationPileEvseElasticDTO.setCreatedAt(opLocationPileEvseEntity.getCreatedAt());
        opLocationPileEvseElasticDTO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
        opLocationPileEvseElasticDTO.setLocationId(opLocationPileEvseEntity.getLocationId());
        opLocationPileEvseElasticDTO.setEvseList(opLocationPileEvseEntity.getEvseList());
        opLocationPileEvseElasticDTO.setName(opLocationPileEvseEntity.getName());
        opLocationPileEvseElasticDTO.setBrandId(opLocationPileEvseEntity.getBrandId());
        opLocationPileEvseElasticDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
        opLocationPileEvseElasticDTO.setProductModel(opLocationPileEvseEntity.getProductModel());
        opLocationPileEvseElasticDTO.setVendor(opLocationPileEvseEntity.getVendor());
        opLocationPileEvseElasticDTO.setOperatorId(opLocationEntity.getOperatorId());
        opLocationPileEvseElasticDTO.setLocationName(opLocationEntity.getName());
        return opLocationPileEvseElasticDTO;
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseEntity
     * @return
     */
    public static OpLocationPileEvseElasticDTO toOpLocationPileEvseElastic(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO,
                                                                           OpLocationPileEvseEntity opLocationPileEvseEntity,
                                                                           OpLocationEvseEntity opLocationEvseEntity,
                                                                           OpLocationEntity opLocationEntity) {
        opLocationPileEvseElasticDTO.setCreatedAt(opLocationPileEvseEntity.getCreatedAt());
        opLocationPileEvseElasticDTO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
        opLocationPileEvseElasticDTO.setLocationId(opLocationPileEvseEntity.getLocationId());
        opLocationPileEvseElasticDTO.setEvseList(opLocationPileEvseEntity.getEvseList());
        opLocationPileEvseElasticDTO.setName(opLocationPileEvseEntity.getName());
        opLocationPileEvseElasticDTO.setBrandId(opLocationPileEvseEntity.getBrandId());
        opLocationPileEvseElasticDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
        opLocationPileEvseElasticDTO.setProductModel(opLocationPileEvseEntity.getProductModel());
        opLocationPileEvseElasticDTO.setVendor(opLocationPileEvseEntity.getVendor());
        opLocationPileEvseElasticDTO.setOperatorId(opLocationEntity.getOperatorId());
        opLocationPileEvseElasticDTO.setLocationName(opLocationEntity.getName());
        if (opLocationEvseEntity != null) {
            opLocationPileEvseElasticDTO.setTariffId(opLocationEvseEntity.getTariffId());
        }
        return opLocationPileEvseElasticDTO;
    }

    public static void toOpLocationPileEvseElasticDto(OpLocationPileEvseElasticDTO target, OpLocationPileEvseEntity pileSource, OpLocationEvseEntity evseSource, OpLocationEntity locationSource, OpLocationConnectorEntity connectorSource) {
        target.setId(pileSource.getId());
        target.setCreatedAt(pileSource.getCreatedAt());
        target.setUpdatedAt(pileSource.getUpdatedAt());
        target.setLocationId(pileSource.getLocationId());
        target.setLocationName(locationSource.getName());
        target.setOperatorId(locationSource.getOperatorId());
        target.setEvseList(pileSource.getEvseList());
        target.setName(pileSource.getName());
        target.setBrandId(pileSource.getBrandId());
        target.setPileSn(pileSource.getPileSn());
        target.setProductModel(pileSource.getProductModel());
        target.setVendor(pileSource.getVendor());
        target.setPower(connectorSource.getPower());
        target.setPowerType(connectorSource.getPowerType());
        target.setTariffId(evseSource.getTariffId());
        target.setStatus(pileSource.getStatus());
        target.setDefaultChargingTime(pileSource.getDefaultChargingTime());
        target.setPublicMark(pileSource.getPublicMark());
        target.setEroamingEnable(pileSource.getEroamingEnable());
    }

    /**
     * 转化
     *
     * @param item
     * @return
     */
    public static OpPileAssociatedRuleDTO toOpPileAssociatedRuleDTO(OpLocationPileEvseElasticDTO item) {
        String powerType = item.getPowerType();
        if (StringUtils.isNotBlank(powerType) && powerType.contains("_")) {
            powerType = powerType.split("_")[0];
        }
        return OpPileAssociatedRuleDTO.builder()
                .id(item.getId())
                .createdAt(item.getCreatedAt())
                .updatedAt(item.getUpdatedAt())
                .locationId(item.getLocationId())
                .pileSn(item.getPileSn())
                .name(item.getName())
                .brandId(item.getBrandId())
                .productModel(item.getProductModel())
                .vendor(item.getVendor())
                .powerType(powerType)
                .operatorId(item.getOperatorId())
                .locationName(item.getLocationName())
                .build();
    }

    /**
     * 转化
     *
     * @param opLocationPileEvseEntity
     * @return
     */
    public static OpPileEvseInfoVO toOpPileEvseInfoVO(OpLocationPileEvseEntity opLocationPileEvseEntity) {
        if (opLocationPileEvseEntity == null) {
            return null;
        }
        OpPileEvseInfoVO opPileEvseInfoVO = new OpPileEvseInfoVO();
        opPileEvseInfoVO.setPileSn(opLocationPileEvseEntity.getPileSn());
        opPileEvseInfoVO.setPileName(opLocationPileEvseEntity.getName());
        opPileEvseInfoVO.setLocationId(opLocationPileEvseEntity.getLocationId());
        opPileEvseInfoVO.setBrandId(opLocationPileEvseEntity.getBrandId());
        opPileEvseInfoVO.setVendor(opLocationPileEvseEntity.getVendor());
        opPileEvseInfoVO.setProductModel(opLocationPileEvseEntity.getProductModel());
        opPileEvseInfoVO.setPileId(opLocationPileEvseEntity.getId());
        opPileEvseInfoVO.setPublicMark(opLocationPileEvseEntity.getPublicMark());
        return opPileEvseInfoVO;

    }

    public static OpPileAssociatedRuleDTO toOpPileAssociatedRuleDTOV2(OpLocationPileEvseElasticDTO item, OpLocationEvseElasticDTO evseDTO) {
        String powerType = item.getPowerType();
        if (StringUtils.isNotBlank(powerType) && powerType.contains("_")) {
            powerType = powerType.split("_")[0];
        }
        String evseId = null;
        if (StringUtils.isNotBlank(evseDTO.getEvseSn()) && evseDTO.getEvseSn().contains("_")) {
            evseId = evseDTO.getEvseSn().split("_")[1];
        }
        return OpPileAssociatedRuleDTO.builder()
                .id(item.getId())
                .createdAt(item.getCreatedAt())
                .updatedAt(item.getUpdatedAt())
                .locationId(item.getLocationId())
                .pileSn(item.getPileSn())
                .name(item.getName())
                .brandId(item.getBrandId())
                .productModel(item.getProductModel())
                .vendor(item.getVendor())
                .powerType(powerType)
                .operatorId(item.getOperatorId())
                .locationName(item.getLocationName())
                .evseSn(evseDTO.getEvseSn())
                .connector(evseId)
                .gunType(evseDTO.getGunType())
                .build();
    }

    /**
     * @param opLocationPileEvseElasticDTO 充电设备组合（桩）表对应的ES实体
     * @return 英国法案认证：查询桩列表 出参模型
     * @function 类型转化方法 (OpLocationPileEvseElasticDTO ——> BritishActVO)
     */
    public static BritishActVO opLocationPileEvseElasticDTOToBritishActVO(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO) {
        BritishActVO britishActVO = new BritishActVO();
        if (opLocationPileEvseElasticDTO != null) {
            Long id = opLocationPileEvseElasticDTO.getId();
            String brandName = opLocationPileEvseElasticDTO.getBrandName();
            String productModel = opLocationPileEvseElasticDTO.getProductModel();
            String pileSn = opLocationPileEvseElasticDTO.getPileSn();
            String defaultChargingTime = opLocationPileEvseElasticDTO.getDefaultChargingTime();
            britishActVO.setId(id);
            britishActVO.setBrandName(brandName);
            britishActVO.setProductModel(productModel);
            britishActVO.setPileSn(pileSn);
            britishActVO.setRandomDelaySwitch(opLocationPileEvseElasticDTO.getRandomDelaySwitch());
            britishActVO.setRandomDelayTime(opLocationPileEvseElasticDTO.getRandomDelayTime());
            britishActVO.setBritainApproveSwitch(opLocationPileEvseElasticDTO.getBritainApproveSwitch());
            if (StrUtil.isNotBlank(defaultChargingTime)) {
                britishActVO.setDefaultChargingTimeVO(JSON.parseArray(defaultChargingTime, DefaultChargingTimeVO.class));
                if (ObjectUtils.isNotEmpty(britishActVO.getDefaultChargingTimeVO())) {
                    britishActVO.setAlertTimeout(britishActVO.getDefaultChargingTimeVO().get(0).getAlertTimeout());
                }
            }
        }
        return britishActVO;
    }
}
