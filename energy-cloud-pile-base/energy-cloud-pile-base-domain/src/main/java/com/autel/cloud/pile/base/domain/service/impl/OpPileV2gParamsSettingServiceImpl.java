package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.StaticFieldConstant;
import com.autel.cloud.pile.base.domain.repository.OpPileV2gParamsSettingRepository;
import com.autel.cloud.pile.base.domain.service.OpPileV2gParamsSettingService;
import com.autel.cloud.pile.base.dto.OpPileV2gParamsSettingDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.feign.WxProxyClient;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileV2gParamsSettingEntity;
import com.autel.cloud.pile.bill.dto.SendMsgDto;
import com.autel.cloud.smart.charge.enums.OcppAction;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * @ClassName OpPileV2gParamsSettingServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/10/17 11:06
 * @Version 0.0.1-SNAPSHOT
 */

@Service
@Slf4j
public class OpPileV2gParamsSettingServiceImpl implements OpPileV2gParamsSettingService {

    private final WxProxyClient wxProxyClient;

    private final OpPileV2gParamsSettingRepository opPileV2gParamsSettingRepository;

    public OpPileV2gParamsSettingServiceImpl(OpPileV2gParamsSettingRepository opPileV2gParamsSettingRepository,
                                             WxProxyClient wxProxyClient) {
        this.opPileV2gParamsSettingRepository = opPileV2gParamsSettingRepository;
        this.wxProxyClient = wxProxyClient;
    }

    private OpPileV2gParamsSettingEntity getEntityByPileSn(String pileSn) {
        LambdaQueryWrapper<OpPileV2gParamsSettingEntity> query = Wrappers.lambdaQuery(OpPileV2gParamsSettingEntity.class)
                .eq(OpPileV2gParamsSettingEntity::getPileSn, pileSn)
                .eq(OpPileV2gParamsSettingEntity::getDeleted, Boolean.FALSE);
        return opPileV2gParamsSettingRepository.getOne(query, Boolean.FALSE);
    }

    private OpPileV2gParamsSettingEntity convertToEntity(OpPileV2gParamsSettingDTO opPileV2gParamsSettingDTO) {
        OpPileV2gParamsSettingEntity settingEntity = new OpPileV2gParamsSettingEntity();
        BeanUtils.copyProperties(opPileV2gParamsSettingDTO, settingEntity);
        settingEntity.setCreatedAt(System.currentTimeMillis());
        settingEntity.setUpdatedAt(System.currentTimeMillis());
        return settingEntity;
    }

    private OpPileV2gParamsSettingEntity convertToEntity(OpPileV2gParamsSettingEntity oldSettingEntity,
                                                         OpPileV2gParamsSettingDTO opPileV2gParamsSettingDTO) {
        OpPileV2gParamsSettingEntity settingEntity = new OpPileV2gParamsSettingEntity();
        BeanUtils.copyProperties(oldSettingEntity, settingEntity);
        if (opPileV2gParamsSettingDTO == null) {
            return settingEntity;
        }
        if (opPileV2gParamsSettingDTO.getChargeFlag() != null) {
            settingEntity.setChargeFlag(opPileV2gParamsSettingDTO.getChargeFlag());
        }
        if (opPileV2gParamsSettingDTO.getImportMaxSoc() != null) {
            settingEntity.setImportMaxSoc(opPileV2gParamsSettingDTO.getImportMaxSoc());
        }
        if (opPileV2gParamsSettingDTO.getImportMinSoc() != null) {
            settingEntity.setImportMinSoc(opPileV2gParamsSettingDTO.getImportMinSoc());
        }
        if (opPileV2gParamsSettingDTO.getExportMaxSoc() != null) {
            settingEntity.setExportMaxSoc(opPileV2gParamsSettingDTO.getExportMaxSoc());
        }
        if (opPileV2gParamsSettingDTO.getExportMinSoc() != null) {
            settingEntity.setExportMinSoc(opPileV2gParamsSettingDTO.getExportMinSoc());
        }
        if (opPileV2gParamsSettingDTO.getExportLimitPower() != null) {
            settingEntity.setExportLimitPower(opPileV2gParamsSettingDTO.getExportLimitPower());
        }
        if (opPileV2gParamsSettingDTO.getImportLimitPower() != null) {
            settingEntity.setImportLimitPower(opPileV2gParamsSettingDTO.getImportLimitPower());
        }
        if (opPileV2gParamsSettingDTO.getStopVoltage() != null) {
            settingEntity.setStopVoltage(opPileV2gParamsSettingDTO.getStopVoltage());
        }
        if (opPileV2gParamsSettingDTO.getButtonMode() != null) {
            settingEntity.setButtonMode(opPileV2gParamsSettingDTO.getButtonMode());
        }
        settingEntity.setUpdatedAt(System.currentTimeMillis());
        return settingEntity;
    }

    private OpPileV2gParamsSettingDTO convertToDTO(OpPileV2gParamsSettingEntity opPileV2gParamsSettingEntity) {
        OpPileV2gParamsSettingDTO settingDTO = new OpPileV2gParamsSettingDTO();
        BeanUtils.copyProperties(opPileV2gParamsSettingEntity, settingDTO);
        return settingDTO;
    }

    @Override
    public Result<Boolean> addSetting(OpPileV2gParamsSettingDTO opPileV2gParamsSettingDTO) {
        log.info("OpPileV2gParamsSettingServiceImpl.addSetting and opPileV2gParamsSettingDTO = "
                + JSON.toJSONString(opPileV2gParamsSettingDTO));
        if (opPileV2gParamsSettingDTO == null || StringUtils.isBlank(opPileV2gParamsSettingDTO.getPileSn())) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        OpPileV2gParamsSettingEntity oldSettingEntity = getEntityByPileSn(opPileV2gParamsSettingDTO.getPileSn());
        OpPileV2gParamsSettingEntity entity;
        if (oldSettingEntity == null) {
            entity = convertToEntity(opPileV2gParamsSettingDTO);
        } else {
            entity = convertToEntity(oldSettingEntity, opPileV2gParamsSettingDTO);
        }
        log.info("OpPileV2gParamsSettingServiceImpl.addSetting and entity = " + JSON.toJSONString(entity));
        chargeChargeMode(entity);
        return Result.ofSucceed(opPileV2gParamsSettingRepository.saveOrUpdate(entity));
    }

    @Override
    public Result<Boolean> deletedSetting(String pileSn) {
        log.info("OpPileV2gParamsSettingServiceImpl.deletedSetting and pileSn = " + pileSn);
        OpPileV2gParamsSettingEntity oldSettingEntity = getEntityByPileSn(pileSn);
        if (oldSettingEntity == null) {
            return Result.ofSucceed(Boolean.FALSE);
        }
        oldSettingEntity.setUpdatedAt(System.currentTimeMillis());
        return Result.ofSucceed(opPileV2gParamsSettingRepository.removeById(oldSettingEntity.getId()));
    }

    @Override
    public Result<OpPileV2gParamsSettingDTO> getSetting(String pileSn) {
        if (StringUtils.isBlank(pileSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        log.info("OpPileV2gParamsSettingServiceImpl.getSetting and language = " + LocaleContextHolder.getLocale());
        OpPileV2gParamsSettingEntity oldSettingEntity = getEntityByPileSn(pileSn);
        if(oldSettingEntity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        OpPileV2gParamsSettingDTO settingDTO = convertToDTO(oldSettingEntity);
        return Result.ofSucceed(settingDTO);
    }

    @Override
    public Result<Boolean> convertMode(String pileSn) {
        if (StringUtils.isBlank(pileSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        log.info("OpPileV2gParamsSettingServiceImpl.convertMode and pileSn = " + pileSn);
        OpPileV2gParamsSettingEntity oldSettingEntity = buildOldEntity(pileSn);
        boolean ret = chargeChargeMode(oldSettingEntity);
        if (ret) {
            return Result.ofSucceed(opPileV2gParamsSettingRepository.saveOrUpdate(oldSettingEntity));
        }
        return Result.ofSucceed(Boolean.FALSE);
    }

    @Override
    public Result<Integer> getChargeMode(String pileSn) {
        if (StringUtils.isBlank(pileSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        try {
            OpPileV2gParamsSettingEntity oldSettingEntity = getEntityByPileSn(pileSn);
            if (oldSettingEntity == null) {
                return Result.ofSucceed(0);
            }
            return Result.ofSucceed(oldSettingEntity.getChargeFlag());
        } catch (Exception e) {
            return Result.ofSucceed(0);
        }
    }

    @Override
    public Result<Boolean> setChargeMode(String pileSn, Integer chargeFlag) {
        if (StringUtils.isBlank(pileSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        if (chargeFlag == null || chargeFlag > 1 || chargeFlag < 0) {
            chargeFlag = 0;
        }
        log.info("OpPileV2gParamsSettingServiceImpl.setChargeMode and pileSn = " + pileSn + " and chargeFlag = " + chargeFlag);
        OpPileV2gParamsSettingEntity oldSettingEntity = buildOldEntity(pileSn);
        oldSettingEntity.setChargeFlag(chargeFlag);
        return Result.ofSucceed(opPileV2gParamsSettingRepository.saveOrUpdate(oldSettingEntity));
    }

    private OpPileV2gParamsSettingEntity buildOldEntity(String pileSn) {
        OpPileV2gParamsSettingEntity oldSettingEntity = getEntityByPileSn(pileSn);
        if (oldSettingEntity == null) {
            oldSettingEntity = new OpPileV2gParamsSettingEntity();
            oldSettingEntity.setPileSn(pileSn);
            oldSettingEntity.setChargeFlag(1);
            oldSettingEntity.setCreatedAt(System.currentTimeMillis());
            oldSettingEntity.setUpdatedAt(System.currentTimeMillis());
        } else {
            oldSettingEntity.setChargeFlag(oldSettingEntity.getChargeFlag() == 0 ? 1 : 0);
        }
        log.info("OpPileV2gParamsSettingServiceImpl.buildOldEntity and oldSettingEntity = " + JSON.toJSONString(oldSettingEntity));
        return oldSettingEntity;
    }

    private boolean chargeChargeMode(OpPileV2gParamsSettingEntity settingEntity) {
        try {
            SendMsgDto sendMsgDto = SendMsgDto.builder().msg(buildMessage(buildCommand(settingEntity), settingEntity.getPileSn()))
                    .receiver("test-device-" + settingEntity.getPileSn()).build();
            log.info("OpPileV2gParamsSettingServiceImpl.chargeChargeMode sendMsgDto = " + JSON.toJSONString(sendMsgDto));
            Result<Boolean> sendRet = wxProxyClient.sendMsg(sendMsgDto);
            log.info("OpPileV2gParamsSettingServiceImpl.chargeChargeMode sendRet = " + JSON.toJSONString(sendRet));
            if (sendRet != null) {
                return sendRet.getData();
            }
        } catch (Exception e) {
            log.error("OpPileV2gParamsSettingServiceImpl.chargeChargeMode exception = ", e);
        }
        return Boolean.FALSE;
    }

    private String buildCommand(OpPileV2gParamsSettingEntity settingEntity) {
        Imap command = Imap.instance().putValue("vendorId", "Autel").putValue("autel", Imap.instance()
                .putValue("cmd", "V2G_CHARGE_CTRL").putValue("seq", 3200).putValue("data", Imap.instance()
                        .putValue("chargeFlag", settingEntity.getChargeFlag()).getMap()
                ).getMap()
        );
        return JSON.toJSONString(command.getMap());
    }


    @Data
    static class Imap {
        private Map<String, Object> map;

        public static Imap instance() {
            return new Imap();
        }

        public Imap putValue(String key, Object value) {
            if (map == null) {
                map = new HashMap<>();
            }
            if (Objects.nonNull(value)) {
                map.put(key, value);
            }
            return this;
        }
    }

    /**
     * 构建信息字符串
     *
     * @param data
     * @return
     */
    private String buildMessage(String data, String pileSn) {
        return String.format(StaticFieldConstant.CALL_FORMAT, pileSn + System.currentTimeMillis(), OcppAction.DATA_TRANSFER.getValue(), data);
    }
}
