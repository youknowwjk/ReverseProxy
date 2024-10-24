package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.dto.OpLocationConnectorDTO;
import com.autel.cloud.pile.base.enums.ConnectorStatusEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;

import java.util.Optional;

public class OpLocationConnectorConvert {

    private OpLocationConnectorConvert() {

    }

    /**
     * @param opLocationConnectorDTO
     * @return
     */
    public static OpLocationConnectorEntity toOpLocationConnectorEntity(OpLocationConnectorDTO opLocationConnectorDTO) {
        if (opLocationConnectorDTO == null) {
            return null;
        }
        OpLocationConnectorEntity opLocationConnectorEntity = new OpLocationConnectorEntity();
        opLocationConnectorEntity.setCreatedAt(System.currentTimeMillis());
        opLocationConnectorEntity.setUpdatedAt(System.currentTimeMillis());
        opLocationConnectorEntity.setId(Optional.ofNullable(opLocationConnectorDTO.getId()).orElse(IdWorker.getId()));
        opLocationConnectorEntity.setDeleted(Optional.ofNullable(opLocationConnectorDTO.getDeleted()).orElse(0));
        opLocationConnectorEntity.setStatus(ConnectorStatusEnum.ENABLE.getCode());
        opLocationConnectorEntity.setLocationEvseId(opLocationConnectorDTO.getLocationEvseId());
        opLocationConnectorEntity.setStandard(opLocationConnectorDTO.getStandard());
        opLocationConnectorEntity.setFormat(opLocationConnectorDTO.getFormat());
        opLocationConnectorEntity.setPowerType(opLocationConnectorDTO.getPowerType());
        opLocationConnectorEntity.setVoltage(opLocationConnectorDTO.getVoltage());
        opLocationConnectorEntity.setAmperage(opLocationConnectorDTO.getAmperage());
        opLocationConnectorEntity.setTariffId(opLocationConnectorDTO.getTariffId());
        opLocationConnectorEntity.setTermsAndConditions(opLocationConnectorDTO.getTermsAndConditions());
        opLocationConnectorEntity.setConnectorId(opLocationConnectorDTO.getConnectorId());
        opLocationConnectorEntity.setImageId(opLocationConnectorDTO.getImageId());
        opLocationConnectorEntity.setGunType(opLocationConnectorDTO.getGunType());
        opLocationConnectorEntity.setPower(opLocationConnectorDTO.getPower());
        return opLocationConnectorEntity;
    }


}
