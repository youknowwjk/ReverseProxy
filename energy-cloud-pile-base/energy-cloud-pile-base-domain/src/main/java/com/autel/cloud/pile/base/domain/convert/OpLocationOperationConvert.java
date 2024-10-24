package com.autel.cloud.pile.base.domain.convert;


import cn.hutool.core.bean.BeanUtil;
import com.alibaba.excel.util.StringUtils;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOperationEntity;

public class OpLocationOperationConvert {

    private OpLocationOperationConvert() {

    }

    /**
     * 转换opLocationOperationEntity
     *
     * @param opLocationDTO
     * @return
     */
    public static OpLocationOperationEntity opLocationOperationEntity(OpLocationDTO opLocationDTO) {
        OpLocationOperationEntity opLocationOperationEntity = new OpLocationOperationEntity();
        BeanUtil.copyProperties(opLocationDTO, opLocationOperationEntity);
        if (!StringUtils.isBlank(opLocationDTO.getExtensionNumber())) {
            opLocationOperationEntity.setServiceTel(opLocationDTO.getServiceTel() + "-" + opLocationDTO.getExtensionNumber());
        }
        Boolean appShow = opLocationDTO.getAppShow();
        if (appShow == null) {
            opLocationOperationEntity.setAppShow(false);
        }
        return opLocationOperationEntity;
    }

    public static OpLocationDTO opLocationDTO(OpLocationOperationEntity opLocationOperationEntity) {
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        opLocationDTO.setAppShow(opLocationOperationEntity.getAppShow());
        opLocationDTO.setOperationType(opLocationOperationEntity.getOperationType());
        opLocationDTO.setServiceTel(opLocationOperationEntity.getServiceTel());
        opLocationDTO.setAnnouncement(opLocationOperationEntity.getAnnouncement());
        opLocationDTO.setOpenType(opLocationOperationEntity.getOpenType());
        opLocationDTO.setTransPower(opLocationOperationEntity.getTransPower());
        opLocationDTO.setBillingRule(opLocationOperationEntity.getBillingRule());
        opLocationDTO.setState(opLocationOperationEntity.getState());
        opLocationDTO.setOperationDate(opLocationOperationEntity.getOperationDate());
        return opLocationDTO;
    }
}
