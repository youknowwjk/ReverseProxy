package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.dto.OpImageDTO;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpImageEntity;

public class OpImageConvert {

    private OpImageConvert() {

    }

    /**
     * 转换OpImageEntity
     *
     * @param opLocationDTO
     * @return
     */
    public static OpImageEntity opImageEntity(OpLocationDTO opLocationDTO, OpImageDTO opImage) {
        OpImageEntity opImageEntity = new OpImageEntity();
        opImageEntity.setUrl(opImage.getUrl());
        opImageEntity.setThumbnail(opImage.getThumbnail());
        opImageEntity.setCreatedAt(System.currentTimeMillis());
        opImageEntity.setUpdatedAt(System.currentTimeMillis());
        opImageEntity.setDeleted(opLocationDTO.getDeleted());
        opImageEntity.setStatus(opLocationDTO.getStatus());
        opImageEntity.setType(opImage.getType());
        opImageEntity.setWidth(opImage.getWidth());
        opImageEntity.setHeight(opImage.getHeight());
        opImageEntity.setId(opImage.getId());
        return opImageEntity;
    }
}
