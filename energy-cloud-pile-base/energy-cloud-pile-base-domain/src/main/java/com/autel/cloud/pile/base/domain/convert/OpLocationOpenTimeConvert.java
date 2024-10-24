package com.autel.cloud.pile.base.domain.convert;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.dto.OpLocationOpenTimeDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOpenTimeEntity;

public class OpLocationOpenTimeConvert {

    private OpLocationOpenTimeConvert() {

    }

    /**
     * 转换opLocationOpenTimeConvert
     *
     * @param opLocationEntity
     * @return
     */
    public static OpLocationOpenTimeEntity opLocationOpenTimeConvert(OpLocationEntity opLocationEntity, OpLocationOpenTimeDTO opLocationOpenTime) {
        OpLocationOpenTimeEntity opLocationOpenTimeEntity = new OpLocationOpenTimeEntity();
        opLocationOpenTimeEntity.setPeriodBegin(opLocationOpenTime.getPeriodBegin());
        opLocationOpenTimeEntity.setPeriodEnd(opLocationOpenTime.getPeriodEnd());
        opLocationOpenTimeEntity.setType(opLocationOpenTime.getType());
        opLocationOpenTimeEntity.setCreatedAt(System.currentTimeMillis());
        opLocationOpenTimeEntity.setUpdatedAt(System.currentTimeMillis());
        opLocationOpenTimeEntity.setDeleted(opLocationEntity.getDeleted());
        opLocationOpenTimeEntity.setStatus(opLocationEntity.getStatus());
        opLocationOpenTimeEntity.setDateValue(JSON.toJSONString(opLocationOpenTime.getDateValue()));
        opLocationOpenTimeEntity.setDateType(opLocationOpenTime.getDateType());
        opLocationOpenTimeEntity.setLocationId(opLocationEntity.getId());
        return opLocationOpenTimeEntity;
    }
}
