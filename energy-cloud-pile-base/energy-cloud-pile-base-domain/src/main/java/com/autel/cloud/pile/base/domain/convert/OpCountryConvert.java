package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCountryEntity;
import com.autel.cloud.pile.base.vo.OpCountryVO;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

public class OpCountryConvert {

    private OpCountryConvert() {

    }

    /**
     * 转换opLocationEntity
     *
     * @param opCountryEntity
     * @return
     */
    public static OpCountryVO toOpCountryVO(OpCountryEntity opCountryEntity) {
        if (opCountryEntity == null) {
            return null;
        }
        OpCountryVO opCountryVO = new OpCountryVO();
        BeanUtils.copyProperties(opCountryEntity, opCountryVO);
        return opCountryVO;
    }

    /**
     * 转换opLocationEntity
     *
     * @param opCountryEntity
     * @return
     */
    public static OpCountryVO toOpCountryCodeVO(OpCountryEntity opCountryEntity) {
        if (opCountryEntity == null) {
            return null;
        }
        OpCountryVO opCountryVO = new OpCountryVO();
        opCountryVO.setId(opCountryEntity.getId());
        opCountryVO.setCode(opCountryEntity.getCode());
        opCountryVO.setAlpha2Code(opCountryEntity.getAlpha2Code());
        return opCountryVO;
    }

    /**
     * 转换opLocationEntitys
     *
     * @param opCountryEntityList
     * @return
     */
    public static List<OpCountryVO> toOpCountryVOs(List<OpCountryEntity> opCountryEntityList) {
        if (CollectionUtils.isEmpty(opCountryEntityList)) {
            return null;
        }
        return opCountryEntityList.stream().map(OpCountryConvert::toOpCountryVO)
                .collect(Collectors.toList());
    }


    /**
     * 转换opLocationEntitys
     *
     * @param opCountryEntityList
     * @return
     */
    public static List<OpCountryVO> toOpCountryCodeVOs(List<OpCountryEntity> opCountryEntityList) {
        if (CollectionUtils.isEmpty(opCountryEntityList)) {
            return null;
        }
        return opCountryEntityList.stream().map(OpCountryConvert::toOpCountryCodeVO)
                .collect(Collectors.toList());
    }

}
