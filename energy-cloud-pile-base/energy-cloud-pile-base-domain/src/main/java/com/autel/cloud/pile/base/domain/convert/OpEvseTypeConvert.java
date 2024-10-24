package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseTypeEntity;
import com.autel.cloud.pile.base.vo.OpEvseTypeVO;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

public class OpEvseTypeConvert {

    private OpEvseTypeConvert() {

    }

    /**
     * opEvseTypeEntity
     *
     * @param opEvseTypeEntity
     * @return
     */
    public static OpEvseTypeVO opEvseTypeVO(OpEvseTypeEntity opEvseTypeEntity) {
        if (opEvseTypeEntity == null) {
            return null;
        }
        OpEvseTypeVO opEvseTypeVO = new OpEvseTypeVO();
        BeanUtils.copyProperties(opEvseTypeEntity, opEvseTypeVO);
        return opEvseTypeVO;
    }

    /**
     * 转换opLocationEntitys
     *
     * @param opEvseTypeEntityList
     * @return
     */
    public static List<OpEvseTypeVO> toEvseTypeVOs(List<OpEvseTypeEntity> opEvseTypeEntityList) {
        if (CollectionUtils.isEmpty(opEvseTypeEntityList)) {
            return null;
        }
        List<OpEvseTypeVO> opEvseTypeVOS = opEvseTypeEntityList.stream().map(opEvseTypeEntity -> opEvseTypeVO(opEvseTypeEntity))
                .collect(Collectors.toList());
        return opEvseTypeVOS;
    }

}
