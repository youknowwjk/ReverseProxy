package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseBrandEntity;
import com.autel.cloud.pile.base.vo.OpEvseBrandVO;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

public class OpEvseBrandConvert {

    private OpEvseBrandConvert() {

    }

    /**
     * @param entity
     * @return
     */
    public static OpEvseBrandVO toOpEvseBrandVO(OpEvseBrandEntity entity) {
        if (entity == null) {
            return null;
        }
        OpEvseBrandVO opEvseBrandVO = new OpEvseBrandVO();
        BeanUtils.copyProperties(entity, opEvseBrandVO);
        return opEvseBrandVO;
    }

    /**
     * 转换opLocationEntitys
     *
     * @param tbBrandEntityList
     * @return
     */
    public static List<OpEvseBrandVO> toBrandVOs(List<OpEvseBrandEntity> tbBrandEntityList) {
        if (CollectionUtils.isEmpty(tbBrandEntityList)) {
            return null;
        }
        List<OpEvseBrandVO> opEvseBrandVOS = tbBrandEntityList.stream().map(TbBrandEntity -> toOpEvseBrandVO(TbBrandEntity))
                .collect(Collectors.toList());
        return opEvseBrandVOS;
    }

}
