package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.dto.OpPileShareWhiteListDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileShareWhiteListEntity;
import com.autel.cloud.pile.base.vo.OpPileShareWhiteListPageVO;
import org.springframework.beans.BeanUtils;

public class OpPileShareWhiteListConvert {

    private OpPileShareWhiteListConvert() {

    }

    /**
     * 转换PileShareWhiteListDTO
     *
     * @param opPileShareWhiteListDTO
     */
    public static OpPileShareWhiteListEntity toPileShareWhiteEntity(OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        OpPileShareWhiteListEntity opPileShareWhiteListEntity = new OpPileShareWhiteListEntity();
        opPileShareWhiteListEntity.setId(opPileShareWhiteListDTO.getId());
        opPileShareWhiteListEntity.setCreatedAt(System.currentTimeMillis());
        opPileShareWhiteListEntity.setUpdatedAt(System.currentTimeMillis());
        opPileShareWhiteListEntity.setPileSn(opPileShareWhiteListDTO.getPileSn());
        return opPileShareWhiteListEntity;
    }

    /**
     * 转换
     *
     * @param opPileShareWhiteListEntity
     * @return
     */
    public static OpPileShareWhiteListPageVO toOpPileShareWhiteListPageVO(OpPileShareWhiteListEntity opPileShareWhiteListEntity) {
        OpPileShareWhiteListPageVO opPileShareWhiteListPageVO = new OpPileShareWhiteListPageVO();
        BeanUtils.copyProperties(opPileShareWhiteListEntity, opPileShareWhiteListPageVO);
        return opPileShareWhiteListPageVO;
    }
}
