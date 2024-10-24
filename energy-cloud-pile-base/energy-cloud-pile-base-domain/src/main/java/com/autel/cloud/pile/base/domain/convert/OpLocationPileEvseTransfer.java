package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.OpLocationPileEvseVO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper
public interface OpLocationPileEvseTransfer {

    OpLocationPileEvseTransfer INSTANCE = Mappers.getMapper(OpLocationPileEvseTransfer.class);


    OpLocationPileEvseVO entity2VO(OpLocationPileEvseEntity billEntity);


    List<OpLocationPileEvseVO> entityList2VOList(List<OpLocationPileEvseEntity> source);


}
