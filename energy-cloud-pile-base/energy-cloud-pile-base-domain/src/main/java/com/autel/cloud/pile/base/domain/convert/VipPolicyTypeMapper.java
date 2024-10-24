package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.VipPolicy;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileVipConfigEntity;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;
@Mapper
public interface VipPolicyTypeMapper {


    VipPolicyTypeMapper INSTANCE = Mappers.getMapper(VipPolicyTypeMapper.class);


    VipPolicy entity2DTO(OpLocationPileVipConfigEntity entity);


    List<VipPolicy> entity2DTO(List<OpLocationPileVipConfigEntity> entity);

}
