package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.vo.GroupPileEvseV3DTO;
import com.autel.cloud.pile.base.vo.GroupPileV3DTO;
import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.MappingTarget;
import org.mapstruct.factory.Mappers;

@Mapper
public interface GroupPileEvseTypeMapper {


    GroupPileEvseTypeMapper INSTANCE = Mappers.getMapper(GroupPileEvseTypeMapper.class);


    GroupPileEvseV3DTO dto2DTO(OpLocationEvseElasticDTO entity);

    GroupPileV3DTO dto2DTO(OpLocationPileEvseElasticDTO entity);

    @AfterMapping
    default void afterMapping(@MappingTarget GroupPileV3DTO target, OpLocationPileEvseElasticDTO source) {
        target.setPileId(source.getId());
    }
}
