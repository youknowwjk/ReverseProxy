package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.dto.ExplanatoryInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ExplanatoryInfoEntity;
import com.autel.cloud.pile.base.vo.ExplanatoryInfoVO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

@Mapper
public interface ExplanatoryInfoConverter {
    ExplanatoryInfoConverter explanatoryInfoConverter = Mappers.getMapper(ExplanatoryInfoConverter.class);

    ExplanatoryInfoEntity dto2Entity (ExplanatoryInfoDTO explanatoryInfoDTO);

    ExplanatoryInfoVO entity2VO (ExplanatoryInfoEntity explanatoryInfoEntity);
}
