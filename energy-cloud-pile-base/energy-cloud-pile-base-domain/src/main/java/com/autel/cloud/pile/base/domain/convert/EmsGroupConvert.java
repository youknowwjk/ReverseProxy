package com.autel.cloud.pile.base.domain.convert;


import com.autel.cloud.pile.base.vo.PileGroupDetailV3VOcopy;
import com.autel.cloud.pile.base.vo.PileGroupForEmsVOcopy;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
@Mapper
public interface EmsGroupConvert {
    EmsGroupConvert emsGroupConvert = Mappers.getMapper(EmsGroupConvert.class);

    PileGroupForEmsVOcopy dto2Entity (PileGroupDetailV3VOcopy source);

}
