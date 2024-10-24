package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.autel.cloud.pile.base.vo.LocationMeterVO;
import org.mapstruct.Mapper;

@Mapper(componentModel = "spring")
public interface LocationMeterConvert {

    LocationMeterVO toVo(LocationMeterEntity source);
}
