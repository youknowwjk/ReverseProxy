package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
* @author temp
* @description 针对表【op_location_meter(电表)】的数据库操作Mapper
* @createDate 2023-01-29 17:26:48
* @Entity generator.domain.LocationMeterEntity
*/
@Mapper
@Repository
public interface LocationMeterMapper extends BaseMapper<LocationMeterEntity> {
}




