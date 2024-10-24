package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseBrandEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Mapper
@Repository
public interface OpEvseBrandMapper extends BaseMapper<OpEvseBrandEntity> {

    Boolean deleteById(Long id);

    List<String> getModelListByBrandName(@Param("brandName")String brandName);
}
