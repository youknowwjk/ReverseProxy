package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocalAuthListEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@Mapper
public interface LocalAuthListMapper extends BaseMapper<LocalAuthListEntity> {
    boolean batchSave(@Param("insertList") List<LocalAuthListEntity> insertList);
}
