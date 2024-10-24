package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileVipConfigEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.Collection;
import java.util.List;

@Mapper
public interface OpLocationPileVipConfigMapper extends BaseMapper<OpLocationPileVipConfigEntity> {

    int insertOrUpdate(OpLocationPileVipConfigEntity entity);


    int batchAddOrUpdate(@Param("list") Collection<OpLocationPileVipConfigEntity> list);

    List<OpLocationPileVipConfigEntity> findUse();
}