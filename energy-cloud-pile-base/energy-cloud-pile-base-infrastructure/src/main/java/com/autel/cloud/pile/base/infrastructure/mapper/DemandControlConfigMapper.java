package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.DemandControlConfigEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
* @author wang
* @description 针对表【demand_control_config(需求费控制器表)】的数据库操作Mapper
* @createDate 2024-04-30 10:16:41
* @Entity generator.domain.DemandControlConfig
*/
public interface DemandControlConfigMapper extends BaseMapper<DemandControlConfigEntity> {

    String findContentByGroupId(@Param("groupId") Long groupId);
}




