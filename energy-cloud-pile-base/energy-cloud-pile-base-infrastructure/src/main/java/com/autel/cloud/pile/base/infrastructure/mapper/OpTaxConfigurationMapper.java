package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpTaxConfigurationEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置 Mapper 接口
 */
@Mapper
@Repository
public interface OpTaxConfigurationMapper extends BaseMapper<OpTaxConfigurationEntity> {
}