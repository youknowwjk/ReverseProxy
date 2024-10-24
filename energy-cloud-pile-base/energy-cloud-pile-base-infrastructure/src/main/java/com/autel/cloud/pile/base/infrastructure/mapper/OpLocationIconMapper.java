package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationIconEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 图标信息 Mapper 接口
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
@Mapper
@Repository
public interface OpLocationIconMapper extends BaseMapper<OpLocationIconEntity> {
}
