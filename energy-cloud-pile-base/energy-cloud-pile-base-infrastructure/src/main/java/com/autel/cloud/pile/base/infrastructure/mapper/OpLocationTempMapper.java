package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationTempEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 场站表 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Mapper
@Repository
public interface OpLocationTempMapper extends BaseMapper<OpLocationTempEntity> {

}
