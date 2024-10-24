package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLimitFreeEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 限时免费日期
 * </p>
 *
 * @author A22587
 * @since 2022-08-19
 */
@Mapper
@Repository
public interface OpLimitFreeMapper extends BaseMapper<OpLimitFreeEntity> {

    void deleteLimitFreeByIds(@Param("ids") List<Long> ids);
}
