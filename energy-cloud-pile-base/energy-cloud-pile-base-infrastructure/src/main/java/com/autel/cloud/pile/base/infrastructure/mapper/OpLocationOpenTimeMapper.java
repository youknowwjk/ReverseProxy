package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOpenTimeEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 场站设施 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Mapper
@Repository
public interface OpLocationOpenTimeMapper extends BaseMapper<OpLocationOpenTimeEntity> {
    /**
     *  根据locationId删除
     * @param locationId
     */
    void deleteByLocationId(Long locationId);

}
