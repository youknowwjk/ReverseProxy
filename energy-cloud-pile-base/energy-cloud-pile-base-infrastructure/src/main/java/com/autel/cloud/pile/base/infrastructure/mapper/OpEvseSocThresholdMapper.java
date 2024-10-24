package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseSocThresholdEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * SOC阀值设置 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Mapper
@Repository
public interface OpEvseSocThresholdMapper extends BaseMapper<OpEvseSocThresholdEntity> {

    /**
     *  根据evseID删除SOC阀值设置表记录（逻辑删除）
     * @param id
     * @return
     */
    Boolean deleteByEvseId(Long id);

    /**
     *  更新记录
     * @param opEvseSocThresholdEntity
     * @return
     */
    Boolean updateByEvseId(OpEvseSocThresholdEntity opEvseSocThresholdEntity);
}
