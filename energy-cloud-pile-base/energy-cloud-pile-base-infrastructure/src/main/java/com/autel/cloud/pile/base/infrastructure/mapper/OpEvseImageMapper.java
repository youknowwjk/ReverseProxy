package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseImageEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 充电设备图片 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Mapper
@Repository
public interface OpEvseImageMapper extends BaseMapper<OpEvseImageEntity> {

    /**
     *  根据locationEvseId删除充电设备图片（逻辑删除）
     * @param id
     * @return
     */
    Boolean deleteByLocationEvseId(Long id);

    /**
     *  更新记录
     * @param opEvseImageEntity
     * @return
     */
    Boolean updateByLocationEvseId(OpEvseImageEntity opEvseImageEntity);
}
