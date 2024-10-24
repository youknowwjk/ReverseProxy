package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseChargingThresholdEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * 充电设备充电阀值预警设置 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Mapper
@Repository
public interface OpEvseChargingThresholdMapper extends BaseMapper<OpEvseChargingThresholdEntity> {

    /**
     *  根据evseId删除充电阈值预警表中的记录（逻辑删除）
     * @param id
     * @return
     */
    Boolean deleteByEvseId(Long id);

    /**
     *  更新记录
     * @param opEvseChargingThresholdEntity
     * @return
     */
    Boolean updateByEvseId(OpEvseChargingThresholdEntity opEvseChargingThresholdEntity);
}
