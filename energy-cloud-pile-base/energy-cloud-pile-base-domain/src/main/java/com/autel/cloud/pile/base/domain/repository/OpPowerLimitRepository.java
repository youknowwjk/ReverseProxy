package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 充电设备组合功率限制开启表 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-07-02
 */
public interface OpPowerLimitRepository extends IService<OpPowerLimitEntity> {

    List<OpPowerLimitEntity> findAllTurnOn();
}
