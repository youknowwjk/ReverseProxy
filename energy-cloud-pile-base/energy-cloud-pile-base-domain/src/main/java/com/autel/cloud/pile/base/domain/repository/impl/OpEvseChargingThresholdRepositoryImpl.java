package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseChargingThresholdMapper;
import com.autel.cloud.pile.base.domain.repository.OpEvseChargingThresholdRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseChargingThresholdEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 充电设备充电阀值预警设置 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Service
public class OpEvseChargingThresholdRepositoryImpl extends ServiceImpl<OpEvseChargingThresholdMapper, OpEvseChargingThresholdEntity> implements OpEvseChargingThresholdRepository {

}
