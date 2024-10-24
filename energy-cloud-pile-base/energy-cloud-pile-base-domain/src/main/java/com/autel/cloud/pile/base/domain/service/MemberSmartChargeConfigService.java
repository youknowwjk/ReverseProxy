package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.DeliveryByUserDTO;
import com.autel.cloud.pile.base.dto.SmartChargeConfigQueryDTO;
import com.autel.cloud.pile.base.dto.SmartChargeProfileDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.MemberSmartChargeConfigEntity;
import com.autel.cloud.pile.bill.dto.ChargingScheduleTaskDTO;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public interface MemberSmartChargeConfigService {

    MemberSmartChargeConfigEntity createDefault(SmartChargeConfigQueryDTO smartChargeConfigQueryDTO);

    MemberSmartChargeConfigEntity updateById(SmartChargeProfileDTO profile);

    MemberSmartChargeConfigEntity selectById(long id);

    @Transactional
    List<DeliveryByUserDTO> batchByUserIdAndLocationId(List<SmartChargeConfigQueryDTO> smartChargeConfigQueryDTO);

    @Transactional
    MemberSmartChargeConfigEntity queryByUserIdAndLocationId(SmartChargeConfigQueryDTO smartChargeConfigQueryDTO);

    @Transactional
    MemberSmartChargeConfigEntity genConfigByUserIdAndLocationId(ChargingScheduleTaskDTO chargingScheduleTaskDTO);

//    /**
//     * 根据 用户 场站 生成 该用户在该场站的 智能充电配置
//     *
//     * @param smartChargeConfigQueryDTO 用户 场站 桩 事务ID 信息
//     * @return 配置
//     */
//    MemberSmartChargeConfigEntity genConfigByUserIdAndLocationId(SmartChargeConfigQueryDTO smartChargeConfigQueryDTO);

    List<MemberSmartChargeConfigEntity> getUserVehicleProfile(long userId);

    int queryEnabledSmartChargeConfigByUserId(Long userId);

    int disabledSmartChargeConfigByUserId(Long userId);

    int update(MemberSmartChargeConfigEntity intelligentChargeConfig);
}
