package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpPowerLimitDTO;
import com.autel.cloud.pile.base.dto.OpPowerLimitSettingDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitEntity;

import java.util.List;

/**
 * @ClassName OpPowerLimitService
 * @Author A22121
 * @Description
 * @Date 2022/7/2 11:01
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpPowerLimitService {
    /**
     * 更新组合是否开启功率限制
     *
     * @param combinationId
     * @return
     */
    Result<Boolean> updateTurnOn(Long combinationId);

    /**
     * 查询组合是否开启功率限制
     *
     * @param combinationId
     * @return
     */
    Result<Boolean> getTurnOn(Long combinationId);

    /**
     * 更新设置方式
     *
     * @param opPowerLimitDTO
     * @return
     */
    Result<Boolean> updatePowerLimit(OpPowerLimitDTO opPowerLimitDTO);

    /**
     * 查询设置方式
     *
     * @param combinationId
     * @return
     */
    Result<OpPowerLimitDTO> queryPowerLimit(Long combinationId);


    /**
     * 添加分时设置功率
     *
     * @param opPowerLimitSettingDTOList
     * @return
     */
    Result<Boolean> addTimeSetting(List<OpPowerLimitSettingDTO> opPowerLimitSettingDTOList);

    /**
     * 更新展示分时设置功率
     *
     * @param combinationId
     * @return
     */
    Result<List<OpPowerLimitSettingDTO>> queryTimeSettingForUpdate(Long combinationId);

    /**
     * 展示分时设置功率
     *
     * @param combinationId
     * @return
     */
    Result<List<OpPowerLimitSettingDTO>> queryTimeSetting(Long combinationId);

    /**
     * 下发配置
     *
     * @param combinationId
     * @return
     */
    Result<Boolean> deliverySetting(Long combinationId);

    /**
     * 清除配置
     *
     * @param combinationId
     * @return
     */
    Result<Boolean> clearSetting(Long combinationId);

    /**
     * 查询所有开启功率下发桩
     *
     * @return
     */
    List<OpPowerLimitEntity> findAllTurnOn();
}
