package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpPileV2gParamsSettingDTO;

/**
 * @ClassName OpPileV2gParamsSettingService
 * @Author A22121
 * @Description
 * @Date 2022/10/17 11:06
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpPileV2gParamsSettingService {
    /**
     * 添加配置
     * @param opPileV2gParamsSettingDTO 配置
     * @return
     */
    Result<Boolean> addSetting(OpPileV2gParamsSettingDTO opPileV2gParamsSettingDTO);
    /**
     * 删除配置
     * @param pileSn 桩sn
     * @return
     */
    Result<Boolean> deletedSetting(String pileSn);
    /**
     * 获取配置
     * @param pileSn 桩sn
     * @return
     */
    Result<OpPileV2gParamsSettingDTO> getSetting(String pileSn);

    /**
     * 切换充放电
     * @param pileSn
     * @return
     */
    Result<Boolean> convertMode(String pileSn);

    /**
     * 获取桩充放电模式
     * @param pileSn
     * @return
     */
    Result<Integer> getChargeMode(String pileSn);

    /**
     * 更新桩充放电模式
     * @param pileSn
     * @param chargeFlag
     * @return
     */
    Result<Boolean> setChargeMode(String pileSn, Integer chargeFlag);
}
