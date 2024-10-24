package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpEvseChargingThresholdDTO;
import com.autel.cloud.pile.base.dto.OpEvseSocThresholdDTO;
import com.autel.cloud.pile.base.vo.OpEvseEnableVO;

import java.util.List;

/**
 * @ClassName OpLocationEvseOperationService
 * @Author A22121
 * @Description
 * @Date 2022/4/27 16:27
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationEvseOperationService {
    /**
     * 版本更新
     * @param evseSn
     * @return
     */
    Result<Boolean> updateVersion(String evseSn);

    /**
     * 批量版本更新
     * @param evseSnList
     * @return
     */
    Result<Boolean> batchUpdateVersion(List<String> evseSnList);

    /**
     * 远程停止充电
     * @param evseSn
     * @return
     */
    Result<Boolean> backendStop(String evseSn);

    /**
     * 禁用充电
     * @param evseSn
     * @return
     */
    Result<Boolean> disable(String evseSn);

    /**
     * 解禁充电
     * @param evseSn
     * @return
     */
    Result<Boolean> able(String evseSn);

    /**
     * 枪重启
     * @param evseSn
     * @return
     */
    Result<Boolean> reset(String evseSn);

    /**
     * 枪解锁
     * @param evseSn
     * @return
     */
    Result<Boolean> unlock(String evseSn);

    /**
     * 获取告警阈值设置
     * @param evseSn
     * @return
     */
    Result<OpEvseChargingThresholdDTO> getChargingThreshold(String evseSn);

    /**
     * 设置告警阈值
     * @param opEvseChargingThresholdDTO
     * @return
     */
    Result<Boolean> setChargingThreshold(OpEvseChargingThresholdDTO opEvseChargingThresholdDTO);

    /**
     * 获取SOC设置
     * @param evseSn
     * @return
     */
    Result<OpEvseSocThresholdDTO> getSocThreshold(String evseSn);

    /**
     * 设置SOC
     * @param opEvseSocThresholdDTO
     * @return
     */
    Result<Boolean> setSocThreshold(OpEvseSocThresholdDTO opEvseSocThresholdDTO);

    /**
     * 获取启用禁用列表
     * @param pileSn
     * @return
     */
    Result<List<OpEvseEnableVO>> getStatusList(String pileSn);
}
