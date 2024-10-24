package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;

import java.util.Map;

/**
 * @ClassName MonitorFeignClient
 * @Author A22121
 * @Description
 * @Date 2022/6/16 20:43
 * @Version 0.0.1-SNAPSHOT
 */
public interface MonitorFeign {

    /**
     * 获取充电设备状态
     *
     * @param evseId
     * @return
     */
    EvseDeviceStatusEnum getEvseStatus(Long evseId);

    /**
     * 获取充电设备状态
     *
     * @param evseSn
     * @return
     */
    EvseDeviceStatusEnum getEvseStatus(String evseSn);

    /**
     * 获取充电设备实时数据
     *
     * @param evseId
     * @return
     */
    Map<String, String> getEvseMonitorInfo(Long evseId);

    /**
     * 获取充电设备soc
     *
     * @param evseId
     * @return
     */
    Integer getEvseMonitorSoc(Long evseId);

}
