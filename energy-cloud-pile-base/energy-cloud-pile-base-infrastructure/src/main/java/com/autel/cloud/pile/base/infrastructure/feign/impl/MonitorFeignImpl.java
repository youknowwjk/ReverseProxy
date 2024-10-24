package com.autel.cloud.pile.base.infrastructure.feign.impl;

import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.MonitorFeign;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * @ClassName OpLocationEvseRedis
 * @Author A22121
 * @Description
 * @Date 2022/4/19 10:01
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
@Slf4j
public class MonitorFeignImpl implements MonitorFeign {


    @Autowired
    private MonitorFeignClient monitorFeignClient;

    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;

    /**
     * 获取充电设备状态
     *
     * @param evseId
     * @return
     */

    @Override
    public EvseDeviceStatusEnum getEvseStatus(Long evseId) {
        try {
            Optional<OpLocationEvseElasticDTO> optional = opLocationEvseElastic.findById(evseId);
            if (!optional.isPresent()) {
                return EvseDeviceStatusEnum.DEFAULT;
            }
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = optional.get();
            Result<String> ret = monitorFeignClient.queryStatusByEvseSn(opLocationEvseElasticDTO.getEvseSn());
            if (StringUtils.isNotBlank(ret.getData())) {
                return EvseDeviceStatusEnum.getEnumByName(ret.getData());
            }
            // 默认状态
        } catch (Exception e) {
            log.info("MonitorFeignImpl getEvseStatus exception = ", e);
        }
        return EvseDeviceStatusEnum.DEFAULT;
    }

    @Override
    public EvseDeviceStatusEnum getEvseStatus(String evseSn) {
        try {
            Result<String> ret = monitorFeignClient.queryStatusByEvseSn(evseSn);
            if (StringUtils.isNotBlank(ret.getData())) {
                return EvseDeviceStatusEnum.getEnumByName(ret.getData());
            }
            // 默认状态
        } catch (Exception e) {
            log.info("MonitorFeignImpl getEvseStatus and evseSn = " + evseSn + " and exception = ", e);
        }
        return EvseDeviceStatusEnum.DEFAULT;
    }

    /**
     * 获取充电设备实时数据
     *
     * @param evseId
     * @return
     */
    @Override
    public Map<String, String> getEvseMonitorInfo(Long evseId) {
        Map<String, String> monitorMap = new HashMap<>(16);
        try {
            Optional<OpLocationEvseElasticDTO> optional = opLocationEvseElastic.findById(evseId);
            if (!optional.isPresent()) {
                return monitorMap;
            }
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = optional.get();
            Result<OpEvseMeterUploadDTO> ret = monitorFeignClient.queryNewMeterByEvseSn(opLocationEvseElasticDTO.getEvseSn());
            OpEvseMeterUploadDTO opEvseMeterUploadDTO = ret.getData();
            if (opEvseMeterUploadDTO != null) {
                monitorMap.put("evseSn", opEvseMeterUploadDTO.getEvseSn());
                monitorMap.put("connectorId", opEvseMeterUploadDTO.getConnectorId());
                monitorMap.put("createTime", toStringValue(opEvseMeterUploadDTO.getCreateTime()));
                monitorMap.put("current", toStringValue(opEvseMeterUploadDTO.getCurrent()));
                monitorMap.put("voltage", toStringValue(opEvseMeterUploadDTO.getVoltage()));
                monitorMap.put("power", toStringValue(opEvseMeterUploadDTO.getPower()));
                monitorMap.put("busId", toStringValue(opEvseMeterUploadDTO.getBusId()));
                monitorMap.put("batterySoc", toStringValue(opEvseMeterUploadDTO.getBatterySoc()));
                monitorMap.put("totalElectricalPower", toStringValue(opEvseMeterUploadDTO.getTotalElectricalPower()));
                monitorMap.put("positiveTemperature", toStringValue(opEvseMeterUploadDTO.getPositiveTemperature()));
                monitorMap.put("negativeTemperature", toStringValue(opEvseMeterUploadDTO.getNegativeTemperature()));
                monitorMap.put("rdTimeLeft", toStringValue(opEvseMeterUploadDTO.getRdTimeLeft()));
                return monitorMap;
            }
            // 默认状态
        } catch (Exception e) {
            log.info("MonitorFeignImpl getEvseMonitorInfo exception = ", e);
        }
        return monitorMap;
    }

    @Override
    public Integer getEvseMonitorSoc(Long evseId) {
        try {
            Map<String, String> map = this.getEvseMonitorInfo(evseId);
            return Integer.parseInt(map.get("batterySoc"));
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * 转字符串
     *
     * @param o
     * @return
     */
    private String toStringValue(Object o) {
        if (o == null) {
            return null;
        }
        return o.toString();
    }
}
