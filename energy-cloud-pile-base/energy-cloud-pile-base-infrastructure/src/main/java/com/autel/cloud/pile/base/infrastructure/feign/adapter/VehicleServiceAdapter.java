package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.vehicle.VehicleServiceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Objects;

@Slf4j
@Component
@RefreshScope
public class VehicleServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private VehicleServiceClient vehicleServiceClient;

    /**
     * 当前用户的所有车辆信息
     */
    public JSONArray getByUserId(Long userId) {
        log.info("getByUserId:{}", userId);
        Result<JSONArray> listResult = vehicleServiceClient.getByUserId(userId);
        log.info("getByUserId result: {}", JSON.toJSONString(listResult));
        JSONArray handle = nullableHandle(listResult);
        if (Objects.isNull(handle)) {
            return new JSONArray();
        }
        return handle;
    }

    public JSONObject getByUserIdAndVehicleId(Long userId, String vehicleId) {
        log.info("getByUserIdAndVehicleId: {}, {}", userId, vehicleId);
//        vehicleId+="?field=information&field=chargeState";
        Result<JSONObject> result = vehicleServiceClient.getByUserIdAndVehicleId(userId, vehicleId);
        log.info("getByUserIdAndVehicleId result: {}", JSON.toJSONString(result));
        return nullableHandle(result);
    }

    /**
     * 当前用户的所有车辆信息
     */
    public boolean disconnect(String vendor) {
        try {
            log.info("disconnect:{}", vendor);
            Result<Boolean> listResult = vehicleServiceClient.disconnect(vendor);
            log.info("disconnect result: {}", JSON.toJSONString(listResult));
            return nullableHandle(listResult);
        } catch (Exception e){
            log.error("disconnect " + vendor, e);
        }
        return false;
    }
}
