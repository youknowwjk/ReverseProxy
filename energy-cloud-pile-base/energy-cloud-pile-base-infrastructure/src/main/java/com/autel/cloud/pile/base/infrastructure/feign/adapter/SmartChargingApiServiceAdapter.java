package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.smartbi.dto.ScheduleArithmeticRequest;
import com.autel.cloud.smartbi.dto.ScheduleArithmeticResponse;
import com.autel.cloud.smartbi.feign.SmartChargingAPIServerFeign;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Slf4j
@Component
public class SmartChargingApiServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private SmartChargingAPIServerFeign smartChargingAPIServerFeign;

    @Value("${webhook.wechat.key.pile-user:66ae591a-4e15-4b57-9a1c-56978ba6152b}")
    protected String webhookWechatKey;

    public Result<ScheduleArithmeticResponse> arithmetic(ScheduleArithmeticRequest scheduleArithmeticRequest) {
        log.info("scheduleArithmeticRequest {}", JSON.toJSONString(scheduleArithmeticRequest));
        Result<ScheduleArithmeticResponse> responseResult = smartChargingAPIServerFeign.arithmetic(scheduleArithmeticRequest);
        log.info("ScheduleArithmeticResponse {}", JSON.toJSONString(responseResult));
        return responseResult;
    }

}
