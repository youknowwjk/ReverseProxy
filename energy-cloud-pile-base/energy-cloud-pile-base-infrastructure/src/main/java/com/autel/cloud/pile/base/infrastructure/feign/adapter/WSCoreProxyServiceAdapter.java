package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.WsCoreClient;
import com.autel.cloud.smart.charge.constant.SmartChargeConstant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Optional;

@Slf4j
@Component
public class WSCoreProxyServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private WsCoreClient wsCoreClient;

    /**
     * 判断桩是否在线
     *
     * @param pileSn
     * @return
     */
    public boolean pileIsOnline(String pileSn) {
        log.info("pileIsOnline pileSn = {}", pileSn);
        Result<Boolean> booleanResult = wsCoreClient.userIsOnline(pileSn, SmartChargeConstant.ENERGY_PROTOCOL_OCPP);
        log.info("userIsOnline pileSn = {} Response = {}", pileSn, JSON.toJSONString(booleanResult));
        Boolean aBoolean = nullableHandle(booleanResult);
        return Optional.ofNullable(aBoolean).orElse(Boolean.FALSE);
    }


}
