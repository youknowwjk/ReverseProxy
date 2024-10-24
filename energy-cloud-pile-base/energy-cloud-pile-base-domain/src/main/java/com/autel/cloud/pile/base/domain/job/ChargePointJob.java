package com.autel.cloud.pile.base.domain.job;

import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
@Slf4j
public class ChargePointJob {



    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    /**
     * 桩订阅提醒即将过期
     */
    @XxlJob("scanImminentExpireChargePoint")
    public ReturnT<String> scanImminentExpireChargePoint(String params) {
        String[] strings = params.split(",");
        Long[] num = new Long[strings.length];
        for (int i = 0; i < strings.length; i++) {
            num[i] = Long.parseLong(strings[i]);
        }
        chargePointMerchantRelationService.scanImminentExpireChargePoint(num);
        return ReturnT.SUCCESS;
    }

}
