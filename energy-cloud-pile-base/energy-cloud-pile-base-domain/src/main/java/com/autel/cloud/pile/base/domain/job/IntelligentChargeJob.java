package com.autel.cloud.pile.base.domain.job;

import com.autel.cloud.pile.base.domain.service.impl.IntelligentChargingScheduling;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

/**
 * @Author A22282
 * @Date 2022/4/20 11:24
 */
@Component
@Slf4j
public class IntelligentChargeJob {

    @Resource
    private IntelligentChargingScheduling intelligentChargingScheduling;

    @XxlJob("triggerIdentifyJob")
    public synchronized ReturnT<String> triggerIdentifyJob(String param) {
        XxlJobLogger.log("triggerIdentifyJob params={}", param);
        intelligentChargingScheduling.triggerIdentifyJob(Integer.parseInt(param));
        return ReturnT.SUCCESS;
    }

    @XxlJob("triggerRefreshSOCJob")
    public ReturnT<String> triggerRefreshSOCJob(String param) {
        XxlJobLogger.log("triggerRefreshSOCJob params={}", param);
        log.info("trigger RefreshSOCJob params={}", param);
        intelligentChargingScheduling.triggerRefreshSOCJob(Long.parseLong(param));
        XxlJobLogger.log("trigger RefreshSOCJob finished");
        return ReturnT.SUCCESS;
    }

    @XxlJob("triggerIntelligentChargeJob")
    public ReturnT<String> triggerIntelligentChargeJob(String param) {
        XxlJobLogger.log("triggerIntelligentChargeJob params={}", param);
        log.info("triggerIntelligentChargeJob params={}", param);
        intelligentChargingScheduling.triggerIntelligentChargeJob(Long.parseLong(param));
        XxlJobLogger.log("triggerIntelligentChargeJob finished");
        return ReturnT.SUCCESS;
    }

    @XxlJob("cleanIntelligentChargeJob")
    public ReturnT<String> cleanIntelligentChargeJob(String param) {
        XxlJobLogger.log("cleanIntelligentChargeJob params={}", param);
        log.info("cleanIntelligentChargeJob params={}", param);
        intelligentChargingScheduling.cleanIntelligentChargeJob(Long.parseLong(param));
        XxlJobLogger.log("cleanIntelligentChargeJob finished");
        return ReturnT.SUCCESS;
    }

}
