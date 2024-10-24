package com.autel.cloud.pile.base.domain.job;

import com.autel.cloud.pile.base.domain.service.PaymentCardService;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @description app 客户绑定的卡，初始化指纹、过期年月
 * @auther A23204
 * @datetime 2023/10/11 11:14
 */
@Slf4j
@Component
public class PayCardHistoryInitJob {

    @Autowired
    PaymentCardService paymentCardService;

    /**
     * description: payCardHistoryInit 初始化指纹、过期年月
     * version: 1.0
     * date: 2023/10/11 11:17 
     * author: A23204 
     * 
     * @param param
     * @return com.xxl.job.core.biz.model.ReturnT<java.lang.String>
     */ 
    @XxlJob("payCardHistoryInit")
    public ReturnT<String> payCardHistoryInit(String param) {

        log.info("--->>> payCardHistoryInit start.");
        paymentCardService.payCardHistoryInit();
        return ReturnT.SUCCESS;
    }

}
