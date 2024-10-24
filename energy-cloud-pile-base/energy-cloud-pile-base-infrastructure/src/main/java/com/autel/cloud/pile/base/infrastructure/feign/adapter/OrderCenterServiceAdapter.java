package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.crm.CrmFeignClient;
import com.autel.cloud.crm.dto.OrderTypeNoticeDTO;
import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
import com.autel.cloud.ordercenter.vo.ReturnOrderReqVO;
import com.autel.cloud.ordercenter.vo.ReturnOrderRespVO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnLicenseVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Optional;

@Slf4j
@Component
public class OrderCenterServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private OrderCenterFeignClient orderCenterFeignClient;

    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;




    public ReturnOrderRespVO orderReturn(ReturnOrderReqVO returnOrderReqVO) {
        try {
            log.info("orderCenterFeignClient.orderReturn request: {}", JSON.toJSONString(returnOrderReqVO));
            Result<ReturnOrderRespVO>  result = orderCenterFeignClient.orderReturn(returnOrderReqVO);
            log.info("orderCenterFeignClient.orderReturn response: {}", JSON.toJSONString(result));
            if (HttpStatus.HTTP_OK != result.getCode()) {
                throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
            }
            return Optional.ofNullable(nullableHandle(result)).orElse(new ReturnOrderRespVO());
        } catch (Exception e) {
            log.error("orderCenterFeignClient.orderReturn:", e);
            throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
        }
    }
}
