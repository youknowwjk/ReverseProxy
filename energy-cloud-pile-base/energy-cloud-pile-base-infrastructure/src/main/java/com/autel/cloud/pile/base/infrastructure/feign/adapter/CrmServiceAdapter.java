package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.commonserver.dto.LarkRobotContentDto;
import com.autel.cloud.commonserver.dto.LarkRobotNotifyDto;
import com.autel.cloud.crm.CrmFeignClient;
import com.autel.cloud.crm.dto.OrderTypeNoticeDTO;
import com.autel.cloud.crm.dto.RfdNoticeDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
@Component
public class CrmServiceAdapter extends AbstractFeignServiceAdapter {

    @Value("${noticeCrmRobotUrl:0b15d498-3d94-4cb7-80ee-3bc7932bc894}")
    private String noticeCrmRobotUrl;

    @Resource
    private CrmFeignClient crmFeignClient;

    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;
    @Autowired
    private CommonServiceAdapter commonServiceAdapter;


    public Result<Boolean> extractCRM(RfdNoticeDto rfdNoticeDto) {
        return crmFeignClient.extractCRM(rfdNoticeDto);
    }


    public Boolean orderTypeNotice(OrderTypeNoticeDTO orderTypeNoticeDTO) {
        try {
            log.info("crmFeignClient.orderPerformanceNotice request: {}", JSON.toJSONString(orderTypeNoticeDTO));
            Result<Boolean>  result = crmFeignClient.orderPerformanceNotice(orderTypeNoticeDTO);
            log.info("crmFeignClient.orderPerformanceNotice response: {}", JSON.toJSONString(result));
            if (result.getCode() != HttpStatus.HTTP_OK) {
                //失败发送飞书机器人
                LarkRobotNotifyDto robotNotifyDto = buildLarkRobotNotifyDto(orderTypeNoticeDTO, JSON.toJSONString(result));
                commonServiceAdapter.robotFeiShuNotify(robotNotifyDto);
            }
            return Optional.ofNullable(nullableHandle(result)).orElse(Boolean.FALSE);
        } catch (Exception e) {
            log.error("crmFeignClient.orderPerformanceNotice:", e);
            //失败发送飞书机器人
            LarkRobotNotifyDto robotNotifyDto = buildLarkRobotNotifyDto(orderTypeNoticeDTO, e.getMessage());
            commonServiceAdapter.robotFeiShuNotify(robotNotifyDto);
        }
        return Boolean.FALSE;
    }

    private LarkRobotNotifyDto buildLarkRobotNotifyDto(OrderTypeNoticeDTO orderTypeNoticeDTO, String errorMessage) {
        List<LarkRobotContentDto> robotContentList =
                Stream.of(
                        new Object[][] {
                                {"div", "环境 : ", System.getenv("NACOS_NS"), "blank"},
                                {"div", "回调事件 : ", orderTypeNoticeDTO.getEvent(), "blank"},
                                {"div", "orderId : ", orderTypeNoticeDTO.getOrderId(), "blank"},
                                {"div", "returnId : ", orderTypeNoticeDTO.getReturnId(), "blank"},
                                {"div", "错误信息 : ", errorMessage, "blank"}
                        })
                        .map(
                                item ->
                                        LarkRobotContentDto.createLarkRobotContentDto(
                                                (String) item[0],
                                                (String) item[1],
                                                Boolean.FALSE,
                                                (String) item[2],
                                                (String) item[3]))
                        .collect(Collectors.toList());
        LarkRobotNotifyDto robotNotifyDto = new LarkRobotNotifyDto("yellow", "CSMS回调CRM接口异常",
                null, robotContentList, null, false, noticeCrmRobotUrl);
        return robotNotifyDto;
    }

}
