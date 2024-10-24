package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.push.feign.PushAppClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Collections;

@Slf4j
@Component
public class PushAppClientAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private PushAppClient pushAppClient;


    @Value("${webhook.wechat.key.pile-user:66ae591a-4e15-4b57-9a1c-56978ba6152b}")
    protected String webhookWechatKey;


    public void getPushAppClient(PushAppClient.JPushMessage jPushMessage) {
        pushAppClient.sendSystemMessage(jPushMessage);
    }


    public Object addAlarm(PushAppClient.AlarmDTO alarmDTO) {
        log.info("addAlarm:{}", JSON.toJSONString(alarmDTO));
        Result<Object> result = pushAppClient.addAlarm(alarmDTO);
        log.info("addAlarm result:{}", JSON.toJSONString(result));
        Object data = nullableHandle(result);
        if (!Result.ofSucceed().getCode().equals(result.getCode())) {
            String content = String.format("调用服务(push-app/alarm) %s", JSON.toJSONString(alarmDTO, SerializerFeature.PrettyFormat));
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return Collections.emptyList();
        }
        return data;
    }

    /**
     * 去掉首页告警信息
     *
     * @param uniqueInfo 订单编号 orderNumber
     */
    public void removeAlarm(String uniqueInfo) {
        try {
            log.info("removeAlarm uniqueInfo={}", JSON.toJSONString(uniqueInfo));
            Result<Object> result = pushAppClient.removeAlarm(uniqueInfo);
            log.info("removeAlarm result:{}", JSON.toJSONString(result));
            if (!Result.ofSucceed().getCode().equals(result.getCode())) {
                String content = String.format("调用服务(push-app/alarm/{uniqueInfo}) %s", uniqueInfo);
                weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            }
        } catch (Exception e) {
            log.error("removeAlarmFailed", e);// 去不掉就算了不影响主业务
        }
    }
}
