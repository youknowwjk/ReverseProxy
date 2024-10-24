package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.service.impl.IntelligentChargingScheduling;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleJobMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitHandler;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;

import static com.autel.cloud.pile.base.infrastructure.amqp.RabbitMQConfig.X_QUEUE_START_SMART_CHARGING;

/**
 * ClassName TimeoutStartIntelligentChargingListener
 * Description 死信队列监听器
 **/
@Slf4j
@Component
public class TimeoutStartSmartChargingListener {

    @Resource
    private IntelligentChargingScheduling intelligentChargingScheduling;

    @RabbitHandler
    @RabbitListener(queues = X_QUEUE_START_SMART_CHARGING + "#{rabbitBean.rabbitmqVersionSuffix}")
    public void timeoutStartSmartCharging(Message message, Channel channel) throws IOException {
        String body = null;
        long tag = 0;
        try {
            tag = message.getMessageProperties().getDeliveryTag();
            body = new String(message.getBody());
            log.info(">>>>>>>死信队列监听到一条到消息 {},  {}<<<<<<<<<", body, tag);
            Long id = Long.parseLong(body);
            boolean b = intelligentChargingScheduling.lastTrySmartSchedule(id);
            log.info(">>>>>>>lastTrySmartSchedule  {}<<<<<<<<<", b);
        } catch (Exception e) {
            log.error("timeoutStartSmartCharging" + body, e);
        } finally {
            channel.basicAck(tag, false);
        }
    }
}