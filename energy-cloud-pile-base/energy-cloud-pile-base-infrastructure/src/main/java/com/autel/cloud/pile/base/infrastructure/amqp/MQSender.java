package com.autel.cloud.pile.base.infrastructure.amqp;

import com.alibaba.fastjson.JSON;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

import static com.autel.cloud.pile.base.constant.AmqpConstant.TOPIC_EXCHANGE_PILE_BASE;
import static com.autel.cloud.pile.base.constant.AmqpConstant.TOPIC_EXCHANGE_PILE_BASE_FANOUT;


@Component
@Slf4j
public class MQSender {

    @Resource
    private RabbitTemplate rabbitTemplate;

    /**
     * 发送信息
     *
     * @param routeKey
     * @param object
     */
    public void send(String routeKey, Object object) {
        log.info("mq send routeKey:{}", routeKey);
        log.info("mq send object:{}", JSON.toJSONString(object));
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, routeKey, JSON.toJSONString(object));
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE_FANOUT /*+ RabbitBean.RABBITMQ_VERSION_SUFFIX*/, routeKey, JSON.toJSONString(object));
    }
}
