package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.constant.RabbitMQConstant;
import com.autel.cloud.pile.base.domain.service.DelayService;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.UUID;

@Service
@Slf4j
public class DelayServiceImpl implements DelayService {

    private Random ran = new Random();

    @Resource
    private RabbitTemplate rabbitTemplate;

    @Resource
    private RabbitBean rabbitBean;

    /**
     * 发送延迟消息
     * @param msg  要发送的消息
     */
    public void sendMessage(String msg){
        String exchange = null;
        //处理exchange
        if(StringUtils.isNotBlank(rabbitBean.rabbitmqVersionSuffix)){
            exchange = RabbitMQConstant.EMS_POWER_DELAY_EXCHANGE_NAME + rabbitBean.rabbitmqVersionSuffix;
        }else{
            exchange = RabbitMQConstant.EMS_POWER_DELAY_EXCHANGE_NAME;
        }
        //发送到rabbit
        rabbitTemplate.convertAndSend(exchange,
                RabbitMQConstant.EMS_POWER_DELAY_ROUTING_KEY,msg,
                message -> {
            message.getMessageProperties().setDelay(RabbitMQConstant.DELAY_QUEUE_TTL);
            return message;
        });
    }

    @Override
    public Boolean sendDelayMessage() {
        Map<String,Object> delayMessage = new HashMap<>();
        Long i = ran.nextInt(20)+1l;
        delayMessage.put("createTime",System.currentTimeMillis());
        delayMessage.put("delayTime",i);
        delayMessage.put("num", UUID.randomUUID().toString().replace("-","").substring(0,32));
        //发送延迟信息
        sendMessage(JSON.toJSONString(delayMessage));
        return true;
    }

    @Override
    public Boolean sendDelayMessage(String sn) {
        //发送延迟信息
        sendMessage(sn);
        return true;
    }
}
