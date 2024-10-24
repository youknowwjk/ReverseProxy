package com.autel.cloud.pile.base.config;

import com.autel.cloud.pile.base.domain.constant.RabbitMQConstant;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import org.apache.commons.lang3.StringUtils;
import org.springframework.amqp.core.*;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.annotation.Resource;
import java.util.HashMap;
import java.util.Map;

@Configuration
public class RabbitMQConfiguration {

    @Resource
    private RabbitBean rabbitBean;

    //延时消息
    //声明交换机
    @Bean
    public CustomExchange emsDelayedExchange(){
        String exchange = null;
        if(StringUtils.isNotBlank(rabbitBean.rabbitmqVersionSuffix)){
            exchange = RabbitMQConstant.EMS_POWER_DELAY_EXCHANGE_NAME + rabbitBean.rabbitmqVersionSuffix;
        }else{
            exchange = RabbitMQConstant.EMS_POWER_DELAY_EXCHANGE_NAME;
        }
        Map<String,Object> argMap = new HashMap<>();
        argMap.put("x-delayed-type","direct");
        return new CustomExchange(exchange,"x-delayed-message",true,false,argMap);
    }
    //声明队列
    @Bean
    public Queue emsDelayQueue(){
        String queue = null;
        if(StringUtils.isNotBlank(rabbitBean.rabbitmqVersionSuffix)){
            queue = RabbitMQConstant.EMS_POWER_DELAY_QUEUE_NAME + rabbitBean.rabbitmqVersionSuffix;
        }else{
            queue = RabbitMQConstant.EMS_POWER_DELAY_QUEUE_NAME;
        }
        return new Queue(queue);
    }
    //绑定
    @Bean
    public Binding emsDelayedQueueBindingDelayedExchange(@Qualifier("emsDelayQueue") Queue emsDelayQueue,
                                                         @Qualifier("emsDelayedExchange") CustomExchange emsDelayedExchange){
        return BindingBuilder.bind(emsDelayQueue)
                .to(emsDelayedExchange)
                .with(RabbitMQConstant.EMS_POWER_DELAY_ROUTING_KEY)
                .noargs();
    }

}
