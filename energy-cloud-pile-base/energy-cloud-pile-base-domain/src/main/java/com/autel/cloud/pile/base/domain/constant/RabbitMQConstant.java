package com.autel.cloud.pile.base.domain.constant;

public class RabbitMQConstant {

    //  单位毫秒
    public static final Integer DELAY_QUEUE_TTL = 30 * 1000;

    //    插件方式实现延时消息 --功率
    //队列
    public static final String EMS_POWER_DELAY_QUEUE_NAME = "ems_power_delay_queue";
    //交换机
    public static final String EMS_POWER_DELAY_EXCHANGE_NAME = "ems_power_delay_exchange";
    //routingKey
    public static final String EMS_POWER_DELAY_ROUTING_KEY = "ems_power_delay_routing_key";
}
