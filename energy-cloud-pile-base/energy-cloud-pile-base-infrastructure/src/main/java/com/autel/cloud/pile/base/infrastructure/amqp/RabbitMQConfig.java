package com.autel.cloud.pile.base.infrastructure.amqp;

import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.*;
import org.springframework.amqp.rabbit.connection.CorrelationData;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Date;
import java.util.HashMap;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;


@Configuration
@Slf4j
public class RabbitMQConfig implements InitializingBean {

    @Autowired
    private RabbitTemplate rabbitTemplate;


    // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓车辆识别过期时间↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    public static final String X_EXCHANGE_START_SMART_CHARGING = "X-Exchange-start-smart-charging";
    public static final String X_QUEUE_START_SMART_CHARGING = "X-Queue-start-smart-charging-biz";
    public static final String TTL_ROUTING_KEY = "ttl-biz";
//    public static final String X_QUEUE_DELIVERY_CHARGING_PROFILE = "X-Queue-deliveryChargingProfileDelay";
//    public static final String TTL_ROUTING_DELIVERY_KEY = "ttl-deliveryChargingProfile";

    @Bean
    public Queue identifyVehicleDelayQueue() {
        return new Queue(X_QUEUE_START_SMART_CHARGING + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
    }

    @Bean
    public CustomExchange identifyVehicleDelayExchange() {
        HashMap<String, Object> map = new HashMap<>();
        map.put("x-delayed-type", "direct");
        return new CustomExchange(X_EXCHANGE_START_SMART_CHARGING + RabbitBean.RABBITMQ_VERSION_SUFFIX, "x-delayed-message", true, false, map);
    }

    @Bean
    public Binding identifyVehicleDelayBinding() {
        return BindingBuilder.bind(identifyVehicleDelayQueue())
                .to(identifyVehicleDelayExchange())
                .with(TTL_ROUTING_KEY).noargs();
    }

//    @Bean
//    public Queue deliveryChargingProfileDelayQueue() {
//        return new Queue(X_QUEUE_DELIVERY_CHARGING_PROFILE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
//    }
//
//    @Bean
//    public Binding deliveryChargingProfileDelayBinding() {
//        return BindingBuilder.bind(deliveryChargingProfileDelayQueue())
//                .to(identifyVehicleDelayExchange())
//                .with(TTL_ROUTING_DELIVERY_KEY).noargs();
//    }

    /**
     * 直连交换机
     **/
    @Bean
    public DirectExchange directExchange() {
        return new DirectExchange(DIRECT_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true, false);
    }


    /**
     * 主题交换机
     **/
    @Bean
    public TopicExchange topicExchange() {
        return new TopicExchange(TOPIC_EXCHANGE_PILE_BASE+ RabbitBean.RABBITMQ_VERSION_SUFFIX, true, false);
    }

    /**
     * 广播交换机
     **/
    @Bean
    public FanoutExchange  fanoutExchange() {
        return new FanoutExchange(TOPIC_EXCHANGE_PILE_BASE_FANOUT+ RabbitBean.RABBITMQ_VERSION_SUFFIX, true, false);
    }

    /**
     * 主题交换机
     **/
    @Bean
    public DirectExchange locationDeleteExchange() {
        return new DirectExchange(PILE_BASE_LOCATION_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX , true, false);
    }

    @Bean
    public Queue locationDeleteQueue() {
        return new Queue(PILE_BASE_LOCATION_DELETE_QUEUE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
    }

    @Bean
    public Binding locationDeleteBinding(Queue locationDeleteQueue, DirectExchange locationDeleteExchange) {
        return BindingBuilder.bind(locationDeleteQueue)
                .to(locationDeleteExchange)
                .with(PILE_BASE_LOCATION_DELETE_ROUTE);
    }

    /**
     * 主题交换机
     **/
    @Bean
    public DirectExchange locationEditAdvExchange() {
        return new DirectExchange(PILE_BASE_LOCATION_EDIT_ADV_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX , true, false);
    }

    @Bean
    public Queue locationAdvQueue() {
        return new Queue(PILE_BASE_LOCATION_ADV_QUEUE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
    }

    @Bean
    public Binding locationAdvBinding(Queue locationAdvQueue, DirectExchange locationEditAdvExchange) {
        return BindingBuilder.bind(locationAdvQueue)
                .to(locationEditAdvExchange)
                .with(PILE_BASE_LOCATION_ADV_ROUTE);
    }

    /**
     * 主题交换机
     **/
    @Bean
    public DirectExchange pileDeleteExchange() {
        return new DirectExchange(PILE_BASE_PILE_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true, false);
    }

    @Bean
    public Queue pileDeleteQueue() {
        return new Queue(PILE_BASE_PILE_DELETE_QUEUE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
    }

    @Bean
    public Binding pileDeleteBinding(Queue pileDeleteQueue, DirectExchange pileDeleteExchange) {
        return BindingBuilder.bind(pileDeleteQueue)
                .to(pileDeleteExchange)
                .with(PILE_BASE_PILE_DELETE_ROUTE);
    }

    /**
     * 主题交换机
     **/
    @Bean
    public DirectExchange pileAddExchange() {
        return new DirectExchange(PILE_BASE_LOCATION_ADD_EXCHANGE+ RabbitBean.RABBITMQ_VERSION_SUFFIX, true, false);
    }

    @Bean
    public Queue pileAddQueue() {
        return new Queue(PILE_BASE_LOCATION_ADD_QUEUE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
    }

    @Bean
    public Binding pileAddBinding(Queue pileAddQueue, DirectExchange pileAddExchange) {
        return BindingBuilder.bind(pileAddQueue)
                .to(pileAddExchange)
                .with(PILE_BASE_LOCATION_ADD_ROUTE);
    }

    /**
     * 主题交换机
     **/
    @Bean
    public DirectExchange locationUpdateExchange() {
        return new DirectExchange(PILE_BASE_LOCATION_UPDATE_ORG_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true, false);
    }

    @Bean
    public Queue locationUpdateQueue() {
        return new Queue(PILE_BASE_LOCATION_UPDATE_ORG_QUEUE + RabbitBean.RABBITMQ_VERSION_SUFFIX, true);
    }

    @Bean
    public Binding locationUpdateBinding(Queue locationUpdateQueue, DirectExchange locationUpdateExchange) {
        return BindingBuilder.bind(locationUpdateQueue)
                .to(locationUpdateExchange)
                .with(PILE_BASE_LOCATION_UPDATE_ORG_ROUTE);
    }


    /**
     * 当交换机没有目标queue投放时 Rabbit会回调用这个函数
     *
     * @param message
     * @param replyCode
     * @param replyText
     * @param exchange
     * @param routingKey
     */
    public void returnedMessage(Message message, int replyCode, String replyText, String exchange, String routingKey) {
        log.info("回退消息收到 message：{}", message);
        log.info("回退消息收到 replyCode:{}", replyCode);
        log.info("回退消息收到 replyText：{}", replyText);
        log.info("回退消息收到 exchange:{}", exchange);
        log.info("回退消息收到 routingKey:{}", routingKey);

    }

    public void confirm(CorrelationData correlationData, boolean ack, String cause) {
        if (!ack) {
            log.info(">>>>>>消息发送失败,原因:" + cause + correlationData.toString());
        } else {
            log.info(">>>>>>>消息发送成功，当前时间为:" + new Date());
        }
    }

    @Override
    public void afterPropertiesSet() throws Exception {
        rabbitTemplate.setReturnCallback(this::returnedMessage);
        rabbitTemplate.setConfirmCallback(this::confirm);
    }
}