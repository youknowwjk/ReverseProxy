package com.autel.cloud.pile.base.domain.listener;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileTariffServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.user.api.dto.SellerDTO;
import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.amqp.core.ExchangeTypes;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.Exchange;
import org.springframework.amqp.rabbit.annotation.Queue;
import org.springframework.amqp.rabbit.annotation.QueueBinding;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Objects;
import java.util.UUID;

import static com.autel.cloud.pile.user.api.constant.RabbitMqConstant.SELLER_UPDATE;
import static com.autel.cloud.pile.user.api.constant.RabbitMqConstant.TOPIC_EXCHANGE_PILE_USER;


/**
 * 商家名称监听
 * 商家名称变化时发送变更事件 然后监听修改 名称
 */
@Component
@Slf4j
public class SellerUpdateEventListener {

    public static final String TOPIC_QUEUE_PILE_BASE_OPERATOR = "TOPIC_QUEUE_PILE_BASE_OPERATOR";

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    @Resource
    private PileTariffServiceAdapter pileTariffServiceAdapter;

    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(
            value = @Queue(value = TOPIC_QUEUE_PILE_BASE_OPERATOR + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = TOPIC_EXCHANGE_PILE_USER + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.TOPIC, ignoreDeclarationExceptions = "true"),
            key = {SELLER_UPDATE}
    ))
    public void listenerSellerDTO(String json, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString().replace("-",""));
        log.info(">>>>>>>listenerSellerDTO 监听到了一条信息: " + json);
        // 重启该微服务 那条未被确认的消息会被重新监听到
        long deliveryTag = message.getMessageProperties().getDeliveryTag();
        log.info(">>>>>>>>listenerSellerDTO deliveryTag: {}<<<<<<<<<", deliveryTag);
        try {
            SellerDTO seller = JSON.parseObject(json, SellerDTO.class);
            Long sellerId = seller.getId();
            pileUserServiceAdapter.refreshSeller(sellerId);
            pileTariffServiceAdapter.refreshSeller(sellerId);
            channel.basicAck(deliveryTag, false);
            log.info(">>>>>>>>>>>>receiver success<<<<<<<<<");
        } catch (Exception e) {
            log.info(">>>>>>>>>>>>>>receiver failed<<<<<<<<<<<");
            log.error("listener", e);
            channel.basicAck(deliveryTag, false);// 一般都可以处理 所以给消费调
        }
    }

}