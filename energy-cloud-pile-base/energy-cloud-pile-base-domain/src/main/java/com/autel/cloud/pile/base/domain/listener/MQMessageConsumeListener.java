package com.autel.cloud.pile.base.domain.listener;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpCostRuleDistributeService;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.ExchangeTypes;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.Exchange;
import org.springframework.amqp.rabbit.annotation.Queue;
import org.springframework.amqp.rabbit.annotation.QueueBinding;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;

/**
 * @author
 * @since
 */
@Slf4j
@Component
public class MQMessageConsumeListener {

    @Resource
    OpCostRuleDistributeService opCostRuleDistributeService;

    /**
     * 计费规则下发消息监听
     */
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = TARIFF_DISPATCH_ONLINE_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = TARIFF_DISPATCH_ONLINE_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.TOPIC, ignoreDeclarationExceptions = "true"),
            key = {TARIFF_DISPATCH_ONLINE_ROUTE_KEY}))
    public void consumerRetryMsg(Message message, Channel channel) throws IOException {
        log.info("============= 计费规则下发消息监听：{}", new String(message.getBody()));
        long deliveryTag = message.getMessageProperties().getDeliveryTag();
        String pileSn = new String(message.getBody());
        Result<Boolean> result = opCostRuleDistributeService.retryDistributeTariff(pileSn);
        log.info("============== the result in the consumerRetryMsg function:{}", result);
        channel.basicAck(deliveryTag, Boolean.FALSE);
    }
}