package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.service.RuleLocationPileService;
import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.ExchangeTypes;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.Exchange;
import org.springframework.amqp.rabbit.annotation.Queue;
import org.springframework.amqp.rabbit.annotation.QueueBinding;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;

/**
 * @Author temp
 * @Date 2022/8/1 23:19
 */
@Component
@Slf4j
public class PileDeleteListener {
    @Autowired
    private RuleLocationPileService ruleLocationPileService;

    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = PILE_BASE_PILE_DELETE_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_PILE_DELETE_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_PILE_DELETE_ROUTE}))
    public void pileDeleteListener(String data, Channel channel, Message message) {
        log.info("pileDeleteListener,data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("pileDeleteListener,tag={}", tag);
        try {
            if (StringUtils.hasText(data)){
                Long pileId = JSON.parseObject(data, Long.class);
                if(!ObjectUtils.isEmpty(pileId)){
                    boolean result = ruleLocationPileService.deleteRuleRelateByPileId(pileId);
                    log.info("pileDeleteListener,result={}", result);
                    if (result){
                        channel.basicAck(tag,false);
                    }
                }
            }
        } catch (Exception e) {
            log.error("pileDeleteListener,exception:{}",e);
            channel.basicReject(tag,true);
        }
    }
}
