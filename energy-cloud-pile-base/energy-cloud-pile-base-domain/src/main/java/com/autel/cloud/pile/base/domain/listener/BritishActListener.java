package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.dto.britishAct.DtmConfigChangeEventDTO;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.ThreadContext;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.amqp.core.ExchangeTypes;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.UUID;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;

/**
 * 英国法案桩侧参数改变时，通过运维上报事件变动通知
 */
@Slf4j
@Component
public class BritishActListener {

    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;


    @RabbitHandler
    @RabbitListener(
            bindings = @QueueBinding(
                    value = @Queue(value = AmqpConstant.DTM_CONFIG_CHANGE_EVENT_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", declare = "true"),
                    exchange = @Exchange(value = AmqpConstant.DTM_CONFIG_CHANGE_EVENT_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", declare = "true", type = "direct"),
                    key = AmqpConstant.DTM_CONFIG_CHANGE_EVENT_ROUTING_KEY
            )
    )
    public void consumeMsg(Message message, Channel channel) throws IOException {

        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString().replace("-",""));

        String body = new String(message.getBody());
        long deliveryTag = message.getMessageProperties().getDeliveryTag();

        log.info("===>>> BritishActListener.consumeMsg body : {}, deliveryTag : {}",
                body,
                deliveryTag);

        try {
            DtmConfigChangeEventDTO dtmConfigChangeEventDTO = JSON.parseObject(body, DtmConfigChangeEventDTO.class);
            if (dtmConfigChangeEventDTO != null
                    && StringUtils.isNotBlank(dtmConfigChangeEventDTO.getSn())
                    && dtmConfigChangeEventDTO.getChangeList() != null
                    && (Integer.valueOf(1).equals(dtmConfigChangeEventDTO.getChangeList().getEVSCP())
                    || Integer.valueOf(1).equals(dtmConfigChangeEventDTO.getChangeList().getOffPeakHour())))  {

                String pileSn = dtmConfigChangeEventDTO.getSn();
                // todo 拿不到userId, 传-1
                Long userId = -1L;

                AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> opLocationPileEvseService.queryEvscp(pileSn, userId)));
                AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> opLocationPileEvseService.queryOffPeakHour(pileSn, userId)));
            }
        } catch (Exception e) {

            log.error("===>>> BritishActListener.consumeMsg Abnormal consumption(消费异常) : ",
                    e);

        } finally {
            channel.basicAck(deliveryTag, false);
        }
    }
}
