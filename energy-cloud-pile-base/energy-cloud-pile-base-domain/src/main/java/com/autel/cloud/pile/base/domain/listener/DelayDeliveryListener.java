package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.strategy.impl.ImmediatelyStrategy;
import com.autel.cloud.pile.base.vo.DelayDeliveryVO;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.*;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.List;

/**
 * @Author temp
 * @Date 2023/2/1 10:49
 */
@Component
@Slf4j
public class DelayDeliveryListener {

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private ImmediatelyStrategy immediatelyStrategy;

    @RabbitListener(bindings = @QueueBinding(
            exchange = @Exchange(value = AmqpConstant.PILE_BASE_SMART_CHARGING_ENERGY_USE_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = "x-delayed-message", arguments = @Argument(name = "x-delayed-type", value = "direct")),
            value = @Queue(value = AmqpConstant.PILE_BASE_SMART_CHARGING_ENERGY_USE_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}"),
            key = AmqpConstant.PILE_BASE_SMART_CHARGING_ENERGY_USE_ROUTE))
    public void deliverySetting(Message message, Channel channel) throws IOException {
        long tag = message.getMessageProperties().getDeliveryTag();
        channel.basicAck(tag, false);
        String msg = new String(message.getBody());
        log.info("deliverySetting,msg={}", JSON.toJSONString(msg));
        if (!StringUtils.isEmpty(msg)) {
            DelayDeliveryVO delayDeliveryVO = JSON.parseObject(msg, DelayDeliveryVO.class);
            Long groupId = delayDeliveryVO.getGroupId();
            //校验延迟序号
            String delayKey = RedisKeyConstant.getSmartChargingDelayDeliveryKey(groupId);
            String redisTime = stringRedisTemplate.opsForValue().get(delayKey);
            log.info("deliverySetting, redis delayTime={}", JSON.toJSONString(redisTime));

            Long delayTime = delayDeliveryVO.getDelayTime();
            //配置下发
            if (!StringUtils.isEmpty(redisTime) && delayTime.longValue() == Long.parseLong(redisTime)) {
                String dataKey = RedisKeyConstant.getSmartChargingDelayDeliveryDataKey(groupId);
                String dataString = stringRedisTemplate.opsForValue().get(dataKey);
                if (!StringUtils.isEmpty(dataString)) {
                    List<OpLocationPileGroupDeliveryVO> deliveryVOList = JSON.parseArray(dataString, OpLocationPileGroupDeliveryVO.class);
                    log.info("deliverySetting, deliveryVOList={}", JSON.toJSONString(deliveryVOList));
                    immediatelyStrategy.delivery(groupId, deliveryVOList);
                    //删除缓存
                    stringRedisTemplate.delete(delayKey);
                    stringRedisTemplate.delete(dataKey);
                }
            }
        }
    }
}
