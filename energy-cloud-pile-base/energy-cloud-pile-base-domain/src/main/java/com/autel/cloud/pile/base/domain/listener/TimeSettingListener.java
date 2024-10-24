package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.webhook.LarkClient;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.model.ControlContentDTO;
import com.autel.cloud.pile.base.domain.service.DemandControlConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.CommonDeliveryDTO;
import com.autel.cloud.pile.base.dto.OpPileGroupTimeSettingDTO;
import com.autel.cloud.smart.charge.constant.SmartChargeConstant;
import com.autel.cloud.smart.charge.dto.MessageRetryDTO;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.annotation.*;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

/**
 * @Author temp
 * @Date 2023/8/21 10:16
 * @Since 1.0.0
 */
@Component
@Slf4j
public class TimeSettingListener extends CommonMessageRetryService {

    @Value("${spring.rabbitmq.version.suffix:}")
    public String rabbitmqVersionSuffix;

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private RabbitTemplate rabbitTemplate;
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @Value("${webhook.lark.alarmKey:7f4bee11-146e-410e-83a7-53128c27ec82}")
    private String alarmKey;

    @Resource
    private LarkClient larkClient;

    @Resource
    private DemandControlConfigService demandControlConfigService;


    @RabbitListener(bindings = @QueueBinding(
            exchange = @Exchange(value = AmqpConstant.PILE_BASE_TIME_SETTING_EXCHANGE + "#{timeSettingListener.rabbitmqVersionSuffix}", type = "x-delayed-message", arguments = @Argument(name = "x-delayed-type", value = "direct")),
            value = @Queue(value = AmqpConstant.PILE_BASE_TIME_SETTING_QUEUE + "#{timeSettingListener.rabbitmqVersionSuffix}"),
            key = AmqpConstant.PILE_BASE_TIME_SETTING_ROUTE))
    public void timeSettingListener(Message message, Channel channel) throws IOException {
        ThreadContext.put(SmartChargeConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        long tag = message.getMessageProperties().getDeliveryTag();
        channel.basicAck(tag, false);
        super.initMessage(message);
    }

    @Override
    protected void execute(MessageRetryDTO dto) {
        log.info("timeSettingListener,dto={}", JSON.toJSONString(dto));
        String msg = dto.getBodyMsg();
        if (!StringUtils.hasText(msg)) {
            return;
        }
        OpPileGroupTimeSettingDTO settingDto = JSON.parseObject(msg, OpPileGroupTimeSettingDTO.class);
        Long groupId = settingDto.getGroupId();
        String key = RedisKeyConstant.getPileBaseTimeSettingKey(groupId);
        String redisValue = stringRedisTemplate.opsForValue().get(key);
        if (!StringUtils.hasText(redisValue)) {
            log.info("timeSettingListener,redisValue is null");
            return;
        }
        OpPileGroupTimeSettingDTO redisDto = JSON.parseObject(redisValue, OpPileGroupTimeSettingDTO.class);
        if (settingDto.getRequestId().longValue() != redisDto.getRequestId().longValue()) {
            log.info("timeSettingListener,settingDto.getRequestId={},redisDto.getRequestId={}", settingDto.getRequestId(), redisDto.getRequestId());
            return;
        }
        try {
            CommonDeliveryDTO paramDto = new CommonDeliveryDTO();
            paramDto.setGroupId(groupId);
            paramDto.setType(12);
            Boolean result = opLocationPileGroupService.deliveryByGroupId(paramDto);
            log.info("timeSettingListener,result={}", result);
        } catch (Exception e) {
            log.error("timeSettingListener,Exception={}", e.getMessage());
        }
        List<OpPileGroupTimeSettingDTO> list = opLocationPileGroupService.loadAllTimeSetting(groupId);
        if (CollectionUtils.isEmpty(list)) {
            log.info("timeSettingListener,TimeSettings is null");
            return;
        }
        OpPileGroupTimeSettingDTO currentDto = list.get(0);
        log.info("timeSettingListener,currentDto={}", JSON.toJSONString(currentDto));
        stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(currentDto), 24L, TimeUnit.HOURS);
        MessageProperties messageProperties = new MessageProperties();
        messageProperties.setDelay(currentDto.getDelay());
        Message message = new Message((JSON.toJSONString(currentDto)).getBytes(StandardCharsets.UTF_8), messageProperties);
        rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_TIME_SETTING_EXCHANGE + rabbitmqVersionSuffix, AmqpConstant.PILE_BASE_TIME_SETTING_ROUTE, message);
    }

    @Override
    protected void successCallback(MessageRetryDTO dto) {

    }

    @Override
    protected void failCallback(MessageRetryDTO dto) {
        this.larkClient.sendMessage(String.format("分时设置重试MQ消费异常\n环境：%s\n消息体：%s", System.getenv("NACOS_NS"), dto.getBodyMsg()), alarmKey);
    }

    @Override
    public Integer getDelay() {
        return 30000;
    }
}
