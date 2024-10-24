package com.autel.cloud.pile.base.domain.listener.ops;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.config.ops.content.SetPosAuthorizedAmountDTO;
import com.autel.cloud.pile.base.config.ops.terminal.SendTerminal;
import com.autel.cloud.pile.base.config.ops.terminal.SetPosAuthorizedAmount;
import com.autel.cloud.pile.base.domain.service.impl.OpPosAuthorizedAmountDistributeServiceImpl;
import com.autel.cloud.pile.base.enums.ops.ConfigMsgParamEnum;
import com.autel.cloud.pile.base.config.ops.constant.TopicConstant;
import com.autel.cloud.pile.base.config.ops.request.DownConfigMsgToOpsDTO;
import com.autel.cloud.pile.base.config.ops.response.GetConfigMsgFromOpsDTO;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.service.OpPosAuthorizedAmountDistributeService;
import com.autel.cloud.pile.base.domain.utils.KafkaUtil;
import com.autel.cloud.pile.base.enums.ops.ConfigMsgActionEnum;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPosAuthorizedAmountDistributeEntity;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.math.BigDecimal;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 监听消息
 */
@Component
@Slf4j
public class PosAuthorizedAmountDistributeListener {

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Autowired
    private OpPosAuthorizedAmountDistributeService opPosAuthorizedAmountDistributeService;

    @Autowired
    private KafkaUtil kafkaUtil;

    @Qualifier("kafkaTemplate")
    @Resource
    private KafkaTemplate<String, String> kafkaTemplate;

    @Autowired
    private DeviceServiceFeign deviceServiceFeign;

    @KafkaListener(topics = TopicConstant.OPS_CONFIG_TOPIC + "#{kafkaUtil.kafkaVersionSuffix}", groupId = TopicConstant.SET_POS_AUTHORIZED_AMOUNT_GROUPId)
    public void onlineMessageConsumer(ConsumerRecord<Object, Object> messageRecord) {

        if (messageRecord == null
                || messageRecord.value() == null) {
            return;
        }

        String message = messageRecord.value().toString();
        GetConfigMsgFromOpsDTO getConfigMsgFromOpsDTO = JSON.parseObject(message, GetConfigMsgFromOpsDTO.class);

        log.info("===>>>PosAuthorizedAmountDistributeListener.onlineMessageConsumer getConfigMsgFromOpsDTO : {}",
                JSON.toJSONString(getConfigMsgFromOpsDTO));

        if (getConfigMsgFromOpsDTO == null
                || StringUtils.isBlank(getConfigMsgFromOpsDTO.getMsgId())
                || !ConfigMsgActionEnum.POS_PRE_SALE_AMOUNT.getActionName().equalsIgnoreCase(getConfigMsgFromOpsDTO.getAction())) {

            log.info("===>>>PosAuthorizedAmountDistributeListener.onlineMessageConsumer 进入到了运维推送的消息有误的分支！");

            return;
        }

        String msgId = getConfigMsgFromOpsDTO.getMsgId();
        String redisResult = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getSetPosAuthorizedAmountKey(msgId));
        if (StringUtils.isNotBlank(redisResult) &&
                JSON.parseObject(redisResult, DownConfigMsgToOpsDTO.class) != null) {
            DownConfigMsgToOpsDTO downConfigMsgToOpsDTO = JSON.parseObject(redisResult, DownConfigMsgToOpsDTO.class);
            SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTO = JSON.parseObject(JSON.toJSONString(downConfigMsgToOpsDTO.getParam()), SetPosAuthorizedAmountDTO.class);
            OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity = new OpPosAuthorizedAmountDistributeEntity();
            opPosAuthorizedAmountDistributeEntity.setId(setPosAuthorizedAmountDTO.getId());
            if (StringUtils.isNotBlank(getConfigMsgFromOpsDTO.getUpdateTime())) {
                opPosAuthorizedAmountDistributeEntity.setUpdateTime(Long.valueOf(getConfigMsgFromOpsDTO.getUpdateTime()));
            }
            if (getConfigMsgFromOpsDTO.getParam() != null
                    && ConfigMsgParamEnum.ACCEPT.getParamName().equalsIgnoreCase(JSON.parseObject(JSON.toJSONString(getConfigMsgFromOpsDTO.getParam()), String.class))) {

                log.info("===>>>PosAuthorizedAmountDistributeListener.onlineMessageConsumer 进入到了设置成功的分支！");

                opPosAuthorizedAmountDistributeEntity.setStatus(1);

                com.autel.cloud.pile.base.dto.pos.SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTOPos = new com.autel.cloud.pile.base.dto.pos.SetPosAuthorizedAmountDTO();
                setPosAuthorizedAmountDTOPos.setMsgId(msgId);
                setPosAuthorizedAmountDTOPos.setPileSn(downConfigMsgToOpsDTO.getSn());
                setPosAuthorizedAmountDTOPos.setAuthorizedAmount(new BigDecimal(setPosAuthorizedAmountDTO.getPreCurrency()).divide(OpPosAuthorizedAmountDistributeServiceImpl._100, 0, BigDecimal.ROUND_HALF_UP));
                deviceServiceFeign.setPosAuthorizedAmount(setPosAuthorizedAmountDTOPos);
                kafkaTemplate.send(TopicConstant.OPS_CTRL_RESP_TOPIC + KafkaUtil.KAFKA_VERSION_SUFFIX, downConfigMsgToOpsDTO.getSn(), JSON.toJSONString(this.buildSendTerminal(downConfigMsgToOpsDTO, opPosAuthorizedAmountDistributeEntity)));
            } else {

                log.info("===>>>PosAuthorizedAmountDistributeListener.onlineMessageConsumer 进入到了设置失败的分支！");

                opPosAuthorizedAmountDistributeEntity.setStatus(0);
                kafkaTemplate.send(TopicConstant.OPS_CTRL_RESP_TOPIC + KafkaUtil.KAFKA_VERSION_SUFFIX, downConfigMsgToOpsDTO.getSn(), JSON.toJSONString(this.buildSendTerminal(downConfigMsgToOpsDTO, opPosAuthorizedAmountDistributeEntity)));

            }
            opPosAuthorizedAmountDistributeService.saveOrUpdatePosAuthorizedAmountDistribute(opPosAuthorizedAmountDistributeEntity);
        }
    }


    /**
     * @param downConfigMsgToOpsDTO
     * @param opPosAuthorizedAmountDistributeEntity
     * @return
     * @function 构建推送消息至前端的协议格式 （运维协议数据格式）
     */
    private SendTerminal buildSendTerminal(DownConfigMsgToOpsDTO downConfigMsgToOpsDTO, OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity) {

        log.info("===>>>PosAuthorizedAmountDistributeListener.buildSendTerminal downConfigMsgToOpsDTO : {} and opPosAuthorizedAmountDistributeEntity : {}",
                JSON.toJSONString(downConfigMsgToOpsDTO),
                JSON.toJSONString(opPosAuthorizedAmountDistributeEntity));

        SendTerminal sendTerminal = new SendTerminal();
        sendTerminal.setType(TopicConstant.SET_POS_AUTHORIZED_AMOUNT_TYPE);

        SetPosAuthorizedAmount setPosAuthorizedAmount = new SetPosAuthorizedAmount();
        setPosAuthorizedAmount.setMsgId(downConfigMsgToOpsDTO.getMsgId());
        setPosAuthorizedAmount.setFlag(opPosAuthorizedAmountDistributeEntity.getStatus());
        setPosAuthorizedAmount.setPileSn(downConfigMsgToOpsDTO.getSn());

        sendTerminal.setData(setPosAuthorizedAmount);
        sendTerminal.setUserId(downConfigMsgToOpsDTO.getAppUser());
        sendTerminal.setMsgId(downConfigMsgToOpsDTO.getMsgId());

        return sendTerminal;
    }
}
