package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpCostRuleDistributeService;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.enums.CostRuleDistributeEfficientFailureReason;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static com.autel.cloud.pile.base.constant.AmqpConstant.TARIFF_DISPATCH_DELAY_QUEUE;

/**
 * @author
 * @since
 */
@Slf4j
@Component
public class DelayEventListener {

    @Resource
    OpCostRuleDistributeService opCostRuleDistributeService;

    /**
     * 计费规则下发延迟消息队列处理
     */
    @RabbitListener(queues = TARIFF_DISPATCH_DELAY_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}")
    public void consumerDelayMsg(Message message, Channel channel) throws IOException {
        log.info("============= 延迟消息监听：{}", new String(message.getBody()));
        long deliveryTag = message.getMessageProperties().getDeliveryTag();
        String pileSn = new String(message.getBody());
        Result<OpCostRuleDistributeDTO> opCostRuleDistributeDTOResult = opCostRuleDistributeService.selectOpCostRuleDistributeByPileSn(pileSn);
        log.info("=========== the opCostRuleDistributeDTOResult in the consumerDelayMsg function:{}", JSONObject.toJSONString(opCostRuleDistributeDTOResult));
        if (null == opCostRuleDistributeDTOResult || null == opCostRuleDistributeDTOResult.getData()) {
            channel.basicAck(deliveryTag, Boolean.FALSE);
            return;
        }
        OpCostRuleDistributeDTO opCostRuleDistributeDTO = opCostRuleDistributeDTOResult.getData();
        //计费规则下发或者生效状态不为null,则桩侧已返回计费规则的结果，无需处理
        if (null != opCostRuleDistributeDTO.getSuccessFlag() && null != opCostRuleDistributeDTO.getEfficientFlag()) {
            log.info("============ because the successFlag is null:{} or efficientFlag is null:{}, direct return", null != opCostRuleDistributeDTO.getSuccessFlag(), null != opCostRuleDistributeDTO.getEfficientFlag());
            channel.basicAck(deliveryTag, Boolean.FALSE);
            return;
        }
        List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList = new ArrayList<>();
        if (null == opCostRuleDistributeDTO.getSuccessFlag()) {
            log.info("============= because of the successFlag is null, update successFlag is 0 begin ============");
            opCostRuleDistributeDTO.setSuccessFlag(0);
            opCostRuleDistributeDTO.setFailureReason(CostRuleDistributeEfficientFailureReason.TIME_OUT.getCode() + "");
            opCostRuleDistributeDTO.setDistributeTime(null);
            opCostRuleDistributeDTOList.add(opCostRuleDistributeDTO);
            opCostRuleDistributeService.updateOpCostRuleDistributeByPileSn(opCostRuleDistributeDTOList);
            log.info("============= because of the successFlag is null, update successFlag is 0 end ============");
        } else if (null == opCostRuleDistributeDTO.getEfficientFlag()) {
            log.info("============= because of the efficientFlag is null, update successFlag is 0 begin ============");
            opCostRuleDistributeDTO.setEfficientFlag(0);
            opCostRuleDistributeDTO.setEfficientFailureReason(CostRuleDistributeEfficientFailureReason.TIME_OUT.getCode() + "");
            opCostRuleDistributeDTO.setEfficientTime(null);
            opCostRuleDistributeDTOList.add(opCostRuleDistributeDTO);
            opCostRuleDistributeService.updateOpCostRuleEfficientByPileSn(opCostRuleDistributeDTOList);
            log.info("============= because of the efficientFlag is null, update successFlag is 0 end ============");
        }
        channel.basicAck(deliveryTag, Boolean.FALSE);
    }
}