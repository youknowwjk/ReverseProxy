package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.webhook.LarkClient;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.CommonDeliveryDTO;
import com.autel.cloud.pile.base.dto.FirstDownThenUpDTO;
import com.autel.cloud.pile.base.util.DlbUtil;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import com.autel.cloud.smart.charge.constant.SmartChargeConstant;
import com.autel.cloud.smart.charge.dto.MessageRetryDTO;
import com.autel.cloud.smart.monitor.dto.EvseMonitorMistakeDTO;
import com.autel.cloud.smart.monitor.dto.RedeliveryDTO;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.*;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2023/7/17 16:18
 * @Since 1.0.0
 */
@Component
@Slf4j
public class ReAcknowledgeListener extends CommonMessageRetryService {

    @Value("${spring.rabbitmq.version.suffix:}")
    public String rabbitmqVersionSuffix;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private MonitorFeignClient monitorFeignClient;
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;
    @Value("${webhook.lark.alarmKey:7f4bee11-146e-410e-83a7-53128c27ec82}")
    private String alarmKey;

    @Resource
    private LarkClient larkClient;

    @RabbitListener(bindings = @QueueBinding(
            exchange = @Exchange(value = AmqpConstant.PILE_BASE_FIRST_DOWN_THEN_UP_EXCHANGE + "#{reAcknowledgeListener.rabbitmqVersionSuffix}", type = "x-delayed-message", arguments = @Argument(name = "x-delayed-type", value = "direct")),
            value = @Queue(value = AmqpConstant.PILE_BASE_FIRST_DOWN_THEN_UP_QUEUE + "#{reAcknowledgeListener.rabbitmqVersionSuffix}"),
            key = AmqpConstant.PILE_BASE_FIRST_DOWN_THEN_UP_ROUTE))
    public void reAcknowledgeListener(Message message, Channel channel) throws IOException {
        ThreadContext.put(SmartChargeConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        long tag = message.getMessageProperties().getDeliveryTag();
        channel.basicAck(tag, false);
        super.initMessage(message);
    }

    @Override
    protected void execute(MessageRetryDTO dto) {
        String msg = dto.getBodyMsg();
        log.info("reAcknowledgeListener,msg={}", msg);
        if (!StringUtils.hasText(msg)) {
            return;
        }
        FirstDownThenUpDTO queueDto = JSON.parseObject(msg, FirstDownThenUpDTO.class);
        Long groupId = queueDto.getGroupId();
        String key = RedisKeyConstant.getPileBaseFirstDownThenUpKey(groupId);
        String redisValue = stringRedisTemplate.opsForValue().get(key);
        if (!StringUtils.hasText(redisValue)) {
            log.info("reAcknowledgeListener,redisValue is null.");
            return;
        }
        FirstDownThenUpDTO redisDto = JSON.parseObject(redisValue, FirstDownThenUpDTO.class);
        if (queueDto.getRequestId() == null || !queueDto.getRequestId().equals(redisDto.getRequestId())) {
            log.info("reAcknowledgeListener,requestId is not equal.");
            return;
        }
        List<OpLocationPileGroupDeliveryVO> deliveryVOList = redisDto.getDeliveryVOList();
        List<OpLocationPileGroupDeliveryVO> downList = deliveryVOList.stream().filter(vo -> vo.getFirstDownThenUp() != null && vo.getFirstDownThenUp() == 1).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(downList)) {
            log.info("reAcknowledgeListener,downList is empty.");
            return;
        }
        List<OpLocationPileGroupDeliveryVO> reAckList = this.reAck(downList);
        if (CollectionUtils.isEmpty(reAckList)) {
            //已经完全下降
            CommonDeliveryDTO commonDeliveryDTO = new CommonDeliveryDTO();
            commonDeliveryDTO.setGroupId(groupId);
            commonDeliveryDTO.setType(11);
            try {
                Boolean delivery = opLocationPileGroupService.deliveryByGroupId(commonDeliveryDTO);
                if (delivery == null|| !delivery) {
                    log.info("reAcknowledgeListener,delivery={}", delivery);
                }
            } catch (Exception e) {
                log.info("reAcknowledgeListener,delivery,Exception={}", e.getMessage());
            }
            //stringRedisTemplate.delete(key);
        } else {
            //未达到最大重试次数
            if (dto.checkRetryCount()) {
                log.info("reAcknowledgeListener,重试消息：{}", JSON.toJSONString(dto));
                retrySend(dto);
            } else {
                //达到最大重试次数，删除缓存
                stringRedisTemplate.delete(key);
                log.info("reAcknowledgeListener,已达到最大重试次数，业务数据：{}", JSON.toJSONString(dto));
                List<EvseMonitorMistakeDTO> list = new ArrayList<>(reAckList.size());
                //未下降完成按指定值下发
                reAckList.stream().forEach(vo -> {
                    EvseMonitorMistakeDTO mistakeDTO = new EvseMonitorMistakeDTO();
                    mistakeDTO.setTimeValue(vo.getUpload());
                    mistakeDTO.setDeliveryValue(vo.getUpload());
                    mistakeDTO.setLast(true);
                    mistakeDTO.setType(2);
                    mistakeDTO.setEvseSn(vo.getEvseSn());
                    mistakeDTO.setGroupId(groupId);
                    list.add(mistakeDTO);
                });
                //调用算法分配
                RedeliveryDTO redeliveryDTO = new RedeliveryDTO();
                redeliveryDTO.setGroupId(groupId);
                redeliveryDTO.setSource(11);
                redeliveryDTO.setEvseList(list);
                try {
                    Boolean redelivery = opLocationPileGroupService.redelivery(redeliveryDTO);
                    if (redelivery == null || !redelivery) {
                        log.info("reAcknowledgeListener,redelivery={}", redelivery);
                    }
                } catch (Exception e) {
                    log.info("reAcknowledgeListener,redelivery,Exception={}", e.getMessage());
                }
            }
        }
    }

    private List<OpLocationPileGroupDeliveryVO> reAck(List<OpLocationPileGroupDeliveryVO> downList) {
        List<OpLocationPileGroupDeliveryVO> resultList = new ArrayList<>(downList.size());
        List<String> evseSnList = downList.stream().map(OpLocationPileGroupDeliveryVO::getEvseSn).collect(Collectors.toList());
        Result<List<OpEvseMeterUploadDTO>> listResult = monitorFeignClient.queryNewMeters(evseSnList);
        if (listResult != null && listResult.getCode() == HttpStatus.OK.value() && !CollectionUtils.isEmpty(listResult.getData())) {
            List<OpEvseMeterUploadDTO> resultData = listResult.getData();
            Map<String, OpEvseMeterUploadDTO> collect = resultData.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
            for (OpLocationPileGroupDeliveryVO deliveryVO : downList) {
                String evseSn = deliveryVO.getEvseSn();
                String unit = deliveryVO.getChargingUpUnit();
                OpEvseMeterUploadDTO uploadDTO = collect.get(evseSn);
                if (uploadDTO != null && EvseDeviceStatusEnum.CHARGING.getName().equals(uploadDTO.getEvseStatusName())) {
                    Integer r = DlbUtil.firstDownThenUp(unit, DlbUtil.getSettingValue(deliveryVO), DlbUtil.getUploadValue(unit, uploadDTO));
                    if (r != null && r == 1) {
                        log.info("reAcknowledgeListener,deliveryVO={}", JSON.toJSONString(deliveryVO));
                        OpLocationPileGroupDeliveryVO vo = JSON.parseObject(JSON.toJSONString(deliveryVO), OpLocationPileGroupDeliveryVO.class);
                        vo.setUpload(DlbUtil.getUploadValue(unit, uploadDTO));
                        resultList.add(vo);
                    }
                }
            }
        }
        return resultList;
    }

    @Override
    protected void successCallback(MessageRetryDTO dto) {

    }

    @Override
    protected void failCallback(MessageRetryDTO dto) {
        this.larkClient.sendMessage(String.format("先降后升MQ消费异常\n环境：%s\n消息体：%s", System.getenv("NACOS_NS"), dto.getBodyMsg()), alarmKey);
    }

    @Override
    public Integer getDelay() {
        return 15000;
    }
}
