package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.smart.charge.dto.MessageRetryDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;

import javax.annotation.Resource;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Objects;

/**
 * @Author temp
 * @Date 2022/9/22 21:53
 */
@Slf4j
public abstract class CommonMessageRetryService {
    @Resource
    private RabbitTemplate rabbitTemplate;

    /**
     * 初始化消息
     *
     * @param message
     */
    public void initMessage(Message message) {
        //log.info("{} 收到消息: {}，业务数据：{}", this.getClass().getName(), message.toString(), new String(message.getBody()));
        try {
            //封装消息
            MessageRetryDTO messageRetryDto = buildMessageRetryInfo(message);
            if (log.isInfoEnabled()) {
                //log.info("反序列化消息:{}", messageRetryDto.toString());
            }
            prepareAction(messageRetryDto);
        } catch (Exception e) {
            log.warn("处理消息异常，错误信息：", e);
        }
    }

    /**
     * 准备执行
     *
     * @param retryDto
     */
    protected void prepareAction(MessageRetryDTO retryDto) {
        try {
            execute(retryDto);
            doSuccessCallBack(retryDto);
        } catch (Exception e) {
            log.error("当前任务执行异常，业务数据：" + retryDto.toString(), e);
            //执行失败，计算是否还需要继续重试
            if (retryDto.checkRetryCount()) {
                if (log.isInfoEnabled()) {
                    log.info("重试消息:{}", retryDto.toString());
                }
                retrySend(retryDto);
            } else {
                if (log.isWarnEnabled()) {
                    log.warn("当前任务重试次数已经到达最大次数，业务数据：" + retryDto.toString(), e);
                }
                retryDto.setErrorMsg(e.getMessage());
                doFailCallBack(retryDto);
            }
        }
    }

    /**
     * 任务执行成功，回调服务(根据需要进行重写)
     *
     * @param messageRetryDto
     */
    private void doSuccessCallBack(MessageRetryDTO messageRetryDto) {
        try {
            successCallback(messageRetryDto);
        } catch (Exception e) {
            log.warn("执行成功回调异常，队列描述：{}，错误原因：{}", messageRetryDto.getSourceDesc(), e.getMessage());
        }
    }

    /**
     * 任务执行失败，回调服务(根据需要进行重写)
     *
     * @param messageRetryDto
     */
    private void doFailCallBack(MessageRetryDTO messageRetryDto) {
        try {
            messageRetryDto.setStatus(3);
            failCallback(messageRetryDto);
            saveMessageRetryInfo(messageRetryDto);
        } catch (Exception e) {
            log.warn("执行失败回调异常，队列描述：{}，错误原因：{}", messageRetryDto.getSourceDesc(), e.getMessage());
        }
    }

    /**
     * 执行任务
     *
     * @param dto
     */
    protected abstract void execute(MessageRetryDTO dto);

    /**
     * 成功回调
     *
     * @param dto
     */
    protected abstract void successCallback(MessageRetryDTO dto);

    /**
     * 失败回调
     *
     * @param dto
     */
    protected abstract void failCallback(MessageRetryDTO dto);

    /**
     * 指定延迟时间
     * 单位毫秒
     * @return
     */
    public Integer getDelay() {
        return 0;
    }

    /**
     * 构建消息补偿实体
     * @param message
     * @return
     */
    private MessageRetryDTO buildMessageRetryInfo(Message message){
        //如果头部包含补偿消息实体，直接返回
        Map<String, Object> messageHeaders = message.getMessageProperties().getHeaders();
        if(messageHeaders.containsKey("message_retry_info")){
            Object retryMsg = messageHeaders.get("message_retry_info");
            if(Objects.nonNull(retryMsg)){
                return JSON.parseObject(String.valueOf(retryMsg), MessageRetryDTO.class);
            }
        }
        //自动将业务消息加入补偿实体
        MessageRetryDTO messageRetryDto = new MessageRetryDTO();
        messageRetryDto.setBodyMsg(new String(message.getBody(), StandardCharsets.UTF_8));
        messageRetryDto.setExchangeName(message.getMessageProperties().getReceivedExchange());
        messageRetryDto.setRoutingKey(message.getMessageProperties().getReceivedRoutingKey());
        messageRetryDto.setQueueName(message.getMessageProperties().getConsumerQueue());
        messageRetryDto.setCreateTime(System.currentTimeMillis());
        return messageRetryDto;
    }

    /**
     * 异常消息重新入库
     * @param retryDto
     */
    protected void retrySend(MessageRetryDTO retryDto){
        //将补偿消息实体放入头部，原始消息内容保持不变
        MessageProperties messageProperties = new MessageProperties();
        messageProperties.setContentType(MessageProperties.CONTENT_TYPE_JSON);
        messageProperties.setHeader("message_retry_info", JSON.toJSONString(retryDto));
        Integer delay = this.getDelay();
        if (delay != null && delay > 0) {
            messageProperties.setDelay(delay);
        }
        if (retryDto.getRetryIntervalTime().longValue() > 0) {
            messageProperties.setDelay(retryDto.getRetryIntervalTime().intValue());
        }
        Message message = new Message(retryDto.getBodyMsg().getBytes(), messageProperties);
        rabbitTemplate.convertAndSend(retryDto.getExchangeName(), retryDto.getRoutingKey(), message);
    }



    /**
     * 将异常消息存储到数据库中
     * @param retryDto
     */
    private void saveMessageRetryInfo(MessageRetryDTO retryDto){

    }
}
