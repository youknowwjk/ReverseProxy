package com.autel.cloud.pile.base.domain.support;

import cn.hutool.core.util.StrUtil;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.support.SendResult;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.util.concurrent.ListenableFuture;
import org.springframework.util.concurrent.ListenableFutureCallback;

import javax.annotation.Resource;

/**
 * <p>Describe Class...</p>
 *
 * @Author A22168
 * @Date 2022/6/20 11:01
 */
@Slf4j
@Service
@Data
public class KafkaProducerService {
    @Qualifier("kafkaTemplate")
    @Resource
    private KafkaTemplate<String, String> kafkaTemplate;

    @Value("${spring.kafka.version.suffix:}")
    public String kafkaVersionSuffix;

    /**
     * 发送消息（异步）
     *
     * @param topic   主题
     * @param key     键
     * @param message 消息内容
     */
    public void sendMessageAsync(String topic, String key, String message) {
        if (StrUtil.isBlank(topic)) {
            return;
        }
        String finalTopic = topic;
        // 加上版本后缀
        if (StrUtil.isNotBlank(kafkaVersionSuffix) && !topic.endsWith(kafkaVersionSuffix)) {
            finalTopic = topic + kafkaVersionSuffix;
        }
        String topicSend = finalTopic;
        ListenableFuture<SendResult<String, String>> future = null;
        if (!StringUtils.isEmpty(key)) {
            future = kafkaTemplate.send(topicSend, key, message);
        } else {
            future = kafkaTemplate.send(topicSend, message);
        }

        //添加回调
        future.addCallback(new ListenableFutureCallback<SendResult<String, String>>() {
            @Override
            public void onFailure(Throwable throwable) {
                log.error("sendMessageAsync failure in KafkaProducerService function! topic : {}, message: {}", topicSend, message);
            }

            @Override
            public void onSuccess(SendResult<String, String> stringStringSendResult) {
                log.info("sendMessageAsync success in KafkaProducerService function! topic: {}, message: {}", topicSend, message);
            }
        });
    }
}
