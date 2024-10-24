package com.autel.cloud.pile.base.domain.utils;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;

@Configuration
@Data
public class KafkaUtil {
    @Value("${spring.kafka.version.suffix:}")
    public String kafkaVersionSuffix; // 对该变量使用配置文件参数控制

    public static String KAFKA_VERSION_SUFFIX;

    @PostConstruct
    public void init(){
        KAFKA_VERSION_SUFFIX = kafkaVersionSuffix;
    }
}