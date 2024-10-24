package com.autel.cloud.pile.base.infrastructure.amqp;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import javax.annotation.PostConstruct;

@Configuration
@Data
public class RabbitBean {
    @Value("${spring.rabbitmq.version.suffix:}")
    public String rabbitmqVersionSuffix; // 对该变量使用配置文件参数控制


    public static String RABBITMQ_VERSION_SUFFIX;

    @PostConstruct
    public void init(){
        RABBITMQ_VERSION_SUFFIX = rabbitmqVersionSuffix;
    }

}