package com.autel.cloud.pile.base.domain.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @Author temp
 * @Date 2022/11/17 19:44
 */
@Configuration
@ConfigurationProperties(prefix = "hubject")
@Data
public class HubjectConfig {
    private Boolean enable;
}
