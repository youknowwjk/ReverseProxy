package com.autel.cloud.pile.base.domain.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

/**
 * 发票配置
 *
 * @author A22598
 * @date 2023/06/27
 */
@Configuration
@RefreshScope
@ConfigurationProperties(prefix = "invoice")
@Data
public class InvoiceProperties {
    /**
     * 使用欧洲地址的国家
     */
    private List<String> euInfoCountryList = new ArrayList<>();
}
