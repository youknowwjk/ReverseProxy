package com.autel.cloud.pile.base.config;

import com.xxl.job.core.executor.impl.XxlJobSpringExecutor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Slf4j
@Configuration
public class XxlJobConfig {
    @Value("${xxl.job.admin.addresses}")
    private String adminAddresses;

    @Value("${xxl.job.executor.appname}")
    private String appName;

    @Value("${xxl.job.executor.port}")
    private int port;

    @Value("${xxl.job.executor.logpath}")
    private String logPath;

    @Value("${xxl.job.executor.logretentiondays}")
    private int logRetentionDays;


    //@Bean(initMethod = "start", destroyMethod = "destroy")
    @Bean/*(initMethod = "start", destroyMethod = "destroy")*/
    public XxlJobSpringExecutor xxlJobSpringExecutor() {
        try {
            XxlJobSpringExecutor xxlJobExecutor = new XxlJobSpringExecutor();
            log.info(">>>>>>>>>>> xxl-job config init.");
            xxlJobExecutor.setAdminAddresses(this.adminAddresses);
            xxlJobExecutor.setAppname(this.appName);
            xxlJobExecutor.setPort(this.port);
            xxlJobExecutor.setLogPath(this.logPath);
            xxlJobExecutor.setLogRetentionDays(this.logRetentionDays);
            return xxlJobExecutor;
        } catch (Exception ex) {
            log.error("xxljob error", ex);
            return null;
        }

    }
}