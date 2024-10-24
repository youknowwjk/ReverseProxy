package com.autel.cloud.pile.base;

import com.autel.cloud.infrastructure.sysconfig.annotation.AutelSysApplication;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableAsync;

@AutelSysApplication
@SpringBootApplication
@ComponentScan(basePackages = { "com.autel.cloud.base","com.autel.cloud.pile.base"})
@MapperScan("com.autel.cloud.pile.base.infrastructure.mapper")
@EnableFeignClients("com.autel.cloud")
@EnableAsync
@EnableDiscoveryClient
public class EnergyCloudPileBaseApplication {

    public static void main(String[] args) {
        SpringApplication.run(EnergyCloudPileBaseApplication.class, args);
    }

}
