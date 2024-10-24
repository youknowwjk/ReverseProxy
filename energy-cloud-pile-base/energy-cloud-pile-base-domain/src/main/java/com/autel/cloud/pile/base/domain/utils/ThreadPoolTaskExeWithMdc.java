package com.autel.cloud.pile.base.domain.utils;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.ThreadPoolExecutor;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/10 20:14
 */
@Configuration
public class ThreadPoolTaskExeWithMdc {


    private static final int AVAILABLE_THREADS =  Runtime.getRuntime().availableProcessors();

    public ThreadPoolTaskExecutor taskExecutor() {

        ThreadPoolTaskExecutor taskExecutor = new ThreadPoolMdcWrapper();

        //核心线程数，默认为1
        taskExecutor.setCorePoolSize(AVAILABLE_THREADS);

        //最大线程数，默认为Integer.MAX_VALUE
        taskExecutor.setMaxPoolSize(AVAILABLE_THREADS*2);

        //队列最大长度，一般需要设置值>=notifyScheduledMainExecutor.maxNum；默认为Integer.MAX_VALUE
        taskExecutor.setQueueCapacity(1000);

        //线程池维护线程所允许的空闲时间，默认为60s
        taskExecutor.setKeepAliveSeconds(60);

        //线程池对拒绝任务（无线程可用）的处理策略
        taskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.AbortPolicy());

        // 初始化线程池
        taskExecutor.initialize();

        return  taskExecutor;
    }

}
