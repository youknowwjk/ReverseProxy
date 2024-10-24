package com.autel.cloud.pile.base.config;

import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

@EnableAsync
@Configuration
public class TaskPoolConfig {
    @Value("${core.pool.size:10}")
    private Integer corePooleSize;
    @Value("${max.pool.size:10}")
    private Integer maxPooleSize;
    @Value("${queue.capacity:200}")
    private Integer queueCapacity;
    @Value("${alive.time:60}")
    private Integer aliveTime;


    @Bean("pileBaseTaskExecutor")
    public Executor taskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePooleSize);
        executor.setMaxPoolSize(maxPooleSize);
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(aliveTime);
        executor.setThreadNamePrefix("pileBaseTaskExecutor-");
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        return executor;
    }

    @Bean("smartChargeGroupTaskExecutor")
    public ThreadPoolTaskExecutor  asyncTaskExecutor() {
        ThreadPoolTaskExecutor threadPoolTaskExecutor = new ThreadPoolTaskExecutor();
        threadPoolTaskExecutor.setThreadNamePrefix("smartChargeGroup-query-");//线程前缀
        threadPoolTaskExecutor.setCorePoolSize(corePooleSize);//核心线程数
        threadPoolTaskExecutor.setMaxPoolSize(maxPooleSize);//最大线程数
        threadPoolTaskExecutor.setQueueCapacity(20);//等待队列
        threadPoolTaskExecutor.setKeepAliveSeconds(aliveTime);//线程池维护线程所允许的空闲时间,单位为秒
        threadPoolTaskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());// 线程池对拒绝任务(无线程可用)的处理策略
        threadPoolTaskExecutor.initialize();
        return threadPoolTaskExecutor;
    }

    @Bean("emsTaskExecutor")
    public ThreadPoolTaskExecutor  emsTaskExecutor() {
        ThreadPoolTaskExecutor emsTaskExecutor = new ThreadPoolTaskExecutor();
        emsTaskExecutor.setThreadNamePrefix("smartChargeGroup-query-");//线程前缀
        emsTaskExecutor.setCorePoolSize(corePooleSize);//核心线程数
        emsTaskExecutor.setMaxPoolSize(maxPooleSize);//最大线程数
        emsTaskExecutor.setQueueCapacity(20);//等待队列
        emsTaskExecutor.setKeepAliveSeconds(aliveTime);//线程池维护线程所允许的空闲时间,单位为秒
        emsTaskExecutor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());// 线程池对拒绝任务(无线程可用)的处理策略
        emsTaskExecutor.setThreadNamePrefix("emsTaskExecutor-");
        emsTaskExecutor.initialize();
        return emsTaskExecutor;
    }

    @Bean("schedulingChargeTaskExecutor")
    public Executor schedulingChargeTaskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePooleSize);
        executor.setMaxPoolSize(maxPooleSize);
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(aliveTime);
        executor.setThreadNamePrefix("schedulingChargeTaskExecutor-");
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        return executor;
    }

    @Bean("bizQueryTaskExecutor")
    public ThreadPoolTaskExecutor bizQueryTaskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(corePooleSize);
        executor.setMaxPoolSize(maxPooleSize);
        executor.setQueueCapacity(queueCapacity);
        executor.setKeepAliveSeconds(aliveTime);
        executor.setThreadNamePrefix("bizQueryTaskExecutor-");
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());

        //request上下文、TID传递
        executor.setTaskDecorator(r-> {
            RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
            return RunnableWrapper.of(() -> {
                RequestContextHolder.setRequestAttributes(requestAttributes);
                try {
                    r.run();
                } finally {
                    RequestContextHolder.resetRequestAttributes();
                }
            });
        });
        return executor;
    }
}