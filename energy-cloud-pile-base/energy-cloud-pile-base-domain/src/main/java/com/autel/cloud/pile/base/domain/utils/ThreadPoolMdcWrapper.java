package com.autel.cloud.pile.base.domain.utils;

import org.slf4j.MDC;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.util.concurrent.ListenableFuture;

import java.util.concurrent.Callable;
import java.util.concurrent.Future;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/10 19:53
 */
public class ThreadPoolMdcWrapper extends ThreadPoolTaskExecutor {

    public ThreadPoolMdcWrapper() {

    }

    @Override
    public void execute(Runnable task) {
        super.execute(ThreadMdcUtil.wrap(task, MDC.getCopyOfContextMap()));
    }

    @Override
    public void execute(Runnable task, long startTimeout) {
        super.execute(ThreadMdcUtil.wrap(task, MDC.getCopyOfContextMap()), startTimeout);
    }

    @Override
    public <T> Future<T> submit(Callable<T> task) {
        return super.submit(ThreadMdcUtil.wrap(task, MDC.getCopyOfContextMap()));
    }

    @Override
    public Future<?> submit(Runnable task) {
        return super.submit(ThreadMdcUtil.wrap(task, MDC.getCopyOfContextMap()));
    }

    @Override
    public ListenableFuture<?> submitListenable(Runnable task) {
        return super.submitListenable(ThreadMdcUtil.wrap(task, MDC.getCopyOfContextMap()));
    }

    @Override
    public <T> ListenableFuture<T> submitListenable(Callable<T> task) {
        return super.submitListenable(ThreadMdcUtil.wrap(task, MDC.getCopyOfContextMap()));
    }


}
