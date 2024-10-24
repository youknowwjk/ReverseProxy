package com.autel.cloud.pile.base.domain.utils;


import java.util.concurrent.*;


public class AutelThreadUtils {
    private static final int AVAILABLE_THREADS =  Runtime.getRuntime().availableProcessors();

    private AutelThreadUtils(){

    }
    public static final ExecutorService THREAD_POOL = new ThreadPoolExecutor(
            //核心线程池大小
            AVAILABLE_THREADS,
            //最大核心线程池大小（CPU密集型，根据CPU核数设置）
            AVAILABLE_THREADS+1,
            //超时了没有人调用就会释放
            30,
            //超时单位
            TimeUnit.SECONDS,
            //阻塞队列
            new LinkedBlockingQueue<>(1024),
            //线程工厂，创建线程的
            Executors.defaultThreadFactory(),
            //池子满了，还有人进来，不处理这个人的，抛出异常
            new ThreadPoolExecutor.AbortPolicy());
    public static final ExecutorService SIGN_ENERGY_THREAD_POOL = Executors.newSingleThreadExecutor();


}
