package com.autel.cloud.pile.base.domain.utils;

import org.apache.commons.codec.digest.DigestUtils;
import org.slf4j.MDC;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Callable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/10 19:48
 */
public class ThreadMdcUtil {

    private static final String LOG_TRACE_ID = "traceId";

    public static String createTraceId() {
        String uuid = UUID.randomUUID().toString();
        return DigestUtils.md5Hex(uuid).substring(8, 24);
    }

    public static void setTraceIdIfAbsent() {
        if (MDC.get(LOG_TRACE_ID) == null) {
            MDC.put(LOG_TRACE_ID, createTraceId());
        }
    }

    public static String getTraceId() {
        return MDC.get(LOG_TRACE_ID);
    }

    public static void setTraceId() {
        MDC.put(LOG_TRACE_ID, createTraceId());
    }

    public static void setTraceId(String traceId) {
        MDC.put(LOG_TRACE_ID, traceId);
    }

    public static void clear() {
        MDC.clear();
    }

    public static <T> Callable<T> wrap(final Callable<T> callable, final Map<String, String> context) {
        return () -> {
            if (context == null) {
                MDC.clear();
            } else {
                MDC.setContextMap(context);
            }
            setTraceIdIfAbsent();
            try {
                return callable.call();
            } finally {
                MDC.clear();
            }
        };
    }

    public static Runnable wrap(final Runnable runnable, final Map<String, String> context) {
        return () -> {
            if (context == null) {
                MDC.clear();
            } else {
                MDC.setContextMap(context);
            }
            setTraceIdIfAbsent();
            try {
                runnable.run();
            } finally {
                MDC.clear();
            }
        };
    }


}
