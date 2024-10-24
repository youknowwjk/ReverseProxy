package com.autel.cloud.pile.base.infrastructure.util;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
@Slf4j
public class TimeZoneUtil {

    /**
     * 构造方法私有化
     */
    private TimeZoneUtil() {
    }

    public static String DATE_PATTERN = "yyyy-MM-dd";
    public static String DATE_TIME_PATTERN = "yyyy-MM-dd HH:mm:ss";

    /**
     * 毫秒转目标时区时间
     *
     * @param timestamp 毫秒时间戳
     * @param zoneId    地区ID
     * @return 地区ID的本地时间
     */
    public static LocalDateTime millsToLocalDateTime(Long timestamp, String zoneId) {
        if (Objects.isNull(timestamp)) {
            return null;
        }
        if (StringUtils.isBlank(zoneId)) {
            zoneId = ZoneId.systemDefault().getId();
        }
        Instant instant = Instant.ofEpochMilli(timestamp);
        return LocalDateTime.ofInstant(instant, ZoneId.of(zoneId));
    }


    public static String format(Long timestamp, String zoneId, String datePattern) {
        if (Objects.isNull(timestamp)) {
            return null;
        }
        if (StringUtils.isBlank(zoneId)) {
            zoneId = ZoneId.systemDefault().getId();
        }
        Instant instant = Instant.ofEpochMilli(timestamp);
        return LocalDateTime.ofInstant(instant, ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern(datePattern));
    }

    /**
     * 日期根据时区获取时间戳
     *
     * @param dateTime 日期("yyyy-MM-dd HH:mm:ss")
     * @param zoneId   时区ID
     * @return
     */
    public static Long getTimestamp(String dateTime, String zoneId) {
        LocalDateTime localDateTime = LocalDateTime.parse(dateTime, DateTimeFormatter.ofPattern(DATE_TIME_PATTERN));
        ZoneId zoneIdResult = getZoneId(zoneId);
        long timestamp = localDateTime.atZone(zoneIdResult).toInstant().toEpochMilli();
        return timestamp;
    }


    /**
     * 获取时区
     *
     * @param zoneIdStr
     * @return
     */
    public static ZoneId getZoneId(String zoneIdStr) {
        ZoneId zoneId = null;
        if (StringUtils.isBlank(zoneIdStr)) {
            zoneId = ZoneId.systemDefault();
            return zoneId;
        }
        try {
            zoneId = ZoneId.of(zoneIdStr);
        } catch (Exception ex) {
            log.error("============= getZoneId by zoneIdStr: {} exception", zoneIdStr, ex);
        }
        if (null == zoneId) {
            return ZoneId.systemDefault();
        }
        return zoneId;
    }
}

