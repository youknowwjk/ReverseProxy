package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.bill.dto.TimezoneUserDTO;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneId;

@Service
@Slf4j
public class UserTimezoneServiceImpl {

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    StringRedisTemplate stringRedisTemplate;

    @Autowired
    private IBillFeignClient billFeignClient;

    public Boolean setTimezone(TimezoneUserDTO timezoneUserDTO) {
        String redisKey = String.format(RedisKeyConstant.TIMEZONE_OF_USER, timezoneUserDTO.getUserId());
        return redisUtil.set(redisKey, JSON.toJSONString(timezoneUserDTO));
    }

    /**
     * 获取用户设置的时区
     *
     * @param userId
     * @return
     */
    public TimezoneUserDTO getTimeZoneInfoOfUser(Long userId) {
        log.info("======== the getTimeZoneInfoOfUser function invoked, userId:{}", userId);
        if (null == userId) {
            log.error("======== because userId is null, return.");
            return null;
        }
        /*String userKey = String.format(RedisKeyConstant.TIMEZONE_OF_USER, userId);
        Object timezoneObject = redisUtil.get(userKey);
        log.info("========== the TimeZoneInfoDTO in the redis: {}", timezoneObject);
        if (null != timezoneObject) {
            return JSON.parseObject(timezoneObject.toString(), TimezoneUserDTO.class);
        }*/
        try {
            Result<TimezoneUserDTO> timezoneUserDTOResult = billFeignClient.getTimezone(userId);
            log.info("============ getTimezone invoked, the timezoneUserDTOResult: {}", JSON.toJSONString(timezoneUserDTOResult));
            if (HttpStatus.SC_OK == timezoneUserDTOResult.getCode() && null != timezoneUserDTOResult.getData()) {
                return timezoneUserDTOResult.getData();
            }
        } catch (Exception ex) {
            log.error("======== invoke getTimezone of billFeignClient exception", ex);
        }
        // 返回默认的时区
        String zoneId = ZoneId.systemDefault().getId();
        String offsetTime = LocalDateTime.now(Clock.systemUTC()).atZone(ZoneId.of(zoneId)).getOffset().getId();
        if (offsetTime.equalsIgnoreCase("Z")) {
            offsetTime = "+00:00";
        }
        String timezone = "UTC" + offsetTime;
        TimezoneUserDTO timezoneUserDTO = TimezoneUserDTO.builder()
                .zoneId(zoneId)
                .timezone(timezone)
                .userId(userId)
                .build();
        log.info("=========== the timezoneUserDTO:{}", JSON.toJSONString(timezoneUserDTO));
        return timezoneUserDTO;
    }
}
