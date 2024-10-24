package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.BaseAdminClient;
import com.autel.cloud.pile.bill.vo.UserSetVO;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Maps;
import io.jsonwebtoken.lang.Collections;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Component
@Slf4j
public class AppUserServiceAdapter {

    @Resource
    private BaseAdminClient baseAdminClient;

    private final LoadingCache<Long, UserSetVO> userSetCache = CacheBuilder.newBuilder()
            .expireAfterWrite(30, TimeUnit.MINUTES)
            .initialCapacity(10)
            .maximumSize(100)
            .build(new CacheLoader<Long, UserSetVO>() {
                @Override
                public UserSetVO load(Long id) {
                    return loadUserSettings(id);
                }
            });

    private final LoadingCache<Long, Locale> userLocalCache = CacheBuilder.newBuilder()
            .expireAfterWrite(30, TimeUnit.MINUTES)
            .initialCapacity(10)
            .maximumSize(100)
            .build(new CacheLoader<Long, Locale>() {
                @Override
                public Locale load(Long id) {
                    return loadUserLocaleSettings(id);
                }
            });

    public UserSetVO getUserSettings(Long userId) {
        try {
            return userSetCache.get(userId);
        } catch (ExecutionException e) {
            log.error("loadUserLocaleSettings", e);
        }
        return loadUserSettings(userId);
    }

    /**
     * 获取用户自定义货币单位 语言等信息
     *
     * @param userId 用户ID
     * @return
     */
    public Locale getUserLocaleSettings(Long userId) {
        try {
            return userLocalCache.get(userId);
        } catch (ExecutionException e) {
            log.error("loadUserLocaleSettings", e);
        }
        return loadUserLocaleSettings(userId);
    }


    /**
     * 获取用户自定义货币单位 语言等信息
     *
     * @param userId 用户ID
     * @return
     */
    public Locale loadUserLocaleSettings(Long userId) {
        String language = loadUserSettings(userId).getLanguage();
        Locale locale;
        String[] lan = language.split("-");
        if (lan.length > 1) {
            locale = new Locale(lan[0], lan[1]);
        } else {
            locale = new Locale(lan[0]);
        }
        return locale;
    }

    /**
     * 获取用户自定义货币单位 语言等信息
     *
     * @param userId 用户ID
     * @return
     */
    public UserSetVO loadUserSettings(Long userId) {
        Map<Long, UserSetVO> userSettings = getUserSettings(java.util.Collections.singletonList(userId));
        if (!Collections.isEmpty(userSettings)) {
            return userSettings.getOrDefault(userId, UserSetVO.builder().language("en-US").build());
        }
        return UserSetVO.builder().language("en-US").build();
    }

    /**
     * 获取用户自定义货币单位 语言等信息
     *
     * @param userIdList
     * @return
     */
    public Map<Long, UserSetVO> getUserSettings(List<Long> userIdList) {
        Map<Long, UserSetVO> userSignInfoMap = Maps.newHashMap();
        try {
            List<String> userIdStrList = userIdList.stream()
                    .map(String::valueOf)
                    .collect(Collectors.toList());
            Result<List<UserSetVO>> userSetVOResult = baseAdminClient.queryUserSet(userIdStrList);
            log.info("调取用户自定义货币语言单位：{}", JSON.toJSONString(userSetVOResult));
            if (userSetVOResult.getCode() == 200 && userSetVOResult.getData() != null) {
                List<UserSetVO> data = userSetVOResult.getData();
                for (UserSetVO datum : data) {
                    userSignInfoMap.put(datum.getUserId(), datum);
                }
            }
        } catch (Exception e) {
            log.error("调取用户自定义货币语言单位失败", e);
        }
        return userSignInfoMap;
    }
}
