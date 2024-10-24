package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.home.feign.HomePileClient;
import com.autel.cloud.home.feign.UserVehicleVO;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.vo.UserVehicle;
import com.autel.cloud.pile.base.vo.UserVehicle;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestParam;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

@Slf4j
@Component
public class PileHomeServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private HomePileFeignClient homePileFeignClient;
    @Resource
    private HomePileClient homePileClient;


    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;
    private final LoadingCache<Long, UserVehicle> userVehicleCache = CacheBuilder.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .initialCapacity(100)
            .maximumSize(1000)
            .build(new CacheLoader<Long, UserVehicle>() {
                @Override
                public UserVehicle load(Long id) {
                    return selectById(id);
                }
            });



    /**
     * 查询家桩是否绑定
     */
    public Boolean queryBind(String sn) {
        log.info("queryBind {}", sn);
        Result<Boolean> timeOfDelivery = homePileFeignClient.queryBind(sn);
        log.info("queryBind result {}", JSON.toJSONString(timeOfDelivery));
        return Optional.ofNullable(nullableHandle(timeOfDelivery)).orElse(false);
    }

    public Set<String> batchQueryBind(Collection<String> sns) {
        try {
            log.info("batchQueryBind {}", JSON.toJSONString(sns));
            Set<String> sns1 = new HashSet<>(sns);
            Result<Set<String>>  result = homePileClient.batchQueryBind(sns1);
            log.info("batchQueryBind result {}", JSON.toJSONString(result));
            return Optional.ofNullable(nullableHandle(result)).orElse(Collections.emptySet());
        } catch (Exception e) {
            log.error("batchQueryBind", e);
        }
        return Collections.emptySet();
    }

    /**
     * 查询我的车辆列表（enode 授权了的车辆列表）
     */
    public List<UserVehicle> queryUserVehicleByUserId(Long userId) {
        log.info("queryUserVehicleByUserId {}", userId);
        Result<List<UserVehicle>> result = homePileClient.queryUserVehicleByUserId(userId);
        log.info("queryUserVehicleByUserId result {}", JSON.toJSONString(result));
        return Optional.ofNullable(nullableHandle(result)).orElse(Collections.emptyList());
    }

    /**
     * 查询我的车辆列表
     */
    public List<UserVehicleVO> queryUserVehicles(Long userId) {
        log.info("queryUserVehicles {}", userId);
        Result<List<UserVehicleVO>> result = homePileClient.queryUserVehicles(userId);
        log.info("queryUserVehicles result {}", JSON.toJSONString(result));
        return Optional.ofNullable(nullableHandle(result)).orElse(Collections.emptyList());
    }

    public UserVehicle selectById(Long userVehicleId) {
        if (Objects.isNull(userVehicleId)) {
            return null;
        }
        log.info("selectById {}", userVehicleId);
        Result<UserVehicle> result = homePileClient.queryUserVehicle(userVehicleId);
        log.info("selectById result {}", JSON.toJSONString(result));
        return nullableHandle(result);
    }

    public UserVehicle getUserVehicleByUserVehicleId(Long userVehicleId) {
        log.info("getUserVehicleByUserVehicleId {}", userVehicleId);
        try {
            return userVehicleCache.get(userVehicleId);
        } catch (ExecutionException e) {
            log.error("getUserVehicleByUserVehicleId", e);
        }
        return selectById(userVehicleId);
    }
}
