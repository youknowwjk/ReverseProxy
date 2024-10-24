package com.autel.cloud.pile.base.infrastructure.redis;

import javax.annotation.Resource;
import java.util.Map;
import java.util.Optional;

/**
 * @ClassName AbstractRedis
 * @Author A22121
 * @Description
 * @Date 2022/4/21 17:07
 * @Version 0.0.1-SNAPSHOT
 */
public abstract class AbstractRedis {

    @Resource
    protected RedisUtil redisUtil;

    protected void buildMap(String key, Map<String, String> monitorMap) {
        Optional<Map<Object, Object>> optional = Optional.ofNullable(redisUtil.getMapForHash(key));
        optional.ifPresent(objectObjectMap -> objectObjectMap.forEach((k, v) -> monitorMap.put(k.toString(), v.toString())));
    }

}
