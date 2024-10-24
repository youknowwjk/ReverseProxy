package com.autel.cloud.pile.base.infrastructure.cache;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

/**
 * @Author temp
 * @Date 2022/12/14 12:12
 */
@Component
public class OpLocationPileGroupCache {

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String,Object> redisTemplates;
}
