package com.autel.cloud.pile.base.domain.utils;


import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * @author A20364
 */
@Component
@Slf4j
public class Redis2Util {

    @Resource
    StringRedisTemplate stringRedisTemplate;
    @Resource
    private RedisTemplate<String, Object> redisTemplate;

    private static final String FAIL_MSG = "--> 操作redis失败,key为:";

    private static final String FAIL_2_MSG = "--> 操作redis失败";

    public boolean set(String key, String value) {
        try {
            stringRedisTemplate.opsForValue().set(key, value);
            return true;
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
            return false;
        }
    }

    public boolean set(String key, String value, Long time) {
        try {
            stringRedisTemplate.opsForValue().set(key, value,time, TimeUnit.DAYS);
            return true;
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
            return false;
        }
    }

    public boolean set(String key, String value, Long time,TimeUnit timeUnit) {
        try {
            stringRedisTemplate.opsForValue().set(key, value,time, timeUnit);
            return true;
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
            return false;
        }
    }

    public boolean setnx(String key, String value, Long time) {
        try {
            return stringRedisTemplate.opsForValue().setIfAbsent(key, value,time, TimeUnit.DAYS);
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
            return false;
        }
    }

    public String get(String key) {
        return key == null ? null : stringRedisTemplate.opsForValue().get(key);
    }

    public void delete(String key) {
        try {
            stringRedisTemplate.delete(key);
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
        }
    }

    public void incr(String key) {
        try {
            stringRedisTemplate.opsForValue().increment(key);
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
        }
    }

    public void hdel(String key) {
        try {
            redisTemplate.delete(key);
        } catch (Exception e) {
            log.error(FAIL_MSG + key, e);
        }
    }

    /**
     * HashGet
     *
     * @param key  键 不能为null
     * @param item 项 不能为null
     * @return 值
     */
    public Object hget(String key, String item) {
        return redisTemplate.opsForHash().get(key, item);
    }

    /**
     * 获取hashKey对应的所有键值
     *
     * @param key 键
     * @return 对应的多个键值
     */
    public Map<Object, Object> hmget(String key) {
        return redisTemplate.opsForHash().entries(key);
    }

    /**
     * HashSet
     *
     * @param key 键
     * @param map 对应多个键值
     * @return true 成功 false 失败
     */
    public boolean hmset(String key, Map<Object, Object> map) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            return true;
        } catch (Exception e) {
            log.error(FAIL_2_MSG, e);
            return false;
        }
    }

    /**
     * HashSet 并设置时间
     *
     * @param key  键
     * @param map  对应多个键值
     * @param time 时间(秒)
     * @return true成功 false失败
     */
    public boolean hmset(String key, Map<String, Object> map, long time) {
        try {
            redisTemplate.opsForHash().putAll(key, map);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error(FAIL_2_MSG, e);
            return false;
        }
    }

    /**
     * 向一张hash表中放入数据,如果不存在将创建
     *
     * @param key   键
     * @param item  项
     * @param value 值
     * @return true 成功 false失败
     */
    public boolean hset(String key, String item, Object value) {
        try {
            redisTemplate.opsForHash().put(key, item, value);
            return true;
        } catch (Exception e) {
            log.error(FAIL_2_MSG, e);
            return false;
        }
    }

    /**
     * 向一张hash表中放入数据,如果不存在将创建
     *
     * @param key   键
     * @param item  项
     * @param value 值
     * @param time  时间(秒) 注意:如果已存在的hash表有时间,这里将会替换原有的时间
     * @return true 成功 false失败
     */
    public boolean hset(String key, String item, Object value, long time) {
        try {
            redisTemplate.opsForHash().put(key, item, value);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error(FAIL_2_MSG, e);
            return false;
        }
    }

    /**
     * 删除hash表中的值
     *
     * @param key  键 不能为null
     * @param item 项 可以使多个 不能为null
     */
    public void hdel(String key, Object... item) {
        redisTemplate.opsForHash().delete(key, item);
    }

    /**
     * 判断hash表中是否有该项的值
     *
     * @param key  键 不能为null
     * @param item 项 不能为null
     * @return true 存在 false不存在
     */
    public boolean hHasKey(String key, String item) {
        return redisTemplate.opsForHash().hasKey(key, item);
    }

    /**
     * 指定缓存失效时间
     *
     * @param key  键
     * @param time 时间(秒)
     * @return
     */
    public boolean expire(String key, long time) {
        try {
            if (time > 0) {
                redisTemplate.expire(key, time, TimeUnit.SECONDS);
            }
            return true;
        } catch (Exception e) {
            log.error(FAIL_2_MSG, e);
            return false;
        }
    }

    /**
     * 新增集合元素
     * @param key
     * @param value
     */
    public void saveSetCache(String key,String value) {
        redisTemplate.opsForSet().add(key,value);
    }

    /**
     * 判断元素是否存在集合中
     * @param key
     * @param paramKey
     * @return
     */
    public Boolean isSetMember(String key,String paramKey) {
        return redisTemplate.opsForSet().isMember(key,paramKey);
    }

    /**
     * 获取整个Set的数据
     */
    public Set<Object> getSetAllMember(String key) {
        return redisTemplate.opsForSet().members(key);
    }

}
