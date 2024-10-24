package com.autel.cloud.pile.base.config;

import com.alibaba.fastjson.parser.ParserConfig;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import javax.annotation.Resource;

/**
 * @ClassName RedisUtil
 * @Author A22121
 * @Description
 * @Date 2022/3/15 12:17
 * @Version 0.0.1-SNAPSHOT
 */
@Configuration
@AutoConfigureAfter(RedisAutoConfiguration.class)
public class RedisConfig {

    @Resource
    JedisConnectionFactory factory;

    static {
        ParserConfig.getGlobalInstance().setAutoTypeSupport(true);
    }

    @Bean
    @Qualifier("redisTemplates")
    public RedisTemplate<String, Object> redisTemplate() {
        RedisTemplate<String, Object> redisTemplate = new RedisTemplate<>();
        redisTemplate.setConnectionFactory(factory);
        RedisSerializer<String> keySerializer = new StringRedisSerializer();
        RedisSerializer<Object> valueSerializer = new FastJsonRedisSerializer<>();
        // key采用字符串反序列化对象
        redisTemplate.setKeySerializer(keySerializer);
        // value也采用字符串反序列化对象
        // 原因：管道操作，是对redis命令的批量操作，各个命令返回结果可能类型不同
        // 可能是 Boolean类型 可能是String类型 可能是byte[]类型 因此统一将结果按照String处理
        redisTemplate.setValueSerializer(valueSerializer);
        redisTemplate.setHashKeySerializer(keySerializer);
        redisTemplate.setHashValueSerializer(valueSerializer);
        return redisTemplate;
    }

    @Bean
    public FeignConfiguration feignConfiguration(){
        return new FeignConfiguration();
    }
}
