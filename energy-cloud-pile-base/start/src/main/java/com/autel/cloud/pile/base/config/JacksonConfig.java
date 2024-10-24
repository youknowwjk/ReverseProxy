package com.autel.cloud.pile.base.config;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.*;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import com.fasterxml.jackson.databind.ser.BeanSerializerModifier;
import com.fasterxml.jackson.databind.ser.std.NumberSerializers;
import com.fasterxml.jackson.databind.ser.std.ToStringSerializer;
import com.google.common.collect.ImmutableList;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.web.codec.CodecCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.http.codec.CodecConfigurer;
import org.springframework.http.codec.json.Jackson2JsonEncoder;
import org.springframework.http.converter.json.Jackson2ObjectMapperBuilder;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.io.IOException;
import java.math.BigInteger;
import java.util.List;

/**
 * 添加JacksonConfig配置全局序列化,统一将Long类型转为String类型,避免精度丢失
 *
 * @Author A22327
 * @Description
 * @Date 2022/3/9 17:08
 */
@Configuration
@Slf4j
public class JacksonConfig{

    private final Long  JS_MAX_VALUE = 9007199254740991L;

    @Bean
    public MappingJackson2HttpMessageConverter jackson2HttpMessageConverter() {
        final Jackson2ObjectMapperBuilder builder = new Jackson2ObjectMapperBuilder();
        builder.serializationInclusion(JsonInclude.Include.CUSTOM.NON_NULL);
        final ObjectMapper objectMapper = builder.build();

        ToStringSerializer stringSerializer = ToStringSerializer.instance;
        NumberSerializers.LongSerializer myLongSerializer = new NumberSerializers.LongSerializer(Long.class) {
            @Override
            public void serialize(Object value, JsonGenerator gen, SerializerProvider provider) throws IOException {
                Long l = (Long) value;
                if (l > JS_MAX_VALUE) {
                    stringSerializer.serialize(value, gen, provider);
                } else {
                    super.serialize(value, gen, provider);
                }
            }
        };

        SimpleModule simpleModule = new SimpleModule();
        simpleModule.addSerializer(Long.class, myLongSerializer);
        simpleModule.addSerializer(Long.TYPE, myLongSerializer);
        simpleModule.addSerializer(BigInteger.class, myLongSerializer);
        objectMapper.registerModule(simpleModule);
        // 忽略 transient 关键词属性
        objectMapper.configure(MapperFeature.PROPAGATE_TRANSIENT_MARKER, true);
        return new MappingJackson2HttpMessageConverter(objectMapper);
    }

}