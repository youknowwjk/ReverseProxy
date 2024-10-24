package com.autel.cloud.pile.base.domain.convert;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class ObjectMapperWrapper {

    private static final ObjectMapper objectMapper = new ObjectMapper();

    static {
        objectMapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
    }

    private ObjectMapperWrapper() {
    }

    public static <T> T readValue(String content, Class<T> valueType) throws JsonProcessingException {
        return objectMapper.readValue(content, valueType);
    }

}
