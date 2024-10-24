package com.autel.cloud.pile.base.domain.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.math.BigDecimal;

/**
 * BigDecimal序列化
 *
 * @author A22598
 * @date 2023/07/11
 */
public class BigDecimalSerializer extends JsonSerializer<BigDecimal> {
    @Override
    public void serialize(BigDecimal value, JsonGenerator jsonGenerator, SerializerProvider serializers) throws IOException {
        if (value != null) {
            jsonGenerator.writeString(value.stripTrailingZeros().toPlainString());
        }
    }
}
