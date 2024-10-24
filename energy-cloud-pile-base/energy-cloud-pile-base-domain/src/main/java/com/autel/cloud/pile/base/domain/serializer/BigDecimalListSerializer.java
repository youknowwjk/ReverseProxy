package com.autel.cloud.pile.base.domain.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

public class BigDecimalListSerializer extends JsonSerializer<List<BigDecimal>> {
    @Override
    public void serialize(List<BigDecimal> value, JsonGenerator jsonGenerator, SerializerProvider serializers) throws IOException {
        if (value != null) {
            final String[] objects = value.stream().map(bigDecimal -> bigDecimal.stripTrailingZeros().toPlainString()).collect(Collectors.toList()).toArray(new String[]{});
            jsonGenerator.writeArray(objects, 0, objects.length);
        }
    }
}
