package com.autel.cloud.pile.base.domain.listener;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.excel.annotation.ExcelProperty;
import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.read.listener.PageReadListener;
import com.alibaba.excel.util.ListUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * @author A22208
 */
@Slf4j
public class BatchPageReadListener<T> extends PageReadListener<T> {

    private List<T> cachedDataList = ListUtils.newArrayListWithExpectedSize(BATCH_COUNT);
    /**
     * consumer
     */
    private final Consumer<List<T>> consumer;

    public BatchPageReadListener(Consumer<List<T>> consumer) {
        super(consumer);
        this.consumer = consumer;
    }

    @Override
    public void invoke(T data, AnalysisContext context) {
        // 如果一行Excel数据均为空值，则不装载该行数据
        if (isLineNullValue(data)) {
            return;
        }
        cachedDataList.add(data);
        if (cachedDataList.size() >= BATCH_COUNT) {
            consumer.accept(cachedDataList);
            cachedDataList = ListUtils.newArrayListWithExpectedSize(BATCH_COUNT);
        }
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext context) {
        if (CollUtil.isEmpty(cachedDataList)) {
            return;
        }
        consumer.accept(cachedDataList);
    }

    /**
     * 判断整行单元格数据是否均为空
     */
    private boolean isLineNullValue(T data) {
        if (data instanceof String) {
            return Objects.isNull(data);
        }
        try {
            List<Field> fields = Arrays.stream(data.getClass().getDeclaredFields())
                    .filter(f -> f.isAnnotationPresent(ExcelProperty.class))
                    .collect(Collectors.toList());
            List<Boolean> lineNullList = new ArrayList<>(fields.size());
            for (Field field : fields) {
                field.setAccessible(true);
                Object value = field.get(data);
                if (Objects.isNull(value)) {
                    lineNullList.add(Boolean.TRUE);
                } else {
                    lineNullList.add(Boolean.FALSE);
                }
            }
            return lineNullList.stream().allMatch(Boolean.TRUE::equals);
        } catch (Exception e) {
            log.error("读取数据行[{}]解析失败: {}", data, e.getMessage());
        }
        return true;
    }

}
