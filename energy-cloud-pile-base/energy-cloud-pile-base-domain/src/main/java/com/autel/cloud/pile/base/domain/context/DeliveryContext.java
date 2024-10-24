package com.autel.cloud.pile.base.domain.context;

import com.autel.cloud.pile.base.domain.strategy.DeliveryStrategy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 * @Author temp
 * @Date 2022/12/14 17:41
 */
@Component
@Slf4j
public class DeliveryContext {
    private final Map<String, DeliveryStrategy> strategyMap = new HashMap<>();

    public DeliveryContext(Map<String, DeliveryStrategy> strategyMap) {
        this.strategyMap.clear();
        strategyMap.forEach((k, v) -> this.strategyMap.put(k, v));
    }

    /**
     * 选择策略，找不到对应策略抛异常
     *
     * @param strategy
     * @return
     */
    public DeliveryStrategy getStrategy(String strategy) {
        DeliveryStrategy result = strategyMap.get(strategy);
        if (result == null) {
            log.info("getStrategy,strategy={}", strategy);
            throw new RuntimeException("strategy could not find in the context.");
        }
        return result;
    }
}
