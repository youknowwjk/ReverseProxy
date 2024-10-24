package com.autel.cloud.pile.base.domain.context;

import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.strategy.DistributeStrategy;
import com.autel.cloud.pile.base.enums.AllocationStrategyEnums;
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
public class DistributeContext {
    private final Map<String, DistributeStrategy> strategyMap = new HashMap<>();

    public DistributeContext(Map<String, DistributeStrategy> strategyMap) {
        this.strategyMap.clear();
        strategyMap.forEach((k, v) -> this.strategyMap.put(k, v));
    }

    /**
     * 选择策略，找不到对应策略抛异常
     *
     * @param strategy
     * @return
     */
    public DistributeStrategy getStrategy(String strategy) {
        String tmp = AllocationStrategyEnums.getCodeByValue(strategy);
        if (BaseConstant.INTELLIGENT_DISTRIBUTION.equalsIgnoreCase(tmp)) {
            tmp = BaseConstant.DISTRIBUTE_EVENLY;
        }
        DistributeStrategy result = strategyMap.get(tmp);
        if (result == null) {
            log.info("getStrategy,strategy={}", tmp);
            throw new RuntimeException("strategy could not find in the context.");
        }
        return result;
    }
}
