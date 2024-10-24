package com.autel.cloud.pile.base.domain.strategy;

import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;

/**
 * 分配计算策略
 *
 * @Author temp
 * @Date 2022/12/14 15:21
 */
public interface DistributeStrategy {
    void handleUseStrategy(OncePerRequestContext context);
    void execute(OncePerRequestContext context);
    void handleDelivery(OncePerRequestContext context);
}
