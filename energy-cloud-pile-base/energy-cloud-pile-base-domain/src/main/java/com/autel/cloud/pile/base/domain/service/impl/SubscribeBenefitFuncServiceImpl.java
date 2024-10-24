package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.service.SubscribeBenefitFuncService;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.dto.subscribe.BenefitEditDto;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @description
 * @auther A23204
 * @datetime 2023/12/7 14:04
 */
@Service
@Slf4j
public class SubscribeBenefitFuncServiceImpl implements SubscribeBenefitFuncService {

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Autowired
    private RedisUtil redisUtil;

    @Override
    public Result<Boolean> benefitFuncEdit(BenefitEditDto benefitEditDto) {

        log.info("SubscribeBenefitFuncService.benefitFuncEdit start, serviceId:{}", benefitEditDto.getServiceId());

        // 新的权益-功能点控制更新
        subscribePileRightsService.benefitEdit(benefitEditDto);

        // 删除旧的缓存, 权益id对应的function id列表；
        redisUtil.del(RedisKeyConstant.BENEFIT_ID_FUNCTION_LIST_PREFIX + benefitEditDto.getServiceId());

        // 重新加载 功能点-控制点集合 缓存
        subscribePileRightsService.reSetFunctionIdBenefitList();

        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<Boolean> updateServiceIdBySellerId(String sellerId) {
        subscribePileRightsService.updateServiceIdBySellerId(sellerId);
        return Result.ofSucceed(Boolean.TRUE);
    }
}
