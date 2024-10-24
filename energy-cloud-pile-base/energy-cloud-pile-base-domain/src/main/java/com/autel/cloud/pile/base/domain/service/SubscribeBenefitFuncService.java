package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.subscribe.BenefitEditDto;

/**
 * @description
 * @auther A23204
 * @datetime 2023/12/7 14:03
 */
public interface SubscribeBenefitFuncService {

    Result<Boolean> benefitFuncEdit(BenefitEditDto benefitEditDto);

    Result<Boolean> updateServiceIdBySellerId(String sellerId);
}
