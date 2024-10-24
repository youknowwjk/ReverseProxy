package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.PaymentCardManagerDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TBCardManageEntity;
import com.baomidou.mybatisplus.extension.service.IService;

public interface PaymentCardRepository extends IService<TBCardManageEntity> {

    TBCardManageEntity queryPaymentCardDetail(PaymentCardManagerDTO cardManagerDTO);

    PaymentCardManagerDTO getPaymentCardInfoByPaymentId(String paymentId);
}
