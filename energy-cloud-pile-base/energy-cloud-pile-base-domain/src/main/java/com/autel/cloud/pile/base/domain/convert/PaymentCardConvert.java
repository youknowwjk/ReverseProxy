package com.autel.cloud.pile.base.domain.convert;

import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.pile.base.dto.PaymentCardManagerDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TBCardManageEntity;
import org.springframework.beans.BeanUtils;

public class PaymentCardConvert {

    private PaymentCardConvert() {

    }

    /**
     * 转化
     *
     * @param
     * @return
     */
    public static PaymentCardManagerDTO paymentCardManagerDTO(TBCardManageEntity paymentCardEntity) {
        if (null == paymentCardEntity) {
            throw new MessageCodeException(PileBaseEnum.NON_DEFAULT_PAYMENT_CARD);
        }
        PaymentCardManagerDTO paymentCardManagerDTO = new PaymentCardManagerDTO();
        BeanUtils.copyProperties(paymentCardEntity, paymentCardManagerDTO);
        return paymentCardManagerDTO;
    }
}
