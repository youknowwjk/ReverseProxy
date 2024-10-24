package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.CardManagerDTO;
import com.autel.cloud.pile.base.dto.PaymentCardManagerDTO;
import com.autel.cloud.pile.base.vo.CardManagerV2VO;
import com.autel.cloud.pile.base.vo.CardManagerVO;

import java.util.List;

public interface PaymentCardService {

    Result<PaymentCardManagerDTO> queryPaymentCardDetail(PaymentCardManagerDTO cardManagerDTO);

    CardManagerV2VO bindCard(CardManagerDTO cardManagerDTO);

    Result<List<CardManagerVO>> bindCardList(Long userId);

    Result<Boolean> unbindCard(CardManagerDTO cardManagerDTO);

    Result<Boolean> setDefaultCard(CardManagerDTO cardManagerDTO);

    Result<Boolean> isBindingCard(String userId);

    Result<Boolean> isBindingCardCurrentUser(Long userId);

    Result<Boolean> defaultCardExpire(String userId);

    void payCardHistoryInit();

    Boolean updateRefuseCodeByPaymentId(String paymentId, String refuseCode);

    PaymentCardManagerDTO getPaymentCardInfoByPaymentId(String paymentId);

    Result<CardManagerV2VO> bindCardV2(CardManagerDTO cardManagerDTO);
//
//    Result<Boolean> unbindAllCard(Long userId);
}
