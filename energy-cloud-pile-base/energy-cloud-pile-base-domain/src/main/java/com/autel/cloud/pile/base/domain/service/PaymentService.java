package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.PaymentEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 支付方式服务
 *
 * @author Xzhi
 * @date 2021-12-07 19:35
 */
public interface PaymentService extends IService<PaymentEntity> {

    /**
     * 根据站点ID和商家ID查询支付方式
     */
    PaymentEntity selectByStationIdAndSellerId(Long sellerId, Long stationId);

    /**
     * 根据商家ID查询支付方式
     */
    List<PaymentEntity> selectListBySellerId(Long sellerId);

    /**
     * 根据商家ID和支付方式查询商家支付账户id
     * @param sellerId
     * @param code
     * @return
     */
    PaymentEntity selectAccountIdBySellerIdAndCode(Long sellerId, Integer code);

    PaymentEntity saveReturnPojo(PaymentEntity payment);

    Boolean updateStripeStatusById(PaymentEntity payment);
}