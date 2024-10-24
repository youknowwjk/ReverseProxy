package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.service.PaymentService;
import com.autel.cloud.pile.base.infrastructure.mapper.PaymentMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.PaymentEntity;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * PaymentServiceImpl
 *
 * @author Xzhi
 * @date 2021-12-07 19:43
 */
@Service
public class PaymentServiceImpl extends ServiceImpl<PaymentMapper, PaymentEntity> implements PaymentService {

    @Override
    public PaymentEntity selectByStationIdAndSellerId(Long sellerId, Long stationId) {
        QueryWrapper<PaymentEntity> qw = new QueryWrapper<>();
        qw.eq(BaseConstant.SELLER_ID, sellerId).eq("station_id", stationId).last("limit 1");
        return this.getOne(qw);
    }

    @Override
    public List<PaymentEntity> selectListBySellerId(Long sellerId) {
        QueryWrapper<PaymentEntity> qw = new QueryWrapper<>();
        qw.eq(BaseConstant.SELLER_ID, sellerId);
        return this.list(qw);
    }

    @Override
    public PaymentEntity selectAccountIdBySellerIdAndCode(Long sellerId, Integer code) {
        QueryWrapper<PaymentEntity> qw = new QueryWrapper<>();
        qw.eq(BaseConstant.SELLER_ID, sellerId);
        qw.eq("code", code);
        return this.getOne(qw);
    }

    @Override
    public PaymentEntity saveReturnPojo(PaymentEntity payment) {
        boolean res = this.save(payment);
        if (res) {
            return payment;
        }
        return null;
    }

    @Override
    public Boolean updateStripeStatusById(PaymentEntity payment) {
        return updateById(payment);
    }
}