package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.PaymentCardRepository;
import com.autel.cloud.pile.base.dto.PaymentCardManagerDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.TbCardManageMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TBCardManageEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;

@Service
public class PaymentCardRepositoryImpl extends ServiceImpl<TbCardManageMapper, TBCardManageEntity> implements PaymentCardRepository {

    @Override
    public TBCardManageEntity queryPaymentCardDetail(PaymentCardManagerDTO cardManagerDTO) {
        LambdaQueryWrapper<TBCardManageEntity>  cardWrapper = Wrappers.lambdaQuery(TBCardManageEntity.class)
                .eq(TBCardManageEntity::getUserId,cardManagerDTO.getUserId()).eq(TBCardManageEntity::getDefaultCard,1).eq(TBCardManageEntity::getDeleted, 0);
        return this.baseMapper.selectOne(cardWrapper);
    }

    @Override
    public PaymentCardManagerDTO getPaymentCardInfoByPaymentId(String paymentId) {
        LambdaQueryWrapper<TBCardManageEntity>  cardWrapper = Wrappers.lambdaQuery(TBCardManageEntity.class)
                .eq(TBCardManageEntity::getPaymentId, paymentId);
        TBCardManageEntity cardManageEntity = this.baseMapper.selectOne(cardWrapper);
        if(cardManageEntity != null){
            PaymentCardManagerDTO paymentCardManagerDTO = new PaymentCardManagerDTO();
            BeanUtils.copyProperties(cardManageEntity, paymentCardManagerDTO);
            return paymentCardManagerDTO;
        }
        return null;
    }
}
