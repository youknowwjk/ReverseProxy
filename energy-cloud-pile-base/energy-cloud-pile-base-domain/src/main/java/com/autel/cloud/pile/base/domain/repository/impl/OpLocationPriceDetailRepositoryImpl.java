package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpLocationPriceDetailRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPriceDetailMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPriceDetailEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

/**
 * @Author temp
 * @Date 2023/3/7 15:35
 */
@Service
@Slf4j
public class OpLocationPriceDetailRepositoryImpl extends ServiceImpl<OpLocationPriceDetailMapper, OpLocationPriceDetailEntity> implements OpLocationPriceDetailRepository {
    @Override
    public boolean insertBatch(List<OpLocationPriceDetailEntity> entityList) {
        return this.saveBatch(entityList);
    }

    @Override
    public boolean updateList(List<OpLocationPriceDetailEntity> entityList, Long priceId) {
        int remove = this.deleteBatch(priceId);
        log.info("updateList,remove={}", remove);
        return this.insertBatch(entityList);
    }

    @Override
    public List<OpLocationPriceDetailEntity> findList(Long priceId) {
        return this.findList(Collections.singletonList(priceId));
    }

    @Override
    public List<OpLocationPriceDetailEntity> findList(List<Long> priceIds) {
        return this.list(new LambdaQueryWrapper<OpLocationPriceDetailEntity>()
                .in(OpLocationPriceDetailEntity::getPriceId, priceIds)
                .eq(OpLocationPriceDetailEntity::getDeleted, 0));
    }

    @Override
    public int deleteBatch(Long priceId) {
        return this.deleteBatch(Collections.singletonList(priceId));
    }

    @Override
    public int deleteBatch(List<Long> priceIds) {
        return this.getBaseMapper().delete(new LambdaQueryWrapper<OpLocationPriceDetailEntity>()
                .in(OpLocationPriceDetailEntity::getPriceId, priceIds)
                .eq(OpLocationPriceDetailEntity::getDeleted, 0));
    }
}
