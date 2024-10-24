package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpLocationPriceRepository;
import com.autel.cloud.pile.base.dto.LocationPriceDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPriceMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPriceEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

/**
 * @Author temp
 * @Date 2023/3/7 15:24
 */
@Service
public class OpLocationPriceRepositoryImpl extends ServiceImpl<OpLocationPriceMapper, OpLocationPriceEntity> implements OpLocationPriceRepository {
    @Override
    public List<OpLocationPriceEntity> getByName(LocationPriceDTO dto) {
        return this.list(new LambdaQueryWrapper<OpLocationPriceEntity>()
                .select(OpLocationPriceEntity::getId, OpLocationPriceEntity::getLocationId, OpLocationPriceEntity::getSellerId)
                .eq(OpLocationPriceEntity::getSellerId, dto.getSellerId())
                .eq(OpLocationPriceEntity::getLocationId, dto.getLocationId())
                .eq(OpLocationPriceEntity::getName, dto.getName())
                .eq(OpLocationPriceEntity::getDeleted, 0));
    }

    @Override
    public boolean insert(OpLocationPriceEntity entity) {
        return this.save(entity);
    }

    @Override
    public OpLocationPriceEntity findOne(Long id) {
        return this.getById(id);
    }

    @Override
    public boolean updateEntity(OpLocationPriceEntity entity) {
        return this.updateById(entity);
    }

    @Override
    public boolean delete(Long id) {
        return this.deleteBatch(Collections.singletonList(id)) > 0;
    }

    @Override
    public List<OpLocationPriceEntity> findList(Long locationId, Long sellerId) {
        return this.list(new LambdaQueryWrapper<OpLocationPriceEntity>()
                .eq(OpLocationPriceEntity::getSellerId, sellerId)
                .eq(OpLocationPriceEntity::getLocationId, locationId)
                .eq(OpLocationPriceEntity::getDeleted, 0));
    }

    @Override
    public int deleteBatch(List<Long> ids) {
        return this.getBaseMapper().deleteBatchIds(ids);
    }
}
