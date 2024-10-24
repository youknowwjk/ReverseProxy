package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPriceDetailEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @Author temp
 * @Date 2023/3/7 15:34
 */
public interface OpLocationPriceDetailRepository extends IService<OpLocationPriceDetailEntity> {
    boolean insertBatch(List<OpLocationPriceDetailEntity> entityList);

    boolean updateList(List<OpLocationPriceDetailEntity> entityList, Long priceId);

    List<OpLocationPriceDetailEntity> findList(Long priceId);

    List<OpLocationPriceDetailEntity> findList(List<Long> priceIds);

    int deleteBatch(Long priceId);
    int deleteBatch(List<Long> priceIds);
}
