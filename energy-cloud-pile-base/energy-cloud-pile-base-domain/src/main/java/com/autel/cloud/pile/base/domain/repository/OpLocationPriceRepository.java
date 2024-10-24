package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.LocationPriceDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPriceEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @Author temp
 * @Date 2023/3/7 15:23
 */
public interface OpLocationPriceRepository extends IService<OpLocationPriceEntity> {
    List<OpLocationPriceEntity> getByName(LocationPriceDTO dto);

    boolean insert(OpLocationPriceEntity entity);

    OpLocationPriceEntity findOne(Long id);

    boolean updateEntity(OpLocationPriceEntity entity);

    boolean delete(Long id);

    List<OpLocationPriceEntity> findList(Long locationId, Long sellerId);

    int deleteBatch(List<Long> ids);
}
