package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpImageEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 场站图片 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
public interface OpImageRepository extends IService<OpImageEntity> {

    List<OpImageEntity> getImagesByIds(List<Long> imageIds);
}
