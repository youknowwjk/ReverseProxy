package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpImageMapper;
import com.autel.cloud.pile.base.domain.repository.OpImageRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpImageEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 场站图片 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Service
public class OpImageRepositoryImpl extends ServiceImpl<OpImageMapper, OpImageEntity> implements OpImageRepository {

    @Autowired
    private OpImageMapper opImageMapper;

    @Override
    public List<OpImageEntity> getImagesByIds(List<Long> imageIds) {
        if (CollectionUtils.isNotEmpty(imageIds)) {
            return opImageMapper.getImagesByIds(imageIds);
        }
        return Lists.newArrayList();
    }
}
