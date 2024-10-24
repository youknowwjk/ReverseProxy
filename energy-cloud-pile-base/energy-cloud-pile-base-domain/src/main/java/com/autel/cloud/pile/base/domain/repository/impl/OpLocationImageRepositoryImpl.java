package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationImageMapper;
import com.autel.cloud.pile.base.domain.repository.OpLocationImageRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationImageEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 场站图片 服务实现类ASs
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Service
public class OpLocationImageRepositoryImpl extends ServiceImpl<OpLocationImageMapper, OpLocationImageEntity> implements OpLocationImageRepository {

}
