package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseImageMapper;
import com.autel.cloud.pile.base.domain.repository.OpEvseImageRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseImageEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 充电设备图片 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Service
public class OpEvseImageRepositoryImpl extends ServiceImpl<OpEvseImageMapper, OpEvseImageEntity> implements OpEvseImageRepository {

}
