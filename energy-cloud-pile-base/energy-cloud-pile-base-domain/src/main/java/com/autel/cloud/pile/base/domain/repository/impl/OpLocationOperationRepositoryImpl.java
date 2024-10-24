package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationOperationMapper;
import com.autel.cloud.pile.base.domain.repository.OpLocationOperationRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOperationEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 充电站运营 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Service
public class OpLocationOperationRepositoryImpl extends ServiceImpl<OpLocationOperationMapper, OpLocationOperationEntity> implements OpLocationOperationRepository {

}
