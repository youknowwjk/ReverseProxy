package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpPowerLimitRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpPowerLimitMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 充电设备组合功率限制开启表 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-07-02
 */
@Service
public class OpPowerLimitRepositoryImpl extends ServiceImpl<OpPowerLimitMapper, OpPowerLimitEntity> implements OpPowerLimitRepository {

    @Override
    public List<OpPowerLimitEntity> findAllTurnOn() {
        return this.list(new LambdaQueryWrapper<OpPowerLimitEntity>()
                .select(OpPowerLimitEntity::getId, OpPowerLimitEntity::getCombinationId)
                .eq(OpPowerLimitEntity::getTurnOn, 1)
                .eq(OpPowerLimitEntity::getDeleted, 0));
    }
}
