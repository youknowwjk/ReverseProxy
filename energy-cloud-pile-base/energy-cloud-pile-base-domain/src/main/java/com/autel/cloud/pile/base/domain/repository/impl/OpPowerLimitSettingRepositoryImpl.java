package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpPowerLimitSettingRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpPowerLimitSettingMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitSettingEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 功率限制分时上限设置 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-07-02
 */
@Service
public class OpPowerLimitSettingRepositoryImpl extends ServiceImpl<OpPowerLimitSettingMapper, OpPowerLimitSettingEntity> implements OpPowerLimitSettingRepository {

}
