package com.autel.cloud.pile.base.domain.repository.impl;


import com.autel.cloud.pile.base.domain.repository.OpPileV2gParamsSettingRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpPileV2gParamsSettingMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileV2gParamsSettingEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 桩V2G参数设置 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-10-17
 */
@Service
public class OpPileV2gParamsSettingRepositoryImpl extends ServiceImpl<OpPileV2gParamsSettingMapper, OpPileV2gParamsSettingEntity> implements OpPileV2gParamsSettingRepository {

    private final OpPileV2gParamsSettingMapper opPileV2gParamsSettingMapper;

    public OpPileV2gParamsSettingRepositoryImpl(OpPileV2gParamsSettingMapper opPileV2gParamsSettingMapper) {
        this.opPileV2gParamsSettingMapper = opPileV2gParamsSettingMapper;
    }
}
