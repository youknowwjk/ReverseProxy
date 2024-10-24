package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpGeoDirectionsMapper;
import com.autel.cloud.pile.base.domain.repository.OpGeoDirectionsRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpGeoDirectionsEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 地理位置指引 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Service
public class OpGeoDirectionsRepositoryImpl extends ServiceImpl<OpGeoDirectionsMapper, OpGeoDirectionsEntity> implements OpGeoDirectionsRepository {

}
