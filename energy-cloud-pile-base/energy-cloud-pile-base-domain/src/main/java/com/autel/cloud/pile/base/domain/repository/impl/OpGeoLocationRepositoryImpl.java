package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.infrastructure.mapper.OpGeoLocationMapper;
import com.autel.cloud.pile.base.domain.repository.OpGeoLocationRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpGeoLocationEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 地理位置描述 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Service
public class OpGeoLocationRepositoryImpl extends ServiceImpl<OpGeoLocationMapper, OpGeoLocationEntity> implements OpGeoLocationRepository {

}
