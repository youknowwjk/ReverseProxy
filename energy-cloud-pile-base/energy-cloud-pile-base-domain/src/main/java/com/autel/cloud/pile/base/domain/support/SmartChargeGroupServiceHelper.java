package com.autel.cloud.pile.base.domain.support;

import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileGroupMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;

@Component
public class SmartChargeGroupServiceHelper {


    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private OpLocationPileGroupMapper opLocationPileGroupMapper;


    public OpLocationPileGroupEntity get(Long groupId) {
        return null;
    }


    public OpLocationPileGroupEntity load(Long groupId) {
        return opLocationPileGroupMapper.selectById(groupId);
    }


}
