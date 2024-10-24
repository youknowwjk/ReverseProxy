package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.service.TestService;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

/**
 * @ClassName TestServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/6/29 22:53
 * @Version 0.0.1-SNAPSHOT
 */
@Service
public class TestServiceImpl implements TestService {

    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;

    @Override
    @Transactional(rollbackFor = {Exception.class})
    public Result<String> testException() {
        List<String> list = Arrays.asList("1", "2", "3");
        list.forEach(e -> {
            if (e.equals("2")) {
                throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_CHARGING);
            }
        });
        LambdaUpdateWrapper<OpLocationEvseEntity> evseUpdateWrapper = Wrappers.lambdaUpdate(OpLocationEvseEntity.class);
        evseUpdateWrapper.eq(OpLocationEvseEntity::getId, 1);
        evseUpdateWrapper.set(OpLocationEvseEntity::getDeleted, Boolean.TRUE);
        opLocationEvseRepository.update(evseUpdateWrapper);
        throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_CHARGING);
    }
}
