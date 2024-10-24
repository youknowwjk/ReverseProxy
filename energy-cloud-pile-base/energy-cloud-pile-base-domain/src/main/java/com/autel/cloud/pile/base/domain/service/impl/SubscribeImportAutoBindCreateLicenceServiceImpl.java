package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.domain.service.SubscribeImportAutoBindCreateLicenceService;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/26 10:24
 */
@Service
public class SubscribeImportAutoBindCreateLicenceServiceImpl implements SubscribeImportAutoBindCreateLicenceService {

    @Autowired
    private TbLenBindRelationMapper tbLenBindRelationMapper;

    /**
     * description: saveLicence
     * version: 1.0
     * date: 2023/8/26 14:36
     * author: A23204
     *
     * @param entityList
     * @return void
     */
    @Override
    @Transactional
    public void saveLicence(List<TbLenBindRelationEntity> entityList) {
        // 桩维度保存licence
        entityList.forEach(tbLenBindRelationEntity -> tbLenBindRelationMapper.insert(tbLenBindRelationEntity));
    }
}
