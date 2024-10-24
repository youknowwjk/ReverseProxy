package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;

import java.util.List;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/26 10:24
 */
public interface SubscribeImportAutoBindCreateLicenceService {

    /**
     * description: saveLicence
     * version: 1.0
     * date: 2023/8/26 14:36
     * author: A23204
     *
     * @param entityList
     * @return void
     */
    void saveLicence(List<TbLenBindRelationEntity> entityList);
}
