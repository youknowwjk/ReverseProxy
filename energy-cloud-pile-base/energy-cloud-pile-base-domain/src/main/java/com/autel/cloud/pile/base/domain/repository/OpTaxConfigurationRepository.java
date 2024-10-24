package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpTaxConfigurationEntity;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置 提供数据服务 接口
 */
public interface OpTaxConfigurationRepository extends IService<OpTaxConfigurationEntity> {

    /**
     * @param countryShortCode  国家简码
     * @param provinceShortCode 州/省简码
     * @param zipCode           邮政编码
     * @return Autel管理端-默认税率配置表 实体类
     * @function 根据国家简码，州/省简码，邮政编码获取Autel管理端-默认税率配置表的实体类数据
     */
    OpTaxConfigurationEntity getOpTaxConfigurationByStationLocation(String countryShortCode, String provinceShortCode, String zipCode);

    /**
     * @param currentPage 当前页码数
     * @param pageSize    每页数据量
     * @param keyword     搜索关键词
     * @return Autel管理端-默认税率配置数据列表
     * @function 分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）
     */
    Page<OpTaxConfigurationEntity> getOpTaxConfigurationPageList(Integer currentPage, Integer pageSize, String keyword);

    /**
     * @return 默认税率配置数据集合
     * @function 获得所有的默认税率配置数据
     */
    List<OpTaxConfigurationEntity> getAllOpTaxConfiguration();
}
