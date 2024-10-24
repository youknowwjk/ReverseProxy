package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.repository.OpTaxConfigurationRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpTaxConfigurationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpTaxConfigurationEntity;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置 提供数据服务 实现类
 */
@Service
@Slf4j
public class OpTaxConfigurationRepositoryImpl extends ServiceImpl<OpTaxConfigurationMapper, OpTaxConfigurationEntity> implements OpTaxConfigurationRepository {

    @Autowired
    private OpTaxConfigurationMapper opTaxConfigurationMapper;

    /**
     * 标记条件为空
     */
    public static final String NULL_CONDITION = "NULL_CONDITION";

    /**
     * @param countryShortCode  国家简码
     * @param provinceShortCode 州/省简码
     * @param zipCode           邮政编码
     * @return Autel管理端-默认税率配置表 实体类
     * @function 根据国家简码，州/省简码，邮政编码获取Autel管理端-默认税率配置表的实体类数据
     */
    @Override
    public OpTaxConfigurationEntity getOpTaxConfigurationByStationLocation(String countryShortCode, String provinceShortCode, String zipCode) {

        log.info("====>>>>OpTaxConfigurationRepositoryImpl.getOpTaxConfigurationByStationLocation countryShortCode : {} and provinceShortCode : {} and zipCode : {}", JSON.toJSONString(countryShortCode), JSON.toJSONString(provinceShortCode), JSON.toJSONString(zipCode));

        OpTaxConfigurationEntity opTaxConfigurationEntity = null;

        //处理特殊字符
        countryShortCode = dealAbnormalString(countryShortCode);
        provinceShortCode = dealAbnormalString(provinceShortCode);
        zipCode = dealAbnormalString(zipCode);
        //1.都有值优先匹配
        if (StringUtils.isNotEmpty(countryShortCode) && StringUtils.isNotEmpty(provinceShortCode) && StringUtils.isNotEmpty(zipCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, provinceShortCode, zipCode);
            //1.2未匹配到，查询邮编为空的
            if (Objects.isNull(opTaxConfigurationEntity)){
                opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, provinceShortCode, null);
            }else {
                return opTaxConfigurationEntity;
            }
            //1.3未匹配到 查邮编和省为空的
            if (Objects.isNull(opTaxConfigurationEntity)){
                opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, null, null);
            }else {
                return opTaxConfigurationEntity;
            }
            //1.4未匹配到
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        //2.国家简码+省简码
        if (StringUtils.isNotEmpty(countryShortCode) && StringUtils.isNotEmpty(provinceShortCode) && StringUtils.isEmpty(zipCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, provinceShortCode, null);
            //2.2未匹配到 查邮编和省为空的
            if (Objects.isNull(opTaxConfigurationEntity)){
                opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, null, null);
            }else {
                return opTaxConfigurationEntity;
            }
            //2.3未匹配到
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        //3.国家简码+邮政编码
        if (StringUtils.isNotEmpty(countryShortCode) && StringUtils.isNotEmpty(zipCode) && StringUtils.isEmpty(provinceShortCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, NULL_CONDITION, zipCode);
            //3.2未匹配到 查邮编和省为空的
            if (Objects.isNull(opTaxConfigurationEntity)){
                opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, null, null);
            }else {
                return opTaxConfigurationEntity;
            }
            //3.3未匹配到
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        //4.省简码+邮政编码
        if (StringUtils.isNotEmpty(provinceShortCode) && StringUtils.isNotEmpty(zipCode) && StringUtils.isEmpty(countryShortCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(NULL_CONDITION, provinceShortCode, zipCode);
            //3.2未匹配到 查邮编和省为空的
            if (Objects.isNull(opTaxConfigurationEntity)){
                opTaxConfigurationEntity = voteOpTaxConfigurationEntity(NULL_CONDITION, provinceShortCode, null);
            }else {
                return opTaxConfigurationEntity;
            }
            //3.3未匹配到
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        //5.国家简码
        if (StringUtils.isNotEmpty(countryShortCode) && StringUtils.isEmpty(provinceShortCode) && StringUtils.isEmpty(zipCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(countryShortCode, null, null);
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        //6.省简码
        if (StringUtils.isNotEmpty(provinceShortCode) && StringUtils.isEmpty(countryShortCode) && StringUtils.isEmpty(zipCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(NULL_CONDITION, provinceShortCode, null);
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        //7.邮政编码
        if (StringUtils.isNotEmpty(zipCode) && StringUtils.isEmpty(countryShortCode) && StringUtils.isEmpty(provinceShortCode)){
            opTaxConfigurationEntity = voteOpTaxConfigurationEntity(NULL_CONDITION, NULL_CONDITION, zipCode);
            if (Objects.isNull(opTaxConfigurationEntity)) {
                return null;
            }
        }
        return opTaxConfigurationEntity;
    }

    /**
     * 选举一个税率配置项
     *
     * @param countryShortCode  国家简码
     * @param provinceShortCode 省简码
     * @param zipCode           邮政编码
     * @return {@link OpTaxConfigurationEntity}
     */
    private OpTaxConfigurationEntity voteOpTaxConfigurationEntity(String countryShortCode, String provinceShortCode, String zipCode) {
        LambdaQueryWrapper<OpTaxConfigurationEntity>  lambdaQueryWrapper = new LambdaQueryWrapper<>();
        if (!NULL_CONDITION.equals(countryShortCode) && StringUtils.isNotEmpty(countryShortCode)){
            lambdaQueryWrapper.eq(OpTaxConfigurationEntity::getCountryShortCode, countryShortCode);
        }
        if (!NULL_CONDITION.equals(provinceShortCode) && StringUtils.isNotEmpty(provinceShortCode)){
            lambdaQueryWrapper.eq(OpTaxConfigurationEntity::getProvinceShortCode, provinceShortCode);
        }
        if (!NULL_CONDITION.equals(zipCode) && StringUtils.isNotEmpty(zipCode)){
            lambdaQueryWrapper.eq(OpTaxConfigurationEntity::getZipCode, zipCode);
        }
        //是NULL用这种查询
        if (StringUtils.isEmpty(countryShortCode)){
            lambdaQueryWrapper.and(query -> query.isNull(OpTaxConfigurationEntity::getCountryShortCode).or().eq(OpTaxConfigurationEntity::getCountryShortCode, ""));
        }
        if (StringUtils.isEmpty(provinceShortCode)){
            lambdaQueryWrapper.and(query -> query.isNull(OpTaxConfigurationEntity::getProvinceShortCode).or().eq(OpTaxConfigurationEntity::getProvinceShortCode, ""));
        }
        if (StringUtils.isEmpty(zipCode)){
            lambdaQueryWrapper.and(query -> query.isNull(OpTaxConfigurationEntity::getZipCode).or().eq(OpTaxConfigurationEntity::getZipCode, ""));
        }
        lambdaQueryWrapper.eq(OpTaxConfigurationEntity::getDeleted, 0)
                .orderByDesc(OpTaxConfigurationEntity::getCreateTime, OpTaxConfigurationEntity::getId)
                .last("limit 1");
        return opTaxConfigurationMapper.selectOne(lambdaQueryWrapper);
    }

    private String dealAbnormalString(String countryShortCode) {
        if (StringUtils.isNotBlank(countryShortCode)) {
            // 处理特殊字符
            countryShortCode = StringUtil.escapeChar(countryShortCode);
        }
        return countryShortCode;
    }

    /**
     * @param currentPage 当前页码数
     * @param pageSize    每页数据量
     * @param keyword     搜索关键词
     * @return Autel管理端-默认税率配置数据列表
     * @function 分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）
     */
    @Override
    public Page<OpTaxConfigurationEntity> getOpTaxConfigurationPageList(Integer currentPage, Integer pageSize, String keyword) {

        log.info("====>>>>OpTaxConfigurationRepositoryImpl.getOpTaxConfigurationPageList currentPage : {} and pageSize : {} and keyword : {}", JSON.toJSONString(currentPage), JSON.toJSONString(pageSize), JSON.toJSONString(keyword));

        Page<OpTaxConfigurationEntity> opTaxConfigurationEntityPage = new Page<>(currentPage, pageSize);

        LambdaQueryWrapper<OpTaxConfigurationEntity> lambdaQueryWrapper = Wrappers
                .lambdaQuery(OpTaxConfigurationEntity.class)
                .eq(OpTaxConfigurationEntity::getDeleted, 0);
        if (StringUtils.isNotBlank(keyword)) {
            // 处理特殊字符
            String keywordString = StringUtil.escapeChar(keyword);
            // 支持国家，州/省名称，邮政编码的模糊搜索
            lambdaQueryWrapper
                    .and(target -> target
                            .like(OpTaxConfigurationEntity::getCountryName, keywordString)
                            .or()
                            .like(OpTaxConfigurationEntity::getProvinceName, keywordString)
                            .or()
                            .like(OpTaxConfigurationEntity::getZipCode, keywordString));
        }
        // 指定排序方式
        lambdaQueryWrapper
                .orderByDesc(OpTaxConfigurationEntity::getCreateTime, OpTaxConfigurationEntity::getId);
        return opTaxConfigurationMapper.selectPage(opTaxConfigurationEntityPage, lambdaQueryWrapper);
    }

    /**
     * @return 默认税率配置数据集合
     * @function 获得所有的默认税率配置数据
     */
    @Override
    public List<OpTaxConfigurationEntity> getAllOpTaxConfiguration() {
        LambdaQueryWrapper<OpTaxConfigurationEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpTaxConfigurationEntity::getDeleted, 0)
                .orderByAsc(OpTaxConfigurationEntity::getCreateTime, OpTaxConfigurationEntity::getId);
        return opTaxConfigurationMapper.selectList(lambdaQueryWrapper);
    }
}
