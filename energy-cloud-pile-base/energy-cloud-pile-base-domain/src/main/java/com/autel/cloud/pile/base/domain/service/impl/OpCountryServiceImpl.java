package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.util.StrUtil;
import com.autel.cloud.pile.base.domain.convert.OpCountryConvert;
import com.autel.cloud.pile.base.domain.repository.OpCountryRepository;
import com.autel.cloud.pile.base.domain.service.OpCountryService;
import com.autel.cloud.pile.base.dto.OpCountryDTO;
import com.autel.cloud.pile.base.dto.UserAlpha2CodeAndLanguage;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCountryEntity;
import com.autel.cloud.pile.base.util.ListSortUtil;
import com.autel.cloud.pile.base.vo.OpCountryInfoVO;
import com.autel.cloud.pile.base.vo.OpCountryVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@Log4j2
public class OpCountryServiceImpl implements OpCountryService {

    @Autowired
    private OpCountryRepository opCountryRepository;


    @Override
    public List<OpCountryVO> list() {
        QueryWrapper<OpCountryEntity> queryWrapper = new QueryWrapper<OpCountryEntity>().select("DISTINCT code");
        return OpCountryConvert.toOpCountryVOs(opCountryRepository.list(queryWrapper));
    }

    @Override
    public List<OpCountryVO> getAll(String language) {
        LambdaQueryWrapper<OpCountryEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpCountryEntity::getLanguage, language);
        return OpCountryConvert.toOpCountryVOs(opCountryRepository.list(queryWrapper));
    }

    @Override
    public List<OpCountryVO> getAllCode(String language) {
        LambdaQueryWrapper<OpCountryEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpCountryEntity::getLanguage, language);
        return OpCountryConvert.toOpCountryCodeVOs(opCountryRepository.list(queryWrapper));
    }

    @Override
    public List<OpCountryInfoVO> getCountryInfo(String language, List<String> abbrCodeList) {
        List<OpCountryInfoVO> resultList = Lists.newArrayList();
        LambdaQueryWrapper<OpCountryEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpCountryEntity::getLanguage, language);
        queryWrapper.in(OpCountryEntity::getAlpha2Code, abbrCodeList);
        List<OpCountryEntity> countryList = opCountryRepository.list(queryWrapper);
        if (CollectionUtils.isNotEmpty(countryList)) {
            resultList = countryList.stream().filter(item -> StringUtils.isNotBlank(item.getAlpha2Code()) && StringUtils.isNotBlank(item.getCode()))
                    .map(item -> OpCountryInfoVO.builder()
                            .id(String.valueOf(item.getId()))
                            .name(item.getName())
                            .code(item.getCode())
                            .countryAbbreviation(item.getAlpha2Code())
                            .build()).collect(Collectors.toList());
        }
        return resultList;
    }

    @Override
    public List<OpCountryVO> getCountryInfoByLanguageAndAlpha2Code(UserAlpha2CodeAndLanguage userAlpha2CodeAndLanguage) {
        List<OpCountryEntity> entities;
        String language = userAlpha2CodeAndLanguage.getLanguage();
        if (StrUtil.isBlank(language)) {
            // 根据国家缩写来查询用户国家信息
            entities = opCountryRepository.getCountryInfoByCountryAbbre(userAlpha2CodeAndLanguage.getAlpha2Code());
        } else {
            // 根据国家缩写和语言查询用户国家信息
            entities = opCountryRepository.getCountryInfoByLanguageAndAlpha2Code(userAlpha2CodeAndLanguage);
        }
        List<OpCountryVO> opCountryVOS = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(entities)) {
            List<OpCountryEntity> collect = entities.stream()
                    .filter(ListSortUtil.distinctByKey(OpCountryEntity::getAlpha2Code))
                    .collect(Collectors.toList());
            opCountryVOS = OpCountryConvert.toOpCountryVOs(collect);
        }

        return opCountryVOS;
    }

    @Override
    public List<OpCountryVO> saveCountryCurrencyRelation(List<OpCountryDTO> requestDTO) {
        List<OpCountryEntity> opCountryEntities = opCountryRepository.saveCountryCurrencyRelation(requestDTO);
        return OpCountryConvert.toOpCountryVOs(opCountryEntities);
    }

    @Override
    public Boolean syncCountryCurrencyId() {
        return opCountryRepository.syncCountryCurrencyId();
    }

    @Override
    public List<OpCountryVO> getCountryInfoListByCountryAbbreList(List<String> countryAbbreList) {
        List<OpCountryEntity> entities = opCountryRepository.getCountryInfoListByCountryAbbreList(countryAbbreList);

        List<OpCountryVO> opCountryVOS = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(entities)) {
            List<OpCountryEntity> collect = entities.stream()
                    .filter(ListSortUtil.distinctByKey(OpCountryEntity::getAlpha2Code))
                    .collect(Collectors.toList());
            opCountryVOS = OpCountryConvert.toOpCountryVOs(collect);
        }
        return opCountryVOS;
    }

    @Override
    public Map<String, String> getCountryMap(List<String> countryAbbreList) {
        List<OpCountryEntity> entities = opCountryRepository.getCountryInfoListByCountryAbbreList(countryAbbreList);
        if(CollectionUtils.isEmpty(entities)){
            return new HashMap<>();
        }

        Map<String, String> countryMap = new HashMap<>();
        entities.forEach(c->{
            countryMap.put(c.getAlpha2Code(),c.getName());
        });

        return countryMap;
    }

}
