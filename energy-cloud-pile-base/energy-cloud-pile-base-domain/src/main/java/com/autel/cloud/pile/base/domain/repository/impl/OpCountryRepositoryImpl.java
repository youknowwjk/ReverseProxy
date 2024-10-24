package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.repository.OpCountryRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationConnectorRepository;
import com.autel.cloud.pile.base.domain.service.impl.OpLocationEvseServiceImpl;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.BaseAdminClient;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.OpCountryMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationConnectorMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCountryEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.tariff.dto.CurrencyDTO;
import com.autel.cloud.tariff.feign.CurrencyFeignClient;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Log4j2
public class OpCountryRepositoryImpl extends ServiceImpl<OpCountryMapper, OpCountryEntity> implements OpCountryRepository {

    @Autowired
    OpCountryMapper countryMapper;

    @Resource
    private CurrencyFeignClient currencyFeignClient;
    @Resource
    private BaseAdminClient baseAdminClient;
    @Resource
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;

    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;

    @Autowired
    private OpLocationEvseExpandElastic opLocationEvseExpandElastic;

    @Autowired
    private OpLocationEvseRepositoryImpl opLocationEvseRepository;

    @Resource
    private OpLocationConnectorRepository opLocationConnectorRepository;

    /**
     * 国家简写转成全称
     *
     * @param simpleCode 国家简写
     * @return 国家全称
     */
    @Override
    public String simPleCode2Name(String simpleCode) {
        String countryName = simpleCode;

        log.info("数据库保存的国家简写信息：{}", simpleCode);
        //根据国际化转成转成对应的全称
        if (StringUtils.isNotBlank(simpleCode)) {
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String language = request.getHeader("accept-language");
            log.info("header里面国际化语言：{}", language);
            if (StringUtils.isBlank(language)) {
                language = "zh-CN";
            }
            QueryWrapper<OpCountryEntity> opCountryEntityQueryWrapper = new QueryWrapper<>();
            opCountryEntityQueryWrapper.eq(BaseConstant.ALPHA_2_CODE, simpleCode);
            opCountryEntityQueryWrapper.eq("language", language);
            List<OpCountryEntity> opCountryEntities = this.getBaseMapper().selectList(opCountryEntityQueryWrapper);
            log.info(BaseConstant.OP_COUNTRYENTITIES, JSON.toJSONString(opCountryEntities));
            if (CollectionUtils.isNotEmpty(opCountryEntities)) {
                countryName = opCountryEntities.get(0).getName();
            }
        }
        return countryName;
    }

    /**
     * 根据国家缩写获取国家信息
     *
     * @param countryAbbre 国家简写
     * @return
     */
    @Override
    public List<OpCountryEntity> getCountryInfoByCountryAbbre(String countryAbbre) {
        QueryWrapper<OpCountryEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(BaseConstant.ALPHA_2_CODE, countryAbbre)
                .eq("deleted", 0);
        List<OpCountryEntity> opCountryEntities = this.getBaseMapper().selectList(queryWrapper);
        log.info(BaseConstant.OP_COUNTRYENTITIES, JSON.toJSONString(opCountryEntities));
        return opCountryEntities;
    }

    /**
     * @param userAlpha2CodeAndLanguage
     * @return
     * @function 根据国家缩写和语言获取国家信息
     */
    @Override
    public List<OpCountryEntity> getCountryInfoByLanguageAndAlpha2Code(UserAlpha2CodeAndLanguage userAlpha2CodeAndLanguage) {
        QueryWrapper<OpCountryEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(BaseConstant.ALPHA_2_CODE, userAlpha2CodeAndLanguage.getAlpha2Code())
                .eq("language", userAlpha2CodeAndLanguage.getLanguage())
                .eq("deleted", 0);
        List<OpCountryEntity> opCountryEntities = this.getBaseMapper().selectList(queryWrapper);
        log.info(BaseConstant.OP_COUNTRYENTITIES, JSON.toJSONString(opCountryEntities));
        return opCountryEntities;
    }

    @Override
    public List<OpCountryEntity> saveCountryCurrencyRelation(List<OpCountryDTO> requestDTO) {
        List<String> alphaColl = requestDTO.stream()
                .map(OpCountryDTO::getAlpha2Code)
                .collect(Collectors.toList());
        QueryWrapper<OpCountryEntity> wrapper = new QueryWrapper<>();
        wrapper.in(BaseConstant.ALPHA_2_CODE, alphaColl);
        List<OpCountryEntity> opCountryEntities = this.getBaseMapper().selectList(wrapper);
        Map<String, Long> map = requestDTO.stream()
                .filter(r -> StringUtils.isNotBlank(r.getAlpha2Code()) && r.getCurrencyId() != null && r.getCurrencyId() != 0)
                .collect(Collectors.toMap(
                        OpCountryDTO::getAlpha2Code,
                        OpCountryDTO::getCurrencyId
                ));
        log.info("需要保存的国家数：{}", map.size());
        List<Long> idList = Lists.newArrayList();
        List<OpCountryEntity> resultList = Lists.newArrayList();
        for (OpCountryEntity entity : opCountryEntities) {
            String alpha2Code = entity.getAlpha2Code();
            Long currencyId = map.get(alpha2Code);
            entity.setCurrencyId(currencyId);
            int insert = this.getBaseMapper().updateById(entity);
            if (insert > 0) {
                resultList.add(entity);
            } else {
                idList.add(entity.getId());
            }
        }
        log.info("总数量：{} 保存成功数：{} 失败数：{}", opCountryEntities.size(),
                resultList.size(), idList.size());
        return opCountryEntities;
    }

    @Override
    public Boolean syncCountryCurrencyId() {
        //1、tariff库中获取货币的code，id
        List<CurrencyDTO> currencyIdAndCodes = currencyFeignClient.getCurrencyIdAndCode().getData();
        Map<Integer, Long> currencyIdAndCodeMap = currencyIdAndCodes.stream().collect(Collectors.toMap(CurrencyDTO::getCode, CurrencyDTO::getId));
        List<Integer> currencyCodes = currencyIdAndCodes.stream().map(CurrencyDTO::getCode).collect(Collectors.toList());
        //2、使用第一步获取的code，在admin的country_information表获取国家缩写、货币code
        Map<String, Integer> currencyAndAlpha2CodeMap = baseAdminClient.getCurrencyAndAlpha2CodeMap(currencyCodes).getData();
        //3、根据国家缩写，更新pile-base的op_country表的国家对应货币ID信息
        Set<String> alpha2Code = currencyAndAlpha2CodeMap.keySet();
        List<OpCountryEntity> opCountryEntities = countryMapper.selectList(new LambdaQueryWrapper<OpCountryEntity>()
        .eq(OpCountryEntity::getDeleted,0)
        .in(!ObjectUtils.isEmpty(alpha2Code), OpCountryEntity::getAlpha2Code, alpha2Code));
        for (OpCountryEntity opCountryEntity : opCountryEntities) {
            Integer currencyCode = null;
            Long currencyId = 0L;
            if (!ObjectUtils.isEmpty(currencyAndAlpha2CodeMap) && !ObjectUtils.isEmpty(currencyIdAndCodeMap)) {
                currencyCode = currencyAndAlpha2CodeMap.get(opCountryEntity.getAlpha2Code());
                currencyId = currencyIdAndCodeMap.get(currencyCode);
                opCountryEntity.setCurrencyId(currencyId);
            }
        }
        log.info("更新国家货币的id：{}", JSON.toJSONString(opCountryEntities));
        this.updateBatchById(opCountryEntities);
        return true;
    }

    @Override
    public Boolean updatePower(UpdatePowerDTO updatePowerDTO) {
        if (ObjectUtils.isEmpty(updatePowerDTO) || StringUtils.isBlank(updatePowerDTO.getPileSn()) || ObjectUtils.isEmpty(updatePowerDTO.getPower())) {
            return false;
        }
        String pileSn = updatePowerDTO.getPileSn();
        List<Long> ids = new ArrayList<>();
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = opLocationEvseElastic.findAllByPileSn(pileSn);
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOS) {
            opLocationEvseElasticDTO.setPower(updatePowerDTO.getPower());
            ids.add(opLocationEvseElasticDTO.getId());
        }
        log.info("updatePower,opLocationEvseElasticDTOS={}",opLocationEvseElasticDTOS);
        opLocationEvseElastic.saveAll(opLocationEvseElasticDTOS);
        List<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOS = opLocationEvseExpandElastic.findAllByIdIn(ids);
        for (OpLocationEvseExpandElasticDTO opLocationEvseExpandElasticDTO : opLocationEvseExpandElasticDTOS) {
            opLocationEvseExpandElasticDTO.setPower(updatePowerDTO.getPower());
        }
        opLocationEvseExpandElastic.saveAll(opLocationEvseExpandElasticDTOS);

        log.info("updatePower,opLocationEvseExpandElasticDTOS={}",opLocationEvseExpandElasticDTOS);

        OpLocationPileEvseElasticDTO byPileSn = opLocationPileEvseElastic.findByPileSn(pileSn);
        byPileSn.setPower(updatePowerDTO.getPower());
        opLocationPileEvseElastic.save(byPileSn);
        log.info("updatePower,byPileSn={}",byPileSn);
        String evseSn = pileSn + "_1";
        OpEvseInfoDTO evseByEvseSn = opLocationEvseRepository.getEvseByEvseSn(evseSn);
        if (!ObjectUtils.isEmpty(evseByEvseSn) && !ObjectUtils.isEmpty(evseByEvseSn.getOpLocationConnectorDTOList())) {
            List<OpLocationConnectorEntity> connectorEntityList = new ArrayList<>();
            for (OpLocationConnectorDTO opLocationConnectorDTO : evseByEvseSn.getOpLocationConnectorDTOList()) {
                opLocationConnectorDTO.setPower(updatePowerDTO.getPower());
                OpLocationConnectorEntity opLocationConnectorEntity = new OpLocationConnectorEntity();
                BeanUtils.copyProperties(opLocationConnectorDTO,opLocationConnectorEntity);
                connectorEntityList.add(opLocationConnectorEntity);
            }
            opLocationConnectorRepository.updateBatchById(connectorEntityList);
        }
        Result<Boolean> result = deviceServiceFeign.updatePower(updatePowerDTO);
        if (result == null || result.getData() == null || result.getData().equals(false)) {
            return false;
        }
        return true;
    }

    @Override
    public List<OpCountryEntity> getCountryInfoListByCountryAbbreList(List<String> countryAbbreList) {
        String language = "en-US";
        //获取请求头中的版本信息
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        if (!ObjectUtils.isEmpty(request.getHeader("accept-language"))) {
            language = request.getHeader("accept-language");
        }
        QueryWrapper<OpCountryEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.in(BaseConstant.ALPHA_2_CODE, countryAbbreList)
                .eq("language", language)
                .eq("deleted", 0);
        List<OpCountryEntity> opCountryEntities = this.getBaseMapper().selectList(queryWrapper);
        log.info(BaseConstant.OP_COUNTRYENTITIES, JSON.toJSONString(opCountryEntities));
        return opCountryEntities;
    }
}
