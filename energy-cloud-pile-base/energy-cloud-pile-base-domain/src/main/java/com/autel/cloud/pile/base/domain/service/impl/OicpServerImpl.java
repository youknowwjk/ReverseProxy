package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileEvseConvert;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.OicpService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.oicp.EroamingEvseData;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.EvseDynamicPricingVO;
import com.autel.cloud.pile.base.vo.OpPileEvseInfoVO;
import com.autel.cloud.pile.base.vo.oicp.OicpEvseInfoVO;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelRuleVO;
import com.autel.cloud.tariff.vo.oicp.HubjectCostModelRuleBasicInfoVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author X21216
 * @date 2023/3/23  17:59
 */
@Service
@Slf4j
public class OicpServerImpl implements OicpService {

    @Resource
    private OpLocationRepository opLocationRepository;
    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Autowired
    private TariffFeignClient tariffFeignClient;

    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;

    @Autowired
    private OpLocationEvseService opLocationEvseService;

    @Autowired
    private OpLocationService opLocationService;

    @Value("${ocpi.country-code}")
    private String countryCode;

    @Value("${ocpi.party-id}")
    private String partyId;

    @Override
    public Page<EroamingEvseData> queryEvseDate(Integer page, Integer pageSize) {

        LambdaQueryWrapper<OpLocationPileEvseEntity> eq = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                .eq(OpLocationPileEvseEntity::getEroamingEnable, 1)
                .eq(OpLocationPileEvseEntity::getDeleted, false);
        Page<OpLocationPileEvseEntity> pagePaprm = new Page<>(page, pageSize);
        Page<OpLocationPileEvseEntity> pileEvseEntityPage = opLocationPileEvseRepository.page(pagePaprm, eq);
        List<OpLocationPileEvseEntity> records = pileEvseEntityPage.getRecords();

        List<EroamingEvseData> evseDataList = records.stream()
                .map(opLocationPileEvseEntity -> {
                    OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseEntity);

                    String evseList = opLocationPileEvseEntity.getEvseList();
                    List<Long> evseIds = JSON.parseArray(evseList, Long.class);
                    List<OpLocationEvseDTO> opLocationEvseDTOS = new ArrayList<>();
                    evseIds.forEach(e -> {
                        OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(e);
                        OpLocationEvseElasticDTO byEvseSn = opLocationEvseElastic.findByEvseSn(opLocationEvseDTO.getEvseSn());
                        opLocationEvseDTO.setTariffId(byEvseSn.getTariffId());
                        opLocationEvseDTOS.add(opLocationEvseDTO);
                    });
                    opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
                    OpLocationDTO locationByPileSn = opLocationRepository.getLocationByPileSn(opPileEvseInfoVO.getPileSn());
                    return EroamingEvseData.builder()
                            .locationDTO(locationByPileSn)
                            .opPileEvseInfoVO(opPileEvseInfoVO)
                            .build();
                })
                .collect(Collectors.toList());

        Page<EroamingEvseData> eroamingEvseDataPage = new Page<>();
        BeanUtils.copyProperties(pileEvseEntityPage, eroamingEvseDataPage);
        eroamingEvseDataPage.setRecords(evseDataList);
        return eroamingEvseDataPage;
    }

    /**
     * @param pageDTO
     * @return
     * @function 提供清单页面供EMP拉取(OICP Hubject)
     */
    @Override
    public Page<EvseDynamicPricingVO> page(PageDTO pageDTO) {

        log.info("=====>>>>>OicpServerImpl.page pageDTO : {}", JSON.toJSONString(pageDTO));

        // 先查询所有Hubject计费规则的基本信息
        Result<List<HubjectCostModelRuleBasicInfoVO>> getAllHubjectCostModelRuleBasicInfoVOResult = tariffFeignClient.getAllHubjectCostModelRuleBasicInfoVO();

        log.info("===>>>OicpServerImpl.page getAllHubjectCostModelRuleBasicInfoVOResult : {}", JSON.toJSONString(getAllHubjectCostModelRuleBasicInfoVOResult));

        if (getAllHubjectCostModelRuleBasicInfoVOResult == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(getAllHubjectCostModelRuleBasicInfoVOResult.getCode())
                || ObjectUtils.isEmpty(getAllHubjectCostModelRuleBasicInfoVOResult.getData())) {

            log.info("===>>>OicpServerImpl.page 当前环境不存在Hubject计费规则！");

            return new Page<>();
        }

        List<HubjectCostModelRuleBasicInfoVO> hubjectCostModelRuleBasicInfoVOList = getAllHubjectCostModelRuleBasicInfoVOResult.getData();
        // 计费规则组id集合
        List<Long> tariffGroupIdList = new ArrayList<>();
        // 构建计费规则组id和其下的Hubject计费规则id之间的映射关系
        Map<Long, Long> tariffGroupIdAndtariffIdMap = new HashMap<>();
        for (HubjectCostModelRuleBasicInfoVO hubjectCostModelRuleBasicInfoVO : hubjectCostModelRuleBasicInfoVOList) {
            tariffGroupIdList.add(hubjectCostModelRuleBasicInfoVO.getTariffGroupId());
            tariffGroupIdAndtariffIdMap.put(hubjectCostModelRuleBasicInfoVO.getTariffGroupId(), hubjectCostModelRuleBasicInfoVO.getId());
        }
        // 查询出所有开启了互联互通功能的充电桩的信息
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseService.findAllEroamingPile();
        if (ObjectUtils.isEmpty(opLocationPileEvseElasticDTOList)) {

            log.info("===>>>OicpServerImpl.page 当前环境不存在开通了互联互通功能的充电桩！");

            return new Page<>();
        }

        // 充电枪id集合
        List<Long> evseIdList = new ArrayList<>();
        // 构建充电桩序列号与其信息之间的映射关系
        Map<String, OpLocationPileEvseElasticDTO> pileSnAndOpLocationPileEvseElasticDTOMap = new HashMap<>();
        for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
            String evseList = opLocationPileEvseElasticDTO.getEvseList();
            if (StringUtils.isNotBlank(evseList)) {
                evseIdList.addAll(JSON.parseArray(evseList, Long.class));
            }
            pileSnAndOpLocationPileEvseElasticDTOMap.put(opLocationPileEvseElasticDTO.getPileSn(), opLocationPileEvseElasticDTO);
        }
        // 查询出所有开启了互联互通功能并且配置了含有Hubject计费规则的充电桩下的充电枪信息
        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseService.findEvseInfoList(tariffGroupIdList, evseIdList);
        if (ObjectUtils.isEmpty(opLocationEvseEntityList)) {

            log.info("===>>>OicpServerImpl.page 当前环境不存在开启了互联互通功能并且配置了含有Hubject计费规则的充电桩下的充电枪！");

            return new Page<>();
        }

        // 当前页充电设备集合
        final List<OpLocationEvseEntity> currentPageRecords = ListUtils.partition(opLocationEvseEntityList, pageDTO.getPageSize()).get(pageDTO.getPage() - 1);
        // 当前页计费规则组的id集合
        List<Long> currentPageTariffGroupIdList = new ArrayList<>();
        // 当前页场站id集合
        List<Long> currentPageLocationIdList = new ArrayList<>();
        // 当前页evseId集合
        List<String> currentPageEvseIdList = new ArrayList<>();
        // 构建evseId与其对应的充电枪信息之间的映射关系
        Map<String, OpLocationEvseEntity> evseIdAndOpLocationEvseEntityMap = new HashMap<>();
        // 构建evseId与其对应的充电桩序列号之间的映射关系
        Map<String, String> evseIdAndPileSnMap = new HashMap<>();
        for (OpLocationEvseEntity opLocationEvseEntity : currentPageRecords) {
            currentPageTariffGroupIdList.add(opLocationEvseEntity.getTariffId());
            currentPageLocationIdList.add(opLocationEvseEntity.getLocationId());
            String evseSn = opLocationEvseEntity.getEvseSn();
            int lastIndexOf = evseSn.lastIndexOf("_");
            // 截取充电桩序列号
            String pileSn = evseSn.substring(0, lastIndexOf);
            // 截取枪号
            String connector = evseSn.substring((lastIndexOf + 1));
            // 拼接evseId
            String evseId = countryCode + "*" + partyId + "*E" + pileSn + "*" + connector;
            currentPageEvseIdList.add(evseId);
            evseIdAndOpLocationEvseEntityMap.put(evseId, opLocationEvseEntity);
            evseIdAndPileSnMap.put(evseId, pileSn);
        }
        // 当前页Hubject计费规则的id集合
        List<Long> currentPageTariffIdList = new ArrayList<>();
        for (Long currentPageTariffGroupId : currentPageTariffGroupIdList) {
            currentPageTariffIdList.add(tariffGroupIdAndtariffIdMap.get(currentPageTariffGroupId));
        }
        // 查询Hubject计费规则的详情
        Result<List<CostModelRuleVO>> getHubjectCostModelRuleInfoResult = tariffFeignClient.getHubjectCostModelRuleInfo(currentPageTariffIdList);

        log.info("===>>>OicpServerImpl.page getHubjectCostModelRuleInfoResult : {}", JSON.toJSONString(getHubjectCostModelRuleInfoResult));

        if (getHubjectCostModelRuleInfoResult == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(getHubjectCostModelRuleInfoResult.getCode())
                || ObjectUtils.isEmpty(getHubjectCostModelRuleInfoResult.getData())) {

            log.info("===>>>OicpServerImpl.page 当前环境不存在这样的Hubject计费规则！");

            return new Page<>();
        }

        List<CostModelRuleVO> costModelRuleVOList = getHubjectCostModelRuleInfoResult.getData();
        // 构建计费规则组id和充电设备所绑定的计费规则详情之间的映射关系
        Map<Long, CostModelRuleVO> tariffGroupIdAndCostModelRuleVOMap = new HashMap<>();
        for (CostModelRuleVO costModelRuleVO : costModelRuleVOList) {
            tariffGroupIdAndCostModelRuleVOMap.put(costModelRuleVO.getCostModelRuleGroupId(), costModelRuleVO);
        }
        // 查询场站信息
        List<OpLocationEntity> opLocationEntityList = opLocationService.findLocationInfoByLocationIdList(currentPageLocationIdList);
        // 构建场站id与其对应的场站信息之间的映射关系
        Map<Long, OpLocationEntity> locationIdAndOpLocationEntityMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(opLocationEntityList)) {
            for (OpLocationEntity opLocationEntity : opLocationEntityList) {
                locationIdAndOpLocationEntityMap.put(opLocationEntity.getId(), opLocationEntity);
            }
        }
        // 构建返回结果集
        Page<EvseDynamicPricingVO> oicpEvseInfoVOPage = new Page<>();
        List<EvseDynamicPricingVO> oicpEvseInfoVOList = new ArrayList<>();
        for (String evseId : currentPageEvseIdList) {
            EvseDynamicPricingVO oicpEvseInfoVO = new EvseDynamicPricingVO();
            oicpEvseInfoVO.setEvseId(evseId);
            if (evseIdAndOpLocationEvseEntityMap.get(evseId) != null) {
                Long locationId = evseIdAndOpLocationEvseEntityMap.get(evseId).getLocationId();
                if (locationId != null && locationIdAndOpLocationEntityMap.get(locationId) != null) {
                    OpLocationEntity opLocationEntity = locationIdAndOpLocationEntityMap.get(locationId);
                    oicpEvseInfoVO.setLocationAddress(opLocationEntity.getAddress());
                }
            }
            if (StringUtils.isNotBlank(evseIdAndPileSnMap.get(evseId))) {
                String pileSn = evseIdAndPileSnMap.get(evseId);
                if (pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn) != null) {
                    OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn);
                    oicpEvseInfoVO.setPilePower(opLocationPileEvseElasticDTO.getPower());
                }
            }
            if (evseIdAndOpLocationEvseEntityMap.get(evseId) != null) {
                OpLocationEvseEntity opLocationEvseEntity = evseIdAndOpLocationEvseEntityMap.get(evseId);
                Long tariffId = opLocationEvseEntity.getTariffId();
                if (tariffGroupIdAndCostModelRuleVOMap.get(tariffId) != null) {
                    CostModelRuleVO costModelRuleVO = tariffGroupIdAndCostModelRuleVOMap.get(tariffId);
                    CostModelRuleVO costModelRule = JSON.parseObject(JSON.toJSONString(costModelRuleVO), CostModelRuleVO.class);
                    oicpEvseInfoVO.setTariffId(costModelRule.getId().toString());
                    oicpEvseInfoVO.setCostModelRuleVO(costModelRule);
                    if (RuleModelTypeEnum.FREE.getCode().equals(costModelRule.getRuleModelType())) {
                        oicpEvseInfoVO.setFreeMark(true);
                    }
                }
            }
            oicpEvseInfoVOList.add(oicpEvseInfoVO);
        }
        oicpEvseInfoVOPage
                .setRecords(oicpEvseInfoVOList)
                .setTotal(opLocationEvseEntityList.size())
                .setSize(pageDTO.getPageSize())
                .setCurrent(pageDTO.getPage())
                .setPages(ListUtils.partition(opLocationEvseEntityList, pageDTO.getPageSize()).size());
        return oicpEvseInfoVOPage;
    }
}
