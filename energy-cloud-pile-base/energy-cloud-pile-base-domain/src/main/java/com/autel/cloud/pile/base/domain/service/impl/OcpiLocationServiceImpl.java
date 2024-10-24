package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.domain.common.CountryCodeCacheRunner;
import com.autel.cloud.pile.base.domain.service.OcpiLocationService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationConnectorMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationEvseMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationOperationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOperationEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.bill.enums.RoamingProtocolEnum;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.RoamingRulesVO;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.OcpiTariffFeignClient;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelRuleVO;
import com.autel.cloud.tariff.vo.ocpi.GireveCostModelRuleBasicInfoVO;
import com.autel.cloud.tariff.vo.ocpi.TariffVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Author:   A19011
 * Description: OcpiLocationServiceImpl
 * Date:     2022/6/20 15:49
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class OcpiLocationServiceImpl implements OcpiLocationService {
    @Autowired
    OpLocationMapper opLocationMapper;
    @Autowired
    OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    OpLocationConnectorMapper opLocationConnectorMapper;
    @Autowired
    OpLocationOperationMapper opLocationOperationMapper;
    @Autowired
    MonitorFeignClient monitorFeignClient;
    @Value("${ocpi.page.link}")
    private String pageLinkFormat;
    @Value("${ocpi.location-type}")
    private String locationType;
    @Value("${ocpi.location-country}")
    private String locationCountry;
    @Value("${ocpi.location-city}")
    private String locationCity;
    @Value("${ocpi.connector-standard}")
    private String connectorStandard;
    @Value("#{'${ocpi.evse-capabilities}'.split(',')}")
    private List<String> evseCapabilities;
    @Value("${ocpi.country-code}")
    private String countryCode;
    @Value("${ocpi.party-id}")
    private String partyId;
    @Autowired
    private TariffFeignClient tariffFeignClient;

    @Autowired
    private OpLocationService opLocationService;

    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;

    @Autowired
    private OpLocationEvseService opLocationEvseService;

    @Autowired
    private OcpiTariffFeignClient ocpiTariffFeignClient;

    @Autowired
    private IBillFeignClient billFeignClient;

    @Override
    public ResponseEntity<List<OcpiLocationVO>> getLocationList(String dateFrom, String dateTo, Integer offset, Integer limit, String locationLink, String cpoId) {
        log.info("参数校验dateFrom：{}， dateTo：{}, cpoId:{}",dateFrom,dateTo, cpoId);
        if(Boolean.TRUE.equals((!DateUtil.validateDate(dateFrom)) || !DateUtil.validateDate(dateTo))){
            return ResponseEntity.badRequest().build();
        }

        List<String> operatorIdList = new ArrayList<>();
        List<String> excludeOperatorIdList = new ArrayList<>();
        if(StringUtils.isNotBlank(cpoId)){
            List<RoamingRulesVO> roamingRulesVOList = billFeignClient.getSellerByIndependentCpoId(cpoId, RoamingProtocolEnum.OCPI.getCode()).getData();
            log.info("operatorIdList roamingRulesVOList :{}", JSON.toJSONString(roamingRulesVOList));
            if(!CollectionUtils.isEmpty(roamingRulesVOList)){
                roamingRulesVOList.forEach(r->{
                    operatorIdList.add(r.getCpoId());
                });
            }
        }

        if(CollectionUtils.isEmpty(operatorIdList)){
            List<RoamingRulesVO> roamingRulesVOList = billFeignClient.getSellerIdsByCondition(RoamingProtocolEnum.OCPI.getCode(), true).getData();
            log.info("excludeOperatorIdList roamingRulesVOList :{}", JSON.toJSONString(roamingRulesVOList));
            roamingRulesVOList.forEach(r->{
                excludeOperatorIdList.add(r.getCpoId());
            });
        }

        log.info("getLocationList operatorIdList :{}, excludeOperatorIdList:{}",
                JSON.toJSONString(operatorIdList),
                JSON.toJSONString(excludeOperatorIdList));

        Integer totalCount = getTotalCount(dateFrom,dateTo, operatorIdList, excludeOperatorIdList);
        List<OcpiLocationVO> result = new ArrayList<>();
        log.info("分页获取场站参数{},{},{},{}",dateFrom,dateTo,offset,limit);
        List<OpLocationEntity>  locationList = getPageStation(dateFrom,dateTo,offset,limit, operatorIdList, excludeOperatorIdList);
        log.info("分页获取场站当前页记录数{}",locationList.size());

        if(!CollectionUtils.isEmpty(locationList)){
            for(OpLocationEntity location : locationList){
                OcpiLocationMappingVO locationMappingVO = convertOcpiLocationMappingVO(location);
                result.add(getLocationVO(locationMappingVO, null));
            }
        }

        boolean isLastPage = totalCount-offset <= limit;
        if(!isLastPage){
            offset = offset + limit;
            String link = String.format(pageLinkFormat, locationLink, StringUtils.defaultString(dateFrom,""),
                    StringUtils.defaultString(dateTo,""), limit, offset);
            link = link.replace("date_from=&","");
            link = link.replace("date_to=&","");
            return ResponseEntity.ok().header("X-Total-Count", totalCount+"")
                    .header("X-Limit", limit+"")
                    .header("Link", link)
                    .body(result);
        }

        return ResponseEntity.ok().header("X-Total-Count", totalCount+"")
                .header("X-Limit", limit+"")
                .body(result);
    }

    /**
     * 对象转化
     * @param location
     * @return
     */
    private OcpiLocationMappingVO convertOcpiLocationMappingVO(OpLocationEntity location){
        return OcpiLocationMappingVO.builder()
                .id(location.getId().toString())
                .operatorId(location.getOperatorId())
                .createTime(DateUtil.timeStamp2ISO8601(location.getCreatedAt()))
                .latitude(String.format("%.6f",Double.parseDouble(location.getLatitude())))
                .longitude(String.format("%.6f",Double.parseDouble(location.getLongitude())))
                .postCode(location.getPostalCode())
                .name(location.getName())
                .street(location.getAddress())
                .city(StringUtils.defaultIfBlank(location.getCity(), locationCity))
                .country(StringUtils.defaultIfBlank(CountryCodeCacheRunner.countryCodeMap.get(location.getCountry()), locationCountry))
                .timeZone(location.getZoneId())
                .updateTime(DateUtil.timeStamp2ISO8601(location.getUpdatedAt()))
                .locationType(location.getLocationType() == null ? 9 :  location.getLocationType())
                .build();
    }


    /**
     * 分页获取DB场站数据
     * @param dateFrom
     * @param dateTo
     * @param offset
     * @param limit
     * @param operatorIdList
     * @return
     */
    private List<OpLocationEntity> getPageStation(String dateFrom, String dateTo, Integer offset, Integer limit,
                                                  List<String> operatorIdList, List<String> excludeOperatorIdList){
        QueryWrapper<OpLocationEntity> queryWrapper=new QueryWrapper<>();
        queryWrapper.eq("status", 1);
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("hubject_check", 1);
        if(!CollectionUtils.isEmpty(operatorIdList)){
            queryWrapper.in("operator_id", operatorIdList);
        }
        if(!CollectionUtils.isEmpty(excludeOperatorIdList)){
            queryWrapper.notIn("operator_id", excludeOperatorIdList);
        }

        if(StringUtils.isNotBlank(dateFrom)){
            queryWrapper.apply("IFNULL(updated_at,created_at) >= 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateFrom) + "')");
        }
        if(StringUtils.isNotBlank(dateTo)){
            queryWrapper.apply("IFNULL(updated_at,created_at) < 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateTo) + "')");
        }
        queryWrapper.orderByAsc("created_at").orderByAsc("id");
        queryWrapper.last("limit " + offset +","+ limit);
        return opLocationMapper.selectList(queryWrapper);
    }

    /**
     * 分页获取DB场站数据
     * @param dateFrom
     * @param dateTo
     * @param operatorIdList
     * @return
     */
    private Integer getTotalCount(String dateFrom, String dateTo, List<String> operatorIdList, List<String> excludeOperatorIdList){
        QueryWrapper<OpLocationEntity> queryWrapper=new QueryWrapper<>();
        queryWrapper.eq("status", 1);
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("hubject_check", 1);
        if(!CollectionUtils.isEmpty(operatorIdList)){
            queryWrapper.in("operator_id", operatorIdList);
        }
        if(!CollectionUtils.isEmpty(excludeOperatorIdList)){
            queryWrapper.notIn("operator_id", excludeOperatorIdList);
        }

        if(StringUtils.isNotBlank(dateFrom)){
            queryWrapper.apply("IFNULL(updated_at,created_at) >= 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateFrom) + "')");
        }
        if(StringUtils.isNotBlank(dateTo)){
            queryWrapper.apply("IFNULL(updated_at,created_at) < 1000 * UNIX_TIMESTAMP('" + DateUtil.formatUtcYYYYMMDDHHmmss(dateTo) + "')");
        }
        queryWrapper.orderByAsc("created_at").orderByAsc("id");
        return opLocationMapper.selectCount(queryWrapper);
    }


    @Override
    public OcpiLocationVO getEvseLocation(String locationId, String evseUid) {
        OpLocationEntity location = opLocationMapper.selectById(locationId);
        if(location == null || location.getId() == null || location.getHubjectCheck() == null || !location.getHubjectCheck()){
            return null;
        }
        OcpiLocationMappingVO locationMappingVO = convertOcpiLocationMappingVO(location);
        log.info("locationMappingVO :{}", JSON.toJSONString(locationMappingVO));
        return getLocationVO(locationMappingVO, evseUid);
    }

    @Override
    public Boolean updateLocationOcpiEnalbed(String locationId, Boolean ocpiEnabled) {
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if(opLocationEntity == null){
            return false;
        }
        opLocationEntity.setOcpiEnabled(ocpiEnabled);
        return opLocationMapper.updateById(opLocationEntity) > 0;
    }

    @Override
    public Boolean queryLocationOcpiEnabled(String locationId) {
        Boolean ocpiEnalbed = opLocationMapper.queryLocationOcpiEnabled(locationId);
        if(ocpiEnalbed != null && ocpiEnalbed){
            return true;
        }
        return false;
    }

    @Override
    public Boolean queryTariffOcpiEnabled(String tariffId) {
        List<Boolean> ocpiEnabledList = opLocationMapper.queryTariffOcpiEnabled(tariffId);
        for(Boolean ocpiEnalbed : ocpiEnabledList){
            if(ocpiEnalbed != null && ocpiEnalbed){
                return true;
            }
        }
        return false;
    }

    /**
     * @param pageDTO
     * @return
     * @function 提供清单页面供EMP拉取(OCPI Gireve)
     */
    @Override
    public Page<EvseDynamicPricingVO> page(PageDTO pageDTO) {

        log.info("=====>>>>>OcpiLocationServiceImpl.page pageDTO : {}", JSON.toJSONString(pageDTO));

        // 先查询所有Gireve计费规则的基本信息
        Result<List<GireveCostModelRuleBasicInfoVO>> gireveCostModelRuleBasicInfoVO = tariffFeignClient.getAllGireveCostModelRuleBasicInfoVO();

        log.info("===>>>OcpiLocationServiceImpl.page gireveCostModelRuleBasicInfoVO : {}", JSON.toJSONString(gireveCostModelRuleBasicInfoVO));

        if (gireveCostModelRuleBasicInfoVO == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(gireveCostModelRuleBasicInfoVO.getCode())
                || ObjectUtils.isEmpty(gireveCostModelRuleBasicInfoVO.getData())) {

            log.info("===>>>OcpiLocationServiceImpl.page 当前环境不存在Gireve计费规则！");

            return new Page<>();
        }

        List<GireveCostModelRuleBasicInfoVO> gireveCostModelRuleBasicInfoVOList = gireveCostModelRuleBasicInfoVO.getData();
        // 计费规则组id集合
        List<Long> tariffGroupIdList = new ArrayList<>();
        // 构建计费规则组id和其下的Gireve计费规则id之间的映射关系
        Map<Long, Long> tariffGroupIdAndtariffIdMap = new HashMap<>();
        for (GireveCostModelRuleBasicInfoVO costModelRuleBasicInfoVO : gireveCostModelRuleBasicInfoVOList) {
            tariffGroupIdList.add(costModelRuleBasicInfoVO.getTariffGroupId());
            tariffGroupIdAndtariffIdMap.put(costModelRuleBasicInfoVO.getTariffGroupId(), costModelRuleBasicInfoVO.getId());
        }
        // 查询出所有开启了互联互通功能的充电桩的信息
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseService.findAllEroamingPile();
        if (ObjectUtils.isEmpty(opLocationPileEvseElasticDTOList)) {

            log.info("===>>>OcpiLocationServiceImpl.page 当前环境不存在开通了互联互通功能的充电桩！");

            return new Page<>();
        }

        // 充电枪id集合
        List<Long> evseIdList = new ArrayList<>();
        // 构建充电桩序列号与其信息之间的映射关系
        Map<String, OpLocationPileEvseElasticDTO> pileSnAndOpLocationPileEvseElasticDTOMap = new HashMap<>();
        Map<Long, String> sellerCpoMap = new HashMap<>();
        Map<Long, Long> locationSellerMap = new HashMap<>();
        for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
            locationSellerMap.put(opLocationPileEvseElasticDTO.getLocationId(), opLocationPileEvseElasticDTO.getOperatorId());
            if(StringUtils.isBlank(sellerCpoMap.get(opLocationPileEvseElasticDTO.getOperatorId()))){
                String cpoId = "";
                try{
                    RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(opLocationPileEvseElasticDTO.getOperatorId()).getData();
                    if(roamingRulesVO != null){
                        cpoId = roamingRulesVO.getOcpiId();
                    }
                }catch (Exception e){
                    log.error("获取ocpiId异常");
                }

                if(StringUtils.isBlank(cpoId)){
                    cpoId = countryCode + "*" + partyId;
                }
                sellerCpoMap.put(opLocationPileEvseElasticDTO.getOperatorId(), cpoId);
            }

            String evseList = opLocationPileEvseElasticDTO.getEvseList();
            if (StringUtils.isNotBlank(evseList)) {
                evseIdList.addAll(JSON.parseArray(evseList, Long.class));
            }
            pileSnAndOpLocationPileEvseElasticDTOMap.put(opLocationPileEvseElasticDTO.getPileSn(), opLocationPileEvseElasticDTO);
        }
        // 查询出所有开启了互联互通功能并且配置了含有Gireve计费规则的充电桩下的充电枪信息
        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseService.findEvseInfoList(tariffGroupIdList, evseIdList);
        if (ObjectUtils.isEmpty(opLocationEvseEntityList)) {

            log.info("===>>>OcpiLocationServiceImpl.page 当前环境不存在开启了互联互通功能并且配置了含有Gireve计费规则的充电桩下的充电枪！");

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

            String cpoId = sellerCpoMap.get(locationSellerMap.get(opLocationEvseEntity.getLocationId()));
            // 拼接evseId
            String evseId = cpoId + "*E" + pileSn + "*" + connector;
            currentPageEvseIdList.add(evseId);
            evseIdAndOpLocationEvseEntityMap.put(evseId, opLocationEvseEntity);
            evseIdAndPileSnMap.put(evseId, pileSn);
        }
        // 当前页Gireve计费规则的id集合
        List<Long> currentPageTariffIdList = new ArrayList<>();
        for (Long currentPageTariffGroupId : currentPageTariffGroupIdList) {
            currentPageTariffIdList.add(tariffGroupIdAndtariffIdMap.get(currentPageTariffGroupId));
        }
        // 查询Gireve计费规则的详情
        Result<List<CostModelRuleVO>> gireveCostModelRuleInfoResult = tariffFeignClient.getGireveCostModelRuleInfo(currentPageTariffIdList);

        log.info("===>>>OcpiLocationServiceImpl.page gireveCostModelRuleInfoResult : {}", JSON.toJSONString(gireveCostModelRuleInfoResult));

        if (gireveCostModelRuleInfoResult == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(gireveCostModelRuleInfoResult.getCode())
                || ObjectUtils.isEmpty(gireveCostModelRuleInfoResult.getData())) {

            log.info("===>>>OcpiLocationServiceImpl.page 当前环境不存在这样的Gireve计费规则！");

            return new Page<>();
        }

        List<CostModelRuleVO> costModelRuleVOList = gireveCostModelRuleInfoResult.getData();
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
        Page<EvseDynamicPricingVO> ocpiEvseInfoVOPage = new Page<>();
        List<EvseDynamicPricingVO> ocpiEvseInfoVOList = new ArrayList<>();
        for (String evseId : currentPageEvseIdList) {
            EvseDynamicPricingVO ocpiEvseInfoVO = new EvseDynamicPricingVO();
            ocpiEvseInfoVO.setEvseId(evseId);
            if (evseIdAndOpLocationEvseEntityMap.get(evseId) != null) {
                Long locationId = evseIdAndOpLocationEvseEntityMap.get(evseId).getLocationId();
                if (locationId != null && locationIdAndOpLocationEntityMap.get(locationId) != null) {
                    OpLocationEntity opLocationEntity = locationIdAndOpLocationEntityMap.get(locationId);
                    ocpiEvseInfoVO.setLocationAddress(opLocationEntity.getAddress());
                }
            }
            if (StringUtils.isNotBlank(evseIdAndPileSnMap.get(evseId))) {
                String pileSn = evseIdAndPileSnMap.get(evseId);
                if (pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn) != null) {
                    OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn);
                    ocpiEvseInfoVO.setPilePower(opLocationPileEvseElasticDTO.getPower());
                }
            }
            if (evseIdAndOpLocationEvseEntityMap.get(evseId) != null) {
                OpLocationEvseEntity opLocationEvseEntity = evseIdAndOpLocationEvseEntityMap.get(evseId);
                Long tariffId = opLocationEvseEntity.getTariffId();
                if (tariffGroupIdAndCostModelRuleVOMap.get(tariffId) != null) {
                    CostModelRuleVO costModelRuleVO = tariffGroupIdAndCostModelRuleVOMap.get(tariffId);
                    CostModelRuleVO costModelRule = JSON.parseObject(JSON.toJSONString(costModelRuleVO), CostModelRuleVO.class);
                    ocpiEvseInfoVO.setTariffId(costModelRule.getId().toString());
                    ocpiEvseInfoVO.setCostModelRuleVO(costModelRule);
                    if (RuleModelTypeEnum.FREE.getCode().equals(costModelRule.getRuleModelType())) {
                        ocpiEvseInfoVO.setFreeMark(true);
                    }
                }
            }
            ocpiEvseInfoVOList.add(ocpiEvseInfoVO);
        }
        ocpiEvseInfoVOPage
                .setRecords(ocpiEvseInfoVOList)
                .setTotal(opLocationEvseEntityList.size())
                .setSize(pageDTO.getPageSize())
                .setCurrent(pageDTO.getPage())
                .setPages(ListUtils.partition(opLocationEvseEntityList, pageDTO.getPageSize()).size());
        return ocpiEvseInfoVOPage;
    }

    /**
     * LocationVO 值获取和设置 ( 缺少必要字段： type, city, country)
     * @param locationMappingVO
     * @return
     */
    private OcpiLocationVO getLocationVO(OcpiLocationMappingVO locationMappingVO, String evseUid){
        String address = locationMappingVO.getStreet();
        if(StringUtils.isNotBlank(address) && address.length() > 45){
            address = address.substring(0,35) + "...";
        }

        Integer locationType = locationMappingVO.getLocationType();
        String type = "OTHER";
        switch (locationType) {
            case 1:
                type = "ON_STREET";
                break;
            case 2:
                type = "PARKING_GARAGE";
                break;
            case 3:
                type = "UNDERGROUND_GARAGE";
                break;
            case 4:
                type = "PARKING_LOT";
                break;
            case 5:
                type = "PARKING_LOT";
                break;
            case 6:
                type = "PARKING_LOT";
                break;
            case 7:
                type = "PARKING_LOT";
                break;
            case 8:
                type = "PARKING_LOT";
                break;
        }

        OcpiLocationVO locationVO =  OcpiLocationVO.builder().id(locationMappingVO.getId())
                .address(address)
                .name(locationMappingVO.getName())
                .postal_code(locationMappingVO.getPostCode())
                .last_updated(locationMappingVO.getUpdateTime())
                .type(type)
                .city(locationMappingVO.getCity())
                .country(locationMappingVO.getCountry())
                .time_zone(locationMappingVO.getTimeZone())
                .build();

        OcpiGeoLocation coordinates = new OcpiGeoLocation();
        coordinates.setLatitude(String.format("%.6f",Double.parseDouble(locationMappingVO.getLatitude())));
        coordinates.setLongitude(String.format("%.6f",Double.parseDouble(locationMappingVO.getLongitude())));
        locationVO.setCoordinates(coordinates);

        String cpoId = "";
        try{
            RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(locationMappingVO.getOperatorId()).getData();
            if(roamingRulesVO != null){
                cpoId = roamingRulesVO.getOcpiId();
            }
        }catch (Exception e){
            log.error("获取ocpiId异常");
        }

        if(StringUtils.isBlank(cpoId)){
            cpoId = countryCode + "*" + partyId;
        }

        List<OcpiEvseVO> evseVOList = new ArrayList<>();
        List<OpLocationEvseEntity> evseList = null;

        // 场站列表获取的数据是实时数据，cdr、session获取场站数据可能是历史数据，所以桩、计费可能是没有互联互通的也需要查
        Boolean isRealTimeData = StringUtils.isBlank(evseUid);
        if(isRealTimeData){
            evseList = opLocationEvseMapper.getEvseListByLocationId(Long.valueOf(locationMappingVO.getId()),evseUid);
        }else{
            evseList = opLocationEvseMapper.getEvseListByEvseId(evseUid);
        }
        log.info("evseList :{}", JSON.toJSONString(evseList));
        if(!CollectionUtils.isEmpty(evseList)){
            String finalCpoId = cpoId;
            evseList.forEach(g -> {
                OcpiEvseVO ocpiEvseVO = getEvseVO(g, finalCpoId);
                if(ocpiEvseVO != null && !CollectionUtils.isEmpty(ocpiEvseVO.getConnectors())){
                    evseVOList.add(ocpiEvseVO);
                }
            });
        }
        locationVO.setEvses(evseVOList);

       /* QueryWrapper<OpLocationOperationEntity> queryWrapper=new QueryWrapper<>();
        queryWrapper.eq("status", 1);
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("location_id", locationMappingVO.getId());
        queryWrapper.select("operation_date","updated_at");*/
        //OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(queryWrapper);
        HoursVO hoursVO = new HoursVO();
        hoursVO.setTwentyfourseven(true);
        locationVO.setOpening_times(hoursVO);
        return locationVO;
    }

    private HoursVO getOpeningTimes(OpLocationOperationEntity opLocationOperationEntity) {
        Long operationDate = opLocationOperationEntity.getOperationDate();
        Long updateTime = opLocationOperationEntity.getUpdatedAt();
        List<RegularHoursVO> regularHoursList = new ArrayList<>();
        for(int i=1; i<8; i++){
            RegularHoursVO regularHoursVO = RegularHoursVO.builder()
                    .weekday(i)
                    .period_begin("00:00")
                    .period_end("24:00")
                    .build();
            regularHoursList.add(regularHoursVO);
        }

        String openingTime = DateUtil.timeStamp2ISO8601(operationDate);
        String beforeTime = DateUtil.timeStamp2ISO8601(updateTime);
        String endTime = DateUtil.timeStamp2ISO8601(operationDate + 157680000000L);
        List<ExceptionalPeriodVO> exceptionalOpeningList = new ArrayList<>();
        ExceptionalPeriodVO exceptionalOpeningVO = ExceptionalPeriodVO.builder()
                .period_begin(openingTime)
                .period_end(endTime)
                .build();
        exceptionalOpeningList.add(exceptionalOpeningVO);

        List<ExceptionalPeriodVO> exceptionalClosingList = new ArrayList<>();
        ExceptionalPeriodVO exceptionalClosingVO = ExceptionalPeriodVO.builder()
                .period_begin(beforeTime)
                .period_end(openingTime)
                .build();
        exceptionalClosingList.add(exceptionalClosingVO);
        return HoursVO.builder().regular_hours(regularHoursList)
                .twentyfourseven(true)
                .exceptional_openings(exceptionalOpeningList)
                .exceptional_closings(exceptionalClosingList)
                .build();
    }

    /**
     * evseVO数据获取
     * @param evse
     * @return
     */
    private OcpiEvseVO getEvseVO(OpLocationEvseEntity evse, String cpoId) {
        log.info("getEvseVO evse:{}", JSON.toJSONString(evse));
        OcpiEvseVO evseVO = OcpiEvseVO.builder().uid(evse.getId().toString()).build();
        evseVO.setStatus("UNKNOWN");
        evseVO.setCapabilities(evseCapabilities);

        String pileSn = evse.getEvseSn().substring(0, evse.getEvseSn().indexOf("_"));
        String gunNo = evse.getEvseSn().substring(evse.getEvseSn().indexOf("_") + 1, evse.getEvseSn().length());
        evseVO.setEvse_id(cpoId + "*E" + pileSn + "*" + gunNo);
        evseVO.setPhysical_reference(pileSn);
        try{
            String evseStatus = monitorFeignClient.queryStatusByEvseSn(evse.getEvseSn()).getData();
            log.info("枪{}:监控状态：{}", evse.getEvseSn(), evseStatus);
            if(StringUtils.isNotBlank(evseStatus)){
                switch (evseStatus){
                    case "Available":
                    case "Preparing":
                        evseVO.setStatus("AVAILABLE");
                        break;
                    case "Charging":
                        evseVO.setStatus("CHARGING");
                        break;
                    case "Unavailable":
                        evseVO.setStatus("INOPERATIVE");
                        break;
                    default:
                        evseVO.setStatus("BLOCKED");
                        break;
                }
            }
        }catch (Exception e){
            log.error("获取枪状态据异常： {}", e.getStackTrace());
        }

        List<OcpiConnectorVO> connectors = getConnectorVOList(evse.getId());
        evseVO.setConnectors(connectors);
        evseVO.setLast_updated(DateUtil.timeStamp2ISO8601(evse.getUpdatedAt()));
        log.info("evseVO :{}", JSON.toJSONString(evseVO));
        return evseVO;
    }

    /**
     * 基于枪来获取connector数据
     * @param evseId
     * @return
     */
    private List<OcpiConnectorVO> getConnectorVOList(Long evseId){
        List<OcpiConnectorVO> connectors = new ArrayList<>();
        try{
            String tariffId = null;
            try{
                TariffVO tariffVO = ocpiTariffFeignClient.getEvseTariff(evseId.toString()).getData();
                if(tariffVO != null){
                    tariffId = tariffVO.getId();
                }
            }catch (Exception e){
            }

            List<OpLocationConnectorEntity> connectorList = opLocationConnectorMapper.getConnectorListByEvseId(evseId);
            if(!CollectionUtils.isEmpty(connectorList)){
                String finalTariffId = tariffId;
                connectorList.forEach(connector ->{
                    String powerType = connector.getPowerType();
                    if(StringUtils.isNotBlank(powerType) && powerType.startsWith("DC")){
                        powerType = "DC";
                        connector.setFormat(StringUtils.defaultIfBlank(connector.getFormat(),"CABLE").toUpperCase());
                    }

                    int voltage = connector.getVoltage().intValue();
                    if(connector.getPower() <= 22 && connector.getVoltage().intValue() > 230){
                        voltage = 230;
                    }

                    OcpiConnectorVO connectorVO = OcpiConnectorVO.builder()
                            .id(connector.getId().toString())
                            .amperage(connector.getAmperage().intValue())
                            .voltage(voltage)
                            .standard(convertStandard(connector.getGunType()))
                            .power_type(StringUtils.defaultIfBlank(powerType, "AC_1_PHASE").toUpperCase())
                            .format(StringUtils.defaultIfBlank(connector.getFormat(),"SOCKET").toUpperCase())
                            .last_updated(DateUtil.timeStamp2ISO8601(connector.getUpdatedAt()))
                            .build();

                    connectorVO.setTariff_id(finalTariffId);
                    connectors.add(connectorVO);
                });
            }
        }catch (Exception e){
            log.error("获取桩数据异常： {}", e.getStackTrace());
        }
        return connectors;
    }

    private String convertStandard(Integer gunType) {
        String result = "IEC_62196_T2";
        switch (gunType) {
            case 1:
                result = "IEC_62196_T2_COMBO";
                break;
            case 2:
                result = "CHADEMO";
                break;
            case 3:
                result = "IEC_62196_T2";
                break;
            case 4:
                result = "IEC_62196_T3C";
                break;
            case 5:
                result = "IEC_62196_T1";
                break;
            case 6:
                result = "DOMESTIC_E";
                break;
            case 7:
                result = "IEC_62196_T1_COMBO";
                break;
            case 10:
                result = "TESLA_R";
                break;
            default:
                break;
        }
        return result;
    }
}