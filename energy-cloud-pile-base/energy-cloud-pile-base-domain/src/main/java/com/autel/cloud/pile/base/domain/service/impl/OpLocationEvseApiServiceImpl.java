package com.autel.cloud.pile.base.domain.service.impl;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.LocationEvseStatusMappingEnum;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseApiService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.api.LocationEvsePageQueryDto;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileBillServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileMonitorServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseTypeEntity;
import com.autel.cloud.pile.base.vo.api.LocationEvseApiInfoVO;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.text.DecimalFormat;
import java.util.*;
import java.util.stream.Collectors;


@RefreshScope
@Service
@Log4j2
public class OpLocationEvseApiServiceImpl implements OpLocationEvseApiService {

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private OpLocationService opLocationService;

    @Resource
    private PileBillServiceAdapter pileBillServiceAdapter;

    @Resource
    private PileMonitorServiceAdapter pileMonitorServiceAdapter;

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String,Object> redisTemplate;

    @Override
    public OpenApiPagelVO<LocationEvseApiInfoVO> list(LocationEvsePageQueryDto dto) {

        OpenApiPagelVO<LocationEvseApiInfoVO> resultPage = new OpenApiPagelVO<>(dto.getPage(), dto.getPageSize(), 0);
        resultPage.setContent(Lists.newArrayList());

        //1. 查询权限场站范围
        List<Long> locationIds = new ArrayList<>(opLocationService.getLocationIdBySellerId(dto.getSellerId()));
        //List<Long> locationIds = Arrays.asList(1608703060025745617L,199L,3004L);
        Long siteId = dto.getSiteId();

        if(CollectionUtils.isEmpty(locationIds) || (null != siteId && !locationIds.contains(siteId) )){
            log.info("locationIds is {},siteId is {}", JSON.toJSONString(locationIds),siteId);
            return resultPage;
        }
        if(StringUtils.isNotBlank(dto.getSiteName())){
            dto.setSiteName( dto.getSiteName().trim());
        }
        if(StringUtils.isNotBlank(dto.getChargerName())){
            dto.setChargerName( dto.getChargerName().trim());
        }

        if(StringUtils.isNotBlank(dto.getState())){
            List<LocationEvseStatusMappingEnum> evseStatueNameByLocationEvse = LocationEvseStatusMappingEnum.getEvseStatueNameByLocationEvse(dto.getState());
            if(CollectionUtils.isNotEmpty(evseStatueNameByLocationEvse)){
                List<String> evseStates = evseStatueNameByLocationEvse.stream().map(LocationEvseStatusMappingEnum::getName).distinct().collect(Collectors.toList());
                dto.setEvseStates(evseStates);
            }else {
                log.error("OpLocationEvseApiServiceImpl list param state is error {}",JSON.toJSONString(dto));
                return resultPage;
            }
        }

        if(null != siteId){
            locationIds = Arrays.asList(siteId);
        }
        //2. 查询设备数据
        // 如果 chargerId 是空的话，直接查 枪es 数据, 负责 先查桩 再查枪
        Long chargerId = dto.getChargerId();
        String pileSn  = null;
        if( null != chargerId ){
            //1. 查询桩es数据
            List<OpLocationPileEvseElasticDTO> pileEvseElasticDTOS = queryLocationPileEvseBaseInfo(locationIds, dto.getSiteName(), dto.getChargerId(), dto.getChargerName());
            log.info("pileEvseElasticDTOS is {}",JSON.toJSONString(pileEvseElasticDTOS));
            if(CollectionUtils.isEmpty(pileEvseElasticDTOS)){
                return resultPage;
            }
            pileSn = pileEvseElasticDTOS.get(0).getPileSn();
        }
        // 查询枪数据
        OpenApiPagelVO<LocationEvseApiInfoVO> voPage = queryLocationEvseBaseInfo(locationIds, pileSn, dto);
        List<LocationEvseApiInfoVO> records = voPage.getContent();
        // 填充 chargerId 和  activeAt
        fillData(records,chargerId);
        // 填充枪试试数据
        // 填充实时电压 电流 功率数据
        fillNewMeters(records);
        return voPage;
    }

    @Override
    public LocationEvseApiInfoVO queryOpLocationEvseById(Long id,Long sellerId) {
        // 根据枪id 查询枪详情
        List<Long> locationIds = new ArrayList<>( opLocationService.getLocationIdBySellerId( sellerId ) );
        //List<Long> locationIds = Arrays.asList(1608703060025745617L,199L,3004L);
        LocationEvseApiInfoVO locationEvseApiInfoVO = new LocationEvseApiInfoVO();
        if(CollectionUtils.isEmpty(locationIds)){
            return locationEvseApiInfoVO;
        }
        // 根据枪id 和 场站id,查询枪详情
        List<LocationEvseApiInfoVO> records = queryLocationEvseByEvseId(locationIds, id);
        if(CollectionUtils.isEmpty(records)){
            return locationEvseApiInfoVO;
        }
        fillData(records,null);
        // 填充实时电压 电流 功率数据
        fillNewMeters(records);
        locationEvseApiInfoVO = records.get(0);
        return locationEvseApiInfoVO;
    }

    public void fillNewMeters(List<LocationEvseApiInfoVO> records){
        //1. 获取充电桩的枪sn
        if(CollectionUtils.isEmpty(records)){
            return;
        }
        List<String> chargePointSns = records.stream().map(LocationEvseApiInfoVO::getEvseSn)
                .filter(item -> StringUtils.isNotBlank(item)).distinct().collect(Collectors.toList());
        if(CollectionUtils.isEmpty(chargePointSns)){
            return;
        }
        // 查询枪实时数据
        List<OpEvseMeterUploadDTO> opEvseMeters = pileMonitorServiceAdapter.queryNewMeterByEvseSnList(chargePointSns);
        /*if(CollectionUtils.isEmpty(opEvseMeters)){
            return;
        }*/
        Map<String, OpEvseMeterUploadDTO> groupByEvseSn = opEvseMeters.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, v -> v, (v1, v2) -> v1));
        DecimalFormat decimalFormat = new DecimalFormat("#.###");
        records.stream().forEach(item -> {
            String evseSn = item.getEvseSn();
            if(StringUtils.isNotBlank(evseSn)){
                OpEvseMeterUploadDTO opEvseMeterUploadDTO = groupByEvseSn.get(evseSn);
                if(null != opEvseMeterUploadDTO){
                    Double power = opEvseMeterUploadDTO.getPower();
                    if(null == power || power == 0){
                        item.setInstantKw("0");
                    }else {
                        double kw = power / 1000;
                        String powerStr = decimalFormat.format(kw);
                        item.setInstantKw(powerStr);
                    }
                    Double current = opEvseMeterUploadDTO.getCurrent();
                    if(null != current){
                        String currentStr = decimalFormat.format(current);
                        item.setInstantCurrent(currentStr);
                    }
                    Double voltage = opEvseMeterUploadDTO.getVoltage();
                    if(null != voltage){
                        String voltageStr = decimalFormat.format(voltage);
                        item.setInstantVoltage(voltageStr);
                    }
                }else {
                    item.setInstantCurrent("0");
                    item.setInstantKw("0");
                    item.setInstantVoltage("0");
                }
            }
        });

    }

    public List<LocationEvseApiInfoVO> queryLocationEvseByEvseId(List<Long> locationIds,Long id)
    {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId",locationIds));
        queryBuilder.must(QueryBuilders.matchQuery("id",id));

        log.info("OpLocationEvseApiServiceImpl -> queryLocationEvseByEvseId es query condition   is {}", queryBuilder.toString());


        SearchHits<OpLocationEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .build(), OpLocationEvseElasticDTO.class);

        List<OpLocationEvseElasticDTO> pileEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("OpLocationEvseApiServiceImpl -> queryLocationEvseByEvseId-->{}", JSON.toJSONString(pileEvseElastics));
        // 数据类型转换
        List<LocationEvseApiInfoVO> locationEvses = changeTo(pileEvseElastics);
        return locationEvses;
    }

    public void fillData(List<LocationEvseApiInfoVO> records,Long chargerId){
        if( null != chargerId){
            records.stream().forEach(item -> item.setChargerId(chargerId));
        }else {
            // 根据 pileSn 查询  chargerId
            List<String> pileSns = records.stream().map(LocationEvseApiInfoVO::getChargerSn).filter(item -> null != item).distinct().collect(Collectors.toList());
            List<OpLocationPileEvseElasticDTO> pileEvses = queryLocationPileEvseChargeId(pileSns);
            if(CollectionUtils.isNotEmpty(pileEvses)){
                Map<String, OpLocationPileEvseElasticDTO> groupByPileSn = pileEvses.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, v -> v, (v1, v2) -> v1));
                records.stream().forEach(item -> {
                    OpLocationPileEvseElasticDTO opLocationPile = groupByPileSn.get(item.getChargerSn());
                    if(null != opLocationPile){
                        item.setChargerId(opLocationPile.getId());
                    }
                });
            }
        }
        // 填充最近充电时间
        List<String> evseSns = records.stream().map(LocationEvseApiInfoVO::getEvseSn).filter(item -> null != item).distinct().collect(Collectors.toList());
        List<EnergyBillVO> lastBillInfoByEvseList = pileBillServiceAdapter.findLastBillInfoByEvseList(evseSns);
        if(CollectionUtils.isNotEmpty(lastBillInfoByEvseList)){
            Map<String, EnergyBillVO> groupByEvseSn = lastBillInfoByEvseList.stream().collect(Collectors.toMap(EnergyBillVO::getEvseSn, v -> v, (v1, v2) -> v1));
            records.stream().forEach(item -> {
                EnergyBillVO billCDRInfoVO = groupByEvseSn.get(item.getEvseSn());
                if(null != billCDRInfoVO){
                    item.setActiveAt(billCDRInfoVO.getCreateTime());
                }
            });
        }
    }



    public OpenApiPagelVO<LocationEvseApiInfoVO> queryLocationEvseBaseInfo(List<Long> locationIds,String pileSn,LocationEvsePageQueryDto dto )
    {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId",locationIds));
        if(StringUtils.isNotBlank( dto.getSiteName() )){
            //String siteName = QueryParserBase.escape(dto.getSiteName());
            queryBuilder.must(QueryBuilders.matchQuery("locationName", dto.getSiteName() ));
        }

        if(StringUtils.isNotBlank( pileSn )){
            queryBuilder.must(QueryBuilders.matchQuery("pileSn", pileSn));
        }

        if(StringUtils.isNotBlank( dto.getChargerName() )){
            //String chargerName = QueryParserBase.escape(dto.getChargerName());
            queryBuilder.must(QueryBuilders.matchQuery("pileName", dto.getChargerName()));
        }

        if(CollectionUtils.isNotEmpty( dto.getEvseStates() )){
            //String state = QueryParserBase.escape(dto.getState());
            //queryBuilder.must(QueryBuilders.wildcardQuery("state", String.format("%s", state)));
            queryBuilder.must(QueryBuilders.termsQuery("state", dto.getEvseStates()));
        }

        log.info("OpLocationEvseApiServiceImpl -> queryLocationEvseBaseInfo es query condition   is {}", queryBuilder.toString());

        String orderBy = "createdAt";
        SortOrder sortOrder = SortOrder.DESC;

        SearchHits<OpLocationEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(dto.getPage().intValue() - 1, dto.getPageSize().intValue()))
                .withSorts(SortBuilders.fieldSort(orderBy).order(sortOrder))
                .build(), OpLocationEvseElasticDTO.class);

        long totalHits = searchHits.getTotalHits();
        List<OpLocationEvseElasticDTO> pileEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("OpLocationEvseApiServiceImpl -> queryLocationEvseBaseInfo-->{}", JSON.toJSONString(pileEvseElastics));
        // 数据类型转换
        OpenApiPagelVO<LocationEvseApiInfoVO> resultPage = new OpenApiPagelVO<>(dto.getPage(), dto.getPageSize(), 0);
        resultPage.setTotalRows(totalHits);
        List<LocationEvseApiInfoVO> locationEvses = changeTo(pileEvseElastics);
        resultPage.setContent(locationEvses);
        resultPage.initTotalPages();
        return resultPage;
    }

    private List<LocationEvseApiInfoVO> changeTo(List<OpLocationEvseElasticDTO> pileEvseElastics){
        if(CollectionUtils.isEmpty(pileEvseElastics)){
            return Lists.newArrayList();
        }

        List<OpEvseTypeEntity> evseTypeList = (List<OpEvseTypeEntity>) redisTemplate.opsForValue().get(RedisKeyConstant.getStringEvseTypeList());
        Map<Integer, OpEvseTypeEntity> evseType = Maps.newHashMap();
        if(CollectionUtils.isNotEmpty(evseTypeList)){
            evseType = evseTypeList.stream().collect(Collectors.toMap(OpEvseTypeEntity::getGunType, (v -> v), (v1, v2) -> v1));
        }
        Map<Integer, OpEvseTypeEntity> finalEvseType = evseType;
        List<LocationEvseApiInfoVO> collect = pileEvseElastics.stream().map(item -> {
            LocationEvseApiInfoVO locationEvseApiInfoVO = new LocationEvseApiInfoVO(item.getId(), item.getEvseId(), item.getEvseSn(),item.getLocationId(), item.getLocationName(),
                    null, item.getPileName(), item.getPileSn(),item.getCreatedAt(),null, item.getPowerType(), String.valueOf(item.getGunType()), item.getState(),
                    Objects.toString(item.getPower(),null));
            String chargerType = locationEvseApiInfoVO.getChargerType();
            if (StringUtils.isNotBlank(chargerType)) {
                String lowerCase = chargerType.toLowerCase();
                if (lowerCase.startsWith("ac")) {
                    locationEvseApiInfoVO.setChargerType("AC");
                } else {
                    locationEvseApiInfoVO.setChargerType("DC");
                }
            }
            // 补充枪类型
            OpEvseTypeEntity opEvseTypeEntity = finalEvseType.get(item.getGunType());
            if(null != opEvseTypeEntity){
                locationEvseApiInfoVO.setType(opEvseTypeEntity.getName());
            }
            // 补全枪状态
            LocationEvseStatusMappingEnum evseName = LocationEvseStatusMappingEnum.getLocationEvseStatueNameByEvseState(locationEvseApiInfoVO.getState());
            locationEvseApiInfoVO.setState(evseName.getLocationName());

            return locationEvseApiInfoVO;
        }).collect(Collectors.toList());
        return  collect;
    }

    public List<OpLocationPileEvseElasticDTO> queryLocationPileEvseChargeId(List<String> pileSns )
    {
        if(CollectionUtils.isEmpty(pileSns)){
            return Lists.newArrayList();
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("pileSn",pileSns));

        log.info("OpLocationEvseApiServiceImpl -> queryLocationPileEvseChargeId es query condition   is {}", queryBuilder.toString());


        SearchHits<OpLocationPileEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .build(), OpLocationPileEvseElasticDTO.class);

        List<OpLocationPileEvseElasticDTO> pileEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        return pileEvseElastics;
    }

    public List<OpLocationPileEvseElasticDTO> queryLocationPileEvseBaseInfo(List<Long> locationIds,String siteName, Long chargerId,String chargerName )
    {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId",locationIds));
        if(StringUtils.isNotBlank( siteName )){
            siteName = QueryParserBase.escape(siteName);
            queryBuilder.must(QueryBuilders.matchQuery("locationName", siteName));
        }
        if( null != chargerId){
            queryBuilder.must(QueryBuilders.matchQuery("id", chargerId));
        }
        if(StringUtils.isNotBlank( chargerName )){
            chargerName = QueryParserBase.escape(chargerName);
            queryBuilder.must(QueryBuilders.matchQuery("name", chargerName));
        }

        log.info("OpLocationEvseApiServiceImpl -> queryLocationPileEvseBaseInfo es query condition   is {}", queryBuilder.toString());

        String orderBy = "createdAt";
        SortOrder sortOrder = SortOrder.DESC;

        SearchHits<OpLocationPileEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort(orderBy).order(sortOrder))
                .build(), OpLocationPileEvseElasticDTO.class);

        List<OpLocationPileEvseElasticDTO> pileEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        return pileEvseElastics;
    }
}
