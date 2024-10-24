package com.autel.cloud.pile.base.domain.service.impl;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.LocationEvseStatusMappingEnum;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseApiService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.api.LocationPilePageQueryDto;
import com.autel.cloud.pile.base.dto.api.LocationPileQueryDto;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileMonitorServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseTypeEntity;
import com.autel.cloud.pile.base.vo.api.EvseApiInfoVO;
import com.autel.cloud.pile.base.vo.api.PileEvseApiInfoVO;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


@RefreshScope
@Service
@Log4j2
public class OpLocationPileEvseApiServiceImpl implements OpLocationPileEvseApiService {

    @Resource
    private OpLocationService opLocationService;

    @Resource
    private PileMonitorServiceAdapter pileMonitorServiceAdapter;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String,Object> redisTemplate;

    @Override
    public OpenApiPagelVO<PileEvseApiInfoVO> list(LocationPilePageQueryDto dto) {

        OpenApiPagelVO<PileEvseApiInfoVO> resultPage = new OpenApiPagelVO<PileEvseApiInfoVO>(dto.getPage(), dto.getPageSize(), 0);
        resultPage.setContent(Lists.newArrayList());

        //1. 根据场站权限查询数据
        Long siteId = dto.getSiteId();
        List<Long> locationIds = new ArrayList<>(opLocationService.getLocationIdBySellerId(dto.getSellerId()));
        //List<Long> locationIds = Arrays.asList(1608703060025745617L,199L,3004L);
        if(CollectionUtils.isEmpty(locationIds) || (null != siteId && !locationIds.contains(siteId) )){
            log.info("locationIds is {},siteId is {}", JSON.toJSONString(locationIds),siteId);
            return resultPage;
        }
        if(null != siteId){
            locationIds = Arrays.asList(siteId);
        }
        if(StringUtils.isNotBlank(dto.getSiteName())){
            dto.setSiteName(dto.getSiteName().trim());
        }

        // 2. 查询设备信息
        OpenApiPagelVO<PileEvseApiInfoVO> voPage = queryPageLocationPileBaseInfo(locationIds, dto.getSiteName(),null,null, dto.getPage(), dto.getPageSize());
        // 3. 查询设备的枪信息
        List<PileEvseApiInfoVO> records = voPage.getContent();
        queryLocationEvseBaseInfo(records);
        fillNewMeters( records );
        return voPage;
    }

    @Override
    public List<PileEvseApiInfoVO> queryLocation(LocationPileQueryDto dto) {
        //1. 权限场站数据获取
        if(null == dto.getChargerId() &&  StringUtils.isBlank(dto.getChargerName())){
            log.error("siteId is null and siteName is null,dto is {}",JSON.toJSONString(dto));
            throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
        }
        List<Long> locationIds = new ArrayList<>( opLocationService.getLocationIdBySellerId(dto.getSellerId()) );
        //List<Long> locationIds = Arrays.asList(1608703060025745617L,199L,3004L);
        if(CollectionUtils.isEmpty(locationIds) ) {
            log.info("locationIds is {}", JSON.toJSONString(locationIds));
            return Lists.newArrayList();
        }
        if(StringUtils.isNotBlank(dto.getChargerName())){
            dto.setChargerName(dto.getChargerName().trim());
        }
        // 根据设备id和充电桩名称查询设备信息
        OpenApiPagelVO<PileEvseApiInfoVO> voPage = queryPageLocationPileBaseInfo(locationIds, null, dto.getChargerId(), dto.getChargerName(), 1L, 200L);
        List<PileEvseApiInfoVO> records = voPage.getContent();
        queryLocationEvseBaseInfo(records);
        // 填充充电枪的瞬时功率 电流 电压
        fillNewMeters( records );
        return records;
    }

    public void fillNewMeters(List<PileEvseApiInfoVO> records){
        //1. 获取充电桩的枪sn
        if(CollectionUtils.isEmpty(records)){
            return;
        }
        List<String> chargePointSns = records.stream().map(PileEvseApiInfoVO::getChargePoints).flatMap(List::stream).map(EvseApiInfoVO::getChargePointSn)
                .filter(item -> StringUtils.isNotBlank(item)).distinct().collect(Collectors.toList());
        if(CollectionUtils.isEmpty(chargePointSns)){
            return;
        }
        // 查询枪实时数据
        List<OpEvseMeterUploadDTO> opEvseMeters = pileMonitorServiceAdapter.queryNewMeterByEvseSnList(chargePointSns);
        /*
        if(CollectionUtils.isEmpty(opEvseMeters)){
            return;
        }
        */
        Map<String, OpEvseMeterUploadDTO> groupByEvseSn = opEvseMeters.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, v -> v, (v1, v2) -> v1));
        DecimalFormat decimalFormat = new DecimalFormat("#.###");
        records.stream().forEach(item -> {
            List<EvseApiInfoVO> chargePoints = item.getChargePoints();
            if(CollectionUtils.isNotEmpty(chargePoints)){
                chargePoints.stream().forEach(item2 -> {
                    String chargePointSn = item2.getChargePointSn();
                    OpEvseMeterUploadDTO opEvseMeterUploadDTO = groupByEvseSn.get(chargePointSn);
                    if(null != opEvseMeterUploadDTO){
                        Double power = opEvseMeterUploadDTO.getPower();
                        if(null == power || power == 0){
                            item2.setInstantKw("0");
                        }else {
                            double kw = power / 1000;
                            String powerStr = decimalFormat.format(kw);
                            item2.setInstantKw(powerStr);
                        }
                        Double current = opEvseMeterUploadDTO.getCurrent();
                        if(null != current){
                            String currentStr = decimalFormat.format(current);
                            item2.setInstantCurrent(currentStr);
                        }
                        Double voltage = opEvseMeterUploadDTO.getVoltage();
                        if(null != voltage){
                            String voltageStr = decimalFormat.format(voltage);
                            item2.setInstantVoltage(voltageStr);
                        }
                    }else {
                        item2.setInstantCurrent("0");
                        item2.setInstantKw("0");
                        item2.setInstantVoltage("0");
                    }
                });
            }
        });

    }

    public void queryLocationEvseBaseInfo(List<PileEvseApiInfoVO> records)
    {
        if(CollectionUtils.isEmpty(records)){
            return;
        }

        List<String> pileSns = records.stream().map(PileEvseApiInfoVO::getChargerSn).distinct().filter(item -> StringUtils.isNotBlank(item)).collect(Collectors.toList());
        if(CollectionUtils.isEmpty(pileSns)){
            return;
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("pileSn",pileSns));
        log.info("OpLocationPileEvseApiServiceImpl -> queryLocationEvseBaseInfo es query condition   is {}", queryBuilder.toString());

        SearchHits<OpLocationEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class);

        List<OpLocationEvseElasticDTO> opLocationEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("OpLocationPileEvseApiServiceImpl -> opLocationEvseElastics-->{}", JSON.toJSONString(opLocationEvseElastics));
        // 补充枪信息
        Map<String, List<OpLocationEvseElasticDTO>> groupByPileSn = opLocationEvseElastics.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        // 枪类型
        List<OpEvseTypeEntity> evseTypeList = (List<OpEvseTypeEntity>) redisTemplate.opsForValue().get(RedisKeyConstant.getStringEvseTypeList());
        Map<Integer, OpEvseTypeEntity> evseType = Maps.newHashMap();
        if(CollectionUtils.isNotEmpty(evseTypeList)){
            evseType = evseTypeList.stream().collect(Collectors.toMap(OpEvseTypeEntity::getGunType, (v -> v), (v1, v2) -> v1));
        }

        Map<Integer, OpEvseTypeEntity> finalEvseType = evseType;
        records.stream().forEach(item -> {
            String chargerSn = item.getChargerSn();
            List<OpLocationEvseElasticDTO> opLocationEvses = groupByPileSn.get(chargerSn);
            if(CollectionUtils.isNotEmpty(opLocationEvses)){
                List<EvseApiInfoVO> evseApiInfos = opLocationEvses.stream().map(seed -> {
                    EvseApiInfoVO evseApiInfoVO = new EvseApiInfoVO(seed.getId(), seed.getEvseSn(), String.valueOf(seed.getGunType()), seed.getState());
                    OpEvseTypeEntity opEvseTypeEntity = finalEvseType.get(seed.getGunType());
                    if(null != opEvseTypeEntity){
                        evseApiInfoVO.setChargePointType( opEvseTypeEntity.getName() );
                    }
                    // 枪状态转换
                    LocationEvseStatusMappingEnum evseName = LocationEvseStatusMappingEnum.getLocationEvseStatueNameByEvseState(evseApiInfoVO.getChargePointState());
                    evseApiInfoVO.setChargePointState(evseName.getLocationName());
                    return evseApiInfoVO;
                }).collect(Collectors.toList());
                item.setChargePoints(evseApiInfos);
            }
        });
    }

    public OpenApiPagelVO<PileEvseApiInfoVO> queryPageLocationPileBaseInfo(List<Long> locationIds,String locationName,Long chargerId,String chargerName,Long page ,Long pageSize)
    {
        OpenApiPagelVO<PileEvseApiInfoVO> resultPage = new OpenApiPagelVO<>(page, pageSize, 0);
        resultPage.setContent(Lists.newArrayList());

        if(CollectionUtils.isEmpty(locationIds)){
            return resultPage;
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId",locationIds));

        if(StringUtils.isNotBlank( locationName )){
            //locationName = QueryParserBase.escape(locationName);
            queryBuilder.must(QueryBuilders.matchQuery("locationName", locationName));
        }

        if( null != chargerId){
            queryBuilder.must(QueryBuilders.matchQuery("id", chargerId));
        }

        if(StringUtils.isNotBlank( chargerName )){
            //chargerName = QueryParserBase.escape(chargerName);
            queryBuilder.must(QueryBuilders.matchQuery("name", chargerName));
        }

        log.info("OpLocationPileEvseApiServiceImpl -> queryLocationBaseInfo es query condition   is {}", queryBuilder.toString());

        String orderBy = "createdAt";
        SortOrder sortOrder = SortOrder.DESC;


        SearchHits<OpLocationPileEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(page.intValue() - 1, pageSize.intValue()))
                .withSorts(SortBuilders.fieldSort(orderBy).order(sortOrder))
                .build(), OpLocationPileEvseElasticDTO.class);
        // 总记录数
        long totalHits = searchHits.getTotalHits();
        List<OpLocationPileEvseElasticDTO> pileEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("OpLocationPileEvseApiServiceImpl -> queryLocationPileBaseInfo-->{}", JSON.toJSONString(pileEvseElastics));
        //类型转换

        List<PileEvseApiInfoVO> list = pileEvseElastics.stream().map(item -> {
            PileEvseApiInfoVO pileEvseApiInfoVO = new PileEvseApiInfoVO(item.getId(), item.getLocationId(), item.getLocationName(), item.getName(), item.getPileSn(), item.getCreatedAt(),
                    item.getUpdatedAt(), item.getPower(), item.getPowerType(), item.getBrandName(), item.getPowerType(), null);
            String chargerType = pileEvseApiInfoVO.getChargerType();
            if(StringUtils.isNotBlank(chargerType)){
                String lowerCase = chargerType.toLowerCase();
                if(lowerCase.startsWith("ac")){
                    pileEvseApiInfoVO.setChargerType("AC");
                }else {
                    pileEvseApiInfoVO.setChargerType("DC");
                }
            }
            return pileEvseApiInfoVO;
        }).collect(Collectors.toList());

        resultPage.setContent(list);
        resultPage.setTotalRows(totalHits);
        resultPage.initTotalPages();
        return resultPage;
    }
}
