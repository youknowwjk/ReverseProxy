package com.autel.cloud.pile.base.domain.service.impl;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.openapi.dto.OpenApiPageDto;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.domain.service.OpLocationApiService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.api.LocationQueryDto;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.vo.OcpiGeoLocation;
import com.autel.cloud.pile.base.vo.api.AddressApiVO;
import com.autel.cloud.pile.base.vo.api.LocationApiInfoVO;
import com.autel.cloud.pile.base.vo.api.StreetApiVO;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.aggregations.Aggregation;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.elasticsearch.search.aggregations.bucket.terms.TermsAggregationBuilder;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.clients.elasticsearch7.ElasticsearchAggregations;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


@RefreshScope
@Service
@Log4j2
public class OpLocationApiServiceImpl implements OpLocationApiService {

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private OpLocationService opLocationService;

    public List<String> chargingState = Arrays.asList(EvseDeviceStatusEnum.CHARGING.getName(),EvseDeviceStatusEnum.SUSPENDED_EVSE.getName());
    public List<String> occupiedState = Arrays.asList(EvseDeviceStatusEnum.FINISHING.getName(),EvseDeviceStatusEnum.PREPARING.getName(),EvseDeviceStatusEnum.SUSPENDED_EV.getName());
    public List<String> availableState = Arrays.asList(EvseDeviceStatusEnum.AVAILABLE.getName());


    @Override
    public OpenApiPagelVO<LocationApiInfoVO> list(OpenApiPageDto dto) {
        //1. 查询当前商家有权限的场站信息
        List<Long> locationIds = new ArrayList<>(opLocationService.getLocationIdBySellerId(dto.getSellerId()));
        //List<Long> locationIds = Arrays.asList(1608703060025745617L,199L,3004L);
        log.info("OpLocationApiServiceImpl -> list,locationIds is {}",JSON.toJSONString(locationIds));
        OpenApiPagelVO<LocationApiInfoVO> voPage = queryLocationByLocation(locationIds,null, dto);
        return voPage;
    }

    @Override
    public LocationApiInfoVO queryLocation(LocationQueryDto dto) {
        Long siteId = dto.getSiteId();
        String siteName = dto.getSiteName();
        LocationApiInfoVO apiInfoVO = new LocationApiInfoVO();
        if(null == siteId &&  StringUtils.isBlank(siteName)){
            log.error("siteId is null and siteName is null,dto is {}",JSON.toJSONString(dto));
            throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
        }

        if(StringUtils.isNotBlank(dto.getSiteName())){
            dto.setSiteName(dto.getSiteName().trim());
        }
        List<Long> locationIds = new ArrayList<>(opLocationService.getLocationIdBySellerId(dto.getSellerId()));
        //List<Long> locationIds = Arrays.asList(1608703060025745617L,199L,3004L);

        if (CollectionUtils.isEmpty(locationIds) || (null != siteId && !locationIds.contains(siteId))){
            return apiInfoVO;
        }

        if(null != siteId){
            locationIds = Arrays.asList(siteId);
        }
        OpenApiPageDto pageDTO = new OpenApiPageDto();
        pageDTO.setPage(1L);
        pageDTO.setPageSize(5L);
        OpenApiPagelVO<LocationApiInfoVO> voPage = queryLocationByLocation(locationIds,  dto.getSiteName(), pageDTO);
        List<LocationApiInfoVO> records = voPage.getContent();
        if(CollectionUtils.isNotEmpty(records)){
             apiInfoVO = records.get(0);
        }
        return apiInfoVO;
    }

    private OpenApiPagelVO<LocationApiInfoVO> queryLocationByLocation(List<Long> locationIds,String locationName, OpenApiPageDto dto){
        //2. 场站基本信息查询
        OpenApiPagelVO<LocationApiInfoVO> locationPage = queryLocationBaseInfo(locationIds, locationName, dto);
        List<LocationApiInfoVO> records = locationPage.getContent();
        if(CollectionUtils.isEmpty(records)){
            return locationPage;
        }
        // 补充桩抢数据
        locationIds = records.stream().map(LocationApiInfoVO::getId).distinct().filter(item -> null != item).collect(Collectors.toList());
        fillLocationPileInfo(records,locationIds);
        // 补全枪信息
        fillLocationEvseInfo(records,locationIds);
        return locationPage;
    }

    private void fillLocationEvseInfo(List<LocationApiInfoVO> list,List<Long> locationIds){
        if(CollectionUtils.isEmpty(list) || CollectionUtils.isEmpty(locationIds)){
            return;
        }
        Map<Long, LocationApiInfoVO> groupByLocationId = list.stream().collect(Collectors.toMap(LocationApiInfoVO::getId, v -> v, (v1, v2) -> v1));

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId",locationIds));
        // 补充 Charging，Occupied，Available 枪数
        // 聚合处理
        TermsAggregationBuilder aggregationBuilder = AggregationBuilders.terms("locationId").field("locationId");
        TermsAggregationBuilder statusCount = AggregationBuilders.terms("state").field("state");

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withAggregations(aggregationBuilder.subAggregation(statusCount))
                .build();
        log.info("fillLocationEvseInfo es query condition   is {}", searchQuery.getQuery().toString());
        log.info("fillLocationEvseInfo es query agg condition   is {}", searchQuery.getAggregations().toString());

        ElasticsearchAggregations locationAggregations = (ElasticsearchAggregations) elasticsearchRestTemplate
                .searchForStream(searchQuery, OpLocationEvseElasticDTO.class).getAggregations();
        log.info("fillLocationEvseInfo es query result   is {}", JSON.toJSONString(locationAggregations));

        Map<String, Aggregation> queryResultAsMap = locationAggregations.aggregations().asMap();

        Terms  locationIdTerms = (Terms ) queryResultAsMap.get("locationId");
        List<? extends Terms.Bucket> locationBuckets = locationIdTerms.getBuckets();
        // 遍历每个桶数据
        for (Terms.Bucket locationIdBucket : locationBuckets) {

            String locationId = locationIdBucket.getKeyAsString();
            long locationDocCount = locationIdBucket.getDocCount();
            log.info("locationId is {}, locationDocCount is {}",locationId,locationDocCount);
            LocationApiInfoVO apiInfoVO = groupByLocationId.get(Long.parseLong(locationId));

            Terms aggregation = (Terms) locationIdBucket.getAggregations().asMap().get("state");
            for (Terms.Bucket statusBucket : aggregation.getBuckets()) {
                String status = statusBucket.getKeyAsString();
                Long statusDocCount = statusBucket.getDocCount();
                int count = statusDocCount.intValue();
                log.info("status is {}, statusDocCount is {}",status,statusDocCount);
                Integer activeChargePointCount = (null == apiInfoVO.getActiveChargePointCount() ? 0:apiInfoVO.getActiveChargePointCount()) ;
                Integer availableChargePointCount = ( null == apiInfoVO.getAvailableChargePointCount() ? 0 : apiInfoVO.getAvailableChargePointCount());

                if( null != apiInfoVO){
                    if( chargingState.contains(status) || occupiedState.contains(status) ){
                        apiInfoVO.setActiveChargePointCount( activeChargePointCount + count );
                    }
                    if( availableState.contains(status)){
                        apiInfoVO.setAvailableChargePointCount( availableChargePointCount + count );
                    }
                }
            }
        }
    }

    private void fillLocationPileInfo(List<LocationApiInfoVO> list,List<Long> locationIds){
        // 获取每个场站 桩数量 枪数量 ac枪数量  dc 枪数量
        if(CollectionUtils.isEmpty(list) || CollectionUtils.isEmpty(locationIds)){
            return;
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId",locationIds));

        log.info("fillLocationPileInfo es query condition   is {}", queryBuilder.toString());

        SearchHits<OpLocationPileEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .build(), OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> pileEvseElastics = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if(CollectionUtils.isEmpty(pileEvseElastics)){
            return;
        }
        // 设备处理
        Map<Long, List<OpLocationPileEvseElasticDTO>> groupByLocationId =
                pileEvseElastics.stream().collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));
        list.stream().forEach(item ->  {
            Long id = item.getId();
            List<OpLocationPileEvseElasticDTO> locationPiles = groupByLocationId.get(id);
            Integer  numAcPort = 0;
            Integer  numDcPort = 0;
            // 计算场站设备的 桩数量 枪数量 ac枪数量  dc 枪数量
            if(CollectionUtils.isNotEmpty(locationPiles)){
                item.setChargerCount(locationPiles.size());
                // 充电枪的数量
                for (int i = 0; i < locationPiles.size(); i++)
                {
                    OpLocationPileEvseElasticDTO locationPile = locationPiles.get(i);
                    String evseList = locationPile.getEvseList();
                    String powerType = locationPile.getPowerType();
                    if(StringUtils.isNotBlank(evseList)){
                        // 移除字符串中的方括号
                        evseList = evseList.replaceAll("\\[|\\]", "");
                        String[] numbers = evseList.split(",");
                        if(StringUtils.isNotBlank(powerType) && powerType.toLowerCase().startsWith("ac_")){
                            numAcPort = numAcPort + numbers.length;
                        }else {
                            numDcPort = numDcPort + numbers.length;
                        }
                    }
                }
            }else{
                item.setChargerCount(0);
            }
            item.setNumAcPort(numAcPort);
            item.setNumDcPort(numDcPort);
            item.setChargePointCount(numAcPort + numDcPort);
            });
    }

    private OpenApiPagelVO<LocationApiInfoVO> queryLocationBaseInfo(List<Long> locationIds,String locationName,OpenApiPageDto dto){
        OpenApiPagelVO<LocationApiInfoVO> resultPage = new OpenApiPagelVO<LocationApiInfoVO>(dto.getPage(), dto.getPageSize(), 0);
        resultPage.setContent(Lists.newArrayList());

        if(CollectionUtils.isEmpty(locationIds)){
            return resultPage;
        }

        BoolQueryBuilder search = QueryBuilders.boolQuery();
        search.must(QueryBuilders.termsQuery("id", locationIds));
        search.must(QueryBuilders.termsQuery("businessType", Arrays.asList(1,2)));
        if(StringUtils.isNotBlank(locationName)){
            //locationName = QueryParserBase.escape(locationName);
            search.must(QueryBuilders.matchQuery("name", locationName));
        }

        int page = dto.getPage().intValue();
        Integer pageSize = dto.getPageSize().intValue();
        String orderBy = "createdAt";
        SortOrder sortOrder = SortOrder.DESC;

        log.info("queryLocationBaseInfo es query condition   is {}", search.toString());

        SearchHits<OpLocationElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(search)
                .withPageable(PageRequest.of(page-1, pageSize))
                .withSorts(SortBuilders.fieldSort(orderBy).order(sortOrder))
                .build(), OpLocationElasticDTO.class);
        // 总记录数
        long totalHits = searchHits.getTotalHits();
        List<OpLocationElasticDTO> opLocationElasticDTOS = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("opLocationElasticDTOS-->{}", JSON.toJSONString(opLocationElasticDTOS));

        if (CollectionUtils.isEmpty(opLocationElasticDTOS)) {
            return resultPage;
        }
        // 数据转换
        List<LocationApiInfoVO> list = opLocationElasticDTOS.stream().map(item -> {
            LocationApiInfoVO apiInfoVO = new LocationApiInfoVO();
            apiInfoVO.setId(item.getId());
            apiInfoVO.setSiteName(item.getName());
            apiInfoVO.setCreatedAt(item.getCreatedAt());
            apiInfoVO.setUpdatedAt(item.getUpdatedAt());
            // 场站地址信息补充
            OcpiGeoLocation coordinates = new OcpiGeoLocation(item.getLatitude(), item.getLongitude());
            StreetApiVO streetApiVO = new StreetApiVO();
            streetApiVO.setCountry(item.getCountry());
            streetApiVO.setProvince(item.getProvince());
            streetApiVO.setCity(item.getCity());
            streetApiVO.setAddress(item.getAddress());
            AddressApiVO location = new AddressApiVO();
            location.setAddress(streetApiVO);
            location.setCoordinates(coordinates);
            apiInfoVO.setLocation(location);
            return apiInfoVO;
        }).collect(Collectors.toList());
        resultPage.setContent(list);
        resultPage.setTotalRows(totalHits);
        resultPage.initTotalPages();
        return resultPage;
    }
}
