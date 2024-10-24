package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.home.feign.HomePileClient;
import com.autel.cloud.pile.base.domain.model.ChargePointDTO;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseOpenService;
import com.autel.cloud.pile.base.dto.OpLocationForAdvParamDTO;
import com.autel.cloud.pile.base.enums.BrandEnum;
import com.autel.cloud.pile.base.enums.CategoryEnum;
import com.autel.cloud.pile.base.enums.ConnectorGunTypeEnum;
import com.autel.cloud.pile.base.feign.CreativePlatformFeignClient;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargePointMerchantRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseBrandModelMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationEvseMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.vo.ChargePointVO;
import com.autel.cloud.pile.base.vo.Connector;
import com.autel.cloud.pile.base.vo.OpLocationPileForAdvVO;
import com.autel.cloud.pile.base.vo.OpLocationPileFromAdvVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.FieldSortBuilder;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.*;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toMap;

/**
 * @Author A22282
 * @Date 2023/12/12 20:35
 */
@Service
@Slf4j
public class OpLocationPileEvseOpenServiceImpl implements OpLocationPileEvseOpenService {

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;
    @Resource
    private CreativePlatformFeignClient platformFeignClient;
    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Resource
    private OpLocationRepository opLocationRepository;
    @Resource
    private OpLocationMapper opLocationMapper;
    @Resource
    private OpLocationOperationRepository opLocationOperationRepository;
    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;
    @Resource
    private OpLocationEvseMapper opLocationEvseMapper;
    @Resource
    private OpLocationConnectorRepository opLocationConnectorRepository;
    @Resource
    private DeviceServiceFeign deviceServiceClient;
    @Resource
    private HomePileClient homePileClient;
    @Resource
    private ChargePointMerchantRelationMapper chargePointMerchantRelationMapper;
    @Resource
    private OpEvseBrandModelMapper opEvseBrandModelMapper;


    @Override
    public Page<OpLocationPileForAdvVO> getAdvList(OpLocationForAdvParamDTO dto) {
        log.info("getAdvList,dto={}", JSON.toJSONString(dto));
        Page<OpLocationPileForAdvVO> resultPage = null;
        Integer page = dto.getPage();
        Integer pageSize = dto.getPageSize();
        Integer businessType = dto.getBusinessType();
        List<Long> sellerIdList = dto.getSellerIdList();
        List<Integer> types = new ArrayList<>();
        if (businessType == null) {
            types.add(2);
            types.add(3);
        } else {
            types.add(businessType);
        }

        BoolQueryBuilder boolQuery = QueryBuilders.boolQuery();
        boolQuery.must(QueryBuilders.termsQuery("businessType", types));
        if (!CollectionUtils.isEmpty(sellerIdList)) {
            boolQuery.must(QueryBuilders.termsQuery("operatorId", sellerIdList));
        }
        List<OpLocationElasticDTO> locationDtoList = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                .withQuery(boolQuery)
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "timeZone", "zoneId"}, null))
                .withPageable(PageRequest.of(0, 500))
                .build(), OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(locationDtoList)) {
            resultPage = new Page<>(page, pageSize);
        } else {
            List<Long> locationIds = locationDtoList.stream().map(OpLocationElasticDTO::getId).collect(Collectors.toList());
            Map<Long, OpLocationElasticDTO> locationMap = locationDtoList.stream().collect(toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
            BoolQueryBuilder query = QueryBuilders.boolQuery();
            query.must(QueryBuilders.termsQuery("locationId", locationIds));
            NativeSearchQuery build = new NativeSearchQueryBuilder()
                    .withQuery(query)
                    .withPageable(PageRequest.of(page - 1, pageSize))
                    .withSorts(new FieldSortBuilder("id").order(SortOrder.ASC))
                    .build();
            SearchHits<OpLocationPileEvseElasticDTO> search = this.elasticsearchRestTemplate.search(build, OpLocationPileEvseElasticDTO.class);
            long total = search.getTotalHits();
            List<OpLocationPileEvseElasticDTO> pileDtoList = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(pileDtoList)) {
                resultPage = new Page<>(page, pageSize);
            } else {
                List<OpLocationPileForAdvVO> voList = new ArrayList<>(pileDtoList.size());

                Set<String> pileSnList = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toSet());

                List<OpLocationEvseElasticDTO> evseDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                        .withQuery(QueryBuilders.termsQuery("pileSn", pileSnList))
                        .build(), OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
                Map<String, OpLocationEvseElasticDTO> evseDtoMap = new HashMap<>();
                if (!CollectionUtils.isEmpty(evseDtoList)) {
                    evseDtoList.stream().forEach(e -> evseDtoMap.put(e.getPileSn(), e));
                }
                List<ChargePileDTO> chargePileList = this.deviceServiceClient.queryPileList(new ArrayList<>(pileSnList)).getData();
                Map<String, ChargePileDTO> chargePileMap = new HashMap<>();
                if (!CollectionUtils.isEmpty(chargePileList)) {
                    chargePileList.stream().forEach(e -> chargePileMap.put(e.getSn(), e));
                }

                List<ChargePointVO> listToUse = this.chargePointMerchantRelationService.findChargePointBySNs(pileSnList, null);
                Map<String, ChargePointVO> pileVoMap = listToUse.stream().collect(toMap(ChargePointVO::getSn, e -> e, (f, s) -> f));

                pileDtoList.stream().forEach(pileDto -> {
                    OpLocationPileForAdvVO pileVo = new OpLocationPileForAdvVO();
                    pileVo.setLocationId(pileDto.getLocationId());
                    pileVo.setLocationName(pileDto.getLocationName());
                    pileVo.setSellerId(pileDto.getOperatorId());
                    pileVo.setPileSn(pileDto.getPileSn());
                    pileVo.setPileName(pileDto.getName());
                    ChargePointVO vo = pileVoMap.get(pileDto.getPileSn());
                    if (vo != null) {
                        pileVo.setOverchargingPileFlag(vo.getOverchargingPileFlag());
                        pileVo.setScreen1Pixel(vo.getScreen1Pixel());
                        pileVo.setScreen1Size(vo.getScreen1Size());
                    }
                    OpLocationElasticDTO locationDto = locationMap.get(pileDto.getLocationId());
                    pileVo.setTimeZone(locationDto.getTimeZone());
                    pileVo.setZoneId(locationDto.getZoneId());
                    String brandName = null;
                    OpLocationEvseElasticDTO evseDto = evseDtoMap.get(pileDto.getPileSn());
                    if (evseDto != null) {
                        pileVo.setPilePin(evseDto.getPinCode());
                    }
                    if (pileVo.getPilePin() == null) {
                        ChargePileDTO chargePile = chargePileMap.get(pileDto.getPileSn());
                        if (chargePile != null) {
                            pileVo.setPilePin(chargePile.getPin());
                        }
                    }
                    Long brandId = pileDto.getBrandId();
                    if (brandId != null) {
                        for (BrandEnum value : BrandEnum.values()) {
                            if (value.getCode().equals(brandId.intValue())) {
                                brandName = value.getName();
                            }
                        }
                    }
                    pileVo.setBrandName(brandName);
                    pileVo.setThirdPart("Autel".equalsIgnoreCase(brandName) ? 0 : 1);
                    voList.add(pileVo);
                });

                resultPage = new Page<>(page, pageSize, total);
                resultPage.setRecords(voList);
                resultPage.setPages((long) Math.ceil((double) total / pageSize));
            }
        }
        return resultPage;
    }

    @Override
    @Transactional
    public List<Long> history(List<Long> sellerIdList) {
        Integer page = 1;
        Integer pageSize = 100;
        Integer add = 0;
        Integer update = 0;
        Long now = System.currentTimeMillis();
        List<Long> updateLocationIds = new ArrayList<>();
        List<Long> resultIds = new ArrayList<>();

        while (true) {
            OpLocationForAdvParamDTO dto = new OpLocationForAdvParamDTO();
            dto.setPage(page);
            dto.setPageSize(pageSize);
            if (!CollectionUtils.isEmpty(sellerIdList)) {
                dto.setSellerIdList(sellerIdList);
            }
            Result<Page<OpLocationPileFromAdvVO>> deviceList = this.platformFeignClient.getDeviceList(dto);
            log.info("history,page={},deviceList={}", page, JSON.toJSONString(deviceList));
            if (deviceList == null || deviceList.getCode() != HttpStatus.OK.value() || deviceList.getData() == null || CollectionUtils.isEmpty(deviceList.getData().getRecords())) {
                break;
            }
            List<OpLocationPileFromAdvVO> records = deviceList.getData().getRecords();
            Map<String, OpLocationPileFromAdvVO> mapToUse = records.stream().collect(toMap(OpLocationPileFromAdvVO::getPileSn, e -> e, (f, s) -> f));
            List<String> pileSnList = records.stream().map(OpLocationPileFromAdvVO::getPileSn).collect(Collectors.toList());

            LambdaQueryWrapper<OpLocationPileEvseEntity> pileWrapper = Wrappers.lambdaQuery();
            pileWrapper.in(OpLocationPileEvseEntity::getPileSn, pileSnList);
            pileWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
            List<OpLocationPileEvseEntity> pileEvseEntityList = this.opLocationPileEvseRepository.list(pileWrapper);
            //已存在
            if (!CollectionUtils.isEmpty(pileEvseEntityList)) {
                pileEvseEntityList.stream().forEach(e -> {
                    String pileSn = e.getPileSn();
                    OpLocationPileFromAdvVO existVo = mapToUse.get(pileSn);
                    if (existVo != null) {
                        records.remove(existVo);
                    }
                });

                List<Long> locationIds = pileEvseEntityList.stream().map(OpLocationPileEvseEntity::getLocationId).distinct().collect(Collectors.toList());
                LambdaQueryWrapper<OpLocationEntity> locationWrapper = Wrappers.lambdaQuery();
                locationWrapper.in(OpLocationEntity::getId, locationIds);
                locationWrapper.eq(OpLocationEntity::getDeleted, 0);
                locationWrapper.eq(OpLocationEntity::getBusinessType, 1);
                List<OpLocationEntity> locationEntityList = this.opLocationRepository.list(locationWrapper);
                if (!CollectionUtils.isEmpty(locationEntityList)) {
                    locationEntityList.stream().forEach(e -> {
                        Long id = e.getId();
                        e.setBusinessType(2);
                        e.setUpdatedAt(now);
                        updateLocationIds.add(id);
                    });
                    this.opLocationRepository.updateBatchById(locationEntityList);
                    update += locationEntityList.size();
                }
            }

            //处理新增
            if (!CollectionUtils.isEmpty(records)) {
                //统一桩处理
                List<OpLocationPileFromAdvVO> tmpRecords = new ArrayList<>(records);
                List<ChargePointMerchantRelationEntity> pointVoList = this.chargePointMerchantRelationMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                        .in(ChargePointMerchantRelationEntity::getSn, records.stream().map(OpLocationPileFromAdvVO::getPileSn).collect(Collectors.toSet())));
                if (!CollectionUtils.isEmpty(pointVoList)) {
                    List<String> tmpList = pointVoList.stream().map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
                    records.stream().forEach(e -> {
                        String pileSn = e.getPileSn();
                        if (tmpList.contains(pileSn)) {
                            tmpRecords.remove(e);
                        }
                    });
                }
                if (!CollectionUtils.isEmpty(tmpRecords)) {
                    log.info("history,统一桩录入，tmpRecords={}", JSON.toJSONString(tmpRecords));
                    //插入统一桩
                    tmpRecords.stream().forEach(vo -> {
                        ChargePointDTO param = new ChargePointDTO();
                        com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileInfo = this.deviceServiceClient.pileDetail(vo.getPileSn()).getData();
                        if (chargePileInfo == null) {
                            records.remove(vo);
                            log.info("history,统一桩录入，device不存在={}", JSON.toJSONString(vo));
                            return;
                        }
                        param.setBrandId(Long.valueOf(Optional.ofNullable(chargePileInfo.getBrandId()).orElse("1")));
                        param.setBrandName(Optional.ofNullable(chargePileInfo.getBrandName()).orElse("Autel"));
                        param.setProductNamePdm(chargePileInfo.getProductNamePdm());
                        param.setZoneId(Optional.ofNullable(vo.getZoneId()).orElse("Asia/Shanghai"));
                        param.setName(Optional.ofNullable(vo.getPileName()).orElse(vo.getPileSn()));
                        param.setSn(chargePileInfo.getSn());
                        param.setPartProduct(chargePileInfo.getPartProduct());
                        param.setPin(chargePileInfo.getPin());


                        Integer connectorNum = chargePileInfo.getConnectorNum();
                        Integer connectorType = chargePileInfo.getConnectorType();

                        int max = Optional.ofNullable(connectorNum).orElse(1);
                        int connType = Optional.ofNullable(connectorType).orElse(1);

                        List<Connector> connectors = new ArrayList<>();
                        for (int i = 1; i <= max; i++) {
                            Connector connector = new Connector();
                            connector.setConnectorId(i);
                            connector.setConnectorType(connType);
                            connector.setConnectorName(ConnectorGunTypeEnum.getEnumByCode(connType).getName());
                            connectors.add(connector);
                        }
                        param.setPowerType(Objects.nonNull(chargePileInfo.getCategory()) && chargePileInfo.getCategory() == 2 ? "DC" : "AC");
                        param.setConnectors(connectors);
                        if (Objects.nonNull(chargePileInfo.getOutputPower())) {
                            param.setRatedPower(BigDecimal.valueOf(chargePileInfo.getOutputPower()));
                        } else {
                            param.setRatedPower(BigDecimal.valueOf(22L));
                        }
                        String tmp = "AC_1_PHASE";
                        param.setPhases(tmp);
                        if (chargePileInfo.getCategory() != null) {
                            String type = CategoryEnum.getEnumByCode(chargePileInfo.getCategory()).getDesc();
                            int phase = chargePileInfo.getPhase() == null ? 1 : chargePileInfo.getPhase();
                            param.setPhases(type + "_" + phase + "_" + ChargePointMerchantRelationService.phaseConstant);
                        }
                        this.chargePointMerchantRelationService.save(param, vo.getSellerId());
                    });
                }
                if (!CollectionUtils.isEmpty(records)) {
                    List<ChargePointMerchantRelationEntity> pointVoListTmp = this.chargePointMerchantRelationMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                            .in(ChargePointMerchantRelationEntity::getSn, records.stream().map(OpLocationPileFromAdvVO::getPileSn).collect(Collectors.toSet())));
                    Map<String, ChargePointMerchantRelationEntity> pointVoMapTmp = pointVoListTmp.stream().collect(toMap(ChargePointMerchantRelationEntity::getSn, e -> e, (f, s) -> s));
                    //品牌查询
                    List<Long> brandIds = pointVoListTmp.stream().map(ChargePointMerchantRelationEntity::getBrandId).distinct().collect(Collectors.toList());
                    LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
                    queryWrapper.in(OpEvseBrandModelEntity::getBrandId, brandIds);
                    queryWrapper.eq(OpEvseBrandModelEntity::getDeleted, 0);
                    Map<Long, List<OpEvseBrandModelEntity>> brandMap = this.opEvseBrandModelMapper.selectList(queryWrapper).stream().collect(Collectors.groupingBy(OpEvseBrandModelEntity::getBrandId));

                    List<OpLocationPileFromAdvVO> tmpToUse = new ArrayList<>(records);
                    List<String> bindSns = new ArrayList<>();
                    //桩绑定用户，则为家桩，否则为商桩
                    Result<Set<String>> restResult = this.homePileClient.batchQueryBind(tmpToUse.stream().map(OpLocationPileFromAdvVO::getPileSn).collect(Collectors.toSet()));
                    if (restResult != null && !CollectionUtils.isEmpty(restResult.getData())) {
                        log.info("history,桩已被绑定={}", JSON.toJSONString(restResult.getData()));
                        bindSns.addAll(restResult.getData());
                    }
                    //创建场站并添加桩到场站
                    //过滤已绑定
                    List<OpLocationPileFromAdvVO> pileVoList = records.stream().filter(e -> {
                        if (bindSns.isEmpty()) {
                            return true;
                        }
                        if (!bindSns.contains(e.getPileSn())) {
                            return true;
                        }
                        return false;
                    }).collect(Collectors.toList());

                    if (!CollectionUtils.isEmpty(pileVoList)) {
                        Map<Long, List<OpLocationPileFromAdvVO>> groupBySellerId = pileVoList.stream().collect(Collectors.groupingBy(OpLocationPileFromAdvVO::getSellerId));
                        log.info("history,查询桩基础信息={}", JSON.toJSONString(pileVoList.stream().map(OpLocationPileFromAdvVO::getPileSn).collect(Collectors.toList())));
                        //查询基础数据
                        List<ChargePileDTO> chargePileDtoList = this.deviceServiceClient.queryPileList(pileVoList.stream().map(OpLocationPileFromAdvVO::getPileSn).collect(Collectors.toList())).getData();
                        Map<String, ChargePileDTO> chargePileMap = chargePileDtoList.stream().collect(toMap(ChargePileDTO::getSn, e -> e, (f, s) -> f));
                        //按商家分组
                        groupBySellerId.forEach((sellerId, v) -> {
                            //按场站名称分组
                            Map<String, List<OpLocationPileFromAdvVO>> groupByLocationName = v.stream().collect(Collectors.groupingBy(OpLocationPileFromAdvVO::getLocationName));
                            //校验场站名称是否存在
                            List<String> names = groupByLocationName.keySet().stream().collect(Collectors.toList());
                            LambdaQueryWrapper<OpLocationEntity> qw = Wrappers.lambdaQuery();
                            qw.eq(OpLocationEntity::getOperatorId, sellerId);
                            qw.in(OpLocationEntity::getName, names);
                            qw.eq(OpLocationEntity::getDeleted, 0);
                            List<OpLocationEntity> existList = this.opLocationRepository.list(qw);
                            Map<String, OpLocationEntity> existMap = new HashMap<>();
                            if (!CollectionUtils.isEmpty(existList)) {
                                existList.stream().forEach(e -> existMap.put(e.getName(), e));
                            }
                            groupByLocationName.forEach((locationName, v2) -> {
                                //创建场站
                                OpLocationPileFromAdvVO tmpVo = v2.get(0);
                                OpLocationEntity locationEntity = new OpLocationEntity();
                                locationEntity.setUpdatedAt(now);
                                locationEntity.setCreatedAt(now);
                                locationEntity.setBusinessType(3);
                                locationEntity.setName(locationName);
                                locationEntity.setOperatorId(sellerId);
                                locationEntity.setAddress(tmpVo.getAddress());
                                locationEntity.setCity(tmpVo.getCity());
                                locationEntity.setProvince(tmpVo.getProvince());
                                locationEntity.setCountry(tmpVo.getCountry());
                                locationEntity.setLongitude(tmpVo.getLongitude());
                                locationEntity.setLatitude(tmpVo.getLatitude());
                                locationEntity.setTimeZone(tmpVo.getTimeZone());
                                locationEntity.setZoneId(tmpVo.getZoneId());
                                OpLocationEntity existEntity = existMap.get(locationName);
                                if (existEntity != null) {
                                    locationEntity = existEntity;
                                } else {
                                    this.opLocationRepository.save(locationEntity);
                                }
                                Long locationId = locationEntity.getId();
                                OpLocationOperationEntity operationEntity = new OpLocationOperationEntity();
                                operationEntity.setLocationId(locationId);
                                operationEntity.setOpenType(1);
                                operationEntity.setAppShow(false);
                                if (existEntity == null) {
                                    this.opLocationOperationRepository.save(operationEntity);
                                }
                                resultIds.add(locationId);

                                v2.stream().forEach(vo -> {
                                    //添加桩
                                    String pileSn = vo.getPileSn();
                                    ChargePileDTO chargePile = chargePileMap.get(pileSn);
                                    if (chargePile == null) {
                                        log.info("history,chargePile is null,vo={}", JSON.toJSONString(vo));
                                        return;
                                    }
                                    ChargePointMerchantRelationEntity chargePoint = pointVoMapTmp.get(pileSn);
                                    List<Long> evseIds = new ArrayList<>();
                                    OpLocationPileEvseEntity pileEvseEntity = new OpLocationPileEvseEntity();
                                    pileEvseEntity.setLocationId(locationId);
                                    pileEvseEntity.setPileSn(pileSn);

                                    List<Connector> listToUse = JSON.parseArray(chargePoint.getConnectors().toString(), Connector.class);
                                    listToUse.stream().forEach(conn -> {
                                        OpLocationEvseEntity evseEntity = new OpLocationEvseEntity();
                                        evseEntity.setDeleted(0);
                                        evseEntity.setStatus(0);
                                        evseEntity.setCreatedAt(now);
                                        evseEntity.setUpdatedAt(now);
                                        evseEntity.setLocationId(locationId);
                                        evseEntity.setState(EvseDeviceStatusEnum.DEFAULT.getName());
                                        evseEntity.setEvseId("");
                                        evseEntity.setLatitude("");
                                        evseEntity.setLongitude("");
                                        evseEntity.setEvseSn(pileSn + "_" + conn.getConnectorId());
                                        evseEntity.setPinCode(chargePile.getPin());
                                        this.opLocationEvseRepository.save(evseEntity);
                                        Long evseId = evseEntity.getId();
                                        evseIds.add(evseId);
                                        OpLocationConnectorEntity connectorEntity = new OpLocationConnectorEntity();
                                        connectorEntity.setLocationEvseId(evseId);
                                        Long brandId = chargePoint.getBrandId();
                                        BigDecimal voltage = BigDecimal.ZERO;
                                        BigDecimal amperage = BigDecimal.ZERO;
                                        BigDecimal power = BigDecimal.ZERO;
                                        String powerType = "AC_1_PHASE";
                                        //道通桩
                                        if (brandId.longValue() == 1) {
                                            amperage = BigDecimal.valueOf(Optional.ofNullable(chargePile.getElectricCurrent()).orElse(0D));
                                            voltage = BigDecimal.valueOf(Optional.ofNullable(chargePile.getVoltage()).orElse(0D));
                                            power = BigDecimal.valueOf(Optional.ofNullable(chargePile.getOutputPower()).orElse(0D));
                                            Integer category = Optional.ofNullable(chargePile.getCategory()).orElse(1);
                                            Integer phase = Optional.ofNullable(chargePile.getPhase()).orElse(1);
                                            if (category == 1) {
                                                powerType = "AC_" + phase + "_PHASE";
                                            } else {
                                                powerType = "DC_" + phase + "_PHASE";
                                            }
                                        } else {
                                            //非道通桩
                                            List<OpEvseBrandModelEntity> tmpList = brandMap.get(brandId);
                                            if (!CollectionUtils.isEmpty(tmpList)) {
                                                OpEvseBrandModelEntity be = tmpList.stream().filter(e -> e.getProductModel().equals(chargePoint.getPartProduct())).findAny().orElse(null);
                                                if (be != null) {
                                                    amperage = be.getAmperage();
                                                    voltage = be.getVoltage();
                                                    power = BigDecimal.valueOf(Optional.ofNullable(be.getPower()).orElse(0D));
                                                    powerType = be.getPowerType();
                                                }
                                            }
                                        }
                                        connectorEntity.setPowerType(powerType);
                                        connectorEntity.setAmperage(amperage);
                                        connectorEntity.setVoltage(voltage);
                                        connectorEntity.setStatus(1);
                                        connectorEntity.setPower(power.doubleValue());
                                        connectorEntity.setConnectorId(conn.getConnectorId().toString());
                                        connectorEntity.setGunType(conn.getConnectorType());
                                        this.opLocationConnectorRepository.save(connectorEntity);
                                    });

                                    pileEvseEntity.setEvseList(JSON.toJSONString(evseIds));
                                    pileEvseEntity.setName(vo.getPileName());
                                    pileEvseEntity.setBrandId(Long.valueOf(chargePile.getBrandId()));
                                    pileEvseEntity.setProductModel(chargePile.getProductModel());
                                    pileEvseEntity.setPublicMark(1);
                                    pileEvseEntity.setEroamingEnable(0);
                                    this.opLocationPileEvseRepository.save(pileEvseEntity);
                                });
                            });
                        });
                        add += records.size();
                    }
                }
            }
            page++;
            dto.setPage(page);
            dto.setPageSize(pageSize);
            if (!CollectionUtils.isEmpty(sellerIdList)) {
                dto.setSellerIdList(sellerIdList);
            }
        }

        log.info("history,add={},update={}", add, update);
        return resultIds;
    }

    @Override
    @Transactional
    public Integer deletes() {

        LambdaQueryWrapper<OpLocationEntity> locationQuery = Wrappers.lambdaQuery();
        locationQuery.eq(OpLocationEntity::getDeleted, 0);
        locationQuery.last("GROUP BY operator_id,name HAVING(COUNT(name) >1 )ORDER BY id desc");
        List<OpLocationEntity> locationEntityList = this.opLocationRepository.list(locationQuery);
        if (CollectionUtils.isEmpty(locationEntityList)) {
            return 0;
        }
        List<OpLocationEntity> allEntityList = this.opLocationRepository.list(new LambdaUpdateWrapper<OpLocationEntity>()
                .in(OpLocationEntity::getName, locationEntityList.stream().map(OpLocationEntity::getName).collect(Collectors.toList()))
                .eq(OpLocationEntity::getDeleted, 0));
        Map<Long, List<OpLocationEntity>> collect = allEntityList.stream().collect(Collectors.groupingBy(OpLocationEntity::getOperatorId));
        List<Long> count = new ArrayList<>();
        collect.forEach((sellerId, list) -> {
            List<OpLocationEntity> listToUse = list.stream().filter(e -> e.getBusinessType() != null && e.getBusinessType() == 3).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(listToUse) || list.size() < 2) {
                return;
            }
            List<OpLocationEntity> tmpList = new ArrayList<>(list);
            OpLocationEntity existEntity = list.stream().sorted(Comparator.comparing(OpLocationEntity::getId)).collect(Collectors.toList()).get(0);
            Long locationId = existEntity.getId();
            tmpList.remove(existEntity);
            if (!tmpList.isEmpty()) {
                List<Long> deleteIds = tmpList.stream().map(OpLocationEntity::getId).collect(Collectors.toList());
                this.opLocationRepository.removeByIds(deleteIds);
                count.addAll(deleteIds);
                this.opLocationOperationRepository.remove(new LambdaUpdateWrapper<OpLocationOperationEntity>().in(OpLocationOperationEntity::getLocationId, deleteIds).eq(OpLocationOperationEntity::getDeleted, 0));
                this.elasticsearchRestTemplate.delete(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("id", deleteIds)).build(), OpLocationElasticDTO.class);

                List<OpLocationPileEvseEntity> pileEntityList = this.opLocationPileEvseRepository.list(new LambdaUpdateWrapper<OpLocationPileEvseEntity>().in(OpLocationPileEvseEntity::getLocationId, deleteIds).eq(OpLocationPileEvseEntity::getDeleted, 0));
                if (CollectionUtils.isEmpty(pileEntityList)) {
                    return;
                }
                List<Long> evseIds = new ArrayList<>();
                pileEntityList.stream().forEach(pileEntity -> {
                    pileEntity.setLocationId(locationId);
                    evseIds.addAll(JSON.parseArray(pileEntity.getEvseList(), Long.class));
                });
                this.opLocationPileEvseRepository.updateBatchById(pileEntityList);
                List<OpLocationPileEvseElasticDTO> pileDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("id", pileEntityList.stream().map(OpLocationPileEvseEntity::getId).collect(Collectors.toList()))).build(), OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
                if (!CollectionUtils.isEmpty(pileDtoList)) {
                    List<IndexQuery> updateList = new ArrayList<>();
                    pileDtoList.stream().forEach(e -> {
                        e.setLocationId(locationId);
                        updateList.add(new IndexQueryBuilder().withId(e.getId().toString()).withObject(e).build());
                    });
                    this.elasticsearchRestTemplate.bulkIndex(updateList, OpLocationPileEvseElasticDTO.class);
                }
                if (evseIds.isEmpty()) {
                    return;
                }
                List<OpLocationEvseEntity> evseEntityList = this.opLocationEvseRepository.listByIds(evseIds);
                if (!CollectionUtils.isEmpty(evseEntityList)) {
                    List<IndexQuery> updateList = new ArrayList<>();
                    evseEntityList.stream().forEach(evseEntity -> {
                        evseEntity.setLocationId(locationId);
                    });
                    this.opLocationEvseRepository.updateBatchById(evseEntityList);
                    List<OpLocationEvseElasticDTO> evseDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("id", evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toList()))).build(), OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(evseDtoList)) {
                        evseDtoList.stream().forEach(e -> {
                            e.setLocationId(locationId);
                            updateList.add(new IndexQueryBuilder().withId(e.getId().toString()).withObject(e).build());
                        });
                        this.elasticsearchRestTemplate.bulkIndex(updateList, OpLocationEvseElasticDTO.class);
                    }
                    List<OpLocationEvseExpandElasticDTO> expandList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("id", evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toList()))).build(), OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
                    List<IndexQuery> listUse = new ArrayList<>();
                    if (!CollectionUtils.isEmpty(expandList)) {
                        expandList.stream().forEach(e -> {
                            e.setLocationId(locationId);
                            listUse.add(new IndexQueryBuilder().withId(e.getId().toString()).withObject(e).build());
                        });
                        this.elasticsearchRestTemplate.bulkIndex(listUse, OpLocationEvseExpandElasticDTO.class);
                    }
                }
            }
        });

        return count.size();
    }
}
