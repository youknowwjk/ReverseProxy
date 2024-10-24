package com.autel.cloud.pile.base.domain.service.impl;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateUnit;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.date.LocalDateTimeUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.OpLocationEvseConvert;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.EvseStatisticService;
import com.autel.cloud.pile.base.dto.EvseStatisticDto;
import com.autel.cloud.pile.base.dto.OpLocationEvseRealTimeDTO;
import com.autel.cloud.pile.base.dto.statistics.*;
import com.autel.cloud.pile.base.enums.ConnectorPowerTypeEnum;
import com.autel.cloud.pile.base.enums.LocationEvseStatusV2Enum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.PileWorkStatusHistoryDTO;
import com.autel.cloud.pile.base.infrastructure.feign.MonitorFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.OpImageMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationConnectorMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.vo.EvsePowerStatisticVO;
import com.autel.cloud.pile.base.vo.EvseRealtimeDataVO;
import com.autel.cloud.pile.base.vo.EvseStatusStatisticVO;
import com.autel.cloud.pile.base.vo.OpLocationEvseVO;
import com.autel.cloud.pile.bill.constant.RedisKeys;
import com.autel.cloud.pile.bill.vo.ChargingBillCacheVO;
import com.autel.cloud.tariff.dto.ChargingDTO;
import com.autel.cloud.tariff.dto.TariffComputeDTO;
import com.autel.cloud.tariff.feign.BillCostFeignClient;
import com.autel.cloud.ws.feign.WsCoreClient;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.apache.lucene.search.TotalHits;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Service
@Log4j2
public class EvseStatisticServiceImpl implements EvseStatisticService {

    @Resource
    private MonitorFeignClient monitorFeignClient;

    @Autowired
    private BillCostFeignClient billCostFeignClient;

    @Autowired
    private RedisUtil redisUtil;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    private final OpLocationEvseRepository opLocationEvseRepository;

    private final OpLocationRepository opLocationRepository;

    private final MonitorFeign monitorFeign;

    private final OpLocationEvseElastic opLocationEvseElastic;

    private final OpImageMapper opImageMapper;

    private final OpLocationConnectorMapper opLocationConnectorMapper;

    @Autowired
    private RestHighLevelClient restHighLevelClient;

    @Resource
    private WsCoreClient wsCoreClient;

    public EvseStatisticServiceImpl(OpLocationEvseRepository opLocationEvseRepository,
                                    OpLocationRepository opLocationRepository,
                                    MonitorFeign monitorFeign,
                                    OpLocationEvseElastic opLocationEvseElastic,
                                    OpImageMapper opImageMapper,
                                    OpLocationConnectorMapper opLocationConnectorMapper) {
        this.opLocationEvseRepository = opLocationEvseRepository;
        this.opLocationRepository = opLocationRepository;
        this.monitorFeign = monitorFeign;
        this.opLocationEvseElastic = opLocationEvseElastic;
        this.opImageMapper = opImageMapper;
        this.opLocationConnectorMapper = opLocationConnectorMapper;
    }

    @Override
    public Result<EvseStatusStatisticVO> statisticEvseStatus(EvseStatisticDto evseStatisticDto) {
        EvseStatusStatisticVO evseStatusStatisticVO = new EvseStatusStatisticVO();
        LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                .eq(OpLocationEvseEntity::getLocationId, evseStatisticDto.getLocationUid())
                .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE);
        List<OpLocationEvseEntity> evseEntityList = opLocationEvseRepository.list(queryWrapper);
        if (CollectionUtils.isEmpty(evseEntityList)) {
            return Result.ofSucceed(evseStatusStatisticVO);
        }
        Set<Long> evseUidSet = evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toSet());
        // 充电枪类型
        LambdaQueryWrapper<OpLocationConnectorEntity> opLocationConnectorEntityWrapper = new LambdaQueryWrapper<>();
        opLocationConnectorEntityWrapper.in(OpLocationConnectorEntity::getLocationEvseId, evseUidSet);
        List<OpLocationConnectorEntity> opLocationConnectorEntityList = opLocationConnectorMapper.selectList(opLocationConnectorEntityWrapper);
        AtomicInteger dcEvseCount = new AtomicInteger(0);
        AtomicInteger acEvseCount = new AtomicInteger(0);
        AtomicInteger chargingEvseCount = new AtomicInteger(0);
        AtomicInteger availableEvseCount = new AtomicInteger(0);
        AtomicInteger unavailableEvseCount = new AtomicInteger(0);
        AtomicInteger retentionEvseCount = new AtomicInteger(0);

        AtomicInteger charging = new AtomicInteger(0);
        AtomicInteger reservation = new AtomicInteger(0);
        AtomicInteger preCharge = new AtomicInteger(0);
        AtomicInteger idle = new AtomicInteger(0);
        AtomicInteger offline = new AtomicInteger(0);
        AtomicInteger prohibited = new AtomicInteger(0);
        AtomicInteger problematic = new AtomicInteger(0);
        AtomicInteger finished = new AtomicInteger(0);

        // 充电枪状态统计
        evseEntityList.forEach(entity -> {
            OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorEntityList
                    .stream()
                    .filter(opLocationConnector -> opLocationConnector.getLocationEvseId().equals(entity.getId()))
                    .findAny()
                    .orElse(null);
            String state = monitorFeign.getEvseStatus(entity.getId()).getName();
            // 1: AC_1_PHASE, 2: AC_3_PHASE, 3: DC
            if (null != opLocationConnectorEntity
                    && (ConnectorPowerTypeEnum.AC_1_PHASE.getCode().equals(opLocationConnectorEntity.getPowerType()) || ConnectorPowerTypeEnum.AC_3_PHASE.getCode().equals(opLocationConnectorEntity.getPowerType()))) {
                acEvseCount.incrementAndGet();
            } else {
                dcEvseCount.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.AVAILABLE.getName().equals(state)) {
                availableEvseCount.incrementAndGet();
                idle.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.PREPARING.getName().equals(state)) {
                availableEvseCount.incrementAndGet();
                preCharge.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.FAULTED.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                problematic.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
                chargingEvseCount.incrementAndGet();
                charging.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.DEFAULT.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                offline.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.UNAVAILABLE.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                prohibited.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.FINISHING.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                finished.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.SUSPENDED_EVSE.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                prohibited.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.SUSPENDED_EV.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                prohibited.incrementAndGet();
            }
            if (EvseDeviceStatusEnum.RESERVED.getName().equals(state)) {
                unavailableEvseCount.incrementAndGet();
                reservation.incrementAndGet();
            }
        });
        EvseStatusStatisticVO.ChargingEvse chargingEvse = new EvseStatusStatisticVO.ChargingEvse();
        chargingEvse.setCharging(charging.get());
        chargingEvse.setPreCharge(preCharge.get());
        chargingEvse.setReservation(reservation.get());
        EvseStatusStatisticVO.AvailableEvse availableEvse = new EvseStatusStatisticVO.AvailableEvse();
        availableEvse.setIdle(idle.get());
        EvseStatusStatisticVO.RetentionEvse retentionEvse = new EvseStatusStatisticVO.RetentionEvse();
        retentionEvse.setFinished(finished.get());
        EvseStatusStatisticVO.UnavailableEvse unavailableEvse = new EvseStatusStatisticVO.UnavailableEvse();
        unavailableEvse.setOffline(offline.get());
        unavailableEvse.setProblematic(problematic.get());
        unavailableEvse.setProhibited(prohibited.get());
        evseStatusStatisticVO.setAvailableEvse(availableEvse);
        evseStatusStatisticVO.setChargingEvse(chargingEvse);
        evseStatusStatisticVO.setEvseCount(evseEntityList.size());
        evseStatusStatisticVO.setAvailableEvseCount(availableEvseCount.get());
        evseStatusStatisticVO.setChargingEvseCount(chargingEvseCount.get());
        evseStatusStatisticVO.setRetentionEvseCount(retentionEvseCount.get());
        evseStatusStatisticVO.setRetentionEvse(retentionEvse);
        evseStatusStatisticVO.setUnavailableEvse(unavailableEvse);
        evseStatusStatisticVO.setUnavailableEvseCount(unavailableEvseCount.get());
        evseStatusStatisticVO.setAcEvseCount(acEvseCount.get());
        evseStatusStatisticVO.setDcEvseCount(dcEvseCount.get());
        return Result.ofSucceed(evseStatusStatisticVO);
    }

    @Override
    public Result<List<EvseRealtimeDataVO>> getEvseRealtimeData(EvseStatisticDto evseStatisticDto) {
        List<EvseRealtimeDataVO> evseRealtimeDataVOList = new ArrayList<>();
        List<OpLocationEvseVO> opLocationEvseVOList = new ArrayList<>();
        Set<Long> evseIdSet = new HashSet<>();
        // 先从es查询
        QueryBuilder queryBuilder = QueryBuilders.boolQuery()
                .must(QueryBuilders.termQuery("locationId", evseStatisticDto.getLocationUid()))
                .must(QueryBuilders.termQuery("deleted", Boolean.FALSE));
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : iterable) {
            OpLocationEvseVO opLocationEvseVO = new OpLocationEvseVO();
            BeanUtils.copyProperties(opLocationEvseElasticDTO, opLocationEvseVO);
            opLocationEvseVOList.add(opLocationEvseVO);
            evseIdSet.add(opLocationEvseElasticDTO.getId());
        }
        if (CollectionUtils.isEmpty(evseIdSet)) {
            // es查询不到，从mysql查询
            LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                    .eq(OpLocationEvseEntity::getLocationId, evseStatisticDto.getLocationUid())
                    .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE);
            List<OpLocationEvseEntity> evseEntityList = opLocationEvseRepository.list(queryWrapper);
            evseIdSet = evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toSet());
            if (CollectionUtils.isEmpty(evseIdSet)) {
                return Result.ofSucceed(evseRealtimeDataVOList);
            }
            evseEntityList.forEach(evseEntity -> {
                OpLocationEvseVO opLocationEvseVO = new OpLocationEvseVO();
                BeanUtils.copyProperties(evseEntity, opLocationEvseVO);
                opLocationEvseVOList.add(opLocationEvseVO);
            });
        }
        // 查询场站信息
        OpLocationEntity opLocationEntity = opLocationRepository.getById(evseStatisticDto.getLocationUid());

        // 充电枪类型
        LambdaQueryWrapper<OpLocationConnectorEntity> opLocationConnectorEntityWrapper = new LambdaQueryWrapper<>();
        opLocationConnectorEntityWrapper.in(OpLocationConnectorEntity::getLocationEvseId, evseIdSet);
        List<OpLocationConnectorEntity> opLocationConnectorEntityList = opLocationConnectorMapper.selectList(opLocationConnectorEntityWrapper);

        opLocationEvseVOList.forEach(opLocationEvseVO -> {
            OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = new OpLocationEvseRealTimeDTO();
            Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(opLocationEvseVO.getId());
            OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);
            // 按充电量过滤
            if (evseStatisticDto.getSocLower() != null
                    && null != opLocationEvseRealTimeDTO
                    && opLocationEvseRealTimeDTO.getBatterySoc() != null
                    && opLocationEvseRealTimeDTO.getBatterySoc() < evseStatisticDto.getSocLower()) {
                return;
            }
            if (evseStatisticDto.getSocUpper() != null
                    && null != opLocationEvseRealTimeDTO
                    && opLocationEvseRealTimeDTO.getBatterySoc() != null
                    && opLocationEvseRealTimeDTO.getBatterySoc() > evseStatisticDto.getSocUpper()) {
                return;
            }
            // 充电枪类型
            OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorEntityList
                    .stream()
                    .filter(opLocationConnector -> opLocationConnector.getLocationEvseId().equals(opLocationEvseVO.getId()))
                    .findAny()
                    .orElse(null);
            String state = monitorFeign.getEvseStatus(opLocationEvseVO.getId()).getName();
            EvseRealtimeDataVO evseRealtimeDataVO = new EvseRealtimeDataVO();
            evseRealtimeDataVO.setState(state);
            evseRealtimeDataVO.setEvseNo(opLocationEvseVO.getEvseId());
            evseRealtimeDataVO.setEvseName(opLocationEvseVO.getEvseId());
            evseRealtimeDataVO.setLocationName(opLocationEntity.getName());
            evseRealtimeDataVO.setLocationUid(opLocationEntity.getId());
            evseRealtimeDataVO.setUid(opLocationEvseVO.getId());
            evseRealtimeDataVO.setAddress(opLocationEntity.getAddress());
            evseRealtimeDataVO.setLatitude(opLocationEvseVO.getLatitude());
            evseRealtimeDataVO.setLongitude(opLocationEvseVO.getLongitude());
            evseRealtimeDataVO.setPileSn(opLocationEvseVO.getEvseId().split("_")[0]);
            if (null != opLocationConnectorEntity) {
                evseRealtimeDataVO.setPileType(opLocationConnectorEntity.getPowerType());
            }
            evseRealtimeDataVO.setMerchantId(opLocationEntity.getOperatorId());
            // 充电中的充电桩
            if (EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
                evseRealtimeDataVO.setCurrent(opLocationEvseRealTimeDTO.getCurrent());
                evseRealtimeDataVO.setVoltage(opLocationEvseRealTimeDTO.getVoltage());
                evseRealtimeDataVO.setPower(opLocationEvseRealTimeDTO.getPower());
                evseRealtimeDataVO.setBusId(opLocationEvseRealTimeDTO.getBusId());
                evseRealtimeDataVO.setBatterySoc(opLocationEvseRealTimeDTO.getBatterySoc());
                evseRealtimeDataVO.setTotalElectricalPower(opLocationEvseRealTimeDTO.getTotalElectricalPower());
                evseRealtimeDataVO.setRdTimeLeft(opLocationEvseRealTimeDTO.getRdTimeLeft());
                evseRealtimeDataVO.setCreateTime(opLocationEvseRealTimeDTO.getCreateTime());
                Object chargingBillObject = redisUtil.get(RedisKeys.getBillChargingInfo(opLocationEvseRealTimeDTO.getBusId()));
                if (null != chargingBillObject) {
                    ChargingBillCacheVO chargingBillCacheVO = (ChargingBillCacheVO) chargingBillObject;
                    long reportTimestamp = DateUtil.parse(evseRealtimeDataVO.getCreateTime(), DatePattern.NORM_DATETIME_PATTERN).getTime();
                    long duration = this.calculateDurationByTimezone(opLocationEntity.getTimeZone(), chargingBillCacheVO.getStartTime(), reportTimestamp);
                    evseRealtimeDataVO.setMemberId(chargingBillCacheVO.getMemberId());
                    evseRealtimeDataVO.setChargeDuration((int) duration / 1000 / 60);
                }
            }
            evseRealtimeDataVOList.add(evseRealtimeDataVO);
        });
        return Result.ofSucceed(evseRealtimeDataVOList);
    }

    @Override
    public Result<EvseRealtimeDataVO> getEvseRealtimeDataDetail(Long locationUid, Long evseUid) {
        // 查询场站信息
        OpLocationEntity opLocationEntity = opLocationRepository.getById(locationUid);

        // 充电枪
        OpLocationEvseEntity evseEntity = opLocationEvseRepository.getById(evseUid);

        // 充电枪类型
        LambdaQueryWrapper<OpLocationConnectorEntity> opLocationConnectorEntityWrapper = new LambdaQueryWrapper<>();
        opLocationConnectorEntityWrapper.eq(OpLocationConnectorEntity::getLocationEvseId, evseUid)
                .eq(OpLocationConnectorEntity::getDeleted, 0);
        OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorMapper.selectOne(opLocationConnectorEntityWrapper);

        // 充电枪图片
        List<Map> opEvseImageEntityList = opImageMapper.selectEvseImgs(Arrays.asList(evseUid));
        List<String> imgUrls = new ArrayList<>();
        opEvseImageEntityList.forEach(opEvseImageEntity -> {
            if (null != opEvseImageEntity.get("url") && !StringUtils.isBlank(String.valueOf(opEvseImageEntity.get("url")))) {
                imgUrls.add(String.valueOf(opEvseImageEntity.get("url")));
            }
        });

        String state = monitorFeign.getEvseStatus(evseEntity.getId()).getName();
        EvseRealtimeDataVO evseRealtimeDataVO = new EvseRealtimeDataVO();
        evseRealtimeDataVO.setState(state);
        evseRealtimeDataVO.setEvseNo(evseEntity.getEvseId());
        evseRealtimeDataVO.setEvseName(evseEntity.getEvseId());
        evseRealtimeDataVO.setLocationName(opLocationEntity.getName());
        evseRealtimeDataVO.setLocationUid(opLocationEntity.getId());
        evseRealtimeDataVO.setUid(evseEntity.getId());
        evseRealtimeDataVO.setAddress(opLocationEntity.getAddress());
        evseRealtimeDataVO.setLatitude(evseEntity.getLatitude());
        evseRealtimeDataVO.setLongitude(evseEntity.getLongitude());
        evseRealtimeDataVO.setPileSn(evseEntity.getEvseId().split("_")[0]);
        if (CollUtil.isNotEmpty(imgUrls)) {
            evseRealtimeDataVO.setImageUrl(imgUrls);
        }
        if (null != opLocationConnectorEntity) {
            evseRealtimeDataVO.setPileType(opLocationConnectorEntity.getPowerType());
        }
        evseRealtimeDataVO.setMerchantId(opLocationEntity.getOperatorId());
        // 充电中的充电桩
        if (EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
            // 从缓存中获取充电枪实时数据
            OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = new OpLocationEvseRealTimeDTO();
            Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(evseUid);
            OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);

            evseRealtimeDataVO.setCurrent(opLocationEvseRealTimeDTO.getCurrent());
            evseRealtimeDataVO.setVoltage(opLocationEvseRealTimeDTO.getVoltage());
            evseRealtimeDataVO.setPower(opLocationEvseRealTimeDTO.getPower());
            evseRealtimeDataVO.setBusId(opLocationEvseRealTimeDTO.getBusId());
            evseRealtimeDataVO.setBatterySoc(opLocationEvseRealTimeDTO.getBatterySoc());
            evseRealtimeDataVO.setTotalElectricalPower(opLocationEvseRealTimeDTO.getTotalElectricalPower());
            evseRealtimeDataVO.setRdTimeLeft(opLocationEvseRealTimeDTO.getRdTimeLeft());
            evseRealtimeDataVO.setCreateTime(opLocationEvseRealTimeDTO.getCreateTime());
            //todo 查询充电中的订单，根据充电订单中的开始充电时间、开始充电时电表读数进行计算
            // 已充电量 = 实时电表读数 - 该枪充电中订单的开始充电时电表读数
            // 充电时长 = 实时时间 - 充电订单中的开始充电时间
            // 消费费用(调用计费模块接口返回)
            Object chargingBillObject = redisUtil.get(RedisKeys.getBillChargingInfo(opLocationEvseRealTimeDTO.getBusId()));
            if (null != chargingBillObject) {
                Long reportTimestamp = null != evseRealtimeDataVO.getCreateTime() ? Long.parseLong(evseRealtimeDataVO.getCreateTime()) : System.currentTimeMillis();
                ChargingBillCacheVO chargingBillCacheVO = (ChargingBillCacheVO) chargingBillObject;
                ChargingDTO chargingDTO = new ChargingDTO();
                chargingDTO.setBusId(opLocationEvseRealTimeDTO.getBusId());
                chargingDTO.setChargePointSn(chargingBillCacheVO.getEvseSn());
                chargingDTO.setCreateTime(reportTimestamp);
                chargingDTO.setTotalElectricalPower(BigDecimal.valueOf(opLocationEvseRealTimeDTO.getTotalElectricalPower()));
                TariffComputeDTO tariffComputeDTO = billCostFeignClient.realtimeFeeCalculate(chargingDTO).getData();
                log.info("========= the tariffComputeDTO after realtimeFeeCalculate invoked:{}", tariffComputeDTO);
                evseRealtimeDataVO.setChargeDuration(tariffComputeDTO.getChargeTimes());
                evseRealtimeDataVO.setChargedEnergy(opLocationEvseRealTimeDTO.getTotalElectricalPower() - chargingBillCacheVO.getMeterStart().doubleValue());
                evseRealtimeDataVO.setChargeFee(tariffComputeDTO.getPowerBalance().doubleValue());
                evseRealtimeDataVO.setMemberId(chargingBillCacheVO.getMemberId());
            }
        }
        return Result.ofSucceed(evseRealtimeDataVO);
    }

    @Override
    public Result<List<EvsePowerStatisticVO>> getStatisticPower(Long locationUid, Long evseUid) {
        List<EvsePowerStatisticVO> evsePowerStatisticVOList = new ArrayList<>();
        String state = monitorFeign.getEvseStatus(evseUid).getName();
        // 充电中的充电桩
        if (!EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
            return Result.ofSucceed(evsePowerStatisticVOList);
        }
        //todo 从缓存中获取充电枪实时数据
        OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = new OpLocationEvseRealTimeDTO();
        Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(evseUid);
        OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);
        //todo 调用充电桩上报的实时数据存储在ES中的接口
        OpEvseMeterUploadDTO opEvseMeterUploadDTO = new OpEvseMeterUploadDTO();
        opEvseMeterUploadDTO.setBusId(opLocationEvseRealTimeDTO.getBusId());
        Result<List<OpEvseMeterUploadDTO>> evseMeterList = monitorFeignClient.selectMeterByBusId(opEvseMeterUploadDTO);
        if (!evseMeterList.getCode().equals(HttpStatus.SC_OK)) {
            return Result.ofFailed(HttpCodeEnum.BAD_REQUEST);
        }
        evseMeterList.getData().forEach(temp -> {
            EvsePowerStatisticVO evsePowerStatisticVO = new EvsePowerStatisticVO();
            evsePowerStatisticVO.setPower(temp.getTotalElectricalPower());
            evsePowerStatisticVO.setTimestamp(temp.getCreateTime());
            evsePowerStatisticVOList.add(evsePowerStatisticVO);
        });
        return Result.ofSucceed(evsePowerStatisticVOList);
    }

    @Override
    public GunStatusStatisDTO getGunStatusStatis(GunStatusQueryDTO gunStatusQueryDTO) {
        GunStatusStatisDTO gunStatusStatisDTO = new GunStatusStatisDTO();
        List<GunDayStatusStatisDTO> list = new ArrayList<>();
        List<TimeValueDTO> gunStatusList = new ArrayList<>();

        // 获取参数
        String sn = gunStatusQueryDTO.getSn();
        Long endTime = gunStatusQueryDTO.getEndTime();
        Long startTime = gunStatusQueryDTO.getStartTime();
        Integer gunNo = gunStatusQueryDTO.getGunNo();
        String timezoneId = gunStatusQueryDTO.getTimezoneId();

        // 兼容前版本, 不传时间时取当前的时间
        long end = Math.min(endTime, System.currentTimeMillis());

        String timezone = StringUtils.isEmpty(timezoneId) ? BaseConstant.GMT_8 : timezoneId;

        Calendar beginCalendar = Calendar.getInstance();
        beginCalendar.setTimeZone(TimeZone.getTimeZone(timezone));
        beginCalendar.setTimeInMillis(startTime);
        beginCalendar.set(Calendar.HOUR_OF_DAY, 0);
        beginCalendar.set(Calendar.MINUTE, 0);
        beginCalendar.set(Calendar.SECOND, 0);
        beginCalendar.set(Calendar.MILLISECOND, 0);
        // 同上, 默认取7天前0时的时间
        long begin = beginCalendar.getTimeInMillis();

        try {
            List<TimeValueDTO> gunStatusHis = queryGunStatusHis(sn, gunNo, begin, end);

            if (CollectionUtils.isEmpty(gunStatusHis) && Boolean.FALSE.equals(isDeviceOnline(sn))) {
                return gunStatusStatisDTO;
            }

            //再查一下第一条数据时间之前的一条数据：如果存在，该条数据的状态作为第一条数据的状态值；如果不存在，在第一条状态数据之前都不展示
            TimeValueDTO firstState = queryGunStatusHisPrev(sn, gunNo, begin);

            gunStatusList.addAll(gunStatusHis);

            //构造 startTime ~ endTime 的时间区间
            List<TimeRange> timeRangeList = new ArrayList<>();
            while (beginCalendar.getTimeInMillis() < endTime) {
                long sTime = beginCalendar.getTimeInMillis();
                beginCalendar.add(Calendar.DATE, 1);
                long eTime = Math.min(beginCalendar.getTimeInMillis(), System.currentTimeMillis());
                TimeRange timeRange = new TimeRange();
                timeRange.setStartTime(sTime);
                timeRange.setEndTime(eTime);
                timeRange.setStatusList(new ArrayList<>());
                timeRangeList.add(timeRange);

                GunDayStatusStatisDTO gunDayStatusStatisDTO = new GunDayStatusStatisDTO();
                gunDayStatusStatisDTO.setDate(date2Str(sTime, "MM-dd", TimeZone.getTimeZone(timezone)));
                list.add(gunDayStatusStatisDTO);
            }

            // 把状态集合装进按天分好的时间集合中
            packageStateIntoList(gunStatusList, timeRangeList);

            // 最前的一条数据状态
            Integer currentStatus = -1; //前面已经没有数据了，状态就用一个特殊值，前端可以根据这个标识值进行处理（不展示或者一个特别的颜色）
            if (null != firstState) {
                //前面还有数据，该状态就作为第一个状态值
                currentStatus = Integer.parseInt(firstState.getValue());
            }

            //将日期数据列表中的数据包装成数据段格式（包含开始，结束时间，状态值）
            for (int i = 0; i < timeRangeList.size(); i++) {
                // 取出封装进每天的状态数据
                List<TimeValueDTO> statusList = timeRangeList.get(i).getStatusList();
                if (!CollectionUtils.isEmpty(statusList)) {
                    List<GunStageWorkStatusDTO> currentDayStatus = new ArrayList<>();
                    for (int j = 0; j < statusList.size(); j++) {

                        //如果是该天的第一个状态，前面需要补一段昨日的最后一个状态数据（0点到当前时间）
                        if (j == 0) {
                            GunStageWorkStatusDTO addGunStageWorkStatusDTO = new GunStageWorkStatusDTO();
                            addGunStageWorkStatusDTO.setStartTime(timeRangeList.get(i).getStartTime());
                            addGunStageWorkStatusDTO.setStatus(currentStatus);
                            addGunStageWorkStatusDTO.setEndTime(statusList.get(j).getTime());
                            currentDayStatus.add(addGunStageWorkStatusDTO);
                        }

                        //该天真正的第一个变化的状态
                        GunStageWorkStatusDTO gunStageWorkStatusDTO = new GunStageWorkStatusDTO();
                        gunStageWorkStatusDTO.setStartTime(statusList.get(j).getTime());

                        if (j == statusList.size() - 1) { // 已经是该日期区间最后一个状态事件了(包括只有一个状态)
                            gunStageWorkStatusDTO.setEndTime(timeRangeList.get(i).getEndTime());
                        } else {
                            gunStageWorkStatusDTO.setEndTime(statusList.get(j + 1).getTime());
                        }
                        currentStatus = Integer.parseInt(statusList.get(j).getValue());
                        gunStageWorkStatusDTO.setStatus(currentStatus);
                        currentDayStatus.add(gunStageWorkStatusDTO);
                    }
                    list.get(i).setData(currentDayStatus);
                } else {
                    // 直接续上前一天最后一个状态
                    GunStageWorkStatusDTO gunStageWorkStatusDTO = new GunStageWorkStatusDTO();
                    gunStageWorkStatusDTO.setStartTime(timeRangeList.get(i).getStartTime());

                    if (timeRangeList.get(i).getEndTime() > System.currentTimeMillis()) {
                        gunStageWorkStatusDTO.setEndTime(System.currentTimeMillis());
                    } else {
                        gunStageWorkStatusDTO.setEndTime(timeRangeList.get(i).getEndTime());
                    }
                    gunStageWorkStatusDTO.setStatus(currentStatus);

                    List<GunStageWorkStatusDTO> currentDayStatus = new ArrayList<>();
                    currentDayStatus.add(gunStageWorkStatusDTO);
                    list.get(i).setData(currentDayStatus);
                }
            }
            List<GunDayStatusStatisDTO> reversList = new ArrayList<>();
            for (int i = list.size() - 1; i >= 0; i--) {
                reversList.add(list.get(i));
            }
            gunStatusStatisDTO.setSegments(reversList);
        } catch (Exception e) {
            log.error("failed to getGunStatusStatis v3", e);
        }

        return gunStatusStatisDTO;
    }

    private static String date2Str(Long timeStamp, String format, TimeZone timeZone) {
        DateFormat sdf = new SimpleDateFormat(format);
        sdf.setTimeZone(timeZone);
        return sdf.format(new Date(timeStamp));
    }

    private Boolean isDeviceOnline(String sn) {
        try {
            sn = sn.toUpperCase();
            if (Boolean.TRUE.equals(isAcmpSupport(sn))) {
                Result<Boolean> ws = wsCoreClient.userIsOnline(sn.toUpperCase(), BaseConstant.ACMP);
                if (null != ws && null != ws.getData()) {
                    return ws.getData();
                }
            } else {
                Result<Boolean> ws = wsCoreClient.userIsOnline(sn.toUpperCase(), BaseConstant.OCPP);
                if (null != ws && null != ws.getData()) {
                    return ws.getData();
                }
            }
        } catch (Exception e) {
            log.error("判断桩是否在线失败: {}", e);
        }
        return false;
    }

    private void packageStateIntoList(List<TimeValueDTO> stateList, List<TimeRange> timeRangeList) {
        //将7天内的状态变更记录分配到对应日期的集合中
        for (TimeValueDTO item : stateList) {
            // 左边闭区间右边开区间: [startTime, endTime)
            for (TimeRange timeRange : timeRangeList) {
                if (item.getTime() >= timeRange.getStartTime() && item.getTime() < timeRange.getEndTime()) {
                    timeRange.getStatusList().add(item);
                    break;
                }
            }
        }
        // 状态数据是倒序(DESC)查出的,分到每天的状态中也会是时间desc的，因此重新排序一下
        for (TimeRange timeRange : timeRangeList) {
            timeRange.getStatusList().sort(Comparator.comparing(TimeValueDTO::getTime));
        }
    }

    private Boolean isAcmpSupport(String sn) {
        if (redisUtil.isSetMember(RedisKeyConstant.ACMP_SUPPORT_PILES, sn.toUpperCase()) || sn.startsWith("D")) {
            //AC Compact, AC Elite 只支持一小部分运维协议，不能返回支持给前端，否则会造成UI变动按支持运维协议来，但功能实际又不支持
            if (sn.startsWith("A")) {
                Object obj = redisUtil.hget(RedisKeyConstant.OPS_DATA_REPORT_INFO + sn, BaseConstant.BUSINESS_TYPE_UPGATEINFO);
                log.info("isAcmpSupport sn:{},obj:{}", sn, obj);
                return !ObjectUtils.isEmpty(obj);
            }
            return true;
        }
        return false;
    }

    private List<TimeValueDTO> queryGunStatusHis(String sn, Integer gunNo, Long startTime, Long endTime) {
        List<TimeValueDTO> result = new ArrayList<>();
        //查询ES获取数据
        //没有交流桩，用有数据的设备进行测试
        String gunStatusIndex = "op_evse_status_record_alias";
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.matchPhraseQuery(BaseConstant.EVSE_SN, sn + "_" + gunNo));
        boolQueryBuilder.must(QueryBuilders.matchPhraseQuery(BaseConstant.CONNECTED_ID, gunNo));
        boolQueryBuilder.must(QueryBuilders.rangeQuery(BaseConstant.CREATE_TIME).gte(startTime).lte(endTime));
        searchSourceBuilder.query(boolQueryBuilder).size(5000).from(0).trackTotalHits(true).sort(BaseConstant.CREATE_TIME, SortOrder.DESC);
        try {
            Page<AcGunStatusEsDTO> esResultDTOPage = queryPageBySearchBuilder(gunStatusIndex, searchSourceBuilder, AcGunStatusEsDTO.class);

            //过滤掉status没有变化的数据
            if (null != esResultDTOPage && null != esResultDTOPage.getRecords() && org.apache.commons.collections4.CollectionUtils.isNotEmpty(esResultDTOPage.getRecords())) {
                /*
                 * 现在              （遍历的时间线）                   7天前
                 *  O-------------------------------------------------->
                 * preState字段用于记录桩的被遍历过的前一个状态,若该字段不为 null并且当前状态与前一个状态相同，则认为该状态从较早的那个时间开始
                 * 例如 11:37 a.m 为充电状态，它前一个状态记录为 11:28 a.m 也为充电状态，此时我们取充电状态的起始时间应该从 11:28 开始
                 */
                LocationEvseStatusV2Enum preState = null;
                for (AcGunStatusEsDTO item : esResultDTOPage.getRecords()) {
                    LocationEvseStatusV2Enum evseStatusEnumByOccpState = LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(item.getStatus());
                    if (Objects.nonNull(preState) && preState.equals(evseStatusEnumByOccpState)) {
                        result.remove(result.size() - 1);
                    }
                    result.add(TimeValueDTO.builder().value(evseStatusEnumByOccpState.getCode().toString()).time(item.getCreateTime()).build());
                    preState = evseStatusEnumByOccpState;
                }
            }
        } catch (Exception e) {
            log.error("failed to query gunstatus ES index:{}", gunStatusIndex, e);
        }
        return result;
    }

    private TimeValueDTO queryGunStatusHisPrev(String sn, Integer gunNo, Long endTime) {
        TimeValueDTO result = null;
        if (Boolean.TRUE.equals(isAcmpSupport(sn))) {
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery().must(QueryBuilders.matchQuery("sn", sn))
                    .must(QueryBuilders.matchQuery(BaseConstant.CONNECTED_ID, gunNo))
                    .filter(QueryBuilders.rangeQuery(BaseConstant.CREATE_TIME).lt(endTime));
            SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder().query(queryBuilder).from(0).size(1).sort(BaseConstant.CREATE_TIME, SortOrder.DESC);
            try {
                Page<PileWorkStatusHistoryDTO> resultPage = queryPageBySearchBuilder(BaseConstant.PILE_WORK_STATUS_HISTORY_INDEX + "*", searchSourceBuilder, PileWorkStatusHistoryDTO.class);

                PileWorkStatusHistoryDTO pileWorkStatusHistory = null;
                if (null != resultPage && org.apache.commons.collections4.CollectionUtils.isNotEmpty(resultPage.getRecords())) {
                    pileWorkStatusHistory = resultPage.getRecords().get(0);
                }

                // 取endTime前的第一条
                if (null != pileWorkStatusHistory) {
                    // value字段 将运维0-9状态转换为为新版1-7状态
                    result = TimeValueDTO.builder().value(LocationEvseStatusV2Enum.convertOPSGunStatus(pileWorkStatusHistory.getStatus()).getCode().toString()).time(pileWorkStatusHistory.getCreateTime()).build();
                }
            } catch (Exception e) {
                log.error("failed to query pile_work_status_his ES index:{}", BaseConstant.PILE_WORK_STATUS_HISTORY_INDEX);
                e.printStackTrace();
            }

        } else {
            //查询ES获取数据
            //没有交流桩，用有数据的设备进行测试
            String gunStatusIndex = "op_evse_status_record_alias";
            SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
            BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
            boolQueryBuilder.must(QueryBuilders.matchPhraseQuery(BaseConstant.EVSE_SN, sn + "_" + gunNo));
            boolQueryBuilder.must(QueryBuilders.matchPhraseQuery(BaseConstant.CONNECTED_ID, gunNo));
            boolQueryBuilder.must(QueryBuilders.rangeQuery(BaseConstant.CREATE_TIME).lt(endTime));
            searchSourceBuilder.query(boolQueryBuilder).size(1).from(0).trackTotalHits(true).sort(BaseConstant.CREATE_TIME, SortOrder.DESC);
            try {
                Page<AcGunStatusEsDTO> esResultDTOPage = queryPageBySearchBuilder(gunStatusIndex, searchSourceBuilder, AcGunStatusEsDTO.class);
                //过滤掉status没有变化的数据
                if (null != esResultDTOPage && null != esResultDTOPage.getRecords() && org.apache.commons.collections4.CollectionUtils.isNotEmpty(esResultDTOPage.getRecords())) {
                    log.info("status list size:{}", esResultDTOPage.getRecords().size());
                    AcGunStatusEsDTO item = esResultDTOPage.getRecords().get(0);
                    LocationEvseStatusV2Enum evseDeviceStatusEnum = LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(item.getStatus());
                    if (null == evseDeviceStatusEnum) {
                        log.error("failed to match gunstatus:{}", item.getStatus());
                    } else {
                        result = TimeValueDTO.builder().value(evseDeviceStatusEnum.getCode().toString()).time(item.getCreateTime()).build();
                    }
                }

            } catch (Exception e) {
                log.error("failed to query gunstatus ES index:{}", gunStatusIndex);
                e.printStackTrace();
            }
        }
        return result;
    }

    private <T> Page<T> queryPageBySearchBuilder(String index, SearchSourceBuilder searchSourceBuilder, Class<T> tClass) {
        log.info("[queryPageBySearchBuilder]ES查询条件 - {}", searchSourceBuilder);
        String[] indices = index.split(",");
        SearchRequest request = new SearchRequest(indices);
        request.source(searchSourceBuilder);
        try {
            SearchResponse response = restHighLevelClient.search(request, RequestOptions.DEFAULT);
            org.elasticsearch.search.SearchHit[] hits = response.getHits().getHits();
            TotalHits totalHits = response.getHits().getTotalHits();
            Page<T> page = new Page<>();
            if (totalHits != null) {
                page.setTotal(totalHits.value);
            }
            List<T> res = new ArrayList<>(hits.length);
            for (org.elasticsearch.search.SearchHit hit : hits) {
                res.add(JSON.parseObject(hit.getSourceAsString(), tClass));
            }
            page.setRecords(res);
            return page;
        } catch (Exception e) {
            e.printStackTrace();
//            throw new BusinessException(ES_REQUEST_FAILED);
            throw new MessageCodeException(PileBaseEnum.LOCATION_NAME_REPEATED);
        }

    }

    private Long calculateDurationByTimezone(String timezone, Long beginTimestamp, Long endTimestamp) {
        LocalDateTime beginLocalDateTime = LocalDateTimeUtil.ofUTC(beginTimestamp);
        LocalDateTime endLocalDateTime = LocalDateTimeUtil.ofUTC(endTimestamp);
        Date endDate = null;
        Date beginDate = null;
        try {
            beginDate = utcToZone(beginLocalDateTime, timezone);
            endDate = utcToZone(endLocalDateTime, timezone);
        } catch (ParseException e) {
            log.error("时区转换异常，{}", JSONUtil.toJsonStr(e));
        }
        return DateUtil.between(beginDate, endDate, DateUnit.MS);
    }

    /**
     * 服务器时间转换成对应时区时间
     *
     * @param time UTC 时间
     * @param zone 时区如： +8 0 +1
     * @return
     */
    public static Date utcToZone(LocalDateTime time, String zone) throws ParseException {
        String s = DateUtil.formatLocalDateTime(time);
        return timeZoneTransfer(s, "yyyy-MM-dd HH:mm:ss", "0", zone);
    }

    /**
     * 时区转换
     *
     * @param time           时间字符串
     * @param pattern        格式 "yyyy-MM-dd HH:mm"
     * @param nowTimeZone    eg:+8，0，+9，-1 等等
     * @param targetTimeZone 同nowTimeZone
     * @return
     */
    public static Date timeZoneTransfer(String time, String pattern, String nowTimeZone, String targetTimeZone) throws ParseException {
        if (StrUtil.isBlank(time)) {
            throw new ParseException("DateTime Parse Error!!", 5000);
        }
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat(pattern);
        simpleDateFormat.setTimeZone(TimeZone.getTimeZone("GMT" + nowTimeZone));
        Date date;
        try {
            date = simpleDateFormat.parse(time);
        } catch (ParseException e) {
            throw new ParseException("DateTime Parse Error!!", 5000);
        }
        simpleDateFormat.setTimeZone(TimeZone.getTimeZone("GMT" + targetTimeZone));
        return DateUtil.parse(simpleDateFormat.format(date));
    }

    public static void main(String[] args) {
        String[] myArray = {"Apple", "Banana", "Orange"};
        List<String> myList = Arrays.asList(myArray);
        myList.add("Guava");
    }
}
