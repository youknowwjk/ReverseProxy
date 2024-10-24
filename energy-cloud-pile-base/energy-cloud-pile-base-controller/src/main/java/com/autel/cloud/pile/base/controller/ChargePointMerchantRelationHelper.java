package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.device.dto.ChargePileDTO;
import com.autel.cloud.device.feign.DeviceClient;
import com.autel.cloud.pile.base.ChargePointNoticeEvent;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.convert.ChargePointMerchantRelationTypeMapper;
import com.autel.cloud.pile.base.domain.model.ChargePointDTO;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.dto.UpdatePileNameDTO;
import com.autel.cloud.pile.base.enums.CategoryEnum;
import com.autel.cloud.pile.base.enums.ConnectorGunTypeEnum;
import com.autel.cloud.pile.base.enums.MerchantChargePointRelationEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.AdvertPlatformAppFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.MgmtFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.AdvertPlatformAppServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileDeviceServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.vo.Connector;
import com.autel.cloud.pile.base.vo.OcppLocationEVSEVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.util.Asserts;
import org.elasticsearch.index.query.QueryBuilders;
import org.slf4j.MDC;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.TOPIC_EXCHANGE_PILE_BASE;
import static com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService.phaseConstant;

@RestController
@RequestMapping("/chargePoint")
@Slf4j
@Validated
@Api(value = "统一桩管理", tags = "统一桩管理")
public class ChargePointMerchantRelationHelper {
    @Resource
    private OpLocationPileEvseMapper opLocationPileEvseMapper;
    @Resource
    private OpLocationMapper opLocationMapper;
    @Resource
    private DeviceClient deviceServiceFeign;
    @Resource
    private PileDeviceServiceAdapter pileDeviceServiceAdapter;
    @Resource
    private OpLocationConnectorMapper opLocationConnectorMapper;
    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    @Resource
    private ChargePointMerchantRelationMapper chargePointMerchantRelationMapper;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private OpLocationPileEvseElastic opLocationPileEvseElastic;

    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Resource
    private RabbitTemplate rabbitTemplate;
    @Resource
    OpLocationEvseExpandElastic opLocationEvseExpandElastic;

    @Resource
    private OpLocationEvseService opLocationEvseService;


    @Resource
    private OpLocationEvseMapper opLocationEvseMapper;


    /**
     * [{"connectorType":1,"connectorId":1,"connectorName":"","power":0.0},{"connectorType":2,"connectorId":2,"connectorName":"","power":0.0},
     * {"connectorType":3,"connectorId":3,"connectorName":"","power":0.0},{"connectorType":6,"connectorId":4,"connectorName":"","power":0.0}]
     *
     * "connectorType":3,  修改为 "connectorType":5,
     *
     * @param merchantId
     * @return
     */
    @GetMapping("/fixedGunType")
    @ApiOperation(value = "所有type 2的枪全部改为type 1   ")
    public Result<String> fixedGunType(@RequestParam(value = "merchantId", defaultValue = "0") Long merchantId) {
        IPage<ChargePointMerchantRelationEntity> billEntityIPage = null;
        long current = 1, size = 30;
        IPage<ChargePointMerchantRelationEntity> page = new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(current, size);
        do {
            LambdaQueryWrapper<ChargePointMerchantRelationEntity> query = new LambdaQueryWrapper<>();
            query.eq(merchantId > 0,   ChargePointMerchantRelationEntity::getMerchantId, merchantId);
            query.orderByAsc(ChargePointMerchantRelationEntity::getId);
            billEntityIPage = chargePointMerchantRelationMapper.selectPage(page, query);
            billEntityIPage.getRecords().forEach(entity -> {
                try {
                    boolean sync = false;
                    String s = JSON.toJSONString(entity.getConnectors());
                    List<Connector> connectors = JSON.parseArray(s, Connector.class);
                    if (!CollectionUtils.isEmpty(connectors)) {
                        for (Connector connector : connectors) {
                            if (connector.getConnectorType() == 3 && entity.getSn().startsWith("AL")) {
                                sync = true;
                                connector.setConnectorType(5);
                                connector.setConnectorName("Type 1");
                            }
                        }
                    }
                    if (sync) {
                        entity.setConnectors(connectors);
                        log.info("sync {}", JSON.toJSONString(entity));
                        chargePointMerchantRelationMapper.updateById(entity);
                        try {
                            push(entity);
                        }catch (Exception e) {
                            log.info("push failed  {}", JSON.toJSONString(entity));
                            log.error("push failed ", e);
                        }

                        List<OpLocationEvseElasticDTO> allByPileSn = opLocationEvseElastic.findAllByPileSn(entity.getSn());
                        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO: allByPileSn) {

                            Optional<OpLocationEvseExpandElasticDTO> byId = opLocationEvseExpandElastic.findById(opLocationEvseElasticDTO.getId());
                            if (byId.isPresent()) {
                                OpLocationEvseExpandElasticDTO opLocationEvseExpandElasticDTO = byId.get();
                                if(opLocationEvseExpandElasticDTO.getGunType() == 3) {
                                    opLocationEvseExpandElasticDTO.setGunType(5);
                                    opLocationEvseExpandElastic.save(opLocationEvseExpandElasticDTO);
                                }
                            }
                            if(opLocationEvseElasticDTO.getGunType() == 3) {
                                opLocationEvseElasticDTO.setGunType(5);

                                LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper1 = Wrappers.lambdaQuery(OpLocationEvseEntity.class);
                                queryWrapper1.eq(OpLocationEvseEntity::getEvseSn, opLocationEvseElasticDTO.getEvseSn());
                                queryWrapper1.eq(OpLocationEvseEntity::getDeleted, 0);
                                List<OpLocationEvseEntity> evseEntityList = opLocationEvseMapper.selectList(queryWrapper1);
                                for (OpLocationEvseEntity evseEntity : evseEntityList) {
                                    List<OpLocationConnectorEntity> connectorListByEvseId = opLocationConnectorMapper.getConnectorListByEvseId(evseEntity.getId());
                                    for (OpLocationConnectorEntity connectorEntity : connectorListByEvseId ){
                                        if(connectorEntity.getGunType() == 3) {
                                            connectorEntity.setGunType(5);
                                            opLocationConnectorMapper.updateById(connectorEntity);
                                        }
                                    }
                                }
                                opLocationEvseElastic.save(opLocationEvseElasticDTO);
                            }
                        }
                    }
                }catch (Exception e) {
                    log.info("failed  {}", JSON.toJSONString(entity));
                    log.error("handle failed ", e);
                    throw e;
                }


            });
            current++;
            page.setCurrent(current);
        } while (current <= billEntityIPage.getPages());
        return Result.ofSucceed();
    }

    @ApiOperation(value = "处理历史桩", notes = "查询道通充电桩")
    @PostMapping("/syncLocationPile")
    public Result<Object> syncLocationPile(@RequestParam Long locationId) {
        // ↓↓↓↓↓↓↓↓↓↓↓ 在es但是没有在MySQL数据库里的数据 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper1 = Wrappers.lambdaQuery(OpLocationEvseEntity.class);
        queryWrapper1.select(OpLocationEvseEntity::getId, OpLocationEvseEntity::getEvseSn);
        queryWrapper1.eq(OpLocationEvseEntity::getLocationId, locationId);
        queryWrapper1.eq(OpLocationEvseEntity::getDeleted, 0);
        List<OpLocationEvseEntity> evseEntityList = opLocationEvseMapper.selectList(queryWrapper1);
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(evseEntityList)) {
            List<Long> collect = evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toList());
            syncLocationEvse(locationId, collect);
        } else {
            syncLocationEvse(locationId, Collections.emptyList());
        }

        // 只查询 在 数据库里面未删除的记录id
        LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper2 = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        queryWrapper2.select(OpLocationPileEvseEntity::getId, OpLocationPileEvseEntity::getPileSn);
        queryWrapper2.eq(OpLocationPileEvseEntity::getLocationId, locationId);
        queryWrapper2.eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> OpLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(queryWrapper2);
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(OpLocationPileEvseEntityList)) {
            List<Long> collect = OpLocationPileEvseEntityList.stream().map(OpLocationPileEvseEntity::getId).collect(Collectors.toList());
            syncLocationPileEvse(locationId, collect);
        } else {
            syncLocationPileEvse(locationId, Collections.emptyList());
            return Result.ofSucceed();
        }
        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 在es但是没有在MySQL数据库里的数据 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // ↓↓↓↓↓↓↓↓↓↓↓ 在MySQL数据库里的数据 不在es ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        if (CollectionUtils.isEmpty(OpLocationPileEvseEntityList)) {
            return Result.ofSucceed();
        }
        LambdaQueryWrapper<OpLocationPileEvseEntity> query = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        query.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
        List<String> collect = OpLocationPileEvseEntityList.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList());
        query.in(OpLocationPileEvseEntity::getPileSn, collect);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(query);

        checkAndSync(opLocationPileEvseEntities);

        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 在MySQL数据库里的数据 不在es ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        return Result.ofSucceed();
    }

    private AtomicBoolean checkAndSync(List<OpLocationPileEvseEntity> opLocationPileEvseEntities) {
        AtomicBoolean sync = new AtomicBoolean(Boolean.FALSE);
        Set<Long> collect = opLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getLocationId).collect(Collectors.toSet());
        List<OpLocationPileEvseElasticDTO> allByLocationIdIn = opLocationPileEvseElastic.findAllByLocationIdIn(collect);
        Map<Long, List<OpLocationPileEvseElasticDTO>> locationPiles = allByLocationIdIn.stream().collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));
        for (OpLocationPileEvseEntity pileEvse : opLocationPileEvseEntities) {
            if (!locationPiles.containsKey(pileEvse.getLocationId())) {
                doSync(sync, pileEvse);
            } else {
                Set<String> pileSNs = locationPiles.get(pileEvse.getLocationId()).stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toSet());
                if (!pileSNs.contains(pileEvse.getPileSn())) {
                    doSync(sync, pileEvse);
                }
            }
        }
        return sync;
    }

    private void doSync(AtomicBoolean sync, OpLocationPileEvseEntity pileEvse) {
        log.warn("mysql es 数据不同步 直接删除mysql 桩数据: {}", JSON.toJSONString(pileEvse));
        sync.compareAndSet(Boolean.FALSE, Boolean.TRUE);
        OpLocationPileEvseEntity entity = opLocationPileEvseMapper.selectById(pileEvse.getId());
        if (Objects.isNull(entity)) {
            return;
        }
        entity.setUpdatedAt(System.currentTimeMillis());
        entity.setDeleted(1);
        opLocationPileEvseMapper.updateById(entity);
        String evseList = pileEvse.getEvseList();
        if (org.springframework.util.StringUtils.hasText(evseList)) {
            List<Long> evseIds = JSON.parseArray(evseList, Long.class);
            for (Long evseId : evseIds) {
                log.warn("mysql es 数据不同步 直接删除mysql 枪数据: {}", JSON.toJSONString(pileEvse));
                opLocationEvseMapper.deleteByEvseId(evseId);
                opLocationPileEvseElastic.deleteById(evseId);
            }
        }
    }

    private void syncLocationPileEvse(Long locationId, Collection<Long> ids){
        if (ids.isEmpty()) {
            List<OpLocationPileEvseElasticDTO> result = opLocationPileEvseElastic.findAllByLocationId(locationId);
            log.info("sync findAllByLocationId OpLocationPileEvseElasticDTO {}", JSON.toJSONString(result));
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(result)) {
                List<Long> docIds = result.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationPileEvseElastic.deleteAllById(docIds);
            }
        } else {
            List<OpLocationPileEvseElasticDTO> result = opLocationPileEvseElastic.findByLocationIdAndIdNotIn(locationId, ids);
            log.info("sync findByLocationIdAndIdNotIn OpLocationPileEvseElasticDTO {}", JSON.toJSONString(result));
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(result)) {
                List<Long> docIds = result.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationPileEvseElastic.deleteAllById(docIds);
            }
        }
    }

    private void syncLocationEvse(Long locationId, Collection<Long> ids){
        if (ids.isEmpty()) {
            List<OpLocationEvseElasticDTO> result = opLocationEvseElastic.findAllByLocationId(locationId);
            log.info("sync findAllByLocationId OpLocationEvseElasticDTO {}", JSON.toJSONString(result));
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(result)) {
                List<Long> docIds = result.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationEvseElastic.deleteAllById(docIds);
            }
        } else {
            List<OpLocationEvseElasticDTO> result = opLocationEvseElastic.findByLocationIdAndIdNotIn(locationId, ids);
            log.info("sync findByLocationIdAndIdNotIn OpLocationEvseElasticDTO {}", JSON.toJSONString(result));
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(result)) {
                List<Long> docIds = result.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationEvseElastic.deleteAllById(docIds);
            }
        }
    }

    @ApiOperation(value = "处理历史桩", notes = "查询道通充电桩")
    @PostMapping("/fixedPileName")
    public Result<Object> fixedPileName(@RequestBody List<PileNameDTO> list) {
        int count = 0;
        for (PileNameDTO item : list) {
            try {
                if (Objects.nonNull(item.getMerchantId())) {
                    LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper1 = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
                    queryWrapper1.eq(ChargePointMerchantRelationEntity::getSn, item.getSn())
                            .eq(ChargePointMerchantRelationEntity::getMerchantId, item.getMerchantId())
                            .eq(ChargePointMerchantRelationEntity::getRelation, 0);
                    ChargePointMerchantRelationEntity ops = chargePointMerchantRelationMapper.selectOne(queryWrapper1);
                    if (Objects.nonNull(ops) && ops.getSn().equalsIgnoreCase(ops.getName())) {
                        ops.setName(item.getPileName());
                        chargePointMerchantRelationMapper.updateById(ops);
                        push(ops);
                    }
                }
                if (Objects.nonNull(item.getOperationId())) {
                    LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
                    queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, item.getSn())
                            .eq(ChargePointMerchantRelationEntity::getMerchantId, item.getOperationId())
                            .eq(ChargePointMerchantRelationEntity::getRelation, 1);
                    ChargePointMerchantRelationEntity owner = chargePointMerchantRelationMapper.selectOne(queryWrapper);
                    if (Objects.nonNull(owner)  && owner.getSn().equalsIgnoreCase(owner.getName())) {
                        owner.setName(item.getPileName());
                        chargePointMerchantRelationMapper.updateById(owner);
                        push(owner);
                        UpdatePileNameDTO updatePileNameDTO = new UpdatePileNameDTO();
                        updatePileNameDTO.setPileName(owner.getName());
                        updatePileNameDTO.setPileSn(owner.getSn());
                        opLocationEvseService.updatePileName(updatePileNameDTO);
                    }
                }
                count++;
            } catch (Exception e) {
                log.error("fixedPileName" + JSON.toJSONString(item), e);
            }
        }
        return Result.ofSucceed(count + " /      " + list.size());
    }

    private void push(ChargePointMerchantRelationEntity ops) {
        ChargePointNoticeEvent chargePointNoticeEvent = ChargePointMerchantRelationTypeMapper.INSTANCE.chargePointMerchantRelationEntityToChargePointNoticeEvent(ops);
        chargePointNoticeEvent.setEvent(ChargePointNoticeEvent.Event.UPDATE);
        if (Objects.isNull(chargePointNoticeEvent.getOwner())) {
            ChargePointMerchantRelationEntity relation = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.OWNER);
            Optional.ofNullable(relation).ifPresent(target -> chargePointNoticeEvent.setOwner(target.getMerchantId()));
        }
        if (Objects.isNull(chargePointNoticeEvent.getMaintenance())) {
            ChargePointMerchantRelationEntity relation = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.MAINTENANCE);
            Optional.ofNullable(relation).ifPresent(target -> chargePointNoticeEvent.setMaintenance(target.getMerchantId()));
        }
        Optional.ofNullable(chargePointNoticeEvent.getOwner()).ifPresent(id -> chargePointNoticeEvent.setOwnerName(pileUserServiceAdapter.findMerchantById(id).getName()));
        Optional.ofNullable(chargePointNoticeEvent.getMaintenance()).ifPresent(id -> chargePointNoticeEvent.setMaintenanceName(pileUserServiceAdapter.findMerchantById(id).getName()));

        log.info("chargePointNoticeEvent=  {}", JSON.toJSONString(chargePointNoticeEvent));
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, ChargePointNoticeEvent.class.getSimpleName(), JSON.toJSONString(chargePointNoticeEvent));
    }

    @Data
    public static class PileNameDTO {
        private String sn;//运维商家编号
        private String pileName;//运维商家编号
        private Long merchantId;//运维商家编号
        private Long operationId;// 运营商家编号


    }

    @ApiOperation(value = "处理历史桩", notes = "查询道通充电桩")
    @PostMapping("/fixedOwnerPileName")
    public Result<Object> update(@RequestBody Set<String> sns) {

        List<String> errorSNs = new ArrayList<>();
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
        lambdaQueryWrapper.in(org.apache.commons.collections4.CollectionUtils.isNotEmpty(sns), OpLocationPileEvseEntity::getPileSn, sns);
        lambdaQueryWrapper.select(OpLocationPileEvseEntity::getPileSn, OpLocationPileEvseEntity::getLocationId, OpLocationPileEvseEntity::getName, OpLocationPileEvseEntity::getBrandId);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
        for (OpLocationPileEvseEntity entity : opLocationPileEvseEntities) {
            try {
                LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
                queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, entity.getPileSn())
                        .eq(ChargePointMerchantRelationEntity::getRelation, 1);

                ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
                chargePointMerchantRelationEntity.setName(entity.getName());
                chargePointMerchantRelationMapper.updateById(chargePointMerchantRelationEntity);


                push(chargePointMerchantRelationEntity);
            } catch (Exception e) {
                errorSNs.add(entity.getPileSn());
                log.error("history biz add " + entity.getPileSn(), e);
            }
        }
        return Result.ofSucceed(errorSNs);
    }

    private ChargePointMerchantRelationEntity findRelation(String sn, MerchantChargePointRelationEnum merchantChargePointRelationEnum) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class)
                .eq(ChargePointMerchantRelationEntity::getSn, sn).eq(ChargePointMerchantRelationEntity::getRelation, merchantChargePointRelationEnum.getKey());
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
        return chargePointMerchantRelationEntity;
    }

    @ApiOperation(value = "处理历史桩", notes = "查询道通充电桩")
    @PostMapping("/history")
    public Result<Object> history(@RequestBody Set<String> sns, @RequestParam(value = "grantLicense", defaultValue = "0") int grantLicense, @RequestParam(value = "day", defaultValue = "30") int day) {

        List<String> errorSNs = new ArrayList<>();
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
        lambdaQueryWrapper.in(org.apache.commons.collections4.CollectionUtils.isNotEmpty(sns), OpLocationPileEvseEntity::getPileSn, sns);
        lambdaQueryWrapper.select(OpLocationPileEvseEntity::getPileSn, OpLocationPileEvseEntity::getLocationId, OpLocationPileEvseEntity::getName, OpLocationPileEvseEntity::getBrandId);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
        for (OpLocationPileEvseEntity entity : opLocationPileEvseEntities) {
            try {
                Long locationId = entity.getLocationId();
                OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
                if (Objects.nonNull(opLocationEntity)) {
                    HashSet<String> es = new HashSet<>();
                    es.add(entity.getPileSn());
                    Asserts.notNull(opLocationEntity.getOperatorId(), entity.getPileSn() + " getOperatorId is null");
                    history(es, opLocationEntity.getOperatorId(), opLocationEntity.getZoneId());
                }
            } catch (Exception e) {
                errorSNs.add(entity.getPileSn());
                log.error("history biz add " + entity.getPileSn(), e);
            }
        }
        return Result.ofSucceed(errorSNs);
    }


    public List<String> history(Set<String> sns, Long merchantId, String zoneId) {
        List<String> errorSNs = new ArrayList<>();
        Result<List<ChargePileDTO>> result = deviceServiceFeign.queryPileList(new ArrayList<>(sns));
        for (ChargePileDTO chargePileDTO : result.getData()) {
            try {
                ChargePointDTO param = new ChargePointDTO();
                com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileInfo = pileDeviceServiceAdapter.getChargePileInfo(chargePileDTO.getSn());
                param.setBrandId(Long.valueOf(chargePileInfo.getBrandId()));
                param.setBrandName(chargePileInfo.getBrandName());
                param.setProductNamePdm(chargePileInfo.getProductNamePdm());
                param.setName(chargePileInfo.getSn());
                param.setZoneId(Optional.ofNullable(zoneId).orElse("Asia/Shanghai"));
                param.setName_1(chargePileInfo.getSn());
                param.setName_2(chargePileInfo.getSn());
                param.setName_3(chargePileInfo.getSn());
                param.setSn(chargePileInfo.getSn());
                param.setPartProduct(chargePileDTO.getPartProduct());
                param.setPin(chargePileDTO.getPin());
                List<Connector> connectors = new ArrayList<>();
                OcppLocationEVSEVO locationByPileSn = opLocationConnectorMapper.getLocationByPileSn(chargePileDTO.getSn());
                log.info("OcppLocationEVSEVO {}", JSON.toJSONString(locationByPileSn));
                List<OpLocationConnectorEntity> connectorListBysn = opLocationConnectorMapper.getConnectorListBysn(chargePileDTO.getSn());
                for (OpLocationConnectorEntity entity : connectorListBysn) {
                    Connector connector = new Connector();
                    connector.setConnectorId(Integer.valueOf(entity.getConnectorId()));
                    connector.setConnectorType(entity.getGunType());
                    connector.setPower(BigDecimal.valueOf(entity.getPower()));
                    connector.setConnectorName(ConnectorGunTypeEnum.getEnumByCode(entity.getGunType()).getName());
                    connectors.add(connector);
                }
                param.setPowerType(Objects.nonNull(chargePileInfo.getCategory()) && chargePileInfo.getCategory() == 2 ? "DC" : "AC");
                param.setConnectors(connectors);
                if (Objects.nonNull(chargePileInfo.getOutputPower())) {
                    param.setRatedPower(BigDecimal.valueOf(chargePileDTO.getOutputPower()));
                }

                param.setMerchantType(pileUserServiceAdapter.getMerchantTypeById(merchantId));
                String type = CategoryEnum.getEnumByCode(Objects.isNull(chargePileInfo.getCategory()) ? 1 : chargePileInfo.getCategory()).getDesc();
                int phase = chargePileDTO.getPhase() == null ? 3 : chargePileDTO.getPhase();
                param.setPhases(type + "_" + phase + "_" + phaseConstant);
                chargePointMerchantRelationService.save(param, merchantId);
            } catch (Exception e) {
                errorSNs.add(chargePileDTO.getSn());
                log.error("history add " + chargePileDTO.getSn(), e);
            }
        }
        return errorSNs;
    }


    @ApiOperation(value = "同步运维桩添加设备到统一桩管理", notes = "同步广告添加设备到统一桩管理")
    @PostMapping("/history-ops")
    public Result<Void> historyOps(@RequestBody MgmtFeignClient.PileHistoryDTO dto, @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        Page<MgmtFeignClient.PileInfoPO> history = null;
        AtomicReference<MgmtFeignClient.PileInfoPO> lastBillId = new AtomicReference<>();
        try {
            do {
                history = creativePlatformAppServiceAdapter.history(dto);
                if (Objects.requireNonNull(history).hasNext()) {
                    dto.setPage(BigDecimal.valueOf(history.getCurrent() + 1).intValue());
                }
                history.getRecords().forEach(item -> {
                    lastBillId.set(item);
                    try {
                        log.info("正在同步运维添加设备到统一桩管理 {}", JSON.toJSONString(item));
                        ChargePointDTO param = new ChargePointDTO();
                        SellerDetailVO sellerDetailVO = pileUserServiceAdapter.findMerchantById(Long.parseLong(item.getMerchantId()));
                        if (Objects.nonNull(sellerDetailVO)) {
                            param.setMerchantType(pileUserServiceAdapter.getMerchantTypeById(Long.parseLong(item.getMerchantId())));
                        } else {
                            sellerDetailVO = pileUserServiceAdapter.findMerchantById(Long.parseLong(item.getOperationId()));
                            if (Objects.nonNull(sellerDetailVO)) {
                                param.setMerchantType(pileUserServiceAdapter.getMerchantTypeById(Long.parseLong(item.getMerchantId())));
                            } else {
                                return;
                            }
                        }
                        com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileInfo = pileDeviceServiceAdapter.getChargePileInfo(item.getSn());
                        param.setBrandId(Long.valueOf(chargePileInfo.getBrandId()));
                        param.setBrandName(chargePileInfo.getBrandName());
                        param.setProductNamePdm(chargePileInfo.getProductNamePdm());
                        param.setName(chargePileInfo.getSn());
                        param.setZoneId(Optional.ofNullable(zoneId).orElse("Asia/Shanghai"));
                        param.setName(item.getPileName());
                        param.setSn(chargePileInfo.getSn());
                        param.setPartProduct(chargePileInfo.getPartProduct());
                        param.setPin(item.getPin());
                        Integer connectorNum = chargePileInfo.getConnectorNum();
                        Integer connectorType = chargePileInfo.getConnectorType();

                        int max = Optional.ofNullable(connectorNum).orElse(1);
                        int connType = Optional.ofNullable(connectorType).orElse(1);

                        List<Connector> connectors = new ArrayList<>();


                        List<MgmtFeignClient.PileGunPO> pileGunPOList = item.getPileGunPOList();

                        if (Objects.nonNull(pileGunPOList) && !pileGunPOList.isEmpty()) {
                            for (MgmtFeignClient.PileGunPO gunItem : pileGunPOList) {
                                Connector connector = new Connector();
                                connector.setConnectorId(gunItem.getGunNo());
                                connector.setConnectorType(gunItem.getGunType());
                                connector.setConnectorName(ConnectorGunTypeEnum.getEnumByCode(connType).getName());
                                connectors.add(connector);
                            }
                        } else {
                            for (int i = 1; i <= max; i++) {
                                Connector connector = new Connector();
                                connector.setConnectorId(i);
                                connector.setConnectorType(connType);
                                connector.setConnectorName(ConnectorGunTypeEnum.getEnumByCode(connType).getName());
                                connectors.add(connector);
                            }
                        }
                        param.setPowerType(Objects.nonNull(chargePileInfo.getCategory()) && chargePileInfo.getCategory() == 2 ? "DC" : "AC");
                        param.setConnectors(connectors);
                        if (Objects.nonNull(chargePileInfo.getOutputPower())) {
                            param.setRatedPower(BigDecimal.valueOf(chargePileInfo.getOutputPower()));
                        }


                        String type = CategoryEnum.getEnumByCode(chargePileInfo.getCategory()).getDesc();
                        int phase = chargePileInfo.getPhase() == null ? 3 : chargePileInfo.getPhase();
                        param.setPhases(type + "_" + phase + "_" + ChargePointMerchantRelationService.phaseConstant);
                        chargePointMerchantRelationService.save(param, Long.parseLong(sellerDetailVO.getId()));
                    } catch (Exception e) {
                        log.error("同步运维添加设备到统一桩管理" + JSON.toJSONString(item), e);
                    }
                });
            } while (Objects.nonNull(history.getRecords()) && Objects.requireNonNull(history).hasNext());
        } catch (Exception e) {
            log.error("history-ops sn :" + lastBillId.get(), e);
            String format = String.format("迁移运维桩历史终止 lastBillId=%s", JSON.toJSONString(lastBillId.get()));
            log.info(format);
        }

        return Result.ofSucceed();
    }

    @Resource
    private AdvertPlatformAppServiceAdapter creativePlatformAppServiceAdapter;

    @ApiOperation(value = "同步广告添加设备到统一桩管理", notes = "同步广告添加设备到统一桩管理")
    @PostMapping("/history-ads")
    public Result<Object> history(@RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        List<AdvertPlatformAppFeignClient.ResponseData> sync = creativePlatformAppServiceAdapter.sync();
        if (CollectionUtils.isEmpty(sync)) {
            log.info("no data");
            return Result.ofSucceed();
        }
        List<String> success = new ArrayList<>();
        List<String> errors = new ArrayList<>();
        log.info("正在同步广告桩 {}", sync.stream().map(AdvertPlatformAppFeignClient.ResponseData::getSn).collect(Collectors.toSet()));
        log.info("正在同步广告添加设备到统一桩管理 共{}", sync.size());
        for (AdvertPlatformAppFeignClient.ResponseData item : sync) {
            try {
                log.info("正在同步广告添加设备到统一桩管理 {}", JSON.toJSONString(item));
                ChargePointDTO param = new ChargePointDTO();
                com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileInfo = pileDeviceServiceAdapter.getChargePileInfo(item.getSn());
                param.setBrandId(Long.valueOf(chargePileInfo.getBrandId()));
                param.setBrandName(chargePileInfo.getBrandName());
                param.setProductNamePdm(chargePileInfo.getProductNamePdm());
                param.setName(chargePileInfo.getSn());
                param.setZoneId(Optional.ofNullable(zoneId).orElse("Asia/Shanghai"));
                param.setName(item.getName());
                param.setSn(chargePileInfo.getSn());
                param.setPartProduct(chargePileInfo.getPartProduct());
                param.setPin(item.getPin());


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
                }
                String type = CategoryEnum.getEnumByCode(chargePileInfo.getCategory()).getDesc();
                int phase = chargePileInfo.getPhase() == null ? 3 : chargePileInfo.getPhase();
                param.setPhases(type + "_" + phase + "_" + ChargePointMerchantRelationService.phaseConstant);
                chargePointMerchantRelationService.save(param, Long.parseLong(item.getSellerId()));
                success.add(item.getSn());
            } catch (Exception e) {
                errors.add(item.getSn());
                log.error("同步广告添加设备到统一桩管理" + JSON.toJSONString(item), e);
            }
        }

        Map<String, Collection<String>> result = new HashMap<>();
        result.put("success", success);
        result.put("errors", errors);
        return Result.ofSucceed(result);
    }
//
//    @ApiOperation(value = "表数据迁移", notes = "表数据迁移")
//    @PostMapping("/insert")
//    public Result<Object> insert(@RequestBody List<String> sns) {
//        LambdaQueryWrapper<ChargePointEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointEntity.class);
//        queryWrapper.in(!CollectionUtils.isEmpty(sns), ChargePointEntity::getSn, sns);
//        Page<ChargePointEntity> page = new Page<>(1, 30);
//        do {
//            page = chargePointMapper.selectPage(page, queryWrapper);
//            if (Objects.nonNull(page)) {
//                List<ChargePointEntity> records = page.getRecords();
//                for (ChargePointEntity chargePointEntity : records) {
//                    ChargePointMerchantRelationEntity entity = ChargePointMerchantRelationTypeMapper.INSTANCE.map(chargePointEntity);
//                    try {
//                        if (Objects.nonNull(chargePointEntity.getOwner())) {
//                            entity.setId(IdWorker.getId());
//                            entity.setMerchantId(chargePointEntity.getOwner());
//                            entity.setBindTime(chargePointEntity.getOwnerBindTime());
//                            entity.setSubExpire(chargePointEntity.getOwnerSubExpire());
//                            entity.setSubStatus(chargePointEntity.getOwnerSubStatus());
//                            entity.setRelation(MerchantChargePointRelationEnum.OWNER.getKey());
//                            chargePointMerchantRelationMapper.insert(entity);
//                        }
//                    } catch (Exception e) {
//                        log.error("insert " + chargePointEntity.getSn(), e);
//                    }
//                    try {
//                        if (Objects.nonNull(chargePointEntity.getMaintenance())) {
//                            entity.setId(IdWorker.getId());
//                            entity.setMerchantId(chargePointEntity.getMaintenance());
//                            entity.setBindTime(chargePointEntity.getMaintenanceBindTime());
//                            entity.setSubExpire(chargePointEntity.getMaintenanceSubExpire());
//                            entity.setSubStatus(chargePointEntity.getMaintenanceSubStatus());
//                            entity.setRelation(MerchantChargePointRelationEnum.MAINTENANCE.getKey());
//                            chargePointMerchantRelationMapper.insert(entity);
//                        }
//                    } catch (Exception e) {
//                        log.error("insert " + chargePointEntity.getSn(), e);
//                    }
//                }
//                page.setCurrent(page.getCurrent() + 1);
//            }
//        } while (Objects.nonNull(page) && page.getCurrent() <= page.getPages());
//
//        return Result.ofSucceed();
//    }
}
