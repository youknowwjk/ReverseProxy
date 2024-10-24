package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.base.common.util.LocaleResultUtil;
import com.autel.cloud.base.common.util.ThreadPoolContext;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.repository.OpPowerLimitRepository;
import com.autel.cloud.pile.base.domain.repository.OpPowerLimitSettingRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.domain.service.OpPowerLimitService;
import com.autel.cloud.pile.base.dto.OpPowerLimitDTO;
import com.autel.cloud.pile.base.dto.OpPowerLimitSettingDTO;
import com.autel.cloud.pile.base.dto.PowerLimitItemDTO;
import com.autel.cloud.pile.base.dto.PowerLimitParamDTO;
import com.autel.cloud.pile.base.enums.ChargingProfilePurpose;
import com.autel.cloud.pile.base.enums.NumberPhasesEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitSettingEntity;
import com.autel.cloud.smart.charge.feign.SmartChargeFeign;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * @ClassName OpPowerLimitServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/7/2 11:01
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class OpPowerLimitServiceImpl implements OpPowerLimitService {

    private final OpPowerLimitRepository opPowerLimitRepository;

    private final OpPowerLimitSettingRepository opPowerLimitSettingRepository;

    private final OpLocationPileEvseElastic opLocationPileEvseElastic;

    private final SmartChargeFeign smartChargeFeign;
    @Resource
    private OpLocationService opLocationService;
    @Resource
    private OpLocationEvseService opLocationEvseService;

    public OpPowerLimitServiceImpl(OpPowerLimitRepository opPowerLimitRepository,
                                   OpPowerLimitSettingRepository opPowerLimitSettingRepository,
                                   OpLocationPileEvseElastic opLocationPileEvseElastic,
                                   SmartChargeFeign smartChargeFeign) {
        this.opPowerLimitRepository = opPowerLimitRepository;
        this.opPowerLimitSettingRepository = opPowerLimitSettingRepository;
        this.opLocationPileEvseElastic = opLocationPileEvseElastic;
        this.smartChargeFeign = smartChargeFeign;
    }

    @Override
    public Result<Boolean> updateTurnOn(Long combinationId) {
        if (combinationId == null || combinationId < 1) {
            return Result.ofFailed(LocaleResultUtil.result(BaseConstant.PARAM_ERROR));
        }
        OpPowerLimitEntity opPowerLimitEntity = getOpPowerLimitEntity(combinationId);
        if (opPowerLimitEntity == null) {
            opPowerLimitEntity = new OpPowerLimitEntity();
            opPowerLimitEntity.setTurnOn(1);
            opPowerLimitEntity.setCombinationId(combinationId);
            opPowerLimitEntity.setCreatedAt(System.currentTimeMillis());
            opPowerLimitEntity.setUpdatedAt(System.currentTimeMillis());
            return Result.ofSucceed(opPowerLimitRepository.save(opPowerLimitEntity));
        }
        opPowerLimitEntity.setUpdatedAt(System.currentTimeMillis());
        opPowerLimitEntity.setTurnOn(opPowerLimitEntity.getTurnOn() == 0 ? 1 : 0);
        boolean ret = opPowerLimitRepository.saveOrUpdate(opPowerLimitEntity);
        if (!ret) {
            return Result.ofFailed(LocaleResultUtil.result(BaseConstant.PARAM_ERROR));
        }
        int turnOn = opPowerLimitEntity.getTurnOn();
        ThreadPoolContext.execute(() -> {
            if (turnOn == 1) {
                // 清除配置然后重新下发
                this.deliverySetting(combinationId);
            } else {
                this.clearSetting(combinationId);
            }
        });
        return Result.ofSucceed(opPowerLimitEntity.getTurnOn() == 0 ? Boolean.FALSE : Boolean.TRUE);
    }

    @Override
    public Result<Boolean> updatePowerLimit(OpPowerLimitDTO opPowerLimitDTO) {
        if (opPowerLimitDTO == null || opPowerLimitDTO.getCombinationId() == null
                || opPowerLimitDTO.getCombinationId() < 1) {
            return Result.ofFailed(LocaleResultUtil.result(BaseConstant.PARAM_ERROR));
        }
        OpPowerLimitEntity opPowerLimitEntity = getOpPowerLimitEntity(opPowerLimitDTO.getCombinationId());
        if (opPowerLimitEntity == null) {
            opPowerLimitEntity = new OpPowerLimitEntity();
            opPowerLimitEntity.setTurnOn(0);
            opPowerLimitEntity.setCombinationId(opPowerLimitDTO.getCombinationId());
            opPowerLimitEntity.setCreatedAt(System.currentTimeMillis());
            opPowerLimitEntity.setUpdatedAt(System.currentTimeMillis());
        }
        opPowerLimitEntity.setUpdatedAt(System.currentTimeMillis());
        if (opPowerLimitDTO.getAdvancedSetting() != null) {
            opPowerLimitEntity.setAdvancedSetting(opPowerLimitDTO.getAdvancedSetting());
        }
        if (opPowerLimitDTO.getLongTermEffective() != null) {
            opPowerLimitEntity.setLongTermEffective(opPowerLimitDTO.getLongTermEffective());
        }
        if (opPowerLimitDTO.getEffectiveStartDate() != null) {
            opPowerLimitEntity.setEffectiveStartDate(opPowerLimitDTO.getEffectiveStartDate());
        }
        if (opPowerLimitDTO.getEffectiveEndDate() != null) {
            opPowerLimitEntity.setEffectiveEndDate(opPowerLimitDTO.getEffectiveEndDate());
        }
        opPowerLimitEntity.setTurnOn(opPowerLimitDTO.getTurnOn());
        boolean ret = opPowerLimitRepository.saveOrUpdate(opPowerLimitEntity);
        Integer turnOn = opPowerLimitDTO.getTurnOn();
        Long combinationId = opPowerLimitDTO.getCombinationId();
        if (ret) {
            if (turnOn == 1) {
                // 清除配置然后重新下发
                this.deliverySetting(combinationId);
            } else {
                this.clearSetting(combinationId);
            }
        }
        return Result.ofSucceed(ret);
    }

    @Override
//    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> addTimeSetting(List<OpPowerLimitSettingDTO> opPowerLimitSettingDTOList) {
        if (CollectionUtils.isEmpty(opPowerLimitSettingDTOList)) {
            return Result.ofFailed(LocaleResultUtil.result(BaseConstant.PARAM_ERROR));
        }
        Long combinationId = opPowerLimitSettingDTOList.get(0).getCombinationId();
        LambdaQueryWrapper<OpPowerLimitSettingEntity> queryWrapper = Wrappers.lambdaQuery(OpPowerLimitSettingEntity.class);
        queryWrapper.eq(OpPowerLimitSettingEntity::getCombinationId, combinationId);
        opPowerLimitSettingRepository.remove(queryWrapper);
        opPowerLimitSettingDTOList.forEach(opPowerLimitSettingDTO -> {
            OpPowerLimitSettingEntity opPowerLimitSettingEntity = toOpPowerLimitSettingEntity(opPowerLimitSettingDTO);
            opPowerLimitSettingRepository.saveOrUpdate(opPowerLimitSettingEntity);
        });
        // 清除配置然后重新下发
        this.deliverySetting(combinationId);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<Boolean> getTurnOn(Long combinationId) {
        if (combinationId == null || combinationId < 1) {
            return Result.ofFailed(LocaleResultUtil.result(BaseConstant.PARAM_ERROR));
        }
        OpPowerLimitEntity opPowerLimitEntity = getOpPowerLimitEntity(combinationId);
        if (opPowerLimitEntity == null) {
            return Result.ofSucceed(Boolean.FALSE);
        }
        return Result.ofSucceed(opPowerLimitEntity.getTurnOn() == 0 ? Boolean.FALSE : Boolean.TRUE);
    }

    @Override
    public Result<OpPowerLimitDTO> queryPowerLimit(Long combinationId) {
        if (combinationId == null || combinationId < 1) {
            return Result.ofFailed(LocaleResultUtil.result(BaseConstant.PARAM_ERROR));
        }
        OpPowerLimitEntity opPowerLimitEntity = getOpPowerLimitEntity(combinationId);
        if (opPowerLimitEntity == null) {
            return Result.ofSucceed(null);
        }
        return Result.ofSucceed(toOpPowerLimitDTO(opPowerLimitEntity));
    }

    private static OpPowerLimitDTO toOpPowerLimitDTO(OpPowerLimitEntity entity) {
        OpPowerLimitDTO dto = new OpPowerLimitDTO();
        BeanUtils.copyProperties(entity, dto);
        return dto;
    }

    private OpPowerLimitEntity getOpPowerLimitEntity(Long combinationId) {
        LambdaQueryWrapper<OpPowerLimitEntity> queryWrapper = Wrappers.lambdaQuery(OpPowerLimitEntity.class);
        queryWrapper.eq(OpPowerLimitEntity::getDeleted, Boolean.FALSE);
        queryWrapper.eq(OpPowerLimitEntity::getCombinationId, combinationId);
        return opPowerLimitRepository.getOne(queryWrapper);
    }

    @Override
    public Result<List<OpPowerLimitSettingDTO>> queryTimeSettingForUpdate(Long combinationId) {
        List<OpPowerLimitSettingEntity> retList = opPowerLimitSettingEntityList(combinationId);
        if (CollectionUtils.isEmpty(retList)) {
            return Result.ofSucceed(Collections.emptyList());
        }
        return Result.ofSucceed(retList.stream()
                .map(OpPowerLimitServiceImpl::toOpPowerLimitSettingDTO).collect(Collectors.toList()));
    }

    private List<OpPowerLimitSettingEntity> opPowerLimitSettingEntityList(Long combinationId) {
        LambdaQueryWrapper<OpPowerLimitSettingEntity> queryWrapper = Wrappers.lambdaQuery(OpPowerLimitSettingEntity.class);
        queryWrapper.eq(OpPowerLimitSettingEntity::getDeleted, Boolean.FALSE);
        queryWrapper.eq(OpPowerLimitSettingEntity::getCombinationId, combinationId);
        return opPowerLimitSettingRepository.list(queryWrapper);
    }

    /**
     * 转化
     *
     * @param entity
     * @return
     */
    private static OpPowerLimitSettingDTO toOpPowerLimitSettingDTO(OpPowerLimitSettingEntity entity) {
        OpPowerLimitSettingDTO dto = new OpPowerLimitSettingDTO();
        BeanUtils.copyProperties(entity, dto);
        return dto;
    }

    /**
     * 转化
     *
     * @param dto
     * @return
     */
    private static OpPowerLimitSettingEntity toOpPowerLimitSettingEntity(OpPowerLimitSettingDTO dto) {
        OpPowerLimitSettingEntity entity = new OpPowerLimitSettingEntity();
        BeanUtils.copyProperties(dto, entity);
        entity.setCreatedAt(System.currentTimeMillis());
        entity.setUpdatedAt(System.currentTimeMillis());
        JSONArray jsonArray = JSON.parseArray(entity.getWeekDay());
        entity.setWeekDay(JSON.toJSONString(jsonArray));
        return entity;
    }

    @Override
    public Result<List<OpPowerLimitSettingDTO>> queryTimeSetting(Long combinationId) {
        return queryTimeSettingForUpdate(combinationId);
    }

    @Override
    public Result<Boolean> deliverySetting(Long combinationId) {
        try {
            // 获取配置
            OpPowerLimitEntity opPowerLimitEntity = getOpPowerLimitEntity(combinationId);
            if (opPowerLimitEntity == null) {
                return Result.ofFailed(LocaleResultUtil.result(BaseConstant.DATA_IS_NULL));
            }
            if (opPowerLimitEntity.getTurnOn() != 1) {
                // 不开启功率设置
                return Result.ofFailed(LocaleResultUtil.result("power.limit.turnOnPowerSetting"));
            }
            Optional<OpLocationPileEvseElasticDTO> optional = opLocationPileEvseElastic.findById(combinationId);
            if (!optional.isPresent()) {
                return Result.ofFailed(LocaleResultUtil.result(BaseConstant.DATA_IS_NULL));
            }
            //功率为空取枪功率
            OpLocationPileEvseElasticDTO pileDto = optional.get();
            if (pileDto.getPower() == null) {
                List<OpLocationEvseElasticDTO> evseList = opLocationEvseService.findListByPileSn(pileDto.getPileSn());
                if (!CollectionUtils.isEmpty(evseList)) {
                    OpLocationEvseElasticDTO evseDto = evseList.get(0);
                    if (evseDto.getPower() != null) {
                        pileDto.setPower(evseDto.getPower());
                    } else {
                        log.info("OpPowerLimitServiceImpl.deliverySetting,power is null pileDto={}", JSON.toJSONString(pileDto));
                    }
                }
            }
            //查询时区ID
            String zoneId = this.getZoneId(pileDto.getLocationId());

            List<OpPowerLimitSettingEntity> retList = opPowerLimitSettingEntityList(combinationId);
            if (CollectionUtils.isEmpty(retList)) {
                return Result.ofFailed(LocaleResultUtil.result(BaseConstant.DATA_IS_NULL));
            }
            log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_DELIVERYSETTING_PILESN + pileDto.getPileSn() + " OpPowerLimitSettingEntityList = " + JSON.toJSONString(retList));
            if (opPowerLimitEntity.getAdvancedSetting() == 0) {
                // 不开启自定义时间功率设置 从头到位都是同一个功率
                retList = retList.stream()
                        .filter(item -> item.getWeekDay().equals(JSON.toJSONString(Arrays.asList(0, 1, 2, 3, 4, 5, 6))))
                        .collect(Collectors.toList());
                if (CollectionUtils.isEmpty(retList)) {
                    return Result.ofFailed(LocaleResultUtil.result(BaseConstant.DATA_IS_NULL));
                }
                OpPowerLimitSettingEntity opPowerLimitSettingEntity = retList.get(0);
                log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_DELIVERYSETTING_PILESN + pileDto.getPileSn() + " opPowerLimitSettingEntity = " + JSON.toJSONString(opPowerLimitSettingEntity));

                PowerLimitParamDTO powerLimitParamDTO = buildPowerLimitParamDTOForDelivery(opPowerLimitEntity,pileDto, Collections.singletonList(opPowerLimitSettingEntity),zoneId);
                log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_DELIVERYSETTING_PILESN + pileDto.getPileSn() + " powerLimitParamDTO = " + JSON.toJSONString(powerLimitParamDTO));

                if (powerLimitParamDTO != null) {
                    return smartChargeFeign.delivery(powerLimitParamDTO);
                }
            } else {
                Boolean isUse = Boolean.TRUE;
                long now = System.currentTimeMillis();
                // 开启自定义有效期
                if (opPowerLimitEntity.getLongTermEffective() == 1) {
                    // 非长期有效
                    if (opPowerLimitEntity.getEffectiveStartDate() > now
                            || opPowerLimitEntity.getEffectiveEndDate() < now) {
                        // 不在有效期内
                        isUse = Boolean.FALSE;
                    }
                }
                if (Boolean.TRUE.equals(isUse)) {
                    // 获取当前的配置
                    int weekday = LocalDateTime.now(ZoneId.of(zoneId)).getDayOfWeek().getValue();
                    if (weekday == 7) {
                        weekday = 0;
                    }
                    int finalWeekday = weekday;
                    retList = retList.stream()
                            .filter(item -> item.getWeekDay().contains(String.valueOf(finalWeekday)))
                            .collect(Collectors.toList());
                } else {
                    retList = Collections.emptyList();
                }
                log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_DELIVERYSETTING_PILESN + pileDto.getPileSn() + " OpPowerLimitSettingEntityList = " + JSON.toJSONString(retList));
                PowerLimitParamDTO powerLimitParamDTO = buildPowerLimitParamDTOForDelivery(opPowerLimitEntity,pileDto, retList,zoneId);
                log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_DELIVERYSETTING_PILESN + pileDto.getPileSn() + " powerLimitParamDTO = " + JSON.toJSONString(powerLimitParamDTO));
                if (powerLimitParamDTO != null) {
                    return smartChargeFeign.delivery(powerLimitParamDTO);
                }
            }
            return Result.ofSucceed(Boolean.FALSE);
        } catch (Exception e) {
            log.info("OpPowerLimitServiceImpl.deliverySetting Exception = ", e);
            return Result.ofSucceed(Boolean.FALSE);
        }
    }

    private String getZoneId(Long locationId){
        return opLocationService.getZoneId(locationId);
    }

    @Override
    public Result<Boolean> clearSetting(Long combinationId) {
        try {
            // 获取配置
            Optional<OpLocationPileEvseElasticDTO> optional = opLocationPileEvseElastic.findById(combinationId);
            if (!optional.isPresent()) {
                return Result.ofFailed(LocaleResultUtil.result(BaseConstant.DATA_IS_NULL));
            }
            PowerLimitParamDTO powerLimitParamDTO = buildPowerLimitParamDTOForClear(optional.get());
            log.info("OpPowerLimitServiceImpl.clearSetting powerLimitParamDTO = " + JSON.toJSONString(powerLimitParamDTO));
            if (powerLimitParamDTO != null) {
                return smartChargeFeign.clear(powerLimitParamDTO);
            }
            return Result.ofSucceed(Boolean.FALSE);
        } catch (Exception e) {
            log.info("OpPowerLimitServiceImpl.clearSetting Exception = ", e);
            return Result.ofSucceed(Boolean.FALSE);
        }
    }

    @Override
    public List<OpPowerLimitEntity> findAllTurnOn() {
        return opPowerLimitRepository.findAllTurnOn();
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    static class SettingDTO {
        private Integer start;
        private Integer end;
        private BigDecimal power;
        private String powerUnit;
        private String timeZone;
    }

    private static PowerLimitParamDTO buildPowerLimitParamDTOForClear(OpLocationPileEvseElasticDTO pileEvse) {
        return PowerLimitParamDTO.builder()
                .pileSn(pileEvse.getPileSn())
                .connectorId(0)
                .chargingProfileId(5)
                .chargingRateUnit("W")
                .stackLevel(10)
                .chargingProfilePurpose(ChargingProfilePurpose.CHARGE_POINT_MAX_PROFILE.getValue())
                .chargingProfileKind("Absolute")
                .duration(24 * 60 * 60)
                .build();
    }

    private PowerLimitParamDTO buildPowerLimitParamDTOForDelivery(OpPowerLimitEntity opPowerLimitEntity,OpLocationPileEvseElasticDTO pileEvse, List<OpPowerLimitSettingEntity> settingEntityList,String zoneId) {
        log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_BUILDPOWERLIMITPARAMDTODELIVERY_PILSAN + pileEvse.getPileSn() + " settingEntityList = " + JSON.toJSONString(settingEntityList));
        BigDecimal defaultPower = BigDecimal.valueOf(pileEvse.getPower());
        List<OpPowerLimitSettingEntity> retList = opPowerLimitSettingEntityList(pileEvse.getId());
        if (CollectionUtils.isEmpty(settingEntityList) && !CollectionUtils.isEmpty(retList)) {
            settingEntityList = new ArrayList<>();
            OpPowerLimitSettingEntity settingEntity = new OpPowerLimitSettingEntity();
            BeanUtils.copyProperties(retList.get(0), settingEntity);
            settingEntity.setPower(defaultPower);
            settingEntity.setStartTime("00:00");
            settingEntity.setEndTime("24:00");
            settingEntity.setPowerUnit("KW");
            settingEntityList.add(settingEntity);
        }
        List<PowerLimitItemDTO> powerLimitItemDTOList = new ArrayList<>();
        settingEntityList = settingEntityList.stream()
                .sorted((item1, item2) -> {
                    int start1 = Integer.parseInt(item1.getStartTime().split(":")[0]) * 60 * 60
                            + Integer.parseInt(item1.getStartTime().split(":")[1]) * 60;
                    int start2 = Integer.parseInt(item2.getStartTime().split(":")[0]) * 60 * 60
                            + Integer.parseInt(item2.getStartTime().split(":")[1]) * 60;
                    return start1 - start2;
                }).collect(Collectors.toList());
        log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_BUILDPOWERLIMITPARAMDTODELIVERY_PILSAN + pileEvse.getPileSn() + " settingEntityList = " + JSON.toJSONString(settingEntityList));
        List<SettingDTO> settingDTOS = new ArrayList<>();
        AtomicInteger index = new AtomicInteger(0);
        settingEntityList.forEach(item -> {
            int start = Integer.parseInt(item.getStartTime().split(":")[0]) * 60 * 60
                    + Integer.parseInt(item.getStartTime().split(":")[1]) * 60;
            int end = Integer.parseInt(item.getEndTime().split(":")[0]) * 60 * 60
                    + Integer.parseInt(item.getEndTime().split(":")[1]) * 60;
            if (end == 23 * 60 * 60 + 59 * 60) {
                end = 24 * 60 * 60;
            }
            settingDTOS.add(SettingDTO.builder().start(start).end(end).power(item.getPower())
                    .powerUnit(item.getPowerUnit()).timeZone(item.getTimeZone()).build());
            index.incrementAndGet();
        });
        log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_BUILDPOWERLIMITPARAMDTODELIVERY_PILSAN + pileEvse.getPileSn() + " settingDTOS = " + JSON.toJSONString(settingDTOS));

        if (CollectionUtils.isEmpty(settingDTOS)) {
            return null;
        }
        List<SettingDTO> settingRetDTOS = new ArrayList<>();
        AtomicInteger lastStart = new AtomicInteger(0);
        settingDTOS.forEach(setting -> {
            if (setting.getStart() > lastStart.get()) {
                settingRetDTOS.add(SettingDTO.builder().start(lastStart.get()).end(setting.getStart()).power(defaultPower)
                        .powerUnit(setting.getPowerUnit()).build());
            }
            settingRetDTOS.add(SettingDTO.builder().start(setting.getStart()).end(setting.getEnd()).power(setting.getPower())
                    .powerUnit(setting.getPowerUnit()).build());
            lastStart.set(setting.getEnd());
        });
        if (lastStart.get() < 24 * 60 * 60) {
            settingRetDTOS.add(SettingDTO.builder().start(lastStart.get()).end(24 * 60 * 60).power(defaultPower)
                    .powerUnit(settingDTOS.get(0).getPowerUnit()).build());
        }
        log.info(BaseConstant.OPPOWERLIMIT_SERVICEIMPL_BUILDPOWERLIMITPARAMDTODELIVERY_PILSAN + pileEvse.getPileSn() + " settingRetDTOS = " + JSON.toJSONString(settingRetDTOS));

        if (CollectionUtils.isEmpty(settingRetDTOS)) {
            return null;
        }

        settingRetDTOS.forEach(setting -> powerLimitItemDTOList.add(PowerLimitItemDTO.builder().startDuration(setting.getStart()).power(setting.getPower())
                .powerUnit(setting.getPowerUnit().toUpperCase(Locale.ROOT)).build()));
        String phaseNum = NumberPhasesEnum.getPhaseNum(pileEvse.getPowerType());
        //是否开启自定义
        Integer advancedSetting = opPowerLimitEntity.getAdvancedSetting();
        //是否长期有效
        Integer longTermEffective = opPowerLimitEntity.getLongTermEffective();
        String to = null;
        String recurrencyKind = null;
        String chargingProfileKind = "Absolute";
        if (advancedSetting == 1) {
            if (longTermEffective == 0) {
                to = getNowDayTimeBehindStart(-365, zoneId);
                recurrencyKind = "Daily";
                chargingProfileKind = "Recurring";
            } else {
                to = getNowDayTimeBehindStart(-1, zoneId);
            }
        } else {
            to = getNowDayTimeBehindStart(-365, zoneId);
            recurrencyKind = "Daily";
            chargingProfileKind = "Recurring";
        }
        return PowerLimitParamDTO.builder()
                .pileSn(pileEvse.getPileSn())
                .connectorId(0)
                .chargingProfileId(5)
                .chargingRateUnit("W")
                .stackLevel(10)
                .chargingProfilePurpose(ChargingProfilePurpose.CHARGE_POINT_MAX_PROFILE.getValue())
                .chargingProfileKind(chargingProfileKind)
                .recurrencyKind(recurrencyKind)
                .validFrom(getNowDayTimeBehindStart(0, zoneId))
                .validTo(to)
                .phaseNum(phaseNum)
                .startSchedule(getNowDayTimeBehindStart(0, zoneId))//获取0时区的一天开始时间 然后算上客户时区
                .duration(86400)
                .powerLimitItemDTOList(powerLimitItemDTOList)
                .build();
    }

    public static String getNowDayTimeBehindStart(int n, String zoneId) {
        LocalDateTime localDatetimeStart = LocalDateTime.now(ZoneId.of(zoneId)).minusDays(n).withHour(0).withMinute(0).withSecond(0).withNano(0);
        LocalDateTime start = localDatetimeStart.atZone(ZoneId.of(zoneId)).withZoneSameInstant(ZoneOffset.of("+0")).toLocalDateTime();
        return start.format(DateTimeFormatter.ofPattern(DEFAULT_LONG_PATTERN));
    }

    public static final String DEFAULT_LONG_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
}
