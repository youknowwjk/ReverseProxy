package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.home.feign.UserVehicleVO;
import com.autel.cloud.pile.base.SmartChargeProfile;
import com.autel.cloud.pile.base.domain.service.MemberSmartChargeConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.DeliveryByUserDTO;
import com.autel.cloud.pile.base.dto.SmartChargeConfigQueryDTO;
import com.autel.cloud.pile.base.dto.SmartChargeProfileDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileHomeServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileMonitorServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleJobMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.MemberSmartChargeConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.MemberSmartChargeConfigEntity;
import com.autel.cloud.pile.base.vo.UserVehicle;
import com.autel.cloud.pile.bill.dto.ChargingScheduleTaskDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Executor;

@Slf4j
@Service
public class MemberSmartChargeConfigServiceImpl implements MemberSmartChargeConfigService {

    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    @Resource
    private PileMonitorServiceAdapter pileMonitorServiceAdapter;

    @Resource
    private MemberSmartChargeConfigMapper memberSmartChargeConfigMapper;

    @Resource
    private PileHomeServiceAdapter pileHomeServiceAdapter;

    @Resource
    private IntelligentChargeScheduleJobMapper intelligentChargeScheduleJobMapper;

    @Resource
    private ApplicationEventPublisher applicationEventPublisher;


    @Resource(name = "schedulingChargeTaskExecutor")
    private Executor schedulingChargeTaskExecutor;

    @Override
    public int queryEnabledSmartChargeConfigByUserId(Long userId) {
        LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class);
        queryWrapper.eq(MemberSmartChargeConfigEntity::getUserId, userId).eq(MemberSmartChargeConfigEntity::getStatus, 1);
        List<MemberSmartChargeConfigEntity> list = memberSmartChargeConfigMapper.selectList(queryWrapper);
        log.info("queryEnableSmartChargeConfigByUserId {}", JSON.toJSONString(list));
        return list.size();
    }

    @Override
    public int disabledSmartChargeConfigByUserId(Long userId) {
        LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class);
        queryWrapper.eq(MemberSmartChargeConfigEntity::getUserId, userId).ne(MemberSmartChargeConfigEntity::getStatus, 0);
        List<MemberSmartChargeConfigEntity> list = memberSmartChargeConfigMapper.selectList(queryWrapper);
        for (MemberSmartChargeConfigEntity entity : list) {
            entity.setStatus(0);
            entity.setProfile(null);
            log.info("2 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(entity));
            memberSmartChargeConfigMapper.updateById(entity);
        }
        log.info("disabledSmartChargeConfigByUserId {}", JSON.toJSONString(list));
        return list.size();
    }


    @Override
    public int update(MemberSmartChargeConfigEntity intelligentChargeConfig) {
        log.info("3 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(intelligentChargeConfig));
        return memberSmartChargeConfigMapper.updateById(intelligentChargeConfig);
    }

    private Optional<UserVehicleVO> obtainUserMainVehicle(Long userId) {
        List<UserVehicleVO> userVehicles = pileHomeServiceAdapter.queryUserVehicles(userId);
        if (!CollectionUtils.isEmpty(userVehicles)) {
            return userVehicles.stream().filter(target -> Boolean.TRUE.equals(target.getMainVehicle())).findFirst();
        }
        return Optional.empty();
    }

    @Override
    public MemberSmartChargeConfigEntity createDefault(SmartChargeConfigQueryDTO smartChargeConfigQueryDTO) {
        log.info("用户在此场站没有配置过 {}", JSON.toJSONString(smartChargeConfigQueryDTO));
        long userId = smartChargeConfigQueryDTO.getUserId();
        long locationId = smartChargeConfigQueryDTO.getLocationId();
        MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = new MemberSmartChargeConfigEntity();
        memberSmartChargeConfigEntity.setStatus(0);//  首次创建的时候默认关闭 产品 楼阁 决定的
        memberSmartChargeConfigEntity.setId(IdWorker.getId());
        memberSmartChargeConfigEntity.setCreateTime(System.currentTimeMillis());
        memberSmartChargeConfigEntity.setUpdateTime(System.currentTimeMillis());
        memberSmartChargeConfigEntity.setUserId(userId);
        memberSmartChargeConfigEntity.setLocationId(locationId);

        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryWrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class);
        queryWrapper.eq(IntelligentChargeScheduleJob::getUserId, userId).eq(IntelligentChargeScheduleJob::getOrderSeq, smartChargeConfigQueryDTO.getTransactionId());
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectOne(queryWrapper);

        SmartChargeProfile profile = new SmartChargeProfile();
        profile.setId(memberSmartChargeConfigEntity.getId());
        if (Objects.nonNull(intelligentChargeScheduleJob) && Objects.nonNull(intelligentChargeScheduleJob.getVehicleId())) {
            // 代表是识别出车辆
            profile.setVehicleId(intelligentChargeScheduleJob.getVehicleId());
            profile.setFixedVehicle(1);
        } else {
            // 默认 用户主车辆信息
            obtainUserMainVehicle(smartChargeConfigQueryDTO.getUserId()).ifPresent(userVehicle -> {
                if (!StringUtils.hasText(userVehicle.getBatteryCapacity())) {
                    throw new MessageCodeException(PileBaseEnum.SET_BATTERY_CAPACITY_FOR_THE_VEHICLE_FIRST);
                }
                profile.setMake(userVehicle.getMake());
                profile.setName(userVehicle.getName());
                profile.setYear(userVehicle.getYear());
                profile.setModel(userVehicle.getModel());
                profile.setEdition(userVehicle.getEdition());
                profile.setBatteryCapacity(userVehicle.getBatteryCapacity());
                profile.setVehicleId(userVehicle.getId());
                profile.setId(memberSmartChargeConfigEntity.getId());
                log.info("4 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(memberSmartChargeConfigEntity));
                log.info("update default main vehicle result {}", memberSmartChargeConfigMapper.updateById(memberSmartChargeConfigEntity));
            });
        }
        memberSmartChargeConfigEntity.setProfile(profile);
        memberSmartChargeConfigMapper.insert(memberSmartChargeConfigEntity);
        return memberSmartChargeConfigEntity;
    }

    @Override
    public MemberSmartChargeConfigEntity updateById(SmartChargeProfileDTO profileDTO) {

        MemberSmartChargeConfigEntity exist = memberSmartChargeConfigMapper.selectById(profileDTO.getId());
        if (Objects.isNull(exist)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        if (Objects.nonNull(profileDTO.getVehicleId())) {
            UserVehicle vehicle = pileHomeServiceAdapter.selectById(profileDTO.getVehicleId());
            if (Objects.isNull(vehicle) || org.apache.commons.lang3.StringUtils.isBlank(vehicle.getBatteryCapacity())) {
                log.error("请先为车辆设置标称电池容量 profileDTO {}", JSON.toJSONString(profileDTO));
                throw new MessageCodeException(PileBaseEnum.SET_BATTERY_CAPACITY_FOR_THE_VEHICLE_FIRST);
            }
        }

        Optional.ofNullable(profileDTO.getLocationId()).ifPresent(exist::setLocationId);
        Optional.ofNullable(profileDTO.getStatus()).ifPresent(exist::setStatus);
        SmartChargeProfile profile = exist.getProfile();
        Optional.ofNullable(profileDTO.getDriveTime()).ifPresent(profile::setDriveTime);
        Optional.ofNullable(profileDTO.getInitialTime()).ifPresent(profile::setInitialTime);
        Optional.ofNullable(profileDTO.getId()).ifPresent(profile::setId);
        Optional.ofNullable(profileDTO.getStatus()).ifPresent(profile::setStatus);


        Optional.ofNullable(profileDTO.getMake()).ifPresent(profile::setMake);
        Optional.ofNullable(profileDTO.getEdition()).ifPresent(profile::setEdition);
        Optional.ofNullable(profileDTO.getVehicleId()).ifPresent(profile::setVehicleId);
        Optional.ofNullable(profileDTO.getModel()).ifPresent(profile::setModel);
        Optional.ofNullable(profileDTO.getYear()).ifPresent(profile::setYear);

        Optional.ofNullable(profileDTO.getBatteryCare()).ifPresent(profile::setBatteryCare);
        Optional.ofNullable(profileDTO.getStartSOC()).ifPresent(profile::setStartSOC);
        Optional.ofNullable(profileDTO.getTargetSOC()).ifPresent(profile::setTargetSOC);
        Optional.ofNullable(profileDTO.getMinSOC()).ifPresent(profile::setMinSOC);

        exist.setUpdateTime(System.currentTimeMillis());
        log.info("5 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(exist));
        if (memberSmartChargeConfigMapper.updateById(exist) <= 0) {
            throw new MessageCodeException(PileBaseEnum.ADD_MEMBER_CUSTOMER_FAIL);
        }
        schedulingChargeTaskExecutor.execute(RunnableWrapper.of(() -> tryReschedule(selectById(exist.getId()))));
        return exist;
    }


    private void tryReschedule(MemberSmartChargeConfigEntity memberSmartChargeConfigEntity) {
        log.info("tryReschedule MemberSmartChargeConfigEntity: {}", JSON.toJSONString(memberSmartChargeConfigEntity));
        applicationEventPublisher.publishEvent(memberSmartChargeConfigEntity);
    }

    @Override
    public MemberSmartChargeConfigEntity selectById(long id) {
        MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = memberSmartChargeConfigMapper.selectById(id);
        log.info("selectById: {}", JSON.toJSONString(memberSmartChargeConfigEntity));
        return memberSmartChargeConfigEntity;
    }


    @Override
    public List<DeliveryByUserDTO> batchByUserIdAndLocationId(List<SmartChargeConfigQueryDTO> smartChargeConfigQueryDTO) {
        log.info("batchByUserIdAndLocationId {}", JSON.toJSONString(smartChargeConfigQueryDTO));
        List<DeliveryByUserDTO> result = new ArrayList<>();
        for (SmartChargeConfigQueryDTO query : smartChargeConfigQueryDTO) {
            DeliveryByUserDTO deliveryByUserDTO = new DeliveryByUserDTO();
            try {
                MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = queryByUserIdAndLocationId(query);
                SmartChargeProfile profile = memberSmartChargeConfigEntity.getProfile();
                IntelligentChargeScheduleJob scheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.parseLong(query.getTransactionId()));
                if (Objects.isNull(scheduleJob)) {
                    log.warn("调度job 还没有来的及创建 先返回快充配置 {}", JSON.toJSONString(query));
                    deliveryByUserDTO.setFast(1);
                    deliveryByUserDTO.setEvseSn(query.getEvseSn());
                    deliveryByUserDTO.setUserId(query.getUserId());
                    deliveryByUserDTO.setLocationId(query.getLocationId());
                    result.add(deliveryByUserDTO);
                    continue;
                }
                String evseSn = scheduleJob.getEvseSn();
                deliveryByUserDTO.setEvseSn(evseSn);
                Long userId = scheduleJob.getUserId();
                deliveryByUserDTO.setUserId(userId);
                // 开始充电时间
                String openingZonedTime = scheduleJob.getOpeningZonedTime();
                LocalDateTime startTime = LocalDateTime.parse(openingZonedTime, formatter);
                ZoneId zoneId = ZoneId.of(scheduleJob.getZoneId());

                LocalTime driveTime = profile.getDriveTime();
                LocalDateTime localDateTime = driveTime.atDate(LocalDate.now(zoneId));
                if (localDateTime.isAfter(startTime)) {
                    long milli = localDateTime.atZone(zoneId).toInstant().toEpochMilli();
                    deliveryByUserDTO.setDriveTimestamp(milli);
                } else {
                    long milli = localDateTime.plusDays(1).atZone(zoneId).toInstant().toEpochMilli();
                    deliveryByUserDTO.setDriveTimestamp(milli);
                }
                int smartChargeSwitch = scheduleJob.getSmartChargeSwitch();
                deliveryByUserDTO.setFast(smartChargeSwitch == 1 ? 0 : 1);
                deliveryByUserDTO.setStartSOC(profile.getStartSOC());
                // 大概率的获取不到当前soc
                deliveryByUserDTO.setCurrentSOC(Optional.ofNullable(pileMonitorServiceAdapter.readSOCFromMonitorCache(scheduleJob)).orElse(profile.getStartSOC()));
                deliveryByUserDTO.setTargetSOC(profile.getTargetSOC());
                deliveryByUserDTO.setMinSOC(profile.getMinSOC());

                deliveryByUserDTO.setBatteryCare(profile.getBatteryCare());
                deliveryByUserDTO.setBatteryCapacity(new BigDecimal(profile.getBatteryCapacity()));

                deliveryByUserDTO.setOrderSeq(scheduleJob.getOrderSeq());
                deliveryByUserDTO.setLocationId(scheduleJob.getLocationId());
                String orderNumber = scheduleJob.getOrderNumber();
                deliveryByUserDTO.setOrderNumber(orderNumber);
                result.add(deliveryByUserDTO);

            } catch (Exception e) {
                // 异常之后  给默认值 不影响其他桩下发
                log.error("batchByUserIdAndLocationId {}" + JSON.toJSONString(query), e);
                deliveryByUserDTO.setFast(1);
                deliveryByUserDTO.setEvseSn(query.getEvseSn());
                deliveryByUserDTO.setUserId(query.getUserId());
                deliveryByUserDTO.setLocationId(query.getLocationId());
                result.add(deliveryByUserDTO);
            }
        }
        return result;
    }

    /**
     * 启动充电 创建了 智能充电配置之后调用
     *
     * @param smartChargeConfigQueryDTO
     * @return
     */
    @Override
    public MemberSmartChargeConfigEntity queryByUserIdAndLocationId(SmartChargeConfigQueryDTO smartChargeConfigQueryDTO) {
        Long userId = smartChargeConfigQueryDTO.getUserId();
        Long locationId = smartChargeConfigQueryDTO.getLocationId();
        LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class);
        queryWrapper.eq(MemberSmartChargeConfigEntity::getUserId, userId).eq(MemberSmartChargeConfigEntity::getLocationId, locationId);
        MemberSmartChargeConfigEntity entity = memberSmartChargeConfigMapper.selectOne(queryWrapper);
        SmartChargeProfile profile = entity.getProfile();
        Assert.notNull(entity, " entity must not null " + JSON.toJSONString(smartChargeConfigQueryDTO));
        Assert.notNull(profile, " profile must not null " + JSON.toJSONString(smartChargeConfigQueryDTO));

        // 先看看有没有 车辆id
        profile.setBatteryCapacity(null);
        Long vehicleId = profile.getVehicleId();
        UserVehicle vehicle = pileHomeServiceAdapter.selectById(vehicleId);
        if (Objects.nonNull(vehicle)) {
            profile.setMake(vehicle.getMake());
            profile.setName(vehicle.getName());
            profile.setYear(vehicle.getYear());
            profile.setModel(vehicle.getModel());
            profile.setEdition(vehicle.getEdition());
            profile.setBatteryCapacity(vehicle.getBatteryCapacity());
            profile.setVehicleId(vehicle.getId());
            profile.setId(entity.getId());
            log.info("10 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(entity));
            log.info("10 update  vehicle result {}", memberSmartChargeConfigMapper.updateById(entity));
        } else {
            Optional<UserVehicleVO> userVehicleVO = obtainUserMainVehicle(smartChargeConfigQueryDTO.getUserId());
            if (userVehicleVO.isPresent()) {
                UserVehicleVO userVehicle = userVehicleVO.get();
                profile.setMake(userVehicle.getMake());
                profile.setName(userVehicle.getName());
                profile.setYear(userVehicle.getYear());
                profile.setModel(userVehicle.getModel());
                profile.setEdition(userVehicle.getEdition());
                profile.setBatteryCapacity(userVehicle.getBatteryCapacity());
                profile.setVehicleId(userVehicle.getId());
                profile.setFixedVehicle(0);
                profile.setId(entity.getId());
                log.info("15 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(entity));
                log.info("15 update default main vehicle result {}", memberSmartChargeConfigMapper.updateById(entity));
            } else {
                profile.setMake(null);
                profile.setName(null);
                profile.setYear(null);
                profile.setModel(null);
                profile.setEdition(null);
                profile.setBatteryCapacity(null);
                profile.setVehicleId(null);
                profile.setFixedVehicle(0);
                profile.setId(entity.getId());
            }
        }
        if (!StringUtils.hasText(profile.getBatteryCapacity())) {
            log.error("请先为车辆设置标称电池容量 result {}", JSON.toJSONString(smartChargeConfigQueryDTO));
            throw new MessageCodeException(PileBaseEnum.SET_BATTERY_CAPACITY_FOR_THE_VEHICLE_FIRST);
        }
        return entity;
    }

    /**
     * 启动充电完成的时候 生成一次
     * 开启智能充电的时候 重新生成一次
     *
     * @param chargingScheduleTaskDTO 用户 场站 桩 事务ID 信息
     * @return
     */
    @Override
    public MemberSmartChargeConfigEntity genConfigByUserIdAndLocationId(ChargingScheduleTaskDTO chargingScheduleTaskDTO) {

        Long userId = chargingScheduleTaskDTO.getUserId();
        Long locationId = chargingScheduleTaskDTO.getLocationId();
        SmartChargeConfigQueryDTO smartChargeConfigQueryDTO = new SmartChargeConfigQueryDTO();
        smartChargeConfigQueryDTO.setLocationId(chargingScheduleTaskDTO.getLocationId());
        smartChargeConfigQueryDTO.setTransactionId(chargingScheduleTaskDTO.getOrderSeq());
        smartChargeConfigQueryDTO.setUserId(chargingScheduleTaskDTO.getUserId());
        LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class);
        queryWrapper.eq(MemberSmartChargeConfigEntity::getUserId, userId).eq(MemberSmartChargeConfigEntity::getLocationId, locationId);
        MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = Optional.ofNullable(memberSmartChargeConfigMapper.selectOne(queryWrapper)).orElseGet(() -> createDefault(smartChargeConfigQueryDTO));
        SmartChargeProfile profile = memberSmartChargeConfigEntity.getProfile();
        LocalTime localTime = LocalTime.parse(chargingScheduleTaskDTO.getOpeningZonedTime(), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        LocalTime initialTime = localTime.plusHours(1).withMinute(0).withSecond(0).withNano(0);
        profile.setInitialTime(initialTime);
        profile.setDriveTime(initialTime.plusHours(6));
        profile.setFixedVehicle(0);
        BigDecimal batteryLevel = pileMonitorServiceAdapter.readSOCFromMonitorCache(chargingScheduleTaskDTO.getEvseSn(), chargingScheduleTaskDTO.getOrderSeq());
        if (Objects.nonNull(batteryLevel) && batteryLevel.compareTo(BigDecimal.ZERO) > 0) {
            profile.setStartSOC(batteryLevel);
            profile.setFixedStartSOC(1);
        } else {
            profile.setFixedStartSOC(0);
            profile.setStartSOC(BigDecimal.ZERO);
        }
        Optional<UserVehicleVO> optional = obtainUserMainVehicle(smartChargeConfigQueryDTO.getUserId());
        if (optional.isPresent()) {
            optional.ifPresent(userVehicle -> {
                profile.setName(userVehicle.getName());
                profile.setYear(userVehicle.getYear());
                profile.setMake(userVehicle.getMake());
                profile.setEdition(userVehicle.getEdition());
                profile.setModel(userVehicle.getModel());
                profile.setBatteryCapacity(userVehicle.getBatteryCapacity());
                profile.setFixedVehicle(0);
                profile.setVehicleId(userVehicle.getId());
                profile.setId(memberSmartChargeConfigEntity.getId());
            });
        } else {
            profile.setStatus(0);
            profile.setBatteryCapacity("");
        }
        if (!StringUtils.hasText(profile.getBatteryCapacity())) {
            log.warn("没有电池容量关闭公共智能充电 {}", JSON.toJSONString(profile));
            profile.setStatus(0);
        }
        log.info("6 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(memberSmartChargeConfigEntity));
        memberSmartChargeConfigEntity.setUpdateTime(System.currentTimeMillis());
        memberSmartChargeConfigMapper.updateById(memberSmartChargeConfigEntity);
        return memberSmartChargeConfigEntity;
    }

    @Override
    public List<MemberSmartChargeConfigEntity> getUserVehicleProfile(long userId) {
        LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class);
        queryWrapper.eq(MemberSmartChargeConfigEntity::getUserId, userId);
        return memberSmartChargeConfigMapper.selectList(queryWrapper);
    }

    public String getZoneId(Long locationId) {
        return opLocationService.getZoneId(locationId);
    }

    @Resource
    private OpLocationService opLocationService;
}
