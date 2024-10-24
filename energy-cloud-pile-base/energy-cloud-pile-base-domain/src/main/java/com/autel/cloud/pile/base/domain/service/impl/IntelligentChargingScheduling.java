package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.charge.soc.ChargeStatusVO;
import com.autel.cloud.charge.soc.SocVO;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.dto.OpEvseStatusUploadDTO;
import com.autel.cloud.pile.base.SmartChargeProfile;
import com.autel.cloud.pile.base.VehicleProfile;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.data.JobQuery;
import com.autel.cloud.pile.base.domain.data.QuickChargingScheduleDTO;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.domain.service.MemberSmartChargeConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.NumberPhasesEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitMQConfig;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.WsCoreClient;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.*;
import com.autel.cloud.pile.base.infrastructure.feign.dto.*;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleJobMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.MemberSmartChargeConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileGroupMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleRecord;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.MemberSmartChargeConfigEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.vo.ChargingDetailVO;
import com.autel.cloud.pile.base.vo.LocationVO;
import com.autel.cloud.pile.base.vo.UserVehicle;
import com.autel.cloud.pile.bill.dto.ChargingScheduleTaskDTO;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.vo.EnergyBillDetailVO;
import com.autel.cloud.push.enums.ChargingScheduleStatusEnum;
import com.autel.cloud.push.feign.PushAppClient;
import com.autel.cloud.smart.charge.constant.SmartChargeStatus;
import com.autel.cloud.smart.charge.dto.ChargingTaskDTO;
import com.autel.cloud.smart.charge.dto.NextDrivePlan;
import com.autel.cloud.smart.charge.dto.ProfileParamDTO;
import com.autel.cloud.smart.charge.enums.ChargingUpUnitEnum;
import com.autel.cloud.smart.charge.enums.ProfileTypeEnum;
import com.autel.cloud.smart.charge.enums.SmartChargeStatusEnum;
import com.autel.cloud.smart.charge.enums.TriggerJobEventType;
import com.autel.cloud.smart.charge.vo.ChargingGraphVO;
import com.autel.cloud.smartbi.dto.*;
import com.autel.cloud.uitl.DateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.gavaghan.geodesy.Ellipsoid;
import org.gavaghan.geodesy.GeodeticCalculator;
import org.gavaghan.geodesy.GlobalCoordinates;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageBuilder;
import org.springframework.amqp.core.MessageDeliveryMode;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.event.EventListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.LockSupport;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.infrastructure.amqp.RabbitMQConfig.TTL_ROUTING_KEY;
import static com.autel.cloud.pile.bill.constant.RedisKeys.BUSINESS_INTELLIGENT_CHARGE_SCHEDULE;
import static com.autel.cloud.smart.charge.constant.SmartChargeStatus.ENABLE_FAILED_STATUS;


@Service
@Slf4j
public class IntelligentChargingScheduling {

    public static final String PRIORITY_FLAG = "2";
    private final GeodeticCalculator geodeticCalculator = new GeodeticCalculator();

    private ScheduledExecutorService scheduledExecutorService = Executors.newSingleThreadScheduledExecutor();

    @Value("${intelligent.identify.match.location:true}")
    public boolean identifyMatchLocation; // 对该变量使用配置文件参数控制

    /**
     * 智能调度充电时间
     */
    @Value("${intelligent.charge.retryTimes:100}")
    public int retryTimes; // 对该变量使用配置文件参数控制

    @Value("${intelligent.charge.delayTime:5}")
    public int delayTime; // 对该变量使用配置文件参数控制

    public static final String LIMIT_ONE = "limit 1";

    public static final Integer STACK_LEVEL = 20;

    public static final Integer PROFILE_ID = 1;

    @Resource
    private RabbitTemplate rabbitTemplate;

    @Resource
    private MemberSmartChargeConfigService memberSmartChargeConfigService;
    /**
     * 智能调度充电时间 秒
     */
    @Value("${intelligent.charge.schedule.timeout:30}")
    public int timeout; // 对该变量使用配置文件参数控制

    /**
     * 智能调度充电时间 秒
     */
    @Value("${trySmartChargeTimeout:30}")
    public long trySmartChargeTimeout; // 对该变量使用配置文件参数控制

    @Resource
    private WebHookServiceAdapter webHookServiceAdapter;
    @Resource
    private VehicleServiceAdapter vehicleServiceAdapter;
    @Resource
    private WsCoreClient wsCoreClient;
    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private MemberSmartChargeConfigMapper intelligentChargeConfigMapper;

    @Resource
    private IntelligentChargeScheduleJobMapper intelligentChargeScheduleJobMapper;

    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Resource
    private IntelligentChargeScheduleRecordMapper intelligentChargeScheduleRecordMapper;
    @Resource
    private MemberSmartChargeConfigMapper memberSmartChargeConfigMapper;

    @Resource
    private OpLocationPileGroupMapper opLocationPileGroupMapper;

    @Resource
    private OpLocationPileGroupRepository opLocationPileGroupRepository;

    @Resource
    private PileTariffServiceAdapter pileTariffServiceAdapter;

    @Resource
    private PileDeviceServiceAdapter pileDeviceServiceAdapter;

    @Resource
    private PushAppClientAdapter pushAppClientAdapter;

    @Resource
    private PileMonitorServiceAdapter pileMonitorServiceAdapter;

    @Resource
    private WSCoreProxyServiceAdapter wsCoreProxyServiceAdapter;

    @Resource
    private AppUserServiceAdapter appUserServiceAdapter;

    @Resource
    private PileHomeServiceAdapter pileHomeServiceAdapter;

    @Resource
    private PileBillServiceAdapter pileBillServiceAdapter;


    private final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private final DateTimeFormatter HH_mm = DateTimeFormatter.ofPattern("HH:mm");

    @Resource(name = "schedulingChargeTaskExecutor")
    private Executor schedulingChargeTaskExecutor;

    @Resource
    private SmartChargeServiceAdapter smartChargeServiceAdapter;

    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    public ChargingScheduleCountDownDTO countDown(ChargingScheduleCountDownQueryDTO chargingScheduleTaskDTO) {
        log.info("countDown {}", JSON.toJSONString(chargingScheduleTaskDTO));
        String transactionId = chargingScheduleTaskDTO.getTransactionId();
        return getChargingScheduleInfo(transactionId);
    }

    public ChargingScheduleCountDownDTO getChargingScheduleInfo(String transactionId) {
        ChargingScheduleCountDownDTO chargingScheduleCountDownDTO = new ChargingScheduleCountDownDTO();
        chargingScheduleCountDownDTO.setTransactionId(transactionId);
        String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, transactionId);
        Long expire = stringRedisTemplate.boundValueOps(format).getExpire();
        log.info("{} expire={}", format, expire);
        if (Objects.nonNull(expire) && expire >= 0) {
            String message = stringRedisTemplate.opsForValue().get(format);
            chargingScheduleCountDownDTO.setMessage(message);
            if (expire <= trySmartChargeTimeout) {
                chargingScheduleCountDownDTO.setRemainingTime(DateUtil.formatTime(expire));
                chargingScheduleCountDownDTO.setExpire(expire);
            }
        }
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.parseLong(transactionId));
        if (Objects.nonNull(intelligentChargeScheduleJob)) {
            log.info("intelligentChargeJobQuery: {}", JSON.toJSONString(intelligentChargeScheduleJob));
            chargingScheduleCountDownDTO.setSn(intelligentChargeScheduleJob.getEvseSn().split("_")[0]);
            BigDecimal soc = pushAppSOCFromMonitor(intelligentChargeScheduleJob);
            chargingScheduleCountDownDTO.setSoc(soc);
            chargingScheduleCountDownDTO.setSmartChargeSwitch(intelligentChargeScheduleJob.getSmartChargeSwitch());
            chargingScheduleCountDownDTO.setSmartChargeStatus(intelligentChargeScheduleJob.getSmartChargeStatus());

            MemberSmartChargeConfigEntity entity = memberSmartChargeConfigMapper.selectById(intelligentChargeScheduleJob.getProfileId());
            if (Objects.nonNull(entity)) {
                LocalTime driveTime = entity.getProfile().getDriveTime();
                chargingScheduleCountDownDTO.setDriveTime(driveTime.format(DateTimeFormatter.ofPattern("HH:mm")));
                chargingScheduleCountDownDTO.setTargetSOC(entity.getProfile().getTargetSOC());
            }
        }
        return chargingScheduleCountDownDTO;
    }

    /**
     * 立刻快充
     *
     * @param chargingTaskDTO 参数
     * @return 结果
     */
    public Result<Set<Long>> quickCharge(QuickChargingScheduleDTO chargingTaskDTO) {
        // 充电桩离线，无法切换为普通充电模式，建议您停止后重新启动
        String evseSn = chargingTaskDTO.getSn() + "_" + chargingTaskDTO.getGunNo();
        EnergyBillDetailVO energyBillDetailVO = pileBillServiceAdapter.selectBillBaseByBusId(chargingTaskDTO.getTransactionId());
        if (Objects.isNull(energyBillDetailVO) || !energyBillDetailVO.getEvseSn().equalsIgnoreCase(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.valueOf(chargingTaskDTO.getTransactionId()));
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(intelligentChargeScheduleJob.getUserId());
        try {
            if (assertPileIsOffline(chargingTaskDTO.getSn(), evseSn)) {
                log.info("Piles: {} is not online ", evseSn);
                String networkOffline = MessageSourceHolder.getMessage("SWITCH_INTELLIGENT_CHARGING_FAILED_NETWORK_OFFLINE", new Object[]{}, "充电桩离线，无法切换为普通充电模式，建议您停止后重新启动", locale);
                Result<Set<Long>> setResult = Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR, networkOffline);
                setResult.setData(new HashSet<>(Collections.singletonList(intelligentChargeScheduleJob.getUserId())));
                return setResult;
            }
            JobQuery jobQuery = new JobQuery();
            jobQuery.setTransactionId(chargingTaskDTO.getTransactionId());
            jobQuery.setSwitchSmartCharge(0);
            switchSmartCharge(jobQuery);
            String quickChargeSuccess = MessageSourceHolder.getMessage("SWITCH_NORMAL_CHARGING_SUCCESS", new Object[]{}, "切换成功", locale);
            Result<Set<Long>> booleanResult = Result.ofSucceed();
            booleanResult.setMessage(quickChargeSuccess);
            booleanResult.setData(new HashSet<>(Collections.singletonList(intelligentChargeScheduleJob.getUserId())));
            return booleanResult;
        } catch (Exception e) {
            log.error("quickCharge failed", e);
        }
        String quickChargeFailed = MessageSourceHolder.getMessage("SWITCH_INTELLIGENT_CHARGING_FAILED_OTHER", new Object[]{}, "充电桩离线，无法切换为普通充电模式，建议您停止后重新启动", locale);
        Result<Set<Long>> booleanResult = Result.ofSucceed();
        booleanResult.setMessage(quickChargeFailed);
        booleanResult.setData(new HashSet<>(Collections.singletonList(intelligentChargeScheduleJob.getUserId())));
        return booleanResult;
    }

    public boolean assertPileIsOffline(String sn, String evseSn) {
        OpEvseStatusUploadDTO opEvseStatusUploadDTO = pileMonitorServiceAdapter.queryStatusInfoByEvseSn(evseSn);
        return EvseDeviceStatusEnum.DEFAULT.getName().equals(opEvseStatusUploadDTO.getStatus()) || !wsCoreProxyServiceAdapter.pileIsOnline(sn);

    }

    /**
     * @param exchange   exchange
     * @param routingKey routingKey
     * @param delayTime  毫秒
     * @param content    呢绒
     */
    public void sendDelayMessage(String exchange, String routingKey, Integer delayTime, String content) {
        Message msg = MessageBuilder.withBody(content.getBytes()).setContentType(MessageProperties.CONTENT_TYPE_JSON)
                .setDeliveryMode(MessageDeliveryMode.PERSISTENT)
                .setContentEncoding("utf-8").build();
        rabbitTemplate.setMandatory(true);
        rabbitTemplate.convertAndSend(exchange, routingKey, msg, message -> {
            message.getMessageProperties().setDelay(delayTime);
            return message;
        });
        log.info("sendDelayMessage= {}", content);
    }

    /**
     * 这里只是 识别车辆  公共智能充电之后 会批量拉取用户的智能充电配(期望SOC 充电截至时间)
     * <p>
     * 这里和家用智能充电不同的是 即使没有识别出车辆那么也可以智能充电
     *
     * @param chargingScheduleTaskDTO 调度参数
     * @return 是否调度成功
     */
    public Boolean schedule(ChargingScheduleTaskDTO chargingScheduleTaskDTO) {
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.parseLong(chargingScheduleTaskDTO.getOrderSeq()));
        if (Objects.nonNull(intelligentChargeScheduleJob)) {
            log.warn("Order was scheduled {}", JSON.toJSONString(chargingScheduleTaskDTO));
            webHookServiceAdapter.sendMessage(chargingScheduleTaskDTO.getOrderSeq(), chargingScheduleTaskDTO.getEvseSn(), chargingScheduleTaskDTO.getOrderNumber(), "重复调度");
            return false;
        }

        Long userId = chargingScheduleTaskDTO.getUserId();
        String evseSn = chargingScheduleTaskDTO.getEvseSn();
        String pileSN = evseSn.split("_")[0];

        int updateCount = intelligentChargeScheduleJobMapper.updateFinishedJob(evseSn, chargingScheduleTaskDTO.getOrderSeq());
        if (updateCount > 0) {
            log.warn("updateFinishedJobByEvseSN updateCount={}", updateCount);
        }
        // 根据用户和场站ID查询是否自动开启公共智能充电  如果生成初始配置(开始充电时间和期望结束时间)
        MemberSmartChargeConfigEntity config = memberSmartChargeConfigService.genConfigByUserIdAndLocationId(chargingScheduleTaskDTO);
        SmartChargeStatusEnum smartChargeStatus = getSmartChargeStatus(pileSN, config);
        Integer smartChargeSwitch = SmartChargeStatusEnum.USER_SET_ENABLE.equals(smartChargeStatus) ? 1 : 0;
        log.info("pileSn= {} getSmartChargeStatus= {},  smartChargeSwitch ={}, SmartChargeConfig= {} ", pileSN, smartChargeStatus, smartChargeSwitch, JSON.toJSONString(config));
        IntelligentChargeScheduleJob job = IntelligentChargeScheduleJob.builder()
                .id(Long.valueOf(chargingScheduleTaskDTO.getOrderSeq()))
                .userId(chargingScheduleTaskDTO.getUserId())
                .orderNumber(chargingScheduleTaskDTO.getOrderNumber())
                .openingZonedTime(chargingScheduleTaskDTO.getOpeningZonedTime())
                .createTime(LocalDateTime.now(ZoneId.of(chargingScheduleTaskDTO.getZoneId())))
                .timestamp(System.currentTimeMillis())
                .status(JobStatus.NEW.getStatus())
                .smartChargeStatus(smartChargeStatus.getStatus())
                .zoneId(chargingScheduleTaskDTO.getZoneId())
                .evseSn(evseSn)
                .orderSeq(chargingScheduleTaskDTO.getOrderSeq())
                .evseId(chargingScheduleTaskDTO.getEvseId())
                .profileId(Optional.ofNullable(config).map(MemberSmartChargeConfigEntity::getId).orElse(0L))
                .smartChargeSwitch(smartChargeSwitch)
                .locationId(chargingScheduleTaskDTO.getLocationId())
                .chargingType(chargingScheduleTaskDTO.getChargingType())
                .evseType(chargingScheduleTaskDTO.getEvseType())
                .build();
        intelligentChargeScheduleJobMapper.insert(job);

        if (smartChargeSwitch == 1) { // 开了智能充电的发送
            sendDelayMessage(RabbitMQConfig.X_EXCHANGE_START_SMART_CHARGING + RabbitBean.RABBITMQ_VERSION_SUFFIX, TTL_ROUTING_KEY, timeout * 1000, job.getId().toString());
        }
        // 识别车辆
        // 第1步 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓识别车辆 不管任何情况 都需要识别车辆 所以放在第一位 当没有电池标量的时候 停止↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        boolean identifyResult = doIdentify(job);
        // 第1步 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑识别车辆↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // 第2步  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 检查桩已开启智能充电 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        if (BigInteger.ZERO.intValue() == job.getSmartChargeSwitch()) {
            log.info("Pile's smartChargeSwitch : {} is disabled", evseSn);
            // 未开启开启智能充电 直接返回false 不需要智能充电
            webHookServiceAdapter.sendMessage(chargingScheduleTaskDTO.getOrderSeq(), chargingScheduleTaskDTO.getEvseSn(), chargingScheduleTaskDTO.getOrderNumber(), "未开启开启智能充电不需要激发智能充电");
            return false;
        }
        // 第2步 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑检查桩已开启智能充电↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // 第3步  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 桩已连网判断  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        if (assertPileIsOffline(pileSN, evseSn)) {
            log.info("Pile: {} is not online ", evseSn);
            Locale locale = appUserServiceAdapter.getUserLocaleSettings(userId);
            webHookServiceAdapter.sendMessage(chargingScheduleTaskDTO.getOrderSeq(), chargingScheduleTaskDTO.getEvseSn(), chargingScheduleTaskDTO.getOrderNumber(), "桩不在线不需要激发智能充电");
            noticeMessage(job, "DISABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SWITCH_SMART_CHARGING_FAILED_NETWORK_UNAVAILABLE);
            // 不在线 直接返回false 不需要智能充电
            job.setSmartChargeStatus(ENABLE_FAILED_STATUS);
            intelligentChargeScheduleJobMapper.updateById(job);
            return false;
        }
        // 第3步 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑桩已连网判断↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        /*
         *      SWITCH_FAILED(-1, "切换失败"),
         *      PILE_NO_SUPPORT(0, "桩不支持智能充电"),
         *      PILE_SUPPORT(1, "桩支持智能充电 但是用户默认关闭"),
         *      USER_SET_ENABLE(3, "桩支持智能充电 用户默认开启"),
         *      SWITCH_SUCCESS(7, "切换成功");
         */

        log.info("开启了智能充电 {}", JSON.toJSONString(job));
        intelligentChargeScheduleJobMapper.updateById(job);
        //  切换智能充电倒计时
        String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
        String message = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.TRY_SCHEDULE_INTELLIGENT_CHARGING.getI18nKey(), ChargingScheduleStatusEnum.TRY_SCHEDULE_INTELLIGENT_CHARGING.getI18nValue());
        stringRedisTemplate.opsForValue().set(format, message, trySmartChargeTimeout, TimeUnit.SECONDS);
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job, locale)));
        log.info("result: {} doIdentify ", identifyResult);
        DeliveryByUserDTO deliveryByUserDTO = getDeliveryByUserDTO(config, job);
        Boolean aBoolean = opLocationPileGroupService.deliveryByUser(deliveryByUserDTO);
        log.info("result: {} deliveryByUser ", aBoolean);
        if (Boolean.TRUE.equals(aBoolean)) {
            // 怎么判断切换成功
            LockSupport.parkNanos(100000000);
            ChargingDetailVO lastChargingDetailVO = smartChargeServiceAdapter.findLastChargingDetailVO(job.getEvseSn(), job.getOrderSeq());
            if (Objects.nonNull(lastChargingDetailVO)) {
                message = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.INTELLIGENT_CHARGING_SUCCESS.getI18nKey(), ChargingScheduleStatusEnum.INTELLIGENT_CHARGING_SUCCESS.getI18nValue());
                setValueExcludeExpire(format, message);
                job.setSmartChargeSwitch(1);
                job.setSmartChargeStatus(SmartChargeStatus.SWITCH_SUCCESS_STATUS);
                if (JobStatus.QUICK.getStatus() == job.getStatus()) {
                    job.setStatus(JobStatus.RUNNING.getStatus());
                }
                intelligentChargeScheduleJobMapper.updateById(job);
                doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job, locale)));
                doExecuteTask(RunnableWrapper.of(() -> noticeMessage(job, "ENABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SWITCH_SMART_CHARGING_SUCCESS)));
            }
        }
        return aBoolean;
    }

    /**
     * 切换智能充电倒计时最后一次 查询是否下发
     *
     * @param id
     * @return
     */
    public boolean lastTrySmartSchedule(Long id) {
        log.info("lastTrySmartSchedule {}", id);
        IntelligentChargeScheduleJob job = intelligentChargeScheduleJobMapper.selectById(id);
        if (job.getSmartChargeStatus() == SmartChargeStatus.SWITCH_SUCCESS_STATUS) {
            log.info("SWITCH_SUCCESS {}", JSON.toJSONString(job));
            return true;
        }
        if (job.getSmartChargeSwitch() == 0) {
            log.info("close smartcharge {}", JSON.toJSONString(job));
            return true;
        }

        BigDecimal batteryLevel = pileMonitorServiceAdapter.readSOCFromMonitorCache(job.getEvseSn(), job.getOrderSeq());
        if (Objects.nonNull(batteryLevel) && batteryLevel.compareTo(BigDecimal.ZERO) > 0 && Objects.nonNull(job.getProfileId())) {
            MemberSmartChargeConfigEntity entity = memberSmartChargeConfigMapper.selectById(job.getProfileId());
            if (Objects.nonNull(entity) && Objects.nonNull(entity.getProfile()) && 1 != entity.getProfile().getFixedStartSOC()) {
                entity.getProfile().setFixedVehicle(1);
                entity.getProfile().setStartSOC(batteryLevel);
                log.info("12 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(entity));
                memberSmartChargeConfigMapper.updateById(entity);
                tryReschedule(entity);
            }
        }

        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        ChargingDetailVO lastChargingDetailVO = smartChargeServiceAdapter.findLastChargingDetailVO(job.getEvseSn(), job.getOrderSeq());
        if (Objects.nonNull(lastChargingDetailVO)) {
            String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
            String message = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.INTELLIGENT_CHARGING_SUCCESS.getI18nKey(), ChargingScheduleStatusEnum.INTELLIGENT_CHARGING_SUCCESS.getI18nValue());
            setValueExcludeExpire(format, message);
            job.setSmartChargeStatus(SmartChargeStatus.SWITCH_SUCCESS_STATUS);
            if (JobStatus.QUICK.getStatus() == job.getStatus()) {
                job.setStatus(JobStatus.RUNNING.getStatus());
            }
            job.setSmartChargeSwitch(1);
            intelligentChargeScheduleJobMapper.updateById(job);
            pushAppScheduleMessage(job, locale);
            noticeMessage(job, "ENABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SWITCH_SMART_CHARGING_SUCCESS);
            return true;
        } else {
            job.setSmartChargeSwitch(1);
            job.setSmartChargeStatus(SmartChargeStatus.ENABLE_FAILED_STATUS); // 切换失败还是要回到  但是这里 要给出 "由于网络连接不稳定，智能充电切换失败，请稍后重试" 的提示 执行完之后 再回到  桩支持智能充电   显示“开启智能充电”
            pushAppMessage(job, locale);
            doExecuteTask(RunnableWrapper.of(() -> noticeMessage(job, "DISABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SWITCH_SMART_CHARGING_FAILED_NETWORK_UNAVAILABLE)));
            intelligentChargeScheduleJobMapper.updateById(job);
            log.info("monitorMessage lastTrySmartSchedule failed {}", job.getOrderSeq());
            String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
            String msg1 = MessageSourceHolder.getMessage("SWITCH_SMART_CHARGING_FAILED_OBTAIN_CAR_INFO_FAILED", new Object[]{}, locale);
            setValueExcludeExpire(format, msg1);
            job.setSmartChargeStatus(SmartChargeStatus.PILE_SUPPORT_STATUS); // 切换失败还是要回到 桩支持智能充电   显示“开启智能充电”
            job.setSmartChargeSwitch(0); // 没有切换成功 要还原到 普通充电状态
            intelligentChargeScheduleJobMapper.updateById(job);
            pushAppMessage(job, locale);
        }
        return false;
    }

    public void switchFailureCaseBatteryCapacityEmpty(IntelligentChargeScheduleJob job, SmartChargeStatusEnum smartChargeStatusEnum) {
        log.info("switchFailureCaseBatteryCapacityEmpty {}", JSON.toJSONString(job));
        Assert.isTrue(SmartChargeStatusEnum.BATTERY_CAPACITY_EMPTY.equals(smartChargeStatusEnum), "错误原因为电池标量没有");
        job.setSmartChargeStatus(smartChargeStatusEnum.getStatus());
        job.setSmartChargeSwitch(1);
        intelligentChargeScheduleJobMapper.updateById(job);
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        pushAppMessage(job, locale);
        doExecuteTask(RunnableWrapper.of(() -> noticeMessage(job, "DISABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SET_BATTERY_CAPACITY_FOR_THE_VEHICLE_FIRST)));
        // 通知完app 之后要回到  开启智能充电 状态
        job.setSmartChargeSwitch(0);
        job.setSmartChargeStatus(SmartChargeStatusEnum.PILE_SUPPORT.getStatus());
        intelligentChargeScheduleJobMapper.updateById(job);
    }


    public DeliveryByUserDTO getDeliveryByUserDTO(MemberSmartChargeConfigEntity config, IntelligentChargeScheduleJob job) {
        Assert.notNull(job, "job must not null " + JSON.toJSONString(job));
        Assert.notNull(config, "config must not null " + JSON.toJSONString(config));
        SmartChargeProfile profile = config.getProfile();
        if (!StringUtils.hasText(profile.getBatteryCapacity())) {
            switchFailureCaseBatteryCapacityEmpty(job, SmartChargeStatusEnum.BATTERY_CAPACITY_EMPTY);
            throw new MessageCodeException(PileBaseEnum.SET_BATTERY_CAPACITY_FOR_THE_VEHICLE_FIRST);
        }
        DeliveryByUserDTO deliveryByUserDTO = new DeliveryByUserDTO();
        deliveryByUserDTO.setOrderNumber(job.getOrderNumber());
        deliveryByUserDTO.setOrderSeq(job.getOrderSeq());
        deliveryByUserDTO.setLocationId(job.getLocationId());
        deliveryByUserDTO.setBatteryCare(profile.getBatteryCare());
        // 大概率的获取不到当前soc  后面再覆盖
        deliveryByUserDTO.setCurrentSOC(Optional.ofNullable(pileMonitorServiceAdapter.readSOCFromMonitorCache(job)).orElse(profile.getStartSOC()));

        deliveryByUserDTO.setTargetSOC(profile.getTargetSOC());
        deliveryByUserDTO.setStartSOC(profile.getStartSOC());

        String openingZonedTime = job.getOpeningZonedTime();
        LocalDateTime startTime = LocalDateTime.parse(openingZonedTime, formatter);
        ZoneId zoneId = ZoneId.of(job.getZoneId());

        LocalTime driveTime = profile.getDriveTime();
        LocalDateTime localDateTime = driveTime.atDate(LocalDate.now(zoneId));
        if (localDateTime.isAfter(startTime)) {
            long milli = localDateTime.atZone(zoneId).toInstant().toEpochMilli();
            deliveryByUserDTO.setDriveTimestamp(milli);
        } else {
            long milli = localDateTime.plusDays(1).atZone(zoneId).toInstant().toEpochMilli();
            deliveryByUserDTO.setDriveTimestamp(milli);
        }
        deliveryByUserDTO.setFast(job.getSmartChargeSwitch() == 1 ? 0 : 1);
        ChargingDetailVO lastChargingDetailVO = smartChargeServiceAdapter.findLastChargingDetailVO(job.getEvseSn(), job.getOrderSeq());
        if (Objects.nonNull(lastChargingDetailVO)) {
            deliveryByUserDTO.setSavedEnergyFee(lastChargingDetailVO.getSavedEnergyFee());
        }
        deliveryByUserDTO.setEvseSn(job.getEvseSn());
        deliveryByUserDTO.setMinSOC(profile.getMinSOC());
        deliveryByUserDTO.setUserId(job.getUserId());
        deliveryByUserDTO.setBatteryCapacity(new BigDecimal(profile.getBatteryCapacity()));

        return deliveryByUserDTO;
    }

    /**
     * 根据用户ID 和场站ID 查询智能充电状态
     *
     * @param smartChargeStatusQueryDTO
     * @return
     */
    public int getSmartChargeStatus(SmartChargeStatusQueryDTO smartChargeStatusQueryDTO) {
        Long userId = smartChargeStatusQueryDTO.getUserId();
        Long locationId = smartChargeStatusQueryDTO.getLocationId();
        String pileSN = smartChargeStatusQueryDTO.getPileSN();
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryJobWrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class)
                .eq(IntelligentChargeScheduleJob::getOrderSeq, smartChargeStatusQueryDTO.getOrderSeq())
                .eq(IntelligentChargeScheduleJob::getLocationId, smartChargeStatusQueryDTO.getLocationId())
                .eq(IntelligentChargeScheduleJob::getUserId, userId);
        IntelligentChargeScheduleJob scheduleJob = intelligentChargeScheduleJobMapper.selectOne(queryJobWrapper);
        if (Objects.nonNull(scheduleJob)) {
            return scheduleJob.getSmartChargeStatus();
        }
        LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class)
                .eq(MemberSmartChargeConfigEntity::getLocationId, locationId)
                .eq(MemberSmartChargeConfigEntity::getUserId, userId);
        MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = intelligentChargeConfigMapper.selectOne(queryWrapper);
        return getSmartChargeStatus(pileSN, memberSmartChargeConfigEntity).getStatus();
    }

    /**
     * SWITCH_FAILED(-1, "切换失败"),
     * PILE_NO_SUPPORT(0, "桩不支持智能充电"),
     * PILE_SUPPORT(1, "桩支持智能充电 但是用户默认关闭"),
     * USER_SET_ENABLE(3, "桩支持智能充电 用户默认开启"),
     * SWITCH_SUCCESS(7, "切换成功");
     */
    private SmartChargeStatusEnum getSmartChargeStatus(String pileSN, MemberSmartChargeConfigEntity memberSmartChargeConfigEntity) {
        boolean enableSmartChargePile = opLocationPileGroupService.isEnableSmartChargePile(pileSN);
        SmartChargeStatusEnum pileSupport = enableSmartChargePile ? SmartChargeStatusEnum.PILE_SUPPORT : SmartChargeStatusEnum.PILE_NO_SUPPORT;
        if (Objects.isNull(memberSmartChargeConfigEntity)) {
            return pileSupport;
        }
        // 记录本次充电调度JOB
        boolean present = Optional.of(memberSmartChargeConfigEntity).map(MemberSmartChargeConfigEntity::getStatus).filter(t -> t == 1).isPresent();
        return present ? SmartChargeStatusEnum.bitStatus(pileSupport, SmartChargeStatusEnum.USER_SET_ENABLE) : SmartChargeStatusEnum.bitStatus(pileSupport);
    }


    private void doExecuteTask(Runnable command) {
        schedulingChargeTaskExecutor.execute(command);
    }

    /**
     * 找到 app 用户哪辆车正在充电 根据充电状态
     *
     * @param job 智能充电job
     * @return 智能充电配置
     */
    private MemberSmartChargeConfigEntity identify(IntelligentChargeScheduleJob job) {
        try {
            String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
            Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
            webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), LocalDateTime.now(ZoneId.of(job.getZoneId())).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")) + "尝试识别车辆信息");
            job.setStatus(JobStatus.TRY_LINKED.getStatus());//
            intelligentChargeScheduleJobMapper.updateById(job);
            String message = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.AUTH_VEHICLE.getI18nKey(), ChargingScheduleStatusEnum.AUTH_VEHICLE.getI18nValue());
            setValueExcludeExpire(format, message);
            pushAppScheduleMessage(job, locale);
            List<UserVehicle> userVehicles = pileHomeServiceAdapter.queryUserVehicleByUserId(job.getUserId());
            if (CollectionUtils.isEmpty(userVehicles)) {
                webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), "识别 用户还没有任何车辆的智能充电配置");
                job.setStatus(JobStatus.FAILURE_VEHICLE.getStatus());//
                intelligentChargeScheduleJobMapper.updateById(job);
                return null;
            }

            for (UserVehicle userVehicle : userVehicles) {
                String userVehicleName = userVehicle.getName() + "(" + userVehicle.getMake() + userVehicle.getModel() + userVehicle.getYear() + ")";

                String enodeId = userVehicle.getEnodeId();
                if (!StringUtils.hasText(enodeId)) {
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), "识别 " + userVehicleName + "没有授权");
                    continue;
                }
                JSONObject vehicleDetailVO = vehicleServiceAdapter.getByUserIdAndVehicleId(job.getUserId(), enodeId);
                if (Objects.isNull(vehicleDetailVO)) {
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + " 没有车辆信息");
                    continue;
                }
                JSONObject chargeStatusVO = vehicleDetailVO.getJSONObject("chargeStatusVO");
                if (Objects.isNull(chargeStatusVO)) {
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + " 没有车辆充电状态");
                    continue;
                }
                JSONObject makeInfoVO = vehicleDetailVO.getJSONObject("makeInfoVO");
                if (Objects.isNull(makeInfoVO)) {
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + " 没有车辆品牌信息");
                    continue;
                }
                String lastUpdated = chargeStatusVO.getString("lastUpdated");
                Boolean charging = chargeStatusVO.getBoolean("charging");
                BigDecimal batteryLevel = chargeStatusVO.getBigDecimal("batteryLevel");
                String chargingStatus = Boolean.TRUE.equals(charging) ? " 正在充电" : " 未充电";
                String msg = String.format("%s %s %s 当前电量：%s", userVehicleName, lastUpdated, chargingStatus, batteryLevel);
                webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicle.getId(), msg);
                if (matchVehicle(vehicleDetailVO, userVehicle, job)) { // 开发环境这里简单一下  prod
                    Long userId = job.getUserId();
                    Long locationId = job.getLocationId();
                    LambdaQueryWrapper<MemberSmartChargeConfigEntity> queryWrapper = Wrappers.lambdaQuery(MemberSmartChargeConfigEntity.class);
                    queryWrapper.eq(MemberSmartChargeConfigEntity::getUserId, userId).eq(MemberSmartChargeConfigEntity::getLocationId, locationId);
                    MemberSmartChargeConfigEntity intelligentChargeConfig = memberSmartChargeConfigMapper.selectOne(queryWrapper);
                    if (Objects.nonNull(intelligentChargeConfig)) {
                        intelligentChargeConfig.getProfile().setMake(userVehicle.getMake());
                        intelligentChargeConfig.getProfile().setName(userVehicle.getName());
                        intelligentChargeConfig.getProfile().setModel(userVehicle.getModel());
                        intelligentChargeConfig.getProfile().setYear(userVehicle.getYear());
                        intelligentChargeConfig.getProfile().setBatteryCapacity(userVehicle.getBatteryCapacity());
                        if (StringUtils.hasText(userVehicle.getBatteryCapacity())) {
                            // 识别出充电汽车 如果是智能充电 状态改为running 否则 是普通充电 quick
                            int status = job.getSmartChargeSwitch().equals(BigDecimal.ZERO.intValue()) ? JobStatus.QUICK.getStatus() : JobStatus.LINK_SUCCESS.getStatus();
                            job.setStatus(status);
                        }
                        intelligentChargeConfig.getProfile().setVehicleId(userVehicle.getId());
                        intelligentChargeConfig.getProfile().setId(intelligentChargeConfig.getId());
                        intelligentChargeConfig.getProfile().setFixedVehicle(1);
                        intelligentChargeConfig.getProfile().setStartSOC(batteryLevel);
                        intelligentChargeConfig.getProfile().setFixedStartSOC(1);
                        log.info("1 memberSmartChargeConfigMapper.updateById {}", JSON.toJSONString(intelligentChargeConfig));
                        log.info("update result {}", memberSmartChargeConfigMapper.updateById(intelligentChargeConfig));
                        job.setVehicleId(userVehicle.getId());
                        job.setProfileId(intelligentChargeConfig.getId());
                        job.setUpdateTime(LocalDateTime.now(ZoneId.of(job.getZoneId())));
                        intelligentChargeScheduleJobMapper.updateById(job);
                        tryReschedule(intelligentChargeConfig);
                    }
                    String identify = String.format("推断该订单的正在充电车辆 %s %s %s 当前电量：%s", userVehicleName, lastUpdated, chargingStatus, batteryLevel);
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicle.getId(), identify);
                    return intelligentChargeConfig;
                }
            }
        } catch (Exception e) {
            log.error("identify User Vehicle failed", e);
        }
        return null;
    }

    @Value("${NACOS_NS:NONE}")
    private String env;

    private boolean matchVehicle(JSONObject vehicleDetailVO, UserVehicle userVehicle, IntelligentChargeScheduleJob job) {
        try {
            String userVehicleName = userVehicle.getName() + "(" + userVehicle.getMake() + userVehicle.getModel() + userVehicle.getYear() + userVehicle.getVin() + ")";
            JSONObject chargeStatusVO = vehicleDetailVO.getJSONObject("chargeStatusVO");
            if (!env.contains("pro")) {
                log.info("非生产环境: {} 模拟数据 只判断是否充电中即可 ", env);
                return chargeStatusVO.getBoolean("charging");
            }
            // 插枪状态  可靠
            Boolean pluggedIn = chargeStatusVO.getBoolean("pluggedIn");
            if (Boolean.FALSE.equals(pluggedIn)) {
                return false;
            }

            // VIN 码校验   可靠
            JSONObject makeInfoVO = vehicleDetailVO.getJSONObject("makeInfoVO");
            if (!Objects.equals(makeInfoVO.getString("vin"), userVehicle.getVin())) {
                webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + String.format("车辆VIN码不匹配 %s %s", makeInfoVO.getString("vin"), userVehicle.getVin()));
                return false;
            }

            if (identifyMatchLocation && vehicleDetailVO.containsKey("locationVO")) {
                String pileSN = job.getEvseSn().split("_")[0];
                LocationVO locationVO = vehicleDetailVO.getObject("locationVO", LocationVO.class);
                double distanceMeter = getDistanceMeter(pileSN, locationVO);
                log.info("pileSN={} userVehicleName={} distanceMeter={}", pileSN, userVehicleName, distanceMeter);
                if (distanceMeter > 100) {
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + String.format("车辆和桩的距离大于%d m", 100));
                    return false;
                }
            }

            // 最后更新时间   不可靠
//            Date lastUpdated = chargeStatusVO.getDate("lastUpdated");
//            if (System.currentTimeMillis() - lastUpdated.getTime() > 300000L) {
//                webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + String.format("车辆充电信息最后更新时间大于 %d ms", 300000));
//                return false;
//            }

            // 获取车上电流读数 如果没有这个数据代表没有在充电中  不可靠   车辆暂停也没有电流
            BigDecimal chargeRate = chargeStatusVO.getBigDecimal("chargeRate");
            String powerDeliveryState = chargeStatusVO.getString("powerDeliveryState");

            if ("PLUGGED_IN:STOPPED".equalsIgnoreCase(powerDeliveryState)) {
                OpEvseStatusUploadDTO statusUploadDTO = pileMonitorServiceAdapter.queryStatusInfoByEvseSn(job.getEvseSn());
                if (Objects.nonNull(statusUploadDTO) && !"SuspendedEV".equalsIgnoreCase(statusUploadDTO.getStatus())) {
                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + "车辆暂停");
                    return false;
                }
            }

            // 充电状态    不可靠 当车辆暂停时 也是未充电状态
            Boolean charging = chargeStatusVO.getBoolean("charging");
            if (!"PLUGGED_IN:STOPPED".equalsIgnoreCase(powerDeliveryState) && Boolean.FALSE.equals(charging)) {
                log.info("charging should be true");
                return false;
            }

            if (!"PLUGGED_IN:STOPPED".equalsIgnoreCase(powerDeliveryState) && Objects.isNull(chargeRate)) {
                webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + String.format("车辆没有电流 %s", userVehicleName));
                log.info("chargeRate should be not null");
                return false;
            }

            // 充电电流与桩电流比较 误差小于3A   不可靠
//            OpEvseMeterUploadDTO opEvseMeterUploadDTO = pileMonitorServiceAdapter.queryMeterByEvseSnCache(job.getEvseSn());
//            if (Objects.nonNull(chargeRate) && Objects.nonNull(opEvseMeterUploadDTO) && Objects.nonNull(opEvseMeterUploadDTO.getCurrent())) {
//                BigDecimal abs = chargeRate.subtract(BigDecimal.valueOf(opEvseMeterUploadDTO.getCurrent())).abs();
//                // 桩的电流和车上读取到的电流一样(有误差 3A)
//                if (BigDecimal.valueOf(3).compareTo(abs) < 0) {
//                    webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), userVehicleName + String.format("桩的电流和车上读取到的电流 误差大于3A  %f %f", opEvseMeterUploadDTO.getCurrent(), chargeRate.doubleValue()));
//                    return false;
//                }
//            }
            return true;
        } catch (Exception e) {
            log.warn("vehicleDetailVO {} userVehicle {}", JSON.toJSONString(vehicleDetailVO), JSON.toJSONString(userVehicle));
            log.error("matchVehicle", e);
        }
        return false;
    }

    private void setValueExcludeExpire(String key, String message) {
        Long expire = stringRedisTemplate.getExpire(key, TimeUnit.SECONDS);
        if (Objects.nonNull(expire) && expire > 0) {
            stringRedisTemplate.opsForValue().set(key, message, expire, TimeUnit.SECONDS);
        }
    }

    private void pushAppScheduleMessage(IntelligentChargeScheduleJob job) {
        log.info("pushAppScheduleMessage1 {}", JSON.toJSONString(job));
        if (BigInteger.ZERO.intValue() == job.getSmartChargeSwitch()) {
            return;// 不是智能充电就不要推送了
        }
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        pushAppMessage(job, locale);
    }

    private void pushAppScheduleMessage(IntelligentChargeScheduleJob job, Locale locale) {
        log.info("pushAppScheduleMessage2 {}", JSON.toJSONString(job));
        if (BigInteger.ZERO.intValue() == job.getSmartChargeSwitch()) {
            return;// 不是智能充电就不要推送了
        }
        pushAppMessage(job, locale);
    }

    private void pushAppMessage(IntelligentChargeScheduleJob job, Locale locale) {
        log.info("pushAppMessage {}", JSON.toJSONString(job));
        String evseSn = job.getEvseSn();
        ChargingScheduleCountDownQueryDTO chargingScheduleCountDownQueryDTO = ChargingScheduleCountDownQueryDTO.builder()
                .userId(job.getUserId()).locale(locale).transactionId(job.getOrderSeq()).sn(evseSn.split("_")[0]).gunNo(evseSn.split("_")[1]).build();

        MessageDTO<ChargingScheduleCountDownDTO> msg = new MessageDTO<>();
        msg.setCmd(MessageDTO.CmdEnum.SMART_CHARGE_SCHEDULE.getValue());
        ChargingScheduleCountDownDTO result = countDown(chargingScheduleCountDownQueryDTO);
        msg.setData(result);
        msg.setSeq(System.currentTimeMillis() + "");
        SendMsgDTO sendMsg = SendMsgDTO.builder().msg(msg.toString()).receiver(chargingScheduleCountDownQueryDTO.getUserId().toString()).build();
        log.info("pushAppScheduleMessage {}", msg.toString());
        wsCoreClient.sendMsg(sendMsg);
    }

    /**
     * monitor 优先从桩上报的soc 如果没有从车辆上读取
     *
     * @param intelligentChargeScheduleJob
     * @return
     */
    public BigDecimal pushAppSOCFromMonitor(IntelligentChargeScheduleJob intelligentChargeScheduleJob) {
        String evseSn = intelligentChargeScheduleJob.getEvseSn();
        Long userId = intelligentChargeScheduleJob.getUserId();
        Long userVehicleId = intelligentChargeScheduleJob.getVehicleId();
        log.info("pushAppSOCFromMonitor evseSn={} userId={} userVehicleId={}", evseSn, userId, userVehicleId);
        BigDecimal bigDecimal = pileMonitorServiceAdapter.readSOCFromMonitorCache(intelligentChargeScheduleJob);
        if (Objects.nonNull(bigDecimal)) {
            MessageDTO<SocVO> msg = new MessageDTO<>();
            msg.setCmd(MessageDTO.CmdEnum.SOC_DISPLAY.getValue());
            SocVO socVO = new SocVO();
            socVO.setDeviceType(DeviceTypeEnum.BUSINESS_PILE.getValue());
            socVO.setConnectorId(evseSn.split("_")[1]);
            socVO.setEvseSn(evseSn);
            socVO.setSn(evseSn.split("_")[0]);
            socVO.setSoc(bigDecimal);
            msg.setData(socVO);
            msg.setSeq(System.currentTimeMillis() + "");
            doExecuteTask(RunnableWrapper.of(() ->
                    {
                        SendMsgDTO sendMsg = SendMsgDTO.builder().msg(msg.toString()).receiver(userId.toString()).build();
                        log.info("pushAppSOC {}", msg.toString());
                        wsCoreClient.sendMsg(sendMsg);
                    }
            ));
            return bigDecimal;
        }
        return null;
    }


    /**
     * 从开始充电到下次用车时间端的 电价
     * 未来电价走势
     *
     * @param startScheduleTime 用户开始智能调度时间
     * @param nextDriveTime     下一次出行时间
     * @param rules             用户设置的电价
     * @return 到下一次出行时
     */
    public static List<TimeDecimalValue> futurePrice(LocalDateTime startScheduleTime, LocalDateTime nextDriveTime, List<ElectricityPrice> rules) {
        log.info("the futurePrice:{} {} {}", startScheduleTime, nextDriveTime, JSON.toJSONString(rules));
        Assert.notNull(rules, PileBaseEnum.DATA_NOT_EXIST.getMessage());
        List<TimeDecimalValue> result = new ArrayList<>();
        //  启动充电时当天的价格走势
        for (ElectricityPrice ruleWeeksDTO : rules) {
            List<Integer> weeks = ruleWeeksDTO.getDays();
            if (weeks.contains(startScheduleTime.getDayOfWeek().getValue())) {
                List<DailyPrice> weeksRules = ruleWeeksDTO.getDetails();
                for (DailyPrice costRulesDTO : weeksRules) {
                    LocalTime startLocalTime = LocalTime.parse(costRulesDTO.getStartTime(), DateTimeFormatter.ofPattern("HH:mm"));
                    LocalDateTime localDateTime = startScheduleTime.withHour(startLocalTime.getHour()).withMinute(startLocalTime.getMinute());
                    String format = localDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"));
                    TimeDecimalValue timeDecimalValue = new TimeDecimalValue(format, localDateTime, costRulesDTO.getPrice());
                    result.add(timeDecimalValue);
                }
            }
        }
        // 启动充电时次天的价格走势
        int offset = 1;
        while (nextDriveTime.isAfter(startScheduleTime.plusDays(offset).withHour(0).withMinute(0).withSecond(0))) {
            for (ElectricityPrice ruleWeeksDTO : rules) {
                List<Integer> weeks = ruleWeeksDTO.getDays();
                if (weeks.contains(startScheduleTime.plusDays(offset).getDayOfWeek().getValue())) {
                    List<DailyPrice> weeksRules = ruleWeeksDTO.getDetails();
                    for (DailyPrice costRulesDTO : weeksRules) {
                        LocalTime startLocalTime = LocalTime.parse(costRulesDTO.getStartTime(), DateTimeFormatter.ofPattern("HH:mm"));
                        LocalDateTime localDateTime = startScheduleTime.plusDays(offset).withHour(startLocalTime.getHour()).withMinute(startLocalTime.getMinute());
                        String format = localDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"));
                        TimeDecimalValue timeDecimalValue = new TimeDecimalValue(format, localDateTime, costRulesDTO.getPrice());
                        result.add(timeDecimalValue);
                    }
                }
            }
            offset++;
        }
        return result;
    }

    /**
     * @param weekPlans
     * @param zoneId
     * @return
     */
    private NextDrivePlan nextNextDrivePlan(List<VehicleProfile.WeekPlan> weekPlans, ZoneId zoneId) {
        LocalDateTime now = LocalDateTime.now(zoneId);
        // 当前时间是不是当天出行计划之前 如果是那么 就是今天 否则明天
        for (VehicleProfile.WeekPlan weekPlan : weekPlans) {
            if (weekPlan.getWeeks().contains(now.getDayOfWeek().getValue())) {
                LocalTime driveTime = weekPlan.getDailyPlan().getDriveTime();
                LocalDateTime localDateTime = driveTime.atDate(LocalDate.now(zoneId));
                if (localDateTime.isAfter(now)) {
                    return new NextDrivePlan(localDateTime, weekPlan.getDailyPlan().getTargetSOC());
                }
            }
        }
        for (VehicleProfile.WeekPlan weekPlan : weekPlans) {
            if (weekPlan.getWeeks().contains(now.plusDays(1).getDayOfWeek().getValue())) {
                LocalTime driveTime = weekPlan.getDailyPlan().getDriveTime();
                LocalDateTime localDateTime = driveTime.atDate(LocalDate.now(zoneId).plusDays(1));
                return new NextDrivePlan(localDateTime, weekPlan.getDailyPlan().getTargetSOC());
            }
        }
        return new NextDrivePlan(now.plusDays(1).withHour(8).withMinute(0).withSecond(0), BigDecimal.TEN.multiply(BigDecimal.TEN));
    }

    private void noticeMessage(IntelligentChargeScheduleJob job, String title, Locale locale, PushAppClient.MessageBehaviorEnum messageBehaviorEnum) {
        if (job.getSmartChargeSwitch() == BigDecimal.ZERO.intValue()) {
            return;
        }
        String evseSn = job.getEvseSn();
        String pileSN = evseSn.split("_")[0];
        PushAppClient.JPushMessage dto = new PushAppClient.JPushMessage();
        Map<String, String> messageExtras = new HashMap<>();
        messageExtras.put("messageBehavior", String.valueOf(messageBehaviorEnum.getValue()));
        messageExtras.put("sn", pileSN);
        messageExtras.put("primaryKey", job.getOrderSeq());
        messageExtras.put("deviceType", "1");
        dto.setMessageExtras(messageExtras);
        dto.setSave(true);
        dto.setTargetType(0);
        dto.setMsgContentType("text");
        List<String> targetList = new ArrayList<>();
        targetList.add(String.valueOf(job.getUserId()));
//        Set<Long> joinedFamilyUserIds = shareFamilyServiceAdapter.findJoinedFamilyUserIdsBySN(pileSN);
//        for (Long joinedFamilyUserId : joinedFamilyUserIds) {
//            targetList.add(joinedFamilyUserId.toString());
//        }
        dto.setTargetList(targetList);
        dto.setMessageId(IdWorker.getIdStr());
        dto.setMessageExtras(messageExtras);

        dto.setMsgContent(MessageSourceHolder.getMessage(messageBehaviorEnum.name(), new Object[]{}, locale));
        dto.setMsgTitle(MessageSourceHolder.getMessage(title, new Object[]{}, locale));
        log.info("pushAppClient: {}", JSON.toJSONString(dto));
        pushAppClientAdapter.getPushAppClient(dto);


    }

    /**
     * 修改配置后尝试重新调度
     *
     * @param config 配置
     * @return
     */
    @EventListener
    public boolean tryReschedule(MemberSmartChargeConfigEntity config) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(IntelligentChargeScheduleJob::getProfileId, config.getId());
        queryWrapper.eq(IntelligentChargeScheduleJob::getUserId, config.getUserId());
        queryWrapper.orderByDesc(IntelligentChargeScheduleJob::getId);
        queryWrapper.last("limit 1");
        IntelligentChargeScheduleJob job = intelligentChargeScheduleJobMapper.selectOne(queryWrapper);
        DeliveryByUserDTO deliveryByUserDTO = getDeliveryByUserDTO(config, job);
        Boolean aBoolean = opLocationPileGroupService.deliveryByUser(deliveryByUserDTO);
        if (Boolean.TRUE.equals(aBoolean)) {
            // 最好延迟调用
            doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job)));
        }
        return aBoolean;
    }

    protected boolean doTriggerIntelligentChargeJob(IntelligentChargeScheduleJob job, TriggerJobEventType triggerJobEventType) {
        log.info("doTriggerIntelligentChargeJob {}, {}", JSON.toJSONString(job), triggerJobEventType.name());
        if (Objects.isNull(job)) {
            log.info("job is null {}, {}", JSON.toJSONString(job), triggerJobEventType.name());
            return false;
        }
        MemberSmartChargeConfigEntity config = memberSmartChargeConfigMapper.selectById(job.getProfileId());
        DeliveryByUserDTO deliveryByUserDTO = getDeliveryByUserDTO(config, job);
        return opLocationPileGroupService.deliveryByUser(deliveryByUserDTO);
    }

    private BigDecimal calculateMaxPriceIndex(BigDecimal maxPrice) {
        BigDecimal maxPriceIndex = maxPrice.setScale(0, RoundingMode.HALF_UP);
        return maxPriceIndex.compareTo(maxPrice) <= 0 ? maxPriceIndex.add(BigDecimal.valueOf(0.5)) : maxPriceIndex;
    }

    /**
     * 下发智能充电策略配置
     *
     * @param job                        job
     * @param scheduleArithmeticResponse 算法
     * @return 下发是否成功
     */
    private boolean doDelivery(IntelligentChargeScheduleJob job, ScheduleArithmeticResponse scheduleArithmeticResponse) {
        log.info("job= {}", JSON.toJSONString(job));
        List<TimeSeriesIndex> timeSeriesIndexes = scheduleArithmeticResponse.getTimeSeriesIndex();
        if (CollectionUtils.isEmpty(timeSeriesIndexes)) {
            return true;
        }
        // 为减少数据量这里做一个合并
        TimeSeriesIndex preElement = null;
        Iterator<TimeSeriesIndex> iterator = timeSeriesIndexes.iterator();
        while (iterator.hasNext()) {
            TimeSeriesIndex next = iterator.next();
            if (Objects.nonNull(preElement) && preElement.getChargingUp().compareTo(next.getChargingUp()) == 0) {
                iterator.remove();
            }
            preElement = next;
        }
        log.info("doDelivery merged timeSeriesIndexes = {}", JSON.toJSONString(timeSeriesIndexes));
        List<PowerLimitItemDTO> powerLimitItemDTOList = new ArrayList<>();
        powerLimitItemDTOList.add(PowerLimitItemDTO.builder().startDuration(BigDecimal.ZERO.intValue()).power(BigDecimal.valueOf(6)).powerUnit(ChargingUpUnitEnum.A.getName()).build());
        ZonedDateTime zonedDateTime = Instant.ofEpochMilli(job.getTimestamp()).atZone(ZoneOffset.UTC).truncatedTo(ChronoUnit.DAYS);// 开始调度的UTC 0点
        long second = zonedDateTime.toEpochSecond();
        for (TimeSeriesIndex seriesIndex : timeSeriesIndexes) {
            long duration = seriesIndex.getTime().atZone(ZoneId.of(job.getZoneId())).toEpochSecond() - second;
            powerLimitItemDTOList.add(PowerLimitItemDTO.builder().startDuration(BigDecimal.valueOf(duration).intValue()).power(seriesIndex.getChargingUp()).powerUnit(ChargingUpUnitEnum.A.getName()).build());
        }
        PowerLimitParamDTO build = PowerLimitParamDTO.builder()
                .pileSn(job.getEvseSn().split("_")[0])
                .connectorId(Integer.valueOf(job.getEvseSn().split("_")[1]))
                .chargingProfileId(PROFILE_ID)
                .chargingRateUnit(ChargingUpUnitEnum.A.getName())
                .transactionId(Integer.valueOf(job.getOrderSeq()))
                .stackLevel(STACK_LEVEL)
                .chargingProfilePurpose(ProfileTypeEnum.TX_PROFILE.getName())
                .chargingProfileKind(ChargingProfiles.ChargingProfileKindEnum.ABSOLUTE.getValue())
                .phaseNum(getPhaseNum(job.getEvseSn().split("_")[0]))
                .powerLimitItemDTOList(powerLimitItemDTOList)
                .startSchedule(zonedDateTime.format(DateTimeFormatter.ofPattern(DEFAULT_LONG_PATTERN)))
                .build();
        Boolean deliver = smartChargeServiceAdapter.delivery(build);
        Boolean get = smartChargeServiceAdapter.get(build);
        return deliver && get;
    }

    private String getPhaseNum(String pileSn) {
        ChargePileDTO chargePileInfo = pileDeviceServiceAdapter.getChargePileInfo(pileSn);
        if (chargePileInfo != null) {
            Integer phase = chargePileInfo.getPhase();
            return NumberPhasesEnum.getPhaseNum(phase);
        }
        return null;
    }

    public static final String DEFAULT_LONG_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";

    public void triggerIntelligentChargeJob(Long parseInt) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.le(IntelligentChargeScheduleJob::getTimestamp, System.currentTimeMillis() - parseInt * 60 * 1000);
        queryWrapper.eq(IntelligentChargeScheduleJob::getSmartChargeSwitch, BigDecimal.ONE.intValue());
        queryWrapper.in(IntelligentChargeScheduleJob::getStatus, getExpectIntelligentChargeStatus());// 如果一开始都没有尝试智能充电成功那么 定时job就不看看
        List<IntelligentChargeScheduleJob> intelligentChargeScheduleJobs = intelligentChargeScheduleJobMapper.selectList(queryWrapper);
        log.info("triggerIntelligentChargeJob jobs size = {}", intelligentChargeScheduleJobs.size());
        XxlJobLogger.log("jobs size = {}", intelligentChargeScheduleJobs.size());
        for (IntelligentChargeScheduleJob job : intelligentChargeScheduleJobs) {
            try {
                log.info("handle job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                pushAppSOCFromMonitor(job);
                XxlJobLogger.log("handle job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                EnergyBillDetailVO energyBillDetailVO = pileBillServiceAdapter.selectBillBaseByBusId(job.getOrderSeq());
                Assert.isTrue(energyBillDetailVO.getEvseSn().equalsIgnoreCase(job.getEvseSn()), MessageSourceHolder.getMessage("400"));
                if (!OrderStatusEnum.START_SUCCESS.getValue().equals(energyBillDetailVO.getOrderStatus())) {
                    log.info("Order: {}  {},{} is not charging, set job terminate", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                    job.setStatus(JobStatus.TERMINATIONS.getStatus());
                    intelligentChargeScheduleJobMapper.updateById(job);
                    continue;
                }
                log.info("trigger triggerIntelligentChargeJob job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                XxlJobLogger.log("trigger job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                doTriggerIntelligentChargeJob(job, TriggerJobEventType.TIMER_JOB);
            } catch (Exception e) {
                log.error("triggerFailed  job={}", JSON.toJSONString(job), e);
                XxlJobLogger.log("triggerFailed job={}", JSON.toJSONString(job));
            }
        }
    }

    private Object[] getExpectIntelligentChargeStatus() {
        return new Object[]{JobStatus.RUNNING.getStatus(), JobStatus.TRY_SCHEDULE.getStatus(), JobStatus.LINK_SUCCESS.getStatus()};
    }

    /**
     * 识别车辆 在 还没有超时的订单 定时继续识别   这个时候的订单状态是 try_linked
     * 对于超时的 那么 就得设置成超时识别了
     *
     * @param offsetMin 识别汽车 超时时间  (单位 分钟)
     */
    public void triggerIdentifyJob(Integer offsetMin) {
        this.timeout = offsetMin * 60;
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(IntelligentChargeScheduleJob::getStatus, JobStatus.TRY_LINKED.getStatus());// 如果一开始都没有尝试智能充电成功那么 定时job就不看看
        List<IntelligentChargeScheduleJob> intelligentChargeScheduleJobs = intelligentChargeScheduleJobMapper.selectList(queryWrapper);
        XxlJobLogger.log("triggerIdentifyJob size = {}", intelligentChargeScheduleJobs.size());
        for (IntelligentChargeScheduleJob job : intelligentChargeScheduleJobs) {
            try {
                XxlJobLogger.log("handle  trigger Identify  job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                EnergyBillDetailVO energyBillDetailVO = pileBillServiceAdapter.selectBillBaseByBusId(job.getOrderSeq());
                Assert.isTrue(energyBillDetailVO.getEvseSn().equalsIgnoreCase(job.getEvseSn()), MessageSourceHolder.getMessage("400"));
                if (!OrderStatusEnum.START_SUCCESS.getValue().equals(energyBillDetailVO.getOrderStatus())) {
                    log.info("Order: {}  {},{} is not charging, set job terminate", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                    job.setStatus(JobStatus.TERMINATIONS.getStatus());
                    intelligentChargeScheduleJobMapper.updateById(job);
                    continue;
                }
                String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
                Long expire = stringRedisTemplate.getExpire(format, TimeUnit.MILLISECONDS);
                log.info("expire :{}  {}  {}  {} ", expire, job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                if (Objects.isNull(expire) || expire < 0) {
                    XxlJobLogger.log("timeout is short wait timeout trigger; format={}", format);
                    job.setStatus(JobStatus.FAILURE_VEHICLE.getStatus());//
                    intelligentChargeScheduleJobMapper.updateById(job);
                } else if (150000L > expire) {
                    XxlJobLogger.log("newSingleThreadScheduledExecutor; delay time = {}", expire);
                    String last = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
                    Boolean absent = stringRedisTemplate.opsForValue().setIfAbsent(last, job.getOrderSeq(), BigDecimal.ONE.longValue(), TimeUnit.HOURS);// 一定大于 150 000 毫秒
                    if (Boolean.TRUE.equals(absent)) {
                        log.info("newSingleThreadScheduledExecutor : {}  {}  {} ", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                        scheduledExecutorService.schedule(() -> doIdentify(job), expire - 2000, TimeUnit.MILLISECONDS);
                    }
                } else {
                    XxlJobLogger.log("fixed time schedule; delay time = {}", expire);
                    boolean identifyResult = doIdentify(job);
                    XxlJobLogger.log("triggerIdentifyJob ={} {} {} identifyResult={}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), identifyResult);
                }
            } catch (Exception e) {
                log.error("triggerIdentifyJob  Failed  job={}", JSON.toJSONString(job), e);
                XxlJobLogger.log("triggerIdentifyJob Failed job={}", JSON.toJSONString(job));
            }
        }
    }

    /**
     * 识别车辆 并读取SOC
     *
     * @param job job
     * @return true 识别出来  false 没有识别出
     */
    public boolean doIdentify(IntelligentChargeScheduleJob job) {
        log.info("doIdentify {}", JSON.toJSONString(job));
        String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        if (JobStatus.NEW.getStatus() == job.getStatus()) {
            log.info("try to link vehicle {}", job.getStatus());
            job.setStatus(JobStatus.TRY_LINKED.getStatus());
            intelligentChargeScheduleJobMapper.updateById(job);
        }
        if (JobStatus.TRY_LINKED.getStatus() != job.getStatus()) {
            log.info("canceled try to link vehicle {}", JobStatus.TRY_LINKED);
            return false;
        }
        AtomicBoolean returnValue = new AtomicBoolean(false);
        log.info("try to identify vehicle {}", JobStatus.TRY_LINKED);
        MemberSmartChargeConfigEntity intelligentChargeConfig = identify(job);
        if (Objects.nonNull(intelligentChargeConfig)) {
            log.info("monitorMessage getVehicleLinked Status success {}", job.getOrderSeq());
            returnValue.set(true);
            String msg = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.AUTH_VEHICLE_SUCCESS.getI18nKey(), ChargingScheduleStatusEnum.AUTH_VEHICLE_SUCCESS.getI18nValue());
            setValueExcludeExpire(format, msg);
            doExecuteTask(RunnableWrapper.of(() -> pushAppSOCFromMonitor(job)));
            doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job, locale)));
            webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), job.getVehicleId(), LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")) + "识别车辆信息成功" + JSON.toJSONString(intelligentChargeConfig));
        } else {
            // 如果 大于超时时间 那么 就设置为识别车辆识别 桩 failure_vehicle
            if (timeout * 1000 <= System.currentTimeMillis() - job.getTimestamp()) {
                log.info("identify Vehicle timeout {}", job.getOrderSeq());
                String msg = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.AUTH_VEHICLE_FAILED.getI18nKey(), ChargingScheduleStatusEnum.AUTH_VEHICLE_FAILED.getI18nValue());
                setValueExcludeExpire(format, msg);
                job.setStatus(JobStatus.FAILURE_VEHICLE.getStatus());//
                intelligentChargeScheduleJobMapper.updateById(job);
                webHookServiceAdapter.sendMessage(job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber(), LocalDateTime.now(ZoneId.of(job.getZoneId())).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")) + " 识别车辆信息失败");
            }
        }
        return returnValue.get();
    }

    /**
     * 推送SOC
     * 大于 等于 LINK_SUCCESS 且 profiledId>0 的都是连接成功的车辆  排除 结束的订单那么 就需要给推送SOC
     *
     * @param parseInt 多少小时之内的订单
     */
    public void triggerRefreshSOCJob(Long parseInt) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.gt(IntelligentChargeScheduleJob::getTimestamp, System.currentTimeMillis() - parseInt * 60 * 60 * 1000);
        queryWrapper.gt(IntelligentChargeScheduleJob::getProfileId, BigDecimal.ZERO.intValue()); // 代表识别出正在充电的车辆
        queryWrapper.ge(IntelligentChargeScheduleJob::getStatus, JobStatus.LINK_SUCCESS.getStatus()); // 连接成功
        queryWrapper.ne(IntelligentChargeScheduleJob::getStatus, JobStatus.FINISH.getStatus()); // 结束充电不再推送
        List<IntelligentChargeScheduleJob> intelligentChargeScheduleJobs = intelligentChargeScheduleJobMapper.selectList(queryWrapper);
        log.info("triggerRefreshSOCJob size = {}", intelligentChargeScheduleJobs.size());
        XxlJobLogger.log("triggerRefreshSOCJob size = {}", intelligentChargeScheduleJobs.size());
        for (IntelligentChargeScheduleJob job : intelligentChargeScheduleJobs) {
            try {
                log.info("handle trigger triggerRefreshSOC job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                XxlJobLogger.log("handle  trigger triggerRefreshSOC job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                log.info("trigger job={} {} {}", job.getOrderSeq(), job.getEvseSn(), job.getOrderNumber());
                doExecuteTask(RunnableWrapper.of(() -> pushAppSOCFromMonitor(job)));
            } catch (Exception e) {
                log.error("triggerIdentifyJob  Failed  job={}", JSON.toJSONString(job), e);
                XxlJobLogger.log("triggerRefreshSOC Failed job={}", JSON.toJSONString(job));
            }
        }
    }

    @Transactional
    public void cleanIntelligentChargeJob(Long parseInt) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.le(IntelligentChargeScheduleJob::getTimestamp, System.currentTimeMillis() - parseInt * 24 * 3600 * 1000);
        // queryWrapper.in(IntelligentChargeScheduleJob::getStatus, JobStatus.TERMINATIONS.getStatus(), JobStatus.FAILURE_NETWORK.getStatus(), JobStatus.FAILURE_VEHICLE.getStatus(), JobStatus.FINISH.getStatus(), JobStatus.QUICK.getStatus());
        List<IntelligentChargeScheduleJob> intelligentChargeScheduleJobs = intelligentChargeScheduleJobMapper.selectList(queryWrapper);

        if (!CollectionUtils.isEmpty(intelligentChargeScheduleJobs)) {
            LambdaQueryWrapper<IntelligentChargeScheduleRecord> queryRecordWrapper = new LambdaQueryWrapper<>();
            List<Long> collect = intelligentChargeScheduleJobs.stream().map(IntelligentChargeScheduleJob::getId).collect(Collectors.toList());
            queryRecordWrapper.in(IntelligentChargeScheduleRecord::getJobId, collect);
            intelligentChargeScheduleRecordMapper.delete(queryRecordWrapper);
            intelligentChargeScheduleJobMapper.deleteBatchIds(collect);
        }
        LambdaQueryWrapper<IntelligentChargeScheduleRecord> queryWrapperRecord = new LambdaQueryWrapper<>();
        ZonedDateTime val = Instant.ofEpochMilli(System.currentTimeMillis() - parseInt * 24 * 3600 * 1000).atZone(ZoneId.systemDefault());
        String format = val.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        queryWrapperRecord.le(IntelligentChargeScheduleRecord::getScheduleTime, format);
        intelligentChargeScheduleRecordMapper.delete(queryWrapperRecord);
    }

    public ChargingSchedulePlanDTO schedulePlan(ChargingTaskDTO chargingTaskDTO) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> queryJobWrapper = new LambdaQueryWrapper<>();
        queryJobWrapper.eq(IntelligentChargeScheduleJob::getOrderSeq, chargingTaskDTO.getTransactionId());
        IntelligentChargeScheduleJob scheduleJob = intelligentChargeScheduleJobMapper.selectOne(queryJobWrapper);
        if (Objects.nonNull(scheduleJob)) {

            MemberSmartChargeConfigEntity entity = memberSmartChargeConfigMapper.selectById(scheduleJob.getProfileId());

            ChargingSchedulePlanDTO chargingPlan = new ChargingSchedulePlanDTO();
            SmartChargeProfile profile = entity.getProfile();
            chargingPlan.setProfile(profile);
            chargingPlan.setStartSOC(profile.getStartSOC());
            ZoneId zoneId = ZoneId.of(scheduleJob.getZoneId());
            chargingPlan.setTargetSOC(profile.getTargetSOC());

            chargingPlan.setStartTime(scheduleJob.getOpeningZonedTime().substring(0, 16));
            String key = RedisKeyConstant.getSmartChargeOptionalKey(scheduleJob.getEvseSn());
            String fast = stringRedisTemplate.opsForValue().get(key);
            chargingPlan.setPriority(PRIORITY_FLAG.equalsIgnoreCase(fast));

            // 开始充电时间
            LocalDateTime beginCharging = LocalDateTime.parse(scheduleJob.getOpeningZonedTime(), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
            LocalDateTime zonedDateTime = beginCharging.atZone(zoneId).withHour(profile.getDriveTime().getHour()).withMinute(profile.getDriveTime().getMinute()).withSecond(0).toLocalDateTime();
            if (zonedDateTime.isBefore(beginCharging)) {
                chargingPlan.setPredictStopTime(zonedDateTime.plusDays(1));
            } else {
                chargingPlan.setPredictStopTime(zonedDateTime);
            }
            // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓电价↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
            List<TimeDecimalValue> prices = new ArrayList<>();
            Long rootByPileSn = opLocationPileGroupRepository.findRootByPileSn(scheduleJob.getEvseSn().split("_")[0]);
            if (Objects.nonNull(rootByPileSn)) {
                OpLocationPileGroupEntity opLocationPileGroupEntity = opLocationPileGroupMapper.selectById(rootByPileSn);
                List<ElectricityPrice> json = opLocationPileGroupEntity.getPrices();
                List<ElectricityPrice> pricesList = JSON.parseArray(JSON.toJSONString(json), ElectricityPrice.class);

                List<TimeDecimalValue> futurePrice = futurePrice(beginCharging, chargingPlan.getPredictStopTime(), pricesList);
                prices.addAll(futurePrice);
                chargingPlan.setCurrency(pileTariffServiceAdapter.getCurrencyListBySellerId(opLocationPileGroupEntity.getMerchantId()));
            } else {
                log.error("充电组没有查询到 {}", JSON.toJSONString(chargingTaskDTO));
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
            }
            chargingPlan.setPrice(prices);
            BigDecimal maxPrice = prices.stream().map(TimeDecimalValue::getValue).max(BigDecimal::compareTo).orElse(BigDecimal.TEN);
            chargingPlan.setMaxPrice(calculateMaxPriceIndex(maxPrice));

            // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑电价↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

            // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓充电计划图↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
            ProfileParamDTO profileParamDTO = new ProfileParamDTO();
            profileParamDTO.setEvseSn(scheduleJob.getEvseSn());
            profileParamDTO.setTransactionId(scheduleJob.getOrderSeq());
            ChargingGraphVO chargingGraphVO = smartChargeServiceAdapter.graphList(profileParamDTO);

            // 充电时间区间
            Long planEndTime = chargingGraphVO.getPlanEndTime();

            LocalDateTime planEndDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(planEndTime), ZoneId.of(scheduleJob.getZoneId()));
            ChargingDuration chargingDuration = new ChargingDuration();
            chargingDuration.setStartTime(beginCharging);
            chargingDuration.setEndTime(planEndDateTime);
            chargingPlan.setChargingDurations(Collections.singletonList(chargingDuration));

            List<TimeSeriesIndex> timeSeriesIndices = new ArrayList<>();
            LocalDateTime now = LocalDateTime.now(zoneId);
            Long actureTime = System.currentTimeMillis();
            //////////// 已经充电的时间段 从 monitor 读取
            List<OpEvseMeterUploadDTO> opEvseMeterUploadDTOS = pileMonitorServiceAdapter.queryMeterByBusId(scheduleJob.getOrderSeq(), beginCharging.atZone(zoneId).toEpochSecond() * 1000);
            if (!CollectionUtils.isEmpty(opEvseMeterUploadDTOS)) {
                for (OpEvseMeterUploadDTO opEvseMeterUploadDTO : opEvseMeterUploadDTOS) {
                    if (Objects.isNull(opEvseMeterUploadDTO) || Objects.isNull(opEvseMeterUploadDTO.getBatterySoc()) || BigDecimal.ZERO.compareTo(opEvseMeterUploadDTO.getBatterySoc()) >= 0) {
                        continue;
                    }
                    LocalDateTime localDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(opEvseMeterUploadDTO.getCreateTime()), zoneId);
                    TimeSeriesIndex timeSeriesIndex = new TimeSeriesIndex();
                    timeSeriesIndex.setSoc(opEvseMeterUploadDTO.getBatterySoc());
                    timeSeriesIndex.setTime(localDateTime);
                    timeSeriesIndex.setPrice(calcPrice(prices, timeSeriesIndex.getTime()));
                    if (now.isAfter(timeSeriesIndex.getTime()) && !timeSeriesIndices.contains(timeSeriesIndex)) {
                        timeSeriesIndices.add(timeSeriesIndex);
                        actureTime = opEvseMeterUploadDTO.getCreateTime();
                    }
                }
            }

            //  ↓↓↓↓↓↓↓↓↓↓↓为空 说明  桩和车上都没有读取到 soc 那么就使用 上次 算法算出的soc↓↓↓↓↓↓↓↓↓↓↓

            if (CollectionUtils.isEmpty(timeSeriesIndices)) {
                for (ChargingDetailVO chargingDetailVO : chargingGraphVO.getSocList()) {
                    TimeSeriesIndex timeSeriesIndex = new TimeSeriesIndex();
                    timeSeriesIndex.setSavedEnergyFee(chargingDetailVO.getSavedEnergyFee());
                    timeSeriesIndex.setSoc(BigDecimal.valueOf(chargingDetailVO.getSoc()));
                    LocalTime localTime = LocalTime.parse(chargingDetailVO.getBeginTime(), DateTimeFormatter.ofPattern("HH:mm:ss"));
                    LocalDateTime localDateTime = beginCharging.withHour(localTime.getHour()).withMinute(localTime.getMinute());
                    if (localDateTime.isBefore(beginCharging)) {
                        timeSeriesIndex.setTime(localDateTime.plusDays(1));
                    } else {
                        timeSeriesIndex.setTime(localDateTime);
                    }
                    timeSeriesIndex.setPrice(calcPrice(prices, timeSeriesIndex.getTime()));
                    if (now.isAfter(timeSeriesIndex.getTime()) && !timeSeriesIndices.contains(timeSeriesIndex)) {
                        timeSeriesIndices.add(timeSeriesIndex);
                        actureTime = chargingDetailVO.getStartPeriod();
                    }
                }
            }
            //  ↓↓↓↓↓↓↓↓↓↓↓ 未来预计充的时间段  ↓↓↓↓↓↓↓↓↓↓↓
            List<ChargingDetailVO> socList = chargingGraphVO.getSocList();
            if (!CollectionUtils.isEmpty(socList)) {
                for (ChargingDetailVO chargingDetailVO : socList) {
                    TimeSeriesIndex timeSeriesIndex = new TimeSeriesIndex();
                    timeSeriesIndex.setSavedEnergyFee(chargingDetailVO.getSavedEnergyFee());
                    timeSeriesIndex.setSoc(BigDecimal.valueOf(chargingDetailVO.getSoc()));
                    LocalTime localTime = LocalTime.parse(chargingDetailVO.getBeginTime(), DateTimeFormatter.ofPattern("HH:mm:ss"));
                    LocalDateTime localDateTime = beginCharging.withHour(localTime.getHour()).withMinute(localTime.getMinute());
                    if (localDateTime.isBefore(beginCharging)) {
                        timeSeriesIndex.setTime(localDateTime.plusDays(1));
                    } else {
                        timeSeriesIndex.setTime(localDateTime);
                    }
                    timeSeriesIndex.setPrice(calcPrice(prices, timeSeriesIndex.getTime()));
                    if ((actureTime + 60 * 1000) < chargingDetailVO.getStartPeriod() && !timeSeriesIndices.contains(timeSeriesIndex)
                            && (now.isBefore(timeSeriesIndex.getTime()) || now.isEqual(timeSeriesIndex.getTime()))) {
                        timeSeriesIndices.add(timeSeriesIndex);
                    }
                }
                chargingPlan.setSavedEnergyFee(socList.get(socList.size() - 1).getSavedEnergyFee());
            }

            // 最后 价格
            if (!CollectionUtils.isEmpty(timeSeriesIndices)) {
                TimeSeriesIndex lastTimeSeriesIndex = timeSeriesIndices.get(timeSeriesIndices.size() - 1);
                LocalDateTime lastTime = lastTimeSeriesIndex.getTime();
                BigDecimal lastSoc = lastTimeSeriesIndex.getSoc();
                BigDecimal savedEnergyFee = lastTimeSeriesIndex.getSavedEnergyFee();
                for (TimeDecimalValue price : prices) {
                    LocalDateTime parse = LocalDateTime.parse(price.getTime(), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"));
                    if (parse.isAfter(lastTime) && (parse.isBefore(chargingPlan.getPredictStopTime()))) {
                        TimeSeriesIndex timeSeriesIndex = new TimeSeriesIndex();
                        timeSeriesIndex.setSavedEnergyFee(savedEnergyFee);
                        timeSeriesIndex.setSoc(lastSoc);
                        timeSeriesIndex.setPrice(price.getValue());
                        timeSeriesIndex.setTime(parse);
                        timeSeriesIndices.add(timeSeriesIndex);
                    }
                }
                TimeSeriesIndex timeSeriesIndex = new TimeSeriesIndex();
                timeSeriesIndex.setSavedEnergyFee(savedEnergyFee);
                timeSeriesIndex.setSoc(lastSoc);
                timeSeriesIndex.setPrice(calcPrice(prices, chargingPlan.getPredictStopTime()));
                timeSeriesIndex.setTime(chargingPlan.getPredictStopTime());
                timeSeriesIndices.add(timeSeriesIndex);
            }
            chargingPlan.setTimeSeriesIndex(timeSeriesIndices);
            return chargingPlan;
        }
        return null;
    }

    private static BigDecimal calcPrice(List<TimeDecimalValue> prices, LocalDateTime timeFull) {
        LocalDateTime time = timeFull.truncatedTo(ChronoUnit.MINUTES);
        for (int i = 0; i < prices.size() - 1; i++) {
            TimeDecimalValue timeDecimalValue = prices.get(i);
            LocalDateTime localDateTime = timeDecimalValue.getLocalDateTime().truncatedTo(ChronoUnit.MINUTES);
            LocalDateTime priceEndTime = prices.get(i + 1).getLocalDateTime().truncatedTo(ChronoUnit.MINUTES);
            if ((localDateTime.isBefore(time) || localDateTime.isEqual(time)) && priceEndTime.isAfter(time)) {
                return timeDecimalValue.getValue();
            }
        }
        LocalDateTime localDateTime = prices.get(prices.size() - 1).getLocalDateTime().truncatedTo(ChronoUnit.MINUTES);
        if (localDateTime.isBefore(time) || localDateTime.isEqual(time)) {
            return prices.get(prices.size() - 1).getValue();
        }
        log.warn("calcPrice {} {}", JSON.toJSONString(prices), timeFull.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
        return BigDecimal.ZERO;
    }


    private final LoadingCache<String, Optional<LocationVO>> pileLocationVOCache = CacheBuilder.newBuilder()
            .expireAfterWrite(1, TimeUnit.DAYS)
            .initialCapacity(100)
            .maximumSize(5000)
            .build(new CacheLoader<String, Optional<LocationVO>>() {
                @Override
                public Optional<LocationVO> load(String sn) {
                    return Optional.ofNullable(loadPileLocationVO(sn));
                }
            });


    private double getDistanceMeter(String sn, LocationVO locationVO) {
        LocationVO pileLocationVO = getPileLocationVO(sn);
        if (Objects.nonNull(pileLocationVO) && Objects.nonNull(pileLocationVO.getLatitude()) && Objects.nonNull(pileLocationVO.getLongitude())
                && Objects.nonNull(locationVO) && Objects.nonNull(locationVO.getLatitude()) && Objects.nonNull(locationVO.getLongitude())) {
            GlobalCoordinates source = new GlobalCoordinates(pileLocationVO.getLatitude(), pileLocationVO.getLongitude());
            GlobalCoordinates target = new GlobalCoordinates(locationVO.getLatitude(), locationVO.getLongitude());
            try {
                return geodeticCalculator.calculateGeodeticCurve(Ellipsoid.Sphere, source, target).getEllipsoidalDistance();
            } catch (Exception e) {
                log.error("calculateGeodeticCurve", e);
            }
        }
        return -1;
    }

    private LocationVO getPileLocationVO(String sn) {
        try {
            LocationVO locationVO = pileLocationVOCache.get(sn).orElse(null);
            log.info("getPileLocationVO {} {}", sn, JSON.toJSONString(locationVO));
            return locationVO;
        } catch (ExecutionException e) {
            log.warn("getPileLocationVO {} failed!", sn);
            log.error("getPileLocationVO failed!", e);
        }
        return loadPileLocationVO(sn);
    }

    public LocationVO loadPileLocationVO(String sn) {
        try {
            List<OpLocationEvseElasticDTO> allByPileSn = opLocationEvseElastic.findAllByPileSn(sn);
            if (!CollectionUtils.isEmpty(allByPileSn)) {
                log.info("sn={} allByPileSn={} ", sn, JSON.toJSONString(allByPileSn));
                OpLocationEvseElasticDTO opLocationEvseElasticDTO = allByPileSn.get(0);
                if (Objects.nonNull(opLocationEvseElasticDTO)) {
                    Double longitude = Double.valueOf(opLocationEvseElasticDTO.getLongitude());
                    Double latitude = Double.valueOf(opLocationEvseElasticDTO.getLatitude());
                    log.info("sn={} longitude = {} latitude={} ", sn, longitude, latitude);
                    return new LocationVO(longitude, latitude);
                }
            }
        } catch (Exception e) {
            log.error("loadPileLocationVO  failed" + sn, e);
        }
        return null;
    }

    public void enableSmartCharge(IntelligentChargeScheduleJob job) {
        // 调用用卢愈全的 开启智能充电
        job.setSmartChargeStatus(SmartChargeStatus.USER_SET_ENABLE_STATUS);
        job.setSmartChargeSwitch(job.getSmartChargeSwitch());
        intelligentChargeScheduleJobMapper.updateById(job);
        //  切换智能充电倒计时
        String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
        String message = MessageSourceHolder.getMessage(ChargingScheduleStatusEnum.TRY_SCHEDULE_INTELLIGENT_CHARGING.getI18nKey(), ChargingScheduleStatusEnum.TRY_SCHEDULE_INTELLIGENT_CHARGING.getI18nValue());
        stringRedisTemplate.opsForValue().set(format, message, trySmartChargeTimeout, TimeUnit.SECONDS);
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job, locale)));
        log.info("enableSmartCharge {} !", JSON.toJSONString(job));
        if (job.getSmartChargeSwitch() == 1) { // 开了智能充电的发送
            sendDelayMessage(RabbitMQConfig.X_EXCHANGE_START_SMART_CHARGING + RabbitBean.RABBITMQ_VERSION_SUFFIX, TTL_ROUTING_KEY, timeout * 1000, job.getId().toString());
        }
        SmartChargeConfigQueryDTO smartChargeConfigQueryDTO = new SmartChargeConfigQueryDTO();
        smartChargeConfigQueryDTO.setLocationId(job.getLocationId());
        smartChargeConfigQueryDTO.setTransactionId(job.getOrderSeq());
        smartChargeConfigQueryDTO.setUserId(job.getUserId());
        smartChargeConfigQueryDTO.setEvseSn(job.getEvseSn());
        MemberSmartChargeConfigEntity config = memberSmartChargeConfigService.queryByUserIdAndLocationId(smartChargeConfigQueryDTO);
        DeliveryByUserDTO deliveryByUserDTO = getDeliveryByUserDTO(config, job);
        Boolean aBoolean = opLocationPileGroupService.deliveryByUser(deliveryByUserDTO);
        ChargingDetailVO lastChargingDetailVO = smartChargeServiceAdapter.findLastChargingDetailVO(job.getEvseSn(), job.getOrderSeq());
        if (Boolean.TRUE.equals(aBoolean) && Objects.nonNull(lastChargingDetailVO)) {
            job.setSmartChargeStatus(SmartChargeStatus.SWITCH_SUCCESS_STATUS);
            if (JobStatus.QUICK.getStatus() == job.getStatus()) {
                job.setStatus(JobStatus.RUNNING.getStatus());
            }
            intelligentChargeScheduleJobMapper.updateById(job);
            doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job, locale)));
            doExecuteTask(RunnableWrapper.of(() -> noticeMessage(job, "ENABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SWITCH_SMART_CHARGING_SUCCESS)));
        }
        // 如果失败  延迟队列会最后一次尝试
    }

    public void disableSmartCharge(IntelligentChargeScheduleJob job) {
        // 调用用卢愈全的 开启智能充电
        job.setSmartChargeStatus(SmartChargeStatus.PILE_SUPPORT_STATUS);
        job.setSmartChargeSwitch(job.getSmartChargeSwitch());
        intelligentChargeScheduleJobMapper.updateById(job);
        SmartChargeConfigQueryDTO smartChargeConfigQueryDTO = new SmartChargeConfigQueryDTO();
        smartChargeConfigQueryDTO.setLocationId(job.getLocationId());
        smartChargeConfigQueryDTO.setTransactionId(job.getOrderSeq());
        smartChargeConfigQueryDTO.setUserId(job.getUserId());
        MemberSmartChargeConfigEntity config = memberSmartChargeConfigService.queryByUserIdAndLocationId(smartChargeConfigQueryDTO);
        DeliveryByUserDTO deliveryByUserDTO = getDeliveryByUserDTO(config, job);
        Boolean aBoolean = opLocationPileGroupService.deliveryByUser(deliveryByUserDTO);
        String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, job.getOrderSeq());
        log.info("disableSmartCharge delete cache {}", stringRedisTemplate.delete(format));
        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
        doExecuteTask(RunnableWrapper.of(() -> pushAppMessage(job, locale)));
        log.info("disableSmartCharge {} result {} !", JSON.toJSONString(job), aBoolean);
    }

    public void switchSmartCharge(JobQuery jobQuery) {
        log.info("  {}", JSON.toJSONString(jobQuery));
        LambdaQueryWrapper<IntelligentChargeScheduleJob> wrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class);
        wrapper.eq(IntelligentChargeScheduleJob::getOrderSeq, jobQuery.getTransactionId());
        IntelligentChargeScheduleJob job = intelligentChargeScheduleJobMapper.selectOne(wrapper);
        Assert.notNull(job, "expire job" + JSON.toJSONString(jobQuery));
        job.setSmartChargeSwitch(jobQuery.getSwitchSmartCharge());
        EnergyBillDetailVO energyBillDetailVO = pileBillServiceAdapter.selectBillBaseByBusId(jobQuery.getTransactionId());
        String evseSn = job.getEvseSn();
        if (Objects.isNull(energyBillDetailVO) || !energyBillDetailVO.getEvseSn().equalsIgnoreCase(evseSn)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        if (jobQuery.getSwitchSmartCharge() == 0) {
            disableSmartCharge(job);
        } else {
            enableSmartCharge(job);
        }
    }

//    public void switchSuccessSmartCharge(JobQuery jobQuery) {
//        LambdaQueryWrapper<IntelligentChargeScheduleJob> wrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class);
//        wrapper.eq(IntelligentChargeScheduleJob::getOrderSeq, jobQuery.getTransactionId());
//        IntelligentChargeScheduleJob job = intelligentChargeScheduleJobMapper.selectOne(wrapper);
//        job.setSmartChargeStatus(SmartChargeStatus.SWITCH_SUCCESS_STATUS);
//        if (JobStatus.QUICK.getStatus() == job.getStatus()) {
//            job.setStatus(JobStatus.RUNNING.getStatus());
//        }
//        intelligentChargeScheduleJobMapper.updateById(job);
//        Locale locale = appUserServiceAdapter.getUserLocaleSettings(job.getUserId());
//        doExecuteTask(RunnableWrapper.of(() -> pushAppScheduleMessage(job, locale)));
//        doExecuteTask(RunnableWrapper.of(() -> noticeMessage(job, "ENABLE_INTELLIGENT_CHARGING", locale, PushAppClient.MessageBehaviorEnum.SWITCH_SMART_CHARGING_SUCCESS)));
//    }
}
