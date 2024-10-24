package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.IdUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.common.util.LocaleResultUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.webhook.LarkClient;
import com.autel.cloud.ems.BasicDTO;
import com.autel.cloud.ems.constant.RedisConstant;
import com.autel.cloud.ems.dto.PileGunDTO;
import com.autel.cloud.ems.feign.EmsExchangeClient;
import com.autel.cloud.edge.EdgeFeignClient;
import com.autel.cloud.edge.dto.edge.UpdateGatewayDTO;
import com.autel.cloud.ems.BasicDTO;
import com.autel.cloud.ems.constant.RedisConstant;
import com.autel.cloud.ems.dto.UpdateGroupInfo;
import com.autel.cloud.ems.feign.EmsExchangeClient;
import com.autel.cloud.meter.data.api.dto.MeasuredDTO;
import com.autel.cloud.meter.data.api.feign.MeterDataClient;
import com.autel.cloud.meter.data.api.vo.MeterDataRecordVO;
import com.autel.cloud.monitor.dto.MeterValueStatisticDTO;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.EvseMeterValueFeignClient;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.monitor.vo.MeterValueStatisticVO;
import com.autel.cloud.pile.base.VipPolicy;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.context.DeliveryContext;
import com.autel.cloud.pile.base.domain.context.DistributeContext;
import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;
import com.autel.cloud.pile.base.domain.convert.EmsGroupConvert;
import com.autel.cloud.pile.base.domain.convert.GroupPileEvseTypeMapper;
import com.autel.cloud.pile.base.domain.convert.OncePerRequestConvert;
import com.autel.cloud.pile.base.domain.convert.SmartChargeGroupTypeMapper;
import com.autel.cloud.pile.base.domain.model.ControlContentDTO;
import com.autel.cloud.pile.base.domain.model.DemandControlAddConfigDTO;
import com.autel.cloud.pile.base.domain.model.DemandControlUpdateConfigDTO;
import com.autel.cloud.pile.base.domain.model.LocationPileDTO;
import com.autel.cloud.pile.base.domain.model.vo.TransactionInfoVo;
import com.autel.cloud.pile.base.domain.repository.LocationMeterRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupAssociateRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileVipConfigRepository;
import com.autel.cloud.pile.base.domain.service.*;
import com.autel.cloud.pile.base.domain.utils.TransactionUtils;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingParam;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingResult;
import com.autel.cloud.pile.base.dto.pile.GroupTypeDto;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.enums.meter.MeterbrandEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.ChargeScheduleClient;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.SmartBiClient;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileMonitorServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ArithmeticChargingParamDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DemandControlConfigEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.DlbUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.TransactionInfoVO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.bill.dto.SendMsgDto;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.feign.IOrderServiceFeign;
import com.autel.cloud.pile.bill.vo.*;
import com.autel.cloud.pile.user.api.dto.MemberGroupVipDTO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.MemberGroupVipVO;
import com.autel.cloud.smart.charge.constant.RedisKey;
import com.autel.cloud.smart.charge.dto.DeliveryParamDTO;
import com.autel.cloud.smart.charge.dto.HistoryParamDTO;
import com.autel.cloud.smart.charge.dto.ProfileParamDTO;
import com.autel.cloud.smart.charge.feign.SmartChargeFeign;
import com.autel.cloud.smart.charge.vo.ChargingGraphVO;
import com.autel.cloud.smart.charge.vo.ChargingProfileSettingVO;
import com.autel.cloud.smart.charge.vo.OpChargingSettingVO;
import com.autel.cloud.smart.monitor.dto.EvseMonitorMistakeDTO;
import com.autel.cloud.smart.monitor.dto.RedeliveryDTO;
import com.autel.cloud.tariff.dto.TariffComputeDTO;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelRuleEntityVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.google.common.base.Functions;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StopWatch;

import javax.annotation.Resource;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.BaseConstant.EMS_GROUP_TYPE;
import static java.lang.Long.parseLong;
import static java.lang.Long.valueOf;

/**
 * @ClassName OpLocationPileGroupServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/7/13 16:50
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class OpLocationPileGroupServiceImpl implements OpLocationPileGroupService, CommandLineRunner {

    @Value("${levelLimit:2}")
    private int levelLimit;

    @Resource
    private OpLocationPileVipConfigRepository opLocationPileVipConfigRepository;
    @Resource
    @Lazy
    OpLocationPileGroupRepository opLocationPileGroupRepository;
    @Resource
    private OpLocationPileGroupAssociateRepository opLocationPileGroupAssociateRepository;
    @Resource
    private SmartChargeFeign smartChargeFeign;
    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;
    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;
    @Resource
    private IntelligentChargeScheduleJobService intelligentChargeScheduleJobService;

    @Resource
    private OpLocationPileEvseService opLocationPileEvseService;
    @Resource
    private OpLocationEvseService opLocationEvseService;
    @Resource
    private DistributeContext distributeContext;
    @Resource
    private OpLocationService opLocationService;
    @Resource
    private MonitorFeignClient monitorFeignClient;
    @Resource
    private PileMonitorServiceAdapter pileMonitorServiceAdapter;
    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String, Object> redisTemplate;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private LocationMeterService locationMeterService;
    @Resource
    private HomePileFeignClient homePileFeignClient;
    @Resource
    private DeliveryContext deliveryContext;
    @Resource
    private MeterDataClient meterDataClient;
    @Resource
    private DeviceServiceFeign deviceServiceFeign;
    @Value("${spring.rabbitmq.version.suffix:}")
    private String rabbitmqVersionSuffix;
    @Resource
    private RabbitTemplate rabbitTemplate;
    @Resource
    private IBillFeignClient billFeignClient;
    @Resource
    private LocationMeterRepository locationMeterRepository;
    @Resource
    private OncePerRequestConvert oncePerRequestConvert;
    @Resource
    private TariffFeignClient tariffFeignClient;
    @Resource
    private SmartBiClient smartBiClient;
    @Resource
    private PileUserFeign pileUserFeign;
    @Resource
    private IOrderServiceFeign iOrderServiceFeign;
    @Resource
    private EvseMeterValueFeignClient evseMeterValueFeignClient;
    @Resource
    private DemandControlConfigService demandControlConfigService;
    @Resource
    private MemberSmartChargeConfigService memberSmartChargeConfigService;
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;
    @Resource
    EmsExchangeClient emsExchangeClient;
    @Resource
    private ChargeScheduleClient chargeScheduleClient;
    @Autowired
    private EdgeFeignClient edgeFeignClient;
    @Value("${ems.deliver.ratio:0.3}")
    private String emsDeliverRatio;
    @Resource
    private LarkClient larkClient;
    @Value("${webhook.lark.meter.offline:1849dc3b-008a-4f47-8f36-dd2ba5f10e00}")
    private String webhookKey;
    @Value("${ems.blacklist.sellIds:1697165343801135105}")
    private List<String> blacklistSellIds;

    private final String eastronKey = "energy:pile:base:eastron:meter:info";

    public static final String ENERGY_PILE_BASE_CHARGING_UP_EMS_GROUP_ID = "energy:pile:base:chargingUp:ems:groupId:";
    // 记录电表 id 的缓存 key
    public static final String METER_OFFLINE_EXPIRATION_TIME_KEY = "energy:pile:base:meter:offline:";

    public static final String CHARGE_PILE_GUN_KEY = "ENERGY:CHARGE:PILE_GUN:%s:%s";


    public OpLocationPileGroupServiceImpl(OpLocationPileGroupRepository opLocationPileGroupRepository) {
        this.opLocationPileGroupRepository = opLocationPileGroupRepository;
    }


    @Resource(name = "bizQueryTaskExecutor")
    private ThreadPoolTaskExecutor queryExecuor;
    private boolean checkGroupNameUnique(OpLocationPileGroupEntity opLocationPileGroupEntity) {
        LambdaQueryWrapper<OpLocationPileGroupEntity> lqw = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class)
                .eq(OpLocationPileGroupEntity::getMerchantId, opLocationPileGroupEntity.getMerchantId())
                .ne(Objects.nonNull(opLocationPileGroupEntity.getId()), OpLocationPileGroupEntity::getId, opLocationPileGroupEntity.getId())
                .eq(OpLocationPileGroupEntity::getName, opLocationPileGroupEntity.getName())
                .eq(OpLocationPileGroupEntity::getDeleted, 0);
        return com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileGroupRepository.getBaseMapper().selectList(lqw));
    }


    private boolean checkControlNameUnique(OpLocationPileGroupEntity opLocationPileGroupEntity) {
        LambdaQueryWrapper<OpLocationPileGroupEntity> lqw = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class)
                .eq(OpLocationPileGroupEntity::getMerchantId, opLocationPileGroupEntity.getMerchantId())
                .ne(Objects.nonNull(opLocationPileGroupEntity.getId()), OpLocationPileGroupEntity::getId, opLocationPileGroupEntity.getId())
                .eq(OpLocationPileGroupEntity::getName, opLocationPileGroupEntity.getName())
                .eq(OpLocationPileGroupEntity::getDeleted, 0);
        return com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileGroupRepository.getBaseMapper().selectList(lqw));
    }

    /**
     * @param smartChargeGroupConfigUpdateParamDTO
     * @return OpLocationPileGroupService的 update
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> updateV3(SmartChargeGroupConfigUpdateParamDTOcopy smartChargeGroupConfigUpdateParamDTO) {
        log.info("updateV3 {}", JSON.toJSONString(smartChargeGroupConfigUpdateParamDTO));
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.getById(smartChargeGroupConfigUpdateParamDTO.getId());
        if (entity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        Long ordinaryRootId = opLocationPileGroupRepository.findOrdinaryRoot(smartChargeGroupConfigUpdateParamDTO.getId());
        OpLocationPileGroupEntity one = opLocationPileGroupRepository.findOne(ordinaryRootId);
        if (Objects.equals(one.getGroupType(), EMS_GROUP_TYPE)) {
            //根据电表ID查询电表信息
            List<LocationMeterEntity> LocationMeters=locationMeterService.getByMeterIds(Collections.singletonList(smartChargeGroupConfigUpdateParamDTO.getMeterId()));
            //过滤掉brandEnum为0的电表数据，即Enegic品牌电表
            List<Long> enegicMeterIds=LocationMeters.stream().filter(v->v.getBrandEnum()==0).map(LocationMeterEntity::getId).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(enegicMeterIds)) {
                throw new MessageCodeException(PileBaseEnum.WRONG_METER_EMS_GROUP);
            }
        }
        smartChargeGroupConfigUpdateParamDTO.setStatus(entity.getStatus());// 编辑的时候 这个没有编辑内容内  还是沿用 历史状态
        OpLocationPileGroupEntity opLocationPileGroupEntity = getPileGroupByConditionV3(smartChargeGroupConfigUpdateParamDTO);
        if (opLocationPileGroupEntity != null) {
            throw new MessageCodeException(PileBaseEnum.HAS_SAME_GROUP_NAME_IN_LOCATION);
        }

        // 找出本次编辑 移除的pileSNs  移除的子组
        Set<Long> includeSubGroupIds = smartChargeGroupConfigUpdateParamDTO.getPileGroupList().stream().map(PileGroupTreeVOcopy::getGroupId).filter(Objects::nonNull).collect(Collectors.toSet());
        Set<String> includeSubPileSns = smartChargeGroupConfigUpdateParamDTO.getPileGroupList().stream().map(PileGroupTreeVOcopy::getPileSn).filter(Objects::nonNull).collect(Collectors.toSet());
        for (String item : includeSubPileSns) {
            /*if (smartChargeGroupConfigUpdateParamDTO.getLoadType() == 1 && !item.startsWith("A")) {
                throw new MessageCodeException(PileBaseEnum.ALM_ONLY_SUPPORT_AC_PILE);
            }*/
        }
        Set<String> removedSubPileSns = opLocationPileGroupRepository.findRemovePileSns(smartChargeGroupConfigUpdateParamDTO.getMerchantId(), smartChargeGroupConfigUpdateParamDTO.getId(), includeSubPileSns);
        Set<Long> removedSubGroupIds = opLocationPileGroupRepository.findRemoveSubGroupIds(smartChargeGroupConfigUpdateParamDTO.getMerchantId(), smartChargeGroupConfigUpdateParamDTO.getId(), includeSubGroupIds);
        smartChargeGroupConfigUpdateParamDTO.setRemovePileSns(removedSubPileSns);
        smartChargeGroupConfigUpdateParamDTO.setRemoveGroupIds(removedSubGroupIds);
        List<OpLocationPileGroupAssociateEntity> deleteList = null;
        if (!CollectionUtils.isEmpty(removedSubPileSns)) {
            deleteList = this.opLocationPileGroupAssociateRepository.findList(new ArrayList<>(removedSubPileSns));
        }

        this.removeKey(smartChargeGroupConfigUpdateParamDTO.getId(), null);
        opLocationPileGroupRepository.deleteByIds(Collections.singletonList(smartChargeGroupConfigUpdateParamDTO.getId()));
        opLocationPileGroupRepository.updatePidByIds(smartChargeGroupConfigUpdateParamDTO.getRemoveGroupIds());// 原来是子节点的 变成根节点
        opLocationPileVipConfigRepository.deleteByGroupIds(Collections.singletonList(smartChargeGroupConfigUpdateParamDTO.getId()));
        opLocationPileGroupAssociateRepository.deleteByGroupIds(Collections.singletonList(smartChargeGroupConfigUpdateParamDTO.getId()));
        SmartChargeGroupConfigAddParamDTOcopy smartChargeGroupConfigAddParamDTO = SmartChargeGroupTypeMapper.INSTANCE.update2Add(smartChargeGroupConfigUpdateParamDTO);

        //编辑的不是根群组
        Long pid = entity.getPid();
        if (pid.longValue() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID.longValue()) {
            smartChargeGroupConfigAddParamDTO.setPid(pid);
        }
        smartChargeGroupConfigAddParamDTO.setGroupType(entity.getGroupType());
        smartChargeGroupConfigAddParamDTO.setFrom(true);
        Result<Boolean> result = addV3(smartChargeGroupConfigAddParamDTO);
        Set<Long> removeGroupIds = smartChargeGroupConfigUpdateParamDTO.getRemoveGroupIds();
        Set<Long> modifyGroupIds = Optional.ofNullable(removeGroupIds).orElse(new HashSet<>());
        modifyGroupIds.add(smartChargeGroupConfigUpdateParamDTO.getId());
        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> modifyGroupIds.forEach(rootId -> {
            OpLocationPileGroupStrategyDTO param = new OpLocationPileGroupStrategyDTO();
            param.setRootId(rootId);
            param.setUpdateType(2);
            OncePerRequestContext context = this.createdContext(param);
            distributeContext.getStrategy(entity.getAllocationStrategy()).execute(context);
            this.sendMQ(rootId, 2);
            this.removeKey(rootId, null);
            this.refreshTimeSetting(rootId, false);
        })), 3L, TimeUnit.SECONDS);
        if (!CollectionUtils.isEmpty(deleteList)) {
            List<OpLocationPileGroupAssociateEntity> finalDeleteList = deleteList;
            ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                //删除配置
                OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
                OncePerRequestContext context1 = this.createdContext(dto);
                context1.setDeleteList(finalDeleteList);
                distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context1);
            }), 3L, TimeUnit.SECONDS);
        }
        return result;
    }


    @Override
    //EMS离线时将默认负载上限设置为充电上限
    public void handleEmsGroupOffLine(Long groupId) {
        log.info("handleEmsGroupOffLine {}", groupId);
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.getById(groupId);
        //非EMS群组直接返回
        if (entity == null|| !Objects.equals(entity.getGroupType(), EMS_GROUP_TYPE)) {
            return;
        }
        String chargingUpKey = ENERGY_PILE_BASE_CHARGING_UP_EMS_GROUP_ID + groupId;
        stringRedisTemplate.opsForValue().set(chargingUpKey,entity.getChargingUp().toString());
        entity.setChargingUp(entity.getChargingUpEms());
        opLocationPileGroupRepository.saveOrUpdate(entity);
        log.info("EMS群组离线下发信息{}",JSON.toJSONString(entity));
        deliveryContext(groupId);
    }

    @Override
    //EMS在线场景
    public void handleEmsGroupOnLine(EnergyEmsPowerInfoVO ems) {
        log.info("handleEmsGroupOnLine is {}", JSON.toJSONString(ems));
        Long groupId=ems.getGroupId();
        String chargingUpKey = ENERGY_PILE_BASE_CHARGING_UP_EMS_GROUP_ID + groupId;
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.getById(groupId);
        //非EMS群组直接返回
        if (entity == null|| !Objects.equals(entity.getGroupType(), EMS_GROUP_TYPE)) {
            return;
        }
        //拿到上次的充电上限配置
        String value = stringRedisTemplate.opsForValue().get(chargingUpKey);
        BigDecimal chargingUp = StringUtils.isEmpty(value)?entity.getChargingUp():new BigDecimal(value);

        BigDecimal chargingUpEms=StringUtils.equals(entity.getChargingUpUnit(),UnitEnum.POWER.getCode())?ems.getPowerTotal():ems.getCurrentTotal();
        BigDecimal ratio=calculateDifferenceRatio(chargingUpEms,chargingUp,2);
        boolean deliverFlag=ratio.compareTo(new BigDecimal(emsDeliverRatio))<0||chargingUpEms.compareTo(BigDecimal.ZERO)<=0;
        //比例相差3个点之内不重新下发
        if (deliverFlag) {
            log.info("EMS群组功率变化相差3个点之内不重新下发{},messageId is {}",JSON.toJSONString(entity),ems.getMessageId());
            return;
        }
        entity.setChargingUp(chargingUpEms);
        stringRedisTemplate.opsForValue().set(chargingUpKey,entity.getChargingUp().toString());
        opLocationPileGroupRepository.saveOrUpdate(entity);
        log.info("EMS群组下发信息{},messageId is {}",JSON.toJSONString(entity),ems.getMessageId());
        deliveryContext(groupId);
    }

    /**
     * 计算两个 BigDecimal 值之间的相差比例，以第二个值为基础
     *
     * @param a 第一个 BigDecimal 值
     * @param b 第二个 BigDecimal 值
     * @param scale 保留的小数位数
     * @return 相差比例
     */
    public static BigDecimal calculateDifferenceRatio(BigDecimal a, BigDecimal b, int scale) {
        if (a == null || b == null) {
            throw new IllegalArgumentException("Input values cannot be null");
        }

        BigDecimal difference = a.subtract(b).abs();

        // 防止除以零的异常
        if (b.compareTo(BigDecimal.ZERO) == 0) {
            throw new IllegalArgumentException("The base value (b) cannot be zero");
        }

        return difference.divide(b, scale, RoundingMode.HALF_UP);
    }

    @Override
    public String generateDefaultGroupName() {

        final Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        LambdaQueryWrapper<OpLocationPileGroupEntity> lqw = new LambdaQueryWrapper<>();
        lqw.select(OpLocationPileGroupEntity::getId)
                .eq(OpLocationPileGroupEntity::getMerchantId, sellerId)
                .eq(OpLocationPileGroupEntity::getDeleted, 0);
        List<OpLocationPileGroupEntity> opLocationPileGroupEntityList = opLocationPileGroupRepository.getBaseMapper().selectList(lqw);

        int count = 1;
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationPileGroupEntityList)) {
            count = opLocationPileGroupEntityList.size() + 1;
        }

        String defaultGroupName = BaseConstant.SMART_CHARGING_PILE_GROUP_PREFIX + "_" + (count);

        OpLocationPileGroupEntity opLocationPileGroupEntity = new OpLocationPileGroupEntity();
        opLocationPileGroupEntity.setMerchantId(sellerId);

        int maxIterations = 3;
        int iterations = 0;
        do {
            opLocationPileGroupEntity.setName(defaultGroupName);
            boolean flag = this.checkGroupNameUnique(opLocationPileGroupEntity);
            if (flag) {
                return defaultGroupName;
            }
            defaultGroupName = BaseConstant.SMART_CHARGING_PILE_GROUP_PREFIX + "_" + (++count);
            iterations++;
        } while (iterations < maxIterations);
        return null;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> addDemandControlGroup(DemandControlAddConfigDTO dto) {
        log.info("addV3 {}", JSON.toJSONString(dto));
        OpLocationPileGroupEntity pileGroupEntity = SmartChargeGroupTypeMapper.INSTANCE.dto2Entity(dto.getSmartChargeGroupConfigAddParamDTO());
        pileGroupEntity.setName(dto.getControllerName());
        pileGroupEntity.setMerchantId(dto.getMerchantId());
        //查询商家下控制器数量
        int count = demandControlConfigService.countDemandControlConfig(dto);
        if (count >= 10) {
            log.error("商家下控制器数量已达上限 {}", JSON.toJSONString(dto));
            throw new MessageCodeException(PileBaseEnum.CONTROLLER_COUNT_LIMIT);
        }

        //校验名称是否重复
        if (!this.checkControlNameUnique(pileGroupEntity)) {
            log.error("该商家下已有同名的群组 {}", JSON.toJSONString(pileGroupEntity));
            throw new MessageCodeException(PileBaseEnum.HAS_SAME_GROUP_NAME_IN_LOCATION);
        }
        if (Objects.isNull(pileGroupEntity.getLoadType())) {
            //需求控制器给默认
            pileGroupEntity.setChargingUp(new BigDecimal(6));
            pileGroupEntity.setChargingUpUnit("KW");
            pileGroupEntity.setChargingUpType("Power");
            pileGroupEntity.setScene(0);
            pileGroupEntity.setEnergyUseStrategy(0);
            pileGroupEntity.setAllocationStrategy("2");
            pileGroupEntity.setUpDuration(5);
            pileGroupEntity.setMinReserve(0);
            pileGroupEntity.setOverloadFuse(0);
            pileGroupEntity.setName(dto.getControllerName());
            BeanUtils.copyProperties(pileGroupEntity, dto.getSmartChargeGroupConfigAddParamDTO());
        }else{
            if (pileGroupEntity.getLoadType() == 0){
                pileGroupEntity.setElectricUp(null);
                pileGroupEntity.setPowerEquipmentUp(null);
                pileGroupEntity.setPowerEquipmentStartTime(null);
                pileGroupEntity.setMeterVoltage(null);
                pileGroupEntity.setPowerEquipmentEnabled(false);
            }
        }
        pileGroupEntity.setGroupType(1);
        pileGroupEntity.setCreatedAt(System.currentTimeMillis());
        pileGroupEntity.setUpdatedAt(System.currentTimeMillis());
        log.info("opLocationPileGroupEntity {}", JSON.toJSONString(pileGroupEntity));
        // 保存群组
        opLocationPileGroupRepository.saveOrUpdate(pileGroupEntity);
        //保存需求费控制配置
        DemandControlConfigEntity entity = new DemandControlConfigEntity();
        entity.setControllerName(dto.getControllerName());
        entity.setIntervalTime(dto.getIntervalTime());
        entity.setMerchantId(dto.getMerchantId());
        entity.setGroupId(pileGroupEntity.getId());
        entity.setControllerContent(JSONObject.toJSONString(dto.getControllerContent()));
        entity.setStatus(dto.getStatus());
        entity.setDeleted(dto.getDeleted());
        entity.setSns(CollectionUtils.isEmpty(dto.getPileSNList())?"":String.join(",", dto.getPileSNList()));
        entity.setControlRange(dto.getControlRange());
        entity.setCreatedAt(System.currentTimeMillis());
        entity.setUpdatedAt(System.currentTimeMillis());
        //保存需求费控制配置
        demandControlConfigService.save(entity);
        return saveGroupInfo(dto.getSmartChargeGroupConfigAddParamDTO(), pileGroupEntity);
    }



    @Override
    public Result<Boolean> updateDemandControlGroup(DemandControlUpdateConfigDTO dto) {
        //保存需求费控制配置
        DemandControlConfigEntity entity = new DemandControlConfigEntity();

        entity.setId(dto.getId());
        entity.setControllerName(dto.getControllerName());
        entity.setIntervalTime(dto.getIntervalTime());
        entity.setMerchantId(dto.getMerchantId());
        entity.setControllerContent(JSONObject.toJSONString(dto.getControllerContent()));
        entity.setStatus(dto.getStatus());
        entity.setDeleted(dto.getDeleted());
        entity.setSns(CollectionUtils.isEmpty(dto.getPileSNList())?"":String.join(",", dto.getPileSNList()));
        entity.setControlRange(dto.getControlRange());
        entity.setUpdatedAt(System.currentTimeMillis());

        //保存需求费控制配置
        demandControlConfigService.updateById(entity);
        SmartChargeGroupConfigUpdateParamDTOcopy smartChargeGroupConfigUpdateParamDTO=dto.getSmartChargeGroupConfigUpdateParamDTO();
        smartChargeGroupConfigUpdateParamDTO.setMerchantId(dto.getMerchantId());
        smartChargeGroupConfigUpdateParamDTO.setName(dto.getControllerName());
        return updateV3(smartChargeGroupConfigUpdateParamDTO);
    }

    @Override
    public String generateDefaultControlName(DemandControlAddConfigDTO dto) {
        //查询商家下的需求费控制配置列表
        List<DemandControlConfigEntity> list = demandControlConfigService.listAllDemandControlConfig(dto);
        int count = 1;
        if (!CollectionUtils.isEmpty(list)) {
            count = list.size() + 1;
        }
        return "Control_" + count;
    }

    @Override
    public Boolean updateDemandControlStatus(Integer status, Integer deleted, Long id) {
        return demandControlConfigService.updateDemandControlStatus(status, deleted, id);
    }

    public  List<PileGroupForEmsVOcopy> getPileGroupForEms(Long groupId){
        List<PileGroupForEmsVOcopy> list = new ArrayList<>();
        PileGroupDetailV3VOcopy detailV3VO= queryDetailForEms(groupId);
        PileGroupForEmsVOcopy pileGroupForEmsVO=new PileGroupForEmsVOcopy();
        BeanUtil.copyProperties(detailV3VO, pileGroupForEmsVO);
        list.add(pileGroupForEmsVO);
        return list;
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> addV3(SmartChargeGroupConfigAddParamDTOcopy opLocationPileGroupDTO) {
        log.info("addV3 {}", JSON.toJSONString(opLocationPileGroupDTO));
        OpLocationPileGroupEntity pileGroupEntity = SmartChargeGroupTypeMapper.INSTANCE.dto2Entity(opLocationPileGroupDTO);
        pileGroupEntity.setName(StringUtils.isEmpty(opLocationPileGroupDTO.getName()) ? "" : opLocationPileGroupDTO.getName());
        pileGroupEntity.setMerchantId(opLocationPileGroupDTO.getMerchantId());
        //校验名称是否重复
        if (!this.checkControlNameUnique(pileGroupEntity)) {
            log.error("该商家下已有同名的群组 {}", JSON.toJSONString(pileGroupEntity));
            throw new MessageCodeException(PileBaseEnum.HAS_SAME_GROUP_NAME_IN_LOCATION);
        }
        if (Objects.isNull(pileGroupEntity.getLoadType())) {
            //需求控制器给默认
            pileGroupEntity.setChargingUp(new BigDecimal(6));
            pileGroupEntity.setChargingUpUnit("KW");
            pileGroupEntity.setChargingUpType("Power");
            pileGroupEntity.setScene(0);
            pileGroupEntity.setEnergyUseStrategy(0);
            pileGroupEntity.setAllocationStrategy("2");
            pileGroupEntity.setLoadType(3);
            pileGroupEntity.setUpDuration(5);
            pileGroupEntity.setMinReserve(0);
            pileGroupEntity.setOverloadFuse(0);
            BeanUtils.copyProperties(pileGroupEntity, opLocationPileGroupDTO);
        }else{
            if (pileGroupEntity.getLoadType() == 0){
                pileGroupEntity.setElectricUp(null);
                pileGroupEntity.setPowerEquipmentUp(null);
                pileGroupEntity.setPowerEquipmentStartTime(null);
                pileGroupEntity.setMeterVoltage(null);
                pileGroupEntity.setPowerEquipmentEnabled(false);
            }
            // group_type comment '群组类型 :0，普通群组 1，需求控制费群组 2：边缘云 3：EMS',   loadType" :0：0：DLB， 1：ALM， 2：EMS  3,demand"
            if (pileGroupEntity.getLoadType() == 2){
                pileGroupEntity.setGroupType(3);
            }
        }
        pileGroupEntity.setCreatedAt(System.currentTimeMillis());
        pileGroupEntity.setUpdatedAt(System.currentTimeMillis());
        log.info("opLocationPileGroupEntity {}", JSON.toJSONString(pileGroupEntity));
        // 保存群组
        opLocationPileGroupRepository.saveOrUpdate(pileGroupEntity);
        return saveGroupInfo(opLocationPileGroupDTO, pileGroupEntity);
    }


    public Result<Boolean> saveGroupInfo(SmartChargeGroupConfigAddParamDTOcopy opLocationPileGroupDTO, OpLocationPileGroupEntity pileGroupEntity) {
        final Long pileGroupEntityId = pileGroupEntity.getId();
        List<Long> groupIds = new ArrayList<>();
        List<String> pileSnList = new ArrayList<>();
        List<OpLocationPileGroupAssociateEntity> associateEntityList = new ArrayList<>();
        List<PileGroupTreeVOcopy> pileGroupList = opLocationPileGroupDTO.getPileGroupList();

        Long rootId = null;
        boolean hasEdgeEMs=false;

        Function<List<PileGroupTreeVOcopy>,List<String>> hasEdgeFunc=(pileGroupListP)->{
            if(!CollectionUtils.isEmpty(pileGroupListP)){
                List<Long> childIds = pileGroupListP.stream()
                        .filter(item -> Objects.nonNull(item.getGroupId()) && item.getGroupId() > 0)
                        .map(PileGroupTreeVOcopy::getGroupId).collect(Collectors.toList());
                log.info("add_group,childs:{}",JSON.toJSONString(childIds));
                if(CollectionUtils.isEmpty(childIds)){
                    return null;
                }
                LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper=new LambdaQueryWrapper<>();
                queryWrapper.select(OpLocationPileGroupEntity::getId);
                queryWrapper.in(OpLocationPileGroupEntity::getId,childIds);
                queryWrapper.in(OpLocationPileGroupEntity::getGroupType, Arrays.asList(3,2));
                List<String> edgeEmsGroupIds= opLocationPileGroupRepository.listObjs(queryWrapper,ob->String.valueOf(ob));
                return edgeEmsGroupIds;
            }
            return null;
        };
        List<String> subEdgeEmsGroupIds=null;
        if(opLocationPileGroupDTO.getFrom()){//
            //边缘云 群组更新，通知边缘服务
            rootId=opLocationPileGroupRepository.findOrdinaryRoot(pileGroupEntity.getId());
            hasEdgeEMs = opLocationPileGroupRepository.hasEdgeEMs(rootId);//当前自身、下级是否有 ems/edge
            subEdgeEmsGroupIds = hasEdgeFunc.apply(pileGroupList);//新加的下级是否有 ems/edge

            log.info("smart_group_update,hasEdgeEMs:{},subEdgeEmsGroupIds:{}",hasEdgeEMs,JSON.toJSONString(subEdgeEmsGroupIds));
        }else{//新增
            rootId=pileGroupEntityId;
            subEdgeEmsGroupIds = hasEdgeFunc.apply(pileGroupList);
            log.info("smart_group_add,subEdgeEmsGroupIds:{}",subEdgeEmsGroupIds);
        }
        batchUpdatePileGroup(pileGroupList, opLocationPileGroupDTO.getGroupType());

        for (PileGroupTreeVOcopy item : pileGroupList) {
            if (Objects.nonNull(item.getGroupId()) && item.getGroupId() > 0) {
                OpLocationPileGroupEntity one = opLocationPileGroupRepository.findOne(item.getGroupId());
                if (Objects.isNull(one)) {
                    log.error("不存在的子群组 {}", item.getGroupId());
                    throw new MessageCodeException(PileBaseEnum.HAS_SAME_GROUP_NAME_IN_LOCATION);
                }
                /*if (opLocationPileGroupDTO.getLoadType() == 1 && one.getLoadType() == 0 &&
                        ("KW".equalsIgnoreCase(one.getChargingUpUnit()) || "W".equalsIgnoreCase(one.getChargingUpUnit()))) {
                    log.error("ALM不应包含负载单位为KW的DLB类型子组 {}", item.getGroupId());
                    throw new MessageCodeException(PileBaseEnum.ALM_ONLY_CONTAIN_DLB_OF_KW_SUBGROUP);
                }*/
                groupIds.add(item.getGroupId());
            } else if (org.springframework.util.StringUtils.hasText(item.getPileSn())) {
                Assert.notNull(item.getPileId(), JSON.toJSONString(item));
                Assert.hasText(item.getPileSn(), JSON.toJSONString(item));
                /*if (opLocationPileGroupDTO.getLoadType() == 1 && !item.getPileSn().startsWith("A")) {
                    log.error("ALM仅支持AC桩 {}", item.getPileSn());
                    throw new MessageCodeException(PileBaseEnum.ALM_ONLY_SUPPORT_AC_PILE);
                }*/
                pileSnList.add(item.getPileSn());
                OpLocationPileGroupAssociateEntity entity = new OpLocationPileGroupAssociateEntity();
                entity.setUpdatedAt(System.currentTimeMillis());
                entity.setPileId(item.getPileId());
                entity.setDeleted(Boolean.FALSE);
                entity.setGroupId(pileGroupEntityId);
                entity.setCreatedAt(System.currentTimeMillis());
                entity.setPileSn(item.getPileSn());
                entity.setStatus(0);
                entity.setLocationId(item.getLocationId());
                associateEntityList.add(entity);
            } else {
                log.warn(" Illegal data {}", JSON.toJSONString(item));
            }
        }

        if (!associateEntityList.isEmpty()) {
            //查找原来的关系
            List<OpLocationPileGroupAssociateVO> existDetailList = opLocationPileGroupAssociateRepository.findDetailList(pileSnList);
            if (!CollectionUtils.isEmpty(existDetailList)) {
                throw new MessageCodeException(PileBaseEnum.PILE_IS_IN_OTHER_GROUP);
            }
            //保存桩和群组关系
            boolean insert = opLocationPileGroupAssociateRepository.insertBatch(associateEntityList);
            Assert.isTrue(insert, "batch insert fail " + JSON.toJSONString(associateEntityList));
        }

        if (!groupIds.isEmpty()) {
            List<OpLocationPileGroupEntity> updateEntityList = opLocationPileGroupRepository.findByIds(groupIds);
            log.info("before copyParentValue {} ", JSON.toJSONString(updateEntityList));
            updateEntityList.forEach(entity -> {
                entity.setPid(pileGroupEntityId);
                entity.setUpdatedAt(System.currentTimeMillis());
                //继承父级属性
                this.copyParentValue(entity, pileGroupEntity);
                List<OpLocationPileGroupEntity> list = opLocationPileGroupRepository.findByPidAndMerchantId(opLocationPileGroupDTO.getMerchantId(), entity.getId(), null);
                if (!CollectionUtils.isEmpty(list)) {  // 我们最多三层组 所以可以这么写
                    log.info("before sub copyParentValue {} ", JSON.toJSONString(list));
                    list.forEach(sub -> {
                        sub.setUpdatedAt(System.currentTimeMillis());
                        //继承父级属性
                        this.copyParentValue(sub, pileGroupEntity);
                    });
                    log.info("after sub copyParentValue {} ", JSON.toJSONString(list));
                    opLocationPileGroupRepository.updateBatch(list);
                }
            });
            log.info("after copyParentValue {} ", JSON.toJSONString(updateEntityList));
            opLocationPileGroupRepository.updateBatch(updateEntityList);
        }

        // 保存 VIP 策略
        List<VipPolicy> vipPolicys = opLocationPileGroupDTO.getVipPolicy();
        if (!CollectionUtils.isEmpty(vipPolicys) && 1 == opLocationPileGroupDTO.getPriority()) {
            vipPolicys.forEach(vipPolicy -> vipPolicy.setGroupId(pileGroupEntityId));
            opLocationPileVipConfigRepository.batchAddOrUpdate(vipPolicys);
        }

        if(opLocationPileGroupDTO.getFrom()){//
            //边缘云 群组更新，通知边缘服务
            if(hasEdgeEMs){
                List<String> rootGid = Arrays.asList(rootId + "");
                //一对一更新
                notifyEdgeSmartCharge(1, rootGid, rootGid,rootId, "50501003");
            }
            if(!CollectionUtils.isEmpty(subEdgeEmsGroupIds)){//新加的下级中存在edge/ems，通知边缘云删除
                notifyEdgeSmartCharge(0, subEdgeEmsGroupIds, subEdgeEmsGroupIds,rootId, "50501003");
            }
        }else{//新增
            if(!CollectionUtils.isEmpty(subEdgeEmsGroupIds)){
                //边缘云群组 上又加了一层新群组，通知边缘云 删除旧群组
                //50501002 添加失败提示信息
                notifyEdgeSmartCharge(0, subEdgeEmsGroupIds, subEdgeEmsGroupIds,rootId, "50501002");
            }
        }

        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            dto.setRootId(pileGroupEntityId);
            dto.setUpdateType(1);
            Boolean from = opLocationPileGroupDTO.getFrom();
            if (from != null && from) {
                dto.setUpdateType(2);
            }
            OncePerRequestContext context = this.createdContext(dto);
            distributeContext.getStrategy(opLocationPileGroupDTO.getAllocationStrategy()).execute(context);
            this.sendMQ(pileGroupEntityId, 1);
            this.removeKey(pileGroupEntityId, null);
            this.refreshTimeSetting(pileGroupEntityId, false);
            this.notifyEms(pileGroupEntityId);
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }

    /**
     * 充电配置发生变化
     * @param status
     * @param oldGids 旧的根群组id
     * @param newGids  新的根群组id
     * @param opType
     * @return
     */
    private void notifyEdgeSmartCharge(Integer status, List<String> oldGids,List<String> newGids, Long rootId, String errCode){
        //去掉子组 边缘云标记
        opLocationPileGroupRepository.updateSubGroupType(rootId);

        TransactionUtils.afterCommitAsyncExecute(()->{
            //通知边缘云
            try {
                UpdateGatewayDTO gwDto = new UpdateGatewayDTO();
                gwDto.setStatus(status);
                gwDto.setSmartOrdParentChargeIds(oldGids);
                gwDto.setSmartNewParentChargeId(newGids);
                log.info("updateLinkedGateWay_reqParam:{}",JSON.toJSONString(gwDto));
                Result<Boolean> booleanResult = edgeFeignClient.updateLinkedGateWay(gwDto);
                log.info("updateLinkedGateWay_resp:{},oldId:{},newId:{},status:{}",booleanResult,JSON.toJSONString(oldGids),JSON.toJSONString(newGids),status);
                if (booleanResult == null || booleanResult.getData()==null || !booleanResult.getData()) {
                    log.error("智能充电配置变动，通知边缘云失败,groupId:{}",rootId);
                    //删除失败
    //                throw new MessageCodeException(errCode);
                }else {
                    notifyToEms(rootId);
                }
            } catch (Exception e) {
                log.error("notifyEdgeSmartCharge_exp:",e);
            }
        },queryExecuor);

    }



    public void deliveryContext(Long groupId){
        OpLocationPileGroupEntity one = opLocationPileGroupRepository.findOne(groupId);
        OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
        dto.setRootId(groupId);
        dto.setUpdateType(12);
        OncePerRequestContext context = this.createdContext(dto);
        distributeContext.getStrategy(one.getAllocationStrategy()).execute(context);
        this.sendMQ(groupId, 1);
        this.removeKey(groupId, null);
        this.refreshTimeSetting(groupId, false);
    }

    private void notifyEms(Long groupId) {
        PileGroupDetailV3VOcopy detailV3VO= queryDetailForEms(groupId);
        //非EMS群组直接返回
        if (detailV3VO == null||detailV3VO.getGroupType()!=3) {
            return;
        }
        PileGroupForEmsVOcopy pileGroupForEmsVO=new PileGroupForEmsVOcopy();
        BeanUtils.copyProperties(detailV3VO, pileGroupForEmsVO);
        BasicDTO<PileGroupForEmsVOcopy> exchange=new BasicDTO<>();
        exchange.setData(Collections.singletonList(pileGroupForEmsVO));
        //下发群组信息
        emsExchangeClient.exchangeChargeGroupInfo(exchange);
        //更新redis缓存
        updatePileData();
    }


    private void batchUpdatePileGroup(List<PileGroupTreeVOcopy> pileGroupList, Integer groupType) {
        if (CollectionUtils.isEmpty(pileGroupList)) {
            return;
        }
        List<Long> groupIds = pileGroupList.stream().filter(item -> Objects.nonNull(item.getGroupId()) && item.getGroupId() > 0).map(PileGroupTreeVOcopy::getGroupId).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(groupIds)) {
            return;
        }
        Set<Long> updateIds = new HashSet<>(groupIds);
        List<OpLocationPileGroupEntity> updateEntityList = opLocationPileGroupRepository.findByIds(groupIds);
        updateEntityList.forEach(entity -> {
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(entity.getId());
            updateIds.addAll(children.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet()));
        });
        opLocationPileGroupRepository.updateBatchGroupType(groupType,updateIds);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> add(OpLocationPileGroupDTO opLocationPileGroupDTO) {
        if (opLocationPileGroupDTO == null || opLocationPileGroupDTO.getId() != null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        //校验名称是否重复
        OpLocationPileGroupEntity opLocationPileGroupEntity = getPileGroupByCondition(opLocationPileGroupDTO);
        if (opLocationPileGroupEntity != null) {
            throw new MessageCodeException(PileBaseEnum.HAS_SAME_GROUP_NAME_IN_LOCATION);
        }
        //校验父群组是否存在
        if (opLocationPileGroupDTO.getPid() != null) {
            boolean check = opLocationPileGroupRepository.check(opLocationPileGroupDTO.getPid());
            if (!check) {
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
            }
        }
        OpLocationPileGroupEntity pileGroupEntity = toOpLocationPileGroupEntity(opLocationPileGroupDTO);
        long now = System.currentTimeMillis();
        pileGroupEntity.setUpdatedAt(now);
        pileGroupEntity.setCreatedAt(now);
        //保存群组
        opLocationPileGroupRepository.insert(pileGroupEntity);
        Long parentId = pileGroupEntity.getId();
        List<OpLocationPileGroupAssociateEntity> associateEntityList = new ArrayList<>();
        List<String> pileSnList = new ArrayList<>();
        List<Long> groupIds = new ArrayList<>();
        opLocationPileGroupDTO.getAssociateDTOList().stream().forEach(dto -> {
            Long groupId = dto.getGroupId();
            //按组新增
            if (groupId != null) {
                groupIds.add(groupId);
            } else {
                //按桩新增
                associateEntityList.add(toPileGroupAssociateEntity(pileGroupEntity, dto));
                pileSnList.add(dto.getPileSn());
            }
        });

        if (!associateEntityList.isEmpty()) {
            //查找原来的关系
            List<OpLocationPileGroupAssociateVO> existDetailList = opLocationPileGroupAssociateRepository.findDetailList(pileSnList);
            if (!CollectionUtils.isEmpty(existDetailList)) {
                throw new MessageCodeException(PileBaseEnum.PILE_IS_IN_OTHER_GROUP);
            }
            //保存桩和群组关系
            boolean insert = opLocationPileGroupAssociateRepository.insertBatch(associateEntityList);
            if (!insert) {
                throw new RuntimeException("batch insert fail.");
            }
        }
        Set<Long> rootIds = new HashSet<>();
        rootIds.add(parentId);
        if (!groupIds.isEmpty()) {
            List<OpLocationPileGroupEntity> updateEntityList = opLocationPileGroupRepository.findByIds(groupIds);
            opLocationPileGroupRepository.updateBatch(
                    updateEntityList.stream().map(entity -> {
                        entity.setPid(parentId);
                        entity.setUpdatedAt(now);
                        //继承父级属性
                        this.copyParentValue(entity, pileGroupEntity);
                        return entity;
                    }).collect(Collectors.toList())
            );
        }
        //保存redis并触发配置下发
        Set<Long> parentIds = rootIds;
        log.info("add,rootIds={}", JSON.toJSONString(rootIds));
        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            parentIds.stream().forEach(rootId -> {
                OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
                dto.setRootId(rootId);
                dto.setUpdateType(1);
                OncePerRequestContext context = this.createdContext(dto);
                distributeContext.getStrategy(opLocationPileGroupDTO.getAllocationStrategy()).execute(context);
                this.sendMQ(rootId, 1);
                this.removeKey(rootId, null);
                this.refreshTimeSetting(rootId, false);
            });
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }

    private void refreshTimeSetting(Long rootId, boolean delete) {
        String key = RedisKeyConstant.getPileBaseTimeSettingKey(rootId);
        if (delete) {
            stringRedisTemplate.delete(key);
        } else {
            List<OpPileGroupTimeSettingDTO> list = this.loadAllTimeSetting(rootId);
            if (!CollectionUtils.isEmpty(list)) {
                OpPileGroupTimeSettingDTO currentDto = list.get(0);
                stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(currentDto), 24L, TimeUnit.HOURS);
                MessageProperties messageProperties = new MessageProperties();
                messageProperties.setDelay(currentDto.getDelay());
                Message message = new Message((JSON.toJSONString(currentDto)).getBytes(StandardCharsets.UTF_8), messageProperties);
                rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_TIME_SETTING_EXCHANGE + rabbitmqVersionSuffix, AmqpConstant.PILE_BASE_TIME_SETTING_ROUTE, message);
            } else {
                stringRedisTemplate.delete(key);
            }
        }
    }

    private void removeKey(Long rootId, List<OpLocationPileGroupEntity> children) {
        if (CollectionUtils.isEmpty(children)) {
            children = opLocationPileGroupRepository.findChildren(rootId);
        }
        if (!CollectionUtils.isEmpty(children)) {
            children.forEach(e -> {
                if (e.getLoadType() == 1) {
                    Long meterId = e.getMeterId();
                    if (meterId != null) {
                        String key = RedisKeyConstant.getPileGroupRelateMeterKey(meterId);
                        String meterValueKey = RedisKeyConstant.getPileGroupRelateMeterValueKey(meterId);
                        String evseKey = RedisKeyConstant.getPileGroupRelateEvseKey(e.getId());
                        if (stringRedisTemplate.hasKey(key)) {
                            stringRedisTemplate.delete(key);
                        }
                        if (stringRedisTemplate.hasKey(meterValueKey)) {
                            stringRedisTemplate.delete(meterValueKey);
                        }
                        if (stringRedisTemplate.hasKey(evseKey)) {
                            stringRedisTemplate.delete(evseKey);
                        }
                    }
                }
            });
        }
    }

    private void sendMQ(Long groupId, Integer type) {
        PileGroupUpdateDTO updateDTO = PileGroupUpdateDTO.builder()
                .groupId(groupId)
                .type(type)
                .build();
        rabbitTemplate.convertAndSend(AmqpConstant.PILE_GROUP_UPDATE_EXCHANGE + rabbitmqVersionSuffix, AmqpConstant.PILE_GROUP_UPDATE_ROUTE, JSON.toJSONString(updateDTO));
    }

    /**
     * 子群组继承父群组负载管理类型、用能策略、分配策略
     *
     * @param child  子群组
     * @param parent 父群组
     */
    private void copyParentValue(OpLocationPileGroupEntity child, OpLocationPileGroupEntity parent) {
        //child.setLoadType(parent.getLoadType());
        child.setEnergyUseStrategy(parent.getEnergyUseStrategy());
        //child.setTariffId(parent.getTariffId());
        child.setFavor(parent.getFavor());
        child.setPlanTime(parent.getPlanTime());
        //child.setMeterId(parent.getMeterId());
//        child.setPriority(parent.getPriority());
        child.setAllocationStrategy(parent.getAllocationStrategy());
        child.setStatus(parent.getStatus());
//        if (Objects.nonNull(parent.getGroupType()) && parent.getGroupType()==0) {
//            child.setChargingUpType(parent.getChargingUpType());
//            child.setChargingUpUnit(parent.getChargingUpUnit());
//            //子群组上限不能超过父级
//            if (child.getChargingUp().compareTo(parent.getChargingUp()) > 0) {
//                child.setChargingUp(parent.getChargingUp());
//            }
//        }
        child.setSecurityEnabled(parent.getSecurityEnabled());
        child.setMinReserve(parent.getMinReserve());

        child.setMerchantId(parent.getMerchantId());
        child.setScene(parent.getScene());
        child.setOverloadFuse(parent.getOverloadFuse());
        child.setUpDuration(parent.getUpDuration());
        if (!CollectionUtils.isEmpty(parent.getPrices())) {
            child.setPrices(parent.getPrices());
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> deleteV3(Long groupId) {
        log.info("delete group {}", groupId);
        if (groupId == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.getById(groupId);
        if (entity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        List<OpLocationPileGroupEntity> childrenEntityList = opLocationPileGroupRepository.findChildren(groupId);
        ThreadPoolUtil.getExecutor().execute(() -> this.removeKey(null, childrenEntityList));

        //删除群组
        Set<Long> collect = childrenEntityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        log.info("delete group ids {}", JSON.toJSONString(collect));
        opLocationPileGroupRepository.deleteBatch(collect);
        //删除VIP群组
        opLocationPileVipConfigRepository.deleteByGroupIds(collect);
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(collect);
        if (CollectionUtils.isEmpty(associateEntityList)) {
            log.info("delete,no pile associate.");
            return Result.ofSucceed(Boolean.TRUE);
        }
        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        /*List<OpLocationEvseElasticDTO> evseElasticDtoList = opLocationEvseService.findList(pileSnList);
        boolean present = evseElasticDtoList.stream().filter(dto -> EvseDeviceStatusEnum.DEFAULT.getName().equalsIgnoreCase(dto.getState())).findAny().isPresent();
        if (present) {
            Result<Boolean> result = Result.ofSucceed(Boolean.FALSE);
            result.setMessage("pile outline");
            return result;
        }*/
        //删除群组关联的桩
        opLocationPileGroupAssociateRepository.deleteBatch(associateEntityList);
        //如果需求控制器下没有任何桩，删除需求控制器
        Long rootId=opLocationPileGroupRepository.findOrdinaryRoot(groupId);
        List<OpLocationPileGroupEntity> children = this.opLocationPileGroupRepository.findChildren(rootId, 1);
        if (CollectionUtils.isEmpty(children)) {
            opLocationPileGroupService.deleteDemandControlById(rootId);
        }
        if(Objects.equals(2,entity.getGroupType())||Objects.equals(3,entity.getGroupType())){
            //边缘云 群组删除，通知边缘服务
            List<String> rootGid = Arrays.asList(groupId + "");
            notifyEdgeSmartCharge(0, rootGid,rootGid, groupId, "_ktqTsQx1XEKL");
        }

        //删除的不是根群组，触发群组配置下发
        if (entity.getPid() != 0L) {
            Long root = opLocationPileGroupRepository.findOrdinaryRoot(entity.getId());
            boolean flag = false;
            Set<Long> deleteIds = new HashSet<>();
            List<OpLocationPileGroupEntity> childrenList = opLocationPileGroupRepository.findChildren(root);
            OpLocationPileGroupEntity parentEntity = null;
            if (!CollectionUtils.isEmpty(childrenList)) {
                List<OpLocationPileGroupAssociateEntity> list = opLocationPileGroupAssociateRepository.findList(childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet()));
                if (!CollectionUtils.isEmpty(list)) {
                    flag = true;
                    parentEntity = childrenList.get(0);
                } else {
                    deleteIds.addAll(childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet()));
                }
            } else {
                Set<Long> groupIds = new HashSet<>();
                groupIds.add(root);
                List<OpLocationPileGroupAssociateEntity> list = opLocationPileGroupAssociateRepository.findList(groupIds);
                if (!CollectionUtils.isEmpty(list)) {
                    flag = true;
                }
            }
            if (deleteIds.size() > 0) {
                opLocationPileGroupRepository.deleteBatch(deleteIds);
            }
            if (flag) {
                OpLocationPileGroupEntity finalParentEntity = parentEntity;
                ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                    //触发配置下发
                    OpLocationPileGroupStrategyDTO params = new OpLocationPileGroupStrategyDTO();
                    params.setRootId(root);
                    params.setUpdateType(5);
                    distributeContext.getStrategy(finalParentEntity.getAllocationStrategy()).execute(this.createdContext(params));
                    this.sendMQ(root, 2);
                    this.removeKey(root, null);
                    this.refreshTimeSetting(root, false);
                }), 3L, TimeUnit.SECONDS);
            }
        }

        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            //删除配置
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            OncePerRequestContext context = this.createdContext(dto);
            context.setDeleteList(associateEntityList);
            distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
            this.sendMQ(groupId, 3);
            this.removeKey(groupId, null);
            this.refreshTimeSetting(groupId, true);
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> clearSettings(Long groupId) {
        log.info("delete group {}", groupId);
        if (groupId == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        OpLocationPileGroupEntity rootEntity = opLocationPileGroupRepository.getById(groupId);
        if (rootEntity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        List<OpLocationPileGroupEntity> childrenEntityList = opLocationPileGroupRepository.findChildren(groupId);
        ThreadPoolUtil.getExecutor().execute(() -> this.removeKey(null, childrenEntityList));
        //删除群组
        Set<Long> collect = childrenEntityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(collect);
        if (CollectionUtils.isEmpty(associateEntityList)) {
            log.info("delete,no pile associate.");
            return Result.ofSucceed(Boolean.TRUE);
        }

        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            //删除配置
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            OncePerRequestContext context = this.createdContext(dto);
            context.setDeleteList(associateEntityList);
            distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
            this.sendMQ(groupId, 3);
            this.removeKey(groupId, null);
            this.refreshTimeSetting(groupId, true);
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> distributeSettings(Long groupId) {
        log.info("distributeSettings group {}", groupId);
        if (groupId == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        OpLocationPileGroupEntity rootEntity = opLocationPileGroupRepository.getById(groupId);
        if (rootEntity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        List<OpLocationPileGroupEntity> childrenEntityList = opLocationPileGroupRepository.findChildren(groupId);
        ThreadPoolUtil.getExecutor().execute(() -> this.removeKey(null, childrenEntityList));
        Set<Long> collect = childrenEntityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(collect);
        if (CollectionUtils.isEmpty(associateEntityList)) {
            log.info("distributeSettings,no pile associate.");
            return Result.ofSucceed(Boolean.TRUE);
        }
        Long root=rootEntity.getId();


        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            //触发配置下发
            OpLocationPileGroupStrategyDTO params = new OpLocationPileGroupStrategyDTO();
            params.setRootId(root);
            params.setUpdateType(2);
            distributeContext.getStrategy(rootEntity.getAllocationStrategy()).execute(this.createdContext(params));
            this.sendMQ(root, 2);
            this.removeKey(root, null);
            this.refreshTimeSetting(root, false);
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> delete(Long groupId) {
        if (groupId == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.getById(groupId);
        if (entity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        List<OpLocationPileGroupEntity> childrenEntityList = opLocationPileGroupRepository.findChildren(groupId);
        ThreadPoolUtil.getExecutor().execute(() -> {
            this.removeKey(null, childrenEntityList);
        });

        //删除群组
        Set<Long> collect = childrenEntityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        opLocationPileGroupRepository.deleteBatch(collect);
        //删除VIP群组
        opLocationPileVipConfigRepository.deleteByGroupIds(collect);
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(collect);
        if (CollectionUtils.isEmpty(associateEntityList)) {
            log.info("delete,no pile associate.");
            return Result.ofSucceed(Boolean.TRUE);
        }
        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        /*List<OpLocationEvseElasticDTO> evseElasticDtoList = opLocationEvseService.findList(pileSnList);
        boolean present = evseElasticDtoList.stream().filter(dto -> EvseDeviceStatusEnum.DEFAULT.getName().equalsIgnoreCase(dto.getState())).findAny().isPresent();
        if (present) {
            Result<Boolean> result = Result.ofSucceed(Boolean.FALSE);
            result.setMessage("pile outline");
            return result;
        }*/
        //删除群组关联的桩
        opLocationPileGroupAssociateRepository.deleteBatch(associateEntityList);
        //删除的不是根群组，触发群组配置下发
        if (entity.getPid() != 0L) {
            Long root = opLocationPileGroupRepository.findOrdinaryRoot(entity.getId());
            boolean flag = false;
            Set<Long> deleteIds = new HashSet<>();
            List<OpLocationPileGroupEntity> childrenList = opLocationPileGroupRepository.findChildren(root);
            OpLocationPileGroupEntity parentEntity = null;
            if (!CollectionUtils.isEmpty(childrenList)) {
                List<OpLocationPileGroupAssociateEntity> list = opLocationPileGroupAssociateRepository.findList(childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet()));
                if (!CollectionUtils.isEmpty(list)) {
                    flag = true;
                    parentEntity = childrenList.get(0);
                } else {
                    deleteIds.addAll(childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet()));
                }
            } else {
                Set<Long> groupIds = new HashSet<>();
                groupIds.add(root);
                List<OpLocationPileGroupAssociateEntity> list = opLocationPileGroupAssociateRepository.findList(groupIds);
                if (!CollectionUtils.isEmpty(list)) {
                    flag = true;
                }
            }
            if (deleteIds.size() > 0) {
                opLocationPileGroupRepository.deleteBatch(deleteIds);
            }
            if (flag) {
                OpLocationPileGroupEntity finalParentEntity = parentEntity;
                ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                    //触发配置下发
                    OpLocationPileGroupStrategyDTO params = new OpLocationPileGroupStrategyDTO();
                    params.setRootId(root);
                    params.setUpdateType(5);
                    OncePerRequestContext context = this.createdContext(params);
                    distributeContext.getStrategy(finalParentEntity.getAllocationStrategy()).execute(context);
                    this.sendMQ(root, 2);
                    this.removeKey(root, null);
                    this.refreshTimeSetting(root, false);
                }), 3L, TimeUnit.SECONDS);
            }
        }

        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            //删除配置
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            OncePerRequestContext context = this.createdContext(dto);
            context.setDeleteList(associateEntityList);
            distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
            this.sendMQ(groupId, 3);
            this.removeKey(groupId, null);
            this.refreshTimeSetting(groupId, true);
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> update(OpLocationPileGroupDTO opLocationPileGroupDTO) {
        if (opLocationPileGroupDTO == null || opLocationPileGroupDTO.getId() == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.getById(opLocationPileGroupDTO.getId());
        if (entity == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }

        OpLocationPileGroupEntity opLocationPileGroupEntity = getPileGroupByCondition(opLocationPileGroupDTO);
        if (opLocationPileGroupEntity != null && !opLocationPileGroupEntity.getId().equals(opLocationPileGroupDTO.getId())) {
            throw new MessageCodeException(PileBaseEnum.HAS_SAME_GROUP_NAME_IN_LOCATION);
        }
        this.setUpdateEntity(entity, opLocationPileGroupDTO);
        log.info("update,entity={}", JSON.toJSONString(entity));
        //更新的不是根群组，需要继承根群组属性
        OpLocationPileGroupEntity rootEntity = entity;
        if (entity.getPid() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID) {
            rootEntity = opLocationPileGroupRepository.getById(opLocationPileGroupRepository.findOrdinaryRoot(entity.getId()));
            this.copyParentValue(entity, rootEntity);
        }
        this.removeKey(rootEntity.getId(), null);
        //更新群组
        opLocationPileGroupRepository.updateEntity(entity);
        OpLocationPileGroupEntity finalRootEntity = rootEntity;
        //群组触发
        Set<OpLocationPileGroupEntity> rootEntities = new HashSet<>();
        rootEntities.add(rootEntity);

        //查询原来数据
        Long id = opLocationPileGroupDTO.getId();
        List<OpLocationPileGroupEntity> oldChildrenList = opLocationPileGroupRepository.findChildren(id, false);
        Set<Long> oldGroupIds = oldChildrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        Map<Long, OpLocationPileGroupEntity> tmpMap = oldChildrenList.stream().collect(Collectors.toMap(OpLocationPileGroupEntity::getId, e -> e));
        List<OpLocationPileGroupEntity> updateList = new ArrayList<>();

        List<Long> inputGroupIds = new ArrayList<>();
        Map<String, OpLocationPileGroupAssociateDTO> inputPileMap = new HashMap<>();
        opLocationPileGroupDTO.getAssociateDTOList().stream().forEach(dto -> {
            if (dto.getGroupId() != null) {
                inputGroupIds.add(dto.getGroupId());
            } else {
                inputPileMap.put(dto.getPileSn(), dto);
            }
        });
        //删除和新增的群组
        List<Long> addGroupList = new ArrayList<>(inputGroupIds);
        List<Long> deleteGroupList = new ArrayList<>(oldGroupIds);
        if (!oldGroupIds.isEmpty()) {
            oldGroupIds.stream().forEach(groupId -> {
                if (inputGroupIds.contains(groupId)) {
                    addGroupList.remove(groupId);
                    deleteGroupList.remove(groupId);
                    updateList.add(tmpMap.get(groupId));
                }
            });
        }
        if (!addGroupList.isEmpty()) {
            List<OpLocationPileGroupEntity> addEntityList = opLocationPileGroupRepository.findByIds(addGroupList);
            long now = System.currentTimeMillis();
            addEntityList.stream().forEach(e -> {
                e.setUpdatedAt(now);
                e.setPid(id);
                this.copyParentValue(e, entity);
                if (entity.getPid() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID) {
                    this.copyParentValue(e, finalRootEntity);
                }
            });
            log.info("update,addGroupList={}", JSON.toJSONString(addGroupList));
            opLocationPileGroupRepository.updateBatch(addEntityList);
        }
        if (!deleteGroupList.isEmpty()) {
            List<OpLocationPileGroupEntity> deleteEntityList = opLocationPileGroupRepository.findByIds(deleteGroupList);
            long now = System.currentTimeMillis();
            deleteEntityList.stream().forEach(e -> {
                e.setUpdatedAt(now);
                e.setPid(BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID);
            });
            rootEntities.addAll(deleteEntityList);
            log.info("update,deleteGroupList={}", JSON.toJSONString(deleteGroupList));
            opLocationPileGroupRepository.updateBatch(deleteEntityList);
        }
        if (!updateList.isEmpty()) {
            long now = System.currentTimeMillis();
            updateList.stream().forEach(e -> {
                e.setUpdatedAt(now);
                this.copyParentValue(e, entity);
                if (entity.getPid() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID) {
                    this.copyParentValue(e, finalRootEntity);
                }
            });
            log.info("update,updateList={}", JSON.toJSONString(updateList));
            opLocationPileGroupRepository.updateBatch(updateList);
        }

        List<OpLocationPileGroupAssociateEntity> oldAssociateList = opLocationPileGroupAssociateRepository.findList(id);
        //删除和新增的桩
        Map<String, OpLocationPileGroupAssociateDTO> addPileMap = new HashMap<>(inputPileMap);
        Map<String, OpLocationPileGroupAssociateEntity> deletePileMap = oldAssociateList.stream().collect(Collectors.toMap(OpLocationPileGroupAssociateEntity::getPileSn, e -> e, (f, s) -> f));
        if (!oldAssociateList.isEmpty()) {
            Set<String> inputPileSnList = inputPileMap.keySet();
            oldAssociateList.stream().forEach(e -> {
                String sn = e.getPileSn();
                if (inputPileSnList.contains(sn)) {
                    addPileMap.remove(sn);
                    deletePileMap.remove(sn);
                }
            });
        }
        List<OpLocationPileGroupAssociateEntity> addAssociateEntityList = new ArrayList<>();
        if (!addPileMap.isEmpty()) {
            Long locationId = opLocationPileGroupDTO.getLocationId();
            long now = System.currentTimeMillis();
            addPileMap.forEach((k, v) -> {
                OpLocationPileGroupAssociateEntity e = new OpLocationPileGroupAssociateEntity();
                e.setLocationId(locationId);
                e.setPileId(v.getPileId());
                e.setUpdatedAt(now);
                e.setCreatedAt(now);
                e.setPileSn(k);
                e.setGroupId(id);
                addAssociateEntityList.add(e);
            });
            log.info("update,addPileMap={}", JSON.toJSONString(addPileMap));
            opLocationPileGroupAssociateRepository.insertBatch(addAssociateEntityList);
        }
        List<OpLocationPileGroupAssociateEntity> deleteAssociateEntityList = new ArrayList<>();
        if (!deletePileMap.isEmpty()) {
            long now = System.currentTimeMillis();
            deletePileMap.forEach((k, v) -> {
                v.setUpdatedAt(now);
                v.setDeleted(true);
                deleteAssociateEntityList.add(v);
            });
            log.info("update,deletePileMap={}", JSON.toJSONString(deletePileMap));
            //删除数据
            opLocationPileGroupAssociateRepository.deleteBatch(deleteAssociateEntityList);
            //删除配置
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            OncePerRequestContext context = this.createdContext(dto);
            context.setDeleteList(deleteAssociateEntityList);
            distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
        }

        ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
            rootEntities.stream().forEach(e -> {
                Long rootId = e.getId();
                OpLocationPileGroupStrategyDTO param = new OpLocationPileGroupStrategyDTO();
                param.setRootId(rootId);
                param.setUpdateType(2);
                OncePerRequestContext context = this.createdContext(param);
                distributeContext.getStrategy(finalRootEntity.getAllocationStrategy()).execute(context);
                this.sendMQ(rootId, 2);
                this.removeKey(rootId, null);
                this.refreshTimeSetting(rootId, false);
            });
        }), 3L, TimeUnit.SECONDS);
        return Result.ofSucceed(Boolean.TRUE);
    }

    private void setUpdateEntity(OpLocationPileGroupEntity entity, OpLocationPileGroupDTO dto) {
        if (StringUtils.isNotBlank(dto.getName())) {
            entity.setName(dto.getName());
        }
        if (StringUtils.isNotBlank(dto.getPhaseNum())) {
            entity.setPhaseNum(dto.getPhaseNum());
        }
        if (StringUtils.isNotBlank(dto.getAllocationStrategy())) {
            entity.setAllocationStrategy(dto.getAllocationStrategy());
        }
        if (dto.getChargingUp() != null) {
            entity.setChargingUp(dto.getChargingUp());
        }
        if (dto.getChargingUpType() != null) {
            entity.setChargingUpType(dto.getChargingUpType());
        }
        if (dto.getChargingUpUnit() != null) {
            entity.setChargingUpUnit(dto.getChargingUpUnit());
            if (UnitEnum.POWER.getCode().equals(dto.getChargingUpUnit())) {
                entity.setChargingUpType("Power");
            }
        }
        if (!ObjectUtils.isEmpty(dto.getLoadType())) {
            entity.setLoadType(dto.getLoadType());
            if (dto.getLoadType() == 0) {
                entity.setMeterId(null);
                entity.setMeterLocation(null);
            }
            if (dto.getLoadType() == 1) {
                entity.setMeterId(dto.getMeterId());
                entity.setMeterLocation(dto.getMeterLocation());
            }
        }
        if (!ObjectUtils.isEmpty(dto.getEnergyUseStrategy())) {
            entity.setEnergyUseStrategy(dto.getEnergyUseStrategy());
            if (dto.getEnergyUseStrategy() == 0) {
                entity.setTariffId(null);
                entity.setFavor(null);
                entity.setPlanTime(null);
            }
            if (dto.getEnergyUseStrategy() == 1) {
                entity.setTariffId(dto.getTariffId());
                entity.setFavor(dto.getFavor());
                entity.setPlanTime(dto.getPlanTime());
            }
        }
        if (!ObjectUtils.isEmpty(dto.getPriority())) {
            entity.setPriority(dto.getPriority());
        }

        entity.setSecurityEnabled(false);
        if (!ObjectUtils.isEmpty(dto.getSecurityEnabled())) {
            entity.setSecurityEnabled(dto.getSecurityEnabled());
        }
        if (!ObjectUtils.isEmpty(dto.getOfflineValue())) {
            entity.setOfflineValue(dto.getOfflineValue());
        }
        if (!ObjectUtils.isEmpty(dto.getMinReserve())) {
            entity.setMinReserve(dto.getMinReserve());
        }
        List<TimeSettingDetailDTO> timeSettingDetails = dto.getTimeSettingDetails();
        if (!CollectionUtils.isEmpty(timeSettingDetails)) {
            entity.setTimeSettingMode(1);
            timeSettingDetails.stream().forEach(d -> d.setId(IdWorker.getId()));
            entity.setTimeSettingDetail(JSON.toJSONString(timeSettingDetails));
        } else {
            entity.setTimeSettingMode(0);
            entity.setTimeSettingDetail(null);
        }
        entity.setUpdatedAt(System.currentTimeMillis());
    }

    @Override
    public  List<PileGroupDetailV3VOcopy> queryDetailsV3(List<Long> groupIds) {
        List<PileGroupDetailV3VOcopy> result = new ArrayList<>();
        groupIds.forEach(v->result.add(this.detailV3(v)));
        return result;
    }

    @Override
    public void deleteDemandControlById(Long rootId) {
        demandControlConfigService.deleteDemandControlById(rootId);
    }

    @Override
    public Long setLoadTypeWhiteList(Long userId, Integer type) {
        //业务处理
        if (!ObjectUtils.isEmpty(userId)){
            //获取hash key
            String hashKey = getHashKey(type);
            //设置redis数据
            if(!ObjectUtils.isEmpty(hashKey)){
                redisTemplate.opsForHash().put(hashKey, userId.toString(), userId.toString());
                log.info("setAlm,userId={}", userId);
            }
        }

        return userId;
    }

    @Override
    public Boolean removeLoadTypeWhiteList(Long userId, Integer loadType) {
        //业务处理
        if (!ObjectUtils.isEmpty(userId)){
            //获取hash key
            String hashKey = getHashKey(loadType);
            //移除redis数据
            if(!ObjectUtils.isEmpty(hashKey)){
                redisTemplate.opsForHash().delete(hashKey, userId.toString(), userId.toString());
                log.info("removeLoadTypeWhiteList,userId={}", userId);
            }
        }
        return true;
    }

    /**
     * 获取 hash Key
     * @param type
     * @return
     */
    private String getHashKey(Integer type){
        String hashKey = null;
        if(type != null){
            switch (type){
                case 1:
                    hashKey = RedisKeyConstant.getHashPileBaseAlmUserInfo();
                    break;
                case 2:
                    hashKey = RedisKeyConstant.getHashPileBaseEmsUserInfo();
                    break;
                case 3:
                    hashKey = RedisKeyConstant.getHashPileBaseDemandUserInfo();
                    break;
                default:
                    break;
            }
        }
        return hashKey;
    }

    @Override
    public List<Long> queryEmsGroupId() {
        return opLocationPileGroupRepository.queryEmsGroupId();
    }

    @Override
    public void loadEmsGroupData() {
        //获取全部EMS类型的groupId
        List<Long> longList = queryEmsGroupId();
        log.info("全部EMS类型的groupId数量：{}",longList);
        //处理数据
        if(longList == null || longList.size() == 0){
            return;
        }
        //清理缓存
        Map<Long,String> idMap = new HashMap<>();
        Map<Long,Long> rootMap = new HashMap<>();
        Set<Long> rootSet = new HashSet<>();
        for(Long groupId : longList){
            //获取顶层群组groupId
            Long ordinaryRoot = opLocationPileGroupRepository.findOrdinaryRoot(groupId);
            rootMap.put(groupId,ordinaryRoot);
            rootSet.add(ordinaryRoot);
            String key = String.format(RedisConstant.REDIS_EMS_GROUP_INFO_HASH_KEY,ordinaryRoot);
            //清空hash数据
            redisTemplate.delete(key);
            idMap.put(groupId,key);
        }

        //捕获加载的数据
        Map<String,Object> resultMap = new HashMap<>();
        //遍历数据
        for(Long groupId : longList){
            try {
                Long ordinaryRoot = rootMap.get(groupId);
                if(ordinaryRoot != null && rootSet.contains(ordinaryRoot)){
                    //获取群组数据，从根节点遍历
                    List<PileGroupForEmsVOcopy> pileGroupForEms = getPileGroupForEms(ordinaryRoot);
                    //遍历群组数据
                    if(pileGroupForEms != null && pileGroupForEms.size() > 0 ){
                        String key = idMap.get(groupId);
                        for(PileGroupForEmsVOcopy item : pileGroupForEms){
                            //递归遍历
                            handleChildrenData(item,resultMap,key);
                        }
                    }
                    //移除,减少遍历
                    rootSet.remove(ordinaryRoot);
                }
            }catch (Exception e){
                log.error("加载EMS桩枪数据，获取群组信息异常:{}",e);
            }
        }
        log.info("加载EMS桩枪对应关系的数据：{}",JSON.toJSONString(resultMap));
    }

    /**
     * 递归遍历
     */
    private void handleChildrenData(PileGroupForEmsVOcopy pileGroupForEmsVO,Map<String,Object> resultMap,String key){
        //遍历枪的数据
        handlePileData(pileGroupForEmsVO.getGroupPileDTOList(),resultMap,key);
        //遍历下级
        if(pileGroupForEmsVO.getChildrenList() != null && pileGroupForEmsVO.getChildrenList().size() >0){
            List<PileGroupForEmsVOcopy> childrenList = pileGroupForEmsVO.getChildrenList();
            for(PileGroupForEmsVOcopy item : childrenList){
                //递归遍历
                handleChildrenData(item,resultMap,key);
            }
        }
    }

    /**
     * 遍历桩的数据
     * @param groupPileDTOList
     */
    private void handlePileData(List<GroupPileEmsVO> groupPileDTOList,Map<String,Object> resultMap,String key){
        if(groupPileDTOList != null && groupPileDTOList.size() >0){
            for(GroupPileEmsVO item : groupPileDTOList){
                String pileSn = item.getPileSn();
                List<String> evseSns = new ArrayList<>();
                if(item.getEvseInfoList() != null && item.getEvseInfoList().size() >0){
                    for(GroupPileEvseV3DTO gun: item.getEvseInfoList()){
                        evseSns.add(gun.getEvseSn());
                    }
                }
                if(evseSns.size() >0){
                    PileGunDTO pileGunDTO = new PileGunDTO();
                    pileGunDTO.setPileSn(pileSn);
                    pileGunDTO.setGroupId(key.split(":")[3]);
                    pileGunDTO.setEvseSns(evseSns);
                    resultMap.put(pileSn,pileGunDTO);
                    stringRedisTemplate.opsForHash().put(key,pileSn,JSON.toJSONString(pileGunDTO));
                }
            }
        }
    }

    /**
     * 更新群组的数据
     */
    private void updatePileData(){
        loadEmsGroupData();
    }

    @Override
    public List<Integer> getLoadTypeWhiteList(Long userId) {
        log.info("getLoadTypeWhiteList,userId={}", userId);
        Boolean alm = redisTemplate.opsForHash().hasKey(RedisKeyConstant.getHashPileBaseAlmUserInfo(), userId.toString());
        Boolean ems = redisTemplate.opsForHash().hasKey(RedisKeyConstant.getHashPileBaseEmsUserInfo(), userId.toString());
        Boolean demand = redisTemplate.opsForHash().hasKey(RedisKeyConstant.getHashPileBaseDemandUserInfo(), userId.toString());
        List<Integer> list = new ArrayList<>();
        if (alm){
            list.add(1);
        }
        if (ems){
            list.add(2);
        }
        if (demand){
            list.add(3);
        }
        return list;
    }


    @Override
    public PileGroupDetailV3VOcopy detailV3(Long groupId) {
        log.info("查询group数据，groupId:{}", groupId);
        OpLocationPileGroupEntity opLocationPileGroup = opLocationPileGroupRepository.getById(groupId);
        if (opLocationPileGroup == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        List<Long> meterIds = new ArrayList<>();
        List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(groupId, false);
        if (!CollectionUtils.isEmpty(children)) {
            List<OpLocationPileGroupEntity> entityList = children.stream().filter(e -> e.getMeterId() != null && e.getLoadType() == 1).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(entityList)) {
                meterIds.addAll(entityList.stream().map(OpLocationPileGroupEntity::getMeterId).collect(Collectors.toList()));
            }
        }
        PileGroupDetailV3VOcopy result = SmartChargeGroupTypeMapper.INSTANCE.entity2VO(opLocationPileGroup);
        if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1) {
            LocationMeterEntity locationMeterEntity = locationMeterRepository.getById(opLocationPileGroup.getMeterId());
            if (locationMeterEntity != null) {
                result.setMeterName(locationMeterEntity.getName());
                result.setMeterBrandEnum(locationMeterEntity.getBrandEnum());
            }
            meterIds.add(opLocationPileGroup.getMeterId());
        }
        Map<String, MeterDataRecordVO> meterMap = new HashMap<>();
        if (!meterIds.isEmpty()) {
            meterMap.putAll(this.getMeterRecord(meterIds));
        }

        // 找该组内的  桩
        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(groupId);
        List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> list = opLocationPileEvseService.findList(pileSnList);
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = new HashMap<>();
        try {
            pileDtoMap.putAll(list.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity())));
        } catch (Exception e) {
            // 理论 PileSn 唯一 历史垃圾数据会不唯一那么就取第一个作为map的value
            log.warn("data Error   pileSnList = {} \n data error list={}", JSON.toJSONString(pileSnList), JSON.toJSONString(list));
            pileDtoMap.putAll(list.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity(), (f, s) -> f)));
        }
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);// 里面有当 前SOC
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = new HashMap<>();
        try {
            evseMeterDtoMap.putAll(evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, Functions.identity())));
        } catch (Exception e) {
            // 理论 evseSN 唯一 历史垃圾数据会不唯一那么就取第一个作为map的value
            log.warn("data Error  evseSnList = {} \n evseMeterDtoList={}", JSON.toJSONString(evseSnList), JSON.toJSONString(evseMeterDtoList));
            evseMeterDtoMap.putAll(evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, Functions.identity(), (f, s) -> f)));
        }
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        Map<String, ChargingProfileSettingVO> profileMap = new HashMap<>();
        if (profileList != null && !CollectionUtils.isEmpty(profileList)) {
            try {
                profileMap.putAll(profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, Functions.identity())));
            } catch (Exception e) {
                log.warn("data Error  ChargingProfileSettingVO = {} \n ChargingProfileSettingVO={}", JSON.toJSONString(evseMeterDtoList), JSON.toJSONString(profileList));
                profileMap.putAll(profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, Functions.identity(), (f, s) -> f)));
            }
        }
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        String zoneId = getZoneId(opLocationPileGroup.getLocationId());
        result.setZoneId(zoneId);
        List<OpLocationPileGroupAssociateEntity> associateEntities = associateEntityMap.get(opLocationPileGroup.getId());// 智能充电组关联的桩SN
//        log.info("groupId= {},  associate piles {}", groupId, JSON.toJSONString(associateEntities));
        //当前登录用户为员工过滤数据
        boolean isSeller = LoginUserUtil.isSellerAdmin();
        List<GroupPileV3DTO> groupPileList = getGroupPileDtoListV3(associateEntities, pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, zoneId, isSeller);
//        log.info("groupId= {},  GroupPileV3DTO piles {}", groupId, JSON.toJSONString(groupPileList));
        result.setGroupPileDTOList(groupPileList);// 设置根组下面桩的数据
        Integer timeSettingMode = result.getTimeSettingMode();
        if (timeSettingMode != null && timeSettingMode == 1 && !CollectionUtils.isEmpty(result.getTimeSettingDetail())) {
            BigDecimal chargingUp = this.getChargingUp(System.currentTimeMillis(), zoneId, null, result.getTimeSettingDetail());
            if (chargingUp != null) {
                result.setChargingUp(chargingUp);
            }
        }
        setGroupTimeValuesV3(result);
        if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1 && !CollectionUtils.isEmpty(meterMap)) {
            this.setMeteValueV3(opLocationPileGroup.getMeterId(), meterMap, result, opLocationPileGroup.getMeterLocation());
        }
        if (Objects.nonNull(opLocationPileGroup.getPriority()) && opLocationPileGroup.getPriority() == 1) {
            try {
                List<VipPolicy> groupVipPolicy = opLocationPileVipConfigRepository.findByGroupId(groupId);
                result.setVipPolicy(groupVipPolicy);
            } catch (Exception e) {
                log.error("groupId = " + groupId, e);
            }
        }

        // 找该组内的  子智能充电组
        List<OpLocationPileGroupEntity> subGroup = opLocationPileGroupRepository.findChildrenPageListV3(groupId);
        if (CollectionUtils.isEmpty(subGroup)) {
            log.info("分页单层数据， groupId：{}", groupId);
            result.setGroupPileDTOList(getGroupPilePageListV3(groupId, isSeller));
            result.setSingleLayerEnabled(true);
            setSingleRootGroupTimeValuesV3(groupId, result, isSeller);
        } else {
            result.setChildrenList(new ArrayList<>());
            subGroup.forEach(t -> result.getChildrenList().add(detailV3(t.getId())));
            result.setSingleLayerEnabled(false);
            setRootGroupTimeValuesV3(groupId, result, meterMap, isSeller);
        }
        result.setTotalPile(getTotalPileCount(result));
        result.setGroupType(opLocationPileGroup.getGroupType());
        return result;
    }


    @Override
    public PileGroupDetailV3VOcopy queryDetailForEms(Long groupId) {
        log.info("查询group数据，groupId:{}", groupId);
        OpLocationPileGroupEntity opLocationPileGroup = opLocationPileGroupRepository.getById(groupId);
        if (opLocationPileGroup == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        List<Long> meterIds = new ArrayList<>();
        List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(groupId, false);
        if (!CollectionUtils.isEmpty(children)) {
            List<OpLocationPileGroupEntity> entityList = children.stream().filter(e -> e.getMeterId() != null && e.getLoadType() == 1).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(entityList)) {
                meterIds.addAll(entityList.stream().map(OpLocationPileGroupEntity::getMeterId).collect(Collectors.toList()));
            }
        }
        PileGroupDetailV3VOcopy result = SmartChargeGroupTypeMapper.INSTANCE.entity2VO(opLocationPileGroup);
        if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1) {
            LocationMeterEntity locationMeterEntity = locationMeterRepository.getById(opLocationPileGroup.getMeterId());
            if (locationMeterEntity != null) {
                result.setMeterName(locationMeterEntity.getName());
            }
            meterIds.add(opLocationPileGroup.getMeterId());
        }
        Map<String, MeterDataRecordVO> meterMap = new HashMap<>();
        if (!meterIds.isEmpty()) {
            meterMap.putAll(this.getMeterRecord(meterIds));
        }

        // 找该组内的  桩
        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(groupId);
        List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> list = opLocationPileEvseService.findList(pileSnList);
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = new HashMap<>();
        try {
            pileDtoMap.putAll(list.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity())));
        } catch (Exception e) {
            // 理论 PileSn 唯一 历史垃圾数据会不唯一那么就取第一个作为map的value
            log.warn("data Error   pileSnList = {} \n data error list={}", JSON.toJSONString(pileSnList), JSON.toJSONString(list));
            pileDtoMap.putAll(list.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity(), (f, s) -> f)));
        }
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);// 里面有当 前SOC
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = new HashMap<>();
        try {
            evseMeterDtoMap.putAll(evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, Functions.identity())));
        } catch (Exception e) {
            // 理论 evseSN 唯一 历史垃圾数据会不唯一那么就取第一个作为map的value
            log.warn("data Error  evseSnList = {} \n evseMeterDtoList={}", JSON.toJSONString(evseSnList), JSON.toJSONString(evseMeterDtoList));
            evseMeterDtoMap.putAll(evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, Functions.identity(), (f, s) -> f)));
        }
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        Map<String, ChargingProfileSettingVO> profileMap = new HashMap<>();
        if (profileList != null && !CollectionUtils.isEmpty(profileList)) {
            try {
                profileMap.putAll(profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, Functions.identity())));
            } catch (Exception e) {
                log.warn("data Error  ChargingProfileSettingVO = {} \n ChargingProfileSettingVO={}", JSON.toJSONString(evseMeterDtoList), JSON.toJSONString(profileList));
                profileMap.putAll(profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, Functions.identity(), (f, s) -> f)));
            }
        }
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        String zoneId = getZoneId(opLocationPileGroup.getLocationId());
        List<OpLocationPileGroupAssociateEntity> associateEntities = associateEntityMap.get(opLocationPileGroup.getId());// 智能充电组关联的桩SN
//        log.info("groupId= {},  associate piles {}", groupId, JSON.toJSONString(associateEntities));
        //EMS默认为
        boolean isSeller = true;
        List<GroupPileV3DTO> groupPileList = getGroupPileDtoListV3(associateEntities, pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, zoneId, isSeller);
//        log.info("groupId= {},  GroupPileV3DTO piles {}", groupId, JSON.toJSONString(groupPileList));
        result.setGroupPileDTOList(groupPileList);// 设置根组下面桩的数据
        Integer timeSettingMode = result.getTimeSettingMode();
        if (timeSettingMode != null && timeSettingMode == 1 && !CollectionUtils.isEmpty(result.getTimeSettingDetail())) {
            BigDecimal chargingUp = this.getChargingUp(System.currentTimeMillis(), zoneId, null, result.getTimeSettingDetail());
            if (chargingUp != null) {
                result.setChargingUp(chargingUp);
            }
        }
        setGroupTimeValuesV3(result);
        if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1 && !CollectionUtils.isEmpty(meterMap)) {
            this.setMeteValueV3(opLocationPileGroup.getMeterId(), meterMap, result, opLocationPileGroup.getMeterLocation());
        }
        if (Objects.nonNull(opLocationPileGroup.getPriority()) && opLocationPileGroup.getPriority() == 1) {
            try {
                List<VipPolicy> groupVipPolicy = opLocationPileVipConfigRepository.findByGroupId(groupId);
                result.setVipPolicy(groupVipPolicy);
            } catch (Exception e) {
                log.error("groupId = " + groupId, e);
            }
        }

        // 找该组内的  子智能充电组
        List<OpLocationPileGroupEntity> subGroup = opLocationPileGroupRepository.findChildrenPageListV3(groupId);
        if (CollectionUtils.isEmpty(subGroup)) {
            log.info("分页单层数据， groupId：{}", groupId);
            result.setGroupPileDTOList(getGroupPilePageListV3(groupId, isSeller));
            result.setSingleLayerEnabled(true);
            setSingleRootGroupTimeValuesV3(groupId, result, isSeller);
        } else {
            result.setChildrenList(new ArrayList<>());
            subGroup.forEach(t -> result.getChildrenList().add(queryDetailForEms(t.getId())));
            result.setSingleLayerEnabled(false);
            setRootGroupTimeValuesV3(groupId, result, meterMap, isSeller);
        }
        result.setTotalPile(getTotalPileCount(result));
        result.setGroupType(opLocationPileGroup.getGroupType());
        return result;
    }



    public static int getTotalPileCount(PileGroupDetailV3VOcopy pileGroup) {
        if (pileGroup == null) {
            return 0;
        }
        int count = (pileGroup.getGroupPileDTOList() == null) ? 0 : pileGroup.getGroupPileDTOList().size();
        if (pileGroup.getChildrenList() != null) {
            for (PileGroupDetailV3VOcopy child : pileGroup.getChildrenList()) {
                count += getTotalPileCount(child); // 递归调用
            }
        }
        return count;
    }


    @Override
    public Result<OpLocationPileGroupV2VO> detail(Long groupId, Integer page, Integer pageSize) {
        OpLocationPileGroupV2VO result = new OpLocationPileGroupV2VO();
        log.info("查询group数据，groupId:{}", groupId);
        OpLocationPileGroupEntity opLocationPileGroup = opLocationPileGroupRepository.getById(groupId);
        if (opLocationPileGroup == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        List<Long> meterIds = new ArrayList<>();
        List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(groupId, false);
        if (!CollectionUtils.isEmpty(children)) {
            List<OpLocationPileGroupEntity> entityList = children.stream().filter(e -> e.getMeterId() != null && e.getLoadType() == 1).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(entityList)) {
                meterIds.addAll(entityList.stream().map(OpLocationPileGroupEntity::getMeterId).collect(Collectors.toList()));
            }
        }

        BeanUtils.copyProperties(opLocationPileGroup, result);
        this.setTimeSettingDetail(opLocationPileGroup, result);
        if (result.getSecurityEnabled() == null) {
            result.setSecurityEnabled(false);
        }

        if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1) {
            LocationMeterEntity locationMeterEntity = locationMeterRepository.getById(opLocationPileGroup.getMeterId());
            if (locationMeterEntity != null) {
                result.setMeterName(locationMeterEntity.getName());
            }
            meterIds.add(opLocationPileGroup.getMeterId());
        }
        Map<String, MeterDataRecordVO> meterMap = null;
        if (meterIds.size() > 0) {
            meterMap = this.getMeterRecord(meterIds);
        }

        Page<OpLocationPileGroupEntity> groupPageResult = opLocationPileGroupRepository.findChildrenPageList(groupId, page, pageSize);
        if (groupPageResult == null || CollectionUtils.isEmpty(groupPageResult.getRecords())) {
            log.info("分页单层数据， groupId：{}", groupId);
            result.setGroupPileDTOList(getGroupPilePageList(groupId, page, pageSize));
            result.setChildrenList(null);
            result.setSingleLayerEnabled(true);
            setSingleRootGroupTimeValues(groupId, result);
            if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1 && !CollectionUtils.isEmpty(meterMap)) {
                this.setMeteValue(opLocationPileGroup.getMeterId(), meterMap, result, opLocationPileGroup.getMeterLocation());
            }
            return Result.ofSucceed(result);
        }

        result.setChildrenList(getChildrenList(groupId, page, pageSize, groupPageResult, meterMap));
        result.setSingleLayerEnabled(false);
        result.setGroupPileDTOList(null);
        setRootGroupTimeValues(groupId, result, meterMap);
        if (opLocationPileGroup.getMeterId() != null && opLocationPileGroup.getLoadType() == 1 && !CollectionUtils.isEmpty(meterMap)) {
            this.setMeteValue(opLocationPileGroup.getMeterId(), meterMap, result, opLocationPileGroup.getMeterLocation());
        }
        return Result.ofSucceed(result);
    }



    @Override
    public PileGroupDetailV3VOcopy findGroupDetailByPileSn(String pileSn) {
        return null;
    }

    @Override
    public boolean isEnableSmartChargePile(String pileSn) {
        return false;
    }

    private void setTimeSettingDetailV3(OpLocationPileGroupEntity entity, PileGroupDetailV3VOcopy vo) {
        String detail = entity.getTimeSettingDetail();
        if (StringUtils.isNotBlank(detail)) {
            List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
            log.info("timeSettingDetailDTOS {}", JSON.toJSONString(timeSettingDetailDTOS));
            vo.setTimeSettingDetail(timeSettingDetailDTOS.stream().sorted((f, s) -> (int) (f.getId() - s.getId())).collect(Collectors.toList()));
        }
    }

    private void setTimeSettingDetail(OpLocationPileGroupEntity entity, OpLocationPileGroupV2VO vo) {
        String detail = entity.getTimeSettingDetail();
        if (StringUtils.isNotBlank(detail)) {
            List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
            vo.setTimeSettingDetails(timeSettingDetailDTOS.stream().sorted((f, s) -> (int) (f.getId() - s.getId())).collect(Collectors.toList()));
        }
    }

    private void setMeteValueV3(Long meterId, Map<String, MeterDataRecordVO> meterMap, PileGroupDetailV3VOcopy result, Integer meterLocation) {
        if (CollectionUtils.isEmpty(meterMap)) {
            return;
        }
        MeterDataRecordVO recordVO = meterMap.get(meterId.toString());
        if (recordVO != null) {
            Boolean powerEquipmentEnabled = result.getPowerEquipmentEnabled();
            BigDecimal powerEquipmentUp = result.getPowerEquipmentUp();
            BigDecimal electricUp = result.getElectricUp();
            if (powerEquipmentEnabled != null && powerEquipmentEnabled){
                MeterDataRecordVO.Meter meter = recordVO.getMeters().get(0);
                BigDecimal value1 = Optional.ofNullable(meter.getL1()).orElse(BigDecimal.ZERO).add(Optional.ofNullable(meter.getL2()).orElse(BigDecimal.ZERO)).add(Optional.ofNullable(meter.getL3()).orElse(BigDecimal.ZERO));
                BigDecimal value2 = Optional.ofNullable(meter.getL4()).orElse(BigDecimal.ZERO).add(Optional.ofNullable(meter.getL5()).orElse(BigDecimal.ZERO)).add(Optional.ofNullable(meter.getL6()).orElse(BigDecimal.ZERO));
                if (value1.compareTo(value2) > 0) {
                    result.setChargingUp(electricUp);
                    result.setMeterValue(value1.setScale(2, BigDecimal.ROUND_HALF_UP));
                }else {
                    result.setChargingUp(powerEquipmentUp);
                    result.setMeterValue(value2.setScale(2, BigDecimal.ROUND_HALF_UP));
                }
            }else {
                BigDecimal max = DlbUtil.getAlmValue(recordVO);
                String whiteKey = "energy:pile:base:alm:meter:white:future:" + meterId;
                String white = stringRedisTemplate.opsForValue().get(whiteKey);
                if (meterLocation == 1 && StringUtils.isNotBlank(white)) {
                    max = max.add(result.getTimeAmperage());
                }
                result.setMeterValue(max.setScale(2, BigDecimal.ROUND_HALF_UP));
            }
        }
    }

    private void setMeteValue(Long meterId, Map<String, MeterDataRecordVO> meterMap, OpLocationPileGroupV2VO result, Integer meterLocation) {
        if (CollectionUtils.isEmpty(meterMap)) {
            return;
        }
        MeterDataRecordVO recordVO = meterMap.get(meterId.toString());
        if (recordVO != null) {
            BigDecimal max = DlbUtil.getAlmValue(recordVO);
            String whiteKey = "energy:pile:base:alm:meter:white:future:" + meterId;
            String white = stringRedisTemplate.opsForValue().get(whiteKey);
            if (meterLocation == 1 && StringUtils.isNotBlank(white)) {
                max = max.add(result.getTimeAmperage());
            }
            result.setMeterValue(max.setScale(2, BigDecimal.ROUND_HALF_UP));
        }
    }


    private void setSingleRootGroupTimeValuesV3(Long groupId, PileGroupDetailV3VOcopy result, boolean isSeller) {
        PileGroupDetailV3VOcopy root = new PileGroupDetailV3VOcopy();
        root.setGroupPileDTOList(getGroupPilePageListV3(groupId, isSeller));
        setGroupTimeValuesV3(root);
        result.setTimeVoltage(root.getTimeVoltage());
        result.setTimeAmperage(root.getTimeAmperage());
        result.setTimePower(root.getTimePower());
    }


    private void setSingleRootGroupTimeValues(Long groupId, OpLocationPileGroupV2VO result) {
        OpLocationPileGroupV2VO root = new OpLocationPileGroupV2VO();
        root.setGroupPileDTOList(getGroupPilePageList(groupId, 1, Integer.MAX_VALUE));
        setGroupTimeValues(root);
        result.setTimeVoltage(root.getTimeVoltage());
        result.setTimeAmperage(root.getTimeAmperage());
        result.setTimePower(root.getTimePower());
    }

    /**
     * 查询双层组下所有子组数据
     *
     * @param groupId
     * @param result
     */
    private void setRootGroupTimeValuesV3(Long groupId, PileGroupDetailV3VOcopy result, Map<String, MeterDataRecordVO> meterMap, boolean isSeller) {
        PileGroupDetailV3VOcopy root = new PileGroupDetailV3VOcopy();
        List<OpLocationPileGroupEntity> groupPageResult = opLocationPileGroupRepository.findChildrenPageListV3(groupId);
        root.setChildrenList(getChildrenListV3(groupId, groupPageResult, meterMap, isSeller));
        setGroupTimeValuesV3(root);
        result.setTimeVoltage(root.getTimeVoltage());
        result.setTimeAmperage(root.getTimeAmperage());
        result.setTimePower(root.getTimePower());
    }

    /**
     * 查询双层组下所有子组数据
     *
     * @param groupId
     * @param result
     */
    private void setRootGroupTimeValues(Long groupId, OpLocationPileGroupV2VO result, Map<String, MeterDataRecordVO> meterMap) {
        OpLocationPileGroupV2VO root = new OpLocationPileGroupV2VO();
        Page<OpLocationPileGroupEntity> groupPageResult = opLocationPileGroupRepository.findChildrenPageList(groupId, 1, Integer.MAX_VALUE);
        root.setChildrenList(getChildrenList(groupId, 1, Integer.MAX_VALUE, groupPageResult, meterMap));
        setGroupTimeValues(root);
        result.setTimeVoltage(root.getTimeVoltage());
        result.setTimeAmperage(root.getTimeAmperage());
        result.setTimePower(root.getTimePower());
    }

    private List<PileGroupDetailV3VOcopy> getChildrenListV3(Long groupId, List<OpLocationPileGroupEntity> subGroup, Map<String, MeterDataRecordVO> meterMap, boolean isSeller) {
        List<PileGroupDetailV3VOcopy> pageResult = new ArrayList<>();
        log.info("查询双层数据， groupId:{}", groupId);
        Set<Long> groupIds = subGroup.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        log.info("获取childrenEntityList groupId列表：{}", JSON.toJSON(groupIds));
        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(groupIds);
        log.info("查询group下的子数据, size：{}", associateList == null ? 0 : associateList.size());
        if (CollectionUtils.isEmpty(associateList)) {
            return pageResult;
        }

        List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = opLocationPileEvseService.findList(pileSnList).stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = null;
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            evseMeterDtoMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<String, ChargingProfileSettingVO> profileMap = null;
        if (!CollectionUtils.isEmpty(profileList)) {
            profileMap = profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));

        List<PileGroupDetailV3VOcopy> childrenList = new ArrayList<>();
        Map<String, OpEvseMeterUploadDTO> finalEvseMeterDtoMap = evseMeterDtoMap;
        Map<String, ChargingProfileSettingVO> finalProfileMap = profileMap;
        subGroup.forEach(entity -> {
            PileGroupDetailV3VOcopy vo = new PileGroupDetailV3VOcopy();
            BeanUtils.copyProperties(entity, vo);
            this.setTimeSettingDetailV3(entity, vo);
            String zoneId = this.getZoneId(entity.getLocationId());
            List<GroupPileV3DTO> groupPileList = getGroupPileDtoListV3(associateEntityMap.get(entity.getId()), pileDtoMap, evseDtoMap, finalEvseMeterDtoMap, finalProfileMap, zoneId, isSeller);
            vo.setGroupPileDTOList(groupPileList);
            setGroupTimeValuesV3(vo);
            if (entity.getMeterId() != null && entity.getLoadType() == 1) {
                this.setMeteValueV3(entity.getMeterId(), meterMap, vo, entity.getMeterLocation());
            }
            childrenList.add(vo);
        });

        return childrenList;
    }

    private Page<OpLocationPileGroupV2VO> getChildrenList(Long groupId, int page, int pageSize, Page<OpLocationPileGroupEntity> groupPageResult, Map<String, MeterDataRecordVO> meterMap) {
        Page<OpLocationPileGroupV2VO> pageResult = new Page<>(page, pageSize);

        log.info("查询双层数据， groupId:{}", groupId);
        List<OpLocationPileGroupEntity> childrenEntityList = groupPageResult.getRecords();
        Set<Long> groupIds = childrenEntityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        log.info("获取childrenEntityList groupId列表：{}", JSON.toJSON(groupIds));

        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(groupIds);
        log.info("查询group下的子数据, size：{}", associateList == null ? 0 : associateList.size());
        if (CollectionUtils.isEmpty(associateList)) {
            pageResult.setTotal(0);
            pageResult.setRecords(new ArrayList<>());
            return pageResult;
        }

        List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = opLocationPileEvseService.findList(pileSnList).stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = null;
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            evseMeterDtoMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<String, ChargingProfileSettingVO> profileMap = null;
        if (!CollectionUtils.isEmpty(profileList)) {
            profileMap = profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));

        List<OpLocationPileGroupV2VO> childrenList = new ArrayList<>();
        Map<String, OpEvseMeterUploadDTO> finalEvseMeterDtoMap = evseMeterDtoMap;
        Map<String, ChargingProfileSettingVO> finalProfileMap = profileMap;
        childrenEntityList.forEach(entity -> {
            OpLocationPileGroupV2VO vo = new OpLocationPileGroupV2VO();
            BeanUtils.copyProperties(entity, vo);
            this.setTimeSettingDetail(entity, vo);
            String zoneId = this.getZoneId(entity.getLocationId());
            List<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPileList = getGroupPileDtoList(associateEntityMap.get(entity.getId()), pileDtoMap, evseDtoMap, finalEvseMeterDtoMap, finalProfileMap, zoneId);
            Page<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPilePageResult = new Page<OpLocationPileGroupV2VO.GroupPileV2DTO>(1, groupPileList.size());
            groupPilePageResult.setTotal(groupPileList.size());
            groupPilePageResult.setRecords(groupPileList);
            vo.setGroupPileDTOList(groupPilePageResult);
            setGroupTimeValues(vo);
            if (entity.getMeterId() != null && entity.getLoadType() == 1) {
                this.setMeteValue(entity.getMeterId(), meterMap, vo, entity.getMeterLocation());
            }
            childrenList.add(vo);
        });

        pageResult.setTotal(groupPageResult.getTotal());
        pageResult.setRecords(childrenList);
        return pageResult;
    }

    /**
     * 分页查询单层组下桩的数据
     *
     * @param groupId
     * @return
     */
    private List<GroupPileV3DTO> getGroupPilePageListV3(Long groupId, boolean isSeller) {
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findPageListV3(groupId);
        if (CollectionUtils.isEmpty(associateEntityList)) {
            return Collections.emptyList();
        }

        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = opLocationPileEvseService.findList(pileSnList).stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = null;
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            evseMeterDtoMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<String, ChargingProfileSettingVO> profileMap = null;
        if (!CollectionUtils.isEmpty(profileList)) {
            profileMap = profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateEntityList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        String zoneId = ZoneId.systemDefault().getId(); // todo this.getZoneId(associateEntityList.get(0).getLocationId());
        return getGroupPileDtoListV3(associateEntityMap.get(groupId), pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, zoneId, isSeller);
    }


    /**
     * 分页查询单层组下桩的数据
     *
     * @param groupId
     * @param page
     * @param pageSize
     * @return
     */
    private Page<OpLocationPileGroupV2VO.GroupPileV2DTO> getGroupPilePageList(Long groupId, int page, int pageSize) {
        Page<OpLocationPileGroupV2VO.GroupPileV2DTO> pageResult = new Page<>(page, pageSize);
        Page<OpLocationPileGroupAssociateEntity> associatePageResult = opLocationPileGroupAssociateRepository.findPageList(groupId, page, pageSize);
        if (associatePageResult == null || CollectionUtils.isEmpty(associatePageResult.getRecords())) {
            pageResult.setTotal(0);
            pageResult.setRecords(new ArrayList<>());
        }
        List<OpLocationPileGroupAssociateEntity> associateEntityList = associatePageResult.getRecords();

        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = opLocationPileEvseService.findList(pileSnList).stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = null;
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            evseMeterDtoMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<String, ChargingProfileSettingVO> profileMap = null;
        if (!CollectionUtils.isEmpty(profileList)) {
            profileMap = profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateEntityList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        pageResult.setTotal(associatePageResult.getTotal());
        String zoneId = this.getZoneId(associateEntityList.get(0).getLocationId());
        pageResult.setRecords(getGroupPileDtoList(associateEntityMap.get(groupId), pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, zoneId));
        return pageResult;
    }

    private List<ChargingProfileSettingVO> getProfileList(List<OpEvseMeterUploadDTO> evseMeterDtoList) {
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            List<OpEvseMeterUploadDTO> list = evseMeterDtoList.stream().filter(dto -> StringUtils.isNotBlank(dto.getBusId())).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(list)) {
                List<ProfileParamDTO> paramDto = list.stream().map(d -> {
                    ProfileParamDTO paramDTO = new ProfileParamDTO();
                    paramDTO.setEvseSn(d.getEvseSn());
                    paramDTO.setTransactionId(d.getBusId());
                    return paramDTO;
                }).collect(Collectors.toList());
                return smartChargeFeign.lastProfileList(paramDto).getData();
            }
        }
        return Collections.emptyList();
    }

    private List<GroupPileV3DTO> getGroupPileDtoListV3(
            List<OpLocationPileGroupAssociateEntity> associateEntityList,
            Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
            Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap,
            Map<String, ChargingProfileSettingVO> profileMap,
            String zoneId, boolean isSeller) {
        List<GroupPileV3DTO> pileResultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(associateEntityList)) {
            return pileResultList;
        }
        List<String> existList = new ArrayList<>();
        if (!isSeller) {
            Collection<Long> locationIds = this.pileUserServiceAdapter.getLocationIdsByUserId();
            if (!CollectionUtils.isEmpty(locationIds)) {
                List<OpLocationPileEvseVO> pileDtoList = this.opLocationPileEvseService.getListByLocationIds(new ArrayList<>(locationIds)).getData();
                if (!CollectionUtils.isEmpty(pileDtoList)) {
                    existList.addAll(pileDtoList.stream().map(OpLocationPileEvseVO::getPileSn).collect(Collectors.toList()));
                }
            }
        }
        associateEntityList.forEach(entity -> {
            String pileSn = entity.getPileSn();
            OpLocationPileEvseElasticDTO pile = pileDtoMap.get(pileSn);
            GroupPileV3DTO pileDto = GroupPileEvseTypeMapper.INSTANCE.dto2DTO(pile);
            Assert.notNull(evseDtoMap.get(pileSn), "pileSn = " + pileSn);
            if (!isSeller) {
                boolean havePermission = false;
                if (existList.contains(pileSn)) {
                    havePermission = true;
                }
                pileDto.setHavePermission(havePermission);
            }
            pileDto.setEvseInfoList(
                    evseDtoMap.get(pileSn).stream().sorted(Comparator.comparing(OpLocationEvseElasticDTO::getEvseSn)).map(evse -> {
                        GroupPileEvseV3DTO evseDto = GroupPileEvseTypeMapper.INSTANCE.dto2DTO(evse);
                        evseDto.setTimeAmperage(evse.getAmperage());
                        evseDto.setVoltage(evse.getVoltage());
                        evseDto.setFast(this.getFastValue(evse.getEvseSn()));
                        evseDto.setPause(this.getPauseValue(evse.getEvseSn()));
                        evseDto.setPower(BigDecimal.valueOf(Optional.ofNullable(evse.getPower()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
                        this.setTimeValueV3(evseMeterDtoMap, evseDto, zoneId);
                        this.setEndTimeV3(profileMap, evseDto, zoneId);
                        return evseDto;
                    }).collect(Collectors.toList())
            );
            pileResultList.add(pileDto);
        });
        return pileResultList;
    }

    private List<OpLocationPileGroupV2VO.GroupPileV2DTO> getGroupPileDtoList(
            List<OpLocationPileGroupAssociateEntity> associateEntityList,
            Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
            Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap,
            Map<String, ChargingProfileSettingVO> profileMap,
            String zoneId) {
        List<OpLocationPileGroupV2VO.GroupPileV2DTO> pileResultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(associateEntityList)) {
            return pileResultList;
        }
        associateEntityList.stream().forEach(entity -> {
            String pileSn = entity.getPileSn();
            OpLocationPileGroupV2VO.GroupPileV2DTO pileDto = new OpLocationPileGroupV2VO.GroupPileV2DTO();
            OpLocationPileEvseElasticDTO pile = pileDtoMap.get(pileSn);
            BeanUtils.copyProperties(pile, pileDto);
            pileDto.setGroupPileEvseDTOList(
                    evseDtoMap.get(pileSn).stream().sorted(Comparator.comparing(OpLocationEvseElasticDTO::getEvseSn)).map(evse -> {
                        OpLocationPileGroupV2VO.GroupPileEvseDTO evseDto = new OpLocationPileGroupV2VO.GroupPileEvseDTO();
                        BeanUtils.copyProperties(evse, evseDto);
                        evseDto.setTimeAmperage(evse.getAmperage());
                        evseDto.setVoltage(evse.getVoltage());
                        evseDto.setFast(this.getFastValue(evse.getEvseSn()));
                        evseDto.setPause(this.getPauseValue(evse.getEvseSn()));
                        evseDto.setPower(new BigDecimal(Optional.ofNullable(evse.getPower()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
                        this.setTimeValue(evseMeterDtoMap, evseDto, zoneId);
                        this.setEndTime(profileMap, evseDto, zoneId);
                        return evseDto;
                    }).collect(Collectors.toList())
            );
            pileResultList.add(pileDto);
        });
        return pileResultList;
    }

    private void setEndTimeV3(Map<String, ChargingProfileSettingVO> profileMap, GroupPileEvseV3DTO evseDto, String zoneId) {
        String evseSn = evseDto.getEvseSn();
        if (!CollectionUtils.isEmpty(profileMap)) {
            ChargingProfileSettingVO vo = profileMap.get(evseSn);
            if (vo != null && EvseDeviceStatusEnum.CHARGING.getName().equals(evseDto.getState())) {
                evseDto.setEndTime(vo.getPlanEndTime());
                if (vo.getPlanEndTime() != null) {
                    String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(vo.getPlanEndTime()), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                    evseDto.setFormatEndTime(format);
                }
            }
        }
    }

    private void setEndTime(Map<String, ChargingProfileSettingVO> profileMap, OpLocationPileGroupV2VO.GroupPileEvseDTO evseDto, String zoneId) {
        String evseSn = evseDto.getEvseSn();
        if (!CollectionUtils.isEmpty(profileMap)) {
            ChargingProfileSettingVO vo = profileMap.get(evseSn);
            if (vo != null && EvseDeviceStatusEnum.CHARGING.getName().equals(evseDto.getState())) {
                evseDto.setEndTime(vo.getPlanEndTime());
                if (vo.getPlanEndTime() != null) {
                    String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(vo.getPlanEndTime()), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                    evseDto.setFormatEndTime(format);
                }
            }
        }
    }

    private Integer getFastValue(String evseSn) {
        String redisValue = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getSmartChargeFastKey(evseSn));
        if (StringUtils.isNotBlank(redisValue)) {
            return Integer.valueOf(redisValue);
        }
        return 0;
    }

    private void setFastValue(String evseSn, Integer fast) {
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getSmartChargeFastKey(evseSn), fast.toString(), 30L, TimeUnit.DAYS);
    }

    private Integer getPauseValue(String evseSn) {
        String redisValue = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getSmartChargePauseKey(evseSn));
        if (StringUtils.isNotBlank(redisValue)) {
            return Integer.valueOf(redisValue);
        }
        return 0;
    }

    private void setPauseValue(String evseSn, Integer pause) {
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getSmartChargePauseKey(evseSn), pause.toString(), 30L, TimeUnit.DAYS);
    }

//    private void setTimeValueV3(Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap, GroupPileEvseV3DTO evseDto, String zoneId) {
//        if (!CollectionUtils.isEmpty(evseMeterDtoMap)) {
//            String evseSn = evseDto.getEvseSn();
//            String state = evseDto.getState();
//            OpEvseMeterUploadDTO meterDto = evseMeterDtoMap.get(evseSn);
//            if (meterDto != null) {
//                //充电中读取电流、功率
//                EnergyBillVO billVO = billFeignClient.findLastBillByEvse(evseSn).getData();
//                if (EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(state) && billVO != null && billVO.getOrderSeq() != null && billVO.getOrderSeq().equals(meterDto.getBusId())) {
//                    evseDto.setTimeAmperage(BigDecimal.valueOf(Optional.ofNullable(meterDto.getCurrent()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
//                    evseDto.setTimePower(BigDecimal.valueOf(Optional.ofNullable(meterDto.getPower()).orElse(0D) / 1000).setScale(2, BigDecimal.ROUND_DOWN));
//                    if (billVO.getCreateTime() != null) {
//                        evseDto.setStartTime(billVO.getCreateTime());
//                        String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(billVO.getCreateTime()), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
//                        evseDto.setFormatStartTime(format);
//                    }
//                    evseDto.setStartSOC(billVO.getStartSoc());
//                    evseDto.setCurrentSOC(Optional.of(meterDto).map(OpEvseMeterUploadDTO::getBatterySoc).orElse(null));// 当前SOC
//                    evseDto.setTargetSOC(BigDecimal.valueOf(100));// 目标SOC
//                } else {
//                    evseDto.setTimeAmperage(BigDecimal.ZERO);
//                    evseDto.setTimePower(BigDecimal.ZERO);
//                }
//                evseDto.setTimeVoltage(BigDecimal.valueOf(Optional.ofNullable(meterDto.getVoltage()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
//            }
//        }
//    }

    private void setTimeValueV3(Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap, GroupPileEvseV3DTO evseDto, String zoneId) {
        if (!CollectionUtils.isEmpty(evseMeterDtoMap)) {
            String evseSn = evseDto.getEvseSn();
            String state = evseDto.getState();
            OpEvseMeterUploadDTO meterDto = evseMeterDtoMap.get(evseSn);
            if (meterDto != null) {
                //充电中读取电流、功率
                BillInfoVO billVO = billFeignClient.getBillByOrderSeqFromDB(meterDto.getBusId()).getData();
                if (EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(state) && billVO != null && billVO.getOrderSeq() != null && billVO.getOrderSeq().equals(meterDto.getBusId())) {
                    evseDto.setTimeAmperage(BigDecimal.valueOf(Optional.ofNullable(meterDto.getCurrent()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
                    evseDto.setTimePower(BigDecimal.valueOf(Optional.ofNullable(meterDto.getPower()).orElse(0D) / 1000).setScale(2, BigDecimal.ROUND_DOWN));
                    if (billVO.getStartTime() != null) {
                        evseDto.setStartTime(billVO.getStartTime());
                        String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(billVO.getStartTime()), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                        evseDto.setFormatStartTime(format);
                    }
//                    evseDto.setStartSOC(billVO.getStartSoc());
                    evseDto.setCurrentSOC(Optional.of(meterDto).map(OpEvseMeterUploadDTO::getBatterySoc).orElse(null));// 当前SOC
                    evseDto.setTargetSOC(BigDecimal.valueOf(100));// 目标SOC
                } else {
                    evseDto.setTimeAmperage(BigDecimal.ZERO);
                    evseDto.setTimePower(BigDecimal.ZERO);
                }
                evseDto.setTimeVoltage(BigDecimal.valueOf(Optional.ofNullable(meterDto.getVoltage()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
            }
        }
    }


    private void setTimeValue(Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap, OpLocationPileGroupV2VO.GroupPileEvseDTO evseDto, String zoneId) {
        if (!CollectionUtils.isEmpty(evseMeterDtoMap)) {
            String evseSn = evseDto.getEvseSn();
            String state = evseDto.getState();
            OpEvseMeterUploadDTO meterDto = evseMeterDtoMap.get(evseSn);
            if (meterDto != null) {
                //充电中读取电流、功率
                EnergyBillVO billVO = billFeignClient.findLastBillByEvse(evseSn).getData();
                if (EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(state) && billVO != null && billVO.getOrderSeq() != null && billVO.getOrderSeq().equals(meterDto.getBusId())) {
                    evseDto.setTimeAmperage(new BigDecimal(Optional.ofNullable(meterDto.getCurrent()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
                    evseDto.setTimePower(new BigDecimal(Optional.ofNullable(meterDto.getPower()).orElse(0D) / 1000).setScale(2, BigDecimal.ROUND_DOWN));
                    if (billVO.getCreateTime() != null) {
                        evseDto.setStartTime(billVO.getCreateTime());
                        String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(billVO.getCreateTime()), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                        evseDto.setFormatStartTime(format);
                    }
                } else {
                    evseDto.setTimeAmperage(BigDecimal.ZERO);
                    evseDto.setTimePower(BigDecimal.ZERO);
                }
                evseDto.setTimeVoltage(new BigDecimal(Optional.ofNullable(meterDto.getVoltage()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
            }
        }
    }

    /**
     * 桩(组)范围展示：
     * 随订阅上线，则
     * 仅展示商户下有licence订阅的桩列表
     * 可跨场站设置
     * 展示层级优先级为 组  → 桩，桩展示场站信息
     * 若订阅为上，则
     * 所选桩范围仍选择场站后，再在该场站下选择
     * 可选项控制：
     * 1、ALM仅可选AC桩
     * 2、DLB可选所有未被同一层级选择的桩(组)
     * 3、AC/DC桩混排规则：
     * 若选择DC桩，则组内需均为DC桩；
     * 若选择AC桩，则组内可AC & DC混排
     * 4、DLB若为多层组，则其所包含的组充电策略单位(kW/A)需相同
     *
     * @param paramDto
     * @return
     */
    @Override
    public Result<List<PileGroupTreeVOcopy>> queryPileInMerchant(PileEvseGroupParamDTO paramDto, long merchantId, String zoneId) {
        String keyWord=paramDto.getKeyword();
        String searchType=paramDto.getSearchType();
        paramDto.setKeyword("");
        List<PileGroupTreeVOcopy> result = queryPileAndGroup(paramDto, merchantId, zoneId);
        return Result.ofSucceed(filterByKeyword(result,keyWord,searchType));
    }
    // 过滤方法
    public List<PileGroupTreeVOcopy> filterByKeyword(List<PileGroupTreeVOcopy> list, String keyword,String searchType) {
        return list.stream()
                .map(item -> filterItem(item, keyword,searchType))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    // 递归过滤每个元素及其子元素
    private PileGroupTreeVOcopy filterItem(PileGroupTreeVOcopy item, String keyword,String searchType) {
        if (StringUtils.equals(BaseConstant.GROUP_SEARCH_TYPE,searchType)){
            return item.getGroupName() != null && item.getGroupName().contains(keyword)?item:null;
        }
        // 检查当前元素是否符合条件
        boolean matches = (item.getPileName() != null && item.getPileName().contains(keyword)) ||
                (item.getPileSn() != null && item.getPileSn().contains(keyword)) ||
                (item.getLocationName() != null && item.getLocationName().contains(keyword)) ||
                (item.getGroupName() != null && item.getGroupName().contains(keyword));

        // 递归检查子元素
        if (item.getChildren() != null) {
            List<PileGroupTreeVOcopy> filteredChildren = item.getChildren().stream()
                    .map(child -> filterItem(child, keyword,searchType))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());

            // 如果有子元素符合条件，设置为过滤后的子元素列表
            if (!filteredChildren.isEmpty()) {
                item.setChildren(filteredChildren);
                matches = true; // 如果子元素符合条件，则父元素也符合条件
            }
        }

        // 如果当前元素及其子元素都不符合条件，返回null
        return matches ? item : null;
    }


    public List<PileGroupTreeVOcopy> queryPileAndGroup(PileEvseGroupParamDTO paramDto, long merchantId, String zoneId) {
        List<PileGroupTreeVOcopy> groupTreeVOs = new ArrayList<>();
        List<PileGroupTreeVOcopy> result = new ArrayList<>();


        final Map<Long, List<OpLocationPileGroupAssociateVO>> associateMap = new HashMap<>();

        final Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = new HashMap<>();

        final Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = new HashMap<>();
        Map<Long,OpLocationElasticDTO> locationMap = new HashMap<>();

        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 查询场站里面未加入组的桩 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓  getZoneId
        List<Long> locationIds = this.pileUserFeign.getLocationIds().getData();
        if (!CollectionUtils.isEmpty(locationIds)) {
            String keyword = paramDto.getKeyword();
            List<OpLocationElasticDTO> locationDtoList = this.opLocationService.findByIds(locationIds, null);
            locationMap.putAll(locationDtoList.stream().collect(Collectors.toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f)));
            for (OpLocationElasticDTO locationDto : locationDtoList) {
                Long locationId = locationDto.getId();
                String locationName = locationDto.getName();
                paramDto.setLocationId(locationId);
                LocationPileDTO locationPileDTO = queryPileInLocationV3(paramDto);
                Map<Long, List<OpLocationPileGroupAssociateVO>> tmpMap = locationPileDTO.getAssociateMap();
                if (!CollectionUtils.isEmpty(tmpMap)) {
                    tmpMap.forEach((k, v) -> associateMap.merge(k, v, (o, n) -> {
                        o.addAll(n);
                        return o;
                    }));
                }
                pileDtoMap.putAll(locationPileDTO.getPileDtoMap());
                evseDtoMap.putAll(locationPileDTO.getEvseDtoMap());

                for (OpLocationPileEvseGroupListVO.PileInfoVo pileInfoVo : locationPileDTO.getPileInfoVoList()) {
                    PileGroupTreeVOcopy pileGroupTreeVO = new PileGroupTreeVOcopy();
                    pileGroupTreeVO.setMerchantId(merchantId);
                    pileGroupTreeVO.setPileId(pileInfoVo.getPileId());
                    pileGroupTreeVO.setPileName(pileInfoVo.getPileName());
                    pileGroupTreeVO.setPileSn(pileInfoVo.getPileSn());
                    pileGroupTreeVO.setPower(pileInfoVo.getPower());
                    pileGroupTreeVO.setPowerType(pileInfoVo.getPowerType());
                    pileGroupTreeVO.setEvseInfoList(pileInfoVo.getEvseInfoList());
                    pileGroupTreeVO.setLocationId(locationId);
                    pileGroupTreeVO.setLocationName(locationName);
                    groupTreeVOs.add(pileGroupTreeVO);
                }
            }
        }
        log.info("所有场站的桩  groupTreeVOs: {}", JSON.toJSONString(groupTreeVOs));
        if (this.checkAlmForAutel(paramDto)) {
            return groupTreeVOs;
        }
        // 过滤掉 没有运营权益的桩  去掉 20230905 修改  查询商家下的所有桩 不用过滤是否有license

        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 查询场站里面未加入组的桩 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 查询商家下加入组的桩 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        List<Long> groupIdsFilterByKeyword = new ArrayList<>();
        if (org.springframework.util.StringUtils.hasText(paramDto.getKeyword())) {
            groupIdsFilterByKeyword.addAll(opLocationPileGroupAssociateRepository.findGroupIdByMerchantIdAndPileSnAndGroupName(merchantId, paramDto.getKeyword(), paramDto.getKeyword()));
            if (CollectionUtils.isEmpty(groupIdsFilterByKeyword)) {
                return groupTreeVOs;
            }
        }
        List<OpLocationPileGroupEntity> list = opLocationPileGroupRepository.findByPidAndMerchantId(merchantId, BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID, null);
        //编辑子群组排查所在根群组
        Long groupId = paramDto.getGroupId();
        Long selfRoot = null;
        int tmpLevel = levelLimit;
        if (groupId != null) {
            OpLocationPileGroupEntity entity = this.opLocationPileGroupRepository.findOne(groupId);
            if (entity != null) {
                if (!CollectionUtils.isEmpty(list)) {
                    List<OpLocationPileGroupEntity> includeList = list.stream().filter(e -> e.getId().equals(groupId)).collect(Collectors.toList());
                    if (CollectionUtils.isEmpty(includeList)) {
                        list.add(entity);
                    }
                }
                if (!BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID.equals(entity.getPid())) {
                    selfRoot = this.opLocationPileGroupRepository.findOrdinaryRoot(groupId);
                    List<OpLocationPileGroupEntity> tmpList = this.opLocationPileGroupRepository.findChildren(selfRoot);
                    OpLocationPileGroupEntity currentEntity = tmpList.stream().filter(e -> e.getId().equals(groupId)).findFirst().orElse(null);
                    if (currentEntity != null) {
                        tmpLevel = tmpLevel - currentEntity.getLevel();
                        tmpLevel = tmpLevel <= 0 ? 0 : tmpLevel;
                    }
                }
                if (paramDto.getUnit() == null) {
                    paramDto.setUnit(entity.getChargingUpUnit());
                }
            }
        }
        for (OpLocationPileGroupEntity op : list) { // list 根节点list
            // 查询
            List<OpLocationPileGroupEntity> childrenList = opLocationPileGroupRepository.findChildren(op.getId());
            int max = childrenList.stream().map(OpLocationPileGroupEntity::getLevel).max(Integer::compareTo).orElse(0);
            log.info("id={}  level = {},levelLimit={}", op.getId(), max,tmpLevel);
            if (Objects.nonNull(paramDto.getGroupId()) && paramDto.getGroupId().equals(op.getId())) {
                // 如果是编辑 找该组 的子组也要显示在 选择列表
                log.info("edit id={}  level = {}", op.getId(), max);
            } else if (max >= tmpLevel) {  // 只查询 1, 2 层 的数据;
                // 只查询 1, 2 层 的数据;
                continue;
            }
            //按条件筛选
            if (paramDto.getUnit() != null && !paramDto.getUnit().equalsIgnoreCase(op.getChargingUpUnit())) {
                continue;
            }
            Set<Long> collect = childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
            if (org.springframework.util.StringUtils.hasText(paramDto.getKeyword()) && Collections.disjoint(groupIdsFilterByKeyword, collect)) {
                continue;
            }
            //过滤ALM接柴油机
            if (op.getLoadType() == 1 && op.getPowerEquipmentEnabled() != null && op.getPowerEquipmentEnabled()) {
                continue;
            }

            PileGroupTreeVOcopy pileGroupTreeVO = new PileGroupTreeVOcopy();
            pileGroupTreeVO.setMerchantId(merchantId);
            pileGroupTreeVO.setGroupId(op.getId());
            pileGroupTreeVO.setGroupName(op.getName());
            pileGroupTreeVO.setPid(op.getPid());
            pileGroupTreeVO.setLocationId(op.getLocationId());
            pileGroupTreeVO.setChargingUpUnit(op.getChargingUpUnit());
            pileGroupTreeVO.setGroupType(op.getGroupType());
            List<OpLocationPileGroupAssociateVO> opLocationPileGroupAssociateVOS = associateMap.get(op.getId());

            List<PileGroupTreeVOcopy> children = new ArrayList<>(findChildren(pileGroupTreeVO, childrenList, associateMap, pileDtoMap, evseDtoMap,locationMap));
            List<OpLocationPileEvseGroupListVO.PileInfoVo> pileInfoVoList = getPileInfoVoList(opLocationPileGroupAssociateVOS, pileDtoMap, evseDtoMap);
            for (OpLocationPileEvseGroupListVO.PileInfoVo pileInfoVo : pileInfoVoList) {
                PileGroupTreeVOcopy pileTypeTreeVO = new PileGroupTreeVOcopy();
                pileTypeTreeVO.setMerchantId(merchantId);
                pileTypeTreeVO.setPileId(pileInfoVo.getPileId());
                pileTypeTreeVO.setPileName(pileInfoVo.getPileName());
                pileTypeTreeVO.setPowerType(pileInfoVo.getPowerType());
                pileTypeTreeVO.setPileSn(pileInfoVo.getPileSn());
                pileTypeTreeVO.setPower(pileInfoVo.getPower());
                Long locationId = pileInfoVo.getLocationId();
                pileTypeTreeVO.setLocationId(locationId);
                OpLocationElasticDTO locationDto = locationMap.get(locationId);
                if (locationDto != null) {
                    pileTypeTreeVO.setLocationName(locationDto.getName());
                }
                pileTypeTreeVO.setEvseInfoList(pileInfoVo.getEvseInfoList());

                children.add(pileTypeTreeVO);
            }
            pileGroupTreeVO.setChildren(children);
            if (Objects.nonNull(paramDto.getGroupId()) && paramDto.getGroupId().equals(op.getId())) {
                // 编辑 把他的下一级组节点加入列表  桩上面已经加了 这里不用再加
                for (PileGroupTreeVOcopy node : children) {
                    if (Objects.nonNull(node.getGroupId()) && node.getGroupId() > 0) {
                        groupTreeVOs.add(0, node);
                    }
                }
            } else {
                groupTreeVOs.add(0, pileGroupTreeVO);
            }
        }
        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 查询商家下加入组的桩 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 如果是编辑页面调用此接口 需要将 传入的组的子组显示到选择列表 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 如果是编辑页面调用此接口 需要将 传入的组的子组显示到选择列表 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        //拿到所有的groupId
        Set<Long> groupIds = groupTreeVOs.stream().map(PileGroupTreeVOcopy::getGroupId).collect(Collectors.toSet());
        List<OpLocationPileGroupEntity> entityList = opLocationPileGroupRepository.findByIds(new ArrayList<>(groupIds));
        int loadType=paramDto.getLoadType();

        //过过滤demand标签群组
        List<Long> fileterGroupIdList=opLocationPileGroupAssociateRepository.findDemandGroupIds(merchantId);
        //如果为EMS类型群组，则过滤掉brandEnum为0的电表数据
        if (!ObjectUtils.isEmpty(loadType)&&loadType==2){
            List<Long> meterIds=entityList.stream().distinct().map(OpLocationPileGroupEntity::getMeterId).collect(Collectors.toList());
            //根据电表ID查询电表信息
            List<LocationMeterEntity> LocationMeters=locationMeterService.getByMeterIds(meterIds);
            //过滤掉brandEnum为0的电表数据，即Enegic品牌电表
            List<Long> enegicMeterIds=LocationMeters.stream().filter(v->v.getBrandEnum()==0).map(LocationMeterEntity::getId).collect(Collectors.toList());
            //拿到Enegic品牌电表对应的groupId
            fileterGroupIdList=entityList.stream().filter(v->enegicMeterIds.contains(v.getMeterId())).map(OpLocationPileGroupEntity::getId).collect(Collectors.toList());
            //过滤掉brandEnum为0的电表数据，即Enegic品牌电表
        }
        //转换为Map，复制属性
        Map<Long, OpLocationPileGroupEntity> groupMap = entityList.stream().collect(Collectors.toMap(OpLocationPileGroupEntity::getId, Function.identity()));
        //遍历pilesInMerchants
        for (PileGroupTreeVOcopy pileGroupTreeVO : groupTreeVOs) {
            if(fileterGroupIdList.contains(pileGroupTreeVO.getGroupId())){
                //过滤掉brandEnum为0的电表数据，即Enegic品牌电表
                continue;
            }
            if (groupMap.containsKey(pileGroupTreeVO.getGroupId())) {
                OpLocationPileGroupEntity groupEntity = groupMap.get(pileGroupTreeVO.getGroupId());
                OpLocationPileGroupDTO OpLocationPileGroupDTO=new OpLocationPileGroupDTO();
                BeanUtils.copyProperties(groupEntity, OpLocationPileGroupDTO);
                pileGroupTreeVO.setOpLocationPileGroupDTO(OpLocationPileGroupDTO);
            }
            result.add(pileGroupTreeVO);
        }
        return result;
    }




    private Boolean checkAlmForAutel(PileEvseGroupParamDTO paramDto) {
        int loadType = paramDto.getLoadType();
        Boolean powerEquipmentEnabled = paramDto.getPowerEquipmentEnabled();
        if (loadType == 1 && powerEquipmentEnabled != null && powerEquipmentEnabled) {
            return true;
        }
        return false;
    }

    private List<PileGroupTreeVOcopy> findChildren(PileGroupTreeVOcopy currentNode, List<OpLocationPileGroupEntity> childrenList,
                                               Map<Long, List<OpLocationPileGroupAssociateVO>> finalAssociateMap,
                                               Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
                                               Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
                                               Map<Long,OpLocationElasticDTO> locationMap) {
        List<PileGroupTreeVOcopy> resultTreeVoList = new ArrayList<>();
        if (CollectionUtils.isEmpty(childrenList)) return resultTreeVoList;
        childrenList.forEach(entity -> {
            Long pid = entity.getPid();
            Long groupId = entity.getId();
            Long parentId = currentNode.getGroupId();
            if (parentId.longValue() == pid.longValue()) {
                PileGroupTreeVOcopy pileGroupTreeVO = new PileGroupTreeVOcopy();
                List<OpLocationPileGroupAssociateVO> opLocationPileGroupAssociateVOS = finalAssociateMap.get(groupId);
                pileGroupTreeVO.setMerchantId(entity.getMerchantId());
                pileGroupTreeVO.setPid(entity.getPid());
                pileGroupTreeVO.setLevel(entity.getLevel());
                pileGroupTreeVO.setGroupId(groupId);
                pileGroupTreeVO.setGroupName(entity.getName());
                pileGroupTreeVO.setLocationId(entity.getLocationId());
                List<PileGroupTreeVOcopy> children = new ArrayList<>(findChildren(pileGroupTreeVO, childrenList, finalAssociateMap, pileDtoMap, evseDtoMap,locationMap));
                List<OpLocationPileEvseGroupListVO.PileInfoVo> pileInfoVoList = getPileInfoVoList(opLocationPileGroupAssociateVOS, pileDtoMap, evseDtoMap);
                for (OpLocationPileEvseGroupListVO.PileInfoVo pileInfoVo : pileInfoVoList) {
                    PileGroupTreeVOcopy pileTypeTreeVO = new PileGroupTreeVOcopy();
                    pileTypeTreeVO.setMerchantId(currentNode.getMerchantId());
                    pileTypeTreeVO.setPileId(pileInfoVo.getPileId());
                    pileTypeTreeVO.setPileName(pileInfoVo.getPileName());
                    pileTypeTreeVO.setPowerType(pileInfoVo.getPowerType());
                    pileTypeTreeVO.setPileSn(pileInfoVo.getPileSn());
                    pileTypeTreeVO.setPower(pileInfoVo.getPower());
                    Long locationId = pileInfoVo.getLocationId();
                    pileTypeTreeVO.setLocationId(locationId);
                    OpLocationElasticDTO locationDto = locationMap.get(locationId);
                    if (locationDto != null) {
                        pileTypeTreeVO.setLocationName(locationDto.getName());
                    }

                    pileTypeTreeVO.setEvseInfoList(pileInfoVo.getEvseInfoList());

                    children.add(pileTypeTreeVO);
                }
                if (CollectionUtils.isEmpty(children)) {
                    return;
                }
                pileGroupTreeVO.setChildren(children);
                resultTreeVoList.add(pileGroupTreeVO);
            }
        });
        return resultTreeVoList;
    }

    /**
     * 查询 没有入场站的
     *
     * @param paramDto
     * @return
     */
    public LocationPileDTO queryPileInLocationV3(PileEvseGroupParamDTO paramDto) {
        log.info("queryPileInLocation,paramDto={}", JSON.toJSONString(paramDto));

        LocationPileDTO locationPileDTO = new LocationPileDTO();
        OpLocationPileEvseGroupListVO resultVo = new OpLocationPileEvseGroupListVO();
        OpLocationPileGroupEntity entity = null;
        Long groupId = paramDto.getGroupId();
        Long locationId = paramDto.getLocationId();
        //编辑
        if (groupId != null) {
            entity = opLocationPileGroupRepository.findOne(groupId);
            if (entity == null) {
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
            }
        }
        //查询场站下的桩
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findByLocationId(locationId, paramDto.getKeyword());
        //过滤第三方桩
        List<OpLocationPileEvseElasticDTO> autelPileDtoList = pileDtoList.stream().filter(dto -> dto.getBrandId() != null && dto.getBrandId().intValue() == BrandEnum.AUTEL.getCode()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(autelPileDtoList)) {
            return locationPileDTO;
        }
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = autelPileDtoList.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity(), (f, s) -> f));
        List<String> pileSnList = autelPileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
        //查询桩是否已经加入群组
        List<OpLocationPileGroupAssociateVO> associateVoList = opLocationPileGroupAssociateRepository.findDetailList(pileSnList);
        Map<Long, List<OpLocationPileGroupAssociateVO>> associateMap = new HashMap<>(); // key 智能充电组ID     value 场站里面已经加入该组的桩
        List<String> groupPileSnList = new ArrayList<>(); // 场站里面已经加入组的桩
        if (!CollectionUtils.isEmpty(associateVoList)) {
            associateMap.putAll(associateVoList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateVO::getGroupId)));
            groupPileSnList.addAll(associateVoList.stream().map(OpLocationPileGroupAssociateVO::getPileSn).collect(Collectors.toList()));
        }
        //查询桩下的枪
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));

        List<OpLocationPileEvseGroupListVO.PileInfoVo> pileEvseVoList = new ArrayList<>();
        //未加入群组的桩
        for (OpLocationPileEvseElasticDTO dto : autelPileDtoList) { // autelPileDtoList 场站里面的道通桩
            String pileSn = dto.getPileSn();
            boolean join = false;
            if (groupPileSnList.contains(pileSn)) {
                if (!CollectionUtils.isEmpty(associateMap) && entity != null) { // 编辑的时候 entity 是有值的
                    List<OpLocationPileGroupAssociateVO> voList = associateMap.get(entity.getId());
                    if (!CollectionUtils.isEmpty(voList)) {
                        List<String> list = voList.stream().map(OpLocationPileGroupAssociateVO::getPileSn).collect(Collectors.toList());
                        join = list.contains(pileSn);
                    }
                }
                if (!join) {
                    continue;
                }
            }
            OpLocationPileEvseGroupListVO.PileInfoVo vo = new OpLocationPileEvseGroupListVO.PileInfoVo();
            BeanUtils.copyProperties(dto, vo);
            vo.setPileId(dto.getId());
            vo.setPileName(dto.getName());
            vo.setJoin(join);
            if (CollectionUtils.isEmpty(evseDtoMap.get(pileSn))) {
                vo.setAmperage(BigDecimal.ZERO);
                vo.setVoltage(BigDecimal.ZERO);
                vo.setPower(BigDecimal.ZERO);
                vo.setPowerType(pileSn.startsWith("A") ? "AC_1_PHASE" : "DC");
            } else {
                vo.setAmperage(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getAmperage()).orElse(BigDecimal.ZERO));
                vo.setVoltage(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getVoltage()).orElse(BigDecimal.ZERO));
                vo.setPower(BigDecimal.valueOf(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getPower()).orElse(0D)));
                vo.setPowerType(evseDtoMap.get(pileSn).get(0).getPowerType());
            }
            if (evseDtoMap.get(pileSn) != null) {
                List<OpLocationPileEvseGroupListVO.EvseInfo> evseInfoList = evseDtoMap.get(pileSn).stream().map(d -> {
                    OpLocationPileEvseGroupListVO.EvseInfo evseInfo = new OpLocationPileEvseGroupListVO.EvseInfo();
                    BeanUtils.copyProperties(d, evseInfo);
                    return evseInfo;
                }).collect(Collectors.toList());
                vo.setEvseInfoList(evseInfoList);
            }
            //按条件筛选
            if (paramDto.getUnit() != null) {
                if (UnitEnum.CURRENT.getCode().equals(paramDto.getUnit()) && pileSn.startsWith("D")) {
                    continue;
                }
                if (UnitEnum.POWER.getCode().equals(paramDto.getUnit()) && pileSn.startsWith("A")) {
                    continue;
                }
            }
            /*if (paramDto.getLoadType() == 1 && pileSn.startsWith("D")) { // ALM只返回AC桩吧
                continue;
            }*/
            pileEvseVoList.add(vo);
        }
        locationPileDTO.getAssociateMap().putAll(associateMap);
        locationPileDTO.getEvseDtoMap().putAll(evseDtoMap);
        locationPileDTO.getPileDtoMap().putAll(pileDtoMap);
        locationPileDTO.getPileInfoVoList().addAll(pileEvseVoList);
        return locationPileDTO;
    }

    @Override
    public Result<OpLocationPileEvseGroupListVO> queryPileInLocation(OpLocationPileEvseGroupParamDTO paramDto) {
        log.info("queryPileInLocation,paramDto={}", JSON.toJSONString(paramDto));
        OpLocationPileEvseGroupListVO resultVo = new OpLocationPileEvseGroupListVO();
        OpLocationPileGroupEntity entity = null;
        Long groupId = paramDto.getGroupId();
        Long locationId = paramDto.getLocationId();

        //编辑
        if (groupId != null) {
            entity = opLocationPileGroupRepository.findOne(groupId);
            if (entity == null) {
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
            }
        }

        OpLocationElasticDTO locationDto = opLocationService.findById(locationId);
        if (locationDto == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        resultVo.setLocationId(locationId);
        resultVo.setLocationName(locationDto.getName());
        resultVo.setPriceFlag(locationDto.getPriceFlag());
        //查询场站下的桩
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findByLocationId(locationId, paramDto.getKeyword());
        //过滤第三方桩
        pileDtoList = pileDtoList.stream().filter(dto -> dto.getBrandId() != null && dto.getBrandId().intValue() == BrandEnum.AUTEL.getCode()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(pileDtoList)) {
            resultVo.setPileInfoVoList(new ArrayList<>());
            return Result.ofSucceed(resultVo);
        }
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = pileDtoList.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        List<String> pileSnList = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
        //查询桩是否已经加入群组
        List<OpLocationPileGroupAssociateVO> associateVoList = opLocationPileGroupAssociateRepository.findDetailList(pileSnList);
        Map<Long, List<OpLocationPileGroupAssociateVO>> associateMap = null;
        List<String> groupPileSnList = null;
        if (!CollectionUtils.isEmpty(associateVoList)) {
            associateMap = associateVoList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateVO::getGroupId));
            groupPileSnList = associateVoList.stream().map(OpLocationPileGroupAssociateVO::getPileSn).collect(Collectors.toList());
        }
        //查询桩下的枪
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));

        List<OpLocationPileEvseGroupListVO.PileInfoVo> pileEvseVoList = new ArrayList<>();
        //未加入群组的桩
        for (OpLocationPileEvseElasticDTO dto : pileDtoList) {
            String pileSn = dto.getPileSn();
            boolean join = false;
            if (!CollectionUtils.isEmpty(groupPileSnList) && groupPileSnList.contains(pileSn)) {
                if (!CollectionUtils.isEmpty(associateMap) && entity != null) {
                    List<OpLocationPileGroupAssociateVO> voList = associateMap.get(entity.getId());
                    if (!CollectionUtils.isEmpty(voList)) {
                        List<String> list = voList.stream().map(OpLocationPileGroupAssociateVO::getPileSn).collect(Collectors.toList());
                        join = list.contains(pileSn);
                    }
                }
                if (!join) {
                    continue;
                }
            }
            OpLocationPileEvseGroupListVO.PileInfoVo vo = new OpLocationPileEvseGroupListVO.PileInfoVo();
            BeanUtils.copyProperties(dto, vo);
            vo.setPileId(dto.getId());
            vo.setPileName(dto.getName());
            vo.setJoin(join);
            vo.setAmperage(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getAmperage()).orElse(BigDecimal.ZERO));
            vo.setVoltage(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getVoltage()).orElse(BigDecimal.ZERO));
            vo.setPower(BigDecimal.valueOf(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getPower()).orElse(0D)));
            vo.setPowerType(evseDtoMap.get(pileSn).get(0).getPowerType());
            if (evseDtoMap.get(pileSn) != null) {
                List<OpLocationPileEvseGroupListVO.EvseInfo> evseInfoList = evseDtoMap.get(pileSn).stream().map(d -> {
                    OpLocationPileEvseGroupListVO.EvseInfo evseInfo = new OpLocationPileEvseGroupListVO.EvseInfo();
                    BeanUtils.copyProperties(d, evseInfo);
                    return evseInfo;
                }).collect(Collectors.toList());
                vo.setEvseInfoList(evseInfoList);
            }
            //按条件筛选
            if (paramDto.getUnit() != null) {
                if (UnitEnum.CURRENT.getCode().equals(paramDto.getUnit()) && pileSn.startsWith("D")) {
                    continue;
                }
                if (UnitEnum.POWER.getCode().equals(paramDto.getUnit()) && pileSn.startsWith("A")) {
                    continue;
                }
            }
            pileEvseVoList.add(vo);
        }
        resultVo.setPileInfoVoList(pileEvseVoList);
        //返回群组树
        List<Long> rootIds = opLocationPileGroupRepository.findGroupId(locationId, BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID, null);
        if (groupId != null) {
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(groupId, false);
            if (!CollectionUtils.isEmpty(children)) {
                rootIds.addAll(children.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toList()));
            }
        }
        if (CollectionUtils.isEmpty(rootIds) || CollectionUtils.isEmpty(associateMap)) {
            return Result.ofSucceed(resultVo);
        }

        List<OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO> pileEvseTreeVoList = new ArrayList<>();
        for (Long rootId : rootIds) {
            List<OpLocationPileGroupEntity> childrenList = opLocationPileGroupRepository.findChildren(rootId);
            OpLocationPileGroupEntity parentEntity = childrenList.get(0);
            if (childrenList.size() > 1) {
                continue;//只返回单层
            }
            List<OpLocationPileGroupAssociateVO> opLocationPileGroupAssociateVOS = associateMap.get(rootId);
            OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO pileEvseTreeVO = new OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO();
            BeanUtils.copyProperties(parentEntity, pileEvseTreeVO);
            pileEvseTreeVO.setGroupId(rootId);
            pileEvseTreeVO.setGroupName(parentEntity.getName());
            if (parentEntity.getPid().longValue() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID) {
                pileEvseTreeVO.setJoin(true);
            }
            pileEvseTreeVO.setPileInfoVoList(this.getPileInfoVoList(opLocationPileGroupAssociateVOS, pileDtoMap, evseDtoMap));
            pileEvseTreeVO.setChildrenList(this.generateChildrenList(rootId, childrenList, associateMap, pileDtoMap, evseDtoMap));
            //按条件筛选
            if (paramDto.getUnit() != null) {
                if (!parentEntity.getChargingUpUnit().equals(paramDto.getUnit())) {
                    continue;
                }
            }
            pileEvseTreeVoList.add(pileEvseTreeVO);
        }
        resultVo.setPileEvseTreeVoList(pileEvseTreeVoList);
        return Result.ofSucceed(resultVo);
    }

    private List<OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO> generateChildrenList(Long parentId,
                                                                                              List<OpLocationPileGroupEntity> childrenList,
                                                                                              Map<Long, List<OpLocationPileGroupAssociateVO>> finalAssociateMap,
                                                                                              Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
                                                                                              Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap) {
        List<OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO> resultTreeVoList = new ArrayList<>();
        if (CollectionUtils.isEmpty(childrenList)) return resultTreeVoList;
        childrenList.stream().forEach(entity -> {
            Long pid = entity.getPid();
            Long groupId = entity.getId();
            if (parentId.longValue() == pid.longValue()) {
                OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO pileEvseTreeVO = new OpLocationPileEvseGroupListVO.OpLocationPileEvseTreeVO();
                List<OpLocationPileGroupAssociateVO> opLocationPileGroupAssociateVOS = finalAssociateMap.get(groupId);
                BeanUtils.copyProperties(entity, pileEvseTreeVO);
                pileEvseTreeVO.setGroupId(groupId);
                pileEvseTreeVO.setGroupName(entity.getName());
                pileEvseTreeVO.setPileInfoVoList(this.getPileInfoVoList(opLocationPileGroupAssociateVOS, pileDtoMap, evseDtoMap));
                pileEvseTreeVO.setChildrenList(this.generateChildrenList(groupId, childrenList, finalAssociateMap, pileDtoMap, evseDtoMap));
                resultTreeVoList.add(pileEvseTreeVO);
            }
        });
        return resultTreeVoList;
    }

    private List<OpLocationPileEvseGroupListVO.PileInfoVo> getPileInfoVoList(List<OpLocationPileGroupAssociateVO> associateList,
                                                                             Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
                                                                             Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap
    ) {
        List<OpLocationPileEvseGroupListVO.PileInfoVo> pileInfoVoList = new ArrayList<>();
        if (CollectionUtils.isEmpty(associateList)) return pileInfoVoList;
        associateList.stream().forEach(associateVO -> {
            String pileSn = associateVO.getPileSn();
            OpLocationPileEvseGroupListVO.PileInfoVo pileInfoVo = new OpLocationPileEvseGroupListVO.PileInfoVo();
            OpLocationPileEvseElasticDTO pileDto = pileDtoMap.get(pileSn);
            BeanUtils.copyProperties(pileDto, pileInfoVo);
            pileInfoVo.setPileId(pileDto.getId());
            pileInfoVo.setPileName(pileDto.getName());
            pileInfoVo.setPower(BigDecimal.valueOf(Optional.ofNullable(evseDtoMap.get(pileSn).get(0).getPower()).orElse(0D)));
            pileInfoVo.setPowerType(evseDtoMap.get(pileSn).get(0).getPowerType());
            pileInfoVo.setLocationId(pileDto.getLocationId());
            pileInfoVo.setEvseInfoList(
                    evseDtoMap.get(pileSn).stream().map(evse -> {
                        OpLocationPileEvseGroupListVO.EvseInfo evseInfo = new OpLocationPileEvseGroupListVO.EvseInfo();
                        BeanUtils.copyProperties(evse, evseInfo);
                        return evseInfo;
                    }).collect(Collectors.toList())
            );
            pileInfoVoList.add(pileInfoVo);
        });
        return pileInfoVoList;
    }


    @Override
    public Result<List<PileGroupDetailV3VOcopy>> queryGroupBySns(List<String> sns) {
        //查询桩所在的群组
        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(sns);
        //取出群组ID
        Set<Long> groupIdList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getGroupId).collect(Collectors.toSet());
        Set<Long> rootIds= new HashSet<>();
        groupIdList.forEach(id -> {
        Long root = opLocationPileGroupRepository.findOrdinaryRoot(id);
            if (root != null) {
                rootIds.add(root);
            }
        });
        List<PileGroupDetailV3VOcopy> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(rootIds)) {
            rootIds.forEach(id -> {
                PileGroupDetailV3VOcopy vo=opLocationPileGroupService.detailV3(id);
                result.add(vo);
            });
        }
        return Result.ofSucceed(result);
    }

    @Override
    public Result<ArithmeticChargingResult>  queryArithmeticCharging(ArithmeticChargingParam req) {
        try {
            ArithmeticChargingParamDTO dto=new ArithmeticChargingParamDTO();
            dto.setParams(req);
            dto.setModuleFun("ChargeSchedule.ScheduleModelRule.ModelMain");
            log.info("queryArithmeticCharging,dto={}", JSON.toJSONString(dto));
            Result<ArithmeticChargingResult> result=chargeScheduleClient.queryArithmeticCharging(dto);
            log.info("queryArithmeticCharging,result={}", JSON.toJSONString(result));
            return result;
        }catch (Exception e){
            log.error("queryArithmeticCharging error",e);
            return Result.ofFailed(LocaleResultUtil.result("queryArithmeticCharging 1 error "));
        }

    }

    @Override
    public Result<Page<PileGroupVOcopy>> queryV3(PileGroupParamDTO pileGroupParamDTO) {
        log.info("queryV3,dto={}", JSON.toJSONString(pileGroupParamDTO));
        Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        List<Integer> filterTypes= opLocationPileGroupService.getLoadTypeWhiteList(userId);
        StopWatch stopWatch = new StopWatch("群组列表搜索");

        String keyword = pileGroupParamDTO.getSearchValue();
        if (this.checkIfKeywordV3(pileGroupParamDTO)) {
            Set<Long> parentIds = new HashSet<>();
            //按群组名称搜索
            stopWatch.start("按群组名称搜索");
            Set<Long> groupIdList = new HashSet<>(opLocationPileGroupRepository.findGroupIdV3(pileGroupParamDTO));
            stopWatch.stop();
            //按桩SN搜索
            stopWatch.start("按桩SN搜索");
            if (StringUtils.isNotBlank(keyword)) {
                groupIdList.addAll(opLocationPileGroupAssociateRepository.findGroupIdByMerchantIdAndPileSn(pileGroupParamDTO.getMerchantId(), keyword));
            }
            if (!CollectionUtils.isEmpty(groupIdList)) {
                groupIdList.forEach(id -> {
                    Long root = opLocationPileGroupRepository.findOrdinaryRoot(id);
                    if (root != null) {
                        parentIds.add(root);
                    }
                });
            }
            stopWatch.stop();
            if (parentIds.isEmpty()) {
                Page<PileGroupVOcopy> page = new Page<>(pileGroupParamDTO.getPage(), pileGroupParamDTO.getPageSize(), 0);
                return Result.ofSucceed(page);
            }
            pileGroupParamDTO.setRootIds(parentIds);
        }
        stopWatch.start("找满足条件的根节点");
        Page<OpLocationPileGroupEntity> rootGroup = opLocationPileGroupRepository.findRootGroup(pileGroupParamDTO);
        stopWatch.stop();
        stopWatch.start("填充信息");
        Page<PileGroupVOcopy> page = new Page<>(rootGroup.getCurrent(), rootGroup.getSize(), rootGroup.getTotal());
        List<PileGroupVOcopy> records = new ArrayList<>();
        String zoneId = null;
        if (!CollectionUtils.isEmpty(rootGroup.getRecords())) {
            Long locationId = rootGroup.getRecords().stream().filter(e -> e.getLocationId() != null).findFirst().map(OpLocationPileGroupEntity::getLocationId).orElse(null);
            if (locationId != null) {
                zoneId = this.getZoneId(locationId);
            }
        }
        if (zoneId == null) {
            zoneId = BaseConstant.CHINA_ZONE_ID;
        }
        String finalZoneId = zoneId;
        for(OpLocationPileGroupEntity item:rootGroup.getRecords()){
            PileGroupVOcopy rootGroupNode = new PileGroupVOcopy();
            records.add(rootGroupNode);
            populateGroup(item, rootGroupNode, finalZoneId);
        }

        List<PileGroupVOcopy> demandGroups=records.stream().filter(e->!ObjectUtils.isEmpty(e.getGroupType())&&e.getGroupType()==1).collect(Collectors.toList());
        records.removeAll(demandGroups);
        List<PileGroupVOcopy> demandChildren=new ArrayList<>();
        demandGroups.forEach(v->{
            List<PileGroupVOcopy> children = v.getChildren();
            children.forEach(c->{
                c.setGroupType(1);
            });
            demandChildren.addAll(children);
        });
        records.addAll(demandChildren);
        records=records.stream().filter(v->ObjectUtils.nullSafeEquals(0,v.getLoadType())||filterTypes.contains(v.getLoadType())).collect(Collectors.toList());
        page.setRecords(records);
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return Result.ofSucceed(page);
    }

    private void populateGroup(OpLocationPileGroupEntity item, PileGroupVOcopy pileGroupVO,String zoneId) {
        pileGroupVO.setEnergyUseStrategy(item.getEnergyUseStrategy());
        pileGroupVO.setId(item.getId());
        pileGroupVO.setPid(item.getPid());
        pileGroupVO.setLoadType(item.getLoadType());
        pileGroupVO.setName(item.getName());
        pileGroupVO.setMinReserve(item.getMinReserve());
        pileGroupVO.setPriority(item.getPriority());
        pileGroupVO.setMerchantId(item.getMerchantId());
        pileGroupVO.setSecurityEnabled(item.getSecurityEnabled());
        pileGroupVO.setStatus(item.getStatus());
        pileGroupVO.setOverloadFuse(item.getOverloadFuse());
        pileGroupVO.setUpDuration(item.getUpDuration());
        pileGroupVO.setAllocationStrategy(item.getAllocationStrategy());
        pileGroupVO.setMeterId(item.getMeterId());
        pileGroupVO.setChargingUp(item.getChargingUp());
        pileGroupVO.setGroupType(item.getGroupType());
        Integer timeSettingMode = item.getTimeSettingMode();
        if (timeSettingMode != null && timeSettingMode == 1 && StringUtils.isNotEmpty(item.getTimeSettingDetail())) {
            List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(item.getTimeSettingDetail(), TimeSettingDetailDTO.class);
            long now = System.currentTimeMillis();
            BigDecimal chargingUp = this.getChargingUp(now, zoneId, null, timeSettingDetailDTOS);
            if (chargingUp != null) {
                pileGroupVO.setChargingUp(chargingUp);
            }
            pileGroupVO.setTimeSettingDetail(timeSettingDetailDTOS.stream().sorted().collect(Collectors.toList()));
        }
        pileGroupVO.setMeterLocation(item.getMeterLocation());
        pileGroupVO.setChargingUpUnit(item.getChargingUpUnit());

        pileGroupVO.setTimeSettingMode(item.getTimeSettingMode());
        String detail = item.getTimeSettingDetail();
        if (org.springframework.util.StringUtils.hasText(detail)) {
            List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
            pileGroupVO.setTimeSettingDetail(timeSettingDetailDTOS.stream().sorted().collect(Collectors.toList()));
        }

        List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(item.getId());
        statMonitorMetric(item, pileGroupVO);
        List<PileGroupVOcopy> subChildren = findSubChildren(pileGroupVO, children,zoneId);
        pileGroupVO.setChildren(subChildren);
        pileGroupVO.setSingleLayerEnabled(CollectionUtils.isEmpty(subChildren));
    }

    private void statMonitorMetric(OpLocationPileGroupEntity groupEntity, PileGroupVOcopy pileGroupVO) {
        Long groupId = pileGroupVO.getId();

        //  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 该组下面的 桩 枪 (不包括子组里面的桩枪数量) ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        // 查找该组(root) 直接关联的桩
        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(groupId);
        // pileSnList  查找该组(root) 直接关联的桩SN
        List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);

        // 统计 桩枪总数量 在线数
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        AtomicInteger onlineGun = new AtomicInteger(0);
        AtomicInteger totalGun = new AtomicInteger(0);
        Collection<String> onlinePileSNs = new HashSet<>();
        List<String> onlinePileEvseSNs = new ArrayList<>();
        List<String> chargingList = new ArrayList<>();
        boolean flag = false;
        for (String pileSn : pileSnList) {
            List<OpLocationEvseElasticDTO> list = evseDtoMap.get(pileSn);
            if (evseDtoMap.containsKey(pileSn) && Objects.nonNull(list)) {
                totalGun.addAndGet(list.size());
                for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : list) {
                    if (!"Default".equalsIgnoreCase(opLocationEvseElasticDTO.getState())) {
                        onlineGun.incrementAndGet();
                        onlinePileSNs.add(pileSn);
                        onlinePileEvseSNs.add(opLocationEvseElasticDTO.getEvseSn());
                    }
                    if (EvseDeviceStatusEnum.CHARGING.getName().equals(opLocationEvseElasticDTO.getState())) {
                        flag = true;
                        chargingList.add(opLocationEvseElasticDTO.getEvseSn());
                    }
                }
            }
        }
        pileGroupVO.setOnlineGun(pileGroupVO.getOnlineGun() + onlineGun.get());
        pileGroupVO.setTotalGun(pileGroupVO.getTotalGun() + totalGun.get());
        log.info("onlinePileSNs: {}", JSON.toJSONString(onlinePileSNs));
        // 判断桩是否在线
        pileGroupVO.setOnlinePile(pileGroupVO.getOnlinePile() + onlinePileSNs.size());
        pileGroupVO.setTotalPile(pileGroupVO.getTotalPile() + pileSnList.size());
        //  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 该组下面的 桩 枪 (不包括子组里面的桩枪数量) ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        //  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 该组下面所有的 充电枪的电流电压功率SOC 监控值   (不包含子组里面的桩枪) ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
//        List<String> evseSnList = evseDtoList.stream().filter(item -> !"Default".equalsIgnoreCase(item.getState())).map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(chargingList);// 里面有当 前SOC
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = new HashMap<>();
        try {
            evseMeterDtoMap.putAll(evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, Functions.identity())));
        } catch (Exception e) {
            // 理论 evseSN 唯一 历史垃圾数据会不唯一那么就取第一个作为map的value
            log.warn("data Error  evseSnList = {} \n evseMeterDtoList={}", JSON.toJSONString(onlinePileSNs), JSON.toJSONString(evseMeterDtoList));
            evseMeterDtoMap.putAll(evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, Functions.identity(), (f, s) -> f)));
        }

        /*List<String> tmpList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        if (flag && !CollectionUtils.isEmpty(tmpList)) {
            String zoneId = this.getZoneId(groupEntity.getLocationId());
            ZonedDateTime zonedDateTime = ZonedDateTime.now(ZoneId.of(zoneId)).withHour(0).withMinute(0).withSecond(0).withNano(0);
            MeterValueStatisticDTO sd = new MeterValueStatisticDTO();
            sd.setBeginDate(zonedDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd 00:00:00")));
            sd.setEndDate(zonedDateTime.plusDays(1L).format(DateTimeFormatter.ofPattern("yyyy-MM-dd 00:00:00")));
            sd.setEvseSnList(tmpList);
            Result<List<MeterValueStatisticVO>> listResult = evseMeterValueFeignClient.statisticByEvseSn(sd);
            if (listResult != null && !CollectionUtils.isEmpty(listResult.getData())) {
                MeterValueStatisticVO vo = listResult.getData().stream().sorted((f, s) -> s.getDateTime().compareTo(f.getDateTime())).collect(Collectors.toList()).get(0);
                pileGroupVO.setTimeAmperage(BigDecimal.valueOf(Optional.ofNullable(vo.getCurrencySum()).orElse(0D)).setScale(2, BigDecimal.ROUND_HALF_UP));
                pileGroupVO.setTimePower(BigDecimal.valueOf(Optional.ofNullable(vo.getPowerSum()).orElse(0D)).divide(BigDecimal.valueOf(1000L), 2, BigDecimal.ROUND_HALF_UP).setScale(2, BigDecimal.ROUND_HALF_UP));
            }
        }*/
        //  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 该组下面所有的 充电枪的电流电压功率SOC 监控值   (不包含子组里面的桩枪)  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        //  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 该组下面所有的 电表  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

        List<Long> meterIds = new ArrayList<>();
        List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(groupId, false);
        if (!CollectionUtils.isEmpty(children)) {
            List<OpLocationPileGroupEntity> entityList = children.stream().filter(e -> e.getMeterId() != null && e.getLoadType() == 1).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(entityList)) {
                meterIds.addAll(entityList.stream().map(OpLocationPileGroupEntity::getMeterId).collect(Collectors.toList()));
            }
        }
        if (groupEntity.getMeterId() != null && groupEntity.getLoadType() == 1) {
            meterIds.add(groupEntity.getMeterId());
        }

        Map<String, MeterDataRecordVO> meterMap = new HashMap<>();
        if (!meterIds.isEmpty()) {
            meterMap.putAll(this.getMeterRecord(meterIds));
        }
        //  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 该组下面所有的 电表  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑


        //  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 该组下面所有的 充电桩和枪 (不包含子组里面的桩枪)  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
//        List<OpLocationPileEvseElasticDTO> list = opLocationPileEvseService.findList(pileSnList);
//        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = new HashMap<>();
//        try {
//            pileDtoMap.putAll(list.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity())));
//        } catch (Exception e) {
//            // 理论 PileSn 唯一 历史垃圾数据会不唯一那么就取第一个作为map的value
//            log.warn("data Error   pileSnList = {} \n data error list={}", JSON.toJSONString(pileSnList), JSON.toJSONString(list));
//            pileDtoMap.putAll(list.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, Functions.identity(), (f, s) -> f)));
//        }

        //  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 该组下面所有的 充电桩和枪 (不包含子组里面的桩枪)  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
        directAssociateGunMetricV3(pileGroupVO, associateList, evseDtoMap, evseMeterDtoMap);

//        List<GroupPileV3DTO> groupPileList = directAssociateGunMetric(associateList, pileDtoMap, evseDtoMap, evseMeterDtoMap);
//        for (GroupPileV3DTO item : groupPileList) {
//            item.getEvseInfoList().forEach(gun -> {
//                pileGroupVO.setTimeAmperage(nullToZero(pileGroupVO.getTimeAmperage()).add(nullToZero(gun.getTimeAmperage())));
//                pileGroupVO.setTimePower(nullToZero(pileGroupVO.getTimePower()).add(nullToZero(gun.getTimePower())));
//            });
//        }

        if (groupEntity.getMeterId() != null && groupEntity.getLoadType() == 1 && !CollectionUtils.isEmpty(meterMap)) {
            if (CollectionUtils.isEmpty(meterMap)) {
                return;
            }
            MeterDataRecordVO recordVO = meterMap.get(groupEntity.getMeterId().toString());
            if (recordVO != null) {
                BigDecimal max = DlbUtil.getAlmValue(recordVO);
                String whiteKey = "energy:pile:base:alm:meter:white:future:" + groupEntity.getMeterId();
                String white = stringRedisTemplate.opsForValue().get(whiteKey);
                if (groupEntity.getMeterLocation() == 1 && StringUtils.isNotBlank(white)) {
                    max = max.add(nullToZero(pileGroupVO.getTimeAmperage()));
                }
                pileGroupVO.setMeterValue(max.setScale(2, BigDecimal.ROUND_HALF_UP));
            }
        }
    }

    private static BigDecimal nullToZero(BigDecimal value) {
        return Optional.ofNullable(value).orElse(BigDecimal.ZERO);
    }

    /**
     * 组直接关联桩(不包含组子组)的每个枪的 电流电压
     *
     * @param pileGroupVO
     * @param associateEntityList
     * @param evseDtoMap
     * @param evseMeterDtoMap
     * @return GroupPileV3DTO 只看 实际电流 和 功率
     */
    private void directAssociateGunMetricV3(
            PileGroupVOcopy pileGroupVO,
            List<OpLocationPileGroupAssociateEntity> associateEntityList,
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
            Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap) {
        if (CollectionUtils.isEmpty(associateEntityList) || CollectionUtils.isEmpty(evseMeterDtoMap)) {
            return;
        }
        associateEntityList.forEach(entity -> {
            String pileSn = entity.getPileSn();
            Assert.notNull(evseDtoMap.get(pileSn), "pileSn = " + pileSn);
            evseDtoMap.get(pileSn).forEach(gun -> {
                if (!CollectionUtils.isEmpty(evseMeterDtoMap)) {
                    String evseSn = gun.getEvseSn();
                    String state = gun.getState();
                    OpEvseMeterUploadDTO meterDto = evseMeterDtoMap.get(evseSn);
                    log.info("meterValueGun {} {} {}", pileSn, JSON.toJSONString(gun), JSON.toJSONString(meterDto));
                    if (meterDto != null && EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(state)) {
                        log.info("read metric pileSn={} groupId={} evseSn= {} state= {} Current = {} power={}", pileSn, entity.getGroupId(), evseSn, state, meterDto.getCurrent(), meterDto.getPower());
                        BigDecimal current = BigDecimal.valueOf(Optional.ofNullable(meterDto.getCurrent()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN);
                        BigDecimal power = BigDecimal.valueOf(Optional.ofNullable(meterDto.getPower()).orElse(0D) / 1000).setScale(2, BigDecimal.ROUND_DOWN);
                        pileGroupVO.setTimeAmperage(nullToZero(pileGroupVO.getTimeAmperage()).add(nullToZero(current)));
                        pileGroupVO.setTimePower(nullToZero(pileGroupVO.getTimePower()).add(nullToZero(power)));
                    }
                }
            });
        });
    }

    /**
     * 组直接关联桩(不包含组子组)的每个枪的 电流电压
     *
     * @param associateEntityList
     * @param pileDtoMap
     * @param evseDtoMap
     * @param evseMeterDtoMap
     * @return GroupPileV3DTO 只看 实际电流 和 功率
     */
    private List<GroupPileV3DTO> directAssociateGunMetric(
            List<OpLocationPileGroupAssociateEntity> associateEntityList,
            Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
            Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap) {
        List<GroupPileV3DTO> pileResultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(associateEntityList) || CollectionUtils.isEmpty(evseMeterDtoMap)) {
            return pileResultList;
        }
        associateEntityList.forEach(entity -> {
            String pileSn = entity.getPileSn();
            OpLocationPileEvseElasticDTO pile = pileDtoMap.get(pileSn);
            GroupPileV3DTO pileDto = GroupPileEvseTypeMapper.INSTANCE.dto2DTO(pile);
            Assert.notNull(evseDtoMap.get(pileSn), "pileSn = " + pileSn);
            pileDto.setEvseInfoList(
                    evseDtoMap.get(pileSn).stream().sorted(Comparator.comparing(OpLocationEvseElasticDTO::getEvseSn)).map(evse -> {
                        GroupPileEvseV3DTO evseDto = GroupPileEvseTypeMapper.INSTANCE.dto2DTO(evse);
                        evseDto.setVoltage(evse.getVoltage());
                        if (!CollectionUtils.isEmpty(evseMeterDtoMap)) {
                            String evseSn = evseDto.getEvseSn();
                            String state = evseDto.getState();
                            OpEvseMeterUploadDTO meterDto = evseMeterDtoMap.get(evseSn);
                            if (meterDto != null && EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(state)) {
                                log.info("read metric groupId={} evseSn= {} state= {} Current = {} power={}", entity.getGroupId(), evseSn, state, meterDto.getCurrent(), meterDto.getPower());
                                evseDto.setTimeAmperage(BigDecimal.valueOf(Optional.ofNullable(meterDto.getCurrent()).orElse(0D)).setScale(2, BigDecimal.ROUND_DOWN));
                                evseDto.setTimePower(BigDecimal.valueOf(Optional.ofNullable(meterDto.getPower()).orElse(0D) / 1000).setScale(2, BigDecimal.ROUND_DOWN));
                            }
                        }
                        return evseDto;
                    }).collect(Collectors.toList())
            );
            pileResultList.add(pileDto);
        });
        return pileResultList;
    }

    private List<PileGroupVOcopy> findSubChildren(PileGroupVOcopy currentNode, List<OpLocationPileGroupEntity> children,String zoneId) {
        List<PileGroupVOcopy> result = new ArrayList<>();
        for (OpLocationPileGroupEntity entity : children) {
            if (entity.getPid().equals(currentNode.getId())) {
                PileGroupVOcopy subNode = new PileGroupVOcopy();
                subNode.setEnergyUseStrategy(entity.getEnergyUseStrategy());
                subNode.setId(entity.getId());
                subNode.setPid(entity.getPid());
                subNode.setLoadType(entity.getLoadType());
                subNode.setName(entity.getName());
                subNode.setMinReserve(entity.getMinReserve());
                subNode.setPriority(entity.getPriority());
                subNode.setMerchantId(entity.getMerchantId());
                subNode.setSecurityEnabled(entity.getSecurityEnabled());
                subNode.setStatus(entity.getStatus());
                subNode.setOverloadFuse(entity.getOverloadFuse());
                subNode.setUpDuration(entity.getUpDuration());
                subNode.setSingleLayerEnabled(CollectionUtils.isEmpty(subNode.getChildren()));
                subNode.setAllocationStrategy(entity.getAllocationStrategy());
                subNode.setMeterId(entity.getMeterId());
                subNode.setChargingUp(entity.getChargingUp());
                Integer timeSettingMode = entity.getTimeSettingMode();
                if (timeSettingMode != null && timeSettingMode == 1 && StringUtils.isNotEmpty(entity.getTimeSettingDetail())) {
                    List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(entity.getTimeSettingDetail(), TimeSettingDetailDTO.class);
                    long now = System.currentTimeMillis();
                    BigDecimal chargingUp = this.getChargingUp(now, zoneId, null, timeSettingDetailDTOS);
                    if (chargingUp != null) {
                        subNode.setChargingUp(chargingUp);
                    }
                    subNode.setTimeSettingDetail(timeSettingDetailDTOS.stream().sorted().collect(Collectors.toList()));
                }
                subNode.setMeterLocation(entity.getMeterLocation());
                subNode.setChargingUpUnit(entity.getChargingUpUnit());

                subNode.setTimeSettingMode(entity.getTimeSettingMode());
                String detail = entity.getTimeSettingDetail();
                if (org.springframework.util.StringUtils.hasText(detail)) {
                    List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
                    subNode.setTimeSettingDetail(timeSettingDetailDTOS.stream().sorted().collect(Collectors.toList()));
                }

                statMonitorMetric(entity, subNode);
                subNode.setChildren(findSubChildren(subNode, children,zoneId));
                currentNode.setTimePower(nullToZero(currentNode.getTimePower()).add(nullToZero(subNode.getTimePower())));
                currentNode.setTimeAmperage(nullToZero(currentNode.getTimeAmperage()).add(nullToZero(subNode.getTimeAmperage())));
                currentNode.setOnlinePile(currentNode.getOnlinePile() + subNode.getOnlinePile());
                currentNode.setTotalPile(currentNode.getTotalPile() + subNode.getTotalPile());
                currentNode.setOnlineGun(currentNode.getOnlineGun() + subNode.getOnlineGun());
                currentNode.setTotalGun(currentNode.getTotalGun() + subNode.getTotalGun());

                result.add(0, subNode);
            }
        }
        return result;
    }

    @Override
    public Result<List<OpLocationPileGroupV2VO>> queryV2(OpLocationPileGroupParamDTO dto) {
        log.info("queryV2,dto={}", JSON.toJSONString(dto));
        StopWatch stopWatch = new StopWatch("群组列表搜索");
        stopWatch.start("查找根群组ID");
        Long locationId = dto.getLocationId();
        String keyword = dto.getSearchValue();
        Long rootId = BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID;
        List<OpLocationPileGroupV2VO> resultList = new ArrayList<>();
        Set<Long> parentIds = new HashSet<>();
        if (this.checkIfKeyword(dto)) {
            //按群组名称搜索
            Set<Long> groupIdList = new HashSet<>();
            groupIdList.addAll(opLocationPileGroupRepository.findGroupId(dto));
            //按桩SN搜索
            if (StringUtils.isNotBlank(keyword)) {
                groupIdList.addAll(opLocationPileGroupAssociateRepository.findGroupId(locationId, keyword));
            }
            if (!CollectionUtils.isEmpty(groupIdList)) {
                groupIdList.forEach(id -> {
                    Long root = opLocationPileGroupRepository.findOrdinaryRoot(id);
                    if (root != null) {
                        parentIds.add(root);
                    }
                });
            }
        } else {
            //查找场站下根群组ID
            parentIds.addAll(opLocationPileGroupRepository.findGroupId(locationId, rootId, null));
        }
        stopWatch.stop();
        log.info("queryV2,parentIds={}", JSON.toJSONString(parentIds));
        if (parentIds.isEmpty()) {
            log.info(stopWatch.prettyPrint());
            return Result.ofSucceed(resultList);
        }
        //生成群组树
        stopWatch.start("生成群组树");
        Map<Long, List<OpLocationPileGroupEntity>> allMap = new HashMap<>();
        Map<Long, OpLocationPileGroupEntity> rootMap = new HashMap<>(parentIds.size());
        //所有群组ID
        Set<Long> allIds = new HashSet<>(parentIds);
        List<Long> meterIds = new ArrayList<>();
        Long two = dto.getPid();
        parentIds.forEach(pid -> {
            List<OpLocationPileGroupEntity> childrenList = opLocationPileGroupRepository.findChildren(pid);
            int size = childrenList.size();
            //单层
            if (two != null) {
                if (two == 0L && size > 1) {
                    return;
                }
                if (two == 1 && size == 1) {
                    return;
                }
            }
            allMap.put(pid, childrenList);
            rootMap.put(pid, childrenList.get(0));
            allIds.addAll(childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toList()));
            meterIds.addAll(childrenList.stream().filter(e -> e.getLoadType() == 1 && e.getMeterId() != null).map(OpLocationPileGroupEntity::getMeterId).collect(Collectors.toList()));
        });

        //查询电表读数
        Map<String, MeterDataRecordVO> meterMap = null;
        if (meterIds.size() > 0) {
            meterMap = this.getMeterRecord(meterIds);
        }

        //所有群组下的桩SN
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(allIds);
        Map<Long, List<OpLocationPileGroupAssociateEntity>> assciateEntityMap = associateEntityList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        //查询桩信息
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = opLocationPileEvseService.findList(pileSnList).stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        //查询枪信息
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = null;
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            evseMeterDtoMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
        }
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        Map<String, ChargingProfileSettingVO> profileMap = null;
        if (!CollectionUtils.isEmpty(profileList)) {
            profileMap = profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<String, OpEvseMeterUploadDTO> finalEvseMeterDtoMap = evseMeterDtoMap;
        Map<String, ChargingProfileSettingVO> finalProfileMap = profileMap;
        Map<String, MeterDataRecordVO> finalMeterMap = meterMap;
        allMap.forEach((k, v) -> {
            OpLocationPileGroupV2VO vo = new OpLocationPileGroupV2VO();
            OpLocationPileGroupEntity parent = rootMap.get(k);
            String zoneId = this.getZoneId(parent.getLocationId());
            BeanUtils.copyProperties(parent, vo);
            this.setTimeSettingDetail(parent, vo);

            Page<OpLocationPileGroupV2VO> childrenPageResult = generateTree(assciateEntityMap, parent.getId(), v, pileDtoMap, evseDtoMap, finalEvseMeterDtoMap, finalProfileMap, finalMeterMap);
            Page<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPilePageResult = getGroupPileDTOS(assciateEntityMap.get(k), pileDtoMap, evseDtoMap, finalEvseMeterDtoMap, finalProfileMap, zoneId);
            vo.setChildrenList(childrenPageResult);
            vo.setGroupPileDTOList(groupPilePageResult);
            if (childrenPageResult == null || CollectionUtils.isEmpty(childrenPageResult.getRecords())) {
                vo.setChildrenList(null);
            }
            if (groupPilePageResult == null || CollectionUtils.isEmpty(groupPilePageResult.getRecords())) {
                vo.setGroupPileDTOList(null);
            }

            vo.setSingleLayerEnabled(true);
            if (vo.getChildrenList() != null && !CollectionUtils.isEmpty(vo.getChildrenList().getRecords())) {
                vo.setSingleLayerEnabled(false);
            }

            this.setGroupTimeValues(vo);
            if (parent.getMeterId() != null && parent.getLoadType() == 1) {
                LocationMeterEntity locationMeterEntity = locationMeterRepository.getById(parent.getMeterId());
                if (locationMeterEntity != null) {
                    vo.setMeterName(locationMeterEntity.getName());
                }
                this.setMeteValue(parent.getMeterId(), finalMeterMap, vo, parent.getMeterLocation());
            }
            resultList.add(vo);
        });
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return Result.ofSucceed(resultList);
    }

    private Map<String, MeterDataRecordVO> getMeterRecord(List<Long> meterIds) {
        Map<String, MeterDataRecordVO> meterMap = new HashMap<>();
        List<String> ids = meterIds.stream().map(Object::toString).collect(Collectors.toList());
        //三分钟以内在线
        Result<List<MeterDataRecordVO>> lastList = meterDataClient.findLastList(ids);
        long now = System.currentTimeMillis();
        if (lastList != null && HttpStatus.OK.value() == lastList.getCode() && !CollectionUtils.isEmpty(lastList.getData())) {
            List<MeterDataRecordVO> data = lastList.getData();
            meterMap.putAll(data.stream().filter(vo -> now - vo.getUpdateTime() < DlbUtil.getMeterOfflineOffset()).collect(Collectors.toMap(MeterDataRecordVO::getExternalId, e -> e, (f, s) -> s)));
        }
        return meterMap;
    }

    private void setGroupTimeValuesV3(PileGroupDetailV3VOcopy vo) {
        BigDecimal timeVoltage = BigDecimal.ZERO;
        BigDecimal timeAmperage = BigDecimal.ZERO;
        BigDecimal timePower = BigDecimal.ZERO;
        List<PileGroupDetailV3VOcopy> childrenList = vo.getChildrenList();
        List<GroupPileV3DTO> groupPileDTOList = vo.getGroupPileDTOList();
        if (!CollectionUtils.isEmpty(childrenList)) {
            for (PileGroupDetailV3VOcopy groupVO : childrenList) {
                timeVoltage = timeVoltage.add(Optional.ofNullable(groupVO.getTimeVoltage()).orElse(BigDecimal.ZERO));
                timeAmperage = timeAmperage.add(Optional.ofNullable(groupVO.getTimeAmperage()).orElse(BigDecimal.ZERO));
                timePower = timePower.add(Optional.ofNullable(groupVO.getTimePower()).orElse(BigDecimal.ZERO));
            }
        }
        if (!CollectionUtils.isEmpty(groupPileDTOList)) {
            for (GroupPileV3DTO groupPileDTO : groupPileDTOList) {
                List<GroupPileEvseV3DTO> evseDTOList = groupPileDTO.getEvseInfoList();
                if (!CollectionUtils.isEmpty(evseDTOList)) {
                    for (GroupPileEvseV3DTO pileEvseDTO : evseDTOList) {
                        timeVoltage = timeVoltage.add(Optional.ofNullable(pileEvseDTO.getTimeVoltage()).orElse(BigDecimal.ZERO));
                        timeAmperage = timeAmperage.add(Optional.ofNullable(pileEvseDTO.getTimeAmperage()).orElse(BigDecimal.ZERO));
                        timePower = timePower.add(Optional.ofNullable(pileEvseDTO.getTimePower()).orElse(BigDecimal.ZERO));
                    }
                }
            }
        }
        vo.setTimeVoltage(timeVoltage);
        vo.setTimeAmperage(timeAmperage);
        vo.setTimePower(timePower);
    }

    private void setGroupTimeValues(OpLocationPileGroupV2VO vo) {
        BigDecimal timeVoltage = BigDecimal.ZERO;
        BigDecimal timeAmperage = BigDecimal.ZERO;
        BigDecimal timePower = BigDecimal.ZERO;
        List<OpLocationPileGroupV2VO> childrenList = null;
        if (vo.getChildrenList() != null) {
            childrenList = vo.getChildrenList().getRecords();
        }


        List<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPileDTOList = null;
        if (vo.getGroupPileDTOList() != null) {
            groupPileDTOList = vo.getGroupPileDTOList().getRecords();
        }


        if (!CollectionUtils.isEmpty(childrenList)) {
            log.info("setGroupTimeValues childrenList :{}", JSON.toJSON(childrenList));
            for (OpLocationPileGroupV2VO groupVO : childrenList) {
                log.info("setGroupTimeValues groupVO :{}", JSON.toJSON(groupVO));
                timeVoltage = timeVoltage.add(Optional.ofNullable(groupVO.getTimeVoltage()).orElse(BigDecimal.ZERO));
                timeAmperage = timeAmperage.add(Optional.ofNullable(groupVO.getTimeAmperage()).orElse(BigDecimal.ZERO));
                timePower = timePower.add(Optional.ofNullable(groupVO.getTimePower()).orElse(BigDecimal.ZERO));
            }
        }
        if (!CollectionUtils.isEmpty(groupPileDTOList)) {
            for (OpLocationPileGroupV2VO.GroupPileV2DTO groupPileDTO : groupPileDTOList) {
                List<OpLocationPileGroupV2VO.GroupPileEvseDTO> evseDTOList = groupPileDTO.getGroupPileEvseDTOList();
                if (!CollectionUtils.isEmpty(evseDTOList)) {
                    for (OpLocationPileGroupV2VO.GroupPileEvseDTO pileEvseDTO : evseDTOList) {
                        timeVoltage = timeVoltage.add(Optional.ofNullable(pileEvseDTO.getTimeVoltage()).orElse(BigDecimal.ZERO));
                        timeAmperage = timeAmperage.add(Optional.ofNullable(pileEvseDTO.getTimeAmperage()).orElse(BigDecimal.ZERO));
                        timePower = timePower.add(Optional.ofNullable(pileEvseDTO.getTimePower()).orElse(BigDecimal.ZERO));
                    }
                }
            }
        }
        vo.setTimeVoltage(timeVoltage);
        vo.setTimeAmperage(timeAmperage);
        vo.setTimePower(timePower);
    }

    private boolean checkIfKeyword(OpLocationPileGroupParamDTO dto) {
        String keyword = dto.getSearchValue();
        Long pid = dto.getPid();
        Integer loadType = dto.getLoadType();
        if (StringUtils.isNotBlank(keyword) || pid != null || loadType != null) {
            return true;
        }
        return false;
    }

    private boolean checkIfKeywordV3(PileGroupParamDTO dto) {
        String keyword = dto.getSearchValue();
        Long pid = dto.getPid();
        Integer loadType = dto.getLoadType();
        if (StringUtils.isNotBlank(keyword) || pid != null || loadType != null || StringUtils.isNotBlank(dto.getGroupName())) {
            return true;
        }
        return false;
    }

    private Page<OpLocationPileGroupV2VO.GroupPileV2DTO> getGroupPileDTOS(
            List<OpLocationPileGroupAssociateEntity> associateEntityList,
            Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
            Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap,
            Map<String, ChargingProfileSettingVO> profileMap,
            String zoneId
    ) {
        Page<OpLocationPileGroupV2VO.GroupPileV2DTO> pageList = new Page<>();
        List<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPileDTOList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(associateEntityList)) {
            associateEntityList.stream().forEach(entity -> {
                String pileSn = entity.getPileSn();
                OpLocationPileEvseElasticDTO elasticDto = pileDtoMap.get(pileSn);
                OpLocationPileGroupV2VO.GroupPileV2DTO pileDto = new OpLocationPileGroupV2VO.GroupPileV2DTO();
                BeanUtils.copyProperties(elasticDto, pileDto);
                List<OpLocationEvseElasticDTO> evseList = evseDtoMap.get(pileSn);
                pileDto.setGroupPileEvseDTOList(getGroupPileEvseDTOS(evseList, evseMeterDtoMap, profileMap, zoneId));
                groupPileDTOList.add(pileDto);
            });
        }
        pageList.setRecords(groupPileDTOList);
        pageList.setTotal(groupPileDTOList.size());
        pageList.setPages(1);
        pageList.setSize(groupPileDTOList.size());
        return pageList;
    }

    private List<OpLocationPileGroupV2VO.GroupPileEvseDTO> getGroupPileEvseDTOS(List<OpLocationEvseElasticDTO> evseList,
                                                                                Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap,
                                                                                Map<String, ChargingProfileSettingVO> profileMap,
                                                                                String zoneId) {
        List<OpLocationPileGroupV2VO.GroupPileEvseDTO> groupPileEvseDTOList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(evseList)) {
            evseList.stream().forEach(evseDto -> {
                OpLocationPileGroupV2VO.GroupPileEvseDTO pileEvseDto = new OpLocationPileGroupV2VO.GroupPileEvseDTO();
                BeanUtils.copyProperties(evseDto, pileEvseDto);
                pileEvseDto.setPause(this.getPauseValue(evseDto.getEvseSn()));
                pileEvseDto.setFast(this.getFastValue(evseDto.getEvseSn()));
                this.setTimeValue(evseMeterDtoMap, pileEvseDto, zoneId);
                this.setEndTime(profileMap, pileEvseDto, zoneId);
                groupPileEvseDTOList.add(pileEvseDto);
            });
        }
        return groupPileEvseDTOList;
    }

    private Page<OpLocationPileGroupV2VO> generateTree(
            Map<Long, List<OpLocationPileGroupAssociateEntity>> assciateEntityMap,
            Long pid,
            List<OpLocationPileGroupEntity> pileGroupEntities,
            Map<String, OpLocationPileEvseElasticDTO> pileDtoMap,
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap,
            Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap,
            Map<String, ChargingProfileSettingVO> profileMap,
            Map<String, MeterDataRecordVO> finalMeterMap
    ) {
        Page<OpLocationPileGroupV2VO> pageList = new Page<>();
        List<OpLocationPileGroupV2VO> childrenList = new ArrayList<>();
        if (CollectionUtils.isEmpty(pileGroupEntities)) {
            return null;
        }
        pileGroupEntities.stream().forEach(entity -> {
            Long parentId = entity.getPid();
            if (parentId != null && pid.longValue() == parentId.longValue()) {
                OpLocationPileGroupV2VO vo = new OpLocationPileGroupV2VO();
                BeanUtils.copyProperties(entity, vo);
                this.setTimeSettingDetail(entity, vo);
                String zoneId = this.getZoneId(entity.getLocationId());
                Page<OpLocationPileGroupV2VO> childrenPageResult = generateTree(assciateEntityMap, entity.getId(), pileGroupEntities, pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, finalMeterMap);
                Page<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPilePageResult = getGroupPileDTOS(assciateEntityMap.get(entity.getId()), pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, zoneId);
                vo.setChildrenList(childrenPageResult);
                vo.setGroupPileDTOList(groupPilePageResult);
                if (childrenPageResult == null || CollectionUtils.isEmpty(childrenPageResult.getRecords())) {
                    vo.setChildrenList(null);
                }
                if (groupPilePageResult == null || CollectionUtils.isEmpty(groupPilePageResult.getRecords())) {
                    vo.setGroupPileDTOList(null);
                }

                vo.setSingleLayerEnabled(true);
                if (vo.getChildrenList() != null && !CollectionUtils.isEmpty(vo.getChildrenList().getRecords())) {
                    vo.setSingleLayerEnabled(false);
                }

                this.setGroupTimeValues(vo);
                if (vo.getLoadType() == 1 && vo.getMeterId() != null) {
                    this.setMeteValue(vo.getMeterId(), finalMeterMap, vo, entity.getMeterLocation());
                }
                childrenList.add(vo);
            }
        });

        pageList.setRecords(childrenList);
        pageList.setTotal(childrenList.size());
        pageList.setPages(1);
        pageList.setSize(childrenList.size());
        return pageList;
    }

    @Override
    public Boolean deliveryByEvseSn(String evseSn, String state) {
        log.info("deliveryByEvseSn,evseSn={},state={}", evseSn, state);
        //标识该枪启动充电
        String key = "energy:pile:base:charging:start:evse_sn:" + evseSn;
        stringRedisTemplate.opsForValue().set(key, state, 5L, TimeUnit.MINUTES);
        //清除缓存
        try {
            this.removeKey(evseSn, state);
            String pileSn = CommonUtil.getPileSn(evseSn);
            OpLocationPileGroupEntity groupEntity = opLocationPileGroupRepository.findName(pileSn);
            if (groupEntity == null || groupEntity.getStatus() != 1) {
                return false;
            }
            Long root = opLocationPileGroupRepository.findOrdinaryRoot(groupEntity.getId());
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            dto.setRootId(root);
            dto.setUpdateType(3);
            OncePerRequestContext context = this.createdContext(dto);
            distributeContext.getStrategy(groupEntity.getAllocationStrategy()).execute(context);
        } finally {
            stringRedisTemplate.delete(key);
        }
        return true;
    }

    @Override
    public Boolean deliveryByGroupId(CommonDeliveryDTO commonDeliveryDTO) {
        log.info("deliveryByGroupId,commonDeliveryDTO={}", JSON.toJSONString(commonDeliveryDTO));
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.findOne(commonDeliveryDTO.getGroupId());
        if (entity == null) {
            return false;
        }
        Long root = opLocationPileGroupRepository.findOrdinaryRoot(entity.getId());
        OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
        dto.setRootId(root);
        dto.setUpdateType(commonDeliveryDTO.getType());
        OncePerRequestContext context = this.createdContext(dto);
        distributeContext.getStrategy(entity.getAllocationStrategy()).execute(context);
        return true;
    }

    @Override
    public Boolean deliveryByUser(DeliveryByUserDTO dto) {
        log.info("deliveryByUser,dto={}", JSON.toJSONString(dto));
        String evseSn = dto.getEvseSn();
        String pileSn = CommonUtil.getPileSn(evseSn);
        OpLocationPileGroupEntity groupEntity = opLocationPileGroupRepository.findName(pileSn);
        //是否存在群组
        if (groupEntity == null || groupEntity.getStatus() != 1) {
            return false;
        }
        Long groupId = groupEntity.getId();
        Long root = opLocationPileGroupRepository.findOrdinaryRoot(groupId);
        OpLocationPileGroupStrategyDTO param = new OpLocationPileGroupStrategyDTO();
        param.setRootId(root);
        param.setUpdateType(13);
        param.setDiy(1);
        OncePerRequestContext context = this.createdContext(param);
        context.getUserDTOMap().put(evseSn, dto);
        distributeContext.getStrategy(groupEntity.getAllocationStrategy()).execute(context);
        return true;
    }

    @Override
    public Long getRootId(Long groupId) {
        return opLocationPileGroupRepository.findOrdinaryRoot(groupId);
    }

    /**
     * @param meterIdList
     * @return
     * @function 获得电表和该电表所关联的充电桩群组之间的映射关系
     */
    @Override
    public Map<Long, List<OpLocationPileGroupEntity>> getMeterIdAndOpLocationPileGroupEntityListMap(List<Long> meterIdList) {

        log.info("OpLocationPileGroupServiceImpl.getMeterIdAndOpLocationPileGroupEntityListMap meterIdList : {}",
                JSON.toJSONString(meterIdList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(meterIdList)) {
            return null;
        }

        List<OpLocationPileGroupEntity> opLocationPileGroupEntityList = opLocationPileGroupRepository.findAllByMeterIdList(meterIdList);

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileGroupEntityList)) {
            return null;
        }
        return opLocationPileGroupEntityList
                .stream()
                .collect(Collectors.groupingBy(OpLocationPileGroupEntity::getMeterId));
    }

    /**
     * @param idList
     * @return
     * @function 根据id批量查询
     */
    @Override
    public List<OpLocationPileGroupEntity> findAllByIdIn(List<Long> idList) {

        log.info("OpLocationPileGroupServiceImpl.findAllByIdIn idList : {}",
                JSON.toJSONString(idList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(idList)) {
            return null;
        }

        return opLocationPileGroupRepository.findByIds(idList);
    }

    private void removeKey(String evseSn, String state) {
        //清除优先充电或暂停充电
        String pauseKey = RedisKeyConstant.getSmartChargePauseKey(evseSn);
        String fastKey = RedisKeyConstant.getSmartChargeFastKey(evseSn);
        String optionalKey = RedisKeyConstant.getSmartChargeOptionalKey(evseSn);
        if (EvseDeviceStatusEnum.FINISHING.getName().equals(state)
                || EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
            if (Optional.ofNullable(stringRedisTemplate.hasKey(pauseKey)).orElse(false)) {
                stringRedisTemplate.delete(pauseKey);
            }
            if (Optional.ofNullable(stringRedisTemplate.hasKey(fastKey)).orElse(false)) {
                stringRedisTemplate.delete(fastKey);
            }
            if (Optional.ofNullable(stringRedisTemplate.hasKey(optionalKey)).orElse(false)) {
                stringRedisTemplate.delete(optionalKey);
            }
        }
        //取消三分钟下发
        String key = RedisKey.getChargeIntervalDeliveryKey(evseSn);
        //清除下发值
        String lastKey = RedisKeyConstant.getChargeLastDeliveryKey(evseSn);
        //清除刷新结束时间
        String refreshKey = RedisKey.getChargeRefreshEndKey(evseSn);
        if (EvseDeviceStatusEnum.FINISHING.getName().equals(state)) {
            if (Optional.ofNullable(stringRedisTemplate.hasKey(key)).orElse(false)) {
                stringRedisTemplate.delete(key);
            }
            if (Optional.ofNullable(stringRedisTemplate.hasKey(lastKey)).orElse(false)) {
                stringRedisTemplate.delete(lastKey);
            }
            if (Optional.ofNullable(stringRedisTemplate.hasKey(refreshKey)).orElse(false)) {
                stringRedisTemplate.delete(refreshKey);
            }
        }
    }

    @Override
    public Result<String> isAssociatePileGroup(String pileSn) {
        log.info("isAssociatePileGroup,pileSn={}", pileSn);
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.findName(pileSn);
        log.info("isAssociatePileGroup,entity={}", JSON.toJSONString(entity));
        String groupName = "";
        if (Objects.nonNull(entity)) {
            groupName = entity.getName();
        }
        return Result.ofSucceed(groupName);
    }

    @Override
    public List<OpLocationPileGroupEntity> findAllRoot(Long rootId) {
        return opLocationPileGroupRepository.findAllRoot(rootId);
    }

    @Override
    public Long updateRoot(OpLocationPileGroupEntity rootEntity) {
        Long root = rootEntity.getId();
        log.info("updateRoot,rootEntity={}", JSON.toJSONString(root));
        opLocationPileGroupRepository.updateById(rootEntity);
        List<OpLocationPileGroupEntity> childrenList = opLocationPileGroupRepository.findChildren(root);
        List<OpLocationPileGroupEntity> updateList = new ArrayList<>();
        childrenList.stream().forEach(entity -> {
            if (entity.getPid().longValue() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID.longValue()) {
                if (entity.getChargingUp().compareTo(rootEntity.getChargingUp()) > 0) {
                    entity.setChargingUp(rootEntity.getChargingUp());
                    entity.setUpdatedAt(System.currentTimeMillis());
                    updateList.add(entity);
                }
            }
        });
        if (!updateList.isEmpty()) {
            opLocationPileGroupRepository.updateBatch(updateList);
        }
        log.info("updateRoot,updateList={}", JSON.toJSONString(updateList));
        return root;
    }

    @Override
    public Result<String> setVip(String evseSn, Long userId) {
        String pileSn = CommonUtil.getPileSn(evseSn);
        String connectorId = CommonUtil.getGunNo(evseSn).toString();

        String chargePileGunRedisKey = String.format(RedisKeyConstant.getChargePileGunKey(pileSn, connectorId));
        TransactionInfoVO vo = new TransactionInfoVO();
        vo.setUserId(userId.toString());
        stringRedisTemplate.opsForValue().set(chargePileGunRedisKey, JSON.toJSONString(vo));

        OpLocationPileEvseElasticDTO pileDto = opLocationPileEvseService.findByPileSn(pileSn);
        Long operatorId = pileDto.getOperatorId();

        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
        stringRedisTemplate.opsForValue().set(snOperatorIdKey, operatorId.toString());
        return Result.ofSucceed(evseSn);
    }

    private String getPhaseNum(String pileSn) {
        Result<ChargePileDTO> chargePileDTOResult = deviceServiceFeign.pileDetail(pileSn);
        log.info("almStart,chargePileDTOResult={}", JSON.toJSONString(chargePileDTOResult));
        if (chargePileDTOResult != null && chargePileDTOResult.getCode() == 200 && chargePileDTOResult.getData() != null) {
            Integer phase = chargePileDTOResult.getData().getPhase();
            return NumberPhasesEnum.getPhaseNum(phase);
        }
        return null;
    }

    @Override
    public Boolean almStart(MeterParamDTO paramDto) {
        log.info("almStart,paramDto={}", JSON.toJSONString(paramDto));
        Long meterId = null;
        try {
            meterId = parseLong(paramDto.getExternalId());
        } catch (NumberFormatException e) {
            log.info("almStart,meterId,exception:", e);
        }
        if (meterId == null) {
            log.info("almStart,meterId is null.");
            return false;
        }
        //家桩
        PowerLoadBalanceVO powerLoadBalanceVO = this.getPowerLoadBalanceVO(meterId);
        if (powerLoadBalanceVO != null) {
            //第一次上报下发默认配置
            if (paramDto.getFirst() || paramDto.getOnline() == null || !paramDto.getOnline()) {
                this.deliveryDefaultValue(paramDto, meterId, powerLoadBalanceVO);
            }
            return this.homePileDelivery(paramDto, meterId, powerLoadBalanceVO);
        }
        //查找电表所属场站
        LocationMeterVO meterVO = locationMeterService.detail(meterId);
        if (meterVO == null) {
            log.info("almStart,meterVO is null.");
            return false;
        }

        //查询关联的群组
        OpLocationPileGroupEntity pileGroupEntity = opLocationPileGroupRepository.findOne(null, meterVO.getId());
        if (pileGroupEntity == null || pileGroupEntity.getLoadType() == 0) {
            log.info("almStart,pileGroupEntity is null.");
            return false;
        }
        //ALM不支持按功率下发
        /*if (UnitEnum.POWER.getCode().equals(pileGroupEntity.getChargingUpUnit())) {
            log.info("almStart,alm is not support power.");
            return false;
        }*/
        Long groupId = pileGroupEntity.getId();
        Long root = opLocationPileGroupRepository.findOrdinaryRoot(groupId);
        //设置限流标识
        String key = "energy:pile:base:alm:delivery:rooId:" + root;
        stringRedisTemplate.opsForValue().set(key, root.toString(), 15L, TimeUnit.SECONDS);
        try {
            OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
            dto.setRootId(root);
            dto.setUpdateType(8);
            distributeContext.getStrategy(pileGroupEntity.getAllocationStrategy()).execute(this.createdContext(dto));
        } finally {
            stringRedisTemplate.delete(key);
        }

        return true;
    }

    /**
     * 查询当前群组及子群组关联的桩
     *
     * @param rootId
     * @return
     */
    private List<String> getAssociatePileSnList(Long rootId) {
        List<String> pileSnList = new ArrayList<>();
        Set<Long> pids = new HashSet<>();
        pids.add(rootId);
        List<OpLocationPileGroupEntity> children = this.opLocationPileGroupRepository.findChildren(rootId, 1);
        if (!CollectionUtils.isEmpty(children)) {
            pids.addAll(children.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toList()));
        }
        List<OpLocationPileGroupAssociateEntity> list = this.opLocationPileGroupAssociateRepository.findList(pids);
        if (!CollectionUtils.isEmpty(list)) {
            pileSnList.addAll(list.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList()));
        }
        return pileSnList;
    }

    private void deliveryDefaultValue(MeterParamDTO paramDto, Long meterId, PowerLoadBalanceVO powerLoadBalanceVO) {
        List<PowerLoadBalanceVO.PilePowerTraffic> dataInfo = powerLoadBalanceVO.getInfo();
        if (!CollectionUtils.isEmpty(dataInfo)) {
            List<OpLocationPileGroupDeliveryVO> deliveryVOList = new ArrayList<>(dataInfo.size());
            dataInfo.stream().forEach(dto -> {
                OpLocationPileGroupDeliveryVO deliveryVO = new OpLocationPileGroupDeliveryVO();
                String pileSn = dto.getSn();
                deliveryVO.setEvseSn(pileSn + "_1");
                deliveryVO.setPileSn(pileSn);
                deliveryVO.setPower(dto.getSpecifiedPower().doubleValue());
                deliveryVO.setState(LocationEVSEStatusEnum.DEFAULT.getName());
                deliveryVO.setGroupId(meterId);
                deliveryVO.setChargingUpType("Current");
                deliveryVO.setChargingUpUnit("A");
                deliveryVO.setPhaseNum(this.getPhaseNum(dto.getSn()));
                deliveryVO.setAssociateId(paramDto.getMonitorid());
                deliveryVO.setUsageScenario(UsageScenarioEnum.HOME_PILE.getCode());
                deliveryVO.setChargingUp(powerLoadBalanceVO.getOfflineCurrent().setScale(1, BigDecimal.ROUND_DOWN));
                deliveryVOList.add(deliveryVO);
            });
            log.info("almStart,计算结果,deliveryDefaultValue,deliveryVOList={}", JSON.toJSONString(deliveryVOList));
            deliveryContext.getStrategy(BaseConstant.IMMEDIATELY_STRATEGY).delivery(null, deliveryVOList);
        }
    }

    private PowerLoadBalanceVO getPowerLoadBalanceVO(Long groupId) {
        GroupQueryDTO queryDto = new GroupQueryDTO();
        queryDto.setGroupId(groupId);
        Result<PowerLoadBalanceVO> balanceVOResult = homePileFeignClient.groupProfile(queryDto);
        log.info("getPowerLoadBalanceVO,balanceVOResult={}", JSON.toJSONString(balanceVOResult));
        if (balanceVOResult != null && balanceVOResult.getCode() == 200 && balanceVOResult.getData() != null) {
            PowerLoadBalanceVO powerLoadBalanceVo = balanceVOResult.getData();
            return powerLoadBalanceVo;
        }
        return null;
    }

    private boolean homePileDelivery(MeterParamDTO paramDto, long meterId, PowerLoadBalanceVO powerLoadBalanceVo) {
        List<PowerLoadBalanceVO.PilePowerTraffic> dataInfo = powerLoadBalanceVo.getInfo();
        if (!CollectionUtils.isEmpty(dataInfo)) {
            List<String> pileSnList = dataInfo.stream().map(PowerLoadBalanceVO.PilePowerTraffic::getSn).collect(Collectors.toList());
            //默认单枪
            List<OpEvseMeterUploadDTO> associateCurrentList = this.getAssociateCurrentListByEvseList(pileSnList.stream().map(d -> d + "_1").collect(Collectors.toList()));
            BigDecimal associateCurrent = this.getAssociateCurrent(associateCurrentList).setScale(1, BigDecimal.ROUND_DOWN);
            BigDecimal calculate = powerLoadBalanceVo.getAvailableCurrent()
                    .subtract(paramDto.getL1().subtract(associateCurrent).max(BigDecimal.ZERO))
                    .multiply(BigDecimal.ONE.subtract(BigDecimal.valueOf(powerLoadBalanceVo.getReserveCurrentRate()).divide(BigDecimal.valueOf(100)))).setScale(1, BigDecimal.ROUND_DOWN);
            BigDecimal minValue = powerLoadBalanceVo.getOfflineCurrent().multiply(BigDecimal.valueOf(pileSnList.size())).setScale(1, BigDecimal.ROUND_DOWN);
            log.info("almStart,homePile,associateCurrent={},calculate={},minValue={}", associateCurrent, calculate, minValue);
            //可用电流小于群组最低
            if (calculate.compareTo(minValue) <= 0) {
                log.info("almStart,homePile,calculate={},pileNumber={}", calculate, pileSnList.size());
            }
            //小于6A值下发0A
            if (calculate.compareTo(BaseConstant.MIN_CURRENT) < 0) {
                calculate = BigDecimal.ZERO;
            }
            //电表离线
            if (paramDto.getL1().compareTo(BigDecimal.valueOf(-1L)) == 0) {
                calculate = minValue;
            }
            List<OpLocationPileGroupDeliveryVO> deliveryVOList = new ArrayList<>(dataInfo.size());
            BigDecimal finalCalculate = calculate;
            List<UploadMeterDTO.UploadPileDTO> info = new ArrayList<>();
            Map<String, OpEvseMeterUploadDTO> associateMap = null;
            if (!CollectionUtils.isEmpty(associateCurrentList)) {
                associateMap = associateCurrentList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
            }
            Map<String, OpEvseMeterUploadDTO> finalAssociateMap = associateMap;
            dataInfo.stream().forEach(dto -> {
                if (!CollectionUtils.isEmpty(finalAssociateMap)) {
                    OpEvseMeterUploadDTO opEvseMeterUploadDTO = finalAssociateMap.get(dto.getSn() + "_1");
                    String state = opEvseMeterUploadDTO.getEvseStatusName();
                    if (opEvseMeterUploadDTO != null) {
                        BigDecimal timeValue = BigDecimal.valueOf(Optional.ofNullable(opEvseMeterUploadDTO.getCurrent()).orElse(0D));
                        OpLocationPileGroupDeliveryVO deliveryVO = new OpLocationPileGroupDeliveryVO();
                        deliveryVO.setEvseSn(dto.getSn() + "_1");
                        deliveryVO.setPileSn(dto.getSn());
                        deliveryVO.setPower(dto.getSpecifiedPower().doubleValue());
                        deliveryVO.setState(state);
                        deliveryVO.setGroupId(meterId);
                        deliveryVO.setChargingUpType("Current");
                        deliveryVO.setChargingUpUnit("A");
                        deliveryVO.setUsageScenario(UsageScenarioEnum.HOME_PILE.getCode());
                        deliveryVO.setOfflineCurrent(powerLoadBalanceVo.getOfflineCurrent());
                        deliveryVO.setPhaseNum(this.getPhaseNum(dto.getSn()));
                        deliveryVO.setAssociateId(paramDto.getMonitorid());
                        BigDecimal current = finalCalculate.min(dto.getSpecifiedCurrent().setScale(1, BigDecimal.ROUND_DOWN));
                        deliveryVO.setChargingUp(current);
                        deliveryVO.setZoneId(this.getZoneIdForHomePile(dto.getSn()));
                        if ((EvseDeviceStatusEnum.CHARGING.getName().equals(state))
                                || (EvseDeviceStatusEnum.SUSPENDED_EVSE.getName().equals(state))
                                || (EvseDeviceStatusEnum.SUSPENDED_EV.getName().equals(state))) {
                            deliveryVOList.add(deliveryVO);
                        }
                        UploadMeterDTO.UploadPileDTO uploadPileDTO = new UploadMeterDTO.UploadPileDTO();
                        uploadPileDTO.setSn(dto.getSn());
                        uploadPileDTO.setAllocatedCurrent(current);
                        uploadPileDTO.setCurrent(timeValue);
                        uploadPileDTO.setPileStatus(LocationEVSEStatusEnum.DEFAULT.getName().equals(state) ? 1 : 0);
                        uploadPileDTO.setOcppStatus(state);
                        info.add(uploadPileDTO);
                    }
                }
            });
            log.info("almStart,计算结果,homePile,deliveryVOList={}", JSON.toJSONString(deliveryVOList));
            if (paramDto.getDelivery() && !deliveryVOList.isEmpty()) {
                deliveryContext.getStrategy(BaseConstant.IMMEDIATELY_STRATEGY).delivery(null, deliveryVOList);
            }
            if (!CollectionUtils.isEmpty(info)) {
                UploadMeterDTO uploadMeterDTO = new UploadMeterDTO();
                uploadMeterDTO.setGroupId(meterId);
                uploadMeterDTO.setMeterStatus(paramDto.getOnline() ? 1 : -1);
                uploadMeterDTO.setMeterCurrent(paramDto.getL1());
                uploadMeterDTO.setInfo(info);
                homePileFeignClient.updateTraffic(uploadMeterDTO);
                stringRedisTemplate.opsForValue().set(RedisKeyConstant.getStringSmartChargingHomePilePush(String.valueOf(meterId)), JSON.toJSONString(uploadMeterDTO), 10L, TimeUnit.MINUTES);
            }
            return true;
        }
        return false;
    }

    private String getZoneIdForHomePile(String pileSn) {
        Result<UserChargePileVO> userIdBySN = homePileFeignClient.getUserIdBySN(pileSn);
        log.info("almStart,userIdBySN={}", JSON.toJSONString(userIdBySN));
        if (userIdBySN != null && userIdBySN.getCode() == 200 && userIdBySN.getData() != null) {
            UserChargePileVO data = userIdBySN.getData();
            String userId = data.getUserId();
            if (StringUtils.isNotBlank(userId)) {
                String key = "ENERGY:USER:TIMEZONE:" + userId;
                String redisValue = stringRedisTemplate.opsForValue().get(key);
                log.info("almStart,redisValue={}", redisValue);
                if (StringUtils.isNotBlank(redisValue)) {
                    String zoneId = JSONObject.parseObject(redisValue).getString("zoneId");
                    return zoneId;
                }
            }
        }
        return "Asia/Shanghai";
    }

    private BigDecimal getAssociateCurrent(List<OpEvseMeterUploadDTO> associateCurrentList) {
        if (!CollectionUtils.isEmpty(associateCurrentList)) {
            Double sum = associateCurrentList.stream().filter(d -> d.getCurrent() != null && EvseDeviceStatusEnum.CHARGING.getName().equals(d.getEvseStatusName())).map(OpEvseMeterUploadDTO::getCurrent).reduce(0D, (f, s) -> f + s);
            return BigDecimal.valueOf(sum);
        }
        return BigDecimal.ZERO;
    }

    private List<OpEvseMeterUploadDTO> getAssociateCurrentList(List<String> pileSnList) {
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        List<OpLocationEvseElasticDTO> evseResultList = evseDtoList.stream().filter(d -> LocationEVSEStatusEnum.CHARGING.getName().equalsIgnoreCase(d.getState())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(evseResultList)) {
            return this.getAssociateCurrentListByEvseList(evseResultList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList()));
        }
        return null;
    }

    private List<OpEvseMeterUploadDTO> getAssociateCurrentListByEvseList(List<String> evseList) {
        Result<List<OpEvseMeterUploadDTO>> listResult = monitorFeignClient.queryNewMeters(evseList);
        log.info("getAssociateCurrent,listResult={}", JSON.toJSONString(listResult));
        if (listResult != null && listResult.getCode() == 200 && !CollectionUtils.isEmpty(listResult.getData())) {
            List<OpEvseMeterUploadDTO> data = listResult.getData();
            return data;
        }
        return null;
    }

    @Override
    public OpLocationPileGroupEntity getOne(Long id) {
        return opLocationPileGroupRepository.findOne(id);
    }

    @Override
    public Long status(Long groupId, Integer status) {
        log.info("status,groupId={},status={}", groupId, status);
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.findOne(groupId);
        if (entity != null) {
            OpLocationPileGroupEntity updateEntity = new OpLocationPileGroupEntity();
            long now = System.currentTimeMillis();
            updateEntity.setId(groupId);
            updateEntity.setUpdatedAt(now);
            updateEntity.setStatus(status);
            updateEntity.setMinReserve(entity.getMinReserve());
            if(Objects.equals(entity.getGroupType(),2) && status==0){
                log.info("禁用群组，去掉边缘云标记");
                updateEntity.setGroupType(0);
            }
            boolean update = opLocationPileGroupRepository.updateById(updateEntity);
            Long rootId = groupId;
            if (entity.getPid() != BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID) {
                rootId = opLocationPileGroupRepository.findOrdinaryRoot(groupId);
            }
            //子群组继承父群组状态
            List<OpLocationPileGroupEntity> list = opLocationPileGroupRepository.findChildren(groupId);
            if (!CollectionUtils.isEmpty(list)) {
                list.forEach(e -> {
                    e.setStatus(status);
                    e.setUpdatedAt(now);
                });
                opLocationPileGroupRepository.updateBatch(list);
            }
            if((Objects.equals(entity.getGroupType(),2)||Objects.equals(entity.getGroupType(),3))
                    && status==0){
                log.info("edge/ems群组禁用,通知边缘云,status:{}",status);
                //边缘云 群组删除，通知边缘服务
                List<String> rootGid = Arrays.asList(rootId + "");
                notifyEdgeSmartCharge(0, rootGid,rootGid, rootId, "_ktqTsQx1XEKL");
            }
            //从根群组查找所有群组
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(rootId);
            List<OpLocationPileGroupEntity> deleteList = children.stream().filter(e -> e.getStatus() == 0).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(deleteList)) {
                Set<Long> ids = deleteList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
                List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(ids);
                if (!CollectionUtils.isEmpty(associateEntityList)) {
                    ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                        //删除配置
                        OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
                        OncePerRequestContext context = this.createdContext(dto);
                        context.setDeleteList(associateEntityList);
                        distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
                    }), 3L, TimeUnit.SECONDS);
                }
            }
            List<OpLocationPileGroupEntity> deliveryList = children.stream().filter(e -> e.getStatus() == 1).collect(Collectors.toList());
            Long finalRootId = rootId;
            ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                if (!CollectionUtils.isEmpty(deliveryList)) {
                    OpLocationPileGroupStrategyDTO param = new OpLocationPileGroupStrategyDTO();
                    param.setRootId(finalRootId);
                    param.setUpdateType(6);
                    OncePerRequestContext context = this.createdContext(param);
                    distributeContext.getStrategy(children.get(0).getAllocationStrategy()).execute(context);
                }
                this.sendMQ(finalRootId, 3);
            }), 3L, TimeUnit.SECONDS);
            if (update) {
                return groupId;
            }
        }
        return null;
    }

    @Override
    public Long clearDelivery(String pileSn) {
        log.info("clearDelivery,pileSn={}", pileSn);
        OpLocationPileGroupAssociateEntity entity = opLocationPileGroupAssociateRepository.findOne(pileSn);
        this.deleteByEntity(entity);
        return entity.getId();
    }

    @Override
    public List<OpLocationPileGroupHistoryDetailVO> historyDetail(OpLocationPileGroupHistoryParamDTO dto) {
        log.info("historyDetail,dto={}", JSON.toJSONString(dto));
        List<OpLocationPileGroupHistoryDetailVO> detailList = new ArrayList<>();
        Long groupId = dto.getGroupId();
        Long startTime = dto.getStartTime();
        Long endTime = dto.getEndTime();
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.findOne(groupId);
        if (entity == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        List<OpLocationPileGroupEntity> childrenList = this.opLocationPileGroupRepository.findChildren(groupId);
        Set<Long> groupIds = childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(groupIds);
        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(pileSnList)) {
            log.error("historyDetail,pileSnList is null.");
            return detailList;
        }
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        if (CollectionUtils.isEmpty(evseDtoList)) {
            log.error("historyDetail,evseDtoList is null.");
            return detailList;
        }

        HistoryParamDTO paramDTO = new HistoryParamDTO();
        paramDTO.setGroupIds(new ArrayList<>(groupIds));
        paramDTO.setPileSnList(pileSnList);
        long now = System.currentTimeMillis();
        long lastTime = now;
        if (startTime == null) {
            startTime = now - TimeUnit.HOURS.toMillis(24L);
            endTime = now;
        } else {
            endTime = startTime + TimeUnit.HOURS.toMillis(24L);
        }
        if (now - startTime.longValue() > TimeUnit.HOURS.toMillis(24L)) {
            lastTime = endTime;
        }
        paramDTO.setBeginTime(startTime);
        paramDTO.setEndTime(endTime);
        //缓存标识查询最新数据源
        String key = "energy:pile:base:historyDetail:flag";
        if (stringRedisTemplate.hasKey(key)) {
            List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
            String zoneId = this.getZoneId(entity.getLocationId());
            MeterValueStatisticDTO sd = new MeterValueStatisticDTO();
            sd.setBeginDate(LocalDateTime.ofInstant(Instant.ofEpochMilli(startTime), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
            sd.setEndDate(LocalDateTime.ofInstant(Instant.ofEpochMilli(endTime), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));
            sd.setEvseSnList(evseSnList);
            Result<List<MeterValueStatisticVO>> listResult = evseMeterValueFeignClient.statisticByEvseSn(sd);
            log.info("historyDetail,from monitor listResult={}", JSON.toJSONString(listResult));
            if (listResult == null || listResult.getCode() != HttpStatus.OK.value() || listResult.getData() == null) {
                return detailList;
            }
            List<MeterValueStatisticVO> list = listResult.getData();
            //取第一把枪功率电流
            Map<String, OpLocationEvseElasticDTO> tmp = evseDtoList.stream().collect(Collectors.toMap(OpLocationEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
            BigDecimal amperage = tmp.values().stream().filter(vo -> vo.getAmperage() != null).map(OpLocationEvseElasticDTO::getAmperage).reduce((f, s) -> f.add(s)).orElse(BigDecimal.ZERO);
            BigDecimal power = BigDecimal.valueOf(tmp.values().stream().filter(vo -> vo.getPower() != null).map(OpLocationEvseElasticDTO::getPower).reduce((f, s) -> f + s).orElse(0D));
            List<OpLocationPileGroupHistoryDetailVO> tempList = new ArrayList<>();
            list.stream().forEach(vo -> {
                OpLocationPileGroupHistoryDetailVO detailVO = new OpLocationPileGroupHistoryDetailVO();
                detailVO.setAmperage(amperage);
                detailVO.setPower(power);
                detailVO.setCreateTime(valueOf(vo.getDateTime()));
                //detailVO.setCreateTime(LocalDateTime.parse(vo.getDateTime(), DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli());
                detailVO.setTimeAmperage(BigDecimal.valueOf(Optional.ofNullable(vo.getCurrencySum()).orElse(0D)).setScale(2, BigDecimal.ROUND_HALF_UP));
                detailVO.setTimePower(BigDecimal.valueOf(Optional.ofNullable(vo.getPowerSum()).orElse(0D)).divide(BigDecimal.valueOf(1000L), 2, BigDecimal.ROUND_HALF_UP).setScale(2, BigDecimal.ROUND_HALF_UP));
                tempList.add(detailVO);
            });
            //排序
            List<OpLocationPileGroupHistoryDetailVO> sortList = tempList.stream().sorted((f, s) -> (int) (f.getCreateTime() - s.getCreateTime())).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(sortList)) {
                //补零
                List<OpLocationPileGroupHistoryDetailVO> newList = new ArrayList<>(sortList.size() * 2);
                OpLocationPileGroupHistoryDetailVO lastVo = sortList.get(0);
                OpLocationPileGroupHistoryDetailVO endVo = sortList.get(sortList.size() - 1);
                //开始前补零
                OpLocationPileGroupHistoryDetailVO firstVo = lastVo;
                Long t = TimeUnit.MINUTES.toMillis(2);
                boolean isCharging = evseDtoList.stream().filter(vo -> EvseDeviceStatusEnum.CHARGING.getName().equals(vo.getState())).findFirst().orElse(null) != null ? true : false;
                Long tmpEnd = firstVo.getCreateTime();
                for (Long i = startTime; i < tmpEnd; ) {
                    OpLocationPileGroupHistoryDetailVO zeroVo = new OpLocationPileGroupHistoryDetailVO();
                    zeroVo.setAmperage(amperage);
                    zeroVo.setPower(power);
                    zeroVo.setCreateTime(i);
                    zeroVo.setTimePower(BigDecimal.ZERO);
                    zeroVo.setTimeAmperage(BigDecimal.ZERO);
                    newList.add(zeroVo);
                    i += t;
                }
                for (OpLocationPileGroupHistoryDetailVO vo : sortList) {
                    if (vo.getCreateTime() - lastVo.getCreateTime() > t && lastVo.getTimeAmperage().compareTo(BigDecimal.ZERO) == 0 && lastVo.getTimePower().compareTo(BigDecimal.ZERO) == 0) {
                        OpLocationPileGroupHistoryDetailVO addVo = new OpLocationPileGroupHistoryDetailVO();
                        addVo.setAmperage(vo.getAmperage());
                        addVo.setPower(vo.getPower());
                        addVo.setCreateTime(vo.getCreateTime() - TimeUnit.SECONDS.toMillis(1));
                        addVo.setTimeAmperage(lastVo.getTimeAmperage());
                        addVo.setTimePower(lastVo.getTimePower());
                        newList.add(addVo);
                    }
                    newList.add(vo);
                    lastVo = vo;
                }
                //结束后补零
                if (!isCharging) {
                    Long tmpFirst = endVo.getCreateTime() + t;
                    for (Long i = tmpFirst; i < endTime; ) {
                        OpLocationPileGroupHistoryDetailVO zeroVo = new OpLocationPileGroupHistoryDetailVO();
                        zeroVo.setAmperage(amperage);
                        zeroVo.setPower(power);
                        zeroVo.setCreateTime(i);
                        zeroVo.setTimePower(BigDecimal.ZERO);
                        zeroVo.setTimeAmperage(BigDecimal.ZERO);
                        newList.add(zeroVo);
                        i += t;
                    }
                }
                return newList;
            }
            return sortList;
        }
        List<OpChargingSettingVO> historyList = this.getHistoryDelivery(paramDTO);

        if (CollectionUtils.isEmpty(historyList)) {
            return detailList;
        }
        //取第一把枪功率电流
        Map<String, OpLocationEvseElasticDTO> tmp = evseDtoList.stream().collect(Collectors.toMap(OpLocationEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        BigDecimal amperage = tmp.values().stream().filter(vo -> vo.getAmperage() != null).map(OpLocationEvseElasticDTO::getAmperage).reduce((f, s) -> f.add(s)).orElse(BigDecimal.ZERO);
        BigDecimal power = BigDecimal.valueOf(tmp.values().stream().filter(vo -> vo.getPower() != null).map(OpLocationEvseElasticDTO::getPower).reduce((f, s) -> f + s).orElse(0D));
        String zoneId = this.getZoneId(entity.getLocationId());
        List<OpLocationPileGroupHistoryDetailVO> tempList = new ArrayList<>();
        List<OpLocationPileGroupHistoryDetailVO> list = new ArrayList<>();
        //订单号
        Set<String> orderIds = new HashSet<>();
        historyList.stream().forEach(vo -> {
            OpLocationPileGroupHistoryDetailVO detailVO = this.toHistoryVo(vo, zoneId, orderIds);
            if (detailVO != null) {
                tempList.add(detailVO);
            }
        });
        if (CollectionUtils.isEmpty(tempList)) {
            return detailList;
        }
        List<EnergyBillDetailVO> startTimes = null;
        if (orderIds.size() > 0) {
            //查询订单开始和结束时间
            startTimes = billFeignClient.getStartTimes(new ArrayList<>(orderIds));
            log.info("historyDetail,startTimes={}", JSON.toJSONString(startTimes));
        }
        //聚合
        tempList.stream().collect(Collectors.groupingBy(OpLocationPileGroupHistoryDetailVO::getCreateTime)).forEach((k, v) -> {
            OpLocationPileGroupHistoryDetailVO detailVO = new OpLocationPileGroupHistoryDetailVO();
            detailVO.setCreateTime(k);
            v.stream().reduce(detailVO, (f, s) -> {
                //下发值
                f.setSettingAmperage(f.getSettingAmperage().add(s.getSettingAmperage().setScale(2, BigDecimal.ROUND_HALF_UP)));
                f.setSettingPower(f.getSettingPower().add(s.getSettingPower().divide(BigDecimal.valueOf(1000L), 2, BigDecimal.ROUND_HALF_UP)));
                //上报值
                f.setTimeAmperage(f.getTimeAmperage().add(s.getTimeAmperage().setScale(2, BigDecimal.ROUND_HALF_UP)));
                f.setTimePower(f.getTimePower().add(s.getTimePower().divide(BigDecimal.valueOf(1000L), 2, BigDecimal.ROUND_HALF_UP)));
                //额定值
                f.setPower(power);
                f.setAmperage(amperage);
                return f;
            });
            list.add(detailVO);
        });
        //排序
        List<OpLocationPileGroupHistoryDetailVO> sortList = list.stream().sorted((f, s) -> (int) (f.getCreateTime() - s.getCreateTime())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(sortList)) {
            OpLocationPileGroupHistoryDetailVO firstVo = sortList.get(0);
            OpLocationPileGroupHistoryDetailVO lastVo = sortList.get(sortList.size() - 1);
            //补回第一个
            if (!CollectionUtils.isEmpty(startTimes)) {
                EnergyBillDetailVO firstDetailVo = startTimes.stream().filter(vo -> vo.getStartDateTime() != null).sorted(Comparator.comparing(EnergyBillDetailVO::getStartDateTime)).findFirst().orElse(null);
                if (firstDetailVo != null) {
                    OpLocationPileGroupHistoryDetailVO first = new OpLocationPileGroupHistoryDetailVO();
                    BeanUtils.copyProperties(firstVo, first);
                    first.setCreateTime(Math.max(firstDetailVo.getStartDateTime(), paramDTO.getBeginTime()));
                    detailList.add(first);
                }
            }
            detailList.addAll(sortList);
            //补回最后一个
            if (!CollectionUtils.isEmpty(startTimes)) {
                EnergyBillDetailVO nullVo = startTimes.stream().filter(vo -> vo.getStopDateTime() == null).findFirst().orElse(null);
                EnergyBillDetailVO lastDetailVo = startTimes.stream().filter(vo -> vo.getStopDateTime() != null).sorted(Comparator.comparing(EnergyBillDetailVO::getStopDateTime).reversed()).findFirst().orElse(null);
                if (nullVo != null || lastDetailVo != null) {
                    OpLocationPileGroupHistoryDetailVO last = new OpLocationPileGroupHistoryDetailVO();
                    BeanUtils.copyProperties(lastVo, last);
                    last.setCreateTime(lastTime);
                    if (lastDetailVo != null) {
                        last.setCreateTime(Math.min(lastTime, lastDetailVo.getStopDateTime()));
                    }
                    detailList.add(last);
                }
            }
        }
        return detailList;
    }

    @Override
    public String getZoneId(Long locationId) {
        return opLocationService.getZoneId(locationId);
    }

    private OpLocationPileGroupHistoryDetailVO toHistoryVo(OpChargingSettingVO vo, String zoneId, Set<String> orderIds) {
        String profileValue = vo.getProfileValue();
        String timeValue = vo.getTimeValue();
        OpLocationPileGroupHistoryDetailVO detailVO = new OpLocationPileGroupHistoryDetailVO();
        detailVO.setCreateTime(vo.getCreatedAt());
        if (StringUtils.isNotBlank(profileValue)) {
            SetChargingProfile setChargingProfiles = this.getSetChargingProfiles(profileValue);
            if (setChargingProfiles != null) {
                ChargingSchedule chargingSchedule = setChargingProfiles.getCsChargingProfiles().getChargingSchedule();
                Integer transactionId = setChargingProfiles.getCsChargingProfiles().getTransactionId();
                if (transactionId != null) {
                    orderIds.add(String.valueOf(transactionId));
                }
                ChargingSchedulePeriod period = this.getChargingSchedulePeriod(vo, zoneId, chargingSchedule);
                String chargingRateUnit = chargingSchedule.getChargingRateUnit();
                if ("A".equalsIgnoreCase(chargingRateUnit)) {
                    detailVO.setSettingAmperage(period.getLimit());
                } else {
                    detailVO.setSettingPower(period.getLimit());
                }
            }
        }
        if (StringUtils.isNotBlank(timeValue)) {
            OpEvseMeterUploadDTO opEvseMeterUploadDTO = JSON.parseObject(timeValue, OpEvseMeterUploadDTO.class);
            detailVO.setTimeAmperage(new BigDecimal(Optional.ofNullable(opEvseMeterUploadDTO.getCurrent()).orElse(0D)));
            detailVO.setTimePower(new BigDecimal(Optional.ofNullable(opEvseMeterUploadDTO.getPower()).orElse(0D)));
            if (EvseDeviceStatusEnum.CHARGING.getName().equals(opEvseMeterUploadDTO.getEvseStatusName())) {
                return detailVO;
            }
        }
        return null;
    }

    private ChargingSchedulePeriod getChargingSchedulePeriod(OpChargingSettingVO vo, String zoneId, ChargingSchedule chargingSchedule) {
        List<ChargingSchedulePeriod> chargingSchedulePeriod = chargingSchedule.getChargingSchedulePeriod();
        List<ChargingSchedulePeriod> schedulePeriods = chargingSchedulePeriod.stream().sorted((f, s) -> (int) f.getStartPeriod().compareTo(s.getStartPeriod())).collect(Collectors.toList());
        Long createdAt = vo.getCreatedAt();
        Integer second = this.getSecond(createdAt, zoneId);
        ChargingSchedulePeriod period = schedulePeriods.get(0);
        for (ChargingSchedulePeriod schedulePeriod : schedulePeriods) {
            if (second.compareTo(schedulePeriod.getStartPeriod()) >= 0 && second.compareTo(schedulePeriod.getStartPeriod()) <= 0) {
                period = schedulePeriod;
            }
        }
        return period;
    }

    private GetCompositeSchedule getGetCompositeSchedule(String msg) {
        SendMsgDto sendMsgDto = JSON.parseObject(msg, SendMsgDto.class);
        String sendMsg = sendMsgDto.getMsg();
        JSONArray array = JSON.parseArray(sendMsg);
        GetCompositeSchedule getCompositeSchedule = array.getObject(2, GetCompositeSchedule.class);
        return getCompositeSchedule;
    }

    private Integer getSecond(Long createdAt, String zoneId) {
        Instant instant = Instant.ofEpochMilli(createdAt);
        LocalDateTime localDateTime = LocalDateTime.ofInstant(instant, ZoneId.of(zoneId));
        LocalDateTime dateTime = localDateTime.atZone(ZoneId.of(zoneId)).withZoneSameInstant(ZoneId.of(BaseConstant.GMT)).toLocalDateTime();
        return BigDecimal.valueOf(dateTime.getLong(ChronoField.SECOND_OF_DAY)).intValue();
    }

    private SetChargingProfile getSetChargingProfiles(String msg) {
        SendMsgDto sendMsgDto = JSON.parseObject(msg, SendMsgDto.class);
        String sendMsg = sendMsgDto.getMsg();
        JSONArray array = JSON.parseArray(sendMsg);
        SetChargingProfile setChargingProfile = array.getObject(3, SetChargingProfile.class);
        return setChargingProfile;
    }

    private List<OpChargingSettingVO> getHistoryDelivery(HistoryParamDTO paramDTO) {
        Result<List<OpChargingSettingVO>> listResult = smartChargeFeign.getHistory(paramDTO);
        log.info("getHistoryDelivery,listResult={}", JSON.toJSONString(listResult));
        if (listResult != null && listResult.getCode() == 200 && listResult.getData() != null) {
            return listResult.getData();
        }
        return null;
    }

    @Override
    public Long clear(String pileSn) {
        //删除配置
        OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
        List<OpLocationPileGroupAssociateEntity> list = this.opLocationPileGroupAssociateRepository.findList(Collections.singletonList(pileSn));
        OncePerRequestContext context = this.createdContext(dto);
        context.setDeleteList(list);
        distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
        return 0L;
    }

    @Override
    public boolean checkStatus(Long locationId, Long priceId) {
        log.info("checkStatus,locationId={},priceId={}", locationId, priceId);
        List<OpLocationPileGroupEntity> list = opLocationPileGroupRepository.findList(locationId, priceId);
        if (!CollectionUtils.isEmpty(list)) {
            return true;
        }
        return false;
    }

    @Override
    public Integer syncStatus(Integer status, Long groupId) {
        log.info("syncStatus,status={},groupId={}", status, groupId);
        List<OpLocationPileGroupEntity> updateList = new ArrayList<>();
        if (groupId == null) {
            List<OpLocationPileGroupEntity> entityList = opLocationPileGroupRepository.findAll();
            updateList.addAll(entityList);
        } else {
            updateList.add(opLocationPileGroupRepository.findOne(groupId));
        }
        if (status == null) {
            status = 1;
        }
        long now = System.currentTimeMillis();
        Integer finalStatus = status;
        updateList.stream().forEach(entity -> {
            entity.setStatus(finalStatus);
            entity.setUpdatedAt(now);
        });
        opLocationPileGroupRepository.updateBatch(updateList);
        return updateList.size();
    }

    @Override
    public Boolean checkAlm(Long userId) {
        log.info("checkAlm,userId={}", userId);
        Boolean result = redisTemplate.opsForHash().hasKey(RedisKeyConstant.getHashPileBaseAlmUserInfo(), userId.toString());
        log.info("checkAlm,result={}", result);
        return result;
    }

    @Override
    public Long setAlm(Long userId) {
        log.info("setAlm,userId={}", userId);
        redisTemplate.opsForHash().put(RedisKeyConstant.getHashPileBaseAlmUserInfo(), userId.toString(), userId.toString());
        //默认有效期90天
        redisTemplate.expire(RedisKeyConstant.getHashPileBaseAlmUserInfo(), 3650L, TimeUnit.DAYS);
        return userId;
    }

    @Override
    public List<OpLocationPileGroupEntity> findByLocationId(Long locationId) {
        return opLocationPileGroupRepository.findByLocationId(locationId);
    }

    @Override
    public Boolean homePileUpdate(Long groupId, Integer flag) {
        log.info("homePileUpdate,groupId={},flag={}", groupId, flag);
        //更新
        if (flag == 2) {
            MeterDataRecordVO meterDataRecordVO = this.getMeterDataRecordVO(groupId);
            PowerLoadBalanceVO powerLoadBalanceVO = this.getPowerLoadBalanceVO(groupId);
            if (meterDataRecordVO != null && powerLoadBalanceVO != null) {
                MeterParamDTO paramDTO = new MeterParamDTO();
                BeanUtils.copyProperties(meterDataRecordVO.getMeters().get(0), paramDTO);
                //下发离线配置
                this.deliveryDefaultValue(paramDTO, groupId, powerLoadBalanceVO);
                //下发计算配置
                paramDTO.setDelivery(true);
                return this.homePileDelivery(paramDTO, groupId, powerLoadBalanceVO);
            }
            return false;
        }
        //删除
        if (flag == 3) {
            PowerLoadBalanceVO powerLoadBalanceVO = this.getPowerLoadBalanceVO(groupId);
            if (powerLoadBalanceVO != null) {
                List<PowerLoadBalanceVO.PilePowerTraffic> info = powerLoadBalanceVO.getInfo();
                List<String> pileSnList = info.stream().map(PowerLoadBalanceVO.PilePowerTraffic::getSn).collect(Collectors.toList());
                List<OpLocationPileGroupDeliveryVO> deleteList = new ArrayList<>();
                pileSnList.stream().forEach(pileSn -> {
                    OpLocationPileGroupDeliveryVO vo = new OpLocationPileGroupDeliveryVO();
                    vo.setPileSn(pileSn);
                    vo.setEvseSn(pileSn + "_1");
                    vo.setAssociateId(groupId);
                    vo.setGroupId(groupId);
                    deleteList.add(vo);
                    //删除缓存
                    stringRedisTemplate.delete(RedisKey.getChargeIntervalDeliveryKey(pileSn + "_1"));
                });
                log.info("homePileUpdate,计算结果,deleteList={}", JSON.toJSONString(deleteList));
                DeliveryParamDTO paramDTO = new DeliveryParamDTO();
                paramDTO.setDeliveryVoList(deleteList);
                Result<Boolean> booleanResult = smartChargeFeign.clearChargeProfile(paramDTO);
                log.info("homePileUpdate,booleanResult={}", booleanResult);
                return true;
            }
            return false;
        }
        return false;
    }

    @Override
    public Boolean checkCost(Long userId) {
        log.info("checkCost,userId={}", userId);
        Boolean result = redisTemplate.opsForHash().hasKey(RedisKeyConstant.getHashPileBaseCostUserInfo(), userId.toString());
        log.info("checkCost,result={}", result);
        return result;
    }

    @Override
    public Long setCost(Long userId) {
        log.info("setCost,userId={}", userId);
        redisTemplate.opsForHash().put(RedisKeyConstant.getHashPileBaseCostUserInfo(), userId.toString(), userId.toString());
        //默认有效期90天
        redisTemplate.expire(RedisKeyConstant.getHashPileBaseCostUserInfo(), 3650L, TimeUnit.DAYS);
        return userId;
    }

    @Override
    public List<EstimatedChargeLineVO> estimatedChargeLine(String evseSn) {
        List<EstimatedChargeLineVO> estimatedChargeLineList = new ArrayList<>();

        String orderSeq = "";
        try {
            EnergyBillVO energyBillVO = billFeignClient.findLastBillByEvse(evseSn).getData();
            if (energyBillVO != null && OrderStatusEnum.START_SUCCESS.getValue().equals(energyBillVO.getOrderStatus())) {
                orderSeq = energyBillVO.getOrderSeq();
            }
        } catch (Exception e) {
            log.error("estimatedChargeLine查询evseSn :{} 最近的订单异常", evseSn, e);
        }

        log.info("estimatedChargeLine evseSn:{} 充电中订单orderSeq :{}", evseSn, orderSeq);
        if (StringUtils.isBlank(orderSeq)) {
            return new ArrayList<>();
        }

        List<OpEvseMeterUploadDTO> evseMeterList = null;
        try {
            OpEvseMeterUploadDTO opEvseMeterUploadDTO = new OpEvseMeterUploadDTO();
            opEvseMeterUploadDTO.setBusId(orderSeq);
            evseMeterList = monitorFeignClient.selectMeterByBusId(opEvseMeterUploadDTO).getData();
        } catch (Exception e) {
            log.error("estimatedChargeLine 查询evseSn :{} 最近的订单:{}实时数据异常", evseSn, orderSeq, e);
        }
        Long now = System.currentTimeMillis();
        try {
            ChargingProfileSettingVO chargingProfileSettingVO = smartChargeFeign.findLastProfile(evseSn).getData();
            if (chargingProfileSettingVO == null) {
                evseMeterList.forEach(temp -> {
                    EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                    chargeData.setTimeAmperage(BigDecimal.valueOf(temp.getCurrent()).divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP));
                    chargeData.setCreateTime(temp.getCreateTime());
                    chargeData.setEstimateEnabled(false);
                    estimatedChargeLineList.add(chargeData);
                });
                return estimatedChargeLineList;
            }

            if ("A".equalsIgnoreCase(chargingProfileSettingVO.getChargingUpUnit())) {
                evseMeterList.forEach(temp -> {
                    EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                    chargeData.setTimeAmperage(BigDecimal.valueOf(temp.getCurrent()).divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP));
                    chargeData.setCreateTime(temp.getCreateTime());
                    chargeData.setEstimateEnabled(false);
                    estimatedChargeLineList.add(chargeData);
                });
            } else {
                evseMeterList.forEach(temp -> {
                    EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                    chargeData.setTimeAmperage(new BigDecimal(Optional.ofNullable(temp.getPower()).orElse(0D) / 1000).setScale(2, BigDecimal.ROUND_DOWN));
                    chargeData.setCreateTime(temp.getCreateTime());
                    chargeData.setEstimateEnabled(false);
                    estimatedChargeLineList.add(chargeData);
                });
            }
            OpLocationPileGroupEntity opLocationPileGroupEntity = this.getOne(chargingProfileSettingVO.getGroupId());
            int energyUseStrategy = opLocationPileGroupEntity.getEnergyUseStrategy();
            if (energyUseStrategy == 1) {
                String planTime = chargingProfileSettingVO.getPlanTimeList();
                if (StringUtils.isNotBlank(planTime)) {
                    OpEvseDTO opEvseDTO = opLocationEvseService.getEvseInfoByEvseSn(evseSn).getData();
                    Long endTime = now + 3600000;
                    ZoneId zoneId = ZoneId.of(opEvseDTO.getZoneId());
                    LocalDateTime localDateTime = LocalDateTime.now(ZoneId.of(opEvseDTO.getZoneId()));
                    LocalDate localDate = localDateTime.toLocalDate();

                    List<PlanTimeVO> planTimeVOList = JSONObject.parseArray(planTime, PlanTimeVO.class);
                    Collections.reverse(planTimeVOList);
                    log.info("planTimeVOList :{}", JSON.toJSONString(planTimeVOList));

                    List<EstimatedChargeLineVO> futureChargeLineVOList = new ArrayList<>();
                    for (PlanTimeVO pt : planTimeVOList) {
                        LocalTime localPlanTime = LocalTime.parse(pt.getBeginTime());
                        LocalDateTime localPlanDateTime = LocalDateTime.of(localDate, localPlanTime);
                        long beginTime = localPlanDateTime.atZone(zoneId).toInstant().toEpochMilli();
                        if (beginTime > now) {
                            if (beginTime < endTime) {
                                EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                                if ("A".equalsIgnoreCase(chargingProfileSettingVO.getChargingUpUnit())) {
                                    chargeData.setTimeAmperage(pt.getChargingUp().divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP));
                                } else {
                                    chargeData.setTimeAmperage(pt.getChargingUp().divide(BigDecimal.valueOf(1000), 2, BigDecimal.ROUND_DOWN));
                                }

                                chargeData.setCreateTime(beginTime);
                                chargeData.setEstimateEnabled(true);
                                futureChargeLineVOList.add(chargeData);
                            }
                        } else {
                            EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                            if ("A".equalsIgnoreCase(chargingProfileSettingVO.getChargingUpUnit())) {
                                chargeData.setTimeAmperage(pt.getChargingUp().divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP));
                            } else {
                                chargeData.setTimeAmperage(pt.getChargingUp().divide(BigDecimal.valueOf(1000), 2, BigDecimal.ROUND_DOWN));
                            }
                            chargeData.setCreateTime(now + 60000);
                            chargeData.setEstimateEnabled(true);
                            estimatedChargeLineList.add(chargeData);
                            break;
                        }
                    }

                    if (!CollectionUtils.isEmpty(futureChargeLineVOList)) {
                        futureChargeLineVOList.sort(Comparator.comparing(EstimatedChargeLineVO::getCreateTime));
                    }

                    EstimatedChargeLineVO maxEstimatedChargeLineVO = estimatedChargeLineList.get(estimatedChargeLineList.size() - 1);
                    long maxPlanTime = maxEstimatedChargeLineVO.getCreateTime() + 60000;
                    EstimatedChargeLineVO futureEstimatedChargeLine = maxEstimatedChargeLineVO;
                    for (long i = maxPlanTime; i < endTime; ) {
                        EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                        futureEstimatedChargeLine = getFutureEstimatedChargeLine(i, futureChargeLineVOList, futureEstimatedChargeLine);
                        if ("A".equalsIgnoreCase(chargingProfileSettingVO.getChargingUpUnit())) {
                            chargeData.setTimeAmperage(futureEstimatedChargeLine.getTimeAmperage().divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP));
                        } else {
                            chargeData.setTimeAmperage(futureEstimatedChargeLine.getTimeAmperage().divide(BigDecimal.valueOf(1), 2, BigDecimal.ROUND_DOWN));
                        }

                        chargeData.setCreateTime(i);
                        chargeData.setEstimateEnabled(true);
                        estimatedChargeLineList.add(chargeData);

                        i += 60000;
                    }
                }
            } else {
                BigDecimal chargingUp = chargingProfileSettingVO.getChargingUp();
                if (chargingUp != null) {
                    for (int i = 1; i <= 60; i++) {
                        EstimatedChargeLineVO chargeData = new EstimatedChargeLineVO();
                        chargeData.setTimeAmperage(chargingUp.divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP));
                        chargeData.setCreateTime(now + 60000 * i);
                        chargeData.setEstimateEnabled(true);
                        estimatedChargeLineList.add(chargeData);
                    }
                }
            }
        } catch (Exception e) {
            log.error("estimatedChargeLine 获取evseSn ：{}配置数据异常", evseSn, e);
        }
        return estimatedChargeLineList;
    }

    @Override
    public Boolean fastCharging(PileGroupFastDTO dto) {
        log.info("fastCharging,dto={}", JSON.toJSONString(dto));
        String evseSn = dto.getEvseSn();
        Long sellerId = dto.getSellerId();
        Long userId = dto.getUserId();
        String lockKey = new StringBuilder(sellerId.toString()).append(":").append(userId.toString()).toString();
        Boolean lock = this.stringRedisTemplate.opsForValue().setIfAbsent(lockKey, "click", 10L, TimeUnit.SECONDS);
        if (lock == null || !lock) {
            log.info("fastCharging,get lock fail.");
            throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
        }
        try {
            if (StringUtils.isEmpty(evseSn)) {
                return false;
            }
            String pileSn = CommonUtil.getPileSn(evseSn);
            OpLocationPileGroupEntity groupEntity = opLocationPileGroupRepository.findName(pileSn);
            //是否存在群组
            if (groupEntity == null || groupEntity.getStatus() != 1) {
                return false;
            }
            Integer fastValue = this.getFastValue(evseSn);
            Integer pauseValue = this.getPauseValue(evseSn);
            Integer click = dto.getClick();
            String key = RedisKeyConstant.getSmartChargeOptionalKey(evseSn);
            boolean flag = false;
            //点击暂停按钮
            if (click == 0) {
                Integer pause = dto.getPause();
                //停止暂停
                if (pause == 0) {
                    if (pauseValue == 1) {
                        //不设置缓存
                        stringRedisTemplate.opsForValue().set(key, "0", 30L, TimeUnit.DAYS);
                        flag = true;
                    }
                } else {
                    //开始暂停
                    if (pauseValue == 0) {
                        //设置缓存，暂停
                        stringRedisTemplate.opsForValue().set(key, "1", 30L, TimeUnit.DAYS);
                        flag = true;
                    }
                    this.setFastValue(evseSn, 0);
                }
                this.setPauseValue(evseSn, pause);
            } else {
                //点击优先按钮
                Integer fast = dto.getFast();
                //停止优先
                if (fast == 0) {
                    if (fastValue == 1) {
                        //不设置缓存
                        stringRedisTemplate.opsForValue().set(key, "0", 30L, TimeUnit.DAYS);
                        flag = true;
                    }
                } else {
                    //开始优先
                    if (fastValue == 0) {
                        //设置缓存，优先
                        stringRedisTemplate.opsForValue().set(key, "2", 30L, TimeUnit.DAYS);
                        flag = true;
                    }
                    this.setPauseValue(evseSn, 0);
                }
                this.setFastValue(evseSn, fast);
            }
            Long groupId = groupEntity.getId();
            if (flag) {
                Long root = opLocationPileGroupRepository.findOrdinaryRoot(groupId);
                OpLocationPileGroupStrategyDTO param = new OpLocationPileGroupStrategyDTO();
                param.setRootId(root);
                param.setUpdateType(9);
                distributeContext.getStrategy(groupEntity.getAllocationStrategy()).execute(this.createdContext(param));
            }
        } finally {
            this.stringRedisTemplate.delete(lockKey);
        }
        return true;
    }

    @Override
    public PileGroupChargeLineVO chargeLine(String evseSn, String transactionId) {
        log.info("chargeLine,evseSn={}", evseSn);
        PileGroupChargeLineVO resultVo = new PileGroupChargeLineVO();
        //订单号是否存在
        if (transactionId == null) {
            String sn = CommonUtil.getPileSn(evseSn);
            Integer gunNo = CommonUtil.getGunNo(evseSn);

            String chargePileGunRedisKey = String.format(CHARGE_PILE_GUN_KEY, sn, gunNo);
            String chargePileGunRedisValue = stringRedisTemplate.opsForValue().get(chargePileGunRedisKey);
            TransactionInfoVo transactionInfoVo = JSON.parseObject(chargePileGunRedisValue, TransactionInfoVo.class);
            if(transactionInfoVo != null){
                transactionId = transactionInfoVo.getTransactionId();
            }
//            Result<EnergyBillVO> billVOResult = billFeignClient.findLastBillByEvse(evseSn);
//            if (billVOResult != null && billVOResult.getCode() == HttpStatus.OK.value() && billVOResult.getData() != null) {
//                EnergyBillVO billVO = billVOResult.getData();
//                if (billVO.getOrderStatus() != null && billVO.getOrderStatus() > 0) {
//                    transactionId = billVO.getOrderSeq();
//                }
//            }
        }
        if (StringUtils.isEmpty(transactionId)) {
            return resultVo;
        }
        //查询桩所属群组
        OpLocationPileGroupEntity groupEntity = opLocationPileGroupRepository.findName(CommonUtil.getPileSn(evseSn));
        if (groupEntity == null || groupEntity.getStatus() != 1) {
            return resultVo;
        }
        resultVo.setChargingUpUnit(groupEntity.getChargingUpUnit());
        resultVo.setEnergyUseStrategy(groupEntity.getEnergyUseStrategy());
        //按订单号查询充电曲线
        OpEvseMeterUploadDTO uploadDTO = new OpEvseMeterUploadDTO();
        uploadDTO.setBusId(transactionId);
        Result<List<OpEvseMeterUploadDTO>> meterByBusId = monitorFeignClient.selectMeterByBusId(uploadDTO);
        log.info("chargeLine,meterByBusId={}", JSON.toJSONString(meterByBusId));
        if (meterByBusId != null && meterByBusId.getCode() == HttpStatus.OK.value() && !CollectionUtils.isEmpty(meterByBusId.getData())) {
            List<OpEvseMeterUploadDTO> data = meterByBusId.getData();
            data.stream().forEach(opEvseMeterUploadDTO -> {
                BigDecimal tmp = BigDecimal.valueOf(Optional.ofNullable(opEvseMeterUploadDTO.getCurrent()).orElse(0D)).setScale(2, BigDecimal.ROUND_HALF_UP);
                if (UnitEnum.POWER.getCode().equals(resultVo.getChargingUpUnit())) {
                    tmp = BigDecimal.valueOf(Optional.ofNullable(opEvseMeterUploadDTO.getPower()).orElse(0D)).divide(BigDecimal.valueOf(1000L), 2, RoundingMode.HALF_UP);
                }
                resultVo.getDetailList().add(PileGroupChargeLineVO.ChargeLineDetail.builder()
                        .actualValue(tmp)
                        .createTime(opEvseMeterUploadDTO.getCreateTime())
                        .build());
            });
        }
        if (CollectionUtils.isEmpty(resultVo.getDetailList())) {
            return resultVo;
        }
        //查询最大电流或功率
        OpEvseInfoDTO infoDTO = opLocationEvseService.getEvseByEvseSn(evseSn).getData();
        if (infoDTO == null) {
            return resultVo;
        }
        BigDecimal maxVal = infoDTO.getAmperage();
        if (UnitEnum.POWER.getCode().equals(resultVo.getChargingUpUnit())) {
            maxVal = BigDecimal.valueOf(Optional.ofNullable(infoDTO.getPower()).orElse(0D));
        }
        //按订单号查询下发值
        List<ProfileParamDTO> paramDto = new ArrayList<>();
        ProfileParamDTO dto = new ProfileParamDTO();
        dto.setEvseSn(evseSn);
        dto.setTransactionId(transactionId);
        paramDto.add(dto);
        Result<List<ChargingGraphVO>> listResult = smartChargeFeign.graphList(paramDto);
        if (listResult != null && listResult.getCode() == HttpStatus.OK.value() && !CollectionUtils.isEmpty(listResult.getData())) {
            List<PlanTimeListVO> planTimeList = listResult.getData().get(0).getProfileList().stream().sorted(Comparator.comparing(PlanTimeListVO::getCreateTime).reversed()).collect(Collectors.toList());
            //Long startTime = this.mergeSettingVo(resultData, maxVal, resultVo.getChargingUpUnit(), planTimeList);
            log.info("chargeLine,planTimeList={}", JSON.toJSONString(planTimeList));
            List<PileGroupChargeLineVO.ChargeLineDetail> actualList = resultVo.getDetailList().stream().sorted((f, s) -> (int) (f.getCreateTime().compareTo(s.getCreateTime()))).collect(Collectors.toList());
            List<PileGroupChargeLineVO.ChargeLineDetail> addList = new ArrayList<>();
            for (PileGroupChargeLineVO.ChargeLineDetail detail : actualList) {
                Long createTime = detail.getCreateTime();
                for (PlanTimeListVO vo : planTimeList) {
                    if (createTime.longValue() >= vo.getCreateTime().longValue()) {
                        detail.setUseValue(vo.getUse());
                        detail.setUnUseValue(maxVal);
                        detail.setSource(vo.getSource());
                        if (createTime.longValue() > vo.getCreateTime().longValue()) {
                            detail.setSource(null);
                            addList.add(PileGroupChargeLineVO.ChargeLineDetail.builder()
                                    .unUseValue(maxVal)
                                    .source(vo.getSource())
                                    .createTime(vo.getCreateTime())
                                    .useValue(vo.getUse())
                                    .actualValue(detail.getActualValue())
                                    .build());
                        }
                        break;
                    }
                }
            }
            if (addList.size() > 0) {
                addList = addList.stream().collect(Collectors.toMap(PileGroupChargeLineVO.ChargeLineDetail::getCreateTime, e -> e, (f, s) -> f)).values().stream().collect(Collectors.toList());
                log.info("chargeLine,addList={}", JSON.toJSONString(addList));
                resultVo.getDetailList().addAll(addList);
            }
            Long begin = actualList.get(actualList.size() - 1).getCreateTime();
            PlanTimeListVO now = null;
            PlanTimeListVO last = null;
            List<PileGroupChargeLineVO.ChargeLineDetail> futureList = new ArrayList<>();
            for (PlanTimeListVO vo : planTimeList) {
                if (last == null) {
                    last = vo;
                }
                if (begin.longValue() <= vo.getCreateTime().longValue()) {
                    futureList.add(PileGroupChargeLineVO.ChargeLineDetail.builder()
                            .useValue(vo.getUse())
                            .createTime(vo.getCreateTime())
                            .unUseValue(maxVal)
                            .build());
                }
                if (begin.longValue() > vo.getCreateTime().longValue() && begin.longValue() <= last.getCreateTime().longValue()) {
                    now = vo;
                }
                last = vo;
            }
            PileGroupChargeLineVO.ChargeLineDetail detail = actualList.get(actualList.size() - 1);
            PileGroupChargeLineVO.ChargeLineDetail first = detail;
            if (now != null) {
                //detail.setSource(now.getSource());
                detail.setUnUseValue(now.getUnUse());
                detail.setUseValue(now.getUse());
            }
            if (futureList.size() > 0) {
                futureList = futureList.stream().sorted((f, s) -> (int) (f.getCreateTime() - s.getCreateTime())).collect(Collectors.toList());
                List<PileGroupChargeLineVO.ChargeLineDetail> tmpList = new ArrayList<>(futureList);
                for (PileGroupChargeLineVO.ChargeLineDetail d : futureList) {
                    if (first != null) {
                        PileGroupChargeLineVO.ChargeLineDetail other = new PileGroupChargeLineVO.ChargeLineDetail();
                        BeanUtils.copyProperties(first, other);
                        other.setActualValue(null);
                        other.setSource(null);
                        other.setCreateTime(d.getCreateTime() - 1000);
                        tmpList.add(other);
                    }
                    first = d;
                }
                resultVo.getDetailList().addAll(tmpList);
            }
        }
        if (!CollectionUtils.isEmpty(resultVo.getDetailList())) {
            //日期格式化
            Long locationId = infoDTO.getLocationId();
            String zoneId = this.getZoneId(locationId);
            resultVo.getDetailList().stream().forEach(vo -> {
                Long createTime = vo.getCreateTime();
                String format = LocalDateTime.ofInstant(Instant.ofEpochMilli(createTime), ZoneId.of(zoneId)).format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
                vo.setFormatTime(format);
            });
            //去重
            List<PileGroupChargeLineVO.ChargeLineDetail> tmpList = resultVo.getDetailList();
            Map<Long, List<PileGroupChargeLineVO.ChargeLineDetail>> map = tmpList.stream().collect(Collectors.groupingBy(PileGroupChargeLineVO.ChargeLineDetail::getCreateTime));
            List<PileGroupChargeLineVO.ChargeLineDetail> newList = new ArrayList<>(map.size());
            map.forEach((k, v) -> {
                PileGroupChargeLineVO.ChargeLineDetail d = v.get(0);
                v.stream().forEach(vo -> {
                    if (vo.getUseValue() != null) {
                        d.setUseValue(vo.getUseValue());
                    }
                    if (vo.getSource() != null) {
                        d.setSource(vo.getSource());
                    }
                    if (vo.getUnUseValue() != null) {
                        d.setUnUseValue(vo.getUnUseValue());
                    }
                    if (vo.getActualValue() != null) {
                        d.setActualValue(vo.getActualValue());
                    }
                });
                newList.add(d);
            });
            //排序
            List<PileGroupChargeLineVO.ChargeLineDetail> list = newList.stream().sorted((f, s) -> (int) (f.getCreateTime() - s.getCreateTime())).collect(Collectors.toList());
            resultVo.setDetailList(list);
        }
        return resultVo;
    }

    @Override
    public BigDecimal getByMeterId(Long meterId) {
        String key = RedisKeyConstant.getPileGroupRelateMeterKey(meterId);
        String meterKey = RedisKeyConstant.getPileGroupRelateMeterValueKey(meterId);
        String redisValue = stringRedisTemplate.opsForValue().get(key);
        OpLocationPileGroupEntity entity = null;
        BigDecimal result = null;
        if (StringUtils.isNotBlank(redisValue)) {
            entity = JSON.parseObject(redisValue, OpLocationPileGroupEntity.class);
        } else {
            List<OpLocationPileGroupEntity> list = opLocationPileGroupRepository.findAllByMeterIdList(Collections.singletonList(meterId));
            if (!CollectionUtils.isEmpty(list)) {
                entity = list.get(0);
                stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(entity), 30L, TimeUnit.DAYS);
            }
        }
        boolean flag = false;
        if (entity == null) {
            result = BigDecimal.valueOf(-1L);
            flag = true;
        } else {
            if (entity.getMeterLocation() == 0) {
                result = BigDecimal.valueOf(0L);
                flag = true;
            } else {
                Long rootId = entity.getId();
                String rootKey = RedisKeyConstant.getPileGroupRelateEvseKey(rootId);
                List<String> evseSnList;
                Set<String> members = stringRedisTemplate.opsForSet().members(rootKey);
                if (!CollectionUtils.isEmpty(members)) {
                    evseSnList = new ArrayList<>(members);
                } else {
                    List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(rootId);
                    Set<Long> ids = children.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
                    List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(ids);
                    List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
                    evseSnList = opLocationEvseService.findList(pileSnList).stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
                    stringRedisTemplate.opsForSet().add(rootKey, evseSnList.toArray(new String[0]));
                    stringRedisTemplate.expire(rootKey, 30L, TimeUnit.DAYS);
                }
                List<OpEvseMeterUploadDTO> data = monitorFeignClient.queryNewMeters(evseSnList).getData();
                if (!CollectionUtils.isEmpty(data)) {
                    String unit = entity.getChargingUpUnit();
                    result = BigDecimal.valueOf(data.stream().filter(d -> d.getCurrent() != null && EvseDeviceStatusEnum.CHARGING.getName().equals(d.getEvseStatusName())).mapToDouble(OpEvseMeterUploadDTO::getCurrent).sum());
                    if (UnitEnum.POWER.getCode().equals(unit)) {
                        result = BigDecimal.valueOf(data.stream().filter(d -> d.getPower() != null && EvseDeviceStatusEnum.CHARGING.getName().equals(d.getEvseStatusName())).mapToDouble(OpEvseMeterUploadDTO::getPower).sum()).divide(BigDecimal.valueOf(1000L));
                    }
                }
            }
        }
        if (flag && result != null) {
            stringRedisTemplate.opsForValue().set(meterKey, result.toString(), 30L, TimeUnit.DAYS);
        }
        return result;
    }

    private Long mergeSettingVo(List<ChargingProfileSettingVO> resultData, BigDecimal maxVal, String chargingUpUnit, List<PlanTimeListVO> resultList) {
        List<ChargingProfileSettingVO> list = resultData.stream().sorted((f, s) -> (int) (s.getCreateTime() - f.getCreateTime())).collect(Collectors.toList());
        ChargingProfileSettingVO lastVo = list.get(0);
        List<PlanTimeListVO> listVOS = new ArrayList<>();
        //成本最优去掉第一个
        Long lastTime = null;
        List<PlanTimeListVO> tmpList = null;
        for (int index = 0; index < list.size(); index++) {
            tmpList = new ArrayList<>();
            ChargingProfileSettingVO settingVO = list.get(index);
            String pl = settingVO.getPlanTimeList();
            Long offset = this.getOffset(settingVO);
            List<PlanTimeVO> planTimeVOS = JSON.parseArray(pl, PlanTimeVO.class).stream().sorted((f, s) -> (int) (f.getStartPeriod() - s.getStartPeriod())).collect(Collectors.toList());
            boolean flag = false;
            if (planTimeVOS.size() > 1) {
                PlanTimeVO first = planTimeVOS.get(0);
                PlanTimeVO second = planTimeVOS.get(1);
                if ("00:00".equals(first.getBeginTime()) && first.getChargingUp().equals(second.getChargingUp())) {
                    flag = true;
                    planTimeVOS.remove(0);
                }
            }
            Integer source = null;
            BigDecimal max = maxVal;
            if (flag) {
                max = planTimeVOS.stream().sorted((f, s) -> (int) (s.getChargingUp().compareTo(f.getChargingUp()))).collect(Collectors.toList()).get(0).getChargingUp();
                if (UnitEnum.POWER.getCode().equals(chargingUpUnit)) {
                    max = max.divide(BigDecimal.valueOf(1000L), 2, BigDecimal.ROUND_HALF_UP);
                }
            }
            if (lastTime == null) {
                lastTime = planTimeVOS.get(0).getStartPeriod() * 1000 + offset;
            }
            PlanTimeListVO last = null;
            PlanTimeListVO end = null;
            for (int count = planTimeVOS.size() - 1; count >= 0; count--) {
                PlanTimeVO planTimeVO = planTimeVOS.get(count);
                Long createTime = planTimeVO.getStartPeriod() * 1000 + offset;
                BigDecimal chargingUp = planTimeVO.getChargingUp();
                if (UnitEnum.POWER.getCode().equals(chargingUpUnit)) {
                    chargingUp = chargingUp.divide(BigDecimal.valueOf(1000L), 2, BigDecimal.ROUND_HALF_UP);
                }
                if (count == 0) {
                    source = settingVO.getSource();
                } else {
                    source = null;
                }
                PlanTimeListVO build = PlanTimeListVO.builder()
                        .beginTime(planTimeVO.getBeginTime())
                        .use(chargingUp)
                        .unUse(maxVal)
                        .createTime(createTime)
                        .source(source)
                        .build();
                if (last == null) {
                    last = build;
                }
                if (index == 0 || createTime.longValue() <= lastTime.longValue()) {
                    tmpList.add(build);
                }
                if (index == 0 && flag) {
                    boolean fast = true;
                    if (settingVO.getPlanEndTime().longValue() > createTime.longValue() && settingVO.getPlanEndTime().longValue() < last.getCreateTime().longValue()) {
                        end = new PlanTimeListVO();
                        end.setUse(build.getUse());
                        end.setEnd(true);
                        if (settingVO.getPlanEndTime().longValue() == settingVO.getPlanEndTimeFast().longValue()) {
                            end.setUnUse(build.getUnUse());
                            fast = false;
                        }
                        end.setCreateTime(settingVO.getPlanEndTime());
                        tmpList.add(end);
                    }
                    if (fast && settingVO.getPlanEndTimeFast().longValue() > createTime.longValue() && settingVO.getPlanEndTimeFast().longValue() < last.getCreateTime().longValue()) {
                        PlanTimeListVO fastEnd = new PlanTimeListVO();
                        fastEnd.setUnUse(build.getUnUse());
                        fastEnd.setEnd(true);
                        fastEnd.setCreateTime(settingVO.getPlanEndTimeFast());
                        tmpList.add(fastEnd);
                    }
                }
                if (count == 0) {
                    lastTime = createTime;
                }
                last = build;
            }
            if (index == 0 && end == null && tmpList.size() > 0) {
                PlanTimeListVO lastPlanTimeVo = tmpList.get(0);
                if (flag) {
                    //使用智能充电预估结束时间
                    tmpList.add(PlanTimeListVO.builder()
                            .use(lastPlanTimeVo.getUse())
                            .createTime(settingVO.getPlanEndTime())
                            .end(true)
                            .build());
                    //未使用智能充电预估结束时间
                    tmpList.add(PlanTimeListVO.builder()
                            .unUse(lastPlanTimeVo.getUnUse())
                            .createTime(settingVO.getPlanEndTimeFast())
                            .end(true)
                            .build());
                } else {
                    //使用智能充电预估结束时间
                    tmpList.add(PlanTimeListVO.builder()
                            .use(lastPlanTimeVo.getUse())
                            .unUse(lastPlanTimeVo.getUnUse())
                            .createTime(lastPlanTimeVo.getCreateTime() + 5 * 3600000)
                            .end(true)
                            .build());
                }
            }
            if (index == 0 && end != null) {
                tmpList = tmpList.stream().filter(vo -> vo.getCreateTime().longValue() <= settingVO.getPlanEndTime().longValue()).collect(Collectors.toList());
            }
            if (tmpList.size() > 0) {
                listVOS.addAll(tmpList);
            }
        }
        //合并处理
        listVOS = listVOS.stream().sorted((f, s) -> (int) (f.getCreateTime().compareTo(s.getCreateTime()))).collect(Collectors.toList());
        Map<Long, PlanTimeListVO> map = listVOS.stream().collect(Collectors.toMap(PlanTimeListVO::getCreateTime, e -> e, (f, s) -> f));
        resultList.addAll(map.values().stream().sorted((f, s) -> (int) (s.getCreateTime() - f.getCreateTime())).collect(Collectors.toList()));
        return this.getStartTime(lastVo);
    }

    private Long getOffset(ChargingProfileSettingVO settingVO) {
        SetChargingProfile setChargingProfiles = getSetChargingProfiles(settingVO.getProfileValue());
        String startSchedule = setChargingProfiles.getCsChargingProfiles().getChargingSchedule().getStartSchedule();
        LocalDateTime parse = LocalDateTime.parse(startSchedule, DateTimeFormatter.ofPattern(BaseConstant.DEFAULT_LONG_PATTERN));
        return LocalDateTime.from(parse).atZone(ZoneId.of(BaseConstant.GMT)).toInstant().toEpochMilli();
    }

    private Long getStartTime(ChargingProfileSettingVO lastSettingVo) {
        SetChargingProfile setChargingProfiles = getSetChargingProfiles(lastSettingVo.getProfileValue());
        String startSchedule = setChargingProfiles.getCsChargingProfiles().getChargingSchedule().getStartSchedule();
        LocalDateTime parse = LocalDateTime.parse(startSchedule, DateTimeFormatter.ofPattern(BaseConstant.DEFAULT_LONG_PATTERN));
        long offset = LocalDateTime.from(parse).atZone(ZoneId.of(BaseConstant.GMT)).toInstant().toEpochMilli();
        List<PlanTimeVO> list = JSON.parseArray(lastSettingVo.getPlanTimeList(), PlanTimeVO.class).stream().sorted((f, s) -> (int) (f.getStartPeriod() - s.getStartPeriod())).collect(Collectors.toList());
        int index = 0;
        if (list.size() > 1) {
            index = 1;
        }
        return list.get(index).getStartPeriod() * 1000 + offset;
    }

    private EstimatedChargeLineVO getFutureEstimatedChargeLine(Long time, List<EstimatedChargeLineVO> futureChargeLineVOList, EstimatedChargeLineVO futureEstimatedChargeLine) {
        if (CollectionUtils.isEmpty(futureChargeLineVOList)) {
            return futureEstimatedChargeLine;
        }

        for (EstimatedChargeLineVO estimatedChargeLineVO : futureChargeLineVOList) {
            if (time >= estimatedChargeLineVO.getCreateTime()) {
                return estimatedChargeLineVO;
            }
        }
        return futureEstimatedChargeLine;
    }

    private Long getTime(String time, ZoneId zoneId, LocalDate localDate) {
        LocalTime localPlanTime = LocalTime.parse(time);
        LocalDateTime localPlanDateTime = LocalDateTime.of(localDate, localPlanTime);
        return localPlanDateTime.atZone(zoneId).toInstant().toEpochMilli();
    }


    @Override
    public List<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPileList(Long groupId) {
        List<OpLocationPileGroupAssociateEntity> associateList = opLocationPileGroupAssociateRepository.findList(groupId);
        if (CollectionUtils.isEmpty(associateList)) {
            return new ArrayList<>();
        }

        List<String> pileSnList = associateList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = opLocationPileEvseService.findList(pileSnList).stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        List<OpEvseMeterUploadDTO> evseMeterDtoList = pileMonitorServiceAdapter.queryNewMeters(evseSnList);
        Map<String, OpEvseMeterUploadDTO> evseMeterDtoMap = null;
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            evseMeterDtoMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
        }
        List<ChargingProfileSettingVO> profileList = this.getProfileList(evseMeterDtoList);
        Map<String, ChargingProfileSettingVO> profileMap = null;
        if (!CollectionUtils.isEmpty(profileList)) {
            profileMap = profileList.stream().collect(Collectors.toMap(ChargingProfileSettingVO::getEvseSn, e -> e, (f, s) -> f));
        }
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        String zoneId = this.getZoneId(associateList.get(0).getLocationId());
        List<OpLocationPileGroupV2VO.GroupPileV2DTO> pileResultList = getGroupPileDtoList(associateEntityMap.get(groupId), pileDtoMap, evseDtoMap, evseMeterDtoMap, profileMap, zoneId);

        if (CollectionUtils.isEmpty(pileResultList)) {
            return new ArrayList<>();
        }
        return pileResultList;
    }

    @Override
    public List<OpLocationPileGroupVO> loadAll(Long rootId) {
        List<OpLocationPileGroupEntity> entityList = opLocationPileGroupRepository.findAllRoot(rootId);
        if (CollectionUtils.isEmpty(entityList)) {
            return null;
        }
        Set<Long> pIds = entityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
        Set<Long> ids = new HashSet<>();
        Map<Long, List<OpLocationPileGroupEntity>> rootMap = new HashMap<>(pIds.size());
        pIds.stream().forEach(pid -> {
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(pid, 1);
            if (CollectionUtils.isEmpty(children)) {
                return;
            }
            ids.addAll(children.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toList()));
            rootMap.put(pid, children);
        });
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(ids);
        if (CollectionUtils.isEmpty(associateEntityList)) {
            return null;
        }
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateEntityList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findList(pileSnList);
        if (CollectionUtils.isEmpty(pileDtoList)) {
            return null;
        }
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        if (CollectionUtils.isEmpty(evseDtoList)) {
            return null;
        }
        List<OpLocationPileGroupVO> resultList = new ArrayList<>();
        rootMap.forEach((k, v) -> {
            OpLocationPileGroupEntity parentEntity = v.get(0);
            OpLocationPileGroupVO vo = oncePerRequestConvert.toGroupVo(parentEntity);
            vo.setZoneId(this.getZoneId(vo.getLocationId()));
            List<OpLocationPileGroupAssociateEntity> associateList = associateEntityMap.get(k);
            if (!CollectionUtils.isEmpty(associateList)) {
                vo.setGroupPileDTOList(this.setAssociatePileList(associateList, pileDtoList, evseDtoList));
            }
            if (v.size() > 1) {
                vo.setChildrenList(this.generateAssociateChildren(k, v, associateEntityMap, pileDtoList, evseDtoList));
            }
            resultList.add(vo);
        });
        log.info("loadAll,resultList={}", JSON.toJSONString(resultList));
        return resultList;
    }

    @Override
    public Boolean redelivery(RedeliveryDTO dto) {
        log.info("redelivery,dto={}", JSON.toJSONString(dto));
        List<EvseMonitorMistakeDTO> evseList = dto.getEvseList();
        if (CollectionUtils.isEmpty(evseList)) {
            return false;
        }
        OpLocationPileGroupEntity root = opLocationPileGroupRepository.findOne(dto.getGroupId());
        if (root == null || root.getDeleted() == 1) {
            return false;
        }
        OpLocationPileGroupStrategyDTO paramDto = new OpLocationPileGroupStrategyDTO();
        paramDto.setRootId(root.getId());
        if (dto.getSource() != null) {
            paramDto.setUpdateType(dto.getSource());
        } else {
            paramDto.setUpdateType(4);
        }
        OncePerRequestContext context = this.createdContext(paramDto);
        context.putAll(evseList);
        distributeContext.getStrategy(root.getAllocationStrategy()).execute(context);
        return true;
    }

    private List<OpLocationPileGroupVO> generateAssociateChildren(Long parentId, List<OpLocationPileGroupEntity> childrenList, Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap, List<OpLocationPileEvseElasticDTO> pileDtoList, List<OpLocationEvseElasticDTO> evseDtoList) {
        List<OpLocationPileGroupVO> resultList = new ArrayList<>();
        childrenList.stream().forEach(entity -> {
            Long pid = entity.getPid();
            if (parentId.longValue() == pid.longValue()) {
                OpLocationPileGroupVO vo = oncePerRequestConvert.toGroupVo(entity);
                vo.setZoneId(this.getZoneId(vo.getLocationId()));
                List<OpLocationPileGroupAssociateEntity> associateList = associateEntityMap.get(entity.getId());
                if (!CollectionUtils.isEmpty(associateList)) {
                    vo.setGroupPileDTOList(this.setAssociatePileList(associateList, pileDtoList, evseDtoList));
                }
                if (childrenList.size() > 1) {
                    vo.setChildrenList(this.generateAssociateChildren(entity.getId(), childrenList, associateEntityMap, pileDtoList, evseDtoList));
                }
                resultList.add(vo);
            }
        });
        return resultList;
    }

    private List<OpLocationPileGroupVO.GroupPileDTO> setAssociatePileList(List<OpLocationPileGroupAssociateEntity> associateList, List<OpLocationPileEvseElasticDTO> pileDtoList, List<OpLocationEvseElasticDTO> evseDtoList) {
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = pileDtoList.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<OpLocationPileGroupVO.GroupPileDTO> resultList = new ArrayList<>(associateList.size());
        associateList.stream().forEach(entity -> {
            String pileSn = entity.getPileSn();
            if (pileDtoMap.get(pileSn) != null && !CollectionUtils.isEmpty(evseDtoMap.get(pileSn))) {
                OpLocationPileGroupVO.GroupPileDTO dto = oncePerRequestConvert.toPileVo(entity);
                dto.setGroupPileEvseDTOList(evseDtoMap.get(pileSn).stream().map(evseDto -> {
                    OpLocationPileGroupVO.GroupPileEvseDTO pileEvseDTO = oncePerRequestConvert.toEvseVo(evseDto);
                    return pileEvseDTO;
                }).collect(Collectors.toList()));
                resultList.add(dto);
            }
        });
        return resultList;
    }

    private MeterDataRecordVO getMeterDataRecordVO(Long groupId) {
        Result<MeterDataRecordVO> last = meterDataClient.getLast(groupId.toString());
        log.info("getMeterDataRecordVO,last={}", JSON.toJSONString(last));
        if (last != null && last.getCode() == 200 && last.getData() != null) {
            return last.getData();
        }
        return null;
    }

    @Override
    public Result<OpPileGroupAssociateVO> queryAssociateByPileSn(String pileSn) {
        OpLocationPileGroupAssociateEntity entity = opLocationPileGroupAssociateRepository.findOne(pileSn);
        if (entity == null) {
            return Result.ofSucceed(null);
        }
        Long groupId = entity.getGroupId();
        OpLocationPileGroupEntity groupEntity = opLocationPileGroupRepository.findOne(groupId);
        if (groupEntity == null || groupEntity.getStatus() == 0) {
            return Result.ofSucceed(null);
        }
        OpPileGroupAssociateVO vo = new OpPileGroupAssociateVO();
        BeanUtils.copyProperties(entity, vo);
        if (groupEntity.getLocationId() != null) {
            vo.setLocationId(groupEntity.getLocationId());
            vo.setZoneId(this.getZoneId(groupEntity.getLocationId()));
        }
        vo.setEnergyUseStrategy(groupEntity.getEnergyUseStrategy());
        vo.setFavor(groupEntity.getFavor());
        vo.setPlanTime(groupEntity.getPlanTime());
        vo.setMinReserve(groupEntity.getMinReserve());
        vo.setLoadType(groupEntity.getLoadType());
        vo.setPowerEquipmentEnabled(groupEntity.getPowerEquipmentEnabled());
        vo.setPowerEquipmentStartTime(groupEntity.getPowerEquipmentStartTime());
        vo.setPowerEquipmentUp(groupEntity.getPowerEquipmentUp());
        vo.setElectricUp(groupEntity.getElectricUp());
        vo.setMeterVoltage(groupEntity.getMeterVoltage());
        vo.setMeterId(groupEntity.getMeterId());
        return Result.ofSucceed(vo);
    }

    private OpLocationPileGroupEntity getPileGroupByConditionV3(SmartChargeGroupConfigUpdateParamDTOcopy opLocationPileGroupDTO) {
        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class)
                .eq(OpLocationPileGroupEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationPileGroupEntity::getMerchantId, opLocationPileGroupDTO.getMerchantId())
                .eq(OpLocationPileGroupEntity::getName, opLocationPileGroupDTO.getName())
                .ne(OpLocationPileGroupEntity::getId, opLocationPileGroupDTO.getId()).last("limit 1");
        return opLocationPileGroupRepository.getOne(queryWrapper);
    }

    private OpLocationPileGroupEntity getPileGroupByCondition(OpLocationPileGroupDTO opLocationPileGroupDTO) {
        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class)
                .eq(OpLocationPileGroupEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationPileGroupEntity::getLocationId, opLocationPileGroupDTO.getLocationId())
                .eq(OpLocationPileGroupEntity::getName, opLocationPileGroupDTO.getName());
        return opLocationPileGroupRepository.getOne(queryWrapper);
    }

    public OpLocationPileGroupAssociateEntity toPileGroupAssociateEntity(OpLocationPileGroupEntity pileGroupEntity, OpLocationPileGroupAssociateDTO dto) {
        OpLocationPileGroupAssociateEntity entity = new OpLocationPileGroupAssociateEntity();
        BeanUtils.copyProperties(dto, entity);
        entity.setCreatedAt(System.currentTimeMillis());
        entity.setUpdatedAt(System.currentTimeMillis());
        entity.setGroupId(pileGroupEntity.getId());
        entity.setLocationId(pileGroupEntity.getLocationId());
        return entity;
    }

    public OpLocationPileGroupEntity toOpLocationPileGroupEntity(OpLocationPileGroupDTO dto) {
        OpLocationPileGroupEntity entity = new OpLocationPileGroupEntity();
        BeanUtils.copyProperties(dto, entity);
        entity.setUseSmartCharging(1);
        Long pid = dto.getPid();
        if (pid != null) {
            entity.setPid(pid);
        }
        if (UnitEnum.POWER.getCode().equals(dto.getChargingUpUnit())) {
            entity.setChargingUpType("Power");
        }
        entity.setSecurityEnabled(false);
        if (dto.getSecurityEnabled() != null) {
            entity.setSecurityEnabled(dto.getSecurityEnabled());
        }
        List<TimeSettingDetailDTO> timeSettingDetails = dto.getTimeSettingDetails();
        if (!CollectionUtils.isEmpty(timeSettingDetails)) {
            entity.setTimeSettingMode(1);
            timeSettingDetails.stream().forEach(d -> {
                d.setId(IdWorker.getId());
            });
            entity.setTimeSettingDetail(JSON.toJSONString(timeSettingDetails));
        }
        entity.setStatus(1);
        return entity;
    }

    private void deleteByEntity(OpLocationPileGroupAssociateEntity associateEntity) {
        if (associateEntity == null) {
            return;
        }
        //所在群组只有一个桩，删除群组
        Long groupId = associateEntity.getGroupId();
        OpLocationPileGroupEntity entity = opLocationPileGroupRepository.findOne(groupId);
        List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(groupId);
        boolean flag = true;
        if (!CollectionUtils.isEmpty(associateEntityList)) {
            if (associateEntityList.size() == 1) {
                Set<Long> ids = new HashSet<>();
                ThreadPoolUtil.getExecutor().execute(RunnableWrapper.of(() -> this.removeKey(null, Collections.singletonList(entity))));
                List<OpLocationPileGroupEntity> children = this.opLocationPileGroupRepository.findChildren(groupId, false);
                if (CollectionUtils.isEmpty(children)) {
                    ids.add(groupId);
                }
                Long root = this.opLocationPileGroupRepository.findOrdinaryRoot(groupId);
                if (!groupId.equals(root)) {
                    List<OpLocationPileGroupEntity> tmpList = this.opLocationPileGroupRepository.findChildren(root);
                    OpLocationPileGroupEntity currentEntity = tmpList.stream().filter(e -> e.getId().equals(groupId)).findFirst().orElse(null);
                    Integer level = currentEntity.getLevel();
                    //父辈级没有桩且只有一个子群组也要删除
                    tmpList = tmpList.stream().filter(e -> e.getLevel().intValue() < level.intValue()).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(tmpList)) {
                        Map<Integer, List<OpLocationPileGroupEntity>> tmpMap = tmpList.stream().collect(Collectors.groupingBy(OpLocationPileGroupEntity::getLevel));
                        Set<Long> asIds = new HashSet<>();
                        tmpMap.forEach((k, v) -> {
                            if (v.size() == 1) {
                                asIds.add(v.get(0).getId());
                            }
                        });
                        if (!CollectionUtils.isEmpty(asIds)) {
                            ids.addAll(asIds);
                            List<OpLocationPileGroupAssociateEntity> listToUse = this.opLocationPileGroupAssociateRepository.findList(asIds);
                            if (!CollectionUtils.isEmpty(listToUse)) {
                                Map<Long, List<OpLocationPileGroupAssociateEntity>> mapToUse = listToUse.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
                                asIds.stream().forEach(tmpId -> {
                                    if (!CollectionUtils.isEmpty(mapToUse.get(tmpId))) {
                                        ids.remove(tmpId);
                                    }
                                });
                            }
                        }
                    }
                }
                opLocationPileGroupRepository.deleteBatch(ids);
                if (entity.getPid() == 0L) {
                    //是根群组不需要触发配置下发
                    flag = false;
                }
            }
        }
        associateEntity.setDeleted(Boolean.TRUE);
        associateEntity.setUpdatedAt(System.currentTimeMillis());
        opLocationPileGroupAssociateRepository.deleteBatch(Arrays.asList(associateEntity));
        //删除的不是根群组，触发群组配置下发
        if (flag) {
            Long root = opLocationPileGroupRepository.findOrdinaryRoot(entity.getId());
            if (root != null) {
                ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                    //触发配置下发
                    OpLocationPileGroupStrategyDTO params = new OpLocationPileGroupStrategyDTO();
                    params.setRootId(root);
                    params.setUpdateType(7);
                    distributeContext.getStrategy(opLocationPileGroupRepository.findOne(root).getAllocationStrategy()).execute(this.createdContext(params));
                    this.sendMQ(root, 2);
                    this.removeKey(root, null);
                    this.refreshTimeSetting(root, false);
                }), 3L, TimeUnit.SECONDS);
            }
        }
        //不能异步
        //删除配置
        OpLocationPileGroupStrategyDTO dto = new OpLocationPileGroupStrategyDTO();
        OncePerRequestContext context = this.createdContext(dto);
        context.setDeleteList(Collections.singletonList(associateEntity));
        distributeContext.getStrategy(BaseConstant.CLEAR_CHARGING_PROFILE).execute(context);
        this.sendMQ(groupId, 3);
        this.removeKey(groupId, null);
        this.refreshTimeSetting(groupId, true);
    }

    @Override
    public Result<Boolean> deletedAssociateByPileId(Long pileId) {
        try {
            OpLocationPileGroupAssociateEntity associateEntity = opLocationPileGroupAssociateRepository.findOne(pileId);
            this.deleteByEntity(associateEntity);
            return Result.ofSucceed(Boolean.TRUE);
        } catch (Exception e) {
            log.error("OpLocationPileGroupServiceImpl.deletedAssociateByPileId exception = ", e);
            return Result.ofSucceed(Boolean.FALSE);
        }
    }

    @Override
    public void run(String... args) throws Exception {
        ThreadPoolUtil.getScheduledExecutor().schedule(() -> {
            List<OpPileGroupTimeSettingDTO> list = this.loadAllTimeSetting(null);
            if (!CollectionUtils.isEmpty(list)) {
                list.stream().forEach(dto -> {
                    Long groupId = dto.getGroupId();
                    String key = RedisKeyConstant.getPileBaseTimeSettingKey(groupId);
                    stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(dto), 24L, TimeUnit.HOURS);
                    MessageProperties messageProperties = new MessageProperties();
                    messageProperties.setDelay(dto.getDelay());
                    Message message = new Message((JSON.toJSONString(dto)).getBytes(StandardCharsets.UTF_8), messageProperties);
                    rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_TIME_SETTING_EXCHANGE + rabbitmqVersionSuffix, AmqpConstant.PILE_BASE_TIME_SETTING_ROUTE, message);
                });
            }
        }, 10L, TimeUnit.SECONDS);
    }

    @Override
    public List<OpPileGroupTimeSettingDTO> loadAllTimeSetting(Long rootId) {
        List<OpPileGroupTimeSettingDTO> resultList = new ArrayList<>();
        List<OpLocationPileGroupEntity> allEntityList = opLocationPileGroupRepository.findAllRoot(rootId);
        if (CollectionUtils.isEmpty(allEntityList)) {
            return resultList;
        }
        long now = System.currentTimeMillis();
        Map<Long, OpPileGroupTimeSettingDTO> resultMap = new HashMap<>();
        allEntityList.stream().forEach(entity -> {
            Long id = entity.getId();
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(id);
            for (OpLocationPileGroupEntity child : children) {
                this.setNextTime(now, resultMap, child, id);
            }
        });
        log.info("loadAllTimeSetting,resultMap={}", JSON.toJSONString(resultMap));
        if (!CollectionUtils.isEmpty(resultMap)) {
            resultMap.forEach((k, v) -> resultList.add(v));
        }
        return resultList;
    }


    public void loadControlContentDTOs(Long groupId, Map<Long, OpPileGroupTimeSettingDTO> resultMap,List<ControlContentDTO> controlContentDTOList) {
        List<OpLocationPileGroupEntity> allEntityList = opLocationPileGroupRepository.findAllRoot(groupId);
        if (CollectionUtils.isEmpty(allEntityList)) {
            return;
        }
        long now = System.currentTimeMillis();
        allEntityList.stream().forEach(entity -> {
            Long id = entity.getId();
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(id);
            for (OpLocationPileGroupEntity child : children) {
                this.setControlTime(now, resultMap, child, id,controlContentDTOList);
            }
        });
        log.info("loadAllTimeSetting,resultMap={}", JSON.toJSONString(resultMap));
    }





    private void setControlTime(long now, Map<Long, OpPileGroupTimeSettingDTO> resultMap, OpLocationPileGroupEntity entity, Long rootId,List<ControlContentDTO> controlContentDTOList) {
        Long locationId = entity.getLocationId();
        String zoneId = this.getZoneId(locationId);
        LocalDate localDate = LocalDateTime.ofInstant(Instant.ofEpochMilli(now), ZoneId.of(zoneId)).toLocalDate();
        int month = localDate.getMonthValue();
        int season = convertMonthToSeason(month);
        int week =convertToWeek(localDate);
        controlContentDTOList = controlContentDTOList.stream().filter(c -> ((c.getSeason() == season||ObjectUtils.nullSafeEquals(c.getSeason(),0))
                && (c.getWeekDay() == week||ObjectUtils.nullSafeEquals(c.getWeekDay(),0))
        )).sorted(Comparator.comparing(ControlContentDTO::getStartTime)).collect(Collectors.toList());
        //填补间隙
        controlContentDTOList=fillGaps(controlContentDTOList, rootId);
        if (!CollectionUtils.isEmpty(controlContentDTOList)) {
            for (ControlContentDTO dto : controlContentDTOList) {
                long startTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getStartTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (dto.getEndTime().equals("24:00")) {
                    dto.setEndTime("23:59");
                }
                long endTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getEndTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (dto.getEndTime().equals("23:59")) {
                    endTime = endTime + TimeUnit.SECONDS.toMillis(60) - 1;
                }
                if (now >= startTime && now < endTime) {
                    Integer value = Math.toIntExact(endTime - now);
                    OpPileGroupTimeSettingDTO settingDto = new OpPileGroupTimeSettingDTO();
                    settingDto.setNextTime(dto.getEndTime());
                    settingDto.setGroupId(rootId);
                    settingDto.setDelay(value);
                    settingDto.setChargingUp(dto.getPowerUp());
                    settingDto.setRequestId(IdWorker.getId());
                    resultMap.compute(rootId, (k, o) -> o == null ? settingDto : settingDto.getDelay() < o.getDelay() ? settingDto : o);
                    break;
                }
            }
        }
    }

    public static List<ControlContentDTO> fillGaps(List<ControlContentDTO> controlContentDTOList, Long rootId) {
        List<ControlContentDTO> result = new ArrayList<>();
        // 按开始时间排序
        Collections.sort(controlContentDTOList, Comparator.comparing(ControlContentDTO::getStartTime));
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");
        LocalTime startOfDay = LocalTime.of(0, 0);
        LocalTime endOfDay = LocalTime.of(23, 59); // 定义一天的结束时间为23:59
        LocalTime endOfDayExtended = LocalTime.of(0, 0); // 用于表示24:00
        // 处理时间段并填补间隙
        LocalTime previousEnd = startOfDay;
        for (ControlContentDTO dto : controlContentDTOList) {
            LocalTime startTime = LocalTime.parse(dto.getStartTime(), formatter);
            LocalTime endTime = LocalTime.parse(dto.getEndTime(), formatter);

            // 如果之前的结束时间和当前的开始时间之间有间隙，则添加间隙
            if (previousEnd.isBefore(startTime)) {
                result.add(new ControlContentDTO(rootId, previousEnd.format(formatter), startTime.format(formatter)));
            }
            result.add(dto);
            previousEnd = endTime;
        }
        // 如果最后一个时间段结束时间不是一天的最后，补齐最后的间隙到24:00
        if (previousEnd.isBefore(endOfDay)) {
            result.add(new ControlContentDTO(rootId, previousEnd.format(formatter), "24:00"));
        }
        return result;
    }




    private void setNextTime(long now, Map<Long, OpPileGroupTimeSettingDTO> resultMap, OpLocationPileGroupEntity entity, Long rootId) {
        Long locationId = entity.getLocationId();
        String zoneId = this.getZoneId(locationId);
        LocalDate localDate = LocalDateTime.ofInstant(Instant.ofEpochMilli(now), ZoneId.of(zoneId)).toLocalDate();

        //这里分时控制的逻辑是未被加入需求控制器，则走分时，如果加入了，则走需求控制器，所以要加上controlContentDTOList为空的判断
        String controllerContent = demandControlConfigService.findContentByGroupId(opLocationPileGroupRepository.findOrdinaryRoot(rootId));
        List<ControlContentDTO> controlContentDTOList = JSON.parseArray(controllerContent, ControlContentDTO.class);
        if (!CollectionUtils.isEmpty(controlContentDTOList)){
            loadControlContentDTOs(rootId,resultMap,controlContentDTOList);
        }

        if (entity.getTimeSettingMode() == 1 && StringUtils.isNotBlank(entity.getTimeSettingDetail())) {
            String detail = entity.getTimeSettingDetail();
            List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
            LocalDateTime localDateTime = LocalDateTime.now(ZoneId.of(zoneId));
            int day = localDateTime.getDayOfWeek().getValue();
            TimeSettingDetailDTO detailDTO = timeSettingDetailDTOS.stream().filter(d -> d.getDays().contains(day)).findFirst().get();
            if (detailDTO != null) {
                List<TimeSettingDetailDTO.DetailDTO> details = detailDTO.getDetails().stream().sorted(Comparator.comparing(TimeSettingDetailDTO.DetailDTO::getStartTime)).collect(Collectors.toList());
                for (TimeSettingDetailDTO.DetailDTO dto : details) {
                    long startTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getStartTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                    if (dto.getEndTime().equals("24:00")) {
                        dto.setEndTime("23:59");
                    }
                    long endTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getEndTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                    if (dto.getEndTime().equals("23:59")) {
                        endTime = endTime + TimeUnit.SECONDS.toMillis(60) - 1;
                    }
                    if (now >= startTime && now < endTime) {
                        Integer value = Math.toIntExact(endTime - now);
                        OpPileGroupTimeSettingDTO settingDto = new OpPileGroupTimeSettingDTO();
                        settingDto.setNextTime(dto.getEndTime());
                        settingDto.setGroupId(rootId);
                        settingDto.setDelay(value);
                        settingDto.setChargingUp(dto.getChargingUp());
                        settingDto.setRequestId(IdWorker.getId());
                        resultMap.compute(rootId, (k, o) -> o == null ? settingDto : settingDto.getDelay() < o.getDelay() ? settingDto : o);
                        break;
                    }
                }
            }
        }
    }

    private OncePerRequestContext createdContext(OpLocationPileGroupStrategyDTO dto) {
        log.info("下发信息为{}", JSON.toJSONString(dto));
        Long rootId = dto.getRootId();
        Integer diy = dto.getDiy();
        OncePerRequestContext context = new OncePerRequestContext(rootId, dto.getUpdateType());
        context.setStatus(dto.getStatus());
        //查询群组
        if (rootId != null) {
            List<DeliveryGroupDTO> deliveryGroupList = new ArrayList<>();
            List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(rootId, 1);
            if (CollectionUtils.isEmpty(children)) {
                log.error("OncePerRequestContext,children is empty.");
                return null;
            }
            children.stream().forEach(e -> {
                DeliveryGroupDTO d = oncePerRequestConvert.toDeliveryDto(e);
                deliveryGroupList.add(d);
                //设置根群组
                if (rootId.longValue() == e.getId().longValue()) {
                    context.setRootDto(d);
                }
                //设置时区ID
                if (context.getZoneId() == null) {
                    context.setZoneId(this.getZoneId(d.getLocationId()));
                }
                //设置VIP列表
                if (d.getPriority() == 1) {
                    List<VipPolicy> vipPolicy = this.opLocationPileVipConfigRepository.findList(d.getId());
                    LocalDateTime now = LocalDateTime.now(ZoneId.of(context.getZoneId()));
                    //过滤失效数据
                    List<VipPolicy> tmpList = vipPolicy.stream().filter(v -> v.getEffectiveDate().compareTo(now) <= 0 && now.compareTo(v.getExpirationDate()) <= 0).collect(Collectors.toList());
                    d.setVipPolicy(tmpList);
                }
            });
            //是否生成树形结构
            boolean tree = false;
            String strategy = context.getRootDto().getAllocationStrategy();
            if (context.getRootDto().getEnergyUseStrategy() == 0 && (strategy.equals(BaseConstant.DISTRIBUTE_EVENLY) || strategy.equals(BaseConstant.DISTRIBUTE_EQUALLY))) {
                tree = true;
            }
            //查询群组关联的桩
            Set<Long> groupIds = deliveryGroupList.stream().map(DeliveryGroupDTO::getId).collect(Collectors.toSet());
            List<OpLocationPileGroupAssociateEntity> associateEntityList = opLocationPileGroupAssociateRepository.findList(groupIds);
            if (CollectionUtils.isEmpty(associateEntityList)) {
                log.error("OncePerRequestContext,associateEntityList is empty.");
                return null;
            }
            Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateEntityList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
            List<String> pileSnList = associateEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
            //查询桩信息
            List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseService.findList(pileSnList);
            Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = pileDtoList.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
            //查询枪信息
            List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
            Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
            context.setPileSnList(pileSnList);
            //计费规则ID
            Set<Long> tariffIds = new HashSet<>();
            //查询用户特征
            Set<Long> userIds = new HashSet<>();
            //充电中上报值查询
            Set<String> chargingList = new HashSet<>();
            //VIP用户
            Set<Long> vipIds = new HashSet<>();
            deliveryGroupList.stream().forEach(d -> {
                Long id = d.getId();
                String chargingUpUnit = d.getChargingUpUnit();
                Integer energyUseStrategy = d.getEnergyUseStrategy();
                Integer priority = d.getPriority();
                List<VipPolicy> vipPolicy = d.getVipPolicy();

                List<OpLocationPileGroupAssociateEntity> entityList = associateEntityMap.get(id);
                if (CollectionUtils.isEmpty(entityList)) {
                    return;
                }
                List<DeliveryPileInfoDTO> pileInfoList = new ArrayList<>();
                entityList.stream().forEach(e -> {
                    String pileSn = e.getPileSn();
                    OpLocationPileEvseElasticDTO pileDto = pileDtoMap.get(pileSn);
                    if (pileDto == null) {
                        return;
                    }
                    DeliveryPileInfoDTO pileInfo = oncePerRequestConvert.toDeliveryDto(pileDto);
                    List<OpLocationEvseElasticDTO> tmpList = evseDtoMap.get(pileSn);
                    if (CollectionUtils.isEmpty(tmpList)) {
                        return;
                    }
                    List<DeliveryEvseInfoDTO> evseInfoList = new ArrayList<>();
                    tmpList.stream().forEach(evseDto -> {
                        DeliveryEvseInfoDTO evseInfo = oncePerRequestConvert.toDeliveryDto(evseDto);
                        String evseSn = evseInfo.getEvseSn();
                        String key = "energy:pile:base:charging:start:evse_sn:" + evseSn;
                        String redisState = stringRedisTemplate.opsForValue().get(key);
                        if (redisState != null) {
                            evseInfo.setState(redisState);
                        }
                        String state = evseInfo.getState();
                        //设置相数
                        evseInfo.setPhaseNum(NumberPhasesEnum.getPhaseNum(evseDto.getPowerType()));
                        //成本优先
                        if (energyUseStrategy != null && energyUseStrategy == 1) {
                            Long tariffId = evseDto.getTariffId();
                            if (tariffId != null) {
                                tariffIds.add(tariffId);
                            }
                            if (DlbUtil.isCharging(state)) {
                                //设置用户ID
                                Long userId = this.getUserId(evseSn);
                                if (userId != null) {
                                    userIds.add(userId);
                                }
                                evseInfo.setUserId(userId);
                                //设置已冲电量
                                evseInfo.setTotalElectricalPower(this.getSumPower(evseSn));
                                //设置上次分配结果
                                String lastKey = RedisKeyConstant.getChargeLastDeliveryKey(evseInfo.getEvseSn());
                                String lastValue = stringRedisTemplate.opsForValue().get(lastKey);
                                if (StringUtils.isNotBlank(lastValue)) {
                                    OpLocationPileGroupDeliveryVO deliveryVO = JSON.parseObject(lastValue, OpLocationPileGroupDeliveryVO.class);
                                    List<PlanTimeVO> planTimeList = deliveryVO.getPlanTimeList();
                                    evseInfo.setPlanTimeList(JSON.parseArray(JSON.toJSONString(planTimeList), PlanTimeVO.class));
                                }
                                //设置是否优先充电或暂停
                                String k = RedisKeyConstant.getSmartChargeOptionalKey(evseInfo.getEvseSn());
                                String val = stringRedisTemplate.opsForValue().get(k);
                                if (StringUtils.isNotBlank(val)) {
                                    evseInfo.setFast(Integer.valueOf(val));
                                }
                            }
                        }
                        //充电中
                        if (DlbUtil.isCharging(state)) {
                            chargingList.add(evseSn);
                        }
                        //是否VIP
                        if (priority == 1 && DlbUtil.isCharging(state)) {
                            //是否包含全部用户
                            if (!CollectionUtils.isEmpty(vipPolicy)) {
                                VipPolicy vipDto = vipPolicy.stream().filter(v -> v.getPrincipalId().longValue() == -1L && v.getType() == 1).findAny().orElse(null);
                                if (vipDto != null) {
                                    evseInfo.setIsVip(true);
                                } else {
                                    Long userId = this.getUserId(evseSn);
                                    if (userId != null) {
                                        evseInfo.setUserId(userId);
                                        vipIds.add(userId);
                                    } else {
                                        log.error("OncePerRequestContext,evseSn={},userId is null.", evseSn);
                                    }
                                }
                            }
                        }
                        //设置插枪时间
                        evseInfo.setAttachTime(this.getAttachTime(evseSn));
                        evseInfoList.add(evseInfo);
                    });
                    //设置桩最大电流功率
                    OpLocationEvseElasticDTO evseDto = tmpList.get(0);
                    BigDecimal pileLimit = evseDto.getAmperage();
                    if (UnitEnum.POWER.getCode().equals(chargingUpUnit)) {
                        pileLimit = BigDecimal.valueOf(Optional.ofNullable(evseDto.getPower()).orElse(0D));
                    }
                    pileInfo.setPileLimit(pileLimit);
                    pileInfo.setUsablePileLimit(pileLimit);
                    pileInfo.setAssociateId(e.getGroupId());
                    //设置枪列表信息
                    pileInfo.setEvseInfoList(evseInfoList);
                    pileInfoList.add(pileInfo);
                });
                d.setPileInfoList(pileInfoList);
            });
            //查询计费规则
            if (tariffIds.size() > 0) {
                this.handleTariff(deliveryGroupList, tariffIds);
            }
            //查询用户特征
            if (userIds.size() > 0) {
                this.handleUserFeature(deliveryGroupList, userIds);
            }
            //查询自定义用户特征
            if (userIds.size() > 0 && diy != 1) {

            }
            //设置最新上报值
            if (chargingList.size() > 0) {
                this.handleMeterValue(deliveryGroupList, chargingList);
            }
            //设置VIP用户
            if (vipIds.size() > 0) {
                this.handleVip(context, deliveryGroupList, vipIds);
            }
            context.setDeliveryGroupList(deliveryGroupList);
            //分时控制处理
            this.handleTimeSetting(context);
            //ALM处理
            this.handleAlm(context);
            //生成树形结构
            if (tree) {
                DeliveryGroupDTO rootDto = context.getRootDto();
                DeliveryGroupTreeVO deliveryGroupTreeVO = oncePerRequestConvert.toTreeVo(rootDto);
                Long id = deliveryGroupTreeVO.getId();
                deliveryGroupTreeVO.setChildrenList(this.reduceTree(context.getDeliveryGroupList(), id, deliveryGroupTreeVO.getChargingUp()));
                if (CollectionUtils.isEmpty(deliveryGroupTreeVO.getPileInfoList())) {
                    deliveryGroupTreeVO.setPileInfoList(new ArrayList<>());
                }
                context.setDeliveryTreeVo(deliveryGroupTreeVO);
                BigDecimal total = BigDecimal.ZERO;
                Integer pileNumber = 0;
                //最小分配6A
                if (!CollectionUtils.isEmpty(deliveryGroupTreeVO.getPileInfoList())) {
                    total = total.add(deliveryGroupTreeVO.getPileInfoList().stream().map(DeliveryPileInfoTreeVO::getPileLimit).reduce((f, s) -> f.add(s)).get());
                    pileNumber += deliveryGroupTreeVO.getPileInfoList().size();
                }
                if (!CollectionUtils.isEmpty(deliveryGroupTreeVO.getChildrenList())) {
                    total = total.add(deliveryGroupTreeVO.getChildrenList().stream().map(DeliveryGroupTreeVO::getTotal).reduce((f, s) -> f.add(s)).get());
                    pileNumber += deliveryGroupTreeVO.getChildrenList().stream().map(DeliveryGroupTreeVO::getPileNumber).reduce((f, s) -> f + s).get();
                }
                deliveryGroupTreeVO.setTotal(total);
                deliveryGroupTreeVO.setPileNumber(pileNumber);
                log.info("OncePerRequestContext,deliveryGroupTreeVO={},source={}", JSON.toJSONString(deliveryGroupTreeVO), context.getSource());
            } else {
                log.info("OncePerRequestContext,deliveryGroupList={},source={}", JSON.toJSONString(deliveryGroupList), context.getSource());
            }
        }
        return context;
    }

    private void handleTariff(List<DeliveryGroupDTO> deliveryGroupList, Set<Long> tariffIds) {
        Map<Long, CostModelRuleEntityVO> costMap = null;
        Result<List<CostModelRuleEntityVO>> costRuleList = tariffFeignClient.getCostRuleList(new ArrayList<>(tariffIds));
        if (costRuleList != null && costRuleList.getCode() == HttpStatus.OK.value() && !CollectionUtils.isEmpty(costRuleList.getData())) {
            List<CostModelRuleEntityVO> listData = costRuleList.getData();
            costMap = listData.stream().collect(Collectors.toMap(CostModelRuleEntityVO::getId, e -> e, (f, s) -> f));
            log.info("OncePerRequestContext,costMap={}", JSON.toJSONString(costMap));
        }
        if (!CollectionUtils.isEmpty(costMap)) {
            Map<Long, CostModelRuleEntityVO> finalCostMap = costMap;
            deliveryGroupList.stream().forEach(e -> {
                List<DeliveryPileInfoDTO> pileInfoList = e.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileInfo -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseInfo -> {
                        Long tariffId = evseInfo.getTariffId();
                        if (tariffId == null) {
                            return;
                        }
                        CostModelRuleEntityVO costDto = finalCostMap.get(tariffId);
                        if (costDto == null) {
                            log.error("OncePerRequestContext,tariffId={} costDto is null.", tariffId);
                            return;
                        }
                        CostModelRuleEntityVO cd = JSON.parseObject(JSON.toJSONString(costDto), CostModelRuleEntityVO.class);
                        evseInfo.setCostDetail(cd);
                    });
                });
            });
        }
    }

    private void handleUserFeature(List<DeliveryGroupDTO> deliveryGroupList, Set<Long> userIds) {
        List<UserFeatureDTO> userList = this.getUserList(new ArrayList<>(userIds));
        if (!CollectionUtils.isEmpty(userList)) {
            Map<Long, List<UserFeatureDTO>> userMap = userList.stream().collect(Collectors.groupingBy(UserFeatureDTO::getUserId));
            deliveryGroupList.stream().forEach(d -> {
                List<DeliveryPileInfoDTO> pileInfoList = d.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileInfo -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseInfo -> {
                        Long userId = evseInfo.getUserId();
                        if (userId == null) {
                            return;
                        }
                        evseInfo.setUserFeatureList(userMap.get(userId));
                    });
                });
            });
        }
    }

    private void handleMeterValue(List<DeliveryGroupDTO> deliveryGroupList, Set<String> chargingList) {
        List<OpEvseMeterUploadDTO> evseMeterDtoList = this.getEvseMeterList(new ArrayList<>(chargingList));
        if (!CollectionUtils.isEmpty(evseMeterDtoList)) {
            Map<String, OpEvseMeterUploadDTO> evseMeterMap = evseMeterDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f));
            deliveryGroupList.stream().forEach(d -> {
                List<DeliveryPileInfoDTO> pileInfoList = d.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileInfo -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseInfo -> {
                        OpEvseMeterUploadDTO uploadDTO = evseMeterMap.get(evseInfo.getEvseSn());
                        if (uploadDTO != null) {
                            BigDecimal timeValue;
                            if (UnitEnum.POWER.getCode().equals(d.getChargingUpUnit())) {
                                timeValue = BigDecimal.valueOf(Optional.ofNullable(uploadDTO.getPower()).orElse(0D)).divide(BigDecimal.valueOf(1000L));
                            } else {
                                timeValue = BigDecimal.valueOf(Optional.ofNullable(uploadDTO.getCurrent()).orElse(0D));
                            }
                            evseInfo.setTimeValue(timeValue);
                            evseInfo.setUploadAmperage(BigDecimal.valueOf(Optional.ofNullable(uploadDTO.getCurrent()).orElse(0D)));
                            evseInfo.setUploadPower(BigDecimal.valueOf(Optional.ofNullable(uploadDTO.getPower()).orElse(0D)).divide(BigDecimal.valueOf(1000L)));
                        }
                    });
                });
            });
        }
    }

    private void handleVip(OncePerRequestContext context, List<DeliveryGroupDTO> deliveryGroupList, Set<Long> vipIds) {
        log.info("OncePerRequestContext,deliveryGroupList={},vipIds={}", JSON.toJSONString(deliveryGroupList), vipIds);
        Long sellerId = context.getRootDto().getSellerId();
        List<MemberGroupVipVO> memberGroupVipVOList = this.findVips(vipIds, sellerId);
        if (!CollectionUtils.isEmpty(memberGroupVipVOList)) {
            Map<Long, MemberGroupVipVO> memberMap = memberGroupVipVOList.stream().collect(Collectors.toMap(MemberGroupVipVO::getUserId, e -> e, (f, s) -> f));
            deliveryGroupList.stream().forEach(d -> {
                Integer priority = d.getPriority();
                List<VipPolicy> vipPolicy = d.getVipPolicy();
                if (priority == null || priority != 1 || CollectionUtils.isEmpty(vipPolicy)) {
                    return;
                }
                List<Long> memberIds = vipPolicy.stream().filter(e -> e.getType() == 2).map(VipPolicy::getPrincipalId).collect(Collectors.toList());
                List<Long> memberGroupIds = vipPolicy.stream().filter(e -> e.getType() == 1).map(VipPolicy::getPrincipalId).collect(Collectors.toList());
                List<DeliveryPileInfoDTO> pileInfoList = d.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileInfo -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseInfo -> {
                        String state = evseInfo.getState();
                        Long userId = evseInfo.getUserId();
                        if (!DlbUtil.isCharging(state) || userId == null) {
                            return;
                        }
                        MemberGroupVipVO vipVO = memberMap.get(userId);
                        if (vipVO == null) {
                            return;
                        }
                        Long memberId = vipVO.getMemberId();
                        List<Long> tmpMemberGroupIds = vipVO.getMemberGroupIds();
                        if (!CollectionUtils.isEmpty(memberIds) && memberIds.contains(memberId) || this.checkMemberGroup(memberGroupIds, tmpMemberGroupIds)) {
                            evseInfo.setIsVip(true);
                        }
                    });
                });
            });
        }
        log.info("OncePerRequestContext handleVip deliveryGroupList={}", JSON.toJSONString(deliveryGroupList));
    }
    //此处计算ALM可用群组上限
    private void handleAlm(OncePerRequestContext context) {
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        DeliveryGroupDTO rootDto = context.getRootDto();
        long now = System.currentTimeMillis();
        List<Long> meterIds=deliveryGroupList.stream().distinct().map(DeliveryGroupDTO::getMeterId).collect(Collectors.toList());
        //根据电表ID查询电表信息
        List<LocationMeterEntity> LocationMeters=locationMeterService.getByMeterIds(meterIds);
        //过滤掉brandEnum为0的电表数据，即Enegic品牌电表
        List<Long> eastronMeterIds=LocationMeters.stream().filter(v->v.getBrandEnum()==2).map(LocationMeterEntity::getId).collect(Collectors.toList());
        deliveryGroupList.stream().forEach(dto -> {
            Integer loadType = dto.getLoadType();
            Long meterId = dto.getMeterId();
            Integer meterLocation = dto.getMeterLocation();
            BigDecimal chargingUp = dto.getChargingUp();
            String unit = dto.getChargingUpUnit();
            DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
            groupDTO.setChargingUpUnit(unit);
            groupDTO.setMinReserve(dto.getMinReserve());
            groupDTO.setLoadType(loadType);
            groupDTO.setPowerEquipmentEnabled(dto.getPowerEquipmentEnabled());
            BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
            if (loadType == null || loadType != 1 || meterId == null) {
                return;
            }
            //道通电表处理逻辑
            Boolean powerEquipmentEnabled = dto.getPowerEquipmentEnabled();
            if (powerEquipmentEnabled != null && powerEquipmentEnabled) {
                this.handleAlmForAutel(context);
                return;
            }
            if (!CollectionUtils.isEmpty(eastronMeterIds)){
                if (eastronMeterIds.contains(dto.getMeterId())){
                    this.handleAlmForEastron(context);
                    return;
                }
            }
            MeterDataRecordVO last = meterDataClient.getLast(meterId.toString()).getData();
            if (last == null) {
                log.error("OncePerRequestContext,almStart,meterId={} last is null.", meterId);
                return;
            }
            Long updateTime = last.getUpdateTime();
            MeterDataRecordVO.Meter meter = last.getMeters().get(0);
            Map<String, BigDecimal> almDeliveryMap = null;
            //电表离线
            if (!"online".equals(last.getMeterStatus()) || (now - updateTime.longValue()) >= DlbUtil.getMeterOfflineOffset() || !meter.getOnline()) {
                almDeliveryMap = this.getAlmEvseSn(dto.getLevel(), minValue, context, dto.getId());
                log.info("OncePerRequestContext,almStart,meter is offline meterId={}", meterId);
            } else {
                BigDecimal meterValue = this.getByMeterId(meterId).max(BigDecimal.ZERO);
                //根据电表位置计算可用功率上限
                BigDecimal other = DlbUtil.getMeterValue(meter.getL1(), meter.getL2(), meter.getL3(), unit);
                String whiteKey = "energy:pile:base:alm:meter:white:future:" + meterId;
                String white = stringRedisTemplate.opsForValue().get(whiteKey);
                log.info("OncePerRequestContext,almStart,other={},meterLocation={},whiteKey={}", other, dto.getMeterLocation(), whiteKey);
                if (meterLocation == 1 && (StringUtils.isBlank(white))) {
                    other = other.subtract(meterValue);
                }
                BigDecimal future = meterDataClient.getFuture(meterId.toString()).getData();
                log.info("OncePerRequestContext,almStart,future={},meterValue={},other={},meter={}", future, meterValue, other, JSON.toJSONString(meter));
                //桩的可用功率上限
                BigDecimal calculate;
//                if (other.compareTo(BigDecimal.ZERO) < 0) {
//                    if (meterLocation == 0) {
//                        other = DlbUtil.getMeterValue(meter.getL1(), meter.getL2(), meter.getL3(), unit);
//                    }
//                }
                if (UnitEnum.CURRENT.getCode().equals(unit)) {
                    other = other.max(future);
                }
                //桩的可用功率上限计算结果
                calculate = chargingUp.subtract(other).min(chargingUp).min(rootDto.getChargingUp());
                if (meterLocation == 1) {
                    calculate = chargingUp.subtract(other);
                }
                if (calculate.compareTo(minValue) < 0) {
                    BigDecimal minTmp = BigDecimal.ZERO;
                    if (UnitEnum.POWER.getCode().equals(unit)) {
                        minTmp = BaseConstant.MIN_POWER_ZERO;
                    }
                    almDeliveryMap = this.getAlmEvseSn(dto.getLevel(), minTmp, context, dto.getId());
                    log.info("OncePerRequestContext calculate is smaller than minValue,calculate={},almDeliveryMap={},unit={}", calculate, JSON.toJSONString(almDeliveryMap), unit);
                } else {
                    dto.setChargingUp(calculate);
                }
                log.info("ALM计算结果为:充电单位:unit={},预估值：future={},上报值:meterValue={},其他负载:other={},电表位置:meterLocation={},白名单:white={},计算结果:calculate={},almDeliveryMap={}"
                        ,unit,future,meterValue,other,dto.getMeterLocation(),white,calculate,JSON.toJSONString(almDeliveryMap));            }
            if (!CollectionUtils.isEmpty(almDeliveryMap)) {
                context.addAll(almDeliveryMap);
            }
        });
    }

    private void handleAlmForEastron(OncePerRequestContext context) {
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        DeliveryGroupDTO rootDto = context.getRootDto();
        long now = System.currentTimeMillis();
        deliveryGroupList.stream().forEach(dto -> {
            Integer loadType = dto.getLoadType();
            Long meterId = dto.getMeterId();
            Integer meterLocation = dto.getMeterLocation();
            BigDecimal chargingUp = dto.getChargingUp();
            String unit = dto.getChargingUpUnit();
            DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
            groupDTO.setChargingUpUnit(unit);
            //电表离线情况下，都以8A或者4.2KW下发
            groupDTO.setMinReserve(1);
            groupDTO.setLoadType(loadType);
            groupDTO.setPowerEquipmentEnabled(dto.getPowerEquipmentEnabled());
            BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
            if (loadType == null || loadType != 1 || meterId == null) {
                return;
            }
            String meterIdStr=meterId.toString();
            String eastronMeterKey = eastronKey+meterIdStr;
            String eastronMeterInfo = stringRedisTemplate.opsForValue().get(eastronMeterKey);
            JSONObject eastronJsonObject = JSON.parseObject(eastronMeterInfo);
            log.info("OncePerRequestContext eastronMeterInfo={}", eastronJsonObject);
            if (StringUtils.isEmpty(eastronMeterInfo)) {
                log.error("OncePerRequestContext eastronMeterInfo  is null,meterId is {}.", meterId);
                return;
            }
            Long updateTime = (Long) eastronJsonObject.get("updateTime");
            // 取出 "current" 属性并转换为 List<Double>
            List<Double> currents = extractCurrentFromJson(eastronMeterInfo);
            Map<String, BigDecimal> almDeliveryMap = null;
            //电表离线
            if ((now - updateTime.longValue()) >= DlbUtil.getMeterOfflineOffset() ) {
                almDeliveryMap = this.getAlmEvseSn(dto.getLevel(), minValue, context, dto.getId());
                log.info("OncePerRequestContext,almStart,meter is offline meterId={}", meterId);
                sendMeterOfflineMessage(meterId, dto.getSellerId());
            } else {
                log.info("OncePerRequestContext,almStart,meter is online meterId={}", meterId);
                BigDecimal L1=BigDecimal.valueOf(currents.get(0));
                BigDecimal L2=BigDecimal.valueOf(currents.get(1));
                BigDecimal L3=BigDecimal.valueOf(currents.get(2));
                BigDecimal meterValue = this.getByMeterId(meterId).max(BigDecimal.ZERO);
                //根据电表位置计算可用功率上限
                BigDecimal other = DlbUtil.getMeterValue(L1,L2,L3, unit);
                String whiteKey = "energy:pile:base:alm:meter:white:future:" + meterId;
                String white = stringRedisTemplate.opsForValue().get(whiteKey);
                if (meterLocation == 1 && (StringUtils.isBlank(white))) {
                    other = other.subtract(meterValue);
                }
                BigDecimal future = meterDataClient.getFuture(meterId.toString()).getData();
                log.info("OncePerRequestContext,almStart,future={},meterValue={},other={},meterLocation={},white={},currents={}", future, meterValue, other, dto.getMeterLocation(),white,currents);
                //桩的可用功率上限
                BigDecimal calculate;
//                if (other.compareTo(BigDecimal.ZERO) < 0) {
//                    if (meterLocation == 0) {
//                        other = DlbUtil.getMeterValue(L1,L2,L3, unit);
//                    }
//                }
                if (UnitEnum.CURRENT.getCode().equals(unit)) {
                    other = other.max(future);
                }
                //桩的可用功率上限计算结果
                calculate = chargingUp.subtract(other).min(chargingUp).min(rootDto.getChargingUp());
                if (meterLocation == 1) {
                    calculate = chargingUp.subtract(other);
                }
                if (calculate.compareTo(minValue) < 0) {
                    BigDecimal minTmp = BigDecimal.ZERO;
                    if (UnitEnum.POWER.getCode().equals(unit)) {
                        minTmp = BaseConstant.MIN_POWER_ZERO;
                    }
                    almDeliveryMap = this.getAlmEvseSn(dto.getLevel(), minTmp, context, dto.getId());
                    log.info("OncePerRequestContext calculate is smaller than minValue,calculate={},almDeliveryMap={}",calculate,JSON.toJSONString(almDeliveryMap));
                } else {
                    dto.setChargingUp(calculate);
                }
                log.info("ALM计算结果为:充电单位:unit={},预估值：future={},上报值:meterValue={},其他负载:other={},电表位置:meterLocation={},白名单:white={},电流:currents={},计算结果:calculate={},almDeliveryMap={}"
                        ,unit,future,meterValue,other,dto.getMeterLocation(),white,currents,calculate,JSON.toJSONString(almDeliveryMap));
            }
            if (!CollectionUtils.isEmpty(almDeliveryMap)) {
                context.addAll(almDeliveryMap);
            }
        });
    }


    // 使用 Gson 解析 JSON 字符串并提取 current 数组，转换为 List<Double>
    public static List<Double> extractCurrentFromJson(String jsonString) {
        List<Double> currentList = new ArrayList<>();

        // 使用 Gson 解析 JSON 字符串
        Gson gson = new Gson();
        JsonObject jsonObject = gson.fromJson(jsonString, JsonObject.class);

        // 提取 meters 字符串，并解析为 JsonObject
        if (jsonObject.has("meters")) {
            String metersString = jsonObject.get("meters").getAsString();
            JsonObject metersObject = gson.fromJson(metersString, JsonObject.class);

            // 提取 current 数组
            if (metersObject.has("current")) {
                JsonArray currentArray = metersObject.getAsJsonArray("current");
                for (int i = 0; i < currentArray.size(); i++) {
                    currentList.add(currentArray.get(i).getAsDouble());
                }

                // 检查 List 长度是否小于3
                if (currentList.size() < 3) {
                    return new ArrayList<>(); // 返回空 List
                }
            }else{
                log.info("OncePerRequestContext extractCurrentFromJson no current jsonString={}",jsonString);
            }
        }else{
            log.info("OncePerRequestContext extractCurrentFromJson no meters jsonString={}",jsonString);
        }

        return currentList;
    }



    private void handleAlmForAutel(OncePerRequestContext context) {
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        if (CollectionUtils.isEmpty(deliveryGroupList)) {
            return;
        }
        long now = System.currentTimeMillis();
        Long offValue = DlbUtil.getMeterOfflineOffset(0);
        deliveryGroupList.stream().forEach(dto -> {
            Long meterId = dto.getMeterId();
            String unit = dto.getChargingUpUnit();
            BigDecimal meterVoltage = dto.getMeterVoltage();
            BigDecimal chargingUp = dto.getElectricUp();
            DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
            groupDTO.setChargingUpUnit(dto.getChargingUpUnit());
            groupDTO.setMinReserve(dto.getMinReserve());
            groupDTO.setLoadType(dto.getLoadType());
            groupDTO.setPowerEquipmentEnabled(dto.getPowerEquipmentEnabled());
            BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
            MeterDataRecordVO last = meterDataClient.getLast(meterId.toString()).getData();
            if (last == null) {
                log.error("OncePerRequestContext,almStart,meterId={} last is null.", meterId);
                return;
            }
            Long updateTime = last.getUpdateTime();
            MeterDataRecordVO.Meter meter = last.getMeters().get(0);
            Integer electricSource = meter.getElectricSource();
            Map<String, BigDecimal> almDeliveryMap = null;
            //电表离线
            if (now - updateTime.longValue() >= offValue.longValue()) {
                almDeliveryMap = this.getAlmEvseSn(dto.getLevel(), minValue, context, dto.getId());
                log.info("OncePerRequestContext,almStart,meter is offline meterId={},now={}", meterId, now);
            } else {
                BigDecimal meterValue = this.getByMeterId(meterId).max(BigDecimal.ZERO);
                BigDecimal other;
                BigDecimal temp;
                BigDecimal value1 = Optional.ofNullable(meter.getL1()).orElse(BigDecimal.ZERO).add(Optional.ofNullable(meter.getL2()).orElse(BigDecimal.ZERO)).add(Optional.ofNullable(meter.getL3()).orElse(BigDecimal.ZERO));
                BigDecimal value2 = Optional.ofNullable(meter.getL4()).orElse(BigDecimal.ZERO).add(Optional.ofNullable(meter.getL5()).orElse(BigDecimal.ZERO)).add(Optional.ofNullable(meter.getL6()).orElse(BigDecimal.ZERO));
                BigDecimal tmp = value1;
                if (value1.compareTo(value2) < 0) {
                    tmp = value2;
                    chargingUp = dto.getPowerEquipmentUp();
                }
                other = tmp.multiply(meterVoltage).divide(BigDecimal.valueOf(1000L));
                temp = other;
                other = other.subtract(meterValue);
                if (other.compareTo(BigDecimal.ZERO) <= 0) {
                    other = temp;
                }
                log.info("OncePerRequestContext,almStart,meterValue={},other={},meter={}", meterValue, other, JSON.toJSONString(meter));
                BigDecimal calculate = chargingUp.subtract(other).min(chargingUp);
                if (calculate.compareTo(minValue) < 0) {
                    BigDecimal minTmp = BigDecimal.ZERO;
                    if (UnitEnum.POWER.getCode().equals(unit)) {
                        minTmp = BigDecimal.valueOf(1.8);
                    }
                    almDeliveryMap = this.getAlmEvseSn(dto.getLevel(), minTmp, context, dto.getId());
                } else {
                    dto.setChargingUp(calculate);
                }
                log.info("OncePerRequestContext,almStart,calculate={},almDeliveryMap={}", calculate, JSON.toJSONString(almDeliveryMap));
            }
            if (!CollectionUtils.isEmpty(almDeliveryMap)) {
                context.addAll(almDeliveryMap);
            }
        });
    }

    private void handleTimeSetting(OncePerRequestContext context) {
        DeliveryGroupDTO rootDto = context.getRootDto();
        Integer timeSettingMode = rootDto.getTimeSettingMode();
        List<TimeSettingDetailDTO> timeSettingDetail = rootDto.getTimeSettingDetail();
        long now = System.currentTimeMillis();
        String zoneId = context.getZoneId();
        //根据根群组ID查询需求控制费信息
        String controllerContent = demandControlConfigService.findContentByGroupId(rootDto.getId());
        List<ControlContentDTO> controlContentDTOList = JSON.parseArray(controllerContent, ControlContentDTO.class);
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        List<Long> groupIds = deliveryGroupList.stream().map(DeliveryGroupDTO::getId).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(groupIds)){
            groupIds.remove(rootDto.getId());
        }
        //这里分时控制的逻辑是未被加入需求控制器，则走分时，如果加入了，则走需求控制器，所以要加上controlContentDTOList为空的判断
        if (timeSettingMode != null && timeSettingMode == 1 && !CollectionUtils.isEmpty(timeSettingDetail)) {
            rootDto.setChargingUp(this.getChargingUp(now, zoneId, null, rootDto.getTimeSettingDetail()));
        }

        if (!CollectionUtils.isEmpty(controlContentDTOList)){
            BigDecimal demandChargeUp=this.getChargingUpByDemand(now, zoneId, null, controlContentDTOList);
            if (demandChargeUp.compareTo(BigDecimal.ZERO) == 0){
                //如果需求控制器的充电功率为0，则刷新其下级群组
                groupIds.forEach(this::deliveryContext);
                context.setDeliveryFlag(false);
            }else{
                rootDto.setChargingUp(demandChargeUp);
                rootDto.setChargingUpType("Power");
                rootDto.setChargingUpUnit("KW");
            }
        }
        this.reduceSetChargingUp(deliveryGroupList, now, zoneId, rootDto.getChargingUp(), rootDto.getId(),controlContentDTOList);
    }

    private void reduceSetChargingUp(List<DeliveryGroupDTO> deliveryGroupList, long now, String zoneId, BigDecimal chargingUp, Long parentId,List<ControlContentDTO> controlContentDTOList) {
        if (!CollectionUtils.isEmpty(deliveryGroupList)) {
            for (DeliveryGroupDTO entity : deliveryGroupList) {
                if (entity.getPid().longValue() == parentId.longValue()) {
                    Integer timeSettingMode = entity.getTimeSettingMode();
                    if (timeSettingMode != null && timeSettingMode == 1 && !CollectionUtils.isEmpty(entity.getTimeSettingDetail())) {
                        entity.setChargingUp(this.getChargingUp(now, zoneId, chargingUp, entity.getTimeSettingDetail()));
                    }
                    if (!CollectionUtils.isEmpty(controlContentDTOList)) {
                        entity.setChargingUp(this.getChargingUpByDemand(now, zoneId, chargingUp, controlContentDTOList));
                        entity.setChargingUpType("Power");
                        entity.setChargingUpUnit("KW");
                    }
                    this.reduceSetChargingUp(deliveryGroupList, now, zoneId, entity.getChargingUp(), entity.getId(),controlContentDTOList);
                }
            }
        }
    }

    private List<MemberGroupVipVO> findVips(Set<Long> vipIds, Long sellerId) {
        MemberGroupVipDTO dto = new MemberGroupVipDTO();
        dto.setSellerId(sellerId);
        dto.setUserIds(new ArrayList<>(vipIds));
        Result<List<MemberGroupVipVO>> listResult = pileUserFeign.findList(dto);
        log.info("OncePerRequestContext,findVips={}",JSON.toJSONString(listResult));
        if (listResult != null && listResult.getCode().equals(HttpStatus.OK.value()) && !CollectionUtils.isEmpty(listResult.getData())) {
            return listResult.getData();
        }
        return null;
    }

    private boolean checkMemberGroup(List<Long> memberGroupIds, List<Long> tmpIds) {
        if (CollectionUtils.isEmpty(memberGroupIds) || CollectionUtils.isEmpty(tmpIds)) {
            return false;
        }
        for (Long memberGroupId : memberGroupIds) {
            if (tmpIds.contains(memberGroupId)) {
                return true;
            }
        }
        return false;
    }

    private List<DeliveryGroupTreeVO> reduceTree(List<DeliveryGroupDTO> deliveryGroupList, Long parentId, BigDecimal limit) {
        List<DeliveryGroupTreeVO> childrenList = new ArrayList<>();
        if (CollectionUtils.isEmpty(deliveryGroupList)) {
            return childrenList;
        }
        deliveryGroupList.stream().forEach(d -> {
            if (d.getPid().longValue() == parentId.longValue()) {
                DeliveryGroupTreeVO vo = oncePerRequestConvert.toTreeVo(d);
                BigDecimal chargingUp = vo.getChargingUp();
                vo.setChargingUp(chargingUp.min(limit));
                vo.setUsableChargingUp(vo.getChargingUp());
                vo.setChildrenList(this.reduceTree(deliveryGroupList, vo.getId(), vo.getChargingUp()));
                if (CollectionUtils.isEmpty(vo.getPileInfoList())) {
                    vo.setPileInfoList(new ArrayList<>());
                }
                BigDecimal total = BigDecimal.ZERO;
                Integer pileNumber = 0;
                //最小分配6A
                if (!CollectionUtils.isEmpty(vo.getPileInfoList())) {
                    total = total.add(vo.getPileInfoList().stream().map(DeliveryPileInfoTreeVO::getPileLimit).reduce((f, s) -> f.add(s)).get());
                    pileNumber += vo.getPileInfoList().size();
                }
                if (!CollectionUtils.isEmpty(vo.getChildrenList())) {
                    total = total.add(vo.getChildrenList().stream().map(DeliveryGroupTreeVO::getTotal).reduce((f, s) -> f.add(s)).get());
                    pileNumber += vo.getChildrenList().stream().map(DeliveryGroupTreeVO::getPileNumber).reduce((f, s) -> f + s).get();
                }
                vo.setTotal(total);
                vo.setPileNumber(pileNumber);
                childrenList.add(vo);
            }
        });
        return childrenList;
    }

    private Long getAttachTime(String evseSn) {
        Long attachTime = System.currentTimeMillis();
        String key = RedisKeyConstant.getChargePileGunKey(CommonUtil.getPileSn(evseSn), CommonUtil.getGunNo(evseSn).toString());
        String redisValue = stringRedisTemplate.opsForValue().get(key);
        if (StringUtils.isNotBlank(redisValue)) {
            TransactionInfoVO transactionInfoVO = JSON.parseObject(redisValue, TransactionInfoVO.class);
            String transactionId = transactionInfoVO.getTransactionId();
            Result<BillInfoVO> billByOrderSeqFromDB = billFeignClient.getBillByOrderSeqFromDB(transactionId);
            if (billByOrderSeqFromDB != null && billByOrderSeqFromDB.getCode() == 200 && billByOrderSeqFromDB.getData() != null) {
                BillInfoVO billInfoVO = billByOrderSeqFromDB.getData();
                attachTime = billInfoVO.getStartTime();
            }
        }
        return attachTime;
    }

    private List<OpEvseMeterUploadDTO> getEvseMeterList(List<String> evseSnList) {
        Result<List<OpEvseMeterUploadDTO>> listResult = monitorFeignClient.queryNewMeters(evseSnList);
        log.info("OncePerRequestContext,getEvseMeterList,listResult={}", JSON.toJSONString(listResult));
        if (listResult != null && listResult.getData() != null && listResult.getCode() == 200) {
            return listResult.getData();
        }
        return null;
    }

    private BigDecimal getSumPower(String evseSn) {
        Result<OrderVO> orderResult = iOrderServiceFeign.findLatestOrderByEvseSn(evseSn);
        if (orderResult != null && orderResult.getData() != null) {
            OrderVO orderVO = orderResult.getData();
            String orderSeq = orderVO.getOrderSeq();
            if (StringUtils.isNotBlank(orderSeq)) {
                String key = RedisKeyConstant.getStringChargingTariffComputeInfo(orderSeq);
                TariffComputeDTO dto = (TariffComputeDTO) redisTemplate.opsForValue().get(key);
                if (dto != null) {
                    BigDecimal sumPower = dto.getSumPower();
                    if (sumPower != null) {
                        return sumPower.divide(BigDecimal.valueOf(1000L));
                    }
                }
            }
        }
        return BigDecimal.ZERO;
    }


    private List<UserFeatureDTO> getUserList(List<Long> userIds) {
        Result<Map<Long, List<UserFeatureDTO>>> listResult = smartBiClient.queryUserChargeFeatureById(userIds);
        log.info("OncePerRequestContext,getUserMap,listResult={}", JSON.toJSONString(listResult));
        if (listResult != null && !CollectionUtils.isEmpty(listResult.getData()) && listResult.getCode() == 200) {
            return listResult.getData().values().stream().reduce(new ArrayList<>(), (f, s) -> {
                f.addAll(s);
                return f;
            });
        }
        return null;
    }

    private Map<String, BigDecimal> getAlmEvseSn(Integer level, BigDecimal delivery, OncePerRequestContext context, Long rootId) {
        Map<String, BigDecimal> resultMap = new HashMap<>();
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        //过滤当前群组及子群组
        List<DeliveryGroupDTO> children = deliveryGroupList.stream().filter(d -> d.getId().longValue() == rootId.longValue() || d.getLevel().intValue() > level.intValue()).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(children)) {
            children.stream().forEach(dto -> {
                List<DeliveryPileInfoDTO> pileInfoList = dto.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileDto -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileDto.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseDto -> {
                        resultMap.put(evseDto.getEvseSn(), delivery);
                    });
                });
            });
        }
        return resultMap;
    }

    private Long getUserId(String evseSn) {
        String pileSn = CommonUtil.getPileSn(evseSn);
        String connectorId = CommonUtil.getGunNo(evseSn).toString();
        String chargePileGunRedisKey = String.format(RedisKeyConstant.getChargePileGunKey(pileSn, connectorId));
        String chargePileGunRedisValue = stringRedisTemplate.opsForValue().get(chargePileGunRedisKey);
        TransactionInfoVO transactionInfoVo = JSON.parseObject(chargePileGunRedisValue, TransactionInfoVO.class);
        if (Objects.nonNull(transactionInfoVo)) {
            return parseLong(transactionInfoVo.getUserId());
        }
        log.error("OncePerRequestContext,getUserId,chargePileGunRedisValue={},transactionInfoVo is null.", chargePileGunRedisValue);
        return null;
    }

    private BigDecimal getChargingUpByDemand(long now, String zoneId, BigDecimal chargingUp,List<ControlContentDTO> controlContentDTOList) {
        LocalDate localDate = LocalDateTime.ofInstant(Instant.ofEpochMilli(now), ZoneId.of(zoneId)).toLocalDate();
        int month = localDate.getMonthValue();
        int season = convertMonthToSeason(month);
        int week =convertToWeek(localDate);
        controlContentDTOList = controlContentDTOList.stream().filter(c -> ((c.getSeason() == season||ObjectUtils.nullSafeEquals(c.getSeason(),0))
                && (c.getWeekDay() == week||ObjectUtils.nullSafeEquals(c.getWeekDay(),0))
                )).sorted(Comparator.comparing(ControlContentDTO::getStartTime)).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(controlContentDTOList)) {
            for (ControlContentDTO dto : controlContentDTOList) {
                long startTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getStartTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (dto.getEndTime().equals("24:00")) {
                    dto.setEndTime("23:59");
                }
                long endTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getEndTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (dto.getEndTime().equals("23:59")) {
                    endTime = endTime + TimeUnit.SECONDS.toMillis(60) - 1;
                }
                if (now >= startTime && now < endTime) {
                    return chargingUp != null ? dto.getPowerUp().min(chargingUp) : dto.getPowerUp();
                }
            }
        }
        return chargingUp!= null ? chargingUp : BigDecimal.ZERO;
    }

    private int convertToWeek(LocalDate localDate) {
        //工作日/周末  1：周末 2：工作日
        int day = localDate.getDayOfWeek().getValue();
        if (day == 6 || day == 7) {
            return 1;
        }
        return 2;
    }

    private int convertMonthToSeason(int month) {
        //1：夏季(6月-9月）、2：冬季(10月-5月)
        if (month >= 6 && month <= 9) {
            return 1;
        }
        return 2;
    }


    private BigDecimal getChargingUp(long now, String zoneId, BigDecimal chargingUp, List<TimeSettingDetailDTO> timeSettingDetailDTOS) {
        LocalDateTime localDateTime = LocalDateTime.now(ZoneId.of(zoneId));
        LocalDate localDate = localDateTime.toLocalDate();
        int day = localDateTime.getDayOfWeek().getValue();
        TimeSettingDetailDTO detailDTO = timeSettingDetailDTOS.stream().filter(d -> d.getDays().contains(day)).findFirst().get();
        if (detailDTO != null) {
            List<TimeSettingDetailDTO.DetailDTO> details = detailDTO.getDetails().stream().sorted(Comparator.comparing(TimeSettingDetailDTO.DetailDTO::getStartTime)).collect(Collectors.toList());
            for (TimeSettingDetailDTO.DetailDTO dto : details) {
                long startTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getStartTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                String et = dto.getEndTime();
                if (et.equals("24:00")) {
                    et = "23:59";
                }
                long endTime = LocalDateTime.of(localDate, LocalTime.parse(et)).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (et.equals("23:59")) {
                    endTime = endTime + TimeUnit.SECONDS.toMillis(60);
                }
                if (now >= startTime && now < endTime) {
                    return chargingUp != null ? dto.getChargingUp().min(chargingUp) : dto.getChargingUp();
                }
            }
        }
        return null;
    }

    private void reduceSetChargingUp(List<DeliveryGroupDTO> deliveryGroupList, long now, String zoneId, BigDecimal chargingUp, Long parentId) {
        if (!CollectionUtils.isEmpty(deliveryGroupList)) {
            for (DeliveryGroupDTO entity : deliveryGroupList) {
                if (entity.getPid().longValue() == parentId.longValue()) {
                    Integer timeSettingMode = entity.getTimeSettingMode();
                    if (timeSettingMode != null && timeSettingMode == 1 && !CollectionUtils.isEmpty(entity.getTimeSettingDetail())) {
                        entity.setChargingUp(this.getChargingUp(now, zoneId, chargingUp, entity.getTimeSettingDetail()));
                    }
                    this.reduceSetChargingUp(deliveryGroupList, now, zoneId, entity.getChargingUp(), entity.getId());
                }
            }
        }
    }

    @Override
    @Transactional
    public Boolean syncMeterDataFromOp(MeterSyncDataDTO dto) {
        Boolean flag = this.handleMeterData(dto);
        //根据电表sn查询电表是否存在，如果不存在则插入
        LocationMeterEntity locationMeterEntity = new LocationMeterEntity();
        locationMeterEntity.setSn(dto.getMeterSn());
        if (flag){
            if (locationMeterRepository.checkSnUnique(locationMeterEntity)) {
                Long id = IdWorker.getId();
                locationMeterEntity.setId(id);
                locationMeterEntity.setName(locationMeterService.generateDefaultMeterName(MeterbrandEnum.AUTEL.getBrandCode()));
                locationMeterEntity.setBrandEnum(MeterbrandEnum.AUTEL.getBrandCode());
                locationMeterEntity.setOperatorId(LoginUserUtil.getSellerId());
                boolean add = locationMeterRepository.add(locationMeterEntity);
                if (!add) {
                    throw new MessageCodeException(PileBaseEnum.FAILED_TO_ADD_METER);
                }
            }
        }
        return flag;
    }

    private Boolean handleMeterData(MeterSyncDataDTO dto) {
        log.info("syncMeterDataFromOp,dto={}", JSON.toJSONString(dto));
        Long sellerId = LoginUserUtil.getSellerId();
        List<String> sns= dto.getSns();
        if (CollectionUtils.isEmpty(sns)) {
            throw new MessageCodeException(PileBaseEnum.SLAVE_PILE_IS_EMPTY);
        }
        List<OpLocationPileEvseElasticDTO> opLocationPiles = this.opLocationPileEvseService.findList(sns).
                stream().filter(e -> e.getOperatorId() == sellerId.longValue()).collect(Collectors.toList());
        log.info("syncMeterDataFromOp,opLocationPilesList={}", JSON.toJSONString(opLocationPiles));
        if (opLocationPiles.size() < dto.getSns().size()) {
            return false;
        }
        //根据桩的sn查找所在群组并校验
        List<OpLocationPileGroupAssociateEntity> pileAssociateList = this.opLocationPileGroupAssociateRepository.findList(sns)
                .stream().filter(e ->!ObjectUtils.nullSafeEquals(e.getGroupId(),dto.getGroupId())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(pileAssociateList)) {
            log.info("syncMeterDataFromOp,pileAssociateList is not empty.");
            throw new MessageCodeException(PileBaseEnum.PILE_OR_METER_HAS_BEEN_BOUND);
        }
        //根据电表ID查询电表是否被关联
        List<OpLocationPileGroupEntity> meterAssociateList = opLocationPileGroupRepository.findAllByMeterIdList(Collections.singletonList(dto.getMeterId()))
                .stream().filter(e ->!ObjectUtils.nullSafeEquals(e.getId(),dto.getGroupId())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(meterAssociateList)) {
            log.info("syncMeterDataFromOp,meterAssociateList is not empty.");
            throw new MessageCodeException(PileBaseEnum.PILE_OR_METER_HAS_BEEN_BOUND);
        }

        return true;
    }

    /**
     * 发送电表离线的消息到飞书，同一个电表 id 每隔三分钟推送一次消息，在黑名单中的商家不发送告警消息
     * @param meterId 电表 id
     * @param sellerId 商家 id
     */
    private void sendMeterOfflineMessage(Long meterId, Long sellerId) {
        if (!CollectionUtils.isEmpty(blacklistSellIds) && blacklistSellIds.contains(String.valueOf(sellerId))) {
            log.info("sendMeterOfflineMessage 商家 id:{} 在黑名单中，不发送告警消息", sellerId);
            return;
        }
        log.info("sendMeterOfflineMessage 给商家 id:{} 发送电表 id:{} 的告警消息", sellerId, meterId);
        String key = METER_OFFLINE_EXPIRATION_TIME_KEY + meterId;
        if (Boolean.TRUE.equals(stringRedisTemplate.hasKey(key))) {
            log.info("sendMeterOfflineMessage 已经发送过告警消息，电表 id:{}", meterId);
            return;
        }
        // 缓存电表 id，三分钟后过期
        stringRedisTemplate.opsForValue().set(key, String.valueOf(Boolean.TRUE), 3, TimeUnit.MINUTES);
        String offlineAlarmMsg = String.format("商家id：%s，电表id: %s 已离线，请关注!", sellerId, meterId);
        log.info("sendMeterOfflineMessage 发送电表离线告警消息:{}", offlineAlarmMsg);
        Map<String, Object> resp = larkClient.sendMessage(offlineAlarmMsg, webhookKey);
        log.info("sendMeterOfflineMessage 发送告警消息的响应:{}", Objects.isNull(resp)? "返回为空":resp);
    }

    @Transactional
    @Override
    public boolean updateGroupType(GroupTypeDto dto) {
        LambdaUpdateWrapper<OpLocationPileGroupEntity> wp=new LambdaUpdateWrapper<>();
        wp.in(OpLocationPileGroupEntity::getId,dto.getIdList());
        wp.set(OpLocationPileGroupEntity::getGroupType,dto.getGroupType());
        return opLocationPileGroupRepository.update(wp);
    }
    @Override
    public List<PileGroupDetailV3VOcopy> queryGroupBySellerId(String sellerId) throws Exception{

        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class);
        queryWrapper.select(OpLocationPileGroupEntity::getId)
                .eq(OpLocationPileGroupEntity::getMerchantId,Long.valueOf(sellerId))
                .eq(OpLocationPileGroupEntity::getDeleted, 0)
                .eq(OpLocationPileGroupEntity::getPid, 0);
        List<Long> rootGroupIdList = opLocationPileGroupRepository.listObjs(queryWrapper, e -> (Long) e);

        List<PileGroupDetailV3VOcopy> result = new ArrayList<>();
        List<Future<PileGroupDetailV3VOcopy>> futureList=new ArrayList<>();
        for (Long rootGroupId : rootGroupIdList) {
            ////过滤demand标签和电表为energic的
            Future<PileGroupDetailV3VOcopy> submit = queryExecuor.submit(() -> {
                if (opLocationPileGroupRepository.jugeEdgeGroupId(rootGroupId)) {
                    return opLocationPileGroupService.detailV3(rootGroupId);
                }
                return null;
            });
            futureList.add(submit);
        }
        for (Future<PileGroupDetailV3VOcopy> f : futureList) {
            try {
                PileGroupDetailV3VOcopy t = f.get();
                if (t != null) {
                    result.add(t);
                }
            } catch (Exception e) {//暂跳过
                log.warn("queryGroupBySellerId查询错误",e);
            }
        }
        return result;
    }

    @Override
    public List<PileGroupDetailV3VOcopy> queryGroupByGroupIds(List<Long> groupIdList) throws Exception {
        List<PileGroupDetailV3VOcopy> result = new ArrayList<>();

        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper=new LambdaQueryWrapper<>();
        queryWrapper.select(OpLocationPileGroupEntity::getId);
        queryWrapper.in(OpLocationPileGroupEntity::getId,groupIdList);
        queryWrapper.eq(OpLocationPileGroupEntity::getDeleted,0);
        List<Long> ids= opLocationPileGroupRepository.listObjs(queryWrapper,ob->(Long)ob);
        log.info("queryGroupByGroupIds,reqGroupIds:{},realIds:{}",JSON.toJSONString(groupIdList),JSON.toJSONString(ids));

        List<Future<PileGroupDetailV3VOcopy>> futureList=new ArrayList<>(ids.size());
        for (Long groupId : ids) {
            futureList.add(queryExecuor.submit(() -> opLocationPileGroupService.detailV3(groupId)));
        }
        for (Future<PileGroupDetailV3VOcopy> f : futureList) {
            try {
                result.add(f.get());
            } catch (Exception e) {
                log.warn("queryGroupByGroupIds查询错误",e);
            }
        }
        return result;
    }

    /**
     * 边缘网关新增、修改、删除时  调用
     * @param groupInfo
     * @return
     */
    @Override
    public Result<Boolean> updateGroupInfo(UpdateGroupInfo groupInfo) {
        log.info("updateGroupInfo,req:{}",JSON.toJSONString(groupInfo));
        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper=new LambdaQueryWrapper<>();
        queryWrapper.in(OpLocationPileGroupEntity::getId,groupInfo.getGroupId());
        queryWrapper.eq(OpLocationPileGroupEntity::getGroupType,3);
        int emsCount = opLocationPileGroupRepository.count(queryWrapper);

        LambdaUpdateWrapper<OpLocationPileGroupEntity> wp=new LambdaUpdateWrapper<>();
        wp.in(OpLocationPileGroupEntity::getId,groupInfo.getGroupId());
        wp.set(OpLocationPileGroupEntity::getUpdatedAt,System.currentTimeMillis());
        switch (groupInfo.getUpdateType()){
            case -1://删除（群组与边缘网关解除关联）
                wp.eq(OpLocationPileGroupEntity::getGroupType,2);
                wp.set(OpLocationPileGroupEntity::getGroupType,0);
                break;
            case 0://新增（群组关联边缘网关）
            case 1://更新
                wp.and(t->t.isNull(OpLocationPileGroupEntity::getGroupType)
                        .or().notIn(OpLocationPileGroupEntity::getGroupType,1,3));//过滤EMS、demand群组
                wp.set(OpLocationPileGroupEntity::getGroupType,2);
                break;
            case 3://禁用暂不处理
                break;
            default:
                log.warn("update_type_Illegal,{}",groupInfo.getUpdateType());
                break;
        }
        opLocationPileGroupRepository.update(wp);//只更新边缘云标记
        if(emsCount==0){
            return Result.ofSucceed(true);
        }
        return sendToEms(groupInfo.getSn(),groupInfo.getGroupId());
    }

    /**
     * 更新群组配置时 调用 通知ems
     * @param groupId
     */
    private void notifyToEms(Long groupId){
        Map<Object, Object> entries = redisTemplate.opsForHash().entries(RedisConstant.REDIS_EMS_SN_GROUP_RELATION);
        if(CollectionUtils.isEmpty(entries)){
            log.info("REDIS_EMS_SN_GROUP_RELATION为空");
            return;
        }
        Map<String,List<Long>> gwsnGroupIdMap=new HashMap<>();
        for(Map.Entry<Object, Object> item :entries.entrySet()) {
            List<Long> groupIds = (List<Long>) item.getValue();
            if(!CollectionUtils.isEmpty(groupIds) && groupIds.contains(groupId)){

                LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper=new LambdaQueryWrapper<>();
                queryWrapper.select(OpLocationPileGroupEntity::getId);
                queryWrapper.in(OpLocationPileGroupEntity::getId,groupIds);
                queryWrapper.eq(OpLocationPileGroupEntity::getDeleted,0);
                queryWrapper.eq(OpLocationPileGroupEntity::getGroupType, 3);
                List<Long> emsGroupIds= opLocationPileGroupRepository.listObjs(queryWrapper,ob->(Long)ob);
                log.info("notifyToEms_gatewaysn:{},emsgroupId:{},allGroupId:{}",JSON.toJSONString(groupIds),JSON.toJSONString(emsGroupIds));
                if(!CollectionUtils.isEmpty(emsGroupIds)){
                    gwsnGroupIdMap.put(String.valueOf(item.getKey()),emsGroupIds);
                }
            }
        }
        if(gwsnGroupIdMap.isEmpty()){
            return ;
        }
        //与群组相关的所有网关都要重新通知下发给ems
        gwsnGroupIdMap.forEach((k,v)->{
            try {
                sendToEms(k,v);
            } catch (Exception e) {
                log.warn("gwSN:"+k+",ems_notify_exp:",e);
            }
        });

    }

    private Result<Boolean> sendToEms(String sn,List<Long> groupIdList){
        log.info("loadEmsGroupData,start");
        loadEmsGroupData();
        log.info("loadEmsGroupData,end");

        BasicDTO<PileGroupForEmsVOcopy> dto=new BasicDTO<>();
        dto.setSn(sn);
        dto.setTime(System.currentTimeMillis());
        dto.setMsgId(IdUtil.getSnowflakeNextIdStr());

        List<PileGroupForEmsVOcopy> collect = groupIdList.stream().map(gid -> {
            PileGroupDetailV3VOcopy pileGroupDetailV3VOcopy = detailV3(gid);
            PileGroupForEmsVOcopy emsVO = EmsGroupConvert.emsGroupConvert.dto2Entity(pileGroupDetailV3VOcopy);
            return emsVO;
        }).collect(Collectors.toList());
        dto.setData(collect);
        Result<Boolean> booleanResult = emsExchangeClient.exchangeChargeGroupInfo(dto);
        log.info("sendEms_resp:{}",booleanResult);
        return booleanResult;
    }

}