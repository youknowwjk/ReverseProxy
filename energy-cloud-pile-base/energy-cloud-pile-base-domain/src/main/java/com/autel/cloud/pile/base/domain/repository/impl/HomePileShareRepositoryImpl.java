package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.common.LocationCommon;
import com.autel.cloud.pile.base.domain.convert.DozerConvert;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.DistributeCostRuleService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.CategoryEnum;
import com.autel.cloud.pile.base.enums.ConnectorGunTypeEnum;
import com.autel.cloud.pile.base.enums.EVSETypeEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.OpsMgmtClient;
import com.autel.cloud.pile.base.infrastructure.feign.WxProxyClient;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.PileUsageDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.OpLimitFreeVO;
import com.autel.cloud.pile.base.vo.RandomDelayVO;
import com.autel.cloud.pile.base.vo.UserSharePileListVO;
import com.autel.cloud.pile.base.vo.app.HomeShareDetailVO;
import com.autel.cloud.pile.user.api.dto.SellerAddAppDTO;
import com.autel.cloud.pile.user.api.enums.PileUserEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelHeadVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import javax.validation.constraints.NotBlank;
import java.util.*;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;

@Log4j2
@Service
public class HomePileShareRepositoryImpl extends ServiceImpl<OpLocationMapper, OpLocationEntity> implements HomePileShareRepository {

    private static String HOME_SHARE = "home_share";

    @Autowired
    private OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    private OpImageMapper opImageMapper;
    @Autowired
    private OpLocationConnectorMapper opLocationConnectorMapper;
    @Autowired
    private BasicTariffModuleMapper basicTariffModuleMapper;
    @Autowired
    private OpLocationMapper opLocationMapper;
    @Autowired
    private RuleLocationPileMapper ruleLocationPileMapper;
    @Autowired
    private OpLocationPileEvseMapper opLocationPileEvseMapper;
    @Autowired
    private OpLocationOpenTimeMapper opLocationOpenTimeMapper;
    @Autowired
    private OpLocationImageMapper opLocationImageMapper;
    @Autowired
    private OpLocationOperationMapper opLocationOperationMapper;
    @Autowired
    private OpLimitFreeMapper opLimitFreeMapper;
    @Autowired
    private OpLocationElastic opLocationElastic;
    @Autowired
    private OpLocationOpenTimeRepository opLocationOpenTimeRepository;
    @Autowired
    private RuleRepository ruleRepository;
    @Autowired
    private RuleMapper ruleMapper;
    @Autowired
    private OpLocationImageRepository opLocationImageRepository;
    @Autowired
    private OpImageRepository opImageRepository;
    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;
    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;
    @Autowired
    private TariffFeignClient tariffFeignClient;
    @Autowired
    private DeviceServiceFeign deviceServiceFeign;
    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    private HomePileFeignClient homePileFeignClient;

    @Autowired
    private DistributeCostRuleService distributeCostRuleService;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private WxProxyClient wsCoreClient;
    @Resource
    private RabbitTemplate rabbitTemplate;

    @Autowired
    private LocationCommon locationCommon;

    @Resource
    private OpsMgmtClient opsMgmtClient;

    private static final String RANDOMDELAYTIME = "RandomDelayTime";

    /**
     * 新建家桩共享
     *
     * @param homeShareAddDTO 添加对象
     * @return 新建结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public OpLocationEntity add(HomeShareAddDTO homeShareAddDTO) {

        log.info("HomePileShareRepositoryImpl homeShareAddDTO setTheLatitudeAndLongitudeOfTheStation before ：{}", JSON.toJSONString(homeShareAddDTO));

        // todo 对站点的纬度和经度进行特殊处理 https://jira.autel.com/browse/ESAAS-17208
        // 站点纬度
        @NotBlank(message = "站点所在纬度不能为空") String latitude = homeShareAddDTO.getLatitude();
        // 站点经度
        @NotBlank(message = "站点所在经度不能为空") String longitude = homeShareAddDTO.getLongitude();

        // 重新设置纬度和经度
        Map<String, String> latitudeAndLongitudeMap = locationCommon.setTheLatitudeAndLongitudeOfTheStation(latitude, longitude, null);

        // 重新设置后的纬度和经度
        latitude = latitudeAndLongitudeMap.get("latitude");
        longitude = latitudeAndLongitudeMap.get("longitude");

        // 重新赋值
        homeShareAddDTO.setLatitude(latitude);
        homeShareAddDTO.setLongitude(longitude);

        log.info("HomePileShareRepositoryImpl homeShareAddDTO setTheLatitudeAndLongitudeOfTheStation after ：{}", JSON.toJSONString(homeShareAddDTO));

        Long userId = UserUtil.getUserId();
        log.info(BaseConstant.CURRENT_USER, userId);
        if (userId == null) {
            log.info("当前用户id为空");
            return null;
        }

        //获取当前用户的家桩列表，如果不存在，说明不是当前用户的家桩，不能进行家桩共享操作
        Result<Boolean> bindResult = homePileFeignClient.queryBindByUser(homeShareAddDTO.getPileSn(), userId);
        if (bindResult == null) {
            log.info("查询桩是否被当前用户绑定");
            return null;
        } else {
            if (bindResult.getData() == null || !bindResult.getData()) {
                log.info("SN：{} 不是用户:{}绑定的家桩", homeShareAddDTO.getPileSn(), userId);
                return null;
            }
        }

        Long sellerId = UserUtil.getUserId();
        long currentTimeMillis = System.currentTimeMillis();
        log.info("当前时间戳：{}", currentTimeMillis);
        OpLocationEntity opLocationEntity;
        //SN转成大写
        try {
            homeShareAddDTO.setPileSn(homeShareAddDTO.getPileSn().toUpperCase());
        } catch (Exception e) {
            log.info("Conversion failed");
        }

        //如果桩已经存在es，不能添加
        List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findByPileSnLike(homeShareAddDTO.getPileSn());
        if (CollectionUtils.isNotEmpty(esPileList)) {
            throw new MessageCodeException(PileBaseEnum.PILE_HAVE_SHARE);
        }

        List<OpLocationElasticDTO> esStationList = opLocationElastic.findAllByOperatorId(userId);
        log.info("当前用户id:{} 的场站: {}", userId, JSON.toJSONString(esStationList));
        if (CollectionUtils.isEmpty(esStationList)) {
            //1、保存op_location表
            opLocationEntity = new OpLocationEntity();
            opLocationEntity.setCreatedAt(currentTimeMillis);
            opLocationEntity.setUpdatedAt(currentTimeMillis);
            opLocationEntity.setDeleted(0);
            opLocationEntity.setStatus(EVSETypeEnum.SHARE_PILE.getCode());
            opLocationEntity.setType(HOME_SHARE);
            opLocationEntity.setName(homeShareAddDTO.getName());
            opLocationEntity.setCountry(homeShareAddDTO.getCountry());
            opLocationEntity.setProvince(homeShareAddDTO.getProvince());
            opLocationEntity.setCity(homeShareAddDTO.getCity());
            opLocationEntity.setAddress(homeShareAddDTO.getAddress());
            opLocationEntity.setPostalCode(homeShareAddDTO.getPostalCode());
            opLocationEntity.setLongitude(homeShareAddDTO.getLongitude());
            opLocationEntity.setLatitude(homeShareAddDTO.getLatitude());
            opLocationEntity.setOperatorId(sellerId);
            opLocationEntity.setSubOperatorId(sellerId);
            opLocationEntity.setOwnerId(sellerId);
            opLocationEntity.setTimeZone(StringUtils.isBlank(homeShareAddDTO.getTimeZone()) ? StringUtils.EMPTY : homeShareAddDTO.getTimeZone());
            opLocationEntity.setChargingWhenClosed(0);
            opLocationEntity.setZoneId(homeShareAddDTO.getZoneId());
            save(opLocationEntity);
            Long stationId = opLocationEntity.getId();

            //2.保存op_location_operation表
            OpLocationOperationEntity opLocationOperationEntity = new OpLocationOperationEntity();
            opLocationOperationEntity.setCreatedAt(currentTimeMillis);
            opLocationOperationEntity.setUpdatedAt(currentTimeMillis);
            opLocationOperationEntity.setDeleted(0);
            opLocationOperationEntity.setOperationType(HOME_SHARE);
            opLocationOperationEntity.setState(1);
            opLocationOperationEntity.setServiceTel(homeShareAddDTO.getServiceTel());
            opLocationOperationEntity.setAnnouncement(homeShareAddDTO.getDesc());
            opLocationOperationEntity.setAnnouncementAt(currentTimeMillis);
            opLocationOperationEntity.setOpenType(1);
            opLocationOperationEntity.setAppShow(true);
            opLocationOperationEntity.setLocationId(stationId);
            opLocationOperationEntity.setServiceTel(homeShareAddDTO.getServiceTel());
            opLocationOperationEntity.setAnnouncement(homeShareAddDTO.getDesc());
            opLocationOperationEntity.setGroupId(sellerId);
            opLocationOperationMapper.insert(opLocationOperationEntity);

            //3.保存图片
            if (CollectionUtils.isNotEmpty(homeShareAddDTO.getOpImageList())) {
                //删除场站原有的图片
                opLocationImageMapper.deleteByLocationId(opLocationEntity.getId());
                //保存图片
                List<OpImageEntity> opImageEntities = homeShareAddDTO.getOpImageList().stream().map(opImageDTO -> {
                    OpImageEntity opImageEntity = DozerConvert.map(opImageDTO, OpImageEntity.class);
                    opImageEntity.setCreatedAt(currentTimeMillis);
                    opImageEntity.setUpdatedAt(currentTimeMillis);
                    return opImageEntity;
                }).collect(Collectors.toList());
                opImageRepository.saveBatch(opImageEntities);
                //保存场站图片
                List<OpLocationImageEntity> opLocationImageEntities = opImageEntities.stream().map(e -> {
                    OpLocationImageEntity opLocationImageEntity = new OpLocationImageEntity();
                    opLocationImageEntity.setImageId(e.getId());
                    opLocationImageEntity.setLocationId(stationId);
                    opLocationImageEntity.setDeleted(0);
                    return opLocationImageEntity;
                }).collect(Collectors.toList());
                opLocationImageRepository.saveBatch(opLocationImageEntities);
            }

            //4.保存营业时间表
            if (CollectionUtils.isNotEmpty(homeShareAddDTO.getOpenTimeList())) {
                List<OpLocationOpenTimeEntity> opLocationOpenTimeEntities = homeShareAddDTO.getOpenTimeList().stream().map(openTimeDTO -> {
                    OpLocationOpenTimeEntity opLocationOpenTimeEntity = new OpLocationOpenTimeEntity();
                    opLocationOpenTimeEntity.setCreatedAt(currentTimeMillis);
                    opLocationOpenTimeEntity.setUpdatedAt(currentTimeMillis);
                    opLocationOpenTimeEntity.setDeleted(0);
                    opLocationOpenTimeEntity.setStatus(0);
                    opLocationOpenTimeEntity.setLocationId(stationId);
                    opLocationOpenTimeEntity.setDateValue(JSON.toJSONString(openTimeDTO.getDateValue()));
                    opLocationOpenTimeEntity.setDateType("daily");
                    opLocationOpenTimeEntity.setType(1);
                    opLocationOpenTimeEntity.setPeriodBegin(openTimeDTO.getPeriodBegin());
                    opLocationOpenTimeEntity.setPeriodEnd(openTimeDTO.getPeriodEnd());
                    return opLocationOpenTimeEntity;
                }).collect(Collectors.toList());
                opLocationOpenTimeRepository.saveBatch(opLocationOpenTimeEntities);

                //创建进场控制规则
                RuleDTO ruleDTO = buildRuleDTO(currentTimeMillis, homeShareAddDTO.getOpenTimeList());
                Result<String> addRuleResult = ruleRepository.addRule(ruleDTO);
                if (addRuleResult == null || addRuleResult.getData() == null) {
                    throw new MessageCodeException(PileBaseEnum.ADD_ENTER_RULE_FAILURE);
                }
            }

        } else {
            Long stationId = esStationList.get(0).getId();
            opLocationEntity = opLocationMapper.selectById(stationId);
            log.info("数据库场站实体: {}", JSON.toJSONString(opLocationEntity));
            if (opLocationEntity == null) {
                throw new MessageCodeException(PileBaseEnum.CANNOT_FIND_STATION_WITH_ID_IN_DATABASE);
            }
        }

        //初始化商家
        try {
            SellerAddAppDTO sellerAddAppDTO = new SellerAddAppDTO();
            sellerAddAppDTO.setUserId(UserUtil.getUserId());
            Result<Boolean> initSellerResult = pileUserFeign.saveForApp(null, sellerAddAppDTO);
            log.info("初始化商家结果:{}", initSellerResult);
        } catch (Exception e) {
            log.info("初始化商家失败：" + e);
        }

        //5.远程调用device更新设备类型
        PileUsageDTO pileUsageDTO = new PileUsageDTO();
        pileUsageDTO.setUsage(EVSETypeEnum.SHARE_PILE.getCode());
        Result<Boolean> updateDeviceUsageResult = deviceServiceFeign.updateUsage(pileUsageDTO, homeShareAddDTO.getPileSn());
        if (updateDeviceUsageResult == null || !updateDeviceUsageResult.getData()) {
            throw new MessageCodeException(PileBaseEnum.UPDATE_DEVICE_USE_SCENARIO_FAILURE);
        }
        //6.远程调用创建计费规则
        if (homeShareAddDTO.getOpLocationBasicTariffAddFirstDTO() == null) {
            throw new MessageCodeException(PileBaseEnum.WITHOUT_FILL_TARIFF_INFO);
        }
        Long tariffId;
        try {
            com.autel.cloud.tariff.dto.OpLocationBasicTariffAddFirstDTO opLocationBasicTariffAddFirstDTO = DozerConvert.map(homeShareAddDTO.getOpLocationBasicTariffAddFirstDTO(), com.autel.cloud.tariff.dto.OpLocationBasicTariffAddFirstDTO.class);
            Result<CostModelHeadVO> costModelHeadVOResult = tariffFeignClient.addOpLocationBasicTariff(opLocationBasicTariffAddFirstDTO);
            log.info("远程调用创建计费规则结果：{}", JSON.toJSONString(costModelHeadVOResult));
            tariffId = costModelHeadVOResult.getData().getId();
        } catch (Exception e) {
            log.info("添加计费和下发计费信息异常: " + e);
            throw new MessageCodeException(PileBaseEnum.ADD_TARIFF_FAILURE);
        }
        try {
            log.info("=========== 家桩共享新增计费规则,下发begin, tariffId: {}, pileSn: {}, timezone:{} ========================", tariffId, homeShareAddDTO.getPileSn(), homeShareAddDTO.getTimeZone());
            distributeCostRuleService.dispatchTariffOfPublicPile(tariffId, homeShareAddDTO.getPileSn(), homeShareAddDTO.getTimeZone());
        } catch (Exception e) {
            log.info("家桩共享新增计费规则,下发计费失败:" + e);
        }

        //7.保存设备
        OpLocationEvseDTO opLocationEvseDTO = buildCreateEVSEDTO(homeShareAddDTO, opLocationEntity.getId());
        log.info("创建设备的参数--opLocationEvseDTO:{}", JSON.toJSONString(opLocationEvseDTO));
        log.info("场站实体对象:{}", JSON.toJSONString(opLocationEntity));
        opLocationEvseRepository.createEvse(opLocationEvseDTO, opLocationEntity);
        try {
            stringRedisTemplate.delete(RedisKeyConstant.getStringAddPileException(userId));
        } catch (Exception e) {
            log.info("添加桩删除es异常,e={}",e);
        }
        OpLocationPileEvseElasticDTO esPile = opLocationPileEvseElastic.findByPileSn(homeShareAddDTO.getPileSn());
        log.info("保存设备成功");


        try {
            //8.更新桩枪计费规则
            log.info("更新桩枪计费规则开始");
            if (homeShareAddDTO.getOpLocationBasicTariffAddFirstDTO() != null && esPile != null) {
                //es桩基础版计费信息和计费规则id
                OpLocationBasicTariffAddFirstDTO opLocationBasicTariffAddFirstDTO = homeShareAddDTO.getOpLocationBasicTariffAddFirstDTO();
                String opLocationBasicTariffAddFirstDTOStr = JSON.toJSONString(opLocationBasicTariffAddFirstDTO);
                log.info("添加家桩共享计费规则数据:{}", opLocationBasicTariffAddFirstDTOStr);
                esPile.setCostRule(opLocationBasicTariffAddFirstDTOStr);
                esPile.setTariffId(tariffId);
                opLocationPileEvseElastic.save(esPile);
                //枪计费规则id
                log.info("枪计费规则,根据SN集合找枪");
                List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByPileSn(homeShareAddDTO.getPileSn());
                log.info("枪计费规则,根据SN集合找枪结果:{}", JSON.toJSONString(esEVSEList));
                if (CollectionUtils.isNotEmpty(esEVSEList)) {
                    //es枪计费规则id
                    OpLocationEvseElasticDTO esEVSE = esEVSEList.get(0);
                    esEVSE.setTariffId(tariffId);
                    opLocationEvseElastic.save(esEVSE);
                    //数据库枪计费规则id
                    OpLocationEvseEntity opLocationEvseEntity = opLocationEvseMapper.selectById(esEVSE.getId());
                    if (opLocationEvseEntity != null) {
                        opLocationEvseEntity.setTariffId(tariffId);
                        opLocationEvseMapper.updateById(opLocationEvseEntity);
                    }
                }
            }
            log.info("更新桩枪计费规则结束");

            //9、更新这个桩es的status字段和关联计费规则
            log.info("更新桩类型和关联计费规则");
            LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                    .eq(OpLocationPileEvseEntity::getPileSn, homeShareAddDTO.getPileSn())
                    .eq(OpLocationPileEvseEntity::getDeleted, 0);
            List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
            if (CollectionUtils.isNotEmpty(opLocationPileEvseEntities)) {
                //桩类型 3-家桩共享
                OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseEntities.get(0);
                opLocationPileEvseEntity.setStatus(EVSETypeEnum.SHARE_PILE.getCode());
                opLocationPileEvseMapper.updateById(opLocationPileEvseEntity);

                Optional<OpLocationPileEvseElasticDTO> optionalOpLocationPileEvseElasticDTO = opLocationPileEvseElastic.findById(opLocationPileEvseEntity.getId());
                if (optionalOpLocationPileEvseElasticDTO.isPresent()) {
                    OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = optionalOpLocationPileEvseElasticDTO.get();
                    opLocationPileEvseElasticDTO.setStatus(EVSETypeEnum.SHARE_PILE.getCode());
                    opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
                }
            }
            log.info("更新桩类型和关联计费规则完成");

            //10、给该场站的所有设备关联同一进场规则
            log.info("给该场站的所有设备关联同一进场规则");
            esPileList = opLocationPileEvseElastic.findAllByLocationId(opLocationEntity.getId());
            if (CollectionUtils.isNotEmpty(esPileList) && CollectionUtils.isNotEmpty(homeShareAddDTO.getOpenTimeList())) {
                LambdaUpdateWrapper<RuleEntity> ruleEntityLambdaUpdateWrapper = Wrappers.lambdaUpdate(RuleEntity.class).eq(RuleEntity::getName, currentTimeMillis);
                List<RuleEntity> ruleEntities = ruleMapper.selectList(ruleEntityLambdaUpdateWrapper);
                log.info("根据当前时间戳：{} 查询出进场控制规则:{}", currentTimeMillis, JSON.toJSONString(ruleEntities));
                //创建了计费规则（第一次共享）
                if (CollectionUtils.isNotEmpty(ruleEntities)) {
                    log.info("场站id:{} 的桩集合{}", opLocationEntity.getId(), JSON.toJSONString(esPileList));
                    RuleEntity ruleEntity = ruleEntities.get(0);
                    RelatePileDTO relatePileDTO = new RelatePileDTO();
                    relatePileDTO.setRuleId(ruleEntity.getId());
                    relatePileDTO.setRuleName(ruleEntity.getName());
                    for (OpLocationPileEvseElasticDTO esPile2 : esPileList) {
                        List<PileDTO> pileDTOList = Lists.newArrayList();
                        PileDTO pileDTO = new PileDTO();
                        pileDTO.setLocationId(esPile2.getLocationId());
                        pileDTO.setPileId(esPile2.getId());
                        pileDTOList.add(pileDTO);
                        relatePileDTO.setPiles(pileDTOList);
                        ruleRepository.relatePile(relatePileDTO);
                    }
                } else {
                    //没有创建计费规则
                    Long ruleId = null;
                    for (OpLocationPileEvseElasticDTO esPile2 : esPileList) {
                        if (esPile2.getRuleId() != null) {
                            ruleId = esPile2.getRuleId();
                        }
                    }
                    for (OpLocationPileEvseElasticDTO esPile2 : esPileList) {
                        esPile2.setRuleId(ruleId);
                        opLocationPileEvseElastic.save(esPile2);
                    }
                }
            }
            log.info("给该场站的所有设备关联同一进场规则完成");
        } catch (Exception e) {
            log.info("更新8、9/10 桩枪计费规则、进场控制规则失败:" + e);
            //删除es桩
            if (esPile != null) {
                log.info("esPile:{}", JSON.toJSONString(esPile));
                opLocationPileEvseElastic.deleteById(esPile.getId());
            }
            //删除es枪
            List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByPileSn(homeShareAddDTO.getPileSn());
            if (CollectionUtils.isNotEmpty(esEVSEList)) {
                log.info("es枪:{}", JSON.toJSONString(esEVSEList));
                opLocationEvseElastic.deleteAll(esEVSEList);
            }
            throw new MessageCodeException(PileBaseEnum.ADD_TARIFF_FAILURE);
        }

        //11.保存es场站
        OpLocationElasticDTO opLocationElasticDTO = DozerConvert.map(homeShareAddDTO, OpLocationElasticDTO.class);
        opLocationElasticDTO.setId(opLocationEntity.getId());
        opLocationElasticDTO.setCreatedAt(currentTimeMillis);
        opLocationElasticDTO.setUpdatedAt(currentTimeMillis);
        opLocationElasticDTO.setStatus(EVSETypeEnum.SHARE_PILE.getCode());
        opLocationElasticDTO.setType(HOME_SHARE);
        opLocationElasticDTO.setOperatorId(UserUtil.getUserId());
        opLocationElasticDTO.setSubOperatorId(UserUtil.getUserId());
        opLocationElasticDTO.setOwnerId(UserUtil.getUserId());
        opLocationElasticDTO.setChargingWhenClosed(0);
        opLocationElasticDTO.setOpenType(0);
        opLocationElasticDTO.setOperationType(HOME_SHARE);
        opLocationElasticDTO.setState(0);
        opLocationElasticDTO.setAppShow(true);
        opLocationElasticDTO.setBillingRule(JSON.toJSONString(homeShareAddDTO.getOpLocationBasicTariffAddFirstDTO()));
        opLocationElasticDTO.setName(homeShareAddDTO.getName());
        opLocationElasticDTO.setCountry(homeShareAddDTO.getCountry());
        opLocationElasticDTO.setAddress(homeShareAddDTO.getAddress());
        opLocationElasticDTO.setCity(homeShareAddDTO.getCity());
        opLocationElasticDTO.setPostalCode(homeShareAddDTO.getPostalCode());
        opLocationElasticDTO.setCountry(homeShareAddDTO.getCountry());
        opLocationElasticDTO.setProvince(homeShareAddDTO.getProvince());
        opLocationElasticDTO.setLatitude(homeShareAddDTO.getLatitude());
        opLocationElasticDTO.setLongitude(homeShareAddDTO.getLongitude());
        opLocationElasticDTO.setTimeZone(homeShareAddDTO.getTimeZone());
        opLocationElasticDTO.setZoneId(homeShareAddDTO.getZoneId());
        opLocationElasticDTO.setServiceTel(homeShareAddDTO.getServiceTel());
        opLocationElasticDTO.setAnnouncement(homeShareAddDTO.getDesc());
        opLocationElasticDTO.setPersonalSurName(homeShareAddDTO.getPersonalSurName());
        opLocationElasticDTO.setPersonalName(homeShareAddDTO.getPersonalName());
        GeoPoint location = new GeoPoint(Double.parseDouble(homeShareAddDTO.getLatitude()), Double.parseDouble(homeShareAddDTO.getLongitude()));
        opLocationElasticDTO.setLocation(location);
        opLocationElasticDTO.setPlatform(1);
        opLocationElastic.save(opLocationElasticDTO);
        //新增场站发送MQ
        log.info("send to mq locationId :{}", opLocationElasticDTO.getId());
        rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_ADD_EXPAND_EXCHANGE+ RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_EXPAND_ROUTE,JSON.toJSONString(opLocationElasticDTO.getId()));

        /*try{
            sendRefreshPileConfigMsg(homeShareAddDTO.getPileSn(), UserUtil.getUserId().toString());
            log.info("add sendRefreshPileConfigMsg pileSN :{}", homeShareAddDTO.getPileSn());
        }catch (Exception e){
            log.error("add sendRefreshPileConfigMsg 异常",e);
        }*/
        return opLocationEntity;
    }

    /**
     * 构建创建进场控制的添加参数
     *
     * @param openTimeList 营业时间集合
     * @return 创建进场控制的添加参数
     */
    private RuleDTO buildRuleDTO(long currentTimeMillis, List<OpLocationOpenTimeDTO> openTimeList) {
        RuleDTO ruleDTO = new RuleDTO();
        ruleDTO.setUserId(UserUtil.getUserId());
        ruleDTO.setSellerId(UserUtil.getUserId());
        ruleDTO.setName(String.valueOf(currentTimeMillis));
        List<RuleDetailDTO> ruleDetails = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(openTimeList)) {
            //7*24h营业
            if (openTimeList.size() == 1 && Objects.equals(openTimeList.get(0).getDateValue().get(0), "0")) {
                OpLocationOpenTimeDTO opLocationOpenTimeDTO = openTimeList.get(0);
                RuleDetailDTO ruleDetailDTO = new RuleDetailDTO();
                ruleDetailDTO.setAction(0);
                ruleDetailDTO.setDays(Lists.newArrayList(1, 2, 3, 4, 5, 6, 7));
                ruleDetailDTO.setStartTime(opLocationOpenTimeDTO.getPeriodBegin());
                log.info("营业结束时间：{}  营业结束时间等于24点:{}", opLocationOpenTimeDTO.getPeriodEnd(), Objects.equals(opLocationOpenTimeDTO.getPeriodEnd(), BaseConstant.TWENTY_FOUR));
                ruleDetailDTO.setEndTime(Objects.equals(opLocationOpenTimeDTO.getPeriodEnd(), BaseConstant.TWENTY_FOUR) ? "23:59" : opLocationOpenTimeDTO.getPeriodEnd());
                ruleDetailDTO.setMemberGroupId(Lists.newArrayList(-1L));
                ruleDetails.add(ruleDetailDTO);
            } else {
                for (OpLocationOpenTimeDTO opLocationOpenTimeDTO : openTimeList) {
                    RuleDetailDTO ruleDetailDTO = new RuleDetailDTO();
                    ruleDetailDTO.setAction(0);
                    List<String> dataValueList = opLocationOpenTimeDTO.getDateValue();
                    List<Integer> dataValue = new ArrayList<>();
                    for (String s : dataValueList) {
                        dataValue.add(Integer.parseInt(s));
                    }
                    ruleDetailDTO.setDays(dataValue);
                    ruleDetailDTO.setStartTime(opLocationOpenTimeDTO.getPeriodBegin());
                    log.info("营业结束时间：{}  营业结束时间等于24点:{}", opLocationOpenTimeDTO.getPeriodEnd(), Objects.equals(opLocationOpenTimeDTO.getPeriodEnd(), BaseConstant.TWENTY_FOUR));
                    ruleDetailDTO.setEndTime(Objects.equals(opLocationOpenTimeDTO.getPeriodEnd(), BaseConstant.TWENTY_FOUR) ? "23:59" : opLocationOpenTimeDTO.getPeriodEnd());
                    ruleDetailDTO.setMemberGroupId(Lists.newArrayList(-1L));
                    ruleDetails.add(ruleDetailDTO);
                }
            }
        }
        ruleDTO.setDetails(ruleDetails);
        return ruleDTO;
    }

    /**
     * 编辑家桩共享桩信息
     *
     * @param homeShareUpdateDTO 桩详情
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public OpLocationEntity update(HomeShareUpdateDTO homeShareUpdateDTO) {
        return null;
    }

    /**
     * 编辑地址信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateAddress(HomeShareUpdateDTO homeShareUpdateDTO) {

        log.info("HomePileShareRepositoryImpl homeShareUpdateDTO setTheLatitudeAndLongitudeOfTheStation before ：{}", JSON.toJSONString(homeShareUpdateDTO));

        Long userId = UserUtil.getUserId();
        Long currentTimeMillis = System.currentTimeMillis();
        OpLocationEntity opLocationEntity = null;
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findAllByOperatorId(userId);
        log.info(BaseConstant.CURRENT_USER_AND_LOCATION, userId, JSON.toJSONString(esStationList));
        if (CollectionUtils.isNotEmpty(esStationList)) {

            // todo 场站id(op_location表主键id)?
            Long stationId = esStationList.get(0).getId();

            // todo 对站点的纬度和经度进行特殊处理 https://jira.autel.com/browse/ESAAS-17208
            // 站点纬度
            @NotBlank(message = "站点所在纬度不能为空") String latitude = homeShareUpdateDTO.getLatitude();
            // 站点经度
            @NotBlank(message = "站点所在经度不能为空") String longitude = homeShareUpdateDTO.getLongitude();

            // 重新设置纬度和经度
            Map<String, String> latitudeAndLongitudeMap = locationCommon.setTheLatitudeAndLongitudeOfTheStation(latitude, longitude, stationId);

            // 重新设置后的纬度和经度
            latitude = latitudeAndLongitudeMap.get("latitude");
            longitude = latitudeAndLongitudeMap.get("longitude");

            // 重新赋值
            homeShareUpdateDTO.setLatitude(latitude);
            homeShareUpdateDTO.setLongitude(longitude);

            log.info("HomePileShareRepositoryImpl homeShareUpdateDTO setTheLatitudeAndLongitudeOfTheStation after ：{}", JSON.toJSONString(homeShareUpdateDTO));

            //判断ES中是否存有该数据
            //1、更新op_location表
            opLocationEntity = opLocationMapper.selectById(stationId);
            if (opLocationEntity != null) {
                opLocationEntity.setUpdatedAt(currentTimeMillis);
                opLocationEntity.setName(homeShareUpdateDTO.getName());
                opLocationEntity.setCountry(homeShareUpdateDTO.getCountry());
                opLocationEntity.setProvince(homeShareUpdateDTO.getProvince());
                opLocationEntity.setCity(homeShareUpdateDTO.getCity());
                opLocationEntity.setAddress(homeShareUpdateDTO.getAddress());
                opLocationEntity.setPostalCode(homeShareUpdateDTO.getPostalCode());
                opLocationEntity.setLongitude(homeShareUpdateDTO.getLongitude());
                opLocationEntity.setLatitude(homeShareUpdateDTO.getLatitude());
                opLocationEntity.setZoneId(homeShareUpdateDTO.getZoneId());
                opLocationEntity.setTimeZone(homeShareUpdateDTO.getTimeZone());
                updateById(opLocationEntity);
            }

            //2、更新op_location_operation表
            OpLocationOperationEntity opLocationOperationEntity = null;
            LambdaQueryWrapper<OpLocationOperationEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationOperationEntity.class).eq(OpLocationOperationEntity::getLocationId, stationId);
            List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(lambdaQueryWrapper);
            if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
                opLocationOperationEntity = opLocationOperationEntities.get(0);
                opLocationOperationEntity.setUpdatedAt(currentTimeMillis);
                opLocationOperationEntity.setAnnouncement(homeShareUpdateDTO.getDesc());
                opLocationOperationEntity.setAnnouncementAt(currentTimeMillis);
                opLocationOperationMapper.updateById(opLocationOperationEntity);
            }

            //3、更新营业时间表
            //删除原有的
            opLocationOpenTimeMapper.deleteByLocationId(stationId);
            if (CollectionUtils.isNotEmpty(homeShareUpdateDTO.getOpenTimeList())) {
                //保存新的
                List<OpLocationOpenTimeEntity> opLocationOpenTimeEntities = homeShareUpdateDTO.getOpenTimeList().stream().map(openTimeDTO -> {
                    OpLocationOpenTimeEntity opLocationOpenTimeEntity = new OpLocationOpenTimeEntity();
                    opLocationOpenTimeEntity.setCreatedAt(currentTimeMillis);
                    opLocationOpenTimeEntity.setUpdatedAt(currentTimeMillis);
                    opLocationOpenTimeEntity.setDeleted(0);
                    opLocationOpenTimeEntity.setStatus(0);
                    opLocationOpenTimeEntity.setLocationId(stationId);
                    opLocationOpenTimeEntity.setDateValue(JSON.toJSONString(openTimeDTO.getDateValue()));
                    opLocationOpenTimeEntity.setDateType("daily");
                    opLocationOpenTimeEntity.setType(1);
                    opLocationOpenTimeEntity.setPeriodBegin(openTimeDTO.getPeriodBegin());
                    opLocationOpenTimeEntity.setPeriodEnd(openTimeDTO.getPeriodEnd());
                    return opLocationOpenTimeEntity;
                }).collect(Collectors.toList());
                opLocationOpenTimeRepository.saveBatch(opLocationOpenTimeEntities);

                //创建进场控制规则
                RuleDTO ruleDTO = buildRuleDTO(currentTimeMillis, homeShareUpdateDTO.getOpenTimeList());
                Result<String> addRuleResult = ruleRepository.addRule(ruleDTO);
                if (addRuleResult == null || addRuleResult.getData() == null) {
                    throw new MessageCodeException(PileBaseEnum.ADD_ENTER_RULE_FAILURE);
                }
            }

            //4.更新图片信息
            //删除场站原有的图片
            opLocationImageMapper.deleteByLocationId(stationId);
            if (CollectionUtils.isNotEmpty(homeShareUpdateDTO.getOpImageList())) {
                //保存图片
                List<OpImageEntity> opImageEntities = homeShareUpdateDTO.getOpImageList().stream().map(opImageDTO -> {
                    OpImageEntity opImageEntity = DozerConvert.map(opImageDTO, OpImageEntity.class);
                    opImageEntity.setCreatedAt(currentTimeMillis);
                    opImageEntity.setUpdatedAt(currentTimeMillis);
                    return opImageEntity;
                }).collect(Collectors.toList());
                opImageRepository.saveBatch(opImageEntities);
                //保存场站图片
                List<OpLocationImageEntity> opLocationImageEntities = opImageEntities.stream().map(e -> {
                    OpLocationImageEntity opLocationImageEntity = new OpLocationImageEntity();
                    opLocationImageEntity.setImageId(e.getId());
                    opLocationImageEntity.setLocationId(stationId);
                    opLocationImageEntity.setDeleted(0);
                    return opLocationImageEntity;
                }).collect(Collectors.toList());
                opLocationImageRepository.saveBatch(opLocationImageEntities);
            }

            //给场站设备关联进场规则
            //数据库的绑定先清空
            LambdaQueryWrapper<RuleLocationPileEntity> ruleLocationPileEntityLambdaQueryWrapper = Wrappers.lambdaQuery(RuleLocationPileEntity.class).eq(RuleLocationPileEntity::getLocationId, stationId);
            ruleLocationPileMapper.delete(ruleLocationPileEntityLambdaQueryWrapper);
            //es桩的进场规则id置为空
            List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findAllByLocationId(stationId);
            if (CollectionUtils.isNotEmpty(esPileList)) {
                esPileList.forEach(esPile -> esPile.setRuleId(null));
            }
            //绑定
            if (CollectionUtils.isNotEmpty(homeShareUpdateDTO.getOpenTimeList()) && CollectionUtils.isNotEmpty(esPileList)) {
                List<PileDTO> addRulePiles = Lists.newArrayList();
                for (OpLocationPileEvseElasticDTO esPile : esPileList) {
                    PileDTO pileDTO = new PileDTO();
                    pileDTO.setLocationId(stationId);
                    pileDTO.setPileId(esPile.getId());
                    addRulePiles.add(pileDTO);
                }
                LambdaUpdateWrapper<RuleEntity> ruleEntityLambdaUpdateWrapper = Wrappers.lambdaUpdate(RuleEntity.class).eq(RuleEntity::getName, currentTimeMillis);
                List<RuleEntity> ruleEntities = ruleMapper.selectList(ruleEntityLambdaUpdateWrapper);
                if (CollectionUtils.isNotEmpty(ruleEntities)) {
                    RuleEntity ruleEntity = ruleEntities.get(0);
                    RelatePileDTO relatePileDTO = new RelatePileDTO();
                    relatePileDTO.setRuleId(ruleEntity.getId());
                    relatePileDTO.setRuleName(ruleEntity.getName());
                    relatePileDTO.setPiles(addRulePiles);
                    ruleRepository.relatePile(relatePileDTO);
                }
            }

            //5、更新ES场站
            OpLocationElasticDTO opLocationElasticDTO = esStationList.get(0);
            opLocationElasticDTO.setId(stationId);
            opLocationElasticDTO.setUpdatedAt(currentTimeMillis);
            opLocationElasticDTO.setName(homeShareUpdateDTO.getName());
            opLocationElasticDTO.setAddress(homeShareUpdateDTO.getAddress());
            opLocationElasticDTO.setCountry(homeShareUpdateDTO.getCountry());
            opLocationElasticDTO.setCity(homeShareUpdateDTO.getCity());
            opLocationElasticDTO.setPostalCode(homeShareUpdateDTO.getPostalCode());
            opLocationElasticDTO.setCountry(homeShareUpdateDTO.getCountry());
            opLocationElasticDTO.setProvince(homeShareUpdateDTO.getProvince());
            opLocationElasticDTO.setLatitude(homeShareUpdateDTO.getLatitude());
            opLocationElasticDTO.setLongitude(homeShareUpdateDTO.getLongitude());
            GeoPoint location = new GeoPoint(Double.parseDouble(homeShareUpdateDTO.getLatitude()), Double.parseDouble(homeShareUpdateDTO.getLongitude()));
            opLocationElasticDTO.setLocation(location);
            opLocationElasticDTO.setAnnouncement(homeShareUpdateDTO.getDesc());
            opLocationElasticDTO.setTimeZone(homeShareUpdateDTO.getTimeZone());
            opLocationElasticDTO.setZoneId(homeShareUpdateDTO.getZoneId());
            opLocationElastic.save(opLocationElasticDTO);
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE+ RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_UPDATE_ROUTE,JSON.toJSONString(stationId));
        }
        return Boolean.TRUE;
    }

    /**
     * 编辑个人信息
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean updatePersonalInfo(HomeShareUpdateDTO homeShareUpdateDTO) {
        Long userId = UserUtil.getUserId();
        Long currentTimeMillis = System.currentTimeMillis();
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findAllByOperatorId(userId);
        log.info(BaseConstant.CURRENT_USER_AND_LOCATION, userId, JSON.toJSONString(esStationList));
        if (CollectionUtils.isNotEmpty(esStationList)) {
            Long stationId = esStationList.get(0).getId();

            //2、更新op_location_operation表
            LambdaQueryWrapper<OpLocationOperationEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationOperationEntity.class).eq(OpLocationOperationEntity::getLocationId, stationId);
            List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(lambdaQueryWrapper);
            if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
                OpLocationOperationEntity opLocationOperationEntity = opLocationOperationEntities.get(0);
                opLocationOperationEntity.setUpdatedAt(currentTimeMillis);
                opLocationOperationEntity.setServiceTel(homeShareUpdateDTO.getServiceTel());
                opLocationOperationMapper.updateById(opLocationOperationEntity);
            }

            //5、更新ES场站
            OpLocationElasticDTO opLocationElasticDTO = esStationList.get(0);
            opLocationElasticDTO.setPersonalSurName(homeShareUpdateDTO.getPersonalSurName());
            opLocationElasticDTO.setPersonalName(homeShareUpdateDTO.getPersonalName());
            opLocationElasticDTO.setServiceTel(homeShareUpdateDTO.getServiceTel());
            opLocationElastic.save(opLocationElasticDTO);
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE+ RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_UPDATE_ROUTE,JSON.toJSONString(stationId));

        }
        return Boolean.TRUE;
    }

    /**
     * 编辑价格信息和收款账号
     *
     * @param homeShareUpdateDTO 编辑信息
     * @return 编辑结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateCostRule(HomeShareUpdateDTO homeShareUpdateDTO) {
        Long userId = UserUtil.getUserId();
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findAllByOperatorId(userId);
        log.info(BaseConstant.CURRENT_USER_AND_LOCATION, userId, JSON.toJSONString(esStationList));
        if (CollectionUtils.isNotEmpty(esStationList)) {

            //更新ES场站
            OpLocationElasticDTO opLocationElasticDTO = esStationList.get(0);
            opLocationElasticDTO.setStripeAccount(homeShareUpdateDTO.getStripeAccount());
            opLocationElastic.save(opLocationElasticDTO);
            log.info("用户场站:{}", JSON.toJSONString(opLocationElasticDTO));

            //更新桩枪计费规则
            if (StringUtils.isNotBlank(homeShareUpdateDTO.getPileSn()) && homeShareUpdateDTO.getOpLocationBasicTariffAddFirstDTO() != null) {
                log.info("SN码:{} 计费规则对象：{}", homeShareUpdateDTO.getPileSn(), JSON.toJSONString(homeShareUpdateDTO.getOpLocationBasicTariffAddFirstDTO()));
                OpLocationPileEvseElasticDTO esPile = opLocationPileEvseElastic.findByPileSn(homeShareUpdateDTO.getPileSn());
                log.info("es桩:{}", JSON.toJSONString(esPile));

                //远程调用创建计费规则
                Long tariffId;
                try {
                    com.autel.cloud.tariff.dto.OpLocationBasicTariffAddFirstDTO opLocationBasicTariffAddFirstDTO = DozerConvert.map(homeShareUpdateDTO.getOpLocationBasicTariffAddFirstDTO(), com.autel.cloud.tariff.dto.OpLocationBasicTariffAddFirstDTO.class);
                    Result<CostModelHeadVO> costModelHeadVOResult = tariffFeignClient.addOpLocationBasicTariff(opLocationBasicTariffAddFirstDTO);
                    tariffId = costModelHeadVOResult.getData().getId();
                    log.info("远程调用创建计费规则结果：{}", JSON.toJSONString(costModelHeadVOResult));
                } catch (Exception e) {
                    throw new MessageCodeException(PileBaseEnum.ADD_TARIFF_FAILURE);
                }
                try {
                    log.info("=========== 家桩共享修改计费规则,下发begin, tariffId: {}, pileSn: {}, timezone:{} ========================", tariffId, homeShareUpdateDTO.getPileSn(), null);
                    distributeCostRuleService.dispatchTariffOfPublicPile(tariffId, homeShareUpdateDTO.getPileSn(), null);
                } catch (Exception e) {
                    log.info("家桩共享修改计费规则,下发计费失败:" + e);
                }
                log.info("计费规则id:{}", tariffId);
                if (esPile != null) {
                    //es桩基础版计费信息和计费规则id
                    esPile.setCostRule(JSON.toJSONString(homeShareUpdateDTO.getOpLocationBasicTariffAddFirstDTO()));
                    esPile.setTariffId(tariffId);
                    opLocationPileEvseElastic.save(esPile);
                    //枪计费规则id
                    List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByPileSn(homeShareUpdateDTO.getPileSn());
                    if (CollectionUtils.isNotEmpty(esEVSEList)) {
                        //es枪计费规则id
                        OpLocationEvseElasticDTO esEVSE = esEVSEList.get(0);
                        esEVSE.setTariffId(tariffId);
                        opLocationEvseElastic.save(esEVSE);
                        //数据库枪计费规则id
                        OpLocationEvseEntity opLocationEvseEntity = opLocationEvseMapper.selectById(esEVSE.getId());
                        opLocationEvseEntity.setTariffId(tariffId);
                        opLocationEvseMapper.updateById(opLocationEvseEntity);
                    }
                }
            }
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE+ RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_UPDATE_ROUTE,JSON.toJSONString(opLocationElasticDTO.getId()));
        }
        return Boolean.TRUE;
    }

    @Override
    @Transactional
    public Boolean pileStop(String pileSN,Long userId) {
        if (userId == null ){
            userId = UserUtil.getUserId();
        }
        log.info("HomePileShareRepositoryImpl.pileStop and pileSn = " + pileSN + " and userId = " + userId);

        if (userId == null) {
            log.info("当前用户id为空");
            return Boolean.FALSE;
        }
        OpLocationPileEvseElasticDTO esPile = opLocationPileEvseElastic.findByPileSn(pileSN);
        log.info("停止共享的桩信息：{}", JSON.toJSONString(esPile));
        if (esPile == null || !Objects.equals(esPile.getStatus(), EVSETypeEnum.SHARE_PILE.getCode()) || !Objects.equals(esPile.getOperatorId(),userId)) {
            log.info("桩:{} 不是家桩共享类型或者不是当前用户共享的家桩，不能停止家桩共享", pileSN);
            return Boolean.FALSE;
        }
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findAllByOperatorId(userId);
        log.info("当前用户:{} 的es场站：{}", userId, JSON.toJSONString(esStationList));
        if (CollectionUtils.isEmpty(esStationList)) {
            log.info("用户:{} 没有私桩共享场站", userId);
            return Boolean.FALSE;
        }

        //远程调用device服务更新设备使用场景为2-家桩
        try {
            PileUsageDTO pileUsageDTO = new PileUsageDTO();
            pileUsageDTO.setUsage(EVSETypeEnum.HOME_PILE.getCode());
            Boolean aBoolean = deviceServiceFeign.updateUsage(pileUsageDTO, pileSN).getData();
            //桩激活指标上报
            if (aBoolean) {
                rabbitTemplate.convertAndSend("DIRECT_EXCHANGE_PILE_ACTIVATION_UNBIND_AUTEL"+ RabbitBean.RABBITMQ_VERSION_SUFFIX, "PILE.ACTIVATION.UNBIND_AUTEL", pileSN);
            }
            log.info("更新设备");
        } catch (Exception e) {
            throw new MessageCodeException(PileBaseEnum.UPDATE_DEVICE_USE_SCENARIO_FAILURE);
        }

        //删除数据库桩
        opLocationPileEvseMapper.deleteByPileSN(pileSN);

        //删除数据库枪
        opLocationEvseMapper.deleteByPileSN(pileSN);

        //删除es桩
        log.info("停止家桩共享删除es桩：{}", userId);
        opLocationPileEvseElastic.deleteById(esPile.getId());

        //删除es枪
        List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByPileSn(pileSN);
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            log.info("停止家桩共享删除es枪：{}", userId);
            opLocationEvseElastic.deleteAll(esEVSEList);
            esEVSEList.forEach(dto -> rabbitTemplate.convertAndSend(PILE_BASE_GUN_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_GUN_DELETE_ROUTE, JSON.toJSONString(dto.getId())));
        }

        //场站信息
        if (CollectionUtils.isNotEmpty(esStationList)) {
            Long locationId = esStationList.get(0).getId();
            List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findAllByLocationId(locationId);
            //如果没有共享桩了，删除场站
            if (CollectionUtils.isEmpty(esPileList)) {
                //场站表 逻辑删除
                opLocationMapper.deleteByLocationId(locationId);
                //运营表
                opLocationOperationMapper.deleteByLocationId(locationId);
                //运营时间
                opLocationOpenTimeMapper.deleteByLocationId(locationId);
                //场站图片
                opLocationImageMapper.deleteByLocationId(locationId);
                //es场站
                opLocationElastic.deleteById(locationId);
                rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_DELETE_ROUTE,JSON.toJSONString(locationId));
            }
        }

        // 缓存桩运营商用户删除
        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSN);
        stringRedisTemplate.delete(snOperatorIdKey);
/*
        try{
            sendRefreshPileConfigMsg(pileSN, UserUtil.getUserId().toString());
            log.info("stop sendRefreshPileConfigMsg pileSN :{}", pileSN);
        }catch (Exception e){
            log.error("stop sendRefreshPileConfigMsg 异常",e);
        }*/

        return Boolean.TRUE;
    }

    /**
     * 刷新桩配置
     * @param pileSN
     */
    private void sendRefreshPileConfigMsg(String pileSN, String userId) {
        /*Result<List<UserSharePileJoinListVO>> joinListResult = homePileFeignClient.listForJoin(pileSN);
        log.info("查询桩:{} 已邀请列表:{}", pileSN, JSONObject.toJSONString(joinListResult));
        if (joinListResult != null && org.apache.commons.collections4.CollectionUtils.isNotEmpty(joinListResult.getData())) {
            Long seq = IdWorker.getId();
            for (UserSharePileJoinListVO joinVO : joinListResult.getData()) {
                Long familyMemberUserId = joinVO.getFamilyMemberUserId();
                if (!userId.equals(familyMemberUserId)) {
                    MessageDTO<String> msg = new MessageDTO<>();
                    msg.setSeq(seq.toString());
                    msg.setData(pileSN);
                    msg.setCmd(MessageDTO.CmdEnum.REFRESH_CONFIG.getValue());
                    SendMsgDto sendMsgDto = new SendMsgDto();
                    sendMsgDto.setReceiver(joinVO.getFamilyMemberUserId().toString());
                    sendMsgDto.setMsg(msg.toString());
                    wsCoreClient.sendMsg(sendMsgDto);
                    seq++;
                }
            }
        }*/
    }

    /**
     * 用户停止家桩共享
     */
    @Override
    @Transactional
    public Boolean userStop() {
        //获取用户的桩
        List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findByOperatorId(UserUtil.getUserId());
        if (CollectionUtils.isNotEmpty(esPileList)) {
            esPileList.forEach(esPile -> pileStop(esPile.getPileSn(),null));
        }
        return Boolean.TRUE;
    }

    @Override
    public Boolean userStopV2(Long userId) {
        //获取用户的桩
        List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findByOperatorId(userId);
        if (CollectionUtils.isNotEmpty(esPileList)) {
            esPileList.forEach(esPile -> pileStop(esPile.getPileSn(),userId));
        }
        return Boolean.TRUE;
    }

    /**
     * 家桩共享详情
     *
     * @return 家桩共享详情
     */
    @Override
    public HomeShareDetailVO detail(String pileSN) {
        HomeShareDetailVO homeShareDetailVO = new HomeShareDetailVO();
        boolean userHaveShare = false;
        boolean pileHaveShare = false;

        List<OpLocationPileEvseElasticDTO> esPileList;
        if (StringUtils.isNotBlank(pileSN)) {
            //桩是否已经共享
            esPileList = opLocationPileEvseElastic.findByPileSnLike(pileSN);
            if (CollectionUtils.isNotEmpty(esPileList)) {
                pileHaveShare = true;
            }
        }


        Long userId = UserUtil.getUserId();
        log.info(BaseConstant.CURRENT_USER, userId);
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findAllByOperatorId(userId);
        if (CollectionUtils.isNotEmpty(esStationList)) {
            OpLocationElasticDTO opLocationElasticDTO = esStationList.get(0);
            log.info("es场站信息:{}", JSON.toJSONString(opLocationElasticDTO));
            Long stationId = opLocationElasticDTO.getId();
            homeShareDetailVO.setStationId(stationId);

            //用户是否已共享
            userHaveShare = true;

            if (StringUtils.isNotBlank(pileSN)) {
                //桩是否已经共享
                esPileList = opLocationPileEvseElastic.findByPileSnLike(pileSN);
                if (CollectionUtils.isNotEmpty(esPileList)) {
                    //基础版计费信息（从桩获取）
                    homeShareDetailVO.setOpLocationBasicTariffAddFirstDTO(JSON.parseObject(esPileList.get(0).getCostRule(), OpLocationBasicTariffAddFirstDTO.class));
                } else {
                    //基础版计费信息（获取该场站的其他桩的第一个的计费信息）
                    List<OpLocationPileEvseElasticDTO> pileList = opLocationPileEvseElastic.findByPileSnNotAndLocationId(pileSN, opLocationElasticDTO.getId());
                    if (CollectionUtils.isNotEmpty(pileList)) {
                        for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : pileList) {
                            if (StringUtils.isNotBlank(opLocationPileEvseElasticDTO.getCostRule())) {
                                homeShareDetailVO.setOpLocationBasicTariffAddFirstDTO(JSON.parseObject(opLocationPileEvseElasticDTO.getCostRule(), OpLocationBasicTariffAddFirstDTO.class));
                                break;
                            }
                        }
                    }
                }
            }

            //营业时间
            LambdaQueryWrapper<OpLocationOpenTimeEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationOpenTimeEntity.class)
                    .eq(OpLocationOpenTimeEntity::getLocationId, stationId)
                    .eq(OpLocationOpenTimeEntity::getDeleted, 0);
            List<OpLocationOpenTimeEntity> opLocationOpenTimeEntities = opLocationOpenTimeMapper.selectList(lambdaQueryWrapper);
            log.info("站点:{} 的营业时间:{}", stationId, opLocationOpenTimeEntities);
            List<OpLocationOpenTimeDTO> opLocationOpenTimeDTOS = new ArrayList<>();
            if (CollectionUtils.isNotEmpty(opLocationOpenTimeEntities)) {
                for (OpLocationOpenTimeEntity opLocationOpenTimeEntity : opLocationOpenTimeEntities) {
                    OpLocationOpenTimeDTO opLocationOpenTimeDTO = new OpLocationOpenTimeDTO();
                    opLocationOpenTimeDTO.setPeriodBegin(opLocationOpenTimeEntity.getPeriodBegin());
                    opLocationOpenTimeDTO.setPeriodEnd(opLocationOpenTimeEntity.getPeriodEnd());
                    if (StringUtils.isNotBlank(opLocationOpenTimeEntity.getDateValue())) {
                        JSONArray jsonArray;
                        try {
                            jsonArray = JSON.parseArray(opLocationOpenTimeEntity.getDateValue());
                        } catch (Exception e) {
                            throw new MessageCodeException("解析开放时间dateValue失败：" + JSON.toJSONString(opLocationOpenTimeEntity));
                        }
                        ArrayList<Object> list2 = com.google.common.collect.Lists.newArrayList(jsonArray);
                        List<String> collect = list2.stream().map(Object::toString).collect(Collectors.toList());
                        opLocationOpenTimeDTO.setDateValue(collect);
                    }
                    opLocationOpenTimeDTOS.add(opLocationOpenTimeDTO);
                }
            }
            homeShareDetailVO.setOpenTimeList(opLocationOpenTimeDTOS);

            //图片
            LambdaQueryWrapper<OpLocationImageEntity> opLocationImageEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationImageEntity.class)
                    .eq(OpLocationImageEntity::getLocationId, stationId)
                    .eq(OpLocationImageEntity::getDeleted, 0);
            List<OpLocationImageEntity> opLocationImageEntities = opLocationImageMapper.selectList(opLocationImageEntityLambdaQueryWrapper);
            List<OpImageDTO> opImageList = opLocationImageEntities.stream().map(opLocationImage -> {
                Long id = opLocationImage.getImageId();
                OpImageEntity opImageEntity = opImageMapper.selectById(id);
                return DozerConvert.map(opImageEntity, OpImageDTO.class);
            }).collect(Collectors.toList());
            homeShareDetailVO.setOpImageList(opImageList);

            homeShareDetailVO.setPileSn(pileSN);
            homeShareDetailVO.setName(opLocationElasticDTO.getName());
            homeShareDetailVO.setCountry(opLocationElasticDTO.getCountry());
            homeShareDetailVO.setProvince(opLocationElasticDTO.getProvince());
            homeShareDetailVO.setCity(opLocationElasticDTO.getCity());
            homeShareDetailVO.setAddress(opLocationElasticDTO.getAddress());
            homeShareDetailVO.setLatitude(opLocationElasticDTO.getLatitude());
            homeShareDetailVO.setLongitude(opLocationElasticDTO.getLongitude());
            homeShareDetailVO.setTimeZone(opLocationElasticDTO.getTimeZone());
            homeShareDetailVO.setZoneId(opLocationElasticDTO.getZoneId());
            homeShareDetailVO.setPostalCode(opLocationElasticDTO.getPostalCode());
            homeShareDetailVO.setDesc(opLocationElasticDTO.getAnnouncement());
            homeShareDetailVO.setStripeAccount(opLocationElasticDTO.getStripeAccount());
            homeShareDetailVO.setPersonalName(opLocationElasticDTO.getPersonalName());
            homeShareDetailVO.setPersonalSurName(opLocationElasticDTO.getPersonalSurName());
            homeShareDetailVO.setServiceTel(opLocationElasticDTO.getServiceTel());
            homeShareDetailVO.setStatus(opLocationElasticDTO.getStatus());
        }
        homeShareDetailVO.setUserHaveShare(userHaveShare);
        homeShareDetailVO.setPileHaveShare(pileHaveShare);
        return homeShareDetailVO;
    }

    /**
     * 查询限时免费
     */
    @Override
    public OpLimitFreeVO limitFree() {
        LambdaQueryWrapper<OpLimitFreeEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLimitFreeEntity::getDeleted, 0);
        OpLimitFreeEntity opLimitFreeEntity = opLimitFreeMapper.selectOne(queryWrapper);
        OpLimitFreeVO opLimitFreeVO = new OpLimitFreeVO();
        opLimitFreeVO.setFreeDate(opLimitFreeEntity.getFreeDate());
        return opLimitFreeVO;
    }

    @Override
    public Boolean userHaveShare() {
        Long userId = UserUtil.getUserId();
        log.info(BaseConstant.CURRENT_USER, userId);
        List<OpLocationElasticDTO> userStationList = opLocationElastic.findAllByOperatorId(userId);
        return CollectionUtils.isEmpty(userStationList) ? Boolean.FALSE : Boolean.TRUE;
    }

    /**
     * 桩是否已共享
     *
     * @return 是否有共享场站
     */
    @Override
    public Boolean pileHaveShare(String pileSN) {
        OpLocationPileEvseElasticDTO pile = opLocationPileEvseElastic.findByPileSn(pileSN);
        return pile == null ? Boolean.FALSE : Boolean.TRUE;
    }

    /**
     * 用户所有已共享的桩
     *
     * @return 用户所有已共享的桩
     */
    @Override
    public List<String> userHaveSharePileList() {
        List<String> haveSharePileSNList = Lists.newArrayList();
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        if (jwtInfo == null || jwtInfo.getId() == null){
            throw new MessageCodeException(PileUserEnum.PARSE_TOKEN_ERROR);
        }
        Iterable<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOIterable = opLocationPileEvseElastic.findByOperatorId(jwtInfo.getId());
        ArrayList<OpLocationPileEvseElasticDTO> esPileList = Lists.newArrayList(opLocationPileEvseElasticDTOIterable);
        if (CollectionUtils.isNotEmpty(esPileList)) {
            haveSharePileSNList = esPileList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).distinct().collect(Collectors.toList());
        }

        try {
            //当前用户作为被共享者的记录
            Result<List<UserSharePileListVO>> listResult = homePileFeignClient.listAsFamilyUserId();
            log.info("======HomePileShareRepositoryImpl userHaveSharePileList:{}", listResult);
            if (listResult != null && CollectionUtils.isNotEmpty(listResult.getData())) {
                //根据SN查询es索引，如果有记录说明有被私桩共享
                List<String> snList = listResult.getData().stream().map(UserSharePileListVO::getSn).distinct().collect(Collectors.toList());
                List<OpLocationPileEvseElasticDTO> esPileEVSEList = opLocationPileEvseElastic.findByPileSnIn(snList);
                if (CollectionUtils.isNotEmpty(esPileEVSEList)) {
                    for (OpLocationPileEvseElasticDTO esPileEVSE : esPileEVSEList) {
                        if (!haveSharePileSNList.contains(esPileEVSE.getPileSn())) {
                            haveSharePileSNList.add(esPileEVSE.getPileSn());
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.info("");
        }
        return haveSharePileSNList;
    }

    /**
     * 更新限时免费
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public OpLimitFreeVO updateLimitFree(Long freeDate) {
        LambdaQueryWrapper<OpLimitFreeEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLimitFreeEntity::getDeleted, 0);
        OpLimitFreeEntity opLimitFreeEntity = opLimitFreeMapper.selectOne(queryWrapper);
        opLimitFreeEntity.setFreeDate(freeDate);
        opLimitFreeEntity.setUpdatedAt(System.currentTimeMillis());
        opLimitFreeMapper.updateById(opLimitFreeEntity);
        OpLimitFreeVO opLimitFreeVO = new OpLimitFreeVO();
        opLimitFreeVO.setFreeDate(opLimitFreeEntity.getFreeDate());
        return opLimitFreeVO;
    }

    /**
     * 新增限时免费
     */
    @Override
    public OpLimitFreeVO addLimitFree(Long freeDate) {
        //先查表里面有deleted为0的数据吗
        LambdaQueryWrapper<OpLimitFreeEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLimitFreeEntity::getDeleted, 0);
        OpLimitFreeEntity opLimitFreeEntity = new OpLimitFreeEntity();
        List<OpLimitFreeEntity> opLimitFreeEntityList = opLimitFreeMapper.selectList(queryWrapper);
        if (!opLimitFreeEntityList.isEmpty()) {
            //有就先删掉原数据，再新增
            List<Long> ids = new ArrayList<>();
            for (OpLimitFreeEntity opLimitFreeEntity1 : opLimitFreeEntityList) {
                ids.add(opLimitFreeEntity1.getId());
            }
            opLimitFreeMapper.deleteLimitFreeByIds(ids);
        }
        //没有就直接新增
        opLimitFreeEntity.setFreeDate(freeDate);
        opLimitFreeEntity.setCreatedAt(System.currentTimeMillis());
        opLimitFreeEntity.setUpdatedAt(System.currentTimeMillis());
        opLimitFreeEntity.setDeleted(0);
        opLimitFreeMapper.insert(opLimitFreeEntity);
        OpLimitFreeVO opLimitFreeVO = new OpLimitFreeVO();
        opLimitFreeVO.setFreeDate(freeDate);
        return opLimitFreeVO;
    }

    /**
     * 随机延迟开关
     */
    @Override
    public Boolean randomDelay(RandomDelayDTO randomDelayDTO) {
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(randomDelayDTO.getPileSn());
        if (opLocationPileEvseElasticDTO != null) {
            opLocationPileEvseElasticDTO.setRandomDelaySwitch(randomDelayDTO.getRandomDelaySwitch());
            if (randomDelayDTO.getRandomDelayTime() != null) {
                opLocationPileEvseElasticDTO.setRandomDelayTime(randomDelayDTO.getRandomDelayTime());
            }else {
                opLocationPileEvseElasticDTO.setRandomDelayTime(null);
            }
            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
        }
        return true;
    }

    /**
     * 是否开启随机延迟
     */
    @Override
    public RandomDelayVO randomDelayQuery(String pileSN) {
        RandomDelayVO randomDelayVO = new RandomDelayVO();
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(pileSN);
        if (opLocationPileEvseElasticDTO != null) {
            if (opLocationPileEvseElasticDTO.getRandomDelaySwitch() == null) {
                randomDelayVO.setRandomDelaySwitch(false);
            } else {
                randomDelayVO.setRandomDelaySwitch(opLocationPileEvseElasticDTO.getRandomDelaySwitch());
            }
            if (opLocationPileEvseElasticDTO.getRandomDelayTime() != null) {
                randomDelayVO.setRandomDelayTime(opLocationPileEvseElasticDTO.getRandomDelayTime());
            } else {
                randomDelayVO.setRandomDelayTime(null);
            }
        } else {
            randomDelayVO.setRandomDelaySwitch(false);
            randomDelayVO.setRandomDelayTime(null);
        }
        return randomDelayVO;
    }

    /**
     * 构建创建设备对象
     *
     * @param homeShareAddDTO 家桩共享添加对象
     * @param stationId       场站id
     * @return 创建设备对象
     */
    private OpLocationEvseDTO buildCreateEVSEDTO(HomeShareAddDTO homeShareAddDTO, Long stationId) {

        //根据SN获取桩详细信息
        Result<ChargePileDTO> chargePileDTOResult = deviceServiceFeign.pileDetail(homeShareAddDTO.getPileSn());
        if (chargePileDTOResult == null || chargePileDTOResult.getCode() != HttpStatus.OK.value()) {
            throw new MessageCodeException(PileBaseEnum.CAN_NOT_FIND_SN_INFO, new Object[]{homeShareAddDTO.getPileSn()});
        }
        ChargePileDTO chargePileDTO = chargePileDTOResult.getData();
        log.info("feign调用device服务，根据SN获取桩详细信息结果：{}", JSON.toJSONString(chargePileDTO));

        //转成createEVSE的参数格式
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        opLocationEvseDTO.setBrandId(1L);
        opLocationEvseDTO.setBrandName("Autel");
        opLocationEvseDTO.setLocationId(stationId);
        opLocationEvseDTO.setPileName(homeShareAddDTO.getPileSn());
        opLocationEvseDTO.setPileSN(homeShareAddDTO.getPileSn());
        opLocationEvseDTO.setPinCode(chargePileDTO.getPin());
        opLocationEvseDTO.setPower(chargePileDTO.getOutputPower());
        String phaseConstant = "PHASE";
        Integer category = chargePileDTO.getCategory();
        String type = CategoryEnum.getEnumByCode(category).getDesc();
        Integer phase = chargePileDTO.getPhase();
        opLocationEvseDTO.setPowerType(type + "_" + phase + "_" + phaseConstant);
        opLocationEvseDTO.setThirdPart(0);
        //连接器
        List<OpLocationConnectorDTO> oplocationConnectorScanDTOS = new ArrayList<>();
        OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
        opLocationConnectorDTO.setConnectorId(String.valueOf(1));
        opLocationConnectorDTO.setGunType(this.getGunType(homeShareAddDTO.getPileSn()));
        oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        opLocationEvseDTO.setOpLocationConnectorDTOs(oplocationConnectorScanDTOS);

        return opLocationEvseDTO;
    }

    /**
     * 美标type 1  SN第二位是L
     * 欧标type2 SN第二位是E
     * 国标G**  SN第二位是G
     * @param pileSn
     * @return
     */
    private Integer getGunType(String pileSn) {
        if (StringUtils.isBlank(pileSn) || pileSn.length() < 2) {
            return ConnectorGunTypeEnum.GB_T_AC.getCode();
        }
        String flag = pileSn.substring(1, 2).toUpperCase();
        Integer type;
        switch (flag) {
            case "L":
                type = ConnectorGunTypeEnum.TYPE_1.getCode();
                break;
            case "E":
                type = ConnectorGunTypeEnum.TYPE_2.getCode();
                break;
            default:
                type = ConnectorGunTypeEnum.GB_T_AC.getCode();
                break;
        }
        return type;
    }

    @Override
    public Boolean updateHomePileInfo(OpLocationPileEvseDTO opLocationPileEvseDTO) {
        log.info("updateHomePileInfo.opLocationPileEvseDTO:{}", JSON.toJSONString(opLocationPileEvseDTO));
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(opLocationPileEvseDTO.getPileSn());
        if (ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTO)) {
            log.info("updateHomePileInfo.opLocationPileEvseElasticDTO:{}", JSON.toJSONString(opLocationPileEvseElasticDTO));
            opLocationPileEvseElasticDTO.setName(opLocationPileEvseDTO.getName());
            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
        }
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findByEvseSn(opLocationPileEvseDTO.getPileSn() + "_" + 1);
        if (ObjectUtils.isNotEmpty(opLocationEvseElasticDTO)) {
            log.info("updateHomePileInfo.opLocationEvseElasticDTO:{}", JSON.toJSONString(opLocationEvseElasticDTO));
            opLocationEvseElasticDTO.setPileName(opLocationPileEvseDTO.getName());
            opLocationEvseElastic.save(opLocationEvseElasticDTO);
            return true;
        }
        return false;
    }

}
