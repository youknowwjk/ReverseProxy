package com.autel.cloud.pile.base.domain.service.impl;


import cn.hutool.core.collection.CollUtil;
import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.common.util.LocaleResultUtil;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.model.DataAuthorizeDto;
import com.autel.cloud.base.model.Node;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.monitor.dto.OpEvseStatusUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.ocpi.vo.location.LocationVO;
import com.autel.cloud.pile.base.GunDetailVO;
import com.autel.cloud.pile.base.OpLocationEvseInfoDTO;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.OpLocationConvert;
import com.autel.cloud.pile.base.domain.convert.OpLocationTempConvert;
import com.autel.cloud.pile.base.domain.model.OpLocationMenuQueryDto;
import com.autel.cloud.pile.base.domain.model.dto.LocationPowerDTO;
import com.autel.cloud.pile.base.domain.model.dto.SetLocationEroamingForLocationDTO;
import com.autel.cloud.pile.base.domain.model.dto.SetLocationTypeForLocationDTO;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.domain.model.vo.SelectLocationInfoForEroamingVO;
import com.autel.cloud.pile.base.domain.model.vo.location.LocationBaseVO;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.*;
import com.autel.cloud.pile.base.domain.utils.AesUtil;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.domain.utils.MessageSourceUtil;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.common.SearchDTO;
import com.autel.cloud.pile.base.dto.fleet.SelectLocationForFleetDTO;
import com.autel.cloud.pile.base.dto.location.LocationSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.oicp.*;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.dto.tariff.BindCostModelRuleGroupForGunDTO;
import com.autel.cloud.pile.base.dto.tax.BasicTaxDTO;
import com.autel.cloud.pile.base.dto.tax.TaxDTO;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.infrastructure.amqp.MQSender;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElasticService;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.*;
import com.autel.cloud.pile.base.infrastructure.feign.*;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ConnectorDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.PileInfoDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.UpdatePileLocationDTO;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OicpFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.mapper.OpImageMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationOperationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.util.CompetenceUtil;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.advancePayment.AppPrePaymentVO;
import com.autel.cloud.pile.base.vo.advancePayment.LocationInfoVO;
import com.autel.cloud.pile.base.vo.app.AggMapVO;
import com.autel.cloud.pile.base.vo.fleet.SelectLocationForFleetVO;
import com.autel.cloud.pile.base.vo.location.LocationBasicInfoPackageVO;
import com.autel.cloud.pile.base.vo.location.LocationBasicInfoVO;
import com.autel.cloud.pile.base.vo.location.LocationDataVO;
import com.autel.cloud.pile.base.vo.location.LocationSimpleInfoQueryVO;
import com.autel.cloud.pile.bill.dto.UnitDto;
import com.autel.cloud.pile.bill.dto.wallet.AccountTypeEnum;
import com.autel.cloud.pile.bill.dto.wallet.UserAccountInfoDto;
import com.autel.cloud.pile.bill.enums.CurrencyTypeEnum;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.enums.PayMethodEnum;
import com.autel.cloud.pile.bill.feign.BaseAdminFeign;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.feign.IChargingFeignClient;
import com.autel.cloud.pile.bill.feign.PayTradingFeignClient;
import com.autel.cloud.pile.bill.vo.BillInfoForEvseVO;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.autel.cloud.pile.bill.vo.emsp.TbWalletConfigVO;
import com.autel.cloud.pile.bill.vo.wallet.AccountDetail;
import com.autel.cloud.pile.bill.vo.wallet.UserOnBoardingDetail;
import com.autel.cloud.pile.user.api.constant.RedisKeys;
import com.autel.cloud.pile.user.api.dto.MarketingRuleDTO;
import com.autel.cloud.pile.user.api.dto.QuerySubTreeDTO;
import com.autel.cloud.pile.user.api.dto.UserCompetenceDTO;
import com.autel.cloud.pile.user.api.enums.SubTreeEnum;
import com.autel.cloud.pile.user.api.feign.PileMerchantUserFeign;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.*;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.CurrencyDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelBasicInfoVO;
import com.autel.cloud.tariff.vo.CostModelRuleEntityVO;
import com.autel.cloud.tariff.vo.CurrentMomentFeeRulesVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.collect.Lists;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.query.*;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StopWatch;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.PILE_BASE_LOCATION_UPDATE_EXCHANGE;
import static com.autel.cloud.pile.base.constant.AmqpConstant.PILE_BASE_LOCATION_UPDATE_ROUTE;

/**
 * @ClassName OpLocationServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/4/14 11:59
 * @Version 0.0.1-SNAPSHOT
 */
@RefreshScope
@Service
@Log4j2
public class OpLocationServiceImpl implements OpLocationService {

    private final OpLocationRepository opLocationRepository;
    @Resource
    OpLocationOperationMapper opLocationOperationMapper;
    @Resource
    OpLocationMapper opLocationMapper;
    @Resource
    private MQSender mqSender;
    @Autowired
    private MonitorFeign monitorFeign;
    @Resource
    private IChargingFeignClient chargingFeignClient;
    @Autowired
    private OpCountryRepository opCountryRepository;
    @Autowired
    private OpLocationTempRepository opLocationTempRepository;
    @Autowired
    @Lazy
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;
    @Autowired
    private OpLocationEvseService opLocationEvseService;
    @Resource
    private OpsMgmtClient opsMgmtClient;
    @Autowired
    private TariffAPPFeign tariffAPPFeign;
    @Resource
    private OicpFeignClientProxy oicpFeignClient;
    @Resource
    private RabbitTemplate rabbitTemplate;
    @Resource
    private OpLocationElastic opLocationElastic;

    @Autowired
    private SellerAccountService sellerAccountService;
    @Autowired
    private OpLocationPriceService opLocationPriceService;

    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @Autowired
    private TariffFeignClient tariffFeignClient;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;
    @Resource
    private OpLocationEvseExpandElasticService opLocationEvseExpandElasticService;
    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Resource
    private PileUserServiceFeign pileUserServiceFeign;
    @Resource
    private BaseAdminFeign baseAdminFeign;
    @Resource
    private OpImageMapper opImageMapper;
    @Resource
    private CommonUtilService commonUtilService;
    @Value("${payment.maxPrepaymentAmount:10000000}")
    private Long maxPrepaymentAmount;
    @Autowired
    private MonitorFeignClient monitorFeignClient;
    @Value("${payment.defaultPrepaymentAmount:50}")
    private int defaultPrepaymentAmount;

    @Autowired
    @Lazy
    private RuleRepository ruleRepository;
    @Value("#{${payment.currencyPrepayment}}")
    private Map<String, String> currencyPrepayment;
    @Autowired
    private IBillFeignClient billFeignClient;

    @Resource
    private PileUserFeign pileUserFeign;
    @Autowired
    private PileBaseAsync pileBaseAsync;


    @Autowired
    private PileMerchantUserFeign pileMerchantUserFeign;

    @Autowired
    private MessageSourceUtil messageSourceUtil;

    @Resource
    private PayTradingFeignClient payTradingFeignClient;

    @Resource
    private PileBillClient pileBillClient;

    @Autowired
    private AesUtil aesUtil;

    @Value("${planetev.contactEmail:planetev@planetcomm.com}")
    protected String contactEmail;

    private static final String AES_ALGORITHM = "AES/CBC/PKCS5Padding";

    private static final String CHARGE_STATION_IN_OPEN = "charge_station_in_opening";
    private static final String CHARGE_STATION_TO_CLOSE = "charge_station_to_close";
    private static final String CHARGE_STATION_ALL_DAY_WORK = "charge_station_all_day_work";
    private static final String MONDAY= "charge_station_day_monday";
    private static final String TUESDAY= "charge_station_day_tuesday";
    private static final String WEDNESDAY= "charge_station_day_wednesday";
    private static final String THURSDAY= "charge_station_day_thursday";
    private static final String FRIDAY= "charge_station_day_friday";
    private static final String SATURDAY= "charge_station_day_saturday";
    private static final String SUNDAY= "charge_station_day_sunday";

    /**
     * 冒号
     */
    public static final String SYMBOL_COLON = ":";
    /**
     * 轻应用用户token key前缀
     */
    public static final String USER_TOKEN_KEY = "uc:token:8:";

    private final LoadingCache<Long, Optional<OpLocationElasticDTO>> locationCache = CacheBuilder.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .initialCapacity(100)
            .maximumSize(1000)
            .build(new CacheLoader<Long, Optional<OpLocationElasticDTO>>() {
                @Override
                public Optional<OpLocationElasticDTO> load(Long id) {
                    return findByLocationId(id);
                }
            });

    public OpLocationServiceImpl(OpLocationRepository opLocationRepository) {
        this.opLocationRepository = opLocationRepository;
    }

    @Override
    public Result<Long> add(OpLocationDTO opLocationDTO) {
        OpLocationEntity opLocationEntity;
        if (opLocationDTO == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("param is null"));
        }

        opLocationDTO.setId(null);
        opLocationDTO.setHubjectCheck(false);
        opLocationDTO.setLocationType(null);
        opLocationDTO.setConditionsValue(null);

        // 查询场站名称是否已存在
        if (this.isNameExistence(opLocationDTO)) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NAME_REPEATED);
        }

        opLocationEntity = opLocationRepository.add(opLocationDTO);

        //添加成本电价
        LocationPriceDTO locationPriceDTO = opLocationDTO.getLocationPriceDTO();
        if (locationPriceDTO != null){
            locationPriceDTO.setLocationId(opLocationEntity.getId());
            Long add = opLocationPriceService.add(locationPriceDTO);
            log.info("OpLocationServiceImpl.addLocationByNew.add={}", add);
        }
        return Result.ofSucceed(opLocationEntity.getId());
    }

    private boolean isNameExistence(OpLocationDTO opLocationDTO) {

        if (opLocationDTO == null
                || StringUtils.isBlank(opLocationDTO.getName())) {
            throw new MessageCodeException(PileBaseEnum.STATION_NAME_CANNOT_BE_EMPTY);
        }
        Long sellerId = UserUtil.getSellerId();
        LambdaQueryWrapper<OpLocationEntity> query = Wrappers.lambdaQuery(OpLocationEntity.class)
                .eq(OpLocationEntity::getName, opLocationDTO.getName())
                .eq(OpLocationEntity::getOperatorId, sellerId)
                .eq(OpLocationEntity::getDeleted, Boolean.FALSE)
                .ne(opLocationDTO.getId() != null, OpLocationEntity::getId, opLocationDTO.getId());
        return opLocationRepository.count(query) > 0;
    }

    @Override
    public Result<Boolean> delete(Long id) {
        OpLocationEntity opLocationEntity = opLocationRepository.selectById(id);
        Boolean bo = opLocationRepository.delete(id);
        if (bo != null && bo) {
            opLocationPriceService.deleteByLocationId(id);
        }
        if (Objects.nonNull(opLocationEntity)) {
            OpLocationDTO opLocationDTO = new OpLocationDTO();
            opLocationDTO.setName(opLocationEntity.getName());
            opLocationDTO.setId(opLocationEntity.getId());
            mqSender.send(AmqpConstant.LOCATION_DELETE, opLocationDTO);
        }
        return Result.ofSucceed(bo);
    }

    @Override
    public Result<Boolean> update(OpLocationDTO opLocationDTO) {
        StopWatch stopWatch = new StopWatch("updateOpLocationDTO更新场站信息");
        Boolean bo = false;
        if (opLocationDTO == null
                || opLocationDTO.getId() == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("param doesn't exist!"));
        }

        OpLocationEntity opLocationEntity = opLocationMapper.selectById(opLocationDTO.getId());
        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }

        opLocationDTO.setHubjectCheck(opLocationEntity.getHubjectCheck());
        opLocationDTO.setConditionsValue(null);
        opLocationDTO.setLocationType(opLocationEntity.getLocationType());

        // 校验名称是否已存在
        stopWatch.start("校验名称是否已存在");

        // 查询场站名称是否已存在
        if (this.isNameExistence(opLocationDTO)) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NAME_REPEATED);
        }
        stopWatch.stop();

        stopWatch.start("doUpdate");
        //pushhub 字段仅判断推送到hub的数据是删除还是新增 0-默认,1-删除,2-新增
        Map<Integer, List<OpLocationPileEvseEntity>> map = changeHubjectLocationStatus(opLocationDTO);

        //更新成本电价
        LocationPriceDTO locationPriceDTO = opLocationDTO.getLocationPriceDTO();
        if (locationPriceDTO != null) {
            locationPriceDTO.setLocationId(opLocationDTO.getId());
            if (locationPriceDTO.getId() == null) {
                locationPriceDTO.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
                opLocationPriceService.add(locationPriceDTO);
            } else {
                //禁用需检查是否关联
                if (opLocationDTO.getPriceFlag() != null && opLocationDTO.getPriceFlag() == 0) {
                    if (opLocationPileGroupService.checkStatus(opLocationDTO.getId(), locationPriceDTO.getId())) {
                        throw new MessageCodeException(PileBaseEnum.LOCATION_PRICE_ASSOCIATE_PILE);
                    }
                }
                opLocationPriceService.edit(locationPriceDTO);
            }
        }

        bo = opLocationRepository.update(opLocationDTO);

        stopWatch.stop();
        stopWatch.start("sendMessage");
        mqSender.send(AmqpConstant.LOCATION_UPDATE, opLocationDTO);
        stopWatch.stop();
        stopWatch.start("新运维平台同步更新桩的位置信息");
        //重构后
        try {
            log.info("新运维平台同步更新桩的位置信息: {}", opLocationDTO.getId());
            UpdatePileLocationDTO updatePileV2DTO = new UpdatePileLocationDTO();
            updatePileV2DTO.setLocationId(String.valueOf(opLocationDTO.getId()));
            updatePileV2DTO.setMerchantId(String.valueOf(UserUtil.getSellerId()));
            updatePileV2DTO.setCountry(opLocationDTO.getCountry());
            updatePileV2DTO.setState(opLocationDTO.getProvince());
            updatePileV2DTO.setCity(opLocationDTO.getCity());
            updatePileV2DTO.setZipCode(opLocationDTO.getPostalCode());
            updatePileV2DTO.setAddress(opLocationDTO.getAddress());
            updatePileV2DTO.setLongitude(opLocationDTO.getLongitude());
            updatePileV2DTO.setLatitude(opLocationDTO.getLatitude());

            opsMgmtClient.baseEditPileLocation(updatePileV2DTO);
        } catch (Exception e) {
            log.error("update ops-mgmt pile_location_information error: {}", e);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        //同步充电设备扩展类
        log.info("update location send to mq location={}", opLocationDTO.getId());
        rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_UPDATE_ROUTE, JSON.toJSONString(opLocationDTO.getId()));

        try {
            // 场站信息修改成功之后需要发送MQ消息到车队那边
            List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByLocationId(opLocationDTO.getId());
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                // 获取userId
                Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
                // 构造入参对象
                List<EvseInfoModifyDTO> evseInfoModifyDTOList = new ArrayList<>();
                for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                    if (org.apache.commons.lang3.StringUtils.isNotBlank(opLocationEvseElasticDTO.getEvseSn())) {
                        EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                        evseInfoModifyDTO.setEvseSn(opLocationEvseElasticDTO.getEvseSn());
                        evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.UPDATE.getCode());
                        evseInfoModifyDTOList.add(evseInfoModifyDTO);
                    }
                }

                // 异步推送
                ThreadPoolUtil.getExecutor().execute(() -> opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTOList, opLocationDTO.getId(), userId));
            }
        } catch (Exception e) {
            log.error("场站信息修改成功之后，推送充电枪信息给车队那边出现异常", e);
        }

        return Result.ofSucceed(bo);
    }

    /**
     * 判断hub场站状态是否改变
     *
     * @param opLocationDTO
     */
    private Map<Integer, List<OpLocationPileEvseEntity>> changeHubjectLocationStatus(OpLocationDTO opLocationDTO) {
        Map<Integer, List<OpLocationPileEvseEntity>> map = new HashMap<>();
        //判断是否关闭hubject场站
        OpLocationEntity opLocationHub = opLocationMapper.selectById(opLocationDTO.getId());
        if (opLocationHub == null) {
            return null;
        }
        if (null != opLocationHub.getHubjectCheck() && opLocationHub.getHubjectCheck()) {
            if (opLocationDTO.getHubjectCheck() != null && !opLocationDTO.getHubjectCheck()) {
                List<Long> evseIds = new ArrayList<>();

                LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQuery = Wrappers.lambdaQuery();
                lambdaQuery.eq(OpLocationPileEvseEntity::getLocationId, opLocationDTO.getId())
                        .eq(OpLocationPileEvseEntity::getDeleted, 0);
                List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseRepository.list(lambdaQuery);
                if (CollectionUtils.isNotEmpty(opLocationPileEvseEntities)) {
                    opLocationPileEvseEntities.forEach(opLocationPileEvseEntity -> {
                        List<Long> evses = new ArrayList<>();
                        String evseList = opLocationPileEvseEntity.getEvseList();
                        if (StringUtils.isNotEmpty(evseList)) {
                            evses = JSON.parseArray(evseList, Long.class);
                            evses.forEach(evseIds::add);
                        }
                    });
                }
                log.info("场站下所有的充电设备evseIds:{}", evseIds);
                evseIds.forEach(evseId -> {
                    //桩下如果充电设备在充电中,不允许删除
                    EvseDeviceStatusEnum evseStatus = monitorFeign.getEvseStatus(evseId);
                    log.info("OpLocationEvseImpl.deleteByPileId and evseStatus = " + JSON.toJSONString(evseStatus));
                    if (EvseDeviceStatusEnum.PREPARING.equals(evseStatus) ||
                            EvseDeviceStatusEnum.CHARGING.equals(evseStatus) ||
                            EvseDeviceStatusEnum.FINISHING.equals(evseStatus) ||
                            EvseDeviceStatusEnum.SUSPENDED_EVSE.equals(evseStatus) ||
                            EvseDeviceStatusEnum.SUSPENDED_EV.equals(evseStatus) ||
                            EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                        throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_STATUS);
                    }
                    BillInfoForEvseVO billInfoForEvseVO = getBillInfo(evseId);
                    if (billInfoForEvseVO != null) {
                        throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_STATUS);
                    }
                });
                map.put(1, opLocationPileEvseEntities);
                return map;
            }
        } else if (null == opLocationHub.getHubjectCheck() || !opLocationHub.getHubjectCheck()) {
            if (opLocationDTO.getHubjectCheck() != null && opLocationDTO.getHubjectCheck()) {
                LambdaQueryWrapper<OpLocationPileEvseEntity> querywrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                        .eq(OpLocationPileEvseEntity::getLocationId, opLocationDTO.getId())
                        .eq(OpLocationPileEvseEntity::getDeleted, 0);
                List<OpLocationPileEvseEntity> opLocationEvseEntityList = opLocationPileEvseRepository.list(querywrapper);
                if (CollectionUtils.isNotEmpty(opLocationEvseEntityList)) {
                    map.put(2, opLocationEvseEntityList);
                    return map;
                }
            }
        }
        return null;
    }

    private BillInfoForEvseVO getBillInfo(Long evseId) {
        try {
            Result<BillInfoForEvseVO> getCurrentOrderInfoRet = chargingFeignClient
                    .getCurrentOrderInfo(evseId);
            log.info("OpLocationEvseImpl getBillInfoForEvseVO and getCurrentOrderInfoRet = "
                    + JSON.toJSONString(getCurrentOrderInfoRet));
            return getCurrentOrderInfoRet.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseImpl.getBillInfoForEvseVO exception = ", e);
            return null;
        }
    }

    @Override
    public List<OpLocationMenuDTO> getStationMenu() {
        List<OpLocationMenuDTO> menuDTOList = opLocationRepository.getStationMenu();
        return menuDTOList;
    }

    @Override
    public Result<List<OpLocationMenuDTO>> getStationMenuBySellerId(String merchantId) {
        List<OpLocationMenuDTO> menuDTOList = opLocationRepository.getStationMenuBySellerId(merchantId);
        return Result.ofSucceed(menuDTOList);
    }

    @Override
    public Result<List<OpLocationMenuDTO>> getEMSPStationMenu() {
        List<OpLocationMenuDTO> menuDTOList = opLocationRepository.getEMSPStationMenu();
        return Result.ofSucceed(menuDTOList);
    }

    @Override
    public com.autel.cloud.base.model.Result<List<OpLocationMenuDTO>> getUserStationMenu() {
        List<OpLocationMenuDTO> menuDTOList = opLocationRepository.getUserStationMenu();
        List<DataAuthorizeDto> permissionData = new ArrayList<>();
        if (!CollectionUtils.isEmpty(menuDTOList)) {
            DataAuthorizeDto dto = new DataAuthorizeDto();
            dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
            dto.setAppId(CompetenceUtil.APP_ID);
            dto.setSellerId(LoginUserUtil.getSellerId().toString());
            dto.setUserId(LoginUserUtil.getUserId().toString());
            List<Node> data = new ArrayList<>();
            List<Node> stationData = new ArrayList<>();
            for (OpLocationMenuDTO opLocationMenuDTO : menuDTOList) {
                Node node = new Node();
                node.setLevel(SubTreeEnum.STATION.getCode());
                node.setNode(opLocationMenuDTO.getId().toString());
                stationData.add(node);
            }
            String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
            String competence = stringRedisTemplate.opsForValue().get(key);
            try {
                if (StringUtils.isBlank(competence)) {
                    UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
                    userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
                    userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
                    Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
                    if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
                        competence = userCompetence.getData().getCompetence().toString();
                    }
                }
            } catch (Exception e) {
                log.info("查询用户权限报错,e={}",e);
            }
            if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
                Node seller = new Node();
                seller.setNode(LoginUserUtil.getSellerId().toString());
                seller.setLevel(SubTreeEnum.SELLER.getCode());
                seller.setChildNodes(stationData);
                data.add(seller);
                dto.setData(data);
            }else {
                dto.setData(stationData);
            }
            permissionData.add(dto);
        }
        return CompetenceUtil.encapsulation(menuDTOList,permissionData);
    }

    @Override
    public List<OpLocationDTO> getList(List<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return null;
        }
        List<OpLocationDTO> resultList = new ArrayList<>();
        List<OpLocationElasticDTO> list = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("id", ids)).withSourceFilter(new FetchSourceFilter(new String[]{"id", "name"}, null)).build(), OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isNotEmpty(list)) {
            list.stream().forEach(e -> {
                OpLocationDTO dto = new OpLocationDTO();
                dto.setId(e.getId());
                dto.setName(e.getName());
                resultList.add(dto);
            });
        }
        return resultList;
    }

    @Override
    public List<SelectLocationForFleetVO> selectLocationForFleet(SelectLocationForFleetDTO selectLocationForFleetDTO) {

        log.info("===>>> OpLocationServiceImpl.selectLocationForFleet selectLocationForFleetDTO : {}",
                JSON.toJSONString(selectLocationForFleetDTO));

        if (selectLocationForFleetDTO == null) {
            return null;
        }

        if (selectLocationForFleetDTO.getSellerId() == null) {
            Long sellerId = LoginUserUtil.getSellerId();
            if (sellerId == null) {
                return null;
            }
            selectLocationForFleetDTO.setSellerId(sellerId);
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("===>>> OpLocationServiceImpl.selectLocationForFleet getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        if (getLocationIdsResult == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getLocationIdsResult.getData())) {
            return null;
        }

        List<Long> locationIdList = new ArrayList<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(selectLocationForFleetDTO.getLocationIdList())) {
            for (Long id : selectLocationForFleetDTO.getLocationIdList()) {
                if (getLocationIdsResult.getData().contains(id)) {
                    locationIdList.add(id);
                }
            }
        } else {
            locationIdList.addAll(getLocationIdsResult.getData());
        }


        List<OpLocationEntity> opLocationEntityList = opLocationMapper.selectList(new LambdaQueryWrapper<OpLocationEntity>()
                .eq(OpLocationEntity::getOperatorId, selectLocationForFleetDTO.getSellerId())
                .eq(OpLocationEntity::getDeleted, 0)
                .in(com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(locationIdList), OpLocationEntity::getId, locationIdList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEntityList)) {
            return null;
        }

        List<SelectLocationForFleetVO> selectLocationForFleetVOList = new ArrayList<>();
        opLocationEntityList.forEach(val -> {
            SelectLocationForFleetVO selectLocationForFleetVO = new SelectLocationForFleetVO();
            selectLocationForFleetVO.setLocationId(val.getId());
            selectLocationForFleetVO.setLocationName(val.getName());
            selectLocationForFleetVO.setLongitude(val.getLongitude());
            selectLocationForFleetVO.setLatitude(val.getLatitude());
            selectLocationForFleetVO.setLocationAddress(val.getAddress());
            selectLocationForFleetVOList.add(selectLocationForFleetVO);
        });
        return selectLocationForFleetVOList;
    }

    @Override
    public Long getMaxPowerLocationId(Long sellerId) {

        log.info("===>>> OpLocationServiceImpl.getMaxPowerLocationId sellerId : {}",
                JSON.toJSONString(sellerId));

        if (sellerId == null) {
            return null;
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("===>>> OpLocationServiceImpl.getMaxPowerLocationId getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        if (getLocationIdsResult == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getLocationIdsResult.getData())) {
            return null;
        }


        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findAllByLocationIdIn(new HashSet<>(getLocationIdsResult.getData()));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileEvseElasticDTOList)) {
            return null;
        }

        Map<Long, List<OpLocationPileEvseElasticDTO>> locationIdAndOpLocationPileEvseElasticDTOListMap = opLocationPileEvseElasticDTOList
                .stream()
                .collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));

        List<LocationPowerDTO> locationPowerDTOList = new ArrayList<>();
        locationIdAndOpLocationPileEvseElasticDTOListMap.forEach((key, value) -> {
            LocationPowerDTO locationPowerDTO = new LocationPowerDTO();
            locationPowerDTO.setLocationId(key);
            BigDecimal totalPowerOfPile = new BigDecimal("0");
            for (OpLocationPileEvseElasticDTO item : value) {
                Double power = item.getPower();
                if (power != null) {
                    totalPowerOfPile = totalPowerOfPile.add(new BigDecimal(String.valueOf(power)));
                }
            }
            locationPowerDTO.setTotalPowerOfPile(totalPowerOfPile);
            locationPowerDTOList.add(locationPowerDTO);
        });

        return locationPowerDTOList
                .stream()
                .max(Comparator.comparing(LocationPowerDTO::getTotalPowerOfPile))
                .get()
                .getLocationId();
    }

    @Override
    public List<LocationDataVO> getLocationData(Long userId) {

        log.info("===>>> OpLocationServiceImpl.getLocationData userId : {}",
                JSON.toJSONString(userId));

        if (userId == null) {
            return null;
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("===>>> OpLocationServiceImpl.getLocationData getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        if (getLocationIdsResult == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getLocationIdsResult.getData())) {
            return null;
        }

        List<Long> locationIdList = getLocationIdsResult.getData();

        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findAllByLocationIdIn(new HashSet<>(locationIdList));
        Map<Long, List<OpLocationPileEvseElasticDTO>> locationIdAndOpLocationPileEvseElasticDTOListMap = new HashMap<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
            locationIdAndOpLocationPileEvseElasticDTOListMap.putAll(opLocationPileEvseElasticDTOList
                    .stream()
                    .collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId)));
        }

        List<LocationDataVO> locationDataVOS = new ArrayList<>();
        for (Long locationId : locationIdList) {

            if (locationId == null) {
                continue;
            }

            LocationDataVO locationDataVO = new LocationDataVO();

            locationDataVO.setLocationId(locationId);
            List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOLists = locationIdAndOpLocationPileEvseElasticDTOListMap.get(locationId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileEvseElasticDTOLists)) {
                locationDataVO.setTotalPilePowerOfLocation(new BigDecimal("0"));
            } else {
                BigDecimal totalPilePowerOfLocation = new BigDecimal("0");
                for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOLists) {
                    Double power = opLocationPileEvseElasticDTO.getPower();
                    if (power != null) {
                        totalPilePowerOfLocation = totalPilePowerOfLocation.add(new BigDecimal(String.valueOf(power)));
                    }
                }
                locationDataVO.setTotalPilePowerOfLocation(totalPilePowerOfLocation);
            }

            locationDataVOS.add(locationDataVO);
        }
        return locationDataVOS;
    }

    @Override
    public List<Long> getLocationIdBySellerId(Long sellerId) {
        return opLocationRepository.getLocationIdBySellerId(sellerId);
    }

    @Override
    public Page<SelectLocationInfoForEroamingVO> selectLocationInfoForEroaming(PageDTO pageDTO) {

        log.info("===>>> OpLocationServiceImpl.selectLocationInfoForEroaming pageDTO : {}",
                JSON.toJSONString(pageDTO));

        Page<SelectLocationInfoForEroamingVO> result = new Page<>(pageDTO.getPage(), pageDTO.getPageSize());

        if (LoginUserHolder.getLoginUser() == null
                || LoginUserHolder.getLoginUser().getPayload() == null
                || LoginUserHolder.getLoginUser().getPayload().getSellerId() == null) {
            return result;
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("===>>> OpLocationServiceImpl.selectLocationInfoForEroaming getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        if (getLocationIdsResult == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getLocationIdsResult.getData())) {
            return result;
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        queryBuilder.must(QueryBuilders.termsQuery("id", getLocationIdsResult.getData()));
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pageDTO.getSearchValue())) {
            queryBuilder.must(QueryBuilders.wildcardQuery("name", String.format("*%s*", QueryParserBase.escape(pageDTO.getSearchValue()))));
        }

        Pageable pageable = PageRequest.of(pageDTO.getPage() - 1,
                pageDTO.getPageSize());

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("id").order(SortOrder.DESC))
                .withTrackTotalHits(true)
                .withPageable(pageable)
                .build();

        SearchHits<OpLocationElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationElasticDTO.class);
        SearchPage<OpLocationElasticDTO> page = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(list)) {
            return result;
        }

        List<SelectLocationInfoForEroamingVO> selectLocationInfoForEroamingVOS = new ArrayList<>();
        for (OpLocationElasticDTO opLocationElasticDTO : list) {
            SelectLocationInfoForEroamingVO selectLocationInfoForEroamingVO = new SelectLocationInfoForEroamingVO();
            selectLocationInfoForEroamingVO.setLocationId(opLocationElasticDTO.getId());
            selectLocationInfoForEroamingVO.setLocationName(opLocationElasticDTO.getName());
            selectLocationInfoForEroamingVO.setAddress(opLocationElasticDTO.getAddress());
            selectLocationInfoForEroamingVO.setLocationType(opLocationElasticDTO.getLocationType());
            selectLocationInfoForEroamingVO.setHubjectCheck(opLocationElasticDTO.getHubjectCheck() == null ? false : opLocationElasticDTO.getHubjectCheck());
            selectLocationInfoForEroamingVOS.add(selectLocationInfoForEroamingVO);
        }

        result.setRecords(selectLocationInfoForEroamingVOS);
        result.setTotal(page.getTotalElements());
        result.setSize(page.getSize());
        result.setPages(page.getTotalPages());
        result.setCurrent(pageDTO.getPage());
        return result;
    }

    @Override
    public Boolean setLocationTypeForLocation(SetLocationTypeForLocationDTO setLocationTypeForLocationDTO) {

        log.info("===>>> OpLocationServiceImpl.setLocationTypeForLocation setLocationTypeForLocationDTO : {}",
                JSON.toJSONString(setLocationTypeForLocationDTO));

        OpLocationEntity opLocationEntity = opLocationMapper.selectOne(new LambdaQueryWrapper<OpLocationEntity>()
                .eq(OpLocationEntity::getId, setLocationTypeForLocationDTO.getLocationId())
                .eq(OpLocationEntity::getDeleted, 0)
                .eq(OpLocationEntity::getOperatorId, LoginUserHolder.getLoginUser().getPayload().getSellerId()));

        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }

        opLocationEntity.setLocationType(setLocationTypeForLocationDTO.getLocationType());
        opLocationEntity.setUpdatedAt(System.currentTimeMillis());
        int count = opLocationMapper.updateById(opLocationEntity);
        if (count > 0) {
            Optional<OpLocationElasticDTO> optional = opLocationElastic.findById(opLocationEntity.getId());
            if (optional.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optional.get();
                opLocationElasticDTO.setLocationType(opLocationEntity.getLocationType());
                opLocationElasticDTO.setUpdatedAt(opLocationEntity.getUpdatedAt());
                opLocationElastic.save(opLocationElasticDTO);
            }

            if(opLocationEntity.getHubjectCheck()){
                // 实时推送场站数据到互联互通平台
                pileBaseAsync.syncEroamingLocationType(opLocationEntity);
                pileBaseAsync.pushLocationEvseList(opLocationEntity, ActionType.update.getCode());
            }
        }
        return true;
    }

    @Override
    public Boolean setLocationEroamingForLocation(SetLocationEroamingForLocationDTO setLocationEroamingForLocationDTO) {

        log.info("===>>> OpLocationServiceImpl.setLocationEroamingForLocation setLocationEroamingForLocationDTO : {}",
                JSON.toJSONString(setLocationEroamingForLocationDTO));

        OpLocationEntity opLocationEntity = opLocationMapper.selectOne(new LambdaQueryWrapper<OpLocationEntity>()
                .eq(OpLocationEntity::getId, setLocationEroamingForLocationDTO.getLocationId())
                .eq(OpLocationEntity::getDeleted, 0)
                .eq(OpLocationEntity::getOperatorId, LoginUserHolder.getLoginUser().getPayload().getSellerId()));

        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }

        opLocationEntity.setHubjectCheck(Integer.valueOf(1).equals(setLocationEroamingForLocationDTO.getOperateType()));
        opLocationEntity.setUpdatedAt(System.currentTimeMillis());
        int count = opLocationMapper.updateById(opLocationEntity);
        if (count > 0) {
            Optional<OpLocationElasticDTO> optional = opLocationElastic.findById(opLocationEntity.getId());
            if (optional.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optional.get();
                opLocationElasticDTO.setHubjectCheck(opLocationEntity.getHubjectCheck());
                opLocationElasticDTO.setUpdatedAt(opLocationEntity.getUpdatedAt());
                opLocationElastic.save(opLocationElasticDTO);
            }
            // 实时推送场站数据到互联互通平台
            if(opLocationEntity.getHubjectCheck()){
                pileBaseAsync.syncEroamingLocation(opLocationEntity, ActionType.update.getCode());
                pileBaseAsync.pushLocationEvseList(opLocationEntity, ActionType.update.getCode());
            }else{
                pileBaseAsync.syncEroamingLocation(opLocationEntity, ActionType.delete.getCode());
                pileBaseAsync.pushLocationEvseList(opLocationEntity, ActionType.delete.getCode());
            }

        }

        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOS = opLocationPileEvseElastic.findAllByLocationId(setLocationEroamingForLocationDTO.getLocationId());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileEvseElasticDTOS)) {
            return true;
        }

        for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOS) {
            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> {
                SetPileEroamingForPileDTO setPileEroamingForPileDTO = new SetPileEroamingForPileDTO();
                setPileEroamingForPileDTO.setPileSn(opLocationPileEvseElasticDTO.getPileSn());
                setPileEroamingForPileDTO.setOperateType(setLocationEroamingForLocationDTO.getOperateType());
                opLocationEvseService.setPileEroamingForPile(setPileEroamingForPileDTO);
            }));
        }
        return true;
    }

    @Override
    public Map<Long, Boolean> batchGetSellerEroamingLocationFlag(List<Long> sellerIdList) {

        log.info("===>>> OpLocationServiceImpl.batchGetSellerEroamingLocationFlag sellerIdList : {}",
                JSON.toJSONString(sellerIdList));

        if (ObjectUtils.isEmpty(sellerIdList)) {
            return null;
        }

        Map<Long, Boolean> sellerIdAndHaveEroamingLocationFlagMap = new HashMap<>();
        sellerIdList.forEach(val -> sellerIdAndHaveEroamingLocationFlagMap.put(val, false));

        List<OpLocationEntity> opLocationEntityList = opLocationMapper.selectList(new LambdaQueryWrapper<OpLocationEntity>()
                .eq(OpLocationEntity::getDeleted, 0)
                .in(OpLocationEntity::getOperatorId, sellerIdList));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEntityList)) {
            for (OpLocationEntity opLocationEntity : opLocationEntityList) {
                if (sellerIdList.contains(opLocationEntity.getOperatorId())
                        && opLocationEntity.getHubjectCheck() != null
                        && opLocationEntity.getHubjectCheck()) {
                    sellerIdAndHaveEroamingLocationFlagMap.put(opLocationEntity.getOperatorId(), true);
                }
            }
        }
        return sellerIdAndHaveEroamingLocationFlagMap;
    }

    @Override
    public com.autel.cloud.base.model.Result<List<OpLocationMenuDTO>> getStationMenuIncludedDeleted() {
        List<OpLocationMenuDTO> menuDTOList = opLocationRepository.getStationMenuIncludedDeleted();
        List<DataAuthorizeDto> permissionData = new ArrayList<>();
        if (!CollectionUtils.isEmpty(menuDTOList)) {
            DataAuthorizeDto dto = new DataAuthorizeDto();
            dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
            dto.setAppId(CompetenceUtil.APP_ID);
            dto.setSellerId(LoginUserUtil.getSellerId().toString());
            dto.setUserId(LoginUserUtil.getUserId().toString());
            List<Node> data = new ArrayList<>();
            List<Node> stationData = new ArrayList<>();
            for (OpLocationMenuDTO opLocationMenuDTO : menuDTOList) {
                Node node = new Node();
                node.setLevel(SubTreeEnum.STATION.getCode());
                node.setNode(opLocationMenuDTO.getId().toString());
                stationData.add(node);
            }
            String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
            String competence = stringRedisTemplate.opsForValue().get(key);
            try {
                if (StringUtils.isBlank(competence)) {
                    UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
                    userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
                    userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
                    Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
                    if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
                        competence = userCompetence.getData().getCompetence().toString();
                    }
                }
            } catch (Exception e) {
                log.info("查询用户权限报错,e={}", e);
            }
            if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
                Node seller = new Node();
                seller.setNode(LoginUserUtil.getSellerId().toString());
                seller.setLevel(SubTreeEnum.SELLER.getCode());
                seller.setChildNodes(stationData);
                data.add(seller);
                dto.setData(data);
            } else {
                dto.setData(stationData);
            }
            permissionData.add(dto);
        }
        return CompetenceUtil.encapsulation(menuDTOList, permissionData);
    }

    @Override
    public Page<LocationBaseVO> getLocationDropDownPage(OpLocationMenuQueryDto pageDTO) {

        log.info("===>>> OpLocationServiceImpl.getLocationDropDownPage pageDTO : {}",
                JSON.toJSONString(pageDTO));

        Page<LocationBaseVO> result =  new Page<>(pageDTO.getPage(), pageDTO.getPageSize());
        try {
            result = opLocationRepository.getLocationDropDownPage(pageDTO);
        } catch (Exception e) {

            log.error("===>>> OpLocationServiceImpl.getLocationDropDownPage error : {}",
                    e);

        }
        return result;
    }


    @Override
    public Page<OpLocationMenuDTO> getStationMenuIncludedDeletedWithoutAuthorize(PageDTO pageDTO, Boolean isIncludeDeleted) {
        return opLocationRepository.getStationMenuIncludedDeletedWithoutAuthorize(pageDTO,isIncludeDeleted);
    }


    @Override
    public List<OpLocationDTO> getOpLocationBySellerId(QueryOplcationDTO queryOplcationDTO) {
        List<OpLocationDTO> menuDTOList = opLocationRepository.getOpLocationBySellerId(queryOplcationDTO);
        return menuDTOList;
    }

    /**
     * 场站是否存在
     *
     * @param groupIds 组织机构id集合
     * @return 场站是否存在
     */
    @Override
    public Result<Boolean> existsByGroupIdIn(List<Long> groupIds) {
        return Result.ofSucceed(opLocationRepository.existsByGroupIdIn(groupIds));
    }

    @Override
    public Result<List<OpLocationOperationDTO>> getLocationList(List<String> groupIdList) {
        List<Long> idList = groupIdList.stream().map(Long::valueOf).collect(Collectors.toList());
        LambdaQueryWrapper<OpLocationOperationEntity> qw = new LambdaQueryWrapper<>();
        qw.select(OpLocationOperationEntity::getLocationId, OpLocationOperationEntity::getGroupId, OpLocationOperationEntity::getId);
        qw.in(OpLocationOperationEntity::getGroupId, idList);
        List<OpLocationOperationEntity> locationList = opLocationOperationMapper.selectList(qw);
        if (CollectionUtils.isEmpty(locationList)) {
            return Result.ofSucceed(new ArrayList<>());
        }
        List<Long> locationIdList = locationList.stream().map(OpLocationOperationEntity::getLocationId).collect(Collectors.toList());
        List<OpLocationEntity> entityList = opLocationMapper.selectBatchIds(locationIdList);
        Map<Long, String> map = new HashMap<>();
        if (!CollectionUtils.isEmpty(entityList)) {
            map = entityList.stream().collect(Collectors.toMap(OpLocationEntity::getId, OpLocationEntity::getName, (e1, e2) -> e1));
        }
        List<OpLocationOperationDTO> dtoList = new ArrayList<>();
        for (OpLocationOperationEntity location : locationList) {
            OpLocationOperationDTO dto = new OpLocationOperationDTO();
            dto.setId(String.valueOf(location.getId()));
            dto.setLocationId(String.valueOf(location.getLocationId()));
            dto.setGroupId(String.valueOf(location.getGroupId()));
            dto.setLocationName(map.get(location.getLocationId()));
            dtoList.add(dto);
        }
        return Result.ofSucceed(dtoList);
    }

    @Override
    public Result<Page<OpLocationDTO>> pages(OpLocationQueryDTO opLocationQueryDTO) {
        Page<OpLocationDTO> opLocationDTOPage = null;
        try {
            List<Long> locationId = null;
            if (opLocationQueryDTO.getName() != null && !opLocationQueryDTO.getName().equals("")) {
                locationId = opLocationRepository.selectByName(opLocationQueryDTO);
                if (locationId.isEmpty()) {
                    return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("location doesn't exist !"));
                }
            }
            opLocationDTOPage = opLocationRepository.pages(opLocationQueryDTO, locationId);
            if (opLocationDTOPage.getRecords().isEmpty()) {
                return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("location doesn't exist!"));
            }
        } catch (Exception e) {
            log.error("page method exception = ", e);
        }
        return Result.ofSucceed(opLocationDTOPage);
    }

    @Override
    public Result<OpLocationDTO> details(Long id) {
        OpLocationDTO opLocationDTO = opLocationRepository.details(id);
        if (opLocationDTO != null) {
            LambdaQueryWrapper<OpCountryEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.eq(OpCountryEntity::getLanguage, opLocationDTO.getCountry())
                    .eq(OpCountryEntity::getAlpha2Code, LocaleContextHolder.getLocale()).eq(OpCountryEntity::getDeleted, Boolean.FALSE);
            OpCountryEntity opCountryEntity = opCountryRepository.getOne(queryWrapper);
            if (opCountryEntity != null) {
                opLocationDTO.setCountryName(opCountryEntity.getName());
            }
        }
        return Result.ofSucceed(opLocationDTO);
    }

    private LocationPriceDTO toLocationPriceDto(OpLocationPriceVO opLocationPriceVO) {
        LocationPriceDTO dto = new LocationPriceDTO();
        BeanUtils.copyProperties(opLocationPriceVO, dto);
        List<OpLocationPriceDetailVO> priceDetails = opLocationPriceVO.getPriceDetails();
        if (CollectionUtils.isNotEmpty(priceDetails)) {
            dto.setPriceDetails(priceDetails.stream().map(vo -> {
                LocationPriceDetailDTO priceDetailDTO = new LocationPriceDetailDTO();
                BeanUtils.copyProperties(vo, priceDetailDTO);
                List<OpLocationPriceDetailVO.DayPriceVO> dayPriceList = vo.getDayPriceList();
                priceDetailDTO.setDayPriceList(dayPriceList.stream().map(dayPriceVO -> {
                    LocationPriceDetailDTO.DayPriceDTO priceDTO = new LocationPriceDetailDTO.DayPriceDTO();
                    BeanUtils.copyProperties(dayPriceVO, priceDTO);
                    return priceDTO;
                }).collect(Collectors.toList()));
                return priceDetailDTO;
            }).collect(Collectors.toList()));
            //排序
            List<LocationPriceDetailDTO> sortList = dto.getPriceDetails().stream().sorted((f, s) -> (int) (f.getId() - s.getId())).collect(Collectors.toList());
            dto.setPriceDetails(sortList);
        }
        return dto;
    }

    @Override
    public Result<OpLocationAddressDTO> getLocationAddress(Long id) {
        return opLocationRepository.getLocationAddress(id);
//        Result<OpLocationAddressDTO> locationAddress = opLocationRepository.getLocationAddress(id);
//        List<DataAuthorizeDto> permissionData = new ArrayList<>();
//        OpLocationAddressDTO opLocationAddressDTO = locationAddress.getData();
//        if (ObjectUtils.isEmpty(opLocationAddressDTO)) {
//            return CompetenceUtil.encapsulation(opLocationAddressDTO,permissionData);
//        }
//        DataAuthorizeDto dto = new DataAuthorizeDto();
//        dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
//        dto.setAppId(CompetenceUtil.APP_ID);
//        dto.setSellerId(LoginUserUtil.getSellerId().toString());
//        dto.setUserId(LoginUserUtil.getUserId().toString());
//        List<Node> data = new ArrayList<>();
//        List<Node> stationData = new ArrayList<>();
//        Node node = new Node();
//        node.setLevel(SubTreeEnum.STATION.getCode());
//        node.setNode(opLocationAddressDTO.getId().toString());
//        stationData.add(node);
//        String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
//        String competence = stringRedisTemplate.opsForValue().get(key);
//        try {
//            if (StringUtils.isBlank(competence)) {
//                UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
//                userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
//                userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
//                Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
//                if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
//                    competence = userCompetence.getData().getCompetence().toString();
//                }
//            }
//        } catch (Exception e) {
//            log.info("查询用户权限报错,e={}",e);
//        }
//        if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
//            Node seller = new Node();
//            seller.setNode(LoginUserUtil.getSellerId().toString());
//            seller.setLevel(SubTreeEnum.SELLER.getCode());
//            seller.setChildNodes(stationData);
//            data.add(seller);
//            dto.setData(data);
//        }else {
//            dto.setData(stationData);
//        }
//        permissionData.add(dto);
//        return CompetenceUtil.encapsulation(opLocationAddressDTO,permissionData);

    }

    @Override
    public void downLocationXls(HttpServletRequest request, HttpServletResponse response) {
        String[] titles = new String[]{
                "location.name",
                "location.deleted",
                "location.status",
                "location.type",
                "location.address",
                "location.city",
                "location.postal.code",
                "location.country",
                "location.latitude",
                "location.longitude",
                "location.operator.id",
                "location.sub.operator.id",
                "location.owner.id",
                "location.time.zone",
                "location.charging.when.closed",
                "location.operation.type",
                "location.operation.state",
                "location.operation.service.tel",
                "location.operation.announcement",
                "location.operation.open.type",
                "location.operation.app.show",
                "location.operation.trans.power",
                "location.operation.billing.rule",
                "open.time.date.value",
                "open.time.period.begin",
                "open.time.period.end",
                "open.time.type",
                "open.time.date.type",
                "image.url",
                "image.thumbnail",
                "image.type",
                "image.width",
                "image.height"
        };
        HSSFWorkbook wb = null;
        try {
            String fileName = "Location_import.xls";
            log.info("开始下载location_import.xls" + fileName);
            //创建HSSFWorkbook对象(excel的文档对象)
            wb = new HSSFWorkbook();
            //建立新的sheet对象（excel的表单）
            HSSFSheet sheet = wb.createSheet("工作表");
            //在sheet里创建第一行，参数为行索引(excel的行)，可以是0～65535之间的任何一个
            HSSFRow row1 = sheet.createRow(0);
            //创建单元格（excel的单元格，参数为列索引，可以是0～255之间的任何一个
            HSSFCell cell = row1.createCell(0);
            //设置单元格内容
            cell.setCellValue("说明：请勿修改格式布局，否则将会导致数据导入失败！");
            //合并单元格CellRangeAddress构造参数依次表示起始行，截至行，起始列， 截至列
            sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, titles.length));
            //在sheet里创建第二行
            HSSFRow row2 = sheet.createRow(1);
            //创建单元格并设置单元格内容
            for (int i = 0; i < titles.length; i++) {
                ResultCodeEnum resultCodeEnum = LocaleResultUtil.result(titles[i]);
                row2.createCell(i).setCellValue(resultCodeEnum.getMessage());
            }

            OutputStream output = null;
            try {
                //输出Excel文件
                output = response.getOutputStream();
                response.reset();
                response.setHeader("Content-disposition", "attachment; filename=location_import.xls");
                response.setContentType("application/msexcel");
                wb.write(output);
            } catch (IOException e) {
                log.error("downLocationXls:" + e);
            } finally {
                try {
                    log.info("模板文件下载成功！");
                    assert output != null;
                    output.close();
                } catch (IOException e) {
                    log.error("downLocationXls:" + e);
                }
            }
        } catch (Exception e) {
            log.info(e);
        } finally {
            if (wb != null) {
                try {
                    wb.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
        }
    }

    @Override
    public Result<Boolean> importLocations(MultipartFile file) {

        Workbook workbook = null;
        String filename = file.getOriginalFilename();
        try {
            if (filename != null) {
                if (filename.endsWith("xls")) {
                    workbook = new HSSFWorkbook(file.getInputStream());
                } else if (filename.endsWith("xlsx")) {
                    workbook = new XSSFWorkbook(file.getInputStream());
                } else {
                    return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("文件不是Excel文件！"));
                }
            } else {
                return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("文件为空！"));
            }
            Sheet sheet = workbook.getSheet("工作表");
            int lastRowNum = sheet.getLastRowNum();
            if (lastRowNum <= 2) {
                return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("请最少填写一行！"));
            }
            for (int i = 3; i < lastRowNum + 1; i++) {
                Row row = sheet.getRow(i);
                if (row != null) {
                    // 读取cell单元格的内容
                    OpLocationDTO opLocationDTO = OpLocationConvert.excelToOpLocationDTO(row);
                    // 将一条数据插入数据库中
                    add(opLocationDTO);
                }
            }
        } catch (Exception e) {
            log.error(e);
        }
        return Result.ofSucceed(Boolean.TRUE);
    }


    @Override
    public Result<List<OpLocationDTO>> getStationMapInfo() {
        List<OpLocationDTO> list = opLocationRepository.getStationMapList();
        return Result.ofSucceed(list);
    }

    /**
     * 5月新版场站分页
     *
     * @param opLocationPageDTO 检索对象
     * @return 5月新版场站分页
     */
    @Override
    public Result<Page<OpLocationPageVO>> page(OpLocationPageDTO opLocationPageDTO) {
        Page<OpLocationPageVO> stationPageVOS = opLocationRepository.stationPage(opLocationPageDTO);
        return Result.ofSucceed(stationPageVOS);
    }

    /**
     * 5月新版场站详情查询
     *
     * @param id 场站id
     * @return 场站详情
     */
    @Override
    public com.autel.cloud.base.model.Result<OpLocationDetailVO> detail(Long id) {
        OpLocationDetailVO opLocationDetailVO = opLocationRepository.detail(id);
        //查询成本电价
        List<OpLocationPriceVO> priceVOList = opLocationPriceService.getList(id);
        if (!CollectionUtils.isEmpty(priceVOList)) {
            log.error("details, getList={}", JSON.toJSONString(priceVOList));
            LocationPriceDTO dto = this.toLocationPriceDto(priceVOList.get(0));
            opLocationDetailVO.setLocationPriceDTO(dto);
        }
        if(opLocationDetailVO.getPayMethod() == null){
            opLocationDetailVO.setPayMethod(PayMethodEnum.PAY_LATER.getValue());
        }
        List<DataAuthorizeDto> permissionData = new ArrayList<>();
        if (!ObjectUtils.isEmpty(opLocationDetailVO)) {
            DataAuthorizeDto dto = new DataAuthorizeDto();
            dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
            dto.setAppId(CompetenceUtil.APP_ID);
            dto.setSellerId(LoginUserUtil.getSellerId().toString());
            dto.setUserId(LoginUserUtil.getUserId().toString());
            List<Node> data = new ArrayList<>();
            List<Node> stationData = new ArrayList<>();
            Node node = new Node();
            node.setLevel(SubTreeEnum.STATION.getCode());
            node.setNode(opLocationDetailVO.getId().toString());
            stationData.add(node);
            String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
            String competence = stringRedisTemplate.opsForValue().get(key);
            try {
                if (StringUtils.isBlank(competence)) {
                    UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
                    userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
                    userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
                    Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
                    if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
                        competence = userCompetence.getData().getCompetence().toString();
                    }
                }
            } catch (Exception e) {
                log.info("查询用户权限报错,e={}",e);
            }
            if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
                Node seller = new Node();
                seller.setNode(LoginUserUtil.getSellerId().toString());
                seller.setLevel(SubTreeEnum.SELLER.getCode());
                seller.setChildNodes(stationData);
                data.add(seller);
                dto.setData(data);
            }else {
                dto.setData(stationData);
            }
            permissionData.add(dto);
        }
        return CompetenceUtil.encapsulation(opLocationDetailVO,permissionData);
    }

    /**
     * APP地图搜索场站
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 场站数据
     */
    @Override
    public Result<List<OpLocationMapQueryVO>> mapQuery(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        List<OpLocationMapQueryVO> locationList = opLocationRepository.mapQuery2(opLocationMapQueryDTO);
        return Result.ofSucceed(locationList);
    }

    /**
     * 地图找桩支持搜索
     *
     * @param opLocationMapQueryPageDTO 检索对象
     * @return 场站数据
     */
    @Override
    public Result<Page<OpLocationMapQueryVO>> mapQueryPile(OpLocationMapQueryPageDTO opLocationMapQueryPageDTO) {
        return opLocationRepository.mapQueryPile(opLocationMapQueryPageDTO);
    }

    /**
     * 地图找桩支持搜索
     *
     * @param opLocationMapQueryPageDTO 检索对象
     * @return 场站数据
     */
    @Override
    public Result<Page<OpLocationMapQueryVO>> mapQueryPileForOCPI(OpLocationMapQueryPageDTO opLocationMapQueryPageDTO) {
        return opLocationRepository.mapQueryPileForOCPI(opLocationMapQueryPageDTO);
    }

    @Override
    public Result<HubjectConfigVO> checkNeedHubject() {
        HubjectConfigVO vo = new HubjectConfigVO();
        boolean flag = oicpFeignClient.checkUserIsGray();
        vo.setEnable(flag);
        return Result.ofSucceed(vo);
    }

    /**
     * APP场站卡片
     *
     * @param opLocationCardDTO 检索对象
     * @return 返回对象
     */
    @Override
    public Result<OpLocationCardVO> stationCardData(OpLocationCardDTO opLocationCardDTO) {
        OpLocationCardVO opLocationCardVO = opLocationRepository.stationCardData(opLocationCardDTO);
        return Result.ofSucceed(opLocationCardVO);
    }

    /**
     * APP场站推荐
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 场站数据
     */
    @Override
    public Result<List<OpLocationCardVO>> recommend(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        List<OpLocationCardVO> locationList = opLocationRepository.recommend(opLocationMapQueryDTO);
        return Result.ofSucceed(locationList);
    }

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    @Override
    public Result<OpLocationAPPDetailVO> appDetail(OpLocationDetailDTO opLocationDetailDTO) {
        OpLocationAPPDetailVO opLocationAPPDetailVO = opLocationRepository.appDetail(opLocationDetailDTO);
        // （返回该场站下是否包含英标桩的标志）
        if (opLocationAPPDetailVO != null && opLocationAPPDetailVO.getStationId() != null) {
            opLocationAPPDetailVO.setContainBritainStandPileFlag(this.isContainBritainStandPile(opLocationAPPDetailVO.getStationId()));
        }

        // 查询钱包余额信息
        try{
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            if(AppIdEnum.PLANET_EV.getValue().toString().equals(appId)){
                // 查询钱包金额信息
                Long currentUserId = LoginUserHolder.getLoginUser() == null ? null : LoginUserHolder.getLoginUser().getId();

                WalletBalanceVo walletBalanceVo = queryWalletBalanceInfo(currentUserId.toString(),appId);
                log.info("queryWalletBalanceInfo walletBalanceVo :{}",JSON.toJSONString(walletBalanceVo));
                if(walletBalanceVo != null){
                    opLocationAPPDetailVO.setCurrencySign(walletBalanceVo.getCurrencySign());
                    opLocationAPPDetailVO.setWalletBalance(walletBalanceVo.getWalletBalance());
                    opLocationAPPDetailVO.setChargeAmountLimit(walletBalanceVo.getChargeAmountLimit());
                    opLocationAPPDetailVO.setStopChargeAmoutLimit(walletBalanceVo.getStopChargeAmoutLimit());
                    opLocationAPPDetailVO.setStopChargeLimitStatus(walletBalanceVo.getStopChargeLimitStatus());
                    opLocationAPPDetailVO.setChargeLimitStatus(walletBalanceVo.getChargeLimitStatus());
                    opLocationAPPDetailVO.setWalletStatus(walletBalanceVo.getWalletStatus());
                    opLocationAPPDetailVO.setContactEmail(walletBalanceVo.getContactEmail());
                }
            }
        }catch(Exception e){
            log.error("query wallet balance error,e:{}",e.getMessage());
        }


        return Result.ofSucceed(opLocationAPPDetailVO);
    }




    private WalletBalanceVo queryWalletBalanceInfo(String userId,String appId) {

        WalletBalanceVo walletBalanceVo = new WalletBalanceVo();

        // 1.查询钱包余额信息
        AccountDetail accountDetail = queryWalletBlance(userId,appId);
        if(accountDetail != null){
            walletBalanceVo.setWalletBalance(new BigDecimal(accountDetail.getAmount()));
            walletBalanceVo.setCurrencySign(getCurrencyTypeEnumByCurrencyType(accountDetail.getCurrency()).getCurrencySign());
            walletBalanceVo.setWalletStatus(accountDetail.getStatus());
            walletBalanceVo.setContactEmail(contactEmail);
        }else{
            walletBalanceVo.setWalletBalance(BigDecimal.ZERO);
            walletBalanceVo.setCurrencySign(CurrencyTypeEnum.THB.getCurrencySign());
            walletBalanceVo.setWalletStatus(WalletStatusEnum.WALLET_NOT_OPENED.getValue());
            walletBalanceVo.setContactEmail(contactEmail);
        }

        // 2.查询EMSP配置金额阈值信息
        Result<TbWalletConfigVO> result = pileBillClient.getConfig();
        log.info("queryWalletConfig result :{}",JSON.toJSONString(result));
        if(result != null && result.getCode().equals(org.springframework.http.HttpStatus.OK.value()) && result.getData() != null){
            TbWalletConfigVO data = result.getData();
            walletBalanceVo.setStopChargeLimitStatus(data.getBalanceLimit());
            walletBalanceVo.setChargeLimitStatus(data.getChargingLimit());
            if(data.getChargingLimit() != null && data.getChargingLimit() == 1){
                walletBalanceVo.setChargeAmountLimit(data.getChargingAmount());
            }
            if(data.getBalanceLimit() != null && data.getBalanceLimit() == 1){
                walletBalanceVo.setStopChargeAmoutLimit(data.getBalanceAmount());
            }
        }


        return walletBalanceVo;
    }

    public static CurrencyTypeEnum getCurrencyTypeEnumByCurrencyType(String getCurrencyTypeEnumByCurrencyType) {
        for (CurrencyTypeEnum currencyEnum : CurrencyTypeEnum.values()) {
            if (currencyEnum.getCurrencyType().equalsIgnoreCase(getCurrencyTypeEnumByCurrencyType)) {
                return currencyEnum;
            }
        }
        return CurrencyTypeEnum.CNY;
    }


    private AccountDetail queryWalletBlance(String userId,String appId){

        try {
            UserAccountInfoDto userAccountInfoDto = new UserAccountInfoDto();
            userAccountInfoDto.setUserId(userId);
            // 查询钱包余额增加商家ID
            String appTenantIdKey = com.autel.cloud.pile.bill.constant.RedisKeys.getaAppTenantIdKey(appId);
            String tenantId = stringRedisTemplate.opsForValue().get(appTenantIdKey);
            userAccountInfoDto.setTenantId(tenantId);
            Result<UserOnBoardingDetail> result = payTradingFeignClient.getUserAccounts(userAccountInfoDto);
            log.info("getUserAccounts result is {}", JSON.toJSONString(result));

            return Optional.ofNullable(result)
                    .filter(r -> r.getCode() == org.springframework.http.HttpStatus.OK.value())
                    .map(Result::getData)
                    .map(UserOnBoardingDetail::getAccountDetails)
                    .filter(CollUtil::isNotEmpty)
                    .map(details -> details.stream()
                            .filter(detail -> String.valueOf(AccountTypeEnum.BALANCE_ACCOUNT.getCode()).equals(detail.getAccountType())
                                    && CurrencyTypeEnum.THB.getCurrencyType().equals(detail.getCurrency()))
                            .findFirst()
                            .orElse(null))
                    .orElse(null);
        } catch (Exception e) {
            log.error("queryWalletBalance error, userId: {},", userId);
            log.error("queryWalletBlance 异常", e);
        }
        return null;

    }


    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    @Override
    public Result<OpLocationAPPDetailVO> appDetailForOCPI(OpLocationDetailDTO opLocationDetailDTO) {
        OpLocationAPPDetailVO opLocationAPPDetailVO = opLocationRepository.appDetailForOCPI(opLocationDetailDTO);
        // （返回该场站下是否包含英标桩的标志）
        if (opLocationAPPDetailVO != null && opLocationAPPDetailVO.getStationId() != null) {
            opLocationAPPDetailVO.setContainBritainStandPileFlag(this.isContainBritainStandPile(opLocationAPPDetailVO.getStationId()));
        }
        return Result.ofSucceed(opLocationAPPDetailVO);
    }

    @Override
    public Result<Integer> deleteOCPIEMSPData(List<Long> sellerIdList) {
        return Result.ofSucceed(opLocationRepository.deleteOCPIEMSPData(sellerIdList));
    }

    /**
     * @param stationId 场站id
     * @return 返回该场站下是否包含英标桩的标志
     * @function 判断该场站下是否包含英标桩
     */
    private Boolean isContainBritainStandPile(Long stationId) {

        log.info("===>>>OpLocationServiceImpl.isContainBritainStandPile stationId : {}", JSON.toJSONString(stationId));

        Boolean flag = Boolean.FALSE;
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseRepository.findByLocationId(stationId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
            for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
                Boolean britainStandPileMark = opLocationPileEvseElasticDTO.getBritainStandPileMark();
                if (britainStandPileMark != null && britainStandPileMark) {
                    flag = Boolean.TRUE;
                    break;
                }
            }
        }
        return flag;
    }

    /**
     * APP根据站点ID获取枪列表
     *
     * @param gunListPageDTO 检索对象
     * @return 枪列表
     */
    @Override
    public Result<Page<GunListPageVO>> getGunListByStationId(GunListPageDTO gunListPageDTO) {
        return Result.ofSucceed(opLocationRepository.getGunListByStationId(gunListPageDTO));
    }

    @Override
    public Result<Integer> JudgeOpenType() {
        Integer result = opLocationRepository.JudgeOpenType();
        return Result.ofSucceed(result);
    }

    @Override
    public Integer synchronizationData(List<Long> sellerIds) {
        return opLocationRepository.synchronizationData(sellerIds);
    }

    @Override
    public Result<Boolean> synchronizationOnlyData() {
        return Result.ofSucceed(opLocationRepository.synchronizationOnlyData());
    }

    @Override
    public Result<Boolean> synchronizationLocation(Long id) {
        return Result.ofSucceed(opLocationRepository.synchronizationLocation(id));
    }

    @Override
    public Result<OpLocationDTO> getDetailsFromEsById(Long id) {

        log.info("===========>>>>>>>>>> OpLocationServiceImpl.getDetailsFromEsById id : {}", JSON.toJSONString(id));

        OpLocationElasticDTO opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(id);
        return Result.ofSucceed(OpLocationConvert.toOpLocationDTO(opLocationElasticDTO));
    }

    /**
     * 场站列表对桩集合进行排序
     *
     * @param pileSortDTO 排序对象
     * @return 桩集合
     */
    @Override
    public Result<List<PilePageVO>> sortPile(PileSortDTO pileSortDTO) {
        return Result.ofSucceed(opLocationRepository.sortPile(pileSortDTO));
    }

    @Override
    public Result<List<OpLocationDTO>> getLocationBySellerId() {
        try {
            Long sellerId = UserUtil.getSellerId();
            log.info("sellerId:{}", sellerId);
            // 查询场站信息
            List<OpLocationDTO> opLocationDTOList = opLocationRepository.getLocationBySellerId(sellerId);
            return Result.ofSucceed(opLocationDTOList);
        } catch (Exception e) {
            log.error("查询场站信息失败:", e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
//        try {
//            Long sellerId = UserUtil.getSellerId();
//            log.info("sellerId:{}", sellerId);
//            // 查询场站信息
//            List<OpLocationDTO> opLocationDTOList = opLocationRepository.getLocationBySellerId(sellerId);
//            List<DataAuthorizeDto> permissionData = new ArrayList<>();
//            if (!CollectionUtils.isEmpty(opLocationDTOList)) {
//                DataAuthorizeDto dto = new DataAuthorizeDto();
//                dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
//                dto.setAppId(CompetenceUtil.APP_ID);
//                dto.setSellerId(LoginUserUtil.getSellerId().toString());
//                dto.setUserId(LoginUserUtil.getUserId().toString());
//                List<Node> data = new ArrayList<>();
//                List<Node> stationData = new ArrayList<>();
//                for (OpLocationDTO opLocationDTO : opLocationDTOList) {
//                    Node node = new Node();
//                    node.setLevel(SubTreeEnum.STATION.getCode());
//                    node.setNode(opLocationDTO.getId().toString());
//                    stationData.add(node);
//                }
//                String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
//                String competence = stringRedisTemplate.opsForValue().get(key);
//                try {
//                    if (StringUtils.isBlank(competence)) {
//                        UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
//                        userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
//                        userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
//                        Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
//                        if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
//                            competence = userCompetence.getData().getCompetence().toString();
//                        }
//                    }
//                } catch (Exception e) {
//                    log.info("查询用户权限报错,e={}",e);
//                }
//                if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
//                    Node seller = new Node();
//                    seller.setNode(LoginUserUtil.getSellerId().toString());
//                    seller.setLevel(SubTreeEnum.SELLER.getCode());
//                    seller.setChildNodes(stationData);
//                    data.add(seller);
//                    dto.setData(data);
//                }else {
//                    dto.setData(stationData);
//                }
//                permissionData.add(dto);
//            }
//            return CompetenceUtil.encapsulation(opLocationDTOList,permissionData);
//        } catch (Exception e) {
//            log.error("查询场站信息失败:", e);
//            return CompetenceUtil.encapsulation(new ArrayList<>(),new ArrayList<>());
//        }
    }

    @Override
    public Result<List<Long>> getLocationIdBySellerId() {
        try {
            Long sellerId = UserUtil.getSellerId();
            // 查询场站信息
            List<Long> opLocationDTOList = opLocationRepository.getLocationIdBySellerId(sellerId);
            return Result.ofSucceed(opLocationDTOList);
        } catch (Exception e) {
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
//        try {
//            Long sellerId = UserUtil.getSellerId();
//            // 查询场站信息
//            List<Long> opLocationDTOList = opLocationRepository.getLocationIdBySellerId(sellerId);
//            List<DataAuthorizeDto> permissionData = new ArrayList<>();
//            if (!ObjectUtils.isEmpty(opLocationDTOList)) {
//                DataAuthorizeDto dto = new DataAuthorizeDto();
//                dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
//                dto.setAppId(CompetenceUtil.APP_ID);
//                dto.setSellerId(LoginUserUtil.getSellerId().toString());
//                dto.setUserId(LoginUserUtil.getUserId().toString());
//                List<Node> data = new ArrayList<>();
//                List<Node> stationData = new ArrayList<>();
//                for (Long along : opLocationDTOList) {
//                    Node node = new Node();
//                    node.setLevel(SubTreeEnum.STATION.getCode());
//                    node.setNode(along.toString());
//                    stationData.add(node);
//                }
//                String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
//                String competence = stringRedisTemplate.opsForValue().get(key);
//                try {
//                    if (StringUtils.isBlank(competence)) {
//                        UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
//                        userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
//                        userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
//                        Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
//                        if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
//                            competence = userCompetence.getData().getCompetence().toString();
//                        }
//                    }
//                } catch (Exception e) {
//                    log.info("查询用户权限报错,e={}",e);
//                }
//                if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
//                    Node seller = new Node();
//                    seller.setNode(LoginUserUtil.getSellerId().toString());
//                    seller.setLevel(SubTreeEnum.SELLER.getCode());
//                    seller.setChildNodes(stationData);
//                    data.add(seller);
//                    dto.setData(data);
//                }else {
//                    dto.setData(stationData);
//                }
//                permissionData.add(dto);
//            }
//            return CompetenceUtil.encapsulation(opLocationDTOList,permissionData);
//        } catch (Exception e) {
//            return CompetenceUtil.encapsulation(new ArrayList<>(),new ArrayList<>());
//        }
    }

    /**
     * 更新场站公告
     *
     * @param opLocationAnnouncementUpdateDTO 更新对象
     * @return 更新结果
     */
    @Override
    public Result<Void> updateAnnouncement(OpLocationAnnouncementUpdateDTO opLocationAnnouncementUpdateDTO) {
        opLocationRepository.updateAnnouncement(opLocationAnnouncementUpdateDTO);
        log.info("updateAnnouncement location send to mq location={}", opLocationAnnouncementUpdateDTO.getId());
        rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_UPDATE_ROUTE, JSON.toJSONString(opLocationAnnouncementUpdateDTO.getId()));
        return Result.ofSucceed();
    }

    @Override
    public Result<List<OpLocationInfoVO>> getLocationByGroupId(List<Long> groupIdList) {
        return Result.ofSucceed(opLocationRepository.getLocationByGroupId(groupIdList));
    }

    @Override
    public Result<OpLocationDTO> getLocationByPileSn(String pileSn) {
        return Result.ofSucceed(opLocationRepository.getLocationByPileSn(pileSn));
//        List<DataAuthorizeDto> permissionData = new ArrayList<>();
//        if (ObjectUtils.isEmpty(locationByPileSn)) {
//            return CompetenceUtil.encapsulation(locationByPileSn,permissionData);
//        }
//        DataAuthorizeDto dto = new DataAuthorizeDto();
//        dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
//        dto.setAppId(CompetenceUtil.APP_ID);
//        dto.setSellerId(LoginUserUtil.getSellerId().toString());
//        dto.setUserId(LoginUserUtil.getUserId().toString());
//        List<Node> data = new ArrayList<>();
//        List<Node> stationData = new ArrayList<>();
//        Node node = new Node();
//        node.setLevel(SubTreeEnum.STATION.getCode());
//        node.setNode(locationByPileSn.getId().toString());
//        stationData.add(node);
//        String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
//        String competence = stringRedisTemplate.opsForValue().get(key);
//        try {
//            if (StringUtils.isBlank(competence)) {
//                UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
//                userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
//                userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
//                Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
//                if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
//                    competence = userCompetence.getData().getCompetence().toString();
//                }
//            }
//        } catch (Exception e) {
//            log.info("查询用户权限报错,e={}",e);
//        }
//        if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
//            Node seller = new Node();
//            seller.setNode(LoginUserUtil.getSellerId().toString());
//            seller.setLevel(SubTreeEnum.SELLER.getCode());
//            seller.setChildNodes(stationData);
//            data.add(seller);
//            dto.setData(data);
//        }else {
//            dto.setData(stationData);
//        }
//        permissionData.add(dto);
//        return CompetenceUtil.encapsulation(locationByPileSn,permissionData);
    }

    @Override
    public Result<List<OpLocationListDTO>> getLocationByPileSnList(List<String> pileSnList) {
        return Result.ofSucceed(opLocationRepository.getLocationByPileSnList(pileSnList));
//        List<OpLocationListDTO> locationByPileSnList = opLocationRepository.getLocationByPileSnList(pileSnList);
//
//        List<DataAuthorizeDto> permissionData = new ArrayList<>();
//        if (!CollectionUtils.isEmpty(locationByPileSnList)) {
//            DataAuthorizeDto dto = new DataAuthorizeDto();
//            dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
//            dto.setAppId(CompetenceUtil.APP_ID);
//            dto.setSellerId(LoginUserUtil.getSellerId().toString());
//            dto.setUserId(LoginUserUtil.getUserId().toString());
//            List<Node> data = new ArrayList<>();
//            List<Node> stationData = new ArrayList<>();
//            for (OpLocationListDTO opLocationMenuDTO : locationByPileSnList) {
//                Node node = new Node();
//                node.setLevel(SubTreeEnum.STATION.getCode());
//                node.setNode(opLocationMenuDTO.getId().toString());
//                stationData.add(node);
//            }
//            String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
//            String competence = stringRedisTemplate.opsForValue().get(key);
//            try {
//                if (StringUtils.isBlank(competence)) {
//                    UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
//                    userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
//                    userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
//                    Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
//                    if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
//                        competence = userCompetence.getData().getCompetence().toString();
//                    }
//                }
//            } catch (Exception e) {
//                log.info("查询用户权限报错,e={}",e);
//            }
//            if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
//                Node seller = new Node();
//                seller.setNode(LoginUserUtil.getSellerId().toString());
//                seller.setLevel(SubTreeEnum.SELLER.getCode());
//                seller.setChildNodes(stationData);
//                data.add(seller);
//                dto.setData(data);
//            }else {
//                dto.setData(stationData);
//            }
//            permissionData.add(dto);
//        }
//        return CompetenceUtil.encapsulation(locationByPileSnList,permissionData);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<OpLocationForNewVO> addForNew(OpLocationForNewDTO opLocationForNewDTO) {

        log.info("===>>>OpLocationServiceImpl.addForNew opLocationForNewDTO: {}", JSON.toJSONString(opLocationForNewDTO));

        // 前置处理，校验数据合法性
        if (opLocationForNewDTO.getOpLocationDTO() != null) {
            Boolean hubjectCheck = opLocationForNewDTO.getOpLocationDTO().getHubjectCheck();
            if (hubjectCheck != null && hubjectCheck) {
                opLocationForNewDTO.getOpLocationDTO().setTaxDTO(null);
            }
            TaxDTO taxDTO = opLocationForNewDTO.getOpLocationDTO().getTaxDTO();
            if (taxDTO != null && !Integer.valueOf(1).equals(taxDTO.getSetLocalTax())) {
                opLocationForNewDTO.getOpLocationDTO().getTaxDTO().setSetLocalTax(0);
                opLocationForNewDTO.getOpLocationDTO().getTaxDTO().setLocalTaxDTO(null);
            }
            opLocationForNewDTO.getOpLocationDTO().setConditionsValue(null);
        }

        //新手建站第一步
        OpLocationForNewVO opLocationForNewVO = new OpLocationForNewVO();
        if (opLocationForNewDTO.getOpLocationDTO() != null) {
            opLocationForNewVO.setResult(handlerLocation(opLocationForNewDTO.getOpLocationDTO()));
            return Result.ofSucceed(opLocationForNewVO);
        }
        //新手建站第二步
        if (CollectionUtils.isNotEmpty(opLocationForNewDTO.getOpLocationEvseDTOs())) {
            opLocationForNewVO.setResult(handlerPile(opLocationForNewDTO.getOpLocationEvseDTOs()));
            return Result.ofSucceed(opLocationForNewVO);
        }
        //新手建站第三步
        if (CollUtil.isNotEmpty(opLocationForNewDTO.getPileTariffList())) {
            opLocationForNewVO.setResult(handlerPileTariffMap(opLocationForNewDTO.getPileTariffList()));
            return Result.ofSucceed(opLocationForNewVO);
        }
        opLocationForNewVO.setResult(Boolean.FALSE);
        return Result.ofSucceed(opLocationForNewVO);
    }

    @Override
    @Transactional
    public Result<Long> addLocationByNew(List<PileTariffMapDTO> pileTariffList) {

        log.info("OpLocationServiceImpl.addLocationByNew.pileTariffList = {}", pileTariffList);

        if (CollUtil.isEmpty(pileTariffList)) {
            throw new MessageCodeException(PileBaseEnum.CREATE_LOCATION_FAILED);
        }
        List<String> pileSNList = pileTariffList.stream().map(PileTariffMapDTO::getPileSn).collect(Collectors.toList());
        LambdaQueryWrapper<OpLocationTempEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpLocationTempEntity::getUserId, UserUtil.getUserId());
        OpLocationTempEntity opLocationTempEntity = opLocationTempRepository.getOne(queryWrapper);
        log.info("OpLocationServiceImpl.addLocationByNew.opLocationTempEntity={}", opLocationTempEntity);
        if (opLocationTempEntity == null) {
            throw new MessageCodeException(PileBaseEnum.CREATE_LOCATION_FIRST);
        }
        //添加场站
        Result<Long> locationResult = this.add(JSON.parseObject(opLocationTempEntity.getLocationJson(), OpLocationDTO.class));
        log.info("OpLocationServiceImpl.addLocationByNew.locationResult={}", locationResult);
        if (locationResult.getCode() != HttpCodeEnum.OK.getCode() || locationResult.getCode() == null) {
            throw new MessageCodeException(PileBaseEnum.CREATE_LOCATION_FAILED);
        }
        //添加成本电价
        OpLocationDTO locationDto = JSON.parseObject(opLocationTempEntity.getLocationJson(), OpLocationDTO.class);
        LocationPriceDTO locationPriceDTO = locationDto.getLocationPriceDTO();
        if (locationPriceDTO != null) {
            locationPriceDTO.setLocationId(locationResult.getData());
            Long add = opLocationPriceService.add(locationPriceDTO);
            log.info("OpLocationServiceImpl.addLocationByNew.add={}", add);
        }
        Long userId = LoginUserHolder.getLoginUser().getId();
        OpLocationExceptionDTO exceptionDto = new OpLocationExceptionDTO();
        exceptionDto.setLocationId(locationResult.getData());
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getStringAddLocationException(userId), JSON.toJSONString(exceptionDto), 1L, TimeUnit.MINUTES);
        //添加设备
        List<OpLocationEvseDTO> opLocationEvseDTOS = JSON.parseArray(opLocationTempEntity.getPileJson(), OpLocationEvseDTO.class);
        opLocationEvseDTOS.forEach(evse -> {
            evse.setLocationId(locationResult.getData());

            try {
                log.info("OpLocationServiceImpl.addLocationByNew 新运维平台同步添加桩: {}", evse.getPileSN());
                OpLocationDTO opLocationDTO1 = JSON.parseObject(opLocationTempEntity.getLocationJson(), OpLocationDTO.class);

                PileInfoDTO pileInfoDTO = new PileInfoDTO();
                pileInfoDTO.setSn(evse.getPileSN());
                pileInfoDTO.setPin(evse.getPinCode());
                pileInfoDTO.setAddress(opLocationDTO1.getAddress());
                pileInfoDTO.setLongitude(evse.getLongitude());
                pileInfoDTO.setLatitude(evse.getLatitude());
                pileInfoDTO.setBrand(evse.getBrandName());
                pileInfoDTO.setCategory(2);
                pileInfoDTO.setRatedPower(String.valueOf(evse.getPower()));
                if (!StringUtils.isEmpty(evse.getPowerType())) {
                    if ("AC_1_PHASE".equals(evse.getPowerType())) {
                        pileInfoDTO.setPowerSupplyPhases(1);
                    } else if ("AC_3_PHASE".equals(evse.getPowerType())) {
                        pileInfoDTO.setPowerSupplyPhases(2);
                    } else if ("DC".equals(evse.getPowerType())) {
                        pileInfoDTO.setPowerSupplyPhases(3);
                    }
                }
                pileInfoDTO.setPileName(evse.getPileName());
                pileInfoDTO.setCountry(opLocationDTO1.getCountry());
                pileInfoDTO.setPileState(opLocationDTO1.getProvince());
                pileInfoDTO.setCity(opLocationDTO1.getCity());
                pileInfoDTO.setZipCode(opLocationDTO1.getPostalCode());
                pileInfoDTO.setCustomer(evse.getVendor());
                pileInfoDTO.setModel(evse.getProductModel());
                pileInfoDTO.setVendor(evse.getVendor());
                pileInfoDTO.setMerchantId(String.valueOf(LoginUserHolder.getLoginUser().getPayload().getSellerId()));
                pileInfoDTO.setPileGroupId(String.valueOf(evse.getLocationId()));
                pileInfoDTO.setGroupId(String.valueOf(opLocationDTO1.getGroupId()));

                List<Integer> opLocationConnectorDTOs = evse.getOpLocationConnectorDTOs().stream().map(OpLocationConnectorDTO::getGunType).collect(Collectors.toList());
                if (!opLocationConnectorDTOs.isEmpty()) {
                    List<ConnectorDTO> gunTypeList = new ArrayList<>();
                    for (int i = 0; i < opLocationConnectorDTOs.size(); ++i) {
                        gunTypeList.add(ConnectorDTO.builder().connectorNo(i + 1).connectorType(opLocationConnectorDTOs.get(i)).build());
                    }
                    pileInfoDTO.setConnectorList(gunTypeList);
                }
                log.info("OpLocationServiceImpl.addLocationByNew and pileInfoDTO = " + JSON.toJSONString(pileInfoDTO));
                opsMgmtClient.saveOpsPile(pileInfoDTO);
            } catch (Exception e) {
                log.error("OpLocationServiceImpl.addLocationByNew add ops-mgmt pile error: {}", e.getMessage(), e);
            }
        });
        List<PileVO> pileVOS = opLocationEvseRepository.createEvse(opLocationEvseDTOS);
        try {
            stringRedisTemplate.delete(RedisKeyConstant.getStringAddPileException(userId));
        } catch (Exception e) {
            log.info("添加桩删除es异常,e={}",e);
        }
        exceptionDto.setPileIds(pileVOS.stream().map(PileVO::getPileId).collect(Collectors.toList()));
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getStringAddLocationException(userId), JSON.toJSONString(exceptionDto), 1L, TimeUnit.MINUTES);

        log.info("OpLocationServiceImpl.addLocationByNew.pileVOS = {}", pileVOS);

        //todo 构造关联计费规则的入参
        log.info("OpLocationServiceImpl.addLocationByNew开始为充电设备(充电枪)绑定计费规则组！");

        List<BindCostModelRuleGroupForGunDTO> bindCostModelRuleGroupForGunDTOList = new ArrayList<>();

        //查出桩数据
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQuery = Wrappers.lambdaQuery();
        lambdaQuery.in(OpLocationPileEvseEntity::getPileSn, pileSNList)
                .eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseRepository.list(lambdaQuery);

        log.info("OpLocationServiceImpl.addLocationByNew.lambdaQuery.opLocationPileEvseEntities = {}", opLocationPileEvseEntities);

        // 充电设备的序列号与计费规则组id之间的映射关系
        Map<String, Long> evseSnAndTariffGroupIdMap = new HashMap<>();
        for (PileTariffMapDTO pileTariffMapDTO : pileTariffList) {
            String pileSn = pileTariffMapDTO.getPileSn();
            List<EvseAndTarifVO> list = pileTariffMapDTO.getList();
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(list)) {
                for (EvseAndTarifVO evseAndTarifVO : list) {
                    String connector = evseAndTarifVO.getConnector();
                    Long tariffGroupId = evseAndTarifVO.getTariffId();
                    evseSnAndTariffGroupIdMap.put(pileSn + "_" + connector, tariffGroupId);
                }
            }
        }

        // 充电设备的序列号集合
        List<String> evseSnList = new ArrayList<>(evseSnAndTariffGroupIdMap.keySet());
        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseRepository.queryTariffIdListByEvseSnList(evseSnList);
        for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
            BindCostModelRuleGroupForGunDTO bindCostModelRuleGroupForGunDTO = new BindCostModelRuleGroupForGunDTO();
            bindCostModelRuleGroupForGunDTO.setGunId(opLocationEvseEntity.getId());
            bindCostModelRuleGroupForGunDTO.setTariffGroupId(evseSnAndTariffGroupIdMap.get(opLocationEvseEntity.getEvseSn()));
            bindCostModelRuleGroupForGunDTOList.add(bindCostModelRuleGroupForGunDTO);
        }

        // 调用为充电设备(充电枪)绑定计费规则组的方法
        opLocationEvseService.bindCostModelRuleGroupForGun(bindCostModelRuleGroupForGunDTOList);

        OpLocationDTO opLocationDTO = JSON.parseObject(opLocationTempEntity.getLocationJson(), OpLocationDTO.class);
        if (opLocationDTO != null
                && opLocationDTO.getHubjectCheck() != null
                && opLocationDTO.getHubjectCheck()) {
            // 批量开启某个场站下所有的属性为公开的充电桩的互联互通属性
            opLocationEvseService.batchSetPileEroaming(locationResult.getData());
        }

        opLocationTempRepository.removeById(opLocationTempEntity.getId());
        stringRedisTemplate.delete(RedisKeyConstant.getStringAddLocationException(userId));

        try {
            // 充电枪信息添加成功之后需要发送MQ消息到车队那边
            for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
                EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                evseInfoModifyDTO.setEvseSn(opLocationEvseEntity.getEvseSn());
                evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.ADD.getCode());
                opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTO);
            }
        } catch (Exception e) {
            log.error("新手建站添加充电桩后，推送充电枪信息给车队那边出现异常 : {}", e);
        }

        return Result.ofSucceed(locationResult.getData());
    }

    private void toUpdateEvseDataUpdateDTO(OpLocationDTO opLocationDTO, List<OpLocationPileEvseEntity> opLocationPileEvseEntities, EvseDataUpdateDto evseDataUpdateDTO) {
        evseDataUpdateDTO.setLocationId(String.valueOf(opLocationDTO.getId()));
        List<EvsePrice> evsePrices = new ArrayList<>();
        opLocationPileEvseEntities.forEach(opLocationPileEvseEntity -> {
            EvsePrice evsePrice = new EvsePrice();
            evsePrice.setEvseSn(String.valueOf(opLocationPileEvseEntity.getId()));
            evsePrices.add(evsePrice);
        });
        evseDataUpdateDTO.setEvsePrices(evsePrices);
    }

    private void toAddEvseDataUpdateDTO(Long opLocationId, List<OpLocationPileEvseEntity> opLocationPileEvseEntities, EvseDataUpdateDto evseDataUpdateDTO) {
        evseDataUpdateDTO.setLocationId(String.valueOf(opLocationId));
        List<EvsePrice> evsePrices = new ArrayList<>();
        opLocationPileEvseEntities.forEach(opLocationPileEvseEntity -> {
            EvsePrice evsePrice = new EvsePrice();
            evsePrice.setEvseSn(String.valueOf(opLocationPileEvseEntity.getId()));
            evsePrices.add(evsePrice);
        });
        evseDataUpdateDTO.setEvsePrices(evsePrices);
    }

    /**
     * 充电桩与计费规则映射关系
     *
     * @param pileTariffList
     * @return
     */
    private Boolean handlerPileTariffMap(List<PileTariffMapDTO> pileTariffList) {
        LambdaQueryWrapper<OpLocationTempEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpLocationTempEntity::getUserId, UserUtil.getUserId());
        OpLocationTempEntity opLocationTempEntity = opLocationTempRepository.getOne(queryWrapper);
        log.info("OpLocationServiceImpl.handlerPileTariffMap.opLocationTempEntity={}", opLocationTempEntity);
        if (opLocationTempEntity == null) {
            throw new MessageCodeException(PileBaseEnum.CREATE_LOCATION_FIRST);
        }

        // todo  如果商家要绑定的某个充电桩的计费规则是收费的，并且商家自己的Stripe账户不能进行收费，那么此时就不能对这个充电桩配置该收费的计费规则
        if (!this.getSellerBindChargingPileFlag(pileTariffList)) {
            // 给出提示语：关联此计费规则需要先设置收款账户 在完成设置之前，此充电桩暂无法使用。
            throw new MessageCodeException(PileBaseEnum.PLEASE_BIND_THE_RECEIVING_ACCOUNT_FIRST);
        }

        opLocationTempEntity.setPileTariffJson(JSON.toJSONString(pileTariffList));
        return opLocationTempRepository.updateById(opLocationTempEntity);
    }

    /**
     * @param pileTariffList 充电桩与计费规则映射关系集合
     * @return 商家是否可以给充电桩绑定计费规则的标志
     * @function 商家是否可以给充电桩绑定计费规则
     */
    private Boolean getSellerBindChargingPileFlag(List<PileTariffMapDTO> pileTariffList) {
        // 获取sellerId
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        log.info("======>>>>>>>>>>> OpLocationServiceImpl.getSellerBindChargingPileFlag payload : {}", JSON.toJSONString(payload));
        // 默认是可以绑定的
        Boolean isSellerBindChargingPile = true;
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(payload)) {
            Long sellerId = payload.getSellerId();
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(sellerId)
                    && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileTariffList)) {
                for (PileTariffMapDTO pileTariffMapDTO : pileTariffList) {
                    Long tariffId = pileTariffMapDTO.getTariffId();
                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(tariffId)
                            && !sellerAccountService.isSellerBindChargingPile(tariffId, sellerId)) {
                        isSellerBindChargingPile = false;
                        break;
                    }
                }
            }
        }

        log.info("======>>>>>>>>>>> OpLocationServiceImpl.getSellerBindChargingPileFlag isSellerBindChargingPile : {}", JSON.toJSONString(isSellerBindChargingPile));

        return isSellerBindChargingPile;
    }

    private Boolean handlerPile(List<OpLocationEvseDTO> opLocationEvseDTOs) {
        log.info("OpLocationServiceImpl.handlerPile.opLocationEvseDTOs = {}", opLocationEvseDTOs);
        opLocationEvseRepository.onlyValidEvse(opLocationEvseDTOs);
        LambdaQueryWrapper<OpLocationTempEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpLocationTempEntity::getUserId, UserUtil.getUserId());
        OpLocationTempEntity opLocationTempEntity = opLocationTempRepository.getOne(queryWrapper);
        log.info("OpLocationServiceImpl.handlerPile.opLocationTempEntity={}", opLocationTempEntity);
        if (opLocationTempEntity == null) {
            throw new MessageCodeException(PileBaseEnum.CREATE_LOCATION_FIRST);
        }
        opLocationTempEntity.setPileJson(JSON.toJSONString(opLocationEvseDTOs));
        return opLocationTempRepository.updateById(opLocationTempEntity);
    }

    private Boolean handlerLocation(OpLocationDTO opLocationDTO) {
        // 查询场站名称是否已存在
        if (isNameExistence(opLocationDTO)) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NAME_REPEATED);
        }
        LambdaQueryWrapper<OpLocationTempEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpLocationTempEntity::getUserId, UserUtil.getUserId());
        OpLocationTempEntity opLocationTempEntity = opLocationTempRepository.getOne(queryWrapper);
        log.info("OpLocationServiceImpl.handlerLocation.opLocationTempEntity={}", opLocationTempEntity);
        //成本电价设置商家ID
        LocationPriceDTO locationPriceDTO = opLocationDTO.getLocationPriceDTO();
        if (locationPriceDTO != null) {
            locationPriceDTO.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        }
        if (opLocationTempEntity == null) {
            //新增
            OpLocationTempEntity locationTempEntity = new OpLocationTempEntity();
            locationTempEntity.setLocationJson(JSON.toJSONString(opLocationDTO));
            locationTempEntity.setUserId(UserUtil.getUserId());
            return opLocationTempRepository.save(locationTempEntity);
        } else {
            //更新
            opLocationTempEntity.setLocationJson(JSON.toJSONString(opLocationDTO));
            return opLocationTempRepository.updateById(opLocationTempEntity);
        }
    }

    @Override
    public Result<OpLocationForNewDTO> queryLocationForNewInfo() {
        //获取当前用户建站的临时表缓存数据
        LambdaQueryWrapper<OpLocationTempEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpLocationTempEntity::getUserId, UserUtil.getUserId());
        OpLocationTempEntity locationTempEntity = opLocationTempRepository.getOne(queryWrapper);
        if (ObjectUtils.isEmpty(locationTempEntity)) {
            return null;
        }
        //获取建站第三步的数据
        String pileTariffJson = locationTempEntity.getPileTariffJson();
        if (!ObjectUtils.isEmpty(pileTariffJson)) {
            List<PileTariffMapDTO> pileTariffMapDTOS = JSON.parseArray(pileTariffJson, PileTariffMapDTO.class);
            List<PileTariffMapDTO> pileTariffMapDTOList = new ArrayList<>();
            pileTariffMapDTOS.forEach(pileTariffMapDTO -> {
                //根据计费规则id判断计费规则是否存在
                Long tariffId = pileTariffMapDTO.getTariffId();
                if (!ObjectUtils.isEmpty(tariffId)) {
                    List<CostModelRuleDTO> costModelRuleDTOS = queryTariff(tariffId);
                    log.info("costModelRuleDTOS:{}", costModelRuleDTOS);
                    if (costModelRuleDTOS == null || costModelRuleDTOS.isEmpty()) {
                        pileTariffMapDTO.setTariffId(null);
                    }
                }
                pileTariffMapDTOList.add(pileTariffMapDTO);
            });
            //格式化数据：pileSn、pileName、tariffId
            pileTariffJson = JSON.toJSONString(pileTariffMapDTOList);
            log.info("pileTariffJson:{}", pileTariffJson);
            locationTempEntity.setPileTariffJson(pileTariffJson);
        }
        Long tariffId = locationTempEntity.getTariffId();
        if (!ObjectUtils.isEmpty(tariffId)) {
            List<CostModelRuleDTO> costModelRuleDTOS = queryTariff(tariffId);
            if (costModelRuleDTOS == null || costModelRuleDTOS.isEmpty()) {
                locationTempEntity.setTariffId(null);
            }
        }
        log.info("locationTempEntity :{}", locationTempEntity);
        OpLocationForNewDTO opLocationForNewDTO = OpLocationTempConvert.toOpLocationForNewDTO(locationTempEntity);
        return Result.ofSucceed(opLocationForNewDTO);
    }

    @Override
    public Result<Boolean> deletedGroupIdAndGroupName(Long userId) {
        LambdaQueryWrapper<OpLocationTempEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(OpLocationTempEntity::getUserId, userId);
        OpLocationTempEntity locationTempEntity = opLocationTempRepository.getOne(queryWrapper);
        OpLocationDTO opLocationDTO = JSON.parseObject(locationTempEntity.getLocationJson(), OpLocationDTO.class);
        opLocationDTO.setGroupId(null);
        opLocationDTO.setGroupName(null);
        locationTempEntity.setLocationJson(JSON.toJSONString(opLocationDTO));
        opLocationTempRepository.updateById(locationTempEntity);
        return Result.ofSucceed(true);
    }

    public List<CostModelRuleDTO> queryTariff(Long tariffId) {
        List<Long> tariffIds = new ArrayList<>();
        tariffIds.add(tariffId);
        Result<List<CostModelRuleDTO>> listResult = tariffAPPFeign.queryTariffByIds(tariffIds);
        return listResult.getData();
    }

    @Override
    public List<String> getSellerIdList(String locationName) {
        LambdaQueryWrapper<OpLocationEntity> qw = new LambdaQueryWrapper<>();
        qw.select(OpLocationEntity::getOperatorId);
        qw.like(OpLocationEntity::getName, locationName);
        List<OpLocationEntity> entityList = opLocationMapper.selectList(qw);
        if (CollectionUtils.isEmpty(entityList)) {
            return new ArrayList<>();
        }
        return entityList.stream().map(OpLocationEntity::getOperatorId).map(String::valueOf).collect(Collectors.toList());
    }

    /**
     * 获取时区列表
     *
     * @return 时区列表
     */
    @Override
    public Result<List<TimeZoneVO>> getTimeZoneList() {
        return Result.ofSucceed(opLocationRepository.getTimeZoneList());
    }

    @Override
    public Result<Boolean> updateLocationUpdateTime(Long locationId) {

        log.info("===>>> OpLocationServiceImpl.updateLocationUpdateTime locationId : {}",
                JSON.toJSONString(locationId));

        Boolean result = opLocationRepository.updateLocationUpdateTime(locationId);

        log.info("updateLocation location send to mq location={}", locationId);

        rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_UPDATE_ROUTE, JSON.toJSONString(locationId));
        return Result.ofSucceed(result);
    }

    @Override
    public Result<List<GunStatusGroupVO>> stationGunStatusGroupVO(Long id) {
        return Result.ofSucceed(opLocationRepository.stationGunStatusGroupVO(id));
    }

    @Override
    public Result<Void> refreshGroupName() {
        opLocationRepository.refreshGroupName();
        return Result.ofSucceed();
    }

    @Override
    public Result<List<EvseSnStatusVO>> gunStatusByEvseSn(List<String> EvseSnList) {
        return Result.ofSucceed(opLocationRepository.gunStatusByEvseSn(EvseSnList));
    }


    @Override
    public Result<Void> updateGroupName(Long groupId, String groupName) {
        opLocationRepository.updateGroupName(groupId, groupName);
        return Result.ofSucceed();
    }

    @Override
    public List<LocationInfoDTO> getLocationByKeyword(String keyword) {
        return opLocationRepository.getLocationByKeyword(keyword);
    }

    @Override
    public IPage<PileDetailVO> getPilePageByKeyword(RuleSitePageDTO ruleSitePageDTO) {
        return opLocationRepository.getPilePageByKeyword(ruleSitePageDTO);
    }

    @Override
    public List<SiteInfoVo> getSiteList(List<Long> locationIds, String keyword) {
        return opLocationRepository.getSiteList(locationIds, keyword);
    }

    @Override
    public Result<List<OpLocationMenuDTO>> getStationIncludedDeletedOrderByCreateAt() {
        List<OpLocationMenuDTO> menuDTOList = opLocationRepository.getStationIncludedDeletedOrderByCreateAt();
        return Result.ofSucceed(menuDTOList);
    }

    @Override
    public Result<Boolean> syncZoneId(List<Long> locationIds) {
        return Result.ofSucceed(opLocationRepository.syncZoneId(locationIds));
    }

    @Override
    public List<OpLocationEntity> findAll() {
        return opLocationRepository.findAll();
    }

    @Override
    public List<OpLocationEntity> populationZoneId(List<OpLocationEntity> list) {
        return opLocationRepository.populationZoneId(list);
    }

    @Override
    public List<OpLocationEntity> saveZoneId(List<OpLocationEntity> list) {
        return opLocationRepository.saveZoneId(list);
    }

    @Override
    public Result<Boolean> initializeOpLocationProvinceToES() {
        return opLocationRepository.initializeOpLocationProvinceToES();
    }

    @Override
    public Result<SearchItemInfoVO> searchInfo(SearchItemInfoDTO searchItemInfoDTO) {
        log.info("OpLocationServiceImpl.searchInfo start and searchItemInfoDTO = "
                + JSON.toJSONString(searchItemInfoDTO));
        try {
            SearchItemInfoVO list = opLocationRepository.searchInfo(searchItemInfoDTO);
            return Result.ofSucceed(list);
        } catch (Exception e) {
            log.info("OpLocationServiceImpl.searchInfo and Exception = ", e);
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST);
        }
    }

    @Override
    public Page<OpLocationPageVO> searchByPage(OpLocationPageDTO opLocationPageDTO) {
        log.info("OpLocationServiceImpl.searchByPage start and opLocationPageDTO = "
                + JSON.toJSONString(opLocationPageDTO));
        try {
            Page<OpLocationPageVO> opLocationPageVOPage = opLocationRepository.searchByPage(opLocationPageDTO);
            //log.info("验证searchByPage：{},:{}", opLocationPageVOPage.getRecords(), LoginUserHolder.getLoginUser().getPayload().getSellerId());
            return opLocationPageVOPage;
        } catch (Exception e) {
            log.info("OpLocationServiceImpl.searchByPage and Exception = ", e);
            Page<OpLocationPageVO> resultPage = new Page<>(opLocationPageDTO.getPage(), opLocationPageDTO.getPageSize());
            resultPage.setTotal(0);
            resultPage.setRecords(Lists.newArrayList());
            return resultPage;
        }
    }

    @Override
    public com.autel.cloud.base.model.Result<Page<OpLocationPageVO>> searchByPageV2(OpLocationPageDTO opLocationPageDTO) {
        log.info("OpLocationServiceImpl.searchByPage start and opLocationPageDTO = "
                + JSON.toJSONString(opLocationPageDTO));
        try {
            Page<OpLocationPageVO> opLocationPageVOPage = opLocationRepository.searchByPageV2(opLocationPageDTO);
            log.info("验证searchByPage：{},:{}", opLocationPageVOPage.getRecords(), LoginUserHolder.getLoginUser().getPayload().getSellerId());
            //log.info("验证searchByPage：{},:{}", opLocationPageVOPage.getRecords(), LoginUserHolder.getLoginUser().getPayload().getSellerId());

            List<DataAuthorizeDto> permissionData = new ArrayList<>();
            if (!CollectionUtils.isEmpty(opLocationPageVOPage.getRecords())) {
                DataAuthorizeDto dto = new DataAuthorizeDto();
                dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                dto.setAppId(CompetenceUtil.APP_ID);
                dto.setSellerId(LoginUserUtil.getSellerId().toString());
                dto.setUserId(LoginUserUtil.getUserId().toString());
                List<Node> data = new ArrayList<>();
                List<Node> stationData = new ArrayList<>();
                for (OpLocationPageVO OpLocationPageVO : opLocationPageVOPage.getRecords()) {
                    Node node = new Node();
                    node.setLevel(SubTreeEnum.STATION.getCode());
                    node.setNode(OpLocationPageVO.getId().toString());
                    stationData.add(node);
                }
                String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
                String competence = stringRedisTemplate.opsForValue().get(key);
                try {
                    if (StringUtils.isBlank(competence)) {
                        UserCompetenceDTO userCompetenceDTO = new UserCompetenceDTO();
                        userCompetenceDTO.setUserId(LoginUserUtil.getUserId());
                        userCompetenceDTO.setSellerId(LoginUserUtil.getSellerId());
                        Result<UserCompetenceVO> userCompetence = pileUserServiceFeign.getUserCompetence(userCompetenceDTO);
                        if (!ObjectUtils.isEmpty(userCompetence) && !ObjectUtils.isEmpty(userCompetence.getData())) {
                            competence = userCompetence.getData().getCompetence().toString();
                        }
                    }
                } catch (Exception e) {
                    log.info("查询用户权限报错,e={}",e);
                }
                if (LoginUserUtil.isSellerAdmin() || (!StringUtils.isBlank(competence) && "1".equals(competence))) {
                    Node seller = new Node();
                    seller.setNode(LoginUserUtil.getSellerId().toString());
                    seller.setLevel(SubTreeEnum.SELLER.getCode());
                    seller.setChildNodes(stationData);
                    data.add(seller);
                    dto.setData(data);
                }else {
                    dto.setData(stationData);
                }
                permissionData.add(dto);
            }
            return CompetenceUtil.encapsulation(opLocationPageVOPage,permissionData);
        } catch (Exception e) {
            log.info("OpLocationServiceImpl.searchByPage and Exception = ", e);
            Page<OpLocationPageVO> resultPage = new Page<>(opLocationPageDTO.getPage(), opLocationPageDTO.getPageSize());
            resultPage.setTotal(0);
            resultPage.setRecords(Lists.newArrayList());
            List<DataAuthorizeDto> permissionData = new ArrayList<>();
            return CompetenceUtil.encapsulation(resultPage,permissionData);
        }
    }

//@Override
//public String searchByPage(OpLocationPageDTO opLocationPageDTO) {
//    log.info("OpLocationServiceImpl.searchByPage start and opLocationPageDTO = "
//            + JSON.toJSONString(opLocationPageDTO));
//    try {
//        Page<OpLocationPageVO> opLocationPageVOPage = opLocationRepository.searchByPage(opLocationPageDTO);
//        log.info("验证searchByPage：{},:{}",opLocationPageVOPage.getRecords(),LoginUserHolder.getLoginUser().getPayload().getSellerId());
//        return String.valueOf(opLocationPageVOPage.getRecords());
//    } catch (Exception e) {
//        log.info("OpLocationServiceImpl.searchByPage and Exception = ", e);
//        Page<OpLocationPageVO> resultPage = new Page<>(opLocationPageDTO.getPage(), opLocationPageDTO.getPageSize());
//        resultPage.setTotal(0);
//        resultPage.setRecords(Lists.newArrayList());
////        return resultPage;
//    }
//}

    @Override
    public Result<OpStatisticsPileAndEvseVO> pileAndEvse(Long id) {
        if (id == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        OpStatisticsPileAndEvseVO pileAndEvseVO = opLocationRepository.pileAndEvse(id);
        return Result.ofSucceed(pileAndEvseVO);
    }

    /**
     * 三方桩激活文档下载
     *
     * @param response response
     * @return 返回对象
     */
    @Override
    public Result<Void> download(HttpServletResponse response, String brandName) {
        opLocationRepository.download(response, brandName);
        return Result.ofSucceed();
    }

    /**
     * 批量查询时区
     *
     * @param pileSnList
     * @return
     */
    @Override
    public List<OpLocationDTO> getDetails(List<String> pileSnList) {
        List<OpLocationDTO> opLocationDTOS = new ArrayList<>();
        for (int i = 0; i < pileSnList.size(); i++) {
            OpLocationPileEvseDTO opLocationPileEvseDTO = opLocationPileEvseRepository.queryByPileSn(pileSnList.get(i));
            if (opLocationPileEvseDTO != null) {
                Result<OpLocationDTO> detailsFromEsById = getDetailsFromEsById(opLocationPileEvseDTO.getLocationId());
                if (detailsFromEsById != null) {
                    opLocationDTOS.add(detailsFromEsById.getData());
                } else {
                    opLocationDTOS.add(null);
                }
            } else {
                opLocationDTOS.add(null);
            }
        }
        log.info("查询到的detail：{}", opLocationDTOS.size());
        return opLocationDTOS;
    }

    /**
     * 添加平台字段到场站和枪ES中
     *
     * @return
     */
    @Override
    public Boolean syncOpLocationESPlatform() {
        return opLocationRepository.syncOpLocationESPlatform();
    }

    @Override
    public Boolean updateOpLocationESPlatform() {
        return opLocationRepository.updateOpLocationESPlatform();
    }

    @Override
    public String getZoneIdByLocationId(Long locationId) {
        try {
            return locationCache.get(locationId).map(OpLocationElasticDTO::getZoneId).orElse(null);
        } catch (ExecutionException e) {
            return getZoneId(locationId);
        }
    }

    @Override
    public String getZoneId(Long locationId) {
        return opLocationRepository.getZoneId(locationId);
    }

    @Override
    public OpLocationElasticDTO findById(Long locationId) {
        try {
            return locationCache.get(locationId).orElse(null);
        } catch (ExecutionException e) {
            log.error("findById OpLocationElasticDTO", e);
            return opLocationElastic.findById(locationId).orElse(null);
        }
    }

    public Optional<OpLocationElasticDTO> findByLocationId(Long locationId) {
        if (locationId != null) {
            return opLocationElastic.findById(locationId);
        }
        return Optional.empty();
    }


    /**
     * 存储hubject数据到es
     *
     * @param opLocationSavePileEsDTO
     * @return
     */
    @Override
    public Result<Boolean> savePileToEs(OpLocationSavePileEsDTO opLocationSavePileEsDTO) {

        return opLocationRepository.savePileToEs(opLocationSavePileEsDTO);
    }

    @Override
    public Result<Boolean> deletePileToEs(BaseEsIdsDTO baseEsIdsDTO, boolean isAll) {
        return opLocationRepository.deletePileToEs(baseEsIdsDTO, isAll);
    }

    @Override
    public Result<Boolean> savePileStatusToEs(List<PileStatusToEsDTO> pileStatusToEsDTOS) {
        return opLocationRepository.savePileStatusToEs(pileStatusToEsDTOS);
    }

    @Override
    public Result<Long> detailById(Long id) {
        Long orgId = opLocationRepository.detailById(id);
        return Result.ofSucceed(orgId);
    }

    @Override
    public Result<AggMapVO> mapAgg(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        return Result.ofSucceed(opLocationRepository.mapAgg(opLocationMapQueryDTO));
    }

    @Override
    public Result<AggMapVO> mapAggForOCPI(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        return Result.ofSucceed(opLocationRepository.mapAggForOCPI(opLocationMapQueryDTO));
    }

    /**
     * @return 迁移结果
     * @function 税费重构——计费规则的税率迁移
     */
    @Override
    public Boolean taxRateMigrate() {
        // 远程调用计费规则的获得所有的税率不为空的计费规则的记录集合接口
        Result<List<CostModelRuleEntityVO>> result = tariffFeignClient.getAllNotEmptyTaxRateCostModelRuleList();

        log.info("===>>>OpLocationServiceImpl.taxRateMigrate result : {}", JSON.toJSONString(result));

        if (result == null
                || HttpStatus.HTTP_OK != result.getCode()
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData())) {

            log.info("===>>>无需迁移！");

            return false;
        }
        List<CostModelRuleEntityVO> costModelRuleEntityVOList = result.getData();
        for (CostModelRuleEntityVO costModelRuleEntityVO : costModelRuleEntityVOList) {
            Long tariffId = costModelRuleEntityVO.getId();
            BigDecimal taxRate = costModelRuleEntityVO.getTaxRate();
            // 根据计费规则id，查询充电设备表，获得该计费规则所使用的场站集合
            List<Long> locationIdList = opLocationEvseService.getAllLocationIdByTariffId(tariffId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(locationIdList)) {
                for (Long locationId : locationIdList) {
                    if (locationId != null) {
                        // 如果场站id不为空时，查询场站表
                        OpLocationEntity opLocationEntity = opLocationRepository.getLocationInfoById(locationId);
                        if (opLocationEntity != null && org.apache.commons.lang3.StringUtils.isBlank(opLocationEntity.getTaxConfiguration())) {
                            // todo 此时才需要迁移，避免覆盖用户的数据
                            // 1.迁移到数据库
                            TaxDTO taxDTO = new TaxDTO();
                            BasicTaxDTO basicTaxDTO = new BasicTaxDTO();
                            basicTaxDTO.setTaxName("TAX");
                            basicTaxDTO.setTaxRate(taxRate);
                            taxDTO.setBasicTax(basicTaxDTO);
                            opLocationEntity.setTaxConfiguration(JSON.toJSONString(taxDTO));
                            opLocationEntity.setUpdatedAt(System.currentTimeMillis());
                            opLocationRepository.taxRateMigrate(opLocationEntity);
                            // 2.迁移到ES
                            // 先根据主键查询出来数据
                            OpLocationElasticDTO opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(locationId);
                            if (opLocationElasticDTO != null) {
                                opLocationElasticDTO.setTaxConfiguration(opLocationEntity.getTaxConfiguration());
                                opLocationElasticDTO.setUpdatedAt(opLocationEntity.getUpdatedAt());
                                // 执行更新操作
                                opLocationElastic.save(opLocationElasticDTO);
                            }
                        }
                    }
                }
            }
        }
        return true;
    }

    @Override
    public boolean deleteExceptionData(OpLocationExceptionDTO exceptionDTO) {
        log.info("deleteExceptionData,exceptionDTO={}", JSON.toJSONString(exceptionDTO));
        Long locationId = exceptionDTO.getLocationId();
        if (locationId != null) {
            opLocationElastic.deleteById(locationId);
            log.info("deleteExceptionData,locationId={}", locationId);
        }
        List<Long> pileIds = exceptionDTO.getPileIds();
        if (CollectionUtils.isNotEmpty(pileIds)) {
            Iterable<OpLocationPileEvseElasticDTO> pileEvseDtoIt = opLocationPileEvseElastic.findAllById(pileIds);
            Set<String> pileSnList = new HashSet<>();
            pileEvseDtoIt.forEach(pileEvseDto -> {
                String pileSn = pileEvseDto.getPileSn();
                pileSnList.add(pileSn);
            });
            log.info("deleteExceptionData,pileSnList={}", JSON.toJSONString(pileSnList));
            pileIds.forEach(pileId -> opLocationPileEvseElastic.deleteById(pileId));
            if (!pileSnList.isEmpty()) {
                List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseElastic.findAllByPileSnIn(pileSnList);
                opLocationEvseElastic.deleteAll(evseDtoList);
                log.info("deleteExceptionData,evseDtoList={}", JSON.toJSONString(evseDtoList));
                opLocationEvseExpandElasticService.deleteByIds(evseDtoList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList()));
            }
        }
        return true;
    }

    /**
     * @param locationSimpleInfoQueryDTO 场站信息的简单查询实体 入参模型
     * @return 场站信息
     * @function 场站信息的简单查询
     */
    @Override
    public List<LocationSimpleInfoQueryVO> locationSimpleInfoQuery(LocationSimpleInfoQueryDTO locationSimpleInfoQueryDTO) {

        log.info("===>>> OpLocationServiceImpl.locationSimpleInfoQuery locationSimpleInfoQueryDTO : {}", JSON.toJSONString(locationSimpleInfoQueryDTO));

        if (locationSimpleInfoQueryDTO == null || locationSimpleInfoQueryDTO.getSellerId() == null) {
            return null;
        }
        List<OpLocationEntity> opLocationEntityList = opLocationRepository.getLocationInfoBySellerId(locationSimpleInfoQueryDTO.getSellerId());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEntityList)) {
            List<LocationSimpleInfoQueryVO> locationSimpleInfoQueryVOList = new ArrayList<>();
            for (OpLocationEntity opLocationEntity : opLocationEntityList) {
                LocationSimpleInfoQueryVO locationSimpleInfoQueryVO = new LocationSimpleInfoQueryVO();
                locationSimpleInfoQueryVO.setId(opLocationEntity.getId());
                locationSimpleInfoQueryVO.setName(opLocationEntity.getName());
                locationSimpleInfoQueryVOList.add(locationSimpleInfoQueryVO);
            }
            return locationSimpleInfoQueryVOList;
        }
        return null;
    }

    @Override
    public LocationRoamingVO getLocationRoamingByEvseSn(String evseSn) {
        return opLocationRepository.getLocationRoamingByEvseSn(evseSn);
    }

    @Override
    public Integer syncField() {
        LambdaQueryWrapper<OpLocationEntity> query = Wrappers.lambdaQuery();
        query.eq(OpLocationEntity::getDeleted, 0);

        List<OpLocationEntity> entityList = this.opLocationRepository.list(query);

        if (CollectionUtils.isEmpty(entityList)) {
            log.info("syncField,entityList is empty.");
            return 0;
        }

        List<Long> locationIds = entityList.stream().map(OpLocationEntity::getId).collect(Collectors.toList());
        NativeSearchQuery build = new NativeSearchQueryBuilder()
                .withQuery(QueryBuilders.boolQuery().must(QueryBuilders.termsQuery("id", locationIds)))
                .withPageable(PageRequest.of(0, 1000))
                .build();
        Map<Long, OpLocationElasticDTO> dtoMap = this.elasticsearchRestTemplate.searchForStream(build, OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList()).stream().collect(Collectors.toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
        NativeSearchQuery build2 = new NativeSearchQueryBuilder()
                .withQuery(QueryBuilders.boolQuery().must(QueryBuilders.termsQuery("locationId", locationIds)))
                .withPageable(PageRequest.of(0, 1000))
                .build();
        Map<Long, List<OpLocationEvseExpandElasticDTO>> evseMap = this.elasticsearchRestTemplate.searchForStream(build2, OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList()).stream().collect(Collectors.groupingBy(OpLocationEvseExpandElasticDTO::getLocationId));

        List<IndexQuery> locationUpdateList = new ArrayList<>();
        List<IndexQuery> evseUpdateList = new ArrayList<>();
        entityList.stream().forEach(entity -> {
            OpLocationElasticDTO dto = dtoMap.get(entity.getId());
            if (dto != null) {
                dto.setBusinessType(entity.getBusinessType());
                IndexQuery builder = new IndexQueryBuilder().withId(dto.getId().toString()).withObject(dto).build();
                locationUpdateList.add(builder);
            }
            List<OpLocationEvseExpandElasticDTO> evseDtoList = evseMap.get(entity.getId());
            if (CollectionUtils.isNotEmpty(evseDtoList)) {
                evseDtoList.stream().forEach(evseDto -> {
                    evseDto.setBusinessType(entity.getBusinessType());
                    IndexQuery builder = new IndexQueryBuilder().withId(evseDto.getId().toString()).withObject(evseDto).build();
                    evseUpdateList.add(builder);
                });
            }
        });

        if (locationUpdateList.size() > 0) {
            //批量保存
            this.elasticsearchRestTemplate.bulkIndex(locationUpdateList, OpLocationElasticDTO.class);
            //立即刷新
            this.elasticsearchRestTemplate.indexOps(OpLocationElasticDTO.class).refresh();
        }
        if (evseUpdateList.size() > 0) {
            //批量保存
            this.elasticsearchRestTemplate.bulkIndex(evseUpdateList, OpLocationEvseExpandElasticDTO.class);
            //立即刷新
            this.elasticsearchRestTemplate.indexOps(OpLocationEvseExpandElasticDTO.class).refresh();
        }
        int size = entityList.size();
        return size;
    }

    @Override
    public String getZonIdByEvseSn(String evseSn) {
        return opLocationRepository.getZonIdByEvseSn(evseSn);
    }

    @Override
    public OpLocationEvseInfoDTO getEvseInfo(String evseSn) {
        final OpLocationEvseElasticDTO evseInfo = opLocationRepository.getEvseInfo(evseSn);
        final OpLocationEvseInfoDTO opLocationEvseInfoDTO = new OpLocationEvseInfoDTO();
        if (Objects.nonNull(evseInfo)) {
            BeanUtils.copyProperties(evseInfo, opLocationEvseInfoDTO);
        }
        return opLocationEvseInfoDTO;
    }

    @Override
    public GetSellerInfoAndLocationInfoVO getSellerInfoAndLocationInfo(String pileSn) {
        GetSellerInfoAndLocationInfoVO getSellerInfoAndLocationInfoVO = opLocationRepository.getLocationInfo(pileSn);
        return getSellerInfoAndLocationInfoVO;
    }

    @Override
    public Boolean updatePower(UpdatePowerDTO updatePowerDTO) {
        return opCountryRepository.updatePower(updatePowerDTO);
    }

    /**
     * @param currentPageLocationIdList
     * @return
     * @function 查询场站信息
     */
    @Override
    public List<OpLocationEntity> findLocationInfoByLocationIdList(List<Long> currentPageLocationIdList) {

        log.info("===>>>OpLocationServiceImpl.findLocationInfoByLocationIdList currentPageLocationIdList : {}", JSON.toJSONString(currentPageLocationIdList));

        if (ObjectUtils.isEmpty(currentPageLocationIdList)) {
            return null;
        }
        return opLocationRepository.listByIds(currentPageLocationIdList);
    }

    /**
     * @param sellerIdList
     * @return
     * @function 批量获得商家与该商家所拥有的所有的场站信息之间的映射关系 （供运维使用）
     */
    @Override
    public LocationBasicInfoPackageVO getLocationBasicInfoPackageVOBySellerIdList(List<String> sellerIdList) {

        log.info("===>>>OpLocationServiceImpl.getLocationBasicInfoPackageVOBySellerIdList sellerIdList : {}", JSON.toJSONString(sellerIdList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(sellerIdList)) {
            return null;
        }

        // 类型转换
        List<Long> sellerIds = new ArrayList<>();
        sellerIdList.forEach(var -> {
            if (org.apache.commons.lang3.StringUtils.isNotBlank(var)
                    && NumberUtils.isDigits(var)) {
                sellerIds.add(Long.valueOf(var));
            }
        });

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(sellerIds)) {
            return null;
        }

        // 根据商家id集合，批量查询场站信息
        List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByOperatorIdIn(sellerIds);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationElasticDTOList)) {
            // 获得场站id集合
            Set<Long> locationIdSet = opLocationElasticDTOList
                    .stream()
                    .map(OpLocationElasticDTO::getId)
                    .collect(Collectors.toSet());
            // 根据场站id集合，批量查询充电桩信息
            List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findAllByLocationIdIn(locationIdSet);

            // 构建场站id与该场站下所有的充电桩信息之间的映射关系
            Map<Long, List<OpLocationPileEvseElasticDTO>> locationIdAndOpLocationPileEvseElasticDTOListMap = new HashMap<>();
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
                locationIdAndOpLocationPileEvseElasticDTOListMap
                        .putAll(opLocationPileEvseElasticDTOList
                                .stream()
                                .filter(var -> org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileSn()))
                                .collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId)));
            }

            // 构建商家id与商家下所有的场站信息之间的映射关系
            Map<Long, List<OpLocationElasticDTO>> sellerIdAndOpLocationElasticDTOListMap = opLocationElasticDTOList
                    .stream()
                    .collect(Collectors.groupingBy(OpLocationElasticDTO::getOperatorId));
            return this.buildLocationBasicInfoPackageVO(locationIdAndOpLocationPileEvseElasticDTOListMap, sellerIdAndOpLocationElasticDTOListMap);

        }
        return null;
    }

    /**
     * @param pileSnList
     * @return
     * @function 根据传入的充电桩序列号集合，查询这些充电桩的信息及其所在的场站信息，并以商家id为映射条件返回这些信息（供运维使用）
     */
    @Override
    public LocationBasicInfoPackageVO getLocationBasicInfoPackageVOByPileSnList(List<String> pileSnList) {

        log.info("===>>>OpLocationServiceImpl.getLocationBasicInfoPackageVOByPileSnList pileSnList : {}", JSON.toJSONString(pileSnList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileSnList)) {
            return null;
        }

        List<String> pileSns = pileSnList
                .stream()
                .filter(var -> org.apache.commons.lang3.StringUtils.isNotBlank(var))
                .collect(Collectors.toList());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileSns)) {
            return null;
        }

        // 根据充电桩序列号集合，批量查询充电桩信息
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(pileSns);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileEvseElasticDTOList)) {
            return null;
        }

        // 构建场站id与该场站下所有的充电桩信息之间的映射关系
        Map<Long, List<OpLocationPileEvseElasticDTO>> locationIdAndOpLocationPileEvseElasticDTOListMap = opLocationPileEvseElasticDTOList
                .stream()
                .filter(var -> var.getLocationId() != null)
                .collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(locationIdAndOpLocationPileEvseElasticDTOListMap)) {
            return null;
        }

        // 获得场站id集合
        Set<Long> locationIdSet = locationIdAndOpLocationPileEvseElasticDTOListMap.keySet();
        // 根据场站id集合，批量查询场站信息
        List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByIdIn(locationIdSet);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationElasticDTOList)) {
            return null;
        }

        // 构建商家id与商家下所有的场站信息之间的映射关系
        Map<Long, List<OpLocationElasticDTO>> sellerIdAndOpLocationElasticDTOListMap = opLocationElasticDTOList
                .stream()
                .filter(var -> var.getOperatorId() != null)
                .collect(Collectors.groupingBy(OpLocationElasticDTO::getOperatorId));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(sellerIdAndOpLocationElasticDTOListMap)) {
            return null;
        }

        return this.buildLocationBasicInfoPackageVO(locationIdAndOpLocationPileEvseElasticDTOListMap, sellerIdAndOpLocationElasticDTOListMap);
    }

    /**
     * @param locationIdAndOpLocationPileEvseElasticDTOListMap
     * @param sellerIdAndOpLocationElasticDTOListMap
     * @return
     * @function 构建LocationBasicInfoPackageVO实体
     */
    private LocationBasicInfoPackageVO buildLocationBasicInfoPackageVO(Map<Long, List<OpLocationPileEvseElasticDTO>> locationIdAndOpLocationPileEvseElasticDTOListMap, Map<Long, List<OpLocationElasticDTO>> sellerIdAndOpLocationElasticDTOListMap) {

        log.info("===>>>OpLocationServiceImpl.buildLocationBasicInfoPackageVO locationIdAndOpLocationPileEvseElasticDTOListMap : {} and sellerIdAndOpLocationElasticDTOListMap : {}",
                JSON.toJSONString(locationIdAndOpLocationPileEvseElasticDTOListMap),
                JSON.toJSONString(sellerIdAndOpLocationElasticDTOListMap));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(sellerIdAndOpLocationElasticDTOListMap)) {
            return null;
        }

        // 构造返回对象
        LocationBasicInfoPackageVO locationBasicInfoPackageVO = new LocationBasicInfoPackageVO();
        Map<String, List<LocationBasicInfoPackageVO.LocationBasicInfoVO>> sellerIdAndLocationBasicInfoVOListMap = new HashMap<>();
        List<String> pileSnInSellerIdAndLocationBasicInfoVOListMapList = new ArrayList<>();
        sellerIdAndOpLocationElasticDTOListMap.forEach((key, value) -> {
            List<LocationBasicInfoPackageVO.LocationBasicInfoVO> locationBasicInfoVOList = new ArrayList<>();
            value.forEach(var -> {
                LocationBasicInfoPackageVO.LocationBasicInfoVO locationBasicInfoVO = new LocationBasicInfoPackageVO.LocationBasicInfoVO();
                BeanUtils.copyProperties(var, locationBasicInfoVO);
                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(locationIdAndOpLocationPileEvseElasticDTOListMap.get(var.getId()))) {
                    List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOS = locationIdAndOpLocationPileEvseElasticDTOListMap.get(var.getId());
                    locationBasicInfoVO.setPileSnList(opLocationPileEvseElasticDTOS
                            .stream()
                            .map(OpLocationPileEvseElasticDTO::getPileSn)
                            .collect(Collectors.toList()));
                    pileSnInSellerIdAndLocationBasicInfoVOListMapList.addAll(locationBasicInfoVO.getPileSnList());
                }
                locationBasicInfoVOList.add(locationBasicInfoVO);
            });
            sellerIdAndLocationBasicInfoVOListMap.put(String.valueOf(key), locationBasicInfoVOList);
        });
        locationBasicInfoPackageVO.setSellerIdAndLocationBasicInfoVOListMap(sellerIdAndLocationBasicInfoVOListMap);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileSnInSellerIdAndLocationBasicInfoVOListMapList)) {
            locationBasicInfoPackageVO.setPileSnInSellerIdAndLocationBasicInfoVOListMapList(pileSnInSellerIdAndLocationBasicInfoVOListMapList);
        }
        return locationBasicInfoPackageVO;
    }

    /**
     * @param pileSimpleInfoQueryDTO
     * @return
     * @function 获得场站基础信息（供运维使用）
     */
    @Override
    public Page<LocationBasicInfoVO> getLocationBasicInfoVOPage(SearchDTO pileSimpleInfoQueryDTO) {

        log.info("===>>>OpLocationServiceImpl.getLocationBasicInfoVOPage getLocationBasicInfoVOPage : {}",
                JSON.toJSONString(pileSimpleInfoQueryDTO));

        Page<LocationBasicInfoVO> locationBasicInfoVOPage = new Page<>();
        if (pileSimpleInfoQueryDTO == null
                || pileSimpleInfoQueryDTO.getSellerId() == null) {
            return locationBasicInfoVOPage;
        }

        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileSimpleInfoQueryDTO.getSearchValue())) {
            LambdaQueryWrapper<OpLocationEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
            lambdaQueryWrapper
                    .eq(OpLocationEntity::getOperatorId, pileSimpleInfoQueryDTO.getSellerId())
                    .like(OpLocationEntity::getName, StringUtil.escapeChar(pileSimpleInfoQueryDTO.getSearchValue()))
                    .eq(OpLocationEntity::getDeleted, 0)
                    .orderByDesc(OpLocationEntity::getUpdatedAt, OpLocationEntity::getId);

            List<OpLocationEntity> opLocationEntityList = opLocationMapper.selectList(lambdaQueryWrapper);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEntityList)) {
                return locationBasicInfoVOPage;
            }

            List<LocationBasicInfoVO> locationBasicInfoVOList = new ArrayList<>();
            opLocationEntityList.forEach(var -> {
                LocationBasicInfoVO locationBasicInfoVO = new LocationBasicInfoVO();
                locationBasicInfoVO.setId(var.getId());
                locationBasicInfoVO.setLocationName(var.getName());
                locationBasicInfoVOList.add(locationBasicInfoVO);
            });

            locationBasicInfoVOPage.setCurrent(1L);
            locationBasicInfoVOPage.setTotal(locationBasicInfoVOList.size());
            locationBasicInfoVOPage.setPages(1L);
            locationBasicInfoVOPage.setSize(locationBasicInfoVOList.size());
            locationBasicInfoVOPage.setRecords(locationBasicInfoVOList);
            return locationBasicInfoVOPage;
        }

        Page<OpLocationEntity> page = new Page<>(pileSimpleInfoQueryDTO.getPage(), pileSimpleInfoQueryDTO.getPageSize());
        LambdaQueryWrapper<OpLocationEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpLocationEntity::getOperatorId, pileSimpleInfoQueryDTO.getSellerId())
                .eq(OpLocationEntity::getDeleted, 0)
                .orderByDesc(OpLocationEntity::getUpdatedAt, OpLocationEntity::getId);
        opLocationMapper.selectPage(page, lambdaQueryWrapper);

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(page.getRecords())) {
            List<LocationBasicInfoVO> locationBasicInfoVOList = new ArrayList<>();
            page.getRecords().forEach(var -> {
                LocationBasicInfoVO locationBasicInfoVO = new LocationBasicInfoVO();
                locationBasicInfoVO.setId(var.getId());
                locationBasicInfoVO.setLocationName(var.getName());
                locationBasicInfoVOList.add(locationBasicInfoVO);
            });

            locationBasicInfoVOPage.setCurrent(page.getCurrent());
            locationBasicInfoVOPage.setTotal(page.getTotal());
            locationBasicInfoVOPage.setPages(page.getPages());
            locationBasicInfoVOPage.setSize(page.getSize());
            locationBasicInfoVOPage.setRecords(locationBasicInfoVOList);
        }
        return locationBasicInfoVOPage;
    }

    @Override
    public List<OpLocationElasticDTO> findByIds(List<Long> ids,String keyword) {
        return this.opLocationRepository.findByIds(ids,keyword);
    }

    @Override
    public List<EroamingPileVO> queryEroamingPileListByTariff(Long tariffId) {
        return opLocationRepository.getEroamingPileListByTariff(tariffId);
    }

    @Override
    public LocationSimpleInfoQueryVO setLikeRoaming(Long locationId) {
        return opLocationRepository.setLikeRoaming(locationId);
    }

    @Override
    public LocationInfoVO getSimplyLocationInfoByLocationId(com.autel.cloud.pile.base.dto.advancePayment.LocationInfoDTO locationInfoDTO) {
        LocationInfoVO result = new LocationInfoVO();
        // 场站运营商id
        Long operatorId = null;
        // 场站时区id
        String zoneId = null;
        String locationName = null;
        if (locationInfoDTO.getStationId() != null) {
            Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(locationInfoDTO.getStationId());
            if (optionalOpLocationElasticDTO.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();

                log.info("========>>>>>>>>>>>> getSimplyLocationInfoByLocationId opLocationElasticDTO : {}", JSON.toJSONString(opLocationElasticDTO));

                operatorId = opLocationElasticDTO.getOperatorId();
                zoneId = opLocationElasticDTO.getZoneId();
                locationName = opLocationElasticDTO.getName();
            }
        }
        result.setStationId(locationInfoDTO.getStationId());
        result.setSellerId(operatorId);
        result.setZoneId(zoneId);
        result.setLocationName(locationName);
        return result;
    }

    @Override
    public Object ocpiPutLocation(Long operatorId, Map<String,Long> tariffIdMap, LocationVO locationVO) {
        return opLocationRepository.ocpiPutLocation(operatorId, tariffIdMap, locationVO);
    }

    @Override
    public Boolean prePaymentEnabled(Long locationId) {
        Integer payMethod = opLocationMapper.getPayMethod(locationId);
        if(payMethod == null){
            payMethod = PayMethodEnum.PAY_LATER.getValue();
        }
        return PayMethodEnum.ADVANCE_PAYMENT.getValue().equals(payMethod);
    }

    @Override
    public List<Long> getIdsByBusinessType(Collection<Long> locationIds, List<Integer> businessTypes) {
        if (CollectionUtils.isEmpty(locationIds)) {
            return null;
        }
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        query.must(QueryBuilders.termsQuery("id", locationIds));
        if (CollectionUtils.isNotEmpty(businessTypes)) {
            query.must(QueryBuilders.termsQuery("businessType", businessTypes));
        }
        List<OpLocationElasticDTO> locationDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(query)
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "businessType"}, null))
                .build(), OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(locationDtoList)) {
            return null;
        }
        return locationDtoList.stream().map(OpLocationElasticDTO::getId).collect(Collectors.toList());
    }

    @Override
    public AppPrePaymentVO appPrePaymentVO(Long locationId) {
        AppPrePaymentVO appPrePaymentVO = new AppPrePaymentVO();
        OpLocationEntity opLocationEntity = opLocationMapper.getLocationPrePayment(locationId);
        Integer payMethod = PayMethodEnum.PAY_LATER.getValue();
        if(opLocationEntity != null && opLocationEntity.getPayMethod() != null ){
            payMethod = opLocationEntity.getPayMethod();
        }
        appPrePaymentVO.setPrePaymentEnabled(PayMethodEnum.ADVANCE_PAYMENT.getValue().equals(payMethod));

        if(opLocationEntity != null){
            appPrePaymentVO.setPrePaymentTire(opLocationEntity.getPrepaymentAmountTier());
        }

        return appPrePaymentVO;
    }

    @Override
    public List<LocationListVO> locationListByOperatorId(LocationListDTO locationListDTO) {
        return opLocationRepository.locationListByOperatorId(locationListDTO);
    }

    @Override
    public Map<String, List<CommonVO>> getLocationIdSellerMap() {
        return opLocationRepository.getLocationIdSellerMap();
    }

    @Override
    public List<NodeVO> querySubTree(QuerySubTreeDTO querySubTreeDTO) {
        return opLocationRepository.querySubTree(querySubTreeDTO);
    }

    @Override
    public AppPileDetailVO appPileDetail(String sn) {
        AppPileDetailVO appPileDetailVO = null;

        List<OpLocationEvseElasticDTO> opLocationEvseList =  opLocationEvseRepository.findListByPileSn(sn);
        if(org.apache.commons.collections4.CollectionUtils.isEmpty(opLocationEvseList)){
            throw new MessageCodeException(PileBaseEnum.PILE_GUN_NOT_EXIST);
        }

        //添加emsp条件
        Long operatorId = opLocationEvseList.get(0).getOperatorId();
        opLocationRepository.appAddEMSPCondition(String.valueOf(operatorId),PileBaseEnum.PILE_GUN_NOT_EXIST);

        if(org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationEvseList)){
            if(!opLocationEvseList.get(0).getSubscriptionCheck()){
                throw new MessageCodeException(PileBaseEnum.PILE_LICENCE_EXPIRED);
            }
            Long locationId = opLocationEvseList.get(0).getLocationId();
            Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(locationId);

            if (optionalOpLocationElasticDTO.isPresent() && optionalOpLocationElasticDTO.get().getPlatform() == 1) {
                OpLocationElasticDTO  opLocationElasticDTO = optionalOpLocationElasticDTO.get();
                if(opLocationElasticDTO.getOpenType() == 2){
                    throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_OPEN);
                }

                appPileDetailVO = AppPileDetailVO.builder()
                        .locationId(locationId.toString())
                        .locationName(opLocationElasticDTO.getName())
                        .sn(sn)
                        .alpha2Code(opLocationElasticDTO.getCountry())
                        .isAllDay(1)
                        .emailRegularExpression(baseAdminFeign.getPattern("email").getData())
                        .build();

                String language = "en-US";
                HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
                String requestLanguage = request.getHeader("accept-language");
                if(StringUtils.isNotBlank(requestLanguage)){
                    language = requestLanguage;
                }
                // 轻应用在获取桩详情时设置用户语言
                UnitDto unitDto = new UnitDto();
                unitDto.setLanguage(language);
                baseAdminFeign.setUnitV2(unitDto);

                appPileDetailVO.setNoTariffMsg(messageSourceUtil.getMessage(PileBaseEnum.OCPP_EVSE_NO_TARIFF.getCode()+"", language));

                // 查询场站运营时间
                setOpenTime(opLocationElasticDTO, appPileDetailVO, language);

                //查询logo
                QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
                opLocationOperationEntityQueryWrapper.eq("location_id", locationId);
                opLocationOperationEntityQueryWrapper.select("logo_image_id");
                OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(opLocationOperationEntityQueryWrapper);
                if (opLocationOperationEntity != null && opLocationOperationEntity.getLogoImageId() != null) {
                    OpImageEntity opImageEntity = opImageMapper.selectById(opLocationOperationEntity.getLogoImageId());
                    if (opImageEntity != null) {
                        appPileDetailVO.setLogoPath(opImageEntity.getUrl());
                    }
                }

                //查询货币
                List<CurrencyDTO> currencyList = tariffFeignClient.getCurrencyListBySellerId(opLocationElasticDTO.getOperatorId()).getData();
                String currencyCode = currencyList.get(0).getCode().toString();
                appPileDetailVO.setCurrencyCode(currencyList.get(0).getCode());
                appPileDetailVO.setCurrencySign(currencyList.get(0).getCurrencySign());
                appPileDetailVO.setCurrencyType(currencyList.get(0).getCurrencyType());
                appPileDetailVO.setPileName(opLocationEvseList.get(0).getPileName());


                JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
                String currentUserId;
                if(jwtInfo != null){
                    currentUserId = jwtInfo.getId() == null ? "" : jwtInfo.getId().toString();
                } else {
                    currentUserId = "";
                }

                List<String> evseSnList = opLocationEvseList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
                Map<String, OpEvseStatusUploadDTO> snStatusMap = monitorFeignClient.queryStatusInfoByEvseSnList(evseSnList).getData();


                //设置枪列表数据
                List<GunDetailVO> gunDetailVOList = new ArrayList<>();
                List<GunDetailVO> finalGunDetailVOList = gunDetailVOList;

                AtomicReference<TransactionInfoVO> chargingTransactionCache = new AtomicReference<>();
                opLocationEvseList.forEach(e->{
                    String powerType = StringUtils.isBlank(e.getPowerType()) ? "" : e.getPowerType().substring(0, 2) ;
                    String gunNo = e.getEvseSn().split("_")[1];
                    String connectorDisplayName = commonUtilService.getConnectorDisplayName(sn, gunNo);
                    String pileGunKey = RedisKeyConstant.getChargePileGunKey(sn, gunNo);
                    String redisValue = stringRedisTemplate.opsForValue().get(pileGunKey);

                    BigDecimal connectionFee = null;
                    BigDecimal energyFee = null;
                    BigDecimal idleFee = null;
                    BigDecimal timeFee = null;
                    String timeFeeUnit = "";
                    String idleFeeUnit = "";
                    if(e.getTariffId() != null){
                        TariffRuleOfPileDTO tariffRuleOfPileDTO = new TariffRuleOfPileDTO();
                        tariffRuleOfPileDTO.setPileNo(sn);
                        tariffRuleOfPileDTO.setEvseNumber(Integer.parseInt(gunNo));
                        tariffRuleOfPileDTO.setEvseSn(e.getEvseSn());
                        tariffRuleOfPileDTO.setCurrentTimestamp(System.currentTimeMillis());
                        tariffRuleOfPileDTO.setTariffId(e.getTariffId());
                        tariffRuleOfPileDTO.setStationTimezone(opLocationElasticDTO.getZoneId());

                        // 调用tariff服务获取当前时刻计费规则
                        CurrentMomentFeeRulesVO currentMomentFeeRules = tariffFeignClient.getCurrentMomentFeeRules(tariffRuleOfPileDTO).getData();
                        connectionFee = currentMomentFeeRules.getStartPrice();
                        energyFee = currentMomentFeeRules.getEnergyPrice();
                        idleFee = currentMomentFeeRules.getIdlePrice();
                        timeFee = currentMomentFeeRules.getDurationPrice();
                        timeFeeUnit = currentMomentFeeRules.getDurationPriceUnit();
                        idleFeeUnit = currentMomentFeeRules.getIdlePriceUnit();
                    }

                    String userId = "";
                    String chargingUserEmail = "";
                    Boolean chargeByCurrentUserEnabled = false;
                    if(StringUtils.isNotBlank(redisValue)){
                        TransactionInfoVO transactionInfoVo = JSON.parseObject(redisValue, TransactionInfoVO.class);
                        userId = transactionInfoVo.getUserId();
                        chargeByCurrentUserEnabled = userId.equals(currentUserId);
                        if(chargeByCurrentUserEnabled){
                            chargingTransactionCache.set(transactionInfoVo);
                        }

                        if(StringUtils.isNotBlank(transactionInfoVo.getTransactionId())){
                            try{
                                chargingUserEmail = billFeignClient.getChargingUserEmail(transactionInfoVo.getTransactionId()).getData();
                                log.info("appPileDetail ChargingOrderSeq: {}, chargingUserEmail :{} ", transactionInfoVo.getTransactionId(), chargingUserEmail);
                            }catch (Exception e1){
                            }
                        }
                    }

                    boolean isFreeStart = false;
                    Boolean hasTariff = e.getTariffId() != null;
                    if(hasTariff){
                        List<String> tariffIdList = new ArrayList<>();
                        tariffIdList.add(e.getTariffId().toString());
                        isFreeStart = isFreeStart(e.getOperatorId(), e.getEvseSn(), tariffIdList);
                    }

                    GunDetailVO gunDetailVO = GunDetailVO.builder()
                            .evseSn(e.getEvseSn())
                            .gunNo(gunNo)
                            .power(e.getPower())
                            .connectorDisplayName(connectorDisplayName)
                            .gunType(e.getGunType())
                            .gunTypeName(ConnectorGunTypeEnum.getEnumByCode(e.getGunType()).getName())
                            .connectionFee(connectionFee)
                            .energyFee(energyFee)
                            .idleFee(idleFee)
                            .timeFee(timeFee)
                            .prepaymentAmount(isFreeStart ? 0 : getPrepayment(e.getLocationId(), connectionFee, currencyCode))
                            .maxPrepaymentAmount(maxPrepaymentAmount)
                            .freeAutoStartEnabled(isFreeAutoStart(e.getTariffId()))
                            .freeStartEnabled(isFreeStart)
                            .powerType(powerType)
                            .chargeByCurrentUserEnabled(chargeByCurrentUserEnabled)
                            .timeFeeUnit(timeFeeUnit)
                            .idleFeeUnit(idleFeeUnit)
                            .chargingUserEmail(chargingUserEmail)
                            .hasTariff(hasTariff)
                            .build();

                    OpEvseStatusUploadDTO opEvseStatusUploadDTO = snStatusMap.get(e.getEvseSn());
                    if(opEvseStatusUploadDTO != null){
                        gunDetailVO.setStatusName(opEvseStatusUploadDTO.getStatus());
                        gunDetailVO.setStatus(EvseDeviceStatusEnum.getEnumByName(opEvseStatusUploadDTO.getStatus()).getCode());
                        gunDetailVO.setInfo(opEvseStatusUploadDTO.getCpVoltage());
                    }

                    if(hasTariff){
                        if (org.apache.commons.lang3.StringUtils.isNotBlank(redisValue) ||
                                EvseDeviceStatusEnum.CHARGING.getCode().equals(gunDetailVO.getStatus()) ||
                                EvseDeviceStatusEnum.SUSPENDED_EVSE.getCode().equals(gunDetailVO.getStatus()) ||
                                EvseDeviceStatusEnum.SUSPENDED_EV.getCode().equals(gunDetailVO.getStatus()) ||
                                EvseDeviceStatusEnum.FINISHING.getCode().equals(gunDetailVO.getStatus())) {
                            gunDetailVO.setAppStatus(AppStatusEnum.IN_USE.getValue());
                        }else{
                            if(EvseDeviceStatusEnum.AVAILABLE.getCode().equals(gunDetailVO.getStatus())){
                                gunDetailVO.setAppStatus(AppStatusEnum.AVAILABLE.getValue());
                            }else if(EvseDeviceStatusEnum.PREPARING.getCode().equals(gunDetailVO.getStatus())){
                                gunDetailVO.setAppStatus(AppStatusEnum.PLUGIN.getValue());
                            }else if(EvseDeviceStatusEnum.RESERVED.getCode().equals(gunDetailVO.getStatus())){
                                gunDetailVO.setAppStatus(AppStatusEnum.RESERVED.getValue());
                            }else if(EvseDeviceStatusEnum.UNAVAILABLE.getCode().equals(gunDetailVO.getStatus()) ||
                                    EvseDeviceStatusEnum.FAULTED.getCode().equals(gunDetailVO.getStatus())){
                                gunDetailVO.setAppStatus(AppStatusEnum.UNAVAILABLE.getValue());
                            }else {
                                gunDetailVO.setAppStatus(AppStatusEnum.OFFLINE.getValue());
                            }
                        }
                    }else{
                        gunDetailVO.setAppStatus(AppStatusEnum.UNAVAILABLE.getValue());
                    }

                    finalGunDetailVOList.add(gunDetailVO);

                });

                if(chargingTransactionCache.get() != null){
                    TransactionInfoVO transactionInfoVO = chargingTransactionCache.get();
                    String gunNo = transactionInfoVO.getGunNo();
                    String pileSn = transactionInfoVO.getDeviceNum();
                    appPileDetailVO.setChargingGunNo(gunNo);
                    appPileDetailVO.setChargingSn(pileSn);
                    appPileDetailVO.setChargingOrderSeq(transactionInfoVO.getTransactionId());
                }else{
                    if(StringUtils.isNotBlank(currentUserId)){
                        try{
                            EnergyBillVO energyBillVO = billFeignClient.getUnfinishedBillByUserId(currentUserId, DeviceTypeEnum.BUSINESS_PILE.getValue()).getData();
                            log.info("appPileDetail findLastBillByUserId  userId:{}, energyBillVO :{}", currentUserId, JSON.toJSONString(energyBillVO));
                            if(energyBillVO != null){
                                String evseSn = energyBillVO.getEvseSn();
                                String gunNo = evseSn.split("_")[1];
                                String pileSn = evseSn.split("_")[0];
                                appPileDetailVO.setChargingGunNo(gunNo);
                                appPileDetailVO.setChargingSn(pileSn);
                                appPileDetailVO.setChargingOrderSeq(energyBillVO.getOrderSeq());
                            }
                        }catch (Exception e1){
                            log.error("appPileDetail  findLastBillByUserId  userId:{} 异常：{}", currentUserId, e1);
                        }
                    }
                }

                String chargingGunNo = appPileDetailVO.getChargingGunNo();
                if(CollectionUtils.isNotEmpty(gunDetailVOList)){
                    // 当有滞留费时，订单缓存为空，此时需要比较未完成订单是否是当前的枪
                    if(chargingTransactionCache.get() == null && StringUtils.isNotBlank(appPileDetailVO.getChargingOrderSeq())
                            && StringUtils.isNotBlank(appPileDetailVO.getChargingSn())
                            && StringUtils.isNotBlank(chargingGunNo)
                            && appPileDetailVO.getSn().equals(appPileDetailVO.getChargingSn())){
                        gunDetailVOList.forEach(g->{
                            if(chargingGunNo.equals(g.getGunNo())){
                                g.setChargeByCurrentUserEnabled(true);
                            }
                        });
                    }


                    gunDetailVOList = gunDetailVOList.stream().sorted(Comparator.comparing(o-> new Integer((o.getGunNo())))).collect(Collectors.toList());
                }
                appPileDetailVO.setGunDetailVOList(gunDetailVOList);
            }
        }

        log.info("appPileDetailVO :{}", JSON.toJSONString(appPileDetailVO));
        return appPileDetailVO;
    }

    private void setOpenTime(OpLocationElasticDTO opLocationElasticDTO, AppPileDetailVO appPileDetailVO, String language) {
        try {
            //场站所在时区的时间
            String timeOffset = org.apache.commons.lang3.StringUtils.isBlank(opLocationElasticDTO.getTimeZone()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getTimeZone();
            String timeZone = org.apache.commons.lang3.StringUtils.isBlank(opLocationElasticDTO.getZoneId()) ? timeOffset : opLocationElasticDTO.getZoneId();
            log.info("场站所在时区：{}", timeZone);
            Date nowDate = new Date();
            log.info("nowDate：{}", nowDate);
            Instant instant = Instant.ofEpochMilli(nowDate.getTime());
            ZoneId zoneId;
            try {
                zoneId = ZoneId.of(timeZone);
            } catch (Exception e) {
                zoneId = ZoneId.of("Asia/Shanghai");
            }
            LocalDateTime zoneLocalDateTime = LocalDateTime.ofInstant(instant, zoneId);
            int weekDay = zoneLocalDateTime.getDayOfWeek().getValue();
            int hour = zoneLocalDateTime.getHour();
            int minute = zoneLocalDateTime.getMinute();
            String nowTimeStr = hour + ":" + minute;
            DateFormat dateFormat = new SimpleDateFormat("HH:mm");
            Date nowTime = dateFormat.parse(nowTimeStr);
            log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
            log.info(BaseConstant.NOW_TIME, nowTime);

            Boolean isOpen = true;
            String chargingTime = "24:00";
            Integer chargingWeekDay = 0;
            //进场信息查询
            try {
                RuleRelateForAppDTO ruleRelateForAppDTO = new RuleRelateForAppDTO();
                ruleRelateForAppDTO.setUserId(UserUtil.getUserId());
                ruleRelateForAppDTO.setSellerId(opLocationElasticDTO.getOperatorId());
                ruleRelateForAppDTO.setLocationId(opLocationElasticDTO.getId());
                ruleRelateForAppDTO.setZoneId(opLocationElasticDTO.getZoneId());
                Result<RuleRelateForAppVO> ruleForAppResult = ruleRepository.getRuleForApp(ruleRelateForAppDTO);
                log.info("查询进场控制结果信息：{}", JSON.toJSONString(ruleForAppResult));
                if (ruleForAppResult != null && ruleForAppResult.getData() != null) {
                    RuleRelateForAppVO ruleRelateForAppVO = ruleForAppResult.getData();
                    isOpen = ruleRelateForAppVO.getIsOpen() && isOpen;
                    chargingTime = ruleRelateForAppVO.getChangingTime();
                    if(ruleRelateForAppVO.getDay() != null){
                        chargingWeekDay = ruleRelateForAppVO.getDay();
                    }
                }
            } catch (Exception e) {
                log.info("进场信息查询失败：",e);
            }

            log.info("isOpen :{}, chargingTime:{}, chargingWeekDay:{}", isOpen, chargingTime, chargingWeekDay);

            chargingTime = StringUtils.defaultString(chargingTime, "");
            // openStr = 营业中/休息 changingTime
            String openStr = String.format("%s/%s %s", messageSourceUtil.getMessage(CHARGE_STATION_IN_OPEN, language), messageSourceUtil.getMessage(CHARGE_STATION_TO_CLOSE, language), chargingTime);
            String openTimeStr = "";
            if("zh-cn".equalsIgnoreCase(language)){
                // openStr = 营业中/休息
                if(StringUtils.isBlank(chargingTime)){
                    openStr = String.format("%s/%s%s", messageSourceUtil.getMessage(CHARGE_STATION_IN_OPEN, language), chargingTime, messageSourceUtil.getMessage(CHARGE_STATION_TO_CLOSE, language));
                }else{
                    // openStr = 营业中/changingTime 休息
                    openStr = String.format("%s/%s %s", messageSourceUtil.getMessage(CHARGE_STATION_IN_OPEN, language), chargingTime, messageSourceUtil.getMessage(CHARGE_STATION_TO_CLOSE, language));
                }
                // openTimeStr = 星期几changingTime
                openTimeStr = chargingWeekDay > 0 ? convertWeekDay(chargingWeekDay, language) + chargingTime : chargingTime;
            }else{
                if(StringUtils.isBlank(chargingTime)){
                    // openTimeStr = 星期几
                    openTimeStr = chargingWeekDay > 0 ? String.format("%s%s", chargingTime, convertWeekDay(chargingWeekDay, language)) : chargingTime;
                }else{
                    // openStr = 营业中/changingTime 休息
                    openTimeStr = chargingWeekDay > 0 ? String.format("%s %s", chargingTime, convertWeekDay(chargingWeekDay, language)) : chargingTime;
                }
            }

            // closeStr = 休息中/营业 openTimeStr
            String closeStr = String.format("%s/%s %s", messageSourceUtil.getMessage(CHARGE_STATION_TO_CLOSE, language), messageSourceUtil.getMessage(CHARGE_STATION_IN_OPEN, language), openTimeStr);
            if("zh-cn".equalsIgnoreCase(language)){
                if(StringUtils.isBlank(openTimeStr)){
                    //closeStr = 休息中/营业
                    closeStr = String.format("%s/%s%s", messageSourceUtil.getMessage(CHARGE_STATION_TO_CLOSE, language), openTimeStr, messageSourceUtil.getMessage(CHARGE_STATION_IN_OPEN, language));
                }else{
                    // closeStr = 休息中/openTimeStr 营业
                    closeStr = String.format("%s/%s %s", messageSourceUtil.getMessage(CHARGE_STATION_TO_CLOSE, language),openTimeStr, messageSourceUtil.getMessage(CHARGE_STATION_IN_OPEN, language));
                }
            }

            String stationStatusStr = isOpen  ? openStr : closeStr;
            log.info("setOpenTime isOpen : {}, openStr:{} , closeStr: {}" , isOpen, openStr,closeStr);
            // 如果open并且changingTime 是 "24:00"，那么 mutableAttributeStr就是 “全天营业”
            if(isOpen && chargingTime.equals("24:00")){
                appPileDetailVO.setIsAllDay(1);
                appPileDetailVO.setOpenTime(messageSourceUtil.getMessage(CHARGE_STATION_ALL_DAY_WORK, language));
            }else{
                appPileDetailVO.setIsAllDay(0);
                appPileDetailVO.setOpenTime(stationStatusStr);
            }
        } catch (Exception e) {
            log.error("OpLocationRepositoryImpl appDetail", e);
        }
    }

    private String convertWeekDay(Integer weekDay, String language){
        String weekDayStr = "";
        switch (weekDay) {
            case 1:
                weekDayStr = messageSourceUtil.getMessage(MONDAY, language);
                break;
            case 2:
                weekDayStr = messageSourceUtil.getMessage(TUESDAY, language);
                break;
            case 3:
                weekDayStr = messageSourceUtil.getMessage(WEDNESDAY, language);
                break;
            case 4:
                weekDayStr = messageSourceUtil.getMessage(THURSDAY, language);
                break;
            case 5:
                weekDayStr = messageSourceUtil.getMessage(FRIDAY, language);
                break;
            case 6:
                weekDayStr = messageSourceUtil.getMessage(SATURDAY, language);
                break;
            case 7:
                weekDayStr = messageSourceUtil.getMessage(SUNDAY, language);
                break;
            default:
                break;
        }
        return weekDayStr;
    }
    public boolean isFreeAutoStart(Long tariffId){
        if(tariffId == null){
            return false;
        }

        Result<CostModelBasicInfoVO> costModelBasicInfoVOResult = tariffFeignClient.queryTariffBasicInfoById(tariffId);
        log.info("============= isStartWithFree costModelBasicInfoVOResult of the tariffId: {}, the costModelBasicInfoVOResult: {}", tariffId, JSON.toJSON(costModelBasicInfoVOResult));
        if (null == costModelBasicInfoVOResult || org.apache.http.HttpStatus.SC_OK != costModelBasicInfoVOResult.getCode() || null == costModelBasicInfoVOResult.getData()) {
            return false;
        }
        CostModelBasicInfoVO costModelBasicInfoVO = costModelBasicInfoVOResult.getData();
        log.info("============= the costModelBasicInfoVO: {}", JSON.toJSON(costModelBasicInfoVO));
        //非免费自动自动充电
        if (costModelBasicInfoVO.getRuleModelType() != null && costModelBasicInfoVO.getAutoCharging() != null &&
                RuleModelTypeEnum.FREE.getCode().equals(costModelBasicInfoVO.getRuleModelType()) && 1 == costModelBasicInfoVO.getAutoCharging()) {
            return true;
        }
        return false;
    }

    public boolean isFreeStart(Long operatorId, String evseSn, List<String> tariffIdList){
        if(org.apache.commons.collections4.CollectionUtils.isEmpty(tariffIdList)){
            return false;
        }

        Boolean isFree = tariffFeignClient.judgeFreeByTariffId(tariffIdList).getData();
        log.info("isFree :{}", isFree);
        try{
            Map<String, Integer> userMarketingRuleMap = getUserMarketingRule(LoginUserHolder.getLoginUser().getId(), operatorId, evseSn);
            if (null != userMarketingRuleMap.get("discount")) {
                BigDecimal discount = BigDecimal.valueOf(userMarketingRuleMap.get("discount"));
                if (discount.compareTo(BigDecimal.valueOf(100)) == 0) {
                    isFree = true;
                }
            }
        }catch (Exception e){
            log.error("isFreeStart 异常: {}", e);
        }

        return isFree;
    }

    public Map<String, Integer> getUserMarketingRule(Long userId, Long operatorId, String evseSn) {
        log.info("============ getUserMarketingRule invoked, the userId: {}, operatorId: {}, evseSn: {}", userId, operatorId, evseSn);
        Map<String, Integer> userMarketingRuleMap = new HashMap<>();
        // 免费时长
        Integer freeTimes = 0;
        // 折扣率
        Integer discount = 0;
        if (null != userId && null != operatorId) {
            MarketingRuleDTO marketingRuleDTO = new MarketingRuleDTO();
            marketingRuleDTO.setSellerId(operatorId);
            marketingRuleDTO.setUserId(userId);
            marketingRuleDTO.setEveSn(evseSn);
            log.info("=========== the params when marketingRules invoked: {}", JSON.toJSONString(marketingRuleDTO));
            Result<List<MarketingRuleVO>> marketingRuleVOResult = pileUserFeign.marketingRules(marketingRuleDTO);
            log.info("=========== the marketingRuleVOList after marketingRules invoked: {}", JSON.toJSONString(marketingRuleVOResult));
            if (null != marketingRuleVOResult && org.apache.http.HttpStatus.SC_OK == marketingRuleVOResult.getCode() && CollUtil.isNotEmpty(marketingRuleVOResult.getData())) {
                List<MarketingRuleVO> marketingRuleVOS = marketingRuleVOResult.getData();
                // 折扣，取最大值
                MarketingRuleVO discountMarket = marketingRuleVOS.stream().filter(marketingRuleVO -> {
                    return marketingRuleVO.getRule() == 0;
                }).max(Comparator.comparing(MarketingRuleVO::getDiscount)).orElse(null);
                // 免费时长，取最大值
                MarketingRuleVO freeTimesMarket = marketingRuleVOS.stream().filter(marketingRuleVO -> {
                    return marketingRuleVO.getRule() == 1;
                }).max(Comparator.comparing(MarketingRuleVO::getDiscount)).orElse(null);
                if (null != discountMarket) {
                    discount = discountMarket.getDiscount();
                }
                if (null != freeTimesMarket) {
                    freeTimes = freeTimesMarket.getDiscount();
                }
            }
        }
        userMarketingRuleMap.put("freeTimes", freeTimes);
        userMarketingRuleMap.put("discount", discount);
        log.info("=========== the user marketingRules: {}", JSON.toJSONString(userMarketingRuleMap));
        return userMarketingRuleMap;
    }


    /**
     *
     * 预付金额
     *
     * 1） 后台配置主流货币对应的默认预付金额；
     *
     * 2） 如果场站设置了预付金额，取Max(默认预付金额，场站预付金额最小值， 货币最低消费金额， 启动费)
     *
     * showcase更新： 默认50， 取货币的配置默认值
     *
     * @param locationId
     * @return
     */
    public int getPrepayment(Long locationId, BigDecimal connectionFee, String currencyCode) {
        int prepayment = defaultPrepaymentAmount;
        //int currencyMinPrice = (int) Math.ceil(CurrencyTypeEnum.getCurrencyBasePrice(currencyCode));
        try{
            /*if(prepayment < currencyMinPrice){
                prepayment = currencyMinPrice;
            }*/

            String currencyCodePrepayment = currencyPrepayment.get("Prepayment_" + currencyCode);
            if((CurrencyTypeEnum.USD.getCode().toString().equals(currencyCode) ||
                    CurrencyTypeEnum.EUR.getCode().toString().equals(currencyCode) ||
                    CurrencyTypeEnum.CAD.getCode().toString().equals(currencyCode)) &&
                    StringUtils.isNotBlank(currencyCodePrepayment)){
                prepayment = Integer.parseInt(currencyCodePrepayment);
                return prepayment;
            }

            OpLocationEntity opLocationEntity = opLocationMapper.getLocationPrePayment(locationId);
            if(opLocationEntity != null && opLocationEntity.getPayMethod() != null && PayMethodEnum.ADVANCE_PAYMENT.getValue().equals(opLocationEntity.getPayMethod())){
                String prepaymentAmountTier = opLocationEntity.getPrepaymentAmountTier();
                prepayment = Integer.parseInt(prepaymentAmountTier.split(",")[0]);
            }

            if(connectionFee != null){
                int startFee = connectionFee.setScale(0, RoundingMode.CEILING).intValue();
                if(prepayment < startFee){
                    prepayment = startFee;
                }
            }
            if(StringUtils.isNotBlank(currencyCodePrepayment) && prepayment < Integer.parseInt(currencyCodePrepayment)){
                prepayment = Integer.parseInt(currencyCodePrepayment);
            }
        }catch (Exception e){
            log.error("getPrepayment 异常：{}", e);
        }

        prepayment =  new BigDecimal("1.5").multiply( new BigDecimal(prepayment)).setScale(0, RoundingMode.CEILING).intValue();
        return prepayment ;
    }

    @Override
    public H5PileDetailVO h5PileDetail(String sn, String encryptUserId) {
        log.info("h5PileDetail sn:{},  encryptUserId :{}", sn, encryptUserId);
        H5PileDetailVO h5PileDetailVO = H5PileDetailVO.builder()
                .sn(sn)
                .available(false)
                .build();
        Long userId = null;
        if(StringUtils.isNotBlank(encryptUserId)){
            String decryptUserId = aesUtil.decryptAes(encryptUserId.getBytes(), AES_ALGORITHM);
            log.info("h5PileDetail decryptUserId:{} ", decryptUserId);

            if(StringUtils.isBlank(decryptUserId)){
                throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
            }

            userId = Long.parseLong(decryptUserId);

            String tokenRedisKey = USER_TOKEN_KEY + userId;
            String tokenValue = stringRedisTemplate.opsForValue().get(tokenRedisKey);
            log.info("h5PileDetail tokenRedisKey:{}, tokenValue:{} ", tokenRedisKey, tokenValue);
            h5PileDetailVO.setToken(tokenValue);
        }else{
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            if(jwtInfo != null){
                userId = jwtInfo.getId();
            }
        }


        List<OpLocationEvseElasticDTO> opLocationEvseList =  opLocationEvseRepository.findListByPileSn(sn);
        if(org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationEvseList)){
            Long locationId = opLocationEvseList.get(0).getLocationId();
            h5PileDetailVO.setLocationId(locationId == null ? null : locationId.toString());
            Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(locationId);
            if (optionalOpLocationElasticDTO.isPresent() && optionalOpLocationElasticDTO.get().getPlatform() == 1) {
                OpLocationElasticDTO  opLocationElasticDTO = optionalOpLocationElasticDTO.get();
                //场站不开放
                if(opLocationElasticDTO.getOpenType() == 2){
                    h5PileDetailVO.setAvailable(false);
                    return h5PileDetailVO;
                }

                if(!opLocationEvseList.get(0).getSubscriptionCheck()){
                    h5PileDetailVO.setAvailable(false);
                    return h5PileDetailVO;
                }

                if(userId != null){
                    try{
                        EnergyBillVO energyBillVO = billFeignClient.getUnfinishedBillByUserId(userId.toString(), DeviceTypeEnum.BUSINESS_PILE.getValue()).getData();
                        log.info("h5PileDetail findLastBillByUserId  userId:{}, energyBillVO :{}", userId, JSON.toJSONString(energyBillVO));
                        if(energyBillVO != null){
                            String evseSn = energyBillVO.getEvseSn();
                            String gunNo = evseSn.split("_")[1];
                            String pileSn = evseSn.split("_")[0];
                            h5PileDetailVO.setChargingGunNo(gunNo);
                            h5PileDetailVO.setChargingSn(pileSn);
                            h5PileDetailVO.setChargingOrderSeq(energyBillVO.getOrderSeq());
                        }
                    }catch (Exception e1){
                        log.error("h5PileDetail findLastBillByUserId  userId:{} 异常：{}", userId, e1);
                    }
                }

                h5PileDetailVO.setAvailable(true);
            }
        }else{
            h5PileDetailVO.setAvailable(false);
        }
        log.info("h5PileDetailVO :{}", JSON.toJSONString(h5PileDetailVO));
        return h5PileDetailVO;
    }

    @Override
    public OperatorSupportInfoVO getOperatorSupportInfo(String sn) {
        String logo = "";
        String operatorId = "";
        OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseService.getLocationByPileSn(sn).getData();
        if(ocppLocationEVSEVO != null){
            operatorId = ocppLocationEVSEVO.getOperatorId();
        }

        if(StringUtils.isBlank(operatorId)){
            return new OperatorSupportInfoVO();
        }

        //查询logo
        QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
        opLocationOperationEntityQueryWrapper.eq("location_id", ocppLocationEVSEVO.getLocationId());
        opLocationOperationEntityQueryWrapper.select("logo_image_id");
        OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(opLocationOperationEntityQueryWrapper);
        if (opLocationOperationEntity != null && opLocationOperationEntity.getLogoImageId() != null) {
            OpImageEntity opImageEntity = opImageMapper.selectById(opLocationOperationEntity.getLogoImageId());
            if (opImageEntity != null) {
                logo = opImageEntity.getUrl();
            }
        }

        Result<SellerDetailVO> sellerDetailVOResult = pileMerchantUserFeign.detail(Long.parseLong(operatorId));
        if(sellerDetailVOResult != null && sellerDetailVOResult.getCode().equals(HttpCodeEnum.OK.getCode()) && sellerDetailVOResult.getData() != null){
            SellerDetailVO sellerDetailVO = sellerDetailVOResult.getData();

            String telephoneNumber = null;
            if(StringUtils.isNotBlank(sellerDetailVO.getPhoneNumber())){
                String code = CommonUtil.getCode(sellerDetailVO.getAccountCountryCode());
                if (code != null) {
                    code = code.trim();
                    if (!"".equals(code) && !code.startsWith("+")) {
                        code = "+" + code;
                    }
                    log.info("sellerDetailVO,code={}",code);

                    telephoneNumber = code + sellerDetailVO.getPhoneNumber();
                }
            }

            return OperatorSupportInfoVO.builder()
                    .sellerName(sellerDetailVO.getName())
                    .email(sellerDetailVO.getEmail())
                    .telephoneNumber(telephoneNumber)
                    .logo(logo)
                    .build();
        }

        return new OperatorSupportInfoVO();
    }

    @Override
    public String getLocationIdBySn(String sn) {
        OpLocationPileEvseDTO opLocationPileEvseDTO = opLocationPileEvseRepository.queryByPileSn(sn);
        if(opLocationPileEvseDTO != null){
            return opLocationPileEvseDTO.getLocationId().toString();
        }
        return null;
    }
}
