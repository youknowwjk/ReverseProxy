package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.monitor.dto.OpEvseStatusUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.ocpi.enums.ConnectorTypeEnum;
import com.autel.cloud.ocpi.enums.EVSEStatusEnum;
import com.autel.cloud.ocpi.enums.PowerTypeEnum;
import com.autel.cloud.ocpi.vo.location.ConnectorVO;
import com.autel.cloud.ocpi.vo.location.EVSEVO;
import com.autel.cloud.ocpi.vo.location.GeoLocationVO;
import com.autel.cloud.ocpi.vo.location.LocationVO;
import com.autel.cloud.pile.base.bo.CostRuleWeeksBO;
import com.autel.cloud.pile.base.bo.PileSortBO;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.common.LocationCommon;
import com.autel.cloud.pile.base.domain.convert.*;
import com.autel.cloud.pile.base.domain.model.OpLocationMenuQueryDto;
import com.autel.cloud.pile.base.domain.model.vo.location.LocationBaseVO;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.CommonUtilService;
import com.autel.cloud.pile.base.domain.service.impl.OpLocationMultiForStatisticServiceImpl;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.app.EvseExpandElasticDTO;
import com.autel.cloud.pile.base.dto.app.LocationDTO;
import com.autel.cloud.pile.base.dto.oicp.*;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.dto.tax.TaxDTO;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.feign.GoogleApi;
import com.autel.cloud.pile.base.infrastructure.amqp.MQSender;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.*;
import com.autel.cloud.pile.base.infrastructure.feign.*;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.MasterSlaveRelationDTO;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OicpFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.redis.OpLocationRedis;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.infrastructure.util.JudgeTimeUtils;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.infrastructure.util.WordUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ListSortUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.*;
import com.autel.cloud.pile.base.vo.location.LocationSimpleInfoQueryVO;
import com.autel.cloud.pile.bill.constant.RedisKeys;
import com.autel.cloud.pile.bill.dto.PileTypeVO;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.feign.DeviceClient;
import com.autel.cloud.pile.bill.feign.PileBillStationInterfaceFeign;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.autel.cloud.pile.bill.vo.MonthReportVO;
import com.autel.cloud.pile.user.api.dto.AddDataReqDTO;
import com.autel.cloud.pile.user.api.dto.DeleteDataReqDTO;
import com.autel.cloud.pile.user.api.dto.QuerySubTreeDTO;
import com.autel.cloud.pile.user.api.dto.SellerParamDTO;
import com.autel.cloud.pile.user.api.enums.*;
import com.autel.cloud.pile.user.api.feign.PileMerchantUserFeign;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.*;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.pile.user.api.vo.NodeVO;
import com.autel.cloud.pile.user.api.vo.OrganizationByNameVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.autel.cloud.pile.user.api.vo.UserOrgMenuVO;
import com.autel.cloud.tariff.dto.*;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelBasicInfoVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import feign.Feign;
import feign.FeignException;
import feign.Response;
import feign.Util;
import feign.codec.DecodeException;
import feign.codec.Decoder;
import feign.okhttp.OkHttpClient;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections.map.HashedMap;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.elasticsearch.common.geo.GeoDistance;
import org.elasticsearch.common.unit.DistanceUnit;
import org.elasticsearch.index.query.*;
import org.elasticsearch.search.aggregations.Aggregation;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.bucket.geogrid.GeoGrid;
import org.elasticsearch.search.aggregations.bucket.geogrid.GeoGridAggregationBuilder;
import org.elasticsearch.search.aggregations.bucket.geogrid.ParsedGeoHashGrid;
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms;
import org.elasticsearch.search.aggregations.bucket.terms.ParsedTerms;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.elasticsearch.search.aggregations.bucket.terms.TermsAggregationBuilder;
import org.elasticsearch.search.aggregations.metrics.GeoBoundsAggregationBuilder;
import org.elasticsearch.search.aggregations.metrics.ParsedCardinality;
import org.elasticsearch.search.aggregations.metrics.ParsedGeoBounds;
import org.elasticsearch.search.aggregations.metrics.ParsedTopHits;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.clients.elasticsearch7.ElasticsearchAggregations;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;
import org.springframework.data.elasticsearch.core.query.*;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StopWatch;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toMap;

/**
 * <p>
 * 场站表 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Log4j2
@Service
@RefreshScope
public class OpLocationRepositoryImpl extends ServiceImpl<OpLocationMapper, OpLocationEntity> implements OpLocationRepository {
    private static final String SUPPORT_UPGRADE_VERSION = "1.11";
    private static final String VERSION_INTERCEPT_JUDGE = ";";
    private static final String BILLING_RULES_HAS_UPDATED_NEED_TO_UPGRADE_FOR_LOOK = "50201486";
    private static final String X_APPID = "X-AppId";
    private static final int PLANNETEV_APPID = 8;
    private final OpLocationElastic opLocationElastic;
    private final OpImageMapper opImageMapper;
    private final OpLocationImageMapper opLocationImageMapper;
    private final OpLocationOperationMapper opLocationOperationMapper;
    private final OpLocationOpenTimeMapper opLocationOpenTimeMapper;
    private final OpLocationRedis opLocationRedis;
    private final OpLocationFacilityRepository opLocationFacilityRepository;
    // 地球半径，单位：公里
    private static final double EARTH_RADIUS = 6371.0;
    ExecutorService executorService = new ThreadPoolExecutor(5, 10, 1, TimeUnit.HOURS, new ArrayBlockingQueue<Runnable>(10), new ThreadPoolExecutor.AbortPolicy());
    ExecutorService executorService1 = new ThreadPoolExecutor(5, 10, 1, TimeUnit.HOURS, new ArrayBlockingQueue<Runnable>(10), new ThreadPoolExecutor.AbortPolicy());
    @Autowired
    private OpLocationMapper opLocationMapper;
    @Autowired
    private OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    private OpLocationPileEvseMapper opLocationPileEvseMapper;
    @Autowired
    private OpLocationConnectorMapper opLocationConnectorMapper;
    @Autowired
    private OplocationUserMapper opLocationUserMapper;
    @Resource
    private OpLocationERoamingSupportMapper oplocationERoamingSupportMapper;
    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;
    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Autowired
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Autowired
    private TariffAPPFeign tariffAPPFeign;
    @Autowired
    private TariffFeignClient tariffFeignClient;
    @Autowired
    private PileBillStationInterfaceFeign pileBillStationInterfaceFeign;
    @Autowired
    private OpCountryMapper opCountryMapper;
    @Autowired
    private OpLocationOpenTimeRepository opLocationOpenTimeRepository;
    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Autowired
    private OpImageRepository opImageRepository;
    @Autowired
    private OpLocationImageRepository opLocationImageRepository;
    @Autowired
    private PileUserServiceFeign pileUserServiceFeign;
    @Autowired
    private OpEvseBrandMapper opEvseBrandMapper;
    @Autowired
    private OpLocationMultiForStatisticServiceImpl opLocationMultiForStatisticService;
    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    @Lazy
    private RuleRepository ruleRepository;
    @Autowired
    private RabbitTemplate rabbitTemplate;
    @Resource
    private MQSender mqSender;
    @Resource
    private OicpFeignClientProxy oicpFeign;
    @Resource
    private WordUtil wordUtil;
    @Resource
    private RedisTemplate redisTemplate;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private MonitorFeignClient monitorFeignClient;
    @Resource
    private OpLocationEvseExpandElastic opLocationEvseExpandElastic;
    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;
    @Resource
    private HomePileFeignClient homePileFeignClient;
    @Resource
    private PileBaseConvert pileBaseConvert;
    @Resource
    private DeviceServiceFeign deviceServiceClient;
    @Value("${hubject.enable:false}")
    private String hubjectEnable;
    @Value("${support.upgrade.switch:false}")
    private Boolean upgradeSwitch;
    @Value("${pile.subscribe.enable:false}")
    private boolean subscribeEnable;
    @Value("${emsp.match.order:true}")
    private Boolean matchEmspOrder;

    @Autowired
    private LocationCommon locationCommon;
    @Value("#{${reserve-firmware.firmware-version.map:{}}}")
    private Map<String, String> supportReserveFirmwareVersionMap;

    @Resource
    private RedisUtil redisUtil;

    @Resource
    private PileMerchantUserFeign pileMerchantUserFeign;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private DeviceClient deviceClient;

    @Autowired
    private CommonUtilService commonUtilService;

    @Value("${emsp.autel.sellerId:1}")
    private Long emspAutelSellerId;

    @Value("${emsp.plantEV.sellerId:2}")
    private Long emspPlanetEVSellerId;

    @Autowired
    private SaasAccessFeignClient saasAccessFeignClient;


    public OpLocationRepositoryImpl(OpLocationElastic opLocationElastic,
                                    OpImageMapper opImageMapper,
                                    OpLocationImageMapper opLocationImageMapper,
                                    OpLocationOperationMapper opLocationOperationMapper,
                                    OpLocationOpenTimeMapper opLocationOpenTimeMapper,
                                    OpLocationRedis opLocationRedis,
                                    OpLocationFacilityRepository opLocationFacilityRepository) {
        this.opLocationElastic = opLocationElastic;
        this.opImageMapper = opImageMapper;
        this.opLocationImageMapper = opLocationImageMapper;
        this.opLocationOperationMapper = opLocationOperationMapper;
        this.opLocationOpenTimeMapper = opLocationOpenTimeMapper;
        this.opLocationRedis = opLocationRedis;
        this.opLocationFacilityRepository = opLocationFacilityRepository;
    }

    /**
     * 根据组织机构树构建组织机构的排序
     *
     * @param sourceTree 组织机构树
     */
    private static List<Long> buildOrgIdList(List<UserOrgMenuVO> sourceTree, List<Long> orgIdList) {
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(sourceTree)) {
            for (UserOrgMenuVO userOrgMenuVO : sourceTree) {
                orgIdList.add(userOrgMenuVO.getKey());
            }
            for (UserOrgMenuVO userOrgMenuVO : sourceTree) {
                List<UserOrgMenuVO> childrenList = userOrgMenuVO.getChildren();
                if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(childrenList)) {
                    orgIdList = buildOrgIdList(childrenList, orgIdList);
                }
            }
        }
        return orgIdList;
    }

    /**
     * 根据组织机构树构建组织机构的排序
     *
     * @param sourceTree 组织机构树
     */
    private static Map<Long, Integer> buildOrgIdNOMap(List<UserOrgMenuVO> sourceTree) {
        Map<Long, Integer> orgIdNOMap = new HashMap<>();
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(sourceTree)) {
            List<Long> orgIdList = buildOrgIdList(sourceTree, Lists.newArrayList());
            if (CollectionUtils.isNotEmpty(orgIdList)) {
                for (int i = 0; i <= orgIdList.size() - 1; i++) {
                    orgIdNOMap.put(orgIdList.get(i), i + 1);
                }
            }
        }
        return orgIdNOMap;
    }

    //1.1 合并启动费
    private static CostModelRuleDTO mergeStartPrice(CostModelRuleDTO resourceCostModelRuleDTO) {
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);
        try {
            List<CostRuleWeeksDTO> rules = costModelRuleDTO.getRules();
            //weeksRules的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                CostRuleWeeksDTO costRuleWeeksDTO = rules.get(0);
                List<Integer> weeks = new ArrayList<>(Arrays.asList(1, 2, 3, 4, 5, 6, 7));
                costRuleWeeksDTO.setWeeks(weeks);
                List<CostRulesDTO> weeksRules = new ArrayList<>();
                CostRulesDTO costRulesDTO = new CostRulesDTO();
                costRulesDTO.setStartPrice(resourceCostModelRuleDTO.getStartPrice());
                costRulesDTO.setBeginHour(0);
                costRulesDTO.setEndHour(24);
                weeksRules.add(costRulesDTO);
                costRuleWeeksDTO.setWeeksRules(weeksRules);
                List<CostRuleWeeksDTO> startRules = new ArrayList<>();
                startRules.add(costRuleWeeksDTO);
                costModelRuleDTO.setRules(startRules);
            }
            //如果启动费为0，不展示
            if (CollectionUtils.isNotEmpty(costModelRuleDTO.getRules()) && costModelRuleDTO.getRules().size() == 1) {
                CostRuleWeeksDTO rule = costModelRuleDTO.getRules().get(0);
                List<CostRulesDTO> weeksRules = rule.getWeeksRules();
                //只有一个0点到24点
                if (CollectionUtils.isNotEmpty(weeksRules) && weeksRules.size() == 1) {
                    CostRulesDTO costRulesDTO = weeksRules.get(0);
                    if (costRulesDTO.getStartPrice().equals(new BigDecimal(0))) {
                        costModelRuleDTO = new CostModelRuleDTO();
                    }
                }
            }

        } catch (Exception e) {
            log.info("合并启动费失败：" + e);
        }
        return costModelRuleDTO;
    }

    //1.2合并启动费--weekRule的合并
    private static MergeWeekRuleDTO mergeStartPriceWeeksRules(MergeWeekRuleDTO mergeWeekRuleDTO) {

        if (CollectionUtils.isNotEmpty(mergeWeekRuleDTO.getWeeksRules()) &&
                mergeWeekRuleDTO.getWeeksRules().size() >= 2 &&
                mergeWeekRuleDTO.getParseIndex() <= mergeWeekRuleDTO.getWeeksRules().size() - 2) {

            List<CostRulesDTO> weeksRules = mergeWeekRuleDTO.getWeeksRules();
            int parseIndex = mergeWeekRuleDTO.getParseIndex();

            CostRulesDTO costRulesDTO = weeksRules.get(parseIndex);
            CostRulesDTO nextCostRulesDTO = weeksRules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            if (costRulesDTO.getEndTime().equals(nextCostRulesDTO.getBeginTime()) &&
                    costRulesDTO.getStartPrice().equals(nextCostRulesDTO.getStartPrice())) {
                costRulesDTO.setEndHour(nextCostRulesDTO.getEndHour());
                costRulesDTO.setEndMinute(nextCostRulesDTO.getEndMinute());
                List<CostRulesDTO> newWeeksRules = Lists.newArrayList(weeksRules);
                newWeeksRules.remove(parseIndex + 1);
                newWeeksRules.remove(parseIndex);
                newWeeksRules.add(parseIndex, costRulesDTO);
                mergeWeekRuleDTO.setWeeksRules(newWeeksRules);
            } else {
                //不需要合并,解析索引加一位
                mergeWeekRuleDTO.setParseIndex(mergeWeekRuleDTO.getParseIndex() + 1);
            }
            return mergeStartPriceWeeksRules(mergeWeekRuleDTO);
        } else {
            return mergeWeekRuleDTO;
        }
    }

    //1.3 合并启动费--rule的合并
    private static MergeRuleDTO mergeStartPriceRules(MergeRuleDTO mergeRuleDTO) {
        if (CollectionUtils.isNotEmpty(mergeRuleDTO.getRules()) &&
                mergeRuleDTO.getRules().size() >= 2 &&
                mergeRuleDTO.getParseIndex() <= mergeRuleDTO.getRules().size() - 2) {

            List<CostRuleWeeksDTO> rules = mergeRuleDTO.getRules();
            int parseIndex = mergeRuleDTO.getParseIndex();

            CostRuleWeeksDTO rule1 = rules.get(parseIndex);
            CostRuleWeeksDTO rule2 = rules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            boolean merge = true;
            if (CollectionUtils.isNotEmpty(rule1.getWeeksRules()) &&
                    CollectionUtils.isNotEmpty(rule2.getWeeksRules()) &&
                    rule1.getWeeksRules().size() == rule2.getWeeksRules().size()) {
                for (int i = 0; i <= rule1.getWeeksRules().size() - 1; i++) {
                    CostRulesDTO weekRule1 = rule1.getWeeksRules().get(i);
                    CostRulesDTO weekRule2 = rule2.getWeeksRules().get(i);
                    if (!Objects.equals(weekRule1.getBeginTime(), weekRule2.getBeginTime()) ||
                            !Objects.equals(weekRule1.getEndTime(), weekRule2.getEndTime()) ||
                            !Objects.equals(weekRule1.getStartPrice(), weekRule2.getStartPrice())) {
                        merge = false;
                        break;
                    }
                }
            } else {
                merge = false;
            }
            if (merge) {
                rules.remove(parseIndex + 1);
                rules.remove(parseIndex);
                List<Integer> rule1WeekDays = rule1.getWeeks();
                List<Integer> rule2WeekDas = rule2.getWeeks();
                rule1WeekDays.addAll(rule2WeekDas);
                Collections.sort(rule1WeekDays);
                rule1.setWeeks(rule1WeekDays);
                rules.add(parseIndex, rule1);
                mergeRuleDTO.setRules(rules);
            } else {
                mergeRuleDTO.setParseIndex(parseIndex + 1);
            }
            return mergeStartPriceRules(mergeRuleDTO);
        } else {
            return mergeRuleDTO;
        }
    }

    //2.1 合并电量费
    private static CostModelRuleDTO mergeUnitPrice(CostModelRuleDTO resourceCostModelRuleDTO) {
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);
        try {
            List<CostRuleWeeksDTO> rules = costModelRuleDTO.getRules();
            //weeksRules的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                for (CostRuleWeeksDTO costRuleWeeksDTO : rules) {
                    if (CollectionUtils.isNotEmpty(costRuleWeeksDTO.getWeeksRules())) {
                        MergeWeekRuleDTO mergeWeekRuleDTO = new MergeWeekRuleDTO();
                        int parseIndex = 0;
                        mergeWeekRuleDTO.setParseIndex(parseIndex);
                        mergeWeekRuleDTO.setWeeksRules(costRuleWeeksDTO.getWeeksRules());
                        mergeWeekRuleDTO = mergeUnitPriceWeeksRules(mergeWeekRuleDTO);
                        List<CostRulesDTO> newWeeksRules = mergeWeekRuleDTO.getWeeksRules();
                        costRuleWeeksDTO.setWeeksRules(newWeeksRules);
                    }
                }
            }

            //rule的拆分
            rules = splitRules(rules);
            rules = sortRules(rules);
            //rule的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                MergeRuleDTO mergeRuleDTO = new MergeRuleDTO();
                int parseIndex = 0;
                mergeRuleDTO.setParseIndex(parseIndex);
                mergeRuleDTO.setRules(rules);
                mergeRuleDTO = mergeUnitPriceRules(mergeRuleDTO);
                List<CostRuleWeeksDTO> sortRules = sortRules(mergeRuleDTO.getRules());
                costModelRuleDTO.setRules(sortRules);
            }

            //如果七天的电量费都为0，不展示
            if (CollectionUtils.isNotEmpty(costModelRuleDTO.getRules()) && costModelRuleDTO.getRules().size() == 1) {
                CostRuleWeeksDTO rule = costModelRuleDTO.getRules().get(0);
                //只有一组，包含7天
                if (CollectionUtils.isNotEmpty(rule.getWeeks()) && rule.getWeeks().size() == 7) {
                    List<CostRulesDTO> weeksRules = rule.getWeeksRules();
                    //只有一个0点到24点
                    if (CollectionUtils.isNotEmpty(weeksRules) && weeksRules.size() == 1) {
                        CostRulesDTO costRulesDTO = weeksRules.get(0);
                        if (costRulesDTO.getUnitPrice().equals(new BigDecimal(0))) {
                            costModelRuleDTO = new CostModelRuleDTO();
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.info("合并电量费失败：" + e);
        }
        return costModelRuleDTO;
    }

    //2.2.合并电量费--weekRule的合并
    private static MergeWeekRuleDTO mergeUnitPriceWeeksRules(MergeWeekRuleDTO mergeWeekRuleDTO) {

        if (CollectionUtils.isNotEmpty(mergeWeekRuleDTO.getWeeksRules()) &&
                mergeWeekRuleDTO.getWeeksRules().size() >= 2 &&
                mergeWeekRuleDTO.getParseIndex() <= mergeWeekRuleDTO.getWeeksRules().size() - 2) {

            List<CostRulesDTO> weeksRules = mergeWeekRuleDTO.getWeeksRules();
            int parseIndex = mergeWeekRuleDTO.getParseIndex();

            CostRulesDTO costRulesDTO = weeksRules.get(parseIndex);
            CostRulesDTO nextCostRulesDTO = weeksRules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            if (costRulesDTO.getEndTime().equals(nextCostRulesDTO.getBeginTime()) &&
                    costRulesDTO.getUnitPrice().equals(nextCostRulesDTO.getUnitPrice())) {
                costRulesDTO.setEndHour(nextCostRulesDTO.getEndHour());
                costRulesDTO.setEndMinute(nextCostRulesDTO.getEndMinute());
                List<CostRulesDTO> newWeeksRules = Lists.newArrayList(weeksRules);
                newWeeksRules.remove(parseIndex + 1);
                newWeeksRules.remove(parseIndex);
                newWeeksRules.add(parseIndex, costRulesDTO);
                mergeWeekRuleDTO.setWeeksRules(newWeeksRules);
            } else {
                //不需要合并,解析索引加一位
                mergeWeekRuleDTO.setParseIndex(mergeWeekRuleDTO.getParseIndex() + 1);
            }
            return mergeUnitPriceWeeksRules(mergeWeekRuleDTO);
        } else {
            return mergeWeekRuleDTO;
        }
    }

    //2.3 合并电量费--rule的合并
    private static MergeRuleDTO mergeUnitPriceRules(MergeRuleDTO mergeRuleDTO) {
        if (CollectionUtils.isNotEmpty(mergeRuleDTO.getRules()) &&
                mergeRuleDTO.getRules().size() >= 2 &&
                mergeRuleDTO.getParseIndex() <= mergeRuleDTO.getRules().size() - 2) {

            List<CostRuleWeeksDTO> rules = mergeRuleDTO.getRules();
            int parseIndex = mergeRuleDTO.getParseIndex();

            CostRuleWeeksDTO rule1 = rules.get(parseIndex);
            CostRuleWeeksDTO rule2 = rules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            boolean merge = true;
            if (CollectionUtils.isNotEmpty(rule1.getWeeksRules()) &&
                    CollectionUtils.isNotEmpty(rule2.getWeeksRules()) &&
                    rule1.getWeeksRules().size() == rule2.getWeeksRules().size()) {
                for (int i = 0; i <= rule1.getWeeksRules().size() - 1; i++) {
                    CostRulesDTO weekRule1 = rule1.getWeeksRules().get(i);
                    CostRulesDTO weekRule2 = rule2.getWeeksRules().get(i);
                    if (!Objects.equals(weekRule1.getBeginTime(), weekRule2.getBeginTime()) ||
                            !Objects.equals(weekRule1.getEndTime(), weekRule2.getEndTime()) ||
                            !Objects.equals(weekRule1.getUnitPrice(), weekRule2.getUnitPrice())) {
                        merge = false;
                        break;
                    }
                }
            } else {
                merge = false;
            }
            if (merge) {
                rules.remove(parseIndex + 1);
                rules.remove(parseIndex);
                List<Integer> rule1WeekDays = rule1.getWeeks();
                List<Integer> rule2WeekDas = rule2.getWeeks();
                rule1WeekDays.addAll(rule2WeekDas);
                Collections.sort(rule1WeekDays);
                rule1.setWeeks(rule1WeekDays);
                rules.add(parseIndex, rule1);
                mergeRuleDTO.setRules(rules);
            } else {
                mergeRuleDTO.setParseIndex(parseIndex + 1);
            }
            return mergeUnitPriceRules(mergeRuleDTO);
        } else {
            return mergeRuleDTO;
        }
    }

    //3.1 合并时长费
    private static CostModelRuleDTO mergeTimePrice(CostModelRuleDTO resourceCostModelRuleDTO) {
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);
        try {
            List<CostRuleWeeksDTO> rules = costModelRuleDTO.getRules();
            //weeksRules的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                for (CostRuleWeeksDTO costRuleWeeksDTO : rules) {
                    if (CollectionUtils.isNotEmpty(costRuleWeeksDTO.getWeeksRules())) {
                        MergeWeekRuleDTO mergeWeekRuleDTO = new MergeWeekRuleDTO();
                        int parseIndex = 0;
                        mergeWeekRuleDTO.setParseIndex(parseIndex);
                        mergeWeekRuleDTO.setWeeksRules(costRuleWeeksDTO.getWeeksRules());
                        mergeWeekRuleDTO = mergeTimePriceWeeksRules(mergeWeekRuleDTO);
                        List<CostRulesDTO> newWeeksRules = mergeWeekRuleDTO.getWeeksRules();
                        costRuleWeeksDTO.setWeeksRules(newWeeksRules);
                    }
                }
            }
            //rule的拆分
            rules = splitRules(rules);
            rules = sortRules(rules);
            //rule的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                MergeRuleDTO mergeRuleDTO = new MergeRuleDTO();
                int parseIndex = 0;
                mergeRuleDTO.setParseIndex(parseIndex);
                mergeRuleDTO.setRules(rules);
                mergeRuleDTO = mergeTimePriceRules(mergeRuleDTO);
                List<CostRuleWeeksDTO> sortRules = sortRules(mergeRuleDTO.getRules());
                costModelRuleDTO.setRules(sortRules);
            }
            //如果七天的时长都为0，不展示
            if (CollectionUtils.isNotEmpty(costModelRuleDTO.getRules()) && costModelRuleDTO.getRules().size() == 1) {
                CostRuleWeeksDTO rule = costModelRuleDTO.getRules().get(0);
                //只有一组，包含7天
                if (CollectionUtils.isNotEmpty(rule.getWeeks()) && rule.getWeeks().size() == 7) {
                    List<CostRulesDTO> weeksRules = rule.getWeeksRules();
                    //只有一个0点到24点
                    if (CollectionUtils.isNotEmpty(weeksRules) && weeksRules.size() == 1) {
                        CostRulesDTO costRulesDTO = weeksRules.get(0);
                        if (costRulesDTO.getTimePrice().equals(new BigDecimal(0))) {
                            costModelRuleDTO = new CostModelRuleDTO();
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.info("合并时长费失败：" + e);
        }
        return costModelRuleDTO;
    }

    //3.2.合并时长费--weekRule的合并
    private static MergeWeekRuleDTO mergeTimePriceWeeksRules(MergeWeekRuleDTO mergeWeekRuleDTO) {

        if (CollectionUtils.isNotEmpty(mergeWeekRuleDTO.getWeeksRules()) &&
                mergeWeekRuleDTO.getWeeksRules().size() >= 2 &&
                mergeWeekRuleDTO.getParseIndex() <= mergeWeekRuleDTO.getWeeksRules().size() - 2) {

            List<CostRulesDTO> weeksRules = mergeWeekRuleDTO.getWeeksRules();
            int parseIndex = mergeWeekRuleDTO.getParseIndex();

            CostRulesDTO costRulesDTO = weeksRules.get(parseIndex);
            CostRulesDTO nextCostRulesDTO = weeksRules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            if (costRulesDTO.getEndTime().equals(nextCostRulesDTO.getBeginTime()) &&
                    costRulesDTO.getTimePrice().equals(nextCostRulesDTO.getTimePrice())) {
                costRulesDTO.setEndHour(nextCostRulesDTO.getEndHour());
                costRulesDTO.setEndMinute(nextCostRulesDTO.getEndMinute());
                List<CostRulesDTO> newWeeksRules = Lists.newArrayList(weeksRules);
                newWeeksRules.remove(parseIndex + 1);
                newWeeksRules.remove(parseIndex);
                newWeeksRules.add(parseIndex, costRulesDTO);
                mergeWeekRuleDTO.setWeeksRules(newWeeksRules);
            } else {
                //不需要合并,解析索引加一位
                mergeWeekRuleDTO.setParseIndex(mergeWeekRuleDTO.getParseIndex() + 1);
            }
            return mergeTimePriceWeeksRules(mergeWeekRuleDTO);
        } else {
            return mergeWeekRuleDTO;
        }
    }

    //3.3 合并时长费--rule的合并
    private static MergeRuleDTO mergeTimePriceRules(MergeRuleDTO mergeRuleDTO) {
        if (CollectionUtils.isNotEmpty(mergeRuleDTO.getRules()) &&
                mergeRuleDTO.getRules().size() >= 2 &&
                mergeRuleDTO.getParseIndex() <= mergeRuleDTO.getRules().size() - 2) {

            List<CostRuleWeeksDTO> rules = mergeRuleDTO.getRules();
            int parseIndex = mergeRuleDTO.getParseIndex();

            CostRuleWeeksDTO rule1 = rules.get(parseIndex);
            CostRuleWeeksDTO rule2 = rules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            boolean merge = true;
            if (CollectionUtils.isNotEmpty(rule1.getWeeksRules()) &&
                    CollectionUtils.isNotEmpty(rule2.getWeeksRules()) &&
                    rule1.getWeeksRules().size() == rule2.getWeeksRules().size()) {
                for (int i = 0; i <= rule1.getWeeksRules().size() - 1; i++) {
                    CostRulesDTO weekRule1 = rule1.getWeeksRules().get(i);
                    CostRulesDTO weekRule2 = rule2.getWeeksRules().get(i);
                    if (!Objects.equals(weekRule1.getBeginTime(), weekRule2.getBeginTime()) ||
                            !Objects.equals(weekRule1.getEndTime(), weekRule2.getEndTime()) ||
                            !Objects.equals(weekRule1.getTimePrice(), weekRule2.getTimePrice())) {
                        merge = false;
                        break;
                    }
                }
            } else {
                merge = false;
            }
            if (merge) {
                rules.remove(parseIndex + 1);
                rules.remove(parseIndex);
                List<Integer> rule1WeekDays = rule1.getWeeks();
                List<Integer> rule2WeekDas = rule2.getWeeks();
                rule1WeekDays.addAll(rule2WeekDas);
                Collections.sort(rule1WeekDays);
                rule1.setWeeks(rule1WeekDays);
                rules.add(parseIndex, rule1);
                mergeRuleDTO.setRules(rules);
            } else {
                mergeRuleDTO.setParseIndex(parseIndex + 1);
            }
            return mergeTimePriceRules(mergeRuleDTO);
        } else {
            return mergeRuleDTO;
        }
    }

    //4.1 合并停车费
    private static CostModelRuleDTO mergeParkPrice(CostModelRuleDTO resourceCostModelRuleDTO) {
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);
        try {
            List<CostRuleWeeksDTO> rules = costModelRuleDTO.getRules();
            //weeksRules的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                for (CostRuleWeeksDTO costRuleWeeksDTO : rules) {
                    if (CollectionUtils.isNotEmpty(costRuleWeeksDTO.getWeeksRules())) {
                        MergeWeekRuleDTO mergeWeekRuleDTO = new MergeWeekRuleDTO();
                        int parseIndex = 0;
                        mergeWeekRuleDTO.setParseIndex(parseIndex);
                        mergeWeekRuleDTO.setWeeksRules(costRuleWeeksDTO.getWeeksRules());
                        mergeWeekRuleDTO = mergeParkPriceWeeksRules(mergeWeekRuleDTO);
                        List<CostRulesDTO> newWeeksRules = mergeWeekRuleDTO.getWeeksRules();
                        costRuleWeeksDTO.setWeeksRules(newWeeksRules);
                    }
                }
            }
            //rule的拆分
            rules = splitRules(rules);
            rules = sortRules(rules);
            //rule的合并
            if (CollectionUtils.isNotEmpty(rules)) {
                MergeRuleDTO mergeRuleDTO = new MergeRuleDTO();
                int parseIndex = 0;
                mergeRuleDTO.setParseIndex(parseIndex);
                mergeRuleDTO.setRules(rules);
                mergeRuleDTO = mergeParkPriceRules(mergeRuleDTO);
                List<CostRuleWeeksDTO> sortRules = sortRules(mergeRuleDTO.getRules());
                costModelRuleDTO.setRules(sortRules);
            }
            //如果七天的时长都为0，不展示
            if (CollectionUtils.isNotEmpty(costModelRuleDTO.getRules()) && costModelRuleDTO.getRules().size() == 1) {
                CostRuleWeeksDTO rule = costModelRuleDTO.getRules().get(0);
                //只有一组，包含7天
                if (CollectionUtils.isNotEmpty(rule.getWeeks()) && rule.getWeeks().size() == 7) {
                    List<CostRulesDTO> weeksRules = rule.getWeeksRules();
                    //只有一个0点到24点
                    if (CollectionUtils.isNotEmpty(weeksRules) && weeksRules.size() == 1) {
                        CostRulesDTO costRulesDTO = weeksRules.get(0);
                        if (costRulesDTO.getParkingPrice().equals(new BigDecimal(0))) {
                            costModelRuleDTO = new CostModelRuleDTO();
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.info("合并时长费失败：" + e);
        }
        return costModelRuleDTO;
    }

    //4.2.合并停车费--weekRule的合并
    private static MergeWeekRuleDTO mergeParkPriceWeeksRules(MergeWeekRuleDTO mergeWeekRuleDTO) {

        if (CollectionUtils.isNotEmpty(mergeWeekRuleDTO.getWeeksRules()) &&
                mergeWeekRuleDTO.getWeeksRules().size() >= 2 &&
                mergeWeekRuleDTO.getParseIndex() <= mergeWeekRuleDTO.getWeeksRules().size() - 2) {

            List<CostRulesDTO> weeksRules = mergeWeekRuleDTO.getWeeksRules();
            int parseIndex = mergeWeekRuleDTO.getParseIndex();

            CostRulesDTO costRulesDTO = weeksRules.get(parseIndex);
            CostRulesDTO nextCostRulesDTO = weeksRules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            if (costRulesDTO.getEndTime().equals(nextCostRulesDTO.getBeginTime()) &&
                    costRulesDTO.getParkingPrice().equals(nextCostRulesDTO.getParkingPrice())) {
                costRulesDTO.setEndHour(nextCostRulesDTO.getEndHour());
                costRulesDTO.setEndMinute(nextCostRulesDTO.getEndMinute());
                List<CostRulesDTO> newWeeksRules = Lists.newArrayList(weeksRules);
                newWeeksRules.remove(parseIndex + 1);
                newWeeksRules.remove(parseIndex);
                newWeeksRules.add(parseIndex, costRulesDTO);
                mergeWeekRuleDTO.setWeeksRules(newWeeksRules);
            } else {
                //不需要合并,解析索引加一位
                mergeWeekRuleDTO.setParseIndex(mergeWeekRuleDTO.getParseIndex() + 1);
            }
            return mergeParkPriceWeeksRules(mergeWeekRuleDTO);
        } else {
            return mergeWeekRuleDTO;
        }
    }

    //4.3 合并停车费--rule的合并
    private static MergeRuleDTO mergeParkPriceRules(MergeRuleDTO mergeRuleDTO) {
        if (CollectionUtils.isNotEmpty(mergeRuleDTO.getRules()) &&
                mergeRuleDTO.getRules().size() >= 2 &&
                mergeRuleDTO.getParseIndex() <= mergeRuleDTO.getRules().size() - 2) {

            List<CostRuleWeeksDTO> rules = mergeRuleDTO.getRules();
            int parseIndex = mergeRuleDTO.getParseIndex();

            CostRuleWeeksDTO rule1 = rules.get(parseIndex);
            CostRuleWeeksDTO rule2 = rules.get(parseIndex + 1);
            //结束时间等于开始时间并且价格一样,合并
            boolean merge = true;
            if (CollectionUtils.isNotEmpty(rule1.getWeeksRules()) &&
                    CollectionUtils.isNotEmpty(rule2.getWeeksRules()) &&
                    rule1.getWeeksRules().size() == rule2.getWeeksRules().size()) {
                for (int i = 0; i <= rule1.getWeeksRules().size() - 1; i++) {
                    CostRulesDTO weekRule1 = rule1.getWeeksRules().get(i);
                    CostRulesDTO weekRule2 = rule2.getWeeksRules().get(i);
                    if (!Objects.equals(weekRule1.getBeginTime(), weekRule2.getBeginTime()) ||
                            !Objects.equals(weekRule1.getEndTime(), weekRule2.getEndTime()) ||
                            !Objects.equals(weekRule1.getParkingPrice(), weekRule2.getParkingPrice())) {
                        merge = false;
                        break;
                    }
                }
            } else {
                merge = false;
            }
            if (merge) {
                rules.remove(parseIndex + 1);
                rules.remove(parseIndex);
                List<Integer> rule1WeekDays = rule1.getWeeks();
                List<Integer> rule2WeekDas = rule2.getWeeks();
                rule1WeekDays.addAll(rule2WeekDas);
                Collections.sort(rule1WeekDays);
                rule1.setWeeks(rule1WeekDays);
                rules.add(parseIndex, rule1);
                mergeRuleDTO.setRules(rules);
            } else {
                mergeRuleDTO.setParseIndex(parseIndex + 1);
            }
            return mergeParkPriceRules(mergeRuleDTO);
        } else {
            return mergeRuleDTO;
        }
    }

    private static void initPrice(OpLocationCardVO opLocationCardVO, List<CostModelRuleDTO> costModelRuleList, int weekDay, BigDecimal unitPrice, BigDecimal timePrice, BigDecimal parkingPrice, BigDecimal startPrice, BigDecimal idlePrice, BigDecimal costPrice, String timeUnit, boolean haveUnitPrice, boolean haveTimePrice) {
        if (CollectionUtils.isNotEmpty(costModelRuleList)) {
            for (CostModelRuleDTO costModelRuleDTO : costModelRuleList) {
                //最低电量、时长
                List<CostRuleWeeksDTO> costRuleWeeksDTOList = costModelRuleDTO.getRules();
                if (CollectionUtils.isNotEmpty(costRuleWeeksDTOList)) {
                    for (CostRuleWeeksDTO costRuleWeeksDTO : costRuleWeeksDTOList) {
                        List<Integer> weeks = costRuleWeeksDTO.getWeeks();
                        if (weeks.contains(0) || weeks.contains(weekDay)) {
                            List<CostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                            if (CollectionUtils.isNotEmpty(weeksRules)) {
                                for (CostRulesDTO costRulesDTO : weeksRules) {
                                    BigDecimal tempUnitPrice = costRulesDTO.getUnitPrice();
//                                    BigDecimal tempTimePrice = costRulesDTO.getTimePrice();
//                                    BigDecimal tempCostPrice = costRulesDTO.getCostPrice();
//                                    if (costRuleWeeksDTO.getFeeModel().equals(FeeModelEnum.FIXED.getCode())) {
//                                        haveUnitPrice = true;
//                                        if (unitPrice == null || tempUnitPrice.compareTo(unitPrice) < 0) {
//                                            unitPrice = tempUnitPrice;
//                                        }
//                                    } else if (costRuleWeeksDTO.getFeeModel().equals(FeeModelEnum.USER_DEFINED.getCode())) {
//                                        haveTimePrice = true;
//                                        if (timePrice == null || tempTimePrice.compareTo(timePrice) < 0) {
//                                            timePrice = tempTimePrice;
//                                            if (costModelRuleDTO.getEnergyPrice() != null) {
//                                                timeUnit = costRulesDTO.getEnergyUnit().toString();
//                                            }
//                                        }
//                                    }
                                    haveUnitPrice = true;
                                    if (unitPrice == null || tempUnitPrice.compareTo(unitPrice) < 0) {
                                        unitPrice = tempUnitPrice;
                                    }
//                                    try {
//                                        if (costPrice == null || (tempCostPrice != null && tempCostPrice.compareTo(costPrice) < 0)) {
//                                            costPrice = tempCostPrice;
//                                        }
//                                    } catch (Exception e) {
//                                        log.info("init CostPrice 失败:" + e);
//                                    }
                                }
                            }
                        }
                    }
                }
                //最低停车费用
                if (!costModelRuleDTO.getRuleModelType().equals(RuleModelTypeEnum.HUBJECT.getCode())) {
                    //非hubject的停车费是在parkingFeeRuleDTO
                    ParkingFeeRuleDTO parkingFeeRuleDTO = costModelRuleDTO.getParkingFeeRuleDTO();
                    if (parkingFeeRuleDTO != null) {
                        List<PhaseParkingPriceRuleDTO> phaseParkingPriceRuleDTOList = parkingFeeRuleDTO.getPhaseParkingPriceRuleDTOList();
                        if (CollectionUtils.isNotEmpty(phaseParkingPriceRuleDTOList)) {
                            for (PhaseParkingPriceRuleDTO phaseParkingPriceRuleDTO : phaseParkingPriceRuleDTOList) {
                                BigDecimal tempParkingPrice = phaseParkingPriceRuleDTO.getParkingPrice();
                                if (parkingPrice == null || (!tempParkingPrice.equals(new BigDecimal(0)) && tempParkingPrice.compareTo(parkingPrice) < 0)) {
                                    parkingPrice = tempParkingPrice;
                                }
                            }
                        }
                    }
                } else {
                    //hubject的停车费是在rules里面
                    if (CollectionUtils.isNotEmpty(costRuleWeeksDTOList)) {
                        for (CostRuleWeeksDTO costRuleWeeksDTO : costRuleWeeksDTOList) {
                            List<Integer> weeks = costRuleWeeksDTO.getWeeks();
                            if (weeks.contains(0) || weeks.contains(weekDay)) {
                                List<CostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                                if (CollectionUtils.isNotEmpty(weeksRules)) {
                                    for (CostRulesDTO costRulesDTO : weeksRules) {
                                        BigDecimal tempParkingPrice = costRulesDTO.getParkingPrice();
                                        if (parkingPrice == null || (!tempParkingPrice.equals(new BigDecimal(0)) && tempParkingPrice.compareTo(parkingPrice) < 0)) {
                                            parkingPrice = tempParkingPrice;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                //最低启动费
                BigDecimal tempStartPrice = costModelRuleDTO.getStartPrice();
                if (tempStartPrice != null) {
                    if (startPrice == null || tempStartPrice.compareTo(startPrice) < 0) {
                        startPrice = tempStartPrice;
                    }
                }
                //最低滞留费用
                List<PhaseIdlePriceRuleDTO> phaseIdlePriceRuleDTOList = costModelRuleDTO.getPhaseIdlePriceRuleDTOList();
                if (CollectionUtils.isNotEmpty(phaseIdlePriceRuleDTOList)) {
                    for (PhaseIdlePriceRuleDTO phaseIdlePriceRuleDTO : phaseIdlePriceRuleDTOList) {
                        BigDecimal tempIdlePrice = phaseIdlePriceRuleDTO.getIdlePrice();
                        if (idlePrice == null || (!tempIdlePrice.equals(new BigDecimal(0)) && tempIdlePrice.compareTo(idlePrice) < 0)) {
                            idlePrice = tempIdlePrice;
                        }
                    }
                }
            }
        }
        log.info("电量费：" + unitPrice);
        log.info("时长费：" + timePrice);
        log.info("停车费：" + parkingPrice);
        log.info("启动费：" + startPrice);
        log.info("滞留费：" + idlePrice);
        log.info("成本费：" + costPrice);
        log.info("时长费单位：" + timeUnit);
        log.info("是否有电量费：" + haveUnitPrice);
        log.info("是否有时长费：" + haveTimePrice);
        opLocationCardVO.setUnitPrice(unitPrice);
        opLocationCardVO.setTimePrice(timePrice);
        opLocationCardVO.setParkingPrice(parkingPrice);
        opLocationCardVO.setStartPrice(startPrice);
        opLocationCardVO.setIdlePrice(idlePrice);
        opLocationCardVO.setCostPrice(costPrice);
        opLocationCardVO.setTimeUnit(timeUnit);
        opLocationCardVO.setHaveUnitPrice(haveUnitPrice);
        opLocationCardVO.setHaveTimePrice(haveTimePrice);
    }

    private static List<CostRuleWeeksDTO> sortRules(List<CostRuleWeeksDTO> rules) {
        try {
            List<CostRuleWeeksBO> costRuleWeeksBOS = DozerConvert.mapList(rules, CostRuleWeeksBO.class);
            Collections.sort(costRuleWeeksBOS);
            rules = DozerConvert.mapList(costRuleWeeksBOS, CostRuleWeeksDTO.class);

        } catch (Exception e) {
            log.info("排序错误");
        }
        return rules;
    }

    //rule的拆分，分成7个
    private static List<CostRuleWeeksDTO> splitRules(List<CostRuleWeeksDTO> rules) {
        if (CollectionUtils.isNotEmpty(rules)) {
            List<CostRuleWeeksDTO> newRules = Lists.newArrayList();
            for (CostRuleWeeksDTO rule : rules) {
                List<Integer> weeks = rule.getWeeks();
                for (Integer weekDay : weeks) {
                    CostRuleWeeksDTO newRule = DozerConvert.map(rule, CostRuleWeeksDTO.class);
                    newRule.setWeeks(Lists.newArrayList(weekDay));
                    newRules.add(newRule);
                }
            }
            rules = newRules;
        }
        return rules;
    }

    @Override
    public List<Long> selectByName(OpLocationQueryDTO opLocationQueryDTO) {
        return this.getBaseMapper().selectByName(opLocationQueryDTO.getName());
    }

    @Override
    public OpLocationDTO details(Long id) {
        List<Long> locationId = new ArrayList<>();
        locationId.add(id);
        Page<OpLocationDTO> opLocationDTOPage = pages(new OpLocationQueryDTO(), locationId);
        OpLocationDTO opLocationDTO = opLocationDTOPage.getRecords().get(0);

        QueryWrapper<OpLocationOpenTimeEntity> opLocationOpenTimeEntityQueryWrapper = new QueryWrapper<>();
        opLocationOpenTimeEntityQueryWrapper.eq(BaseConstant.LOCATION_ID, locationId.get(0));
        opLocationOpenTimeEntityQueryWrapper.eq(BaseConstant.DELETED, 0);
        List<OpLocationOpenTimeEntity> opLocationOpenTimeEntityList = opLocationOpenTimeMapper.selectList(opLocationOpenTimeEntityQueryWrapper);

        List<OpLocationOpenTimeDTO> opLocationOpenTimeDTOS = new LinkedList<>();

        for (OpLocationOpenTimeEntity op : opLocationOpenTimeEntityList) {
            // 将父类转换成子类
            String opString = JSON.toJSONString(op);
            log.info(opString);
            OpLocationOpenTimeDTO opLocationOpenTime = JSON.parseObject(opString, OpLocationOpenTimeDTO.class);
            opLocationOpenTimeDTOS.add(opLocationOpenTime);
        }
        opLocationDTO.setOpLocationOpenTimeList(opLocationOpenTimeDTOS);

        List<OpImageDTO> opImageDTOS = new LinkedList<>();
        List<Long> imageIds = opLocationImageMapper.selectByLocationId(locationId.get(0));
        for (Long imageId : imageIds) {
            OpImageEntity opImageEntity = opImageMapper.selectById(imageId);
            String opS = JSON.toJSONString(opImageEntity);
            OpImageDTO opImageDTO = JSON.parseObject(opS, OpImageDTO.class);
            opImageDTOS.add(opImageDTO);
        }
        opLocationDTO.setOpImageList(opImageDTOS);
        List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationId(id);
        log.info(BaseConstant.LOCATION_FACILITY_DTOLIST, JSON.toJSONString(locationFacilityDTOList));
        if (CollUtil.isNotEmpty(locationFacilityDTOList)) {
            opLocationDTO.setFacilityList(locationFacilityDTOList);
        }
        return opLocationDTO;
    }

    @Override
    public Result<OpLocationAddressDTO> getLocationAddress(Long id) {
        Optional<OpLocationElasticDTO> opLocationElasticDTOOptional = opLocationElastic.findById(id);
        if (!opLocationElasticDTOOptional.isPresent()) {
            OpLocationEntity opLocationEntity = opLocationMapper.selectById(id);
            if (opLocationEntity != null) {
                return Result.ofSucceed(OpLocationAddressDTO.builder().id(opLocationEntity.getId())
                        .name(opLocationEntity.getName()).address(opLocationEntity.getAddress())
                        .latitude(opLocationEntity.getLatitude()).longitude(opLocationEntity.getLongitude()).build());
            }
            return Result.ofSucceed(null);
        }
        OpLocationElasticDTO opLocationElasticDTO = opLocationElasticDTOOptional.get();
        return Result.ofSucceed(OpLocationAddressDTO.builder().id(opLocationElasticDTO.getId())
                .name(opLocationElasticDTO.getName()).address(opLocationElasticDTO.getAddress())
                .latitude(opLocationElasticDTO.getLatitude()).longitude(opLocationElasticDTO.getLongitude()).build());
    }

    @Override
    public Result<Boolean> initializeOpLocationProvinceToES() {
        //查询数据库中所有场站信息
        LambdaQueryWrapper<OpLocationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLocationEntity::getDeleted, 0);
        List<OpLocationEntity> opLocationEntityList = opLocationMapper.selectList(queryWrapper);
        Map<Long, OpLocationEntity> opLocationEntityMapById = opLocationEntityList.stream().collect(Collectors.toMap(OpLocationEntity::getId, Function.identity()));
        //查询ES中所有场站信息
        Iterable<OpLocationElasticDTO> opLocationElasticDTOS = opLocationElastic.findAll();
        opLocationElasticDTOS.forEach(opLocationElasticDTO -> {
            if (opLocationEntityMapById.get(opLocationElasticDTO.getId()) != null) {
                String province = opLocationEntityMapById.get(opLocationElasticDTO.getId()).getProvince();
                opLocationElasticDTO.setProvince(province);
                //更新ES中信息
                opLocationElastic.save(opLocationElasticDTO);
            }
        });
        return Result.ofSucceed(true);
    }

    @Override
    public OpStatisticsPileAndEvseVO pileAndEvse(Long id) {
        log.info("OpLocationRepositoryImpl.pileAndEvse.id = {}", id);
        OpLocationElasticDTO locationDto = opLocationElastic.findById(id).orElse(null);
        if (locationDto == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }

        OpStatisticsPileAndEvseVO pileAndEvseVO = new OpStatisticsPileAndEvseVO();
        //查询ES设备
        List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(id);
        log.info("OpLocationRepositoryImpl.pileAndEvse.esEVSEList = {}", esEVSEList);
        //构建ES站点设备数量map
        Map<Long, Integer> integerMap = new HashMap<>();
        integerMap.put(id, esEVSEList.size());
        //枪总数量
        int evseNum = 0;
        Integer evseInteger = integerMap.get(id);
        if (evseInteger != null) {
            evseNum = evseInteger;
        }
        pileAndEvseVO.setEvseNum(evseNum);
        //查询ES桩
        List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findAllByLocationId(id);
        log.info("OpLocationRepositoryImpl.pileAndEvse.esPileList = {}", esPileList);
        //构建站点桩数量map
        Map<Long, Integer> integerHashMap = new HashMap<>();
        integerHashMap.put(id, esPileList.size());
        //桩总数量
        int pileNum = 0;
        Integer pileInteger = integerHashMap.get(id);
        if (pileInteger != null) {
            pileNum = pileInteger;
        }
        pileAndEvseVO.setPileNum(pileNum);

        //不同功率枪数统计
        List<PowEvseVO> powEvseVOList = buildStationPowEvseVOList(esEVSEList);
        log.info("OpLocationRepositoryImpl.pileAndEvse.powEvseVOList = {}", powEvseVOList);
        pileAndEvseVO.setPowEvseVOList(powEvseVOList);
        //不同类型枪数统计
        List<TypeEvseVO> typeEvseVOList = buildStationTypeEvseVOList(esEVSEList);
        log.info("OpLocationRepositoryImpl.pileAndEvse.typeEvseVOList = {}", typeEvseVOList);
        pileAndEvseVO.setTypeEvseVOList(typeEvseVOList);
        //不同枪状态统计
        List<EvseStatusVO> evseStatusVOList = buildStationEvseStatusVO(esEVSEList);
        log.info("OpLocationRepositoryImpl.pileAndEvse.evseStatusVOList = {}", evseStatusVOList);
        pileAndEvseVO.setEvseStatusVOList(evseStatusVOList);

        //枪信息按照功率，逆排序展示
        List<EvsePowerInfoVO> powerInfoVOList = reversePower(esEVSEList);
        log.info("OpLocationRepositoryImpl.pileAndEvse.powerInfoVOList = {}", powerInfoVOList);
        pileAndEvseVO.setEvsePowerInfoVOList(powerInfoVOList);

        log.info("OpLocationRepositoryImpl.pileAndEvse.pileAndEvseVO = {}", pileAndEvseVO);
        return pileAndEvseVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public OpLocationEntity add(OpLocationDTO opLocationDTO) {

        log.info("OpLocationRepositoryImpl -> location_add setTheLatitudeAndLongitudeOfTheStation before : {}", JSON.toJSONString(opLocationDTO));

        // todo 对站点的纬度和经度进行特殊处理 https://jira.autel.com/browse/ESAAS-17208
        // 站点纬度
        @NotBlank(message = "站点所在纬度不能为空") String latitude = opLocationDTO.getLatitude();
        // 站点经度
        @NotBlank(message = "站点所在经度不能为空") String longitude = opLocationDTO.getLongitude();
        @NotNull(message = "zoneId is null") String zoneId = opLocationDTO.getZoneId();

        // 重新设置纬度和经度
        Map<String, String> latitudeAndLongitudeMap = locationCommon.setTheLatitudeAndLongitudeOfTheStation(latitude, longitude, null);

        // 重新设置后的纬度和经度
        latitude = latitudeAndLongitudeMap.get("latitude");
        longitude = latitudeAndLongitudeMap.get("longitude");

        // 重新赋值
        opLocationDTO.setLatitude(latitude);
        opLocationDTO.setLongitude(longitude);

        log.info("OpLocationRepositoryImpl -> location_add setTheLatitudeAndLongitudeOfTheStation after : {}", JSON.toJSONString(opLocationDTO));

        // 将数据插入 op_location 表
        OpLocationEntity opLocationEntity = OpLocationConvert.opLocationEntity(opLocationDTO);

        if(com.autel.cloud.pile.bill.enums.PayMethodEnum.ADVANCE_PAYMENT.getValue().equals(opLocationEntity.getPayMethod())){
            String prepayment = pileMerchantUserFeign.getSellerPrepayment(opLocationEntity.getOperatorId().toString()).getData();
            opLocationEntity.setPrepaymentAmountTier(prepayment);
            opLocationDTO.setReservationEnabled(false);
        }else{
            opLocationEntity.setPrepaymentAmountTier(null);
        }
        this.save(opLocationEntity);
        Long locationId = opLocationEntity.getId();

        //处理logo图片
        OpImageEntity opImageEntityLogo = null;
        if (opLocationDTO.getLogoImageDTO() != null) {
            opImageEntityLogo = OpImageConvert.opImageEntity(opLocationDTO, opLocationDTO.getLogoImageDTO());
            opImageMapper.insert(opImageEntityLogo);
        }

        //根据运营时间判断是否开放
        Integer openType = handlerOpenType(opLocationDTO);
        opLocationDTO.setOpenType(openType);
        //  根据locationId将数据插入 op_location_operation 表
        OpLocationOperationEntity opLocationOperationEntity = OpLocationOperationConvert.opLocationOperationEntity(opLocationDTO);
        opLocationOperationEntity.setLocationId(locationId);
        if (opImageEntityLogo != null) {
            opLocationOperationEntity.setLogoImageId(opImageEntityLogo.getId());
        }
        opLocationOperationMapper.insert(opLocationOperationEntity);

        // 根据 locationId 将数据插入 op_location_open_time 表，这里是多条数据
        if (CollectionUtils.isNotEmpty(opLocationDTO.getOpLocationOpenTimeList())) {
            List<OpLocationOpenTimeEntity> opLocationOpenTimeEntities = opLocationDTO.getOpLocationOpenTimeList().stream().map(e -> {
                OpLocationOpenTimeEntity opLocationOpenTimeEntity = OpLocationOpenTimeConvert.opLocationOpenTimeConvert(opLocationEntity, e);

                return opLocationOpenTimeEntity;
            }).collect(Collectors.toList());
            opLocationOpenTimeRepository.saveBatch(opLocationOpenTimeEntities);
        }
        List<OpLocationFacilityEntity> opLocationFacilityEntityList = new ArrayList<>();
        // 根据 locationId 将数据插入 op_location_facility 表，这里是多条数据
        if (CollectionUtils.isNotEmpty(opLocationDTO.getFacilityList())) {
            List<OpLocationFacilityDTO> opLocationFacilityDTOList = new ArrayList<>();
            opLocationDTO.getFacilityList().forEach(facilityDTO -> {
                OpLocationFacilityDTO opLocationFacilityDTO = new OpLocationFacilityDTO();
                BeanUtil.copyProperties(facilityDTO, opLocationFacilityDTO);
                opLocationFacilityDTOList.add(opLocationFacilityDTO);
            });
            opLocationFacilityEntityList = opLocationFacilityRepository.addOpLocationFacilityByLocationId(locationId, opLocationFacilityDTOList);
        }

        // 根据 locationId 将数据插入 op_image 表中，这里是多条数据
        if (CollectionUtils.isNotEmpty(opLocationDTO.getOpImageList())) {
            List<OpImageEntity> opImageEntities = opLocationDTO.getOpImageList().stream().map(e -> {
                OpImageEntity opImageEntity = OpImageConvert.opImageEntity(opLocationDTO, e);
                opImageEntity.setId(null);
                return opImageEntity;
            }).collect(Collectors.toList());
            opImageRepository.saveBatch(opImageEntities);
            List<OpLocationImageEntity> opLocationImageEntities = opImageEntities.stream().map(e -> {
                OpLocationImageEntity opLocationImageEntity = new OpLocationImageEntity();
                opLocationImageEntity.setImageId(e.getId());
                opLocationImageEntity.setLocationId(locationId);
                opLocationImageEntity.setDeleted(0);
                return opLocationImageEntity;
            }).collect(Collectors.toList());
            opLocationImageRepository.saveBatch(opLocationImageEntities);
        }

        List<AddDataReqDTO> addNodeList = new ArrayList<>();
        AddDataReqDTO addDataReqDTO = new AddDataReqDTO();
        addDataReqDTO.setNodeCode(opLocationEntity.getId().toString());
        addDataReqDTO.setNodeLevel(SubTreeEnum.STATION.getCode());
        addDataReqDTO.setNodeName(opLocationEntity.getName());
        addDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
        addDataReqDTO.setParentNodeCode(LoginUserUtil.getSellerId().toString());
        addDataReqDTO.setParentNodeLevel(SubTreeEnum.SELLER.getCode());
        addNodeList.add(addDataReqDTO);
        Result<Boolean> result = saasAccessFeignClient.addNde(addNodeList);
        log.info("添加场站修改权限树,result={}",JSON.toJSONString(result));
        if (ObjectUtils.isEmpty(result) || ObjectUtils.isEmpty(result.getData()) || Boolean.TRUE.equals(!result.getData())) {
            throw new MessageCodeException(PileBaseEnum.CREATE_LOCATION_FAILED);
        }
        //将数据存入es
        OpLocationElasticDTO opLocationElasticDTO = OpLocationElasticConvert.toOpLocationElasticDTO(opLocationEntity, opLocationOperationEntity);
        GeoPoint location = new GeoPoint(Double.parseDouble(opLocationDTO.getLatitude()), Double.parseDouble(opLocationDTO.getLongitude()));
        opLocationElasticDTO.setLocation(location);
        opLocationElasticDTO.setHouseNumber(opLocationEntity.getHouseNumber());
        opLocationElasticDTO.setReservationEnabled(opLocationEntity.getReservationEnabled());
        opLocationElasticDTO.setPlatform(1);
        //成本电价标识，暂时使用，后续删除
        opLocationElasticDTO.setPriceFlag(opLocationDTO.getPriceFlag());
        if (CollUtil.isNotEmpty(opLocationFacilityEntityList)) {
            opLocationElasticDTO.setFacility(JSON.toJSONString(opLocationFacilityEntityList));
        }
        opLocationElasticDTO.setPriceFlag(opLocationDTO.getPriceFlag());
        OpLocationElasticDTO elasticDTO = null;
        try {
            elasticDTO = opLocationElastic.save(opLocationElasticDTO);
        } catch (Exception e) {
            List<DeleteDataReqDTO> dataReqDTOList = new ArrayList<>();
            DeleteDataReqDTO deleteDataReqDTO = new DeleteDataReqDTO();
            deleteDataReqDTO.setNodeCode(opLocationEntity.getId().toString());
            deleteDataReqDTO.setNodeLevel(SubTreeEnum.STATION.getCode());
            deleteDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
            dataReqDTOList.add(deleteDataReqDTO);
            saasAccessFeignClient.deleteNde(dataReqDTOList);
        }
        log.info("OpLocationRepositoryImpl.add.elasticDTO = {}", elasticDTO);
        //新增场站发送MQ
        LocationForAddDTO dto = new LocationForAddDTO();
        dto.setLocationId(opLocationEntity.getId());
        dto.setUserId(LoginUserUtil.getUserId());
        dto.setSellerId(LoginUserUtil.getSellerId());
        if (dto.getUserId() != null) {
            log.info("OpLocationRepositoryImpl,send to mq dto={}", JSON.toJSONString(dto));
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_ADD_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_ADD_ROUTE, JSON.toJSONString(dto));
        }
        return opLocationEntity;
    }

    private Integer handlerOpenType(OpLocationDTO opLocationDTO) {
        Long operationDate = opLocationDTO.getOperationDate();
        if (operationDate == null) {
            return 1;
        }
        return Instant.now().toEpochMilli() - opLocationDTO.getOperationDate() > 0 ? 1 : 2;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean delete(Long id) {
        //校验场站下是否有桩
        Integer pileCount = opLocationPileEvseMapper.selectCount(Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                .eq(OpLocationPileEvseEntity::getLocationId, id)
                .eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE));
        if (pileCount > 0) {
            throw new MessageCodeException(PileBaseEnum.CHARGE_EXISTED, new Object[]{pileCount});
        }
        Integer locationCount = getBaseMapper().selectCount(Wrappers.lambdaQuery(OpLocationEntity.class)
                .eq(OpLocationEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationEntity::getId, id)
        );
        if (locationCount > 0) {
            getBaseMapper().deleteByLocationId(id);
        }
        Integer openCount = opLocationOpenTimeMapper.selectCount(Wrappers.lambdaQuery(OpLocationOpenTimeEntity.class)
                .eq(OpLocationOpenTimeEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationOpenTimeEntity::getLocationId, id));
        if (openCount > 0) {
            opLocationOpenTimeMapper.deleteByLocationId(id);
        }

        Integer operationCount = opLocationOperationMapper.selectCount(Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                .eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationOperationEntity::getLocationId, id));
        if (operationCount > 0) {
            opLocationOperationMapper.deleteByLocationId(id);
        }

        Integer imageCount = opLocationImageMapper.selectCount(Wrappers.lambdaQuery(OpLocationImageEntity.class)
                .eq(OpLocationImageEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationImageEntity::getLocationId, id));
        if (imageCount > 0) {
            opLocationImageMapper.deleteByLocationId(id);
        }

        List<Long> imageIds = opLocationImageMapper.selectByLocationId(id);
        for (Long imageId : imageIds) {
            opImageMapper.deleteByLocationId(imageId);
        }

        List<DeleteDataReqDTO> dataReqDTOList = new ArrayList<>();
        DeleteDataReqDTO deleteDataReqDTO = new DeleteDataReqDTO();
        deleteDataReqDTO.setNodeCode(id.toString());
        deleteDataReqDTO.setNodeLevel(SubTreeEnum.STATION.getCode());
        deleteDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
        dataReqDTOList.add(deleteDataReqDTO);
        Result<Boolean> result = saasAccessFeignClient.deleteNde(dataReqDTOList);
        log.info("删除场站修改权限树,result={}",JSON.toJSONString(result));
        if (ObjectUtils.isEmpty(result) || ObjectUtils.isEmpty(result.getData()) || !result.getData()){
            throw new MessageCodeException("_ktqTsQx1XEKL");
        }
        // 物理删除es中的数据
        opLocationElastic.deleteById(id);

        log.info("delete location send to mq location={}", id);
        rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_DELETE_ROUTE, JSON.toJSONString(id));
        return true;
    }

    /**
     * 场站是否存在
     *
     * @param groupIds 组织机构id集合
     * @return 场站是否存在
     */
    @Override
    public Boolean existsByGroupIdIn(List<Long> groupIds) {
        if (CollectionUtils.isEmpty(groupIds)) {
            return false;
        }
        boolean exit = true;
        //ES查询
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findByGroupIdIn(groupIds);
        if (CollectionUtils.isEmpty(esStationList)) {
            //数据库查询
            OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.existsByGroupIdIn(groupIds);
            if (opLocationOperationEntity == null) {
                exit = false;
            }
        }
        return exit;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean update(OpLocationDTO opLocationDTO) {
        StopWatch stopWatch = new StopWatch("更新场站信息");
        stopWatch.start("更新op_location表中的数据");

        Long locationId = opLocationDTO.getId();
        OpLocationEntity entity = this.selectById(locationId);
        if (entity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        //只允许从仅运营或仅广告改为运营+广告
        if (entity.getBusinessType() != opLocationDTO.getBusinessType()) {
            if ((entity.getBusinessType() == 1 || entity.getBusinessType() == 3) && opLocationDTO.getBusinessType() == 2) {
                log.info("update,edit businessType");
            }else {
                log.error("update,edit businessType forbid");
                throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
            }
        }

        log.info("OpLocationRepositoryImpl 场站修改 setTheLatitudeAndLongitudeOfTheStation before ：{}", JSON.toJSONString(opLocationDTO));

        // todo 对站点的纬度和经度进行特殊处理 https://jira.autel.com/browse/ESAAS-17208
        // 站点纬度
        @NotBlank(message = "站点所在纬度不能为空") String latitude = opLocationDTO.getLatitude();
        // 站点经度
        @NotBlank(message = "站点所在经度不能为空") String longitude = opLocationDTO.getLongitude();

        // 重新设置纬度和经度
        Map<String, String> latitudeAndLongitudeMap = locationCommon.setTheLatitudeAndLongitudeOfTheStation(latitude, longitude, locationId);

        // 重新设置后的纬度和经度
        latitude = latitudeAndLongitudeMap.get("latitude");
        longitude = latitudeAndLongitudeMap.get("longitude");

        // 重新赋值
        opLocationDTO.setLatitude(latitude);
        opLocationDTO.setLongitude(longitude);

        log.info("OpLocationRepositoryImpl 场站修改 setTheLatitudeAndLongitudeOfTheStation after ：{}", JSON.toJSONString(opLocationDTO));

        OpLocationEntity opLocationEntity = OpLocationConvert.opLocationEntity(opLocationDTO);

        if(com.autel.cloud.pile.bill.enums.PayMethodEnum.ADVANCE_PAYMENT.getValue().equals(opLocationEntity.getPayMethod())){
            String prepayment = pileMerchantUserFeign.getSellerPrepayment(opLocationEntity.getOperatorId().toString()).getData();
            opLocationEntity.setPrepaymentAmountTier(prepayment);
            opLocationDTO.setReservationEnabled(false);
        }else{
            opLocationEntity.setPrepaymentAmountTier(null);
        }

        this.getBaseMapper().updateById(opLocationEntity);

        stopWatch.stop();
        stopWatch.start("根据运营时间判断是否开放");
        //根据运营时间判断是否开放
        Integer openType = handlerOpenType(opLocationDTO);
        opLocationDTO.setOpenType(openType);
        // 更新 op_location_operation 表
        OpLocationOperationEntity opLocationOperationEntity = OpLocationOperationConvert.opLocationOperationEntity(opLocationDTO);
        opLocationOperationEntity.setLocationId(locationId);
        if (opLocationDTO.getLogoImageDTO() != null) {
            //处理场站logo
            OpImageEntity opImageEntity = OpImageConvert.opImageEntity(opLocationDTO, opLocationDTO.getLogoImageDTO());
            opImageEntity.setId(null);
            opImageMapper.insert(opImageEntity);
            opLocationOperationEntity.setLogoImageId(opImageEntity.getId());
        } else {
            opLocationOperationEntity.setLogoImageId(0L);
        }
        LambdaUpdateWrapper<OpLocationOperationEntity> opLocationOperationUpdateWrapper = Wrappers.lambdaUpdate();
        opLocationOperationUpdateWrapper.eq(OpLocationOperationEntity::getLocationId, locationId).eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE);
        opLocationOperationMapper.update(opLocationOperationEntity, opLocationOperationUpdateWrapper);

        //批量更新 op_location_open_time 表
        if (CollectionUtils.isNotEmpty(opLocationDTO.getOpLocationOpenTimeList())) {
            List<OpLocationOpenTimeDTO> opLocationOpenTimes = opLocationDTO.getOpLocationOpenTimeList();
            opLocationOpenTimeMapper.deleteByLocationId(locationId);
            List<OpLocationOpenTimeEntity> opLocationOpenTimeEntities = opLocationOpenTimes.stream().map(e -> {
                OpLocationOpenTimeEntity opLocationOpenTimeEntity = OpLocationOpenTimeConvert.opLocationOpenTimeConvert(opLocationEntity, e);
                return opLocationOpenTimeEntity;
            }).collect(Collectors.toList());
            opLocationOpenTimeRepository.saveBatch(opLocationOpenTimeEntities);
        }
        stopWatch.stop();
        stopWatch.start("更新场站图片");
        // 更新 op_image 表
        List<OpImageDTO> opImages = opLocationDTO.getOpImageList();
        opLocationImageMapper.deleteByLocationId(locationId);
        if (CollectionUtils.isNotEmpty(opImages)) {
            List<OpImageEntity> opImageEntities = opImages.stream().map(e -> {
                OpImageEntity opImageEntity = OpImageConvert.opImageEntity(opLocationDTO, e);
                opImageEntity.setId(null);
                return opImageEntity;
            }).collect(Collectors.toList());
            opImageRepository.saveBatch(opImageEntities);
            List<OpLocationImageEntity> opLocationImageEntities = opImageEntities.stream().map(e -> {
                OpLocationImageEntity opLocationImageEntity = new OpLocationImageEntity();
                opLocationImageEntity.setImageId(e.getId());
                opLocationImageEntity.setLocationId(opLocationEntity.getId());
                opLocationImageEntity.setDeleted(0);
                return opLocationImageEntity;
            }).collect(Collectors.toList());
            opLocationImageRepository.saveBatch(opLocationImageEntities);
        }
        stopWatch.stop();
        stopWatch.start("批量修改周边设施");
        List<OpLocationFacilityEntity> opLocationFacilityEntityList;
        List<OpLocationFacilityDTO> opLocationFacilityDTOList = new ArrayList<>();
        // 根据 locationId 将数据插入 op_location_facility 表，这里是多条数据
        if (CollectionUtils.isNotEmpty(opLocationDTO.getFacilityList())) {
            opLocationDTO.getFacilityList().forEach(facilityDTO -> {
                OpLocationFacilityDTO opLocationFacilityDTO = new OpLocationFacilityDTO();
                BeanUtil.copyProperties(facilityDTO, opLocationFacilityDTO);
                opLocationFacilityDTOList.add(opLocationFacilityDTO);
            });
        }
        opLocationFacilityEntityList = opLocationFacilityRepository.updateOpLocationFacilityByLocationId(opLocationEntity.getId(), opLocationFacilityDTOList);
        stopWatch.stop();
        // 更新es中的数据
        stopWatch.start("更新es中的数据");

        OpLocationElasticDTO locationElasticDto = this.opLocationElastic.findById(locationId).orElse(new OpLocationElasticDTO());

        stopWatch.stop();
        stopWatch.start("更新es中的数据");
        OpLocationEntity lastEntity = opLocationMapper.selectById(opLocationEntity.getId());
        //同一个service调用async异步是无效的
        //同步场站信息，桩信息，枪信息
        this.pileBaseConvert.handleRelation(locationElasticDto, lastEntity, opLocationOperationEntity);
        //写入周边设施
        if (CollUtil.isNotEmpty(opLocationFacilityEntityList)) {
            locationElasticDto.setFacility(JSON.toJSONString(opLocationFacilityEntityList));
        }
        locationElasticDto.setPriceFlag(opLocationDTO.getPriceFlag());
        this.opLocationElastic.save(locationElasticDto);

        //同步枪预约状态
        List<OpLocationEvseElasticDTO> evseDtoList = this.opLocationEvseElastic.findAllByLocationId(locationId);
        List<EvseInfoModifyDTO> modifyDtoList = new ArrayList<>();
        Long userId = LoginUserUtil.getUserId();
        if (CollectionUtils.isNotEmpty(evseDtoList)) {
            List<IndexQuery> updateList = new ArrayList<>(evseDtoList.size());

            evseDtoList.stream().forEach(evseDto -> {

                evseDto.setReservationEnabled(locationElasticDto.getReservationEnabled());
                evseDto.setLocationName(locationElasticDto.getName());

                EvseInfoModifyDTO dto = new EvseInfoModifyDTO();
                dto.setEvseSn(evseDto.getEvseSn());
                dto.setOperationType(EvseOperationTypeEnum.UPDATE.getCode());
                dto.setSellerId(locationElasticDto.getOperatorId());
                dto.setZoneId(locationElasticDto.getZoneId());
                dto.setUserId(userId);
                modifyDtoList.add(dto);
                updateList.add(new IndexQueryBuilder().withId(evseDto.getId().toString()).withObject(evseDto).build());
            });

            this.elasticsearchRestTemplate.bulkIndex(updateList, OpLocationEvseElasticDTO.class);
            this.elasticsearchRestTemplate.indexOps(OpLocationEvseElasticDTO.class).refresh();
        }

        List<OpLocationPileEvseElasticDTO> pileDtoList = null;
        if (!entity.getName().equals(opLocationDTO.getName())) {
            pileDtoList = this.opLocationPileEvseElastic.findAllByLocationId(locationId);
            if (CollectionUtils.isNotEmpty(pileDtoList)) {
                List<IndexQuery> updateList = new ArrayList<>(pileDtoList.size());

                pileDtoList.stream().forEach(pileDto -> {
                    pileDto.setLocationName(locationElasticDto.getName());
                    updateList.add(new IndexQueryBuilder().withId(pileDto.getId().toString()).withObject(pileDto).build());
                });
                this.elasticsearchRestTemplate.bulkIndex(updateList, OpLocationPileEvseElasticDTO.class);
                this.elasticsearchRestTemplate.indexOps(OpLocationPileEvseElasticDTO.class).refresh();
            }
        }

        //MQ同步
        try {
            // 场站信息修改成功之后需要发送MQ消息到车队那边
            if (modifyDtoList.size() > 0) {
                rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_EVSE_MODIFY_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, AmqpConstant.PILE_BASE_EVSE_MODIFY_ROUTE, JSON.toJSONString(modifyDtoList));
            }
            //同步充电设备扩展类延迟3秒发送，保证ES已经刷新分片（默认100毫秒）
            log.info("update location send to mq location={}", opLocationDTO.getId());
            ThreadPoolUtil.getScheduledExecutor().schedule(RunnableWrapper.of(() -> {
                rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_UPDATE_ROUTE, JSON.toJSONString(opLocationDTO.getId()));
            }), 3L, TimeUnit.SECONDS);
            //场站修改推送广告服务
            if (locationElasticDto.getBusinessType() == 2 || locationElasticDto.getBusinessType() == 3) {
                if (CollectionUtils.isEmpty(pileDtoList)) {
                    pileDtoList = this.opLocationPileEvseElastic.findAllByLocationId(locationId);
                }
                if (CollectionUtils.isNotEmpty(pileDtoList)) {
                    Set<String> pileSnList = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toSet());
                    List<ChargePointVO> listToUse = this.chargePointMerchantRelationService.findChargePointBySNs(pileSnList, locationElasticDto.getOperatorId());
                    if (CollectionUtils.isNotEmpty(listToUse)) {
                        OpLocationForAdvVO<List<OpLocationPileForAdvVO>> param = new OpLocationForAdvVO();
                        param.setOperationType("edit");
                        List<OpLocationPileForAdvVO> pileVoList = new ArrayList<>(pileDtoList.size());
                        Map<String, ChargePointVO> pileVoMap = listToUse.stream().collect(toMap(ChargePointVO::getSn, e -> e, (f, s) -> f));
                        pileDtoList.stream().forEach(pileDto -> {
                            OpLocationPileForAdvVO pileVo = new OpLocationPileForAdvVO();
                            pileVo.setLocationId(locationId);
                            pileVo.setLocationName(locationElasticDto.getName());
                            pileVo.setSellerId(locationElasticDto.getOperatorId());
                            pileVo.setPileSn(pileDto.getPileSn());
                            pileVo.setPileName(pileDto.getName());
                            ChargePointVO vo = pileVoMap.get(pileDto.getPileSn());
                            if (vo != null) {
                                pileVo.setPilePin(vo.getPin());
                                pileVo.setScreen1Pixel(vo.getScreen1Pixel());
                                pileVo.setScreen1Size(vo.getScreen1Size());
                                pileVo.setOverchargingPileFlag(vo.getOverchargingPileFlag());
                            }
                            pileVo.setTimeZone(locationElasticDto.getTimeZone());
                            pileVo.setZoneId(locationElasticDto.getZoneId());
                            String brandName = pileDto.getBrandName();
                            if (!org.springframework.util.StringUtils.hasText(brandName)) {
                                Long brandId = pileDto.getBrandId();
                                BrandEnum brandEnum = Arrays.stream(BrandEnum.values()).filter(e -> e.getCode().equals(brandId.intValue())).findFirst().orElse(null);
                                if (brandEnum != null) {
                                    brandName = brandEnum.getName();
                                }
                            }
                            pileVo.setBrandName(brandName);
                            pileVo.setThirdPart("Autel".equalsIgnoreCase(brandName) ? 0 : 1);
                            pileVoList.add(pileVo);
                        });
                        param.setData(pileVoList);

                        rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_EDIT_ADV_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_ADV_ROUTE, JSON.toJSONString(param));
                    }
                }
            }
        } catch (Exception e) {
            log.error("场站信息修改成功之后，推送充电枪信息给车队那边出现异常 : {}", e);
        }



        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return true;
    }

    @Override
    public List<OpLocationMenuDTO> getStationMenu() {
        if (UserUtil.getSellerId() == null) {
            return new ArrayList<>();
        }
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID,UserUtil.getSellerId()));
        Iterable<OpLocationElasticDTO> elasticDTOList =
//                opLocationElastic.search(boolQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
        elasticDTOList.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
        return menuDTOList;
    }

    @Override
    public List<OpLocationMenuDTO> getStationMenuBySellerId(String merchantId) {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, Long.valueOf(merchantId)));
        Iterable<OpLocationElasticDTO> elasticDTOList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
        elasticDTOList.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
        return menuDTOList;
    }

    @Override
    public List<OpLocationMenuDTO> getEMSPStationMenu() {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();

        //1、根据当前用户的商家id是哪个emsp平台 0-autel 1-planet
        Long sellerId = LoginUserUtil.getSellerId();
        log.info("===>>OrderController emspQuery matchOrder:{} sellerId:{} ",matchEmspOrder,sellerId);
        Result<Map<Long, String>> userEmspResult = pileUserFeign.getEmsp(sellerId);
        log.info("===>>OrderController emspQuery userEmspResult:{}", JSONObject.toJSONString(userEmspResult));
        //如果是空平，返回空数据
        if (userEmspResult == null || userEmspResult.getData() == null || userEmspResult.getData().size() == 0){
            return Lists.newArrayList();
        }else {
            //获取与该emsp平台合作或者终止合作的cpo商家
            SellerParamDTO sellerParamDTO = new SellerParamDTO();
            sellerParamDTO.setAppId(SellerPlatformEnum.CSMS.getCode());
            sellerParamDTO.setAssociatedPlatform(Integer.valueOf(userEmspResult.getData().get(sellerId)));
            sellerParamDTO.setCollabStatusList(Lists.newArrayList(CollabStatusEnum.COOPERATION.getType(),CollabStatusEnum.STOP_COOPERATION.getType()));
            Result<List<SellerDetailVO>> cpoSellerResult = pileUserFeign.findEmspList(sellerParamDTO);
            log.info("===>>>getEMSPStationMenu sellerParamDTO:{} cpoSellerResult:{}",JSONObject.toJSONString(sellerParamDTO),JSONObject.toJSONString(cpoSellerResult));
            if (cpoSellerResult != null && CollectionUtils.isNotEmpty(cpoSellerResult.getData())){
                List<String> cpoSellerIdList = cpoSellerResult.getData().stream().map(SellerDetailVO::getId).collect(Collectors.toList());
                LambdaQueryWrapper<OpLocationEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationEntity.class)
                        .select(OpLocationEntity::getId, OpLocationEntity::getName, OpLocationEntity::getDeleted)
                        .in(OpLocationEntity::getOperatorId, cpoSellerIdList);
                List<OpLocationEntity> opLocationEntityList = list(lambdaQueryWrapper);
                return DozerConvert.mapList(opLocationEntityList, OpLocationMenuDTO.class);
            }
        }
        return Lists.newArrayList();
    }

    @Override
    public List<OpLocationMenuDTO> getUserStationMenu() {
        List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
        if (UserUtil.getSellerId() == null) {
            return menuDTOList;
        }
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, UserUtil.getSellerId()));

        //当前用户场站id
        try {
            Result<List<Long>> currentUserLocationIdsResult = pileUserServiceFeign.getLocationIds();
            log.info("当前用户的场站id:{}", JSON.toJSONString(currentUserLocationIdsResult));
            if (currentUserLocationIdsResult != null && currentUserLocationIdsResult.getCode() == HttpStatus.OK.value()) {
                List<Long> currentUserLocationIds = currentUserLocationIdsResult.getData();
                if (CollectionUtils.isNotEmpty(currentUserLocationIds)) {
                    boolQueryBuilder.must(QueryBuilders.termsQuery("id", currentUserLocationIds));
                } else {
                    //如果当前用户没有场站id，直接返回
                    return menuDTOList;
                }
            }
        } catch (Exception e) {
            log.info("远程调用当前用户的场站id失败：{}", JSON.toJSONString(e));
        }
        Iterable<OpLocationElasticDTO> elasticDTOList =
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        elasticDTOList.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
        return menuDTOList;
    }

    @Override
    public Set<Long> getAllNotAdvertisingStation(Long sellerId) {
        if (ObjectUtils.isEmpty(sellerId)) {
            sellerId = LoginUserUtil.getSellerId();
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId",sellerId));
        //当前版本暂时注释，后续需要区分广告的话再回复该行代码
        //queryBuilder.must(QueryBuilders.termsQuery("businessType",Arrays.asList(1,2)));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withTrackTotalHits(true)
                .build();
        return elasticsearchRestTemplate.search(searchQuery,OpLocationElasticDTO.class).stream().map(SearchHit::getContent).map(OpLocationElasticDTO::getId).collect(Collectors.toSet());

    }

    @Override
    public Page<LocationBaseVO> getLocationDropDownPage(OpLocationMenuQueryDto pageDTO) {

        log.info("===>>> OpLocationRepositoryImpl.getLocationDropDownPage pageDTO : {}",
                JSON.toJSONString(pageDTO));

        Page<LocationBaseVO> result = new Page<>(pageDTO.getPage(), pageDTO.getPageSize());

        Result<List<Long>> getLocationIdsResult = pileUserServiceFeign.getLocationIds();

        log.info("===>>> OpLocationRepositoryImpl.getLocationDropDownPage getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        if (getLocationIdsResult == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getLocationIdsResult.getData())) {
            return result;
        }

        List<Long> locationIdList = new ArrayList<>(getLocationIdsResult.getData());

        boolean queryBySeller = Boolean.TRUE.equals(pageDTO.getQueryBySeller());

        LambdaQueryWrapper<OpLocationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.in(!queryBySeller, OpLocationEntity::getId, locationIdList)
                .eq(queryBySeller, OpLocationEntity::getOperatorId, LoginUserUtil.getSellerId())
                .eq(OpLocationEntity::getDeleted, 0)
                .like(StringUtils.isNotBlank(pageDTO.getSearchValue()), OpLocationEntity::getName, StringUtil.escapeChar(pageDTO.getSearchValue()));

        Page<OpLocationEntity> opLocationEntityPage = opLocationMapper.selectPage(new Page<>(pageDTO.getPage(), pageDTO.getPageSize()), lqw);
        if (opLocationEntityPage == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEntityPage.getRecords())) {
            return result;
        }

        List<LocationBaseVO> locationBaseVOList = new ArrayList<>();
        opLocationEntityPage.getRecords().forEach(val -> {
            LocationBaseVO locationBaseVO = new LocationBaseVO();
            locationBaseVO.setLocationId(val.getId());
            locationBaseVO.setLocationName(val.getName());
            locationBaseVOList.add(locationBaseVO);
        });

        result.setRecords(locationBaseVOList);
        result.setTotal(opLocationEntityPage.getTotal());
        return result;
    }

    @Override
    public List<OpLocationMenuDTO> getStationMenuIncludedDeleted() {
        // 商家管理员的逻辑 是可以看到所有的场站包括已删除的
        if (LoginUserUtil.isSellerAdmin()) {
            log.info("UserUtil.getUserId={}", UserUtil.getUserId());
            LambdaQueryWrapper<OpLocationEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationEntity.class)
                    .eq(OpLocationEntity::getOperatorId, UserUtil.getSellerId())
                    .select(OpLocationEntity::getId, OpLocationEntity::getName, OpLocationEntity::getDeleted, OpLocationEntity::getCreatedAt);
            List<OpLocationEntity> opLocationEntities = opLocationMapper.selectList(lambdaQuery);
            List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
            opLocationEntities.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
            Collections.sort(menuDTOList);
            log.info("admin locationIds={}", JSON.toJSONString(menuDTOList));
            return menuDTOList;
        } else { // 普通员工就只能看到分配的场站
            Result<List<Long>> locationIds = pileUserServiceFeign.getLocationIds();
            log.info("locationIds={}", JSON.toJSONString(locationIds));
            if (Objects.nonNull(locationIds) && org.apache.commons.collections.CollectionUtils.isNotEmpty(locationIds.getData())) {
                LambdaQueryWrapper<OpLocationEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationEntity.class)
                        .eq(OpLocationEntity::getOperatorId, UserUtil.getSellerId())
                        .in(OpLocationEntity::getId, locationIds.getData())
                        .select(OpLocationEntity::getId, OpLocationEntity::getName, OpLocationEntity::getDeleted, OpLocationEntity::getCreatedAt);
                List<OpLocationEntity> opLocationEntities = opLocationMapper.selectList(lambdaQuery);
                List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
                opLocationEntities.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
                Collections.sort(menuDTOList);
                log.info("user locationIds={}", JSON.toJSONString(menuDTOList));
                return menuDTOList;
            }
        }
        return Collections.emptyList();


    }

    @Override
    public Page<OpLocationMenuDTO> getStationMenuIncludedDeletedWithoutAuthorize(PageDTO pageDTO, Boolean isIncludeDeleted) {
        Page<OpLocationMenuDTO> response = new Page<>(pageDTO.getPage(), pageDTO.getPageSize());
        Page<OpLocationEntity> pageQuery = new Page<>(pageDTO.getPage(), pageDTO.getPageSize());
        LambdaQueryWrapper<OpLocationEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationEntity.class)
                .select(OpLocationEntity::getId, OpLocationEntity::getName, OpLocationEntity::getDeleted, OpLocationEntity::getCreatedAt);
        if (!ObjectUtils.isEmpty(isIncludeDeleted) && Boolean.FALSE.equals(isIncludeDeleted)) {
            lambdaQuery.eq(OpLocationEntity::getDeleted,0);
        }
        pageQuery = opLocationMapper.selectPage(pageQuery, lambdaQuery);
        if (CollUtil.isNotEmpty(pageQuery.getRecords())) {
            List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
            pageQuery.getRecords().forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
            Collections.sort(menuDTOList);
            log.info("admin locationIds={}", JSON.toJSONString(menuDTOList));
            response.setRecords(menuDTOList);
        }
        response.setTotal(pageQuery.getTotal());
        response.setPages(pageQuery.getPages());
        return response;
    }

    @Override
    public List<OpLocationMenuDTO> getStationIncludedDeletedOrderByCreateAt() {
        LambdaQueryWrapper<OpLocationEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationEntity.class)
                .eq(OpLocationEntity::getOperatorId, UserUtil.getSellerId())
                .select(OpLocationEntity::getId, OpLocationEntity::getName, OpLocationEntity::getDeleted, OpLocationEntity::getCreatedAt);
        List<OpLocationEntity> opLocationEntities = opLocationMapper.selectList(lambdaQuery);
        List<OpLocationMenuDTO> menuDTOList = new ArrayList<>();
        opLocationEntities.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationMenuDTO(dto)));
        Collections.sort(menuDTOList);
        return menuDTOList;
    }

    @Override
    public List<OpLocationDTO> getOpLocationBySellerId(QueryOplcationDTO queryOplcationDTO) {
        BoolQueryBuilder stationQueryBuilder = QueryBuilders.boolQuery();
        stationQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, queryOplcationDTO.getOperatorId()));
        if (CollectionUtils.isNotEmpty(queryOplcationDTO.getLocationIds())) {
            BoolQueryBuilder locationBuilder = QueryBuilders.boolQuery();
            queryOplcationDTO.getLocationIds().forEach(locationId -> locationBuilder.should(QueryBuilders.termQuery("id", locationId)));
            stationQueryBuilder.must(locationBuilder);
        }
        Iterable<OpLocationElasticDTO> OpLocationIterator =
//                opLocationElastic.search(stationQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(stationQueryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationDTO> menuDTOList = new ArrayList<>();
        OpLocationIterator.forEach(dto -> menuDTOList.add(OpLocationConvert.toOpLocationDTO(dto)));
        return menuDTOList;
    }

    @Override
    public List<OpLocationDTO> getStationMapList() {
        Iterable<OpLocationElasticDTO> elasticDTOList = opLocationElastic.findAll();
        List<OpLocationDTO> menuDTOList = new ArrayList<>();
        elasticDTOList.forEach(dto -> menuDTOList.add(this.details(dto.getId())));
        return menuDTOList;
    }

    @Override
    public OpLocationOperationInfoDTO getLocationOperationInfo(Long locationId) {
        String operationDate = DateUtil.getNowDay();
        Map<String, String> map = opLocationRedis.getLocationChargeInfo(operationDate, locationId);
        OpLocationOperationInfoDTO opLocationOperationInfoDTO = JSON
                .parseObject(JSON.toJSONString(map), OpLocationOperationInfoDTO.class);
        opLocationOperationInfoDTO.setOperationDate(operationDate);
        return opLocationOperationInfoDTO;
    }

    @Override
    public List<OpLocationOperationInfoDTO> getLocationOperationInfoListForDay(Integer dayNum, Long locationId) {
        log.info("OpLocationRepositoryImpl getLocationOperationInfoListForDay start dayNum = "
                + dayNum + " and locationId = " + locationId);
        List<OpLocationOperationInfoDTO> locationOperationInfoDTOS = new ArrayList<>();
        for (int i = dayNum - 1; i >= 0; i--) {
            String operationDate = DateUtil.getNowDayBehind(i);
            log.info("OpLocationRepositoryImpl getLocationOperationInfoListForDay operationDate = "
                    + operationDate + " and locationId = " + locationId);
            Map<String, String> map = opLocationRedis.getLocationChargeInfo(operationDate, locationId);
            OpLocationOperationInfoDTO opLocationOperationInfoDTO = new OpLocationOperationInfoDTO();
            opLocationOperationInfoDTO.setChargeIncome(Double.parseDouble(Optional.ofNullable(map.get("chargeIncome")).orElse("0")));
            opLocationOperationInfoDTO.setChargeCapacity(Double.parseDouble(Optional.ofNullable(map.get("chargeCapacity")).orElse("0")));
            opLocationOperationInfoDTO.setChargeTimes(Integer.parseInt(Optional.ofNullable(map.get("chargeTimes")).orElse("0")));
            opLocationOperationInfoDTO.setUpdateTime(DateUtil.parseFormatToDate(Optional.ofNullable(map.get("updateTime")).orElse(DateUtil.getNowDayTime()),
                    DateUtil.DEFAULT_PATTERN));
            opLocationOperationInfoDTO.setChargeOrder(Optional.ofNullable(map.get("chargeOrder")).orElse(""));
            opLocationOperationInfoDTO.setOperationDate(operationDate);
            locationOperationInfoDTOS.add(opLocationOperationInfoDTO);
        }
        return locationOperationInfoDTOS;
    }

    @Override
    public Page<OpLocationDTO> pages(OpLocationQueryDTO opLocationQueryDTO, List<Long> locationId) {

        List<Long> locationId1 = null;

        if (opLocationQueryDTO.getOperationType() != null &&
                !opLocationQueryDTO.getOperationType().equals("") && locationId == null) {
            locationId1 = opLocationOperationMapper.selectLocationIdsByOperationType(opLocationQueryDTO.getOperationType());
        }
        if (locationId != null) {
            LambdaQueryWrapper<OpLocationOperationEntity> query = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                    .eq(OpLocationOperationEntity::getDeleted, 0)
                    .eq(OpLocationOperationEntity::getLocationId, locationId.get(0));
            OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(query);

            OpLocationDTO opLocationDTO = new OpLocationDTO();
            if (opLocationOperationEntity != null) {
                opLocationDTO = OpLocationOperationConvert.opLocationDTO(opLocationOperationEntity);
            }

            OpLocationDTO opLocationDTO1 = copyLocationEntities(locationId.get(0), opLocationDTO);
            List<OpLocationDTO> opLocationDTOS = new ArrayList<>();
            opLocationDTOS.add(opLocationDTO1);
            Page<OpLocationDTO> opLocationDTOPage = new Page<>(1, 1, 1);
            if (opLocationQueryDTO.getOperationType() == null || (opLocationDTO1.getOperationType() != null &&
                    opLocationDTO1.getOperationType().equals(opLocationQueryDTO.getOperationType()))) {
                opLocationDTOPage.setRecords(opLocationDTOS);
            }
            return opLocationDTOPage;
        }
        Page<OpLocationOperationEntity> page = new Page<>(opLocationQueryDTO.getStart(), opLocationQueryDTO.getPageSize());

        LambdaQueryWrapper<OpLocationOperationEntity> query = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                .eq(OpLocationOperationEntity::getDeleted, 0);
        if (locationId1 != null) {
            query.eq(OpLocationOperationEntity::getOperationType, opLocationQueryDTO.getOperationType());
        }
        Page<OpLocationOperationEntity> opLocationOperationEntityPage = opLocationOperationMapper.selectPage(page, query);

        List<OpLocationDTO> locationDTOS = new LinkedList<>();

        if (opLocationOperationEntityPage.getRecords() != null) {
            List<Long> locationIds = opLocationOperationEntityPage.getRecords().stream().map(OpLocationOperationEntity::getLocationId).collect(Collectors.toList());
            List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationIdList(locationIds);
            Map<Long, List<OpLocationFacilityDTO>> facilityListMap = locationFacilityDTOList.stream().collect(Collectors.groupingBy(OpLocationFacilityDTO::getLocationId));
            for (OpLocationOperationEntity opLocationOperationEntity : opLocationOperationEntityPage.getRecords()) {
                Long locationId2 = opLocationOperationEntity.getLocationId();
                OpLocationDTO opLocationDTO = OpLocationOperationConvert.opLocationDTO(opLocationOperationEntity);
                OpLocationDTO opLocationDTO1 = copyLocationEntities(locationId2, opLocationDTO);
                if (opLocationDTO1.getLocationId() != null) {
                    if (null != facilityListMap && CollUtil.isNotEmpty(facilityListMap.get(opLocationDTO1.getLocationId()))) {
                        opLocationDTO1.setFacilityList(facilityListMap.get(opLocationDTO1.getLocationId()));
                    }
                    locationDTOS.add(opLocationDTO1);
                }
            }
        }
        Page<OpLocationDTO> opLocationDTOPage = new Page<>(opLocationOperationEntityPage.getCurrent(), opLocationOperationEntityPage.getSize(), locationDTOS.size());
        if (locationDTOS.isEmpty()) {
            return new Page<>();
        }

        opLocationDTOPage.setRecords(locationDTOS);
        return opLocationDTOPage;
    }

    @Override
    public List<OpLocationEntity> selectByName(LambdaQueryWrapper<OpLocationEntity> query) {
        return this.getBaseMapper().selectList(query);
    }

    @Override
    public OpLocationEntity selectOpLocationEntityById(Long id) {
        return getById(id);
    }

    /**
     * 5月新版场站分页
     *
     * @param opLocationPageDTO 检索对象
     * @return 5月新版场站分页
     */
    @Override
    public Page<OpLocationPageVO> stationPage(OpLocationPageDTO opLocationPageDTO) {
        //构建mybatis分页返回对象
        Page<OpLocationPageVO> resultPage = new Page<>(opLocationPageDTO.getPage(), opLocationPageDTO.getPageSize());

        //搜索值(组织机构名称 + 场站名称 + 桩名称)
        Set<Long> stationIds = null;
        if (StringUtils.isNotBlank(opLocationPageDTO.getStationNameOrPileSN())) {
            //桩名称，查出桩所在站点id
            BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
            WildcardQueryBuilder nameQueryBuilder = QueryBuilders.wildcardQuery(BaseConstant.PILESN, "*" + opLocationPageDTO.getStationNameOrPileSN() + "*");
            boolQueryBuilder.must(nameQueryBuilder);
//            Iterable<OpLocationPileEvseElasticDTO> pileElasticDTOIterable = opLocationPileEvseElastic.search(boolQueryBuilder);
            List<OpLocationPileEvseElasticDTO> pileList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());
            stationIds = pileList.stream().map(OpLocationPileEvseElasticDTO::getLocationId).collect(Collectors.toSet());
        }
        //es查询场站
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        //当前商户id
        Long sellerId = UserUtil.getSellerId();
        log.info("当前用户的商户id:{}", sellerId);
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId));
        //组织机构id-点击组织机构树
        if (opLocationPageDTO.getOrgId() != null) {
            try {
                Result<List<Long>> orgIdsResult = pileUserServiceFeign.getOrgChildrenList(opLocationPageDTO.getOrgId());
                if (orgIdsResult != null && orgIdsResult.getCode() == HttpStatus.OK.value()) {
                    List<Long> orgIds = orgIdsResult.getData();
                    if (CollectionUtils.isEmpty(orgIds)) {
                        orgIds = Lists.newArrayList();
                    }
                    orgIds.add(opLocationPageDTO.getOrgId());
                    boolQueryBuilder.must(QueryBuilders.termsQuery(BaseConstant.GROUP_ID, orgIds));
                }
            } catch (Exception e) {
                log.info("远程调用组织机构id及其各子机构失败：{}", JSON.toJSONString(e));
            }
        }
        //当前用户场站id
        try {
            Result<List<Long>> currentUserLocationIdsResult = pileUserServiceFeign.getLocationIds();
            if (currentUserLocationIdsResult != null && currentUserLocationIdsResult.getCode() == HttpStatus.OK.value()) {
                List<Long> currentUserLocationIds = currentUserLocationIdsResult.getData();
                if (CollectionUtils.isNotEmpty(currentUserLocationIds)) {
                    boolQueryBuilder.must(QueryBuilders.termsQuery("id", currentUserLocationIds));
                } else {
                    //如果当前用户没有场站id，直接返回
                    resultPage.setTotal(0);
                    resultPage.setRecords(Lists.newArrayList());
                    return resultPage;
                }
            }
        } catch (Exception e) {
            log.info("远程调用当前用户的场站id失败：{}", JSON.toJSONString(e));
        }
        //场站名称和站点id的组合
        if (StringUtils.isNotBlank(opLocationPageDTO.getStationNameOrPileSN())) {
            BoolQueryBuilder boolQueryBuilder2 = QueryBuilders.boolQuery();
            //场站名称
            boolQueryBuilder2.should(QueryBuilders.wildcardQuery("name", "*" + opLocationPageDTO.getStationNameOrPileSN() + "*"));
            //组织机构名称
            boolQueryBuilder2.should(QueryBuilders.wildcardQuery("groupName", "*" + opLocationPageDTO.getStationNameOrPileSN() + "*"));
            //场站id
            if (CollectionUtils.isNotEmpty(stationIds)) {
                boolQueryBuilder2.should(QueryBuilders.termsQuery("id", stationIds));
            }
            boolQueryBuilder.must(boolQueryBuilder2);
        }
//        Iterable<OpLocationElasticDTO> opLocationElasticDTOIterable = opLocationElastic.search(boolQueryBuilder);
        List<OpLocationElasticDTO> opLocationElasticDTOS = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("ES场站：{}", JSON.toJSONString(opLocationElasticDTOS));
        List<OpLocationPageVO> opLocationPageVOList = DozerConvert.mapList(opLocationElasticDTOS, OpLocationPageVO.class);
        try {
            //排序
            Result<List<UserOrgMenuVO>> userOrgTreeListResult = pileUserFeign.menuList();
            if (userOrgTreeListResult != null && CollectionUtils.isNotEmpty(userOrgTreeListResult.getData())) {
                Map<Long, Integer> orgIdNOMap = buildOrgIdNOMap(userOrgTreeListResult.getData());
                opLocationPageVOList.forEach(opLocationPageVO -> opLocationPageVO.setOrgNo(orgIdNOMap.get(opLocationPageVO.getGroupId())));
                Collections.sort(opLocationPageVOList);
            }
        } catch (Exception e) {
            log.info("stationPage：", e);
        }
        if (CollectionUtils.isNotEmpty(opLocationPageVOList)) {
            Set<Long> locationIds = opLocationPageVOList.stream().map(OpLocationPageVO::getId).collect(Collectors.toSet());
            //开启线程feign查询当月站点收入和新增用户数
            Set<Long> stationIdSet = opLocationPageVOList.stream().map(OpLocationPageVO::getId).filter(Objects::nonNull).collect(Collectors.toSet());
            opLocationMultiForStatisticService.setStationIds(Lists.newArrayList(stationIdSet));
            FutureTask<Map<Long, MonthReportVO>> statisticFutureTask = new FutureTask<>(opLocationMultiForStatisticService);
            new Thread(statisticFutureTask).start();
            //查询场站图片
            Set<Long> imageIds = new HashSet<>();
            Map<Long, Set<Long>> stationIdImageIdSetMap = new HashMap<>();
            QueryWrapper<OpLocationImageEntity> opLocationImageEntityQueryWrapper = new QueryWrapper<>();
            opLocationImageEntityQueryWrapper.in(BaseConstant.LOCATION_ID, locationIds);
            opLocationImageEntityQueryWrapper.eq(BaseConstant.DELETED, 0);
            List<OpLocationImageEntity> opLocationImageEntities = opLocationImageMapper.selectList(opLocationImageEntityQueryWrapper);
            if (CollectionUtils.isNotEmpty(opLocationImageEntities)) {
                for (OpLocationImageEntity opLocationImageEntity : opLocationImageEntities) {
                    if (opLocationImageEntity.getImageId() != null) {
                        imageIds.add(opLocationImageEntity.getImageId());
                        Set<Long> stationImageIds = stationIdImageIdSetMap.get(opLocationImageEntity.getLocationId());
                        if (stationImageIds == null) {
                            stationImageIds = new HashSet<>();
                        }
                        stationImageIds.add(opLocationImageEntity.getImageId());
                        stationIdImageIdSetMap.put(opLocationImageEntity.getLocationId(), stationImageIds);
                    }
                }
            }
            //查询图片实体类并封装成map
            Map<Long, OpImageEntity> imageIdImageEntityMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(imageIds)) {
                List<OpImageEntity> opImageEntities = opImageMapper.selectBatchIds(imageIds);
                if (CollectionUtils.isNotEmpty(opImageEntities)) {
                    for (OpImageEntity opImageEntity : opImageEntities) {
                        imageIdImageEntityMap.put(opImageEntity.getId(), opImageEntity);
                    }
                }
            }
            //查询ES设备
            List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationIdIn(locationIds);
            Map<Long, List<OpLocationEvseElasticDTO>> stationIdESEVSEListMap = buildStationIdESEVSEListMap(esEVSEList);
            Map<Long, OpLocationEvseElasticDTO> evseIdESDTOMap = buildEvseIdESDTOMap(esEVSEList);
            //查询ES桩
            BoolQueryBuilder pileQueryBuilder = QueryBuilders.boolQuery();
            pileQueryBuilder.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIds));
            if (StringUtils.isNotBlank(opLocationPageDTO.getStationNameOrPileSN())) {
                pileQueryBuilder.must(QueryBuilders.wildcardQuery(BaseConstant.PILESN, "*" + opLocationPageDTO.getStationNameOrPileSN() + "*"));
            }
//            Iterable<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOIterable = opLocationPileEvseElastic.search(pileQueryBuilder);
            List<OpLocationPileEvseElasticDTO> esPileList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(pileQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());
            Map<Long, List<OpLocationPileEvseElasticDTO>> stationIdESPileListMap = buildStationIdESPileListMap(esPileList);
            //查询品牌map
            Map<Long, OpEvseBrandEntity> brandIdBrandEntityMap = buildBrandIdBrandEntityMap(esPileList);
            //构建站点桩数量map
            Map<Long, Integer> stationIdPileNumMap = buildStationIdESPileNumMap(opLocationPageDTO.getStationNameOrPileSN(), locationIds, esPileList);
            //获取线程当月站点收入和新增用户数
            Map<Long, MonthReportVO> statisticsMap = new HashMap<>();
            try {
                statisticsMap = statisticFutureTask.get();
            } catch (Exception e) {
                log.info("stationPage：", e);
            } finally {
                statisticFutureTask.cancel(true);
            }

            for (OpLocationPageVO stationPageVo : opLocationPageVOList) {
                //站点桩
                List<OpLocationPileEvseElasticDTO> stationPileList = stationIdESPileListMap.get(stationPageVo.getId());
                //站点es设备
                List<OpLocationEvseElasticDTO> opLocationESEVSEList = stationIdESEVSEListMap.get(stationPageVo.getId());
                //是否运营
                Long operationDate = stationPageVo.getOperationDate();
                if (operationDate != null) {
                    long now = new Date().getTime();
                    if (now < operationDate) {
                        stationPageVo.setOperate(false);
                    }
                }
                //枪总数量
                int evseNum = 0;
                if (CollectionUtils.isNotEmpty(opLocationESEVSEList)) {
                    evseNum = opLocationESEVSEList.size();
                }
                stationPageVo.setEvseNum(evseNum);
                //桩总数量
                int pileNum = 0;
                Integer integer = stationIdPileNumMap.get(stationPageVo.getId());
                if (integer != null) {
                    pileNum = integer;
                }
                stationPageVo.setPileNum(pileNum);
                //不同功率枪数统计
                List<PowerGroupVO> powerGroupVOList = buildStationPowerGroupVOList(opLocationESEVSEList);
                stationPageVo.setPowerGroupVOS(powerGroupVOList);
                //不同类型枪数统计
                List<GunTypeGroupVO> gunTypeGroupVOS = buildStationGunTypeGroupVOList2(opLocationESEVSEList);
                stationPageVo.setGunTypeGroupVOS(gunTypeGroupVOS);
                //不同枪状态统计
                List<GunStatusGroupVO> gunStatusGroupVOS = buildStationGunStatusGroupVO2(opLocationESEVSEList);
                stationPageVo.setGunStatusGroupVOS(gunStatusGroupVOS);
                //桩集合
                List<PilePageVO> pilePageVOList = buildPilePageVOList(stationPileList, evseIdESDTOMap, brandIdBrandEntityMap);
                stationPageVo.setPileList(pilePageVOList);
                //场站图片
                Set<Long> imageIdSet = stationIdImageIdSetMap.get(stationPageVo.getId());
                if (CollectionUtils.isNotEmpty(imageIdSet)) {
                    List<OpLocationDetailImageVO> imageList = Lists.newArrayList();
                    imageIdSet.forEach(imageId -> {
                        OpImageEntity opImageEntity = imageIdImageEntityMap.get(imageId);
                        if (opImageEntity != null) {
                            imageList.add(DozerConvert.map(opImageEntity, OpLocationDetailImageVO.class));
                        }
                    });
                    Collections.sort(imageList);
                    stationPageVo.setImageList(imageList);
                }
                //站点当月收入和新增充电用户数
                MonthReportVO monthReportVO = statisticsMap.get(stationPageVo.getId());
                if (monthReportVO != null) {
                    stationPageVo.setIncome(monthReportVO.getIncome());
                    stationPageVo.setCountCustomer(monthReportVO.getCountCustomer());
                } else {
                    stationPageVo.setIncome(new BigDecimal(0));
                    stationPageVo.setCountCustomer((long) 0);
                }
            }
        }
        resultPage.setRecords(opLocationPageVOList);
        resultPage.setTotal(opLocationPageVOList.size());
        return resultPage;
    }

    private Map<Long, OpLocationEvseElasticDTO> buildEvseIdESDTOMap(List<OpLocationEvseElasticDTO> esEVSEList) {
        Map<Long, OpLocationEvseElasticDTO> evseIdESDTOMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            for (OpLocationEvseElasticDTO evseElasticDTO : esEVSEList) {
                evseIdESDTOMap.put(evseElasticDTO.getId(), evseElasticDTO);
            }
        }
        return evseIdESDTOMap;
    }

    private List<PilePageVO> buildPilePageVOList(List<OpLocationPileEvseElasticDTO> stationPileList, Map<Long, OpLocationEvseElasticDTO> evseIdESDTOMap, Map<Long, OpEvseBrandEntity> brandIdBrandEntityMap) {
        if (CollectionUtils.isEmpty(stationPileList)) {
            return Lists.newArrayList();
        }
        List<PilePageVO> pilePageVOList = DozerConvert.mapList(stationPileList, PilePageVO.class);


        //封装桩其他信息
        for (PilePageVO pilePageVO : pilePageVOList) {
            //品牌
            if (StringUtils.isBlank(pilePageVO.getBrandName())) {
                if (pilePageVO.getBrandId() != null) {
                    OpEvseBrandEntity opEvseBrandEntity = brandIdBrandEntityMap.get(Long.valueOf(pilePageVO.getBrandId()));
                    if (opEvseBrandEntity != null) {
                        pilePageVO.setBrandName(opEvseBrandEntity.getName());
                    }
                }
            }
            //枪集合
            String evseList = pilePageVO.getEvseList();
            log.info("桩：{} 的枪集合:{}", pilePageVO.getId(), evseList);
            if (StringUtils.isNotBlank(evseList)) {
                List<GunStatusVO> gunStatusVOList = Lists.newArrayList();
                JSONArray evseArray = JSON.parseArray(evseList);
                for (Object evseId : evseArray) {
                    GunStatusVO gunStatusVO = new GunStatusVO();
                    gunStatusVO.setId(Long.valueOf(evseId.toString()));
                    OpLocationEvseElasticDTO esEVSE = evseIdESDTOMap.get(Long.valueOf(evseId.toString()));
                    if (esEVSE != null) {
                        String state = esEVSE.getState();
                        state = StringUtils.isBlank(state) ? String.valueOf(EvseDeviceStatusEnum.DEFAULT.getName()) : state;
                        LocationEVSEStatusEnum locationEVSEStatusEnum = LocationEVSEStatusEnum.realTimeStatus2BusinessStatus(EvseDeviceStatusEnum.getEnumByName(state).getName());
                        gunStatusVO.setEvseStatusCode(locationEVSEStatusEnum.getCode());
                        if (esEVSE.getGunType() != null) {
                            gunStatusVO.setGunTypeCode(esEVSE.getGunType());
                        }
                        gunStatusVO.setTariffId(esEVSE.getTariffId());
                        gunStatusVO.setEvseSn(esEVSE.getEvseSn());
                        if (StringUtils.isNotBlank(esEVSE.getEvseSn()) && esEVSE.getEvseSn().contains("_")) {
                            gunStatusVO.setEvseNo(esEVSE.getEvseSn().split("_")[1]);
                        }
                    } else {
                        gunStatusVO.setEvseStatusCode(LocationEVSEStatusEnum.UNAVAILABLE.getCode());
                    }
                    gunStatusVOList.add(gunStatusVO);
                }
                pilePageVO.setGunStatusVOS(gunStatusVOList);
            }
        }
        return pilePageVOList;
    }

    /**
     * 5月新版场站详情查询
     *
     * @param id 场站id
     * @return 场站详情
     */
    @Override
    public OpLocationDetailVO detail(Long id) {
        OpLocationDetailVO opLocationDetailVO = new OpLocationDetailVO();

        //查询场站
        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(id);
        //查询场站运营表
        OpLocationOperationEntity opLocationOperationEntity = null;
        QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
        opLocationOperationEntityQueryWrapper.eq(BaseConstant.LOCATION_ID, id);
        opLocationOperationEntityQueryWrapper.eq(BaseConstant.DELETED, 0);
        List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(opLocationOperationEntityQueryWrapper);
        if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
            opLocationOperationEntity = opLocationOperationEntities.get(0);
        }
        //场站logo和场站图片
        Set<Long> imageIds = new HashSet<>();
        Map<Long, OpImageEntity> imageIdImageEntityMap = null;
        //场站图片
        QueryWrapper<OpLocationImageEntity> opLocationImageEntityQueryWrapper = new QueryWrapper<>();
        opLocationImageEntityQueryWrapper.eq(BaseConstant.LOCATION_ID, id);
        opLocationImageEntityQueryWrapper.eq(BaseConstant.DELETED, 0);
        List<OpLocationImageEntity> opLocationImageEntities = opLocationImageMapper.selectList(opLocationImageEntityQueryWrapper);
        if (CollectionUtils.isNotEmpty(opLocationImageEntities)) {
            imageIds = opLocationImageEntities.stream().map(OpLocationImageEntity::getImageId).collect(Collectors.toSet());
        }
        //场站logo
        if (opLocationOperationEntity != null && opLocationOperationEntity.getLogoImageId() != null) {
            imageIds.add(opLocationOperationEntity.getLogoImageId());
        }
        if (CollectionUtils.isNotEmpty(imageIds)) {
            List<OpImageEntity> opImageEntities = opImageMapper.selectBatchIds(imageIds);
            if (CollectionUtils.isNotEmpty(opImageEntities)) {
                imageIdImageEntityMap = new HashMap<>();
                for (OpImageEntity opImageEntity : opImageEntities) {
                    imageIdImageEntityMap.put(opImageEntity.getId(), opImageEntity);
                }
            }
        }
        //查询es设备信息
        List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(id);
        log.info("站点实时设备信息：{}", esEVSEList);
        //装载基本信息
        OpLocationElasticDTO opLocationElasticDTO = null;
        if (optionalOpLocationElasticDTO.isPresent()) {
            opLocationElasticDTO = optionalOpLocationElasticDTO.get();
            opLocationDetailVO = DozerConvert.map(opLocationElasticDTO, OpLocationDetailVO.class);
            String taxConfiguration = opLocationElasticDTO.getTaxConfiguration();
            if (StringUtils.isNotBlank(taxConfiguration)) {
                opLocationDetailVO.setTaxDTO(JSON.parseObject(taxConfiguration, TaxDTO.class));
            }
            opLocationDetailVO.setUserSellerId(opLocationElasticDTO.getOperatorId());
            //国家
            String country = changeCountry(opLocationDetailVO.getCountry());
            opLocationDetailVO.setCountryName(country);
            //价格公告
            opLocationDetailVO.setPriceAnnouncement(opLocationElasticDTO.getBillingRule());
            //装载开业日期，开业状态
            Long operationDate = opLocationElasticDTO.getOperationDate();
            log.info("开业日期：{}", operationDate);
            long now = new Date().getTime();
            if (operationDate == null) {
                opLocationDetailVO.setOperateStatus(LocationOperateStatusEnum.OPERATING.value());
            } else {
                if (now < operationDate) {
                    opLocationDetailVO.setOperateStatus(LocationOperateStatusEnum.UN_OPERATE.value());
                } else {
                    opLocationDetailVO.setOperateStatus(LocationOperateStatusEnum.OPERATING.value());
                }
            }
        }
        //装载运营表
        if (opLocationOperationEntity != null) {
            log.info("组织机构运营表信息：{}", JSON.toJSONString(opLocationOperationEntity));
            //转载logo
            if (opLocationOperationEntity.getLogoImageId() != null && imageIdImageEntityMap != null) {
                OpImageEntity opImageEntity = imageIdImageEntityMap.get(opLocationOperationEntity.getLogoImageId());
                if (opImageEntity != null) {
                    opLocationDetailVO.setLogoImageDTO(DozerConvert.map(opImageEntity, OpLocationDetailImageVO.class));
                }
            }
        }
        //装载场站图片
        if (imageIdImageEntityMap != null && CollectionUtils.isNotEmpty(opLocationImageEntities)) {
            List<OpLocationDetailImageVO> opLocationDetailImageVOS = new ArrayList<>();
            for (OpLocationImageEntity opLocationImageEntity : opLocationImageEntities) {
                OpImageEntity opImageEntity = imageIdImageEntityMap.get(opLocationImageEntity.getImageId());
                if (opImageEntity != null) {
                    opLocationDetailImageVOS.add(DozerConvert.map(opImageEntity, OpLocationDetailImageVO.class));
                }
            }
            Collections.sort(opLocationDetailImageVOS);
            opLocationDetailVO.setImageList(opLocationDetailImageVOS);
        }

        // 分机号
        if(!StringUtils.isEmpty(opLocationOperationEntity.getServiceTel())){
            String[] split = opLocationOperationEntity.getServiceTel().split("-");
            if(split.length >= 2){
                opLocationDetailVO.setExtensionNumber(split[1]);
            }
        }
        //场站营业时间
//        List<OpLocationOpenTimeVO> stationOpenTimeList = stationOpenTimeList(id);
//        opLocationDetailVO.setOpLocationOpenTimeList(stationOpenTimeList);
        //不同功率枪数统计
        List<PowerGroupVO> powerGroupVOList = buildStationPowerGroupVOList(esEVSEList);
        opLocationDetailVO.setPowerGroupVOS(powerGroupVOList);
        //不同类型枪数统计
        List<GunTypeGroupVO> gunTypeGroupVOS = buildStationGunTypeGroupVOList2(esEVSEList);
        opLocationDetailVO.setGunTypeGroupVOS(gunTypeGroupVOS);
        //不同枪状态统计
        List<GunStatusGroupVO> gunStatusGroupVOS = buildStationGunStatusGroupVO2(esEVSEList);
        opLocationDetailVO.setGunStatusGroupVOS(gunStatusGroupVOS);
        //不同功率和枪类型双维度统计
        List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = buildGunPowerAndTypeGroupVOList(esEVSEList);
        opLocationDetailVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOList);
        //支付方式
        List<PaymentVO> paymentList = stationPaymentList();
        opLocationDetailVO.setPaymentList(paymentList);
        try {
            //场站所在时区的时间
            String timeZone = StringUtils.isBlank(opLocationElasticDTO.getZoneId()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getZoneId();
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
            opLocationDetailVO.setWeekDay(weekDay);
            opLocationDetailVO.setHour(hour);
            opLocationDetailVO.setMinute(minute);
            String nowTimeStr = hour + ":" + minute;
            DateFormat dateFormat = new SimpleDateFormat("HH:mm");
            Date nowTime = dateFormat.parse(nowTimeStr);
            log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
            log.info(BaseConstant.NOW_TIME, nowTime);
            //进场信息查询
            try {
                RuleRelateForAppDTO ruleRelateForAppDTO = new RuleRelateForAppDTO();
                ruleRelateForAppDTO.setUserId(UserUtil.getUserId());
                ruleRelateForAppDTO.setSellerId(opLocationElasticDTO.getOperatorId());
                ruleRelateForAppDTO.setLocationId(opLocationElasticDTO.getId());
                ruleRelateForAppDTO.setZoneId(opLocationElasticDTO.getZoneId());
                Result<RuleRelateForAppVO> ruleForAppResult = ruleRepository.getRuleForApp(ruleRelateForAppDTO);
                log.info("进场信息：{}", JSON.toJSONString(ruleForAppResult));
                if (ruleForAppResult != null && ruleForAppResult.getData() != null) {
                    RuleRelateForAppVO ruleRelateForAppVO = ruleForAppResult.getData();
                    opLocationDetailVO.setOpen(ruleRelateForAppVO.getIsOpen());
                    opLocationDetailVO.setNoAdmittance(ruleRelateForAppVO.getIsLimit());
                    opLocationDetailVO.setChangingTime(ruleRelateForAppVO.getChangingTime());
                    opLocationDetailVO.setChangingWeekDay(ruleRelateForAppVO.getDay());
                }
            } catch (Exception e) {
                log.info("进场信息查询失败：" , e);
            }
        } catch (ParseException e) {
            log.info("opLocationDetailVO：", e);
        }
        List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationId(id);
        log.info(BaseConstant.LOCATION_FACILITY_DTOLIST, JSON.toJSONString(locationFacilityDTOList));
        if (CollUtil.isEmpty(locationFacilityDTOList)) {
            locationFacilityDTOList = Lists.newArrayList();
        }
        opLocationDetailVO.setFacilityList(locationFacilityDTOList);
        opLocationDetailVO.setConditionsValue(null);
        return opLocationDetailVO;
    }

    /**
     * APP地图搜索场站
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 场站数据
     */
    @Override
    public List<OpLocationMapQueryVO> mapQuery(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        StopWatch stopWatch = new StopWatch();
        Long userId = LoginUserHolder.getLoginUser().getId();

        List<OpLocationMapQueryVO> hubjectEvseList = new ArrayList<>();

        stopWatch.start("hubject查询");
        hubjectMapQuery(opLocationMapQueryDTO, hubjectEvseList);
        stopWatch.stop();

        //功率条件查ES桩
        stopWatch.start("功率条件查ES桩");
        Set<Long> powerMatchStationIds = null;
        if (opLocationMapQueryDTO.getHighPower() != null || opLocationMapQueryDTO.getLowPower() != null) {
            Object esPileObject = redisTemplate.opsForValue().get(RedisKeyConstant.getEsPileKey(userId.toString()));
            if (Objects.nonNull(esPileObject)) {
                powerMatchStationIds = Sets.newHashSet(JSONArray.parseArray(esPileObject.toString(), Long.class));
            } else {
                BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
                RangeQueryBuilder rangeQueryBuilder = QueryBuilders.rangeQuery("power");
                if (opLocationMapQueryDTO.getHighPower() != null) {
                    rangeQueryBuilder.lte(opLocationMapQueryDTO.getHighPower());
                }
                if (opLocationMapQueryDTO.getLowPower() != null) {
                    rangeQueryBuilder.gte(opLocationMapQueryDTO.getLowPower());
                }
                boolQueryBuilder.must(rangeQueryBuilder);
//                Iterable<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOIterable = opLocationPileEvseElastic.search(boolQueryBuilder);
                ArrayList<OpLocationPileEvseElasticDTO> esPileList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toCollection(Lists::newArrayList));
                powerMatchStationIds = esPileList.stream().map(OpLocationPileEvseElasticDTO::getLocationId).collect(Collectors.toSet());
                redisTemplate.opsForValue().set(RedisKeyConstant.getEsPileKey(userId.toString()), JSONArray.toJSONString(powerMatchStationIds), 1, TimeUnit.MINUTES);
                log.info("功率满足条件的站点id集合:{}", JSON.toJSONString(powerMatchStationIds));
                if (CollectionUtils.isEmpty(esPileList)) {
                    // 当从ES查询为空，返回hubject设备
                    return hubjectEvseList;
                }
            }

        }
        stopWatch.stop();

        //是否可用 枪类型查询ES枪
        stopWatch.start("是否可用 枪类型查询ES枪");
        Set<Long> gunMatchStationId = null;
        if (opLocationMapQueryDTO.getStatus() != null || CollectionUtils.isNotEmpty(opLocationMapQueryDTO.getGunTypes())) {
            BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
            if (opLocationMapQueryDTO.getStatus() != null && opLocationMapQueryDTO.getStatus() == 1) {
                boolQueryBuilder.must(QueryBuilders.termQuery("state", EvseDeviceStatusEnum.AVAILABLE.getName()));
            }
            if (CollectionUtils.isNotEmpty(opLocationMapQueryDTO.getGunTypes())) {
                boolQueryBuilder.must(QueryBuilders.termsQuery("gunType", opLocationMapQueryDTO.getGunTypes()));
            }
//            Iterable<OpLocationEvseElasticDTO> opLocationEvseElasticDTOIterable = opLocationEvseElastic.search(boolQueryBuilder);
            ArrayList<OpLocationEvseElasticDTO> esEVSEList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toCollection(Lists::newArrayList));
            if (CollectionUtils.isEmpty(esEVSEList)) {
                // 当从ES查询为空，返回hubject设备
                return hubjectEvseList;
            }
            gunMatchStationId = esEVSEList.stream().map(OpLocationEvseElasticDTO::getLocationId).collect(Collectors.toSet());
            log.info("是否可用，枪类型满足条件的站点id:{}集合", JSONArray.toJSONString(gunMatchStationId));
        }
        stopWatch.stop();

        //ES设备
        stopWatch.start("ES设备查询");
        List<OpLocationEvseElasticDTO> esEVSEList;
        Object esEVSEObject = redisTemplate.opsForValue().get(RedisKeyConstant.getStringEsEvse());
        if (esEVSEObject != null) {
            esEVSEList = JSON.parseArray(esEVSEObject.toString(), OpLocationEvseElasticDTO.class);
        } else {
            Iterable<OpLocationEvseElasticDTO> opLocationEvseElasticDTOIterable = opLocationEvseElastic.findAll();
            esEVSEList = Lists.newArrayList(opLocationEvseElasticDTOIterable);
            redisTemplate.opsForValue().set(RedisKeyConstant.getStringEsEvse(), JSON.toJSONString(esEVSEList), 1, TimeUnit.MINUTES);
        }
        List<Long> haveEVSEStationIds = esEVSEList.stream().map(OpLocationEvseElasticDTO::getLocationId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(haveEVSEStationIds)) {
            // 当从ES查询为空，返回hubject设备
            return hubjectEvseList;
        }
        stopWatch.stop();


        //查询场站
        stopWatch.start("查询场站");
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termsQuery("appShow", true));
        if (CollectionUtils.isNotEmpty(powerMatchStationIds)) {
            boolQueryBuilder.must(QueryBuilders.termsQuery("id", powerMatchStationIds));
        }
        if (CollectionUtils.isNotEmpty(gunMatchStationId)) {
            boolQueryBuilder.must(QueryBuilders.termsQuery("id", gunMatchStationId));
        }
        if (CollectionUtils.isNotEmpty(haveEVSEStationIds)) {
            boolQueryBuilder.must(QueryBuilders.termsQuery("id", haveEVSEStationIds));
        }
        if (opLocationMapQueryDTO.getLongitude() != null && opLocationMapQueryDTO.getLatitude() != null) {
            GeoDistanceQueryBuilder geoDistanceQueryBuilder = new GeoDistanceQueryBuilder("location");
            //中心点的构建
            geoDistanceQueryBuilder.point(opLocationMapQueryDTO.getLatitude(), opLocationMapQueryDTO.getLongitude());
            //范围设定
            geoDistanceQueryBuilder.distance(opLocationMapQueryDTO.getRange(), DistanceUnit.KILOMETERS);
            boolQueryBuilder.must(geoDistanceQueryBuilder);
        }
//        Iterable<OpLocationElasticDTO> opLocationElasticDTOIterable = opLocationElastic.search(boolQueryBuilder);
        ArrayList<OpLocationElasticDTO> opLocationElasticDTOArrayList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toCollection(Lists::newArrayList));
        log.info("app地图搜索ES场站：{}  {}", opLocationElasticDTOArrayList.size(), JSON.toJSONString(opLocationElasticDTOArrayList));
        List<OpLocationMapQueryVO> opLocationMapQueryVOList = DozerConvert.mapList(opLocationElasticDTOArrayList, OpLocationMapQueryVO.class);
        stopWatch.stop();
        if (CollectionUtils.isEmpty(opLocationMapQueryVOList)) {
            // 当从ES查询为空，返回hubject设备
            return hubjectEvseList;
        }

        //封装设备map
        stopWatch.start("查询站点设备");
        Map<Long, List<OpLocationEvseElasticDTO>> stationIdStationESEVSEListMap = buildStationIdESEVSEListMap(esEVSEList);
        stopWatch.stop();

        //封装结果集
        stopWatch.start("封装结果集");
        if (CollectionUtils.isNotEmpty(opLocationMapQueryVOList)) {
            double range = opLocationMapQueryDTO.getRange() == null ? 10.00 : opLocationMapQueryDTO.getRange();
            for (OpLocationMapQueryVO opLocationMapQueryVO : opLocationMapQueryVOList) {
                //额外设置stationId，给app传参
                opLocationMapQueryVO.setStationId(opLocationMapQueryVO.getId());
                //范围
                opLocationMapQueryVO.setRange(range);
                //距离
                if (opLocationMapQueryDTO.getLongitude() != null && opLocationMapQueryDTO.getLatitude() != null) {
                    double distance = GeoDistance.ARC.calculate(opLocationMapQueryDTO.getLatitude(), opLocationMapQueryDTO.getLongitude(),
                            Double.valueOf(opLocationMapQueryVO.getLatitude()), Double.valueOf(opLocationMapQueryVO.getLongitude()),
                            DistanceUnit.KILOMETERS);
                    opLocationMapQueryVO.setDistance(String.format("%.2f", distance));
                }
                //站点设备集合
                List<OpLocationEvseElasticDTO> stationEsEVSEList = stationIdStationESEVSEListMap.get(opLocationMapQueryVO.getId());
                log.info("站点：{}   设备集合：{}", opLocationMapQueryVO.getId(), JSON.toJSONString(stationEsEVSEList));
                //场站状态
                Integer locationStatusCode = buildLocationStatus(stationEsEVSEList);
                opLocationMapQueryVO.setLocationStatusCode(locationStatusCode);
                opLocationMapQueryVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                //超充、快充、慢充
                Integer veryFastTotal = 0;
                Integer veryFast = 0;
                Integer fastTotal = 0;
                Integer fast = 0;
                Integer slowTotal = 0;
                Integer slow = 0;
                if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                    Double veryFastPower = 240D;
                    Double fastPower = 22D;

                    for (OpLocationEvseElasticDTO esEVSE : stationEsEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower >= veryFastPower) {
                            veryFastTotal++;
                            if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                veryFast++;
                            }
                        } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                            fastTotal++;
                            if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fast++;
                            }
                        } else {
                            slowTotal++;
                            if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slow++;
                            }
                        }
                    }
                }
                opLocationMapQueryVO.setVeryFastTotal(veryFastTotal);
                opLocationMapQueryVO.setVeryFast(veryFast);
                opLocationMapQueryVO.setFastTotal(fastTotal);
                opLocationMapQueryVO.setFast(fast);
                opLocationMapQueryVO.setSlowTotal(slowTotal);
                opLocationMapQueryVO.setSlow(slow);
                if (String.valueOf(userId).equals(opLocationMapQueryVO.getOperatorId())) {
                    opLocationMapQueryVO.setStatus(2);
                }
                opLocationMapQueryVO.setSellerId(opLocationMapQueryVO.getOperatorId());
                if (opLocationMapQueryVO.getHubjectCheck() != null && opLocationMapQueryVO.getHubjectCheck()) {
                    opLocationMapQueryVO.setThirdPile(1);
                    opLocationMapQueryVO.setAccessibility(conditionsValue2Accessibility(opLocationMapQueryVO.getConditionsValue()));
                }
            }
            //排序
            Collections.sort(opLocationMapQueryVOList);
        }
        opLocationMapQueryVOList.addAll(hubjectEvseList);
        stopWatch.stop();

        log.info("总任务数:{}  总耗时:{}  具体耗时信息:{}", stopWatch.getTaskCount(), stopWatch.getTotalTimeMillis(), JSON.toJSONString(stopWatch.getTaskInfo()));
        return opLocationMapQueryVOList;
    }

    @Override
    public List<OpLocationMapQueryVO> mapQuery2(OpLocationMapQueryDTO dto) {
        //缓存地图搜索条件，用于地图搜索场站时的筛选（ACG-1938）
        try {
            String redisKey = String.format(RedisKeyConstant.APP_MAP_QUERY_REQUIREMENT_USER, LoginUserHolder.getLoginUser().getId());
            redisUtil.set(redisKey, JSON.toJSONString(dto));
        } catch (Exception e) {
            log.info("缓存地图搜索条件出错了");
        }

        Long userId = LoginUserHolder.getLoginUser().getId();
        List<OpLocationMapQueryVO> hubjectEvseList = new ArrayList<>();
        //整合hubject桩到opLocationEvseExpandElastic了
        hubjectMapQuery(dto, hubjectEvseList);
        StopWatch stopWatch = new StopWatch("APP地图搜索场站");
        stopWatch.start("场站搜索");
        BoolQueryBuilder boolSearch = QueryBuilders.boolQuery();
        //按功率查询
        if (dto.getHighPower() != null || dto.getLowPower() != null) {
            RangeQueryBuilder rangeSearch = QueryBuilders.rangeQuery("power");
            if (dto.getHighPower() != null) {
                rangeSearch.lte(dto.getHighPower());
            }
            if (dto.getLowPower() != null) {
                rangeSearch.gte(dto.getLowPower());
            }
            boolSearch.must(rangeSearch);
        }
        //按枪类型查询
        if (dto.getStatus() != null || CollectionUtils.isNotEmpty(dto.getGunTypes())) {
            if (dto.getStatus() != null && dto.getStatus() == 1) {
                boolSearch.must(QueryBuilders.termQuery("gunState", EvseDeviceStatusEnum.AVAILABLE.getName()));
                boolSearch.mustNot(QueryBuilders.termQuery("platform", 2));
            }
            if (CollectionUtils.isNotEmpty(dto.getGunTypes())) {
                boolSearch.must(QueryBuilders.termsQuery("gunType", dto.getGunTypes()));
            }
        }
        //按经纬度查询
        boolSearch.must(QueryBuilders.termsQuery("appShow", true));
        if (dto.getLongitude() != null && dto.getLatitude() != null) {
            GeoDistanceQueryBuilder geoDistanceQueryBuilder = new GeoDistanceQueryBuilder("location");
            //中心点的构建
            geoDistanceQueryBuilder.point(dto.getLatitude(), dto.getLongitude());
            //范围设定
            geoDistanceQueryBuilder.distance(dto.getRange(), DistanceUnit.KILOMETERS);
            boolSearch.must(geoDistanceQueryBuilder);
        }
        //按是否支持预约查询
        if (dto.getReservationEnabled() != null && dto.getReservationEnabled()) {
            boolSearch.must(QueryBuilders.termQuery("reservationEnabled", true));
        }

        //需要过滤掉订阅过期了的桩
        if (subscribeEnable) {
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            // 8 planetev
            if ("8".equalsIgnoreCase(appId)) {
                List<Long> sellerIdList = getSellerIdList("1");
                boolSearch.must(QueryBuilders.termsQuery("operatorId", sellerIdList));
            } else {
                boolSearch.must(QueryBuilders.termQuery("subscriptionCheck", true));
            }
        }
        //广告场站下的桩不展示
        List<Integer> types = new ArrayList<>();
        types.add(1);
        types.add(2);
        boolSearch.must(QueryBuilders.termsQuery("businessType", types));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(boolSearch)
                .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"}))
                .withPageable(PageRequest.of(0, 1000))
                .build();
        //默认滚动查询时间一分钟，每次查10条
        Iterator<OpLocationEvseExpandElasticDTO> it = elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).iterator();
        stopWatch.stop();
        stopWatch.start("结果处理");
        Map<Long, List<OpLocationEvseExpandElasticDTO>> resultMap = new HashMap<>();
        while (it.hasNext()) {
            OpLocationEvseExpandElasticDTO next = it.next();
            Long locationId = next.getLocationId();
            List<OpLocationEvseExpandElasticDTO> list = resultMap.get(locationId);
            if (list == null) {
                list = new ArrayList<>();
            }
            list.add(next);
            resultMap.put(locationId, list);
        }
        log.info("mapQuery2,resultMap size={}", resultMap.size());
        if (!resultMap.isEmpty()) {
            List<OpLocationMapQueryVO> resultList = new ArrayList<>();
            resultMap.forEach((k, v) -> {
                OpLocationMapQueryVO vo = new OpLocationMapQueryVO();
                OpLocationEvseExpandElasticDTO entity = v.get(0);
                BeanUtils.copyProperties(entity, vo);
                double range = dto.getRange() == null ? 10.00 : dto.getRange();
                //额外设置stationId，给app传参
                vo.setStationId(k);
                vo.setId(k);
                //范围
                vo.setRange(range);
                //距离
                if (dto.getLongitude() != null && dto.getLatitude() != null) {
                    double distance = GeoDistance.ARC.calculate(dto.getLatitude(), dto.getLongitude(),
                            Double.valueOf(vo.getLatitude()), Double.valueOf(vo.getLongitude()),
                            DistanceUnit.KILOMETERS);
                    vo.setDistance(String.format("%.2f", distance));
                }
                //站点设备集合
                List<OpLocationEvseExpandElasticDTO> stationEsEVSEList = v;
                //场站状态
                if (!CollectionUtils.isEmpty(stationEsEVSEList)) {
                    stationEsEVSEList = stationEsEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
                }
                Integer locationStatusCode = buildLocationStatus2(stationEsEVSEList);
                vo.setLocationStatusCode(locationStatusCode);
                vo.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                //超充、快充、慢充
                Integer veryFastTotal = 0;
                Integer veryFast = 0;
                Integer fastTotal = 0;
                Integer fast = 0;
                Integer slowTotal = 0;
                Integer slow = 0;
                if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                    Double veryFastPower = 240D;
                    Double fastPower = 22D;
                    for (OpLocationEvseExpandElasticDTO esEVSE : stationEsEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower >= veryFastPower) {
                            veryFastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                veryFast++;
                            }
                        } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                            fastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fast++;
                            }
                        } else {
                            slowTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slow++;
                            }
                        }
                    }
                }
                vo.setVeryFastTotal(veryFastTotal);
                vo.setVeryFast(veryFast);
                vo.setFastTotal(fastTotal);
                vo.setFast(fast);
                vo.setSlowTotal(slowTotal);
                vo.setSlow(slow);
                //判断是否支持预约
                if (null != entity.getReservationEnabled()) {
                    vo.setReservationEnabled(entity.getReservationEnabled());
                }
                if (userId.longValue() == (entity.getOperatorId() == null ? 0L : entity.getOperatorId().longValue())) {
                    vo.setStatus(2);
                }
                vo.setSellerId(String.valueOf(entity.getOperatorId()));
                if (vo.getHubjectCheck() != null && vo.getHubjectCheck()) {
                    vo.setThirdPile(1);
                    vo.setAccessibility(conditionsValue2Accessibility(vo.getConditionsValue()));
                }
                resultList.add(vo);
            });
            //排序
            Collections.sort(resultList);
            hubjectEvseList.addAll(resultList);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return hubjectEvseList;
    }

    /**
     * 地图找桩支持搜索
     *
     * @param opLocationMapQueryPageDTO 检索对象
     * @return 场站数据
     */
    @Override
    public Result<Page<OpLocationMapQueryVO>> mapQueryPile(OpLocationMapQueryPageDTO opLocationMapQueryPageDTO) {
        Long userId = LoginUserHolder.getLoginUser().getId();

        //查询缓存中是否有地图筛选条件
        String redisKey = String.format(RedisKeyConstant.APP_MAP_QUERY_REQUIREMENT_USER, userId);
        String opLocationMapQueryDTOString = (String) redisUtil.get(redisKey);
        log.info("地图搜索场站，用户：{} 的筛选条件：{}", userId, opLocationMapQueryDTOString);
        if (StringUtils.isNotBlank(opLocationMapQueryDTOString)) {
            OpLocationMapQueryDTO opLocationMapQueryDTO = JSON.parseObject(opLocationMapQueryDTOString, OpLocationMapQueryDTO.class);
            opLocationMapQueryPageDTO.setStatus(opLocationMapQueryDTO.getStatus());
            opLocationMapQueryPageDTO.setGunTypes(opLocationMapQueryDTO.getGunTypes());
            opLocationMapQueryPageDTO.setHighPower(opLocationMapQueryDTO.getHighPower());
            opLocationMapQueryPageDTO.setLowPower(opLocationMapQueryDTO.getLowPower());
        }

        //搜索条件构建
        Page<OpLocationMapQueryVO> resultPage = new Page<>();
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        BoolQueryBuilder snNameOrAddressQueryBuilder = QueryBuilders.boolQuery();
        snNameOrAddressQueryBuilder.should(QueryBuilders.wildcardQuery("name", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        snNameOrAddressQueryBuilder.should(QueryBuilders.wildcardQuery("address", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        queryBuilder.must(snNameOrAddressQueryBuilder);
        queryBuilder.must(QueryBuilders.termsQuery("appShow", true));
        //ES设备查询
        BoolQueryBuilder boolSearch = QueryBuilders.boolQuery();
        BoolQueryBuilder snNameOrAddressQueryEvseBuilder = QueryBuilders.boolQuery();
        snNameOrAddressQueryEvseBuilder.should(QueryBuilders.wildcardQuery("name", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        snNameOrAddressQueryEvseBuilder.should(QueryBuilders.wildcardQuery("address", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        boolSearch.must(snNameOrAddressQueryEvseBuilder);
        boolSearch.must(QueryBuilders.termsQuery("appShow", true));
        boolSearch.must(QueryBuilders.termsQuery("businessType", Arrays.asList(1, 2)));
        boolean needHubject = oicpFeign.checkUserIsGray();
        if (!needHubject) {
            boolSearch.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        boolSearch.must(QueryBuilders.existsQuery("tariffId"));
        //按功率查询
        if (opLocationMapQueryPageDTO.getHighPower() != null || opLocationMapQueryPageDTO.getLowPower() != null) {
            RangeQueryBuilder rangeSearch = QueryBuilders.rangeQuery("power");
            if (opLocationMapQueryPageDTO.getHighPower() != null) {
                rangeSearch.lte(opLocationMapQueryPageDTO.getHighPower());
            }
            if (opLocationMapQueryPageDTO.getLowPower() != null) {
                rangeSearch.gte(opLocationMapQueryPageDTO.getLowPower());
            }
            boolSearch.must(rangeSearch);
        }
        //按枪类型查询
        if (opLocationMapQueryPageDTO.getStatus() != null || CollectionUtils.isNotEmpty(opLocationMapQueryPageDTO.getGunTypes())) {
            if (opLocationMapQueryPageDTO.getStatus() != null && opLocationMapQueryPageDTO.getStatus() == 1) {
                boolSearch.must(QueryBuilders.termQuery("gunState", EvseDeviceStatusEnum.AVAILABLE.getName()));
                boolSearch.mustNot(QueryBuilders.termQuery("platform", 2));
            }
            if (CollectionUtils.isNotEmpty(opLocationMapQueryPageDTO.getGunTypes())) {
                boolSearch.must(QueryBuilders.termsQuery("gunType", opLocationMapQueryPageDTO.getGunTypes()));
            }
        }
        //加上planetEV-emsp条件
        appAddEMSPCondition(boolSearch);

        Iterable<OpLocationEvseExpandElasticDTO> search =
//                opLocationEvseExpandElastic.search(boolSearch);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                        .withQuery(boolSearch)
                        .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"})).build(), OpLocationEvseExpandElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());


        List<OpLocationEvseExpandElasticDTO> esEVSEList = Lists.newArrayList(search);
        if (ObjectUtils.isEmpty(esEVSEList)) {
            resultPage.setTotal(0L);
            resultPage.setRecords(new ArrayList<>());
            return Result.ofSucceed(resultPage);
        }
        List<Long> haveEVSEStationIds = esEVSEList.stream().map(OpLocationEvseExpandElasticDTO::getLocationId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        Iterator<OpLocationEvseExpandElasticDTO> it = search.iterator();
        Map<Long, List<OpLocationEvseExpandElasticDTO>> resultMap = new HashMap<>();
        while (it.hasNext()) {
            OpLocationEvseExpandElasticDTO next = it.next();
            Long locationId = next.getLocationId();
            List<OpLocationEvseExpandElasticDTO> list = resultMap.get(locationId);
            if (list == null) {
                list = new ArrayList<>();
            }
            list.add(next);
            resultMap.put(locationId, list);
        }
        //展示有枪的场站
        if (CollectionUtils.isNotEmpty(haveEVSEStationIds)) {
            queryBuilder.must(QueryBuilders.termsQuery("id", haveEVSEStationIds));
        }
        if (!needHubject) {
            queryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        //加上planetEV-emsp条件
        appAddEMSPCondition(queryBuilder);

        PageRequest pageRequest = PageRequest.of(opLocationMapQueryPageDTO.getCurrent() - 1, opLocationMapQueryPageDTO.getPageSize());
        NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"}))
                .withPageable(pageRequest)
                .build();

//        org.springframework.data.domain.Page<OpLocationElasticDTO> opLocationElasticDTOS = opLocationElastic.search(nativeSearchQuery);
//        List<OpLocationElasticDTO> locationElasticDTOS = opLocationElasticDTOS.getContent();
        SearchHits<OpLocationElasticDTO> searchHits =
                elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationElasticDTO.class);
        SearchPage<OpLocationElasticDTO> searchHitsPage = SearchHitSupport.searchPageFor(searchHits, nativeSearchQuery.getPageable());
        List<OpLocationElasticDTO> locationElasticDTOS = searchHitsPage.stream().map(SearchHit::getContent).collect(Collectors.toList());
//        List<OpLocationPileEvseElasticDTO> locationElasticDTOS = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<OpLocationMapQueryVO> opLocationMapQueryVOList = DozerConvert.mapList(locationElasticDTOS, OpLocationMapQueryVO.class);
        //封装结果
        if (CollectionUtils.isNotEmpty(opLocationMapQueryVOList)) {
            for (OpLocationMapQueryVO opLocationMapQueryVO : opLocationMapQueryVOList) {
                opLocationMapQueryVO.setKeyword(opLocationMapQueryPageDTO.getStationNameOrAddress());
                //额外设置stationId，给app传参
                opLocationMapQueryVO.setStationId(opLocationMapQueryVO.getId());
                //站点设备集合
                List<OpLocationEvseExpandElasticDTO> stationEsEVSEList = resultMap.get(opLocationMapQueryVO.getId());
                //场站状态
                if (!CollectionUtils.isEmpty(stationEsEVSEList)) {
                    stationEsEVSEList = stationEsEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
                }
                Integer locationStatusCode = buildLocationStatus2(stationEsEVSEList);
                opLocationMapQueryVO.setLocationStatusCode(locationStatusCode);
                opLocationMapQueryVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                //超充、快充、慢充
                Integer veryFastTotal = 0;
                Integer veryFast = 0;
                Integer fastTotal = 0;
                Integer fast = 0;
                Integer slowTotal = 0;
                Integer slow = 0;
                if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                    Double veryFastPower = 240D;
                    Double fastPower = 22D;

                    for (OpLocationEvseExpandElasticDTO esEVSE : stationEsEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower >= veryFastPower) {
                            veryFastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                veryFast++;
                            }
                        } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                            fastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fast++;
                            }
                        } else {
                            slowTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slow++;
                            }
                        }
                    }
                }
                opLocationMapQueryVO.setVeryFastTotal(veryFastTotal);
                opLocationMapQueryVO.setVeryFast(veryFast);
                opLocationMapQueryVO.setFastTotal(fastTotal);
                opLocationMapQueryVO.setFast(fast);
                opLocationMapQueryVO.setSlowTotal(slowTotal);
                opLocationMapQueryVO.setSlow(slow);
                if (String.valueOf(userId).equals(opLocationMapQueryVO.getOperatorId())) {
                    opLocationMapQueryVO.setStatus(2);
                }
                opLocationMapQueryVO.setSellerId(opLocationMapQueryVO.getOperatorId());
                if (opLocationMapQueryVO.getHubjectCheck() != null && opLocationMapQueryVO.getHubjectCheck()) {
                    opLocationMapQueryVO.setThirdPile(1);
                    opLocationMapQueryVO.setAccessibility(conditionsValue2Accessibility(opLocationMapQueryVO.getConditionsValue()));
                }
            }
            resultPage.setRecords(opLocationMapQueryVOList);
            resultPage.setCurrent(opLocationMapQueryPageDTO.getCurrent());
            resultPage.setSize(opLocationMapQueryVOList.size());

            PageImpl<OpLocationElasticDTO> pageImpl = new PageImpl<>(locationElasticDTOS, pageRequest, searchHits.getTotalHits());
            resultPage.setTotal(pageImpl.getTotalElements());
            resultPage.setPages(pageImpl.getTotalPages());
        }
        return Result.ofSucceed(resultPage);
    }

    /**
     * 地图找桩支持搜索
     *
     * @param opLocationMapQueryPageDTO 检索对象
     * @return 场站数据
     */
    @Override
    public Result<Page<OpLocationMapQueryVO>> mapQueryPileForOCPI(OpLocationMapQueryPageDTO opLocationMapQueryPageDTO) {
        Long userId = LoginUserHolder.getLoginUser().getId();
        //搜索条件构建
        Page<OpLocationMapQueryVO> resultPage = new Page<>();
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        BoolQueryBuilder snNameOrAddressQueryBuilder = QueryBuilders.boolQuery();
        snNameOrAddressQueryBuilder.should(QueryBuilders.wildcardQuery("name", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        snNameOrAddressQueryBuilder.should(QueryBuilders.wildcardQuery("address", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        queryBuilder.must(snNameOrAddressQueryBuilder);
        //ES设备查询
        BoolQueryBuilder boolSearch = QueryBuilders.boolQuery();
        BoolQueryBuilder snNameOrAddressQueryEvseBuilder = QueryBuilders.boolQuery();
        snNameOrAddressQueryEvseBuilder.should(QueryBuilders.wildcardQuery("name", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        snNameOrAddressQueryEvseBuilder.should(QueryBuilders.wildcardQuery("address", "*" + opLocationMapQueryPageDTO.getStationNameOrAddress() + "*"));
        boolSearch.must(snNameOrAddressQueryEvseBuilder);
        boolean needHubject = oicpFeign.checkUserIsGray();
        if (!needHubject) {
            boolSearch.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        boolSearch.must(QueryBuilders.existsQuery("tariffId"));
        //需要过滤掉订阅过期了的桩
        if (subscribeEnable) {
            //boolSearch.must(QueryBuilders.termsQuery("subscriptionCheck", true));
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            // 8 planetev
            if ("8".equalsIgnoreCase(appId)) {
                List<Long> sellerIdList = getSellerIdList("1");
                boolSearch.must(QueryBuilders.termsQuery("operatorId", sellerIdList));
            } else {
                boolSearch.must(QueryBuilders.termQuery("subscriptionCheck", true));
            }
        }

        Iterable<OpLocationEvseExpandElasticDTO> search =
//                opLocationEvseExpandElastic.search(boolSearch);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolSearch).build(), OpLocationEvseExpandElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());


        List<OpLocationEvseExpandElasticDTO> esEVSEList = Lists.newArrayList(search);
        if (ObjectUtils.isEmpty(esEVSEList)) {
            resultPage.setTotal(0L);
            resultPage.setRecords(new ArrayList<>());
            return Result.ofSucceed(resultPage);
        }
        List<Long> haveEVSEStationIds = esEVSEList.stream().map(OpLocationEvseExpandElasticDTO::getLocationId).distinct().filter(Objects::nonNull).collect(Collectors.toList());
        Iterator<OpLocationEvseExpandElasticDTO> it = search.iterator();
        Map<Long, List<OpLocationEvseExpandElasticDTO>> resultMap = new HashMap<>();
        while (it.hasNext()) {
            OpLocationEvseExpandElasticDTO next = it.next();
            Long locationId = next.getLocationId();
            List<OpLocationEvseExpandElasticDTO> list = resultMap.get(locationId);
            if (list == null) {
                list = new ArrayList<>();
            }
            list.add(next);
            resultMap.put(locationId, list);
        }
        //展示有枪的场站
        if (CollectionUtils.isNotEmpty(haveEVSEStationIds)) {
            queryBuilder.must(QueryBuilders.termsQuery("id", haveEVSEStationIds));
        }
        if (!needHubject) {
            queryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        PageRequest pageRequest = PageRequest.of(opLocationMapQueryPageDTO.getCurrent() - 1, opLocationMapQueryPageDTO.getPageSize());
        NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"}))
                .withPageable(pageRequest)
                .build();

//        org.springframework.data.domain.Page<OpLocationElasticDTO> opLocationElasticDTOS = opLocationElastic.search(nativeSearchQuery);
//        List<OpLocationElasticDTO> locationElasticDTOS = opLocationElasticDTOS.getContent();
        SearchHits<OpLocationElasticDTO> searchHits =
                elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationElasticDTO.class);
        SearchPage<OpLocationElasticDTO> searchHitsPage = SearchHitSupport.searchPageFor(searchHits, nativeSearchQuery.getPageable());
        List<OpLocationElasticDTO> locationElasticDTOS = searchHitsPage.stream().map(SearchHit::getContent).collect(Collectors.toList());
//        List<OpLocationPileEvseElasticDTO> locationElasticDTOS = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<OpLocationMapQueryVO> opLocationMapQueryVOList = DozerConvert.mapList(locationElasticDTOS, OpLocationMapQueryVO.class);
        //封装结果
        if (CollectionUtils.isNotEmpty(opLocationMapQueryVOList)) {
            for (OpLocationMapQueryVO opLocationMapQueryVO : opLocationMapQueryVOList) {
                opLocationMapQueryVO.setKeyword(opLocationMapQueryPageDTO.getStationNameOrAddress());
                //额外设置stationId，给app传参
                opLocationMapQueryVO.setStationId(opLocationMapQueryVO.getId());
                //站点设备集合
                List<OpLocationEvseExpandElasticDTO> stationEsEVSEList = resultMap.get(opLocationMapQueryVO.getId());
                //场站状态
                if (!CollectionUtils.isEmpty(stationEsEVSEList)) {
                    stationEsEVSEList = stationEsEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
                }
                Integer locationStatusCode = buildLocationStatus2(stationEsEVSEList);
                opLocationMapQueryVO.setLocationStatusCode(locationStatusCode);
                opLocationMapQueryVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                //超充、快充、慢充
                Integer veryFastTotal = 0;
                Integer veryFast = 0;
                Integer fastTotal = 0;
                Integer fast = 0;
                Integer slowTotal = 0;
                Integer slow = 0;
                if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                    Double veryFastPower = 240D;
                    Double fastPower = 22D;

                    for (OpLocationEvseExpandElasticDTO esEVSE : stationEsEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower >= veryFastPower) {
                            veryFastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                veryFast++;
                            }
                        } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                            fastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fast++;
                            }
                        } else {
                            slowTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slow++;
                            }
                        }
                    }
                }
                opLocationMapQueryVO.setVeryFastTotal(veryFastTotal);
                opLocationMapQueryVO.setVeryFast(veryFast);
                opLocationMapQueryVO.setFastTotal(fastTotal);
                opLocationMapQueryVO.setFast(fast);
                opLocationMapQueryVO.setSlowTotal(slowTotal);
                opLocationMapQueryVO.setSlow(slow);
                if (String.valueOf(userId).equals(opLocationMapQueryVO.getOperatorId())) {
                    opLocationMapQueryVO.setStatus(2);
                }
                opLocationMapQueryVO.setSellerId(opLocationMapQueryVO.getOperatorId());
                if (opLocationMapQueryVO.getHubjectCheck() != null && opLocationMapQueryVO.getHubjectCheck()) {
                    opLocationMapQueryVO.setThirdPile(1);
                    opLocationMapQueryVO.setAccessibility(conditionsValue2Accessibility(opLocationMapQueryVO.getConditionsValue()));
                }
            }
            resultPage.setRecords(opLocationMapQueryVOList);
            resultPage.setCurrent(opLocationMapQueryPageDTO.getCurrent());
            resultPage.setSize(opLocationMapQueryVOList.size());

            PageImpl<OpLocationElasticDTO> pageImpl = new PageImpl<>(locationElasticDTOS, pageRequest, searchHits.getTotalHits());
            resultPage.setTotal(pageImpl.getTotalElements());
            resultPage.setPages(pageImpl.getTotalPages());
        }
        return Result.ofSucceed(resultPage);
    }

    /**
     * 增加Hubject桩接入
     * 1. 根据坐标查询桩和状态
     * 2. 根据桩序列号查询桩详情
     *
     * @param opLocationMapQueryDTO opLocationMapQueryDTO
     * @param hubjectEvseList       hubjectEvseList
     */
    private void hubjectMapQuery(OpLocationMapQueryDTO opLocationMapQueryDTO, List<OpLocationMapQueryVO> hubjectEvseList) {
        if (oicpFeign.checkUserIsGray() && opLocationMapQueryDTO.getStatus() != 1) {
            try {
                log.info("查询hubject充电设备");
                PullEvseStatusDTO pullEvseStatusDTO = new PullEvseStatusDTO();

                GeoCoordinatesDTO.DecimalDegree decimalDegree = null;
                if (opLocationMapQueryDTO.getLongitude() != null && opLocationMapQueryDTO.getLatitude() != null) {
                    decimalDegree = new GeoCoordinatesDTO.DecimalDegree(String.valueOf(opLocationMapQueryDTO.getLongitude()),
                            String.valueOf(opLocationMapQueryDTO.getLatitude()));
                }
                if (opLocationMapQueryDTO.getStatus() == 1) {
                    pullEvseStatusDTO.setEvseStatus(opLocationMapQueryDTO.getStatus());
                }
                pullEvseStatusDTO.setDecimalDegree(decimalDegree);
                double range = opLocationMapQueryDTO.getRange() == null ? 10L : opLocationMapQueryDTO.getRange();
                pullEvseStatusDTO.setRadius(BigDecimal.valueOf(range).setScale(1, RoundingMode.UP));

                try {
                    Result<List<PullEvseDataRespDTO>> hubjectEvseData = oicpFeign.queryEvseData(pullEvseStatusDTO);
                    log.info("云端查询充电桩数据: {}", hubjectEvseData);
                    Map<Long, List<PullEvseDataRespDTO>> hubjectEvseDataMap = hubjectEvseData.getData()
                            .stream()
                            .filter(item -> {
                                if (opLocationMapQueryDTO.getHighPower() != null || opLocationMapQueryDTO.getLowPower() != null) {
                                    //按照功率查询
                                    if (CollectionUtils.isEmpty(item.getChargingFacilities())) {
                                        return false;
                                    }
                                    ChargingFacilityDTO chargingFacilityDTO = item.getChargingFacilities().get(0);
                                    Integer power = chargingFacilityDTO.getPower();
                                    if (power == null || (opLocationMapQueryDTO.getHighPower() != null && power > opLocationMapQueryDTO.getHighPower())) {
                                        return false;
                                    }
                                    return opLocationMapQueryDTO.getLowPower() == null || power >= opLocationMapQueryDTO.getLowPower();
                                }
                                return true;
                            })
                            // todo 按照枪类型查询
                            .collect(Collectors.groupingBy(PullEvseDataRespDTO::getChargingStationEsId));

                    Set<Long> longs = hubjectEvseDataMap.keySet();
                    log.info("从Hubject查询的场站id {}", JSON.toJSONString(longs));
                    if (CollectionUtils.isNotEmpty(longs)) {
                        for (long chargeId : longs) {
                            List<PullEvseDataRespDTO> pullEvseDataRespDTOS = hubjectEvseDataMap.get(chargeId);

                            PullEvseDataRespDTO pullEvseDataRespDTO = pullEvseDataRespDTOS.get(0);

                            OpLocationMapQueryVO opLocationMapQueryVO = new OpLocationMapQueryVO();

                            opLocationMapQueryVO.setStationId(chargeId);
                            opLocationMapQueryVO.setId(chargeId);

                            if (CollectionUtils.isNotEmpty(pullEvseDataRespDTO.getChargingStationNames())) {
                                // todo 后续根据用户语言过滤
                                opLocationMapQueryVO.setName(pullEvseDataRespDTO.getChargingStationNames().get(0).getValue());
                            }

                            LocationAddressDTO hubAddr = pullEvseDataRespDTO.getAddress();
                            if (hubAddr != null) {
                                String country = changeHubjectCountry(hubAddr.getCountry());
                                String address = country + " " + hubAddr.getCity() + " " +
                                        hubAddr.getStreet() + " " + hubAddr.getHouseNum();
                                opLocationMapQueryVO.setAddress(address);
                            }

                            //范围
                            opLocationMapQueryVO.setRange(range);
                            //距离
                            if (opLocationMapQueryDTO.getLongitude() != null && opLocationMapQueryDTO.getLatitude() != null) {
                                String latitude = pullEvseDataRespDTO.getGeoCoordinates().getDecimalDegree().getLatitude();
                                String longitude = pullEvseDataRespDTO.getGeoCoordinates().getDecimalDegree().getLongitude();
                                double distance = GeoDistance.ARC.calculate(Double.parseDouble(latitude), Double.parseDouble(longitude),
                                        opLocationMapQueryDTO.getLatitude(), opLocationMapQueryDTO.getLongitude(),
                                        DistanceUnit.KILOMETERS);
                                opLocationMapQueryVO.setDistance(String.format("%.2f", distance));
                                opLocationMapQueryVO.setLatitude(latitude);
                                opLocationMapQueryVO.setLongitude(longitude);
                            }
                            //站点设备集合

                            //场站状态
                            // todo 先设置为空闲
                            EvseStatusRecordDTO evseStatusRecordDTO = new EvseStatusRecordDTO();
                            evseStatusRecordDTO.setEvseStatus(pullEvseDataRespDTO.getHubStatus());

                            List<EvseStatusRecordDTO> collect = pullEvseDataRespDTOS.stream()
                                    .map(item -> EvseStatusRecordDTO.builder()
                                            .evseId(item.getEvseId())
                                            .evseStatus(item.getHubStatus())
                                            .build())
                                    .collect(Collectors.toList());
                            Integer locationStatusCode = buildHubjectLocationStatus(collect);

                            opLocationMapQueryVO.setLocationStatusCode(locationStatusCode);
                            opLocationMapQueryVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                            //超充、快充、慢充
                            Integer veryFastTotal = 0;
                            Integer veryFast = 0;
                            Integer fastTotal = 0;
                            Integer fast = 0;
                            Integer slowTotal = 0;
                            Integer slow = 0;
                            if (CollectionUtils.isNotEmpty(pullEvseDataRespDTOS)) {
                                double veryFastPower = 240D;
                                double fastPower = 22D;

                                for (PullEvseDataRespDTO esEVSE : pullEvseDataRespDTOS) {

                                    Integer evseStatus = esEVSE.getHubStatus();

                                    if (CollectionUtils.isEmpty(esEVSE.getChargingFacilities())) {
                                        slowTotal++;
                                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                                            slow++;
                                        }
                                        continue;
                                    }
                                    ChargingFacilityDTO chargingFacilityDTO = esEVSE.getChargingFacilities().get(0);
                                    double evsePower = chargingFacilityDTO.getPower() == null ? 0D : chargingFacilityDTO.getPower();
                                    if (evsePower >= veryFastPower) {
                                        veryFastTotal++;
                                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                                            veryFast++;
                                        }
                                    } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                                        fastTotal++;
                                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                                            fast++;
                                        }
                                    } else {
                                        slowTotal++;
                                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                                            slow++;
                                        }
                                    }
                                }
                            }
                            opLocationMapQueryVO.setVeryFastTotal(veryFastTotal);
                            opLocationMapQueryVO.setVeryFast(veryFast);
                            opLocationMapQueryVO.setFastTotal(fastTotal);
                            opLocationMapQueryVO.setFast(fast);
                            opLocationMapQueryVO.setSlowTotal(slowTotal);
                            opLocationMapQueryVO.setSlow(slow);
                            opLocationMapQueryVO.setThirdPile(1);
                            // hubject场站设置为在appshow
                            opLocationMapQueryVO.setAppShow(1);
                            if (pullEvseDataRespDTO.getAccessibility() == 1) {
                                opLocationMapQueryVO.setAccessibility(1);
                            } else if (pullEvseDataRespDTO.getAccessibility() == 3) {
                                opLocationMapQueryVO.setAccessibility(3);
                            } else {
                                opLocationMapQueryVO.setAccessibility(2);
                            }
                            hubjectEvseList.add(opLocationMapQueryVO);
                        }
                    }
                } catch (Exception e) {
                    log.error("从hubject平台拉取充电设备数据失败", e);
                }

            } catch (Exception e) {
                log.error("hubject平台充电设备数据加载失败", e);
            }
        }
    }

    /**
     * 场站卡片
     *
     * @param opLocationCardDTO 检索对象
     * @return 返回对象
     */
    @Override
    public OpLocationCardVO stationCardData(OpLocationCardDTO opLocationCardDTO) {
        OpLocationCardVO opLocationCardVO = new OpLocationCardVO();
        Long stationId = opLocationCardDTO.getStationId();
        Long userId = LoginUserHolder.getLoginUser().getId();
        //查询es场站
        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(stationId);
        if (optionalOpLocationElasticDTO.isPresent() && optionalOpLocationElasticDTO.get().getPlatform() != 2) {
            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
            opLocationCardVO.setStationId(opLocationElasticDTO.getId());
            opLocationCardVO.setName(opLocationElasticDTO.getName());
            opLocationCardVO.setUserSellerId(opLocationElasticDTO.getOperatorId());
            opLocationCardVO.setStatus(opLocationElasticDTO.getStatus());
            if (userId.equals(opLocationElasticDTO.getOperatorId())) {
                opLocationCardVO.setStatus(EVSETypeEnum.HOME_PILE.getCode());
            }
            //私桩共享场站，桩主跟家庭成员一样的icon标识
            if (opLocationCardVO.getStatus() != null && opLocationCardVO.getStatus() == EVSETypeEnum.SHARE_PILE.getCode()) {
                Map<Long, Integer> locationsStatus = this.getLocationsStatus(Arrays.asList(stationId));
                if (!org.springframework.util.CollectionUtils.isEmpty(locationsStatus) && locationsStatus.get(stationId) != null) {
                    opLocationCardVO.setStatus(locationsStatus.get(stationId));
                }
            }
            //国家简码转换
            String country = changeCountry(opLocationElasticDTO.getCountry());
            opLocationCardVO.setCountry(country);
            opLocationCardVO.setCity(opLocationElasticDTO.getCity());
            String address = StringUtils.isBlank(opLocationElasticDTO.getAddress()) ? country + opLocationElasticDTO.getCity() : opLocationElasticDTO.getAddress();
            opLocationCardVO.setAddress(address);
            //距离
            Double lat = opLocationElasticDTO.getLatitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLatitude());
            Double lon = opLocationElasticDTO.getLongitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLongitude());
            opLocationCardVO.setLatitude(lat);
            opLocationCardVO.setLongitude(lon);
            if (opLocationCardDTO.getLatitude() != null && opLocationCardDTO.getLongitude() != null) {
                try {
                    double calculateDistance = GeoDistance.ARC.calculate(lat, lon, opLocationCardDTO.getLatitude(), opLocationCardDTO.getLongitude(), DistanceUnit.KILOMETERS);
                    opLocationCardVO.setDistance(calculateDistance);
                } catch (Exception ignored) {
                }
            }
            //查询db场站运营表
            QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
            opLocationOperationEntityQueryWrapper.eq(BaseConstant.LOCATION_ID, stationId);
            List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(opLocationOperationEntityQueryWrapper);
            if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
                OpLocationOperationEntity opLocationOperationEntity = opLocationOperationEntities.get(0);
                //logPath
                if (opLocationOperationEntity.getLogoImageId() != null) {
                    OpImageEntity opImageEntity = opImageMapper.selectById(opLocationOperationEntity.getLogoImageId());
                    if (opImageEntity != null) {
                        opLocationCardVO.setLogoPath(opImageEntity.getUrl());
                    }
                }
            }
            //查询es枪设备
            List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(stationId);
            esEVSEList = esEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());

            //需要过滤掉订阅过期了的桩
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            if (subscribeEnable && !"8".equalsIgnoreCase(appId)) {
                esEVSEList = esEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getSubscriptionCheck()) && m.getSubscriptionCheck()).collect(Collectors.toList());
            }
            //场站状态
            Integer locationStatusCode = buildLocationStatus(esEVSEList);
            opLocationCardVO.setLocationStatusCode(locationStatusCode);
            opLocationCardVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
            //超充、快充、慢充数量
            Integer veryFastTotalNum = 0;
            Integer veryFastNum = 0;
            Integer fastTotalNum = 0;
            Integer fastNum = 0;
            Integer slowTotalNum = 0;
            Integer slowNum = 0;
            if (CollectionUtils.isNotEmpty(esEVSEList)) {
                Double veryFastPower = 240D;
                Double fastPower = 22D;

                for (OpLocationEvseElasticDTO esEVSE : esEVSEList) {
                    Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                    if (evsePower >= veryFastPower) {
                        veryFastTotalNum++;
                        if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                            veryFastNum++;
                        }
                    } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                        fastTotalNum++;
                        if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                            fastNum++;
                        }
                    } else {
                        slowTotalNum++;
                        if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                            slowNum++;
                        }
                    }
                }
            }
            opLocationCardVO.setVeryFastTotal(veryFastTotalNum);
            opLocationCardVO.setVeryFast(veryFastNum);
            opLocationCardVO.setFastTotal(fastTotalNum);
            opLocationCardVO.setFast(fastNum);
            opLocationCardVO.setSlowTotal(slowTotalNum);
            opLocationCardVO.setSlow(slowNum);
            //不同类型枪数统计
            List<GunTypeGroupVO> gunTypeGroupVOList = buildStationGunTypeGroupVOList2(esEVSEList);
            opLocationCardVO.setPileType(gunTypeGroupVOList);
            //根据设备封装功率范围集合
            Set<Double> stationPowerSet = new HashSet<>();
            if (CollectionUtils.isNotEmpty(esEVSEList)) {
                for (OpLocationEvseElasticDTO esEVSE : esEVSEList) {
                    Double power = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                    stationPowerSet.add(power);
                }
            }
            //不同功率枪数统计
            StationCardPowerCountVO stationCardPowerCountVO = new StationCardPowerCountVO();
            List<StationCardPowerCountDetailVO> fullChargeList = Lists.newArrayList();
            List<StationCardPowerCountDetailVO> slowChargeList = Lists.newArrayList();
            if (!stationPowerSet.isEmpty() && CollectionUtils.isNotEmpty(esEVSEList)) {
                for (Double power : stationPowerSet) {
                    StationCardPowerCountDetailVO fullCharge = new StationCardPowerCountDetailVO();
                    StationCardPowerCountDetailVO slowCharge = new StationCardPowerCountDetailVO();
                    Integer fastTotal = 0;
                    Integer fastFreeTotal = 0;
                    Integer slowTotal = 0;
                    Integer slowFreeTotal = 0;
                    for (OpLocationEvseElasticDTO esEVSE : esEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower.equals(power)) {
                            String state = esEVSE.getState();
                            //快充
                            if (evsePower >= 22D) {
                                fastTotal++;
                                if (Objects.equals(state, EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                    fastFreeTotal++;
                                }
                            } else {
                                //慢充
                                slowTotal++;
                                if (Objects.equals(state, EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                    slowFreeTotal++;
                                }
                            }
                        }
                        fullCharge.setPower(power);
                        fullCharge.setFreeTotal(fastFreeTotal);
                        fullCharge.setTotal(fastTotal);
                        slowCharge.setPower(power);
                        slowCharge.setFreeTotal(slowFreeTotal);
                        slowCharge.setTotal(slowTotal);
                    }
                    if (fullCharge.getTotal() > 0) {
                        fullChargeList.add(fullCharge);
                    }
                    if (slowCharge.getTotal() > 0) {
                        slowChargeList.add(slowCharge);
                    }
                }
            }
            stationCardPowerCountVO.setFullCharge(fullChargeList);
            stationCardPowerCountVO.setSlowCharge(slowChargeList);
            log.info("场站不同功率枪数量：{}", JSON.toJSONString(stationCardPowerCountVO));
            opLocationCardVO.setPileMap(stationCardPowerCountVO);
            //不同功率和枪类型双维度统计
            List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = buildGunPowerAndTypeGroupVOList(esEVSEList);
            opLocationCardVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOList);
            try {
                //场站所在时区的时间
                //涉及时区的计算要使用ZoneId不然会有夏令时问题
                String timeZone = StringUtils.isBlank(opLocationElasticDTO.getZoneId()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getZoneId();
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
                opLocationCardVO.setWeekDay(weekDay);
                opLocationCardVO.setHour(hour);
                opLocationCardVO.setMinute(minute);
                String nowTimeStr = hour + ":" + minute;
                DateFormat dateFormat = new SimpleDateFormat("HH:mm");
                Date nowTime = dateFormat.parse(nowTimeStr);
                log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
                log.info(BaseConstant.NOW_TIME, nowTime);
                //进场信息查询
                try {
                    RuleRelateForAppDTO ruleRelateForAppDTO = new RuleRelateForAppDTO();
                    ruleRelateForAppDTO.setUserId(UserUtil.getUserId());
                    ruleRelateForAppDTO.setSellerId(opLocationElasticDTO.getOperatorId());
                    ruleRelateForAppDTO.setLocationId(opLocationElasticDTO.getId());
                    ruleRelateForAppDTO.setZoneId(opLocationElasticDTO.getZoneId());
                    Result<RuleRelateForAppVO> ruleForAppResult = ruleRepository.getRuleForApp(ruleRelateForAppDTO);
                    if (ruleForAppResult != null && ruleForAppResult.getData() != null) {
                        RuleRelateForAppVO ruleRelateForAppVO = ruleForAppResult.getData();
                        opLocationCardVO.setOpen(ruleRelateForAppVO.getIsOpen());
                        opLocationCardVO.setNoAdmittance(ruleRelateForAppVO.getIsLimit());
                        opLocationCardVO.setChangingTime(ruleRelateForAppVO.getChangingTime());
                        opLocationCardVO.setChangingWeekDay(ruleRelateForAppVO.getDay());
                    }
                } catch (Exception e) {
                    log.info("进场信息查询失败：" , e);
                }
                //价格查询与最低价格解析
                BigDecimal unitPrice = null;
                BigDecimal timePrice = null;
                BigDecimal parkingPrice = null;
                BigDecimal startPrice = null;
                BigDecimal idlePrice = null;
                BigDecimal costPrice = null;
                String timeUnit = "";
                boolean haveUnitPrice = false;
                boolean haveTimePrice = false;
                List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = opLocationEvseElastic.findAllByLocationId(stationId);
                if (CollectionUtils.isNotEmpty(opLocationEvseElasticDTOS)) {
                    //查询计费规则
                    Set<Long> tariffIdSet = opLocationEvseElasticDTOS.stream().map(OpLocationEvseElasticDTO::getTariffId).filter(Objects::nonNull).collect(Collectors.toSet());
                    if (CollectionUtils.isNotEmpty(tariffIdSet)) {
                        List<CostModelRuleDTO> costModelRuleList = getCostModelRuleByTariffIdSet(tariffIdSet);
                        if (CollectionUtils.isNotEmpty(costModelRuleList)) {
                            opLocationCardVO.setCostModelRuleList(costModelRuleList);
                            //货币符号
                            opLocationCardVO.setCurrencySign(costModelRuleList.get(0).getCurrencySign());
                            try {
                                initPrice(opLocationCardVO, costModelRuleList, weekDay, unitPrice, timePrice, parkingPrice, startPrice, idlePrice, costPrice, timeUnit, haveUnitPrice, haveTimePrice);
                            } catch (Exception e) {
                                log.info("初始化价格信息失败:", e);
                            }
                        }
                    }
                }
            } catch (ParseException e) {
                log.info("opLocationCardVO：", e);
            }
            //周边设施
            List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationId(stationId);
            log.info(BaseConstant.LOCATION_FACILITY_DTOLIST, JSON.toJSONString(locationFacilityDTOList));
            if (CollUtil.isEmpty(locationFacilityDTOList)) {
                locationFacilityDTOList = Lists.newArrayList();
            }
            opLocationCardVO.setFacilityList(locationFacilityDTOList);

            //autel桩开启互联互通，不受进场控制影响下，不显示限制进入
            if (null != opLocationElasticDTO.getPlatform()
                    && opLocationElasticDTO.getPlatform() != 1
                    && opLocationElasticDTO.getHubjectCheck() != null
                    && opLocationElasticDTO.getHubjectCheck()) {
                opLocationCardVO.setThirdPile(1);
                opLocationCardVO.setAccessibility(conditionsValue2Accessibility(opLocationElasticDTO.getConditionsValue()));
            }
            return opLocationCardVO;
        }

        if (Boolean.parseBoolean(hubjectEnable) && optionalOpLocationElasticDTO.isPresent()
                && optionalOpLocationElasticDTO.get().getPlatform() == 2) {
            // 如果在es查询不到场站，在hubject查询
            hubjectCardData(opLocationCardDTO, opLocationCardVO, optionalOpLocationElasticDTO.get());
        }
        if (opLocationCardVO.getStationId() == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        return opLocationCardVO;
    }

    private static LocationDTO calculateTopLeft(double latitude, double longitude, double distance) {
        // 纬度变化量
        double deltaLat = distance / EARTH_RADIUS;
        // 经度变化量
        double deltaLon = distance / (EARTH_RADIUS * Math.cos(Math.PI * latitude / 180));

        double topLeftLat = latitude + rad2deg(deltaLat);
        double topLeftLon = longitude - rad2deg(deltaLon);

        LocationDTO locationDTO = new LocationDTO();
        locationDTO.setLat(topLeftLat);
        locationDTO.setLon(topLeftLon);
        return locationDTO;
    }

    private static LocationDTO calculateBottomRight(double latitude, double longitude, double distance) {
        // 纬度变化量
        double deltaLat = distance / EARTH_RADIUS;
        // 经度变化量
        double deltaLon = distance / (EARTH_RADIUS * Math.cos(Math.PI * latitude / 180));

        double bottomRightLat = latitude - rad2deg(deltaLat);
        double bottomRightLon = longitude + rad2deg(deltaLon);

        LocationDTO locationDTO = new LocationDTO();
        locationDTO.setLat(bottomRightLat);
        locationDTO.setLon(bottomRightLon);
        return locationDTO;
    }

    private static double rad2deg(double rad) {
        return rad * 180.0 / Math.PI;
    }

    /**
     * APP场站推荐
     *
     * @param dto 检索对象
     * @return 推荐场站列表
     */
    @Override
    public List<OpLocationCardVO> recommend(OpLocationMapQueryDTO dto) {
        log.info("场站推荐参数:{}", JSONObject.toJSONString(dto));
        List<OpLocationCardVO> cardDataList = Lists.newArrayList();
        String locationField = "location";
        StopWatch stopWatch = new StopWatch("APP地图场站推荐");
        stopWatch.start("场站搜索");
        BoolQueryBuilder boolSearch = QueryBuilders.boolQuery();
        //按功率查询
        if (dto.getHighPower() != null || dto.getLowPower() != null) {
            RangeQueryBuilder rangeSearch = QueryBuilders.rangeQuery("power");
            if (dto.getHighPower() != null) {
                rangeSearch.lte(dto.getHighPower());
            }
            if (dto.getLowPower() != null) {
                rangeSearch.gte(dto.getLowPower());
            }
            boolSearch.must(rangeSearch);
        }
        //按枪类型查询
        if (dto.getStatus() != null || CollectionUtils.isNotEmpty(dto.getGunTypes())) {
            if (dto.getStatus() != null && dto.getStatus() == 1) {
                boolSearch.must(QueryBuilders.termQuery("gunState", EvseDeviceStatusEnum.AVAILABLE.getName()));
            }
            if (CollectionUtils.isNotEmpty(dto.getGunTypes())) {
                boolSearch.must(QueryBuilders.termsQuery("gunType", dto.getGunTypes()));
            }
        }
        //计费规则id不为空
        boolSearch.must(QueryBuilders.existsQuery("tariffId"));

        //手动给一矩形范围，避免查询大量数据
        if (dto.getBottomRight() == null || dto.getTopLeft() == null) {
            if (dto.getLatitude() == null || dto.getLongitude() == null){
                return Lists.newArrayList();
            }
            double distance = 3.0; // 距离，单位：公里
            LocationDTO topLeftLocationDTO = calculateTopLeft(dto.getLatitude(), dto.getLongitude(), distance);
            LocationDTO bottomRightLocationDTO = calculateBottomRight(dto.getLatitude(), dto.getLongitude(), distance);
            dto.setTopLeft(topLeftLocationDTO);
            dto.setBottomRight(bottomRightLocationDTO);
        }
        //矩形条件-推荐范围
        if (dto.getBottomRight() != null && dto.getTopLeft() != null) {
            double top = dto.getTopLeft().getLat();
            double left = dto.getTopLeft().getLon();
            double bottom = dto.getBottomRight().getLat();
            double right = dto.getBottomRight().getLon();
            //推荐范围是上下左右离屏幕边缘1/10以内的范围
            double levelRange = (right - left) / 10;
            double verticalRange = (top - bottom) / 10;
            top = top - verticalRange;
            right = right - levelRange;
            bottom = bottom + verticalRange;
            left = left + levelRange;
            GeoBoundingBoxQueryBuilder geoBoundingBoxQueryBuilder = QueryBuilders.geoBoundingBoxQuery(locationField);
            geoBoundingBoxQueryBuilder.setCorners(top, left, bottom, right);
            ConstantScoreQueryBuilder constantScoreQueryBuilder = QueryBuilders.constantScoreQuery(geoBoundingBoxQueryBuilder);
            boolSearch.must(constantScoreQueryBuilder);
        }
        //appShow
        boolSearch.must(QueryBuilders.termsQuery("appShow", true));
        //platform
        if (!oicpFeign.checkUserIsGray()) {
            boolSearch.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        //按是否支持预约查询
        if (dto.getReservationEnabled() != null && dto.getReservationEnabled()) {
            boolSearch.must(QueryBuilders.termQuery("reservationEnabled", true));
        }
        //需要过滤掉订阅过期了的桩
        if (subscribeEnable) {
            // boolSearch.must(QueryBuilders.termQuery("subscriptionCheck", true));
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            // 8 planetev
            if ("8".equalsIgnoreCase(appId)) {
                List<Long> sellerIdList = getSellerIdList("1");
                boolSearch.must(QueryBuilders.termsQuery("operatorId", sellerIdList));
            } else {
                boolSearch.must(QueryBuilders.termQuery("subscriptionCheck", true));
            }
        }
        //加上planetEV-emsp条件
        appAddEMSPCondition(boolSearch);
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(boolSearch)
                .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"}))
                .withPageable(PageRequest.of(0, 1000))
                .build();
        //默认滚动查询时间一分钟，每次查10条
        Iterator<OpLocationEvseExpandElasticDTO> evseExpandElasticDTOIteratorTemp = elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).iterator();
        List<Long> locationIds = Lists.newArrayList();
        while (evseExpandElasticDTOIteratorTemp.hasNext()) {
            OpLocationEvseExpandElasticDTO next = evseExpandElasticDTOIteratorTemp.next();
            Long locationId = next.getLocationId();
            if (!locationIds.contains(locationId)) {
                locationIds.add(locationId);
            }
        }

        //查询场站所有设备扩展(不经过筛选)
        Iterator<OpLocationEvseExpandElasticDTO> evseExpandElasticDTOIterator = null;
        if (CollectionUtils.isNotEmpty(locationIds)) {
            BoolQueryBuilder boolSearch2 = QueryBuilders.boolQuery();
            boolSearch2.must(QueryBuilders.termsQuery("locationId", locationIds));
            //计费规则不为空
            boolSearch2.must(QueryBuilders.existsQuery("tariffId"));
            //需要过滤掉订阅过期了的桩
            if (subscribeEnable) {
                //boolSearch2.must(QueryBuilders.termQuery("subscriptionCheck", true));
                HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
                String appId = request.getHeader("X-AppId");
                // 8 planetev
                if ("8".equalsIgnoreCase(appId)) {
                    List<Long> sellerIdList = getSellerIdList("1");
                    boolSearch.must(QueryBuilders.termsQuery("operatorId", sellerIdList));
                } else {
                    boolSearch.must(QueryBuilders.termQuery("subscriptionCheck", true));
                }
            }
            NativeSearchQuery searchQuery2 = new NativeSearchQueryBuilder()
                    .withQuery(boolSearch2)
                    .withPageable(PageRequest.of(0, 1000))
                    .build();
            evseExpandElasticDTOIterator = elasticsearchRestTemplate.searchForStream(searchQuery2, OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).iterator();
        }

        //封装每个场站的所有设备(不经过筛选)
        Map<Long, List<OpLocationEvseExpandElasticDTO>> stationMap = new HashMap<>();
        if (evseExpandElasticDTOIterator != null) {
            while (evseExpandElasticDTOIterator.hasNext()) {
                OpLocationEvseExpandElasticDTO next = evseExpandElasticDTOIterator.next();
                Long locationId = next.getLocationId();
                List<OpLocationEvseExpandElasticDTO> evseExpandList = stationMap.get(locationId);
                if (evseExpandList == null) {
                    evseExpandList = new ArrayList<>();
                }
                evseExpandList.add(next);
                stationMap.put(locationId, evseExpandList);
            }
        }

        if (!stationMap.isEmpty()) {
            Map<Long, Integer> locationsStatusMap = this.getLocationsStatus(locationIds);
            Map<Long, String> stationLogoMap = this.getStationLogoMap(locationIds);
            Map<Long, List<OpLocationFacilityDTO>> stationFacilityMap = this.getStationFacilityMap(locationIds);
            Map<Long, OpLocationElasticDTO> opLocationElasticDTOMap = this.getOpLocationElasticDTOList(locationIds);
            //查询计费规则
            Set<Long> tariffIdSet = new HashSet<>();
            for (List<OpLocationEvseExpandElasticDTO> OpLocationEvseExpandElasticList : stationMap.values()) {
                Set<Long> tariffIds = OpLocationEvseExpandElasticList.stream().map(OpLocationEvseExpandElasticDTO::getTariffId).filter(Objects::nonNull).collect(Collectors.toSet());
                tariffIdSet.addAll(tariffIds);
            }
            Map<Long, CostModelRuleDTO> tariffMap = getTariffMap(tariffIdSet);
            stationMap.forEach((stationId, esEvseExpandList) -> {
                OpLocationCardVO opLocationCardVO = buildCardVO(stationId, esEvseExpandList, dto.getLatitude(), dto.getLongitude(), locationsStatusMap, stationLogoMap, stationFacilityMap.get(stationId), opLocationElasticDTOMap.get(stationId), tariffMap);
                cardDataList.add(opLocationCardVO);
            });
            //排序
            Collections.sort(cardDataList);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return cardDataList;
    }


    //APP加上planetEV-emsp条件,条件查询
    private void appAddEMSPCondition(BoolQueryBuilder boolQueryBuilder){
        try {
            //获取正在与EMSP_PLANETEV平台合作的CPO商户id
            List<String> cpoSellerIdList = Lists.newArrayList();
            SellerParamDTO sellerParamDTO = new SellerParamDTO();
            sellerParamDTO.setAppId(SellerPlatformEnum.CSMS.getCode());
            sellerParamDTO.setAssociatedPlatform(EmspPlatformEnum.PLANET_EV.getCode());
            sellerParamDTO.setCollabStatus(CollabStatusEnum.COOPERATION.getType());
            Result<List<SellerDetailVO>> cpoSellerResult = pileUserFeign.findEmspList(sellerParamDTO);
            log.info("===>>>appAddEMSPCondition cpoSellerResult:{}",JSONObject.toJSONString(cpoSellerResult));
            if (cpoSellerResult != null && CollectionUtils.isNotEmpty(cpoSellerResult.getData())){
                cpoSellerIdList = cpoSellerResult.getData().stream().map(SellerDetailVO::getId).collect(Collectors.toList());
            }

            HttpServletRequest httpServletRequest = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String requestAppId = httpServletRequest.getHeader(X_APPID);
            if (StringUtils.isNotBlank(requestAppId) && String.valueOf(AppIdEnum.PLANET_EV.getValue()).equalsIgnoreCase(requestAppId)){
                boolQueryBuilder.must(QueryBuilders.termsQuery("operatorId", cpoSellerIdList));
            }else {
                boolQueryBuilder.mustNot(QueryBuilders.termsQuery("operatorId", cpoSellerIdList));
            }
        } catch (Exception e) {
            log.info("====加上planetEV-emsp条件异常:",e);
        }
    }

    //APP加上planetEV-emsp条件,场站详情/桩详情
    @Override
    public void appAddEMSPCondition(String operatorId,PileBaseEnum pileBaseEnum){
        log.info("===>>>appAddEMSPCondition operatorId:{}",operatorId);
        //获取正在与EMSP_PLANETEV平台合作的CPO商户id
        List<String> cpoSellerIdList = Lists.newArrayList();
        SellerParamDTO sellerParamDTO = new SellerParamDTO();
        sellerParamDTO.setAppId(SellerPlatformEnum.CSMS.getCode());
        sellerParamDTO.setAssociatedPlatform(EmspPlatformEnum.PLANET_EV.getCode());
        sellerParamDTO.setCollabStatus(CollabStatusEnum.COOPERATION.getType());
        Result<List<SellerDetailVO>> cpoSellerResult = pileUserFeign.findEmspList(sellerParamDTO);
        log.info("===>>>appAddEMSPCondition cpoSellerResult:{}",JSONObject.toJSONString(cpoSellerResult));
        if (cpoSellerResult != null && CollectionUtils.isNotEmpty(cpoSellerResult.getData())){
            cpoSellerIdList = cpoSellerResult.getData().stream().map(SellerDetailVO::getId).collect(Collectors.toList());
        }
        //获取请求头的app类型
        HttpServletRequest httpServletRequest = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String requestAppId = httpServletRequest.getHeader(X_APPID);
        //如果是planetEV APP,那么该场站所属运营商id需要与emsp平台合作
        if (StringUtils.isNotBlank(requestAppId) && String.valueOf(AppIdEnum.PLANET_EV.getValue()).equalsIgnoreCase(requestAppId)){
            if (!cpoSellerIdList.contains(operatorId)){
                throw new MessageCodeException(pileBaseEnum);
            }
        }else {
            //如果是autel app,那么不能与emsp合作
            if (cpoSellerIdList.contains(operatorId)){
                throw new MessageCodeException(pileBaseEnum);
            }
        }
    }

    //运营平台查询场站加上planetEV-emsp条件:启动中、已停止的CPO商户id
    private void operationPlatformAddEMSPCondition(BoolQueryBuilder boolQueryBuilder,Integer platForm){
        try {
            log.info("===>>>OpLocationRepositoryImpl.operationPlatformAddEMSPCondition platForm:{}",platForm);
            SellerParamDTO sellerParamDTO = new SellerParamDTO();
            sellerParamDTO.setAppId(SellerPlatformEnum.CSMS.getCode());
            sellerParamDTO.setAssociatedPlatform(platForm);
            sellerParamDTO.setCollabStatus(CollabStatusEnum.COOPERATION.getType());
            sellerParamDTO.setCollabStatusList(Lists.newArrayList(CollabStatusEnum.COOPERATION.getType(),CollabStatusEnum.STOP_COOPERATION.getType()));
            Result<List<SellerDetailVO>> cpoSellerResult = pileUserFeign.findEmspList(sellerParamDTO);
            log.info("===>>>operationPlatformAddEMSPCondition cpoSellerResult:{}",JSONObject.toJSONString(cpoSellerResult));
            if (cpoSellerResult != null && cpoSellerResult.getData() != null){
                List<String> cpoSellerIdList = cpoSellerResult.getData().stream().map(SellerDetailVO::getId).collect(Collectors.toList());
                boolQueryBuilder.must(QueryBuilders.termsQuery("operatorId", cpoSellerIdList));
            }
        } catch (Exception e) {
            log.info("====operationPlatformAddEMSPCondition加上planetEV-emsp条件异常:",e);
        }
    }


    /**
     * 地图推荐封装成卡片对象
     *
     * @param stationId          场站id
     * @param esEVSEList         枪设备扩展集合
     * @param latitude           用户所在位置纬度
     * @param longitude          用户所在位置精度
     * @param locationsStatusMap 场站状态map 1-商桩 2-家桩
     * @return 卡片对象
     */
    private OpLocationCardVO buildCardVO(Long stationId, List<OpLocationEvseExpandElasticDTO> esEVSEList, Double latitude, Double longitude,
                                         Map<Long, Integer> locationsStatusMap,
                                         Map<Long, String> stationLogoMap,
                                         List<OpLocationFacilityDTO> locationFacilityDTOList,
                                         OpLocationElasticDTO opLocationElasticDTO,
                                         Map<Long, CostModelRuleDTO> tariffMap) {
        OpLocationCardVO opLocationCardVO = new OpLocationCardVO();
        OpLocationEvseExpandElasticDTO opLocationEvseExpandElasticDTO = esEVSEList.get(0);
        opLocationCardVO.setStationId(stationId);
        opLocationCardVO.setName(opLocationEvseExpandElasticDTO.getName());
        opLocationCardVO.setUserSellerId(opLocationEvseExpandElasticDTO.getOperatorId());
        opLocationCardVO.setStatus(opLocationEvseExpandElasticDTO.getStatus());
        if (LoginUserHolder.getLoginUser().getId().equals(opLocationEvseExpandElasticDTO.getOperatorId())) {
            opLocationCardVO.setStatus(EVSETypeEnum.HOME_PILE.getCode());
        }
        //私桩共享场站，桩主跟家庭成员一样的icon标识
        if (opLocationCardVO.getStatus() != null && opLocationCardVO.getStatus().equals(EVSETypeEnum.SHARE_PILE.getCode())) {
            if (!org.springframework.util.CollectionUtils.isEmpty(locationsStatusMap) && locationsStatusMap.get(stationId) != null) {
                opLocationCardVO.setStatus(locationsStatusMap.get(stationId));
            }
        }
        //国家简码转换
        String country = changeCountry(opLocationEvseExpandElasticDTO.getCountry());
        opLocationCardVO.setCountry(country);
        opLocationCardVO.setCity(opLocationEvseExpandElasticDTO.getCity());
        String address = StringUtils.isBlank(opLocationEvseExpandElasticDTO.getAddress()) ? country + opLocationEvseExpandElasticDTO.getCity() : opLocationEvseExpandElasticDTO.getAddress();
        opLocationCardVO.setAddress(address);
        //距离
        double distance = 0;
        if (opLocationElasticDTO != null && opLocationElasticDTO.getLatitude() != null && opLocationElasticDTO.getLongitude() != null) {
            Double lat = opLocationElasticDTO.getLatitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLatitude());
            Double lon = opLocationElasticDTO.getLongitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLongitude());
            opLocationCardVO.setLatitude(lat);
            opLocationCardVO.setLongitude(lon);
            try {
                distance = GeoDistance.ARC.calculate(lat, lon, latitude, longitude, DistanceUnit.KILOMETERS);
            } catch (Exception e) {
                log.info("距离计算异常:" + e);
            }
        }
        opLocationCardVO.setDistance(distance);
        //场站logo
        if (StringUtils.isNotBlank(stationLogoMap.get(stationId))) {
            opLocationCardVO.setLogoPath(stationLogoMap.get(stationId));
        }
        //查询es枪设备
        esEVSEList = esEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
        log.info("站点:{}，es枪设备：{}", stationId, esEVSEList.size());
        //场站状态
        Integer locationStatusCode = buildLocationStatus2(esEVSEList);
        opLocationCardVO.setLocationStatusCode(locationStatusCode);
        opLocationCardVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
        //超充、快充、慢充数量
        Integer veryFastTotalNum = 0;
        Integer veryFastNum = 0;
        Integer fastTotalNum = 0;
        Integer fastNum = 0;
        Integer slowTotalNum = 0;
        Integer slowNum = 0;
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            Double veryFastPower = 240D;
            Double fastPower = 22D;

            for (OpLocationEvseExpandElasticDTO esEVSE : esEVSEList) {
                Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                if (evsePower >= veryFastPower) {
                    veryFastTotalNum++;
                    if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                        veryFastNum++;
                    }
                } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                    fastTotalNum++;
                    if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                        fastNum++;
                    }
                } else {
                    slowTotalNum++;
                    if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                        slowNum++;
                    }
                }
            }
        }
        opLocationCardVO.setVeryFastTotal(veryFastTotalNum);
        opLocationCardVO.setVeryFast(veryFastNum);
        opLocationCardVO.setFastTotal(fastTotalNum);
        opLocationCardVO.setFast(fastNum);
        opLocationCardVO.setSlowTotal(slowTotalNum);
        opLocationCardVO.setSlow(slowNum);
        //不同类型枪数统计
        List<GunTypeGroupVO> gunTypeGroupVOList = buildStationGunTypeGroupVOList3(esEVSEList);
        opLocationCardVO.setPileType(gunTypeGroupVOList);
        log.info("场站：{}-{}的枪类型统计：{}", opLocationEvseExpandElasticDTO.getName(), opLocationEvseExpandElasticDTO.getLocationId(), JSON.toJSONString(gunTypeGroupVOList));
        //根据设备封装功率范围集合
        Set<Double> stationPowerSet = new HashSet<>();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            for (OpLocationEvseExpandElasticDTO esEVSE : esEVSEList) {
                Double power = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                stationPowerSet.add(power);
            }
        }
        log.info("站点枪设备功率：{}", JSON.toJSONString(stationPowerSet));
        //不同功率枪数统计
        StationCardPowerCountVO stationCardPowerCountVO = new StationCardPowerCountVO();
        List<StationCardPowerCountDetailVO> fullChargeList = Lists.newArrayList();
        List<StationCardPowerCountDetailVO> slowChargeList = Lists.newArrayList();
        if (!stationPowerSet.isEmpty() && CollectionUtils.isNotEmpty(esEVSEList)) {
            for (Double power : stationPowerSet) {
                StationCardPowerCountDetailVO fullCharge = new StationCardPowerCountDetailVO();
                StationCardPowerCountDetailVO slowCharge = new StationCardPowerCountDetailVO();
                Integer fastTotal = 0;
                Integer fastFreeTotal = 0;
                Integer slowTotal = 0;
                Integer slowFreeTotal = 0;
                for (OpLocationEvseExpandElasticDTO esEVSE : esEVSEList) {
                    Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                    if (evsePower.equals(power)) {
                        String state = esEVSE.getGunState();
                        //快充
                        if (evsePower >= 22D) {
                            fastTotal++;
                            if (Objects.equals(state, EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fastFreeTotal++;
                            }
                        } else {
                            //慢充
                            slowTotal++;
                            if (Objects.equals(state, EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slowFreeTotal++;
                            }
                        }
                    }
                    fullCharge.setPower(power);
                    fullCharge.setFreeTotal(fastFreeTotal);
                    fullCharge.setTotal(fastTotal);
                    slowCharge.setPower(power);
                    slowCharge.setFreeTotal(slowFreeTotal);
                    slowCharge.setTotal(slowTotal);
                }
                if (fullCharge.getTotal() > 0) {
                    fullChargeList.add(fullCharge);
                }
                if (slowCharge.getTotal() > 0) {
                    slowChargeList.add(slowCharge);
                }
            }
        }
        stationCardPowerCountVO.setFullCharge(fullChargeList);
        stationCardPowerCountVO.setSlowCharge(slowChargeList);
        log.info("场站不同功率枪数量：{}", JSON.toJSONString(stationCardPowerCountVO));
        opLocationCardVO.setPileMap(stationCardPowerCountVO);
        //不同功率和枪类型双维度统计
        List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = buildGunPowerAndTypeGroupVOList2(esEVSEList);
        opLocationCardVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOList);
        try {
            //场站所在时区的时间
            //涉及时区的计算要使用ZoneId不然会有夏令时问题
            String timeZone = StringUtils.isBlank(opLocationEvseExpandElasticDTO.getZoneId()) ? BaseConstant.UTC_8 : opLocationEvseExpandElasticDTO.getZoneId();
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
            opLocationCardVO.setWeekDay(weekDay);
            opLocationCardVO.setHour(hour);
            opLocationCardVO.setMinute(minute);
            String nowTimeStr = hour + ":" + minute;
            DateFormat dateFormat = new SimpleDateFormat("HH:mm");
            Date nowTime = dateFormat.parse(nowTimeStr);
            log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
            log.info(BaseConstant.NOW_TIME, nowTime);
            //进场信息查询
            try {
                RuleRelateForAppDTO ruleRelateForAppDTO = new RuleRelateForAppDTO();
                ruleRelateForAppDTO.setUserId(UserUtil.getUserId());
                ruleRelateForAppDTO.setSellerId(opLocationEvseExpandElasticDTO.getOperatorId());
                ruleRelateForAppDTO.setLocationId(opLocationEvseExpandElasticDTO.getLocationId());
                ruleRelateForAppDTO.setZoneId(opLocationEvseExpandElasticDTO.getZoneId());
                Result<RuleRelateForAppVO> ruleForAppResult = ruleRepository.getRuleForApp(ruleRelateForAppDTO);
                log.info("进场信息：{}", JSON.toJSONString(ruleForAppResult));
                if (ruleForAppResult != null && ruleForAppResult.getData() != null) {
                    RuleRelateForAppVO ruleRelateForAppVO = ruleForAppResult.getData();
                    opLocationCardVO.setOpen(ruleRelateForAppVO.getIsOpen());
                    opLocationCardVO.setNoAdmittance(ruleRelateForAppVO.getIsLimit());
                    opLocationCardVO.setChangingTime(ruleRelateForAppVO.getChangingTime());
                    opLocationCardVO.setChangingWeekDay(ruleRelateForAppVO.getDay());
                }
            } catch (Exception e) {
                log.info("进场信息查询失败：" , e);
            }
            //价格查询与最低价格解析
            BigDecimal unitPrice = null;
            BigDecimal timePrice = null;
            BigDecimal parkingPrice = null;
            BigDecimal startPrice = null;
            BigDecimal idlePrice = null;
            BigDecimal costPrice = null;
            String timeUnit = "";
            boolean haveUnitPrice = false;
            boolean haveTimePrice = false;
//            List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findAllByLocationId(stationId);
            if (CollectionUtils.isNotEmpty(esEVSEList)) {
                //查询计费规则
                Set<Long> tariffIdSet = esEVSEList.stream().map(OpLocationEvseExpandElasticDTO::getTariffId).filter(Objects::nonNull).collect(Collectors.toSet());
                if (CollectionUtils.isNotEmpty(tariffIdSet)) {
//                    List<CostModelRuleDTO> costModelRuleList = getCostModelRuleByTariffIdSet(tariffIdSet);
                    List<CostModelRuleDTO> costModelRuleList = Lists.newArrayList();
                    for (Long tariffId : tariffIdSet) {
                        CostModelRuleDTO costModelRuleDTO = tariffMap.get(tariffId);
                        if (costModelRuleDTO != null) {
                            costModelRuleList.add(costModelRuleDTO);
                        }
                    }
                    if (CollectionUtils.isNotEmpty(costModelRuleList)) {
                        opLocationCardVO.setCostModelRuleList(costModelRuleList);
                        //货币符号
                        opLocationCardVO.setCurrencySign(costModelRuleList.get(0).getCurrencySign());
                        try {
                            initPrice(opLocationCardVO, costModelRuleList, weekDay, unitPrice, timePrice, parkingPrice, startPrice, idlePrice, costPrice, timeUnit, haveUnitPrice, haveTimePrice);
                        } catch (Exception e) {
                            log.info("初始化价格信息失败:" + e);
                        }
                    }
                }
            }
        } catch (ParseException e) {
            log.info("opLocationCardVO：", e);
        }
        //周边设施
        log.info(BaseConstant.LOCATION_FACILITY_DTOLIST, JSON.toJSONString(locationFacilityDTOList));
        if (CollUtil.isEmpty(locationFacilityDTOList)) {
            locationFacilityDTOList = Lists.newArrayList();
        }
        opLocationCardVO.setFacilityList(locationFacilityDTOList);

        //autel桩开启互联互通，不受进场控制影响下，不显示限制进入
        if (ObjectUtils.isEmpty(opLocationEvseExpandElasticDTO)
                && null != opLocationEvseExpandElasticDTO.getPlatform()
                && opLocationEvseExpandElasticDTO.getPlatform() != 1
                && opLocationEvseExpandElasticDTO.getHubjectCheck() != null
                && opLocationEvseExpandElasticDTO.getHubjectCheck()) {
            opLocationCardVO.setThirdPile(1);
            opLocationCardVO.setAccessibility(conditionsValue2Accessibility(opLocationEvseExpandElasticDTO.getConditionsValue()));
        }
        return opLocationCardVO;
    }

    /**
     * 构建站点不同类型枪数统计
     *
     * @param opLocationESEVSEList 站点设备
     * @return 站点不同类型枪数统计
     */
    private List<GunTypeGroupVO> buildStationGunTypeGroupVOList3(List<OpLocationEvseExpandElasticDTO> opLocationESEVSEList) {
        List<GunTypeGroupVO> gunTypeGroupVOList = Lists.newArrayList();
        if (CollectionUtils.isEmpty(opLocationESEVSEList)) {
            return gunTypeGroupVOList;
        }
        Map<Integer, Integer> gunTypeTotalEVSENumMap = new HashedMap();
        Map<Integer, Integer> gunTypeFreeEVSENumMap = new HashedMap();
        for (OpLocationEvseExpandElasticDTO esEVSE : opLocationESEVSEList) {
            Integer gunType = esEVSE.getGunType();
            if (gunType == null) {
                gunType = 1;
            }
            Integer gunTypeTotalEVSENum = gunTypeTotalEVSENumMap.get(gunType);
            if (gunTypeTotalEVSENum == null) {
                gunTypeTotalEVSENum = 0;
            }
            gunTypeTotalEVSENum++;
            gunTypeTotalEVSENumMap.put(gunType, gunTypeTotalEVSENum);
            if (StringUtils.isNotBlank(esEVSE.getGunState()) && Objects.equals(esEVSE.getGunState(), String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                Integer gunTypeFreeEVSENum = gunTypeFreeEVSENumMap.get(gunType);
                if (gunTypeFreeEVSENum == null) {
                    gunTypeFreeEVSENum = 0;
                }
                gunTypeFreeEVSENum++;
                gunTypeFreeEVSENumMap.put(gunType, gunTypeFreeEVSENum);
            }
        }
        for (Map.Entry<Integer, Integer> entry : gunTypeTotalEVSENumMap.entrySet()) {
            Integer gunType = entry.getKey();
            Integer gunTypeTotalNum = entry.getValue();
            if (gunTypeTotalNum != 0) {
                GunTypeGroupVO gunTypeGroupVO = new GunTypeGroupVO();
                gunTypeGroupVO.setGunType(gunType);
                gunTypeGroupVO.setTotalCount(gunTypeTotalNum);
                gunTypeGroupVO.setCount(gunTypeTotalNum);
                Integer gunTypeFreeNum = gunTypeFreeEVSENumMap.get(gunType);
                gunTypeFreeNum = gunTypeFreeNum == null ? 0 : gunTypeFreeNum;
                gunTypeGroupVO.setFreeCount(gunTypeFreeNum);
                gunTypeGroupVOList.add(gunTypeGroupVO);
            }
        }
        return gunTypeGroupVOList;
    }

    /**
     * 统计功率/枪类型的枪数量
     *
     * @param esEVSEList 场站设备
     * @return 根据功率/枪类型统计枪数
     */
    private List<GunPowerAndTypeGroupVO> buildGunPowerAndTypeGroupVOList2(List<OpLocationEvseExpandElasticDTO> esEVSEList) {
        List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            //构建两个Map
            Map<String, Integer> totalCountMap = new HashMap<>();
            Map<String, Integer> freeCountMap = new HashMap<>();
            for (OpLocationEvseExpandElasticDTO esEVSE : esEVSEList) {
                Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                Integer evseGunTypeCode = esEVSE.getGunType() == null ? ConnectorGunTypeEnum.CCS_COMBO_2.getCode() : esEVSE.getGunType();
                String key = buildGunPowerAndTypeKey(evsePower, evseGunTypeCode);
                //总数量
                Integer totalCount = totalCountMap.get(key);
                if (totalCount == null) {
                    totalCount = 0;
                }
                totalCount++;
                totalCountMap.put(key, totalCount);
                //可用数量
                if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                    Integer freeCount = freeCountMap.get(key);
                    if (freeCount == null) {
                        freeCount = 0;
                    }
                    freeCount++;
                    freeCountMap.put(key, freeCount);
                }
            }
            //将map封装成对象
            for (Map.Entry<String, Integer> entry : totalCountMap.entrySet()) {
                String key = entry.getKey();
                String[] powerAndGunTypeCodeArray = key.split("-");
                Double power = Double.valueOf(powerAndGunTypeCodeArray[0]);
                Integer gunTypeCode = Integer.valueOf(powerAndGunTypeCodeArray[1]);
                Integer totalCount = entry.getValue();
                Integer freeCount = 0;
                if (freeCountMap.get(key) != null) {
                    freeCount = freeCountMap.get(key);
                }
                GunPowerAndTypeGroupVO gunPowerAndTypeGroupVO = new GunPowerAndTypeGroupVO();
                gunPowerAndTypeGroupVO.setPower(power);
                gunPowerAndTypeGroupVO.setGunTypeCode(gunTypeCode);
                gunPowerAndTypeGroupVO.setTotalCount(totalCount);
                gunPowerAndTypeGroupVO.setFreeCount(freeCount);
                gunPowerAndTypeGroupVOList.add(gunPowerAndTypeGroupVO);
            }
        }
        return gunPowerAndTypeGroupVOList;
    }

    private Map<Long, String> getStationLogoMap(List<Long> stationIds) {
        Map<Long, String> stationLogoMap = new HashMap<>();
        //查询db场站运营表
        QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
        opLocationOperationEntityQueryWrapper.in(BaseConstant.LOCATION_ID, stationIds);
        List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(opLocationOperationEntityQueryWrapper);
        if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
            List<Long> logoImageIdList = Lists.newArrayList();
            Map<Long, Long> imageId2StationMap = new HashMap<>();
            for (OpLocationOperationEntity opLocationOperationEntity : opLocationOperationEntities) {
                if (opLocationOperationEntity.getLogoImageId() != null) {
                    logoImageIdList.add(opLocationOperationEntity.getLogoImageId());
                    imageId2StationMap.put(opLocationOperationEntity.getLogoImageId(), opLocationOperationEntity.getLocationId());
                }
            }
            //查询image表
            if (CollectionUtils.isNotEmpty(logoImageIdList)) {
                List<OpImageEntity> opImageEntities = opImageMapper.selectBatchIds(logoImageIdList);
                if (CollectionUtils.isNotEmpty(opImageEntities)) {
                    for (OpImageEntity opImageEntity : opImageEntities) {
                        if (StringUtils.isNotBlank(opImageEntity.getUrl())) {
                            Long stationId = imageId2StationMap.get(opImageEntity.getId());
                            if (stationId != null) {
                                stationLogoMap.put(stationId, opImageEntity.getUrl());
                            }
                        }
                    }
                }
            }
        }
        return stationLogoMap;
    }

    private Map<Long, List<OpLocationFacilityDTO>> getStationFacilityMap(List<Long> stationIds) {
        Map<Long, List<OpLocationFacilityDTO>> stationFacilityMap = new HashMap<>();
        //查询周边设置
        List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationIdList(stationIds);
        if (CollectionUtils.isNotEmpty(locationFacilityDTOList)) {
            for (OpLocationFacilityDTO opLocationFacilityDTO : locationFacilityDTOList) {
                Long locationId = opLocationFacilityDTO.getLocationId();
                List<OpLocationFacilityDTO> opLocationFacilityDTOS = stationFacilityMap.get(locationId);
                if (opLocationFacilityDTOS == null) {
                    opLocationFacilityDTOS = new ArrayList<>();
                }
                opLocationFacilityDTOS.add(opLocationFacilityDTO);
                stationFacilityMap.put(locationId, opLocationFacilityDTOS);
            }
        }
        return stationFacilityMap;
    }

    private Map<Long, List<OpLocationEvseElasticDTO>> getStationEVSEListMap(List<Long> stationIds) {
        Map<Long, List<OpLocationEvseElasticDTO>> stationEVSEMap = new HashMap<>();
        //查询evse
        List<OpLocationEvseElasticDTO> evseElasticDTOList = opLocationEvseElastic.findAllByLocationIdIn(new HashSet<>(stationIds));
        if (CollectionUtils.isNotEmpty(evseElasticDTOList)) {
            for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : evseElasticDTOList) {
                Long locationId = opLocationEvseElasticDTO.getLocationId();
                List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = stationEVSEMap.get(locationId);
                if (opLocationEvseElasticDTOS == null) {
                    opLocationEvseElasticDTOS = new ArrayList<>();
                }
                opLocationEvseElasticDTOS.add(opLocationEvseElasticDTO);
                stationEVSEMap.put(locationId, opLocationEvseElasticDTOS);
            }
        }
        return stationEVSEMap;
    }

    private Map<Long, List<OpLocationPileEvseElasticDTO>> getStationPileEVSEListMap(List<OpLocationPileEvseElasticDTO> pileEvseElasticDTOList) {
        Map<Long, List<OpLocationPileEvseElasticDTO>> stationPileEVSEMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(pileEvseElasticDTOList)) {
            for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : pileEvseElasticDTOList) {
                Long locationId = opLocationPileEvseElasticDTO.getLocationId();
                List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = stationPileEVSEMap.get(locationId);
                if (opLocationPileEvseElasticDTOList == null) {
                    opLocationPileEvseElasticDTOList = new ArrayList<>();
                }
                opLocationPileEvseElasticDTOList.add(opLocationPileEvseElasticDTO);
                stationPileEVSEMap.put(locationId, opLocationPileEvseElasticDTOList);
            }
        }
        return stationPileEVSEMap;
    }

    /**
     * 私桩共享场站，桩主跟家庭成员一样的icon标识
     *
     * @param locationIds
     * @return key为场站ID，value为场站类型 1：商桩 2：家桩 3：家桩共享
     */
    private Map<Long, Integer> getLocationsStatus(List<Long> locationIds) {
        Map<Long, List<OpLocationPileEvseVO>> locationMap = null;
        Map<String, Long> joinMap = null;
        Map<Long, Integer> resultMap = new HashMap<>(locationIds.size());
        if (CollectionUtils.isNotEmpty(locationIds)) {
            //查询桩
            List<OpLocationPileEvseVO> dataList = opLocationPileEvseRepository.getListByLocationIds(locationIds);
            if (CollectionUtils.isNotEmpty(dataList)) {
                //桩按场站分组
                locationMap = dataList.stream().collect(Collectors.groupingBy(OpLocationPileEvseVO::getLocationId));
                //查询当前用户被家桩共享桩主
                List<UserSharePileListVO> shareUserList = homePileFeignClient.listAsFamilyUserId().getData();
                if (CollectionUtils.isNotEmpty(shareUserList)) {
                    joinMap = shareUserList.stream().collect(Collectors.toMap(UserSharePileListVO::getSn, UserSharePileListVO::getPileOwnerUserId, (f, s) -> f));
                }
            }
        }
        //如果当前场站下有桩已经共享出去，其家庭成员对该场站标识为桩主跟桩主一样的icon
        if (!org.springframework.util.CollectionUtils.isEmpty(locationMap) && !org.springframework.util.CollectionUtils.isEmpty(joinMap)) {
            Map<Long, List<OpLocationPileEvseVO>> finalLocationMap = locationMap;
            Map<String, Long> finalJoinMap = joinMap;
            //查询场站
            List<OpLocationElasticDTO> locationDtoList = opLocationElastic.findAllByIdIn(locationIds.stream().collect(Collectors.toSet()));
            locationDtoList.stream().forEach(vo -> {
                List<OpLocationPileEvseVO> voList = finalLocationMap.get(vo.getId());
                if (CollectionUtils.isNotEmpty(voList)) {
                    voList.stream().filter(pileVo -> finalJoinMap.get(pileVo.getPileSn()) != null && finalJoinMap.get(pileVo.getPileSn()).longValue() == vo.getOperatorId().longValue())
                            .findAny().ifPresent(value -> resultMap.put(vo.getId(), EVSETypeEnum.HOME_PILE.getCode()));
                }
            });
        }
        log.info("getLocationsStatus,resultMap={}", JSON.toJSONString(resultMap));
        return resultMap;
    }

    private Map<Long, OpLocationElasticDTO> getOpLocationElasticDTOList(List<Long> locationIds) {
        Map<Long, OpLocationElasticDTO> resultMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(locationIds)) {
            //查询
            BoolQueryBuilder boolSearch = QueryBuilders.boolQuery();
            boolSearch.must(QueryBuilders.termsQuery("id", new HashSet<>(locationIds)));
            NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                    .withQuery(boolSearch)
                    .withPageable(PageRequest.of(0, 1000))
                    .build();
            Iterator<OpLocationElasticDTO> opLocationElasticDTOIterator = elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationElasticDTO.class).stream().map(SearchHit::getContent).iterator();
            while (opLocationElasticDTOIterator.hasNext()) {
                OpLocationElasticDTO opLocationElasticDTO = opLocationElasticDTOIterator.next();
                resultMap.put(opLocationElasticDTO.getId(), opLocationElasticDTO);
            }
        }
        return resultMap;
    }

    private List<OpLocationPileEvseElasticDTO> getOpLocationPileEvseElasticDTOList(List<Long> locationIds) {
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(locationIds)) {
            //查询
            BoolQueryBuilder boolSearch = QueryBuilders.boolQuery();
            boolSearch.must(QueryBuilders.termsQuery("locationId", new HashSet<>(locationIds)));
            NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                    .withQuery(boolSearch)
                    .withPageable(PageRequest.of(0, 1000))
                    .build();
            Iterator<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOIterator = elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).iterator();
            while (opLocationPileEvseElasticDTOIterator.hasNext()) {
                OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticDTOIterator.next();
                opLocationPileEvseElasticDTOList.add(opLocationPileEvseElasticDTO);
            }
        }
        return opLocationPileEvseElasticDTOList;
    }

    private Map<Long, CostModelRuleDTO> getTariffMap(Set<Long> tariffIdSet) {
        Map<Long, CostModelRuleDTO> resultMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(tariffIdSet)) {
            List<CostModelRuleDTO> costModelRuleList = getCostModelRuleByTariffIdSet(tariffIdSet);
            if (CollectionUtils.isNotEmpty(costModelRuleList)) {
                costModelRuleList.forEach(x -> {
                    resultMap.put(x.getId(), x);
                });
            }
        }
        return resultMap;
    }

    /**
     * 三方桩激活文档下载
     *
     * @param response response
     * @return 返回对象
     */
    @Override
    public void download(HttpServletResponse response, String brandName) {
        if (StringUtils.isBlank(brandName)) {
            brandName = "Alfen";
        }
        if (brandName.equalsIgnoreCase("Alfen")) {
            wordUtil.createWord(response);
        } else if (brandName.equalsIgnoreCase("WallBox")) {
            //下载WallBox文档
            wordUtil.createWallBoxWord(response);
        }
    }

    private void hubjectCardData(OpLocationCardDTO opLocationCardDTO, OpLocationCardVO opLocationCardVO, OpLocationElasticDTO opLocationElasticDTO) {
        try {
            opLocationCardVO.setStationId(opLocationElasticDTO.getId());
            opLocationCardVO.setName(opLocationElasticDTO.getName());
            // hubject 设置为商桩
            opLocationCardVO.setStatus(EVSETypeEnum.BUSINESS_PILE.getCode());
            opLocationCardVO.setAddress(opLocationElasticDTO.getAddress());
            opLocationCardVO.setCountry(opLocationElasticDTO.getCountry());
            opLocationCardVO.setCity(opLocationElasticDTO.getCity());
            opLocationCardVO.setLatitude(Double.valueOf(opLocationElasticDTO.getLatitude()));
            opLocationCardVO.setLongitude(Double.valueOf(opLocationElasticDTO.getLongitude()));
            //距离
            this.setDistance(opLocationCardDTO, opLocationCardVO);
            JwtInfo loginUser = LoginUserHolder.getLoginUser();
            if (loginUser != null && loginUser.getId() != null) {
                Boolean member = redisTemplate.opsForSet().isMember(RedisKeyConstant.getSupportEroamingLocationSetKey(opLocationElasticDTO.getId()), loginUser.getId());
                opLocationCardVO.setLikeRoaming(member);
            }
            opLocationCardVO.setERoamingNum(opLocationElasticDTO.getERoamingNum());
            opLocationCardVO.setPlatform(opLocationElasticDTO.getPlatform());

            Result<List<PullEvseDataRespDTO>> hubjectEvseData = oicpFeign.queryChargingPointData(opLocationElasticDTO.getId());
            log.info("hubject--查询hubject场站信息, {}:{}", opLocationElasticDTO.getId(), JSON.toJSONString(hubjectEvseData));
            if (hubjectEvseData != null && hubjectEvseData.getCode() == 200 && CollectionUtils.isNotEmpty(hubjectEvseData.getData())) {
                List<PullEvseDataRespDTO> pullEvseDataRespDTOList = hubjectEvseData.getData();
                PullEvseDataRespDTO pullEvseDataRespDTO = pullEvseDataRespDTOList.get(0);

                opLocationCardVO.setLogoPath(pullEvseDataRespDTO.getChargingStationImage());

                //超充、快充、慢充数量
                int veryFastTotalNum_hub = 0;
                int veryFastNum_hub = 0;
                int fastTotalNum_hub = 0;
                int fastNum_hub = 0;
                int slowTotalNum_hub = 0;
                int slowNum_hub = 0;
                Integer freeGunNum = 0;
                // 查询桩状态

                double veryFastPower = 240D;
                double fastPower = 22D;
                List<EvseStatusRecordDTO> collect = pullEvseDataRespDTOList.stream()
                        .map(item -> EvseStatusRecordDTO.builder()
                                .evseId(item.getEvseId())
                                .evseStatus(item.getHubStatus())
                                .build())
                        .collect(Collectors.toList());

                //场站状态
                Integer locationStatusCode = buildHubjectLocationStatus(collect);
                opLocationCardVO.setLocationStatusCode(locationStatusCode);
                opLocationCardVO.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));

                Map<String, List<EvseStatusRecordDTO>> evseStatusListMap = collect.stream()
                        .collect(Collectors.groupingBy(EvseStatusRecordDTO::getEvseId));

                for (PullEvseDataRespDTO esEVSE : pullEvseDataRespDTOList) {

                    Integer evseStatus = evseStatusListMap.get(esEVSE.getEvseId()).get(0).getEvseStatus();

                    if (CollectionUtils.isEmpty(esEVSE.getChargingFacilities())) {
                        slowTotalNum_hub++;
                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode() + 1)) {
                            slowNum_hub++;
                        }
                        continue;
                    }
                    ChargingFacilityDTO chargingFacilityDTO = esEVSE.getChargingFacilities().get(0);
                    Double evsePower = chargingFacilityDTO.getPower() == null ? 0D : chargingFacilityDTO.getPower();
                    if (evsePower >= veryFastPower) {
                        veryFastTotalNum_hub++;
                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                            veryFastNum_hub++;
                        }
                    } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                        fastTotalNum_hub++;
                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                            fastNum_hub++;
                        }
                    } else {
                        slowTotalNum_hub++;
                        if (Objects.equals(evseStatus, EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                            slowNum_hub++;
                        }
                    }
                }
                //枪数量
                opLocationCardVO.setVeryFastTotal(veryFastTotalNum_hub);
                opLocationCardVO.setVeryFast(veryFastNum_hub);
                opLocationCardVO.setFastTotal(fastTotalNum_hub);
                opLocationCardVO.setFast(fastNum_hub);
                opLocationCardVO.setSlowTotal(slowTotalNum_hub);
                opLocationCardVO.setSlow(slowNum_hub);

                //不同枪类型统计 根据设备封装功率范围集合 不同功率枪数统计
                setCardPileTypeMethod(pullEvseDataRespDTOList, opLocationCardVO);

                try {
                    //场站所在时区的时间
                    String timeZone = StringUtils.isBlank(opLocationElasticDTO.getTimeZone()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getTimeZone();
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
                    opLocationCardVO.setWeekDay(weekDay);
                    opLocationCardVO.setHour(hour);
                    opLocationCardVO.setMinute(minute);
                    String nowTimeStr = hour + ":" + minute;
                    DateFormat dateFormat = new SimpleDateFormat("HH:mm");
                    Date nowTime = dateFormat.parse(nowTimeStr);
                    log.info("hubject---nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}",
                            nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
                    log.info("hubject---当前时间：{}", nowTime);

                    //场站是否开放（休息中/营业）
                    if (null != pullEvseDataRespDTO.getOpen24Hours() && pullEvseDataRespDTO.getOpen24Hours()) {
                        opLocationCardVO.setOpen(true);
                    } else {
                        if (CollectionUtils.isNotEmpty(pullEvseDataRespDTO.getOpeningTimes())) {
                            List<EvseDataRecordDTO.OpeningTimes> openingTimes = pullEvseDataRespDTO.getOpeningTimes();
                            openingTimes.forEach(time -> {
                                if (10 == time.getOn()) {
                                    boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                    if (b) {
                                        opLocationCardVO.setOpen(true);
                                    }
                                } else if (9 == time.getOn()) {
                                    if (6 == weekDay || 7 == weekDay) {
                                        boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                        if (b) {
                                            opLocationCardVO.setOpen(true);
                                        }
                                    }
                                } else if (8 == time.getOn()) {
                                    if (1 == weekDay || 2 == weekDay || 3 == weekDay || 4 == weekDay || 5 == weekDay) {
                                        boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                        if (b) {
                                            opLocationCardVO.setOpen(true);
                                        }
                                    }
                                } else if (weekDay == time.getOn()) {
                                    boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                    if (b) {
                                        opLocationCardVO.setOpen(true);
                                    }
                                }

                            });
                        }
                    }

                    //  进场信息查询

                    //价格查询与最低价格解析
                    BigDecimal unitPrice = null;
                    BigDecimal parkingPrice = null;
                    BigDecimal startPrice = null;
                    BigDecimal idlePrice = null;
                    BigDecimal costPrice = null;
                    BigDecimal timePrice = null;
                    String currency = null;
                    String currencySign = null;
                    String timeUnit = "";
                    boolean haveUnitPrice = false;
                    boolean haveTimePrice = false;
                    if (CollectionUtils.isNotEmpty(pullEvseDataRespDTOList)) {
                        //查询hubject计费规则
                        List<PriceProductDataDto> priceProductDataDtoList = pullEvseDataRespDTO.getPriceProductDataDtos();

                        if (CollectionUtils.isNotEmpty(priceProductDataDtoList)) {
                            PriceProductDataDto priceProductDataDto = priceProductDataDtoList.get(0);
                            unitPrice = priceProductDataDto.getPricingDefaultPrice();
                            currency = priceProductDataDto.getPricingDefaultPriceCurrency();
                            List<PricingProductDataRecordDto> pricingProductDataList = priceProductDataDto.getPricingProductDataRecordDtoList();
                            if (CollectionUtils.isNotEmpty(pricingProductDataList)) {
                                for (PricingProductDataRecordDto pricingProductDataRecordDto : pricingProductDataList) {
                                    if (pricingProductDataRecordDto.getReferenceUnit() == 3) {
                                        haveUnitPrice = true;
                                        BigDecimal tempUnitPrice = pricingProductDataRecordDto.getPricePerReferenceUnit();
                                        if (unitPrice == null || tempUnitPrice.compareTo(unitPrice) > 0) {
                                            unitPrice = tempUnitPrice;
                                        }
                                    } else {
                                        haveTimePrice = true;
                                        BigDecimal tempTimePrice = pricingProductDataRecordDto.getPricePerReferenceUnit();
                                        if (timePrice == null || tempTimePrice.compareTo(timePrice) < 0) {
                                            timePrice = tempTimePrice;
                                        }
                                    }
                                    //最低停车费用
                                    if (CollectionUtils.isNotEmpty(pricingProductDataRecordDto.getAdditionalReferenceDtoList())) {
                                        for (AdditionalReferenceDto additionalReferenceDto : pricingProductDataRecordDto.getAdditionalReferenceDtoList()) {
                                            BigDecimal tempParkingPrice = additionalReferenceDto.getPricePerAdditionalReferenceUnit();
                                            if (parkingPrice == null || tempParkingPrice.compareTo(parkingPrice) < 0) {
                                                parkingPrice = tempParkingPrice;
                                            }
                                        }
                                    }
                                }
                            }
                            if (CollectionUtils.isNotEmpty(priceProductDataDtoList)) {
                                getCostRuleWeeksList(priceProductDataDtoList, opLocationCardVO, pullEvseDataRespDTOList);
                            }
                        }
                    }
                    opLocationCardVO.setUnitPrice(unitPrice);
                    opLocationCardVO.setParkingPrice(parkingPrice);
                    opLocationCardVO.setStartPrice(startPrice);
                    opLocationCardVO.setIdlePrice(idlePrice);
                    opLocationCardVO.setCostPrice(costPrice);
                    opLocationCardVO.setTimePrice(timePrice);
                    opLocationCardVO.setHaveTimePrice(haveUnitPrice);
                    opLocationCardVO.setHaveUnitPrice(haveTimePrice);
                    currencySign = getCurrencySign(currency);
                    opLocationCardVO.setCurrencySign(currencySign);
                } catch (Exception ex) {
                    log.error("查询hubject计费规则失败", ex);
                }
                if (pullEvseDataRespDTO.getAccessibility() == 1) {
                    opLocationCardVO.setAccessibility(1);
                } else if (pullEvseDataRespDTO.getAccessibility() == 3) {
                    opLocationCardVO.setAccessibility(3);
                } /*else {
                    opLocationCardVO.setAccessibility(2);
                }*/
                // opLocationCardVO.setThirdPile(1);
                opLocationCardVO.setNoAdmittance(false);
            }

        } catch (Exception e) {
            log.error("查询hubject场站失败", e);
        }
    }

    private String getCurrencySign(String currency) {
        if (StringUtils.isNotBlank(currency)) {
            CurrencyDTO currencyDTO = new CurrencyDTO();
            currencyDTO.setCurrencyType(currency);
            try {
                Result<CurrencyDTO> currencyDetail = tariffAPPFeign.getCurrencyDetail(currencyDTO);
                if (currencyDetail.getCode() == HttpCodeEnum.OK.getCode() && currencyDetail.getData() != null) {
                    return currencyDetail.getData().getCurrencySign();
                }
            } catch (Exception e) {
                log.error("getCurrencyDetail error", e);
            }
        }
        return null;
    }

    private void getCostRuleWeeksList(List<PriceProductDataDto> priceProductDataDtoList, OpLocationCardVO opLocationCardVO, List<PullEvseDataRespDTO> pullEvseDataRespDTOList) {
        if (CollectionUtils.isNotEmpty(priceProductDataDtoList)) {
            List<CostModelRuleDTO> costModelRuleDTOS = Lists.newArrayList();
            //
            PriceProductDataDto priceProductDataDto = priceProductDataDtoList.get(0);
            CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
            costModelRuleDTO.setRuleModelType(3);
            List<PricingProductDataRecordDto> pricingProductDataRecordDto = priceProductDataDto.getPricingProductDataRecordDtoList();
            List<CostRuleWeeksDTO> rules = Lists.newArrayList();
            if (CollectionUtils.isNotEmpty(pricingProductDataRecordDto)) {
                for (PricingProductDataRecordDto pDR : pricingProductDataRecordDto) {
                    CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
                    List<Integer> weeks = new ArrayList<>();
                    List<CostRulesDTO> weeksRules = new ArrayList<>();
                    pDR.getProductAvailabilityTimeDtoList().forEach(pAT -> {
                        CostRulesDTO costRulesDTO = new CostRulesDTO();
                        String begin = pAT.getBegin();
                        String end = pAT.getEnd();
                        costRulesDTO.setBeginHour(Integer.valueOf(begin.substring(0, 2)));
                        costRulesDTO.setBeginMinute(Integer.valueOf(begin.substring(3, 5)));
                        costRulesDTO.setEndHour(Integer.valueOf(end.substring(0, 2)));
                        costRulesDTO.setEndMinute(Integer.valueOf(end.substring(3, 5)));
                        if (pAT.getOn() == 8) {
                            weeks.add(1);
                            weeks.add(2);
                            weeks.add(3);
                            weeks.add(4);
                            weeks.add(5);
                        } else if (pAT.getOn() == 9) {
                            weeks.add(6);
                            weeks.add(7);
                        } else if (pAT.getOn() == 10) {
                            weeks.add(1);
                            weeks.add(2);
                            weeks.add(3);
                            weeks.add(4);
                            weeks.add(5);
                            weeks.add(6);
                            weeks.add(7);
                        } else {
                            weeks.add(pAT.getOn());
                        }
                        weeksRules.add(costRulesDTO);
                    });
                    ListUtils.emptyIfNull(pDR.getAdditionalReferenceDtoList()).forEach(referenceDto -> {
                        if (referenceDto.getAdditionRef() == 3) {
                            for (CostRulesDTO costRulesDTO : weeksRules) {
                                costRulesDTO.setParkingPrice(referenceDto.getPricePerAdditionalReferenceUnit());
                                costRulesDTO.setParkingUnit(referenceDto.getAdditionalReferenceUnit());
                            }
                        }
                        if (referenceDto.getAdditionRef() == 1) {
                            for (CostRulesDTO costRulesDTO : weeksRules) {
                                costRulesDTO.setStartPrice(referenceDto.getPricePerAdditionalReferenceUnit());
                            }
                        }
                    });
                    costRuleWeeksDTO.setWeeks(weeks);
                    costRuleWeeksDTO.setFeeModel(2);
                    costRuleWeeksDTO.setWeeksRules(weeksRules);
                    rules.add(costRuleWeeksDTO);
                }
                costModelRuleDTO.setCurrencyType(pricingProductDataRecordDto.get(0).getCurrencyCode());
            } else {
                CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
                List<Integer> weeks = Lists.newArrayList(1, 2, 3, 4, 5, 6, 7);
                List<CostRulesDTO> weeksRules = new ArrayList<>();
                CostRulesDTO costRulesDTO = new CostRulesDTO();
                costRulesDTO.setUnitPrice(priceProductDataDto.getPricingDefaultPrice());
                costRulesDTO.setEnergyUnit(priceProductDataDto.getPricingDefaultReferenceUnit());
                costRulesDTO.setBeginHour(Integer.valueOf("00"));
                costRulesDTO.setBeginMinute(Integer.valueOf("00"));
                costRulesDTO.setEndHour(Integer.valueOf("24"));
                costRulesDTO.setEndMinute(Integer.valueOf("00"));
                weeksRules.add(costRulesDTO);
                costRuleWeeksDTO.setWeeksRules(weeksRules);
                costRuleWeeksDTO.setFeeModel(1);
                costRuleWeeksDTO.setWeeks(weeks);
                costModelRuleDTO.setCurrencyType(priceProductDataDto.getPricingDefaultPriceCurrency());
                rules.add(costRuleWeeksDTO);
            }
            costModelRuleDTO.setRules(rules);
            costModelRuleDTOS.add(costModelRuleDTO);
            opLocationCardVO.setCostModelRuleList(costModelRuleDTOS);
        }


    }

    private void setCardPileTypeMethod(List<PullEvseDataRespDTO> pullEvseDataRespDTOList, OpLocationCardVO opLocationCardVO) {

        if (CollectionUtils.isNotEmpty(pullEvseDataRespDTOList)) {
            HashMap<Integer, GunTypeGroupVO> gunTypeGroupVO = new HashMap<>();
            HashMap<String, GunPowerAndTypeGroupVO> gunTypePowerVO = new HashMap<>();
            HashMap<Integer, StationCardPowerCountDetailVO> cardPowerCountMap = new HashMap<>();

            for (PullEvseDataRespDTO pullEvseData : pullEvseDataRespDTOList) {
                Integer plugType = ConnectorGunTypeEnum.CCS_COMBO_2.getCode();
                if (CollectionUtils.isNotEmpty(pullEvseData.getPlugs())) {
                    plugType = pullEvseData.getPlugs().get(0);
                }
                Integer evsePower = 0;
                if (CollectionUtils.isNotEmpty(pullEvseData.getChargingFacilities())
                        && pullEvseData.getChargingFacilities().get(0).getPower() != null) {
                    evsePower = pullEvseData.getChargingFacilities().get(0).getPower();
                }

                GunTypeGroupVO orDefault = gunTypeGroupVO.getOrDefault(plugType, new GunTypeGroupVO());

                String key = evsePower + "-" + plugType;
                GunPowerAndTypeGroupVO gunPowerAndTypeGroupVO = gunTypePowerVO.getOrDefault(key, new GunPowerAndTypeGroupVO());

                StationCardPowerCountDetailVO stationCardPowerCountDetailVO = cardPowerCountMap.getOrDefault(evsePower, new StationCardPowerCountDetailVO());

                stationCardPowerCountDetailVO.setPower((double) evsePower);
                Integer cardPowerCount = stationCardPowerCountDetailVO.getTotal() != null ? stationCardPowerCountDetailVO.getTotal() + 1 : 1;
                stationCardPowerCountDetailVO.setTotal(cardPowerCount);

                orDefault.setGunType(plugType);
                gunPowerAndTypeGroupVO.setGunTypeCode(plugType);
                gunPowerAndTypeGroupVO.setPower((double) evsePower);

                Integer count = orDefault.getCount() != null ? orDefault.getCount() + 1 : 1;
                orDefault.setCount(count);
                orDefault.setTotalCount(count);
                int freeCount = 0;
                int freeCardCount = 0;
                if (Objects.equals(pullEvseData.getHubStatus(), EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                    orDefault.addFreeCount();
                    freeCount = gunPowerAndTypeGroupVO.getFreeCount() != null ? gunPowerAndTypeGroupVO.getFreeCount() + 1 : 1;
                    freeCardCount = stationCardPowerCountDetailVO.getFreeTotal() != null ? stationCardPowerCountDetailVO.getFreeTotal() + 1 : 1;
                }
                gunPowerAndTypeGroupVO.setFreeCount(freeCount);
                Integer totalCount = gunPowerAndTypeGroupVO.getTotalCount() != null ? gunPowerAndTypeGroupVO.getTotalCount() + 1 : 1;
                gunPowerAndTypeGroupVO.setTotalCount(totalCount);
                stationCardPowerCountDetailVO.setFreeTotal(freeCardCount);
                gunTypeGroupVO.put(plugType, orDefault);
                gunTypePowerVO.put(key, gunPowerAndTypeGroupVO);
                cardPowerCountMap.put(evsePower, stationCardPowerCountDetailVO);
            }
            List<GunTypeGroupVO> collect = new ArrayList<>(gunTypeGroupVO.values());
            List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOS = new ArrayList<>(gunTypePowerVO.values());
            opLocationCardVO.setPileType(collect);
            opLocationCardVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOS);
            StationCardPowerCountVO stationCardPowerCountVO = new StationCardPowerCountVO();
            List<StationCardPowerCountDetailVO> fullChargeList = Lists.newArrayList();
            List<StationCardPowerCountDetailVO> slowChargeList = Lists.newArrayList();
            cardPowerCountMap.forEach((k, v) -> {
                if (k >= 22) {
                    fullChargeList.add(v);
                } else {
                    slowChargeList.add(v);
                }
            });
            stationCardPowerCountVO.setFullCharge(fullChargeList);
            stationCardPowerCountVO.setSlowCharge(slowChargeList);
            opLocationCardVO.setPileMap(stationCardPowerCountVO);
        }
    }

    private void setDetailPileTypeMethod(List<PullEvseDataRespDTO> pullEvseDataRespDTOList, OpLocationAPPDetailVO opLocationAPPDetailVO) {

        if (CollectionUtils.isNotEmpty(pullEvseDataRespDTOList)) {

            HashMap<Integer, GunTypeVO> gunTypeVO = new HashMap<>();
            HashMap<String, GunPowerAndTypeGroupVO> gunTypePowerVO = new HashMap<>();

            for (PullEvseDataRespDTO pullEvseData : pullEvseDataRespDTOList) {
                Integer plugType = ConnectorGunTypeEnum.CCS_COMBO_2.getCode();
                if (CollectionUtils.isNotEmpty(pullEvseData.getPlugs())) {
                    plugType = pullEvseData.getPlugs().get(0);
                }
                Integer evsePower = 0;
                if (CollectionUtils.isNotEmpty(pullEvseData.getChargingFacilities())
                        && pullEvseData.getChargingFacilities().get(0).getPower() != null) {
                    evsePower = pullEvseData.getChargingFacilities().get(0).getPower();
                }

                GunTypeVO orDefault = gunTypeVO.getOrDefault(plugType, new GunTypeVO());

                String key = evsePower + "-" + plugType;
                GunPowerAndTypeGroupVO gunPowerAndTypeGroupVO = gunTypePowerVO.getOrDefault(key, new GunPowerAndTypeGroupVO());
                orDefault.setGunType(plugType);
                gunPowerAndTypeGroupVO.setGunTypeCode(plugType);
                gunPowerAndTypeGroupVO.setPower((double) evsePower);

                Integer count = orDefault.getCount() != null ? orDefault.getCount() + 1 : 1;
                orDefault.setCount(count);

                int freeCount = 0;
                int freeCardCount = 0;
                if (Objects.equals(pullEvseData.getHubStatus(), EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                    freeCount = gunPowerAndTypeGroupVO.getFreeCount() != null ? gunPowerAndTypeGroupVO.getFreeCount() + 1 : 1;
                }
                gunPowerAndTypeGroupVO.setFreeCount(freeCount);
                Integer totalCount = gunPowerAndTypeGroupVO.getTotalCount() != null ? gunPowerAndTypeGroupVO.getTotalCount() + 1 : 1;
                gunPowerAndTypeGroupVO.setTotalCount(totalCount);
                gunTypeVO.put(plugType, orDefault);
                gunTypePowerVO.put(key, gunPowerAndTypeGroupVO);
            }
            List<GunTypeVO> collect = new ArrayList<>(gunTypeVO.values());
            List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOS = new ArrayList<>(gunTypePowerVO.values());
            opLocationAPPDetailVO.setGunType(collect);
            opLocationAPPDetailVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOS);
        }
    }

    private int conditionsValue2Accessibility(Integer conditionsValue) {

        if (conditionsValue == null || conditionsValue == 3) {
            return 2;
        } else if (conditionsValue == 2) {
            return 3;
        }
        return conditionsValue;
    }

    private Integer getAppUserAppIdByUserId(String userId) {

        Integer appId = AppIdEnum.CHARGE_APP.getValue();

        String userAppIdKey = String.format(RedisKeys.APP_USER_APPID_CACHE, userId);
        Object userAppId = redisUtil.get(userAppIdKey);
        if (!ObjectUtils.isEmpty(userAppId)) {
            appId = Integer.valueOf(String.valueOf(userAppId));
        }

        return appId;
    }

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    @Override
    public OpLocationAPPDetailVO appDetail(OpLocationDetailDTO opLocationDetailDTO) {

        OpLocationAPPDetailVO opLocationDetailVO = new OpLocationAPPDetailVO();
        Long stationId = opLocationDetailDTO.getStationId();

        //基本信息
        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(stationId);
        if (optionalOpLocationElasticDTO.isPresent() && optionalOpLocationElasticDTO.get().getPlatform() != 2) {
            //判断是否planetEV APP
            appAddEMSPCondition(String.valueOf(optionalOpLocationElasticDTO.get().getOperatorId()),PileBaseEnum.LOCATION_NOT_EXIST);

            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
            opLocationDetailVO.setName(opLocationElasticDTO.getName());
            opLocationDetailVO.setAddress(opLocationElasticDTO.getAddress());
            opLocationDetailVO.setTimeZone(opLocationElasticDTO.getTimeZone());
            opLocationDetailVO.setStationTimeZone(opLocationElasticDTO.getTimeZone());
            opLocationDetailVO.setPhone(opLocationElasticDTO.getServiceTel());
            opLocationDetailVO.setUserSellerId(opLocationElasticDTO.getOperatorId());
            opLocationDetailVO.setStatus(opLocationElasticDTO.getStatus());
            opLocationDetailVO.setStationId(stationId);
            //场站公告
            opLocationDetailVO.setDiscountName(opLocationElasticDTO.getAnnouncement());
            //场站名字
            Result<SellerDetailVO> result = pileMerchantUserFeign.detail(opLocationElasticDTO.getOperatorId());
            if(!Objects.isNull(result) && !Objects.isNull(result.getData())){
                opLocationDetailVO.setUserSellerName(result.getData().getName());
            }
            if (StrUtil.isNotBlank(opLocationElasticDTO.getFacility())) {
                //周边设施
                opLocationDetailVO.setFacilityList(JSON.parseArray(opLocationElasticDTO.getFacility(), OpLocationFacilityDTO.class));
            }
            //经纬度、距离
            Double lat = opLocationElasticDTO.getLatitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLatitude());
            Double lon = opLocationElasticDTO.getLongitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLongitude());
            opLocationDetailVO.setLatitude(lat);
            opLocationDetailVO.setLongitude(lon);
            //距离
            this.setDistance(opLocationDetailDTO, opLocationDetailVO);
            //查询运营表
            QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
            opLocationOperationEntityQueryWrapper.eq("location_id", stationId);
            opLocationOperationEntityQueryWrapper.select("logo_image_id");
            OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(opLocationOperationEntityQueryWrapper);// todo  CodeReview select 指定列 from  t
            //logo
            if (opLocationOperationEntity != null && opLocationOperationEntity.getLogoImageId() != null) {
                OpImageEntity opImageEntity = opImageMapper.selectById(opLocationOperationEntity.getLogoImageId());// todo  CodeReview select 指定列 from  t
                if (opImageEntity != null) {
                    opLocationDetailVO.setLogoPath(opImageEntity.getUrl());
                }
            }
            //场站图片
            List<String> stationLogoPaths = getStationLogoPath(stationId);
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(stationLogoPaths)) {
                for (String logonPath : stationLogoPaths) {
                    opLocationDetailVO.addPicList(logonPath);
                }
            } else {
                opLocationDetailVO.setPicList(new ArrayList<>());
            }
            //查询es设备
            List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(stationId);
            esEVSEList = esEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());

            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            //需要过滤掉订阅过期了的桩
            if (subscribeEnable && !"8".equalsIgnoreCase(appId)) {
                esEVSEList = esEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getSubscriptionCheck()) && m.getSubscriptionCheck()).collect(Collectors.toList());
            }
            //枪数量
            Integer gunNum = CollectionUtils.isEmpty(esEVSEList) ? 0 : esEVSEList.size();
            opLocationDetailVO.setGunNum(gunNum);
            //空闲枪数量
            Integer freeGunNum = stationFreeGunNum(esEVSEList);
            opLocationDetailVO.setFreeGunNum(freeGunNum);
            //不同枪类型统计
            List<GunTypeGroupVO> gunTypeGroupVOS = buildStationGunTypeGroupVOList2(esEVSEList);
            List<GunTypeVO> gunTypeVOS = DozerConvert.mapList(gunTypeGroupVOS, GunTypeVO.class);
            opLocationDetailVO.setGunType(gunTypeVOS);
            //功率范围
            String powerRange = buildStationPowerRange(esEVSEList);
            opLocationDetailVO.setPowerRange(powerRange);
            //不同功率和枪类型双维度统计
            List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = buildGunPowerAndTypeGroupVOList(esEVSEList);
            opLocationDetailVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOList);
            //其它服务
            List<OtherServiceVO> otherService = stationOtherService();
            opLocationDetailVO.setOtherService(otherService);
            //充电枪列表(7月改版前)
            List<GunListPageVO> gunListPageVOS = buildStationGunListPageVOList(esEVSEList, opLocationDetailDTO.getPageIndex(), opLocationDetailDTO.getPageSize());

            //支付方式
            List<PaymentVO> paymentList = stationPaymentList();
            opLocationDetailVO.setPaymentList(paymentList);
            //判断是否支持预约(场站开关，固件版本号是否支持两方面因素)

            Set<String> reserveNotSupportSet = new HashSet<>();
            try {
                if (!ObjectUtils.isEmpty(this.supportReserveFirmwareVersionMap)) {
                    Map<String, String> supportReserveFirmwareVersionMap = this.supportReserveFirmwareVersionMap;
                    if (CollUtil.isNotEmpty(supportReserveFirmwareVersionMap)) {
                        Set<String> pileSnList = esEVSEList.stream().map(OpLocationEvseElasticDTO::getPileSn).collect(Collectors.toSet());
                        Result<List<ChargePileFirmwareVersionDTO>> firmwareVersionByPileSnListResult = deviceServiceClient.getFirmwareVersionByPileSnList(new ArrayList<>(pileSnList));
                        if (null != firmwareVersionByPileSnListResult
                                && null != firmwareVersionByPileSnListResult.getData()
                                && CollUtil.isNotEmpty(firmwareVersionByPileSnListResult.getData())) {
                            List<ChargePileFirmwareVersionDTO> chargePileFirmwareVersionDTOS = firmwareVersionByPileSnListResult.getData();
                            //遍历，找出不支持预约的桩
                            for (ChargePileFirmwareVersionDTO chargePileFirmwareVersionDTO : chargePileFirmwareVersionDTOS) {
                                if (!ObjectUtils.isEmpty(supportReserveFirmwareVersionMap.get(chargePileFirmwareVersionDTO.getFirmwareId()))) {
                                    String restrictVersion = supportReserveFirmwareVersionMap.get(chargePileFirmwareVersionDTO.getFirmwareId());
                                    String firmwareVersion = chargePileFirmwareVersionDTO.getFirmwareVersion();
                                    //比较两个版本号
                                    if (StringUtils.isNotBlank(restrictVersion)
                                            && StringUtils.isNotBlank(firmwareVersion)
                                            && locationCommon.compareVersion(restrictVersion, firmwareVersion) == 1) {
                                        reserveNotSupportSet.add(chargePileFirmwareVersionDTO.getSn());
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (Exception e) {
                log.error("校验桩固件版本号是否支持预约出错了:{}", e);
            }

            if (null != opLocationElasticDTO.getReservationEnabled() && opLocationElasticDTO.getReservationEnabled()) {
                esEVSEList.forEach(esEVSE -> {
                    if (reserveNotSupportSet.contains(esEVSE.getPileSn())) {
                        esEVSE.setReservationEnabled(false);
                    } else {
                        esEVSE.setReservationEnabled(true);
                    }
                });
            }

            //充电桩列表(7月改版后)
            List<AppDetailPileListVO> pileList = buildAppDetailPileList(esEVSEList);
            List<String> noTariffIdPileSnList = pileList.stream().filter(k -> ObjectUtils.isEmpty(k.getTariffId())).map(AppDetailPileListVO::getPileSn).collect(Collectors.toList());
            pileList = pileList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
            gunListPageVOS = gunListPageVOS.stream().filter(o -> !noTariffIdPileSnList.contains(o.getPileSn())).collect(Collectors.toList());
            Collections.sort(gunListPageVOS);
            opLocationDetailVO.setGunList(gunListPageVOS);

            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            if (jwtInfo != null
                    && jwtInfo.getId() != null) {
                Integer appUserAppIdByUserId = this.getAppUserAppIdByUserId(jwtInfo.getId().toString());
                if (AppIdEnum.CHARGE_APP.getValue().equals(appUserAppIdByUserId)) {
                    for (AppDetailPileListVO appDetailPileListVO : pileList) {
                        if (appDetailPileListVO.getIsNewTariffRuleAndOldVersion() != null && appDetailPileListVO.getIsNewTariffRuleAndOldVersion()) {
                            opLocationDetailVO.setName(opLocationDetailVO.getName() + "(" + MessageSourceHolder.getMessage(BILLING_RULES_HAS_UPDATED_NEED_TO_UPGRADE_FOR_LOOK, "A pricing rule has been updated for this site. Please update the app to the latest version before checking.") + ")");
                            break;
                        }
                    }
                }
            }

            List<String> snList = pileList.stream().map(AppDetailPileListVO::getPileSn).collect(Collectors.toList());
            //返回DLB标识
            List<PowerLoadBalanceVO> loadBalanceVOList = homePileFeignClient.findList(snList).getData();
            if (CollectionUtils.isNotEmpty(loadBalanceVOList)) {
                Map<String, PowerLoadBalanceVO> loadBalanceVOMap = loadBalanceVOList.stream().collect(toMap(PowerLoadBalanceVO::getSn, e -> e, (f, s) -> f));
                pileList.stream().forEach(p -> {
                    String pileSn = p.getPileSn();
                    if (loadBalanceVOMap.get(pileSn) != null) {
                        p.setPileType(JudgeMasterOrSlaveEnums.CLOUD.getPileType());
                    }
                });
            }
            Result<List<MasterSlaveRelationDTO>> listResult = homePileFeignClient.judgeMasterOrSlave(snList);
            if (!ObjectUtils.isEmpty(listResult.getData())) {
                log.info("judgeMasterOrSlave,listResult={}", listResult.getData());
                List<MasterSlaveRelationDTO> masterSlaveRelationDTOS = listResult.getData();
                Map<String, Integer> mainSnAlmMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getMainSn())).collect(toMap(MasterSlaveRelationDTO::getMainSn, MasterSlaveRelationDTO::getAlmEnabled, (e1, e2) -> e2));
                Map<String, Integer> childSnAlmMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getSn())).collect(toMap(MasterSlaveRelationDTO::getSn, MasterSlaveRelationDTO::getAlmEnabled, (e1, e2) -> e2));
                Map<String, Integer> mainSnPileNumMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getMainSn())).collect(toMap(MasterSlaveRelationDTO::getMainSn, MasterSlaveRelationDTO::getPileNum, (e1, e2) -> e2));
                Map<String, Integer> childSnPileNumMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getSn())).collect(toMap(MasterSlaveRelationDTO::getSn, MasterSlaveRelationDTO::getPileNum, (e1, e2) -> e2));
                pileList.forEach(m -> {
                    if (mainSnAlmMap.containsKey(m.getPileSn())) {
                        m.setPileType(JudgeMasterOrSlaveEnums.MASTER.getPileType());
                        m.setPileNum(mainSnPileNumMap.get(m.getPileSn()));
                    } else if (childSnAlmMap.containsKey(m.getPileSn())) {
                        m.setPileType(JudgeMasterOrSlaveEnums.SLAVE.getPileType());
                        m.setPileNum(childSnPileNumMap.get(m.getPileSn()));
                    }
                });
            }
            //设置modbus信息
            try {
                if (CollectionUtils.isNotEmpty(snList)) {
                    //是否支持Modbus
                    Result<List<ChargePileDTO>> deviceChargePileResult = deviceServiceClient.queryPileList(snList);
                    Map<String, Boolean> snEmsEnableMap = buildSNEmsEnableMap(deviceChargePileResult);
                    //modbus配置信息
                    Result<List<DataServiceChargePile>> dataServiceChargePileResult = homePileFeignClient.queryPileList(snList);
                    Map<String, String> snEmsAddressMap = buildSNEmsAddressMap(dataServiceChargePileResult);
                    pileList.forEach(pile -> {
                        pile.setEmsEnable(snEmsEnableMap.get(pile.getPileSn()));
                        pile.setEmsAddress(snEmsAddressMap.get(pile.getPileSn()));
                    });
                }
            } catch (Exception e) {
                log.info("设置modbus信息异常:" + e);
            }

            Map<String, Integer> pileProductTypeMap = new HashMap<>();
            Result<List<PileTypeVO>> productNamePdmResult = deviceClient.getProductTypeByPileSnList(snList);
            if (!ObjectUtils.isEmpty(productNamePdmResult) && CollUtil.isNotEmpty(productNamePdmResult.getData())) {
                List<PileTypeVO> pileTypeVOS = productNamePdmResult.getData();
                pileProductTypeMap = pileTypeVOS.stream().collect(toMap(PileTypeVO::getSn, PileTypeVO::getPileProductType));
            }
            Map<String, Integer> finalPileProductTypeMap = pileProductTypeMap;
            pileList.forEach(pile -> {
                Integer pileProductType = finalPileProductTypeMap.get(pile.getPileSn());
                pile.setPileProductType(null == pileProductType ? 0 : pileProductType);
            });

            opLocationDetailVO.setPileList(pileList);
            try {
                //场站所在时区的时间
                String timeOffset = StringUtils.isBlank(opLocationElasticDTO.getTimeZone()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getTimeZone();
                String timeZone = StringUtils.isBlank(opLocationElasticDTO.getZoneId()) ? timeOffset : opLocationElasticDTO.getZoneId();
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
                opLocationDetailVO.setWeekDay(weekDay);
                opLocationDetailVO.setHour(hour);
                opLocationDetailVO.setMinute(minute);
                String nowTimeStr = hour + ":" + minute;
                DateFormat dateFormat = new SimpleDateFormat("HH:mm");
                Date nowTime = dateFormat.parse(nowTimeStr);
                log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
                log.info(BaseConstant.NOW_TIME, nowTime);
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
                        opLocationDetailVO.setOpen(ruleRelateForAppVO.getIsOpen());
                        opLocationDetailVO.setNoAdmittance(ruleRelateForAppVO.getIsLimit());
                        opLocationDetailVO.setChangingTime(ruleRelateForAppVO.getChangingTime());
                        opLocationDetailVO.setChangingWeekDay(ruleRelateForAppVO.getDay());
                    }
                } catch (Exception e) {
                    log.info("进场信息查询失败：" , e);
                }
            } catch (ParseException e) {
                log.error("OpLocationRepositoryImpl appDetail", e);
            }
            //周边设施
            List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationId(stationId);
            if (CollUtil.isEmpty(locationFacilityDTOList)) {
                locationFacilityDTOList = Lists.newArrayList();
            }
            opLocationDetailVO.setFacilityList(locationFacilityDTOList);
            //autel桩开启互联互通，不受进场控制影响下，不显示限制进入
            if (null != opLocationElasticDTO.getPlatform()
                    && opLocationElasticDTO.getPlatform() != 1
                    && opLocationElasticDTO.getHubjectCheck() != null
                    && opLocationElasticDTO.getHubjectCheck()) {
                opLocationDetailVO.setAccessibility(conditionsValue2Accessibility(opLocationElasticDTO.getConditionsValue()));
                opLocationDetailVO.setThirdPile(1);
            }

            return opLocationDetailVO;
        }

        if (Boolean.parseBoolean(hubjectEnable) && optionalOpLocationElasticDTO.isPresent()
                && optionalOpLocationElasticDTO.get().getPlatform() == 2) {
            hubjectAppDetail(opLocationDetailDTO, opLocationDetailVO, optionalOpLocationElasticDTO.get());
        }
        if (opLocationDetailVO.getStationId() == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        return opLocationDetailVO;
    }

    @Override
    public Integer deleteOCPIEMSPData(List<Long> sellerIdList) {
        List<OpLocationElasticDTO> opLocationElasticDTOList;
        if (CollectionUtils.isEmpty(sellerIdList)) {
            opLocationElasticDTOList = opLocationElastic.findAllByPlatform(OpLocationPlatformEnum.OCPI_EMSP.getCode());
        } else {
            opLocationElasticDTOList = opLocationElastic.findAllByOperatorIdInAndPlatform(sellerIdList, OpLocationPlatformEnum.OCPI_EMSP.getCode());
        }

        if (CollectionUtils.isEmpty(opLocationElasticDTOList)) {
            return 0;
        }

        //场站
        Set<Long> locationIdList = opLocationElasticDTOList.stream().map(OpLocationElasticDTO::getId).collect(Collectors.toSet());
        opLocationElastic.deleteAllByIdIn(locationIdList);
        //桩
        List<OpLocationPileEvseElasticDTO> pileEvseElasticDTOList = opLocationPileEvseElastic.findAllByLocationIdIn(locationIdList);
        if (CollectionUtils.isNotEmpty(pileEvseElasticDTOList)) {
            Set<Long> pileEVSEIdSet = pileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toSet());
            opLocationPileEvseElastic.deleteAllById(pileEVSEIdSet);
        }
        //枪
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByLocationIdIn(locationIdList);
        if (CollectionUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
            Set<Long> evseIdSet = opLocationEvseElasticDTOList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toSet());
            opLocationEvseElastic.deleteAllById(evseIdSet);
        }
        //枪扩展
        List<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOList = opLocationEvseExpandElastic.findAllByLocationIdIn(locationIdList);
        if (CollectionUtils.isNotEmpty(opLocationEvseExpandElasticDTOList)) {
            Set<Long> expandIdSet = opLocationEvseExpandElasticDTOList.stream().map(OpLocationEvseExpandElasticDTO::getId).collect(Collectors.toSet());
            opLocationEvseExpandElastic.deleteAllById(expandIdSet);
        }
        //图片
        LambdaQueryWrapper<OpLocationImageEntity> opLocationImageEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationImageEntity.class).in(OpLocationImageEntity::getLocationId, locationIdList);
        opLocationImageMapper.delete(opLocationImageEntityLambdaQueryWrapper);

        return opLocationElasticDTOList.size();
    }

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    @Override
    public OpLocationAPPDetailVO appDetailForOCPI(OpLocationDetailDTO opLocationDetailDTO) {

        OpLocationAPPDetailVO opLocationDetailVO = new OpLocationAPPDetailVO();
        Long stationId = opLocationDetailDTO.getStationId();

        //基本信息
        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(stationId);
        if (optionalOpLocationElasticDTO.isPresent() && optionalOpLocationElasticDTO.get().getPlatform() != 2) {
            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
            opLocationDetailVO.setName(opLocationElasticDTO.getName());
            opLocationDetailVO.setAddress(opLocationElasticDTO.getAddress());
            opLocationDetailVO.setTimeZone(opLocationElasticDTO.getTimeZone());
            opLocationDetailVO.setStationTimeZone(opLocationElasticDTO.getTimeZone());
            opLocationDetailVO.setPhone(opLocationElasticDTO.getServiceTel());
            opLocationDetailVO.setUserSellerId(opLocationElasticDTO.getOperatorId());
            opLocationDetailVO.setStatus(opLocationElasticDTO.getStatus());
            opLocationDetailVO.setStationId(stationId);
            //场站公告
            opLocationDetailVO.setDiscountName(opLocationElasticDTO.getAnnouncement());
            if (StrUtil.isNotBlank(opLocationElasticDTO.getFacility())) {
                //周边设施
                opLocationDetailVO.setFacilityList(JSON.parseArray(opLocationElasticDTO.getFacility(), OpLocationFacilityDTO.class));
            }
            //经纬度、距离
            Double lat = opLocationElasticDTO.getLatitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLatitude());
            Double lon = opLocationElasticDTO.getLongitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLongitude());
            opLocationDetailVO.setLatitude(lat);
            opLocationDetailVO.setLongitude(lon);
            //距离
            this.setDistance(opLocationDetailDTO, opLocationDetailVO);
            //查询运营表
            QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
            opLocationOperationEntityQueryWrapper.eq("location_id", stationId);
            opLocationOperationEntityQueryWrapper.select("logo_image_id");
            OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(opLocationOperationEntityQueryWrapper);// todo  CodeReview select 指定列 from  t
            //logo
            if (opLocationOperationEntity != null && opLocationOperationEntity.getLogoImageId() != null) {
                OpImageEntity opImageEntity = opImageMapper.selectById(opLocationOperationEntity.getLogoImageId());// todo  CodeReview select 指定列 from  t
                if (opImageEntity != null) {
                    opLocationDetailVO.setLogoPath(opImageEntity.getUrl());
                }
            }
            //场站图片
            List<String> stationLogoPaths = getStationLogoPath(stationId);
            log.info("查询场站图片:{}", JSONArray.toJSONString(stationLogoPaths));
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(stationLogoPaths)) {
                for (String logonPath : stationLogoPaths) {
                    opLocationDetailVO.addPicList(logonPath);
                }
            } else {
                opLocationDetailVO.setPicList(new ArrayList<>());
            }
            //查询es设备
            List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(stationId);
            esEVSEList = esEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
            log.info("站点：{}  的设备：{}", opLocationDetailDTO.getStationId(), JSON.toJSONString(esEVSEList));
            //枪数量
            Integer gunNum = CollectionUtils.isEmpty(esEVSEList) ? 0 : esEVSEList.size();
            opLocationDetailVO.setGunNum(gunNum);
            //空闲枪数量
            Integer freeGunNum = stationFreeGunNum(esEVSEList);
            opLocationDetailVO.setFreeGunNum(freeGunNum);
            //不同枪类型统计
            List<GunTypeGroupVO> gunTypeGroupVOS = buildStationGunTypeGroupVOList2(esEVSEList);
            List<GunTypeVO> gunTypeVOS = DozerConvert.mapList(gunTypeGroupVOS, GunTypeVO.class);
            opLocationDetailVO.setGunType(gunTypeVOS);
            //功率范围
            String powerRange = buildStationPowerRange(esEVSEList);
            opLocationDetailVO.setPowerRange(powerRange);
            //不同功率和枪类型双维度统计
            List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = buildGunPowerAndTypeGroupVOList(esEVSEList);
            opLocationDetailVO.setGunPowerAndTypeGroup(gunPowerAndTypeGroupVOList);
            //其它服务
            List<OtherServiceVO> otherService = stationOtherService();
            opLocationDetailVO.setOtherService(otherService);
            //充电枪列表(7月改版前)
            List<GunListPageVO> gunListPageVOS = buildStationGunListPageVOList(esEVSEList, opLocationDetailDTO.getPageIndex(), opLocationDetailDTO.getPageSize());

            //支付方式
            List<PaymentVO> paymentList = stationPaymentList();
            opLocationDetailVO.setPaymentList(paymentList);
            //判断是否支持预约(场站开关，固件版本号是否支持两方面因素)

            Set<String> reserveNotSupportSet = new HashSet<>();
            try {
                if (!ObjectUtils.isEmpty(this.supportReserveFirmwareVersionMap)) {
                    Map<String, String> supportReserveFirmwareVersionMap = this.supportReserveFirmwareVersionMap;
                    log.info("===>>appDetail.supportReserveFirmwareVersionMap:{}", JSON.toJSONString(supportReserveFirmwareVersionMap));
                    if (CollUtil.isNotEmpty(supportReserveFirmwareVersionMap)) {
                        Set<String> pileSnList = esEVSEList.stream().map(OpLocationEvseElasticDTO::getPileSn).collect(Collectors.toSet());
                        Result<List<ChargePileFirmwareVersionDTO>> firmwareVersionByPileSnListResult = deviceServiceClient.getFirmwareVersionByPileSnList(new ArrayList<>(pileSnList));
                        log.info("===>>appDetail.firmwareVersionByPileSnListResult:{}", JSON.toJSONString(firmwareVersionByPileSnListResult));
                        if (null != firmwareVersionByPileSnListResult
                                && null != firmwareVersionByPileSnListResult.getData()
                                && CollUtil.isNotEmpty(firmwareVersionByPileSnListResult.getData())) {
                            List<ChargePileFirmwareVersionDTO> chargePileFirmwareVersionDTOS = firmwareVersionByPileSnListResult.getData();
                            //遍历，找出不支持预约的桩
                            for (ChargePileFirmwareVersionDTO chargePileFirmwareVersionDTO : chargePileFirmwareVersionDTOS) {
                                if (!ObjectUtils.isEmpty(supportReserveFirmwareVersionMap.get(chargePileFirmwareVersionDTO.getFirmwareId()))) {
                                    String restrictVersion = supportReserveFirmwareVersionMap.get(chargePileFirmwareVersionDTO.getFirmwareId());
                                    String firmwareVersion = chargePileFirmwareVersionDTO.getFirmwareVersion();
                                    //比较两个版本号
                                    if (StringUtils.isNotBlank(restrictVersion)
                                            && StringUtils.isNotBlank(firmwareVersion)
                                            && locationCommon.compareVersion(restrictVersion, firmwareVersion) == 1) {
                                        reserveNotSupportSet.add(chargePileFirmwareVersionDTO.getSn());
                                    }
                                }
                            }
                            log.info("===>>appDetail.reserveNotSupportSet:{}", JSON.toJSONString(reserveNotSupportSet));
                        }
                    }
                }
            } catch (Exception e) {
                log.error("校验桩固件版本号是否支持预约出错了:" + e);
            }

            if (null != opLocationElasticDTO.getReservationEnabled() && opLocationElasticDTO.getReservationEnabled()) {
                esEVSEList.forEach(esEVSE -> {
                    if (reserveNotSupportSet.contains(esEVSE.getPileSn())) {
                        esEVSE.setReservationEnabled(false);
                    } else {
                        esEVSE.setReservationEnabled(true);
                    }
                });
            }

            //充电桩列表(7月改版后)
            List<AppDetailPileListVO> pileList = buildAppDetailPileListForOCPI(esEVSEList);
            List<String> noTariffIdPileSnList = pileList.stream().filter(k -> ObjectUtils.isEmpty(k.getTariffId())).map(AppDetailPileListVO::getPileSn).collect(Collectors.toList());
            pileList = pileList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
            gunListPageVOS = gunListPageVOS.stream().filter(o -> !noTariffIdPileSnList.contains(o.getPileSn())).collect(Collectors.toList());
            Collections.sort(gunListPageVOS);
            opLocationDetailVO.setGunList(gunListPageVOS);

            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            if (jwtInfo != null
                    && jwtInfo.getId() != null) {
                Integer appUserAppIdByUserId = this.getAppUserAppIdByUserId(jwtInfo.getId().toString());
                if (AppIdEnum.CHARGE_APP.getValue().equals(appUserAppIdByUserId)) {
                    for (AppDetailPileListVO appDetailPileListVO : pileList) {
                        if (appDetailPileListVO.getIsNewTariffRuleAndOldVersion() != null && appDetailPileListVO.getIsNewTariffRuleAndOldVersion()) {
                            opLocationDetailVO.setName(opLocationDetailVO.getName() + "(" + MessageSourceHolder.getMessage(BILLING_RULES_HAS_UPDATED_NEED_TO_UPGRADE_FOR_LOOK, "A pricing rule has been updated for this site. Please update the app to the latest version before checking.") + ")");
                            break;
                        }
                    }
                }
            }


            List<String> snList = pileList.stream().map(AppDetailPileListVO::getPileSn).collect(Collectors.toList());
            //返回DLB标识
            List<PowerLoadBalanceVO> loadBalanceVOList = homePileFeignClient.findList(snList).getData();
            if (CollectionUtils.isNotEmpty(loadBalanceVOList)) {
                Map<String, PowerLoadBalanceVO> loadBalanceVOMap = loadBalanceVOList.stream().collect(toMap(PowerLoadBalanceVO::getSn, e -> e, (f, s) -> f));
                pileList.stream().forEach(p -> {
                    String pileSn = p.getPileSn();
                    if (loadBalanceVOMap.get(pileSn) != null) {
                        p.setPileType(JudgeMasterOrSlaveEnums.CLOUD.getPileType());
                    }
                });
            }
            Result<List<MasterSlaveRelationDTO>> listResult = homePileFeignClient.judgeMasterOrSlave(snList);
            if (!ObjectUtils.isEmpty(listResult.getData())) {
                log.info("judgeMasterOrSlave,listResult={}", listResult.getData());
                List<MasterSlaveRelationDTO> masterSlaveRelationDTOS = listResult.getData();
                Map<String, Integer> mainSnAlmMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getMainSn())).collect(toMap(MasterSlaveRelationDTO::getMainSn, MasterSlaveRelationDTO::getAlmEnabled, (e1, e2) -> e2));
                Map<String, Integer> childSnAlmMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getSn())).collect(toMap(MasterSlaveRelationDTO::getSn, MasterSlaveRelationDTO::getAlmEnabled, (e1, e2) -> e2));
                Map<String, Integer> mainSnPileNumMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getMainSn())).collect(toMap(MasterSlaveRelationDTO::getMainSn, MasterSlaveRelationDTO::getPileNum, (e1, e2) -> e2));
                Map<String, Integer> childSnPileNumMap = masterSlaveRelationDTOS.stream().filter(m -> !ObjectUtils.isEmpty(m.getSn())).collect(toMap(MasterSlaveRelationDTO::getSn, MasterSlaveRelationDTO::getPileNum, (e1, e2) -> e2));
                pileList.forEach(m -> {
                    if (mainSnAlmMap.containsKey(m.getPileSn())) {
                        m.setPileType(JudgeMasterOrSlaveEnums.MASTER.getPileType());
                        m.setPileNum(mainSnPileNumMap.get(m.getPileSn()));
                    } else if (childSnAlmMap.containsKey(m.getPileSn())) {
                        m.setPileType(JudgeMasterOrSlaveEnums.SLAVE.getPileType());
                        m.setPileNum(childSnPileNumMap.get(m.getPileSn()));
                    }
                });
            }
            //设置modbus信息
            try {
                if (CollectionUtils.isNotEmpty(snList)) {
                    //是否支持Modbus
                    Result<List<ChargePileDTO>> deviceChargePileResult = deviceServiceClient.queryPileList(snList);
                    log.info("deviceChargePileResult:{}", JSONObject.toJSONString(deviceChargePileResult));
                    Map<String, Boolean> snEmsEnableMap = buildSNEmsEnableMap(deviceChargePileResult);
                    //modbus配置信息
                    Result<List<DataServiceChargePile>> dataServiceChargePileResult = homePileFeignClient.queryPileList(snList);
                    log.info("dataServiceChargePileList:{}", JSONObject.toJSONString(dataServiceChargePileResult));
                    Map<String, String> snEmsAddressMap = buildSNEmsAddressMap(dataServiceChargePileResult);
                    pileList.forEach(pile -> {
                        pile.setEmsEnable(snEmsEnableMap.get(pile.getPileSn()));
                        pile.setEmsAddress(snEmsAddressMap.get(pile.getPileSn()));
                    });
                }
            } catch (Exception e) {
                log.info("设置modbus信息异常:" + e);
            }
            Map<String, Integer> pileProductTypeMap = new HashMap<>();
            Result<List<PileTypeVO>> productNamePdmResult = deviceClient.getProductTypeByPileSnList(snList);
            if (!ObjectUtils.isEmpty(productNamePdmResult) && CollUtil.isNotEmpty(productNamePdmResult.getData())) {
                List<PileTypeVO> pileTypeVOS = productNamePdmResult.getData();
                pileProductTypeMap = pileTypeVOS.stream().collect(toMap(PileTypeVO::getSn, PileTypeVO::getPileProductType));
            }
            Map<String, Integer> finalPileProductTypeMap = pileProductTypeMap;
            pileList.forEach(pile -> {
                Integer pileProductType = finalPileProductTypeMap.get(pile.getPileSn());
                pile.setPileProductType(null == pileProductType ? 0 : pileProductType);
            });
            opLocationDetailVO.setPileList(pileList);
            try {
                //场站所在时区的时间
                String timeOffset = StringUtils.isBlank(opLocationElasticDTO.getTimeZone()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getTimeZone();
                String timeZone = StringUtils.isBlank(opLocationElasticDTO.getZoneId()) ? timeOffset : opLocationElasticDTO.getZoneId();
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
                opLocationDetailVO.setWeekDay(weekDay);
                opLocationDetailVO.setHour(hour);
                opLocationDetailVO.setMinute(minute);
                String nowTimeStr = hour + ":" + minute;
                DateFormat dateFormat = new SimpleDateFormat("HH:mm");
                Date nowTime = dateFormat.parse(nowTimeStr);
                log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
                log.info(BaseConstant.NOW_TIME, nowTime);
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
                        opLocationDetailVO.setOpen(ruleRelateForAppVO.getIsOpen());
                        opLocationDetailVO.setNoAdmittance(ruleRelateForAppVO.getIsLimit());
                        opLocationDetailVO.setChangingTime(ruleRelateForAppVO.getChangingTime());
                        opLocationDetailVO.setChangingWeekDay(ruleRelateForAppVO.getDay());
                    }
                } catch (Exception e) {
                    log.info("进场信息查询失败：", e);
                }
            } catch (ParseException e) {
                log.error("OpLocationRepositoryImpl appDetail", e);
            }
            //周边设施
            List<OpLocationFacilityDTO> locationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationId(stationId);
            log.info("========= the locationFacilityDTOList:{}", JSON.toJSONString(locationFacilityDTOList));
            if (CollUtil.isEmpty(locationFacilityDTOList)) {
                locationFacilityDTOList = Lists.newArrayList();
            }
            opLocationDetailVO.setFacilityList(locationFacilityDTOList);
            //autel桩开启互联互通，不受进场控制影响下，不显示限制进入
            if (null != opLocationElasticDTO.getPlatform()
                    && opLocationElasticDTO.getPlatform() != 1
                    && opLocationElasticDTO.getHubjectCheck() != null
                    && opLocationElasticDTO.getHubjectCheck()) {
                opLocationDetailVO.setAccessibility(conditionsValue2Accessibility(opLocationElasticDTO.getConditionsValue()));
                opLocationDetailVO.setThirdPile(1);
            }

            return opLocationDetailVO;
        }

        if (Boolean.parseBoolean(hubjectEnable) && optionalOpLocationElasticDTO.isPresent()
                && optionalOpLocationElasticDTO.get().getPlatform() == 2) {
            hubjectAppDetail(opLocationDetailDTO, opLocationDetailVO, optionalOpLocationElasticDTO.get());
        }
        if (opLocationDetailVO.getStationId() == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        return opLocationDetailVO;
    }

    private void setDistance(OpLocationCardDTO opLocationCardDTO, OpLocationCardVO opLocationCardVO) {
        if (opLocationCardDTO.getLatitude() != null && opLocationCardDTO.getLongitude() != null) {
            try {
                double calculateDistance = GeoDistance.ARC.calculate(opLocationCardVO.getLatitude(), opLocationCardVO.getLongitude(), opLocationCardDTO.getLatitude(),
                        opLocationCardDTO.getLongitude(), DistanceUnit.KILOMETERS);
                opLocationCardVO.setDistance(Double.valueOf(String.format("%.2f", calculateDistance)));
            } catch (Exception ignored) {
            }
        }
    }

    private void setDistance(OpLocationDetailDTO opLocationDetailDTO, OpLocationAPPDetailVO opLocationDetailVO) {
        if (opLocationDetailDTO.getLatitude() != null && opLocationDetailDTO.getLongitude() != null) {
            try {
                double calculateDistance = GeoDistance.ARC.calculate(opLocationDetailVO.getLatitude(), opLocationDetailVO.getLongitude(),
                        opLocationDetailDTO.getLatitude(), opLocationDetailDTO.getLongitude(), DistanceUnit.KILOMETERS);
                opLocationDetailVO.setDistance(Double.valueOf(String.format("%.2f", calculateDistance)));
            } catch (Exception ignored) {
            }
        }
    }

    private OpLocationAPPDetailVO hubjectAppDetail(OpLocationDetailDTO opLocationDetailDTO, OpLocationAPPDetailVO opLocationDetailVO,
                                                   OpLocationElasticDTO opLocationElasticDTO) {
        try {
            opLocationDetailVO.setStationId(opLocationElasticDTO.getId());
            opLocationDetailVO.setName(opLocationElasticDTO.getName());
            // hubject 设置为商桩
            opLocationDetailVO.setStatus(EVSETypeEnum.BUSINESS_PILE.getCode());
            opLocationDetailVO.setAddress(opLocationElasticDTO.getAddress());

            opLocationDetailVO.setLatitude(Double.valueOf(opLocationElasticDTO.getLatitude()));
            opLocationDetailVO.setLongitude(Double.valueOf(opLocationElasticDTO.getLongitude()));
            opLocationDetailVO.setTimeZone(opLocationElasticDTO.getTimeZone());
            opLocationDetailVO.setStationTimeZone(opLocationElasticDTO.getTimeZone());
            opLocationDetailVO.setPhone(opLocationElasticDTO.getServiceTel());
            //距离
            this.setDistance(opLocationDetailDTO, opLocationDetailVO);
            JwtInfo loginUser = LoginUserHolder.getLoginUser();
            if (loginUser != null && loginUser.getId() != null) {
                Boolean member = redisTemplate.opsForSet().isMember(RedisKeyConstant.getSupportEroamingLocationSetKey(opLocationElasticDTO.getId()), loginUser.getId());
                opLocationDetailVO.setLikeRoaming(member);
            }
            opLocationDetailVO.setERoamingNum(opLocationElasticDTO.getERoamingNum());
            opLocationDetailVO.setPlatform(opLocationElasticDTO.getPlatform());

            // 查询hubject
            Result<List<PullEvseDataRespDTO>> listResult = oicpFeign.queryChargingPointData(opLocationElasticDTO.getId());
            log.info("hubject--查询huject 场站信息, {}: {}", opLocationElasticDTO.getId(), JSON.toJSONString(listResult));
            List<PullEvseDataRespDTO> pullEvseDataRespDTOList = listResult.getData();
            if (listResult.getCode() == 200 && CollectionUtils.isNotEmpty(pullEvseDataRespDTOList)) {

                PullEvseDataRespDTO pullEvseDataRespDTO = pullEvseDataRespDTOList.get(0);
                //logo
                opLocationDetailVO.setLogoPath(pullEvseDataRespDTO.getChargingStationImage());

                //场站图片
                log.info("站点：{}  的设备：{}", opLocationDetailDTO.getStationId(), JSON.toJSONString(pullEvseDataRespDTOList));
                //枪数量
                Integer gunNum = CollectionUtils.isEmpty(pullEvseDataRespDTOList) ? 0 : pullEvseDataRespDTOList.size();
                opLocationDetailVO.setGunNum(gunNum);

                setDetailPileTypeMethod(pullEvseDataRespDTOList, opLocationDetailVO);
                Integer power = 0;
                List<EvseStatusRecordDTO> collect = pullEvseDataRespDTOList.stream()
                        .map(item -> EvseStatusRecordDTO.builder()
                                .evseStatus(item.getHubStatus())
                                .evseId(item.getEvseId())
                                .build())
                        .collect(Collectors.toList());
                //空闲枪数量
                Integer freeGunNum = stationHubjectFreeGunNum(collect);
                opLocationDetailVO.setFreeGunNum(freeGunNum);

                if (CollectionUtils.isNotEmpty(pullEvseDataRespDTO.getChargingFacilities())) {
                    //功率范围
                    Integer lowerPower = pullEvseDataRespDTO.getChargingFacilities().get(0).getPower() == null ? 0 : pullEvseDataRespDTO.getChargingFacilities().get(0).getPower();
                    Integer higherPower = pullEvseDataRespDTO.getChargingFacilities().get(0).getPower() == null ? 0 : pullEvseDataRespDTO.getChargingFacilities().get(0).getPower();
                    for (ChargingFacilityDTO chargingFacilityDTO1 : pullEvseDataRespDTO.getChargingFacilities()) {
                        power = chargingFacilityDTO1.getPower() == null ? 0 : chargingFacilityDTO1.getPower();
                        if (power < lowerPower) {
                            lowerPower = power;
                        }
                        if (power > higherPower) {
                            higherPower = power;
                        }
                    }
                    String powerRange = lowerPower + "-" + higherPower;
                    opLocationDetailVO.setPowerRange(powerRange);
                }
                //支付方式
                List<PaymentVO> paymentList = stationPaymentList();
                opLocationDetailVO.setPaymentList(paymentList);
                try {
                    //场站所在时区的时间
                    String timeZone = StringUtils.isBlank(opLocationElasticDTO.getTimeZone()) ? BaseConstant.UTC_8 : opLocationElasticDTO.getTimeZone();
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
                    opLocationDetailVO.setWeekDay(weekDay);
                    opLocationDetailVO.setHour(hour);
                    opLocationDetailVO.setMinute(minute);
                    String nowTimeStr = hour + ":" + minute;
                    DateFormat dateFormat = new SimpleDateFormat("HH:mm");
                    Date nowTime = dateFormat.parse(nowTimeStr);
                    log.info("nowDate:{}  zoneLocalDateTime:{}  weekDay:{}  hour:{}  minute:{}  nowTime:{}", nowDate, zoneLocalDateTime, weekDay, hour, minute, nowTime);
                    log.info(BaseConstant.NOW_TIME, nowTime);

                    //场站是否开放（休息中/营业）
                    if (null != pullEvseDataRespDTO.getOpen24Hours() && pullEvseDataRespDTO.getOpen24Hours()) {
                        opLocationDetailVO.setOpen(true);
                    } else {
                        if (CollectionUtils.isNotEmpty(pullEvseDataRespDTO.getOpeningTimes())) {
                            List<EvseDataRecordDTO.OpeningTimes> openingTimes = pullEvseDataRespDTO.getOpeningTimes();
                            openingTimes.forEach(time -> {
                                if (10 == time.getOn()) {
                                    boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                    if (b) {
                                        opLocationDetailVO.setOpen(true);
                                    }

                                } else if (9 == time.getOn()) {
                                    if (6 == weekDay || 7 == weekDay) {
                                        boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                        if (b) {
                                            opLocationDetailVO.setOpen(true);
                                        }
                                    }
                                } else if (8 == time.getOn()) {
                                    if (1 == weekDay || 2 == weekDay || 3 == weekDay || 4 == weekDay || 5 == weekDay) {
                                        boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                        if (b) {
                                            opLocationDetailVO.setOpen(true);
                                        }
                                    }
                                } else if (weekDay == time.getOn()) {
                                    boolean b = JudgeTimeUtils.timeIsInRound(nowTimeStr, time.getPeriods().getBegin(), time.getPeriods().getEnd());
                                    if (b) {
                                        opLocationDetailVO.setOpen(true);
                                    }
                                }
                            });
                        }
                    }
                    // todo 进场信息查询
                } catch (ParseException e) {
                    log.error("OpLocationRepositoryImpl appDetail", e);
                }

                // opLocationDetailVO.setThirdPile(1);
                opLocationDetailVO.setNoAdmittance(false);
                List<GunListPageVO> gunListPageVOS = Lists.newArrayList();
                List<AppDetailGunListVO> gunListVOList = Lists.newArrayList();
                List<AppDetailPileListVO> appDetailPileListVOList = Lists.newArrayList();

                for (PullEvseDataRespDTO pullEvseData : pullEvseDataRespDTOList) {
                    GunListPageVO gunListPageVO = new GunListPageVO();

                    if (StringUtils.isNotEmpty(pullEvseData.getEvseId())) {
                        gunListPageVO.setPileSn(pullEvseData.getEvseId());
                        gunListPageVO.setGunNo("1");
                    }
                    Integer gunTypeCode = ConnectorGunTypeEnum.CCS_COMBO_2.getCode();
                    if (CollectionUtils.isNotEmpty(pullEvseData.getPlugs()) && pullEvseData.getPlugs().get(0) != null) {
                        gunTypeCode = pullEvseData.getPlugs().get(0);
                    }
                    gunListPageVO.setGunType(gunTypeCode);

                    Integer gunPower = 0;
                    if (CollectionUtils.isNotEmpty(pullEvseData.getChargingFacilities())
                            && pullEvseData.getChargingFacilities().get(0).getPower() != null) {
                        gunPower = pullEvseData.getChargingFacilities().get(0).getPower();
                    }
                    gunListPageVO.setPower((double) gunPower);

                    if (CollectionUtils.isNotEmpty(pullEvseData.getPriceProductDataDtos())) {
                        String currencySign = getCurrencySign(pullEvseData.getPriceProductDataDtos().get(0).getPricingDefaultPriceCurrency());
                        gunListPageVO.setCurrencySign(currencySign);
                    }
                    //状态
                    Integer stateCodeByStatus = this.getStateCodeByStatus(pullEvseData.getHubStatus());
                    LocationEVSESAPPStatusEnum status = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(EvseDeviceStatusEnum.getEnumByCode(stateCodeByStatus).getName());
                    gunListPageVO.setStatus(status.getCode());
                    gunListPageVO.setState(status.getName());
                    gunListPageVOS.add(gunListPageVO);

                    AppDetailGunListVO gunVO = new AppDetailGunListVO();
                    gunVO.setEvseSn(pullEvseData.getEvseId() + "_1");
                    gunVO.setGunNo("1");
                    gunVO.setConnectorDisplayName(commonUtilService.getConnectorDisplayName(pullEvseData.getEvseId(), "1"));
                    gunVO.setGunType(gunTypeCode);
                    gunVO.setGunTypeName(ConnectorGunTypeEnum.getEnumByCode(gunTypeCode).getName());
                    gunVO.setPower((double) gunPower);
                    gunVO.setState(EvseDeviceStatusEnum.getEnumByCode(stateCodeByStatus).getName());
                    gunVO.setStateCode(stateCodeByStatus);
                    LocationEVSESAPPStatusEnum locationEVSESAPPStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(gunVO.getState());
                    gunVO.setAppEVSEStateCode(locationEVSESAPPStatusEnum.getCode());
                    //给每个枪排序 充电中的桩（枪）> 插枪未启动 > 高功率 > 可用 > 正在使用 > 故障
                    //                CHARGING     SUSPENDED_EVSE/SUSPENDED_EV   AVAILABLE   PREPARING/RESERVED   FAULTED       最后：FINISHING/UNAVAILABLE/DEFAULT
                    if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.CHARGING.getName())) {
                        gunVO.setSortNO(1);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EV.getName()) ||
                            Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EVSE.getName())) {
                        gunVO.setSortNO(2);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                        gunVO.setSortNO2(1);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.PREPARING.getName()) ||
                            Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.RESERVED.getName())) {
                        gunVO.setSortNO2(2);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.FAULTED.getName())) {
                        gunVO.setSortNO2(3);
                    } else {
                        gunVO.setSortNO2(4);
                    }
                    gunListVOList.add(gunVO);

                    //查询hubject计费规则

                    AppDetailPileListVO appDetailPileListVO = new AppDetailPileListVO();
                    List<PriceProductDataDto> priceProductDataDtoList = pullEvseData.getPriceProductDataDtos();
                    log.info("emp 拉取动态价格: {}", JSON.toJSONString(priceProductDataDtoList));
                    try {
                        this.setHubjectCostRule(appDetailPileListVO, priceProductDataDtoList);
                        appDetailPileListVO.setPileSn(pullEvseData.getEvseId());
                        appDetailPileListVO.setPower((double) gunPower);
                    } catch (Exception e) {
                        log.error("Hubject计费规则填充失败", e);
                    }

                    appDetailPileListVO.setGunList(Collections.singletonList(gunVO));
                    appDetailPileListVOList.add(appDetailPileListVO);
                }
                opLocationDetailVO.setPileList(appDetailPileListVOList);
                Collections.sort(gunListPageVOS);
                opLocationDetailVO.setGunList(gunListPageVOS);

//                opLocationDetailVO.setAccessibility(this.getAccessibility(pullEvseDataRespDTO.getAccessibility()));
                //周边设施

            }
        } catch (Exception e) {
            log.error("查询Hubject设备信息异常", e);
        }
        return opLocationDetailVO;
    }

    private Integer getAccessibility(Integer hubAccessibility) {
        Integer autelAccessibility = 2;
        if (hubAccessibility == 1 || hubAccessibility == 3) {
            autelAccessibility = hubAccessibility;
        }
        return autelAccessibility;
    }

    private void setHubjectCostRule(AppDetailPileListVO appDetailPileListVO, List<PriceProductDataDto> priceProductDataDtoList) {
        CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
        if (CollectionUtils.isNotEmpty(priceProductDataDtoList)) {
            HashMap<Integer, List<CostRulesDTO>> weekRule = new HashMap<>(16);

            PriceProductDataDto priceProductDataDto = priceProductDataDtoList.get(0);
            List<PricingProductDataRecordDto> productDataRecordDtoList = priceProductDataDtoList.stream()
                    .map(PriceProductDataDto::getPricingProductDataRecordDtoList)
                    .filter(CollectionUtils::isNotEmpty)
                    .flatMap(Collection::stream)
                    .collect(Collectors.toList());

            if (CollectionUtils.isNotEmpty(productDataRecordDtoList)) {
                productDataRecordDtoList.forEach(item -> item.getProductAvailabilityTimeDtoList()
                        .forEach(timeDto -> {
                            List<Integer> weeks = new ArrayList<>();
                            if (timeDto.getOn() == 8) {
                                weeks.add(1);
                                weeks.add(2);
                                weeks.add(3);
                                weeks.add(4);
                                weeks.add(5);
                            } else if (timeDto.getOn() == 9) {
                                weeks.add(6);
                                weeks.add(7);
                            } else if (timeDto.getOn() == 10) {
                                weeks.add(1);
                                weeks.add(2);
                                weeks.add(3);
                                weeks.add(4);
                                weeks.add(5);
                                weeks.add(6);
                                weeks.add(7);
                            } else {
                                weeks.add(timeDto.getOn());
                            }
                            CostRulesDTO costRulesDTO = new CostRulesDTO();
                            String begin = timeDto.getBegin();
                            String end = timeDto.getEnd();
                            if (StringUtils.isNotBlank(begin)) {
                                costRulesDTO.setBeginHour(Integer.valueOf(begin.substring(0, 2)));
                                costRulesDTO.setBeginMinute(Integer.valueOf(begin.substring(3, 5)));
                            }
                            if (StringUtils.isNotBlank(end)) {
                                costRulesDTO.setEndHour(Integer.valueOf(end.substring(0, 2)));
                                costRulesDTO.setEndMinute(Integer.valueOf(end.substring(3, 5)));
                            }
                            costRulesDTO.setEnergyUnit(item.getReferenceUnit());
                            if (item.getReferenceUnit() == 3) {
                                costRulesDTO.setUnitPrice(item.getPricePerReferenceUnit());
                            } else {
                                costRulesDTO.setTimePrice(item.getPricePerReferenceUnit());
                            }

                            item.getAdditionalReferenceDtoList().forEach(aRD -> {
                                if (aRD.getAdditionRef() == 3) {
                                    costRulesDTO.setParkingPrice(aRD.getPricePerAdditionalReferenceUnit());
                                    costRulesDTO.setParkingUnit(aRD.getAdditionalReferenceUnit());
                                }
                                if (aRD.getAdditionRef() == 1) {
                                    costRulesDTO.setStartPrice(aRD.getPricePerAdditionalReferenceUnit());
                                }
                            });

                            weeks.forEach(week -> {
                                List<CostRulesDTO> orDefault = weekRule.getOrDefault(week, new ArrayList<>());
                                orDefault.add(costRulesDTO);
                                weekRule.put(week, orDefault);
                            });
                            weeks.clear();
                        }));
            } else {
                List<Integer> weeks = Lists.newArrayList(1, 2, 3, 4, 5, 6, 7);
                CostRulesDTO costRulesDTO = new CostRulesDTO();
                costRulesDTO.setEnergyUnit(priceProductDataDto.getPricingDefaultReferenceUnit());
                if (priceProductDataDto.getPricingDefaultReferenceUnit() == 3) {
                    costRulesDTO.setUnitPrice(priceProductDataDto.getPricingDefaultPrice());
                } else {
                    costRulesDTO.setTimePrice(priceProductDataDto.getPricingDefaultPrice());
                }

                costRulesDTO.setBeginHour(Integer.valueOf("00"));
                costRulesDTO.setBeginMinute(Integer.valueOf("00"));
                costRulesDTO.setEndHour(Integer.valueOf("24"));
                costRulesDTO.setEndMinute(Integer.valueOf("00"));
                costRulesDTO.setBeginTime("00:00");
                costRulesDTO.setEndTime("24:00");

                weeks.forEach(week -> {
                    List<CostRulesDTO> orDefault = weekRule.getOrDefault(week, new ArrayList<>());
                    orDefault.add(costRulesDTO);
                    weekRule.put(week, orDefault);
                });
                weeks.clear();
            }

            List<CostRuleWeeksDTO> rules = Lists.newArrayList();
            weekRule.forEach((week, value) -> {
                CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
                costRuleWeeksDTO.setWeeks(Collections.singletonList(week));
                costRuleWeeksDTO.setWeeksRules(value);
                costRuleWeeksDTO.setFeeModel(2);
                if (CollectionUtils.isNotEmpty(value) && value.get(0).getEnergyUnit() == 3) {
                    costRuleWeeksDTO.setFeeModel(1);
                }
                rules.add(costRuleWeeksDTO);
            });
            costModelRuleDTO.setRules(rules);
            costModelRuleDTO.setCurrencyType(priceProductDataDto.getPricingDefaultPriceCurrency());
        }
        costModelRuleDTO.setRuleModelType(3);
        //hubject停车计费合并
        appDetailPileListVO.setParkCostModelRuleDTO(costModelRuleDTO);
    }

    private Integer getStateCodeByStatus(int stateCode) {

        if (stateCode == 1) {
            stateCode = EvseDeviceStatusEnum.AVAILABLE.getCode();
        } else if (stateCode == 2) {
            stateCode = EvseDeviceStatusEnum.RESERVED.getCode();
        } else if (stateCode == 3) {
            stateCode = EvseDeviceStatusEnum.CHARGING.getCode();
        } else if (stateCode == 4) {
            stateCode = EvseDeviceStatusEnum.FAULTED.getCode();
        } else if (stateCode == 5) {
            stateCode = EvseDeviceStatusEnum.UNAVAILABLE.getCode();
        } else if (stateCode == 0) {
            stateCode = EvseDeviceStatusEnum.DEFAULT.getCode();
        }
        return stateCode;
    }

    private List<GunPowerAndTypeGroupVO> buildHubGunPowerAndTypeGroupVOList(PullEvseDataRespDTO pullEvseDataRespDTO, Integer freeGunNum) {
        List<GunPowerAndTypeGroupVO> groupVOS = Lists.newArrayList();
        GunPowerAndTypeGroupVO gunPowerAndTypeGroupVO = new GunPowerAndTypeGroupVO();
        if (CollectionUtils.isNotEmpty(pullEvseDataRespDTO.getPlugs())) {
            gunPowerAndTypeGroupVO.setTotalCount(1);
            gunPowerAndTypeGroupVO.setGunTypeCode(pullEvseDataRespDTO.getPlugs().get(0));
            gunPowerAndTypeGroupVO.setFreeCount(freeGunNum);
            gunPowerAndTypeGroupVO.setPower(Double.valueOf(pullEvseDataRespDTO.getChargingFacilities().get(0).getPower()));
        }
        groupVOS.add(gunPowerAndTypeGroupVO);
        return groupVOS;
    }

    /**
     * APP根据站点ID获取枪列表
     *
     * @param gunListPageDTO 检索对象
     * @return 枪列表
     */
    @Override
    public Page<GunListPageVO> getGunListByStationId(GunListPageDTO gunListPageDTO) {

        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        //查询对象
        NativeSearchQuery nativeSearchQuery = new NativeSearchQuery(boolQueryBuilder);
        //设置分页  没有设置默认(0,10)
        PageRequest pageRequest = PageRequest.of(gunListPageDTO.getCurrent() - 1, gunListPageDTO.getPageSize());
        nativeSearchQuery.setPageable(pageRequest);
        //查询
//        org.springframework.data.domain.Page<OpLocationEvseElasticDTO> evsePage = opLocationEvseElastic.search(nativeSearchQuery);
        SearchHits<OpLocationEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationEvseElasticDTO.class);
        List<OpLocationEvseElasticDTO> evsePage = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());


        List<GunListPageVO> currentPageGunList = new ArrayList<>();

        //枪设备分页查询
        if (CollectionUtils.isNotEmpty(evsePage)) {
            for (OpLocationEvseElasticDTO esEVSE : evsePage) {
                GunListPageVO gunListPageVO = DozerConvert.map(esEVSE, GunListPageVO.class);
                if (StringUtils.isNotBlank(esEVSE.getEvseSn()) && esEVSE.getEvseSn().contains("_")) {
                    gunListPageVO.setGunNo(esEVSE.getEvseSn().split("_")[1]);
                }
                gunListPageVO.setStatus(EvseDeviceStatusEnum.getEnumByName(esEVSE.getState()).getCode());
                currentPageGunList.add(gunListPageVO);
            }
        }
        Page<GunListPageVO> resultPage = new Page<>(gunListPageDTO.getCurrent(), gunListPageDTO.getPageSize());
        resultPage.setRecords(currentPageGunList);
        resultPage.setTotal(searchHits.getTotalHits());
        return resultPage;
    }

    //站点支付方式
    private List<PaymentVO> stationPaymentList() {
        List<PaymentVO> paymentVOS = new ArrayList<>();
        return paymentVOS;
    }

    private Integer stationFreeGunNum(List<OpLocationEvseElasticDTO> esEVSEList) {
        int freeGunNum = 0;
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(esEVSEList)) {
            for (OpLocationEvseElasticDTO esEVSE : esEVSEList) {
                if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                    freeGunNum++;
                }
            }
        }
        return freeGunNum;
    }

    private Integer stationHubjectFreeGunNum(List<EvseStatusRecordDTO> esEVSEList) {
        int freeGunNum = 0;
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(esEVSEList)) {
            for (EvseStatusRecordDTO esEVSE : esEVSEList) {
                if (Objects.equals(esEVSE.getEvseStatus(), EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                    freeGunNum++;
                }
            }
        }
        return freeGunNum;
    }

    //站点其它服务
    private List<OtherServiceVO> stationOtherService() {
        List<OtherServiceVO> otherServiceVOList = new ArrayList<>();
        OtherServiceVO otherServiceVO = new OtherServiceVO();
        otherServiceVOList.add(otherServiceVO);
        return otherServiceVOList;
    }

    private OpLocationDTO copyLocationEntities(Long locationId, OpLocationDTO opLocationDTO) {
        LambdaQueryWrapper<OpLocationEntity> query = Wrappers.lambdaQuery(OpLocationEntity.class)
                .eq(OpLocationEntity::getId, locationId)
                .eq(OpLocationEntity::getDeleted, 0);
        OpLocationEntity opLocationEntity = this.getBaseMapper().selectOne(query);
        OpLocationDTO op = new OpLocationDTO();
        if (opLocationEntity != null && opLocationEntity.getId() != null) {
            op = OpLocationConvert.opLocationDTO(opLocationDTO, opLocationEntity);
            op.setId(opLocationEntity.getId());
        }
        return op;
    }

    //获取场站ImageEntity实体类
    private Map<Long, List<OpImageEntity>> getStationIdOpImageEntity(Long stationId) {
        Map<Long, List<OpImageEntity>> stationIdImageEntityMap = new HashMap<>();
        QueryWrapper<OpLocationImageEntity> locationImageEntityQueryWrapper = new QueryWrapper<>();
        locationImageEntityQueryWrapper.eq("location_id", stationId);
        locationImageEntityQueryWrapper.eq("deleted", 0);
        locationImageEntityQueryWrapper.select("image_id", "location_id");
        List<OpLocationImageEntity> opLocationImageEntities = opLocationImageMapper.selectList(locationImageEntityQueryWrapper);
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationImageEntities)) {
            List<Long> imageIds = opLocationImageEntities.stream().map(OpLocationImageEntity::getImageId).collect(Collectors.toList());
            List<OpImageEntity> opImageEntities = opImageMapper.selectBatchIds(imageIds);
            log.info("图片集合opImageEntities：{}", JSONArray.toJSONString(opImageEntities));
            for (OpLocationImageEntity opLocationImageEntity : opLocationImageEntities) {
                List<OpImageEntity> stationImageEntities = stationIdImageEntityMap.get(opLocationImageEntity.getLocationId());
                if (stationImageEntities == null) {
                    stationImageEntities = new ArrayList<>();
                }
                for (OpImageEntity opImageEntity : opImageEntities) {
                    if (Objects.equals(opImageEntity.getId(), opLocationImageEntity.getImageId())) {
                        stationImageEntities.add(opImageEntity);
                    }
                }
                stationIdImageEntityMap.put(opLocationImageEntity.getLocationId(), stationImageEntities);
            }
        }
        return stationIdImageEntityMap;
    }

    //获取场站图片路径
    private List<String> getStationLogoPath(Long stationId) {
        Map<Long, List<OpImageEntity>> stationIdOpImageEntity = getStationIdOpImageEntity(stationId);
        log.info("stationIdOpImageEntity:{}", JSONObject.toJSONString(stationIdOpImageEntity));
        List<OpImageEntity> opImageEntities = stationIdOpImageEntity.get(stationId);
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opImageEntities)) {
            return opImageEntities.stream().map(OpImageEntity::getUrl).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    @Override
    public Integer JudgeOpenType() {

        LambdaQueryWrapper<OpLocationOperationEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationOperationEntity.class);
        queryWrapper.eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE);
        queryWrapper.eq(OpLocationOperationEntity::getOpenType, OperationOpenTypeEnum.CLOSE.getCode());
        queryWrapper.le(OpLocationOperationEntity::getOperationDate, Instant.now().toEpochMilli());
        List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(queryWrapper);

        //先更新mysql
        LambdaUpdateWrapper<OpLocationOperationEntity> updateWrapper = Wrappers.lambdaUpdate(OpLocationOperationEntity.class);
        updateWrapper.eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE);
        updateWrapper.eq(OpLocationOperationEntity::getOpenType, OperationOpenTypeEnum.CLOSE.getCode());
        updateWrapper.le(OpLocationOperationEntity::getOperationDate, Instant.now().toEpochMilli());
        updateWrapper.set(OpLocationOperationEntity::getOpenType, OperationOpenTypeEnum.OPEN.getCode());
        int update = opLocationOperationMapper.update(null, updateWrapper);
        //再更新es
        if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
            opLocationOperationEntities.forEach(e -> {
                //将数据存入es
                OpLocationEntity opLocationEntity = this.selectOpLocationEntityById(e.getLocationId());
                OpLocationElasticDTO opLocationElasticDTO = OpLocationElasticConvert.toOpLocationElasticDTO(opLocationEntity, e);
                opLocationElasticDTO.setOpenType(OperationOpenTypeEnum.OPEN.getCode());
                opLocationElasticDTO.setPlatform(1);
                OpLocationElasticDTO elasticDTO = opLocationElastic.save(opLocationElasticDTO);
                log.info("OpLocationRepositoryImpl.JudgeOpenType.elasticDTO={}", elasticDTO);
            });
        }
        return update;
    }

    /**
     * 同步场站
     *
     * @param opLocationEntity
     * @return
     */
    @Override
    @Async
    public void syncLocation(OpLocationEntity opLocationEntity) {
        StopWatch stopWatch = new StopWatch("同步场站信息");
        stopWatch.start("查询场站信息");
        log.info("syncLocation start and locationId = " + opLocationEntity.getId());
        Long locationId = opLocationEntity.getId();
        LambdaQueryWrapper<OpLocationOperationEntity> query = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                .eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationOperationEntity::getLocationId, locationId);
        OpLocationOperationEntity opLocationOperationEntity = this.opLocationOperationMapper.selectOne(query);
        if (opLocationOperationEntity == null) {
            log.info("syncLocation opLocationOperationEntity is null ");
            return;
        }
        stopWatch.stop();
        stopWatch.start("同步场站信息");
        OpLocationElasticDTO locationElasticDto = this.opLocationElastic.findById(locationId).orElse(new OpLocationElasticDTO());
        this.pileBaseConvert.handleRelation(locationElasticDto, opLocationEntity, opLocationOperationEntity);

        opLocationElastic.save(locationElasticDto);
        stopWatch.stop();
        stopWatch.start("同步场站桩信息");
        // 通过场站获取桩
        LambdaQueryWrapper<OpLocationPileEvseEntity> evsePileQuery = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                .eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationPileEvseEntity::getLocationId, locationId);
        List<OpLocationPileEvseEntity> pileEntityList = this.opLocationPileEvseMapper.selectList(evsePileQuery);
        log.info("syncLocation and pileEntityList = " + JSON.toJSONString(pileEntityList));
        if (CollectionUtils.isEmpty(pileEntityList)) {
            log.info("syncLocation  pileEntityList is empty");
            return;
        }

        List<IndexQuery> pileUpdateList = new ArrayList<>();
        List<IndexQuery> evseUpdateList = new ArrayList<>();

        pileEntityList.stream().forEach(pileEntity -> {
            List<Long> evseIds = JSON.parseArray(pileEntity.getEvseList(), Long.class);
            if (CollectionUtils.isEmpty(evseIds)) {
                return;
            }
            //同步桩信息
            OpLocationPileEvseElasticDTO pileElasticDto = this.opLocationPileEvseElastic.findById(pileEntity.getId()).orElse(new OpLocationPileEvseElasticDTO());

            //同步枪信息
            LambdaQueryWrapper<OpLocationEvseEntity> evseQuery = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                    .eq(OpLocationEvseEntity::getLocationId, locationId)
                    .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE)
                    .in(OpLocationEvseEntity::getId, evseIds);
            List<OpLocationEvseEntity> evseEntityList = opLocationEvseMapper.selectList(evseQuery);
            log.info("syncLocation and evseEntityList = " + JSON.toJSONString(evseEntityList));
            if (CollectionUtils.isEmpty(evseEntityList)) {
                return;
            }
            evseEntityList.stream().forEach(evseEntity -> {
                LambdaQueryWrapper<OpLocationConnectorEntity> evseConnectorQuery = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                        .eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE)
                        .eq(OpLocationConnectorEntity::getStatus, 1)
                        .eq(OpLocationConnectorEntity::getLocationEvseId, evseEntity.getId());
                OpLocationConnectorEntity connectorEntity = opLocationConnectorMapper.selectOne(evseConnectorQuery);
                OpLocationEvseElasticDTO evseElasticDto = this.opLocationEvseElastic.findById(evseEntity.getId()).orElse(new OpLocationEvseElasticDTO());
                this.pileBaseConvert.handleRelation(evseElasticDto, evseEntity, pileEntity, opLocationEntity, connectorEntity);
                this.pileBaseConvert.handleRelation(pileElasticDto, pileEntity, evseEntity, opLocationEntity, connectorEntity);
                evseUpdateList.add(new IndexQueryBuilder().withId(evseElasticDto.getId().toString()).withObject(evseElasticDto).build());
            });
            pileUpdateList.add(new IndexQueryBuilder().withId(pileElasticDto.getId().toString()).withObject(pileElasticDto).build());
        });

        if (evseUpdateList.size() > 0) {
            //批量保存
            this.elasticsearchRestTemplate.bulkIndex(evseUpdateList, OpLocationEvseElasticDTO.class);
            //立即刷新
            this.elasticsearchRestTemplate.indexOps(OpLocationEvseElasticDTO.class).refresh();
        }
        if (pileUpdateList.size() > 0) {
            //批量保存
            this.elasticsearchRestTemplate.bulkIndex(pileUpdateList, OpLocationPileEvseElasticDTO.class);
            //立即刷新
            this.elasticsearchRestTemplate.indexOps(OpLocationPileEvseElasticDTO.class).refresh();
        }

        log.info("syncLocation end and locationId = " + locationId);
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
    }

    @Override
    public Integer synchronizationData(List<Long> sellerIds) {
        log.info("synchronizationData,sellerIds={}",JSON.toJSONString(sellerIds));
        try {
            // 获取场站
            LambdaQueryWrapper<OpLocationEntity> locationQuery = Wrappers.lambdaQuery(OpLocationEntity.class)
                    .eq(OpLocationEntity::getDeleted, Boolean.FALSE);
            if (CollectionUtils.isNotEmpty(sellerIds)) {
                locationQuery.in(OpLocationEntity::getOperatorId, sellerIds);
            }
            List<OpLocationEntity> opLocationEntityList = this.getBaseMapper().selectList(locationQuery);
            Map<Long, OpLocationEntity> opLocationEntityMap = new HashMap<>();
            if (CollectionUtils.isEmpty(opLocationEntityList)) {
                log.info("synchronizationData,opLocationEntityList is empty. ");
                return 0;
            }
            List<Long> locationIds = opLocationEntityList.stream().map(OpLocationEntity::getId).collect(Collectors.toList());
            //----------------------场站数据同步 开始----------------------------------
            LambdaQueryWrapper<OpLocationOperationEntity> query = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                    .eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE)
                    .in(OpLocationOperationEntity::getLocationId, locationIds);
            List<OpLocationOperationEntity> operationEntityList = this.opLocationOperationMapper.selectList(query);
            if (CollectionUtils.isEmpty(operationEntityList)) {
                log.info("synchronizationData,operationEntityList is empty. ");
                return 0;
            }
            List<IndexQuery> locationUpdateList = new ArrayList<>();
            Map<Long, OpLocationElasticDTO> locationDtoMap = new HashMap<>();
            Map<Long, OpLocationOperationEntity> operationEntityMap = operationEntityList.stream().collect(toMap(OpLocationOperationEntity::getLocationId, e -> e, (f, s) -> f));
            Iterator<OpLocationElasticDTO> iterator = this.opLocationElastic.findAllById(locationIds).iterator();
            while (iterator.hasNext()) {
                OpLocationElasticDTO next = iterator.next();
                locationDtoMap.put(next.getId(), next);
            }
            if (org.springframework.util.CollectionUtils.isEmpty(locationDtoMap)) {
                log.info("synchronizationData,locationDtoMap is empty. ");
            }
            opLocationEntityList.stream().forEach(locationEntity -> {
                Long locationId = locationEntity.getId();
                opLocationEntityMap.put(locationId, locationEntity);
                OpLocationOperationEntity opLocationOperationEntity = operationEntityMap.get(locationId);
                if (opLocationOperationEntity == null) {
                    return;
                }
                OpLocationElasticDTO locationElasticDto = Optional.ofNullable(locationDtoMap.get(locationId)).orElse(new OpLocationElasticDTO());
                this.pileBaseConvert.handleRelation(locationElasticDto, locationEntity, opLocationOperationEntity);
                locationUpdateList.add(new IndexQueryBuilder().withId(locationElasticDto.getId().toString()).withObject(locationElasticDto).build());
            });
            if (locationUpdateList.size() > 0) {
                //批量保存
                this.elasticsearchRestTemplate.bulkIndex(locationUpdateList, OpLocationElasticDTO.class);
                //立即刷新
                this.elasticsearchRestTemplate.indexOps(OpLocationElasticDTO.class).refresh();
            }
            //----------------------场站数据同步 结束----------------------------------
            int count = locationUpdateList.size();
            //----------------------桩枪数据同步 开始----------------------------------
            // 通过场站获取桩
            LambdaQueryWrapper<OpLocationPileEvseEntity> evsePileQuery = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                    .eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE)
                    .in(OpLocationPileEvseEntity::getLocationId, locationIds);
            List<OpLocationPileEvseEntity> pileEntityList = this.opLocationPileEvseMapper.selectList(evsePileQuery);
            if (CollectionUtils.isEmpty(pileEntityList)) {
                log.info("synchronizationData,pileEntityList is empty. ");
                return count;
            }
            log.info("synchronizationData,pileEntityList count={}", pileEntityList.size());

            List<IndexQuery> pileUpdateList = new ArrayList<>();
            List<IndexQuery> evseUpdateList = new ArrayList<>();

            Map<Long, OpLocationPileEvseElasticDTO> pileEvseDtoMap = new HashMap<>();
            Iterator<OpLocationPileEvseElasticDTO> it = this.opLocationPileEvseElastic.findAllById(pileEntityList.stream().map(OpLocationPileEvseEntity::getId).collect(Collectors.toList())).iterator();
            while (it.hasNext()) {
                OpLocationPileEvseElasticDTO next = it.next();
                pileEvseDtoMap.put(next.getId(), next);
            }
            List<Long> evseIds = new ArrayList<>();
            pileEntityList.stream().forEach(e -> {
                String tmp = e.getEvseList();
                if (StringUtils.isEmpty(tmp)) {
                    return;
                }
                List<Long> tmpIds = JSON.parseArray(tmp, Long.class);
                if (CollectionUtils.isEmpty(tmpIds)) {
                    return;
                }
                evseIds.addAll(tmpIds);
            });
            LambdaQueryWrapper<OpLocationEvseEntity> evseQuery = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                    .in(OpLocationEvseEntity::getLocationId, locationIds)
                    .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE)
                    .in(OpLocationEvseEntity::getId, evseIds);
            List<OpLocationEvseEntity> evseEntityList = this.opLocationEvseMapper.selectList(evseQuery);
            if (CollectionUtils.isEmpty(evseEntityList)) {
                log.info("synchronizationData,evseEntityList is empty. ");
                return count;
            }
            log.info("synchronizationData,evseEntityList count={}", evseEntityList.size());
            Iterator<OpLocationEvseElasticDTO> ite = this.opLocationEvseElastic.findAllById(evseIds).iterator();
            Map<Long, OpLocationEvseElasticDTO> evseDtoMap = new HashMap<>();
            while (ite.hasNext()) {
                OpLocationEvseElasticDTO next = ite.next();
                evseDtoMap.put(next.getId(), next);
            }
            Map<Long, OpLocationEvseEntity> evseEntityMap = evseEntityList.stream().collect(toMap(OpLocationEvseEntity::getId, e -> e, (f, s) -> f));
            LambdaQueryWrapper<OpLocationConnectorEntity> evseConnectorQuery = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                    .eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE)
                    .eq(OpLocationConnectorEntity::getStatus, 1)
                    .in(OpLocationConnectorEntity::getLocationEvseId, evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toList()));
            List<OpLocationConnectorEntity> connectorEntityList = this.opLocationConnectorMapper.selectList(evseConnectorQuery);
            Map<Long, OpLocationConnectorEntity> connectorEntityMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(connectorEntityList)) {
                connectorEntityList.stream().forEach(e -> connectorEntityMap.put(e.getLocationEvseId(), e));
            }
            pileEntityList.stream().forEach(pileEntity -> {
                Long locationId = pileEntity.getLocationId();
                List<Long> tmpEvseIds = JSON.parseArray(pileEntity.getEvseList(), Long.class);
                if (CollectionUtils.isEmpty(tmpEvseIds)) {
                    return;
                }
                OpLocationEntity opLocationEntity = opLocationEntityMap.get(locationId);
                //同步桩信息
                OpLocationPileEvseElasticDTO pileElasticDto = Optional.ofNullable(pileEvseDtoMap.get(pileEntity.getId())).orElse(new OpLocationPileEvseElasticDTO());

                //同步枪信息
                tmpEvseIds.stream().forEach(evseId -> {
                    OpLocationEvseEntity evseEntity = evseEntityMap.get(evseId);
                    if (evseEntity == null) {
                        return;
                    }
                    OpLocationConnectorEntity connectorEntity = connectorEntityMap.get(evseId);
                    if (connectorEntity == null) {
                        return;
                    }
                    OpLocationEvseElasticDTO evseElasticDto = Optional.ofNullable(evseDtoMap.get(evseId)).orElse(new OpLocationEvseElasticDTO());
                    this.pileBaseConvert.handleRelation(evseElasticDto, evseEntity, pileEntity, opLocationEntity, connectorEntity);
                    this.pileBaseConvert.handleRelation(pileElasticDto, pileEntity, evseEntity, opLocationEntity, connectorEntity);
                    evseUpdateList.add(new IndexQueryBuilder().withId(evseElasticDto.getId().toString()).withObject(evseElasticDto).build());
                });
                if (pileElasticDto.getId() != null) {
                    pileUpdateList.add(new IndexQueryBuilder().withId(pileElasticDto.getId().toString()).withObject(pileElasticDto).build());
                }
            });

            if (evseUpdateList.size() > 0) {
                //批量保存
                this.elasticsearchRestTemplate.bulkIndex(evseUpdateList, OpLocationEvseElasticDTO.class);
                //立即刷新
                this.elasticsearchRestTemplate.indexOps(OpLocationEvseElasticDTO.class).refresh();
            }
            if (pileUpdateList.size() > 0) {
                //批量保存
                this.elasticsearchRestTemplate.bulkIndex(pileUpdateList, OpLocationPileEvseElasticDTO.class);
                //立即刷新
                this.elasticsearchRestTemplate.indexOps(OpLocationPileEvseElasticDTO.class).refresh();
            }
            //----------------------桩枪数据同步 结束----------------------------------
            return count;
        } catch (Exception e) {
            log.error("OpLocationRepositoryImpl synchronizationData and exception = ", e);
            return 0;
        }
    }

    @Override
    public Boolean synchronizationOnlyData() {
        try {
            // 获取场站
            LambdaQueryWrapper<OpLocationEntity> locationQuery = Wrappers.lambdaQuery(OpLocationEntity.class)
                    .eq(OpLocationEntity::getDeleted, Boolean.FALSE);
            List<OpLocationEntity> opLocationEntityList = this.getBaseMapper().selectList(locationQuery);
            opLocationEntityList.forEach(this::syncLocation);
            return Boolean.TRUE;
        } catch (Exception e) {
            log.error("OpLocationRepositoryImpl synchronizationOnlyData and exception = ", e);
            return Boolean.FALSE;
        }
    }

    @Override
    public Boolean synchronizationLocation(Long id) {
        try {
            OpLocationEntity opLocationEntity = this.getBaseMapper().selectById(id);
            if (opLocationEntity != null) {
                syncLocation(opLocationEntity);
            }
            return Boolean.TRUE;
        } catch (Exception e) {
            log.error("OpLocationRepositoryImpl synchronizationLocation and exception = ", e);
            return Boolean.FALSE;
        }
    }

    /**
     * 获取时区列表
     *
     * @return 时区列表
     */
    @Override
    public List<TimeZoneVO> getTimeZoneList() {
        List<TimeZoneInfoDTO> zoneList = TimeZoneInfoDTO.getTimeZoneList();
        Map<String, List<TimeZoneInfoDTO>> timeZoneMap = zoneList.stream().collect(Collectors.groupingBy(TimeZoneInfoDTO::getTimeZone));
        List<TimeZoneVO> timeZoneList = TimeZoneEnum.timeZoneList();
        timeZoneList.forEach(item -> {
            List<TimeZoneInfoDTO> infoList = timeZoneMap.get(item.getCode());
            if (CollectionUtils.isNotEmpty(infoList)) {
                item.setZoneIdList(infoList.stream().map(TimeZoneInfoDTO::getZoneId).collect(Collectors.toList()));
            }
        });
        return timeZoneList;
    }

    @Override
    public OpLocationElasticDTO getDetailsFromEsById(Long id) {

        log.info("===>>>OpLocationRepositoryImpl.getDetailsFromEsById id : {}", JSON.toJSONString(id));

        if (id == null) {
            return null;
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("id", id));

        Iterable<OpLocationElasticDTO> iterable =
//                opLocationElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        AtomicReference<OpLocationElasticDTO> opLocationElasticDTO = new AtomicReference<>(new OpLocationElasticDTO());
        iterable.forEach(opLocationElasticDTO::set);

        return opLocationElasticDTO.get();
    }

    /**
     * 场站列表对桩集合进行排序
     *
     * @param pileSortDTO 排序对象
     * @return 桩集合
     */
    @Override
    public List<PilePageVO> sortPile(PileSortDTO pileSortDTO) {
        List<PilePageVO> pilePageVOList = pileSortDTO.getPilePageVOList();
        String[] sortNameArr = pileSortDTO.getSortNameArr();
        Boolean[] isAscArr = pileSortDTO.getAscArr();
        List<PileSortBO> pileSortBOS = DozerConvert.mapList(pilePageVOList, PileSortBO.class);
        if (sortNameArr.length > 0) {
            for (int i = sortNameArr.length - 1; i >= 0; i--) {
                String sortName = sortNameArr[i];
                //安装枪状态排序
                if (Objects.equals(PileSortEnum.FIRSTGUNSTATUS.getName(), sortName)) {
                    //正序、倒序
                    Boolean firstGunStatusSortAsc = isAscArr[i];
                    pileSortBOS.forEach(pileSortBO -> {
                        //设置正序倒序
                        pileSortBO.setFirstGunStatusSortAsc(firstGunStatusSortAsc);
                        //设置状态排序顺序
                        pileSortBO.firstGunStatusNO();
                    });
                }
            }
        }
        ListSortUtil.sort(pileSortBOS, sortNameArr, isAscArr);
        log.info("拥有第一个枪状态排序字段的桩集合：{}", JSON.toJSONString(pileSortBOS));
        return DozerConvert.mapList(pileSortBOS, PilePageVO.class);
    }

    /**
     * 更新场站公告
     *
     * @param opLocationAnnouncementUpdateDTO 更新对象
     */
    @Override
    public void updateAnnouncement(OpLocationAnnouncementUpdateDTO opLocationAnnouncementUpdateDTO) {
        long now = new Date().getTime();
        //es
        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(opLocationAnnouncementUpdateDTO.getId());
        if (optionalOpLocationElasticDTO.isPresent()) {
            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
            opLocationElasticDTO.setAnnouncement(opLocationAnnouncementUpdateDTO.getAnnouncement());
            opLocationElasticDTO.setAnnouncementAt(now);
            opLocationElastic.save(opLocationElasticDTO);
        }
        //mysql
        QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
        opLocationOperationEntityQueryWrapper.eq(BaseConstant.LOCATION_ID, opLocationAnnouncementUpdateDTO.getId());
        List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(opLocationOperationEntityQueryWrapper);
        if (CollectionUtils.isNotEmpty(opLocationOperationEntities)) {
            OpLocationOperationEntity opLocationOperationEntity = opLocationOperationEntities.get(0);
            opLocationOperationEntity.setAnnouncement(opLocationAnnouncementUpdateDTO.getAnnouncement());
            opLocationOperationEntity.setAnnouncementAt(now);
            opLocationOperationMapper.updateById(opLocationOperationEntity);
        }
    }

    private List<PowerGroupVO> buildStationPowerGroupVOList(List<OpLocationEvseElasticDTO> opLocationESEVSEList) {
        Map<Double, Integer> powerGunTotalNumMap = new HashedMap();
        Map<Double, Integer> powerGunFreeNumMap = new HashedMap();
        List<PowerGroupVO> powerGroupVOS = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(opLocationESEVSEList)) {
            for (OpLocationEvseElasticDTO esEVSEDTO : opLocationESEVSEList) {
                Double power = esEVSEDTO.getPower();
                if (power == null) {
                    power = 0.00;
                }
                Integer powerTotalNum = powerGunTotalNumMap.get(power);
                if (powerTotalNum == null) {
                    powerTotalNum = 0;
                }
                powerTotalNum++;
                powerGunTotalNumMap.put(power, powerTotalNum);
                if (StringUtils.isNotBlank(esEVSEDTO.getState()) && Objects.equals(esEVSEDTO.getState(), String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                    Integer powerFreeNum = powerGunFreeNumMap.get(power);
                    if (powerFreeNum == null) {
                        powerFreeNum = 0;
                    }
                    powerFreeNum++;
                    powerGunFreeNumMap.put(power, powerFreeNum);
                }
            }
            for (Map.Entry<Double, Integer> entry : powerGunTotalNumMap.entrySet()) {
                Double power = entry.getKey();
                Integer powerTotalNum = entry.getValue();
                PowerGroupVO powerGroupVO = new PowerGroupVO();
                powerGroupVO.setPower(power);
                powerGroupVO.setTotalPileNum(powerTotalNum);
                Integer powerFreeNum = powerGunFreeNumMap.get(power);
                powerFreeNum = powerFreeNum == null ? 0 : powerFreeNum;
                powerGroupVO.setFreePileNum(powerFreeNum);
                powerGroupVOS.add(powerGroupVO);
            }
        }
        Collections.sort(powerGroupVOS);
        return powerGroupVOS;
    }

    /**
     * 不同功率枪数统计
     *
     * @param esEVSEList
     * @return
     */
    private List<PowEvseVO> buildStationPowEvseVOList(List<OpLocationEvseElasticDTO> esEVSEList) {
        Map<Double, Integer> powerEvseTotalNumMap = new HashedMap();
        Map<Double, Integer> powerEvseFreeNumMap = new HashedMap();
        List<PowEvseVO> powEvseVOList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            esEVSEList.forEach(evse -> {
                Double power = evse.getPower();
                if (power == null) {
                    power = 0.00;
                }
                Integer powerTotalNum = powerEvseTotalNumMap.get(power);
                if (powerTotalNum == null) {
                    powerTotalNum = 0;
                }
                powerTotalNum++;
                powerEvseTotalNumMap.put(power, powerTotalNum);
                if (StringUtils.isNotBlank(evse.getState()) && Objects.equals(evse.getState(),
                        String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                    Integer powerFreeNum = powerEvseFreeNumMap.get(power);
                    if (powerFreeNum == null) {
                        powerFreeNum = 0;
                    }
                    powerFreeNum++;
                    powerEvseFreeNumMap.put(power, powerFreeNum);
                }
            });
            powerEvseTotalNumMap.forEach((power, powerTotalNum) -> {
                PowEvseVO powEvseVO = new PowEvseVO();
                powEvseVO.setPower(power);
                powEvseVO.setTotalPileNum(powerTotalNum);
                Integer powerFreeNum = powerEvseFreeNumMap.get(power);
                powerFreeNum = powerFreeNum == null ? 0 : powerFreeNum;
                powEvseVO.setFreePileNum(powerFreeNum);
                powEvseVOList.add(powEvseVO);
            });
        }
        Collections.sort(powEvseVOList);
        return powEvseVOList;
    }

    /**
     * 按照功率和类型进行排序
     *
     * @param esEVSEList
     * @return
     */
    private List<EvsePowerInfoVO> reversePower(List<OpLocationEvseElasticDTO> esEVSEList) {
        Map<String, Integer> totalMap = new HashMap<>();
        Map<String, Integer> freeMap = new HashedMap();
        List<EvsePowerInfoVO> powEvseVOList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            esEVSEList.forEach(evse -> {
                //功率
                Double power = evse.getPower();
                if (power == null) {
                    power = 0.00;
                }
                //类型
                Integer gunType = evse.getGunType();
                if (gunType == null) {
                    gunType = 1;
                }
                String key = power + "-" + gunType;
                Integer totalNum = totalMap.get(key);
                if (totalNum == null) {
                    totalNum = 0;
                }
                totalNum++;
                totalMap.put(key, totalNum);
                //统计空闲状态的枪
                if (StringUtils.isNotBlank(evse.getState()) && Objects.equals(evse.getState(),
                        String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                    Integer freeNum = freeMap.get(key);
                    if (freeNum == null) {
                        freeNum = 0;
                    }
                    freeNum++;
                    freeMap.put(key, freeNum);
                }
            });
            totalMap.forEach((key, totalNum) -> {
                EvsePowerInfoVO evsePowerInfoVO = new EvsePowerInfoVO();
                String[] split = key.split("-");
                evsePowerInfoVO.setPower(Double.valueOf(split[0]));
                evsePowerInfoVO.setGunType(Integer.valueOf(split[1]));
                evsePowerInfoVO.setTotalPileNum(totalNum);
                Integer powerFreeNum = freeMap.get(key);
                powerFreeNum = powerFreeNum == null ? 0 : powerFreeNum;
                evsePowerInfoVO.setFreeCount(powerFreeNum);
                powEvseVOList.add(evsePowerInfoVO);
            });
        }
        powEvseVOList.sort(comparing(EvsePowerInfoVO::getGunType));
        powEvseVOList.sort(comparing(EvsePowerInfoVO::getPower).reversed());
        return powEvseVOList;
    }

    /**
     * 构建站点功率范围
     *
     * @param esEVSEList 站点枪设备
     * @return 站点功率范围
     */
    private String buildStationPowerRange(List<OpLocationEvseElasticDTO> esEVSEList) {

        if (CollectionUtils.isEmpty(esEVSEList)) {
            return "";
        }
        double lowerPower = esEVSEList.get(0).getPower() == null ? 0 : esEVSEList.get(0).getPower();
        double higherPower = esEVSEList.get(0).getPower() == null ? 0 : esEVSEList.get(0).getPower();
        for (OpLocationEvseElasticDTO esEVSE : esEVSEList) {
            Double power = esEVSE.getPower() == null ? 0 : esEVSE.getPower();
            if (power < lowerPower) {
                lowerPower = power;
            }
            if (power > higherPower) {
                higherPower = power;
            }
        }
        return lowerPower + "-" + higherPower;
    }

    /**
     * 构建站点不同类型枪数统计
     *
     * @param opLocationESEVSEList 站点设备
     * @return 站点不同类型枪数统计
     */
    private List<GunTypeGroupVO> buildStationGunTypeGroupVOList2
    (List<OpLocationEvseElasticDTO> opLocationESEVSEList) {
        List<GunTypeGroupVO> gunTypeGroupVOList = Lists.newArrayList();
        if (CollectionUtils.isEmpty(opLocationESEVSEList)) {
            return gunTypeGroupVOList;
        }
        Map<Integer, Integer> gunTypeTotalEVSENumMap = new HashedMap();
        Map<Integer, Integer> gunTypeFreeEVSENumMap = new HashedMap();
        for (OpLocationEvseElasticDTO esEVSE : opLocationESEVSEList) {
            Integer gunType = esEVSE.getGunType();
            if (gunType == null) {
                gunType = 1;
            }
            Integer gunTypeTotalEVSENum = gunTypeTotalEVSENumMap.get(gunType);
            if (gunTypeTotalEVSENum == null) {
                gunTypeTotalEVSENum = 0;
            }
            gunTypeTotalEVSENum++;
            gunTypeTotalEVSENumMap.put(gunType, gunTypeTotalEVSENum);
            if (StringUtils.isNotBlank(esEVSE.getState()) && Objects.equals(esEVSE.getState(), String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                Integer gunTypeFreeEVSENum = gunTypeFreeEVSENumMap.get(gunType);
                if (gunTypeFreeEVSENum == null) {
                    gunTypeFreeEVSENum = 0;
                }
                gunTypeFreeEVSENum++;
                gunTypeFreeEVSENumMap.put(gunType, gunTypeFreeEVSENum);
            }
        }
        for (Map.Entry<Integer, Integer> entry : gunTypeTotalEVSENumMap.entrySet()) {
            Integer gunType = entry.getKey();
            Integer gunTypeTotalNum = entry.getValue();
            if (gunTypeTotalNum != 0) {
                GunTypeGroupVO gunTypeGroupVO = new GunTypeGroupVO();
                gunTypeGroupVO.setGunType(gunType);
                gunTypeGroupVO.setTotalCount(gunTypeTotalNum);
                gunTypeGroupVO.setCount(gunTypeTotalNum);
                Integer gunTypeFreeNum = gunTypeFreeEVSENumMap.get(gunType);
                gunTypeFreeNum = gunTypeFreeNum == null ? 0 : gunTypeFreeNum;
                gunTypeGroupVO.setFreeCount(gunTypeFreeNum);
                gunTypeGroupVOList.add(gunTypeGroupVO);
            }
        }
        return gunTypeGroupVOList;
    }

    /**
     * 不同类型枪数统计
     *
     * @param esEVSEList
     * @return
     */
    private List<TypeEvseVO> buildStationTypeEvseVOList(List<OpLocationEvseElasticDTO> esEVSEList) {
        List<TypeEvseVO> typeEvseVOList = Lists.newArrayList();
        if (CollectionUtils.isEmpty(esEVSEList)) {
            return typeEvseVOList;
        }
        Map<Integer, Integer> gunTypeTotalEVSENumMap = new HashedMap();
        Map<Integer, Integer> gunTypeFreeEVSENumMap = new HashedMap();
        esEVSEList.forEach(evse -> {
            Integer gunType = evse.getGunType();
            if (gunType == null) {
                gunType = 1;
            }
            Integer gunTypeTotalEVSENum = gunTypeTotalEVSENumMap.get(gunType);
            if (gunTypeTotalEVSENum == null) {
                gunTypeTotalEVSENum = 0;
            }
            gunTypeTotalEVSENum++;
            gunTypeTotalEVSENumMap.put(gunType, gunTypeTotalEVSENum);
            if (StringUtils.isNotBlank(evse.getState()) && Objects.equals(evse.getState(),
                    String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                Integer gunTypeFreeEVSENum = gunTypeFreeEVSENumMap.get(gunType);
                if (gunTypeFreeEVSENum == null) {
                    gunTypeFreeEVSENum = 0;
                }
                gunTypeFreeEVSENum++;
                gunTypeFreeEVSENumMap.put(gunType, gunTypeFreeEVSENum);
            }
        });
        gunTypeTotalEVSENumMap.forEach((gunType, gunTypeTotalNum) -> {
            if (gunTypeTotalNum != 0) {
                TypeEvseVO gunTypeGroupVO = new TypeEvseVO();
                gunTypeGroupVO.setGunType(gunType);
                gunTypeGroupVO.setTotalCount(gunTypeTotalNum);
                gunTypeGroupVO.setCount(gunTypeTotalNum);
                Integer gunTypeFreeNum = gunTypeFreeEVSENumMap.get(gunType);
                gunTypeFreeNum = gunTypeFreeNum == null ? 0 : gunTypeFreeNum;
                gunTypeGroupVO.setFreeCount(gunTypeFreeNum);
                typeEvseVOList.add(gunTypeGroupVO);
            }
        });
        return typeEvseVOList;
    }

    /**
     * 站点枪状态统计
     *
     * @param opLocationESEVSEList 站点设备
     * @return 枪状态统计
     */
    private List<GunStatusGroupVO> buildStationGunStatusGroupVO2
    (List<OpLocationEvseElasticDTO> opLocationESEVSEList) {
        List<GunStatusGroupVO> gunStatusGroupVOList = Lists.newArrayList();
        if (CollectionUtils.isEmpty(opLocationESEVSEList)) {
            return gunStatusGroupVOList;
        }
        Map<LocationEVSEStatusEnum, Integer> evseStatusEVSESNumMap = new HashMap<>();
        for (OpLocationEvseElasticDTO esEvseDTO : opLocationESEVSEList) {
            String state = esEvseDTO.getState();

            state = StringUtils.isBlank(state) ? String.valueOf(EvseDeviceStatusEnum.DEFAULT.getName()) : state;
            LocationEVSEStatusEnum locationEVSEStatusEnum = LocationEVSEStatusEnum.realTimeStatus2BusinessStatus(EvseDeviceStatusEnum.getEnumByName(state).getName());
            Integer stateNum = evseStatusEVSESNumMap.get(locationEVSEStatusEnum);
            if (stateNum == null) {
                stateNum = 0;
            }
            stateNum++;
            evseStatusEVSESNumMap.put(locationEVSEStatusEnum, stateNum);
        }
        for (Map.Entry<LocationEVSEStatusEnum, Integer> entry : evseStatusEVSESNumMap.entrySet()) {
            LocationEVSEStatusEnum locationEVSEStatusEnum = entry.getKey();
            Integer stateNum = entry.getValue();
            if (stateNum != 0) {
                GunStatusGroupVO gunStatusGroupVO = new GunStatusGroupVO();
                gunStatusGroupVO.setEvseStatusCode(locationEVSEStatusEnum.getCode());
                gunStatusGroupVO.setTotal(stateNum);
                gunStatusGroupVOList.add(gunStatusGroupVO);
            }
        }
        return gunStatusGroupVOList;
    }

    /**
     * 不同枪状态统计
     *
     * @param esEVSEList
     * @return
     */
    private List<EvseStatusVO> buildStationEvseStatusVO(List<OpLocationEvseElasticDTO> esEVSEList) {
        List<EvseStatusVO> evseStatusVOList = Lists.newArrayList();
        if (CollectionUtils.isEmpty(esEVSEList)) {
            return evseStatusVOList;
        }
        Map<LocationEVSEStatusEnum, Integer> evseStatusEVSESNumMap = new HashMap<>();
        Map<LocationEVSEStatusEnum, Integer> powerEvseFreeNumMap = new HashedMap();
        esEVSEList.forEach(evse -> {
            String state = evse.getState();

            state = StringUtils.isBlank(state) ? String.valueOf(EvseDeviceStatusEnum.DEFAULT.getName()) : state;
            LocationEVSEStatusEnum locationEVSEStatusEnum = LocationEVSEStatusEnum
                    .realTimeStatus2BusinessStatus(EvseDeviceStatusEnum.getEnumByName(state).getName());
            Integer stateNum = evseStatusEVSESNumMap.get(locationEVSEStatusEnum);
            if (stateNum == null) {
                stateNum = 0;
            }
            stateNum++;
            evseStatusEVSESNumMap.put(locationEVSEStatusEnum, stateNum);
            //统计空闲状态的枪
            if (StringUtils.isNotBlank(evse.getState()) && Objects.equals(evse.getState(),
                    String.valueOf(EvseDeviceStatusEnum.AVAILABLE.getName()))) {
                Integer powerFreeNum = powerEvseFreeNumMap.get(locationEVSEStatusEnum);
                if (powerFreeNum == null) {
                    powerFreeNum = 0;
                }
                powerFreeNum++;
                powerEvseFreeNumMap.put(locationEVSEStatusEnum, powerFreeNum);
            }
        });
        evseStatusEVSESNumMap.forEach((locationEVSEStatusEnum, stateNum) -> {
            if (stateNum != 0) {
                EvseStatusVO gunStatusGroupVO = new EvseStatusVO();
                gunStatusGroupVO.setEvseStatusCode(locationEVSEStatusEnum.getCode());
                gunStatusGroupVO.setTotal(stateNum);
                evseStatusVOList.add(gunStatusGroupVO);
            }
        });
        return evseStatusVOList;
    }

    /**
     * 构建充电桩列表
     *
     * @param esEVSEList 充电设备集合
     * @return 充电桩列表
     */
    private List<AppDetailPileListVO> buildAppDetailPileList(List<OpLocationEvseElasticDTO> esEVSEList) {
        List<AppDetailPileListVO> pileList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            Set<String> evseSNSet = new HashSet<>();
            Set<Long> tariffIdSet = new HashSet<>();
            Set<String> pileSnSet = new HashSet<>();
            //构建桩SN-枪集合map
            Map<String, List<OpLocationEvseElasticDTO>> pileSNEvseListMap = new HashMap<>();
            Map<String, String> pileSNTariffIdMap = new HashMap<>();
            esEVSEList.forEach(esEVSE -> {
                List<OpLocationEvseElasticDTO> pileSNEvseList = pileSNEvseListMap.get(esEVSE.getPileSn());
                if (pileSNEvseList == null) {
                    pileSNEvseList = Lists.newArrayList();
                }
                pileSNEvseList.add(esEVSE);
                pileSNEvseListMap.put(esEVSE.getPileSn(), pileSNEvseList);
                pileSnSet.add(esEVSE.getPileSn());
                if (esEVSE.getTariffId() != null) {
                    pileSNTariffIdMap.put(esEVSE.getPileSn(), esEVSE.getTariffId().toString());
                    tariffIdSet.add(esEVSE.getTariffId());
                }
                evseSNSet.add(esEVSE.getEvseSn());
            });
            log.info("pileSNTariffIdMap:{}", JSON.toJSONString(pileSNTariffIdMap));
            //查询计费规则构建成计费规则id-计费规则map
            Map<String, CostModelRuleDTO> tariffIdCostModelRuleMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(tariffIdSet)) {
                List<CostModelRuleDTO> costModelRuleList = getCostModelRuleByTariffIdSet(tariffIdSet);
                if (CollectionUtils.isNotEmpty(costModelRuleList)) {
                    costModelRuleList.forEach(costModelRuleDTO -> tariffIdCostModelRuleMap.put(costModelRuleDTO.getId().toString(), costModelRuleDTO));
                }
            }
            log.info("tariffIdCostModelRuleMap:{}", JSON.toJSONString(tariffIdCostModelRuleMap));
            //批量查询桩状态
            log.info("桩集合:{}", JSONArray.toJSONString(evseSNSet));
            Map<String, OpEvseStatusUploadDTO> snStatusMap = new HashMap<>();
            //整理sn的NFC功能是否支持
            Result<List<PileConfigurationInfoVO>> pileInformationVOListResult = deviceServiceClient.configurationInformation(new ArrayList<>(pileSnSet));
            List<String> pileSnNfcRFIDFunctionList = new ArrayList<>();
            if (pileInformationVOListResult != null && pileInformationVOListResult.getData() != null) {
                List<PileConfigurationInfoVO> pileInformationVOList = pileInformationVOListResult.getData();
                pileInformationVOList.forEach(pileInformationVO -> {
                    if (pileInformationVO.getUnifiedJson() != null) {
                        JSONObject jsonObject = JSON.parseObject(pileInformationVO.getUnifiedJson());
                        Object maxichargerRFID = jsonObject.get("maxicharger.RFID");
                        log.info("pileSn:{}===>>>maxichargerRFID:{}", pileInformationVO.getSn(), maxichargerRFID);
                        if ("2".equals(maxichargerRFID)) {
                            pileSnNfcRFIDFunctionList.add(pileInformationVO.getSn());
                        }
                    }
                });
            }
            if (CollectionUtils.isNotEmpty(evseSNSet)) {
                ArrayList<String> evseSNList = Lists.newArrayList(evseSNSet);
                Result<Map<String, OpEvseStatusUploadDTO>> snStatusMapResult = monitorFeignClient.queryStatusInfoByEvseSnList(evseSNList);
                if (snStatusMapResult != null && snStatusMapResult.getData() != null) {
                    snStatusMap = snStatusMapResult.getData();
                }
            }
            Map<String, OpEvseStatusUploadDTO> finalSnStatusMap = snStatusMap;
            log.info("桩状态map:{}", finalSnStatusMap);
            //构建成桩集合
            for (Map.Entry<String, List<OpLocationEvseElasticDTO>> entry : pileSNEvseListMap.entrySet()) {
                String pileSN = entry.getKey();
                List<OpLocationEvseElasticDTO> pileEvseList = entry.getValue();
                //封装桩对象
                AppDetailPileListVO appDetailPileVO = new AppDetailPileListVO();
//                appDetailPileVO.setPileSn(pileSN.toUpperCase());
                //考虑到三方桩sn
                appDetailPileVO.setPileSn(pileSN);
                //价格模板
                String tariffId = pileSNTariffIdMap.get(pileSN);
                appDetailPileVO.setTariffId(tariffId);

                //判断是否是新的计费规则
                //获取请求头中的版本信息
                HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
                String version = request.getHeader("X-Version");
                String xVersion = request.getHeader("X-Client");
                String xAppId = request.getHeader("X-AppId");
                //判断是否是白牌app,白牌app是从1.0开始，该功能已经支持
                boolean isWhiteCardApp = false;
                if (AppIdEnum.CHARGE_POINT.getValue().toString().equals(xAppId) || AppIdEnum.CHARGE_EVOTA.getValue().toString().equals(xAppId) ||
                        AppIdEnum.PLANET_EV.getValue().toString().equals(xAppId) ) {
                    isWhiteCardApp = true;
                }
                //根据版本号来判断是否还需要统计计费模板（1.11以上的都不需要）
                if (tariffId != null) {
                    CostModelRuleDTO costModelRuleDTO = tariffIdCostModelRuleMap.get(tariffId);
                    log.info("map里面获取计费模板：{}", JSON.toJSONString(costModelRuleDTO));

                    String userVersion = version;
                    if (AppTypeEnum.IOS_CODE.getValue().toString().equals(xVersion) || AppTypeEnum.ANDROID_CODE.getValue().toString().equals(xVersion)) {
                        if (!StringUtils.isBlank(version) && version.contains(VERSION_INTERCEPT_JUDGE)) {
                            userVersion = version.substring(0, version.indexOf(";"));
                        }
                    }
                    if (costModelRuleDTO != null && costModelRuleDTO.getNewVersionCostModelRule() != null && costModelRuleDTO.getNewVersionCostModelRule() == 1) {
                        //判断用户的版本是否不是新版
                        if (((StringUtils.isBlank(version) || locationCommon.compareVersion(userVersion, SUPPORT_UPGRADE_VERSION) == -1)) && !isWhiteCardApp) {
                            appDetailPileVO.setIsNewTariffRuleAndOldVersion(true);
                        }
                    }
                    if (costModelRuleDTO != null
                            && locationCommon.compareVersion(userVersion, SUPPORT_UPGRADE_VERSION) == -1
                            && !isWhiteCardApp
                            && (costModelRuleDTO.getNewVersionCostModelRule() == null || costModelRuleDTO.getNewVersionCostModelRule() == 0)) {
                        appDetailPileVO.setResourceCostModelRuleDTO(costModelRuleDTO);
                        appDetailPileVO.setCostModelRuleDTO(costModelRuleDTO);
                        appDetailPileVO.setStartCostModelRuleDTO(mergeStartPrice(costModelRuleDTO));
                        // todo 合并电量费
                        appDetailPileVO.setUnitCostModelRuleDTO(TariffUtil.mergeUnitPrice(costModelRuleDTO));
                        // todo 合并时长费
                        appDetailPileVO.setTimeCostModelRuleDTO(TariffUtil.mergeTimePrice(costModelRuleDTO));
                        // todo 合并停车费
                        appDetailPileVO.setParkCostModelRuleDTO(TariffUtil.mergeParkPrice(costModelRuleDTO));
                    } else if (costModelRuleDTO != null
                            && (locationCommon.compareVersion(userVersion, SUPPORT_UPGRADE_VERSION) == -1)
                            && !isWhiteCardApp
                            && (costModelRuleDTO.getNewVersionCostModelRule() != null && costModelRuleDTO.getNewVersionCostModelRule() == 1)) {
                        appDetailPileVO.setResourceCostModelRuleDTO(TariffUtil.chargingCost999Template(costModelRuleDTO));
                        appDetailPileVO.setCostModelRuleDTO(TariffUtil.chargingCost999Template(costModelRuleDTO));
                        appDetailPileVO.setUnitCostModelRuleDTO(TariffUtil.chargingCost999Template(costModelRuleDTO));
                    }
                }
                //桩的枪列表
                List<AppDetailGunListVO> pileGunList = DozerConvert.mapList(pileEvseList, AppDetailGunListVO.class);
                pileGunList.forEach(gunVO -> {
                    //枪号
                    if (StringUtils.isNotBlank(gunVO.getEvseSn()) && gunVO.getEvseSn().contains("_")) {
                        String s = CommonUtil.getGunNoStringType(gunVO.getEvseSn());
                        String sn = CommonUtil.getPileSn(gunVO.getEvseSn());
                        gunVO.setGunNo(s);
                        gunVO.setConnectorDisplayName(commonUtilService.getConnectorDisplayName(sn, s));
                    }
                    OpEvseStatusUploadDTO snStatusInfo = finalSnStatusMap.get(gunVO.getEvseSn());
                    //设置枪状态code
                    gunVO.setState(snStatusInfo.getStatus());
                    gunVO.setInfo(snStatusInfo.getCpVoltage());
                    gunVO.setStateCode(EvseDeviceStatusEnum.getEnumByName(gunVO.getState()).getCode());
                    LocationEVSESAPPStatusEnum locationEVSESAPPStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(gunVO.getState());
                    gunVO.setAppEVSEStateCode(locationEVSESAPPStatusEnum.getCode());
                    //给每个枪排序 充电中的桩（枪）> 插枪未启动 > 高功率 > 可用 > 正在使用 > 故障
                    //                CHARGING     SUSPENDED_EVSE/SUSPENDED_EV   AVAILABLE   PREPARING/RESERVED   FAULTED       最后：FINISHING/UNAVAILABLE/DEFAULT
                    if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.CHARGING.getName())) {
                        gunVO.setSortNO(1);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EV.getName()) ||
                            Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EVSE.getName())) {
                        gunVO.setSortNO(2);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                        gunVO.setSortNO2(1);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.PREPARING.getName()) ||
                            Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.RESERVED.getName())) {
                        gunVO.setSortNO2(2);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.FAULTED.getName())) {
                        gunVO.setSortNO2(3);
                    } else {
                        gunVO.setSortNO2(4);
                    }

                    String subscribeKey = RedisKeyConstant.getSubsribeKey(pileSN, gunVO.getGunNo());
                    String subscribeValue = stringRedisTemplate.opsForValue().get(subscribeKey);
                    boolean subscribeEnabled = false;
                    if (StringUtils.isNotBlank(subscribeValue)) {
                        List<String> userIdList = JSON.parseArray(subscribeValue, String.class);
                        if (CollectionUtils.isNotEmpty(userIdList) && userIdList.contains(LoginUserHolder.getLoginUser().getId().toString())) {
                            subscribeEnabled = true;
                        }
                    }

                    gunVO.setSubscribeEnabled(subscribeEnabled);
                    gunVO.setGunTypeName(ConnectorGunTypeEnum.getEnumByCode(gunVO.getGunType()).getName());
                    gunVO.setChargeByCurrentUserEnabled(false);
                    String pileGunKey = RedisKeyConstant.getChargePileGunKey(pileSN, gunVO.getGunNo());
                    String redisValue = stringRedisTemplate.opsForValue().get(pileGunKey);

                    int delayOn = 0;
                    Integer delayTime = 0;
                    boolean isFree = true;
                    String userId = "";
                    if (StringUtils.isNotBlank(redisValue)) {
                        TransactionInfoVO transactionInfoVo = JSON.parseObject(redisValue, TransactionInfoVO.class);
                        userId = transactionInfoVo.getUserId();
                        isFree = false;
//                        BillRandomDelayVO billRandomDelayVO = billFeignClient.getBillRandomDelay(transactionInfoVo.getTransactionId()).getData();
//                        if(billRandomDelayVO != null){
//                            delayOn = billRandomDelayVO.getDelayOn() ? 1 : 0;
//                            delayTime = billRandomDelayVO.getDelayTime();
//                        }
                        //从缓存获取随机延迟信息
                        try {
                            String delayInfoStr = stringRedisTemplate.opsForValue().get(String.format(RedisKeyConstant.CHARGE_DELAY_TIME_KEY, transactionInfoVo.getTransactionId()));
                            log.info("延迟信息，key：{}  value：{}", String.format(RedisKeyConstant.CHARGE_DELAY_TIME_KEY, transactionInfoVo.getTransactionId()), delayInfoStr);
                            if (StringUtils.isNotBlank(delayInfoStr)) {
                                CacheDelayTimeDTO cacheDelayTimeDTO = JSONObject.parseObject(delayInfoStr, CacheDelayTimeDTO.class);
                                if (cacheDelayTimeDTO != null) {
                                    Integer delayTimeLeft = cacheDelayTimeDTO.getDelayTimeLeft();
                                    if (cacheDelayTimeDTO.getDelayTime() != null) {
                                        delayTime = Integer.valueOf(cacheDelayTimeDTO.getDelayTime().toString());
                                    }
                                    if (delayTimeLeft == null || delayTime == 0) {
                                        log.info("随机延迟不启动");
                                        delayOn = 0;
                                        delayTime = 0;
                                    } else {
                                        int timeLeft = 0;
                                        if (OrderStatusEnum.DEFAULT.getValue().equals(cacheDelayTimeDTO.getOrderStatus())) {
                                            timeLeft = getTimeLeft(System.currentTimeMillis(), Long.valueOf(delayTime), delayTimeLeft);
                                        }
                                        delayOn = timeLeft != 0 ? 1 : 0;
                                        delayTime = timeLeft;
                                    }
                                }
                            }
                        } catch (NumberFormatException e) {
                            log.info("从缓存获取随机延迟信息处理失败:" + e);
                        }
                    } else {
                        try {
                            String lastBillInfo = stringRedisTemplate.opsForValue().get(String.format(RedisKeyConstant.CHARGE_LAST_BILL_KEY, gunVO.getEvseSn()));
                            if (StringUtils.isNotBlank(lastBillInfo)) {
                                EnergyBillVO energyBillVO = JSONObject.parseObject(lastBillInfo, EnergyBillVO.class);
                                if (energyBillVO != null && energyBillVO.getUserId() != null) {
                                    userId = energyBillVO.getUserId().toString();
                                }
                            }
                        } catch (Exception e) {
                            log.error("findLastBillByEvse error", e);
                        }
                    }

                    if (AppIdEnum.PLANET_EV.getValue().toString().equals(xAppId) ) {
                        gunVO.setReservationEnabled(false);
                    }

                    gunVO.setFreeAutoStartEnabled(isStartWithFree(pileSN, gunVO.getGunNo()));
                    gunVO.setDelayOn(delayOn);
                    gunVO.setDelayTime(delayTime);
                    gunVO.setFreeEnabled(isFree);
                    try {
                        gunVO.setChargeByCurrentUserEnabled(userId.equals(LoginUserHolder.getLoginUser().getId().toString()));
                    } catch (Exception e) {
                        log.info("获取用户ID失败：{}", e);
                    }
                });
                Collections.sort(pileGunList);
                appDetailPileVO.setGunList(pileGunList);
                try {
                    appDetailPileVO.setName(pileEvseList.get(0).getPileName());
                    String powerType = pileEvseList.get(0).getPowerType();
                    appDetailPileVO.setPowerType(powerType.substring(0, 2));
                } catch (Exception e) {
                    log.info("查询桩名称出错了：{}", e);
                }
                //是否支持NFC功能
                //先默认不支持
                appDetailPileVO.setNfcEnable(false);
                if (pileSnNfcRFIDFunctionList.contains(appDetailPileVO.getPileSn())) {
                    appDetailPileVO.setNfcEnable(true);
                }
                pileList.add(appDetailPileVO);
            }
        }
        //桩排序
        pileList.forEach(pile -> {
            List<AppDetailGunListVO> gunList = pile.getGunList();
            gunList.forEach(gunVO -> {
                if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.CHARGING.getName())) {
                    pile.setSortNO(1);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EV.getName()) ||
                        Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EVSE.getName())) {
                    pile.setSortNO(2);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                    pile.setSortNO2(1);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.PREPARING.getName()) ||
                        Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.RESERVED.getName())) {
                    pile.setSortNO2(2);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.FAULTED.getName())) {
                    pile.setSortNO2(3);
                } else {
                    pile.setSortNO2(4);
                }
                if (pile.getPower() == null || (pile.getPower() != null && gunVO.getPower() > pile.getPower())) {
                    pile.setPower(gunVO.getPower());
                }
            });
        });
        Collections.sort(pileList);
        return pileList;
    }

    /**
     * 构建充电桩列表
     *
     * @param esEVSEList 充电设备集合
     * @return 充电桩列表
     */
    private List<AppDetailPileListVO> buildAppDetailPileListForOCPI(List<OpLocationEvseElasticDTO> esEVSEList) {
        List<AppDetailPileListVO> pileList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            Set<String> evseSNSet = new HashSet<>();
            Set<Long> tariffIdSet = new HashSet<>();
            Set<String> pileSnSet = new HashSet<>();
            //构建桩SN-枪集合map
            Map<String, List<OpLocationEvseElasticDTO>> pileSNEvseListMap = new HashMap<>();
            Map<String, String> pileSNTariffIdMap = new HashMap<>();
            esEVSEList.forEach(esEVSE -> {
                List<OpLocationEvseElasticDTO> pileSNEvseList = pileSNEvseListMap.get(esEVSE.getPileSn());
                if (pileSNEvseList == null) {
                    pileSNEvseList = Lists.newArrayList();
                }
                pileSNEvseList.add(esEVSE);
                pileSNEvseListMap.put(esEVSE.getPileSn(), pileSNEvseList);
                pileSnSet.add(esEVSE.getPileSn());
                if (esEVSE.getTariffId() != null) {
                    pileSNTariffIdMap.put(esEVSE.getPileSn(), esEVSE.getTariffId().toString());
                    tariffIdSet.add(esEVSE.getTariffId());
                }
                evseSNSet.add(esEVSE.getEvseSn());
            });
            log.info("pileSNTariffIdMap:{}", JSON.toJSONString(pileSNTariffIdMap));
            //查询计费规则构建成计费规则id-计费规则map
            Map<String, CostModelRuleDTO> tariffIdCostModelRuleMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(tariffIdSet)) {
                List<CostModelRuleDTO> costModelRuleList = getCostModelRuleByTariffIdSet(tariffIdSet);
                if (CollectionUtils.isNotEmpty(costModelRuleList)) {
                    costModelRuleList.forEach(costModelRuleDTO -> tariffIdCostModelRuleMap.put(costModelRuleDTO.getId().toString(), costModelRuleDTO));
                }
            }
            log.info("tariffIdCostModelRuleMap:{}", JSON.toJSONString(tariffIdCostModelRuleMap));
            //批量查询桩状态
            log.info("桩集合:{}", JSONArray.toJSONString(evseSNSet));
            Map<String, OpEvseStatusUploadDTO> snStatusMap = new HashMap<>();
            //整理sn的NFC功能是否支持
            Result<List<PileConfigurationInfoVO>> pileInformationVOListResult = deviceServiceClient.configurationInformation(new ArrayList<>(pileSnSet));
            List<String> pileSnNfcRFIDFunctionList = new ArrayList<>();
            if (pileInformationVOListResult != null && pileInformationVOListResult.getData() != null) {
                List<PileConfigurationInfoVO> pileInformationVOList = pileInformationVOListResult.getData();
                pileInformationVOList.forEach(pileInformationVO -> {
                    if (pileInformationVO.getUnifiedJson() != null) {
                        JSONObject jsonObject = JSON.parseObject(pileInformationVO.getUnifiedJson());
                        Object maxichargerRFID = jsonObject.get("maxicharger.RFID");
                        log.info("pileSn:{}===>>>maxichargerRFID:{}", pileInformationVO.getSn(), maxichargerRFID);
                        if ("2".equals(maxichargerRFID)) {
                            pileSnNfcRFIDFunctionList.add(pileInformationVO.getSn());
                        }
                    }
                });
            }
            if (CollectionUtils.isNotEmpty(evseSNSet)) {
                ArrayList<String> evseSNList = Lists.newArrayList(evseSNSet);
                Result<Map<String, OpEvseStatusUploadDTO>> snStatusMapResult = monitorFeignClient.queryStatusInfoByEvseSnList(evseSNList);
                if (snStatusMapResult != null && snStatusMapResult.getData() != null) {
                    snStatusMap = snStatusMapResult.getData();
                }
            }
            Map<String, OpEvseStatusUploadDTO> finalSnStatusMap = snStatusMap;
            log.info("桩状态map:{}", finalSnStatusMap);
            //构建成桩集合
            for (Map.Entry<String, List<OpLocationEvseElasticDTO>> entry : pileSNEvseListMap.entrySet()) {
                String pileSN = entry.getKey();
                List<OpLocationEvseElasticDTO> pileEvseList = entry.getValue();
                //封装桩对象
                AppDetailPileListVO appDetailPileVO = new AppDetailPileListVO();
//                appDetailPileVO.setPileSn(pileSN.toUpperCase());
                //考虑到三方桩sn
                appDetailPileVO.setPileSn(pileSN);
                //价格模板
                String tariffId = pileSNTariffIdMap.get(pileSN);
                appDetailPileVO.setTariffId(tariffId);
                //根据版本号来判断是否还需要统计计费模板（1.11以上的都不需要）
                if (tariffId != null) {
                    CostModelRuleDTO costModelRuleDTO = tariffIdCostModelRuleMap.get(tariffId);
                    log.info("map里面获取计费模板：{}", JSON.toJSONString(costModelRuleDTO));
                    //判断是否是新的计费规则
                    //获取请求头中的版本信息
                    HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
                    String version = request.getHeader("X-Version");
                    String xVersion = request.getHeader("X-Client");
                    String xAppId = request.getHeader("X-AppId");
                    //判断是否是白牌app
                    boolean isWhiteCardApp = false;
                    if (AppIdEnum.CHARGE_POINT.getValue().toString().equals(xAppId) || AppIdEnum.CHARGE_EVOTA.getValue().toString().equals(xAppId)) {
                        isWhiteCardApp = true;
                    }
                    String userVersion = version;
                    if (AppTypeEnum.IOS_CODE.getValue().toString().equals(xVersion) || AppTypeEnum.ANDROID_CODE.getValue().toString().equals(xVersion)) {
                        if (!StringUtils.isBlank(version) && version.contains(VERSION_INTERCEPT_JUDGE)) {
                            userVersion = version.substring(0, version.indexOf(";"));
                        }
                    }
                    if (costModelRuleDTO != null && costModelRuleDTO.getNewVersionCostModelRule() != null && costModelRuleDTO.getNewVersionCostModelRule() == 1) {
                        //判断用户的版本是否不是新版
                        if ((StringUtils.isBlank(version) || locationCommon.compareVersion(userVersion, SUPPORT_UPGRADE_VERSION) == -1) && !isWhiteCardApp) {
                            appDetailPileVO.setIsNewTariffRuleAndOldVersion(true);
                        }
                    }
                    if (costModelRuleDTO != null
                            && locationCommon.compareVersion(userVersion, SUPPORT_UPGRADE_VERSION) == -1
                            && !isWhiteCardApp
                            && (costModelRuleDTO.getNewVersionCostModelRule() == null || costModelRuleDTO.getNewVersionCostModelRule() == 0)) {
                        appDetailPileVO.setResourceCostModelRuleDTO(costModelRuleDTO);
                        appDetailPileVO.setCostModelRuleDTO(costModelRuleDTO);
                        appDetailPileVO.setStartCostModelRuleDTO(mergeStartPrice(costModelRuleDTO));
                        // todo 合并电量费
                        appDetailPileVO.setUnitCostModelRuleDTO(TariffUtil.mergeUnitPrice(costModelRuleDTO));
                        // todo 合并时长费
                        appDetailPileVO.setTimeCostModelRuleDTO(TariffUtil.mergeTimePrice(costModelRuleDTO));
                        // todo 合并停车费
                        appDetailPileVO.setParkCostModelRuleDTO(TariffUtil.mergeParkPrice(costModelRuleDTO));
                    } else if (costModelRuleDTO != null
                            && (locationCommon.compareVersion(userVersion, SUPPORT_UPGRADE_VERSION) == -1)
                            && !isWhiteCardApp
                            && (costModelRuleDTO.getNewVersionCostModelRule() != null && costModelRuleDTO.getNewVersionCostModelRule() == 1)) {
                        appDetailPileVO.setResourceCostModelRuleDTO(TariffUtil.chargingCost999Template(costModelRuleDTO));
                        appDetailPileVO.setCostModelRuleDTO(TariffUtil.chargingCost999Template(costModelRuleDTO));
                        appDetailPileVO.setUnitCostModelRuleDTO(TariffUtil.chargingCost999Template(costModelRuleDTO));
                    }
                }
                //桩的枪列表
                List<AppDetailGunListVO> pileGunList = DozerConvert.mapList(pileEvseList, AppDetailGunListVO.class);
                pileGunList.forEach(gunVO -> {
                    //枪号
                    if (StringUtils.isNotBlank(gunVO.getEvseSn()) && gunVO.getEvseSn().contains("_")) {
                        String s = gunVO.getEvseSn().split("_")[1];
                        String sn = gunVO.getEvseSn().split("_")[0];
                        gunVO.setGunNo(s);
                        gunVO.setConnectorDisplayName(commonUtilService.getConnectorDisplayName(sn, s));
                    }
                    OpEvseStatusUploadDTO snStatusInfo = finalSnStatusMap.get(gunVO.getEvseSn());
                    //设置枪状态code 直接拿取es的
//                    gunVO.setState(snStatusInfo.getStatus());
                    gunVO.setInfo(snStatusInfo.getCpVoltage());
                    gunVO.setStateCode(EvseDeviceStatusEnum.getEnumByName(gunVO.getState()).getCode());
                    LocationEVSESAPPStatusEnum locationEVSESAPPStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(gunVO.getState());
                    gunVO.setAppEVSEStateCode(locationEVSESAPPStatusEnum.getCode());
                    //给每个枪排序 充电中的桩（枪）> 插枪未启动 > 高功率 > 可用 > 正在使用 > 故障
                    //                CHARGING     SUSPENDED_EVSE/SUSPENDED_EV   AVAILABLE   PREPARING/RESERVED   FAULTED       最后：FINISHING/UNAVAILABLE/DEFAULT
                    if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.CHARGING.getName())) {
                        gunVO.setSortNO(1);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EV.getName()) ||
                            Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EVSE.getName())) {
                        gunVO.setSortNO(2);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                        gunVO.setSortNO2(1);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.PREPARING.getName()) ||
                            Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.RESERVED.getName())) {
                        gunVO.setSortNO2(2);
                    } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.FAULTED.getName())) {
                        gunVO.setSortNO2(3);
                    } else {
                        gunVO.setSortNO2(4);
                    }

                    String subscribeKey = RedisKeyConstant.getSubsribeKey(pileSN, gunVO.getGunNo());
                    String subscribeValue = stringRedisTemplate.opsForValue().get(subscribeKey);
                    boolean subscribeEnabled = false;
                    if (StringUtils.isNotBlank(subscribeValue)) {
                        List<String> userIdList = JSON.parseArray(subscribeValue, String.class);
                        if (CollectionUtils.isNotEmpty(userIdList) && userIdList.contains(LoginUserHolder.getLoginUser().getId().toString())) {
                            subscribeEnabled = true;
                        }
                    }

                    gunVO.setSubscribeEnabled(subscribeEnabled);
                    gunVO.setGunTypeName(ConnectorGunTypeEnum.getEnumByCode(gunVO.getGunType()).getName());
                    gunVO.setChargeByCurrentUserEnabled(false);
                    String pileGunKey = RedisKeyConstant.getChargePileGunKey(pileSN, gunVO.getGunNo());
                    String redisValue = stringRedisTemplate.opsForValue().get(pileGunKey);

                    int delayOn = 0;
                    Integer delayTime = 0;
                    boolean isFree = true;
                    String userId = "";
                    if (StringUtils.isNotBlank(redisValue)) {
                        TransactionInfoVO transactionInfoVo = JSON.parseObject(redisValue, TransactionInfoVO.class);
                        userId = transactionInfoVo.getUserId();
                        isFree = false;
//                        BillRandomDelayVO billRandomDelayVO = billFeignClient.getBillRandomDelay(transactionInfoVo.getTransactionId()).getData();
//                        if(billRandomDelayVO != null){
//                            delayOn = billRandomDelayVO.getDelayOn() ? 1 : 0;
//                            delayTime = billRandomDelayVO.getDelayTime();
//                        }
                        //从缓存获取随机延迟信息
                        try {
                            String delayInfoStr = stringRedisTemplate.opsForValue().get(String.format(RedisKeyConstant.CHARGE_DELAY_TIME_KEY, transactionInfoVo.getTransactionId()));
                            log.info("延迟信息，key：{}  value：{}", String.format(RedisKeyConstant.CHARGE_DELAY_TIME_KEY, transactionInfoVo.getTransactionId()), delayInfoStr);
                            if (StringUtils.isNotBlank(delayInfoStr)) {
                                CacheDelayTimeDTO cacheDelayTimeDTO = JSONObject.parseObject(delayInfoStr, CacheDelayTimeDTO.class);
                                if (cacheDelayTimeDTO != null) {
                                    Integer delayTimeLeft = cacheDelayTimeDTO.getDelayTimeLeft();
                                    if (cacheDelayTimeDTO.getDelayTime() != null) {
                                        delayTime = Integer.valueOf(cacheDelayTimeDTO.getDelayTime().toString());
                                    }
                                    if (delayTimeLeft == null || delayTime == 0) {
                                        log.info("随机延迟不启动");
                                        delayOn = 0;
                                        delayTime = 0;
                                    } else {
                                        int timeLeft = 0;
                                        if (OrderStatusEnum.DEFAULT.getValue().equals(cacheDelayTimeDTO.getOrderStatus())) {
                                            timeLeft = getTimeLeft(System.currentTimeMillis(), Long.valueOf(delayTime), delayTimeLeft);
                                        }
                                        delayOn = timeLeft != 0 ? 1 : 0;
                                        delayTime = timeLeft;
                                    }
                                }
                            }
                        } catch (NumberFormatException e) {
                            log.info("从缓存获取随机延迟信息处理失败:" + e);
                        }
                    } else {
                        try {
                            String lastBillInfo = stringRedisTemplate.opsForValue().get(String.format(RedisKeyConstant.CHARGE_LAST_BILL_KEY, gunVO.getEvseSn()));
                            if (StringUtils.isNotBlank(lastBillInfo)) {
                                EnergyBillVO energyBillVO = JSONObject.parseObject(lastBillInfo, EnergyBillVO.class);
                                if (energyBillVO != null && energyBillVO.getUserId() != null) {
                                    userId = energyBillVO.getUserId().toString();
                                }
                            }
                        } catch (Exception e) {
                            log.error("findLastBillByEvse error", e);
                        }
                    }

                    gunVO.setFreeAutoStartEnabled(isStartWithFree(pileSN, gunVO.getGunNo()));
                    gunVO.setDelayOn(delayOn);
                    gunVO.setDelayTime(delayTime);
                    gunVO.setFreeEnabled(isFree);
                    try {
                        gunVO.setChargeByCurrentUserEnabled(userId.equals(LoginUserHolder.getLoginUser().getId().toString()));
                    } catch (Exception e) {
                        log.info("获取用户ID失败：{}", e);
                    }
                });
                Collections.sort(pileGunList);
                appDetailPileVO.setGunList(pileGunList);
                try {
                    appDetailPileVO.setName(pileEvseList.get(0).getPileName());
                    String powerType = pileEvseList.get(0).getPowerType();
                    appDetailPileVO.setPowerType(powerType.substring(0, 2));
                } catch (Exception e) {
                    log.info("查询桩名称出错了：{}", e);
                }
                //是否支持NFC功能
                //先默认不支持
                appDetailPileVO.setNfcEnable(false);
                if (pileSnNfcRFIDFunctionList.contains(appDetailPileVO.getPileSn())) {
                    appDetailPileVO.setNfcEnable(true);
                }
                pileList.add(appDetailPileVO);
            }
        }
        //桩排序
        pileList.forEach(pile -> {
            List<AppDetailGunListVO> gunList = pile.getGunList();
            gunList.forEach(gunVO -> {
                if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.CHARGING.getName())) {
                    pile.setSortNO(1);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EV.getName()) ||
                        Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.SUSPENDED_EVSE.getName())) {
                    pile.setSortNO(2);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                    pile.setSortNO2(1);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.PREPARING.getName()) ||
                        Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.RESERVED.getName())) {
                    pile.setSortNO2(2);
                } else if (Objects.equals(gunVO.getState(), EvseDeviceStatusEnum.FAULTED.getName())) {
                    pile.setSortNO2(3);
                } else {
                    pile.setSortNO2(4);
                }
                if (pile.getPower() == null || (pile.getPower() != null && gunVO.getPower() > pile.getPower())) {
                    pile.setPower(gunVO.getPower());
                }
            });
        });
        Collections.sort(pileList);
        return pileList;
    }

    private boolean isStartWithFree(String chargePileSn, String gunNo) {
        OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(chargePileSn + "_" + gunNo);
        log.info("============= isStartWithFree sn: {}, gunNo:{}, opEvseInfoDTO:{}", chargePileSn, gunNo, JSON.toJSON(opEvseInfoDTO));
        if (opEvseInfoDTO == null || opEvseInfoDTO.getTariffId() == null) {
            return false;
        }
        Result<CostModelBasicInfoVO> costModelBasicInfoVOResult = tariffFeignClient.queryTariffBasicInfoById(opEvseInfoDTO.getTariffId());
        log.info("============= isStartWithFree costModelBasicInfoVOResult of the tariffId: {}, the costModelBasicInfoVOResult: {}", opEvseInfoDTO.getTariffId(), JSON.toJSON(costModelBasicInfoVOResult));
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

    private Integer getTimeLeft(Long timeMillis, Long delayTime, Integer delayTimeLeft) {
        log.info("getTimeLeft timeMillis: {}, delayTime: {}, delayTimeLeft: {}", timeMillis, delayTime, delayTimeLeft);
        long timeBetween = timeMillis - delayTime;
        int limitLeft = delayTimeLeft - (int) timeBetween / 1000;
        return Math.max(limitLeft, 0);
    }

    /**
     * app站点详情枪数据内存分页
     *
     * @param evseList 站点所有枪数据集合
     * @return 枪数据分页
     */
    private List<GunListPageVO> buildStationGunListPageVOList(List<OpLocationEvseElasticDTO> evseList, Integer pageIndex, Integer pageSize) {

        List<GunListPageVO> stationGunListPageVOList = new ArrayList<>();
        if (CollectionUtils.isEmpty(evseList)) {
            return stationGunListPageVOList;
        }
        //查询这些设备的计费规则，封装成map
        Set<Long> tariffIdSet = evseList.stream().map(OpLocationEvseElasticDTO::getTariffId).filter(Objects::nonNull).collect(Collectors.toSet());
        Map<Long, CostModelRuleDTO> tariffIdCostModelRuleDTOMap = buildTariffIdCostModelRuleDTOMap(tariffIdSet);
        for (OpLocationEvseElasticDTO stationEVSE : evseList) {
            GunListPageVO gunListPageVO = DozerConvert.map(stationEVSE, GunListPageVO.class);
            //枪号
            if (StringUtils.isNotBlank(stationEVSE.getEvseSn()) && stationEVSE.getEvseSn().contains("_")) {
                String s = stationEVSE.getEvseSn().split("_")[1];
                gunListPageVO.setGunNo(s);
            }
            //状态
            String realTimeState = stationEVSE.getState();
            realTimeState = StringUtils.isBlank(realTimeState) ? EvseDeviceStatusEnum.UNAVAILABLE.getName() : realTimeState;
            LocationEVSESAPPStatusEnum appStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(realTimeState);
            gunListPageVO.setStatus(appStatusEnum.getCode());
            //价格
            if (stationEVSE.getTariffId() != null && tariffIdCostModelRuleDTOMap.get(stationEVSE.getTariffId()) != null) {
                CostModelRuleDTO costModelRuleDTO = tariffIdCostModelRuleDTOMap.get(stationEVSE.getTariffId());
                log.info("计费对象：{}", JSON.toJSONString(costModelRuleDTO));
                if (costModelRuleDTO != null) {
                    //货币
                    gunListPageVO.setCurrencySign(costModelRuleDTO.getCurrencySign());
                    List<StagePriceVO> stagePriceVOList = costRuleWeekList2StagePriceVOList(costModelRuleDTO);
                    //各个时间段价格
                    gunListPageVO.setStagePirce(stagePriceVOList);
                }
            }
            stationGunListPageVOList.add(gunListPageVO);
        }
        return stationGunListPageVOList;
    }

    private List<StagePriceVO> costRuleWeekList2StagePriceVOList(CostModelRuleDTO costModelRuleDTO) {
        List<StagePriceVO> stagePirceVOList = new ArrayList<>();
        log.info("计费规则：{}", JSON.toJSONString(costModelRuleDTO));
        if (costModelRuleDTO == null) {
            return stagePirceVOList;
        }
        List<CostRuleWeeksDTO> costRuleWeeksDTOList = costModelRuleDTO.getRules();
        //今天
        int todayValue = LocalDate.now().getDayOfWeek().getValue();
        log.info("今天todayValue：{}", todayValue);
        for (CostRuleWeeksDTO costRuleWeeksDTO : costRuleWeeksDTOList) {
            List<Integer> weeks = costRuleWeeksDTO.getWeeks();
            //今天的计费
            if (weeks.contains(todayValue)) {
                List<CostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                for (CostRulesDTO costRulesDTO : weeksRules) {
                    StagePriceVO stagePriceVO = new StagePriceVO();
                    stagePriceVO.setTimeStart(costRulesDTO.getBeginHour() + costRulesDTO.getBeginMinute() / 60);
                    stagePriceVO.setTimeEnd(costRulesDTO.getEndHour() + costRulesDTO.getEndMinute() / 60);
                    if (costRulesDTO.getUnitPrice() != null) {
                        stagePriceVO.setEnergyPrice(Double.valueOf(costRulesDTO.getUnitPrice().toString()));
                    }
                    stagePirceVOList.add(stagePriceVO);
                }
            }
        }
        return stagePirceVOList;
    }

    @Override
    public List<OpLocationInfoVO> getLocationByGroupId(List<Long> groupIdList) {
        List<OpLocationInfoVO> resultList = Lists.newArrayList();
        List<OpLocationElasticDTO> esStationList = opLocationElastic.findByGroupIdIn(groupIdList);
        log.info("getLocationByGroupId,esStationList={}", JSON.toJSONString(esStationList));
        if (CollectionUtils.isNotEmpty(esStationList)) {
            Map<Long, List<OpLocationElasticDTO>> locationMap = esStationList.stream().collect(Collectors.groupingBy(OpLocationElasticDTO::getGroupId));
            groupIdList.forEach(item -> {
                OpLocationInfoVO opLocationInfoVO = new OpLocationInfoVO();
                opLocationInfoVO.setOrgId(String.valueOf(item));
                List<OpLocationElasticDTO> locationList = locationMap.get(item);
                if (CollectionUtils.isNotEmpty(locationList)) {
                    opLocationInfoVO.setLocationList(locationList.stream().map(location ->
                            OpLocationNameDTO.builder().locationId(String.valueOf(location.getId()))
                                    .locationName(location.getName()).address(location.getAddress()).build()).collect(Collectors.toList()));
                }
                resultList.add(opLocationInfoVO);
            });
            return resultList;
        }

        List<OpLocationNameDTO> opLocationList = opLocationOperationMapper.getLocationByGroupId(groupIdList);
        log.info("getLocationByGroupId,opLocationList={}", JSON.toJSONString(opLocationList));
        if (CollectionUtils.isNotEmpty(opLocationList)) {
            Map<String, List<OpLocationNameDTO>> locationMap = opLocationList.stream().collect(Collectors.groupingBy(OpLocationNameDTO::getGroupId));
            groupIdList.forEach(item -> {
                OpLocationInfoVO opLocationInfoVO = new OpLocationInfoVO();
                opLocationInfoVO.setOrgId(String.valueOf(item));
                List<OpLocationNameDTO> locationList = locationMap.get(String.valueOf(item));
                if (CollectionUtils.isNotEmpty(locationList)) {
                    opLocationInfoVO.setLocationList(locationList.stream().map(location ->
                            OpLocationNameDTO.builder().locationId(String.valueOf(location.getLocationId()))
                                    .locationName(location.getLocationName()).address(location.getAddress()).build()).collect(Collectors.toList()));
                }
                resultList.add(opLocationInfoVO);
            });
        }
        return resultList;
    }

    /**
     * 国家简写转成全称
     *
     * @param country 国家简写
     * @return 国家全称
     */
    @Override
    public String changeCountry(String country) {
        log.info("数据库保存的国家简写信息：{}", country);
        //根据国际化转成转成对应的全称
        if (StringUtils.isNotBlank(country)) {
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String language = request.getHeader("accept-language");
            log.info("header里面国际化语言：{}", language);
            if (StringUtils.isBlank(language)) {
                language = "zh-CN";
            }
            QueryWrapper<OpCountryEntity> opCountryEntityQueryWrapper = new QueryWrapper<>();
            opCountryEntityQueryWrapper.eq("alpha_2_code", country);
            opCountryEntityQueryWrapper.eq("language", language);
            List<OpCountryEntity> opCountryEntities = opCountryMapper.selectList(opCountryEntityQueryWrapper);
            log.info("opCountryEntities：{}", JSON.toJSONString(opCountryEntities));
            if (CollectionUtils.isNotEmpty(opCountryEntities)) {
                country = opCountryEntities.get(0).getName();
            }
        }
        return country;
    }

    /**
     * 国家简写转成全称
     *
     * @param countryCode 国家简写
     * @return 国家全称
     */
    private String changeHubjectCountry(String countryCode) {
        log.info("数据库保存的国家简写信息：{}", countryCode);
        //根据国际化转成转成对应的全称
        if (StringUtils.isNotBlank(countryCode)) {
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String language = request.getHeader("accept-language");
            log.info("header里面国际化语言：{}", language);
            if (StringUtils.isBlank(language)) {
                language = "zh-CN";
            }
            try {
                // 国家三位码转两位码
                countryCode = CountryCodeEnum.getInstance(countryCode).toString();
            } catch (Exception e) {
                log.error("国家三位码转两位码失败");
            }

            QueryWrapper<OpCountryEntity> opCountryEntityQueryWrapper = new QueryWrapper<>();
            opCountryEntityQueryWrapper.eq("alpha_2_code", countryCode);
            opCountryEntityQueryWrapper.eq("language", language);
            List<OpCountryEntity> opCountryEntities = opCountryMapper.selectList(opCountryEntityQueryWrapper);
            log.info("opCountryEntities：{}", JSON.toJSONString(opCountryEntities));
            if (CollectionUtils.isNotEmpty(opCountryEntities)) {
                countryCode = opCountryEntities.get(0).getName();
            }
        }
        return countryCode;
    }

    @Override
    public List<OpLocationDTO> getLocationBySellerId(Long sellerId) {
        BoolQueryBuilder stationQueryBuilder = QueryBuilders.boolQuery();
        stationQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId));
        Iterable<OpLocationElasticDTO> iterable =
//                opLocationElastic.search(stationQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(stationQueryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationDTO> elasticDTOList = new ArrayList<>();
        iterable.forEach(item -> elasticDTOList.add(OpLocationConvert.toOpLocationDTO(item)));
        return elasticDTOList;
    }

    @Override
    public List<Long> getLocationIdBySellerId(Long sellerId) {
        BoolQueryBuilder stationQueryBuilder = QueryBuilders.boolQuery();
        stationQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(stationQueryBuilder)
//                .withFields("id")
                .withSourceFilter(new FetchSourceFilterBuilder().withIncludes("id").build())
                .build();
        Iterable<OpLocationElasticDTO> iterable =
//                opLocationElastic.search(searchQuery);
                elasticsearchRestTemplate.search(searchQuery, OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<Long> locationIdList = new ArrayList<>();
        iterable.forEach(item -> locationIdList.add(item.getId()));
        return locationIdList;
    }

    @Override
    public OpLocationDTO getLocationByPileSn(String pileSn) {
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(pileSn);
        if (opLocationPileEvseElasticDTO == null) {
            return null;
        }
        Long locationId = opLocationPileEvseElasticDTO.getLocationId();
        Optional<OpLocationElasticDTO> opLocationElasticDTOOptional = opLocationElastic.findById(locationId);
        if (!opLocationElasticDTOOptional.isPresent()) {
            return null;
        }
        OpLocationElasticDTO opLocationElasticDTO = opLocationElasticDTOOptional.get();
        return OpLocationConvert.toOpLocationDTO(opLocationElasticDTO);
    }

    @Override
    public List<OpLocationListDTO> getLocationByPileSnList(List<String> pileSnList) {
        return opLocationMapper.getLocationByPileSnList(pileSnList);
    }

    private Map<Long, Integer> buildStationIdESPileNumMap(String
                                                                  stationSearchValue, Set<Long> locationIds, List<OpLocationPileEvseElasticDTO> esPileList) {
        Map<Long, Integer> stationIdPileNumMap = new HashMap<>();
        //如果有搜索值，应该重新搜索获取场站的所有桩进行统计
        if (StringUtils.isNotBlank(stationSearchValue)) {
            esPileList = opLocationPileEvseElastic.findAllByLocationIdIn(locationIds);

        }
        //构建站点id，站点桩集合map
        if (CollectionUtils.isNotEmpty(esPileList)) {
            for (OpLocationPileEvseElasticDTO esPileDTO : esPileList) {
                Long locationId = esPileDTO.getLocationId();
                Integer pileNum = stationIdPileNumMap.get(locationId);
                if (pileNum == null) {
                    pileNum = 0;
                }
                pileNum++;
                stationIdPileNumMap.put(locationId, pileNum);
            }
        }
        return stationIdPileNumMap;
    }

    private Map<Long, List<OpLocationEvseElasticDTO>> buildStationIdESEVSEListMap
            (List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS) {
        Map<Long, List<OpLocationEvseElasticDTO>> stationIdESEVSEListMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(opLocationEvseElasticDTOS)) {
            for (OpLocationEvseElasticDTO ESEVSE : opLocationEvseElasticDTOS) {
                Long stationId = ESEVSE.getLocationId();
                List<OpLocationEvseElasticDTO> stationEVSEList = stationIdESEVSEListMap.get(stationId);
                if (stationEVSEList == null) {
                    stationEVSEList = new ArrayList<>();
                }
                stationEVSEList.add(ESEVSE);
                stationIdESEVSEListMap.put(stationId, stationEVSEList);
            }
        }
        return stationIdESEVSEListMap;
    }

    private Map<Long, List<OpLocationPileEvseElasticDTO>> buildStationIdESPileListMap
            (List<OpLocationPileEvseElasticDTO> esPileList) {
        Map<Long, List<OpLocationPileEvseElasticDTO>> stationIdPileListMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(esPileList)) {
            for (OpLocationPileEvseElasticDTO esPile : esPileList) {
                Long stationId = esPile.getLocationId();
                List<OpLocationPileEvseElasticDTO> stationPileList = stationIdPileListMap.get(stationId);
                if (stationPileList == null) {
                    stationPileList = new ArrayList<>();
                }
                stationPileList.add(esPile);
                stationIdPileListMap.put(stationId, stationPileList);
            }
        }
        return stationIdPileListMap;
    }

    private Map<Long, OpEvseBrandEntity> buildBrandIdBrandEntityMap
            (List<OpLocationPileEvseElasticDTO> esPileList) {
        Map<Long, OpEvseBrandEntity> brandIdBrandEntityMap = new HashMap<>();
        if (CollectionUtils.isEmpty(esPileList)) {
            return brandIdBrandEntityMap;
        }
        Set<Long> brandIdSet = esPileList.stream().map(OpLocationPileEvseElasticDTO::getBrandId).collect(Collectors.toSet());
        List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectBatchIds(brandIdSet);
        if (CollectionUtils.isNotEmpty(opEvseBrandEntities)) {
            opEvseBrandEntities.forEach(brandEntity -> brandIdBrandEntityMap.put(brandEntity.getId(), brandEntity));
        }
        return brandIdBrandEntityMap;
    }

    @Override
    public Boolean updateLocationUpdateTime(Long locationId) {

        log.info("===>>>OpLocationRepositoryImpl.updateLocationUpdateTime locationId : {}",
                JSON.toJSONString(locationId));

        if (locationId == null) {
            return false;
        }

        long currentTimeMillis = System.currentTimeMillis();
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if (opLocationEntity != null) {
            opLocationEntity.setUpdatedAt(currentTimeMillis);
            opLocationMapper.updateById(opLocationEntity);
        }
        Optional<OpLocationElasticDTO> optional = opLocationElastic.findById(locationId);
        if (optional.isPresent()) {
            OpLocationElasticDTO opLocationElasticDTO = optional.get();
            opLocationElasticDTO.setUpdatedAt(currentTimeMillis);
            opLocationElastic.save(opLocationElasticDTO);
        }
        return true;
    }

    @Override
    public List<GunStatusGroupVO> stationGunStatusGroupVO(Long id) {
        List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(id);
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            return buildStationGunStatusGroupVO2(esEVSEList);
        }
        return Lists.newArrayList();
    }

    @Override
    public List<EvseSnStatusVO> gunStatusByEvseSn(List<String> EvseSnList) {
        List<OpLocationEvseElasticDTO> evseElasticDTOList = opLocationEvseElastic.findAllByEvseSnIn(EvseSnList);
        if (CollectionUtils.isNotEmpty(evseElasticDTOList)) {
            return buildOicpGunStatusVO(evseElasticDTOList);
        }
        return Lists.newArrayList();
    }

    private List<EvseSnStatusVO> buildOicpGunStatusVO(List<OpLocationEvseElasticDTO> evseElasticDTOList) {


        List<EvseSnStatusVO> evseStatusRecordTypeList = ListUtils.emptyIfNull(evseElasticDTOList)
                .stream()
                .map(elasticDTO -> EvseSnStatusVO.builder().evseSn(elasticDTO.getEvseSn())
                        .evseStatus(EvseDeviceStatusEnum.getEnumByName(elasticDTO.getState()).getName())
                        .build())
                .collect(Collectors.toList());
        return evseStatusRecordTypeList;
    }

    @Override
    public void refreshGroupName() {
        //查询es场站
        Iterable<OpLocationElasticDTO> opLocationElasticDTOIterable = opLocationElastic.findAll();
        ArrayList<OpLocationElasticDTO> esOplocationList = Lists.newArrayList(opLocationElasticDTOIterable);
        Set<Long> esGroupIds = esOplocationList.stream().map(OpLocationElasticDTO::getGroupId).filter(Objects::nonNull).collect(Collectors.toSet());
        //查询db场站
        LambdaQueryWrapper<OpLocationOperationEntity> opLocationEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationOperationEntity.class);
        List<OpLocationOperationEntity> dbOplocationList = opLocationOperationMapper.selectList(opLocationEntityLambdaQueryWrapper);
        Set<Long> dbGroupIds = dbOplocationList.stream().map(OpLocationOperationEntity::getGroupId).filter(Objects::nonNull).collect(Collectors.toSet());
        esGroupIds.addAll(dbGroupIds);

        //封装组织机构id map
        Map<Long, com.autel.cloud.pile.user.api.vo.OrganizationVO> orgMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(esGroupIds)) {
            Result<List<com.autel.cloud.pile.user.api.vo.OrganizationVO>> organizationListResult = pileUserFeign.getOrgListByIds(Lists.newArrayList(esGroupIds));
            log.info("feign查询机构机构列表：{}  {}", JSON.toJSONString(esGroupIds), JSON.toJSONString(organizationListResult));
            if (organizationListResult != null && CollectionUtils.isNotEmpty(organizationListResult.getData())) {
                List<com.autel.cloud.pile.user.api.vo.OrganizationVO> orgList = organizationListResult.getData();
                orgList.forEach(organizationVO -> orgMap.put(organizationVO.getId(), organizationVO));
            }
        }

        //给es场站设置组织机构名称
        if (CollectionUtils.isNotEmpty(esOplocationList)) {
            List<OpLocationElasticDTO> updateEsOplocationList = Lists.newArrayList();
            esOplocationList.forEach(esLocation -> {
                if (esLocation.getGroupId() != null) {
                    com.autel.cloud.pile.user.api.vo.OrganizationVO organizationVO = orgMap.get(esLocation.getGroupId());
                    if (organizationVO != null) {
                        esLocation.setGroupName(organizationVO.getName());
                        updateEsOplocationList.add(esLocation);
                    }
                }
            });
            if (CollectionUtils.isNotEmpty(updateEsOplocationList)) {
                opLocationElastic.saveAll(updateEsOplocationList);
            }
        }
        //给db场站设置组织机构名称
        if (CollectionUtils.isNotEmpty(dbOplocationList)) {
            dbOplocationList.forEach(dbLocation -> {
                if (dbLocation.getGroupId() != null) {
                    com.autel.cloud.pile.user.api.vo.OrganizationVO organizationVO = orgMap.get(dbLocation.getGroupId());
                    if (organizationVO != null) {
                        dbLocation.setGroupName(organizationVO.getName());
                        opLocationOperationMapper.updateById(dbLocation);
                    }
                }
            });
        }
    }

    @Override
    public void updateGroupName(Long groupId, String groupName) {
        //查询es场站
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        if (groupId != null) {
            boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.GROUP_ID, groupId));
        }
//        Iterable<OpLocationElasticDTO> opLocationElasticDTOIterable = opLocationElastic.search(boolQueryBuilder);
        ArrayList<OpLocationElasticDTO> esOplocationList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toCollection(Lists::newArrayList));
        //查询db场站
        LambdaQueryWrapper<OpLocationOperationEntity> opLocationEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                .eq(OpLocationOperationEntity::getGroupId, groupId);
        List<OpLocationOperationEntity> dbOplocationList = opLocationOperationMapper.selectList(opLocationEntityLambdaQueryWrapper);

        //给es场站更新组织机构名称
        if (CollectionUtils.isNotEmpty(esOplocationList)) {
            List<OpLocationElasticDTO> updateEsOplocationList = Lists.newArrayList();
            esOplocationList.forEach(esLocation -> {
                esLocation.setGroupName(groupName);
                updateEsOplocationList.add(esLocation);
            });
            if (CollectionUtils.isNotEmpty(updateEsOplocationList)) {
                opLocationElastic.saveAll(updateEsOplocationList);
            }
        }
        //给db场站更新组织机构名称
        if (CollectionUtils.isNotEmpty(dbOplocationList)) {
            dbOplocationList.forEach(dbLocation -> {
                dbLocation.setGroupName(groupName);
                opLocationOperationMapper.updateById(dbLocation);
            });
        }
    }

    @Override
    public List<LocationInfoDTO> getLocationByKeyword(String keyword) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        Long sellerId = payload.getSellerId();
        Long userId = payload.getUserId();
        List<LocationInfoDTO> resultList = new ArrayList<>();
        log.info("getLocationByKeyword,sellerId={},userId={},keyword={}", sellerId, userId, keyword);
        //获取所属场站
        List<Long> locationIds = pileUserFeign.getLocationIds().getData();
        log.info("getLocationByKeyword,locationIds={}", JSON.toJSONString(locationIds));
        if (org.springframework.util.CollectionUtils.isEmpty(locationIds)) {
            return resultList;
        }
        //根据场站和关键字查询es
        BoolQueryBuilder pileSearch = QueryBuilders.boolQuery();
        pileSearch.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId));
        pileSearch.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIds));
        if (org.springframework.util.StringUtils.hasText(keyword)) {
            keyword = QueryParserBase.escape(keyword);
            log.info("===>>>keyWord:{}", keyword);
            BoolQueryBuilder fuzzySearch = QueryBuilders.boolQuery();
            fuzzySearch.should(QueryBuilders.wildcardQuery("locationName", String.format("*%s*", keyword)));
            fuzzySearch.should(QueryBuilders.wildcardQuery(BaseConstant.PILESN, String.format("*%s*", keyword)));
            fuzzySearch.should(QueryBuilders.wildcardQuery("ruleName.keyword", String.format("*%s*", keyword)));
            fuzzySearch.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
            pileSearch.must(fuzzySearch);
        }
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(pileSearch)
                .withPageable(PageRequest.of(0, 1000))
                .withSourceFilter(new FetchSourceFilter(new String[]{"locationId", "locationName", "id", "pileSn", "name", "ruleId", "ruleName"}, null))
                .withSorts(SortBuilders.fieldSort("id").order(SortOrder.ASC))
                .build();
        SearchHitsIterator<OpLocationPileEvseElasticDTO> iterator = this.elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationPileEvseElasticDTO.class);
        while (iterator.hasNext()) {
            OpLocationPileEvseElasticDTO dto = iterator.next().getContent();
            LocationInfoDTO vo = new LocationInfoDTO();
            vo.setLocationId(dto.getLocationId());
            vo.setLocationName(dto.getLocationName());
            vo.setPileId(dto.getId());
            vo.setPileSn(dto.getPileSn());
            vo.setPileName(dto.getName());
            vo.setRuleName(dto.getRuleName());
            vo.setRuleId(dto.getRuleId());
            resultList.add(vo);
        }
        log.info("getLocationByKeyword,es resultList={}", JSON.toJSONString(resultList));
        return resultList;
    }

    @Override
    public IPage<PileDetailVO> getPilePageByKeyword(RuleSitePageDTO ruleSitePageDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        Long sellerId = payload.getSellerId();
        String keyword = ruleSitePageDTO.getKeyword();
        List<Long> locationIds = ruleSitePageDTO.getLocationIds();
        List<Long> pileIds = ruleSitePageDTO.getPileIds();
        IPage<PileDetailVO> resultPage = new Page<>(ruleSitePageDTO.getCurrent(), ruleSitePageDTO.getSize());
        log.info("getPilePageByKeyword,ruleSitePageDTO={}", JSON.toJSONString(ruleSitePageDTO));
        if (org.springframework.util.CollectionUtils.isEmpty(locationIds)) {
            return resultPage;
        }
        //根据场站和关键字查询es
        BoolQueryBuilder pileSearch = QueryBuilders.boolQuery();
        pileSearch.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId));
        pileSearch.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIds));
        pileSearch.must(QueryBuilders.termsQuery("id", pileIds));
        if (org.springframework.util.StringUtils.hasText(keyword)) {
            keyword = QueryParserBase.escape(keyword);
            BoolQueryBuilder fuzzySearch = QueryBuilders.boolQuery();
            fuzzySearch.should(QueryBuilders.wildcardQuery(BaseConstant.PILESN, String.format("*%s*", keyword)));
            fuzzySearch.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
            pileSearch.must(fuzzySearch);
        }
        int page = new BigDecimal(ruleSitePageDTO.getCurrent()).intValue() - 1;
        int size = new BigDecimal(ruleSitePageDTO.getSize()).intValue();

//        org.springframework.data.domain.Page<OpLocationPileEvseElasticDTO> searchResult = opLocationPileEvseElastic.search(pileSearch, PageRequest.of(page, size));
//        List<OpLocationPileEvseElasticDTO> contentList = searchResult.getContent();
        SearchHits<OpLocationPileEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(pileSearch).withPageable(PageRequest.of(page, size)).build(), OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> contentList = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<PileDetailVO> resultList = new ArrayList<>();
        contentList.forEach(dto -> {
            PileDetailVO vo = new PileDetailVO();
            vo.setPileId(dto.getId());
            vo.setPileSn(dto.getPileSn());
            vo.setPileName(dto.getName());
            vo.setPileType(dto.getPowerType());
            vo.setPower(dto.getPower());
            resultList.add(vo);
        });
        log.info("getPilePageByKeyword,es resultList={}", JSON.toJSONString(resultList));
        resultPage.setRecords(resultList);
        resultPage.setTotal(searchHits.getTotalHits());
        if (!resultList.isEmpty()) {
            return resultPage;
        }
        //es有结果直接返回，否则查询数据库
        resultPage = opLocationEvseMapper.selectPageByKeyword(ruleSitePageDTO, ruleSitePageDTO);
        log.info("getPilePageByKeyword,db resultPage={}", JSON.toJSONString(resultPage));
        List<PileDetailVO> records = resultPage.getRecords();
        //回填桩类型
        if (!records.isEmpty()) {
            records.forEach(dto -> {
                String evseList = dto.getEvseList();
                if (org.springframework.util.StringUtils.hasText(evseList)) {
                    Long locationEvseId = JSON.parseArray(evseList, Long.class).get(0);
                    OpLocationConnectorEntity entity = opLocationConnectorMapper.selectOne(new LambdaQueryWrapper<OpLocationConnectorEntity>()
                            .select(OpLocationConnectorEntity::getId, OpLocationConnectorEntity::getPowerType, OpLocationConnectorEntity::getPower)
                            .eq(OpLocationConnectorEntity::getLocationEvseId, locationEvseId)
                            .eq(OpLocationConnectorEntity::getDeleted, 0));
                    if (entity != null) {
                        dto.setPileType(entity.getPowerType());
                        dto.setPower(entity.getPower());
                    }
                }
            });
        }
        return resultPage;
    }

    @Override
    public List<SiteInfoVo> getSiteList(List<Long> locationIds, String keyword) {
        log.info("getSiteList,locationIds={},keyword={}", JSON.toJSONString(locationIds), keyword);
        List<SiteInfoVo> resultList = new ArrayList<>();
        if (org.springframework.util.CollectionUtils.isEmpty(locationIds)) {
            return resultList;
        }
        //先查询es
        BoolQueryBuilder pileSearch = QueryBuilders.boolQuery();
        pileSearch.must(QueryBuilders.termsQuery("locationId", locationIds));
        if (org.springframework.util.StringUtils.hasText(keyword)) {
            keyword = QueryParserBase.escape(keyword);
            BoolQueryBuilder fuzzySearch = QueryBuilders.boolQuery();
            fuzzySearch.should(QueryBuilders.wildcardQuery("locationName", "*" + keyword + "*"));
            fuzzySearch.should(QueryBuilders.wildcardQuery("pileSn", "*" + keyword + "*"));
            pileSearch.must(fuzzySearch);
        }
        SearchHitsIterator<OpLocationPileEvseElasticDTO> search = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                .withQuery(pileSearch)
                .withPageable(PageRequest.of(0, 1000))
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "locationId", "locationName", "pileSn", "name", "power", "powerType"}, null))
                .build(), OpLocationPileEvseElasticDTO.class);
        Map<Long, List<PileInfoVO>> tmpMap = new HashMap<>();
        Map<Long, String> locationNames = new HashMap<>();
        while (search.hasNext()) {
            OpLocationPileEvseElasticDTO pileDto = search.next().getContent();
            Long locationId = pileDto.getLocationId();
            String locationName = pileDto.getLocationName();

            PileInfoVO infoVO = new PileInfoVO();
            infoVO.setPileSn(pileDto.getPileSn());
            infoVO.setPileId(pileDto.getId());
            infoVO.setPileName(pileDto.getName());
            infoVO.setPileType(pileDto.getPowerType());
            infoVO.setPower(pileDto.getPower());
            List<PileInfoVO> list = Arrays.asList(infoVO);
            tmpMap.merge(locationId, list, (f, s) -> {
                f.addAll(s);
                return f;
            });
            locationNames.computeIfAbsent(locationId, k -> locationName);
        }
        tmpMap.forEach((locationId, list) -> {
            SiteInfoVo vo = new SiteInfoVo();
            vo.setLocationId(locationId);
            vo.setLocationName(locationNames.get(locationId));
            vo.setPiles(list);
            resultList.add(vo);
        });
        log.info("getLocationPileList,es resultList={}", JSON.toJSONString(resultList));
        if (!resultList.isEmpty()) {
            return resultList;
        }
        //查询数据库
        List<PileDetailVO> dbResultList = opLocationPileEvseMapper.getLocationPileList(locationIds, keyword);
        log.info("getLocationPileList,db dbResultList={}", JSON.toJSONString(dbResultList));
        if (!org.springframework.util.CollectionUtils.isEmpty(dbResultList)) {
            Map<Long, List<PileDetailVO>> map = dbResultList.stream().collect(Collectors.groupingBy(PileDetailVO::getLocationId));
            map.forEach((locationId, pileList) -> {
                SiteInfoVo vo = new SiteInfoVo();
                vo.setLocationId(locationId);
                vo.setLocationName(pileList.stream().findFirst().get().getLocationName());
                List<PileInfoVO> piles = pileList.stream().map(pile -> {
                    PileInfoVO infoVO = new PileInfoVO();
                    infoVO.setPileSn(pile.getPileSn());
                    infoVO.setPileId(pile.getPileId());
                    infoVO.setPileName(pile.getPileName());
                    String evseList = pile.getEvseList();
                    if (org.springframework.util.StringUtils.hasText(evseList)) {
                        Long locationEvseId = JSON.parseArray(evseList, Long.class).get(0);
                        OpLocationConnectorEntity entity = opLocationConnectorMapper.selectOne(new LambdaQueryWrapper<OpLocationConnectorEntity>()
                                .select(OpLocationConnectorEntity::getId, OpLocationConnectorEntity::getPowerType, OpLocationConnectorEntity::getPower)
                                .eq(OpLocationConnectorEntity::getLocationEvseId, locationEvseId)
                                .eq(OpLocationConnectorEntity::getDeleted, 0));
                        if (entity != null) {
                            infoVO.setPileType(entity.getPowerType());
                            infoVO.setPower(entity.getPower());
                        }
                    }
                    return infoVO;
                }).collect(Collectors.toList());
                vo.setPiles(piles);
                resultList.add(vo);
            });
        }
        return resultList;
    }

    @Override
    public List<OpLocationEntity> findAll() {
        LambdaQueryWrapper<OpLocationEntity> updateWrapper = new LambdaQueryWrapper<>();
        updateWrapper.select(OpLocationEntity::getId, OpLocationEntity::getTimeZone, OpLocationEntity::getLatitude, OpLocationEntity::getLongitude,
                OpLocationEntity::getZoneId,OpLocationEntity::getName,OpLocationEntity::getDeleted);
        return list(updateWrapper);
    }

    @Override
    public List<OpLocationEntity> populationZoneId(List<OpLocationEntity> fillterResultList) {

        OkHttpClient okHttpClient = new OkHttpClient();
        String url = "https://maps.googleapis.com";
        GoogleApi googleApi = Feign.builder().decoder(new Decoder() {
            @Override
            public Object decode(Response response, Type type) throws IOException, DecodeException, FeignException {
                if (response.status() == 404) {
                    return Util.emptyValueOf(type);
                }
                if (response.body() == null) {
                    return null;
                }
                InputStream stream = response.body().asInputStream();
                try {
                    return JSON.parseObject(stream, type);
                } catch (Exception e) {
                    throw new RuntimeException();
                } finally {
                    stream.close();
                }
            }
        }).client(okHttpClient).target(GoogleApi.class, url);

        for (OpLocationEntity opLocationEntity : fillterResultList) {
            long nowTime = System.currentTimeMillis();
            GoogleMapDTO googleMapDTO = googleApi.getZoneId(opLocationEntity.getLatitude() + "," + opLocationEntity.getLongitude(), nowTime / 1000);
            log.info("syncTimezoneName,googleMapDTO={}", googleMapDTO);
            if (googleMapDTO != null && "OK".equalsIgnoreCase(googleMapDTO.getStatus())) {
                opLocationEntity.setZoneId(googleMapDTO.getTimeZoneId());
            } else {
                log.error("call google api failed {}", JSON.toJSONString(opLocationEntity));
            }
        }
        return fillterResultList;
    }

    @Override
    public List<OpLocationEntity> saveZoneId(List<OpLocationEntity> list) {
        for (OpLocationEntity opLocationEntity : list) {
            OpLocationEntity target = opLocationMapper.selectById(opLocationEntity.getId());
            Assert.notNull(target, opLocationEntity.getId() + " OpLocationEntity is null");
            target.setZoneId(opLocationEntity.getZoneId());
            updateById(target);
            Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(opLocationEntity.getId());
            if (optionalOpLocationElasticDTO.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
                opLocationElasticDTO.setZoneId(opLocationEntity.getZoneId());
                opLocationElastic.save(opLocationElasticDTO);
            }
            mqSender.send(AmqpConstant.LOCATION_TIMEZONE_ID_SYNC, target);
        }
        return list;
    }

    @Override
    public Boolean syncZoneId(List<Long> locationIds) {
        OkHttpClient okHttpClient = new OkHttpClient();
        String url = "https://maps.googleapis.com";
        GoogleApi googleApi = Feign.builder().decoder(new Decoder() {
            @Override
            public Object decode(Response response, Type type) throws IOException, DecodeException, FeignException {
                if (response.status() == 404) {
                    return Util.emptyValueOf(type);
                }
                if (response.body() == null) {
                    return null;
                }
                InputStream stream = response.body().asInputStream();
                try {
                    return JSON.parseObject(stream, type);
                } catch (Exception e) {
                    throw new RuntimeException();
                } finally {
                    stream.close();
                }
            }
        }).client(okHttpClient).target(GoogleApi.class, url);
        log.info("syncTimezoneName,locationIds={}", locationIds);
        //同步数据库数据
        LambdaUpdateWrapper<OpLocationEntity> updateWrapper = new LambdaUpdateWrapper<>();
        //updateWrapper.eq(OpLocationEntity::getDeleted, 0); 删除的场站数据也要更新zonId字段
        //如果ids有值，进行拼装特定值更新条件
        if (locationIds != null && !locationIds.isEmpty()) {
            updateWrapper.in(OpLocationEntity::getId, locationIds);
        }
        List<OpLocationEntity> opLocationEntityList = this.list(updateWrapper);
        if (!opLocationEntityList.isEmpty()) {
            List<OpLocationEntity> fillterResultList = opLocationEntityList.stream().filter(item -> item.getZoneId() != null).collect(Collectors.toList());
            log.info("syncTimezoneName,fillterResultList={}", fillterResultList);
            long nowTime = System.currentTimeMillis();
            //timeZoneName是null的数据更新
            if (fillterResultList != null && !fillterResultList.isEmpty()) {
                for (OpLocationEntity opLocationEntity : fillterResultList) {
                    GoogleMapDTO googleMapDTO = googleApi.getZoneId(opLocationEntity.getLatitude() + "," + opLocationEntity.getLongitude(), nowTime / 1000);
                    log.info("syncTimezoneName,googleMapDTO={}", googleMapDTO);
                    if (googleMapDTO != null && "OK".equalsIgnoreCase(googleMapDTO.getStatus())) {
                        //更新数据库(timeZoneName、更新时间)
                        opLocationEntity.setZoneId(googleMapDTO.getTimeZoneId());
                        opLocationEntity.setUpdatedAt(nowTime);
                        this.updateById(opLocationEntity);
                        OpLocationDTO opLocationDTO = new OpLocationDTO();
                        opLocationDTO.setId(opLocationEntity.getId());
                        opLocationDTO.setTimeZone(opLocationEntity.getTimeZone());
                        opLocationDTO.setZoneId(googleMapDTO.getTimeZoneId());
                        opLocationDTO.setName(opLocationEntity.getName());
                        mqSender.send(AmqpConstant.LOCATION_TIMEZONE_ID_SYNC, opLocationDTO);
                        //更新ES
                        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(opLocationEntity.getId());
                        if (optionalOpLocationElasticDTO.isPresent()) {
                            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
                            opLocationElasticDTO.setZoneId(opLocationEntity.getZoneId());
                            opLocationElasticDTO.setUpdatedAt(nowTime);
                            opLocationElastic.save(opLocationElasticDTO);
                        } else {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }

    /**
     * 构建场站状态
     *
     * @param stationEsEVSEList 场站设备列表
     * @return 场站状态
     */
    private Integer buildLocationStatus(List<OpLocationEvseElasticDTO> stationEsEVSEList) {
        //默认不可用
        Integer locationStatusCode = LocationAPPStatusEnum.UNAVAILABLE.getCode();
        try {
            int totalNum = 0;
            int freeNum = 0;
            int chargingNum = 0;
            if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                stationEsEVSEList = stationEsEVSEList.stream().filter(m -> !ObjectUtils.isEmpty(m.getTariffId())).collect(Collectors.toList());
                totalNum = stationEsEVSEList.size();
                for (OpLocationEvseElasticDTO esEVSE : stationEsEVSEList) {
                    String esStateName = esEVSE.getState();
                    LocationEVSESAPPStatusEnum evseStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(esStateName);
                    if (evseStatusEnum.getCode().equals(LocationEVSESAPPStatusEnum.AVAILABLE.getCode())) {
                        freeNum++;
                    } else if (evseStatusEnum.getCode().equals(LocationEVSESAPPStatusEnum.CHARGING.getCode())) {
                        chargingNum++;
                    }
                }
            }
            if (totalNum > 0) {
                log.info("totalNum:{}  freeNum:{}  chargingNum:{}  freeNum/totalNum:{} freeNum/totalNum<=0.2:{}",
                        totalNum, freeNum, chargingNum, (float) freeNum / (float) totalNum, (float) freeNum / (float) totalNum <= (float) 0.2);
                //空闲
                // 若场站总枪口数不大于4个，至少有1个枪为可用状态时，定义为空闲
                // 若场站总枪口数不小于5个，可用状态枪口数大于总枪口数的20%时，定义为空闲
                if ((totalNum <= 4 && freeNum >= 1) || (totalNum >= 5 && (float) freeNum / (float) totalNum > (float) 0.2)) {
                    locationStatusCode = LocationAPPStatusEnum.AVAILABLE.getCode();
                }
                //繁忙
                // 若场站总枪口数不大于4个，全部枪都不是可用状态，并且有一个枪是使用中，定义为繁忙
                // 若场站总枪口数不小于5个，可用状态枪口数小于总枪口数的20%时，定义为繁忙
                else if ((totalNum <= 4 && chargingNum >= 1 && freeNum == 0) || (totalNum >= 5 && (chargingNum + freeNum) >= 1 && (float) freeNum / (float) totalNum <= (float) 0.2)) {
                    locationStatusCode = LocationAPPStatusEnum.BUSY.getCode();
                }
            }
        } catch (Exception e) {
            log.error("locationStatusCode:", e);
        }
        return locationStatusCode;
    }

    private Integer buildLocationStatus2(List<OpLocationEvseExpandElasticDTO> stationEsEVSEList) {
        //默认不可用
        Integer locationStatusCode = LocationAPPStatusEnum.UNAVAILABLE.getCode();
        try {
            int totalNum = 0;
            int freeNum = 0;
            int chargingNum = 0;
            if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                totalNum = stationEsEVSEList.size();
                for (OpLocationEvseExpandElasticDTO esEVSE : stationEsEVSEList) {
                    String esStateName = esEVSE.getGunState();
                    LocationEVSESAPPStatusEnum evseStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(esStateName);
                    if (evseStatusEnum.getCode().equals(LocationEVSESAPPStatusEnum.AVAILABLE.getCode())) {
                        freeNum++;
                    } else if (evseStatusEnum.getCode().equals(LocationEVSESAPPStatusEnum.CHARGING.getCode())) {
                        chargingNum++;
                    }
                }
            }
            if (totalNum > 0) {
                log.info("totalNum:{}  freeNum:{}  chargingNum:{}  freeNum/totalNum:{} freeNum/totalNum<=0.2:{}",
                        totalNum, freeNum, chargingNum, (float) freeNum / (float) totalNum, (float) freeNum / (float) totalNum <= (float) 0.2);
                //空闲
                // 若场站总枪口数不大于4个，至少有1个枪为可用状态时，定义为空闲
                // 若场站总枪口数不小于5个，可用状态枪口数大于总枪口数的20%时，定义为空闲
                if ((totalNum <= 4 && freeNum >= 1) || (totalNum >= 5 && (float) freeNum / (float) totalNum > (float) 0.2)) {
                    locationStatusCode = LocationAPPStatusEnum.AVAILABLE.getCode();
                }
                //繁忙
                // 若场站总枪口数不大于4个，全部枪都不是可用状态，并且有一个枪是使用中，定义为繁忙
                // 若场站总枪口数不小于5个，可用状态枪口数小于总枪口数的20%时，定义为繁忙
                else if ((totalNum <= 4 && chargingNum >= 1 && freeNum == 0) || (totalNum >= 5 && (chargingNum + freeNum) >= 1 && (float) freeNum / (float) totalNum <= (float) 0.2)) {
                    locationStatusCode = LocationAPPStatusEnum.BUSY.getCode();
                }
            }
        } catch (Exception e) {
            log.error("locationStatusCode:", e);
        }
        return locationStatusCode;
    }

    /**
     * 构建hubject场站状态
     *
     * @param collect 设备状态
     * @return 场站状态
     */
    private Integer buildHubjectLocationStatus(List<EvseStatusRecordDTO> collect) {

        int status = LocationAPPStatusEnum.UNAVAILABLE.getCode();
        int totalNum = collect.size();
        int freeNum = 0;
        int chargingNum = 0;

        for (EvseStatusRecordDTO evseStatus : collect) {
            if (Objects.equals(evseStatus.getEvseStatus(), EvseDeviceStatusEnum.AVAILABLE.getCode())) {
                freeNum++;
            } else if (Objects.equals(evseStatus.getEvseStatus(), EvseDeviceStatusEnum.CHARGING.getCode())) {
                chargingNum++;
            }
        }

        //空闲
        // 若场站总枪口数不大于4个，至少有1个枪为可用状态时，定义为空闲
        // 若场站总枪口数不小于5个，可用状态枪口数大于总枪口数的20%时，定义为空闲
        if ((totalNum <= 4 && freeNum >= 1) || (totalNum >= 5 && (float) freeNum / (float) totalNum > (float) 0.2)) {
            status = LocationAPPStatusEnum.AVAILABLE.getCode();
        }
        //繁忙
        // 若场站总枪口数不大于4个，全部枪都不是可用状态，并且有一个枪是使用中，定义为繁忙
        // 若场站总枪口数不小于5个，可用状态枪口数小于总枪口数的20%时，定义为繁忙
        else if ((totalNum <= 4 && chargingNum >= 1 && freeNum == 0) || (totalNum >= 5 && (chargingNum + freeNum) >= 1 && (float) freeNum / (float) totalNum <= (float) 0.2)) {
            status = LocationAPPStatusEnum.BUSY.getCode();
        }

        return status;
    }

    /**
     * 查询计费规则模板封装成map
     *
     * @param tariffIdSet 计费规则id
     * @return 计费规则map
     */
    private Map<Long, CostModelRuleDTO> buildTariffIdCostModelRuleDTOMap(Set<Long> tariffIdSet) {
        Map<Long, CostModelRuleDTO> tariffIdCostModelRuleDTOMap = new HashMap<>();
        List<CostModelRuleDTO> tariffList = getCostModelRuleByTariffIdSet(tariffIdSet);
        if (CollectionUtils.isNotEmpty(tariffList)) {
            //封装成计费规则map
            for (CostModelRuleDTO costModelRule : tariffList) {
                tariffIdCostModelRuleDTOMap.put(costModelRule.getId(), costModelRule);
            }
        }
        return tariffIdCostModelRuleDTOMap;
    }

    /**
     * 查询计费规则
     *
     * @param tariffIdSet 计费规则id集合
     * @return 计费规则
     */
    private List<CostModelRuleDTO> getCostModelRuleByTariffIdSet(Set<Long> tariffIdSet) {
        List<CostModelRuleDTO> costModelRuleList = Lists.newArrayList();
        try {
            if (CollectionUtils.isNotEmpty(tariffIdSet)) {
                log.info("计费规则id集合:{}", JSON.toJSONString(tariffIdSet));
                CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
                costModelRuleDTO.setRuleIds(Lists.newArrayList(tariffIdSet));
                Result<List<CostModelRuleDTO>> tariffAPPFeignResult = tariffFeignClient.queryListByIds(costModelRuleDTO);
                log.info("feign调用tariffAPPFeign结果：{}", JSON.toJSONString(tariffAPPFeignResult));
                if (tariffAPPFeignResult != null && tariffAPPFeignResult.getCode() == HttpStatus.OK.value()) {
                    costModelRuleList = tariffAPPFeignResult.getData();
                }
            }
        } catch (Exception e) {
            log.error("costModelRuleList:", e);
        }
        return costModelRuleList;
    }

    /**
     * 统计功率/枪类型的枪数量
     *
     * @param esEVSEList 场站设备
     * @return 根据功率/枪类型统计枪数
     */
    private List<GunPowerAndTypeGroupVO> buildGunPowerAndTypeGroupVOList
    (List<OpLocationEvseElasticDTO> esEVSEList) {
        List<GunPowerAndTypeGroupVO> gunPowerAndTypeGroupVOList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(esEVSEList)) {
            //构建两个Map
            Map<String, Integer> totalCountMap = new HashMap<>();
            Map<String, Integer> freeCountMap = new HashMap<>();
            for (OpLocationEvseElasticDTO esEVSE : esEVSEList) {
                Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                Integer evseGunTypeCode = esEVSE.getGunType() == null ? ConnectorGunTypeEnum.CCS_COMBO_2.getCode() : esEVSE.getGunType();
                String key = buildGunPowerAndTypeKey(evsePower, evseGunTypeCode);
                //总数量
                Integer totalCount = totalCountMap.get(key);
                if (totalCount == null) {
                    totalCount = 0;
                }
                totalCount++;
                totalCountMap.put(key, totalCount);
                //可用数量
                if (Objects.equals(esEVSE.getState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                    Integer freeCount = freeCountMap.get(key);
                    if (freeCount == null) {
                        freeCount = 0;
                    }
                    freeCount++;
                    freeCountMap.put(key, freeCount);
                }
            }
            //将map封装成对象
            for (Map.Entry<String, Integer> entry : totalCountMap.entrySet()) {
                String key = entry.getKey();
                String[] powerAndGunTypeCodeArray = key.split("-");
                Double power = Double.valueOf(powerAndGunTypeCodeArray[0]);
                Integer gunTypeCode = Integer.valueOf(powerAndGunTypeCodeArray[1]);
                Integer totalCount = entry.getValue();
                Integer freeCount = 0;
                if (freeCountMap.get(key) != null) {
                    freeCount = freeCountMap.get(key);
                }
                GunPowerAndTypeGroupVO gunPowerAndTypeGroupVO = new GunPowerAndTypeGroupVO();
                gunPowerAndTypeGroupVO.setPower(power);
                gunPowerAndTypeGroupVO.setGunTypeCode(gunTypeCode);
                gunPowerAndTypeGroupVO.setTotalCount(totalCount);
                gunPowerAndTypeGroupVO.setFreeCount(freeCount);
                gunPowerAndTypeGroupVOList.add(gunPowerAndTypeGroupVO);
            }
        }
        return gunPowerAndTypeGroupVOList;
    }

    private String buildGunPowerAndTypeKey(Double evsePower, Integer evseGunTypeCode) {
        return evsePower + "-" + evseGunTypeCode;
    }

    @Override
    public SearchItemInfoVO searchInfo(SearchItemInfoDTO searchItemInfoDTO) {
        log.info("OpLocationRepositoryImpl.searchInfo and searchItemInfoDTO = " + JSON.toJSONString(searchItemInfoDTO));
        SearchItemInfoVO searchItemInfoVO = SearchItemInfoVO.builder().build();
        if (StringUtils.isBlank(searchItemInfoDTO.getSearchValue())) {
            return searchItemInfoVO;
        }
        // 搜索组织
        if (StringUtils.isNotBlank(searchItemInfoDTO.getSearchValue())) {
            Result<List<OrganizationByNameVO>> orgListResult = pileUserFeign.searchOrgByName(searchItemInfoDTO.getSearchValue());
            Optional.ofNullable(orgListResult.getData()).orElse(Collections.emptyList()).forEach(org -> searchItemInfoVO.addOrg(org.getId(), org.getName()));
        }

        Result<List<Long>> currentUserLocationIdsResult = pileUserServiceFeign.getLocationIds();
        List<Long> locationIdList = currentUserLocationIdsResult.getData();
        log.info("OpLocationRepositoryImpl.searchInfo and locationIdList = " + JSON.toJSONString(locationIdList));

        // 搜索场站
        BoolQueryBuilder locationBoolQueryBuilder = QueryBuilders.boolQuery();
        BoolQueryBuilder locationSearchBuilder = QueryBuilders.boolQuery();
        locationSearchBuilder.must(QueryBuilders.wildcardQuery("name", "*" + QueryParserBase.escape(searchItemInfoDTO.getSearchValue()) + "*"));
        locationSearchBuilder.must(QueryBuilders.termsQuery("id", locationIdList));
        locationBoolQueryBuilder.must(locationSearchBuilder);
        log.info("OpLocationRepositoryImpl.searchInfo and locationBoolQueryBuilder = " + JSON.toJSONString(locationBoolQueryBuilder));
        Iterable<OpLocationElasticDTO> opLocationElasticDTOIterable =
//                opLocationElastic.search(locationBoolQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(locationBoolQueryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        opLocationElasticDTOIterable.forEach(location -> searchItemInfoVO.addLocation(location.getId(), location.getName()));

        // 搜索桩
        BoolQueryBuilder pileBoolQueryBuilder = QueryBuilders.boolQuery();

        BoolQueryBuilder pileSearchBuilder = QueryBuilders.boolQuery();
        pileSearchBuilder.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIdList));

        BoolQueryBuilder snNameQueryBuilder = QueryBuilders.boolQuery();
        snNameQueryBuilder.should(QueryBuilders.wildcardQuery("name", "*" + QueryParserBase.escape(searchItemInfoDTO.getSearchValue()) + "*"));
        snNameQueryBuilder.should(QueryBuilders.wildcardQuery(BaseConstant.PILESN, "*" + QueryParserBase.escape(searchItemInfoDTO.getSearchValue()) + "*"));

        pileBoolQueryBuilder.must(pileSearchBuilder);
        pileBoolQueryBuilder.must(snNameQueryBuilder);
        log.info("OpLocationRepositoryImpl.searchInfo and pileBoolQueryBuilder = " + JSON.toJSONString(pileBoolQueryBuilder));
        Iterable<OpLocationPileEvseElasticDTO> opLocationPileElasticDTOIterable =
//                opLocationPileEvseElastic.search(pileBoolQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(pileBoolQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        opLocationPileElasticDTOIterable.forEach(pile -> searchItemInfoVO.addPile(pile.getId(), pile.getPileSn()));
        return searchItemInfoVO;
    }

    @Override
    public Page<OpLocationPageVO> searchByPage(OpLocationPageDTO opLocationPageDTO) {
        Page<OpLocationPageVO> resultPage = new Page<>(opLocationPageDTO.getPage(), opLocationPageDTO.getPageSize(), 0);
        resultPage.setRecords(Lists.newArrayList());

        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        Boolean isSearchPile = Boolean.FALSE;
        if (opLocationPageDTO.getOrgId() != null) {
            Result<List<Long>> orgIdsResult = pileUserServiceFeign.getOrgChildrenList(opLocationPageDTO.getOrgId());
            if (orgIdsResult != null && orgIdsResult.getCode() == HttpStatus.OK.value()) {
                List<Long> orgIds = new ArrayList<>();
                if (Boolean.TRUE.equals(Optional.ofNullable(opLocationPageDTO.getHasSubOrg()).orElse(Boolean.TRUE))) {
                    orgIds = Optional.ofNullable(orgIdsResult.getData()).orElse(Lists.newArrayList());
                }
                orgIds.add(opLocationPageDTO.getOrgId());
                boolQueryBuilder.must(QueryBuilders.termsQuery(BaseConstant.GROUP_ID, orgIds));
            } else {
                return resultPage;
            }
        } else if (opLocationPageDTO.getLocationId() != null) {
            boolQueryBuilder.must(QueryBuilders.termQuery("id", opLocationPageDTO.getLocationId()));
        } else if (opLocationPageDTO.getPileId() != null) {
            Optional<OpLocationPileEvseElasticDTO> pileOptional = opLocationPileEvseElastic.findById(opLocationPageDTO.getPileId());
            if (pileOptional.isPresent()) {
                boolQueryBuilder.must(QueryBuilders.termQuery("id", pileOptional.get().getLocationId()));
            } else {
                return resultPage;
            }
            isSearchPile = Boolean.TRUE;
        }

        Result<List<Long>> currentUserLocationIdsResult = pileUserServiceFeign.getLocationIds();
        if (currentUserLocationIdsResult != null && currentUserLocationIdsResult.getCode() == HttpStatus.OK.value()) {
            List<Long> currentUserLocationIds = currentUserLocationIdsResult.getData();
            if (CollectionUtils.isNotEmpty(currentUserLocationIds)) {
                boolQueryBuilder.must(QueryBuilders.termsQuery("id", currentUserLocationIds));
            } else {
                return resultPage;
            }
        }

//        Iterable<OpLocationElasticDTO> opLocationElasticDTOIterable = opLocationElastic.search(boolQueryBuilder);
        List<OpLocationElasticDTO> opLocationElasticDTOS = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationPageVO> opLocationPageVOList = DozerConvert.mapList(opLocationElasticDTOS, OpLocationPageVO.class);
        if (CollectionUtils.isEmpty(opLocationPageVOList)) {
            return resultPage;
        }
        sortLocation(opLocationPageVOList);

        List<Long> locationIdList = opLocationPageVOList.stream().map(OpLocationPageVO::getId)
                .filter(Objects::nonNull).collect(Collectors.toList());
        Set<Long> locationIdSet = new HashSet<>(locationIdList);
//        Future<Map<Long, MonthReportVO>> future = getMonthReport(locationIdList);

        List<OpLocationEvseElasticDTO> evseList = opLocationEvseElastic.findAllByLocationIdIn(locationIdSet);
        List<OpLocationPileEvseElasticDTO> pileList = opLocationPileEvseElastic.findAllByLocationIdIn(locationIdSet);

        Future<Map<Long, OpEvseBrandEntity>> brandFuture = getPileBrand(pileList);
        List<OpLocationDetailImageVO> imageVOList = opLocationImageMapper.selectImageByLocationIdList(locationIdList);
        Map<Long, List<OpLocationDetailImageVO>> imageVOMap = imageVOList.stream()
                .collect(Collectors.groupingBy(OpLocationDetailImageVO::getLocationId));

        Map<Long, List<OpLocationEvseElasticDTO>> evseLocationMap = evseList.stream()
                .collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getLocationId));
        Map<Long, List<OpLocationPileEvseElasticDTO>> pileLocationMap = pileList.stream()
                .collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));

        Boolean finalIsSearchPile = isSearchPile;
        opLocationPageVOList.forEach(opLocationPageVO -> {
            String taxConfiguration = opLocationPageVO.getTaxConfiguration();
            if (StringUtils.isNotBlank(taxConfiguration)) {
                opLocationPageVO.setTaxDTO(JSON.parseObject(taxConfiguration, TaxDTO.class));
            }
            // 补图片
            opLocationPageVO.setImageList(imageVOMap.get(opLocationPageVO.getId()));

            // 是否运营
            Long operationDate = opLocationPageVO.getOperationDate();
            if (operationDate != null) {
                opLocationPageVO.setOperate(new Date().getTime() >= operationDate);
            }

            // 补充统计数据
            List<OpLocationEvseElasticDTO> evseLocationList = Optional
                    .ofNullable(evseLocationMap.get(opLocationPageVO.getId())).orElse(Collections.emptyList());
            List<OpLocationPileEvseElasticDTO> pileLocationList = Optional
                    .ofNullable(pileLocationMap.get(opLocationPageVO.getId())).orElse(Collections.emptyList());
            Map<Long, OpLocationEvseElasticDTO> evseIdMap = evseLocationList.stream()
                    .collect(Collectors.toMap(OpLocationEvseElasticDTO::getId, Function.identity()));

            opLocationPageVO.setEvseNum(evseLocationList.size());
            opLocationPageVO.setPileNum(pileLocationList.size());

            //不同功率枪数统计
            opLocationPageVO.setPowerGroupVOS(buildStationPowerGroupVOList(evseLocationList));
            //不同类型枪数统计
            opLocationPageVO.setGunTypeGroupVOS(buildStationGunTypeGroupVOList2(evseLocationList));
            //不同枪状态统计
            opLocationPageVO.setGunStatusGroupVOS(buildStationGunStatusGroupVO2(evseLocationList));
            //桩集合
            Map<Long, OpEvseBrandEntity> brandEntityMap = getPileBrand(brandFuture);
            List<PilePageVO> pilePageVOList;
            if (Boolean.FALSE.equals(finalIsSearchPile)) {
                pilePageVOList = buildPilePageVOList(pileLocationList, evseIdMap, brandEntityMap);
            } else {
                List<OpLocationPileEvseElasticDTO> list = pileLocationList.stream()
                        .filter(item -> item.getId().equals(opLocationPageDTO.getPileId())).collect(Collectors.toList());
                pilePageVOList = buildPilePageVOList(list, evseIdMap, brandEntityMap);
            }

            opLocationPageVO.setPileList(pilePageVOList);

            //站点当月收入和新增充电用户数  大数据在处理 了这里逻辑不需要了
//            Map<Long, MonthReportVO> statisticsMap = getMonthReport(future);
//            MonthReportVO monthReportVO = statisticsMap.get(opLocationPageVO.getId());
//            if (monthReportVO != null) {
//                opLocationPageVO.setIncome(monthReportVO.getIncome());
//                opLocationPageVO.setCountCustomer(monthReportVO.getCountCustomer());
//            } else {
//                opLocationPageVO.setIncome(BigDecimal.valueOf(0));
//                opLocationPageVO.setCountCustomer(0L);
//            }
        });
        resultPage.setRecords(opLocationPageVOList);
        resultPage.setTotal(opLocationPageVOList.size());
        return resultPage;
    }

    @Override
    public Page<OpLocationPageVO> searchByPageV2(OpLocationPageDTO dto) {
        try {
            Page<OpLocationPageVO> resultPage = new Page<>(dto.getPage(), dto.getPageSize(), 0);
            resultPage.setRecords(Lists.newArrayList());
            String keyword = dto.getSearchValue();

            BoolQueryBuilder search = QueryBuilders.boolQuery();

            Result<List<Long>> currentUserLocationIdsResult = pileUserServiceFeign.getLocationIds();
            List<Long> locationIds = null;
            if (currentUserLocationIdsResult != null && currentUserLocationIdsResult.getCode() == HttpStatus.OK.value()) {
                locationIds = currentUserLocationIdsResult.getData();
                if (CollectionUtils.isNotEmpty(locationIds)) {
                    search.must(QueryBuilders.termsQuery("id", locationIds));
                } else {
                    return resultPage;
                }
            }
            if (StringUtils.isNotEmpty(keyword)) {
                keyword = QueryParserBase.escape(keyword);
                BoolQueryBuilder fuzzy = QueryBuilders.boolQuery();
                //按场站名称搜索
                fuzzy.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
                //按桩名称或SN搜索
                List<OpLocationPileEvseElasticDTO> pileDtoList = this.opLocationPileEvseRepository.findList(locationIds, keyword);
                if (CollectionUtils.isNotEmpty(pileDtoList)) {
                    fuzzy.should(QueryBuilders.termsQuery("id", pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getLocationId).distinct().collect(Collectors.toList())));
                }
                search.must(fuzzy);
            }

            //新增排序
            Integer page = dto.getPage();
            Integer pageSize = dto.getPageSize();
            String orderBy = dto.getOrderBy();
            String orderType = dto.getOrderType();
            SortOrder sortOrder = SortOrder.DESC;
            if (StringUtils.isEmpty(orderBy)) {
                orderBy = "createdAt";
            }
            if (StringUtils.isNotEmpty(orderType)) {
                sortOrder = SortOrder.fromString(orderType);
            }
            SearchHits<OpLocationElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                    .withQuery(search)
                    .withPageable(PageRequest.of(page - 1, pageSize))
                    .withSorts(SortBuilders.fieldSort(orderBy).order(sortOrder))
                    .build(), OpLocationElasticDTO.class);
            long totalHits = searchHits.getTotalHits();
            List<OpLocationElasticDTO> opLocationElasticDTOS = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
            List<OpLocationPageVO> opLocationPageVOList = DozerConvert.mapList(opLocationElasticDTOS, OpLocationPageVO.class);
            if (CollectionUtils.isEmpty(opLocationPageVOList)) {
                return resultPage;
            }



            log.info("opLocationPageVOList-->{}",JSON.toJSONString(opLocationPageVOList));

            List<Long> locationIdList = opLocationPageVOList.stream().map(OpLocationPageVO::getId)
                    .filter(Objects::nonNull).collect(Collectors.toList());
            Set<Long> locationIdSet = new HashSet<>(locationIdList);

//        List<OpLocationPileEvseElasticDTO> pileList = opLocationPileEvseElastic.findAllByLocationIdIn(locationIdSet);



            //ES查询每个场站下的桩数量
            TermsAggregationBuilder locationQueryBuilder = AggregationBuilders.terms("locationId").field("locationId").size(pageSize);
            locationQueryBuilder.subAggregation(AggregationBuilders.topHits("top_docs").size(10));
            BoolQueryBuilder locationBuilder = QueryBuilders.boolQuery();
            if (StringUtils.isNotEmpty(keyword)) {
                BoolQueryBuilder fuzzy = QueryBuilders.boolQuery();
                fuzzy.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
                fuzzy.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyword)));
                locationBuilder.must(fuzzy);
            }
            locationBuilder.must(QueryBuilders.termsQuery("locationId", locationIdSet));
            NativeSearchQuery locationSearchQuery = new NativeSearchQueryBuilder()
                    .withQuery(locationBuilder)
                    .withAggregations(locationQueryBuilder).build();
            ElasticsearchAggregations locationAggregations = (ElasticsearchAggregations) elasticsearchRestTemplate
                    .searchForStream(locationSearchQuery, OpLocationPileEvseElasticDTO.class).getAggregations();
            Map<String, Aggregation> queryResultAsMap = locationAggregations.aggregations().asMap();
            ParsedTerms locationIdTerms = (ParsedTerms) queryResultAsMap.get("locationId");
            List<? extends Terms.Bucket> locationBuckets = locationIdTerms.getBuckets();
            List<OpLocationPileEvseElasticDTO> pileDtoList = new ArrayList<>();
            List<OpLocationEvseElasticDTO> evseList=new ArrayList<>();
            if (StringUtils.isNotEmpty(keyword)) {
                for (Terms.Bucket locationIdBucket : locationBuckets) {
                    ParsedTopHits topHits = (ParsedTopHits) locationIdBucket.getAggregations().asMap().get("top_docs");
                    org.elasticsearch.search.SearchHit[] resultHits =topHits.getHits().getHits();
                    for (org.elasticsearch.search.SearchHit hit : resultHits) {
                        String sourceAsString = hit.getSourceAsString();
                        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = ObjectMapperWrapper.readValue(sourceAsString, OpLocationPileEvseElasticDTO.class);
                        pileDtoList.add(opLocationPileEvseElasticDTO);
                    }
                }
                Set<String> pileSnList= pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toSet());
                evseList = opLocationEvseElastic.findAllByPileSnIn(pileSnList);
            }


            //根据场站ID按照额定功率分组统计
            TermsAggregationBuilder powerQueryBuilder = AggregationBuilders.terms("count_by_locationId").field("locationId")
            .subAggregation(AggregationBuilders.terms("count_by_type").field("gunType").size(100))
            .subAggregation(AggregationBuilders.terms("count_by_power").field("power").size(100))
            .subAggregation(AggregationBuilders.terms("count_by_state").field("state").size(100))
            .subAggregation(AggregationBuilders.cardinality("count_by_pileSn").field("pileSn"))
                    .size(pageSize);
            BoolQueryBuilder powerBuilder = QueryBuilders.boolQuery();
            powerBuilder.must(QueryBuilders.termsQuery("locationId", locationIdSet));
            NativeSearchQuery powerSearchQuery = new NativeSearchQueryBuilder()
                    .withQuery(powerBuilder)
                    .withAggregations(powerQueryBuilder).build();
            ElasticsearchAggregations powerAggregations = (ElasticsearchAggregations) elasticsearchRestTemplate
                    .search(powerSearchQuery, OpLocationEvseElasticDTO.class).getAggregations();
            log.info("searchByPageV2,powerSearchQuery={}",powerSearchQuery.getAggregations());
            Map<String, Aggregation> powerResultAsMap = powerAggregations.aggregations().asMap();
            ParsedTerms powerTerms = (ParsedTerms) powerResultAsMap.get("count_by_locationId");
            List<? extends Terms.Bucket> powerBuckets = powerTerms.getBuckets();
            Map<String,List<PowerGroupVO> > powerMap = new HashMap<>();
            Map<String,List<GunTypeGroupVO> > typeMap = new HashMap<>();
            Map<String,List<GunStatusGroupVO> > stateMap = new HashMap<>();
            Map<String,Long> countMap = new HashMap<>();
            for (Terms.Bucket locationIdBucket : powerBuckets) {
                List<PowerGroupVO> powerGroupVOS = Lists.newArrayList();
                ParsedTerms powerTermsChildren = (ParsedTerms) locationIdBucket.getAggregations().asMap().get("count_by_power");
                List<? extends Terms.Bucket> powerBucketsChildren = powerTermsChildren.getBuckets();
                for (Terms.Bucket powerBucket : powerBucketsChildren) {
                    Long powerCount = powerBucket.getDocCount();
                    PowerGroupVO powerGroupVO = new PowerGroupVO();
                    powerGroupVO.setPower(ObjectUtils.isEmpty(powerBucket.getKeyAsNumber())?0.0:powerBucket.getKeyAsNumber().doubleValue());
                    powerGroupVO.setTotalPileNum(ObjectUtils.isEmpty(powerCount)?0:Math.toIntExact(powerCount));
                    powerGroupVOS.add(powerGroupVO);
                }
                powerMap.put(locationIdBucket.getKeyAsString(), powerGroupVOS);

                List<GunTypeGroupVO> gunTypeGroupVOs = Lists.newArrayList();
                ParsedTerms typeTermsChildren = (ParsedTerms) locationIdBucket.getAggregations().asMap().get("count_by_type");
                List<? extends Terms.Bucket> typeBucketsChildren = typeTermsChildren.getBuckets();
                for (Terms.Bucket typeBucket : typeBucketsChildren) {
                    Long typeCount = typeBucket.getDocCount();
                    GunTypeGroupVO gunTypeGroupVO = new GunTypeGroupVO();
                    gunTypeGroupVO.setGunType(Integer.parseInt(typeBucket.getKeyAsString()) );
                    gunTypeGroupVO.setTotalCount(ObjectUtils.isEmpty(typeCount)?0:typeCount.intValue());
                    gunTypeGroupVOs.add(gunTypeGroupVO);
                }
                typeMap.put(locationIdBucket.getKeyAsString(),gunTypeGroupVOs);


                List<GunStatusGroupVO> gunStateGroupVOs = Lists.newArrayList();
                ParsedTerms stateTermsChildren = (ParsedTerms) locationIdBucket.getAggregations().asMap().get("count_by_state");
                List<? extends Terms.Bucket> gunStateChildren = stateTermsChildren.getBuckets();
                for (Terms.Bucket typeBucket : gunStateChildren) {
                    Long typeCount = typeBucket.getDocCount();
                    GunStatusGroupVO gunStatusGroupVO = new GunStatusGroupVO();
                    LocationEVSEStatusEnum statusEnum = LocationEVSEStatusEnum.realTimeStatus2BusinessStatus(EvseDeviceStatusEnum.getEnumByName(typeBucket.getKeyAsString()).getName());
                    gunStatusGroupVO.setEvseStatusCode(statusEnum.getCode().intValue());
                    gunStatusGroupVO.setTotal(ObjectUtils.isEmpty(typeCount)?0:typeCount.intValue());
                    gunStateGroupVOs.add(gunStatusGroupVO);
                }
                Map<Integer, Integer> mergedStatuses = gunStateGroupVOs.stream()
                        .collect(Collectors.groupingBy(GunStatusGroupVO::getEvseStatusCode, Collectors.summingInt(GunStatusGroupVO::getTotal)));
                gunStateGroupVOs = mergedStatuses.entrySet().stream().map(entry -> {
                    GunStatusGroupVO gunStatusGroupVO = new GunStatusGroupVO();
                    gunStatusGroupVO.setEvseStatusCode(entry.getKey());
                    gunStatusGroupVO.setTotal(entry.getValue());
                    return gunStatusGroupVO;
                }
                ).collect(Collectors.toList());
                stateMap.put(locationIdBucket.getKeyAsString(),gunStateGroupVOs);
                ParsedCardinality pileSnCardinality = (ParsedCardinality) locationIdBucket.getAggregations().asMap().get("count_by_pileSn");
                countMap.put(locationIdBucket.getKeyAsString(),pileSnCardinality.getValue());
            }



            Future<Map<Long, OpEvseBrandEntity>> brandFuture = getPileBrand(pileDtoList);
            List<OpLocationDetailImageVO> imageVOList = opLocationImageMapper.selectImageByLocationIdList(locationIdList);
            Map<Long, List<OpLocationDetailImageVO>> imageVOMap = imageVOList.stream()
                    .collect(Collectors.groupingBy(OpLocationDetailImageVO::getLocationId));

            Map<Long, List<OpLocationEvseElasticDTO>> evseLocationMap = evseList.stream()
                    .collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getLocationId));
            Map<Long, List<OpLocationPileEvseElasticDTO>> pileLocationMap = pileDtoList.stream()
                    .collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));
            //桩集合
            Map<Long, OpEvseBrandEntity> brandEntityMap = getPileBrand(brandFuture);

            List<OpLocationPageVO> result = Lists.newArrayList();
            for (OpLocationPageVO opLocationPageVO : opLocationPageVOList) {

                String locationId =ObjectUtils.isEmpty(opLocationPageVO.getId())?"":opLocationPageVO.getId().toString();
                // 补充统计数据
                List<OpLocationEvseElasticDTO> evseLocationList = Optional
                        .ofNullable(evseLocationMap.get(opLocationPageVO.getId())).orElse(Collections.emptyList());
                List<OpLocationPileEvseElasticDTO> pileLocationList = Optional
                        .ofNullable(pileLocationMap.get(opLocationPageVO.getId())).orElse(Collections.emptyList());
                Map<Long, OpLocationEvseElasticDTO> evseIdMap = evseLocationList.stream()
                        .collect(Collectors.toMap(OpLocationEvseElasticDTO::getId, Function.identity()));

//            //搜索过滤，支持大小写
//            if (StringUtils.isNotEmpty(keyword)) {
//                String finalKeyword = keyword.toLowerCase();
//                pileLocationList = pileLocationList.stream().filter(p -> p.getPileSn() != null && p.getPileSn().toLowerCase().contains(finalKeyword) || p.getName() != null && p.getName().toLowerCase().contains(finalKeyword) || opLocationPageVO.getName() != null && opLocationPageVO.getName().toLowerCase().contains(finalKeyword)).collect(Collectors.toList());
//            }
                List<PilePageVO> pilePageVOList = buildPilePageVOList(pileLocationList, evseIdMap, brandEntityMap);

                // 补图片
                opLocationPageVO.setImageList(imageVOMap.get(opLocationPageVO.getId()));

                // 是否运营
                Long operationDate = opLocationPageVO.getOperationDate();
                if (operationDate != null) {
                    opLocationPageVO.setOperate(new Date().getTime() >= operationDate);
                }

                opLocationPageVO.setEvseNum(evseLocationList.size());

                opLocationPageVO.setPileNum(ObjectUtils.isEmpty(countMap.get(locationId))?0:countMap.get(locationId).intValue());

                //不同功率枪数统计
                opLocationPageVO.setPowerGroupVOS(powerMap.get(locationId));
                //不同类型枪数统计
                opLocationPageVO.setGunTypeGroupVOS(typeMap.get(locationId));
                //不同枪状态统计
                opLocationPageVO.setGunStatusGroupVOS(stateMap.get(locationId));

                opLocationPageVO.setPileList(pilePageVOList);
                result.add(opLocationPageVO);
            }

            resultPage.setRecords(result);
            resultPage.setTotal(totalHits);
            return resultPage;
        } catch (Exception e) {
            log.error("searchByPageV2 error", e);
            return new Page<>(dto.getPage(), dto.getPageSize(), 0);
        }
    }


    @Override
    public List<AppDetailPileListVO> appDetailPileList(List<OpLocationEvseElasticDTO> esEVSEList) {
        return buildAppDetailPileList(esEVSEList);
    }

    public Map<Long, OpEvseBrandEntity> getPileBrand(Future<Map<Long, OpEvseBrandEntity>> future) {
        Map<Long, OpEvseBrandEntity> brandEntityMap = new HashMap<>();
        try {
            brandEntityMap = future.get();
        } catch (Exception e) {
            Thread.currentThread().interrupt();
            log.error("OpLocationRepositoryImpl.getEvseBrand and exception = ", e);
        } finally {
            future.cancel(true);
        }
        return brandEntityMap;
    }

    public Future<Map<Long, OpEvseBrandEntity>> getPileBrand(List<OpLocationPileEvseElasticDTO> pileList) {
        Map<Long, OpEvseBrandEntity> brandEntityMap = new HashMap<>();
        return executorService.submit(() -> {
            if (CollectionUtils.isEmpty(pileList)) {
                return;
            }
            Set<Long> brandIdSet = pileList.stream().map(OpLocationPileEvseElasticDTO::getBrandId).collect(Collectors.toSet());
            List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectBatchIds(brandIdSet);
            if (CollectionUtils.isNotEmpty(opEvseBrandEntities)) {
                opEvseBrandEntities.forEach(brandEntity -> brandEntityMap.put(brandEntity.getId(), brandEntity));
            }
        }, brandEntityMap);
    }

    public Map<Long, MonthReportVO> getMonthReport(Future<Map<Long, MonthReportVO>> future) {
        Map<Long, MonthReportVO> statisticsMap = new HashMap<>();
        try {
            statisticsMap = future.get();
        } catch (Exception e) {
            Thread.currentThread().interrupt();
            log.error("OpLocationRepositoryImpl.getMonthReport and exception = ", e);
        } finally {
            future.cancel(true);
        }
        return statisticsMap;
    }

    public Future<Map<Long, MonthReportVO>> getMonthReport(List<Long> locationIdList) {
        Map<Long, MonthReportVO> reportMap = new HashMap<>();
        return executorService1.submit(() -> {
            try {
                Result<List<MonthReportVO>> statisticsResult = pileBillStationInterfaceFeign.monthReport(locationIdList);
                log.info("OpLocationRepositoryImpl.getMonthReport and statisticsResult = " + JSON.toJSONString(statisticsResult));
                if (statisticsResult.getCode() != HttpStatus.OK.value()) {
                    return;
                }
                List<MonthReportVO> statisticsList = statisticsResult.getData();
                if (CollectionUtils.isEmpty(statisticsList)) {
                    return;
                }
                for (MonthReportVO monthReportVO : statisticsList) {
                    reportMap.put(monthReportVO.getLocationId(), monthReportVO);
                }
            } catch (Exception e) {
                log.error("OpLocationRepositoryImpl.getMonthReport and Exception = ", e);
            }
        }, reportMap);
    }

    /**
     * 按照组织树排序
     *
     * @param opLocationPageVOList
     */
    public void sortLocation(List<OpLocationPageVO> opLocationPageVOList) {
        try {
            //排序
            Result<List<UserOrgMenuVO>> userOrgTreeListResult = pileUserFeign.menuList();
            log.info("OpLocationRepositoryImpl.sortLocation and userOrgTreeListResult = "
                    + JSON.toJSONString(userOrgTreeListResult));
            if (userOrgTreeListResult != null && CollectionUtils.isNotEmpty(userOrgTreeListResult.getData())) {
                Map<Long, Integer> orgIdNOMap = buildOrgIdNOMap(userOrgTreeListResult.getData());
                log.info("OpLocationRepositoryImpl.sortLocation and orgIdNOMap = " + JSON.toJSONString(orgIdNOMap));
                opLocationPageVOList.forEach(opLocationPageVO -> opLocationPageVO.setOrgNo(orgIdNOMap.get(opLocationPageVO.getGroupId())));
                Collections.sort(opLocationPageVOList);
            }
        } catch (Exception e) {
            log.error("OpLocationRepositoryImpl.sortLocation and exception = ", e);
        }
    }

    /**
     * 添加平台字段到场站和枪ES中
     *
     * @return
     */
    @Override
    public Boolean syncOpLocationESPlatform() {
        //更新场站ES(pile_base_op_location_index)
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
        queryBuilder.mustNot(QueryBuilders.termQuery("platform", 1));
        Iterable<OpLocationElasticDTO> opLocationElasticDTOS =
//                opLocationElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        opLocationElasticDTOS.forEach(opLocationElasticDTO -> opLocationElasticDTO.setPlatform(1));
        opLocationElastic.saveAll(opLocationElasticDTOS);

        //更新新枪ES状态ES(pile_base_op_location_evse_expand_index)
        Iterable<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOS =
//                opLocationEvseExpandElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseExpandElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        opLocationEvseExpandElasticDTOS.forEach(opLocationEvseExpandElasticDTO -> opLocationEvseExpandElasticDTO.setPlatform(1));
        opLocationEvseExpandElastic.saveAll(opLocationEvseExpandElasticDTOS);

        return Boolean.TRUE;
    }

    @Override
    public Boolean updateOpLocationESPlatform() {
        //更新场站ES(pile_base_op_location_index)
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("platform", 1));
        Iterable<OpLocationElasticDTO> opLocationElasticDTOS =
//                opLocationElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        opLocationElasticDTOS.forEach(opLocationElasticDTO -> {
            if (!ObjectUtils.isEmpty(opLocationElasticDTO.getId())) {
                if (opLocationElasticDTO.getId() > 99999L) {
                    opLocationElasticDTO.setPlatform(2);
                }
            }

        });
        opLocationElastic.saveAll(opLocationElasticDTOS);

        //更新新枪ES状态ES(pile_base_op_location_evse_expand_index)
        Iterable<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOS =
//                opLocationEvseExpandElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseExpandElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        opLocationEvseExpandElasticDTOS.forEach(opLocationEvseExpandElasticDTO -> {
            if (!ObjectUtils.isEmpty(opLocationEvseExpandElasticDTO.getLocationId()) && opLocationEvseExpandElasticDTO.getLocationId() > 99999L) {
                opLocationEvseExpandElasticDTO.setPlatform(2);
            }
        });
        opLocationEvseExpandElastic.saveAll(opLocationEvseExpandElasticDTOS);

        return true;
    }

    @Override
    public Result<Boolean> savePileToEs(OpLocationSavePileEsDTO opLocationSavePileEsDTO) {
        List<OpLocationElasticDTO> opLocationElasticDTOs = opLocationSavePileEsDTO.getOpLocationElasticDTO();
        List<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTO = opLocationSavePileEsDTO.getOpLocationEvseExpandElasticDTO();
        if (CollectionUtils.isNotEmpty(opLocationElasticDTOs)) {
            opLocationElasticDTOs = opLocationElasticDTOs.stream()
                    .map(oplocationDto -> {
                        Long id = oplocationDto.getId();
                        Long size = redisTemplate.opsForSet().size(RedisKeyConstant.getSupportEroamingLocationSetKey(id));
                        oplocationDto.setERoamingNum(size);
                        return oplocationDto;
                    })
                    .collect(Collectors.toList());
            ArrayList<OpLocationElasticDTO> saveEvseLocations = Lists.newArrayList(opLocationElastic.saveAll(opLocationElasticDTOs));
            log.info("opLocationElasticDTOs size: {}, and saveEvseLocations size: {}", opLocationElasticDTOs.size(), saveEvseLocations.size());
        }
        if (CollectionUtils.isNotEmpty(opLocationEvseExpandElasticDTO)) {
            ArrayList<OpLocationEvseExpandElasticDTO> saveEvseExpands = Lists.newArrayList(opLocationEvseExpandElastic.saveAll(opLocationEvseExpandElasticDTO));
            log.info("opLocationEvseExpandElasticDTO size: {}, and saveEvseExpands size: {}", opLocationEvseExpandElasticDTO.size(), saveEvseExpands.size());
        }
        return Result.ofSucceed();
    }

    @Override
    public Result<Boolean> deletePileToEs(BaseEsIdsDTO baseEsIdsDTO, boolean isAll) {
        if (CollectionUtils.isNotEmpty(baseEsIdsDTO.getLocationIds())) {
            log.info("需要删除es中的场站ids为:{}", baseEsIdsDTO.getLocationIds());
            opLocationElastic.deleteAllByIdIn(baseEsIdsDTO.getLocationIds());
        }
        if (CollectionUtils.isNotEmpty(baseEsIdsDTO.getOpLocationEvseIds())) {
            log.info("需要删除es中的evse ids为:{}", baseEsIdsDTO.getOpLocationEvseIds());
            opLocationEvseExpandElastic.deleteByIdIn(baseEsIdsDTO.getOpLocationEvseIds());
        }
        if (isAll) {
            opLocationElastic.deleteAllByPlatformIs(2);
            opLocationEvseExpandElastic.deleteAllByPlatformIs(2);
            log.info("删除es中来自hubject的所有桩和场站，platform -> 2");
        }
        return Result.ofSucceed();
    }

    @Override
    public Result<Boolean> savePileStatusToEs(List<PileStatusToEsDTO> pileStatusToEsDTOS) {
        if (CollectionUtils.isNotEmpty(pileStatusToEsDTOS)) {
            List<Long> collect = pileStatusToEsDTOS.stream()
                    .map(PileStatusToEsDTO::getEvseExpandId).collect(Collectors.toList());
            HashMap<Long, String> evseStatusMap = new HashMap<>();
            pileStatusToEsDTOS.forEach(item -> evseStatusMap.put(item.getEvseExpandId(), item.getGunStatus()));
            List<OpLocationEvseExpandElasticDTO> evseExpandList = opLocationEvseExpandElastic.findAllByIdIn(collect);
            evseExpandList.forEach(evseExpand -> {
                String evseStatus = evseStatusMap.getOrDefault(evseExpand.getId(), EvseDeviceStatusEnum.DEFAULT.getName());
                evseExpand.setGunState(evseStatus);
            });
            if (CollectionUtils.isNotEmpty(evseExpandList)) {
                opLocationEvseExpandElastic.saveAll(evseExpandList);
            }
        }
        log.info("更新桩状态: {}", JSON.toJSONString(pileStatusToEsDTOS));
        return Result.ofSucceed();
    }

    @Override
    public String getZoneId(Long locationId) {
        if (locationId != null) {
            List<OpLocationElasticDTO> dtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                    .withQuery(QueryBuilders.termQuery("id", locationId))
                    .withPageable(PageRequest.of(0, 1))
                    .withSourceFilter(new FetchSourceFilter(new String[]{"id", "zoneId"}, null))
                    .build(), OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(dtoList)) {
                OpLocationElasticDTO dto = dtoList.get(0);
                return dto.getZoneId();
            }
        }
        return ZoneId.systemDefault().getId();
    }

    /**
     * 通过场站id查询所属组织id
     *
     * @return
     */
    @Override
    public Long detailById(Long id) {
        //查询场站
        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(id);
        OpLocationElasticDTO opLocationElasticDTO = null;
        if (optionalOpLocationElasticDTO.isPresent()) {
            opLocationElasticDTO = optionalOpLocationElasticDTO.get();
            log.info("opLocationElasticDTO:{}", JSON.toJSONString(opLocationElasticDTO));
            Long orgId = opLocationElasticDTO.getGroupId();
            return orgId;
        }
        return null;
    }

    @Override
    public AggMapVO mapAgg(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        log.info("{}地图聚合请求参数:{}", LoginUserHolder.getLoginUser().getId(), JSONObject.toJSONString(opLocationMapQueryDTO));

        AggMapVO aggMapVO = new AggMapVO();

        String locationField = "location";
        String gunStateField = "gunState";
        String geohashGridAggName = "geohashGridAgg";
        String geoBoundAggName = "geoBoundAgg";
        String gunStateAggName = "gunStateAgg";
        if (opLocationMapQueryDTO.getTopLeft() == null) {
            LocationDTO topLeft = LocationDTO.builder().lat(90D).lon(-180D).build();
            opLocationMapQueryDTO.setTopLeft(topLeft);
        }
        if (opLocationMapQueryDTO.getBottomRight() == null) {
            LocationDTO bottomRight = LocationDTO.builder().lat(-90D).lon(180D).build();
            opLocationMapQueryDTO.setBottomRight(bottomRight);
        }
        Double top = opLocationMapQueryDTO.getTopLeft().getLat();
        Double left = opLocationMapQueryDTO.getTopLeft().getLon();
        Double bottom = opLocationMapQueryDTO.getBottomRight().getLat();
        Double right = opLocationMapQueryDTO.getBottomRight().getLon();
        if (top.equals(bottom) && !top.equals(90D) && !bottom.equals(-90D)) {
            top = top + 0.00001D;
            bottom = bottom - 0.00001D;
        }
        if (left.equals(right) && !left.equals(-180D) && !right.equals(180D)) {
            left = left - 0.00001D;
            right = right + 0.00001D;
        }
        double extentTop = top + (top - bottom) / 2 > 90D ? 90D : top + (top - bottom) / 2;
        double extendLeft = left - (right - left) / 2 < -180D ? -180D : left - (right - left) / 2;
        double extentBottom = bottom - (top - bottom) / 2 < -90D ? -90D : bottom - (top - bottom) / 2;
        double extendRight = right + (right - left) / 2 > 180D ? 180D : right + (right - left) / 2;
        log.info("top:{} left:{} bottom:{} right:{}   extentTop:{} extendLeft:{} extentBottom:{} extendRight:{}", top, left, bottom, right, extentTop, extendLeft, extentBottom, extendRight);

        //原先处理Hubject，整合hubject桩到opLocationEvseExpandElastic了
        List<OpLocationMapQueryVO> hubjectEvseList = new ArrayList<>();
        hubjectMapQuery(opLocationMapQueryDTO, hubjectEvseList);

        //原先条件
        BoolQueryBuilder formerBoolQueryBuilder = buildFormerBoolQueryBuilder(opLocationMapQueryDTO);
        //加上矩形条件
        GeoBoundingBoxQueryBuilder geoBoundingBoxQueryBuilder = QueryBuilders.geoBoundingBoxQuery(locationField);
        geoBoundingBoxQueryBuilder.setCorners(extentTop, extendLeft, extentBottom, extendRight);
        ConstantScoreQueryBuilder constantScoreQueryBuilder = QueryBuilders.constantScoreQuery(geoBoundingBoxQueryBuilder);
        formerBoolQueryBuilder.filter(constantScoreQueryBuilder);

        //加上planetEV-emsp条件
        appAddEMSPCondition(formerBoolQueryBuilder);

        double calculateDistance = GeoDistance.ARC.calculate(top, left, bottom, right, DistanceUnit.KILOMETERS);
        aggMapVO.setDistance(calculateDistance);
        if (calculateDistance < 15 ||
                (opLocationMapQueryDTO.getTopLeft().getLat().equals(opLocationMapQueryDTO.getBottomRight().getLat()) && opLocationMapQueryDTO.getTopLeft().getLon().equals(opLocationMapQueryDTO.getBottomRight().getLon()))) {
            //如果两个点距离小于1km，或者两个经纬度在同一个点，直接找出方格内的数据
            log.info("{}两点距离:{} 不聚合", LoginUserHolder.getLoginUser().getId(), calculateDistance);

            //场站类型
            aggMapVO.setDataType(MapAggDataTypeEnum.LOCATION_DATA.getCode());
            NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                    .withQuery(formerBoolQueryBuilder)
                    .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"}))
                    .withPageable(PageRequest.of(0, 1000))
                    .build();
            Iterator<OpLocationEvseExpandElasticDTO> expandElasticDTOCloseableIterator =
                    elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).iterator();

            ArrayList<OpLocationEvseExpandElasticDTO> evseExpandElasticDTOArrayList = Lists.newArrayList(expandElasticDTOCloseableIterator);
            //多加一个枪数据，用于校验结果
//            List<EvseExpandElasticDTO> evseExpandDTOS = DozerConvert.mapList(evseExpandElasticDTOArrayList, EvseExpandElasticDTO.class);
//            aggMapVO.setEvseList(evseExpandDTOS);
            //场站数据
            List<OpLocationMapQueryVO> opLocationMapQueryVOList = buildFormerMapQueryVOList(opLocationMapQueryDTO, evseExpandElasticDTOArrayList, hubjectEvseList);
            //当前用户所在家桩共享的场站标识为桩主
            markAsHomePile(opLocationMapQueryVOList);
            aggMapVO.setLocationList(DozerConvert.mapList(opLocationMapQueryVOList, MapAggLocationVO.class));
        } else {

            //聚合类型
            aggMapVO.setDataType(MapAggDataTypeEnum.AGG_DATA.getCode());

            List<BucketVO> bucketVOList = Lists.newArrayList();

            //计算聚合精度
            int precision = calPrecision(calculateDistance, opLocationMapQueryDTO.getPrecision());
            aggMapVO.setPrecision(precision);


            //地理边界聚合
            GeoGridAggregationBuilder geoGridAggregationBuilder = AggregationBuilders.geohashGrid(geohashGridAggName).field(locationField).precision(precision);
            //geoBounds子聚合
            GeoBoundsAggregationBuilder geoBoundsAggregationBuilder = AggregationBuilders.geoBounds(geoBoundAggName).field(locationField);
            geoGridAggregationBuilder.subAggregation(geoBoundsAggregationBuilder);
            //状态统计子聚合
            TermsAggregationBuilder gunStateTermsAggregationBuilder = AggregationBuilders.terms(gunStateAggName).field(gunStateField);
            geoGridAggregationBuilder.subAggregation(gunStateTermsAggregationBuilder);

            //查询可用枪数量
//            Map<String, Long> availableGunStateCountMap = getAvailableGunStateCountMap(geoGridAggregationBuilder, formerBoolQueryBuilder);

            //组装语句
            NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
//                    .withFields("id")
                    .withSourceFilter(new FetchSourceFilterBuilder().withIncludes("id").build())
                    .withQuery(formerBoolQueryBuilder)
                    .withPageable(PageRequest.of(0, 10000))
                    .withAggregations(geoGridAggregationBuilder).build();
            log.info("======DSL Query  {}", nativeSearchQuery.getQuery());
            log.info("======DSL Aggregations  {}", nativeSearchQuery.getAggregations());

            //查询
            ElasticsearchAggregations elasticsearchAggregations = (ElasticsearchAggregations) elasticsearchRestTemplate.searchForStream(nativeSearchQuery, OpLocationEvseExpandElasticDTO.class).getAggregations();
            if (elasticsearchAggregations == null) {
                log.error("elasticsearchAggregations is null, query: {}", nativeSearchQuery);
                return aggMapVO;
            }

            //解析数据
            Map<String, Aggregation> queryResultAsMap = elasticsearchAggregations.aggregations().asMap();
            ParsedGeoHashGrid geohashGridAgg = (ParsedGeoHashGrid) queryResultAsMap.get(geohashGridAggName);
            List<? extends GeoGrid.Bucket> geohashGridBuckets = geohashGridAgg.getBuckets();
            if (CollectionUtils.isNotEmpty(geohashGridBuckets)) {
                for (GeoGrid.Bucket geohashGridBucket : geohashGridBuckets) {
                    BucketVO bucketVO = new BucketVO();
                    bucketVO.setGeohashAsString(geohashGridBucket.getKeyAsString());
                    bucketVO.setPrecision(precision);
                    bucketVO.setDocCount(geohashGridBucket.getDocCount());
                    bucketVO.setTotalCount(geohashGridBucket.getDocCount());
                    //geohashGrid聚合下的二级聚合map:有geoBounds子聚合和状态统计子聚合
                    Map<String, Aggregation> geohashGridBucketAggMap = geohashGridBucket.getAggregations().getAsMap();
                    //封装可用数量(状态统计子聚合)
                    Long availableCount = 0L;
                    Long chargingCount =  0L;
                    Aggregation gunstateAggregation = geohashGridBucketAggMap.get(gunStateAggName);
                    List<? extends Terms.Bucket> gunstateAggregationBuckets = ((ParsedStringTerms) gunstateAggregation).getBuckets();
                    if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(gunstateAggregationBuckets)) {
                        for (Terms.Bucket gunstateAggregationBucket : gunstateAggregationBuckets) {
                            Object key = gunstateAggregationBucket.getKey();
                            if (key != null && EvseDeviceStatusEnum.AVAILABLE.getName().equals(key.toString())) {
                                availableCount = gunstateAggregationBucket.getDocCount();
                            }else if(key != null && (EvseDeviceStatusEnum.CHARGING.getName().equals(key.toString())
                                    || EvseDeviceStatusEnum.PREPARING.getName().equals(key.toString())
                                    || EvseDeviceStatusEnum.FINISHING.getName().equals(key.toString())
                                    || EvseDeviceStatusEnum.SUSPENDED_EVSE.getName().equals(key.toString())
                                    || EvseDeviceStatusEnum.SUSPENDED_EV.getName().equals(key.toString()))){
                                chargingCount += gunstateAggregationBucket.getDocCount();
                            }
                        }
                    }
                    bucketVO.setChargingCount(chargingCount);
                    bucketVO.setAvailableCount(availableCount);
                    //封装topLeft和bottomRight和中心点(geoBounds子聚合)
                    ParsedGeoBounds parsedGeoBounds = (ParsedGeoBounds) geohashGridBucketAggMap.get(geoBoundAggName);
                    LocationDTO topLeft = DozerConvert.map(parsedGeoBounds.topLeft(), LocationDTO.class);
                    LocationDTO bottomRight = DozerConvert.map(parsedGeoBounds.bottomRight(), LocationDTO.class);
                    bucketVO.setTopLeft(topLeft);
                    bucketVO.setBottomRight(bottomRight);
                    LocationDTO centerLocationDTO = LocationDTO.builder().lat((topLeft.getLat() + bottomRight.getLat()) / 2).lon((topLeft.getLon() + bottomRight.getLon()) / 2).build();
                    bucketVO.setCenter(centerLocationDTO);
                    bucketVOList.add(bucketVO);
                }
            }
            aggMapVO.setBucketList(bucketVOList);
        }
        return aggMapVO;
    }

    @Override
    public AggMapVO mapAggForOCPI(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        log.info("{}地图聚合请求参数:{}", LoginUserHolder.getLoginUser().getId(), JSONObject.toJSONString(opLocationMapQueryDTO));

        AggMapVO aggMapVO = new AggMapVO();

        String locationField = "location";
        String geohashGridAggName = "geohashGridAgg";
        String geoBoundAggName = "geoBoundAgg";
        Double top = opLocationMapQueryDTO.getTopLeft().getLat();
        Double left = opLocationMapQueryDTO.getTopLeft().getLon();
        Double bottom = opLocationMapQueryDTO.getBottomRight().getLat();
        Double right = opLocationMapQueryDTO.getBottomRight().getLon();
        if (top.equals(bottom) && !top.equals(90D) && !bottom.equals(-90D)) {
            top = top + 0.00001D;
            bottom = bottom - 0.00001D;
        }
        if (left.equals(right) && !left.equals(-180D) && !right.equals(180D)) {
            left = top - 0.00001D;
            right = right + 0.00001D;
        }

        //原先条件
        BoolQueryBuilder formerBoolQueryBuilder = buildFormerBoolQueryBuilderForOCPI(opLocationMapQueryDTO);
        //加上矩形条件
        GeoBoundingBoxQueryBuilder geoBoundingBoxQueryBuilder = QueryBuilders.geoBoundingBoxQuery(locationField);
        geoBoundingBoxQueryBuilder.setCorners(top, left, bottom, right);
        ConstantScoreQueryBuilder constantScoreQueryBuilder = QueryBuilders.constantScoreQuery(geoBoundingBoxQueryBuilder);
        formerBoolQueryBuilder.filter(constantScoreQueryBuilder);

        double calculateDistance = GeoDistance.ARC.calculate(top, left, bottom, right, DistanceUnit.KILOMETERS);
        aggMapVO.setDistance(calculateDistance);
        if (calculateDistance <= 15 ||
                (opLocationMapQueryDTO.getTopLeft().getLat().equals(opLocationMapQueryDTO.getBottomRight().getLat())
                        && opLocationMapQueryDTO.getTopLeft().getLon().equals(opLocationMapQueryDTO.getBottomRight().getLon()))) {
            //如果两个点距离小于1km，或者两个经纬度在同一个点，直接找出方格内的数据
            log.info("{}两点距离:{} 不聚合", LoginUserHolder.getLoginUser().getId(), calculateDistance);

            //场站类型
            aggMapVO.setDataType(MapAggDataTypeEnum.LOCATION_DATA.getCode());
            NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                    .withQuery(formerBoolQueryBuilder)
                    .withSourceFilter(new FetchSourceFilter(null, new String[]{"facility"}))
                    .withPageable(PageRequest.of(0, 1000))
                    .build();
            Iterator<OpLocationEvseExpandElasticDTO> expandElasticDTOCloseableIterator =
                    elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseExpandElasticDTO.class).stream().map(SearchHit::getContent).iterator();

            ArrayList<OpLocationEvseExpandElasticDTO> evseExpandElasticDTOArrayList = Lists.newArrayList(expandElasticDTOCloseableIterator);
            //多加一个枪数据，用于校验结果
            List<EvseExpandElasticDTO> evseExpandDTOS = DozerConvert.mapList(evseExpandElasticDTOArrayList, EvseExpandElasticDTO.class);
            aggMapVO.setEvseList(evseExpandDTOS);
            //场站数据
            List<OpLocationMapQueryVO> opLocationMapQueryVOList = buildFormerMapQueryVOList(opLocationMapQueryDTO, evseExpandElasticDTOArrayList);
            //当前用户所在家桩共享的场站标识为桩主
            markAsHomePile(opLocationMapQueryVOList);
            aggMapVO.setLocationList(DozerConvert.mapList(opLocationMapQueryVOList, MapAggLocationVO.class));
        } else {

            //聚合类型
            aggMapVO.setDataType(MapAggDataTypeEnum.AGG_DATA.getCode());

            List<BucketVO> bucketVOList = Lists.newArrayList();
            //计算聚合精度
            int precision = calPrecision(calculateDistance, opLocationMapQueryDTO.getPrecision());
            aggMapVO.setPrecision(precision);
            //地理边界聚合1
            GeoGridAggregationBuilder geoGridAggregationBuilder = AggregationBuilders.geohashGrid(geohashGridAggName).field(locationField).precision(precision);

            //查询可用枪数量
            Map<String, Long> availableGunStateCountMap = getAvailableGunStateCountMap(geoGridAggregationBuilder, formerBoolQueryBuilder);

            //地理边界聚合2
            GeoBoundsAggregationBuilder geoBoundsAggregationBuilder = AggregationBuilders.geoBounds(geoBoundAggName).field(locationField);
            geoGridAggregationBuilder.subAggregation(geoBoundsAggregationBuilder);
            //组装语句
            NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
//                    .withFields("id")
                    .withSourceFilter(new FetchSourceFilterBuilder().withIncludes("id").build())
                    .withQuery(formerBoolQueryBuilder)
                    .withPageable(PageRequest.of(0, 10000))
                    .withAggregations(geoGridAggregationBuilder).build();
            log.info("======DSL Query  {}", nativeSearchQuery.getQuery());
            log.info("======DSL Aggregations  {}", nativeSearchQuery.getAggregations());

            //查询
            ElasticsearchAggregations elasticsearchAggregations = (ElasticsearchAggregations) elasticsearchRestTemplate.searchForStream(nativeSearchQuery, OpLocationEvseExpandElasticDTO.class).getAggregations();
            if (elasticsearchAggregations == null) {
                log.error("elasticsearchAggregations is null, query: {}", nativeSearchQuery);
                return aggMapVO;
            }

            //解析数据
            Map<String, Aggregation> queryResultAsMap = elasticsearchAggregations.aggregations().asMap();
            ParsedGeoHashGrid parsedGeoHashGrid = (ParsedGeoHashGrid) queryResultAsMap.get(geohashGridAggName);
            List<? extends GeoGrid.Bucket> buckets = parsedGeoHashGrid.getBuckets();
            if (CollectionUtils.isNotEmpty(buckets)) {
                for (GeoGrid.Bucket bucket : buckets) {
                    BucketVO bucketVO = new BucketVO();
                    bucketVO.setGeohashAsString(bucket.getKeyAsString());
                    bucketVO.setPrecision(precision);
                    bucketVO.setDocCount(bucket.getDocCount());
                    bucketVO.setTotalCount(bucket.getDocCount());
                    bucketVO.setAvailableCount(availableGunStateCountMap.get(bucketVO.getGeohashAsString()) == null ? 0L : availableGunStateCountMap.get(bucketVO.getGeohashAsString()));
                    //封装topLeft和bottomRight和中心点
                    Map<String, Aggregation> geoBoundMap = bucket.getAggregations().getAsMap();
                    ParsedGeoBounds parsedGeoBounds = (ParsedGeoBounds) geoBoundMap.get(geoBoundAggName);
                    LocationDTO topLeft = DozerConvert.map(parsedGeoBounds.topLeft(), LocationDTO.class);
                    LocationDTO bottomRight = DozerConvert.map(parsedGeoBounds.bottomRight(), LocationDTO.class);
                    bucketVO.setTopLeft(topLeft);
                    bucketVO.setBottomRight(bottomRight);
                    LocationDTO centerLocationDTO = LocationDTO.builder().lat((topLeft.getLat() + bottomRight.getLat()) / 2).lon((topLeft.getLon() + bottomRight.getLon()) / 2).build();
                    bucketVO.setCenter(centerLocationDTO);
                    bucketVOList.add(bucketVO);
                }
            }
            aggMapVO.setBucketList(bucketVOList);
        }
        return aggMapVO;
    }

    /**
     * @param sellerId 商家id
     * @return 场站信息
     * @function 根据商家id获取场站信息
     */
    @Override
    public List<OpLocationEntity> getLocationInfoBySellerId(Long sellerId) {

        log.info("OpLocationRepositoryImpl getLocationInfoBySellerId  sellerId : {}", JSON.toJSONString(sellerId));

        if (sellerId == null) {
            return null;
        }
        LambdaQueryWrapper<OpLocationEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpLocationEntity::getOperatorId, sellerId)
                .eq(OpLocationEntity::getDeleted, 0)
                .orderByDesc(OpLocationEntity::getCreatedAt, OpLocationEntity::getId);
        return opLocationMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param id 场站id
     * @return 场站信息
     * @function 根据场站主键获取场站信息
     */
    @Override
    public OpLocationEntity getLocationInfoById(Long id) {

        log.info("===>>>OpLocationRepositoryImpl.getLocationInfoById id: {}", JSON.toJSONString(id));

        return opLocationMapper.selectById(id);
    }

    /**
     * @param opLocationEntity 场站实体
     * @return 执行结果
     * @function 修复场站的税率数据
     */
    @Override
    public boolean taxRateMigrate(OpLocationEntity opLocationEntity) {

        log.info("===>>>OpLocationRepositoryImpl.taxRateMigrate opLocationEntity: {}", JSON.toJSONString(opLocationEntity));

        LambdaUpdateWrapper<OpLocationEntity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();
        lambdaUpdateWrapper
                .set(OpLocationEntity::getTaxConfiguration, opLocationEntity.getTaxConfiguration())
                .set(OpLocationEntity::getUpdatedAt, opLocationEntity.getUpdatedAt())
                .eq(OpLocationEntity::getId, opLocationEntity.getId());
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public LocationRoamingVO getLocationRoamingByEvseSn(String evseSn) {
        return opLocationMapper.getLocationRoamingByEvseSn(evseSn);
    }

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的场站数据
     */
    @Override
    public List<OpLocationElasticDTO> getAllLocationInfoBySellerId(Long sellerId) {
        return opLocationElastic.findAllByOperatorId(sellerId);
    }

    /**
     * @param locationIdList
     * @return
     * @function 根据场站id查询场站信息
     */
    @Override
    public List<OpLocationEntity> findLocationInfoInLocationList(List<Long> locationIdList) {

        log.info("===>>>OpLocationRepositoryImpl.findLocationInfoInLocationList locationIdList: {}", JSON.toJSONString(locationIdList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(locationIdList)) {
            LambdaQueryWrapper<OpLocationEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
            lambdaQueryWrapper.in(OpLocationEntity::getId, locationIdList)
                    .eq(OpLocationEntity::getDeleted, 0)
                    .orderByDesc(OpLocationEntity::getUpdatedAt, OpLocationEntity::getId);
            return opLocationMapper.selectList(lambdaQueryWrapper);
        }
        return null;
    }

    @Override
    public OpLocationEntity selectById(Long locationId) {
        return this.getById(locationId);
    }

    @Override
    public List<EroamingPileVO> getEroamingPileListByTariff(Long tariffId) {
        return opLocationMapper.getEroamingPileListByTariff(tariffId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public LocationSimpleInfoQueryVO setLikeRoaming(Long locationId) {
        JwtInfo loginUser = LoginUserHolder.getLoginUser();
        if (loginUser == null || loginUser.getId() == null) {
            throw new MessageCodeException(PileUserEnum.REMOTE_MEMBER_CENTER_USER_NOT_EXIST);
        }

        Optional<OpLocationElasticDTO> byId = opLocationElastic.findById(locationId);
        if (!byId.isPresent()) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        // 查询当前场站存在多少用户设置为希望开通互联互通
        String eroamingLocationSetKey = RedisKeyConstant.getSupportEroamingLocationSetKey(locationId);
        Long userId = loginUser.getId();
        LocationSimpleInfoQueryVO simpleInfoQueryVO = new LocationSimpleInfoQueryVO();
        Boolean member = redisTemplate.opsForSet().isMember(eroamingLocationSetKey, userId);
        if (member == null) {
            member = Boolean.FALSE;
        }
        simpleInfoQueryVO.setIsSupported(!member);
        if (member) {
            redisTemplate.opsForSet().remove(eroamingLocationSetKey, userId);
        } else {
            redisTemplate.opsForSet().add(eroamingLocationSetKey, userId);
        }

        Set<Long> members = redisTemplate.opsForSet().members(eroamingLocationSetKey);
        log.info("current user state: {}, eRoamingLocationSet: {}", !member, JSON.toJSONString(members));
        try {
            LambdaQueryWrapper<OpLocationERoamingSupportEntity> eq = Wrappers.lambdaQuery(OpLocationERoamingSupportEntity.class)
                    .eq(OpLocationERoamingSupportEntity::getLocationId, locationId);

            if (CollectionUtils.isEmpty(members)) {
                oplocationERoamingSupportMapper.delete(eq);
            } else {
                OpLocationERoamingSupportEntity build = OpLocationERoamingSupportEntity.builder()
                        .id(IdWorker.getId())
                        .locationId(locationId)
                        .userIds(JSON.toJSONString(members))
                        .build();
                oplocationERoamingSupportMapper.delete(eq);
                oplocationERoamingSupportMapper.insert(build);
            }
        } catch (Exception e) {
            log.error("数据库更新失败，回退Redis缓存数据", e);
            if (!member) {
                redisTemplate.opsForSet().remove(eroamingLocationSetKey, userId);
            } else {
                redisTemplate.opsForSet().add(eroamingLocationSetKey, userId);
            }
            throw e;
        }

        Long num = members == null ? 0L : members.size();
        OpLocationElasticDTO opLocationElasticDTO = byId.get();
        opLocationElasticDTO.setERoamingNum(num);
        opLocationElastic.save(opLocationElasticDTO);
        simpleInfoQueryVO.setERoamingSupportNum(num);
        simpleInfoQueryVO.setSupportNum(num);
        simpleInfoQueryVO.setId(locationId);
        return simpleInfoQueryVO;
    }

    @Override
    public List<OpLocationElasticDTO> findList(Long sellerId, List<String> pileSnList) {
        if (CollectionUtils.isEmpty(pileSnList)) {
            log.info("findList,pileSnList is empty.");
            return null;
        }
        List<Long> locationIds = this.opLocationPileEvseRepository.findLocationIds(sellerId, pileSnList);
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("findList,locationIds is empty.");
            return null;
        }
        List<OpLocationElasticDTO> locationDtoList = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                        .withPageable(PageRequest.of(0, 1000))
                        .withQuery(QueryBuilders.termsQuery("id", locationIds))
                        .build(), OpLocationElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());
        return locationDtoList;
    }

    /**
     * 查询并构建可用枪数量
     *
     * @param formerBoolQueryBuilder 查询条件
     * @return 可用枪数量map
     */
    private Map<String, Long> getAvailableGunStateCountMap(GeoGridAggregationBuilder geoGridAggregationBuilder, BoolQueryBuilder formerBoolQueryBuilder) {
        String geohashGridAggName = "geohashGridAgg";
        String gunStateTermAggName = "gunStateAgg";
        String gunStateField = "gunState";

        Map<String, Long> availableGunStateCountMap = new HashMap<>();

        //根据枪状态聚合，统计每个状态的枪数
        TermsAggregationBuilder gunStateTermAggregationBuilder = AggregationBuilders.terms(gunStateTermAggName).field(gunStateField);
        geoGridAggregationBuilder.subAggregation(gunStateTermAggregationBuilder);
        //组装语句
        NativeSearchQuery gunStatenSearchQuery = new NativeSearchQueryBuilder()
                .withQuery(formerBoolQueryBuilder)
                .withPageable(PageRequest.of(0, 10000))
                .addAggregation(geoGridAggregationBuilder).withSourceFilter(new FetchSourceFilterBuilder().build())
                .withIndicesBoost(new IndexBoost(OpLocationEvseExpandElasticDTO.class.getAnnotation(Document.class).indexName(), 1.0f)).build();
        log.info("======gunState DSL Query  {}", gunStatenSearchQuery.getQuery().toString());
        log.info("======gunState DSL Aggregations  {}", gunStatenSearchQuery.getAggregations().toString());

        //查询
//        Aggregations aggregations = elasticsearchRestTemplate.query(gunStatenSearchQuery, SearchResponse::getAggregations);
        ElasticsearchAggregations elasticsearchAggregations = (ElasticsearchAggregations) elasticsearchRestTemplate.searchForStream(gunStatenSearchQuery, OpLocationEvseExpandElasticDTO.class).getAggregations();
        if (elasticsearchAggregations == null) {
            log.error("elasticsearchAggregations is null, query: {}", gunStatenSearchQuery);
            return availableGunStateCountMap;
        }

        //解析第一层数据
        Map<String, Aggregation> queryResultAsMap = elasticsearchAggregations.aggregations().asMap();
        ParsedGeoHashGrid geoHashGrid = (ParsedGeoHashGrid) queryResultAsMap.get(geohashGridAggName);
        List<? extends GeoGrid.Bucket> geoHashGridBuckets = geoHashGrid.getBuckets();
        if (CollectionUtils.isNotEmpty(geoHashGridBuckets)) {
            for (GeoGrid.Bucket geoHashGridBucket : geoHashGridBuckets) {
                //这是是聚合精度 geohashAsString
                String keyAsString = geoHashGridBucket.getKeyAsString();
                //解析第二层数据
                Map<String, Aggregation> termsMap = geoHashGridBucket.getAggregations().getAsMap();
                ParsedStringTerms parsedStringTerms = (ParsedStringTerms) termsMap.get(gunStateTermAggName);
                List<? extends Terms.Bucket> termsBuckets = parsedStringTerms.getBuckets();
                if (CollectionUtils.isNotEmpty(termsBuckets)) {
                    for (Terms.Bucket termsBucket : termsBuckets) {
                        String gunState = termsBucket.getKeyAsString();
                        if (gunState.equals(EvseDeviceStatusEnum.AVAILABLE.getName())) {
                            long gunCount = termsBucket.getDocCount();
                            availableGunStateCountMap.put(keyAsString, gunCount);
                        }
                    }
                }
            }
        }
        return availableGunStateCountMap;
    }

    /**
     * 最左上角和最右下角的距离为20015.1143521863km，分成12分，每份1667.92619601553km
     * "topLeft": { "lat": 90, "lon": -180},
     * "bottomRight": { "lat": -90,"lon": 180}
     */
    private int calPrecision(double distance, Integer lastPrecision) {
//        int precision = 1;
//        if (distance < 1) {
//            precision = 0;
//        } else if (distance >= 1 && distance < 6) {
//            precision = 6;
//        } else if (distance >= 6 && distance < 43) {
//            precision = 5;
//        } else if (distance >= 43 && distance < 220) {
//            precision = 4;
//        } else if (distance >= 220 && distance < 1399) {
//            precision = 3;
//        } else if (distance >= 1399 && distance < 7072) {
//            precision = 2;
//        } else if (distance >= 7072) {
//            precision = 1;
//        }
//        log.info("{}两点距离:{} app传的聚合精度:{}  计算的聚合精度:{}  最终取值精度:{}", LoginUserHolder.getLoginUser().getId(), distance, lastPrecision, precision, precision);
//        return precision;
        int precision = 1;
        if (distance < 15) {
            precision = 7;
        } else if (distance >= 15 && distance < 40) {
            precision = 6;
        } else if (distance >= 40 && distance < 200) {
            precision = 5;
        } else if (distance >= 200 && distance < 1200) {
            precision = 4;
        } else if (distance >= 1200 && distance < 7000) {
            precision = 3;
        } else if (distance >= 7000 && distance < 12000) {
            precision = 2;
        } else if (distance >= 12000) {
            precision = 1;
        }
        log.info("{}两点距离:{} app传的聚合精度:{}  计算的聚合精度:{}  最终取值精度:{}", LoginUserHolder.getLoginUser().getId(), distance, lastPrecision, precision, precision);
        return precision;
//        int precision = 1;
//        if (distance < 1) {
//            precision = 0;
//        } else if (distance >= 1 && distance < 2) {
//            precision = 12;
//        } else if (distance >= 2 && distance < 3) {
//            precision = 11;
//        } else if (distance >= 3 && distance < 4) {
//            precision = 10;
//        } else if (distance >= 4 && distance < 6) {
//            precision = 9;
//        } else if (distance >= 6 && distance < 8) {
//            precision = 8;
//        } else if (distance >= 8 && distance < 10) {
//            precision = 7;
//        } else if (distance >= 10 && distance < 20) {
//            precision = 6;
//        } else if (distance >= 20 && distance < 30) {
//            precision = 5;
//        } else if (distance >= 30 && distance < 40) {
//            precision = 4;
//        } else if (distance >= 40 && distance < 50) {
//            precision = 3;
//        } else if (distance >= 50 && distance < 60) {
//            precision = 2;
//        } else if (distance >= 60) {
//            precision = 1;
//        }
//        if (lastPrecision == null) {
//            log.info("{}两点距离:{} app传的聚合精度:{}  计算的聚合精度:{}  最终取值精度:{}", LoginUserHolder.getLoginUser().getId(), distance, lastPrecision, precision, precision);
//            return precision;
//        } else {
//            int newPrecision = lastPrecision + 1 > 12 ? 12 : lastPrecision + 1;
//            if (newPrecision > precision) {
//                log.info("{}两点距离:{} app传的聚合精度:{}  计算的聚合精度:{}  最终取值精度:{}", LoginUserHolder.getLoginUser().getId(), distance, lastPrecision, precision, newPrecision);
//                return newPrecision;
//            } else {
//                log.info("{}两点距离:{} app传的聚合精度:{}  计算的聚合精度:{}  最终取值精度:{}", LoginUserHolder.getLoginUser().getId(), distance, lastPrecision, precision, lastPrecision + 1 > 12 ? 12 : precision);
//                return precision;
//            }
//        }
    }

    //构建原先场站地图查询条件
    private BoolQueryBuilder buildFormerBoolQueryBuilder(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termsQuery("businessType", Arrays.asList(1, 2)));
        //按功率查询
        if (opLocationMapQueryDTO.getHighPower() != null || opLocationMapQueryDTO.getLowPower() != null) {
            RangeQueryBuilder rangeSearch = QueryBuilders.rangeQuery("power");
            if (opLocationMapQueryDTO.getHighPower() != null) {
                rangeSearch.lte(opLocationMapQueryDTO.getHighPower());
            }
            if (opLocationMapQueryDTO.getLowPower() != null) {
                rangeSearch.gte(opLocationMapQueryDTO.getLowPower());
            }
            boolQueryBuilder.must(rangeSearch);
        }
        //按枪类型查询
        if (opLocationMapQueryDTO.getStatus() != null || org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationMapQueryDTO.getGunTypes())) {
            if (opLocationMapQueryDTO.getStatus() != null && opLocationMapQueryDTO.getStatus() == 1) {
                boolQueryBuilder.must(QueryBuilders.termQuery("gunState", EvseDeviceStatusEnum.AVAILABLE.getName()));
                boolQueryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
            }
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationMapQueryDTO.getGunTypes())) {
                boolQueryBuilder.must(QueryBuilders.termsQuery("gunType", opLocationMapQueryDTO.getGunTypes()));
            }
        }
        boolQueryBuilder.must(QueryBuilders.termsQuery("appShow", true));
        if (!oicpFeign.checkUserIsGray()) {
            boolQueryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        //按经纬度查询
//        if (opLocationMapQueryDTO.getLongitude() != null && opLocationMapQueryDTO.getLatitude() != null) {
//            GeoDistanceQueryBuilder geoDistanceQueryBuilder = new GeoDistanceQueryBuilder("location");
//            //中心点的构建
//            geoDistanceQueryBuilder.point(opLocationMapQueryDTO.getLatitude(), opLocationMapQueryDTO.getLongitude());
//            //范围设定
//            geoDistanceQueryBuilder.distance(opLocationMapQueryDTO.getRange(), DistanceUnit.KILOMETERS);
//            boolQueryBuilder.must(geoDistanceQueryBuilder);
//        }
        //计费规则id不为空
        boolQueryBuilder.must(QueryBuilders.existsQuery("tariffId"));
        //需要过滤掉订阅过期了的桩
        if (subscribeEnable) {
            //boolQueryBuilder.must(QueryBuilders.termQuery("subscriptionCheck", true));
            HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
            String appId = request.getHeader("X-AppId");
            // 8 planetev
            if ("8".equalsIgnoreCase(appId)) {
                List<Long> sellerIdList = getSellerIdList("1");
                boolQueryBuilder.must(QueryBuilders.termsQuery("operatorId", sellerIdList));
            } else {
                boolQueryBuilder.must(QueryBuilders.termQuery("subscriptionCheck", true));
            }
        }
        return boolQueryBuilder;
    }

    //构建原先场站地图查询条件
    private BoolQueryBuilder buildFormerBoolQueryBuilderForOCPI(OpLocationMapQueryDTO opLocationMapQueryDTO) {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        //按枪类型查询
        if (opLocationMapQueryDTO.getStatus() != null || org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationMapQueryDTO.getGunTypes())) {
            if (opLocationMapQueryDTO.getStatus() != null && opLocationMapQueryDTO.getStatus() == 1) {
                boolQueryBuilder.must(QueryBuilders.termQuery("gunState", EvseDeviceStatusEnum.AVAILABLE.getName()));
                boolQueryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
            }
            if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationMapQueryDTO.getGunTypes())) {
                boolQueryBuilder.must(QueryBuilders.termsQuery("gunType", opLocationMapQueryDTO.getGunTypes()));
            }
        }
        if (!oicpFeign.checkUserIsGray()) {
            boolQueryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
        }
        //按经纬度查询
//        if (opLocationMapQueryDTO.getLongitude() != null && opLocationMapQueryDTO.getLatitude() != null) {
//            GeoDistanceQueryBuilder geoDistanceQueryBuilder = new GeoDistanceQueryBuilder("location");
//            //中心点的构建
//            geoDistanceQueryBuilder.point(opLocationMapQueryDTO.getLatitude(), opLocationMapQueryDTO.getLongitude());
//            //范围设定
//            geoDistanceQueryBuilder.distance(opLocationMapQueryDTO.getRange(), DistanceUnit.KILOMETERS);
//            boolQueryBuilder.must(geoDistanceQueryBuilder);
//        }
        //计费规则id不为空
        boolQueryBuilder.must(QueryBuilders.existsQuery("tariffId"));
        return boolQueryBuilder;
    }

    //构建原先场站地图结果集
    private List<OpLocationMapQueryVO> buildFormerMapQueryVOList(OpLocationMapQueryDTO dto, List<OpLocationEvseExpandElasticDTO> esEvseExpandList, List<OpLocationMapQueryVO> hubjectEvseList) {
        Map<Long, List<OpLocationEvseExpandElasticDTO>> resultMap = new HashMap<>();
        Set<Long> locationIdSet = new HashSet<>();
        if (CollectionUtils.isNotEmpty(esEvseExpandList)) {
            for (OpLocationEvseExpandElasticDTO esEvseExpand : esEvseExpandList) {
                Long locationId = esEvseExpand.getLocationId();
                locationIdSet.add(locationId);
                List<OpLocationEvseExpandElasticDTO> list = resultMap.get(locationId);
                if (list == null) {
                    list = new ArrayList<>();
                }
                list.add(esEvseExpand);
                resultMap.put(locationId, list);
            }
        }
        log.info("mapQuery2,resultMap size={}", resultMap.size());

        //查询场站信息
        Map<Long, OpLocationElasticDTO> locationMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(locationIdSet)) {
            List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByIdIn(locationIdSet);
            opLocationElasticDTOList.forEach(opLocationElasticDTO -> {
                locationMap.put(opLocationElasticDTO.getId(), opLocationElasticDTO);
            });
        }

        if (!resultMap.isEmpty()) {
            List<OpLocationMapQueryVO> resultList = new ArrayList<>();
            resultMap.forEach((k, v) -> {
                OpLocationMapQueryVO vo = new OpLocationMapQueryVO();
                OpLocationEvseExpandElasticDTO entity = v.get(0);
                BeanUtils.copyProperties(entity, vo);
                double range = dto.getRange() == null ? 10.00 : dto.getRange();
                //设置经纬度，从场站里面拿
                OpLocationElasticDTO opLocationElasticDTO = locationMap.get(k);
                if (opLocationElasticDTO != null && opLocationElasticDTO.getLatitude() != null && opLocationElasticDTO.getLongitude() != null) {
                    vo.setLatitude(opLocationElasticDTO.getLatitude());
                    vo.setLongitude(opLocationElasticDTO.getLongitude());
                }
                //额外设置stationId，给app传参
                vo.setStationId(k);
                vo.setId(k);
                //范围
                vo.setRange(range);
                //距离
                if (dto.getLongitude() != null && dto.getLatitude() != null) {
                    double distance = GeoDistance.ARC.calculate(dto.getLatitude(), dto.getLongitude(),
                            Double.valueOf(vo.getLatitude()), Double.valueOf(vo.getLongitude()),
                            DistanceUnit.KILOMETERS);
                    vo.setDistance(String.format("%.2f", distance));
                }
                //站点设备集合
                List<OpLocationEvseExpandElasticDTO> stationEsEVSEList = v;
                //场站状态
                Integer locationStatusCode = buildLocationStatus2(stationEsEVSEList);
                vo.setLocationStatusCode(locationStatusCode);
                vo.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                //超充、快充、慢充
                Integer veryFastTotal = 0;
                Integer veryFast = 0;
                Integer fastTotal = 0;
                Integer fast = 0;
                Integer slowTotal = 0;
                Integer slow = 0;
                if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                    Double veryFastPower = 240D;
                    Double fastPower = 22D;
                    for (OpLocationEvseExpandElasticDTO esEVSE : stationEsEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower >= veryFastPower) {
                            veryFastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                veryFast++;
                            }
                        } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                            fastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fast++;
                            }
                        } else {
                            slowTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slow++;
                            }
                        }
                    }
                }
                vo.setVeryFastTotal(veryFastTotal);
                vo.setVeryFast(veryFast);
                vo.setFastTotal(fastTotal);
                vo.setFast(fast);
                vo.setSlowTotal(slowTotal);
                vo.setSlow(slow);
                Long userId = null;
                try {
                    userId = LoginUserHolder.getLoginUser().getId();
                } catch (Exception e) {
                    userId = 1L;
                }
                if (userId == (entity.getOperatorId() == null ? 0L : entity.getOperatorId().longValue())) {
                    vo.setStatus(2);
                }
                vo.setSellerId(String.valueOf(entity.getOperatorId()));
                if (vo.getHubjectCheck() != null && vo.getHubjectCheck()) {
                    vo.setThirdPile(1);
                    vo.setAccessibility(conditionsValue2Accessibility(vo.getConditionsValue()));
                }
                resultList.add(vo);
            });
            //排序
            Collections.sort(resultList);
            hubjectEvseList.addAll(resultList);
        }
        return hubjectEvseList;
    }

    //构建原先场站地图结果集
    private List<OpLocationMapQueryVO> buildFormerMapQueryVOList(OpLocationMapQueryDTO dto,
                                                                 List<OpLocationEvseExpandElasticDTO> esEvseExpandList) {
        Map<Long, List<OpLocationEvseExpandElasticDTO>> resultMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(esEvseExpandList)) {
            for (OpLocationEvseExpandElasticDTO esEvseExpand : esEvseExpandList) {
                Long locationId = esEvseExpand.getLocationId();
                List<OpLocationEvseExpandElasticDTO> list = resultMap.get(locationId);
                if (list == null) {
                    list = new ArrayList<>();
                }
                list.add(esEvseExpand);
                resultMap.put(locationId, list);
            }
        }
        log.info("mapQuery2,resultMap size={}", resultMap.size());
        List<OpLocationMapQueryVO> resultList = new ArrayList<>();
        if (!resultMap.isEmpty()) {

            resultMap.forEach((k, v) -> {
                OpLocationMapQueryVO vo = new OpLocationMapQueryVO();
                OpLocationEvseExpandElasticDTO entity = v.get(0);
                BeanUtils.copyProperties(entity, vo);
                double range = dto.getRange() == null ? 10.00 : dto.getRange();
                //额外设置stationId，给app传参
                vo.setStationId(k);
                vo.setId(k);
                //范围
                vo.setRange(range);
                //距离
                if (dto.getLongitude() != null && dto.getLatitude() != null) {
                    double distance = GeoDistance.ARC.calculate(dto.getLatitude(), dto.getLongitude(),
                            Double.valueOf(vo.getLatitude()), Double.valueOf(vo.getLongitude()),
                            DistanceUnit.KILOMETERS);
                    vo.setDistance(String.format("%.2f", distance));
                }
                //站点设备集合
                List<OpLocationEvseExpandElasticDTO> stationEsEVSEList = v;
                //场站状态
                Integer locationStatusCode = buildLocationStatus2(stationEsEVSEList);
                vo.setLocationStatusCode(locationStatusCode);
                vo.setLocationAPPStatusEnum(LocationAPPStatusEnum.getEnumByCode(locationStatusCode));
                //超充、快充、慢充
                Integer veryFastTotal = 0;
                Integer veryFast = 0;
                Integer fastTotal = 0;
                Integer fast = 0;
                Integer slowTotal = 0;
                Integer slow = 0;
                if (CollectionUtils.isNotEmpty(stationEsEVSEList)) {
                    Double veryFastPower = 240D;
                    Double fastPower = 22D;
                    for (OpLocationEvseExpandElasticDTO esEVSE : stationEsEVSEList) {
                        Double evsePower = esEVSE.getPower() == null ? 0D : esEVSE.getPower();
                        if (evsePower >= veryFastPower) {
                            veryFastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                veryFast++;
                            }
                        } else if (evsePower >= fastPower && evsePower < veryFastPower) {
                            fastTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                fast++;
                            }
                        } else {
                            slowTotal++;
                            if (Objects.equals(esEVSE.getGunState(), EvseDeviceStatusEnum.AVAILABLE.getName())) {
                                slow++;
                            }
                        }
                    }
                }
                vo.setVeryFastTotal(veryFastTotal);
                vo.setVeryFast(veryFast);
                vo.setFastTotal(fastTotal);
                vo.setFast(fast);
                vo.setSlowTotal(slowTotal);
                vo.setSlow(slow);
                Long userId = null;
                try {
                    userId = LoginUserHolder.getLoginUser().getId();
                } catch (Exception e) {
                    userId = 1L;
                }
                if (userId == (entity.getOperatorId() == null ? 0L : entity.getOperatorId().longValue())) {
                    vo.setStatus(2);
                }
                vo.setSellerId(String.valueOf(entity.getOperatorId()));
                if (vo.getHubjectCheck() != null && vo.getHubjectCheck()) {
                    vo.setThirdPile(1);
                    vo.setAccessibility(conditionsValue2Accessibility(vo.getConditionsValue()));
                }

                String supportEroamingLocationSetKey = RedisKeyConstant.getSupportEroamingLocationSetKey(k);
                Boolean member = redisTemplate.opsForSet().isMember(supportEroamingLocationSetKey, userId);
                Long size = redisTemplate.opsForSet().size(supportEroamingLocationSetKey);

                vo.setLikeRoaming(member);
                vo.setERoamingNum(size);

                resultList.add(vo);
            });
            //排序
            Collections.sort(resultList);
        }
        return resultList;
    }

    //当前用户所在家桩共享的场站标识为桩主
    private void markAsHomePile(List<OpLocationMapQueryVO> opLocationMapQueryVOList) {
        if (CollectionUtils.isNotEmpty(opLocationMapQueryVOList)) {
            List<Long> locationIds = opLocationMapQueryVOList.stream().filter(vo -> vo.getStatus() != null && vo.getStatus() == 3).map(OpLocationMapQueryVO::getStationId).collect(Collectors.toList());
            Map<Long, List<OpLocationPileEvseVO>> locationMap = null;
            Map<String, Long> joinMap = null;
            if (CollectionUtils.isNotEmpty(locationIds)) {
                List<OpLocationPileEvseVO> dataList = opLocationPileEvseRepository.getListByLocationIds(locationIds);
                if (CollectionUtils.isNotEmpty(dataList)) {
                    locationMap = dataList.stream().collect(Collectors.groupingBy(OpLocationPileEvseVO::getLocationId));
                    List<UserSharePileListVO> shareUserList = homePileFeignClient.listAsFamilyUserId().getData();
                    if (CollectionUtils.isNotEmpty(shareUserList)) {
                        joinMap = shareUserList.stream().collect(Collectors.toMap(UserSharePileListVO::getSn, UserSharePileListVO::getPileOwnerUserId, (f, s) -> f));
                    }
                }
            }
            if (!org.springframework.util.CollectionUtils.isEmpty(locationMap) && !org.springframework.util.CollectionUtils.isEmpty(joinMap)) {
                Map<Long, List<OpLocationPileEvseVO>> finalLocationMap = locationMap;
                Map<String, Long> finalJoinMap = joinMap;
                opLocationMapQueryVOList.stream().forEach(vo -> {
                    List<OpLocationPileEvseVO> voList = finalLocationMap.get(vo.getStationId());
                    if (CollectionUtils.isNotEmpty(voList)) {
                        voList.stream().filter(pileVo -> finalJoinMap.get(pileVo.getPileSn()) != null && StringUtils.equals(String.valueOf(finalJoinMap.get(pileVo.getPileSn())), vo.getSellerId()))
                                .findAny().ifPresent(value -> vo.setStatus(2));
                    }
                });
            }
        }
    }


    @Override
    public String getZonIdByEvseSn(String evseSn) {
        return opLocationMapper.getZonIdByEvseSn(evseSn);
    }

    @Override
    public OpLocationEvseElasticDTO getEvseInfo(String evseSn) {
        return opLocationEvseElastic.findByEvseSn(evseSn);
    }

    @Override
    public GetSellerInfoAndLocationInfoVO getLocationInfo(String pileSn) {
        GetSellerInfoAndLocationInfoVO getSellerInfoAndLocationInfoVO = opLocationMapper.getLocationInfo(pileSn + "_1");
        if (!ObjectUtils.isEmpty(getSellerInfoAndLocationInfoVO) && !ObjectUtils.isEmpty(getSellerInfoAndLocationInfoVO.getOperatorId())) {
            Result<SellerInfoVO> detail = pileUserServiceFeign.detail(getSellerInfoAndLocationInfoVO.getOperatorId());
            if (!ObjectUtils.isEmpty(detail) && !ObjectUtils.isEmpty(detail.getData()) && !ObjectUtils.isEmpty(detail.getData().getStatus())) {
                if (detail.getData().getStatus().equals(1)) {
                    getSellerInfoAndLocationInfoVO.setOperatorId(Long.valueOf(detail.getData().getId()));
                    getSellerInfoAndLocationInfoVO.setOperatorName(detail.getData().getName());
                }
            }
        }
        return getSellerInfoAndLocationInfoVO;
    }

    @Override
    public List<OpLocationElasticDTO> findByIds(List<Long> ids,String keyword) {
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        query.must(QueryBuilders.termsQuery("id", ids));
        if (StringUtils.isNotEmpty(keyword)) {
            keyword = QueryParserBase.escape(keyword);
            BoolQueryBuilder fuzzyQuery = QueryBuilders.boolQuery();
            fuzzyQuery.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
            fuzzyQuery.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyword)));
            query.must(fuzzyQuery);
        }
        List<OpLocationElasticDTO> dtoList = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                .withQuery(query)
                .withPageable(PageRequest.of(0, 100))
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "name"}, null))
                .build(), OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("findByIds,query={}",query.toString());
        return dtoList;
    }

    /**
     * 构建设备是否支持Modbus Map
     *
     * @param deviceChargePileResult device chargePile表桩信息
     * @return 设备是否支持Modbus Map
     */
    private Map<String, Boolean> buildSNEmsEnableMap(Result<List<ChargePileDTO>> deviceChargePileResult) {
        Map<String, Boolean> sNEmsEnableMap = new HashMap<>();
        if (deviceChargePileResult != null && CollectionUtils.isNotEmpty(deviceChargePileResult.getData())) {
            for (ChargePileDTO chargePileDTO : deviceChargePileResult.getData()) {
                Integer internalSerialName = chargePileDTO.getInternalSerialName();
                if (PileInternalSerialNameEnums.RESIDENTIAL.getCode().equals(internalSerialName) ||
                        PileInternalSerialNameEnums.WALLBOX.getCode().equals(internalSerialName) ||
                        PileInternalSerialNameEnums.MAXICHARGER_AC_COMMERCIAL.getCode().equals(internalSerialName)) {
                    sNEmsEnableMap.put(chargePileDTO.getSn(), true);
                } else {
                    sNEmsEnableMap.put(chargePileDTO.getSn(), false);
                }
            }
        }
        return sNEmsEnableMap;
    }

    /**
     * 构建设备modbus配置信息Map
     *
     * @param dataServiceChargePileResult data-service chargePile表桩信息
     * @return 设备modbus配置信息Map
     */
    private Map<String, String> buildSNEmsAddressMap(Result<List<DataServiceChargePile>> dataServiceChargePileResult) {
        Map<String, String> sNEmsEnableMap = new HashMap<>();
        if (dataServiceChargePileResult != null && CollectionUtils.isNotEmpty(dataServiceChargePileResult.getData())) {
            for (DataServiceChargePile dataServiceChargePile : dataServiceChargePileResult.getData()) {
                sNEmsEnableMap.put(dataServiceChargePile.getSn(), dataServiceChargePile.getEmsAddress());
            }
        }
        return sNEmsEnableMap;
    }

    /**
     * @param locationId
     * @param ocpiEnabled
     * @return
     * @function 场站ocpi启用设置
     */
    @Override
    public Boolean updateLocationOcpiEnalbed(Long locationId, Boolean ocpiEnabled) {

        log.info("===>>>OpLocationRepositoryImpl.updateLocationOcpiEnalbed locationId : {} and ocpiEnabled : {}",
                JSON.toJSONString(locationId),
                JSON.toJSONString(ocpiEnabled));

        if (locationId == null || ocpiEnabled == null) {
            return false;
        }

        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if (opLocationEntity == null) {
            return false;
        }
        opLocationEntity.setOcpiEnabled(ocpiEnabled);
        return opLocationMapper.updateById(opLocationEntity) > 0;
    }

    //ocpi推送场站
    @Override
    public Object ocpiPutLocation(Long operatorId, Map<String, Long> tariffIdMap, LocationVO locationVO) {
        Long resourceLocationId = null;
        List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByOperatorIdAndPlatform(operatorId, OpLocationPlatformEnum.OCPI_EMSP.getCode());
        log.info("=====ocpiPutLocation opLocationElasticDTOList:{}", JSONObject.toJSONString(opLocationElasticDTOList));
        if (CollectionUtils.isNotEmpty(opLocationElasticDTOList)) {
            OpLocationElasticDTO opLocationElasticDTO = opLocationElasticDTOList.get(0);
            resourceLocationId = opLocationElasticDTO.getId();

            log.info("====ocpiPutLocation 已有场站:{}", JSONObject.toJSONString(opLocationElasticDTO));
            //场站
            opLocationElastic.deleteById(opLocationElasticDTO.getId());
            //桩
            List<OpLocationPileEvseElasticDTO> pileEvseElasticDTOList = opLocationPileEvseElastic.findAllByLocationId(opLocationElasticDTO.getId());
            if (CollectionUtils.isNotEmpty(pileEvseElasticDTOList)) {
                Set<Long> pileEVSEIdSet = pileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toSet());
                opLocationPileEvseElastic.deleteAllById(pileEVSEIdSet);
            }
            //枪
            List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByLocationId(opLocationElasticDTO.getId());
            if (CollectionUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                Set<Long> evseIdSet = opLocationEvseElasticDTOList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toSet());
                opLocationEvseElastic.deleteAllById(evseIdSet);
            }
            //枪扩展
            List<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOList = opLocationEvseExpandElastic.findAllByLocationId(opLocationElasticDTO.getId());
            if (CollectionUtils.isNotEmpty(opLocationEvseExpandElasticDTOList)) {
                Set<Long> expandIdSet = opLocationEvseExpandElasticDTOList.stream().map(OpLocationEvseExpandElasticDTO::getId).collect(Collectors.toSet());
                opLocationEvseExpandElastic.deleteAllById(expandIdSet);
            }
        }

        //1、opLocationElasticDTO
        Long currentTimeMillis = System.currentTimeMillis();
        OpLocationElasticDTO opLocationElasticDTO = new OpLocationElasticDTO();
        opLocationElasticDTO.setId(resourceLocationId != null ? resourceLocationId : currentTimeMillis);
        opLocationElasticDTO.setOperatorId(operatorId);
        opLocationElasticDTO.setCreatedAt(currentTimeMillis);
        opLocationElasticDTO.setUpdatedAt(currentTimeMillis);
        opLocationElasticDTO.setStatus(1);
        opLocationElasticDTO.setType(locationVO.getType());
        opLocationElasticDTO.setName(locationVO.getName());
        opLocationElasticDTO.setAddress(locationVO.getAddress());
        opLocationElasticDTO.setPriceFlag(1);
        opLocationElasticDTO.setCity(locationVO.getCity());
        opLocationElasticDTO.setPostalCode(locationVO.getPostal_code());
        opLocationElasticDTO.setCountry(locationVO.getCountry());
        //经纬度
        GeoLocationVO geoLocationVO = locationVO.getCoordinates();
        if (geoLocationVO != null) {
            Map<String, String> latitudeAndLongitudeMap = locationCommon.setTheLatitudeAndLongitudeOfTheStation(geoLocationVO.getLatitude(), geoLocationVO.getLongitude(), null);
            // 重新设置后的纬度和经度
            String latitude = latitudeAndLongitudeMap.get("latitude");
            String longitude = latitudeAndLongitudeMap.get("longitude");
            GeoPoint location = new GeoPoint(Double.parseDouble(latitude), Double.parseDouble(longitude));
            opLocationElasticDTO.setLatitude(latitude);
            opLocationElasticDTO.setLongitude(longitude);
            opLocationElasticDTO.setLocation(location);
        }
        opLocationElasticDTO.setTimeZone("UTC+8");
        opLocationElasticDTO.setZoneId(StringUtils.isNotBlank(locationVO.getTime_zone()) ? locationVO.getTime_zone() : "Europe/London");
        opLocationElasticDTO.setChargingWhenClosed(LocationChargingWhenClosedEnum.OPEN.getCode());
        opLocationElasticDTO.setOpenType(OpenTimeTypeEnum.OPEN.getCode());
        opLocationElasticDTO.setOperationType(locationVO.getType());
        opLocationElasticDTO.setAppShow(false);
        //周边设施
        if (CollectionUtils.isNotEmpty(locationVO.getFacilities())) {
            List<OpLocationFacilityEntity> opLocationFacilityEntityList = new ArrayList<>();
            List<Integer> facilityCodeList = Lists.newArrayList();
            for (String facility : locationVO.getFacilities()) {
                FacilityEnum facilityEnum = FacilityEnum.getEnumByName(facility);
                if (!facilityCodeList.contains(facilityEnum.getCode())) {
                    //用于避免重复
                    facilityCodeList.add(facilityEnum.getCode());
                    OpLocationFacilityEntity opLocationFacilityEntity = new OpLocationFacilityEntity();
                    opLocationFacilityEntity.setDeleted(0);
                    opLocationFacilityEntity.setFacilityCode(facility);
                    opLocationFacilityEntity.setFacilityStatus(0);
                    opLocationFacilityEntity.setFacilityCode(String.valueOf(facilityEnum.getCode()));
                    opLocationFacilityEntity.setCreatedAt(System.currentTimeMillis());
                    opLocationFacilityEntity.setUpdatedAt(System.currentTimeMillis());
                    opLocationFacilityEntityList.add(opLocationFacilityEntity);
                }
            }
            opLocationElasticDTO.setFacility(JSONArray.toJSONString(opLocationFacilityEntityList));
        }
        opLocationElasticDTO.setPlatform(OpLocationPlatformEnum.OCPI_EMSP.getCode());
        //要为false，不然app场站卡片和详情会显示禁止进入accessibility
        opLocationElasticDTO.setHubjectCheck(Boolean.FALSE);
        opLocationElastic.save(opLocationElasticDTO);


        if (CollectionUtils.isNotEmpty(locationVO.getEvses())) {
            for (EVSEVO evsevo : locationVO.getEvses()) {
                //2、oplocationPileEVSE
                Long pileSNId = System.currentTimeMillis();
                OpLocationPileEvseElasticDTO opLocationPileEvseEntity = new OpLocationPileEvseElasticDTO();
                opLocationPileEvseEntity.setId(pileSNId);
                opLocationPileEvseEntity.setCreatedAt(currentTimeMillis);
                opLocationPileEvseEntity.setUpdatedAt(currentTimeMillis);
                opLocationPileEvseEntity.setLocationId(opLocationElasticDTO.getId());
                opLocationPileEvseEntity.setLocationName(opLocationElasticDTO.getName());
                opLocationPileEvseEntity.setPileSn(StringUtils.isBlank(evsevo.getEvse_id()) ? evsevo.getUid() : evsevo.getEvse_id());
                opLocationPileEvseEntity.setName(StringUtils.isBlank(evsevo.getEvse_id()) ? evsevo.getUid() : evsevo.getEvse_id());
                opLocationPileEvseEntity.setStatus(1);
                opLocationPileEvseEntity.setEroamingEnable(1);
                List<Long> evseIdList = Lists.newArrayList();

                //3、oplocationEVSE 和 oplocationEVSEEXPand
                if (CollectionUtils.isNotEmpty(evsevo.getConnectors())) {
                    for (ConnectorVO connectorVO : evsevo.getConnectors()) {
                        Long evseId = System.currentTimeMillis();
                        evseIdList.add(evseId);

                        //oplocationEVSE
                        OpLocationEvseElasticDTO opLocationEvseElasticDTO = new OpLocationEvseElasticDTO();
                        opLocationEvseElasticDTO.setId(evseId);
                        opLocationEvseElasticDTO.setCreatedAt(currentTimeMillis);
                        opLocationEvseElasticDTO.setUpdatedAt(currentTimeMillis);
                        opLocationEvseElasticDTO.setStatus(1);
                        opLocationEvseElasticDTO.setLocationId(opLocationElasticDTO.getId());
                        opLocationEvseElasticDTO.setLocationName(opLocationElasticDTO.getName());
                        EvseDeviceStatusEnum evseDeviceStatusEnum = EVSEStatusEnum.ocpiStatusEnum2EvseDeviceStatusEnum(evsevo.getStatus());
                        opLocationEvseElasticDTO.setState(evseDeviceStatusEnum.getName());
                        opLocationEvseElasticDTO.setLatitude(opLocationElasticDTO.getLatitude());
                        opLocationEvseElasticDTO.setLongitude(opLocationElasticDTO.getLongitude());
                        //计费规则id
                        opLocationEvseElasticDTO.setTariffId(tariffIdMap.get(connectorVO.getTariff_id()));
                        opLocationEvseElasticDTO.setEvseSn(opLocationPileEvseEntity.getPileSn() + "_" + (evsevo.getConnectors().indexOf(connectorVO) + 1));
                        ConnectorGunTypeEnum connectorGunTypeEnum = ConnectorTypeEnum.ocpiConnectorType2AutelConnectorGunTypeEnum(connectorVO.getStandard());
                        opLocationEvseElasticDTO.setGunType(connectorGunTypeEnum.getCode());
                        opLocationEvseElasticDTO.setPowerType(connectorVO.getPower_type());
                        double power = 22D;
                        try {
                            //电压*功率/1000
                            BigDecimal bigDecimal = BigDecimal.valueOf(connectorVO.getAmperage()).multiply(BigDecimal.valueOf(connectorVO.getVoltage())).divide(BigDecimal.valueOf(1000));
                            if (PowerTypeEnum.AC_3_PHASE.name().equalsIgnoreCase(connectorVO.getPower_type())) {
                                bigDecimal = bigDecimal.multiply(BigDecimal.valueOf(3));
                            }
                            power = bigDecimal.doubleValue();
                        } catch (Exception e) {
                            log.info("根据电流和电压计算功率错误:" + e);
                        }
                        opLocationEvseElasticDTO.setPower(power);
                        opLocationEvseElasticDTO.setVoltage(BigDecimal.valueOf(connectorVO.getVoltage()));
                        opLocationEvseElasticDTO.setAmperage(BigDecimal.valueOf(connectorVO.getAmperage()));
                        opLocationEvseElasticDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
                        opLocationEvseElasticDTO.setPileName(opLocationPileEvseEntity.getName());
                        opLocationEvseElasticDTO.setEvseUid(evsevo.getUid());
                        opLocationEvseElastic.save(opLocationEvseElasticDTO);

                        //oplocationEVSEEXPand
                        OpLocationEvseExpandElasticDTO opLocationEvseExpandElasticDTO = DozerConvert.map(opLocationElasticDTO, OpLocationEvseExpandElasticDTO.class);
                        GeoPoint location = new GeoPoint(Double.parseDouble(opLocationElasticDTO.getLatitude()), Double.parseDouble(opLocationElasticDTO.getLongitude()));
                        opLocationEvseExpandElasticDTO.setLocation(location);
                        opLocationEvseExpandElasticDTO.setId(evseId);
                        opLocationEvseExpandElasticDTO.setLocationId(opLocationElasticDTO.getId());
                        opLocationEvseExpandElasticDTO.setOpenType(OpenTimeTypeEnum.OPEN.getCode());
                        opLocationEvseExpandElasticDTO.setTransPower(BigDecimal.valueOf(power));
                        opLocationEvseExpandElasticDTO.setPower(power);
                        //计费规则id
                        opLocationEvseExpandElasticDTO.setTariffId(tariffIdMap.get(connectorVO.getTariff_id()));
                        opLocationEvseExpandElasticDTO.setGunType(connectorGunTypeEnum.getCode());
                        opLocationEvseExpandElasticDTO.setGunState(evseDeviceStatusEnum.getName());
                        opLocationEvseExpandElastic.save(opLocationEvseExpandElasticDTO);
                    }
                }

                if (CollectionUtils.isNotEmpty(evseIdList)) {
                    opLocationPileEvseEntity.setEvseList(JSONArray.toJSONString(evseIdList));
                }
                opLocationPileEvseElastic.save(opLocationPileEvseEntity);
            }
        }

        //5、图片
        if (locationVO.getImages() != null) {
            opLocationImageMapper.deleteByLocationId(opLocationElasticDTO.getId());
            if (CollectionUtils.isNotEmpty(locationVO.getImages())) {
                List<OpImageEntity> opImageEntities = locationVO.getImages().stream().map(e -> {
                    OpImageEntity opImageEntity = DozerConvert.map(e, OpImageEntity.class);
                    opImageEntity.setCreatedAt(currentTimeMillis);
                    opImageEntity.setUpdatedAt(currentTimeMillis);
                    opImageEntity.setDeleted(0);
                    opImageEntity.setStatus(0);
                    return opImageEntity;
                }).collect(Collectors.toList());
                opImageRepository.saveBatch(opImageEntities);

                List<OpLocationImageEntity> opLocationImageEntities = opImageEntities.stream().map(e -> {
                    OpLocationImageEntity opLocationImageEntity = new OpLocationImageEntity();
                    opLocationImageEntity.setImageId(e.getId());
                    opLocationImageEntity.setLocationId(opLocationElasticDTO.getId());
                    opLocationImageEntity.setDeleted(0);
                    return opLocationImageEntity;
                }).collect(Collectors.toList());
                opLocationImageRepository.saveBatch(opLocationImageEntities);
            }
        }

        //6、开放时间
        return true;
    }

    @Override
    public List<LocationListVO> locationListByOperatorId(LocationListDTO locationListDTO) {
        log.info("locationListByOperatorId,locationListDTO={}",JSON.toJSONString(locationListDTO));
        Long operatorId = LoginUserUtil.getSellerId();
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId",operatorId));
        String keyWord = locationListDTO.getKeyWord();
        if (StringUtils.isNotEmpty(keyWord)) {
            keyWord = QueryParserBase.escape(keyWord);
            queryBuilder.must(QueryBuilders.wildcardQuery("name","*" + keyWord + "*"));
        }
        if (!ObjectUtils.isEmpty(locationListDTO.getBusinessType())) {
            queryBuilder.must(QueryBuilders.termQuery("businessType",locationListDTO.getBusinessType()));
        }
        NativeSearchQuery query = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withTrackTotalHits(true)
                .build();
        List<OpLocationElasticDTO> locationList = elasticsearchRestTemplate.search(query, OpLocationElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("locationListByOperatorId,locationList={}",JSON.toJSONString(locationList));
        if (CollectionUtils.isEmpty(locationList)) {
            return new ArrayList<>();
        }
        List<LocationListVO> resultList = new ArrayList<>();
        for (OpLocationElasticDTO opLocationElasticDTO : locationList) {
            LocationListVO locationListVO = new LocationListVO();
            locationListVO.setLocationId(opLocationElasticDTO.getId());
            locationListVO.setLocationName(opLocationElasticDTO.getName());
            locationListVO.setBusinessType(opLocationElasticDTO.getBusinessType());
            resultList.add(locationListVO);
        }
        return resultList;
    }

    @Override
    public Map<String, List<CommonVO>> getLocationIdSellerMap() {
        Map<String, List<CommonVO>> resultMap = new HashMap<>();
        BoolQueryBuilder builder = QueryBuilders.boolQuery();
        LambdaQueryWrapper<OpLocationEntity> dbQuery = Wrappers.lambdaQuery();
        dbQuery.select(OpLocationEntity::getId, OpLocationEntity::getOperatorId);
        dbQuery.eq(OpLocationEntity::getDeleted, 0);
        List<OpLocationEntity> tmpEntityList = this.opLocationMapper.selectList(dbQuery);
        if (CollectionUtils.isEmpty(tmpEntityList)) {
            log.info("getLocationIdSellerMap,tmpEntityList is empty.");
            return resultMap;
        }
        List<Long> sellerIds = tmpEntityList.stream().map(OpLocationEntity::getOperatorId).distinct().collect(Collectors.toList());
        builder.must(QueryBuilders.termsQuery("operatorId", sellerIds));
        String[] includes = {"operatorId", "id", "name"};
        FetchSourceFilter fetchSourceFilter = new FetchSourceFilter(includes, null);
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(builder)
                .withSourceFilter(fetchSourceFilter)
                .withPageable(PageRequest.of(0, 1000))
                .build();
        // 强制刷新索引
        this.elasticsearchRestTemplate.indexOps(OpLocationElasticDTO.class).refresh();
        // 打印查询内容
        log.info("getLocationIdSellerMap,query: {}", searchQuery.getQuery().toString());
        SearchHitsIterator<OpLocationElasticDTO> iterator = this.elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationElasticDTO.class);
        while (iterator.hasNext()) {
            OpLocationElasticDTO dto = iterator.next().getContent();
            String operatorId = dto.getOperatorId().toString();
            CommonVO vo = new CommonVO();
            vo.setCode(dto.getId().toString());
            vo.setName(dto.getName());
            resultMap.compute(operatorId, (key, old) -> {
                if (old == null) {
                    old = new ArrayList<>();
                }
                old.add(vo);
                return old;
            });
        }
        return resultMap;
    }
    @Override
    public List<NodeVO> querySubTree(QuerySubTreeDTO querySubTreeDTO) {
        List<Long> sellerIds = querySubTreeDTO.getSellerIds();
        String level = querySubTreeDTO.getLevel();
        List<OpLocationElasticDTO> locationElasticDTOS = null;
        List<Long> locationIds = new ArrayList<>();
        if (level.equalsIgnoreCase(SubTreeEnum.SELLER.getCode())) {
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
            queryBuilder.must(QueryBuilders.termsQuery("operatorId",sellerIds));
            String[] includes = {"operatorId", "id","name"};
            FetchSourceFilter fetchSourceFilter = new FetchSourceFilter(includes,null);
            NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                    .withQuery(queryBuilder)
                    .withTrackTotalHits(true)
                    .withSourceFilter(fetchSourceFilter)
                    .build();
            locationElasticDTOS = elasticsearchRestTemplate.search(searchQuery, OpLocationElasticDTO.class).stream().map(SearchHit::getContent).distinct().collect(Collectors.toList());
            log.info("getLocationIdSellerMap,locationElasticDTOS={}",JSON.toJSONString(locationElasticDTOS));
            if (com.baomidou.mybatisplus.core.toolkit.CollectionUtils.isEmpty(locationElasticDTOS)) {
                return new ArrayList<>();
            }
            locationIds = locationElasticDTOS.stream().map(OpLocationElasticDTO::getId).collect(Collectors.toList());
        }
        if (CollectionUtils.isEmpty(locationIds) && level.equalsIgnoreCase(SubTreeEnum.PILE.getCode())) {
            locationIds = sellerIds;
        }
        List<OpLocationPileEvseElasticDTO> pileList = opLocationPileEvseRepository.findList(locationIds, "");
        log.info("getLocationIdSellerMap,pileList={}",JSON.toJSONString(pileList));
        if (level.equalsIgnoreCase(SubTreeEnum.STATION.getCode())) {
            return encapsulationLocationAndPile(locationIds,pileList);
        }
        List<NodeVO> result = new ArrayList<>();
        for (Long sellerId : sellerIds) {
            NodeVO seller = new NodeVO();
            seller.setCode(sellerId.toString());
            seller.setLevel(SubTreeEnum.SELLER.getCode());
            List<NodeVO> childrenList = new ArrayList<>();
            for (OpLocationElasticDTO dto : locationElasticDTOS) {
                if (dto.getOperatorId().equals(sellerId)) {
                    NodeVO vo = new NodeVO();
                    vo.setLevel(SubTreeEnum.STATION.getCode());
                    vo.setName(dto.getName());
                    vo.setCode(dto.getId().toString());
                    if (!CollectionUtils.isEmpty(pileList)) {
                        List<NodeVO> childrenListForPile = new ArrayList<>();
                        for (OpLocationPileEvseElasticDTO elasticDTO : pileList) {
                            if (elasticDTO.getLocationId().equals(dto.getId())) {
                                NodeVO pile = new NodeVO();
                                pile.setCode(elasticDTO.getPileSn());
                                pile.setName(elasticDTO.getName());
                                pile.setLevel(SubTreeEnum.PILE.getCode());
                                childrenListForPile.add(pile);
                            }
                        }
                        childrenListForPile = childrenListForPile.stream().filter(ListSortUtil.distinctByKey(NodeVO::getCode)).collect(Collectors.toList());
                        vo.setChildren(childrenListForPile);
                    }
                    childrenList.add(vo);
                    childrenList = childrenList.stream().filter(ListSortUtil.distinctByKey(NodeVO::getCode)).collect(Collectors.toList());
                    seller.setChildren(childrenList);
                }
            }
            result.add(seller);
        }
        return result;
    }

    private List<NodeVO> encapsulationLocationAndPile(List<Long> locationIds, List<OpLocationPileEvseElasticDTO> pileList) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("id",locationIds));
        String[] includes = {"operatorId", "id","name"};
        FetchSourceFilter fetchSourceFilter = new FetchSourceFilter(includes,null);
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withTrackTotalHits(true)
                .withSourceFilter(fetchSourceFilter)
                .build();
        List<OpLocationElasticDTO> locationElasticDTOS = elasticsearchRestTemplate.search(searchQuery, OpLocationElasticDTO.class).stream().map(SearchHit::getContent).filter(ListSortUtil.distinctByKey(OpLocationElasticDTO::getId)).collect(Collectors.toList());
        log.info("encapsulationLocationAndPile,locationElasticDTOS={}",JSON.toJSONString(locationElasticDTOS));
        List<NodeVO> childrenList = new ArrayList<>();
        for (OpLocationElasticDTO dto : locationElasticDTOS) {
                NodeVO vo = new NodeVO();
                vo.setLevel(SubTreeEnum.STATION.getCode());
                vo.setName(dto.getName());
                vo.setCode(dto.getId().toString());
                if (!CollectionUtils.isEmpty(pileList)) {
                    List<NodeVO> childrenListForPile = new ArrayList<>();
                    for (OpLocationPileEvseElasticDTO elasticDTO : pileList) {
                        if (elasticDTO.getLocationId().equals(dto.getId())) {
                            NodeVO pile = new NodeVO();
                            pile.setCode(elasticDTO.getPileSn());
                            pile.setName(elasticDTO.getName());
                            pile.setLevel(SubTreeEnum.PILE.getCode());
                            childrenListForPile.add(pile);
                        }
                    }
                    childrenListForPile = childrenListForPile.stream().filter(ListSortUtil.distinctByKey(NodeVO::getCode)).collect(Collectors.toList());
                    vo.setChildren(childrenListForPile);
                }
                childrenList.add(vo);
        }
        childrenList = childrenList.stream().filter(ListSortUtil.distinctByKey(NodeVO::getCode)).collect(Collectors.toList());
        return childrenList;
    }

    /**
     * 查询白牌或普通商家的id 列表
     * @param whiteBrand
     * @return
     */
    private List<Long> getSellerIdList(String whiteBrand) {
        Result<List<Long>> listResult = pileUserFeign.sellerAndOrWhiteBrand(whiteBrand);
        if (listResult.getData() != null) {
            return listResult.getData();
        } else {
            return Collections.emptyList();
        }
    }

}
