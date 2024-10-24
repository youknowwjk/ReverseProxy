package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.constant.key.ConfigRedisKeyConstant;
import com.autel.cloud.pile.base.domain.common.LocationCommon;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.convert.ChargePointMerchantRelationTypeMapper;
import com.autel.cloud.pile.base.domain.convert.OpLocationConnectorConvert;
import com.autel.cloud.pile.base.domain.convert.OpLocationEvseConvert;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileEvseConvert;
import com.autel.cloud.pile.base.domain.convert.pile.PileInfoConvert;
import com.autel.cloud.pile.base.domain.facade.ChargePointMerchantRelationFacade;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.domain.model.dto.gun.SelectGunInfoForTariffGroupIdDTO;
import com.autel.cloud.pile.base.domain.model.vo.gun.SelectGunInfoForTariffGroupIdVO;
import com.autel.cloud.pile.base.domain.repository.OpLocationConnectorRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.*;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.domain.utils.MessageSourceUtil;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.app.GunListDto;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.dto.fleet.GetDeviceInfoForFleetDTO;
import com.autel.cloud.pile.base.dto.lockOrUnlockGun.LockOrUnlockGunDTO;
import com.autel.cloud.pile.base.dto.oicp.ActionType;
import com.autel.cloud.pile.base.dto.oicp.TariffEvse;
import com.autel.cloud.pile.base.dto.pile.CheckPileNameDTO;
import com.autel.cloud.pile.base.dto.pile.EvscpSettingDTO;
import com.autel.cloud.pile.base.dto.pile.PileSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileDTO;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.dto.subscribe.BenefitFunctionReqDto;
import com.autel.cloud.pile.base.dto.subscribe.PileBenefitFunctionRespDto;
import com.autel.cloud.pile.base.dto.tariff.BindCostModelRuleGroupForGunDTO;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.enums.device.PileTypeEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElasticService;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.*;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileDeviceServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.dto.*;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OicpFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.GunListPageDto;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.autel.cloud.pile.base.vo.evse.EvseDataVO;
import com.autel.cloud.pile.base.vo.evse.EvseInfoVO;
import com.autel.cloud.pile.base.vo.evse.PileEvseInfoVO;
import com.autel.cloud.pile.base.vo.evse.SaveEvseInfoVO;
import com.autel.cloud.pile.base.vo.fleet.GetDeviceInfoForFleetVO;
import com.autel.cloud.pile.base.vo.evse.EvseTariffInfoVO;
import com.autel.cloud.pile.base.vo.location.LocationInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileInfoVO;
import com.autel.cloud.pile.base.vo.pile.SavePileInfoVO;
import com.autel.cloud.pile.bill.dto.fleet.CacheChargeInfoDTO;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.enums.PayTypeEnum;
import com.autel.cloud.pile.user.api.dto.AddDataReqDTO;
import com.autel.cloud.pile.user.api.dto.DeleteDataReqDTO;
import com.autel.cloud.pile.user.api.dto.MarketingRuleSearchDTO;
import com.autel.cloud.pile.user.api.enums.EmspPlatformEnum;
import com.autel.cloud.pile.user.api.enums.SubTreeEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.MarketingRulePileVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.autel.cloud.tariff.dto.business.QueryBusinessCostModelRuleDetailDTO;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.tax.dto.TaxDTO;
import com.autel.cloud.tariff.vo.QueryIssueCostModelRuleForPileLimitVO;
import com.autel.cloud.tariff.vo.SimpleInformationAboutCostModelRuleVO;
import com.autel.cloud.tariff.vo.business.BusinessCostModelRuleVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.query.IndexQuery;
import org.springframework.data.elasticsearch.core.query.IndexQueryBuilder;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StopWatch;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;
import static com.autel.cloud.pile.base.enums.PileBaseEnum.SN_USED_BY_SOME_LOCATION;
import static java.util.stream.Collectors.toMap;

/**
 * @ClassName OpLocationEvseServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/4/15 11:14
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
@RefreshScope
public class OpLocationEvseServiceImpl implements OpLocationEvseService {

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    private MonitorFeign monitorFeign;

    @Autowired
    private OpEvseBrandModelMapper opEvseBrandModelMapper;
    @Autowired
    private DeviceServiceFeign deviceServiceFeign;
    @Resource
    private HomePileFeignClient homePileClient;
    @Autowired
    private DataServiceFeign dataServiceFeign;
    @Autowired
    private SubscribePileRightsService subscribePileRightsService;
    @Autowired
    @Lazy
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private OpLocationService opLocationService;

    private final OpLocationEvseRepository opLocationEvseRepository;

    @Resource
    private OpLocationRepository opLocationRepository;

    @Resource
    private OpsMgmtClient opsMgmtClient;

    @Autowired
    ProtocolFeignClient protocolFeignClient;

    @Resource
    private OicpFeignClientProxy oicpFeignClient;

    @Resource
    private OpLocationOperationMapper opLocationOperationMapper;

    @Autowired
    private MonitorFeignClient monitorFeignClient;

    @Resource
    private TariffAPPFeign tariffAPPFeign;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Autowired
    private OpCostRuleDistributeService opCostRuleDistributeService;
    @Resource
    private OpLocationEvseExpandElasticService opLocationEvseExpandElasticService;
    @Resource
    private OpLocationElastic opLocationElastic;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;
    @Resource
    private OpLocationPileEvseService opLocationPileEvseService;

    @Autowired
    private SellerAccountService sellerAccountService;
    @Resource
    private OpLocationConnectorService opLocationConnectorService;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Autowired
    @Qualifier("energyCloudPileBaseMessageSourceUtil")
    private MessageSourceUtil messageSourceUtil;

    @Autowired
    private TariffFeignClient tariffFeignClient;

    @Autowired
    private LocationCommon locationCommon;
    @Autowired
    private OcpiLocationService ocpiLocationService;
    @Resource
    private OpLocationMapper opLocationMapper;


    @Value("${ocpp.ocpi.enabled:false}")
    private Boolean ocpiEnabled;
    @Resource
    private RuleService ruleService;
    @Lazy
    @Resource
    private PileBaseAsync pileBaseAsync;

    @Autowired
    private RabbitTemplate rabbitTemplate;

    @Resource
    private OpLocationEvseImpl opLocationEvseImpl;

    @Resource
    private ChargePointMerchantRelationMapper chargePointMerchantRelationMapper;

    @Resource
    private ChargePointMerchantRelationFacade chargePointMerchantRelationFacade;

    @Resource
    private OpLocationConnectorRepository opLocationConnectorRepository;

    @Resource
    OpLocationEvseServiceImpl opLocationEvseServiceImpl;

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;
    @Resource
    private PileDeviceServiceAdapter pileDeviceServiceAdapter;
    @Resource
    private OpLocationPileEvseMapper opLocationPileEvseMapper;
    @Resource
    private OpLocationConnectorMapper opLocationConnectorMapper;
    @Resource
    private OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Value("${release-billing-restrictions.seller-id.list:}")
    private List<Long> releaseBillingRestrictionsSellerIdList;

    @Resource
    private TransactionTemplate transactionTemplate;

    @Autowired
    private SaasAccessFeignClient saasAccessFeignClient;

    @Resource
    private PileBaseCommonService pileBaseCommonService;

    public OpLocationEvseServiceImpl(OpLocationEvseRepository opLocationEvseRepository) {
        this.opLocationEvseRepository = opLocationEvseRepository;
    }

    @Override
    public Result<OpEvseInfoDTO> getEvseByEvseSn(String evseSn) {
        OpEvseInfoDTO op = null;
        try {
            op = opLocationEvseRepository.getEvseByEvseSn(evseSn);
            if (op == null) {
                return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("evseSn is not existed"));
            }
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl getByEvseSn exception and exception = ", e);
        }
        return Result.ofSucceed(op);
    }

    @Override
    public Result<OcppLocationEVSEVO> getLocationEvseVOBySnAndGunNo(String evseSn) {
        OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseRepository.getLocationEvseVOBySnAndGunNo(evseSn);
        if (ocppLocationEVSEVO != null) {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(evseSn.split("_")[0]);
            ocppLocationEVSEVO.setFreeVendEnable(opLocationPileEvseElasticDTO.getFreeVendEnable());
        }
        return Result.ofSucceed(ocppLocationEVSEVO);
    }

    @Override
    public Result<OcppLocationEVSEVO> getLocationByPileSn(String pileSn) {
        OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseRepository.getLocationByPileSn(pileSn);
        return Result.ofSucceed(ocppLocationEVSEVO);
    }

    @Override
    public Result<List<OplocationConnectorScanDTO>> getLocationConnectorListByPileSn(String pileSn) {
        List<OplocationConnectorScanDTO> gunList = opLocationEvseRepository.getLocationConnectorListByPileSn(pileSn);
        return Result.ofSucceed(gunList);
    }

    @Override
    public Result<PageDTO<GunListPageDto>> getGunListRulesByStationId(GunListDto gunListDto) {
        return Result.ofSucceed(opLocationEvseRepository.getGunListRulesByStationId(gunListDto));
    }

    @Override
    public Result<OpLocationEvseDTO> details(Long id) {
        OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(id);
        return Result.ofSucceed(opLocationEvseDTO);
    }

    @Override
    public void downLocationEvseXls(HttpServletRequest request, HttpServletResponse response) {

    }

    @Override
    public Result<List<TariffsEvseNumDTO>> getEvseNumByTariffIds(List<Long> tariffGroupIds) {

        log.info("===>>>OpLocationEvseServiceImpl.getEvseNumByTariffIds tariffGroupIds : {}",
                JSON.toJSONString(tariffGroupIds));

        return Result.ofSucceed(opLocationEvseRepository.getEvseNumByTariffIdsV2(tariffGroupIds));
    }

    /**
     * @param opLocationEvseElasticDTOList 充电枪的信息集合
     * @param tariffGroupIdSet             计费规则组的主键id集合
     * @param gunIdList                    枪id集合
     * @param gunIdAndTariffGroupIdMap     充电枪与计费规则组的绑定关系
     * @param result                       若校验失败，需要给出响应的提示信息
     * @param fiveInchesPileSnList         五寸桩集合
     * @return 校验结果
     * @function 根据充电桩型号校验这些充电枪是否能绑定计费规则组
     */
    private boolean isAssoTariffRule(List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList,
                                     Set<Long> tariffGroupIdSet,
                                     List<Long> gunIdList,
                                     Map<Long, Long> gunIdAndTariffGroupIdMap,
                                     Result<Boolean> result,
                                     List<String> fiveInchesPileSnList) {

        log.info("===>>>OpLocationEvseServiceImpl.isAssoTariffRule " +
                        "opLocationEvseElasticDTOList : {} and tariffGroupIdSet : {} and gunIdList : {} and gunIdAndTariffGroupIdMap: {} and result: {} and fiveInchesPileSnList: {}",
                JSON.toJSONString(opLocationEvseElasticDTOList),
                JSON.toJSONString(tariffGroupIdSet),
                JSON.toJSONString(gunIdList),
                JSON.toJSONString(gunIdAndTariffGroupIdMap),
                JSON.toJSONString(result),
                JSON.toJSONString(fiveInchesPileSnList));

        // 桩sn集合
        Set<String> pileSnSet = opLocationEvseElasticDTOList
                .stream()
                .map(OpLocationEvseElasticDTO::getPileSn)
                .collect(Collectors.toSet());

        Result<List<SimpleChargePileVO>> simpleChargePileInfoListResult = deviceServiceFeign.getSimpleChargePileInfoList(new ArrayList<>(pileSnSet));

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.isAssoTariffRule simpleChargePileInfoListResult : {}",
                JSON.toJSONString(simpleChargePileInfoListResult));

        if (simpleChargePileInfoListResult == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(simpleChargePileInfoListResult.getCode())
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(simpleChargePileInfoListResult.getData())) {
            // 设置失败
            throw new MessageCodeException(PileBaseEnum.ADD_MEMBER_CUSTOMER_FAIL);
        }

        // 8寸桩集合
        List<String> eightInchesPileSnList = new ArrayList<>();
        // 固件版本不支持的桩集合
        List<String> notSupportPileSnList = new ArrayList<>();

        for (SimpleChargePileVO simpleChargePileVO : simpleChargePileInfoListResult.getData()) {
            Integer type = simpleChargePileVO.getType();
            String pileSn = simpleChargePileVO.getSn();
            if (PileTypeEnum.FIVE_INCH_PILE.getCode().equals(type)) {
                // 五寸桩
                fiveInchesPileSnList.add(pileSn);
            } else if (PileTypeEnum.EIGHT_INCH_PILE.getCode().equals(type)) {
                // 八寸桩
                eightInchesPileSnList.add(pileSn);
            } else if (PileTypeEnum.FIRMWARE_VERSION_DOES_NOT_SUPPORT.getCode().equals(type)) {
                // 固件版本不支持的桩
                notSupportPileSnList.add(pileSn);
            }
        }

        Result<List<SimpleInformationAboutCostModelRuleVO>> costModelRuleVOResult = tariffFeignClient.querySimpleInformationAboutCostModelRuleVOByIds(new ArrayList<>(tariffGroupIdSet));

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.isAssoTariffRule costModelRuleVOResult : {}",
                JSON.toJSONString(costModelRuleVOResult));

        if (costModelRuleVOResult == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(costModelRuleVOResult.getCode())
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(costModelRuleVOResult.getData())) {
            // 设置失败
            throw new MessageCodeException(PileBaseEnum.ADD_MEMBER_CUSTOMER_FAIL);
        }

        // 构造计费规则id与其信息之间对应关系
        Map<Long, SimpleInformationAboutCostModelRuleVO> tariffGroupIdAndTariffGroupInfoMap = new HashMap<>();
        for (SimpleInformationAboutCostModelRuleVO datum : costModelRuleVOResult.getData()) {
            tariffGroupIdAndTariffGroupInfoMap.put(datum.getId(), datum);
        }

        // 5寸桩校验
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(fiveInchesPileSnList)
                && !this.loosenRestrictionsBySellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId())) {
            Map<String, Boolean> judgeUSCAPileMap = this.judgeUSCAPile(fiveInchesPileSnList);
            for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                if (fiveInchesPileSnList.contains(opLocationEvseElasticDTO.getPileSn()) && judgeUSCAPileMap.get(opLocationEvseElasticDTO.getPileSn())) {
                    Long tariffGroupId = gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId());
                    SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO = tariffGroupIdAndTariffGroupInfoMap.get(tariffGroupId);
                    if (simpleInformationAboutCostModelRuleVO != null
                            && simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule() != null
                            && Integer.valueOf(1).equals(simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule())) {
                        // 在加州的5寸桩不能绑定新版计费规则
                        this.cannotBindCauseType(fiveInchesPileSnList, result, PileBaseEnum.THE_CONFIGURATION_IS_NOT_SUPPORTED_PLEASE_ADJUST.getCode());
                        return false;
                    }
                }
            }
        }

        // 5寸带屏桩校验
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(fiveInchesPileSnList)
                && !this.loosenRestrictionsBySellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId())) {
            boolean reg = false;
            List<TariffRuleOfPileDTO> tariffRuleOfPileDTOList = new ArrayList<>();
            for (String pileSn : fiveInchesPileSnList) {
                for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                    if (org.apache.commons.lang3.StringUtils.isNotBlank(pileSn)
                            && pileSn.equals(opLocationEvseElasticDTO.getPileSn())
                            && tariffGroupIdAndTariffGroupInfoMap.get(gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId())) != null) {
                        Long tariffGroupId = gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId());
                        if (tariffGroupId != null) {
                            TariffRuleOfPileDTO tariffRuleOfPileDTO = new TariffRuleOfPileDTO();
                            tariffRuleOfPileDTO.setPileNo(pileSn);
                            tariffRuleOfPileDTO.setTariffId(tariffGroupId);
                            tariffRuleOfPileDTOList.add(tariffRuleOfPileDTO);
                        }
                        SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO = tariffGroupIdAndTariffGroupInfoMap.get(gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId()));
                        if ((simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule() == null
                                || simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule() == 0)
                                && RuleModelTypeEnum.USER_DEFINED.getCode().equals(simpleInformationAboutCostModelRuleVO.getRuleModelType())) {
                            reg = true;
                        }
                    }
                }
            }

            if (reg && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(tariffRuleOfPileDTOList)) {
                Result<List<QueryIssueCostModelRuleForPileLimitVO>> queryIssueCostModelRuleForPileLimitResult = tariffFeignClient.queryIssueCostModelRuleForPileLimit(tariffRuleOfPileDTOList);

                log.info("======>>>>>>>>> OpLocationEvseServiceImpl.isAssoTariffRule queryIssueCostModelRuleForPileLimitResult : {}", JSON.toJSONString(queryIssueCostModelRuleForPileLimitResult));

                boolean flag = false;
                if (queryIssueCostModelRuleForPileLimitResult != null
                        && Integer.valueOf(HttpStatus.HTTP_OK).equals(queryIssueCostModelRuleForPileLimitResult.getCode())
                        && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(queryIssueCostModelRuleForPileLimitResult.getData())) {
                    List<QueryIssueCostModelRuleForPileLimitVO> queryIssueCostModelRuleForPileLimitVOList = queryIssueCostModelRuleForPileLimitResult.getData();
                    for (QueryIssueCostModelRuleForPileLimitVO queryIssueCostModelRuleForPileLimitVO : queryIssueCostModelRuleForPileLimitVOList) {
                        if (queryIssueCostModelRuleForPileLimitVO.getSimpleInformationAboutCostModelRuleVO() != null
                                && queryIssueCostModelRuleForPileLimitVO.getSimpleInformationAboutCostModelRuleVO().getFiveInchesBindMark() != null
                                && !queryIssueCostModelRuleForPileLimitVO.getSimpleInformationAboutCostModelRuleVO().getFiveInchesBindMark()) {
                            flag = true;
                            break;
                        }

                    }
                    if (flag) {
                        // 桩%s不支持该计费规则配置，请调整
                        this.cannotBindCauseType(fiveInchesPileSnList, result, PileBaseEnum.BILLING_RULES_DO_NOT_APPLY.getCode());
                        return false;
                    }
                }
            }
        }

        // 8寸桩校验
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(eightInchesPileSnList)) {
            for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                if (eightInchesPileSnList.contains(opLocationEvseElasticDTO.getPileSn())) {
                    Long tariffGroupId = gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId());
                    SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO = tariffGroupIdAndTariffGroupInfoMap.get(tariffGroupId);
                    if (simpleInformationAboutCostModelRuleVO != null
                            && simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule() != null
                            && Integer.valueOf(1).equals(simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule())) {
                        // 八寸桩不能绑定新版计费规则
                        this.cannotBindCauseType(eightInchesPileSnList, result, PileBaseEnum.PLEASE_UPGRADE_THE_FIRMWARE_VERSION.getCode());
                        return false;
                    }
                }
            }
        }

        // 固件版本低的充电桩校验
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(notSupportPileSnList)) {
            for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                if (notSupportPileSnList.contains(opLocationEvseElasticDTO.getPileSn())) {
                    Long tariffGroupId = gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId());
                    SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO = tariffGroupIdAndTariffGroupInfoMap.get(tariffGroupId);
                    if (simpleInformationAboutCostModelRuleVO != null
                            && simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule() != null
                            && Integer.valueOf(1).equals(simpleInformationAboutCostModelRuleVO.getNewVersionCostModelRule())) {
                        // 固件版本低的充电桩不能绑定新版计费规则
                        this.cannotBindCauseType(notSupportPileSnList, result, PileBaseEnum.ASSOCIATE_THIS_BILLING_RULE_AGAIN.getCode());
                        return false;
                    }
                }
            }
        }

        // todo FreeVend限制(开启了FreeVend使能开关的充电桩下的充电枪，只能绑定免费模式的计费规则组)
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(new ArrayList<>(pileSnSet));
        List<String> freeVendPileSn = new ArrayList<>();
        List<Long> gunIdListOfFreeVendPileSnList = new ArrayList<>();
        opLocationPileEvseElasticDTOList.forEach(var -> {
            Integer freeVendEnable = var.getFreeVendEnable();
            String evseList = var.getEvseList();
            String pileSn = var.getPileSn();
            if (Integer.valueOf(1).equals(freeVendEnable)
                    && org.apache.commons.lang3.StringUtils.isNotBlank(evseList)) {
                freeVendPileSn.add(pileSn);
                List<Long> evseIdList = JSON.parseArray(evseList, Long.class);
                evseIdList.forEach(item -> {
                    if (gunIdList.contains(item)) {
                        gunIdListOfFreeVendPileSnList.add(item);
                    }
                });
            }
        });

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(gunIdListOfFreeVendPileSnList)) {
            List<Long> tariffGroupIdListOfFreeVendGunIdList = new ArrayList<>();
            gunIdListOfFreeVendPileSnList.forEach(var -> tariffGroupIdListOfFreeVendGunIdList.add(gunIdAndTariffGroupIdMap.get(var)));
            boolean containChargeMark = false;
            for (Long tariffGroupId : tariffGroupIdListOfFreeVendGunIdList) {
                SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO = tariffGroupIdAndTariffGroupInfoMap.get(tariffGroupId);
                if (simpleInformationAboutCostModelRuleVO != null
                        && !RuleModelTypeEnum.FREE.getCode().equals(simpleInformationAboutCostModelRuleVO.getRuleModelType())) {
                    containChargeMark = true;
                    break;
                }
            }
            if (containChargeMark) {
                // 请关闭充电桩%s的FreeVend功能后，再尝试关联计费规则
                this.cannotBindCauseType(freeVendPileSn, result, PileBaseEnum.RE_ASSOCIATE.getCode());
                return false;
            }
        }
        return true;
    }

    /**
     * @param pileSnList
     * @return
     * @function 判断充电桩是否在美国加州
     */
    @Override
    public Map<String, Boolean> judgeUSCAPile(List<String> pileSnList) {

        log.info("===>>>OpLocationEvseServiceImpl.judgeUSCAPile pileSnList : {}", JSON.toJSONString(pileSnList));

        Map<String, Boolean> judgeUSCAPileMap = new HashMap<>();
        List<PileSimpleInfoQueryDTO> pileSimpleInfoQueryDTOList = new ArrayList<>();
        pileSnList.forEach(var -> {
            judgeUSCAPileMap.put(var, false);
            PileSimpleInfoQueryDTO pileSimpleInfoQueryDTO = new PileSimpleInfoQueryDTO();
            pileSimpleInfoQueryDTO.setPileSn(var);
            pileSimpleInfoQueryDTOList.add(pileSimpleInfoQueryDTO);
        });
        List<PileInfoVO> pileInfoVOList = opLocationPileEvseService.queryPileInfo(pileSimpleInfoQueryDTOList);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileInfoVOList)) {
            for (PileInfoVO pileInfoVO : pileInfoVOList) {
                if (pileInfoVO != null && pileInfoVO.getPileLocationInfoVO() != null) {
                    LocationInfoVO pileLocationInfoVO = pileInfoVO.getPileLocationInfoVO();
                    if (("US".equalsIgnoreCase(pileLocationInfoVO.getCountry())
                            || "USA".equalsIgnoreCase(pileLocationInfoVO.getCountry()))
                            && "CA".equalsIgnoreCase(pileLocationInfoVO.getProvince())) {
                        judgeUSCAPileMap.put(pileInfoVO.getPileSn(), true);
                    }
                }
            }
        }
        return judgeUSCAPileMap;
    }

    /**
     * @param pileSnList 不能被绑定计费规则组的充电桩序列号集合
     * @param result     提示结果
     * @param code       错误码
     * @function 依据不能绑定的类型不同，给出相应的提示语
     */
    private void cannotBindCauseType(List<String> pileSnList, Result<Boolean> result, Integer code) {

        log.info("===>>>OpLocationEvseServiceImpl.cannotBindCauseType pileSnList: {} and result: {} and code: {}",
                JSON.toJSONString(pileSnList),
                JSON.toJSONString(result),
                JSON.toJSONString(code));

        // 获取当前登录用户的语言信息
        String language = messageSourceUtil.getLoginUserLanguage();

        log.info("======>>>>> OpLocationEvseServiceImpl.cannotBindCauseType language: {}", JSON.toJSONString(language));

        String message = messageSourceUtil.getMessage(String.valueOf(code), language);
        String pileSnString = "";
        for (int i = 0; i < pileSnList.size(); i++) {
            String pileSn = pileSnList.get(i);
            if (i != (pileSnList.size() - 1)) {
                pileSnString = pileSnString + pileSn + ", ";
            } else {
                pileSnString = pileSnString + pileSn;
            }
        }
        String messageString = String.format(message, pileSnString);
        result.setCode(code);
        result.setMessage(messageString);
        result.setData(Boolean.FALSE);
    }

    private Boolean updateOpCostRuleDistribute(Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap) {
        List<OpCostRuleDistributeDTO> costRuleDistributeDTOList = new ArrayList<>();
        costRuleDistributeMap.forEach((tempKey, tempValue) -> {
            if (CollUtil.isNotEmpty(tempValue)) {
                costRuleDistributeDTOList.addAll(tempValue);
            }
        });
        Result<Boolean> result = opCostRuleDistributeService.addOrUpdateOpCostRuleDistributeByPileSn(costRuleDistributeDTOList);
        return result.getData();
    }

    private void hubjectBindEvsePrice(List<OpLocationEvseEntity> opLocationEvseEntityList) {
        if (CollectionUtils.isEmpty(opLocationEvseEntityList)) {
            return;
        }
        try {
            List<TariffEvse> collect = opLocationEvseEntityList.stream()
                    .map(item -> TariffEvse.builder()
                            .evseSn(item.getEvseSn())
                            .tariffId(item.getTariffId())
                            .build())
                    .collect(Collectors.toList());

            oicpFeignClient.cpoPushPricingProductData(collect, 1);
        } catch (Exception e) {
            log.error("hubject--推送绑定计费规则失败", e);
        }
    }

    @Override
    public Result<List<OpEvseAssociatedRuleDTO>> getEvseInfoForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {
        Long tariffId = opPileAssociatedRuleParamDTO.getTariffId();
        Boolean isExcludedTariff = opPileAssociatedRuleParamDTO.getIsExcludedTariff();
        List<OpEvseAssociatedRuleDTO> ruleDTOList = opLocationEvseRepository.queryEvseByTariffId(tariffId, isExcludedTariff);
        return Result.ofSucceed(ruleDTOList);
    }

    @Override
    public Result<List<UnionPileTreeVO>> getEvseInfoTreeForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {

        log.info("===>>>OpLocationEvseServiceImpl.getEvseInfoTreeForRules opPileAssociatedRuleParamDTO: {}", JSON.toJSONString(opPileAssociatedRuleParamDTO));

        Long tariffId = opPileAssociatedRuleParamDTO.getTariffId();
        Boolean isExcludedTariff = opPileAssociatedRuleParamDTO.getIsExcludedTariff();
        if (tariffId == null) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        Long currentUserId = UserUtil.getSellerId();
        List<OpPileAssociatedRuleDTO> pileList = opLocationEvseRepository
                .queryPileByTariffId(tariffId, currentUserId, isExcludedTariff);
        log.info("查询商户id：{} 计费规则id：{} ，关联的桩列表数据条数：{}",
                currentUserId, tariffId, pileList.size());
        // 根据桩列表封装树形列表
        List<UnionPileTreeVO> pileTree = getPileTree(pileList);
        // 枪号排序
        log.info("===>>>开始进行枪排序");
        this.gunNumberSorting(pileTree);
        return Result.ofSucceed(pileTree);
    }

    private void gunNumberSorting(List<UnionPileTreeVO> pileTree) {
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileTree)) {
            return;
        }
        for (UnionPileTreeVO unionPileTreeVO : pileTree) {
            List<OpPileAssociatedRuleDTO> piles = unionPileTreeVO.getPiles();
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(piles)) {
                for (OpPileAssociatedRuleDTO pile : piles) {
                    List<OpEvseAssociatedRuleDTO> opEvseAssociatedRuleDTOList = pile.getOpEvseAssociatedRuleDTOList();
                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opEvseAssociatedRuleDTOList) && opEvseAssociatedRuleDTOList.size() > 1) {
                        Collections.sort(opEvseAssociatedRuleDTOList);
                    }
                }
            }
        }
    }

    /**
     * 根据桩列表封装树形列表
     *
     * @param pileList
     * @return
     */
    private List<UnionPileTreeVO> getPileTree(List<OpPileAssociatedRuleDTO> pileList) {
        ArrayListMultimap<Long, OpPileAssociatedRuleDTO> evseInfoMultiMap = ArrayListMultimap.create();
        for (OpPileAssociatedRuleDTO datum : pileList) {
            evseInfoMultiMap.put(datum.getLocationId(), datum);
        }
        List<UnionPileTreeVO> pileTree = Lists.newArrayList();
        evseInfoMultiMap.asMap().forEach((locationId, pileColl) -> {
            // 创建桩节点
            UnionPileTreeVO treeNode = new UnionPileTreeVO();
            treeNode.setLocationId(locationId);
            // todo 需求已经改版，该字段已经没有用了，为兼容前端，故将该字段全部置为false
            treeNode.setHubjectCheck(false);
            // 获取场站名字
            String LocationName = getLocationName(pileColl);
            treeNode.setLocationName(LocationName);
            List<OpPileAssociatedRuleDTO> evseList = new ArrayList<>(pileColl);
            log.info("pileColl size:{}", evseList.size());
            treeNode.setPiles(evseList);
            pileTree.add(treeNode);
        });
        return pileTree;
    }

    /**
     * 获取场站名字
     *
     * @param pileColl
     * @return
     */
    private String getLocationName(Collection<OpPileAssociatedRuleDTO> pileColl) {
        String LocationName = "";
        List<OpPileAssociatedRuleDTO> collect = pileColl.stream()
                .filter(e1 -> StringUtils.isNotEmpty(e1.getLocationName()))
                .collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(collect)) {
            LocationName = collect.get(0).getLocationName();
        }
        return LocationName;
    }

    @Override
    public Result<Page<OpLocationsAndEvsesDTO>> searchSitesOrEvses(OpLocationQueryDTO opLocationQueryDTO) {
        Page<OpLocationsAndEvsesDTO> opLocationsAndEvsesDTOS = null;
        try {
            opLocationsAndEvsesDTOS = opLocationEvseRepository.searchSitesOrEvses(opLocationQueryDTO);
        } catch (Exception e) {
            log.error("searchSitesOrEvses Exception:", e);
        }
        return Result.ofSucceed(opLocationsAndEvsesDTOS);
    }

    @Override
    public Result<Boolean> verifyPileSn(String pileSn) {
        try {
            return Result.ofSucceed(opLocationEvseRepository.verifyPileSn(pileSn));
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl verifyPileSn exception and exception = ", e);
            return Result.ofFailed(HttpCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public Result<Boolean> verifyPin(String pileSn, String pin) {
        try {
            VerifyDTO verifyDTO = new VerifyDTO();
            verifyDTO.setPileSn(pileSn);
            verifyDTO.setPin(pin);
            Boolean result = deviceServiceFeign.verifyPile(verifyDTO).getData();
            return Result.ofSucceed(result);
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl verifyPin exception and exception = ", e);
            return Result.ofFailed(HttpCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public Result<Boolean> verifyThirdPileSn(String pileSn, Long brandId) {
        return Result.ofSucceed(opLocationEvseRepository.verifyThirdPileSn(pileSn, brandId));
    }

    @Override
    public Result<OpLocationEvseDTO> getDetailsFromEsById(Long id) {
        try {
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseRepository.getDetailsFromEsById(id);
            return Result.ofSucceed(OpLocationEvseConvert.toOpLocationEvseDTO(opLocationEvseElasticDTO));
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl getDetailsFromEsById exception and exception = ", e);
            return Result.ofFailed(HttpCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    @Transactional
    public Result<List<PileVO>> createEvse(List<OpLocationEvseDTO> opLocationEvseDTOs) {
        log.info("createEvse.opLocationEvseDTOs = {}", opLocationEvseDTOs);
        if (CollectionUtils.isEmpty(opLocationEvseDTOs)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage("param is null"));
        }
        List<String> snList = opLocationEvseDTOs.stream().map(OpLocationEvseDTO::getPileSN).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(snList)) {
            List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                    .in(ChargePointMerchantRelationEntity::getSn, snList).eq(ChargePointMerchantRelationEntity::getMerchantId, LoginUserHolder.getLoginUser().getPayload().getSellerId()));
            if (!CollectionUtils.isEmpty(chargePointEntities)) {
                List<String> pileSnList = chargePointEntities.stream().map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
                opLocationEvseDTOs = opLocationEvseDTOs.stream().filter(m -> pileSnList.contains(m.getPileSN())).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(opLocationEvseDTOs)) {
                    throw new MessageCodeException("PILE_SN_IS_EMPTY");
                }
            } else {
                throw new MessageCodeException("PILE_SN_IS_EMPTY");
            }
        }
        List<PileVO> pileVOS;
        Long userId = null;
        try {
            userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        } catch (Exception e) {
            userId = LoginUserHolder.getLoginUser().getId();
        }
        String deleteKey = RedisKeyConstant.getStringAddPileException(userId);
        try {
            pileVOS = opLocationEvseRepository.createEvse(opLocationEvseDTOs);
        } finally {
            String pileId = stringRedisTemplate.opsForValue().get(deleteKey);
            if (org.springframework.util.StringUtils.hasText(pileId)) {
                log.info("添加桩失败，删除es数据，pileId={}", pileId);
                opLocationPileEvseElastic.deleteById(Long.valueOf(pileId));
                stringRedisTemplate.delete(deleteKey);
            }
        }
        log.info("createEvse.pileEvseVOS = {}", pileVOS);

        try {
            protocolFeignClient.clearOcppConfig(pileVOS.get(0).getPileSN());
        } catch (Exception e) {
            log.error("clearOcppConfig 异常", e);
        }


        opLocationEvseDTOs.forEach(evse -> {
            //添加重构后运维平台桩
            try {
                log.info("新运维平台同步添加桩: {}", evse.getPileSN());
                OpLocationDTO opLocationDTO = opLocationService.details(evse.getLocationId()).getData();
                if (Objects.isNull(opLocationDTO.getOperatorId())) {
                    opLocationDTO.setOperatorId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
                }
                opsMgmtClient.saveOpsPile(formatPileInfoDTO(evse, opLocationDTO));
            } catch (Exception e) {
                log.error("add ops-mgmt pile error: {}", e);
            }
            opLocationService.updateLocationUpdateTime(evse.getLocationId());
        });

        try {
            // 充电枪信息添加成功之后需要发送MQ消息到车队那边
            List<String> pileSnList = opLocationEvseDTOs
                    .stream()
                    .filter(var -> (var != null && org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileSN())))
                    .map(OpLocationEvseDTO::getPileSN)
                    .collect(Collectors.toList());
            // 查询这些充电桩下的充电枪序列号
            List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseRepository.findList(pileSnList);

            log.info("OpLocationEvseServiceImpl.createEvse opLocationEvseElasticDTOList : {}", JSON.toJSONString(opLocationEvseElasticDTOList));

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                    EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                    evseInfoModifyDTO.setEvseSn(opLocationEvseElasticDTO.getEvseSn());
                    evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.ADD.getCode());
                    opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTO);
                }
            }
        } catch (Exception e) {
            log.error("直接添加充电桩后，推送充电枪信息给车队那边出现异常 : {}", e);
        }

        return Result.ofSucceed(pileVOS);
    }

    private PileInfoDTO formatPileInfoDTO(OpLocationEvseDTO evse, OpLocationDTO opLocationDTO) {
        //查询chargePoint表中owerId和maintenanceid
        ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .select(ChargePointMerchantRelationEntity::getId, ChargePointMerchantRelationEntity::getSn
                        , ChargePointMerchantRelationEntity::getMerchantId, ChargePointMerchantRelationEntity::getName)
                .eq(ChargePointMerchantRelationEntity::getSn, evse.getPileSN()).eq(ChargePointMerchantRelationEntity::getMerchantId, LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        log.info("evse:{}", JSON.toJSONString(evse));
        log.info("opLocationDTO:{}", JSON.toJSONString(opLocationDTO));
        PileInfoDTO pileInfoDTO = new PileInfoDTO();
        pileInfoDTO.setSn(evse.getPileSN());
        pileInfoDTO.setPin(evse.getPinCode());
        pileInfoDTO.setAddress(opLocationDTO.getAddress());
        pileInfoDTO.setLongitude(opLocationDTO.getLongitude());
        pileInfoDTO.setLatitude(opLocationDTO.getLatitude());
        pileInfoDTO.setBrand(evse.getBrandName());
        pileInfoDTO.setCategory(2);
        pileInfoDTO.setRatedPower(String.valueOf(evse.getPower()));
        if (StringUtils.isEmpty(evse.getPowerType())) {
            if ("AC_1_PHASE".equals(evse.getPowerType())) {
                pileInfoDTO.setPowerSupplyPhases(1);
            } else if ("AC_3_PHASE".equals(evse.getPowerType())) {
                pileInfoDTO.setPowerSupplyPhases(2);
            } else if ("DC".equals(evse.getPowerType())) {
                pileInfoDTO.setPowerSupplyPhases(3);
            }
        }
        if (!ObjectUtils.isEmpty(chargePointEntity) && !ObjectUtils.isEmpty(chargePointEntity.getName())) {
            pileInfoDTO.setPileName(chargePointEntity.getName());
        } else {
            pileInfoDTO.setPileName(evse.getPileSN());
        }

        pileInfoDTO.setCountry(opLocationDTO.getCountry());
        pileInfoDTO.setPileState(opLocationDTO.getProvince());
        pileInfoDTO.setCity(opLocationDTO.getCity());
        pileInfoDTO.setZipCode(opLocationDTO.getPostalCode());
        pileInfoDTO.setCustomer(evse.getVendor());
        pileInfoDTO.setModel(evse.getProductModel());
        pileInfoDTO.setVendor(evse.getVendor());
        if (!ObjectUtils.isEmpty(chargePointEntity)) {
            if (chargePointEntity.getRelation() == MerchantChargePointRelationEnum.MAINTENANCE.getKey()) {
                pileInfoDTO.setOperationId(chargePointEntity.getMerchantId().toString());
                ChargePointMerchantRelationEntity ownerRelationEntity = chargePointMerchantRelationMapper
                        .selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>().eq(ChargePointMerchantRelationEntity::getSn, chargePointEntity.getSn())
                                .eq(ChargePointMerchantRelationEntity::getRelation, MerchantChargePointRelationEnum.OWNER.getKey()));
                if (Objects.nonNull(ownerRelationEntity)) {
                    pileInfoDTO.setMerchantId(ownerRelationEntity.getMerchantId().toString());
                }
            }
            if (chargePointEntity.getRelation() == MerchantChargePointRelationEnum.OWNER.getKey()) {
                pileInfoDTO.setMerchantId(chargePointEntity.getMerchantId().toString());
                ChargePointMerchantRelationEntity ownerRelationEntity = chargePointMerchantRelationMapper
                        .selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>().eq(ChargePointMerchantRelationEntity::getSn, chargePointEntity.getSn())
                                .eq(ChargePointMerchantRelationEntity::getRelation, MerchantChargePointRelationEnum.MAINTENANCE.getKey()));
                if (Objects.nonNull(ownerRelationEntity)) {
                    pileInfoDTO.setOperationId(ownerRelationEntity.getMerchantId().toString());
                }
            }
        }

//        if (!ObjectUtils.isEmpty(chargePointEntity) &&!ObjectUtils.isEmpty(chargePointEntity.getOwner())) {
//            pileInfoDTO.setMerchantId(chargePointEntity.getOwner().toString());
//        }
//        if (!ObjectUtils.isEmpty(chargePointEntity) &&!ObjectUtils.isEmpty(chargePointEntity.getMaintenance())) {
//            pileInfoDTO.setOperationId(chargePointEntity.getMaintenance().toString());
//        }

        pileInfoDTO.setPileGroupId(String.valueOf(opLocationDTO.getId()));
        pileInfoDTO.setGroupId(String.valueOf(opLocationDTO.getId()));
        //查询组织id
        QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
        opLocationOperationEntityQueryWrapper.eq("location_id", opLocationDTO.getId());
        opLocationOperationEntityQueryWrapper.eq("deleted", 0);
        List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(opLocationOperationEntityQueryWrapper);
        if (CollUtil.isNotEmpty(opLocationOperationEntities) && Objects.nonNull(opLocationOperationEntities.get(0))) {
            pileInfoDTO.setGroupId(String.valueOf(opLocationOperationEntities.get(0).getGroupId()));
        }
        if (CollUtil.isEmpty(evse.getOpLocationConnectorDTOs())) {
            return pileInfoDTO;
        }
        List<Integer> opLocationConnectorDTOs = evse.getOpLocationConnectorDTOs().stream().map(OpLocationConnectorDTO::getGunType).collect(Collectors.toList());
        if (CollUtil.isNotEmpty(opLocationConnectorDTOs)) {
            List<ConnectorDTO> gunTypeList = new ArrayList<>();
            for (int i = 0; i < opLocationConnectorDTOs.size(); ++i) {
                gunTypeList.add(ConnectorDTO.builder().connectorNo(i + 1).connectorType(opLocationConnectorDTOs.get(i)).build());
            }
            pileInfoDTO.setConnectorList(gunTypeList);
        }
        return pileInfoDTO;
    }


    @Override
    public Result<List<PileVO>> updateEvse(List<OpLocationEvseDTO> opLocationEvseDTOs) {

        log.info("updateEvse.opLocationEvseDTOs = {}", opLocationEvseDTOs);
        Long sellerId = LoginUserUtil.getSellerId();

        if (CollectionUtils.isEmpty(opLocationEvseDTOs)) {
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST);
        }

        List<String> pileSnList = opLocationEvseDTOs
                .stream()
                .filter(var -> (var != null && org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileSN())))
                .map(OpLocationEvseDTO::getPileSN)
                .collect(Collectors.toList());
        // 查询这些充电桩下的充电枪序列号（修改之前）
        List<OpLocationEvseElasticDTO> beforeUpdateOpLocationEvseElasticDTOList = opLocationEvseRepository.findList(pileSnList);

        log.info("OpLocationEvseServiceImpl.updateEvse beforeUpdateOpLocationEvseElasticDTOList : {}", JSON.toJSONString(beforeUpdateOpLocationEvseElasticDTOList));

        List<PileVO> pileVOS = opLocationEvseRepository.updateEvse(opLocationEvseDTOs);

        log.info("updateEvse.pileEvseVOS = {}", pileVOS);

        opLocationEvseDTOs.forEach(evse -> {
            //重构后
            try {
                log.info("同步 资产/统一桩管理 更新桩名称: {}", evse.getPileSN());
                UpdatePileNameDTO updatePileNameDTO = new UpdatePileNameDTO();
                updatePileNameDTO.setMerchantId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
                updatePileNameDTO.setPileName(evse.getPileName());
                updatePileNameDTO.setPileSn(evse.getPileSN());
                applicationEventPublisher.publishEvent(updatePileNameDTO);

                log.info("新运维平台同步更新桩: {}", evse.getPileSN());
                UpdatePileV2DTO updatePileV2DTO = new UpdatePileV2DTO();
                updatePileV2DTO.setSn(evse.getPileSN());
                updatePileV2DTO.setName(evse.getPileName());

                List<Integer> opLocationConnectorDTOs = evse.getOpLocationConnectorDTOs().stream().map(OpLocationConnectorDTO::getGunType).collect(Collectors.toList());
                if (CollUtil.isNotEmpty(opLocationConnectorDTOs)) {
                    List<ConnectorDTO> gunTypeList = new ArrayList<>();
                    for (int i = 0; i < opLocationConnectorDTOs.size(); ++i) {
                        gunTypeList.add(ConnectorDTO.builder().connectorNo(i + 1).connectorType(opLocationConnectorDTOs.get(i)).build());
                    }
                    updatePileV2DTO.setConnectorList(gunTypeList);
                }
                opsMgmtClient.editPile(updatePileV2DTO);
            } catch (Exception e) {
                log.error("update ops-mgmt pile error: {}", e);
            }

            opLocationService.updateLocationUpdateTime(evse.getLocationId());
            String pileSn = evse.getPileSN();
            List<String> connectors = evse.getOpLocationConnectorDTOs().stream().map(OpLocationConnectorDTO::getConnectorId).collect(Collectors.toList());
            for (String connector : connectors) {
                rabbitTemplate.convertAndSend(PILE_BASE_PILE_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_PILE_UPDATE_ROUTE, pileSn + "_" + connector);
            }
            updateHubjectEvse(evse);
        });

        try {
            // 充电枪信息修改成功之后需要发送MQ消息到车队那边(这里需要注意的是修改充电桩信息会涉及到充电枪的增加，删除，修改操作)
            // 查询这些充电桩下的充电枪序列号（修改之后）
            List<OpLocationEvseElasticDTO> afterUpdateOpLocationEvseElasticDTOList = opLocationEvseRepository.findList(pileSnList);

            log.info("OpLocationEvseServiceImpl.updateEvse afterUpdateOpLocationEvseElasticDTOList : {}", JSON.toJSONString(afterUpdateOpLocationEvseElasticDTOList));

            // 构建充电枪序列号的Set集合
            Set<String> evseSnSet = new HashSet<>();

            // 构建修改之前充电枪序列号与充电枪信息之间的映射关系
            Map<String, OpLocationEvseElasticDTO> evseSnAndBeforeUpdateOpLocationEvseElasticDTOMap = new HashMap<>();
            beforeUpdateOpLocationEvseElasticDTOList
                    .forEach(var -> {
                        String evseSn = var.getEvseSn();
                        evseSnSet.add(evseSn);
                        evseSnAndBeforeUpdateOpLocationEvseElasticDTOMap.put(evseSn, var);
                    });

            // 构建修改之后充电枪序列号与充电枪信息之间的映射关系
            Map<String, OpLocationEvseElasticDTO> evseSnAndAfterUpdateOpLocationEvseElasticDTOMap = new HashMap<>();
            afterUpdateOpLocationEvseElasticDTOList
                    .forEach(var -> {
                        String evseSn = var.getEvseSn();
                        evseSnSet.add(evseSn);
                        evseSnAndAfterUpdateOpLocationEvseElasticDTOMap.put(evseSn, var);
                    });

            for (String evseSn : evseSnSet) {
                EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                evseInfoModifyDTO.setEvseSn(evseSn);
                OpLocationEvseElasticDTO beforeUpdateOpLocationEvseElasticDTO = evseSnAndBeforeUpdateOpLocationEvseElasticDTOMap.get(evseSn);
                OpLocationEvseElasticDTO afterUpdateOpLocationEvseElasticDTO = evseSnAndAfterUpdateOpLocationEvseElasticDTOMap.get(evseSn);
                if (beforeUpdateOpLocationEvseElasticDTO == null
                        && afterUpdateOpLocationEvseElasticDTO != null) {
                    // 增加操作
                    evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.ADD.getCode());
                } else if (beforeUpdateOpLocationEvseElasticDTO != null
                        && afterUpdateOpLocationEvseElasticDTO == null) {
                    // 删除操作
                    evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.DELETE.getCode());
                    if (beforeUpdateOpLocationEvseElasticDTO.getLocationId() != null) {
                        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(beforeUpdateOpLocationEvseElasticDTO.getLocationId());
                        if (optionalOpLocationElasticDTO.isPresent()) {
                            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();
                            // 场站运营商id
                            Long operatorId = opLocationElasticDTO.getOperatorId();
                            // 场站时区id
                            String zoneId = opLocationElasticDTO.getZoneId();
                            evseInfoModifyDTO.setSellerId(operatorId);
                            evseInfoModifyDTO.setZoneId(zoneId);
                        }
                    }
                } else {
                    // 更新操作
                    evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.UPDATE.getCode());
                }
                opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTO);
            }
        } catch (Exception e) {
            log.error("修改充电桩信息后，推送充电枪信息给车队那边出现异常 : {}", e);
        }

        if (!CollectionUtils.isEmpty(pileVOS)) {
            //推送广告
            ThreadPoolUtil.getExecutor().execute(RunnableWrapper.of(() -> {
                List<OpLocationPileEvseElasticDTO> pileDtoList = new ArrayList<>();
                List<String> tmpList = pileVOS.stream().map(PileVO::getPileSN).collect(Collectors.toList());
                List<OpLocationElasticDTO> locationDtoList = this.opLocationRepository.findList(sellerId, tmpList);
                if (CollectionUtils.isEmpty(locationDtoList)) {
                    log.info("updateEvse,locationDtoList is empty.");
                    return;
                }
                Map<Long, OpLocationElasticDTO> locationMap = locationDtoList.stream().collect(toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
                pileVOS.stream().forEach(pileVo -> {
                    OpLocationPileEvseElasticDTO pileDto = new OpLocationPileEvseElasticDTO();
                    pileDto.setPileSn(pileVo.getPileSN());
                    pileDto.setName(pileVo.getPileName());
                    pileDto.setBrandId(pileVo.getBrandId());
                    pileDto.setBrandName(pileVo.getBrandName());
                    pileDto.setLocationId(pileVo.getLocationId());
                    pileDtoList.add(pileDto);
                });
                this.pushToAd(pileDtoList, locationMap, sellerId);
            }));
        }
        return Result.ofSucceed(pileVOS);
    }

    private void updateHubjectEvse(OpLocationEvseDTO opLocationEvseDTO) {
        if (opLocationEvseDTO != null) {
            pileBaseAsync.pushEvseList(opLocationEvseDTO.getPileSN(), ActionType.update.getCode());
        }
    }

    @Override
    public Result<List<OpEvseInfoDTO>> queryPileListByOperationId(QueryOplocationDTO queryOplocationDTO) {
        Long operatorId = queryOplocationDTO.getOperatorId();
        List<Long> locationIds = queryOplocationDTO.getLocationIds();
        return Result.ofSucceed(opLocationEvseRepository.queryPileListByOperationId(operatorId, locationIds));
    }

    @Override
    public Result<List<OpLocationEvseDTO>> getEvseListByLocationId(Long locationId) {
        if (locationId == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("param is null"));
        }
        // 判断场站是否存在
        OpLocationEntity opLocationEntity = opLocationRepository.getById(locationId);
        if (opLocationEntity == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("location not existed"));
        }
        return Result.ofSucceed(opLocationEvseRepository.queryEvseByLocationId(locationId));// todo  重新写类似的查询 mysql 包含逻辑删除
    }

    @Override
    public Result<List<OpLocationEvseDTO>> getEvseListIncludeDeletedByLocationId(Long locationId) {
        if (locationId == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("param is null"));
        }
        // 判断场站是否存在
        OpLocationEntity opLocationEntity = opLocationRepository.getById(locationId);
        if (opLocationEntity == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("location not existed"));
        }
        return Result.ofSucceed(opLocationEvseRepository.queryEvseIncludeDeletedByLocationId(locationId));
    }

    @Override
    public Result<List<LocationEvseInfoVO>> findEvseList(List<Long> locationIds) {
        log.info("findEvseList,locationIds={}", JSON.toJSONString(locationIds));
        if (CollectionUtils.isEmpty(locationIds)) {
            return Result.ofSucceed();
        }
        //全选-1
        if (locationIds.size() == 1 && locationIds.get(0) == -1) {
            Payload payload = LoginUserHolder.getLoginUser().getPayload();
            Long sellerId = payload.getSellerId();
            if (sellerId == 0L) {
                List<OpLocationMenuDTO> locationMenuDTOS = (List<OpLocationMenuDTO>) opLocationService.getStationMenu();
                if (!CollectionUtils.isEmpty(locationMenuDTOS)) {
                    locationIds = locationMenuDTOS.stream().map(OpLocationMenuDTO::getId).collect(Collectors.toList());
                }
            } else {
                List<OpLocationDTO> data = (List<OpLocationDTO>) opLocationService.getLocationBySellerId().getData();
                if (!CollectionUtils.isEmpty(data)) {
                    locationIds = data.stream().map(OpLocationDTO::getId).collect(Collectors.toList());
                }
            }
        }
        List<LocationEvseInfoVO> resultList = opLocationEvseRepository.findEvseList(locationIds);
        log.info("findEvseList,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public List<OpLocationEvseElasticDTO> findEvseList(Long locationId, String pileSn) {
        return opLocationEvseElastic.findAllByLocationIdAndPileSn(locationId, pileSn);
    }

    @Override
    public Result<List<OpPileAssociatedRuleDTO>> getPileInfoForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {
        Long tariffId = opPileAssociatedRuleParamDTO.getTariffId();
        Boolean isExcludedTariff = opPileAssociatedRuleParamDTO.getIsExcludedTariff();
        if (tariffId == null) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        Long currentUserId = UserUtil.getSellerId();
        List<OpPileAssociatedRuleDTO> ruleDTOList = opLocationEvseRepository
                .queryPileByTariffId(tariffId, currentUserId, isExcludedTariff);
        return Result.ofSucceed(ruleDTOList);
    }

    @Override
    public Result<Page<OpPileAssociatedRuleDTO>> getPileListForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {
        try {
            log.info("===>>> OpLocationEvseServiceImpl.getPileListForRules opPileAssociatedRuleParamDTO : {}", JSON.toJSONString(opPileAssociatedRuleParamDTO));
            Page<OpPileAssociatedRuleDTO> opPileAssociatedRuleDTOPage = opLocationEvseRepository.queryPileByTariffIdForPage(opPileAssociatedRuleParamDTO);

            log.info("====>>> OpLocationEvseServiceImpl getPileListForRules and opPileAssociatedRuleDTOPage : {}", JSON.toJSONString(opPileAssociatedRuleDTOPage));

            return Result.ofSucceed(opPileAssociatedRuleDTOPage);
        } catch (Exception e) {
            log.error("===>>>OpLocationEvseServiceImpl getPileListForRules and exception : {}", e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    @Deprecated
    public Result<Boolean> updateEvseState(String evseSn, String state) {
        try {
            log.info("OpLocationEvseServiceImpl updateEvseState and evseSn = " + evseSn + " and state = " + state);
            return Result.ofSucceed(opLocationEvseRepository.updateEvseState(evseSn, state));
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl updateEvseState and exception = " + e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public Result<Boolean> updateEvseStateByUpdateAt(String evseSn, String state, Long updatedAt) {
        log.info("OpLocationEvseServiceImpl updateEvseState and evseSn = " + evseSn + " and state = " + state + " and updatedAt = " + updatedAt);
        String key = OpLocationEvseElasticDTO.class.getAnnotation(Document.class).indexName() + ":" + evseSn;
        try {
//            Boolean aBoolean;
//            aBoolean = stringRedisTemplate.opsForValue().setIfAbsent(key, evseSn + ":" + state, 50, TimeUnit.SECONDS);
//            do {
//                aBoolean = stringRedisTemplate.opsForValue().setIfAbsent(key, evseSn + ":" + state, 50, TimeUnit.SECONDS);
//            } while (Boolean.FALSE.equals(aBoolean));
            return Result.ofSucceed(opLocationEvseRepository.updateEvseStateByUpdateAt(evseSn, state, updatedAt));
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl updateEvseState and exception ", e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        } finally {
            log.info("remove lock key :{} {}, {}", key, evseSn + ":" + state, stringRedisTemplate.delete(key));
        }
    }

    @Override
    public Result<Boolean> batchUpdateEvseStateByUpdateAt(Collection<EvseStateDTO> evseStateDTOs) {
        try {
            log.info("OpLocationEvseServiceImpl updateEvseState and evseSn size = " + evseStateDTOs.size());
            return Result.ofSucceed(opLocationEvseRepository.batchUpdateEvseStateByUpdateAt(evseStateDTOs));
        } catch (Exception e) {
            log.error("OpLocationEvseServiceImpl updateEvseState and exception ", e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public OpLocationEvseDetailVO getDetailsFromDbById(Long id) {
        return opLocationEvseRepository.getDetailsFromDbById(id);
    }

    @Override
    public Result<List<PileInfoDTO>> queryLocationAndPileBySeller(Long sellerId) {
        try {
            log.info("queryLocationAndPileBySeller start,sellerId:{}", sellerId);
            List<PileInfoDTO> pileInfoDTOList = new ArrayList<>();
            if (Objects.isNull(sellerId)) {
                return Result.ofSucceed(pileInfoDTOList);
            }
            //查询该商家下所有场站
            QueryOplcationDTO queryOplcationDTO = new QueryOplcationDTO();
            queryOplcationDTO.setOperatorId(sellerId);
            List<OpLocationDTO> opLocationDTOS = (List<OpLocationDTO>) opLocationService.getOpLocationBySellerId(queryOplcationDTO);
            //查询场站下对应的桩
            if (CollUtil.isEmpty(opLocationDTOS)) {
                return Result.ofSucceed(pileInfoDTOList);
            }
            log.info("opLocationDTOS:{}", JSON.toJSONString(opLocationDTOS));
            opLocationDTOS.stream().forEach(p1 -> {
                if (Objects.isNull(p1.getOperatorId())) {
                    p1.setOperatorId(sellerId);
                }
                //查询场站下面的桩  查出来的是桩枪列表，做归并处理
                List<OpLocationEvseDTO> opLocationEvseDTOS = opLocationEvseRepository.queryEvseByLocationId(p1.getId());
                log.info("opLocationEvseDTOS:{}", JSON.toJSONString(opLocationEvseDTOS));
                if (CollUtil.isNotEmpty(opLocationEvseDTOS)) {
                    Map<String, List<OpLocationEvseDTO>> groupByPileSnMap = opLocationEvseDTOS.stream().collect(Collectors.groupingBy(OpLocationEvseDTO::getPileSN));
                    log.info("groupByPileSnMap:{}", JSON.toJSONString(groupByPileSnMap));
                    Iterator<Map.Entry<String, List<OpLocationEvseDTO>>> it = groupByPileSnMap.entrySet().iterator();
                    while (it.hasNext()) {
                        List<OpLocationConnectorDTO> opLocationConnectorDTOs = new ArrayList<>();
                        Map.Entry<String, List<OpLocationEvseDTO>> next = it.next();
                        List<OpLocationEvseDTO> lists = next.getValue();
                        if (CollUtil.isNotEmpty(lists)) {
                            lists.stream().forEach(p -> {
                                OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
                                opLocationConnectorDTO.setGunType(p.getGunType());
                                opLocationConnectorDTOs.add(opLocationConnectorDTO);
                            });
                            OpLocationEvseDTO opLocationEvseDTO = lists.get(0);
                            opLocationEvseDTO.setOpLocationConnectorDTOs(opLocationConnectorDTOs);
                            pileInfoDTOList.add(formatPileInfoDTO(opLocationEvseDTO, p1));
                        }
                    }
                }
            });
            log.info("queryLocationAndPileBySeller end.  pileInfoDTOList:{}", JSON.toJSONString(pileInfoDTOList));
            return Result.ofSucceed(pileInfoDTOList);
        } catch (Exception e) {
            log.error("queryLocationAndPileBySeller查询异常：", e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }

    @Override
    public Result<List<HubPileDTO>> queryPileInfoByPileSn(List<String> pileSnList) {
        List<HubPileDTO> hubPileDTOList = Lists.newArrayList();
        if (CollectionUtils.isEmpty(pileSnList)) {
            return Result.ofSucceed(hubPileDTOList);
        }

        List<HubPileDTO> pileInfoByPileSn = opLocationEvseRepository.getPileInfoByPileSn(pileSnList);

        return Result.ofSucceed(pileInfoByPileSn);
    }

    @Override
    public Integer syncEvseExpand(EvseExpandDTO expandDTO) {
        log.info("syncEvseExpand,expandDTO={}", JSON.toJSONString(expandDTO));
        List<Long> locationIds = expandDTO.getLocationIds();
        List<Long> pileIds = expandDTO.getPileIds();
        //同步前是否删除已有数据
        if (expandDTO.getDelete() != null && expandDTO.getDelete()) {
            opLocationEvseExpandElasticService.deleteAll();
        }
        //按枪ID同步
        if (CollectionUtil.isNotEmpty(pileIds)) {
            return this.syncEvseExpandByEvseIds(pileIds);
        }
        //按场站ID同步
        List<OpLocationElasticDTO> dtoList = this.getLocationListFromEs(locationIds);
        if (CollectionUtils.isEmpty(dtoList)) {
            log.info("syncEvseExpand,dtoList={}", dtoList);
            return 0;
        }
        Map<Long, OpLocationElasticDTO> locationMap = dtoList.stream().collect(Collectors.toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
        log.info("syncEvseExpand,locationMap size={}", locationMap.size());
        if (locationMap.size() > 0) {
            AtomicReference<Integer> count = new AtomicReference<>(0);
            locationMap.forEach((k, v) -> {
                List<OpLocationEvseElasticDTO> evseList = opLocationEvseElastic.findAllByLocationId(k);
                List<OpLocationEvseExpandElasticDTO> list = new ArrayList<>();
                evseList.stream().forEach(evseDto -> {
                    OpLocationEvseExpandElasticDTO dto = new OpLocationEvseExpandElasticDTO();
                    BeanUtils.copyProperties(v, dto);
                    dto.setId(evseDto.getId());
                    dto.setLocationId(v.getId());
                    dto.setPower(evseDto.getPower());
                    dto.setGunState(evseDto.getState());
                    dto.setGunType(evseDto.getGunType());
                    dto.setTariffId(evseDto.getTariffId());
                    dto.setSubscriptionCheck(evseDto.getSubscriptionCheck());
                    list.add(dto);
                });
                if (list.size() > 0) {
                    opLocationEvseExpandElasticService.saveAll(list);
                    log.info("syncEvseExpand,locationId={}, svseSize={}", k, list.size());
                    count.updateAndGet(v1 -> v1 + list.size());
                }
            });
            return count.get();
        }
        return 0;
    }

    @Override
    public Result<OpEvseDTO> getEvseInfoByEvseSn(String evseSn) {
        //根据设备编码查询
        if (StringUtils.isEmpty(evseSn)) {
            log.info("getEvseInfoByEvseSn,evseSn={}", evseSn);
            return Result.ofSucceed();
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("evseSn", evseSn));
        Iterable<OpLocationEvseElasticDTO> search =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        Iterator<OpLocationEvseElasticDTO> it = search.iterator();
        if (it.hasNext()) {
            OpLocationEvseElasticDTO dto = it.next();
            //场站存在查询场站信息
            Long locationId = null;
            String zoneId = null;
            String locationName = null;
            int platForm = OpLocationPlatformEnum.AUTEL.getCode();
            Long operatorId = null;
            if (dto.getLocationId() != null) {
                OpLocationElasticDTO locationResult = opLocationService.findById(dto.getLocationId());
                if (locationResult != null) {
                    locationId = locationResult.getId();
                    zoneId = locationResult.getZoneId();
                    locationName = locationResult.getName();
                    platForm = locationResult.getPlatform();
                    operatorId = locationResult.getOperatorId();
                } else {
                    log.info("getEvseInfoByEvseSn,locationId={}", dto.getLocationId());
                }
            }
            OpEvseDTO result = OpEvseDTO.builder()
                    .id(dto.getId())
                    .evseSn(dto.getEvseSn())
                    .tariffId(dto.getTariffId())
                    .power(dto.getPower())
                    .gunType(dto.getGunType())
                    .locationId(locationId)
                    .locationName(locationName)
                    .zoneId(zoneId)
                    .platForm(platForm)
                    .state(dto.getState())
                    .operatorId(operatorId)
                    .evseUid(dto.getEvseUid())
                    .build();
            return Result.ofSucceed(result);
        }
        return Result.ofSucceed();
    }

    @Override
    public OpLocationInfoDTO getEvseList(String pileSn) {
        if (StringUtils.isEmpty(pileSn)) {
            log.info("getEvseList,pileSn={}", pileSn);
            return null;
        }
        OpLocationPileEvseElasticDTO pileEvseElasticDTO = opLocationPileEvseService.findByPileSn(pileSn);
        if (pileEvseElasticDTO == null) {
            log.info("getEvseList,pileEvseElasticDTO={}", pileEvseElasticDTO);
            return null;
        }
        Long locationId = pileEvseElasticDTO.getLocationId();
        Long ruleId = pileEvseElasticDTO.getRuleId();
        if (locationId != null) {
            OpLocationElasticDTO locationElasticDTO = opLocationService.findById(locationId);
            if (locationElasticDTO != null) {
                OpLocationInfoDTO result = new OpLocationInfoDTO();
                result.setLocationId(locationId);
                result.setLocationName(locationElasticDTO.getName());
                result.setOpenType(locationElasticDTO.getOpenType());
                result.setZoneId(locationElasticDTO.getZoneId());
                result.setOperatorId(locationElasticDTO.getOperatorId());
                List<OpLocationEvseElasticDTO> evseDtoList = this.findEvseList(locationId, pileSn);
                if (!CollectionUtils.isEmpty(evseDtoList)) {
                    List<OplocationConnectorScanDTO> evseList = new ArrayList<>(evseDtoList.size());
                    evseDtoList.forEach(dto -> {
                        OplocationConnectorScanDTO vo = new OplocationConnectorScanDTO();
                        vo.setPileSn(dto.getPileSn());
                        vo.setRuleId(dto.getRuleId());
                        vo.setConnectorNumber(Integer.parseInt(dto.getEvseSn().split("_")[1]));
                        vo.setConnectorType(dto.getGunType());
                        vo.setTariffId(dto.getTariffId());
                        evseList.add(vo);
                    });
                    result.setEvseList(evseList);
                }
                return result;
            }
        }
        return null;
    }

    @Override
    public Result<CostModelRuleDTO> getTatiffRuleByEvseSn(PileEvseDTO pileEvseDTO) {
        String evseSn = pileEvseDTO.getPileSn() + "_" + pileEvseDTO.getGunNo();
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findByEvseSn(evseSn);
        if (ObjectUtils.isEmpty(opLocationEvseElasticDTO) || opLocationEvseElasticDTO.getTariffId() == null) {
            return Result.ofSucceed(null);
        }
        // 计费规则id
        Long tariffId = opLocationEvseElasticDTO.getTariffId();
        // 场站id
        Long locationId = opLocationEvseElasticDTO.getLocationId();
        CostModelRuleDTO costModelRuleDTO = opLocationEvseRepository.getTatiffRuleByTariffId(tariffId);
        if (costModelRuleDTO != null && locationId != null) {
            OpLocationElasticDTO opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(locationId);
            if (opLocationElasticDTO != null) {
                String taxConfiguration = opLocationElasticDTO.getTaxConfiguration();
                if (org.apache.commons.lang3.StringUtils.isNotBlank(taxConfiguration)) {
                    costModelRuleDTO.setTaxDTO(JSON.parseObject(taxConfiguration, TaxDTO.class));
                }
            }
        }
        return Result.ofSucceed(costModelRuleDTO);
    }

    /**
     * @param tariffId 计费规则id
     * @return 某个计费规则id绑定的桩的类型
     * @function 查询某个计费规则id绑定的桩的类型
     */
    @Override
    public List<SimpleChargePileVO> getSimpleChargePileInfoList(Long tariffId) {

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.getSimpleChargePileInfoList tariffId : {}",
                JSON.toJSONString(tariffId));

        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseRepository.getPileSnListByTariffIdList(Lists.newArrayList(tariffId));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseEntityList)) {
            return null;
        }

        Set<String> pileSnSet = new HashSet<>();
        for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
            String evseSn = opLocationEvseEntity.getEvseSn();
            pileSnSet.add(evseSn.split("_")[0]);
        }

        List<SimpleChargePileVO> simpleChargePileVOList = new ArrayList<>();
        pileSnSet.forEach(var -> {
            SimpleChargePileVO simpleChargePileVO = new SimpleChargePileVO();
            simpleChargePileVO.setSn(var);
            simpleChargePileVOList.add(simpleChargePileVO);
        });

        Result<List<SimpleChargePileVO>> result = deviceServiceFeign.getSimpleChargePileInfoList(new ArrayList<>(pileSnSet));

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.getSimpleChargePileInfoList result : {}",
                JSON.toJSONString(result));

        if (result != null
                && Integer.valueOf(HttpStatus.HTTP_OK).equals(result.getCode())
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(result.getData())) {
            simpleChargePileVOList.forEach(var -> {
                for (SimpleChargePileVO simpleChargePileVO : result.getData()) {
                    if (var.getSn().equals(simpleChargePileVO.getSn())){
                        BeanUtils.copyProperties(simpleChargePileVO, var);
                        break;
                    }
                }
            });
        }

        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(new ArrayList<>(pileSnSet));
        simpleChargePileVOList.forEach(var -> {
            for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
                if (var.getSn().equals(opLocationPileEvseElasticDTO.getPileSn())) {
                    var.setFreeVendEnable(opLocationPileEvseElasticDTO.getFreeVendEnable());
                    break;
                }
            }
        });
        return simpleChargePileVOList;
    }


    @Override
    public List<DispatchTariffPileInfoVO> getDispatchTariffPileInfo(Long tariffId) {

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.getDispatchTariffPileInfo tariffId : {}",
                JSON.toJSONString(tariffId));

        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseRepository.getPileSnListByTariffIdList(Lists.newArrayList(tariffId));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseEntityList)) {
            return null;
        }

        Set<String> pileSnSet = new HashSet<>();
        for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
            String evseSn = opLocationEvseEntity.getEvseSn();
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(evseSn) && evseSn.contains("_")) {
                String pileSn = evseSn.split("_")[0];
                pileSnSet.add(pileSn);
            }
        }

        Result<List<DispatchTariffPileInfoVO>> dispatchTariffPileInfoResult = deviceServiceFeign.dispatchTariffPileInfo(new ArrayList<>(pileSnSet));

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.getDispatchTariffPileInfo dispatchTariffPileInfoResult : {}",
                JSON.toJSONString(dispatchTariffPileInfoResult));

        return dispatchTariffPileInfoResult.getData();
    }


    @Override
    public List<OpLocationEvseElasticDTO> findList(List<String> pileSnList) {
        return opLocationEvseRepository.findList(pileSnList);
    }

    @Override
    public Result<Boolean> syncGunType(String pileSn) {
        log.info("syncGunType,pileSn={}", pileSn);
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseRepository.findList(Arrays.asList(pileSn));
        evseDtoList.stream().forEach(dto -> dto.setGunType(this.getGunType(dto.getPileSn())));
        Map<Long, OpLocationEvseElasticDTO> evseDtoMap = evseDtoList.stream().collect(Collectors.toMap(OpLocationEvseElasticDTO::getId, e -> e, (f, s) -> f));
        opLocationEvseRepository.savBatchEs(evseDtoList);
        opLocationEvseExpandElasticService.updateBatch(evseDtoList);
        List<Long> ids = evseDtoList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
        List<OpLocationConnectorEntity> connectorEntityList = opLocationConnectorService.findByEvseId(ids);
        connectorEntityList.stream().forEach(entity -> entity.setGunType(evseDtoMap.get(entity.getLocationEvseId()).getGunType()));
        opLocationConnectorService.updateBatch(connectorEntityList);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<List<Long>> dataCheck() {
        List<Long> resultList = new ArrayList<>();
        List<OpLocationEvseExpandElasticDTO> evseExpandDtoList = opLocationEvseExpandElasticService.findAll();
        log.info("dataCheck,evseExpandDtoList size={}", evseExpandDtoList.size());
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseRepository.findAll();
        log.info("dataCheck,evseDtoList size={}", evseExpandDtoList.size());
        if (evseExpandDtoList.size() == evseDtoList.size()) {
            return Result.ofSucceed(resultList);
        }
        if (evseExpandDtoList.size() > evseDtoList.size()) {
            List<Long> evseIds = evseDtoList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
            evseExpandDtoList.stream().forEach(dto -> {
                if (!evseIds.contains(dto.getId())) {
                    resultList.add(dto.getId());
                }
            });
        } else {
            List<Long> evseExpandIds = evseExpandDtoList.stream().map(OpLocationEvseExpandElasticDTO::getId).collect(Collectors.toList());
            evseDtoList.stream().forEach(dto -> {
                if (!evseExpandIds.contains(dto.getId())) {
                    resultList.add(dto.getId());
                }
            });
        }
        return Result.ofSucceed(resultList);
    }

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 下发充电枪锁枪或者不锁枪的命令
     */
    @Override
    public Boolean setCableEnable(LockOrUnlockGunDTO lockOrUnlockGunDTO) {

        log.info("===>>>OpLocationEvseServiceImpl.setCableEnable lockOrUnlockGunDTO : {}", JSON.toJSONString(lockOrUnlockGunDTO));

        // 充电桩序列号
        @NotNull String pileSn = lockOrUnlockGunDTO.getPileSn();
        // 枪号
        @NotNull Integer connectorId = lockOrUnlockGunDTO.getConnectorId();

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        lockOrUnlockGunDTO.setUserId(String.valueOf(jwtInfo.getPayload().getUserId()));
        lockOrUnlockGunDTO.setNeedToPushMark(true);

        // 充电枪序列号
        String evseSn = pileSn + "_" + String.valueOf(connectorId);

        //todo 充电设备处于离线状态，就不发送锁枪或者解锁枪的命令了
        Result<String> result = monitorFeignClient.queryStatusByEvseSn(evseSn);

        log.info("===>>>OpLocationEvseServiceImpl.setCableEnable result : {}", JSON.toJSONString(result));

        if (result == null
                || HttpStatus.HTTP_OK != result.getCode()
                || EvseDeviceStatusEnum.DEFAULT.getName().equalsIgnoreCase(result.getData())) {
            // 抛出异常
            throw new MessageCodeException(PileBaseEnum.THIS_SETTING_IS_NOT_CURRENTLY_SUPPORTED);
        }

        // 调用协议服务，发送锁枪/解锁枪操作命令
        Result<Boolean> booleanResult = protocolFeignClient.setCableEnable(lockOrUnlockGunDTO);

        log.info("===>>>OpLocationEvseServiceImpl.setCableEnable booleanResult : {}", JSON.toJSONString(booleanResult));

        return Boolean.TRUE;
    }

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 获得充电枪的状态（返回此时充电枪是否已经插入充电口的标志）
     */
    @Override
    public Boolean getEvseInPile(LockOrUnlockGunDTO lockOrUnlockGunDTO) {

        log.info("===>>>OpLocationEvseServiceImpl.getEvseInPile lockOrUnlockGunDTO : {}", JSON.toJSONString(lockOrUnlockGunDTO));

        // 充电桩序列号
        @NotNull String pileSn = lockOrUnlockGunDTO.getPileSn();
        // 枪号
        @NotNull Integer connectorId = lockOrUnlockGunDTO.getConnectorId();

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        lockOrUnlockGunDTO.setUserId(String.valueOf(jwtInfo.getPayload().getUserId()));
        lockOrUnlockGunDTO.setNeedToPushMark(true);

        // 充电枪序列号
        String evseSn = pileSn + "_" + String.valueOf(connectorId);

        //todo 充电设备处于离线状态，就不发送获得充电枪的状态(充电枪是否已经插入充电口)的命令了
        Result<String> result = monitorFeignClient.queryStatusByEvseSn(evseSn);

        log.info("===>>>OpLocationEvseServiceImpl.getEvseInPile result : {}", JSON.toJSONString(result));

        if (result == null
                || HttpStatus.HTTP_OK != result.getCode()
                || EvseDeviceStatusEnum.DEFAULT.getName().equalsIgnoreCase(result.getData())) {
            // 抛出异常
            throw new MessageCodeException(PileBaseEnum.THIS_SETTING_IS_NOT_CURRENTLY_SUPPORTED);
        }

        // 调用协议服务，发送获得充电枪的状态的命令
        Result<Boolean> booleanResult = protocolFeignClient.getCableEnable(lockOrUnlockGunDTO);

        log.info("===>>>OpLocationEvseServiceImpl.getEvseInPile booleanResult : {}", JSON.toJSONString(booleanResult));

        return Boolean.TRUE;
    }

    @Override
    public Result<Map<String, String>> getSnTimeZoneMap(List<String> evseSnList) {
        List<OcppLocationEVSETimeZoneVO> ocppLocationEVSETimeZoneVOList = opLocationEvseRepository.getOcppLocationEVSETimeZoneVOList(evseSnList);
        if (CollectionUtils.isEmpty(ocppLocationEVSETimeZoneVOList)) {
            return Result.ofSucceed(new HashMap<>());
        }
        Map<String, String> map = new HashMap<>();
        ocppLocationEVSETimeZoneVOList.forEach(e -> {
            String pileSn = e.getEvseSn().substring(0, e.getEvseSn().indexOf("_"));
            map.put(pileSn, e.getZoneId());
        });
        return Result.ofSucceed(map);
    }

    /**
     * @param tariffId 计费规则id
     * @return 场站id集合
     * @function 根据计费规则id，查询充电设备表，获得该计费规则所使用的场站集合
     */
    @Override
    public List<Long> getAllLocationIdByTariffId(Long tariffId) {

        log.info("===>>>OpLocationEvseServiceImpl.getAllLocationIdByTariffId tariffId: {}", JSON.toJSONString(tariffId));

        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseRepository.getAllLocationIdByTariffId(tariffId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEvseEntityList)) {
            List<Long> locationIdList = new ArrayList<>();
            for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
                locationIdList.add(opLocationEvseEntity.getLocationId());
            }
            return locationIdList;
        }
        return null;
    }

    /**
     * @param pileEvseDTO 序列号
     * @return 与场站（桩或枪）计费相关的信息
     * @function 查询与场站（桩或枪）计费相关的信息
     */
    @Override
    public TariffInfoVO queryTariffInfoVO(PileEvseDTO pileEvseDTO) {

        log.info("===>>>OpLocationEvseServiceImpl.queryTariffInfoVO pileEvseDTO: {}", JSON.toJSONString(pileEvseDTO));

        if (pileEvseDTO == null
                || pileEvseDTO.getPileSn() == null
                || pileEvseDTO.getGunNo() == null) {
            return null;
        }
        String evseSn = pileEvseDTO.getPileSn() + "_" + pileEvseDTO.getGunNo();
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findByEvseSn(evseSn);
        if (ObjectUtils.isEmpty(opLocationEvseElasticDTO) || opLocationEvseElasticDTO.getTariffId() == null) {
            return null;
        }
        // 充电桩绑定的计费规则组id
        Long tariffGroupId = opLocationEvseElasticDTO.getTariffId();
        TariffInfoVO tariffInfoVO = new TariffInfoVO();
        tariffInfoVO.setTariffGroupId(tariffGroupId);
        // 场站id
        Long locationId = opLocationEvseElasticDTO.getLocationId();
        OpLocationElasticDTO opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(locationId);
        if (opLocationElasticDTO != null) {
            tariffInfoVO.setLocationTaxConfiguration(opLocationElasticDTO.getTaxConfiguration());
            tariffInfoVO.setZoneId(opLocationElasticDTO.getZoneId());
            tariffInfoVO.setTimeZone(opLocationElasticDTO.getTimeZone());
        }
        // 桩是否开启了互联互通的标志
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileEvseDTO.getPileSn());
        if (opLocationPileEvseElasticDTO != null) {
            tariffInfoVO.setHubjectEnable(opLocationPileEvseElasticDTO.getEroamingEnable());
        }
        return tariffInfoVO;
    }

    private boolean loosenRestrictionsBySellerId(Long sellerId) {

        log.info("===>>> CostModelRuleImpl.loosenRestrictionsBySellerId sellerId : {}",
                JSON.toJSONString(sellerId));
        return sellerId != null
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(this.releaseBillingRestrictionsSellerIdList)
                && this.releaseBillingRestrictionsSellerIdList.contains(sellerId);
    }

    @Override
    public Result<Boolean> bindCostModelRuleGroupForGun(List<BindCostModelRuleGroupForGunDTO> bindCostModelRuleGroupForGunDTOList) {

        log.info("===>>>OpLocationEvseServiceImpl.bindCostModelRuleGroupForGun bindCostModelRuleGroupForGunDTOList : {}",
                JSON.toJSONString(bindCostModelRuleGroupForGunDTOList));

        if (CollUtil.isEmpty(bindCostModelRuleGroupForGunDTOList)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }

        Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();

        if (!this.checkBindCostModelRuleGroupForGun(bindCostModelRuleGroupForGunDTOList)) {
            throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
        }

        // 构造返回对象
        Result<Boolean> result = new Result<>();

        // 构造充电枪id与计费规则组id之间的映射关系
        Map<Long, Long> gunIdAndTariffGroupIdMap = new HashMap<>();

        // 枪id集合
        List<Long> gunIdList = new ArrayList<>();
        Set<Long> tariffGroupIdSet = new HashSet<>();
        for (BindCostModelRuleGroupForGunDTO bindCostModelRuleGroupForGunDTO : bindCostModelRuleGroupForGunDTOList) {
            gunIdList.add(bindCostModelRuleGroupForGunDTO.getGunId());
            if (bindCostModelRuleGroupForGunDTO.getTariffGroupId() != null) {
                tariffGroupIdSet.add(bindCostModelRuleGroupForGunDTO.getTariffGroupId());
            }
            gunIdAndTariffGroupIdMap.put(bindCostModelRuleGroupForGunDTO.getGunId(), bindCostModelRuleGroupForGunDTO.getTariffGroupId());
        }

        // 从MySQL数据库查询出充电设备（充电枪）的信息
        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseRepository.getEvseInfoFromMySQL(gunIdList);
        // 从ES中查询出充电设备（充电枪）的信息
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseRepository.getEvseInfoFromES(gunIdList);

        // todo  如果商家要绑定的某个充电枪的计费规则是收费的，并且商家自己的Stripe账户不能进行收费，那么此时就不能对这个充电枪配置该收费的计费规则
        if (!this.getSellerBindNotFreeCostModelRuleGroupforGunMark(tariffGroupIdSet)) {
            // 给出提示语：关联此计费规则需要先设置收款账户 在完成设置之前，此充电桩暂无法使用。
            throw new MessageCodeException(PileBaseEnum.PLEASE_BIND_THE_RECEIVING_ACCOUNT_FIRST);
        }

        // 5寸桩集合
        List<String> fiveInchesPileSnList = new ArrayList<>();
        // todo 根据充电桩型号校验这些充电枪是否能绑定计费规则组
        if (!this.isAssoTariffRule(opLocationEvseElasticDTOList, tariffGroupIdSet, gunIdList, gunIdAndTariffGroupIdMap, result, fiveInchesPileSnList)) {
            // 依据不能绑定的类型不同，给出相应的提示语
            return result;
        }

        long currentTimeMillis = System.currentTimeMillis();
        // 给实体类赋值(MySQL)
        for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
            opLocationEvseEntity.setTariffId(gunIdAndTariffGroupIdMap.get(opLocationEvseEntity.getId()));
            opLocationEvseEntity.setUpdatedAt(currentTimeMillis);
        }
        // 给实体类赋值(ES)
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
            opLocationEvseElasticDTO.setTariffId(gunIdAndTariffGroupIdMap.get(opLocationEvseElasticDTO.getId()));
            opLocationEvseElasticDTO.setUpdatedAt(currentTimeMillis);
        }

        // 批量保存记录
        boolean flag = opLocationEvseRepository.bindCostModelRuleGroupForGun(opLocationEvseEntityList, opLocationEvseElasticDTOList);
        if (flag) {
            // 计费规则下发与清空逻辑处理
            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> this.issueBillingRules(userId, sellerId, gunIdAndTariffGroupIdMap)));
        }
        if (flag && !CollectionUtils.isEmpty(opLocationEvseElasticDTOList)) {
            // 推送eroaming 桩计费绑定关系
            Set<String> pileSnSet = opLocationEvseElasticDTOList.stream()
                    .map(OpLocationEvseElasticDTO::getPileSn)
                    .collect(Collectors.toSet());
            for (String pileSn : pileSnSet) {
                pileBaseAsync.pushEvseList(pileSn, ActionType.update.getCode());
            }
        }
        // 为充电设备（充电枪）绑定计费规则组成功
        result.setCode(200);
        result.setData(Boolean.TRUE);
        return result;
    }

    private void issueBillingRules(Long userId, Long sellerId, Map<Long, Long> gunIdAndTariffGroupIdMap) {

        log.info("===>>>OpLocationEvseServiceImpl.issueBillingRules userId : {} and sellerId : {} and gunIdAndTariffGroupIdMap : {}",
                JSON.toJSONString(userId),
                JSON.toJSONString(sellerId),
                JSON.toJSONString(gunIdAndTariffGroupIdMap));

        Set<Long> gunIdSet = gunIdAndTariffGroupIdMap.keySet();
        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseMapper.selectBatchIds(gunIdSet);
        List<String> pileSnList = opLocationEvseEntityList.stream()
                .filter(var -> var != null && org.apache.commons.lang3.StringUtils.isNotBlank(var.getEvseSn()))
                .map(var -> {
                    return var.getEvseSn().split("_")[0];
                })
                .distinct()
                .collect(Collectors.toList());

        locationCommon.issueBillingRule(pileSnList);
    }

    private boolean checkBindCostModelRuleGroupForGun(List<BindCostModelRuleGroupForGunDTO> bindCostModelRuleGroupForGunDTOList) {

        log.info("===>>>OpLocationEvseServiceImpl.checkBindCostModelRuleGroupForGun bindCostModelRuleGroupForGunDTOList : {}",
                JSON.toJSONString(bindCostModelRuleGroupForGunDTOList));

        List<Long> tariffGroupIdList = bindCostModelRuleGroupForGunDTOList.stream().map(BindCostModelRuleGroupForGunDTO::getTariffGroupId).collect(Collectors.toList());
        List<Long> gunIdList = bindCostModelRuleGroupForGunDTOList.stream().map(BindCostModelRuleGroupForGunDTO::getGunId).collect(Collectors.toList());

        Result<List<Long>> result = null;
        try {
            result = tariffFeignClient.getTariffGroupIds();

            log.info("===>>>OpLocationEvseServiceImpl.checkBindCostModelRuleGroupForGun bindCostModelRuleGroupForGunDTOList : {}",
                    JSON.toJSONString(bindCostModelRuleGroupForGunDTOList));

        } catch (Exception e) {

            log.error("===>>>OpLocationEvseServiceImpl.checkBindCostModelRuleGroupForGun 调用计费服务出现了异常！");

            return false;
        }

        if (result == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData())) {
            return false;
        }

        for (Long tariffGroupId : tariffGroupIdList) {
            boolean flag = false;
            for (Long aLong : result.getData()) {
                if (tariffGroupId.longValue() == aLong.longValue()) {
                    flag = true;
                    break;
                }
            }
            if (!flag) {
                return false;
            }
        }

        List<Long> locationIdList = this.getLocationIdList();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(locationIdList)) {
            return false;
        }

        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseRepository.getEvseInfoFromES(gunIdList);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseElasticDTOList)) {
            return false;
        }
        List<Long> stationIdList = opLocationEvseElasticDTOList.stream().map(OpLocationEvseElasticDTO::getLocationId).collect(Collectors.toList());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(stationIdList)) {
            return false;
        }
        for (Long stationId : stationIdList) {
            boolean flag = false;
            for (Long locationId : locationIdList) {
                if (stationId.longValue() == locationId.longValue()) {
                    flag = true;
                    break;
                }
            }
            if (!flag) {
                return false;
            }
        }
        return true;
    }

    private List<Long> getLocationIdList() {
        try {
            List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
            log.info("PageAuthCheckServiceImpl getLocationIdList locationIdList = "
                    + JSON.toJSONString(locationIdList));
            if (CollectionUtils.isEmpty(locationIdList)) {
                return Collections.emptyList();
            }
            return locationIdList;
        } catch (Exception e) {
            log.info("PageAuthCheckServiceImpl getLocationIdList Exception = ", e);
            return Collections.emptyList();
        }
    }

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的充电枪数据
     */
    @Override
    public List<EvseInfoVO> getAllEvseInfoBySellerId(Long sellerId) {

        if (sellerId == null) {
            return null;
        }

        // 查询充电枪ES，获取数据
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseRepository.getAllEvseInfoBySellerId(sellerId);

        // 构建充电桩序列号与充电桩信息之间的映射关系
        Map<String, OpLocationPileEvseElasticDTO> pileSnAndOpLocationPileEvseElasticDTOMap = new HashMap<>();
        // 查询充电桩ES，获取数据
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseRepository.getAllPileInfoBySellerId(sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
            for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
                String pileSn = opLocationPileEvseElasticDTO.getPileSn();
                pileSnAndOpLocationPileEvseElasticDTOMap.put(pileSn, opLocationPileEvseElasticDTO);
            }
        }

        // 构建场站id与场站信息之间的映射关系
        Map<Long, OpLocationElasticDTO> locationIdAndOpLocationElasticDTOMap = new HashMap<>();
        // 查询场站ES，获取数据
        List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationRepository.getAllLocationInfoBySellerId(sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationElasticDTOList)) {
            for (OpLocationElasticDTO opLocationElasticDTO : opLocationElasticDTOList) {
                Long id = opLocationElasticDTO.getId();
                locationIdAndOpLocationElasticDTOMap.put(id, opLocationElasticDTO);
            }
        }

        // 构造返回对象
        List<EvseInfoVO> evseInfoVOList = new ArrayList<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
            for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                EvseInfoVO evseInfoVO = new EvseInfoVO();
                Long evseId = opLocationEvseElasticDTO.getId();
                String evseSn = opLocationEvseElasticDTO.getEvseSn();
                String pileSn = opLocationEvseElasticDTO.getPileSn();
                Long locationId = opLocationEvseElasticDTO.getLocationId();
                evseInfoVO.setId(evseId);
                evseInfoVO.setEvseSn(evseSn);
                if (pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn) != null) {
                    OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn);
                    PileInfoVO evsePileInfoVO = new PileInfoVO();
                    Long pileId = opLocationPileEvseElasticDTO.getId();
                    String pileName = opLocationPileEvseElasticDTO.getName();
                    Long pileUpdatedAt = opLocationPileEvseElasticDTO.getUpdatedAt();
                    evsePileInfoVO.setId(pileId);
                    evsePileInfoVO.setPileSn(pileSn);
                    evsePileInfoVO.setPileName(pileName);
                    evsePileInfoVO.setUpdatedAt(pileUpdatedAt);
                    if (locationIdAndOpLocationElasticDTOMap.get(locationId) != null) {
                        OpLocationElasticDTO opLocationElasticDTO = locationIdAndOpLocationElasticDTOMap.get(locationId);
                        LocationInfoVO pileLocationInfoVO = new LocationInfoVO();
                        String locationName = opLocationElasticDTO.getName();
                        String country = opLocationElasticDTO.getCountry();
                        String province = opLocationElasticDTO.getProvince();
                        String city = opLocationElasticDTO.getCity();
                        String address = opLocationElasticDTO.getAddress();
                        String postalCode = opLocationElasticDTO.getPostalCode();
                        String latitude = opLocationElasticDTO.getLatitude();
                        String longitude = opLocationElasticDTO.getLongitude();
                        String zoneId = opLocationElasticDTO.getZoneId();
                        Long updatedAt = opLocationElasticDTO.getUpdatedAt();
                        pileLocationInfoVO.setLocationId(locationId);
                        pileLocationInfoVO.setLocationName(locationName);
                        pileLocationInfoVO.setCountry(country);
                        if (org.apache.commons.lang3.StringUtils.isNotBlank(country)) {
                            pileLocationInfoVO.setCountryName(opLocationRepository.changeCountry(country));
                        }
                        pileLocationInfoVO.setProvince(province);
                        pileLocationInfoVO.setCity(city);
                        pileLocationInfoVO.setAddress(address);
                        pileLocationInfoVO.setPostalCode(postalCode);
                        pileLocationInfoVO.setLatitude(latitude);
                        pileLocationInfoVO.setLongitude(longitude);
                        pileLocationInfoVO.setZoneId(zoneId);
                        pileLocationInfoVO.setUpdatedAt(updatedAt);
                        evsePileInfoVO.setPileLocationInfoVO(pileLocationInfoVO);
                    }
                    evseInfoVO.setEvsePileInfoVO(evsePileInfoVO);
                }
                evseInfoVOList.add(evseInfoVO);
            }
        }
        return evseInfoVOList;
    }

    /**
     * @param tariffGroupIdList
     * @param evseIdList
     * @return
     * @function 根据条件筛选出充电枪信息
     */
    @Override
    public List<OpLocationEvseEntity> findEvseInfoList(List<Long> tariffGroupIdList, List<Long> evseIdList) {

        log.info("===>>>OpLocationEvseServiceImpl.findEvseInfoList tariffGroupIdList : {} and evseIdList : {}", JSON.toJSONString(tariffGroupIdList), JSON.toJSONString(evseIdList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(tariffGroupIdList)
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evseIdList)) {
            return null;
        }
        return opLocationEvseRepository.findEvseInfoList(tariffGroupIdList, evseIdList);
    }

    @Override
    public List<String> getTariffIdListBySn(String sn) {
        int gunCount = opLocationPileEvseRepository.getGunCountBySn(sn);
        log.info("getTariffIdListBySn gunCount:{}", gunCount);
        if (gunCount == 0) {
            return new ArrayList<>();
        }

        List<String> evseSnList = new ArrayList<>();
        for (int i = 1; i <= gunCount; i++) {
            String evseSn = sn + "_" + i;
            evseSnList.add(evseSn);
        }

        log.info("evseSnList :{}", JSON.toJSONString(evseSnList));
        List<String> tariffIdList = opLocationEvseRepository.getTariffIdListByEvseSn(evseSnList);
        log.info("tariffIdList :{}", JSON.toJSONString(tariffIdList));
        if (CollectionUtils.isEmpty(tariffIdList)) {
            return new ArrayList<>();
        }

        return tariffIdList;
    }


    @Override
    public List<OpEvseInfoDTO> getEvseListDeleted(String pileSn) {
        List<OpEvseInfoDTO> resultList = new ArrayList<>();
        OpLocationPileEvseEntity entity = opLocationPileEvseService.findLast(pileSn);
        if (entity != null) {
            String evseList = entity.getEvseList();
            List<Long> ids = JSON.parseArray(evseList, Long.class);
            List<OpLocationEvseEntity> list = opLocationEvseRepository.findByIds(ids);
            if (!CollectionUtils.isEmpty(list)) {
                list.stream().forEach(e -> {
                    OpEvseInfoDTO d = new OpEvseInfoDTO();
                    d.setId(e.getId());
                    d.setEvseSn(e.getEvseSn());
                    resultList.add(d);
                });
            }
        }
        return resultList;
    }

    /**
     * @param locationId
     * @return
     * @function 批量开启某个场站下所有的属性为公开的充电桩的互联互通属性
     */
    @Override
    public boolean batchSetPileEroaming(Long locationId) {

        log.info("===>>>OpLocationEvseServiceImpl.batchSetPileEroaming locationId : {}",
                JSON.toJSONString(locationId));

        return opLocationPileEvseService.batchSetPileEroaming(locationId);
    }

    @Override
    public List<EroamingPileVO> queryEroamingPileListByTariff(Long tariffId) {
        return opLocationService.queryEroamingPileListByTariff(tariffId);
    }

    /**
     * @param pileSN
     * @return {@code OpLocationPileEvseElasticDTO}
     */
    @Override
    public OpLocationPileEvseElasticDTO getPileInfoByPileSN(String pileSN) {
        return opLocationPileEvseRepository.findByPileSn(pileSN);
    }

    @Override
    public IPage<SearchEvseByNameOrSnVO> searchEvseByNameOrSn(SearchEvseByNameOrSnDTO dto,Long sellerId) {
        log.info("searchEvseByNameOrSn,dto={}", JSON.toJSONString(dto));
        Long locationId = dto.getLocationId();
        String keyWord = dto.getKeyWord();
        Integer page = dto.getPage();
        Integer pageSize = dto.getPageSize();
        Long marketingRuleId = dto.getMarketingRuleId();
        IPage<SearchEvseByNameOrSnVO> resultPage = new Page<>(page, pageSize);
        if (ObjectUtils.isEmpty(dto)) {
            return resultPage;
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", sellerId));
        List<Long> locationIds;
        if (!ObjectUtils.isEmpty(locationId)) {
            locationIds = Arrays.asList(locationId);
        } else {
            locationIds = this.opLocationService.getLocationIdBySellerId().getData();
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("searchEvseByNameOrSn,locationIds is empty.");
            return resultPage;
        }
        if (StringUtils.isNotEmpty(keyWord)) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            keyWord = QueryParserBase.escape(keyWord);
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", keyWord)));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyWord)));
            queryBuilder.must(queryBuilder2);
        }
        List<Long> tmpIds = new ArrayList<>();
        //员工校验权限
        if (!LoginUserUtil.isSellerAdmin()) {
            List<Long> data = this.pileUserFeign.getLocationIds().getData();
            if (!CollectionUtils.isEmpty(data)) {
                tmpIds.addAll(data);
            }
            locationIds = locationIds.stream().filter(tmpIds::contains).collect(Collectors.toList());
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("searchEvseByNameOrSn,after locationIds is empty.");
            return resultPage;
        }
        queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));

        //Set<Long> notAdvertisingStationList = opLocationRepository.getAllNotAdvertisingStation(LoginUserUtil.getSellerId());
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withPageable(PageRequest.of(page - 1, pageSize)).build();
        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> searchPage = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(list)) {
            log.info("searchEvseByNameOrSn,list is empty.");
            return resultPage;
        }
        //查询该营销规则已绑定的枪
        List<String> evseSnList = list.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        MarketingRuleSearchDTO paramDto = new MarketingRuleSearchDTO();
        paramDto.setEveSnList(evseSnList);
        paramDto.setMarketingRuleId(marketingRuleId);
        paramDto.setSellerId(sellerId);
        List<MarketingRulePileVO> bindRuleVoList = this.pileUserFeign.findBindList(paramDto).getData();
        List<String> bindEvseSnList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(bindRuleVoList)) {
            bindEvseSnList.addAll(bindRuleVoList.stream().map(e -> e.getSn() + "_" + e.getGunNumber()).collect(Collectors.toList()));
        }
        List<SearchEvseByNameOrSnVO> searchEvseByNameOrSnVOS = new ArrayList<>();
        for (OpLocationEvseElasticDTO evseDto : list) {
            SearchEvseByNameOrSnVO searchEvseByNameOrSnVO = new SearchEvseByNameOrSnVO();
            BeanUtils.copyProperties(evseDto, searchEvseByNameOrSnVO);
            searchEvseByNameOrSnVO.setGunNumber(CommonUtil.getGunNo(evseDto.getEvseSn()).toString());
            if (!CollectionUtils.isEmpty(bindEvseSnList) && bindEvseSnList.contains(evseDto.getEvseSn())) {
                searchEvseByNameOrSnVO.setIsAssociated(true);
            }
            searchEvseByNameOrSnVOS.add(searchEvseByNameOrSnVO);
        }

        resultPage.setRecords(searchEvseByNameOrSnVOS);
        resultPage.setTotal(searchPage.getTotalElements());
        resultPage.setPages(searchPage.getTotalPages());
        return resultPage;
    }


    public Result<List<PileBindTimeVO>> batchQueryPileBindTime(List<String> snList) {
        return Result.ofSucceed(opLocationEvseRepository.batchQueryPileBindTime(snList));
    }

    /**
     * @param pileSnList
     * @return
     * @function 批量根据充电桩序列号集合获取充电枪数据集合
     */
    @Override
    public List<EvseInfoVO> availableEvseList(List<String> pileSnList) {

        log.info("===>>>OpLocationEvseServiceImpl.availableEvseList pileSnList : {}",
                JSON.toJSONString(pileSnList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileSnList)) {
            return null;
        }

        Set<String> pileSnSet = pileSnList
                .stream()
                .filter(org.apache.commons.lang3.StringUtils::isNotBlank)
                .collect(Collectors.toSet());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileSnSet)) {
            return null;
        }

        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByPileSnIn(pileSnSet);
        log.info("============ the OpLocationEvseServiceImpl.availableEvseList about opLocationEvseElasticDTOList: {}", JSON.toJSONString(opLocationEvseElasticDTOList));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseElasticDTOList)) {
            return null;
        }

        List<EvseInfoVO> evseInfoVOList = new ArrayList<>();
        // 查询任一充电桩对应的场站是否第3方平台，若是，则返回第3方平台可用充电枪
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElasticDTOList.stream().filter(temp -> null != temp.getLocationId()).findAny().orElse(null);
        log.info("============ the OpLocationEvseServiceImpl.availableEvseList about opLocationEvseElasticDTO: {}", JSON.toJSONString(opLocationEvseElasticDTO));
        if (null != opLocationEvseElasticDTO) {
            OpLocationElasticDTO opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(opLocationEvseElasticDTO.getLocationId());
            if (OpLocationPlatformEnum.OCPI_EMSP.getCode().equals(opLocationElasticDTO.getPlatform())) {
                log.info("============= the platform is OCPI_EMSP =============");
                opLocationEvseElasticDTOList.forEach(var -> {
                    Result<OpEvseDTO> opEvseDTOResult = this.getEvseInfoByEvseSn(var.getEvseSn());
                    log.info("============ the OpLocationEvseServiceImpl.availableEvseList about opEvseDTOResult: {}", JSON.toJSONString(opEvseDTOResult));
                    String evseStatus = opEvseDTOResult.getData().getState();
                    if (EvseDeviceStatusEnum.AVAILABLE.getName().equalsIgnoreCase(evseStatus)
                            || EvseDeviceStatusEnum.PREPARING.getName().equalsIgnoreCase(evseStatus)) {
                        EvseInfoVO evseInfoVO = new EvseInfoVO();
                        evseInfoVO.setId(var.getId());
                        evseInfoVO.setEvseSn(var.getEvseSn());
                        if (org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileName())
                                && org.apache.commons.lang3.StringUtils.isNotBlank(var.getEvseSn())) {
                            evseInfoVO.setEvseName(var.getPileName() + "_" + var.getEvseSn().split("_")[1]);
                        } else {
                            evseInfoVO.setEvseName(var.getEvseSn());
                        }
                        evseInfoVOList.add(evseInfoVO);
                    }
                });
                return evseInfoVOList;
            }
        }
        opLocationEvseElasticDTOList.forEach(var -> {
            if (org.apache.commons.lang3.StringUtils.isNotBlank(var.getEvseSn())) {
                Result<String> result = monitorFeignClient.queryStatusByEvseSn(var.getEvseSn());

                log.info("===>>>OpLocationEvseServiceImpl.availableEvseList result : {}",
                        JSON.toJSONString(result));

                if (result != null
                        && Integer.valueOf(HttpStatus.HTTP_OK).equals(result.getCode())
                        && (EvseDeviceStatusEnum.AVAILABLE.getName().equalsIgnoreCase(result.getData())
                        || EvseDeviceStatusEnum.PREPARING.getName().equalsIgnoreCase(result.getData()))) {
                    EvseInfoVO evseInfoVO = new EvseInfoVO();
                    evseInfoVO.setId(var.getId());
                    evseInfoVO.setEvseSn(var.getEvseSn());
                    if (org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileName())
                            && org.apache.commons.lang3.StringUtils.isNotBlank(var.getEvseSn())) {
                        evseInfoVO.setEvseName(var.getPileName() + "_" + var.getEvseSn().split("_")[1]);
                    } else {
                        evseInfoVO.setEvseName(var.getEvseSn());
                    }
                    evseInfoVOList.add(evseInfoVO);
                }
            }
        });
        return evseInfoVOList;
    }

    @Override
    public List<OpLocationEvseElasticDTO> getSellerEvseInfo(List<String> pileSnList, Long operatorId) {
        // 查询充电枪ES，获取数据
        return opLocationEvseRepository.getSellerEvseInfo(operatorId, pileSnList);
    }

    @Override
    public Boolean updateEvseLastOrderSeq(String evseSn, Long orderSeq) {
        return opLocationEvseRepository.updateEvseLastOrderSeq(evseSn, orderSeq);
    }

    @Override
    public OpLocationEvseEntity findOneByEvseSn(String evseSn) {
        return opLocationEvseRepository.findOneByEvseSn(evseSn);
    }

    @Override
    public List<String> getAllEvseSnByLocationIds(List<Long> locationIds) {
        return opLocationEvseRepository.getAllEvseSnByLocationIds(locationIds);
    }

    @Override
    public Map<String, Long> queryTariffByEvseSnList(List<String> evseSnList) {
        Map<String, Long> map = new HashMap<>();
        List<OpLocationEvseEntity> opLocationEvseList = opLocationEvseMapper.queryTariffByEvseSnList(evseSnList);
        if(!CollectionUtils.isEmpty(opLocationEvseList)){
           for(OpLocationEvseEntity opLocationEvseEntity : opLocationEvseList){
               map.put(opLocationEvseEntity.getEvseSn(), opLocationEvseEntity.getTariffId());
           }
        }
        return map;
    }

    /**
     * @param tariffGroupIdSet 计费规则组的id集合
     * @function 校验商家是否可以绑定收费的计费规则
     */
    private boolean getSellerBindNotFreeCostModelRuleGroupforGunMark(Set<Long> tariffGroupIdSet) {

        log.info("===>>>OpLocationEvseServiceImpl.getSellerBindNotFreeCostModelRuleGroupforGunMark tariffGroupIdSet : {}",
                JSON.toJSONString(tariffGroupIdSet));

        Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        SellerDetailVO sellerDetailVO = pileBaseCommonService.getSellerDetail(sellerId.toString());
        if (sellerDetailVO != null
                && EmspPlatformEnum.PLANET_EV.getCode().equals(sellerDetailVO.getAssociatedPlatform())) {
            return true;
        }

        // 微信支付新增逻辑start
        Result<Integer> envResult = sellerAccountService.queryPayType();
        if (envResult.getData() != null && PayTypeEnum.WE_CHAT.getValue() == envResult.getData()) {
            // 如果是中国区，判断是否有添加微信商户号, 如果添加了，返回true。
            Result<String> accountResult = sellerAccountService.queryBindWXAccount(null);
            if (accountResult.getData() == null) {
                Result<List<SimpleInformationAboutCostModelRuleVO>> result = tariffFeignClient.querySimpleInformationAboutCostModelRuleVOByIds(new ArrayList<>(tariffGroupIdSet));

                log.info("==========>>>>>>>>>OpLocationEvseServiceImpl.getSellerBindNotFreeCostModelRuleGroupforGunMark result : {}",
                        JSON.toJSONString(result));

                if (result != null
                        && Integer.valueOf(200).equals(result.getCode())
                        && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(result.getData())) {
                    List<SimpleInformationAboutCostModelRuleVO> data = result.getData();
                    for (SimpleInformationAboutCostModelRuleVO datum : data) {
                        Integer ruleModelType = datum.getRuleModelType();
                        if (!RuleModelTypeEnum.FREE.getCode().equals(ruleModelType)) {
                            // 这些计费规则组的种类只要有一个不是免费的，商家就绑不了计费规则，提示其设置收款账户
                            return false;
                        }
                    }
                }
            }
            return true;
        }
        // 微信支付新增逻辑end

        // 获取sellerId
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        // 默认是可以绑定的
        boolean flag = true;

        if (!sellerAccountService.isSellerStripeAccountCharged(payload.getSellerId())) {
            // 如果商家的Stripe账号不能收费，此时获得这些计费规则组的种类
            Result<List<SimpleInformationAboutCostModelRuleVO>> result = tariffFeignClient.querySimpleInformationAboutCostModelRuleVOByIds(new ArrayList<>(tariffGroupIdSet));

            log.info("==========>>>>>>>>>OpLocationEvseServiceImpl.getSellerBindNotFreeCostModelRuleGroupforGunMark result : {}", JSON.toJSONString(result));

            if (result != null
                    && Integer.valueOf(200).equals(result.getCode())
                    && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(result.getData())) {
                List<SimpleInformationAboutCostModelRuleVO> data = result.getData();
                for (SimpleInformationAboutCostModelRuleVO datum : data) {
                    Integer ruleModelType = datum.getRuleModelType();
                    if (!RuleModelTypeEnum.FREE.getCode().equals(ruleModelType)) {
                        // 这些计费规则组的种类只要有一个不是免费的，商家就绑不了计费规则，提示其设置收款账户
                        flag = false;
                        break;
                    }
                }
            }
        }
        return flag;
    }

    /**
     * 美标type 1  SN第二位是L
     * 欧标type2 SN第二位是E
     * 国标G**  SN第二位是G
     *
     * @param pileSn
     * @return
     */
    private Integer getGunType(String pileSn) {
        if (org.apache.commons.lang3.StringUtils.isBlank(pileSn) || pileSn.length() < 2) {
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

    public Integer syncEvseExpandByEvseIds(List<Long> pileIds) {
        if (CollectionUtil.isNotEmpty(pileIds)) {
            List<OpLocationEvseElasticDTO> evselist = opLocationEvseElastic.findAllByIdIn(pileIds);
            if (CollectionUtil.isNotEmpty(evselist)) {
                Set<Long> locationIds = evselist.stream().map(OpLocationEvseElasticDTO::getLocationId).collect(Collectors.toSet());
                List<OpLocationElasticDTO> locationList = opLocationElastic.findAllByIdIn(locationIds);
                if (CollectionUtils.isEmpty(locationList)) {
                    return 0;
                }
                Map<Long, OpLocationElasticDTO> locationMap = locationList.stream().collect(Collectors.toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
                List<OpLocationEvseExpandElasticDTO> list = new ArrayList<>();
                evselist.stream().forEach(evseDto -> {
                    Long locationId = evseDto.getLocationId();
                    OpLocationElasticDTO v = locationMap.get(locationId);
                    OpLocationEvseExpandElasticDTO dto = new OpLocationEvseExpandElasticDTO();
                    // 拷贝场站属性
                    BeanUtils.copyProperties(v, dto);
                    dto.setId(evseDto.getId());
                    dto.setLocationId(locationId);
                    dto.setPower(evseDto.getPower());
                    dto.setGunState(evseDto.getState());
                    dto.setGunType(evseDto.getGunType());
                    dto.setTariffId(evseDto.getTariffId());
                    dto.setSubscriptionCheck(evseDto.getSubscriptionCheck());
                    list.add(dto);
                });
                if (list.size() > 0) {
                    opLocationEvseExpandElasticService.saveAll(list);
                    log.info("syncEvseExpand,svseSize={}", list.size());
                    return list.size();
                }
            }
        }
        return 0;
    }

    public List<OpLocationElasticDTO> getLocationListFromEsOld(List<Long> ids) {
        Iterable<OpLocationElasticDTO> result;
        if (CollectionUtil.isEmpty(ids)) {
            result = opLocationElastic.findAll();
        } else {
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
            queryBuilder.must(QueryBuilders.termsQuery("id", ids));
            result =
//                    opLocationElastic.search(queryBuilder);
                    elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).withTrackTotalHits(true).build(), OpLocationElasticDTO.class)
                            .stream().map(SearchHit::getContent).collect(Collectors.toList());
        }
        Iterator<OpLocationElasticDTO> it = result.iterator();
        List<OpLocationElasticDTO> list = new ArrayList<>();
        while (it.hasNext()) {
            OpLocationElasticDTO next = it.next();
            list.add(next);
        }
        return list;
    }

    public List<OpLocationElasticDTO> getLocationListFromEs(List<Long> ids) {
        List<OpLocationElasticDTO> resultList = new ArrayList<>();

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        if (!CollectionUtil.isEmpty(ids)){
            queryBuilder.must(QueryBuilders.termsQuery("id", ids));
        }
        Iterable<OpLocationElasticDTO> result = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).withTrackTotalHits(true).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        Iterator<OpLocationElasticDTO> it = result.iterator();
        while (it.hasNext()) {
            OpLocationElasticDTO next = it.next();
            resultList.add(next);
        }
        log.info("===>>>getLocationListFromEs2 resultList.size:{}",resultList.size());
        return resultList;
    }

    /**
     * @param id 计费规则模型的主键Id
     * @return 计费规则模型关联的充电设备数量
     * @function 查询计费规则模型关联的充电设备数量
     */
    @Override
    public Integer queryCostRuleAssociatedAndSellerAccount(Long id) {

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.queryCostRuleAssociatedAndSellerAccount id : {}", JSON.toJSONString(id));

        // 计费规则已关联的充电设备的数量
        Integer number = 0;
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(id)) {
            number = opLocationEvseRepository.getEvseNumberByTariffId(id);
        }

        log.info("======>>>>>>>>> OpLocationEvseServiceImpl.queryCostRuleAssociatedAndSellerAccount number : {}", JSON.toJSONString(number));

        return number;
    }

    @Override
    public Result<TariffCountVO> getTariffCountByPileSn(PileEvseTariffDTO pileEvseTariffDTO) {
        return Result.ofSucceed(opLocationEvseRepository.getTariffCountByPileSn(pileEvseTariffDTO));
    }

    @Override
    public Result<Map<String, PileEvseSimpleVO>> getPileEvseInfoByEvseSnList(List<String> evseSnList) {
        if (CollectionUtils.isEmpty(evseSnList)) {
            return null;
        }
        Map<String, PileEvseSimpleVO> result = new HashMap<>();
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = opLocationEvseElastic.findAllByEvseSnIn(evseSnList);
        if (CollectionUtils.isEmpty(opLocationEvseElasticDTOS)) {
            return null;
        }
        opLocationEvseElasticDTOS.forEach(opLocationEvseElasticDTO -> {
            PileEvseSimpleVO pileEvseSimpleVO = new PileEvseSimpleVO();
            pileEvseSimpleVO.setLocationName(opLocationEvseElasticDTO.getLocationName());
            pileEvseSimpleVO.setPileSnName(opLocationEvseElasticDTO.getPileName());
            pileEvseSimpleVO.setGunType(opLocationEvseElasticDTO.getGunType());
            pileEvseSimpleVO.setPower(opLocationEvseElasticDTO.getPower());
            result.put(opLocationEvseElasticDTO.getEvseSn(), pileEvseSimpleVO);
        });
        return Result.ofSucceed(result);
    }

    @Override
    public Result<OpenAndRuleInfoVO> getOpenAndRuleInfoByEvseSn(OpenAndRuleInfoDTO openAndRuleInfoDTO) {
        OpenAndRuleInfoVO openAndRuleInfoVO = new OpenAndRuleInfoVO();

        Optional<OpLocationElasticDTO> locationOptional = opLocationElastic.findById(openAndRuleInfoDTO.getStationId());
        if (locationOptional.isPresent()) {
            OpLocationElasticDTO opLocationElasticDTO = locationOptional.get();
            openAndRuleInfoVO.setOpen(OperationOpenTypeEnum.OPEN.getCode().equals(opLocationElasticDTO.getOpenType()));

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

            openAndRuleInfoVO.setWeekDay(weekDay);
            openAndRuleInfoVO.setHour(hour);
            openAndRuleInfoVO.setMinute(minute);

            GetRuleByEvseSnDTO pullRuleDTO = new GetRuleByEvseSnDTO();
            pullRuleDTO.setLocationId(openAndRuleInfoDTO.getStationId());
            pullRuleDTO.setEvseSn(openAndRuleInfoDTO.getEvseSn());
            pullRuleDTO.setUserId(openAndRuleInfoDTO.getUserId());
            openAndRuleInfoVO.setRuleVO(ruleService.getRuleByEvseSn(pullRuleDTO));
        }

        return Result.ofSucceed(openAndRuleInfoVO);
    }

    @Override
    public Result<TariffRuleOfPileDTO> getZoneIdByPileAndEvse(TariffRuleOfPileDTO tariffRuleOfPileDTO) {
        return Result.ofSucceed(opLocationEvseRepository.getZoneIdByPileAndEvse(tariffRuleOfPileDTO));
    }

    /**
     * @param pileSn
     * @return
     * @function 根据充电桩序列号查询充电设备信息
     */
    @Override
    public List<OpLocationEvseElasticDTO> findListByPileSn(String pileSn) {

        log.info("===>>>OpLocationEvseServiceImpl.findListByPileSn pileSn : {}", JSON.toJSONString(pileSn));

        return opLocationEvseRepository.findListByPileSn(pileSn);
    }

    /**
     * @param opLocationEvseElasticDTO
     * @return
     * @function 保存充电设备信息
     */
    @Override
    public boolean updateEvseInfo(OpLocationEvseElasticDTO opLocationEvseElasticDTO) {
        OpLocationEvseElasticDTO dto = opLocationEvseElastic.save(opLocationEvseElasticDTO);
        return dto != null;
    }

    public void syncLocationPile(Long locationId) {
        if (Objects.isNull(locationId)) {
            return;
        }
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
        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(queryWrapper2);
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationPileEvseEntityList)) {
            List<Long> collect = opLocationPileEvseEntityList.stream().map(OpLocationPileEvseEntity::getId).collect(Collectors.toList());
            syncLocationPileEvse(locationId, collect);
        } else {
            syncLocationPileEvse(locationId, Collections.emptyList());
        }
        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 在es但是没有在MySQL数据库里的数据 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // ↓↓↓↓↓↓↓↓↓↓↓ 在MySQL数据库里的数据 不在es ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

        LambdaQueryWrapper<OpLocationPileEvseEntity> query = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        query.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
        List<String> collect = opLocationPileEvseEntityList.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(collect)) {
            return;
        }
        query.in(OpLocationPileEvseEntity::getPileSn, collect);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(query);

        checkAndSync(opLocationPileEvseEntities);

        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 在MySQL数据库里的数据 不在es ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
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
        log.warn("mysql es 数据不同步 直接删除mysql  op_location_pile_evse 桩数据: {}", JSON.toJSONString(pileEvse));
        sync.compareAndSet(Boolean.FALSE, Boolean.TRUE);
        OpLocationPileEvseEntity entity = opLocationPileEvseMapper.selectById(pileEvse.getId());
        if (Objects.isNull(entity)) {
            return;
        }
        opLocationPileEvseMapper.deleteById(pileEvse.getId());
//        entity.setUpdatedAt(System.currentTimeMillis());
//        entity.setDeleted(1);
//        opLocationPileEvseMapper.updateById(entity);
        String evseList = pileEvse.getEvseList();
        if (org.springframework.util.StringUtils.hasText(evseList)) {
            List<Long> evseIds = JSON.parseArray(evseList, Long.class);
            for (Long evseId : evseIds) {
                log.warn("mysql es 数据不同步 直接删除mysql op_location_evse 枪数据: {}", JSON.toJSONString(pileEvse));
                opLocationEvseMapper.deleteByEvseId(evseId);
                log.warn("mysql es 数据不同步 直接删除 es pile_base_op_location_pile_evse_index 枪数据: {}", JSON.toJSONString(pileEvse));
                opLocationPileEvseElastic.deleteById(evseId);
            }
        }
    }

    private void syncLocationPileEvse(Long locationId, Collection<Long> ids) {
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

    private void syncLocationEvse(Long locationId, Collection<Long> ids) {
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

    @Override
    public PileUploadSaveVO savePileListV2(SavePileListV2DTO savePileListV2DTO) {
        String pileBaseAddLocationKey = RedisKeyConstant.getPileBaseAddLocationKey(savePileListV2DTO.getMerchantId());
        String value = UUID.randomUUID().toString();
        Boolean aBoolean = stringRedisTemplate.opsForValue().setIfAbsent(pileBaseAddLocationKey, value, 600, TimeUnit.SECONDS);
        if (Boolean.FALSE.equals(aBoolean)) {
            log.error("lock by other {}  {}={}", JSON.toJSONString(savePileListV2DTO), pileBaseAddLocationKey, value);
            throw new MessageCodeException("response.failure.message");// 产品 蔺成成 说给操作失败
        }
        try {
            return doSavePileListV2(savePileListV2DTO);
        } catch (Exception e) {
            log.error("doSavePileListV2 failed param: " + JSON.toJSONString(savePileListV2DTO), e);
            throw e;
        } finally {
            try {
                syncLocationPile(savePileListV2DTO.getStationId());
            } catch (Exception e) {
                log.error("syncLocationPile failed", e);
            }
            stringRedisTemplate.delete(pileBaseAddLocationKey);
        }
    }

    public PileUploadSaveVO doSavePileListV2(SavePileListV2DTO savePileListV2DTO) {
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(savePileListV2DTO)
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(savePileListV2DTO.getStationId())
                || org.apache.commons.collections4.CollectionUtils.isEmpty(savePileListV2DTO.getPileDTOList())) {
            log.info("场站id或者待添加的桩为空 {}", JSON.toJSONString(savePileListV2DTO));
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED);
        }
        //待添加的桩
        List<BindPileDTO> savePileList = savePileListV2DTO.getPileDTOList();
        List<AddDataReqDTO> addNodeList = new ArrayList<>();
        //记录删除失败的桩
//        List<BindPileDTO> deletedFailedPile = new ArrayList<>();
        //先解绑已绑定场站的桩    q前端 pileDTOList  只有 sn 所以这里永远不会走进 去掉
//        List<BindPileDTO> bindLocationList = savePileListV2DTO.getPileDTOList().stream().filter(m -> !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(m.getBindLocationId())).collect(Collectors.toList());
//        for (BindPileDTO bindPileDTO : bindLocationList) {
//            Result<Boolean> result = opLocationEvseImpl.deleteByPileSn(bindPileDTO.getPileSn(), true);
//            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result) || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData()) || result.getCode() != 200 || !result.getData()) {
//                deletedFailedPile.add(bindPileDTO);
//            }
//        }
//        if (!org.apache.commons.collections4.CollectionUtils.isEmpty(deletedFailedPile)) {
//            savePileList.removeAll(deletedFailedPile);
//        }
//        if (org.apache.commons.collections4.CollectionUtils.isEmpty(savePileList)) {
//            return null;
//        }
        List<String> toAddPileSNs = savePileList.stream().map(BindPileDTO::getPileSn).collect(Collectors.toList());

        LambdaQueryWrapper<OpLocationPileEvseEntity> query = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        query.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
        query.in(OpLocationPileEvseEntity::getPileSn, toAddPileSNs);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(query);
        List<String> joinLocationPiles = opLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(joinLocationPiles)) {
            log.error("joinLocationPiles {}", JSON.toJSONString(joinLocationPiles));
            throw new MessageCodeException(SN_USED_BY_SOME_LOCATION, new Object[]{JSON.toJSONString(joinLocationPiles)});
        }

        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .eq(ChargePointMerchantRelationEntity::getMerchantId, savePileListV2DTO.getMerchantId())
                .in(ChargePointMerchantRelationEntity::getSn, toAddPileSNs));
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointEntities)) {
            log.error("sn not belong current merchant {}", JSON.toJSONString(toAddPileSNs));
            throw new MessageCodeException(PileBaseEnum.SN_NOT_REGISTER);
        }
        Map<String, ChargePointMerchantRelationEntity> chargePointEntityMap = chargePointEntities.stream().collect(Collectors.toMap(ChargePointMerchantRelationEntity::getSn, e -> e, (e1, e2) -> e2));
        if (toAddPileSNs.size() != chargePointEntityMap.size()) {
            toAddPileSNs.removeAll(chargePointEntityMap.keySet());
            log.error("some pile sn not belong current merchant {}", JSON.toJSONString(toAddPileSNs));
            throw new MessageCodeException(PileBaseEnum.SN_NOT_REGISTER);
        }

        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓根据SN 从统一桩管理里面补充桩信息 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        PileUploadSaveVO pileUploadSaveVO = null;
        try {
            List<OpLocationEvseDTO> opLocationEvseDTOs = new ArrayList<>();
            savePileList.forEach(m -> {
                if (!org.apache.commons.lang3.StringUtils.isBlank(m.getPileSn())) {
                    ChargePointMerchantRelationEntity chargePointEntity = chargePointEntityMap.get(m.getPileSn());
                    OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
                    BeanUtils.copyProperties(chargePointEntity, opLocationEvseDTO);
                    opLocationEvseDTO.setLocationId(savePileListV2DTO.getStationId());
                    opLocationEvseDTO.setPhase(chargePointEntity.getPhases());
                    opLocationEvseDTO.setPileSN(chargePointEntity.getSn());
                    opLocationEvseDTO.setPublicMark(savePileListV2DTO.getPublicMark());
                    opLocationEvseDTO.setPileName(chargePointEntity.getName());
                    opLocationEvseDTO.setPinCode(chargePointEntity.getPin());
                    opLocationEvseDTO.setProductModel(chargePointEntity.getPartProduct());
                    if (!org.apache.commons.lang3.StringUtils.isBlank(chargePointEntity.getBrandName()) && BaseConstant.AUTEL.equalsIgnoreCase(chargePointEntity.getBrandName())) {
                        opLocationEvseDTO.setThirdPart(0);
                    } else {
                        opLocationEvseDTO.setThirdPart(1);
                    }
                    if (!org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointEntity.getConnectors())) {
                        log.info("savePileListV2,connectors={}", JSON.toJSONString(chargePointEntity.getConnectors()));
                        List<Connector> array = JSON.parseArray(chargePointEntity.getConnectors().toString(), Connector.class);
                        List<OpLocationConnectorDTO> opLocationConnectorDTOs = new ArrayList<>();
                        for (Connector connector : array) {
                            OpLocationConnectorDTO dto = new OpLocationConnectorDTO();
                            BeanUtils.copyProperties(connector, dto);
                            if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(connector.getPower())) {
                                dto.setPower(connector.getPower().doubleValue());

                            }
                            dto.setConnectorId(connector.getConnectorId().toString());
                            dto.setGunType(connector.getConnectorType());
                            opLocationConnectorDTOs.add(dto);
                        }
                        opLocationEvseDTO.setPower(chargePointEntity.getRatedPower());
                        List<OpLocationConnectorDTO> collect = opLocationConnectorDTOs.stream().sorted(Comparator.comparing(OpLocationConnectorDTO::getConnectorId)).collect(Collectors.toList());
                        opLocationEvseDTO.setOpLocationConnectorDTOs(collect);
                    }
                    opLocationEvseDTOs.add(opLocationEvseDTO);
                }
            });
            log.info("从统一桩管理批量添加桩，opLocationEvseDTOs={}", JSON.toJSONString(opLocationEvseDTOs));
            Result<List<PileVO>> evse = opLocationEvseServiceImpl.createEvse(opLocationEvseDTOs);
            log.info("从统一桩管理批量添加桩，evse={}", JSON.toJSONString(evse));
            chargePointMerchantRelationFacade.bindOwnerRelation(savePileListV2DTO.getMerchantId(), evse.getData().stream().map(PileVO::getPileSN).collect(Collectors.toSet()));
            pileUploadSaveVO = new PileUploadSaveVO();
            if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evse) && evse.getCode() == 200 && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evse.getData())) {
                pileUploadSaveVO.setSuccessNum(evse.getData().size());
                pileUploadSaveVO.setSaveSuccessPileInfoVOList(PileInfoConvert.buildSavePileInfoVOList(evse.getData()));
            }
            if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(savePileListV2DTO) && !org.apache.commons.collections4.CollectionUtils.isEmpty(savePileListV2DTO.getPileDTOList())
                    && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evse) && evse.getCode() == 200 && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evse.getData())) {
                pileUploadSaveVO.setFailureNum(savePileListV2DTO.getPileDTOList().size() - evse.getData().size());
            }
            //同步数据到基础服务
            if (!opLocationEvseDTOs.isEmpty()) {
                Map<Long, Map<String, String>> tmpMap = opLocationEvseDTOs.stream().collect(Collectors.groupingBy(OpLocationEvseDTO::getLocationId, Collectors.mapping(e -> e, Collectors.toMap(OpLocationEvseDTO::getPileSN, OpLocationEvseDTO::getPileName))));
                tmpMap.forEach((locationId, tmp) -> {
                    tmp.forEach((pileSn, pileName) -> {
                        AddDataReqDTO addDataReqDTO = new AddDataReqDTO();
                        addDataReqDTO.setNodeCode(pileSn);
                        addDataReqDTO.setNodeLevel(SubTreeEnum.PILE.getCode());
                        addDataReqDTO.setNodeName(pileName);
                        addDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                        addDataReqDTO.setParentNodeCode(locationId.toString());
                        addDataReqDTO.setParentNodeLevel(SubTreeEnum.STATION.getCode());
                        addNodeList.add(addDataReqDTO);
                    });
                });
            }
            Result<Boolean> result = saasAccessFeignClient.addNde(addNodeList);
            log.info("添加桩修改权限树,result={}", JSON.toJSONString(result));
            if (ObjectUtils.isEmpty(result) || ObjectUtils.isEmpty(result.getData()) || Boolean.TRUE.equals(!result.getData())) {
                throw new MessageCodeException(PileBaseEnum.ADD_DATA_ERROR);
            }
        } catch (Exception e) {
            if (!addNodeList.isEmpty()) {
                List<DeleteDataReqDTO> dataReqDTOList = new ArrayList<>();
                addNodeList.stream().forEach(node -> {
                    DeleteDataReqDTO deleteDataReqDTO = new DeleteDataReqDTO();
                    deleteDataReqDTO.setNodeCode(node.getNodeCode());
                    deleteDataReqDTO.setNodeLevel(SubTreeEnum.PILE.getCode());
                    deleteDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                    dataReqDTOList.add(deleteDataReqDTO);
                });
                saasAccessFeignClient.deleteNde(dataReqDTOList);
                throw new MessageCodeException(PileBaseEnum.OPERATOR_DATA_ERROR);// 产品 蔺成成 说给操作失败
            }
            throw e;
        }
        return pileUploadSaveVO;
    }


    @Override
    public PileUploadSaveVO savePileListV3(SavePileListV3DTO savePileListV2DTO) {
        String pileBaseAddLocationKey = RedisKeyConstant.getPileBaseAddLocationKey(savePileListV2DTO.getMerchantId());
        String value = UUID.randomUUID().toString();
        Boolean aBoolean = stringRedisTemplate.opsForValue().setIfAbsent(pileBaseAddLocationKey, value, 600, TimeUnit.SECONDS);
        if (Boolean.FALSE.equals(aBoolean)) {
            log.error("lock by other {}  {}={}", JSON.toJSONString(savePileListV2DTO), pileBaseAddLocationKey, value);
            throw new MessageCodeException("response.failure.message");// 产品 蔺成成 说给操作失败
        }
        try {
            return doSavePileListV3(savePileListV2DTO);
        } catch (Exception e) {
            log.error("savePileListV3 failed param: " + JSON.toJSONString(savePileListV2DTO), e);
            throw e;
        } finally {
            try {
                syncLocationPile(savePileListV2DTO.getStationId());
            } catch (Exception e) {
                log.error("syncLocationPile failed", e);
            }
            stringRedisTemplate.delete(pileBaseAddLocationKey);
        }
    }

    /**
     * 优化
     * <p>
     * {"pileDTOList":[{"pileSn":"A2121"}],"publicMark":1,"stationId":"1608703060025745620"}
     */
    public PileUploadSaveVO doSavePileListV3(SavePileListV3DTO savePileListV2DTO) {
        PileUploadSaveVO pileUploadSaveVO = new PileUploadSaveVO();
        StopWatch stopWatch = new StopWatch("场站批量添加桩");
        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓校验↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        stopWatch.start("校验参数");
        //校验场站是否存在
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(savePileListV2DTO.getStationId());
        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(savePileListV2DTO)
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(savePileListV2DTO.getStationId())
                || org.apache.commons.collections4.CollectionUtils.isEmpty(savePileListV2DTO.getPileDTOList())) {
            log.info("场站id或者待添加的桩为空 {}", JSON.toJSONString(savePileListV2DTO));
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED);
        }
        //待添加的桩
        List<AddPileDTO> savePileList = savePileListV2DTO.getPileDTOList();
        Set<String> toAddPileSNs = savePileList.stream().map(AddPileDTO::getPileSn).collect(Collectors.toSet());
        Map<String, Boolean> stringBooleanMap = opLocationPileEvseService.judgeBritainStand(new ArrayList<>(toAddPileSNs));
        log.info("OpLocationEvseRepositoryImpl.stringBooleanMap : {}", JSON.toJSONString(stringBooleanMap));
        // 校验这些桩是否是当前商家的资产 桩
        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .eq(ChargePointMerchantRelationEntity::getMerchantId, savePileListV2DTO.getMerchantId())
                .in(ChargePointMerchantRelationEntity::getSn, toAddPileSNs));
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointEntities)) {
            log.error("sn not belong current merchant {}", JSON.toJSONString(toAddPileSNs));
            throw new MessageCodeException(PileBaseEnum.SN_NOT_REGISTER);
        }

        Map<String, ChargePointMerchantRelationEntity> chargePointEntityMap = chargePointEntities.stream().collect(Collectors.toMap(ChargePointMerchantRelationEntity::getSn, e -> e, (e1, e2) -> e2));
        if (toAddPileSNs.size() != chargePointEntityMap.size()) {
            toAddPileSNs.removeAll(chargePointEntityMap.keySet());
            log.error("some pile sn not belong current merchant {}", JSON.toJSONString(toAddPileSNs));
            throw new MessageCodeException(PileBaseEnum.SN_NOT_REGISTER);
        }

        // 这些桩有没有被其他商家绑定
        LambdaQueryWrapper<OpLocationPileEvseEntity> query = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        query.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
        query.in(OpLocationPileEvseEntity::getPileSn, toAddPileSNs);
        List<OpLocationPileEvseEntity> existOpLocationPileEvseEntities = opLocationPileEvseMapper.selectList(query);
        List<String> joinLocationPiles = existOpLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(joinLocationPiles)) {
            log.error("joinLocationPiles {}", JSON.toJSONString(joinLocationPiles));
            throw new MessageCodeException(SN_USED_BY_SOME_LOCATION, new Object[]{JSON.toJSONString(joinLocationPiles)});
        }

        stopWatch.stop();
        stopWatch.start("查询桩设备信息");

        Map<String, ChargePileDTO> pileDTOMap = new HashMap<>();
        List<String> autelPileSNs = chargePointEntities.stream().filter(e -> e.getBrandId() == 1).map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(autelPileSNs)) {
            List<ChargePileDTO> chargePileDTOS = deviceServiceFeign.queryPileList(autelPileSNs).getData();
            pileDTOMap.putAll(chargePileDTOS.stream().collect(Collectors.toMap(ChargePileDTO::getSn, Function.identity())));
        }

        stopWatch.stop();
        stopWatch.start("构造实例对象");


        List<OpLocationEvseDTO> autelOpLocationEvseDTOs = new ArrayList<>();// Autel桩
        List<OpLocationEvseDTO> thirdOpLocationEvseDTOs = new ArrayList<>();// 第三方桩


        // ↓↓↓↓↓↓↓↓↓↓↓↓ 批量操作集合 ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        List<OpLocationEvseDTO> opLocationEvseDTOs = new ArrayList<>();  // 根据SN 从统一桩管理里面补充桩信息done
        List<OpLocationEvseEntity> opLocationEvseEntities = new ArrayList<>();// 构造场站充电设备 以便下面批量保存  saved
        List<OpLocationConnectorEntity> opLocationConnectorEntities = new ArrayList<>();// 构造场站充电枪 以便下面批量保存  saved
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = new ArrayList<>();// 构造场站充电设备ES 以便下面批量保存  saved
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = new ArrayList<>();// 构造场站充电设备ES 以便下面批量保存 saved
        List<Map<String, String>> ocppParamList = new ArrayList<>();// ocpp 以便下面批量发送MQ  done
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOs = new ArrayList<>();// 构造场站充电设备ES 以便下面批量保存 saved
        List<SetPileEroamingDTO> setPileEroamingDTOList = new ArrayList<>();// 异步推送  done



        List<OpLocationPileForAdvVO> pileVoList = new ArrayList<>(1);


        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ 批量操作集合 ↑↑↑↑↑↑↑↑↑

        log.info("start build object {}", JSON.toJSONString(savePileList));
        savePileList.forEach(m -> {
            log.info("build object {}", JSON.toJSONString(m));
            ArrayList<Long> evseIds = new ArrayList<>();
            AtomicBoolean subscriptionCheck = new AtomicBoolean(isSubscriptionCheck(m.getPileSn()));
            ChargePointMerchantRelationEntity chargePointEntity = chargePointEntityMap.get(m.getPileSn());

            deviceNoticeAdsService(opLocationEntity, pileVoList, chargePointEntity);

            OpLocationEvseDTO opLocationEvseDTO = ChargePointMerchantRelationTypeMapper.INSTANCE.entityMapOpLocationEvseDTO(chargePointEntity);
            opLocationEvseDTOs.add(opLocationEvseDTO);
            opLocationEvseDTO.setLocationId(savePileListV2DTO.getStationId());
            opLocationEvseDTO.setPhase(chargePointEntity.getPhases());
            opLocationEvseDTO.setPileSN(chargePointEntity.getSn().toUpperCase());
            opLocationEvseDTO.setPublicMark(savePileListV2DTO.getPublicMark());
            opLocationEvseDTO.setPileName(chargePointEntity.getName());
            opLocationEvseDTO.setPinCode(chargePointEntity.getPin());
            opLocationEvseDTO.setProductModel(chargePointEntity.getPartProduct());
            opLocationEvseDTO.setPowerType(chargePointEntity.getPowerType());
            opLocationEvseDTO.setThirdPart(chargePointEntity.getBrandId() == 1 ? 0 : 1);
            opLocationEvseDTO.setPower(chargePointEntity.getRatedPower());
            // 根据品牌桩进一步细致校验
            validByBrand(opLocationEvseDTO);
            // 构造枪数据
            if (!org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointEntity.getConnectors())) {
                log.info("savePileListV3,connectors={}", JSON.toJSONString(chargePointEntity.getConnectors()));
                List<Connector> array = JSON.parseArray(chargePointEntity.getConnectors().toString(), Connector.class);
                List<OpLocationConnectorDTO> opLocationConnectorDTOs = new ArrayList<>();
                for (Connector connector : array) {
                    OpLocationConnectorDTO dto = ChargePointMerchantRelationTypeMapper.INSTANCE.toConnectorDTO(connector);
                    if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(connector.getPower())) {
                        dto.setPower(connector.getPower().doubleValue());
                    }
                    dto.setConnectorId(connector.getConnectorId().toString());
                    dto.setGunType(connector.getConnectorType());
                    opLocationConnectorDTOs.add(dto);
                }

                List<OpLocationConnectorDTO> collect = opLocationConnectorDTOs.stream().sorted(Comparator.comparing(OpLocationConnectorDTO::getConnectorId)).collect(Collectors.toList());
                opLocationEvseDTO.setOpLocationConnectorDTOs(collect);
            }

            if (opLocationEvseDTO.getThirdPart() == 0) {
                autelOpLocationEvseDTOs.add(opLocationEvseDTO);
                if (pileDTOMap.containsKey(opLocationEvseDTO.getPileSN())) {
                    ChargePileDTO chargePileDTO = pileDTOMap.get(opLocationEvseDTO.getPileSN());
                    //判断是否是家桩共享的桩，如果是的话，需要把订阅开关设置为true
                    if (DeviceTypeEnum.SHARED_HOME_PILE.getValue().equals(chargePileDTO.getUsageScenario())) {
                        subscriptionCheck.set(true);
                    }
                }
            } else {
                thirdOpLocationEvseDTOs.add(opLocationEvseDTO);
            }
            opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(e -> {
                // autel 桩和  非autel 桩的处理逻辑
                if (opLocationEvseDTO.getThirdPart() == 0) {
                    if (pileDTOMap.containsKey(opLocationEvseDTO.getPileSN())) {
                        ChargePileDTO chargePileDTO = pileDTOMap.get(opLocationEvseDTO.getPileSN());
                        e.setAmperage(new BigDecimal(String.valueOf(chargePileDTO.getElectricCurrent() == null ? 0.0 : chargePileDTO.getElectricCurrent())));
                        e.setVoltage(new BigDecimal(String.valueOf(chargePileDTO.getVoltage() == null ? 0.0 : chargePileDTO.getVoltage())));
                        e.setPower(chargePileDTO.getOutputPower() == null ? 0.0 : chargePileDTO.getOutputPower());
                        e.setPowerType(handlePowerType(chargePileDTO.getCategory(), chargePileDTO.getPhase()));
                    }
                } else {
                    LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
                    queryWrapper.eq(OpEvseBrandModelEntity::getBrandName, opLocationEvseDTO.getBrandName())
                            .eq(OpEvseBrandModelEntity::getProductModel, opLocationEvseDTO.getProductModel())
                            .eq(OpEvseBrandModelEntity::getDeleted, 0);
                    OpEvseBrandModelEntity opEvseBrandModelEntity = opEvseBrandModelMapper.selectOne(queryWrapper);
                    e.setPower(opLocationEvseDTO.getPower() == null ? 0.0 : opLocationEvseDTO.getPower());
                    e.setPowerType(opLocationEvseDTO.getPowerType());
                    if (opEvseBrandModelEntity != null) {
                        e.setAmperage(opEvseBrandModelEntity.getAmperage());
                        e.setVoltage(opEvseBrandModelEntity.getVoltage());
                    }
                    e.setAmperage(Objects.nonNull(e.getAmperage()) ? e.getAmperage() : BigDecimal.ZERO);
                    e.setVoltage(Objects.nonNull(e.getVoltage()) ? e.getVoltage() : BigDecimal.ZERO);
                }
                // autel 桩和  非autel 桩的处理逻辑

                // 充电设备构造
                OpLocationEvseEntity opLocationEvseEntity = new OpLocationEvseEntity();
                opLocationEvseEntity.setId(IdWorker.getId());
                opLocationEvseEntity.setDeleted(0);
                opLocationEvseEntity.setCreatedAt(System.currentTimeMillis());
                opLocationEvseEntity.setUpdatedAt(System.currentTimeMillis());
                opLocationEvseEntity.setLocationId(opLocationEvseDTO.getLocationId());
                opLocationEvseEntity.setPinCode(opLocationEvseDTO.getPinCode());
                opLocationEvseEntity.setEvseSn(handlerEvseSn(e.getConnectorId(), opLocationEvseDTO.getPileSN()));
                opLocationEvseEntity.setStatus(0);
                opLocationEvseEntity.setState("");
                opLocationEvseEntity.setEvseId("");
                opLocationEvseEntity.setLongitude("");
                opLocationEvseEntity.setLatitude("");
                opLocationEvseEntity.setLatitude("");
                log.info("build opLocationEvseEntity {}", JSON.toJSONString(opLocationEvseEntity));
                opLocationEvseEntities.add(opLocationEvseEntity);
                evseIds.add(opLocationEvseEntity.getId());

                //添加连接器
                e.setLocationEvseId(opLocationEvseEntity.getId());
                OpLocationConnectorEntity opLocationConnectorEntity = OpLocationConnectorConvert.toOpLocationConnectorEntity(e);
                if (opLocationEvseDTO.getPower() != null) {
                    opLocationConnectorEntity.setPower(opLocationEvseDTO.getPower());
                }
                opLocationConnectorEntity.setPowerType(Optional.ofNullable(opLocationEvseDTO.getPhase()).orElse(opLocationEvseDTO.getPowerType()));
                if (StringUtils.isBlank(opLocationConnectorEntity.getPowerType())) {
                    opLocationConnectorEntity.setPowerType("");
                }
                opLocationConnectorEntity.setStandard("");
                opLocationConnectorEntity.setFormat("");
                opLocationConnectorEntity.setTermsAndConditions("");
                log.info("build opLocationConnectorEntity {}", JSON.toJSONString(opLocationConnectorEntity));
                opLocationConnectorEntities.add(opLocationConnectorEntity);

                //  充电设备 + 连接器 构造 es充电设备连接器对象
                OpLocationEvseElasticDTO evseElasticDTO = OpLocationEvseConvert.toOpLocationEvseElastic(null,opLocationEvseEntity, opLocationEntity, opLocationConnectorEntity);
                evseElasticDTO.setBrandId(opLocationEvseDTO.getBrandId());
                evseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
                evseElasticDTO.setPileSn(opLocationEvseDTO.getPileSN());
                evseElasticDTO.setPileName(opLocationEvseDTO.getPileName());
                evseElasticDTO.setState(monitorFeign.getEvseStatus(opLocationEvseEntity.getEvseSn()).getName());
                evseElasticDTO.setSubscriptionCheck(subscriptionCheck.get());
                log.info("build evseElasticDTO   {}", JSON.toJSONString(evseElasticDTO));
                opLocationEvseElasticDTOS.add(evseElasticDTO);
            });
            // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑  Autel & thirds  分支处理逻辑 ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

            OpLocationPileEvseEntity opLocationPileEvseEntity = OpLocationPileEvseConvert.toOpLocationPileEvseEntity(opLocationEvseDTO);
            opLocationPileEvseEntity.setEvseList(JSON.toJSONString(evseIds));
            opLocationPileEvseEntity.setId(IdWorker.getId());
            opLocationPileEvseEntity.setDeleted(0);
            opLocationPileEvseEntity.setStatus(0);
            if (opLocationEntity.getHubjectCheck() != null
                    && opLocationEntity.getHubjectCheck()
                    && Integer.valueOf(1).equals(opLocationPileEvseEntity.getPublicMark())) {
                opLocationPileEvseEntity.setEroamingEnable(1);
            } else {
                opLocationPileEvseEntity.setEroamingEnable(0);
            }
            Boolean isBritainStandPileFlag = Boolean.FALSE;
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(stringBooleanMap)) {
                Boolean flag = stringBooleanMap.get(opLocationEvseDTO.getPileSN());
                if (flag != null && flag) {
                    // 英标桩需要设置默认充电时间模板
                    isBritainStandPileFlag = Boolean.TRUE;
                    opLocationPileEvseEntity.setDefaultChargingTime(TariffUtil.defaultChargingTemplate());
                }
            }
            log.info("build opLocationPileEvseEntity : {}", JSON.toJSONString(opLocationPileEvseEntity));
            opLocationPileEvseEntities.add(opLocationPileEvseEntity);

            Map<String, String> ocppParam = new HashMap<>();
            ocppParam.put("sn", opLocationEvseDTO.getPileSN());
            ocppParam.put("bindOperatorId", String.valueOf(LoginUserHolder.getLoginUser().getId()));
            ocppParam.put("locationId", opLocationPileEvseEntity.getLocationId().toString());
            ocppParamList.add(ocppParam);

            //同步桩数据到es
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = OpLocationPileEvseConvert
                    .toOpLocationPileEvseElastic(opLocationPileEvseEntity, opLocationEntity);
            opLocationPileEvseElasticDTO.setPower(opLocationEvseDTO.getPower());
            opLocationPileEvseElasticDTO.setPowerType(Optional.ofNullable(opLocationEvseDTO.getPhase()).orElse(opLocationEvseDTO.getPowerType()));
            if (StringUtils.isBlank(opLocationPileEvseElasticDTO.getPowerType())) {
                opLocationPileEvseElasticDTO.setPowerType("");
            }
            opLocationPileEvseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
            opLocationPileEvseElasticDTO.setPublicMark(opLocationEvseDTO.getPublicMark());
            opLocationPileEvseElasticDTO.setEroamingEnable(opLocationPileEvseEntity.getEroamingEnable());
            if (isBritainStandPileFlag) {
                // 英标桩需要设置默认充电时间模板
                opLocationPileEvseElasticDTO.setBritainStandPileMark(Boolean.TRUE);
                opLocationPileEvseElasticDTO.setDefaultChargingTime(TariffUtil.defaultChargingTemplate());
            } else {
                opLocationPileEvseElasticDTO.setBritainStandPileMark(Boolean.FALSE);
            }
            // 处理桩侧上报的英国法案数据
            this.handleEvscpSettingDTO(opLocationPileEvseElasticDTO);
            log.info("build opLocationPileEvseElasticDTO : {}", JSON.toJSONString(opLocationPileEvseElasticDTO));
            opLocationPileEvseElasticDTOs.add(opLocationPileEvseElasticDTO);


            if (Integer.valueOf(1).equals(opLocationPileEvseEntity.getEroamingEnable())) {
                // 异步推送
                SetPileEroamingDTO setPileEroamingDTO = new SetPileEroamingDTO();
                setPileEroamingDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
                setPileEroamingDTO.setPileEroamingOperateType(1);
                setPileEroamingDTOList.add(setPileEroamingDTO);
            }

        });
        log.info("============end build object================");

        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑校验↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        stopWatch.stop();
        stopWatch.start("批量保存");

        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓开始批量保存↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        Assert.notEmpty(opLocationEvseEntities, "opLocationEvseEntities is empty!");
        log.info("opLocationEvseEntities {}", JSON.toJSONString(opLocationEvseEntities));

        Assert.notEmpty(opLocationConnectorEntities, "opLocationConnectorEntities is empty!");
        log.info("opLocationConnectorEntities {}", JSON.toJSONString(opLocationConnectorEntities));

        Assert.notEmpty(opLocationPileEvseEntities, "opLocationPileEvseEntities is empty!");
        log.info("opLocationPileEvseEntities {}", JSON.toJSONString(opLocationPileEvseEntities));

        Assert.notEmpty(opLocationEvseElasticDTOS, "opLocationEvseElasticDTOS is empty!");
        log.info("opLocationEvseElasticDTOS {}", JSON.toJSONString(opLocationEvseElasticDTOS));

        Assert.notEmpty(opLocationPileEvseElasticDTOs, "opLocationPileEvseElasticDTOs is empty!");
        log.info("opLocationPileEvseElasticDTOs {}", JSON.toJSONString(opLocationPileEvseElasticDTOs));

        // 保存顺序是 mysql feign  es
        transactionTemplate.execute(transactionStatus -> {
            try {
                log.info("============开始批量保存================");
                // ↓↓↓ mysql ↓↓↓
                Assert.isTrue(opLocationEvseMapper.batchSave(opLocationEvseEntities) == opLocationEvseEntities.size(), "batch save op_location_evse failed");
                // 连接器保存
                Assert.isTrue(opLocationConnectorMapper.batchSave(opLocationConnectorEntities) == opLocationConnectorEntities.size(), "batch save op_location_connector failed");

                Assert.isTrue(opLocationPileEvseMapper.batchSave(opLocationPileEvseEntities) == opLocationPileEvseEntities.size(), "batch save op_location_pile_evse failed");

                List<AddDataReqDTO> addNodeList = new ArrayList<>();
                for (OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntities) {
                    AddDataReqDTO addDataReqDTO = new AddDataReqDTO();
                    addDataReqDTO.setNodeCode(opLocationPileEvseEntity.getPileSn());
                    addDataReqDTO.setNodeLevel(SubTreeEnum.PILE.getCode());
                    addDataReqDTO.setNodeName(opLocationPileEvseEntity.getName());
                    addDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                    addDataReqDTO.setParentNodeCode(opLocationPileEvseEntity.getLocationId().toString());
                    addDataReqDTO.setParentNodeLevel(SubTreeEnum.STATION.getCode());
                    addNodeList.add(addDataReqDTO);
                }
                Result<Boolean> result = saasAccessFeignClient.addNde(addNodeList);
                log.info("添加桩修改权限树,result={}",JSON.toJSONString(result));
                OpLocationElasticDTO elasticDTO = null;
                if (ObjectUtils.isEmpty(result) || ObjectUtils.isEmpty(result.getData()) || Boolean.TRUE.equals(!result.getData())) {
                    throw new MessageCodeException("_OIpNNZTOPbxI");
                }
                // 更新关系
                Assert.isTrue(chargePointMerchantRelationFacade.bindOwnerRelation(savePileListV2DTO.getMerchantId(), toAddPileSNs) == toAddPileSNs.size(), "batch save bindOwnerRelation failed");

                // ↑↑↑↑↑↑↑ mysql  ↑↑↑↑↑↑↑↑↑↑
                // ↓↓↓ feign ↓↓↓
                // 第三方桩保存到device --- 统一桩管理已经保存了 这里有点多余
                if (!pileDeviceServiceAdapter.batchSaveThirdPile(thirdOpLocationEvseDTOs)) {
                    log.error("添加第三方桩到device 失败 {}", JSON.toJSONString(thirdOpLocationEvseDTOs));
                    throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
                }
                // ↑↑↑↑↑↑↑ feign  ↑↑↑↑↑↑↑↑↑↑

                // ↓↓↓ es ↓↓↓
                opLocationPileEvseElastic.saveAll(opLocationPileEvseElasticDTOs);// pile_base_op_location_pile_evse_index

                opLocationEvseElastic.saveAll(opLocationEvseElasticDTOS); // pile_base_op_location_evse_index
                //同步到设备扩展类
//                    ThreadPoolUtil.getExecutor().execute(() -> {
                List<Long> pileIds = opLocationEvseElasticDTOS.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
                log.info("pileIds= {}", JSON.toJSONString(pileIds));
                this.syncEvseExpand(EvseExpandDTO.builder().pileIds(pileIds).build());
//                    });
                // ↑↑↑↑↑↑↑ es  ↑↑↑↑↑↑↑↑↑↑
                log.info("============结束批量保存================");
                return Boolean.TRUE;
            } catch (Exception e) {
                log.error("batch save failed because:", e);
                transactionStatus.setRollbackOnly();
                List<DeleteDataReqDTO> dataReqDTOList = new ArrayList<>();
                for (OpLocationPileEvseEntity entity : opLocationPileEvseEntities) {
                    DeleteDataReqDTO deleteDataReqDTO = new DeleteDataReqDTO();
                    deleteDataReqDTO.setNodeCode(entity.getPileSn());
                    deleteDataReqDTO.setNodeLevel(SubTreeEnum.PILE.getCode());
                    deleteDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                    dataReqDTOList.add(deleteDataReqDTO);
                }
                saasAccessFeignClient.deleteNde(dataReqDTOList);
                throw new MessageCodeException("response.failure.message");// 产品 蔺成成 说给操作失败
            }
        });

        stopWatch.stop();

        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑完成批量保存↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        // ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓保存完毕之后其他处理↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
        stopWatch.start("保存完毕之后其他处理");

        for (String sn : toAddPileSNs) {
            // 缓存桩运营商用户
            String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(sn);
            try {
                stringRedisTemplate.opsForValue().set(snOperatorIdKey, savePileListV2DTO.getMerchantId().toString());
            } catch (Exception e) {
                log.error("set cache failed key=" + snOperatorIdKey, e);
            }
        }

        if (!CollectionUtils.isEmpty(ocppParamList)) {
            for (Map<String, String> ocppParam : ocppParamList) {
                try {
                    rabbitTemplate.convertAndSend("DIRECT_EXCHANGE_PILE_ACTIVATION_BIND_AUTEL" + RabbitBean.RABBITMQ_VERSION_SUFFIX, "PILE.ACTIVATION.BIND_AUTEL", ocppParam);
                } catch (Exception e) {
                    log.error("send DIRECT_EXCHANGE_PILE_ACTIVATION_BIND_AUTEL failed" + JSON.toJSONString(ocppParam), e);
                }
            }
        }

        if (!CollectionUtils.isEmpty(setPileEroamingDTOList)) {
            for (SetPileEroamingDTO setPileEroamingDTO : setPileEroamingDTOList) {
                try {
                    opLocationPileEvseService.setPileEroaming(setPileEroamingDTO);
                } catch (Exception e) {
                    log.error("setPileEroaming failed" + JSON.toJSONString(setPileEroamingDTO), e);
                }
            }
        }

        // 1、清理桩缓存的配置
        try {
            Result<List<String>> listResult = protocolFeignClient.batchClearOcppConfig(toAddPileSNs);
            log.info("ClearOcppConfigFailed {}", JSON.toJSONString(listResult));
        } catch (Exception e) {
            log.error("clearOcppConfig 异常 param:" + JSON.toJSONString(toAddPileSNs), e);
        }


        // 2、添加重构后运维平台桩
        opLocationEvseDTOs.forEach(item -> {
            try {
                log.info("新运维平台同步添加桩: {}", item.getPileSN());
                OpLocationDTO opLocationDTO = opLocationService.details(item.getLocationId()).getData();
                if (Objects.isNull(opLocationDTO.getOperatorId())) {
                    opLocationDTO.setOperatorId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
                }
                opsMgmtClient.saveOpsPile(formatPileInfoDTO(item, opLocationDTO));
                opLocationService.updateLocationUpdateTime(item.getLocationId());
            } catch (Exception e) {
                log.error("add ops-mgmt pile error:  " + JSON.toJSONString(item), e);
            }
        });


        // 3、充电枪信息添加成功之后需要发送MQ消息到车队那边
        try {
            List<String> pileSnList = opLocationEvseDTOs
                    .stream()
                    .filter(var -> (var != null && org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileSN())))
                    .map(OpLocationEvseDTO::getPileSN)
                    .collect(Collectors.toList());
            // 查询这些充电桩下的充电枪序列号
            List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseRepository.findList(pileSnList);
            log.info("OpLocationEvseServiceImpl.createEvse opLocationEvseElasticDTOList : {}", JSON.toJSONString(opLocationEvseElasticDTOList));
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                    EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                    evseInfoModifyDTO.setEvseSn(opLocationEvseElasticDTO.getEvseSn());
                    evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.ADD.getCode());
                    opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTO);
                }
            }
        } catch (Exception e) {
            log.error("直接添加充电桩后，推送充电枪信息给车队那边出现异常: " + JSON.toJSONString(opLocationEvseDTOs), e);
        }

        // 推送给广告
        if (!CollectionUtils.isEmpty(pileVoList)) {
            OpLocationForAdvVO<List<OpLocationPileForAdvVO>> param = new OpLocationForAdvVO<>();
            param.setOperationType("add");
            param.setData(pileVoList);
            log.info("推送给广告: {}", JSON.toJSONString(param));
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_EDIT_ADV_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_ADV_ROUTE, JSON.toJSONString(param));
        }

        stopWatch.stop();
        // ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑保存完毕之后其他处理↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

        log.info(stopWatch.prettyPrint());
        pileUploadSaveVO.setFailureNum(0);
        pileUploadSaveVO.setSuccessNum(opLocationPileEvseEntities.size());

        List<SavePileInfoVO> successPileInfoList = new ArrayList<>();
        for(OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntities) {
            SavePileInfoVO savePileInfoVO = new SavePileInfoVO();
            savePileInfoVO.setPileId(opLocationPileEvseEntity.getId());
            savePileInfoVO.setPileName(opLocationPileEvseEntity.getName());
            savePileInfoVO.setPileSn(opLocationPileEvseEntity.getPileSn());
            String evseList = opLocationPileEvseEntity.getEvseList();
            if (StringUtils.isNotEmpty(evseList)) {
                List<Long> longList = JSON.parseArray(evseList, Long.class);
                List<SaveEvseInfoVO> evseInfoVOList = new ArrayList<>();
                for (int i = 0; i < longList.size(); i++) {
                    SaveEvseInfoVO e = new SaveEvseInfoVO();
                    e.setConnector(String.format("%02d", i+1));
                    e.setEvseId(longList.get(i));
                    evseInfoVOList.add(e);
                }
                savePileInfoVO.setSaveEvseInfoVOList(evseInfoVOList);
            }
            successPileInfoList.add(savePileInfoVO);
        }
        pileUploadSaveVO.setSaveSuccessPileInfoVOList(successPileInfoList);
        return pileUploadSaveVO;
    }

    // 广告类型场站推送
    private void deviceNoticeAdsService(OpLocationEntity opLocationEntity, List<OpLocationPileForAdvVO> pileVoList, ChargePointMerchantRelationEntity chargePointEntity) {
        if (opLocationEntity.getBusinessType() == 2 || opLocationEntity.getBusinessType() == 3) {
            OpLocationPileForAdvVO pileVo = new OpLocationPileForAdvVO();
            pileVo.setLocationId(opLocationEntity.getId());
            pileVo.setLocationName(opLocationEntity.getName());
            pileVo.setSellerId(opLocationEntity.getOperatorId());
            pileVo.setPileSn(chargePointEntity.getSn());
            pileVo.setPileName(chargePointEntity.getName());
            pileVo.setPilePin(chargePointEntity.getPin());
            pileVo.setOverchargingPileFlag(chargePointEntity.getOverchargingPileFlag());
            pileVo.setScreen1Pixel(chargePointEntity.getScreen1Pixel());
            pileVo.setScreen1Size(chargePointEntity.getScreen1Size());
            pileVo.setTimeZone(opLocationEntity.getTimeZone());
            pileVo.setZoneId(opLocationEntity.getZoneId());
            String brandName = chargePointEntity.getBrandName();
            if (StringUtils.isBlank(brandName)) {
                Long brandId = chargePointEntity.getBrandId();
                BrandEnum brandEnum = Arrays.stream(BrandEnum.values()).filter(e -> e.getCode().equals(brandId.intValue())).findFirst().orElse(null);
                if (brandEnum != null) {
                    brandName = brandEnum.getName();
                }
            }
            pileVo.setBrandName(brandName);
            pileVo.setThirdPart("Autel".equalsIgnoreCase(brandName) ? 0 : 1);
            pileVoList.add(pileVo);
        }
    }

    private void handleEvscpSettingDTO(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO) {

        log.info("===>>>OpLocationEvseRepositoryImpl.handleEvscpSettingDTO opLocationPileEvseElasticDTO : {}",
                JSON.toJSONString(opLocationPileEvseElasticDTO));

        if (opLocationPileEvseElasticDTO == null
                || org.apache.commons.lang3.StringUtils.isBlank(opLocationPileEvseElasticDTO.getPileSn())) {
            return;
        }

        String redisResult = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getPileReportBritishActKey(opLocationPileEvseElasticDTO.getPileSn()));
        if (org.apache.commons.lang3.StringUtils.isBlank(redisResult)
                || JSON.parseObject(redisResult, EvscpSettingDTO.class) == null) {
            return;
        }
        EvscpSettingDTO evscpSettingDTO = JSON.parseObject(redisResult, EvscpSettingDTO.class);
        opLocationPileEvseElasticDTO.setBritainApproveSwitch(evscpSettingDTO.getEnableRegulation());
        opLocationPileEvseElasticDTO.setRandomDelaySwitch(evscpSettingDTO.getEnableRandomDelay());
    }

    private void validByBrand(OpLocationEvseDTO opLocationEvseDTO) {
        if (org.apache.commons.lang3.StringUtils.isBlank(opLocationEvseDTO.getPileName())) {
            log.error("PLEASE_ENTER_THE_CHARGING_PILE_NAME  {}", JSON.toJSONString(opLocationEvseDTO.getPileName()));
            throw new MessageCodeException(PileBaseEnum.PLEASE_ENTER_THE_CHARGING_PILE_NAME);
        }
        // 校验同一场站下的充电桩名称不能相同  在统一桩管理 里面已经校验了桩名称不能重复的问题
        if (opLocationEvseDTO.getLocationId() != null) {
            CheckPileNameDTO checkPileNameDTO = new CheckPileNameDTO();
            checkPileNameDTO.setPileName(opLocationEvseDTO.getPileName());
            checkPileNameDTO.setLocationId(opLocationEvseDTO.getLocationId());
            if (!this.checkPileNameInLocationUnique(checkPileNameDTO)) {
                log.error("NAME_ALREADY_USED  {}", JSON.toJSONString(checkPileNameDTO));
                throw new MessageCodeException(PileBaseEnum.NAME_ALREADY_USED);
            }
        }
        //道通品牌桩的校验
        if (opLocationEvseDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
            //判断是否美标极简版充电桩  美标极简版充电桩不能添加到平台作为商桩(要排在校验桩是否被绑定前，不然桩被改成商桩后，即不能添加在平台作为商桩，也不能添加到app作为家桩)
            Result<ChargePileDTO> chargePileDetailResult = null;
            try {
                chargePileDetailResult = deviceServiceFeign.pileDetail(opLocationEvseDTO.getPileSN());
            } catch (Exception e) {
                log.error("OpLocationEvseRepositoryImpl validEvse", e);
            }
            if (chargePileDetailResult != null && chargePileDetailResult.getData() != null) {
                String productModel = chargePileDetailResult.getData().getProductModel();
                if (StringUtils.isNotEmpty(productModel)) {
                    for (AmericaPileProductModelEnum americaPileProductModelEnum : AmericaPileProductModelEnum.values()) {
                        if (Objects.equals(productModel, americaPileProductModelEnum.getValue())) {
                            log.error("AMERICA_PILE_NOT_ALLOW_ADD  {}", opLocationEvseDTO.getPileSN());
                            throw new MessageCodeException(PileBaseEnum.AMERICA_PILE_NOT_ALLOW_ADD);
                        }
                    }
                }
            }
            //校验pileSN和pin
            VerifyDTO verifyDTO = new VerifyDTO();
            verifyDTO.setPin(opLocationEvseDTO.getPinCode());
            verifyDTO.setPileSn(opLocationEvseDTO.getPileSN());
            Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(verifyDTO);
            if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                log.error("PILESN_NOT_MATCH_PIN  {}", JSON.toJSONString(verifyDTO));
                throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
            }
            //校验桩是否被绑定
            Result<Object> result = dataServiceFeign.addPile(opLocationEvseDTO.getPileSN(), opLocationEvseDTO.getPinCode());
            log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
            if (result == null || !Objects.equals(result.getCode(), HttpCodeEnum.OK.getCode()) || !Objects.equals(result.getData(), Boolean.TRUE)) {
                log.error("OpLocationEvseRepositoryImpl.createEvse failed {}", opLocationEvseDTO.getPileSN());
                throw new MessageCodeException(PileBaseEnum.CHARGE_PILE_HAS_BIND_USER);
            }

            //三方桩的校验
        } else {
            //data-service是否已被绑定为家桩
            Result<Boolean> restResult = homePileClient.queryBind(opLocationEvseDTO.getPileSN());
            if (restResult != null && restResult.getCode().equals(org.apache.http.HttpStatus.SC_OK) && restResult.getData().equals(Boolean.TRUE)) {
                log.error("binded by home  {}", opLocationEvseDTO.getPileSN());
                throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND);
            }
        }
    }

    /**
     * 查询桩是否订阅到期
     * A22587
     *
     * @return 是否有效期的订阅
     */
    public boolean isSubscriptionCheck(String pileSn) {
        String evseSn = pileSn;
        BenefitFunctionReqDto benefitFunctionReqDto = new BenefitFunctionReqDto();
        benefitFunctionReqDto.setPileSn(Lists.newArrayList(evseSn));
        log.info("添加桩时,benefitFunctionReqDto:{}", JSON.toJSONString(benefitFunctionReqDto));

        Result<List<PileBenefitFunctionRespDto>> haveRightsPileList = subscribePileRightsService.getPileFunction(benefitFunctionReqDto);
        log.info("syncPileSnSubscriptionStatus，haveRightsPileList：{}", JSON.toJSONString(haveRightsPileList));

        log.info("添加桩时，haveRightsPileList：{}", JSON.toJSONString(haveRightsPileList));
        if (CollUtil.isNotEmpty(haveRightsPileList.getData())) {
            for (PileBenefitFunctionRespDto agreementFunctionDetailDto : haveRightsPileList.getData()) {
                if (agreementFunctionDetailDto.getUnavailableTime() != null && agreementFunctionDetailDto.getUnavailableTime() > System.currentTimeMillis()
                        &&  CollUtil.isNotEmpty(agreementFunctionDetailDto.getFunctionIdList()) && agreementFunctionDetailDto.getFunctionIdList().contains(PileChargingRights.GUN_SEARCH)) {
                    return true;
                }
            }
        }
        return false;
    }

    private String handlePowerType(Integer category, Integer phase) {
        //1：交流 2：直流 3 交直流'
        if (category == null) {
            category = 1;
        }
        if (phase == null) {
            phase = 3;
        }
        StringBuilder stringBuilder = new StringBuilder();
        switch (category) {
            case 1:
                stringBuilder.append("AC");
                stringBuilder.append("_").append(phase).append("_").append("PHASE");
                break;
            case 2:
                stringBuilder.append("DC");
                stringBuilder.append("_").append(phase).append("_").append("PHASE");
                break;
            default:
                break;
        }
        return stringBuilder.toString();
    }


    private String handlerEvseSn(String connectorId, String pileSN) {
        if (StringUtils.isBlank(connectorId) || StringUtils.isBlank(pileSN)) {
            return null;
        }
        String[] split = connectorId.split("");
        if ("0".equals(split[0])) {
            return pileSN + "_" + split[1];
        }
        return pileSN + "_" + connectorId;
    }


    public boolean checkPileNameInLocationUnique(CheckPileNameDTO checkPileNameDTO) {

        log.info("===>>>OpLocationEvseRepositoryImpl.checkPileNameInLocationUnique checkPileNameDTO : {}", JSON.toJSONString(checkPileNameDTO));

        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getLocationId, checkPileNameDTO.getLocationId());
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getName, checkPileNameDTO.getPileName());
        lambdaQueryWrapper.ne(null != checkPileNameDTO.getPileId(), OpLocationPileEvseEntity::getId, checkPileNameDTO.getPileId());
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
        return CollUtil.isEmpty(opLocationPileEvseEntityList);
    }

    @Override
    public Result<Boolean> updatePublicMark(UpdatePublicMarkDTO updatePublicMarkDTO) {
        if (ObjectUtils.isEmpty(updatePublicMarkDTO) || ObjectUtils.isEmpty(updatePublicMarkDTO.getPileId()) || ObjectUtils.isEmpty(updatePublicMarkDTO.getPublicMark())) {
            log.info("updatePublicMark接口数据为空");
            return Result.ofSucceed(Boolean.FALSE);
        }
        return opLocationEvseRepository.updatePublicMark(updatePublicMarkDTO);
    }

    @Override
    public Boolean updatePileName(UpdatePileNameDTO updatePileNameDTO) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.updatePileName updatePileNameDTO : {}",
                JSON.toJSONString(updatePileNameDTO));

        if (ObjectUtils.isEmpty(updatePileNameDTO)
                || org.apache.commons.lang3.StringUtils.isBlank(updatePileNameDTO.getPileName())
                || org.apache.commons.lang3.StringUtils.isBlank(updatePileNameDTO.getPileSn())) {
            return false;
        }
        Result<OpPileEvseInfoVO> opPileEvseInfoVOResult = opLocationEvseImpl.detailBySn(updatePileNameDTO.getPileSn());

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.updatePileName opPileEvseInfoVOResult : {}",
                JSON.toJSONString(opPileEvseInfoVOResult));

        if (ObjectUtils.isEmpty(opPileEvseInfoVOResult)
                || ObjectUtils.isEmpty(opPileEvseInfoVOResult.getData())) {
            return false;
        }

        List<OpLocationConnectorDTO> toBeInsertOpLocationConnectorDTOList = new ArrayList<>();
        Set<String> needToUpdateEvseSnSet = new HashSet<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(updatePileNameDTO.getOpLocationConnectorDTOs())) {
            needToUpdateEvseSnSet.addAll(opPileEvseInfoVOResult
                    .getData()
                    .getOpLocationEvseDTOS()
                    .stream()
                    .filter(val -> val != null && org.apache.commons.lang3.StringUtils.isNotBlank(val.getEvseSn()))
                    .map(OpLocationEvseDTO::getEvseSn)
                    .collect(Collectors.toSet()));
        } else {
            for (OpLocationConnectorDTO opLocationConnectorDTO : updatePileNameDTO.getOpLocationConnectorDTOs()) {
                String connectorId = opLocationConnectorDTO.getConnectorId();
                if (connectorId == null) {
                    continue;
                }
                boolean needUpdateReg = false;
                for (OpLocationEvseDTO opLocationEvseDTO : opPileEvseInfoVOResult.getData().getOpLocationEvseDTOS()) {
                    String evseSn = opLocationEvseDTO.getEvseSn();
                    if (org.apache.commons.lang3.StringUtils.isNotBlank(evseSn)
                            && connectorId.equals(CommonUtil.getGunNo(evseSn))) {
                        needUpdateReg = true;
                        needToUpdateEvseSnSet.add(evseSn);
                        break;
                    }
                }

                if (!needUpdateReg) {
                    toBeInsertOpLocationConnectorDTOList.add(opLocationConnectorDTO);
                }
            }
        }

        OpPileEvseInfoVO vo = opPileEvseInfoVOResult.getData();
        OpLocationEvseDTO dto = new OpLocationEvseDTO();
        if (!ObjectUtils.isEmpty(vo.getOpLocationEvseDTOS())) {
            List<OpLocationConnectorDTO> opLocationConnectorDTOs = new ArrayList<>();

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(toBeInsertOpLocationConnectorDTOList)) {
                opLocationConnectorDTOs.addAll(toBeInsertOpLocationConnectorDTOList);
            }

            for (OpLocationEvseDTO opLocationEvseDTO : vo.getOpLocationEvseDTOS()) {
                OpPileEvseInfoVO vo1 = new OpPileEvseInfoVO();
                BeanUtils.copyProperties(vo, vo1);
                dto.setPhase(org.springframework.util.StringUtils.hasText(updatePileNameDTO.getPhase()) ? updatePileNameDTO.getPhase() : vo1.getPhase());
                dto.setPowerType(org.springframework.util.StringUtils.hasText(updatePileNameDTO.getPowerType()) ? updatePileNameDTO.getPowerType() : vo1.getPowerType());
                if (Objects.nonNull(updatePileNameDTO.getPower())) {
                    dto.setPower(updatePileNameDTO.getPower().doubleValue());
                } else if (Objects.nonNull(vo1.getPower())) {
                    dto.setPower(vo1.getPower());
                }
                dto.setPileSN(vo1.getPileSn());
                dto.setPileName(updatePileNameDTO.getPileName());
                dto.setLocationId(vo1.getLocationId());
                dto.setBrandId(vo1.getBrandId());
                dto.setBrandName(vo1.getBrandName());
                dto.setThirdPart(vo1.getThirdPart());
                dto.setProductModel(vo1.getProductModel());
                dto.setId(vo1.getPileId());
                dto.setPublicMark(vo1.getPublicMark());
                dto.setPinCode(updatePileNameDTO.getPin());
                if (!ObjectUtils.isEmpty(opLocationEvseDTO.getOpLocationConnectorDTOList())
                        && needToUpdateEvseSnSet.contains(opLocationEvseDTO.getEvseSn())) {
                    OpLocationConnectorDTO opLocationConnectorDTOList = new OpLocationConnectorDTO();
                    BeanUtils.copyProperties(opLocationEvseDTO.getOpLocationConnectorDTOList(), opLocationConnectorDTOList);
                    dto.setPower(Objects.nonNull(updatePileNameDTO.getPower()) ? updatePileNameDTO.getPower().doubleValue() : opLocationConnectorDTOList.getPower());
                    if (Objects.nonNull(updatePileNameDTO.getPower())) {
                        dto.setPower(updatePileNameDTO.getPower().doubleValue());
                    } else {
                        dto.setPower(opLocationConnectorDTOList.getPower());
                    }

                    if (!ObjectUtils.isEmpty(opLocationConnectorDTOList.getPowerType())) {
                        dto.setPowerType(opLocationConnectorDTOList.getPowerType());
                    }
                    if (!ObjectUtils.isEmpty(opLocationEvseDTO) && !ObjectUtils.isEmpty(opLocationEvseDTO.getTariffId())) {
                        opLocationConnectorDTOList.setTariffId(opLocationEvseDTO.getTariffId().toString());
                    }
                    opLocationConnectorDTOs.add(opLocationConnectorDTOList);
                }
            }
            dto.setOpLocationConnectorDTOs(opLocationConnectorDTOs);
        }
        log.info("updatePileName,dto={}", JSON.toJSONString(dto));
        List<OpLocationEvseDTO> list = new ArrayList<>();
        list.add(dto);
        updateEvse(list);
        return true;
    }

    @Override
    public Boolean updatePileNameV2(UpdatePileNameDTO dto) {
        log.error("updatePileNameV2,dto={}", JSON.toJSONString(dto));
        //当前版本只允许修改桩名称
        if (ObjectUtils.isEmpty(dto) || StringUtils.isEmpty(dto.getPileName())
                || StringUtils.isBlank(dto.getPileSn())) {
            return false;
        }
        String pileName = dto.getPileName();
        String pileSn = dto.getPileSn();
        Long sellerId = LoginUserUtil.getSellerId();

        OpLocationPileEvseEntity pileEntity = this.opLocationPileEvseMapper.findOne(pileSn);
        if (pileEntity == null) {
            return false;
            //throw new MessageCodeException(PileBaseEnum.PILE_NOT_IN_LOCATION);
        }
        Long locationId = pileEntity.getLocationId();
        OpLocationEntity locationEntity = this.opLocationRepository.getById(locationId);
        if (locationEntity == null || locationEntity.getDeleted() == 1) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }

        //名称校验
        CheckPileNameDTO checkPileNameDTO = new CheckPileNameDTO();
        checkPileNameDTO.setPileId(pileEntity.getId());
        checkPileNameDTO.setPileName(pileName);
        checkPileNameDTO.setLocationId(locationId);
        if (!this.opLocationEvseRepository.checkPileNameInLocationUnique(checkPileNameDTO)) {
            throw new MessageCodeException(PileBaseEnum.NAME_ALREADY_USED);
        }

        //数据库更新
        Long now = System.currentTimeMillis();
        pileEntity.setName(pileName);
        pileEntity.setUpdatedAt(now);
        this.opLocationPileEvseRepository.updateById(pileEntity);
        List<Long> evseIds = JSON.parseArray(pileEntity.getEvseList(), Long.class);
        List<OpLocationEvseEntity> evseEntityList = this.opLocationEvseRepository.listByIds(evseIds);
        if (!CollectionUtils.isEmpty(evseEntityList)) {

            evseEntityList.stream().forEach(e -> e.setUpdatedAt(now));
            this.opLocationEvseRepository.updateBatchById(evseEntityList);

            List<IndexQuery> updateList = new ArrayList<>();
            this.opLocationEvseElastic.findAllById(evseIds).forEach(evseDto -> {
                evseDto.setPileName(pileName);
                evseDto.setUpdatedAt(now);
                updateList.add(new IndexQueryBuilder().withId(evseDto.getId().toString()).withObject(evseDto).build());
            });
            if (updateList.size() > 0) {
                this.elasticsearchRestTemplate.bulkIndex(updateList, OpLocationEvseElasticDTO.class);
                this.elasticsearchRestTemplate.indexOps(OpLocationEvseElasticDTO.class).refresh();
            }
        } else {
            log.error("updatePileNameV2,evseEntityList is empty pileSn={}", pileSn);
        }
        OpLocationPileEvseElasticDTO pileDto = this.opLocationPileEvseElastic.findById(pileEntity.getId()).orElse(null);
        if (pileDto != null) {
            pileDto.setName(pileName);
            pileDto.setUpdatedAt(now);
            List<IndexQuery> updateList = new ArrayList<>();
            updateList.add(new IndexQueryBuilder().withId(pileDto.getId().toString()).withObject(pileDto).build());
            this.elasticsearchRestTemplate.bulkIndex(updateList, OpLocationPileEvseElasticDTO.class);
            this.elasticsearchRestTemplate.indexOps(OpLocationPileEvseElasticDTO.class).refresh();
        } else {
            log.error("updatePileNameV2,pileDto is empty pileSn={}", pileSn);
        }

        //数据同步处理
        try {
            log.info("同步 资产/统一桩管理 更新桩名称: {}", pileSn);
            UpdatePileNameDTO updatePileNameDTO = new UpdatePileNameDTO();
            updatePileNameDTO.setMerchantId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
            updatePileNameDTO.setPileName(pileName);
            updatePileNameDTO.setPileSn(pileSn);
            applicationEventPublisher.publishEvent(updatePileNameDTO);

            log.info("新运维平台同步更新桩: {}", pileSn);
            UpdatePileV2DTO updatePileV2DTO = new UpdatePileV2DTO();
            updatePileV2DTO.setSn(pileSn);
            updatePileV2DTO.setName(pileName);

            if (!CollectionUtils.isEmpty(evseEntityList)) {
                List<ConnectorDTO> gunTypeList = new ArrayList<>();
                List<OpLocationConnectorEntity> connectorEntityList = this.opLocationConnectorService.findByEvseId(evseIds);
                Map<Long, OpLocationConnectorEntity> connectorEntityMap = connectorEntityList.stream().collect(Collectors.toMap(OpLocationConnectorEntity::getLocationEvseId, e -> e, (f, s) -> f));
                evseEntityList.stream().forEach(evseEntity -> {
                    String evseSn = evseEntity.getEvseSn();
                    OpLocationConnectorEntity connectorEntity = connectorEntityMap.get(evseEntity.getId());
                    gunTypeList.add(ConnectorDTO.builder().connectorNo(Integer.valueOf(connectorEntity.getConnectorId())).connectorType(connectorEntity.getGunType()).build());
                    rabbitTemplate.convertAndSend(PILE_BASE_PILE_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_PILE_UPDATE_ROUTE, evseSn);
                });
                updatePileV2DTO.setConnectorList(gunTypeList);
                opsMgmtClient.editPile(updatePileV2DTO);

            }
            //广告类型场站推送
            Map<Long, OpLocationElasticDTO> locationMap = new HashMap<>();
            OpLocationElasticDTO locationDto = new OpLocationElasticDTO();
            locationDto.setId(locationEntity.getId());
            locationDto.setBusinessType(locationEntity.getBusinessType());
            locationDto.setName(locationEntity.getName());
            locationDto.setTimeZone(locationEntity.getZoneId());
            locationDto.setZoneId(locationEntity.getZoneId());
            locationMap.put(locationId,locationDto);
            this.pushToAd(Arrays.asList(pileDto), locationMap, sellerId);
        } catch (Exception e) {
            log.error("update ops-mgmt pile error: {}", e);
        }
        pileBaseAsync.pushEvseList(pileSn, ActionType.update.getCode());
        return true;
    }

    private void pushToAd(List<OpLocationPileEvseElasticDTO> pileDtoList, Map<Long, OpLocationElasticDTO> locationMap,Long sellerId) {
        List<OpLocationElasticDTO> tmp = locationMap.values().stream().filter(e -> e.getBusinessType() == 2 || e.getBusinessType() == 3).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(tmp)) {
            log.info("pushToAd,tmp is empty.");
            return;
        }
        Set<String> pileSnList = pileDtoList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toSet());
        List<ChargePointVO> listToUse = this.chargePointMerchantRelationService.findChargePointBySNs(pileSnList, sellerId);
        Map<String, ChargePointVO> pileVoMap = listToUse.stream().collect(toMap(ChargePointVO::getSn, e -> e, (f, s) -> f));
        OpLocationForAdvVO<List<OpLocationPileForAdvVO>> param = new OpLocationForAdvVO();
        param.setOperationType("edit");
        List<OpLocationPileForAdvVO> pileVoList = new ArrayList<>(pileDtoList.size());
        pileDtoList.stream().forEach(pileDto -> {
            String pileSn = pileDto.getPileSn();
            OpLocationPileForAdvVO pileVo = new OpLocationPileForAdvVO();
            pileVo.setPileSn(pileSn);
            Long locationId = pileDto.getLocationId();
            OpLocationElasticDTO locationDto = locationMap.get(locationId);
            if (locationDto == null) {
                log.info("pushToAd,locationDto is null.");
                return;
            }
            pileVo.setLocationId(locationId);
            pileVo.setLocationName(locationDto.getName());
            pileVo.setSellerId(sellerId);
            pileVo.setPileSn(pileDto.getPileSn());
            pileVo.setPileName(pileDto.getName());
            ChargePointVO vo = pileVoMap.get(pileDto.getPileSn());
            if (vo != null) {
                pileVo.setPilePin(vo.getPin());
                pileVo.setScreen1Pixel(vo.getScreen1Pixel());
                pileVo.setScreen1Size(vo.getScreen1Size());
                pileVo.setOverchargingPileFlag(vo.getOverchargingPileFlag());
            }
            pileVo.setTimeZone(locationDto.getTimeZone());
            pileVo.setZoneId(locationDto.getZoneId());
            String brandName = pileDto.getBrandName();
            if (StringUtils.isEmpty(brandName)) {
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
        if (CollectionUtils.isEmpty(pileVoList)) {
            log.info("pushToAd,pileVoList is null.");
            return;
        }
        param.setData(pileVoList);
        this.rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_EDIT_ADV_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_ADV_ROUTE, JSON.toJSONString(param));
    }

    @Override
    public List<OpLocationPileEvseDTO> searchPileListBySellerId(QueryPileDTO queryPileDTO) {
        log.info("========== searchPileListBySellerId invoked of OpLocationPileEvseServiceImpl, the queryPileDTO: {}", JSON.toJSONString(queryPileDTO));
        if (null == queryPileDTO || CollUtil.isEmpty(queryPileDTO.getLocationIds()) && null == queryPileDTO.getOperatorId()) {
            throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
        }
        if (CollUtil.isEmpty(queryPileDTO.getLocationIds()) && null != queryPileDTO.getOperatorId()) {
            //查询桩信息
            List<OpLocationPileEvseElasticDTO> pileInformationList = opLocationPileEvseRepository.getAllPileInfoBySellerId(queryPileDTO.getOperatorId());
            List<Long> locationIdList = pileInformationList.stream().map(OpLocationPileEvseElasticDTO::getLocationId).collect(Collectors.toList());
            queryPileDTO.setLocationIds(locationIdList);
        }
        if (CollUtil.isNotEmpty(queryPileDTO.getLocationIds())) {
            LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.in(CollUtil.isNotEmpty(queryPileDTO.getLocationIds()), OpLocationPileEvseEntity::getLocationId, queryPileDTO.getLocationIds());
            queryWrapper.in(CollUtil.isNotEmpty(queryPileDTO.getPileSnList()), OpLocationPileEvseEntity::getPileSn, queryPileDTO.getPileSnList());
            queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
            List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(queryWrapper);
            if (CollUtil.isNotEmpty(opLocationPileEvseEntityList)) {
                List<OpLocationPileEvseDTO> response = new ArrayList<>();
                for (OpLocationPileEvseEntity entity : opLocationPileEvseEntityList) {
                    OpLocationPileEvseDTO tmp = new OpLocationPileEvseDTO();
                    BeanUtils.copyProperties(entity, tmp);
                    response.add(tmp);
                }
                return response;
            }
        }
        return null;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> getPileListByPileSNList(List<String> pileSNList) {
        return opLocationPileEvseElastic.findByPileSnIn(pileSNList);
    }

    @Override
    public Boolean setPileEroamingForPile(SetPileEroamingForPileDTO setPileEroamingForPileDTO) {

        log.info("===>>> OpLocationEvseServiceImpl.setPileEroamingForPile setPileEroamingForPileDTO : {}",
                JSON.toJSONString(setPileEroamingForPileDTO));

        return opLocationPileEvseService.setPileEroamingForPile(setPileEroamingForPileDTO);
    }

    @Override
    public Page<SelectGunInfoForTariffGroupIdVO> selectBindGunInfoForTariffGroupId(SelectGunInfoForTariffGroupIdDTO selectGunInfoForTariffGroupIdDTO) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.selectBindGunInfoForTariffGroupId selectGunInfoForTariffGroupIdDTO : {}",
                JSON.toJSONString(selectGunInfoForTariffGroupIdDTO));

        Page<SelectGunInfoForTariffGroupIdVO> result = new Page<>(selectGunInfoForTariffGroupIdDTO.getPage(), selectGunInfoForTariffGroupIdDTO.getPageSize());

        if (LoginUserHolder.getLoginUser() == null
                || LoginUserHolder.getLoginUser().getPayload() == null
                || LoginUserHolder.getLoginUser().getPayload().getSellerId() == null) {
            return result;
        }

        selectGunInfoForTariffGroupIdDTO.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", selectGunInfoForTariffGroupIdDTO.getSellerId()));
        queryBuilder.must(QueryBuilders.termQuery("tariffId", selectGunInfoForTariffGroupIdDTO.getTariffGroupId()));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(selectGunInfoForTariffGroupIdDTO.getLocationIdList())) {
            queryBuilder.must(QueryBuilders.termsQuery("locationId", selectGunInfoForTariffGroupIdDTO.getLocationIdList()));
        }

        if (org.apache.commons.lang3.StringUtils.isNotBlank(selectGunInfoForTariffGroupIdDTO.getSearchValue())) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", QueryParserBase.escape(selectGunInfoForTariffGroupIdDTO.getSearchValue()))));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", QueryParserBase.escape(selectGunInfoForTariffGroupIdDTO.getSearchValue()))));
            queryBuilder.must(queryBuilder2);
        }

        Pageable pageable = PageRequest.of(selectGunInfoForTariffGroupIdDTO.getPage() - 1,
                selectGunInfoForTariffGroupIdDTO.getPageSize());

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withTrackTotalHits(true)
                .withPageable(pageable)
                .build();

        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> page = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(list)) {
            return result;
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.selectBindGunInfoForTariffGroupId getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        Set<Long> havePermissionLocationIdSet = new HashSet<>();
        if (getLocationIdsResult != null
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(getLocationIdsResult.getData())) {
            havePermissionLocationIdSet.addAll(getLocationIdsResult.getData());
        }

        List<SelectGunInfoForTariffGroupIdVO> selectGunInfoForTariffGroupIdVOS = new ArrayList<>();
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : list) {
            SelectGunInfoForTariffGroupIdVO selectGunInfoForTariffGroupIdVO = new SelectGunInfoForTariffGroupIdVO();

            selectGunInfoForTariffGroupIdVO.setGunId(opLocationEvseElasticDTO.getId());
            selectGunInfoForTariffGroupIdVO.setPileName(opLocationEvseElasticDTO.getPileName());
            selectGunInfoForTariffGroupIdVO.setPileSn(opLocationEvseElasticDTO.getPileSn());
            selectGunInfoForTariffGroupIdVO.setConnectorId(CommonUtil.getGunNo(opLocationEvseElasticDTO.getEvseSn()));
            selectGunInfoForTariffGroupIdVO.setLocationId(opLocationEvseElasticDTO.getLocationId());
            selectGunInfoForTariffGroupIdVO.setLocationName(opLocationEvseElasticDTO.getLocationName());
            selectGunInfoForTariffGroupIdVO.setHaveBind(true);
            selectGunInfoForTariffGroupIdVO.setHavePermission(havePermissionLocationIdSet.contains(opLocationEvseElasticDTO.getLocationId()));

            selectGunInfoForTariffGroupIdVOS.add(selectGunInfoForTariffGroupIdVO);
        }

        result.setRecords(selectGunInfoForTariffGroupIdVOS);
        result.setTotal(page.getTotalElements());
        result.setSize(page.getSize());
        result.setPages(page.getTotalPages());
        result.setCurrent(selectGunInfoForTariffGroupIdDTO.getPage());
        return result;
    }

    @Override
    public Page<SelectGunInfoForTariffGroupIdVO> selectGunInfoForTariffGroupId(SelectGunInfoForTariffGroupIdDTO selectGunInfoForTariffGroupIdDTO) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.selectGunInfoForTariffGroupId selectGunInfoForTariffGroupIdDTO : {}",
                JSON.toJSONString(selectGunInfoForTariffGroupIdDTO));

        Page<SelectGunInfoForTariffGroupIdVO> result = new Page<>(selectGunInfoForTariffGroupIdDTO.getPage(), selectGunInfoForTariffGroupIdDTO.getPageSize());

        if (LoginUserHolder.getLoginUser() == null
                || LoginUserHolder.getLoginUser().getPayload() == null
                || LoginUserHolder.getLoginUser().getPayload().getSellerId() == null) {
            return result;
        }

        selectGunInfoForTariffGroupIdDTO.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", selectGunInfoForTariffGroupIdDTO.getSellerId()));

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.selectGunInfoForTariffGroupId getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        Set<Long> havePermissionLocationIdSet = new HashSet<>();
        if (getLocationIdsResult != null
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(getLocationIdsResult.getData())) {
            havePermissionLocationIdSet.addAll(getLocationIdsResult.getData());
        }

        if (CollectionUtils.isEmpty(havePermissionLocationIdSet)) {
            return result;
        }
        List<Long> tmpIds = selectGunInfoForTariffGroupIdDTO.getLocationIdList();
        if (!CollectionUtils.isEmpty(tmpIds)) {
            tmpIds.removeIf(e -> !havePermissionLocationIdSet.contains(e));
            if (CollectionUtils.isEmpty(tmpIds)) {
                return result;
            }
            queryBuilder.must(QueryBuilders.termsQuery("locationId", tmpIds));
        } else {
            queryBuilder.must(QueryBuilders.termsQuery("locationId", havePermissionLocationIdSet));
        }

        if (org.apache.commons.lang3.StringUtils.isNotBlank(selectGunInfoForTariffGroupIdDTO.getSearchValue())) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", QueryParserBase.escape(selectGunInfoForTariffGroupIdDTO.getSearchValue()))));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", QueryParserBase.escape(selectGunInfoForTariffGroupIdDTO.getSearchValue()))));
            queryBuilder.must(queryBuilder2);
        }

        Pageable pageable = PageRequest.of(selectGunInfoForTariffGroupIdDTO.getPage() - 1,
                selectGunInfoForTariffGroupIdDTO.getPageSize());

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withTrackTotalHits(true)
                .withPageable(pageable)
                .build();

        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> page = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(list)) {
            return result;
        }

        List<SelectGunInfoForTariffGroupIdVO> selectGunInfoForTariffGroupIdVOS = new ArrayList<>();
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : list) {
            SelectGunInfoForTariffGroupIdVO selectGunInfoForTariffGroupIdVO = new SelectGunInfoForTariffGroupIdVO();

            selectGunInfoForTariffGroupIdVO.setGunId(opLocationEvseElasticDTO.getId());
            selectGunInfoForTariffGroupIdVO.setPileName(opLocationEvseElasticDTO.getPileName());
            selectGunInfoForTariffGroupIdVO.setPileSn(opLocationEvseElasticDTO.getPileSn());
            selectGunInfoForTariffGroupIdVO.setConnectorId(CommonUtil.getGunNo(opLocationEvseElasticDTO.getEvseSn()));
            selectGunInfoForTariffGroupIdVO.setLocationId(opLocationEvseElasticDTO.getLocationId());
            selectGunInfoForTariffGroupIdVO.setLocationName(opLocationEvseElasticDTO.getLocationName());
            selectGunInfoForTariffGroupIdVO.setHaveBind(selectGunInfoForTariffGroupIdDTO.getTariffGroupId().equals(opLocationEvseElasticDTO.getTariffId()));
            selectGunInfoForTariffGroupIdVO.setHavePermission(havePermissionLocationIdSet.contains(opLocationEvseElasticDTO.getLocationId()));

            selectGunInfoForTariffGroupIdVOS.add(selectGunInfoForTariffGroupIdVO);
        }

        result.setRecords(selectGunInfoForTariffGroupIdVOS);
        result.setTotal(page.getTotalElements());
        result.setSize(page.getSize());
        result.setPages(page.getTotalPages());
        result.setCurrent(selectGunInfoForTariffGroupIdDTO.getPage());
        return result;
    }

    @Override
    public Page<GetDeviceInfoForFleetVO> getDeviceInfoForFleet(GetDeviceInfoForFleetDTO getDeviceInfoForFleetDTO) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.getDeviceInfoForFleet getDeviceInfoForFleetDTO : {}",
                JSON.toJSONString(getDeviceInfoForFleetDTO));

        if (getDeviceInfoForFleetDTO == null) {
            return new Page<>();
        }

        if (getDeviceInfoForFleetDTO.getSellerId() == null) {
            Long sellerId = LoginUserUtil.getSellerId();
            if (sellerId == null) {
                return new Page<>(getDeviceInfoForFleetDTO.getPage(), getDeviceInfoForFleetDTO.getPageSize());
            }
            getDeviceInfoForFleetDTO.setSellerId(sellerId);
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.getDeviceInfoForFleet getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        List<Long> locationIds = new ArrayList<>();
        if (getLocationIdsResult == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getLocationIdsResult.getData())) {
            return new Page<>(getDeviceInfoForFleetDTO.getPage(), getDeviceInfoForFleetDTO.getPageSize());
        }

        locationIds.addAll(getLocationIdsResult.getData());

        if (getDeviceInfoForFleetDTO.getLocationId() != null
                && !locationIds.contains(getDeviceInfoForFleetDTO.getLocationId())) {
            return new Page<>(getDeviceInfoForFleetDTO.getPage(), getDeviceInfoForFleetDTO.getPageSize());
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", getDeviceInfoForFleetDTO.getSellerId()));
        if (getDeviceInfoForFleetDTO.getLocationId() != null) {
            queryBuilder.must(QueryBuilders.termQuery("locationId", getDeviceInfoForFleetDTO.getLocationId()));
        } else {
            queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));
        }

        if (getDeviceInfoForFleetDTO.getChargeEnable() != null
                && getDeviceInfoForFleetDTO.getChargeEnable()) {
            queryBuilder.must(QueryBuilders.termQuery("subscriptionCheck", Boolean.TRUE));
        }

        if (org.apache.commons.lang3.StringUtils.isNotBlank(getDeviceInfoForFleetDTO.getSearchValue())) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", QueryParserBase.escape(getDeviceInfoForFleetDTO.getSearchValue()))));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", QueryParserBase.escape(getDeviceInfoForFleetDTO.getSearchValue()))));
            queryBuilder.must(queryBuilder2);
        }

        Pageable pageable = PageRequest.of(getDeviceInfoForFleetDTO.getPage() - 1,
                getDeviceInfoForFleetDTO.getPageSize());
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withTrackTotalHits(true)
                .withPageable(pageable)
                .build();

        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> page = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(list)) {
            return new Page<>(getDeviceInfoForFleetDTO.getPage(), getDeviceInfoForFleetDTO.getPageSize());
        }

        Set<Long> locationIdSet = list
                .stream()
                .filter(val -> val.getLocationId() != null)
                .map(OpLocationEvseElasticDTO::getLocationId)
                .collect(Collectors.toSet());

        Map<Long, OpLocationElasticDTO> locationIdAndOpLocationElasticDTOMap = new HashMap<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(locationIdSet)) {
            List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByIdIn(locationIdSet);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationElasticDTOList)) {
                opLocationElasticDTOList.forEach(val -> locationIdAndOpLocationElasticDTOMap.put(val.getId(), val));
            }
        }

        List<String> pileSnList = list
                .stream()
                .filter(val -> org.apache.commons.lang3.StringUtils.isNotBlank(val.getPileSn()))
                .map(OpLocationEvseElasticDTO::getPileSn)
                .distinct()
                .collect(Collectors.toList());

        Map<String, OpLocationPileEvseElasticDTO> pileSnAndOpLocationPileEvseElasticDTOMap = new HashMap<>();
        Map<String, ChargePointMerchantRelationEntity> pileSnAndChargePointMerchantRelationEntityMap = new HashMap<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileSnList)) {
            List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(pileSnList);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
                opLocationPileEvseElasticDTOList.forEach(val -> pileSnAndOpLocationPileEvseElasticDTOMap.put(val.getPileSn(), val));
            }

            List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntityList = chargePointMerchantRelationMapper.selectList(
                    new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                            .in(ChargePointMerchantRelationEntity::getSn, pileSnList)
                            .eq(ChargePointMerchantRelationEntity::getMerchantId, getDeviceInfoForFleetDTO.getSellerId()));

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantRelationEntityList)) {
                chargePointMerchantRelationEntityList.forEach(val -> pileSnAndChargePointMerchantRelationEntityMap.put(val.getSn(), val));
            }
        }

        List<GetDeviceInfoForFleetVO> getDeviceInfoForFleetVOList = new ArrayList<>();
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : list) {

            String pileSn = opLocationEvseElasticDTO.getPileSn();
            Long locationId = opLocationEvseElasticDTO.getLocationId();

            GetDeviceInfoForFleetVO getDeviceInfoForFleetVO = new GetDeviceInfoForFleetVO();

            getDeviceInfoForFleetVO.setPileSn(pileSn);
            getDeviceInfoForFleetVO.setPileName(opLocationEvseElasticDTO.getPileName());

            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn);
            if (opLocationPileEvseElasticDTO != null) {
                Double pilePower = opLocationPileEvseElasticDTO.getPower();
                if (pilePower != null) {
                    getDeviceInfoForFleetVO.setPilePower(new BigDecimal(String.valueOf(pilePower)));
                }
            }

            ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = pileSnAndChargePointMerchantRelationEntityMap.get(pileSn);
            if (chargePointMerchantRelationEntity != null) {
                String powerType = chargePointMerchantRelationEntity.getPowerType();
                getDeviceInfoForFleetVO.setPileType(powerType);
            }

            getDeviceInfoForFleetVO.setEvseSn(opLocationEvseElasticDTO.getEvseSn());

            Integer gunNo = CommonUtil.getGunNo(opLocationEvseElasticDTO.getEvseSn());
            if (gunNo != null) {
                getDeviceInfoForFleetVO.setConnectorNumber(String.valueOf(gunNo));
            }

            Double power = opLocationEvseElasticDTO.getPower();
            if (power != null) {
                getDeviceInfoForFleetVO.setGunPower(new BigDecimal(String.valueOf(power)));
            }

            getDeviceInfoForFleetVO.setLocationId(locationId);
            getDeviceInfoForFleetVO.setLocationName(opLocationEvseElasticDTO.getLocationName());

            OpLocationElasticDTO opLocationElasticDTO = locationIdAndOpLocationElasticDTOMap.get(locationId);
            if (opLocationElasticDTO != null) {
                getDeviceInfoForFleetVO.setZoneId(opLocationElasticDTO.getZoneId());
            }

            getDeviceInfoForFleetVOList.add(getDeviceInfoForFleetVO);
        }

        Page<GetDeviceInfoForFleetVO> result = new Page<>();

        result.setRecords(getDeviceInfoForFleetVOList);
        result.setTotal(page.getTotalElements());
        result.setSize(page.getSize());
        result.setPages(page.getTotalPages());
        result.setCurrent(getDeviceInfoForFleetDTO.getPage());

        return result;
    }

    @Override
    public List<String> queryPileListByLocations(List<Long> locationId) {
        LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.in(OpLocationPileEvseEntity::getLocationId, locationId);
        queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(queryWrapper);
        if (CollUtil.isNotEmpty(opLocationPileEvseEntities)) {
            return opLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList());
        }
        return Collections.emptyList();
    }

    @Override
    public List<EvseDataVO> batchGetEvseInfo(List<String> evseList) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.batchGetEvseInfo evseList : {}",
                JSON.toJSONString(evseList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evseList)) {
            return null;
        }

        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByEvseSnIn(evseList);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseElasticDTOList)) {
            return null;
        }

        List<EvseDataVO> evseDataVOList = new ArrayList<>();
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
            String evseSn = opLocationEvseElasticDTO.getEvseSn();
            String state = opLocationEvseElasticDTO.getState();
            LocationEvseStatusV2Enum locationEvseStatusV2Enum = LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(state);
            EvseDeviceStatusEnum evseDeviceStatusEnum = EvseDeviceStatusEnum.getEnumByName(state);

            EvseDataVO evseDataVO = new EvseDataVO();

            evseDataVO.setEvseSn(evseSn);
            evseDataVO.setGunNo(CommonUtil.getGunNo(evseSn));
            evseDataVO.setStateOfChargingLive(locationEvseStatusV2Enum.getCode());
            evseDataVO.setEvseDeviceStatusCode(evseDeviceStatusEnum.getCode());
            evseDataVO.setEvseDeviceStatusName(state);

            evseDataVOList.add(evseDataVO);
        }

        return evseDataVOList;
    }

    @Override
    public List<PileEvseInfoVO> batchGetEvseData(List<String> evseList) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.batchGetEvseData evseList : {}",
                JSON.toJSONString(evseList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evseList)) {
            return null;
        }

        List<PileEvseInfoVO> pileEvseInfoVOList = new ArrayList<>();
        for (String evseSn : evseList) {

            if (org.apache.commons.lang3.StringUtils.isBlank(evseSn)) {
                continue;
            }

            String redisResult = stringRedisTemplate.opsForValue().get(ConfigRedisKeyConstant.getCacheCurrentChargeGunInfoDtoKey(evseSn));
            if (org.apache.commons.lang3.StringUtils.isBlank(redisResult)) {
                continue;
            }

            CacheChargeInfoDTO cacheChargeInfoDTO = JSON.parseObject(redisResult, CacheChargeInfoDTO.class);
            PileEvseInfoVO pileEvseInfoVO = new PileEvseInfoVO();

            pileEvseInfoVO.setEvseSn(evseSn);
            pileEvseInfoVO.setVehicleMac(cacheChargeInfoDTO.getCarMac());

            pileEvseInfoVOList.add(pileEvseInfoVO);
        }

        return com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileEvseInfoVOList)
                ? null
                : pileEvseInfoVOList;
    }

    @Override
    public List<EvseTariffInfoVO> batchGetEvseTariffInfo(List<String> evseSnList) {

        log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.batchGetEvseTariffInfo evseSnList : {}",
                JSON.toJSONString(evseSnList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evseSnList)) {
            return null;
        }

        List<EvseTariffInfoVO> evseTariffInfoVOS = new ArrayList<>();
        for (String evseSn : evseSnList) {

            EvseTariffInfoVO evseTariffInfoVO = new EvseTariffInfoVO();

            String pileSn = CommonUtil.getPileSn(evseSn);
            Integer gunNo = CommonUtil.getGunNo(evseSn);

            QueryBusinessCostModelRuleDetailDTO queryBusinessCostModelRuleDetailDTO = new QueryBusinessCostModelRuleDetailDTO();
            queryBusinessCostModelRuleDetailDTO.setPileSn(pileSn);
            queryBusinessCostModelRuleDetailDTO.setGunNo(gunNo);

            Result<BusinessCostModelRuleVO> queryBusinessCostModelRuleDetailResult = tariffFeignClient.queryBusinessCostModelRuleDetail(queryBusinessCostModelRuleDetailDTO);

            log.info("======>>>>>>>>>>> OpLocationEvseServiceImpl.batchGetEvseTariffInfo queryBusinessCostModelRuleDetailResult : {}",
                    JSON.toJSONString(queryBusinessCostModelRuleDetailResult));

            evseTariffInfoVO.setEvseSn(evseSn);
            evseTariffInfoVO.setBusinessCostModelRuleVO(queryBusinessCostModelRuleDetailResult.getData());
            evseTariffInfoVOS.add(evseTariffInfoVO);
        }
        return evseTariffInfoVOS;
    }
}
