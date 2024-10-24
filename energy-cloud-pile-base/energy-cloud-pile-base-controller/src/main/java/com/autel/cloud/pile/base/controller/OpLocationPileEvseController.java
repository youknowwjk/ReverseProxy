package com.autel.cloud.pile.base.controller;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.model.DataAuthorizeDto;
import com.autel.cloud.base.model.Node;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.common.CustomizedDateTime;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.mgnt.vo.DnmsgLatestValueVO;
import com.autel.cloud.pile.base.config.ops.content.sim.SimCardInfoDTO;
import com.autel.cloud.pile.base.config.ops.content.wifi.WifiInfoDTO;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.model.PileQrCodeDTO;
import com.autel.cloud.pile.base.domain.model.SelectDeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.SelectDeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.domain.service.*;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.advertise.OpPileAdvertiseReq;
import com.autel.cloud.pile.base.dto.beta.QueryBetaTestPileInfoDTO;
import com.autel.cloud.pile.base.dto.britishAct.SecurityEventDTO;
import com.autel.cloud.pile.base.dto.common.SearchDTO;
import com.autel.cloud.pile.base.dto.config.ClearSmartChargingProfileDTO;
import com.autel.cloud.pile.base.dto.config.PileFreeVendInfoDTO;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.dto.fleet.SelectChargingInfoForFleetDTO;
import com.autel.cloud.pile.base.dto.pile.EvscpSettingDTO;
import com.autel.cloud.pile.base.dto.pile.PileSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileDTO;
import com.autel.cloud.pile.base.enums.NetworkConnectTypeEnum;
import com.autel.cloud.pile.base.enums.NormalWhiteBrandEnum;
import com.autel.cloud.pile.base.enums.SubStatus;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OpsMgntFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargePointMerchantTerminalMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantTerminalEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import com.autel.cloud.pile.base.infrastructure.util.CompetenceUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseBaseInfoVO;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseListVO;
import com.autel.cloud.pile.base.vo.beta.BetaTestPileInfoVO;
import com.autel.cloud.pile.base.vo.britishAct.BritishActVO;
import com.autel.cloud.pile.base.vo.britishAct.ModifyCommonPileToBritainStandPileForTestVO;
import com.autel.cloud.pile.base.vo.britishAct.SecurityEventVO;
import com.autel.cloud.pile.base.vo.fleet.SelectChargingInfoForFleetVO;
import com.autel.cloud.pile.base.vo.location.PromptVO;
import com.autel.cloud.pile.base.vo.pile.OcppConfigurationViewEnableVO;
import com.autel.cloud.pile.base.vo.pile.PileBasicInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileSimpleInfoQueryVO;
import com.autel.cloud.pile.user.api.constant.RedisKeys;
import com.autel.cloud.pile.user.api.dto.UserCompetenceDTO;
import com.autel.cloud.pile.user.api.enums.SubTreeEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.autel.cloud.pile.user.api.vo.UserCompetenceVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.assertj.core.util.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StopWatch;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import java.time.ZoneId;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @ClassName OpPileEvseController
 * @Author A22121
 * @Description
 * @Date 2022/5/10 21:39
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@RequestMapping("/opPileEvse")
@Api(tags = "桩信息接口")
@Slf4j
@Validated
public class OpLocationPileEvseController {

    private final OpLocationPileEvseService opLocationPileEvseService;
    @Autowired
    private OpCostRuleDistributeService opCostRuleDistributeService;

    @Resource
    private OpLocationEvse opLocationEvse;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Autowired
    private HomePileFeignClient homePileFeignClient;

    @Autowired
    private OpsMgntFeignClientProxy opsMgntFeignClientProxy;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private PileUserServiceFeign pileUserServiceFeign;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Autowired
    private ChargePointMerchantTerminalMapper chargePointMerchantTerminalMapper;

    @Resource
    private TbLenBindRelationMapper tbLenBindRelationMapper;

    @Resource
    private PileUserFeign pileUserFeign;


    public OpLocationPileEvseController(OpLocationPileEvseService opLocationPileEvseService) {
        this.opLocationPileEvseService = opLocationPileEvseService;
    }


    @PostMapping("/getPileInfoByPileNameAndLocationId")
    @ApiOperation(value = "通过EvseSn查询桩信息", notes = "通过EvseSn查询桩信息")
    public Result<List<OpLocationPileEvseVO>> getPileInfoByPileNameAndLocationId(@RequestParam("pileName") String pileName, @RequestParam("locationId") Long locationId) {
        return opLocationEvse.getPileInfoByPileNameAndLocationId(pileName, locationId);
    }

    @PostMapping("/getPileInfoByEvseSn")
    @ApiOperation(value = "通过EvseSn查询桩信息", notes = "通过EvseSn查询桩信息")
    public Result<OpEvseInfoDTO> getPileInfoByEvseSn(@RequestParam("evseSn") String evseSn) {
        log.info("========== the evseSn in the getPileInfoByEvseSn:{}", evseSn);
        return opLocationEvse.getPileInfoByEvseSn(evseSn);
    }

    @OperationActionLog(action = "query", object = "pile")
    @PostMapping("/getPileInfoByPileSn")
    @ApiOperation(value = "通过PileSn查询桩信息", notes = "通过PileSn查询桩信息")
    public Result<OpPileEvseInfoVO> getPileInfoByPileSn(@RequestParam("pileSn") String pileSn) {
        log.info("===>>>pileSn: {}", pileSn);
        Result<OpPileEvseInfoVO> result = opLocationEvse.getPileInfoByPileSn(pileSn);
        if (Objects.nonNull(result) && Objects.nonNull(result.getData())) {
            // serviceId->List<ServiceId>
            //List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.PILE_DETAIL);
            List<ChargePointLicenseVO> lastExpireTimeChargePointLicense = chargePointMerchantRelationService.findLastExpireTimeChargePointLicense(Collections.singletonList(result.getData().getPileSn()), PileChargingRights.OPERATION_SERVICE_ID_LIST, LoginUserHolder.getLoginUser().getPayload().getSellerId());
            Map<String, ChargePointLicenseVO> collect = lastExpireTimeChargePointLicense.stream().collect(Collectors.toMap(ChargePointLicenseVO::getSn, Function.identity()));
            if (collect.containsKey(result.getData().getPileSn())) {
                result.getData().setSubscriptionStatus(collect.get(result.getData().getPileSn()).getSubscriptionStatus());
                result.getData().setEffectiveDays(collect.get(result.getData().getPileSn()).getEffectiveDays());
            } else if (isWhiteBrand()) {
                result.getData().setSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
                result.getData().setEffectiveDays(365L);// 暂时默认写死365，暂时用不到
            }
        }
        log.info("============ the OpPileEvseInfoVO in the getPileInfoByPileSn function:{}", JSON.toJSONString(result));
        return result;
    }

    @PostMapping("/getPileInfoByPileSnNoDataAuth")
    @ApiOperation(value = "通过PileSn查询桩信息(无数据权限控制，仅供服务端之间调用，不可暴露给终端)", notes = "通过PileSn查询桩信息(无数据权限控制，仅供服务端之间调用，不可暴露给终端)")
    public Result<OpPileEvseInfoVO> getPileInfoByPileSnNoDataAuth(@RequestParam("pileSn") String pileSn) {
        return opLocationEvse.getPileInfoByPileSn(pileSn);
    }

    @PostMapping("/getPileInfoByPileSnNoAuth")
    @ApiOperation(value = "通过PileSn查询桩信息，无数据权限控制", notes = "通过PileSn查询桩信息，无数据权限控制")
    public Result<OpPileEvseInfoVO> getPileInfoByPileSnNoAuth(@RequestParam("pileSn") String pileSn) {
        Result<OpPileEvseInfoVO> pileInfoByPileSn = opLocationEvse.getPileInfoByPileSn(pileSn);
        log.info("getPileInfoByPileSnNoAuth, result : {}", JSON.toJSONString(pileInfoByPileSn));
        return pileInfoByPileSn;
    }

    @PostMapping("/getPileInfoByPileSnList")
    @ApiOperation(value = "通过PileSn查询桩信息列表", notes = "通过PileSn查询桩信息列表")
    public Result<List<OpPileEvseInfoVO>> getPileInfoByPileSnList(@RequestBody List<String> pileSnList) {
        return opLocationEvse.getPileInfoByPileSnList(pileSnList);
    }

    @PostMapping("/getEvseListByPileSn")
    @ApiOperation(value = "查询充电设备列表", notes = "查询充电设备列表")
    public com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getEvseListByPileSn(@RequestParam("pileSn") String pileSn) {
        return opLocationEvse.getEvseListByPileSn(pileSn);
    }

    @OperationActionLog(action = "query", object = "pile")
    @PostMapping("/page")
    @ApiOperation(value = "场站详情桩分页查询", notes = "场站详情桩分页查询")
    @CustomizedDateTime
    public com.autel.cloud.base.model.Result<Page<PilePageVO>> stationPilePage(@Valid @RequestBody PilePageDTO pilePageDTO) {
        StopWatch stopWatch = new StopWatch("场站详情桩分页查询");
        stopWatch.start("查询场站桩");
        Result<Page<PilePageVO>> pageResult = opLocationEvse.stationPilePage(pilePageDTO);
        stopWatch.stop();
        stopWatch.start("查询桩订阅状态");
        if (Objects.nonNull(pageResult) && Objects.nonNull(pageResult.getData()) && Objects.nonNull(pageResult.getData().getRecords())) {
            List<String> sns = pageResult.getData().getRecords().stream().map(PilePageVO::getPileSn).collect(Collectors.toList());
            // serviceId->List<ServiceId>
            //List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.PILE_DETAIL);
            List<ChargePointLicenseVO> lastExpireTimeChargePointLicense = chargePointMerchantRelationService.findLastExpireTimeChargePointLicense(sns, PileChargingRights.OPERATION_SERVICE_ID_LIST, LoginUserHolder.getLoginUser().getPayload().getSellerId());
            Map<String, ChargePointLicenseVO> collect = lastExpireTimeChargePointLicense.stream().collect(Collectors.toMap(ChargePointLicenseVO::getSn, Function.identity()));
            pageResult.getData().getRecords().forEach(item -> {
                if (collect.containsKey(item.getPileSn())) {
                    item.setSubscriptionStatus(collect.get(item.getPileSn()).getSubscriptionStatus());
                    item.setEffectiveDays(collect.get(item.getPileSn()).getEffectiveDays());
                    item.setExpireDate(collect.get(item.getPileSn()).getExpireDate());
                }
            });
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        Page<PilePageVO> resultData = pageResult.getData();
        List<DataAuthorizeDto> permissionData = new ArrayList<>();
        DataAuthorizeDto dto = new DataAuthorizeDto();
        dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
        dto.setAppId(CompetenceUtil.APP_ID);
        dto.setSellerId(LoginUserUtil.getSellerId().toString());
        dto.setUserId(LoginUserUtil.getUserId().toString());
        List<Node> data = new ArrayList<>();
        List<Node> stationData = new ArrayList<>();
        if (!CollectionUtils.isEmpty(resultData.getRecords())) {
            Map<Long, List<PilePageVO>> locationPileMap = resultData.getRecords().stream().collect(Collectors.groupingBy(PilePageVO::getLocationId));
            locationPileMap.forEach((k,v)->{
                Node node = new Node();
                node.setLevel(SubTreeEnum.STATION.getCode());
                node.setNode(k.toString());
                stationData.add(node);
            });
            String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
            String competence = stringRedisTemplate.opsForValue().get(key);
            try {
                if (org.apache.commons.lang.StringUtils.isBlank(competence)) {
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
        return CompetenceUtil.encapsulation(resultData,permissionData);
    }

    @PostMapping("/pileQrCodePage")
    @ApiOperation(value = "二维码批量下载页面", notes = "二维码批量下载页面")
    public Result<Page<PileQrCodeDTO>> getPileQrCodePage(@RequestParam(value = "page",defaultValue = "1") int page,@RequestParam(value = "pageSize",defaultValue = "30") int pageSize,@RequestParam(value = "keyWord",required = false) String keyWord, @RequestParam(value = "locationId",required = false) String locationId) {
        Page<PileQrCodeDTO> pageRes = opLocationEvse.getPileQrCodePage(keyWord,locationId,page,pageSize);
        return Result.ofSucceed(pageRes);
    }

    @PostMapping("/getEvseListByPileSn/v2")
    @ApiOperation(value = "查询充电设备列表", notes = "查询充电设备列表")
    public com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getEvseListByPileSnv2(@RequestBody @Validated OpPileEvseDTO dto) {

        log.info("===>>> OpLocationPileEvseController.getEvseListByPileSnv2 dto : {}", JSON.toJSONString(dto));

        return opLocationEvse.getEvseListByPileSn(dto.getPileSn());
    }

    @PostMapping("/getHistoryMeterInfo")
    @ApiOperation(value = "桩-运行-图表信息查询", notes = "桩-运行-图表信息查询")
    public com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getHistoryMeterInfo(@RequestBody @Validated OpPileEvseDTO dto) {
        return opLocationEvse.getHistoryMeterInfo(dto.getPileSn());
    }

    @OperationActionLog(action = "query", object = "pile")
    @PostMapping("/getPileInfoByPileSn/v3")
    @ApiOperation(value = "通过PileSn查询桩信息", notes = "通过PileSn查询桩信息")
    public com.autel.cloud.base.model.Result<OpPileEvseInfoVO> getPileInfoByPileSnV3(@RequestBody @Validated OpPileEvseDTO dto) {

        log.info("===>>>OpLocationPileEvseController.getPileInfoByPileSnv3 dto : {}", JSON.toJSONString(dto));
        String pileSn = dto.getPileSn();
        Result<OpPileEvseInfoVO> result = opLocationEvse.getPileInfoByPileSn(pileSn);
        if (Objects.nonNull(result) && Objects.nonNull(result.getData())) {
            OpPileEvseInfoVO opPileEvseInfoVO = result.getData();
            //  serviceId->List<ServiceId>
            //List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.PILE_DETAIL);
            if (isWhiteBrand()) {
                opPileEvseInfoVO.setSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
                opPileEvseInfoVO.setEffectiveDays(365L);// 暂时默认写死365，暂时用不到
            } else {
                // V2.8 判断是否超充桩
                Long merchantId = LoginUserUtil.getSellerId();
                LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda()
                        .eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId).eq(ChargePointMerchantTerminalEntity::getHostSn, dto.getPileSn());
                List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
                List<String> terminalSnList = chargePointMerchantTerminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());
                log.info("merchantId: {}, sn: {}, chargePointMerchantTerminalEntities: {}", merchantId, dto.getPileSn(), chargePointMerchantTerminalEntities.size());
                if (CollUtil.isNotEmpty(terminalSnList)) {
                    log.info("terminalSnList is not empty.");
                    // 是超充，判断全部终端是否订阅
              /*  List<ChargePointLicenseVO> lastExpireTimeChargePointLicense = chargePointMerchantRelationService.findLastExpireTimeChargePointLicense(terminalSnList, PileChargingRights.OPERATION_SERVICE_ID_LIST_ALL, LoginUserHolder.getLoginUser().getPayload().getSellerId());
                Map<String, ChargePointLicenseVO> collect = lastExpireTimeChargePointLicense.stream().collect(Collectors.toMap(ChargePointLicenseVO::getSn, Function.identity()));*/
                    LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
                    queryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
                    queryWrapper.in(TbLenBindRelationEntity::getPileSn, terminalSnList);
                    queryWrapper.in(TbLenBindRelationEntity::getServiceId, PileChargingRights.OPERATION_SERVICE_ID_LIST);
                    queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());
                    List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
                    if (CollUtil.isNotEmpty(tbLenBindRelationEntities)) {
                        Map<String, List<TbLenBindRelationEntity>> collect = tbLenBindRelationEntities.stream().collect(Collectors.groupingBy(TbLenBindRelationEntity::getPileSn));
                        if (CollUtil.size(collect) != terminalSnList.size()) {
                            log.info("getPileInfoByPileSnV3, terminal size != tbLenBindRelationEntities size. ");
                            opPileEvseInfoVO.setSubscriptionStatus(SubStatus.INVALIDITY.getStatus());
                        } else {
                            log.info("getPileInfoByPileSnV3, terminal size = tbLenBindRelationEntities size. ");
                            opPileEvseInfoVO.setSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
                        }
                    }
                } else {
                    List<ChargePointLicenseVO> lastExpireTimeChargePointLicense = chargePointMerchantRelationService.findLastExpireTimeChargePointLicense(Collections.singletonList(pileSn), PileChargingRights.OPERATION_SERVICE_ID_LIST, LoginUserHolder.getLoginUser().getPayload().getSellerId());
                    Map<String, ChargePointLicenseVO> collect = lastExpireTimeChargePointLicense.stream().collect(Collectors.toMap(ChargePointLicenseVO::getSn, Function.identity()));
                    if (collect.containsKey(pileSn)) {
                        opPileEvseInfoVO.setSubscriptionStatus(collect.get(pileSn).getSubscriptionStatus());
                        opPileEvseInfoVO.setEffectiveDays(collect.get(pileSn).getEffectiveDays());
                    }
                }
            }



            // 增加网络设置查询
            DnmsgLatestValueVO dnmsgLatestValueVO = opsMgntFeignClientProxy.getPileNetworkSetting(pileSn);
            List<String> networkList = opPileEvseInfoVO.getConnectType();
            log.info("{}-->networkList:{}", pileSn, JSON.toJSONString(networkList));
            if (Objects.nonNull(dnmsgLatestValueVO)) {

                // 当桩没有上报网络类型时从运维网络设置获取
                if (CollectionUtil.isEmpty(networkList)) {
                    networkList = getConnectTypeList(dnmsgLatestValueVO);
                    opPileEvseInfoVO.setConnectType(networkList);
                }
                List<WifiInfoDTO> availableWifiList = opsMgntFeignClientProxy.getWifiList(dnmsgLatestValueVO);
                opPileEvseInfoVO.setWifiInfo(CollectionUtil.isEmpty(availableWifiList) ? null : availableWifiList.get(0));
                List<SimCardInfoDTO> availableSimList = opsMgntFeignClientProxy.getSimCardList(dnmsgLatestValueVO);;
                opPileEvseInfoVO.setSimCardInfo(CollectionUtil.isEmpty(availableSimList) ? null : availableSimList.get(0));
            }
        }
        log.info("============ the OpPileEvseInfoVO in the getPileInfoByPileSn function:{}", JSON.toJSONString(result));
        OpPileEvseInfoVO resultData = result.getData();
        List<DataAuthorizeDto> permissionData = new ArrayList<>();
        if (!ObjectUtils.isEmpty(resultData)) {
                DataAuthorizeDto dataAuthorizeDto = new DataAuthorizeDto();
                dataAuthorizeDto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                dataAuthorizeDto.setAppId(CompetenceUtil.APP_ID);
                dataAuthorizeDto.setSellerId(LoginUserUtil.getSellerId().toString());
                dataAuthorizeDto.setUserId(LoginUserUtil.getUserId().toString());
                List<Node> data = new ArrayList<>();
                List<Node> stationData = new ArrayList<>();
                Node station = new Node();
                station.setNode(resultData.getLocationId().toString());
                station.setLevel(SubTreeEnum.STATION.getCode());
                stationData.add(station);
            String key = RedisKeys.getUserCompetence(LoginUserUtil.getUserId(), LoginUserUtil.getSellerId());
            String competence = stringRedisTemplate.opsForValue().get(key);
            try {
                if (org.apache.commons.lang.StringUtils.isBlank(competence)) {
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
                dataAuthorizeDto.setData(data);
            }else {
                dataAuthorizeDto.setData(stationData);
            }
                permissionData.add(dataAuthorizeDto);
        }
        return CompetenceUtil.encapsulation(resultData,permissionData);
    }

    private boolean isWhiteBrand() {
        Long sellerId = LoginUserUtil.getSellerId();
        Result<SellerDetailVO> detail = pileUserFeign.detail(sellerId);
        if (detail.getData() != null) {
            return NormalWhiteBrandEnum.whiteBrand(detail.getData().getSellerSubject());
        }
        return false;
    }

    /**
     * 从运维协议网络设置获取连接类型
     * @param dnmsgLatestValueVO
     * @return
     */
    private List<String> getConnectTypeList(DnmsgLatestValueVO dnmsgLatestValueVO) {
        List<String> networkList = Lists.newArrayList();
        if (opsMgntFeignClientProxy.hasWifiConfig(dnmsgLatestValueVO)) {
            networkList.add(NetworkConnectTypeEnum.CONNECT_WIFI.getCode());
        }
        if (opsMgntFeignClientProxy.hasSimCardConfig(dnmsgLatestValueVO)) {
            networkList.add(NetworkConnectTypeEnum.CONNECT_4G.getCode());
        }
        return networkList;
    }


    @PostMapping("/checkExist")
    @ApiOperation(value = "校验桩是否存在", notes = "校验桩是否存在")
    public Result<Boolean> checkExist(@RequestBody @Validated OpPileEvseDTO dto) {
        return Result.ofSucceed(opLocationEvse.checkExist(dto));
    }

    @OperationActionLog(action = "query", object = "pile")
    @PostMapping("/getPileInfoByPileSn/v2")
    @ApiOperation(value = "通过PileSn查询桩信息", notes = "通过PileSn查询桩信息")
    public Result<OpPileEvseInfoVO> getPileInfoByPileSnv2(@RequestBody @Validated OpPileEvseDTO dto) {

        log.info("===>>>OpLocationPileEvseController.getPileInfoByPileSnv2 dto : {}", JSON.toJSONString(dto));

        return this.getPileInfoByPileSn(dto.getPileSn());
    }

    @GetMapping("/check/{pileId}")
    @ApiOperation(value = "删除桩前的校验(必须要处理),用于场站删除充电桩", notes = "删除桩前的校验")
    public Result<Boolean> checkPileIsCanDel(@PathVariable("pileId") Long pileId) {
         opLocationEvse.checkPileIsCanDel(pileId);
         return Result.ofSucceed(Boolean.TRUE);
    }

    @GetMapping("/check/prompt")
    @ApiOperation(value = "删除桩前的校验(提示),用于场站删除充电桩", notes = "删除桩前的校验")
    public Result<PromptVO> checkPilePrompt(@RequestParam("pileSn") String pileSn) {
        return Result.ofSucceed(opLocationEvse.checkPilePrompt(pileSn));
    }

    @GetMapping("/check/del/{pileId}")
    @ApiOperation(value = "删除桩前的校验(包含必填和提示),用于场站删除充电桩", notes = "删除桩前的校验(包含必填和提示),用于场站删除充电桩")
    public Result<PromptVO> checkPileDelPrompt(@PathVariable("pileId") String pileId) {
        return Result.ofSucceed(opLocationEvse.checkPileDelPrompt(Long.parseLong(pileId)));
    }

    @OperationActionLog(action = "delete", object = "deletePile")
    @DeleteMapping("/pile/{pileId}")
    @ApiOperation(value = "删除桩", notes = "删除桩")
    public Result<Boolean> deleteByPileId(@PathVariable("pileId") Long pileId, Boolean deleteFlag) {
        return opLocationEvse.deleteByPileId(pileId, deleteFlag);
    }

    @OperationActionLog(action = "query", object = "pile")
    @GetMapping("/pile/{pileId}")
    @ApiOperation(value = "桩详情", notes = "桩详情")
    public com.autel.cloud.base.model.Result<OpPileEvseInfoVO> detail(@PathVariable("pileId") Long pileId) {

        log.info("===>>>OpLocationPileEvseController.detail pileId : {}", JSON.toJSONString(pileId));

        return opLocationEvse.detail(pileId);
    }

    @GetMapping("/downModuleXls")
    @ApiOperation(value = "桩导入模板下载", notes = "桩导入模板下载")
    public Result<Void> downModuleXls(HttpServletRequest request, HttpServletResponse response) {
        return opLocationPileEvseService.downModuleResourceXls(request, response);
    }

    @GetMapping("/syncRule")
    @ApiOperation(value = "同步进场控制数据到桩ES", notes = "同步进场控制数据到桩ES")
    public Result<Integer> syncRule() {
        return Result.ofSucceed(opLocationPileEvseService.syncRule());
    }

    @GetMapping("/downModuleGenerateXls")
    @ApiOperation(value = "桩导入模板下载(生成的xls文件)", notes = "桩导入模板下载(生成的xls文件")
    public Result<Void> downModuleXls2(HttpServletRequest request, HttpServletResponse response) {
        return opLocationPileEvseService.downModuleGenerateXls(request, response);
    }

    @OperationActionLog(action = "import", object = "pile")
    @PostMapping(value = "/uploadModuleXls/{locationId}")
    @ApiOperation(value = "批量导入桩", notes = "批量导入桩")
    public Result<PileUploadCheckVO> uploadModuleXls(@PathVariable("locationId") String locationId, @RequestParam("file") MultipartFile multipartFile) {
        return opLocationPileEvseService.uploadModuleXls(locationId, multipartFile);
    }

    @ApiOperation(value = "导入充电桩（new）", notes = "导入充电桩(new)")
    @PostMapping("/pile/import/{locationId}")
    @OperationActionLog(action = "import", object = "pile")
    public Object newImport(@PathVariable("locationId") String locationId, @RequestParam("file") MultipartFile file) {
        return Result.ofSucceed(opLocationPileEvseService.newImport(locationId, file));
    }

    @ApiOperation(value = "下载导入错误文件", notes = "下载导入错误文件")
    @PostMapping("/downloadImportErrorFile")
    @OperationActionLog(action = "export", object = "pile")
    public void downloadImportErrorFile(@RequestHeader(value = "Accept-Language", defaultValue = "en-US") String language,
                                        HttpServletResponse response) {
        opLocationPileEvseService.downloadImportErrorFile(language, response);
    }

    @PostMapping(value = "/simulateUploadModuleXls/{locationId}")
    @ApiOperation(value = "批量导入桩", notes = "批量导入桩")
    public Result<PileUploadCheckVO> simulateUploadModuleXls(@PathVariable("locationId") String locationId, @RequestBody List<PileUploadDTO> pileUploadDTOList) {
        return opLocationPileEvseService.simulateUploadModuleXls(locationId, pileUploadDTOList);
    }

    @Log(title = "批量保存桩")
    @PostMapping(value = "/savePileList")
    @ApiOperation(value = "批量保存桩", notes = "批量保存桩")
    public Result<PileUploadSaveVO> savePileList(@RequestBody @Valid PileUploadSaveDTO pileUploadSaveDTO) {

        log.info("===>>>OpLocationPileEvseController.savePileList pileUploadSaveDTO: {}", JSON.toJSONString(pileUploadSaveDTO));

        return opLocationPileEvseService.savePileList(pileUploadSaveDTO);
    }

    @PostMapping(value = "/getEvseByPileId")
    @ApiOperation(value = "批量保存桩之后，根据桩id查询桩枪信息", notes = "批量保存桩之后，根据桩id查询桩枪信息")
    @Deprecated
    public Result<List<EvseAndTariffIdVO>> getEvseByPileId(@RequestBody @Valid PileUploadSaveVO pileUploadSaveVO) {

        log.info("===>>>OpLocationPileEvseController.getEvseByPileId pileUploadSaveVO: {}", JSON.toJSONString(pileUploadSaveVO));

        return opLocationPileEvseService.getEvseByPileId(pileUploadSaveVO);
    }

    @PostMapping(value = "/queryTariffByPileSN/{pileSN}")
    @ApiOperation(value = "根据桩SN码查询计费规则", notes = "根据桩SN码查询计费规则")
    public Result<List<CostModelRuleDTO>> queryTariffByPileSN(@PathVariable("pileSN") String pileSN) {
        return opLocationPileEvseService.queryTariffByPileSN(pileSN);
    }

    @PostMapping(value = "/queryTariffByPileSN")
    @ApiOperation(value = "根据桩SN码查询计费规则", notes = "根据桩SN码查询计费规则")
    public Result<Map<String, CostModelRuleDTO>> queryTariffByPileSN(@RequestBody String... pileSN) {
        return opLocationPileEvseService.queryTariffByPileSN(pileSN);
    }

    @PostMapping(value = "/syncToAdvertise")
    @ApiOperation(value = "查询场站桩枪数据-广告同步", notes = "查询场站桩枪数据-广告同步")
    public Result<Page<LocationForAdvertiseDTO>> syncToAdvertise(@RequestBody PileSyncForAdvertiseParamDTO paramDTO) {

        log.info("===>>>OpLocationPileEvseController.syncToAdvertise paramDTO: {}", JSON.toJSONString(paramDTO));

        return opLocationPileEvseService.syncToAdvertise(paramDTO);
    }

    @GetMapping(value = "/syncPileOperatorIdCache")
    @ApiOperation(value = "桩运营商ID缓存同步", notes = "桩运营商ID缓存同步")
    public Result<Boolean> syncPileOperatorIdCache(@RequestParam(value = "sn", required = false) String sn) {
        return opLocationPileEvseService.syncPileOperatorIdCache(sn);
    }

    @PostMapping(value = "/getListByLocationIds")
    @ApiOperation(value = "根据场站查询桩列表", notes = "根据场站查询桩列表")
    public Result<List<OpLocationPileEvseVO>> getListByLocationIds(@RequestBody List<Long> locationIds) {
        return opLocationPileEvseService.getListByLocationIds(locationIds);
    }

    @PostMapping(value = "/updatePile")
    @ApiOperation(value = "更新桩离线电流", notes = "更新桩离线电流")
    public Result<Long> updatePile(@RequestBody OpLocationPileEvseDTO dto) {
        return Result.ofSucceed(opLocationPileEvseService.updatePile(dto));
    }


    @PostMapping(value = "/scanPileEvceDetail")
    @ApiOperation(value = "落地页设备信息展示", notes = "落地页设备信息展示")
    public Result<List<OpLocationPileEvseLandingPageVO>> scanPileEvceDetail(@RequestBody ScanPileEvseDTO scanPileEvseDTO) {
        return opLocationPileEvseService.scanPileEvceDetail(scanPileEvseDTO);
    }

    /**
     * @param pileSn 商桩序列号
     * @return 商家id
     * @function 根据商家id获取商桩序列号
     */
    @GetMapping(value = "/getSellerIdByPileSn")
    @ApiOperation(value = "根据商桩序列号获取商家id", notes = "根据商桩序列号获取商家id")
    public Result<Long> getSellerIdByPileSn(@RequestParam("pileSn") String pileSn) {

        log.info("===>>> OpLocationPileEvseController.scanPileEvceDetail pileSn : {}", JSON.toJSONString(pileSn));

        return Result.ofSucceed(opLocationPileEvseService.getSellerIdByPileSn(pileSn));
    }

    /**
     * @param currentPage 当前页码
     * @param pageSize    每页数据量
     * @param keyword     关键词（仅支持桩SN的模糊搜索）
     * @return 英标桩列表
     * @function 英国法案认证：查询英标桩列表
     */
    @GetMapping("/getBritishActPileListPage")
    @ApiOperation(value = "英国法案认证：查询英标桩列表", notes = "英国法案认证：查询英标桩列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "currentPage", value = "当前页码，最小位1，长度11位", dataType = "int", paramType = "query", defaultValue = "1", example = "1", required = true),
            @ApiImplicitParam(name = "pageSize", value = "每页数据量，1-50", dataType = "int", paramType = "query", defaultValue = "10", example = "10", required = true),
            @ApiImplicitParam(name = "keyword", value = "关键词，仅支持桩SN的模糊搜索", dataType = "String", paramType = "query", example = "AE0007A1GMBC00093R")
    })
    public Result<Page<BritishActVO>> getBritishActPileListPage(@RequestParam(value = "currentPage", required = true) @Min(value = 1, message = "当前页码必须大于等于1") int currentPage,
                                                                @RequestParam(value = "pageSize", required = true) @Min(value = 1, message = "每页数据量必须大于等于1") @Max(value = 50, message = "每页数据量必须小于等于50") int pageSize,
                                                                @RequestParam(value = "keyword") String keyword) {

        log.info("===>>>OpLocationPileEvseController.getBritishActPileListPage currentPage : {} and pageSize : {} and keyword : {}", JSON.toJSONString(currentPage), JSON.toJSONString(pageSize), JSON.toJSONString(keyword));

        return Result.ofSucceed(opLocationPileEvseService.getBritishActPileListPage(currentPage, pageSize, keyword));
    }

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 英标桩详情
     * @funtion 英国法案认证：查询英标桩详情
     */
    @PostMapping(value = "/getBritishActPileDetail")
    @ApiOperation(value = "英国法案认证：查询英标桩详情", notes = "英国法案认证：查询英标桩详情")
    public Result<BritishActVO> getBritishActPileDetail(@RequestBody BritishActVO britishActVO) {

        log.info("===>>>OpLocationPileEvseController.getBritishActPileDetail britishActVO : {}", JSON.toJSONString(britishActVO));

        return Result.ofSucceed(opLocationPileEvseService.getBritishActPileDetail(britishActVO));
    }

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 英标桩详情
     * @funtion 英国法案认证：编辑默认充电时间
     */
    @PostMapping(value = "/updateBritishActPileDefaultChargingTime")
    @ApiOperation(value = "英国法案认证：编辑默认充电时间", notes = "英国法案认证：编辑默认充电时间")
    public Result<BritishActVO> updateBritishActPileDefaultChargingTime(@RequestBody BritishActVO britishActVO) {

        log.info("===>>>OpLocationPileEvseController.updateBritishActPileDefaultChargingTime britishActVO : {}", JSON.toJSONString(britishActVO));

        return Result.ofSucceed(opLocationPileEvseService.updateBritishActPileDefaultChargingTime(britishActVO));
    }

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 是否需要提醒
     * @funtion 英国法案认证：高峰期充电增加提醒
     */
    @PostMapping(value = "/isPublicPileStartDuringPeakElectricityConsumption")
    @ApiOperation(value = "英国法案认证：高峰期充电增加提醒", notes = "英国法案认证：高峰期充电增加提醒")
    public Result<BritishActVO> isPublicPileStartDuringPeakElectricityConsumption(@RequestBody BritishActVO britishActVO) {

        log.info("===>>>OpLocationPileEvseController.isPublicPileStartDuringPeakElectricityConsumption britishActVO : {}", JSON.toJSONString(britishActVO));

        britishActVO.setCurrentTimeTimestamp(System.currentTimeMillis());
        britishActVO.setPublicPileStartDuringPeakElectricityConsumption(opLocationPileEvseService.isPublicPileStartDuringPeakElectricityConsumption(britishActVO));
        return Result.ofSucceed(britishActVO);
    }

    /**
     * @param currentPage 当前页码
     * @param pageSize    每页数据量
     * @param pileSn      设备序列号
     * @param zoneId      时区id名称
     * @return 安全日志列表
     * @function 英国法案认证：查看安全日志
     */
    @ApiImplicitParams({
            @ApiImplicitParam(name = "currentPage", value = "当前页码, 最小为1，最大11位", dataType = "int", paramType = "query", defaultValue = "1", example = "1", required = true),
            @ApiImplicitParam(name = "pageSize", value = "每页数据量, 范围：1-50", dataType = "int", paramType = "query", defaultValue = "10", example = "10", required = true),
            @ApiImplicitParam(name = "pileSn", value = "设备序列号", dataType = "String", paramType = "query", example = "AE0022H1GN5C00783X", required = true),
            @ApiImplicitParam(name = "zoneId", value = "时区id名称", dataType = "String", paramType = "query", example = "Asia/Shanghai", required = true)
    })
    @GetMapping("/getSecurityEventListPage")
    @ApiOperation(value = "英国法案认证：查看安全日志", notes = "英国法案认证：查看安全日志")
    public Result<Page<SecurityEventVO>> getSecurityEventListPage(@RequestParam(value = "currentPage", required = true) @Min(value = 1, message = "页码必须大于等于1") int currentPage,
                                                                  @RequestParam(value = "pageSize", required = true) @Min(value = 1, message = "每页数量必须大于等于1") int pageSize,
                                                                  @RequestParam(value = "pileSn", required = true) String pileSn,
                                                                  @RequestParam(value = "zoneId", required = true) String zoneId) {

        log.info("===>>>OpLocationPileEvseController.getSecurityEventListPage currentPage : {} and pageSize : {} and pileSn : {} and zoneId : {}", JSON.toJSONString(currentPage), JSON.toJSONString(pageSize), JSON.toJSONString(pileSn), JSON.toJSONString(zoneId));

        if (StringUtils.isBlank(zoneId)) {
            zoneId = ZoneId.systemDefault().getId();
        }

        SecurityEventDTO securityEventDTO = new SecurityEventDTO();
        securityEventDTO.setPage(currentPage);
        securityEventDTO.setPageSize(pageSize);
        securityEventDTO.setPileSn(pileSn);
        securityEventDTO.setZoneId(zoneId);
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String language = request.getHeader("accept-language");

        log.info("header里面国际化语言：{}", language);

        securityEventDTO.setLanguage(language);
        Result<Page<SecurityEventVO>> result = homePileFeignClient.getSecurityEventListForWeb(securityEventDTO);

        log.info("===>>>OpLocationPileEvseController.getSecurityEventListPage result : {}", JSON.toJSONString(result));

        return result;
    }

    /**
     * @param modifyCommonPileToBritainStandPileForTestVO 为方便测试英国法案需求(把充电桩修改为英标桩) 出参模型
     * @return 修改结果
     * @function 为方便测试英国法案需求(需要把充电桩修改为英标桩)。仅供测试使用！请勿随便调用！谢谢！
     */
    @PostMapping(value = "/modifyCommonPileToBritainStandPileForTest")
    @ApiOperation(value = "为方便测试英国法案需求(需要把充电桩修改为英标桩)。仅供测试使用！请勿随便调用！谢谢！", notes = "为方便测试英国法案需求(需要把充电桩修改为英标桩)。仅供测试使用！请勿随便调用！谢谢！")
    public Result<Map<String, Boolean>> modifyCommonPileToBritainStandPileForTest(@RequestBody ModifyCommonPileToBritainStandPileForTestVO modifyCommonPileToBritainStandPileForTestVO) {

        log.info("===>>>OpLocationPileEvseController.modifyCommonPileToBritainStandPileForTest modifyCommonPileToBritainStandPileForTestVO : {}", JSON.toJSONString(modifyCommonPileToBritainStandPileForTestVO));

        String operationPassword = modifyCommonPileToBritainStandPileForTestVO.getOperationPassword();
        return Result.ofSucceed(opLocationPileEvseService.modifyCommonPileToBritainStandPileForTest(modifyCommonPileToBritainStandPileForTestVO, operationPassword));
    }

    @GetMapping("/syncBritainPile")
    @ApiOperation(value = "同步英标桩数据")
    public Result<Boolean> syncBritainPile() {
        return Result.ofSucceed(opLocationPileEvseService.syncBritainPile());
    }

    @PostMapping("/randomDelay")
    @ApiOperation(value = "设置随机延迟开关", notes = "设置随机延迟开关")
    public Result<Boolean> randomDelay(@RequestBody RandomDelayDTO randomDelayDTO) {
        return opLocationPileEvseService.randomDelay(randomDelayDTO);
    }

    @PostMapping("/sendEmailDownLoadHistoryData")
    @ApiOperation("发送下载历史数据的邮件")
    public Result<Boolean> sendEmailDownLoadHistoryData(@RequestBody SendEmailDownLoadHistoryDataDTO sendEmailDownLoadHistoryDataDTO) {
        return opCostRuleDistributeService.sendEmailDownLoadHistoryData(sendEmailDownLoadHistoryDataDTO);
    }

    @GetMapping("/remoteStartPileData")
    @ApiOperation(value = "远程启动桩数据", notes = "远程启动桩数据")
    public Result<RemoteStartPileDataVO> remoteStartPileData(@RequestParam("sn") String sn) {
        return Result.ofSucceed(opLocationPileEvseService.getRemoteStartPileData(sn));
    }

    /**
     * @param pileSimpleInfoQueryDTO 充电桩信息的简单查询实体 入参模型
     * @return 充电桩信息
     * @function 充电桩信息的简单查询
     */
    @RequestMapping(method = {RequestMethod.POST}, value = "/pileSimpleInfoQuery")
    @ApiOperation(value = "充电桩信息的简单查询", notes = "充电桩信息的简单查询")
    public Result<List<PileSimpleInfoQueryVO>> pileSimpleInfoQuery(@RequestBody PileSimpleInfoQueryDTO pileSimpleInfoQueryDTO) {

        log.info("===>>>OpLocationPileEvseController.pileSimpleInfoQuery pileSimpleInfoQueryDTO : {}", JSON.toJSONString(pileSimpleInfoQueryDTO));

        return Result.ofSucceed(opLocationPileEvseService.pileSimpleInfoQuery(pileSimpleInfoQueryDTO));
    }

    /**
     * @param setPileEroamingDTO 设置充电桩的互联互通开关 入参模型
     * @return 操作结果
     * @function 设置充电桩的互联互通开关
     */
    @Deprecated
    @PostMapping("/setPileEroaming")
    @ApiOperation(value = "设置充电桩的互联互通开关", notes = "设置充电桩的互联互通开关")
    public Result<Boolean> setPileEroaming(@RequestBody SetPileEroamingDTO setPileEroamingDTO) {

        log.info("===>>>OpLocationPileEvseController.setPileEroaming setPileEroamingDTO : {}", JSON.toJSONString(setPileEroamingDTO));

        return Result.ofSucceed(opLocationPileEvseService.setPileEroaming(setPileEroamingDTO));
    }

    /**
     * @return 同步条数
     * @function 同步充电桩的公开属性标志
     */
    @GetMapping("/syncPilePublicProperty")
    @ApiOperation(value = "同步充电桩的公开属性标志", notes = "同步充电桩的公开属性标志")
    public Result<Integer> syncPilePublicProperty() {

        log.info("===>>> 同步充电桩的公开属性标志！");

        return Result.ofSucceed(opLocationPileEvseService.syncPilePublicProperty());
    }

    @PostMapping("/getPilesInfoByPileSn")
    @ApiOperation(value = "获取桩及其枪信息", notes = "获取桩及其枪信息")
    public Result<List<OpLocationEvseElasticDTO>> getPilesInfoByPileSn(@RequestBody Set<String> pileSnSet) {
        return Result.ofSucceed(opLocationEvseElastic.findAllByPileSnIn(pileSnSet));
    }

    @GetMapping("/ocppConfigurationViewEnable")
    @ApiOperation(value = "控制充电桩OCPP设置功能的视图展示")
    public Result<OcppConfigurationViewEnableVO> ocppConfigurationViewEnable(@RequestParam("pileSn") String pileSn) {

        log.info("===>>> OpLocationPileEvseController.ocppConfigurationViewEnable pileSn : {}",
                JSON.toJSONString(pileSn));

        return Result.ofSucceed(opLocationPileEvseService.ocppConfigurationViewEnable(pileSn));
    }

    @PostMapping("/clearPileSmartChargingProfile")
    @ApiOperation(value = "清除充电桩所有的SmartCharging Profile功能")
    public Result<Boolean> clearPileSmartChargingProfile(@RequestBody ClearSmartChargingProfileDTO clearSmartChargingProfileDTO) {

        log.info("===>>> OpLocationPileEvseController.clearPileSmartChargingProfile clearSmartChargingProfileDTO : {}",
                JSON.toJSONString(clearSmartChargingProfileDTO));

        return Result.ofSucceed(opLocationPileEvseService.clearPileSmartChargingProfile(clearSmartChargingProfileDTO));
    }

    @PostMapping("/getConfigurations")
    @ApiOperation(value = "实时获取桩配置")
    public Result<Boolean> getConfigurations(@RequestParam("sn") String sn) {
        return opLocationPileEvseService.getConfigurations(sn);
    }

    @PostMapping("/changeConfiguration")
    @ApiOperation(value = "实时更新桩配置")
    public Result<Boolean> changeConfiguration(@RequestBody ChangeConfigurationDTO changeConfigurationDTO) {

        log.info("===>>> OpLocationPileEvseController.changeConfiguration changeConfigurationDTO : {}",
                JSON.toJSONString(changeConfigurationDTO));

        return opLocationPileEvseService.changeConfiguration(changeConfigurationDTO);
    }

    @PostMapping("/sendLocalList")
    @ApiOperation(value = "设置白名单列表")
    public Result<Boolean> sendLocalList(@RequestBody LocalListInformationDTO localListInformationDTO) {
        return opLocationPileEvseService.sendLocalList(localListInformationDTO);
    }

    @PostMapping("/batchSendLocalList")
    @ApiOperation(value = "批量设置白名单列表")
    public Result<Boolean> batchSendLocalList(@RequestBody LocalListDTO localListDTO) {
        return opLocationPileEvseService.batchSendLocalList(localListDTO);
    }

    /**
     * @return
     * @function 同步充电桩名称
     */
    @GetMapping("/syncPileName")
    @ApiOperation(value = "同步充电桩名称", notes = "同步充电桩名称")
    public Result<Integer> syncPileName() {

        log.info("===>>> 同步充电桩名称！");

        return Result.ofSucceed(opLocationPileEvseService.syncPileName());
    }

    /**
     * @param pileSimpleInfoQueryDTOList
     * @return
     * @function 查询充电桩信息
     */
    @PostMapping("/queryPileInfo")
    @ApiOperation(value = "查询充电桩信息", notes = "查询充电桩信息")
    public Result<List<PileInfoVO>> queryPileInfo(@RequestBody List<PileSimpleInfoQueryDTO> pileSimpleInfoQueryDTOList) {

        log.info("===>>>OpLocationPileEvseController.queryPileInfo pileSimpleInfoQueryDTOList : {}", JSON.toJSONString(pileSimpleInfoQueryDTOList));

        return Result.ofSucceed(opLocationPileEvseService.queryPileInfo(pileSimpleInfoQueryDTOList));
    }

    /**
     * @param pileSnList
     * @return
     * @function 根据充电桩序列号批量查询充电桩基础信息
     */
    @PostMapping(value = "/getPileInfoListByPileSnList")
    @ApiOperation(value = "根据充电桩序列号批量查询充电桩基础信息", notes = "根据充电桩序列号批量查询充电桩基础信息")
    public Result<List<com.autel.cloud.pile.base.vo.pile.PileInfoVO>> getPileInfoListByPileSnList(@RequestBody List<String> pileSnList) {

        log.info("===>>>OpLocationPileEvseController.getPileInfoListByPileSnList pileSnList : {}", JSON.toJSONString(pileSnList));

        return Result.ofSucceed(opLocationPileEvseService.getPileInfoListByPileSnList(pileSnList));
    }

    @PostMapping("/updateEsSubscriptionStatusByPileSnList")
    @ApiOperation(value = "更新枪ES的订阅状态")
    public Result<Boolean> updateEsSubscriptionStatusByPileSnList(@RequestBody UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO) {
        return opLocationPileEvseService.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);
    }

    @PostMapping("/handleEvscpSettingDTO")
    @ApiOperation(value = "处理桩侧上报的英国法案参数")
    public Result<Boolean> handleEvscpSettingDTO(@RequestBody EvscpSettingDTO evscpSettingDTO) {

        log.info("===>>>OpLocationPileEvseController.handleEvscpSettingDTO evscpSettingDTO : {}",
                JSON.toJSONString(evscpSettingDTO));

        return Result.ofSucceed(opLocationPileEvseService.handleEvscpSettingDTO(evscpSettingDTO));
    }

    /**
     * @param pileSimpleInfoQueryDTO
     * @return
     * @function 获得充电桩基础信息（供运维使用）
     */
    @RequestMapping(method = {RequestMethod.POST}, value = "/getPileBasicInfoVOPage")
    @ApiOperation(value = "获得充电桩基础信息（供运维使用）", notes = "获得充电桩基础信息（供运维使用）")
    public Result<Page<PileBasicInfoVO>> getPileBasicInfoVOPage(@RequestBody SearchDTO pileSimpleInfoQueryDTO) {

        log.info("===>>>OpLocationPileEvseController.getPileBasicInfoVOPage pileSimpleInfoQueryDTO : {}",
                JSON.toJSONString(pileSimpleInfoQueryDTO));

        return Result.ofSucceed(opLocationPileEvseService.getPileBasicInfoVOPage(pileSimpleInfoQueryDTO));
    }

    @ApiOperation(value = "根据当前用户查询充电桩序列号列表")
    @GetMapping("/getPileSnList")
    public Result<List<String>> getPileSnList() {
        return Result.ofSucceed(opLocationPileEvseService.getPileSnList());
    }

    @GetMapping("/searchPileBySellerId")
    @ApiOperation(value = "通过商家id查找桩信息(营销规则调用)")
    public Result<List<UnionPileTreeVO>> searchPileBySellerId(@RequestParam("operatorId") Long operatorId,
                                                              @RequestParam(value = "locationId",required = false) Long locationId) {
        return Result.ofSucceed(opLocationPileEvseService.searchPileBySellerId(operatorId,locationId));
    }

    @GetMapping("/queryAdvertiseBaseInfo")
    @ApiOperation("桩详情-广告-基础信息查询")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "桩的sn", name = "pileSn", example = "A123131", paramType = "query")
    })
    public Result<AdvertiseBaseInfoVO> queryAdvertiseBaseInfo(@RequestParam("pileSn") String pileSn) {
        return opLocationPileEvseService.queryAdvertiseBaseInfo(pileSn);
    }

    @PostMapping("/queryAdvertiseList")
    @ApiOperation("桩详情-广告-广告信息查询")
    public Result<Page<AdvertiseListVO>> queryAdvertiseList(@RequestBody OpPileAdvertiseReq advertiseReq) {
        return opLocationPileEvseService.queryAdvertiseList(advertiseReq);
    }

    @GetMapping("/getZoneId")
    @ApiOperation("根据SN查询所属场站时区ID")
    public Result<String> getZoneIdByPileSn(@RequestParam("pileSn") @NotEmpty String pileSn) {
        return Result.ofSucceed(opLocationPileEvseService.getZoneIdByPileSn(pileSn));
    }

    @PostMapping("/searchPileByNameOrSn")
    @ApiOperation("根据桩名称或者sn模糊搜索桩")
    public Result<IPage<SearchPileByNameOrSnVO>> searchPileByNameOrSn(@RequestBody SearchPileByNameOrSnDTO searchEvseByNameOrSnDTO) {
        return Result.ofSucceed(opLocationPileEvseService.searchPileByNameOrSn(searchEvseByNameOrSnDTO));
    }


    @PostMapping("/queryBetaTestPileInfo")
    @ApiOperation(value = "查询可参与Beta测试计划的充电桩的信息（供运维使用）")
    public Result<List<BetaTestPileInfoVO>> queryBetaTestPileInfo(@RequestBody QueryBetaTestPileInfoDTO queryBetaTestPileInfoDTO) {

        log.info("===>>>OpLocationPileEvseController.queryBetaTestPileInfo queryBetaTestPileInfoDTO : {}",
                JSON.toJSONString(queryBetaTestPileInfoDTO));

        return Result.ofSucceed(opLocationPileEvseService.queryBetaTestPileInfo(queryBetaTestPileInfoDTO));
    }

    @PostMapping("/updatePileFreeVendInfo")
    @ApiOperation(value = "维护充电桩FreeVend相关信息")
    public Result<Boolean> updatePileFreeVendInfo(@RequestBody PileFreeVendInfoDTO pileFreeVendInfoDTO) {

        log.info("===>>> OpLocationPileEvseController.updatePileFreeVendInfo pileFreeVendInfoDTO : {}",
                JSON.toJSONString(pileFreeVendInfoDTO));

        return Result.ofSucceed(opLocationPileEvseService.updatePileFreeVendInfo(pileFreeVendInfoDTO));
    }

    @GetMapping("/getPileBriefInformation")
    @ApiOperation(value = "获得充电桩简要信息")
    public Result<PileInfoVO> getPileBriefInformation(@RequestParam("pileSn") String pileSn) {

        log.info("===>>> OpLocationPileEvseController.getPileBriefInformation pileSn : {}",
                JSON.toJSONString(pileSn));

        return Result.ofSucceed(opLocationPileEvseService.getPileBriefInformation(pileSn));
    }

    @GetMapping("/getEvseSnByIdAndGunCode")
    @ApiOperation("根据桩ID和枪号获取枪详情")
    public Result<OpLocationEvseElasticDTO> getEvseSnByIdAndGunCode(@RequestParam("id") String id, @RequestParam("gunCode") String gunCode) {
        return Result.ofSucceed(opLocationPileEvseService.getEvseSnByIdAndGunCode(id, gunCode));
    }

    @PostMapping("/searchPileListBySellerId")
    @ApiOperation(value = "通过商家/场站查找桩信息")
    public Result<List<OpLocationPileEvseDTO>> searchPileListBySellerId(@RequestBody QueryPileDTO queryPileDTO) {
        return Result.ofSucceed(opLocationPileEvseService.searchPileListBySellerId(queryPileDTO));
    }

    @PostMapping("/setPileEroamingForPile")
    @ApiOperation(value = "为桩(开启或者关闭)互联互通")
    public Result<Boolean> setPileEroamingForPile(@RequestBody SetPileEroamingForPileDTO setPileEroamingForPileDTO) {

        log.info("===>>> OpLocationPileEvseController.setPileEroamingForPile setPileEroamingForPileDTO : {}",
                JSON.toJSONString(setPileEroamingForPileDTO));

        return Result.ofSucceed(opLocationPileEvseService.setPileEroamingForPile(setPileEroamingForPileDTO));
    }

    @PostMapping("/selectDeviceInfoForPos")
    @ApiOperation(value = "为POS机查询设备信息")
    public Result<List<SelectDeviceInfoForPosVO>> selectDeviceInfoForPos(@RequestBody SelectDeviceInfoForPosDTO selectDeviceInfoForPosDTO) {

        log.info("===>>> OpLocationPileEvseController.selectDeviceInfoForPos selectDeviceInfoForPosDTO : {}",
                JSON.toJSONString(selectDeviceInfoForPosDTO));

        return Result.ofSucceed(opLocationPileEvseService.selectDeviceInfoForPos(selectDeviceInfoForPosDTO));
    }

    @GetMapping("/getPileLocationMap")
    @ApiOperation(value = "查询所有场站和桩关系",notes = "基础服务权限树维护调用")
    public Result<Map<String, List<CommonVO>>> getPileLocationMap() {
        return Result.ofSucceed(opLocationPileEvseService.getPileLocationMap());
    }

    @PostMapping("/selectChargingInfoForFleet")
    @ApiOperation(value = "为车队查询车辆充电的桩信息和充电信息", notes = "为车队查询车辆充电的桩信息和充电信息")
    Result<List<SelectChargingInfoForFleetVO>> selectChargingInfoForFleet(@RequestBody SelectChargingInfoForFleetDTO selectChargingInfoForFleetDTO) {

        log.info("===>>> OpLocationPileEvseController.selectChargingInfoForFleet selectChargingInfoForFleetDTO : {}",
                JSON.toJSONString(selectChargingInfoForFleetDTO));

        return Result.ofSucceed(opLocationPileEvseService.selectChargingInfoForFleet(selectChargingInfoForFleetDTO));
    }

    @GetMapping("/getPileDetailByPileId")
    @ApiOperation(value = "根据pileId查询设备信息", notes = "根据pileId查询设备信息")
    Result<OpPileEvseInfoVO> getPileDetailByPileId(@RequestParam("pileId") Long pileId) {
        log.info("===>>> OpLocationPileEvseController.getPileDetailByPileId param is  : {}",JSON.toJSONString(pileId));
        return Result.ofSucceed(opLocationEvse.getPileDetailByPileId(pileId));
    }
}
