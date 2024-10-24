package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.util.LocaleResultUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.model.DataAuthorizeDto;
import com.autel.cloud.base.model.Node;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.edge.vo.edge.PileBindVO;
import com.autel.cloud.fleet.vo.smartChargingPlan.ChargePlanVo;
import com.autel.cloud.mgnt.vo.DnmsgLatestValueVO;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.ChargePointNoticeEvent;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.convert.*;
import com.autel.cloud.pile.base.domain.model.PileQrCodeDTO;
import com.autel.cloud.pile.base.domain.repository.OpEvseBrandRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationConnectorRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantTerminalService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvse;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.lockOrUnlockGun.LockOrUnlockGunDTO;
import com.autel.cloud.pile.base.dto.oicp.ActionType;
import com.autel.cloud.pile.base.dto.oicp.EroamingEvseData;
import com.autel.cloud.pile.base.dto.oicp.TariffEvse;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.enums.chargepoint.OverchargingPileFlagEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.*;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.*;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.EdgeAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.FleetAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileDeviceServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileMonitorServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.dto.SyncPileInfoForPosDTO;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OicpFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OpsMgntFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.feign.vo.LockOrUnlockGunSupportInfoVO;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargePointMerchantRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationOperationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.infrastructure.util.CompetenceUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.location.PromptVO;
import com.autel.cloud.pile.base.vo.lockOrUnlockGun.LockOrUnlockGunResultVO;
import com.autel.cloud.pile.base.vo.lockOrUnlockGun.PileDeviceEvseStatusVO;
import com.autel.cloud.pile.bill.dto.CheckReserveDTO;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.feign.IChargingFeignClient;
import com.autel.cloud.pile.bill.vo.BillInfoForEvseVO;
import com.autel.cloud.pile.user.api.constant.RedisKeys;
import com.autel.cloud.pile.user.api.dto.DeleteDataReqDTO;
import com.autel.cloud.pile.user.api.dto.UserCompetenceDTO;
import com.autel.cloud.pile.user.api.enums.SubTreeEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.EroamingWhiteSellerVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.autel.cloud.pile.user.api.vo.UserCompetenceVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.compress.utils.Lists;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;

/**
 * @ClassName OpLocationEvseImpl
 * @Author A22121
 * @Description
 * @Date 2022/5/11 9:16
 * @Version 0.0.1-SNAPSHOT
 */
@RefreshScope
@Service
@Slf4j
public class OpLocationEvseImpl implements OpLocationEvse {
    @Resource
    private PileMonitorServiceAdapter pileMonitorServiceAdapter;
    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    private final OpLocationEvseRepository opLocationEvseRepository;
    @Resource
    private IChargingFeignClient chargingFeignClient;
    @Resource
    private MonitorFeignClient monitorFeignClient;
    @Resource
    private OpEvseBrandRepository opEvseBrandRepository;
    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Autowired
    private MonitorFeign monitorFeign;
    @Autowired
    private OpLocationConnectorRepository opLocationConnectorRepository;
    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;
    @Resource
    private OpLocationEvseExpandElastic opLocationEvseExpandElastic;
    @Autowired
    private DataServiceFeign dataServiceFeign;
    @Resource
    private OpsMgmtClient opsMgmtClient;
    @Autowired
    private RabbitTemplate rabbitTemplate;
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;
    @Resource
    private OpLocationOperationMapper opLocationOperationMapper;
    @Resource
    private OicpFeignClientProxy oicpFeignClient;
    @Autowired
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private OpLocationPileEvseElasticService opLocationPileEvseElasticService;
    @Resource
    private OpLocationElastic opLocationElastic;
    @Resource
    private OpLocationMapper opLocationMapper;
    @Resource
    private RedisUtil redisUtil;
    @Resource
    private IBillFeignClient iBillFeignClient;

    @Lazy
    @Resource
    private PileBaseAsync pileBaseAsync;

    @Autowired
    private PileUserFeign pileUserFeign;

    @Autowired
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private ProtocolFeignClient protocolFeignClient;

    @Value("${remoteStartPhoneNumber:00000000000}")
    private String remoteStartPhoneNumber;
    @Autowired
    private OpsMgntFeignClientProxy opsMgntFeignClientProxy;

    @Resource
    private ChargePointMerchantRelationMapper chargePointMerchantRelationMapper;

    @Value("${pile.remoteStart.enable:true}")
    private boolean remoteStartEnable;

    @Resource
    private ChargePointMerchantTerminalService chargePointMerchantTerminalService;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private PileDeviceServiceAdapter pileDeviceServiceAdapter;

    @Value("${hubject.enable:false}")
    private String hubjectEnable;

    @Autowired
    private SaasAccessFeignClient saasAccessFeignClient;
    @Resource
    private PileUserServiceFeign pileUserServiceFeign;

    @Autowired
    private FleetAdapter fleetAdapter;

    @Autowired
    private EdgeAdapter edgeAdapter;



    public OpLocationEvseImpl(OpLocationEvseRepository opLocationEvseRepository) {
        this.opLocationEvseRepository = opLocationEvseRepository;
    }

    @Override
    public Result<OpEvseInfoDTO> getPileInfoByEvseSn(String evseSn) {
        OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(evseSn);
        String pileSn = evseSn.split("_")[0];
        Result<OpPileEvseInfoVO> opPileEvseInfoVORet = this.getPileInfoByPileSn(pileSn);
        if (opPileEvseInfoVORet.getData() != null) {
            opEvseInfoDTO.setPileSn(opPileEvseInfoVORet.getData().getPileSn());
            opEvseInfoDTO.setPileName(opPileEvseInfoVORet.getData().getPileName());
        }
        return Result.ofSucceed(opEvseInfoDTO);
    }

    @Override
    public Result<List<OpLocationPileEvseVO>> getPileInfoByPileNameAndLocationId(String pileName, Long locationId) {
        List<OpLocationPileEvseEntity> pileInfoByPileNameAndLocationId = opLocationEvseRepository.getPileInfoByPileNameAndLocationId(pileName, locationId);
        List<OpLocationPileEvseVO> opLocationPileEvseVOS = OpLocationPileEvseTransfer.INSTANCE.entityList2VOList(pileInfoByPileNameAndLocationId);
        return Result.ofSucceed(opLocationPileEvseVOS);
    }


    @Override
    public Result<OpPileEvseInfoVO> getPileInfoByPileSn(String pileSn) {
        try {
            //
            Map<Integer, ChargePointLicenseVO> collect = new HashMap<>();
            boolean overFlag = false;
            Integer overchargingPileFlag = 0;

            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            boolean loginState = jwtInfo != null && jwtInfo.getPayload() != null && jwtInfo.getPayload().getSellerId() != null;
            if (loginState) {
                // 登录态逻辑
                LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
                queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, pileSn).eq(ChargePointMerchantRelationEntity::getMerchantId, LoginUserUtil.getSellerId());
                ChargePointVO chargePointVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2ChargePointVO(chargePointMerchantRelationMapper.selectOne(queryWrapper));

                // 判空
                if (chargePointVO != null) {
                    overchargingPileFlag = chargePointVO.getOverchargingPileFlag();
                    overFlag = Integer.valueOf(1).equals(overchargingPileFlag);
                }

                Map<String, List<Integer>> snGUnMap = new HashMap<>();
                if (overFlag) {
                    // 查找终端
                    List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(pileSn, LoginUserUtil.getSellerId());
                    List<String> terminalSnList = terminalEntityList.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());
                    terminalEntityList.forEach(item -> {
                        List<Connector> connectorsList = item.getConnectorsList();
                        snGUnMap.put(item.getTerminalSn(), connectorsList.stream().map(Connector::getConnectorId).collect(Collectors.toList()));
                    });
                    // 订阅状态映射关系
                    List<ChargePointLicenseVO> lastExpireTimeChargePointLicense = chargePointMerchantRelationService.findLastExpireTimeChargePointLicense(terminalSnList, PileChargingRights.OPERATION_SERVICE_ID_LIST, LoginUserHolder.getLoginUser().getPayload().getSellerId());
                    lastExpireTimeChargePointLicense.forEach(chargePointLicenseVO -> {
                        String sn = chargePointLicenseVO.getSn();
                        List<Integer> connectors = snGUnMap.get(sn);
                        connectors.forEach(connectorId -> collect.put(connectorId, chargePointLicenseVO));
                    });
                }

            }


            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticService.findOne(pileSn);
            if (opLocationPileEvseElasticDTO == null) {
                return Result.ofFailed(LocaleResultUtil.result("OpLocationEvse.getPileInfoByPileSn.notFoundPile"));
            }
            //获取场站hubject属性，默认为false
            boolean hubject = false;
            Integer businessType = null;
            if (opLocationPileEvseElasticDTO.getLocationId() != null) {
                Optional<OpLocationElasticDTO> byId = opLocationElastic.findById(opLocationPileEvseElasticDTO.getLocationId());
                if (byId.isPresent()) {
                    OpLocationElasticDTO opLocationElasticDTO = byId.get();
                    businessType = opLocationElasticDTO.getBusinessType();
                            log.info("OpLocationEvseImpl.getPileInfoByPileSn.byId.opLocationElasticDTO = {}", opLocationElasticDTO);
                    if (opLocationElasticDTO.getHubjectCheck() != null && opLocationElasticDTO.getHubjectCheck()) {
                        hubject = opLocationElasticDTO.getHubjectCheck();
                    }
                }
            }
            OpLocationPileEvseDTO opLocationPileEvseDTO = OpLocationPileEvseConvert.toOpLocationPileEvseDTO(opLocationPileEvseElasticDTO);
            OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(pileSn + "_1");
            if (opEvseInfoDTO == null) {
                return Result.ofFailed(LocaleResultUtil.result("OpLocationEvse.getPileInfoByPileSn.notFoundEvseList"));
            }
            Result<String> statusRet = monitorFeignClient.queryStatusByEvseSn(pileSn + "_0");
            String status = statusRet.getData();
            OpEvseBrandEntity opEvseBrandEntity = opEvseBrandRepository.getById(opLocationPileEvseDTO.getBrandId());
            //12月新版桩详情增加枪数统计
            int evseSize = 0;
            List<OpLocationEvseElasticDTO> list = opLocationEvseElastic.findAllByPileSn(pileSn);
            if (CollUtil.isNotEmpty(list)) {
                evseSize = list.size();
            }
            //获取桩SN网络连接状态
            List<String> connectType = getConnectType(pileSn);

            boolean eroamingEnable = false;
            if (jwtInfo != null
                    && jwtInfo.getPayload() != null
                    && jwtInfo.getPayload().getSellerId() != null) {
                EroamingWhiteSellerVO eroamingWhiteSellerVO = new EroamingWhiteSellerVO();
                eroamingWhiteSellerVO.setSellerId(jwtInfo.getPayload().getSellerId());
                Result<Boolean> eroamingEnableResult = pileUserFeign.eroamingEnable(eroamingWhiteSellerVO);

                log.info("===>>>OpLocationEvseImpl.getPileInfoByPileSn eroamingEnableResult : {}",
                        JSON.toJSONString(eroamingEnableResult));

                if (eroamingEnableResult != null
                        && Integer.valueOf(HttpStatus.HTTP_OK).equals(eroamingEnableResult.getCode())
                        && eroamingEnableResult.getData() != null) {
                    eroamingEnable = eroamingEnableResult.getData();
                }
            }

            OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseDTO,
                    opEvseInfoDTO,
                    status,
                    opEvseBrandEntity,
                    evseSize,
                    hubject,
                    connectType,
                    opLocationPileEvseElasticDTO,
                    eroamingEnable);

            if (org.apache.commons.lang3.StringUtils.isBlank(opPileEvseInfoVO.getPhase())) {
                ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>().eq(ChargePointMerchantRelationEntity::getSn, opPileEvseInfoVO.getPileSn()).last("limit 1"));
                log.info("桩详情接口,chargePointEntity={}", JSON.toJSONString(chargePointEntity));
                if (!ObjectUtils.isEmpty(chargePointEntity)) {
                    if ("AC_1_PHASE".equalsIgnoreCase(chargePointEntity.getPhases())) {
                        opPileEvseInfoVO.setPhase("1");
                    } else if ("AC_3_PHASE".equalsIgnoreCase(chargePointEntity.getPhases())) {
                        opPileEvseInfoVO.setPhase("3");
                    }
                }
            }
            //设置场站业务类型
            opPileEvseInfoVO.setBusinessType(businessType);

            LockOrUnlockGunSupportInfoVO lockOrUnlockGunSupportInfoVO = deviceServiceFeign.getLockOrUnlockGunSupportInfo(pileSn).getData();
            Boolean supportLockOrUnlockMark = false;
            Boolean requireToUpgradeEnabled = false;
            if(lockOrUnlockGunSupportInfoVO != null){
                supportLockOrUnlockMark = lockOrUnlockGunSupportInfoVO.isLockOrUnlockGunSupportEnabled();
                requireToUpgradeEnabled = lockOrUnlockGunSupportInfoVO.isRequireToUpgradEnabled();
            }
            opPileEvseInfoVO.setSupportLockOrUnlockMark(supportLockOrUnlockMark);
            opPileEvseInfoVO.setRequireToUpgradeEnabled(requireToUpgradeEnabled);

            List<Long> evseIds = JSON.parseArray(opLocationPileEvseDTO.getEvseList(), Long.class);
            List<OpEvseInfoDTO> evseList = opLocationEvseRepository.getEvseByIdList(evseIds);
            log.info("evseList->{}", JSON.toJSONString(evseList));
            List<String> evseSnList = evseList.stream().map(OpEvseInfoDTO::getEvseSn).collect(Collectors.toList());
            Result<Map<String, String>> mapResult = monitorFeignClient.queryStatusByEvseSnList(evseSnList);
            log.info("mapResult->{}", JSON.toJSONString(mapResult));
            Boolean finalSupportLockOrUnlockMark = supportLockOrUnlockMark;
            List<String> remoteStartGunList = new ArrayList<>();
            List<RemoteStopGunVO> remoteStopGunList = new ArrayList<>();
            AtomicReference<Boolean> canRemoteStop = new AtomicReference<>(false);
            boolean finalOverFlag = overFlag;
            List<OpEvseInfoVO> evseInfoList = evseList.stream().map(item -> {
                OpEvseInfoVO opEvseInfoVO = new OpEvseInfoVO();
                opEvseInfoVO.setEvseStatus(mapResult.getData().get(item.getEvseSn()));
                // 将原ocpp状态转换为新版状态
                opEvseInfoVO.setEvseStatusCode(LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(mapResult.getData().get(item.getEvseSn())).getCode());

                opEvseInfoVO.setId(item.getId());
                String gunNo = CommonUtil.getGunNoStringType(item.getEvseSn());
                String pileGunTransactionKey = RedisKeyConstant.getChargePileGunKey(pileSn, gunNo);
                String transactionMsg = stringRedisTemplate.opsForValue().get(pileGunTransactionKey);
                if(gunNo.length() == 1 ){
                    gunNo = "0" + gunNo;
                }
                opEvseInfoVO.setEvseNo(gunNo);
                opEvseInfoVO.setEvseSn(item.getEvseSn());
                opEvseInfoVO.setGunType(item.getGunType());
                opEvseInfoVO.setTariffId(item.getTariffId());

                if(finalSupportLockOrUnlockMark){
                    LockOrUnlockGunDTO lockOrUnlockGunDTO = new LockOrUnlockGunDTO();
                    lockOrUnlockGunDTO.setPileSn(pileSn);
                    lockOrUnlockGunDTO.setConnectorId(Integer.parseInt(item.getEvseSn().split("_")[1]));
                    LockOrUnlockGunResultVO lockOrUnlockGunResultVO = this.getCableEnable(lockOrUnlockGunDTO);
                    if (lockOrUnlockGunResultVO != null
                            && lockOrUnlockGunResultVO.getEvseStatus() != null) {
                        opEvseInfoVO.setGunStatus(lockOrUnlockGunResultVO.getEvseStatus());
                    }
                }

                if (remoteStartEnable  && item.getTariffId() != null
                        && StringUtils.isBlank(transactionMsg)
                        && StringUtils.isNotBlank(opEvseInfoVO.getEvseStatus())
                        && (opEvseInfoVO.getEvseStatus().equalsIgnoreCase(EvseDeviceStatusEnum.AVAILABLE.getName())
                        || opEvseInfoVO.getEvseStatus().equalsIgnoreCase(EvseDeviceStatusEnum.FINISHING.getName())
                        || opEvseInfoVO.getEvseStatus().equalsIgnoreCase(EvseDeviceStatusEnum.PREPARING.getName()))) {
                    remoteStartGunList.add(gunNo);
                }

                // TODO: 在这里改造终端的订阅状态
                if (loginState) {
                    if (isWhiteBrand(LoginUserUtil.getSellerId())) {
                        opEvseInfoVO.setTerminalEffectiveDays(365L);
                        opEvseInfoVO.setTerminalSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
                    }
                } else {
                    if (finalOverFlag) {
                        if (CollUtil.isNotEmpty(collect) && collect.containsKey(Integer.valueOf(item.getEvseSn().split("_")[1]))) {
                            opEvseInfoVO.setTerminalEffectiveDays(collect.get(Integer.valueOf(item.getEvseSn().split("_")[1])).getEffectiveDays());
                            opEvseInfoVO.setTerminalSubscriptionStatus(collect.get(Integer.valueOf(item.getEvseSn().split("_")[1])).getSubscriptionStatus());
                        } else {
                            opEvseInfoVO.setTerminalSubscriptionStatus(SubStatus.INACTIVITY.getStatus());
                        }
                    }
                }

                RemoteStopGunVO remoteStopGunVO = RemoteStopGunVO.builder().gunNo(gunNo).build();
                remoteStopGunVO.setStopEnabled(false);
                if(remoteStartEnable && StringUtils.isNotBlank(transactionMsg) &&
                        StringUtils.isNotBlank(opEvseInfoVO.getEvseStatus()) &&
                        (opEvseInfoVO.getEvseStatus().equals(EvseDeviceStatusEnum.CHARGING.getName()) ||
                                opEvseInfoVO.getEvseStatus().equals(EvseDeviceStatusEnum.SUSPENDED_EV.getName()) ||
                                        opEvseInfoVO.getEvseStatus().equals(EvseDeviceStatusEnum.SUSPENDED_EVSE.getName()))){
                    remoteStopGunVO.setStopEnabled(true);
                    canRemoteStop.set(true);
                }
                remoteStopGunList.add(remoteStopGunVO);
                return opEvseInfoVO;
            }).sorted(Comparator.comparing(o->new Integer((o.getEvseSn().split("_")[1])))).collect(Collectors.toList());

            Result<Map<String, Boolean>> result = deviceServiceFeign.judgePileSupportLockOrUnlockGun(pileSn);

            log.info("===>>> OpLocationEvseImpl getPileInfoByPileSn result : {}", JSON.toJSONString(result));

            remoteStopGunList.sort(Comparator.comparing(o-> new Integer((o.getGunNo()))));
            opPileEvseInfoVO.setCanRemoteStart(!remoteStartGunList.isEmpty());
            opPileEvseInfoVO.setCanRemoteStop(canRemoteStop.get());
            opPileEvseInfoVO.setRemoteStopGunList(remoteStopGunList);
            opPileEvseInfoVO.setEvseInfoList(evseInfoList);
            opPileEvseInfoVO.setIdentificationCode(String.valueOf(opLocationPileEvseElasticDTO.getId()));
            opPileEvseInfoVO.setRemoteStartPhoneNumber(remoteStartPhoneNumber);
            log.info("getPileInfoByPileSn, OverchargingPileFlag: {}", overchargingPileFlag);
            opPileEvseInfoVO.setOverchargingPileFlag(overchargingPileFlag);
            return Result.ofSucceed(opPileEvseInfoVO);
        } catch (Exception e) {
            log.error("OpLocationEvse getPileInfoByPileSn exception = ", e);
            return Result.ofFailed(LocaleResultUtil.result("OpLocationEvse.getPileInfoByPileSn.exception"));
        }
    }

    /**
     * 获取桩SN网络连接状态
     *
     * @param pileSn
     * @return
     */
    private List<String> getConnectType(String pileSn) {

        List<String> networkList = new ArrayList<>();
        DnmsgLatestValueVO dnmsgLatestValueVO = opsMgntFeignClientProxy.getPileNetworkSetting(pileSn);

        if(dnmsgLatestValueVO != null){
            networkList = getConnectTypeList(dnmsgLatestValueVO);
        }

        log.info("OpLocationEvse getPileInfoByPileSn:{}, connectType = {}", pileSn , JSON.toJSONString(networkList));
        return networkList;
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
        if(opsMgntFeignClientProxy.getEthernetEnable(dnmsgLatestValueVO)){
            networkList.add("ethernetEnable");
        }
        return networkList;
    }

    @Override
    public List<OpEvseEnableVO> getAbleDisAbleList(String pileSn) {
        List<OpEvseEnableVO> result = Lists.newArrayList();
        try {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticService.findOne(pileSn);
            if (opLocationPileEvseElasticDTO == null) {
                return result;
            }

            Result<String> statusRet = monitorFeignClient.queryStatusByEvseSn(pileSn + "_0");
            String status = statusRet.getData();
            OpEvseEnableVO pileStatus = new OpEvseEnableVO();
            pileStatus.setStatus(LocationEVSEStatusEnum.UNAVAILABLE.equals(LocationEVSEStatusEnum.getEnumByName(status)) ? 1 : 0);
            pileStatus.setSn(pileSn);
            pileStatus.setType(0);
            result.add(pileStatus);
            log.info("getAbleDisAbleList--pile status->{}", JSON.toJSONString(opLocationPileEvseElasticDTO));
            if (StringUtils.isBlank(opLocationPileEvseElasticDTO.getEvseList())) {
                return result;
            }
            List<Long> evseIds = JSON.parseArray(opLocationPileEvseElasticDTO.getEvseList(), Long.class);
            List<OpEvseInfoDTO> evseList = opLocationEvseRepository.getEvseByIdList(evseIds);
            log.info("evseList->{}", JSON.toJSONString(evseList));
            List<String> evseSnList = evseList.stream().map(OpEvseInfoDTO::getEvseSn).collect(Collectors.toList());
            Result<Map<String, String>> mapResult = monitorFeignClient.queryStatusByEvseSnList(evseSnList);
            log.info("mapResult->{}", JSON.toJSONString(mapResult));
            for (OpEvseInfoDTO opEvseInfoDTO : evseList) {
                OpEvseEnableVO opEvseEnableVO = new OpEvseEnableVO();
                opEvseEnableVO.setStatus(LocationEVSEStatusEnum.UNAVAILABLE.equals(LocationEVSEStatusEnum.getEnumByName(mapResult.getData().get(opEvseInfoDTO.getEvseSn()))) ? 1 : 0);
                opEvseEnableVO.setSn(opEvseInfoDTO.getEvseSn());
                opEvseEnableVO.setType(1);
                result.add(opEvseEnableVO);
            }
            result = result.stream().sorted(Comparator.comparing(OpEvseEnableVO::getSn)).collect(Collectors.toList());
        } catch (Exception e) {
            log.error("OpLocationEvse getAbleDisAbleList exception = ", e);
        }
        return result;
    }

    @Override
    public boolean checkAllEvseEnable(String pileSn) {
        try {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticService.findOne(pileSn);
            if (opLocationPileEvseElasticDTO == null) {
                return Boolean.FALSE;
            }

            log.info("getAbleDisAbleList--pile status->{}", JSON.toJSONString(opLocationPileEvseElasticDTO));
            if (StringUtils.isBlank(opLocationPileEvseElasticDTO.getEvseList())) {
                return Boolean.TRUE;
            }
            List<Long> evseIds = JSON.parseArray(opLocationPileEvseElasticDTO.getEvseList(), Long.class);
            List<OpEvseInfoDTO> evseList = opLocationEvseRepository.getEvseByIdList(evseIds);
            log.info("checkAllEvseEable.evseList->{}", JSON.toJSONString(evseList));
            List<String> evseSnList = evseList.stream().map(OpEvseInfoDTO::getEvseSn).collect(Collectors.toList());
            Result<Map<String, String>> mapResult = monitorFeignClient.queryStatusByEvseSnList(evseSnList);
            log.info("checkAllEvseEable.mapResult->{}", JSON.toJSONString(mapResult));
            for (OpEvseInfoDTO opEvseInfoDTO : evseList) {
                EvseDeviceStatusEnum evseStatus = EvseDeviceStatusEnum.getEnumByName(mapResult.getData().get(opEvseInfoDTO.getEvseSn()));
                if (!(EvseDeviceStatusEnum.AVAILABLE.equals(evseStatus) || EvseDeviceStatusEnum.FAULTED.equals(evseStatus))) {
                    return Boolean.FALSE;
                }
            }
        } catch (Exception e) {
            log.error("OpLocationEvse checkAllEvseEable exception = ", e);
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    @Override
    public Page<PileQrCodeDTO> getPileQrCodePage(String keyWord, String locationId, int page, int pageSize) {
        return opLocationPileEvseRepository.getPileQrCodePage(keyWord, locationId,page, pageSize);
    }

    @Override
    public OpPileEvseInfoVO getPileDetailByPileId(Long pileId) {
        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getById(pileId);
        if(null == opLocationPileEvseEntity || 1 == opLocationPileEvseEntity.getDeleted()){
            return null;
        }
        OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseEntity);
        //处理品牌
        if(null != opPileEvseInfoVO.getBrandId()){
            OpEvseBrandEntity opEvseBrandEntity = opEvseBrandRepository.getById(opPileEvseInfoVO.getBrandId());
            if (opEvseBrandEntity != null) {
                opPileEvseInfoVO.setBrandName(opEvseBrandEntity.getName());
                opPileEvseInfoVO.setThirdPart(opEvseBrandEntity.getThirdPart());
            }
        }
        if(StringUtils.isNotBlank(opLocationPileEvseEntity.getEvseList())){
            String evseList = opLocationPileEvseEntity.getEvseList();
            List<Long> evseIds = JSON.parseArray(evseList, Long.class);
            List<OpLocationEvseDTO> opLocationEvseDTOS = new ArrayList<>();
            evseIds.forEach(e -> {
                OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(e);
                opLocationEvseDTOS.add(opLocationEvseDTO);
            });
            opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
        }
        return opPileEvseInfoVO;
    }

    @Override
    public Result<List<OpPileEvseInfoVO>> getPileInfoByPileSnList(List<String> pileSnList) {
        List<OpPileEvseInfoVO> list = new ArrayList<>();
        try {
            List<OpLocationPileEvseDTO> opLocationPileEvseDTOList = opLocationPileEvseRepository.queryByPileSnList(pileSnList);
            if (opLocationPileEvseDTOList == null) {
                return Result.ofFailed(LocaleResultUtil.result("OpLocationEvse.getPileInfoByPileSn.notFoundPile"));
            }
            for (OpLocationPileEvseDTO item : opLocationPileEvseDTOList) {
                list.add(OpLocationPileEvseConvert.toOpPileEvseInfoVO(item));
            }
            return Result.ofSucceed(list);
        } catch (Exception e) {
            log.error("OpLocationEvse getPileInfoByPileSn exception = ", e);
            return Result.ofFailed(LocaleResultUtil.result("OpLocationEvse.getPileInfoByPileSn.exception"));
        }
    }

    private BillInfoForEvseVO getBillInfoForEvseVO(Long evseId) {
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

    private List<BillInfoForEvseVO> getHistoryBillInfoForEvseVO(OpEvseInfoDTO opEvseInfoDTO) {
        try {
            Result<PageVO<BillInfoForEvseVO>> getHistoryOrderInfoRet = chargingFeignClient
                    .getHistoryOrderInfo(opEvseInfoDTO.getId(), 10, 1);
            log.info("OpLocationEvseImpl getHistoryBillInfoForEvseVO and getHistoryOrderInfoRet = "
                    + JSON.toJSONString(getHistoryOrderInfoRet));
            return getHistoryOrderInfoRet.getData().getContent();
        } catch (Exception e) {
            log.error("OpLocationEvseImpl.getHistoryBillInfoForEvseVO exception = ", e);
            return Collections.emptyList();
        }
    }

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 获得充电枪的状态（返回此时充电枪处于锁枪状态还是解锁枪状态的状态标志）
     */
    private LockOrUnlockGunResultVO getCableEnable(LockOrUnlockGunDTO lockOrUnlockGunDTO) {

        log.info("===>>>OpLocationEvseImpl.getCableEnable lockOrUnlockGunDTO : {}", JSON.toJSONString(lockOrUnlockGunDTO));

        // 充电桩序列号
        @NotNull String pileSn = lockOrUnlockGunDTO.getPileSn();
        // 枪号
        @NotNull Integer connectorId = lockOrUnlockGunDTO.getConnectorId();
        // 充电枪序列号
        String evseSn = pileSn + "_" + String.valueOf(connectorId);

        // 调用device服务获取充电枪的状态（此时充电枪处于锁枪状态还是解锁枪状态）
        Result<PileDeviceEvseStatusVO> result = deviceServiceFeign.queryPileDeviceEvseStatus(evseSn);

        log.info("===>>>OpLocationEvseImpl.getCableEnable result : {}", JSON.toJSONString(result));

        if (result != null
                && HttpStatus.HTTP_OK == result.getCode()
                && result.getData() != null
                && result.getData().getEvseStatus() != null) {
            PileDeviceEvseStatusVO pileDeviceEvseStatusVO = result.getData();
            LockOrUnlockGunResultVO lockOrUnlockGunResultVO = new LockOrUnlockGunResultVO();
            lockOrUnlockGunResultVO.setPileSn(pileSn);
            lockOrUnlockGunResultVO.setConnectorId(connectorId);
            lockOrUnlockGunResultVO.setEvseStatus(pileDeviceEvseStatusVO.getEvseStatus());
            return lockOrUnlockGunResultVO;
        } else {
            // todo 查询不到时，需要向充电桩发送获取充电枪当前状态的命令，同步最新的状态到表中
            Result<String> monitorFeignResult = monitorFeignClient.queryStatusByEvseSn(evseSn);

            log.info("===>>>OpLocationEvseImpl.getCableEnable monitorFeignResult : {}", JSON.toJSONString(monitorFeignResult));

            if (monitorFeignResult != null
                    && HttpStatus.HTTP_OK == monitorFeignResult.getCode()
                    && !EvseDeviceStatusEnum.DEFAULT.getName().equalsIgnoreCase(monitorFeignResult.getData())) {

                lockOrUnlockGunDTO.setNeedToPushMark(false);

                // 调用协议服务，发送获得充电枪的状态的命令
                Result<Boolean> booleanResult = protocolFeignClient.getCableEnable(lockOrUnlockGunDTO);

                log.info("===>>>OpLocationEvseImpl.getCableEnable booleanResult : {}", JSON.toJSONString(booleanResult));

            }
            return null;
        }
    }

    @Override
    public com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getEvseListByPileSn(String pileSn) {

        log.info("===>>> OpLocationEvseImpl.getEvseListByPileSn pileSn : {}", JSON.toJSONString(pileSn));

        Result<Map<String, Boolean>> result = deviceServiceFeign.judgePileSupportLockOrUnlockGun(pileSn);

        log.info("===>>> OpLocationEvseImpl.getEvseListByPileSn result : {}", JSON.toJSONString(result));

        Boolean supportLockOrUnlockMark = false;
        if (result != null && HttpStatus.HTTP_OK == result.getCode()) {
            Map<String, Boolean> map = result.getData();
            if (ObjectUtils.isNotEmpty(map)) {
                supportLockOrUnlockMark = map.get(pileSn);
            }
        }
        try {
            List<DataAuthorizeDto> permissionData = new ArrayList<>();
            List<OpEvseInfoVO> retList = new ArrayList<>();
            //OpLocationPileEvseDTO opLocationPileEvseDTO = opLocationPileEvseRepository.queryByPileSn(pileSn);
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticService.findOne(pileSn);
            if (opLocationPileEvseElasticDTO == null) {
                return CompetenceUtil.encapsulationNotFound(retList,permissionData);
            }
            OpLocationPileEvseDTO opLocationPileEvseDTO = OpLocationPileEvseConvert.toOpLocationPileEvseDTO(opLocationPileEvseElasticDTO);
            JSONArray array = JSON.parseArray(opLocationPileEvseDTO.getEvseList());
            Boolean finalSupportLockOrUnlockMark = supportLockOrUnlockMark;
            array.forEach(item -> {
                OpEvseInfoVO opEvseInfoVO = new OpEvseInfoVO();
                OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseById(Long.parseLong(item.toString()));

                Result<String> statusRet = monitorFeignClient.queryStatusByEvseSn(opEvseInfoDTO.getEvseSn());
                String status = statusRet.getData();
                opEvseInfoVO.setEvseStatus(status);
                opEvseInfoVO.setEvseStatusCode(EvseDeviceStatusEnum.getEnumByName(status).getCode());
                BillInfoForEvseVO billInfoForEvseVO = getBillInfoForEvseVO(opEvseInfoDTO.getId());
                // 获取当前充电信息
                opEvseInfoVO.setCurrentBillInfo(billInfoForEvseVO);
                // 获取历史充电信息
                opEvseInfoVO.setHistoryBillInfoList(getHistoryBillInfoForEvseVO(opEvseInfoDTO));

                // 获取当前充电桩上报信息
                Result<OpEvseMeterUploadDTO> queryNewMeterByEvseSnRet = monitorFeignClient
                        .queryNewMeterByEvseSn(opEvseInfoDTO.getEvseSn());
                log.info("OpLocationEvseImpl getEvseListByPileSn and queryNewMeterByEvseSnRet = "
                        + JSON.toJSONString(queryNewMeterByEvseSnRet));
                // TODO 获取枪的额定电流、电压、功率
                if (queryNewMeterByEvseSnRet.getData() != null) {
                    OpEvseCurrentMeterDTO opEvseCurrentMeterDTO = new OpEvseCurrentMeterDTO();
                    buildMonitorData(queryNewMeterByEvseSnRet.getData());
                    BeanUtils.copyProperties(queryNewMeterByEvseSnRet.getData(), opEvseCurrentMeterDTO);
                    opEvseCurrentMeterDTO.setMaxPower(opEvseInfoDTO.getPower());
                    opEvseCurrentMeterDTO.setMaxBatterySoc(100);
                    if (opEvseInfoDTO.getAmperage() == null || opEvseInfoDTO.getAmperage().doubleValue() < 1) {
                        opEvseCurrentMeterDTO.setMaxCurrent(600D);
                    } else {
                        opEvseCurrentMeterDTO.setMaxCurrent(opEvseInfoDTO.getAmperage().doubleValue());
                    }
                    if (opEvseInfoDTO.getVoltage() == null || opEvseInfoDTO.getVoltage().doubleValue() < 1) {
                        opEvseCurrentMeterDTO.setMaxVoltage(1200D);
                    } else {
                        opEvseCurrentMeterDTO.setMaxVoltage(opEvseInfoDTO.getVoltage().doubleValue());
                    }
                    opEvseInfoVO.setCurrentMeterDTO(opEvseCurrentMeterDTO);
                }

                // 获取当前充电桩上报信息记录
                log.info("OpLocationEvseImpl getEvseListByPileSn and billInfoForEvseVO = " + JSON.toJSONString(billInfoForEvseVO));
                if (billInfoForEvseVO != null) {
                    List<OpEvseMeterUploadDTO> opEvseMeterUploadDTOS = pileMonitorServiceAdapter.queryMeterByBusId(billInfoForEvseVO.getOrderSeq(), billInfoForEvseVO.getStartDateTime());

//                    Result<List<OpEvseMeterUploadDTO>> queryMeterByEvseSnRet = monitorFeignClient
//                            .queryMeterByEvseSnAndStart(opEvseInfoDTO.getEvseSn(), billInfoForEvseVO.getStartDateTime());
//                    log.info("OpLocationEvseImpl getEvseListByPileSn and queryMeterByEvseSnRet = "
//                            + JSON.toJSONString(queryMeterByEvseSnRet));
                    opEvseMeterUploadDTOS.forEach(this::buildMonitorData);
                    List<OpEvseMeterUploadDTO> list = opEvseMeterUploadDTOS.stream()
                            .sorted(Comparator.comparing(OpEvseMeterUploadDTO::getCreateTime)).collect(Collectors.toList());
                    List<OpEvseMeterUploadDTO> currentList = list.stream()
                            .filter(meterUploadDTO -> meterUploadDTO.getCreateTime() > billInfoForEvseVO.getStartDateTime()).collect(Collectors.toList());
                    opEvseInfoVO.setHistoryMeterInfoList(currentList);
                } else {
                    opEvseInfoVO.setHistoryMeterInfoList(Collections.emptyList());
                }
                opEvseInfoVO.setId(opEvseInfoDTO.getId());
                opEvseInfoVO.setEvseNo("0" + opEvseInfoDTO.getEvseSn().split("_")[1]);
                opEvseInfoVO.setEvseSn(opEvseInfoDTO.getEvseSn());
                opEvseInfoVO.setGunType(opEvseInfoDTO.getGunType());
                //计费规则关联到枪
                opEvseInfoVO.setTariffId(opEvseInfoDTO.getTariffId());
                opEvseInfoVO.setSupportLockOrUnlockMark(finalSupportLockOrUnlockMark);
                retList.add(opEvseInfoVO);
            });
            // 如果充电枪支持此功能，就返回其状态
            for (OpEvseInfoVO opEvseInfoVO : retList) {
                if (opEvseInfoVO.getSupportLockOrUnlockMark() != null && opEvseInfoVO.getSupportLockOrUnlockMark()) {
                    String evseSn = opEvseInfoVO.getEvseSn();
                    if (org.apache.commons.lang3.StringUtils.isNotBlank(evseSn) && evseSn.contains("_")) {
                        LockOrUnlockGunDTO lockOrUnlockGunDTO = new LockOrUnlockGunDTO();
                        lockOrUnlockGunDTO.setPileSn(evseSn.split("_")[0]);
                        lockOrUnlockGunDTO.setConnectorId(Integer.valueOf(evseSn.split("_")[1]));
                        LockOrUnlockGunResultVO lockOrUnlockGunResultVO = this.getCableEnable(lockOrUnlockGunDTO);
                        if (lockOrUnlockGunResultVO != null
                                && lockOrUnlockGunResultVO.getEvseStatus() != null) {
                            opEvseInfoVO.setGunStatus(lockOrUnlockGunResultVO.getEvseStatus());
                        }
                    }
                }
            }
            if (!CollectionUtils.isEmpty(retList)) {
                boolean sellerAdmin = LoginUserUtil.isSellerAdmin();
                Payload payload = LoginUserUtil.getPayload();
                Long sellerId = payload.getSellerId();
                Long userId = payload.getUserId();
                if (!sellerAdmin) {
                    String key = RedisKeys.getUserCompetence(userId,sellerId);
                    String competence = stringRedisTemplate.opsForValue().get(key);
                    if (StringUtils.isBlank(competence)) {
                        UserCompetenceDTO param = new UserCompetenceDTO();
                        param.setSellerId(sellerId);
                        param.setUserId(userId);
                        UserCompetenceVO competenceVO = this.pileUserServiceFeign.getUserCompetence(param).getData();
                        if (competenceVO != null && competenceVO.getCompetence() != null) {
                            competence = competenceVO.getCompetence().toString();
                        }
                    }
                    sellerAdmin = "1".equals(competence);
                }
                DataAuthorizeDto dto = new DataAuthorizeDto();
                dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                dto.setAppId(CompetenceUtil.APP_ID);
                dto.setSellerId(sellerId.toString());
                dto.setUserId(userId.toString());
                List<Node> data = new ArrayList<>();
                if (sellerAdmin) {
                    Node sellerNode = new Node();
                    sellerNode.setLevel(SubTreeEnum.SELLER.getCode());
                    sellerNode.setNode(sellerId.toString());
                    List<Node> locationNodeList = new ArrayList<>();
                    Node locationNode = new Node();
                    locationNode.setLevel(SubTreeEnum.STATION.getCode());
                    locationNode.setNode(String.valueOf(opLocationPileEvseElasticDTO.getLocationId()));
                    locationNodeList.add(locationNode);
                    sellerNode.setChildNodes(locationNodeList);
                    data.add(sellerNode);
                }else {
                    Node node = new Node();
                    node.setLevel(SubTreeEnum.STATION.getCode());
                    node.setNode(String.valueOf(opLocationPileEvseElasticDTO.getLocationId()));
                    data.add(node);
                }
                dto.setData(data);
                permissionData.add(dto);
            }
            return CompetenceUtil.encapsulation(retList,permissionData);
        } catch (Exception e) {
            log.error("OpLocationEvse getEvseListByPileSn exception = ", e);
            return CompetenceUtil.encapsulationError(new ArrayList<>(),new ArrayList<>());
        }
    }

    @Override
    public com.autel.cloud.base.model.Result<List<OpEvseInfoVO>> getHistoryMeterInfo(String pileSn) {
        try {
            List<DataAuthorizeDto> permissionData = new ArrayList<>();
            List<OpEvseInfoVO> retList = new ArrayList<>();
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticService.findOne(pileSn);
            if (opLocationPileEvseElasticDTO == null) {
                return CompetenceUtil.encapsulationNotFound(retList,permissionData);
            }
            OpLocationPileEvseDTO opLocationPileEvseDTO = OpLocationPileEvseConvert.toOpLocationPileEvseDTO(opLocationPileEvseElasticDTO);
            JSONArray array = JSON.parseArray(opLocationPileEvseDTO.getEvseList());
            array.forEach(item -> {
                OpEvseInfoVO opEvseInfoVO = new OpEvseInfoVO();
                OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseById(Long.parseLong(item.toString()));
                Result<String> statusRet = monitorFeignClient.queryStatusByEvseSn(opEvseInfoDTO.getEvseSn());
                String status = statusRet.getData();
                opEvseInfoVO.setEvseStatus(status);
                // 将原ocpp状态转换为新版状态
                opEvseInfoVO.setEvseStatusCode(LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(status).getCode());

                BillInfoForEvseVO billInfoForEvseVO = getBillInfoForEvseVO(opEvseInfoDTO.getId());

                // 获取当前充电桩上报信息记录
                log.info("OpLocationEvseImpl getEvseListByPileSn and billInfoForEvseVO = " + JSON.toJSONString(billInfoForEvseVO));
                if (billInfoForEvseVO != null) {
//                    Result<List<OpEvseMeterUploadDTO>> queryMeterByEvseSnRet = monitorFeignClient
//                            .queryMeterByEvseSnAndStart(opEvseInfoDTO.getEvseSn(), billInfoForEvseVO.getStartDateTime());
//                    log.info("OpLocationEvseImpl getEvseListByPileSn and queryMeterByEvseSnRet = "
//                            + JSON.toJSONString(queryMeterByEvseSnRet));
                    List<OpEvseMeterUploadDTO> opEvseMeterUploadDTOS = pileMonitorServiceAdapter.queryMeterByBusId(billInfoForEvseVO.getOrderSeq(), billInfoForEvseVO.getStartDateTime());

//                    Result<List<OpEvseMeterUploadDTO>> queryMeterByEvseSnRet = monitorFeignClient
//                            .queryMeterByEvseSnAndStart(opEvseInfoDTO.getEvseSn(), billInfoForEvseVO.getStartDateTime());
//                    log.info("OpLocationEvseImpl getEvseListByPileSn and queryMeterByEvseSnRet = "
//                            + JSON.toJSONString(queryMeterByEvseSnRet));
                    opEvseMeterUploadDTOS.forEach(this::buildMonitorDataV2);

                    List<OpEvseMeterUploadDTO> list = opEvseMeterUploadDTOS.stream()
                            .sorted(Comparator.comparing(OpEvseMeterUploadDTO::getCreateTime)).collect(Collectors.toList());

                    List<OpEvseMeterUploadDTO> currentList = list.stream()
                            .filter(meterUploadDTO -> meterUploadDTO.getCreateTime() > billInfoForEvseVO.getStartDateTime()).collect(Collectors.toList());

                    opEvseInfoVO.setHistoryMeterInfoList(currentList);
                } else {
                    opEvseInfoVO.setHistoryMeterInfoList(Collections.emptyList());
                }
                opEvseInfoVO.setId(opEvseInfoDTO.getId());
                opEvseInfoVO.setEvseNo("0" + CommonUtil.getGunNoStringType(opEvseInfoDTO.getEvseSn()));
                opEvseInfoVO.setEvseSn(opEvseInfoDTO.getEvseSn());
                opEvseInfoVO.setGunType(opEvseInfoDTO.getGunType());
                retList.add(opEvseInfoVO);
            });


            if (!CollectionUtils.isEmpty(retList)) {
                boolean sellerAdmin = LoginUserUtil.isSellerAdmin();
                Payload payload = LoginUserUtil.getPayload();
                Long sellerId = payload.getSellerId();
                Long userId = payload.getUserId();
                if (!sellerAdmin) {
                    String key = RedisKeys.getUserCompetence(userId,sellerId);
                    String competence = stringRedisTemplate.opsForValue().get(key);
                    if (StringUtils.isBlank(competence)) {
                        UserCompetenceDTO param = new UserCompetenceDTO();
                        param.setSellerId(sellerId);
                        param.setUserId(userId);
                        UserCompetenceVO competenceVO = this.pileUserServiceFeign.getUserCompetence(param).getData();
                        if (competenceVO != null && competenceVO.getCompetence() != null) {
                            competence = competenceVO.getCompetence().toString();
                        }
                    }
                    sellerAdmin = "1".equals(competence);
                }
                DataAuthorizeDto dto = new DataAuthorizeDto();
                dto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
                dto.setAppId(CompetenceUtil.APP_ID);
                dto.setSellerId(sellerId.toString());
                dto.setUserId(userId.toString());
                List<Node> data = new ArrayList<>();
                if (sellerAdmin) {
                    Node sellerNode = new Node();
                    sellerNode.setLevel(SubTreeEnum.SELLER.getCode());
                    sellerNode.setNode(sellerId.toString());
                    List<Node> locationNodeList = new ArrayList<>();
                    Node locationNode = new Node();
                    locationNode.setLevel(SubTreeEnum.STATION.getCode());
                    locationNode.setNode(String.valueOf(opLocationPileEvseElasticDTO.getLocationId()));
                    locationNodeList.add(locationNode);
                    sellerNode.setChildNodes(locationNodeList);
                    data.add(sellerNode);
                }else {
                    Node node = new Node();
                    node.setLevel(SubTreeEnum.STATION.getCode());
                    node.setNode(String.valueOf(opLocationPileEvseElasticDTO.getLocationId()));
                    data.add(node);
                }
                dto.setData(data);
                permissionData.add(dto);
            }
            return CompetenceUtil.encapsulation(retList,permissionData);
        } catch (Exception e) {
            log.error("OpLocationEvse getEvseListByPileSn exception = ", e);
            return CompetenceUtil.encapsulationError(new ArrayList<>(),new ArrayList<>());
        }

    }

    /**
     * 处理数据
     *
     * @param opEvseMeterUploadDTO
     */
    private void buildMonitorDataV2(OpEvseMeterUploadDTO opEvseMeterUploadDTO) {
        if (opEvseMeterUploadDTO.getVoltage() != null) {
            opEvseMeterUploadDTO.setVoltage(BigDecimal.valueOf(opEvseMeterUploadDTO.getVoltage())
                    .divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP).doubleValue());
        } else {
            opEvseMeterUploadDTO.setVoltage(0D);
        }
        if (opEvseMeterUploadDTO.getCurrent() != null) {
            opEvseMeterUploadDTO.setCurrent(BigDecimal.valueOf(opEvseMeterUploadDTO.getCurrent())
                    .divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP).doubleValue());
        } else {
            opEvseMeterUploadDTO.setCurrent(0D);
        }
        if (opEvseMeterUploadDTO.getPower() != null) {
            opEvseMeterUploadDTO.setPower(BigDecimal.valueOf(opEvseMeterUploadDTO.getPower())
                    .divide(BigDecimal.valueOf(1000), 2, RoundingMode.HALF_UP).doubleValue());
        } else {
            opEvseMeterUploadDTO.setPower(0D);
        }
    }

    @Override
    public Boolean checkExist(OpPileEvseDTO dto) {
        return opLocationPileEvseRepository.checkExist(dto.getPileSn());
    }

    /**
     * 处理数据
     *
     * @param opEvseMeterUploadDTO
     */
    private void buildMonitorData(OpEvseMeterUploadDTO opEvseMeterUploadDTO) {
        if (opEvseMeterUploadDTO.getVoltage() != null) {
            opEvseMeterUploadDTO.setVoltage(BigDecimal.valueOf(opEvseMeterUploadDTO.getVoltage())
                    .divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP).doubleValue());
        } else {
            opEvseMeterUploadDTO.setVoltage(0D);
        }
        if (opEvseMeterUploadDTO.getCurrent() != null) {
            opEvseMeterUploadDTO.setCurrent(BigDecimal.valueOf(opEvseMeterUploadDTO.getCurrent())
                    .divide(BigDecimal.valueOf(1), 1, RoundingMode.HALF_UP).doubleValue());
        } else {
            opEvseMeterUploadDTO.setCurrent(0D);
        }
        if (opEvseMeterUploadDTO.getPower() != null) {
            opEvseMeterUploadDTO.setPower(BigDecimal.valueOf(opEvseMeterUploadDTO.getPower())
                    .divide(BigDecimal.valueOf(1000), 1, RoundingMode.HALF_UP).doubleValue());
        } else {
            opEvseMeterUploadDTO.setPower(0D);
        }
    }

    /**
     * 场站详情桩分页查询
     *
     * @param pilePageDTO 场站id
     * @return 桩分院
     */
    @Override
    public Result<Page<PilePageVO>> stationPilePage(PilePageDTO pilePageDTO) {
        return Result.ofSucceed(opLocationPileEvseRepository.stationPilePage(pilePageDTO));
    }

    /**
     * 检查设备是不是可以被删除
     * 1. 充电中不能删除
     * 2. 设备在车队已经发布的充电计划中不能删除
     * @param pileId
     */
    @Override
    public void checkPileIsCanDel(Long pileId)
    {
        //1. 充电中沿用之前的功能
        checkPileIsCharge( pileId );
        //2. 发布的车队充电计划中
        OpLocationPileEvseEntity pileEntity = this.opLocationPileEvseRepository.getById(pileId);
        if(null != pileEntity){
            String pileSn = pileEntity.getPileSn();
            List<ChargePlanVo> planVoList = fleetAdapter.queryPileBindSmartChargingPlan(pileSn);
            if(CollectionUtils.isNotEmpty(planVoList)){
                log.info("checkPileIsCanDel pileId is {},pileSn is {},planVoList is {}",pileId,pileSn,JSON.toJSONString(planVoList));
                // 抛出异常
                throw new MessageCodeException(PileBaseEnum.DEL_PILE_FLEET_CHARGE_PLAN);
            }
        }

    }

    /**
     * 检查设备是不是可以被删除
     * 1. 充电中不能删除
     * @param pileId
     */
    public void checkPileIsCharge(Long pileId)
    {
        log.info("OpLocationEvseImpl.checkPileCharge  pileId = " + pileId);
        OpLocationPileEvseEntity pileEntity = this.opLocationPileEvseRepository.getById(pileId);
        if(null == pileEntity){
            log.info("pileEntity is null");
            return ;
        }

        String pileSn = pileEntity.getPileSn();
        Long locationId = pileEntity.getLocationId();
        List<Long> evseIds = JSON.parseArray(pileEntity.getEvseList(), Long.class);
        log.info("OpLocationEvseImpl.checkPileCharge and evseIds = " + JSON.toJSONString(evseIds));

        List<OpLocationEvseEntity> evseEntityList = this.opLocationEvseRepository.listByIds(evseIds);
        if (CollectionUtils.isEmpty(evseEntityList)) {
            log.info("OpLocationEvseImpl.checkPileCharge  evseEntityList is empty ");
            return ;
        }

        evseEntityList.stream().forEach(evseEntity -> {
            Long evseId = evseEntity.getId();

            EvseDeviceStatusEnum evseStatus = monitorFeign.getEvseStatus(evseId);
            log.info("OpLocationEvseImpl.checkPileCharge and evseStatus = " + JSON.toJSONString(evseStatus));
            if (EvseDeviceStatusEnum.PREPARING.equals(evseStatus) ||
                    EvseDeviceStatusEnum.CHARGING.equals(evseStatus) ||
                    EvseDeviceStatusEnum.FINISHING.equals(evseStatus) ||
                    EvseDeviceStatusEnum.SUSPENDED_EVSE.equals(evseStatus) ||
                    EvseDeviceStatusEnum.SUSPENDED_EV.equals(evseStatus) ||
                    EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                if (EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                    throw new MessageCodeException(PileBaseEnum.PILE_HAS_BEEN_RESERVED);
                }
                throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_CHARGING);
            }
            //桩是否被预约
            CheckReserveDTO dto = new CheckReserveDTO();
            dto.setPileSn(pileSn);
            Result<Boolean> reserveRpc = iBillFeignClient.checkReserve(dto);
            log.info("OpLocationEvseImpl.checkPileCharge and reserveRpc = {}", JSON.toJSONString(reserveRpc));
            if (reserveRpc != null && reserveRpc.getData() != null && reserveRpc.getData()) {
                throw new MessageCodeException(PileBaseEnum.PILE_HAS_BEEN_RESERVED);
            }

            //充电中
            BillInfoForEvseVO billInfoForEvseVO = getBillInfoForEvseVO(evseId);
            if (billInfoForEvseVO != null) {
                throw new MessageCodeException(PileBaseEnum.NOT_DONE_ORDER_EXIST);
            }
        });
    }

    @Override
    public PromptVO checkPilePrompt(String pileSn) {
        PromptVO promptVO = new PromptVO();
        // 群组信息查询
        Result<String> group = opLocationPileGroupService.isAssociatePileGroup(pileSn);
        if(null != group && StringUtils.isNotBlank(group.getData())){
            promptVO.setSmartChargeGroupName(group.getData());
        }
        // 边缘网关信息查询
        List<PileBindVO> pileBinds = edgeAdapter.queryPileBindEdgeGateway(pileSn);
        if (CollectionUtils.isNotEmpty(pileBinds)){
            PileBindVO pileBindVO = pileBinds.get(0);
            promptVO.setEdgeName(pileBindVO.getEdgeGateName());
        }
        return promptVO;
    }

    @Override
    public PromptVO checkPileDelPrompt(Long pileId) {
        //1. 必填校验
        checkPileIsCanDel( pileId );
        //2. 提示校验
        OpLocationPileEvseEntity pileEntity = this.opLocationPileEvseRepository.getById(pileId);
        log.info("pileId is {},pileEntity is {}",pileId,JSON.toJSONString(pileEntity));
        PromptVO promptVO = new PromptVO ();
        if( null != pileEntity ){
            promptVO = checkPilePrompt(pileEntity.getPileSn());
        }
        return promptVO;
    }

    @Override
    @Transactional(rollbackFor = {Exception.class})
    public Result<Boolean> deleteByPileId(Long pileId, Boolean deleteFlag) {
        log.info("OpLocationEvseImpl.deleteByPileId and pileId = " + pileId);

        OpLocationPileEvseEntity pileEntity = this.opLocationPileEvseRepository.getById(pileId);
        if (pileEntity == null || pileEntity.getDeleted() == 1) {
            //数据不一致，从es中删除
            OpLocationPileEvseElasticDTO pileDto = this.opLocationPileEvseElastic.findById(pileId).orElse(null);
            if (pileDto != null) {
                this.opLocationPileEvseElastic.deleteById(pileId);
                String pileSn = pileDto.getPileSn();
                List<OpLocationEvseElasticDTO> evseDtoList = this.opLocationEvseElastic.findAllByPileSn(pileSn);
                if (CollectionUtils.isNotEmpty(evseDtoList)) {
                    List<Long> evseIds = evseDtoList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
                    this.opLocationEvseElastic.deleteAllById(evseIds);
                    this.opLocationEvseExpandElastic.deleteAllById(evseIds);
                }
            }
            return Result.ofSucceed(Boolean.TRUE);
        }

        String pileSn = pileEntity.getPileSn();
        Long locationId = pileEntity.getLocationId();
        List<Long> evseIds = JSON.parseArray(pileEntity.getEvseList(), Long.class);
        log.info("OpLocationEvseImpl.deleteByPileId and evseIds = " + JSON.toJSONString(evseIds));

        List<OpLocationEvseEntity> evseEntityList = this.opLocationEvseRepository.listByIds(evseIds);
        if (CollectionUtils.isEmpty(evseEntityList)) {
            log.error("OpLocationEvseImpl.deleteByPileId and evseEntityList is empty ");
        } else {

            List<String> evseSnList = new ArrayList<>();
            //桩下如果充电设备在充电中,不允许删除
            evseEntityList.stream().forEach(evseEntity -> {
                Long evseId = evseEntity.getId();

                EvseDeviceStatusEnum evseStatus = monitorFeign.getEvseStatus(evseId);
                log.info("OpLocationEvseImpl.deleteByPileId and evseStatus = " + JSON.toJSONString(evseStatus));
                if (EvseDeviceStatusEnum.PREPARING.equals(evseStatus) ||
                        EvseDeviceStatusEnum.CHARGING.equals(evseStatus) ||
                        EvseDeviceStatusEnum.FINISHING.equals(evseStatus) ||
                        EvseDeviceStatusEnum.SUSPENDED_EVSE.equals(evseStatus) ||
                        EvseDeviceStatusEnum.SUSPENDED_EV.equals(evseStatus) ||
                        EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                    if (EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                        throw new MessageCodeException(PileBaseEnum.PILE_HAS_BEEN_RESERVED);
                    }
                    throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_CHARGING);
                }
                //桩是否被预约
                CheckReserveDTO dto = new CheckReserveDTO();
                dto.setPileSn(pileSn);
                Result<Boolean> reserveRpc = iBillFeignClient.checkReserve(dto);
                log.info("OpLocationEvseImpl.deleteByPileId and reserveRpc = {}", JSON.toJSONString(reserveRpc));
                if (reserveRpc != null && reserveRpc.getData() != null && reserveRpc.getData()) {
                    throw new MessageCodeException(PileBaseEnum.PILE_HAS_BEEN_RESERVED);
                }

                //充电中
                BillInfoForEvseVO billInfoForEvseVO = getBillInfoForEvseVO(evseId);
                if (billInfoForEvseVO != null) {
                    throw new MessageCodeException(PileBaseEnum.NOT_DONE_ORDER_EXIST);
                }

                evseSnList.add(evseEntity.getEvseSn());
            });

            Long now = System.currentTimeMillis();
            //数据库删除
            LambdaUpdateWrapper<OpLocationEvseEntity> evseUpdateWrapper = Wrappers.lambdaUpdate(OpLocationEvseEntity.class);
            evseUpdateWrapper.in(OpLocationEvseEntity::getId, evseIds);
            evseUpdateWrapper.set(OpLocationEvseEntity::getDeleted, Boolean.TRUE);
            evseUpdateWrapper.set(OpLocationEvseEntity::getDeletedTime, now);
            this.opLocationEvseRepository.update(evseUpdateWrapper);
            //删除连接器
            LambdaUpdateWrapper<OpLocationConnectorEntity> connectorUpdateWrapper = Wrappers.lambdaUpdate(OpLocationConnectorEntity.class);
            connectorUpdateWrapper.in(OpLocationConnectorEntity::getLocationEvseId, evseIds);
            connectorUpdateWrapper.set(OpLocationConnectorEntity::getDeleted, Boolean.TRUE);
            this.opLocationConnectorRepository.update(connectorUpdateWrapper);
            LambdaUpdateWrapper<OpLocationPileEvseEntity> updateWrapper = Wrappers.lambdaUpdate(OpLocationPileEvseEntity.class);
            updateWrapper.eq(OpLocationPileEvseEntity::getId, pileId);
            updateWrapper.set(OpLocationPileEvseEntity::getDeleted, Boolean.TRUE);
            this.opLocationPileEvseRepository.update(updateWrapper);

            // 同时删除群组对应的桩记录  群组桩记录对应的表OpLocationPileGroupAssociateRepository
            Result<Boolean> deletedAssociate = opLocationPileGroupService.deletedAssociateByPileId(pileId);
            log.info("OpLocationEvseImpl.deleteByPileId and deletedAssociate = " + JSON.toJSONString(deletedAssociate));

            // 删除边缘云的设备
            edgeAdapter.delPileLinkedEdgeGatWay(pileSn);

            //远程服务删除
            Result<Boolean> booleanResult = dataServiceFeign.delPile(pileSn);
            if (!Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                //手动抛异常,回滚事务
                throw new RuntimeException(booleanResult.getMessage());
            }
            List<DeleteDataReqDTO> dataReqDTOList = new ArrayList<>();
            DeleteDataReqDTO deleteDataReqDTO = new DeleteDataReqDTO();
            deleteDataReqDTO.setNodeCode(pileEntity.getPileSn());
            deleteDataReqDTO.setNodeLevel(SubTreeEnum.PILE.getCode());
            deleteDataReqDTO.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
            dataReqDTOList.add(deleteDataReqDTO);
            Result<Boolean> result = saasAccessFeignClient.deleteNde(dataReqDTOList);
            log.info("删除桩修改权限树,result={}",JSON.toJSONString(result));
            if (org.springframework.util.ObjectUtils.isEmpty(result) || org.springframework.util.ObjectUtils.isEmpty(result.getData()) || Boolean.TRUE.equals(!result.getData())){
                throw new MessageCodeException("_ktqTsQx1XEKL");
            }
            //ES数据删除
            this.opLocationPileEvseElastic.deleteById(pileId);
            this.opLocationEvseElastic.deleteAllById(evseIds);

            OpLocationElasticDTO locationDto = this.opLocationElastic.findById(locationId).orElse(null);
            //MQ推送
            //桩激活指标上报
            rabbitTemplate.convertAndSend("DIRECT_EXCHANGE_PILE_ACTIVATION_UNBIND_AUTEL" + RabbitBean.RABBITMQ_VERSION_SUFFIX, "PILE.ACTIVATION.UNBIND_AUTEL", pileSn);
            rabbitTemplate.convertAndSend(PILE_BASE_PILE_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_PILE_DELETE_ROUTE, JSON.toJSONString(pileId));
            evseIds.stream().forEach(evseId -> rabbitTemplate.convertAndSend(PILE_BASE_GUN_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_GUN_DELETE_ROUTE, JSON.toJSONString(evseId)));
            //推送OICP
            if (Integer.valueOf(1).equals(pileEntity.getEroamingEnable()) && Boolean.parseBoolean(hubjectEnable) && oicpFeignClient.checkUserIsGray()) {
                //pileBaseAsync.pushEvseList(pileSn, ActionType.delete.getCode());
                ThreadPoolUtil.getExecutor().execute(RunnableWrapper.of(() -> {
                    OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(pileEntity);
                    List<OpLocationEvseDTO> opLocationEvseDTOS = new ArrayList<>();
                    List<TariffEvse> collect = new ArrayList<>();
                    LambdaUpdateWrapper<OpLocationConnectorEntity> wrapper = Wrappers.lambdaUpdate(OpLocationConnectorEntity.class);
                    wrapper.in(OpLocationConnectorEntity::getLocationEvseId, evseIds);
                    wrapper.orderByDesc(OpLocationConnectorEntity::getId);
                    Map<Long, OpLocationConnectorEntity> connectorEntityMap = this.opLocationConnectorRepository.list(wrapper).stream().collect(Collectors.toMap(OpLocationConnectorEntity::getLocationEvseId, e -> e, (f, s) -> s));
                    evseEntityList.stream().forEach(evseEntity -> {
                        OpLocationEvseDTO evseDto = OpLocationEvseConvert.toOpLocationEvseDTO(evseEntity);
                        OpLocationConnectorEntity connectorEntity = connectorEntityMap.get(evseEntity.getId());
                        if (connectorEntity != null) {
                            OpLocationConnectorDTO connectorDto = OpLocationEvseConvert.toOpLocationConnectorDTO(connectorEntity);
                            evseDto.setOpLocationConnectorDTOList(connectorDto);
                        }
                        TariffEvse tariffEvse = new TariffEvse();
                        tariffEvse.setEvseSn(evseDto.getEvseSn());
                        tariffEvse.setTariffId(evseDto.getTariffId());

                        collect.add(tariffEvse);
                        opLocationEvseDTOS.add(evseDto);
                    });
                    opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
                    EroamingEvseData eroamingEvseData = EroamingEvseData.builder()
                            .locationDTO(OpLocationConvert.toOpLocationDTO(locationDto))
                            .opPileEvseInfoVO(opPileEvseInfoVO)
                            .build();
                    oicpFeignClient.pushEvseData(eroamingEvseData, ActionType.delete.getCode());
                    oicpFeignClient.cpoPushPricingProductData(collect, ActionType.delete.getCode());
                }));
            }
            // 充电枪信息删除成功之后需要发送MQ消息到车队那边
            try {
                if (evseSnList.size() > 0) {
                    evseSnList.stream().forEach(evseSn -> {
                        EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                        evseInfoModifyDTO.setEvseSn(evseSn);
                        evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.DELETE.getCode());
                        if (locationDto != null) {
                            evseInfoModifyDTO.setSellerId(locationDto.getOperatorId());
                            evseInfoModifyDTO.setZoneId(locationDto.getZoneId());
                        }
                        opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTO);

                    });
                }
            } catch (Exception e) {
                log.error("删除充电桩后，需要推送充电枪信息给车队那边出现异常 : {}", e);
            }
            //推送广告服务
            if (locationDto != null && (locationDto.getBusinessType() == 2 || locationDto.getBusinessType() == 3)) {
                OpLocationForAdvVO<OpLocationPileDeleteForAdvVO> paramVo = new OpLocationForAdvVO();
                paramVo.setOperationType("remove");

                ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                        .eq(ChargePointMerchantRelationEntity::getSn, pileSn)
                        .eq(ChargePointMerchantRelationEntity::getMerchantId, locationDto.getOperatorId()));

                OpLocationPileDeleteForAdvVO opLocationPileDeleteForAdvVO = new OpLocationPileDeleteForAdvVO();

                opLocationPileDeleteForAdvVO.setLocationId(locationId);
                opLocationPileDeleteForAdvVO.setPileSnList(Collections.singletonList(pileSn));
                opLocationPileDeleteForAdvVO.setSellerId(locationDto.getOperatorId());

                List<PileDeleteForAdvVO> pileDeleteForAdvVOS = new ArrayList<>();

                PileDeleteForAdvVO pileDeleteForAdvVO = new PileDeleteForAdvVO();

                pileDeleteForAdvVO.setPileSn(pileSn);
                if (chargePointMerchantRelationEntity != null) {
                    pileDeleteForAdvVO.setOverchargingPileFlag(chargePointMerchantRelationEntity.getOverchargingPileFlag());
                }

                pileDeleteForAdvVOS.add(pileDeleteForAdvVO);

                opLocationPileDeleteForAdvVO.setPileInfos(pileDeleteForAdvVOS);

                paramVo.setData(opLocationPileDeleteForAdvVO);


                rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_EDIT_ADV_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,
                        PILE_BASE_LOCATION_ADV_ROUTE,
                        JSON.toJSONString(paramVo));
            }

            //删除缓存
            // 缓存桩运营商用户删除
            String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
            stringRedisTemplate.delete(snOperatorIdKey);

            try {
                AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> this.syncDeviceInfoForPos(pileSn, locationDto.getOperatorId())));
            } catch (Exception e) {
                log.error("删除充电桩后，需要推送给POS那边出现异常 : {}", e);
            }
        }

        return Result.ofSucceed(Boolean.TRUE);
    }

    private void syncDeviceInfoForPos(String pileSn, Long sellerId) {

        if (org.apache.commons.lang3.StringUtils.isBlank(pileSn)
                || sellerId == null) {
            return;
        }

        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper
                .eq(ChargePointMerchantRelationEntity::getSn, pileSn)
                .eq(ChargePointMerchantRelationEntity::getMerchantId, sellerId);
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
        if (chargePointMerchantRelationEntity == null) {
            return;
        }

        Integer overchargingPileFlag = chargePointMerchantRelationEntity.getOverchargingPileFlag();
        Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap = new HashMap<>();
        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(overchargingPileFlag)) {
            List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(pileSn, sellerId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalEntityList)) {
                terminalEntityList.forEach(val -> chargePointMerchantTerminalEntityAndEventMap.put(val, ChargePointNoticeEvent.Event.DELETE));
            }
        }

        List<SyncPileInfoForPosDTO> syncPileInfoForPosDTOList = new ArrayList<>();
        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(overchargingPileFlag)) {
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntityAndEventMap)) {
                chargePointMerchantTerminalEntityAndEventMap.forEach((key, value) -> {
                    SyncPileInfoForPosDTO syncPileInfoForPosDTO = new SyncPileInfoForPosDTO();

                    syncPileInfoForPosDTO.setEvent(value);
                    syncPileInfoForPosDTO.setDeviceType(1);
                    syncPileInfoForPosDTO.setPileSn(chargePointMerchantRelationEntity.getSn());
                    syncPileInfoForPosDTO.setPileName(chargePointMerchantRelationEntity.getName());
                    syncPileInfoForPosDTO.setTerminalSn(key.getTerminalSn());
                    syncPileInfoForPosDTO.setTerminalName(key.getTerminalName());
                    syncPileInfoForPosDTO.setSellerId(chargePointMerchantRelationEntity.getMerchantId());

                    syncPileInfoForPosDTOList.add(syncPileInfoForPosDTO);
                });
            }
        } else {
            SyncPileInfoForPosDTO syncPileInfoForPosDTO = new SyncPileInfoForPosDTO();
            syncPileInfoForPosDTO.setEvent(ChargePointNoticeEvent.Event.DELETE);
            syncPileInfoForPosDTO.setDeviceType(2);
            syncPileInfoForPosDTO.setPileSn(chargePointMerchantRelationEntity.getSn());
            syncPileInfoForPosDTO.setPileName(chargePointMerchantRelationEntity.getName());
            syncPileInfoForPosDTO.setSellerId(chargePointMerchantRelationEntity.getMerchantId());
            syncPileInfoForPosDTOList.add(syncPileInfoForPosDTO);
        }
        syncPileInfoForPosDTOList.forEach(val -> pileDeviceServiceAdapter.syncPileInfoForPos(val));
    }


    @Transactional(rollbackFor = {Exception.class})
    public Result<Boolean> deleteByPileSn(String pileSn, Boolean deleteFlag) {
        log.info("OpLocationEvseImpl.deleteByPileId and pileSn = " + pileSn);

        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getPileInfoByPileSn(pileSn);
        if (opLocationPileEvseEntity == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        String evseList = opLocationPileEvseEntity.getEvseList();
        List<Long> evseIds = new ArrayList<>();
        if (evseList != null) {
            evseIds = JSON.parseArray(evseList, Long.class);
        }
        log.info("OpLocationEvseImpl.deleteByPileId and evseIds = " + JSON.toJSONString(evseIds));
        for (Long evseId : evseIds) {
            //桩下如果充电设备在充电中,不允许删除
            EvseDeviceStatusEnum evseStatus = monitorFeign.getEvseStatus(evseId);
            log.info("OpLocationEvseImpl.deleteByPileId and evseStatus = " + JSON.toJSONString(evseStatus));
            if (EvseDeviceStatusEnum.PREPARING.equals(evseStatus) ||
                    EvseDeviceStatusEnum.CHARGING.equals(evseStatus) ||
                    EvseDeviceStatusEnum.FINISHING.equals(evseStatus) ||
                    EvseDeviceStatusEnum.SUSPENDED_EVSE.equals(evseStatus) ||
                    EvseDeviceStatusEnum.SUSPENDED_EV.equals(evseStatus) ||
                    EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                if (EvseDeviceStatusEnum.RESERVED.equals(evseStatus)) {
                    throw new MessageCodeException(PileBaseEnum.PILE_HAS_BEEN_RESERVED);
                }
                throw new MessageCodeException(PileBaseEnum.EVSE_DEVICE_CHARGING);
            }
            //桩是否被预约
            CheckReserveDTO dto = new CheckReserveDTO();
            dto.setPileSn(opLocationPileEvseEntity.getPileSn());
            Result<Boolean> reserveRpc = iBillFeignClient.checkReserve(dto);
            log.info("OpLocationEvseImpl.deleteByPileId and reserveRpc = {}", JSON.toJSONString(reserveRpc));
            if (reserveRpc != null && reserveRpc.getData() != null && reserveRpc.getData()) {
                throw new MessageCodeException(PileBaseEnum.PILE_HAS_BEEN_RESERVED);
            }

            BillInfoForEvseVO billInfoForEvseVO = getBillInfoForEvseVO(evseId);
            if (billInfoForEvseVO != null) {
                throw new MessageCodeException(PileBaseEnum.NOT_DONE_ORDER_EXIST);
            }
        }
        // 要删除的充电枪的序列号集合
        List<String> evseSnList = new ArrayList<>();
        // 构建充电设备序列号与充电设备信息之间的映射关系
        Map<String, OpLocationEvseEntity> evseSnAndOpLocationEvseEntityMap = new HashMap<>();
        evseIds.forEach(e -> {
            //删除充电设备
            OpLocationEvseEntity opLocationEvseExistEntity = opLocationEvseRepository.getById(e);
            if (opLocationEvseExistEntity != null) {
                String evseSn = opLocationEvseExistEntity.getEvseSn();
                evseSnList.add(evseSn);
                evseSnAndOpLocationEvseEntityMap.put(evseSn, opLocationEvseExistEntity);
                LambdaUpdateWrapper<OpLocationEvseEntity> evseUpdateWrapper = Wrappers.lambdaUpdate(OpLocationEvseEntity.class);
                evseUpdateWrapper.eq(OpLocationEvseEntity::getId, e);
                evseUpdateWrapper.set(OpLocationEvseEntity::getDeleted, Boolean.TRUE);
                evseUpdateWrapper.set(OpLocationEvseEntity::getDeletedTime, System.currentTimeMillis());
                opLocationEvseRepository.update(evseUpdateWrapper);
                //删除连接器
                LambdaUpdateWrapper<OpLocationConnectorEntity> connectorUpdateWrapper = Wrappers.lambdaUpdate(OpLocationConnectorEntity.class);
                connectorUpdateWrapper.eq(OpLocationConnectorEntity::getLocationEvseId, e);
                connectorUpdateWrapper.set(OpLocationConnectorEntity::getDeleted, Boolean.TRUE);
                opLocationConnectorRepository.update(connectorUpdateWrapper);
            }
        });
        OpLocationPileEvseEntity opLocationPileEvseExistEntity = opLocationPileEvseRepository.getPileInfoByPileSn(pileSn);
        if (opLocationPileEvseExistEntity != null) {
            LambdaUpdateWrapper<OpLocationPileEvseEntity> updateWrapper = Wrappers.lambdaUpdate(OpLocationPileEvseEntity.class);
            updateWrapper.eq(OpLocationPileEvseEntity::getPileSn, pileSn);
            updateWrapper.set(OpLocationPileEvseEntity::getDeleted, Boolean.TRUE);
            boolean update = opLocationPileEvseRepository.update(updateWrapper);
            //桩激活指标上报
            if (update) {
                rabbitTemplate.convertAndSend("DIRECT_EXCHANGE_PILE_ACTIVATION_UNBIND_AUTEL" + RabbitBean.RABBITMQ_VERSION_SUFFIX, "PILE.ACTIVATION.UNBIND_AUTEL", opLocationPileEvseExistEntity.getPileSn());
            }
        }
        Result<Boolean> booleanResult = dataServiceFeign.delPile(opLocationPileEvseEntity.getPileSn());
        if (!Objects.equals(Boolean.TRUE, booleanResult.getData())) {
            //手动抛异常,回滚事务
            throw new RuntimeException(booleanResult.getMessage());
        }
        // 同时删除群组对应的桩记录  群组桩记录对应的表OpLocationPileGroupAssociateRepository
        Result<Boolean> deletedAssociate = opLocationPileGroupService.deletedAssociateByPileId(opLocationPileEvseEntity.getId());
        log.info("OpLocationEvseImpl.deleteByPileId and deletedAssociate = " + JSON.toJSONString(deletedAssociate));
        log.info("OpLocationEvseImpl.deleteByPileId and start send to mq pileId = {}", opLocationPileEvseEntity.getId());
        rabbitTemplate.convertAndSend(PILE_BASE_PILE_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_PILE_DELETE_ROUTE, JSON.toJSONString(opLocationPileEvseEntity.getId()));
        log.info("OpLocationEvseImpl.deleteByPileId and end send to mq pileId = {}", opLocationPileEvseEntity.getId());

        // 删除ES数据
        evseIds.forEach(e -> {
            opLocationEvseElastic.deleteById(e);
            rabbitTemplate.convertAndSend(PILE_BASE_GUN_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_GUN_DELETE_ROUTE, JSON.toJSONString(e));
        });
        opLocationPileEvseElastic.deleteById(opLocationPileEvseEntity.getId());
        deleteHubjectEvse(opLocationPileEvseEntity);

        // 缓存桩运营商用户删除
        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(opLocationPileEvseEntity.getPileSn());
        stringRedisTemplate.delete(snOperatorIdKey);

        try {
            // 充电枪信息删除成功之后需要发送MQ消息到车队那边
            for (String evseSn : evseSnList) {
                EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                evseInfoModifyDTO.setEvseSn(evseSn);
                evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.DELETE.getCode());
                if (evseSnAndOpLocationEvseEntityMap.get(evseSn) != null && evseSnAndOpLocationEvseEntityMap.get(evseSn).getLocationId() != null) {
                    Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(evseSnAndOpLocationEvseEntityMap.get(evseSn).getLocationId());
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
                opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTO);
            }
        } catch (Exception e) {
            log.error("删除充电桩后，需要推送充电枪信息给车队那边出现异常 : {}", e);
        }

        return Result.ofSucceed(Boolean.TRUE);
    }

    public void deleteHubjectEvse(OpLocationPileEvseEntity opLocationPileEvseEntity) {
        if (opLocationPileEvseEntity != null && Integer.valueOf(1).equals(opLocationPileEvseEntity.getEroamingEnable())) {
            pileBaseAsync.pushEvseList(opLocationPileEvseEntity.getPileSn(), ActionType.delete.getCode());
        }
    }

    @Override
    public com.autel.cloud.base.model.Result<OpPileEvseInfoVO> detail(Long pileId) {
        //String sellerId = LoginUserUtil.getSellerId().toString();
        //String userId = LoginUserUtil.getUserId().toString();
        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getById(pileId);
        OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseEntity);
        //处理品牌
        OpEvseBrandEntity opEvseBrandEntity = opEvseBrandRepository.getById(opPileEvseInfoVO.getBrandId());
        if (opEvseBrandEntity != null) {
            opPileEvseInfoVO.setBrandName(opEvseBrandEntity.getName());
            opPileEvseInfoVO.setThirdPart(opEvseBrandEntity.getThirdPart());
        }
        String evseList = opLocationPileEvseEntity.getEvseList();
        List<Long> evseIds = JSON.parseArray(evseList, Long.class);
        List<OpLocationEvseDTO> opLocationEvseDTOS = new ArrayList<>();
        evseIds.forEach(e -> {
            OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(e);
            opLocationEvseDTOS.add(opLocationEvseDTO);
        });
        opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
        List<DataAuthorizeDto> permissionData = new ArrayList<>();
        if (!ObjectUtils.isEmpty(opPileEvseInfoVO)) {
            DataAuthorizeDto dataAuthorizeDto = new DataAuthorizeDto();
            dataAuthorizeDto.setNodeType(SubTreeEnum.NODE_TYPE.getCode());
            dataAuthorizeDto.setAppId(CompetenceUtil.APP_ID);
            dataAuthorizeDto.setSellerId(LoginUserUtil.getSellerId().toString());
            dataAuthorizeDto.setUserId(LoginUserUtil.getUserId().toString());
            List<Node> data = new ArrayList<>();
            List<Node> stationData = new ArrayList<>();
            Node station = new Node();
            station.setNode(opPileEvseInfoVO.getLocationId().toString());
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
                    if (!org.springframework.util.ObjectUtils.isEmpty(userCompetence) && !org.springframework.util.ObjectUtils.isEmpty(userCompetence.getData())) {
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
        return CompetenceUtil.encapsulation(opPileEvseInfoVO,permissionData);
    }

    @Override
    public Result<OpPileEvseInfoVO> detailBySn(String sn) {
        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getOne(new LambdaQueryWrapper<OpLocationPileEvseEntity>()
                .eq(OpLocationPileEvseEntity::getPileSn, sn)
                .eq(OpLocationPileEvseEntity::getDeleted, 0));
        if (ObjectUtils.isEmpty(opLocationPileEvseEntity)) {
            return null;
        }
        OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseEntity);
        //处理品牌
        if (ObjectUtils.isEmpty(opPileEvseInfoVO)) {
            return null;
        }
        OpEvseBrandEntity opEvseBrandEntity = opEvseBrandRepository.getById(opPileEvseInfoVO.getBrandId());
        if (opEvseBrandEntity != null) {
            opPileEvseInfoVO.setBrandName(opEvseBrandEntity.getName());
            opPileEvseInfoVO.setThirdPart(opEvseBrandEntity.getThirdPart());
        }
        String evseList = opLocationPileEvseEntity.getEvseList();
        List<Long> evseIds = JSON.parseArray(evseList, Long.class);
        List<OpLocationEvseDTO> opLocationEvseDTOS = new ArrayList<>();
        evseIds.forEach(e -> {
            OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(e);
            opLocationEvseDTOS.add(opLocationEvseDTO);
        });
        opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
        log.info("detailBySn {}", JSON.toJSONString(opPileEvseInfoVO));
        return Result.ofSucceed(opPileEvseInfoVO);
    }

    /**
     * description: isWhiteBrand  如果 sellerId == null， 登录态调用
     * version: 1.0
     * date: 2024/6/25 11:11
     * author: A23204
     *
     * @param
     * @return boolean
     */
    private boolean isWhiteBrand(Long sellerId) {
        if (sellerId == null) {
            sellerId = LoginUserUtil.getSellerId();
        }
        Result<SellerDetailVO> detail = pileUserFeign.detail(sellerId);
        if (detail.getData() != null) {
            return NormalWhiteBrandEnum.whiteBrand(detail.getData().getSellerSubject());
        }
        return false;
    }
}
