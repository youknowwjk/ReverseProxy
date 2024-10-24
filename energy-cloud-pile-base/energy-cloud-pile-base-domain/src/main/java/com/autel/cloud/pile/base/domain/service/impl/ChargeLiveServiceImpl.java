package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.ChargeLiveServie;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantTerminalService;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.dto.OpLocationEvseStateCountDTO;
import com.autel.cloud.pile.base.dto.OpLocationLiveEvsePageDTO;
import com.autel.cloud.pile.base.dto.OpLocationLiveEvseViewDTO;
import com.autel.cloud.pile.base.dto.pos.DeviceInfoDTO;
import com.autel.cloud.pile.base.enums.LocationEvseStatusV2Enum;
import com.autel.cloud.pile.base.enums.NormalWhiteBrandEnum;
import com.autel.cloud.pile.base.enums.SubStatus;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.PileBillClient;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileBillServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantTerminalEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.pos.DeviceInfoVO;
import com.autel.cloud.pile.base.vo.pos.GunInfoVO;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.EnergyBillDetailVO;
import com.autel.cloud.pile.bill.vo.bill.BillCDRInfoVO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.autel.cloud.smart.charge.enums.GunStateEnum;
import com.autel.cloud.tariff.dto.TariffComputeDTO;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.assertj.core.util.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author X20010
 */
@Service
@Slf4j
public class ChargeLiveServiceImpl implements ChargeLiveServie {

    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;


    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;
    @Autowired
    private OpLocationRepository opLocationRepository;

    @Autowired
    private MonitorFeignClient monitorFeignClient;
    @Resource
    private IBillFeignClient billFeignClient;

    @Resource
    private PileUserFeign pileUserFeign;

    @Autowired
    private PileBillServiceAdapter pileBillServiceAdapter;

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String, Object> redisTemplate;

    @Autowired
    private PileBillClient pileBillClient;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private ChargePointMerchantTerminalService chargePointMerchantTerminalService;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Override
    public List<OpLocationEvseStateCountVO> countEvseByState(OpLocationEvseStateCountDTO opLocationEvseStateCountDTO) {
        List<OpLocationEvseElasticDTO> list = opLocationPileEvseRepository
                .listEvse(opLocationEvseStateCountDTO);

        List<OpLocationEvseStateCountVO> countResult = getOpLocationEvseStateCountVOS(list);
        OpLocationEvseStateCountVO totalVo = new OpLocationEvseStateCountVO();
        totalVo.setState(-1);
        totalVo.setCount(countResult.stream().mapToInt(OpLocationEvseStateCountVO::getCount).sum());
        countResult.add(totalVo);
        return countResult;
    }

    /**
     * 获取枪状态统计List
     * @param list
     * @return
     */
    private List<OpLocationEvseStateCountVO> getOpLocationEvseStateCountVOS(List<OpLocationEvseElasticDTO> list) {
        Map<Integer, List<OpLocationEvseElasticDTO>> stateEvseListMap = list.stream().collect(Collectors.groupingBy(o -> LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(o.getState()).getCode()));
        List<OpLocationEvseStateCountVO> countResult = Lists.newArrayList();
        for (LocationEvseStatusV2Enum stateEnum : LocationEvseStatusV2Enum.values()) {
            OpLocationEvseStateCountVO vo = new OpLocationEvseStateCountVO();
            vo.setState(stateEnum.getCode());
            if (stateEvseListMap.containsKey(stateEnum.getCode())) {
                vo.setCount(stateEvseListMap.get(stateEnum.getCode()).size());
            } else {
                vo.setCount(0);
            }
            countResult.add(vo);
        }
        return countResult;
    }


    @Override
    public List<OpLocationEvseStateCountVO> countAllEvseByState(List<Long> locationIds) {
        // 获取当前用户下所有场站ID
        Long sellerId = UserUtil.getSellerId();
        try {
            if (CollUtil.isEmpty(locationIds)) {
                locationIds = pileUserFeign.getLocationIds().getData();
            }
            log.info("pileUserFeign getLocationIds locationIds={}", JSON.toJSONString(locationIds));
        } catch (Exception e) {
            log.info("pileUserFeign getLocationIds error", e);
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            return Lists.newArrayList();
        }
        List<OpLocationEvseElasticDTO> list = opLocationEvseElastic.findByOperatorIdAndLocationIdIn(sellerId, locationIds);
        List<OpLocationEvseStateCountVO> countResult = getOpLocationEvseStateCountVOS(list);
        return countResult;
    }

    @Override
    public List<DeviceInfoVO> selectDeviceInfo(List<DeviceInfoDTO> deviceInfoDTOList) {

        log.info("===>>> ChargeLiveServiceImpl.selectDeviceInfo deviceInfoDTOList : {}",
                JSON.toJSONString(deviceInfoDTOList));

        if (ObjectUtils.isEmpty(deviceInfoDTOList)) {
            return null;
        }

        List<String> pileSnList = deviceInfoDTOList
                .stream()
                .filter(val -> val != null && StringUtils.isNotBlank(val.getPileSn()))
                .map(DeviceInfoDTO::getPileSn)
                .distinct()
                .collect(Collectors.toList());

        if (ObjectUtils.isEmpty(pileSnList)) {
            return null;
        }

        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByPileSnIn(new HashSet<>(pileSnList));
        if (ObjectUtils.isEmpty(opLocationEvseElasticDTOList)) {
            return null;
        }

        Map<String, List<OpLocationEvseElasticDTO>> pileSnAndOpLocationEvseElasticDTOListMap = opLocationEvseElasticDTOList
                .stream()
                .collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));

        List<DeviceInfoVO> deviceInfoVOList = new ArrayList<>();
        pileSnAndOpLocationEvseElasticDTOListMap.forEach((key, value) -> {
            DeviceInfoVO deviceInfoVO = new DeviceInfoVO();
            deviceInfoVO.setPileSn(key);
            deviceInfoVO.setPileName(value.get(0).getPileName());
            List<GunInfoVO> gunInfoVOList = new ArrayList<>();
            value.forEach(val -> {
                GunInfoVO gunInfoVO = new GunInfoVO();
                gunInfoVO.setEvseSn(val.getEvseSn());
                gunInfoVO.setGunNumber(CommonUtil.getGunNo(val.getEvseSn()));
                gunInfoVO.setStateOfChargingLive(LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(val.getState()).getCode());
                gunInfoVO.setEvseDeviceStatusCode(EvseDeviceStatusEnum.getEnumByName(val.getState()).getCode());
                gunInfoVO.setEvseDeviceStatusName(val.getState());
                gunInfoVO.setGunType(val.getGunType());
//                gunInfoVO.setGunTypeIcon();
                if (val.getPower() != null) {
                    gunInfoVO.setPower(BigDecimal.valueOf(val.getPower()));
                }
                gunInfoVO.setTariffId(val.getTariffId());
                gunInfoVOList.add(gunInfoVO);
            });
            deviceInfoVO.setGunInfoVOList(gunInfoVOList
                    .stream()
                    .sorted(Comparator.comparing(GunInfoVO::getGunNumber))
                    .collect(Collectors.toList()));
            deviceInfoVOList.add(deviceInfoVO);
        });
        return deviceInfoVOList;
    }

    @Override
    public List<OpLocationLiveEvseViewVO> listLiveEvseView(OpLocationLiveEvseViewDTO opLocationLiveEvseViewDTO) {
        List<OpLocationLiveEvseViewVO> result = Lists.newArrayList();
        List<OpLocationEvseElasticDTO> list = opLocationPileEvseRepository
                .listEvse(opLocationLiveEvseViewDTO);
        if (CollectionUtil.isNotEmpty(list)) {
            List<String> evseSnList = list.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
            List<String> pileSnList = list.stream().map(OpLocationEvseElasticDTO::getPileSn).collect(Collectors.toList());

            // 计费规则映射关系
            Map<String, Long> evseIsAssociateTarfficMap = getEvseIsAssociateTarfficMap(evseSnList);
            // 订阅状态映射关系
            Map<String, ChargePointLicenseVO> lastExpireTimeChargePointLicenseMap = getPileChargePointLicenseMap(pileSnList);

            // 充电中进度映射关系
            List<String> chargingEvseSnList = list.stream().filter(o -> GunStateEnum.CHARGING.getName().equals(o.getState())).map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
            Map<String, OpEvseMeterUploadDTO> evseChargingMeterMap = getOpEvseMeterUploadDTOMap(chargingEvseSnList);

            list.forEach(o -> {
                OpLocationLiveEvseViewVO vo = new OpLocationLiveEvseViewVO();
                BeanUtil.copyProperties(o, vo);
                String[] eleArray = StringUtils.split(o.getEvseSn(), "_");
                vo.setGunNum(Integer.valueOf(eleArray[eleArray.length - 1]));
                final String state = o.getState();
                vo.setBusinessState(LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(state).getCode());
                // 状态持续时间
                final Long stateUploadTime = o.getStateUploadTime();
                final Long offlineUploadTime = o.getOfflineUploadTime();
                vo.setStateLastUpdateTime(stateUploadTime);
                vo.setOfflineUploadTime(offlineUploadTime);
                if (EvseDeviceStatusEnum.DEFAULT.getName().equals(state) && Objects.nonNull(offlineUploadTime)) {
                    vo.setStateDuration(System.currentTimeMillis() - offlineUploadTime);
                    vo.setStateLastUpdateTime(o.getOfflineUploadTime());
                } else if (Objects.nonNull(stateUploadTime)){
                    vo.setStateDuration(System.currentTimeMillis() - stateUploadTime);
                }
                // 如果充电，查询充电进度
                if (GunStateEnum.CHARGING.getName().equals(vo.getState())) {
                    OpEvseMeterUploadDTO opEvseMeterUploadDTO = evseChargingMeterMap.get(vo.getEvseSn());
                    if (opEvseMeterUploadDTO != null) {
                        vo.setChargingSoc(Objects.nonNull(opEvseMeterUploadDTO.getBatterySoc()) ? opEvseMeterUploadDTO.getBatterySoc().doubleValue() : null);
                        vo.setChargingTimeLeft(opEvseMeterUploadDTO.getRdTimeLeft());
                        vo.setChargingPower(unitConversion(opEvseMeterUploadDTO.getPower()));
                    }
                }
                // 订阅状态
                if (lastExpireTimeChargePointLicenseMap.containsKey(vo.getPileSn())) {
                    vo.setSubscriptionStatus(lastExpireTimeChargePointLicenseMap.get(vo.getPileSn()).getSubscriptionStatus());
                    vo.setEffectiveDays(lastExpireTimeChargePointLicenseMap.get(vo.getPileSn()).getEffectiveDays());
                } else {
                    vo.setSubscriptionStatus(SubStatus.INACTIVITY.getStatus());
                }

                // 是否关联计费规则
                vo.setIsAssociateBillingRules(evseIsAssociateTarfficMap.containsKey(vo.getEvseSn()));
                result.add(vo);
            });
        }
        return result;
    }


    @Override
    public PageVO<OpLocationLiveEvseVO> listLiveEvseByPage(OpLocationLiveEvsePageDTO opLocationLiveEvsePageDTO) {
        PageVO<OpLocationLiveEvseVO> page = new PageVO<>();
        page.setPage(opLocationLiveEvsePageDTO.getPage());
        page.setPageSize(opLocationLiveEvsePageDTO.getPageSize());
        List<OpLocationLiveEvseVO> result = Lists.newArrayList();
        PageVO<OpLocationEvseElasticDTO> searchPage = opLocationPileEvseRepository
                .listEvse(opLocationLiveEvsePageDTO);
        if (CollectionUtil.isNotEmpty(searchPage.getContent())) {
            List<OpLocationEvseElasticDTO> list = searchPage.getContent();
            List<String> evseSnList = list.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
            List<String> pileSnList = list.stream().map(OpLocationEvseElasticDTO::getPileSn).collect(Collectors.toList());

            // 订阅状态映射关系
            Map<String, ChargePointLicenseVO> lastExpireTimeChargePointLicenseMap = getPileChargePointLicenseMap(pileSnList);

            // 计费规则映射关系
            Map<String, Long> evseIsAssociateTarfficMap = getEvseIsAssociateTarfficMap(evseSnList);

            // 充电中进度映射关系
            List<String> chargingEvseSnList = list.stream().filter(o -> GunStateEnum.CHARGING.getName().equals(o.getState())).map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
            Map<String, Long> mapToUse = new HashMap<>();
            if (!CollectionUtils.isEmpty(chargingEvseSnList)) {
                List<BillCDRInfoVO> infoList = this.billFeignClient.findLastList(chargingEvseSnList).getData();
                if (!CollectionUtils.isEmpty(infoList)) {
                    infoList.stream().forEach(e -> mapToUse.put(e.getEvseSn(), e.getStartDateTime()));
                }
            }
            Map<String, OpEvseMeterUploadDTO> evseChargingMeterMap = getOpEvseMeterUploadDTOMap(chargingEvseSnList);

            // 最近一次订单信息映射关系
            List<String> orderSeqList = list.stream().filter(o -> Objects.nonNull(o.getLastOrderSeq())).map(o -> o.getLastOrderSeq().toString()).collect(Collectors.toList());
            Map<Long, EnergyBillDetailVO> evseLastBillMap = getOpEvseLastBillMap(orderSeqList);

            long now = System.currentTimeMillis();
            list.forEach(o -> {
                OpLocationLiveEvseVO vo = new OpLocationLiveEvseVO();
                String evseSn = o.getEvseSn();
                BeanUtil.copyProperties(o, vo);
                vo.setBusinessState(LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(o.getState()).getCode());
                vo.setGunNum(CommonUtil.getGunNo(evseSn));
                // 状态持续时间
                if (!CollectionUtils.isEmpty(mapToUse)) {
                    Long startTime = mapToUse.get(evseSn);
                    if (startTime != null) {
                        vo.setStateDuration(now - startTime);
                    }
                }
                if (vo.getStateDuration() == null && o.getStateUploadTime() != null) {
                    vo.setStateDuration(System.currentTimeMillis() - o.getStateUploadTime());
                }
                vo.setStateLastUpdateTime(o.getStateUploadTime());

                // 是否关联计费规则
                vo.setIsAssociateBillingRules(evseIsAssociateTarfficMap.containsKey(vo.getEvseSn()));

                if (isWhiteBrand()) {
                    vo.setSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
                    vo.setEffectiveDays(365L);
                } else {
                    // TODO：订阅状态适配超充终端
                    ChargePointVO chargePointVO = chargePointMerchantRelationService.findBySN(o.getPileSn(), LoginUserUtil.getSellerId());
                    if (chargePointVO != null && Integer.valueOf(1).equals(chargePointVO.getOverchargingPileFlag())) {
                        // 是超充 查询 终端
                        List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(o.getPileSn(), LoginUserUtil.getSellerId());
                        terminalEntityList.forEach(chargePointMerchantTerminalEntity -> {
                            List<Connector> connectorsList = chargePointMerchantTerminalEntity.getConnectorsList();
                            // DE0120B1GN9C00032G_1
                            String connectorIdStr = o.getEvseSn().split("_")[1];
                            log.info("listLiveEvseByPage, hostsn: {}, evseSn: {}, connectorIdStr: {},  terminalEntityList: {}", o.getPileSn(), o.getEvseSn() , connectorIdStr, JSON.toJSONString(connectorsList));
                            List<Connector> connectors = connectorsList.stream().filter(connector -> Integer.parseInt(connectorIdStr) == connector.getConnectorId()).collect(Collectors.toList());
                            if (CollUtil.isNotEmpty(connectors)) {
                                // 找到了对应的终端 sn
                                String terminalSn = chargePointMerchantTerminalEntity.getTerminalSn();
                                Map<String, ChargePointLicenseVO> pileChargePointLicenseMap = getPileChargePointLicenseMap(Collections.singletonList(terminalSn));
                                log.info("terminal pileChargePointLicenseMap: {} ", JSON.toJSONString(pileChargePointLicenseMap));
                                // 订阅状态
                                if (pileChargePointLicenseMap.containsKey(terminalSn)) {
                                    vo.setSubscriptionStatus(pileChargePointLicenseMap.get(terminalSn).getSubscriptionStatus());
                                    vo.setEffectiveDays(pileChargePointLicenseMap.get(terminalSn).getEffectiveDays());
                                } else {
                                    vo.setSubscriptionStatus(SubStatus.INACTIVITY.getStatus());
                                }
                            }
                        });
                    } else {
                        // 订阅状态
                        if (lastExpireTimeChargePointLicenseMap.containsKey(vo.getPileSn())) {
                            vo.setSubscriptionStatus(lastExpireTimeChargePointLicenseMap.get(vo.getPileSn()).getSubscriptionStatus());
                            vo.setEffectiveDays(lastExpireTimeChargePointLicenseMap.get(vo.getPileSn()).getEffectiveDays());
                        } else {
                            vo.setSubscriptionStatus(SubStatus.INACTIVITY.getStatus());
                        }
                    }
                }

                if (evseLastBillMap.containsKey(o.getLastOrderSeq())) {
                    OpLocationLiveEvseMoreVO.LastOrderInfo lastOrderInfo = getLastOrderInfo(evseLastBillMap.get(o.getLastOrderSeq()));
                    vo.setLastOrder(lastOrderInfo);
                } else {
                    vo.setLastOrder(null);
                }
                // 如果充电，查询充电进度
                if (GunStateEnum.CHARGING.getName().equals(vo.getState())) {
                    OpEvseMeterUploadDTO opEvseMeterUploadDTO = evseChargingMeterMap.get(vo.getEvseSn());
                    if (opEvseMeterUploadDTO != null) {
                        vo.setChargingTimeLeft(opEvseMeterUploadDTO.getRdTimeLeft());
                        vo.setChargingSoc(Objects.nonNull(opEvseMeterUploadDTO.getBatterySoc()) ? opEvseMeterUploadDTO.getBatterySoc().doubleValue() : null);
                        vo.setChargingPower(unitConversion(opEvseMeterUploadDTO.getPower()));
                    }
                }
                result.add(vo);
            });
        }
        page.setTotalPages(searchPage.getTotalPages());
        page.setTotalRows(searchPage.getTotalRows());
        page.setContent(result);
        return page;
    }

    private boolean isWhiteBrand() {
        Long sellerId = LoginUserUtil.getSellerId();
        Result<SellerDetailVO> detail = pileUserFeign.detail(sellerId);
        if (detail.getData() != null) {
            return NormalWhiteBrandEnum.whiteBrand(detail.getData().getSellerSubject());
        }
        return false;
    }

    @Override
    public OpLocationLiveEvseMoreVO getEvseLiveInfo(String evseSn) {
        log.info("========= getEvseLiveInfo invoked, the evseSn: {}", evseSn);
        Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        OpLocationEvseElasticDTO evseInfo = opLocationEvseElastic.findByOperatorIdAndEvseSn(sellerId, evseSn);
        log.info("========= findByOperatorIdAndEvseSn invoked, the evseInfo: {}", JSON.toJSONString(evseInfo));
        if (Objects.nonNull(evseInfo)) {
            OpLocationLiveEvseMoreVO vo = new OpLocationLiveEvseMoreVO();
            BeanUtil.copyProperties(evseInfo, vo);
            vo.setEvseId(String.valueOf(evseInfo.getId()));
//            log.info("get evse status by monitor: evseSn={}, old status={}", evseInfo.getEvseSn(), vo.getState());
//            Result<String> stringResult = monitorFeignClient.queryStatusByEvseSn(evseInfo.getEvseSn());
//            log.info("get evse status by monitor: evseSn={}, response={}", evseInfo.getEvseSn(), JSON.toJSONString(stringResult));
//            final String state = stringResult.getData();
//            if (stringResult.getCode().equals(HttpCodeEnum.OK.getCode()) && StringUtils.isNotEmpty(state)) {
//                vo.setState(state);
//            }
            String state = monitorFeignClient.queryStatusByEvseSn(evseSn).getData();
            log.info("======= the state : {} of evseSn: {}", state, evseSn);
            final Long stateUploadTime = evseInfo.getStateUploadTime();
            vo.setStateLastUpdateTime(stateUploadTime);
            final Long offlineUploadTime = evseInfo.getOfflineUploadTime();
            vo.setOfflineUploadTime(offlineUploadTime);
            if (EvseDeviceStatusEnum.DEFAULT.getName().equals(state) && Objects.nonNull(offlineUploadTime)) {
                vo.setStateLastUpdateTime(offlineUploadTime);
                vo.setStateDuration(System.currentTimeMillis() - offlineUploadTime);
            } else if (Objects.nonNull(stateUploadTime)){
                vo.setStateDuration(System.currentTimeMillis() - stateUploadTime);
            }
            vo.setBusinessState(LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(state).getCode());
            // 充电中更新SOC及剩余充电时间
            if (EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
                OpEvseMeterUploadDTO evseMeterCache = getEvseMeterCache(evseSn);
                log.info("========= the OpEvseMeterUploadDTO: {} of evseSn: {}", JSON.toJSONString(evseMeterCache), evseSn);
                if (Objects.nonNull(evseMeterCache)) {
                    OpLocationLiveEvseMoreVO.ChargingInfo chargingInfo = new OpLocationLiveEvseMoreVO.ChargingInfo();
                    Optional.ofNullable(evseMeterCache.getBatterySoc()).ifPresent(soc -> chargingInfo.setChargingSoc(soc.doubleValue()));
                    chargingInfo.setChargingTimeLeft(evseMeterCache.getRdTimeLeft());
                    chargingInfo.setChargingPower(unitConversion(evseMeterCache.getPower()));
                    vo.setChargingInfo(chargingInfo);
                }
            }
            List<Integer> effectiveOrderStatus = Arrays.asList(OrderStatusEnum.START_SUCCESS.getValue(), OrderStatusEnum.COMPLETED.getValue(), OrderStatusEnum.ERROR.getValue(), OrderStatusEnum.ERROR_HANDLED.getValue());
            // 查询最近一次充电信息
            Result<EnergyBillDetailVO>  energyBillDetailVOResult = pileBillClient.findLastBillByEvseExcludeCurrentOrderSeq(evseSn, null);
            log.info("======== the energyBillDetailVO: {}", JSON.toJSONString(energyBillDetailVOResult));
            if (energyBillDetailVOResult != null && energyBillDetailVOResult.getCode() == HttpStatus.HTTP_OK
                    && energyBillDetailVOResult.getData() != null && energyBillDetailVOResult.getData().getCdrId() != null
                    && energyBillDetailVOResult.getData().getOrderStatus() != null
                    && effectiveOrderStatus.contains(energyBillDetailVOResult.getData().getOrderStatus())) {
                OpLocationLiveEvseMoreVO.LastOrderInfo lastOrderInfo = getLastOrderInfo(energyBillDetailVOResult.getData());
                if (null != lastOrderInfo) {
                    lastOrderInfo.setId(Long.parseLong(energyBillDetailVOResult.getData().getCdrId()));
                }
                vo.setLastOrder(lastOrderInfo);
            }
//            if (Objects.nonNull(evseInfo.getLastOrderSeq())) {
//                EnergyBillDetailVO energyBillDetailVO = pileBillServiceAdapter.selectBillBaseByBusId(evseInfo.getLastOrderSeq().toString());
//                OpLocationLiveEvseMoreVO.LastOrderInfo lastOrderInfo = getLastOrderInfo(energyBillDetailVO);
//                vo.setLastOrder(lastOrderInfo);
//            }

            return vo;
        }
        return null;
    }


    /**
     * 获取最新订单情况
     * @param
     * @param energyBillDetailVO
     * @return
     */
    private OpLocationLiveEvseMoreVO.LastOrderInfo getLastOrderInfo(EnergyBillDetailVO energyBillDetailVO) {
        try {
            OpLocationLiveEvseMoreVO.LastOrderInfo lastOrderInfo = new OpLocationLiveEvseMoreVO.LastOrderInfo();
            BeanUtil.copyProperties(energyBillDetailVO, lastOrderInfo);
            lastOrderInfo.setSumPower(unitConversion(lastOrderInfo.getSumPower()));
            // 充电中更新计费信息（充电度数及账单费用）
            String key = RedisKeyConstant.getStringChargingTariffComputeInfo(energyBillDetailVO.getOrderSeq());
            TariffComputeDTO tariffComputeDTO = (TariffComputeDTO) redisTemplate.opsForValue().get(key);
            if (Objects.nonNull(tariffComputeDTO)) {
                lastOrderInfo.setSumPower(unitConversion(tariffComputeDTO.getSumPower().doubleValue()));
                lastOrderInfo.setTotalAmount(tariffComputeDTO.getTotalBalance().doubleValue());
            }
            return lastOrderInfo;
        } catch (Exception e) {
            log.info("charge live get last order error!", e);
        }
        return null;

    }

    @Override
    public OpLocationLiveEvseCurrentVO getCurrentStatus(String evseSn) {
        OpLocationLiveEvseCurrentVO result = new OpLocationLiveEvseCurrentVO();
        OpLocationLiveEvseMoreVO evseLiveInfo = getEvseLiveInfo(evseSn);
        if (Objects.nonNull(evseLiveInfo)) {
            BeanUtils.copyProperties(evseLiveInfo, result);
            convertChargeInfo(result, evseLiveInfo);
            convertBillInfo(result, evseLiveInfo);

            // 查询桩的额定功率
            ChargePointVO chargePointVO = chargePointMerchantRelationService.findBySN(evseLiveInfo.getPileSn(), LoginUserHolder.getLoginUser().getPayload().getSellerId());
            if (Objects.nonNull(chargePointVO)) {
                result.setRatedPower(chargePointVO.getRatedPower());
            }
        }

        return result;
    }

    /**
     * 转换充电信息
     * @param result
     * @param evseLiveInfo
     */
    private void convertChargeInfo(OpLocationLiveEvseCurrentVO result, OpLocationLiveEvseMoreVO evseLiveInfo) {
        if (Objects.nonNull(evseLiveInfo.getLastOrder())) {
            result.setLastestBillStartTime(evseLiveInfo.getLastOrder().getCreateTime());
            result.setSumPower(evseLiveInfo.getLastOrder().getSumPower());
            result.setTotalAmount(evseLiveInfo.getLastOrder().getTotalAmount());
        }
    }

    /**
     * 转换订单信息
     * @param result
     * @param evseLiveInfo
     */
    private void convertBillInfo(OpLocationLiveEvseCurrentVO result, OpLocationLiveEvseMoreVO evseLiveInfo) {
        if (Objects.nonNull(evseLiveInfo.getChargingInfo())) {
            result.setChargingPower(evseLiveInfo.getChargingInfo().getChargingPower());
            result.setChargingSoc(evseLiveInfo.getChargingInfo().getChargingSoc());
            result.setChargingTimeLeft(evseLiveInfo.getChargingInfo().getChargingTimeLeft());
        }
    }

    /**
     * 枪最新订单映射关系
     * @param orderSeqList
     * @return
     */
    private Map<Long, EnergyBillDetailVO> getOpEvseLastBillMap(List<String> orderSeqList) {
        Map<Long, EnergyBillDetailVO> evseBillMap = Maps.newHashMap();
        try {
            List<EnergyBillDetailVO> energyBillList = billFeignClient.selectBillListByBusIds(orderSeqList);
            energyBillList.forEach(o -> {
                evseBillMap.put(Long.valueOf(o.getOrderSeq()), o);
            });
        } catch (Exception e) {
            log.info("charge live batch get bill error!", e);
        }
        return evseBillMap;
    }

    /**
     * 获取桩订阅映射关系
     * @param pileSnList
     * @return
     */
    private Map<String, ChargePointLicenseVO> getPileChargePointLicenseMap(List<String> pileSnList) {
        // 2.8版本优化 根据启动充电功能点来判断
        List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.START_CHARGE);
        List<ChargePointLicenseVO> lastExpireTimeChargePointLicense = chargePointMerchantRelationService.findLastExpireTimeChargePointLicense
                (pileSnList, functionBenefitList, LoginUserHolder.getLoginUser().getPayload().getSellerId());
        Map<String, ChargePointLicenseVO> lastExpireTimeChargePointLicenseMap = lastExpireTimeChargePointLicense.stream().collect(Collectors.toMap(ChargePointLicenseVO::getSn, Function.identity()));
        return lastExpireTimeChargePointLicenseMap;
    }


    /**
     * 获取枪计费规则映射关系
     *
     * @param evseSnList
     * @return
     */
    private Map<String, Long> getEvseIsAssociateTarfficMap(List<String> evseSnList) {
        Map<String, Long> evseIsAssociateTarfficMap = Maps.newHashMap();
        try {
            log.info("charge live getEvseIsAssociateTarfficMap evseSnList={}", JSON.toJSONString(evseSnList));
            List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseRepository.queryTariffIdListByEvseSnList(evseSnList);
            if (CollectionUtil.isNotEmpty(opLocationEvseEntityList)) {
                opLocationEvseEntityList.forEach(o -> {
                    if (Objects.nonNull(o.getTariffId())) {
                        evseIsAssociateTarfficMap.put(o.getEvseSn(), o.getTariffId());
                    }
                });
            }
        } catch (Exception e) {
            log.error("charge live getEvseIsAssociateTarfficMap error!", e);
        }
        return evseIsAssociateTarfficMap;
    }

    /**
     * 获取充电中枪充电进度信息
     *
     * @param evseSn
     * @return
     */
    private OpEvseMeterUploadDTO getEvseMeterCache(String evseSn) {
        try {
            Result<OpEvseMeterUploadDTO> result = monitorFeignClient.queryMeterByEvseSnCache(evseSn);
            OpEvseMeterUploadDTO meterUploadDTO = result.getData();
            if (Objects.nonNull(meterUploadDTO)) {
                return meterUploadDTO;
            }
        } catch (Exception e) {
            log.error("charge live getEvseMeterCache error!", e);
        }
        return null;
    }

    /**
     * 获取充电中枪充电进度信息
     *
     * @param chargingEvseSnList
     * @return
     */
    private Map<String, OpEvseMeterUploadDTO> getOpEvseMeterUploadDTOMap(List<String> chargingEvseSnList) {
        Map<String, OpEvseMeterUploadDTO> evseChargingMeterMap = Maps.newHashMap();
        try {
            log.info("charge live getOpEvseMeterUploadDTOMap chargingEvseSnList={}", JSON.toJSONString(chargingEvseSnList));
            if (CollectionUtil.isNotEmpty(chargingEvseSnList)) {
                Result<List<OpEvseMeterUploadDTO>> listResult = monitorFeignClient.queryNewMeters(chargingEvseSnList);
                List<OpEvseMeterUploadDTO> meterUploadDTOList = listResult.getData();
                if (CollectionUtil.isNotEmpty(meterUploadDTOList)) {
                    meterUploadDTOList.forEach(o -> {
                        evseChargingMeterMap.put(o.getEvseSn(), o);
                    });
                }
            }
        } catch (Exception e) {
            log.error("charge live getOpEvseMeterUploadDTOMap error!", e);
        }
        return evseChargingMeterMap;
    }

    /**
     * W转换KW
     * @param power
     * @return
     */
    private double unitConversion(Double power) {
        if (Objects.isNull(power)) {
            return 0;
        }
        BigDecimal powerDecimal = BigDecimal.valueOf(power);
        BigDecimal currentPower = powerDecimal.divide(BigDecimal.valueOf(1000));
        BigDecimal result = currentPower.setScale(2, BigDecimal.ROUND_HALF_UP);
        return result.doubleValue();
    }
}