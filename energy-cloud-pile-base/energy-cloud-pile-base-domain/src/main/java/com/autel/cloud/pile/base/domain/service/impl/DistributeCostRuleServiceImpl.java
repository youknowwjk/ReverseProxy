package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.constant.DispatchConstant;
import com.autel.cloud.pile.base.domain.service.DistributeCostRuleService;
import com.autel.cloud.pile.base.domain.service.OpCostRuleDistributeService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.dto.OpEvseAssociatedRuleDTO;
import com.autel.cloud.pile.base.dto.OpPileAssociatedRuleParamDTO;
import com.autel.cloud.pile.base.dto.TariffIssuedDTO;
import com.autel.cloud.pile.base.dto.app.AppUnit;
import com.autel.cloud.pile.base.dto.tariff.*;
import com.autel.cloud.pile.base.enums.device.PileTypeEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.BaseAdminClient;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.ProtocolFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.bill.dto.CDRTariffRuleDTO;
import com.autel.cloud.pile.bill.dto.TimezoneUserDTO;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.feign.ICDRFeignClient;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.autel.cloud.pile.bill.vo.TariffRuleModelVO;
import com.autel.cloud.pile.bill.vo.UserChargePileVO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.tariff.dto.*;
import com.autel.cloud.tariff.feign.BillCostFeignClient;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.TariffIssuedVO;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.google.common.base.Joiner;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class DistributeCostRuleServiceImpl implements DistributeCostRuleService {

    @Resource
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private IBillFeignClient billFeignClient;

    @Resource
    private BaseAdminClient baseAdminClient;

    @Autowired
    MonitorFeignClient monitorFeignClient;

    @Autowired
    private HomePileFeignClient homePileFeignClient;

    @Autowired
    private OpCostRuleDistributeService opCostRuleDistributeService;

    @Autowired
    private ProtocolFeignClient protocolFeignClient;

    @Autowired
    private OpLocationEvseService opLocationEvseService;

    @Resource
    private OpLocationElastic opLocationElastic;

    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Resource
    private OpLocationPileEvseElastic opLocationPileEvseElastic;

    @Resource
    private PileUserFeign pileUserFeign;

    @Resource
    private BillCostFeignClient billCostFeignClient;

    @Resource
    private ICDRFeignClient cdrFeignClient;

    @Autowired
    private UserTimezoneServiceImpl userTimezoneService;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Autowired
    private TariffFeignClient tariffFeignClient;

    @Override
    public Result<Boolean> dispatchTariffByPrice(HomePileTariffDispatchDTO homePileTariffDispatchDTO) {
        log.info("============ the homePileTariffDispatchDTO in the dispatchTariffByPrice: {}", JSON.toJSONString(homePileTariffDispatchDTO));
        ChargePileDTO chargePileInfoVO = deviceServiceFeign.pileDetail(homePileTariffDispatchDTO.getPileSn()).getData();
        log.info("============ the chargePileInfoVO of the pile: {}", JSON.toJSONString(chargePileInfoVO));
        if (chargePileInfoVO == null) {
            log.info("============ because the pile is not existed, it's not needed to dispatch");
            return Result.ofSucceed(true);
        }
        // 若为第三方桩，则不下发计费规则
        if (null != chargePileInfoVO.getThirdPart() && chargePileInfoVO.getThirdPart()) {
            log.info("============ because the pile is third-party, it's not needed to dispatch, the chargePileInfoVO: {}", chargePileInfoVO);
            return Result.ofSucceed(true);
        }
        // 充电桩已被共享，则不下发计费规则
        if (DeviceTypeEnum.NO_ATTRIBUTE_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())
                || DeviceTypeEnum.SHARED_HOME_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())) {
            log.info("============ because the usage of pile has been shared in the dispatchTariffByPrice, it's not needed to dispatch, the chargePileInfoVO: {}", chargePileInfoVO);
            return Result.ofSucceed(true);
        }
        BigDecimal energyPrice = BigDecimal.ZERO;
        if (null != homePileTariffDispatchDTO.getEnergyPrice()) {
            energyPrice = homePileTariffDispatchDTO.getEnergyPrice().setScale(4, BigDecimal.ROUND_HALF_UP);
        }
        // 用户修改电价时，若存在正在充电中的订单则修改充电中的电价
        String pileSn = homePileTariffDispatchDTO.getPileSn();
        String evseSn = pileSn + "_" + "1";
        try {
            Result<EnergyBillVO> energyBillVOResult = billFeignClient.findLastBillByEvse(evseSn);
            log.info("============ the energyBillVOResult while findLastBillByEvse invoked: {}", JSON.toJSONString(energyBillVOResult));
            if (null != energyBillVOResult && HttpStatus.SC_OK == energyBillVOResult.getCode() && null != energyBillVOResult.getData()) {
                EnergyBillVO energyBillVO = energyBillVOResult.getData();
                if (null != energyBillVO.getId() && DeviceTypeEnum.HOME_PILE.getValue().equals(energyBillVO.getEvseType())
                        && OrderStatusEnum.START_SUCCESS.getValue().equals(energyBillVO.getOrderStatus())) {
                    log.info("======== update energy price in the tariff rule, the orderSeq: {}", energyBillVO.getOrderSeq());
                    ChargingEnergyPriceDTO chargingEnergyPriceDTO = new ChargingEnergyPriceDTO();
                    chargingEnergyPriceDTO.setBusId(energyBillVO.getOrderSeq());
                    chargingEnergyPriceDTO.setPrice(energyPrice.stripTrailingZeros().toPlainString());
                    chargingEnergyPriceDTO.setChargePointSn(energyBillVO.getEvseSn());
                    billCostFeignClient.updateEnergyPrice(chargingEnergyPriceDTO);
                    CDRTariffRuleDTO cdrTariffRuleDTO = new CDRTariffRuleDTO();
                    if (StrUtil.isNotBlank(energyBillVO.getCdrId())) {
                        log.info("======== update the tariff rule of bill, the orderSeq: {}", energyBillVO.getOrderSeq());
                        cdrTariffRuleDTO.setId(Long.parseLong(energyBillVO.getCdrId()));
                        cdrTariffRuleDTO.setPrice(energyPrice.stripTrailingZeros().toPlainString());
                        cdrFeignClient.updateTariffRule(cdrTariffRuleDTO);
                    }
                }
            }
        } catch (Exception e) {
            log.error("=========== invoke findLastBillByEvse exception by pileSn: {}", pileSn, e);
        }
        UserChargePileVO userChargePileVO;
        try {
            userChargePileVO = homePileFeignClient.getUserPile(homePileTariffDispatchDTO.getPileSn()).getData();
            log.info("======== the userChargePileVO: {}", userChargePileVO);
        } catch (Exception e) {
            log.error("用户未绑桩sn: {}", homePileTariffDispatchDTO.getPileSn());
            return Result.ofSucceed(false);
        }
        if (null == userChargePileVO || StrUtil.isBlank(userChargePileVO.getUserId())) {
            return Result.ofSucceed(false);
        }
        Long userId = Long.parseLong(userChargePileVO.getUserId());
        String currencySign = null;
        Integer unit = null;
        List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = new ArrayList<>();
        CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
        costRuleDispatchPileSnDTO.setTariffId(userId);
        costRuleDispatchPileSnDTO.setPileSn(homePileTariffDispatchDTO.getPileSn());
        costRuleDispatchPileSnDTO.setUserId(userId);
        costRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
        updateOpCostRuleDistribute(costRuleDistributeMap);
        // 不存在需要下发的计费规则，直接返回
        if (CollUtil.isEmpty(costRuleDistributeMap.get(BaseConstant.DISTRIBUTE))) {
            return Result.ofSucceed(true);
        }
        Result<AppUnit> appUnitResult = baseAdminClient.getSetByUserId(userId + "");
        log.info("========== the user information in the dispatchTariffByPrice function: {}", JSON.toJSON(appUnitResult));
        if (null != appUnitResult && HttpStatus.SC_OK == appUnitResult.getCode() && null != appUnitResult.getData()) {
            currencySign = appUnitResult.getData().getMonetaryUnit();
            unit = appUnitResult.getData().getMonetaryCode();
        }

        Result<String> tariffRuleResult = billFeignClient.getHomePileTariffs();
        log.info("========= the tariffRule in getHomePileTariffs invoked:{} ", JSON.toJSONString(tariffRuleResult));
        String tariffRule = String.format(tariffRuleResult.getData(), energyPrice.toString());
        StationRuleInfoDTO stationRuleInfoDTO = transferStationRuleInfo(tariffRule, currencySign, unit, userId + "");
        List<CostRuleBatchDispatchHomePileDTO> costRuleBatchDispatchHomePileDTOList = new ArrayList<>();
        CostRuleBatchDispatchHomePileDTO costRuleBatchDispatchHomePileDTO = new CostRuleBatchDispatchHomePileDTO();
        costRuleBatchDispatchHomePileDTO.setStationRuleInfoDTO(stationRuleInfoDTO);
        costRuleBatchDispatchHomePileDTO.setPileSn(homePileTariffDispatchDTO.getPileSn());
        costRuleBatchDispatchHomePileDTOList.add(costRuleBatchDispatchHomePileDTO);
        return protocolFeignClient.dispatchHomePileTariff(costRuleBatchDispatchHomePileDTOList);
    }

    @Override
    public Result<Boolean> dispatchHomePileTariff(HomePileTariffDispatchDTO homePileTariffDispatchDTO) {
        log.info("============ the homePileTariffDispatchDTO in the dispatchHomePileTariff: {}", JSON.toJSONString(homePileTariffDispatchDTO));
        ChargePileDTO chargePileInfoVO = deviceServiceFeign.pileDetail(homePileTariffDispatchDTO.getPileSn()).getData();
        log.info("============ the chargePileInfoVO of the pile: {}", JSON.toJSONString(chargePileInfoVO));
        if (chargePileInfoVO == null) {
            log.info("============ because the pile is not existed, it's not needed to dispatch");
            return Result.ofSucceed(true);
        }
        // 若为第三方桩，则不下发计费规则
        if (null != chargePileInfoVO.getThirdPart() && chargePileInfoVO.getThirdPart()) {
            log.info("============ because the pile is third-party, it's not needed to dispatch, the chargePileInfoVO: {}", chargePileInfoVO);
            return Result.ofSucceed(true);
        }
        // 充电桩已被共享，则不下发计费规则
        if (DeviceTypeEnum.NO_ATTRIBUTE_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())
                || DeviceTypeEnum.SHARED_HOME_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())) {
            log.info("============ because the usage of pile has been shared in the dispatchHomePileTariff, it's not needed to dispatch, the chargePileInfoVO: {}", chargePileInfoVO);
            return Result.ofSucceed(true);
        }
        UserChargePileVO userChargePileVO;
        try {
            userChargePileVO = homePileFeignClient.getUserPile(homePileTariffDispatchDTO.getPileSn()).getData();
            log.info("======== the userChargePileVO: {}", userChargePileVO);
        } catch (Exception e) {
            log.error("用户未绑桩sn: {}", homePileTariffDispatchDTO.getPileSn());
            return Result.ofSucceed(false);
        }
        if (null == userChargePileVO || StrUtil.isBlank(userChargePileVO.getUserId()) || null == userChargePileVO.getTariffId()) {
            return Result.ofSucceed(false);
        }
        Long userId = Long.parseLong(userChargePileVO.getUserId());
        String currencySign = null;
        Integer unit = null;
        List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = new ArrayList<>();
        CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
        costRuleDispatchPileSnDTO.setTariffId(userChargePileVO.getTariffId());
        costRuleDispatchPileSnDTO.setPileSn(homePileTariffDispatchDTO.getPileSn());
        costRuleDispatchPileSnDTO.setUserId(userId);
        costRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
        updateOpCostRuleDistribute(costRuleDistributeMap);
        // 不存在需要下发的计费规则，直接返回
        if (CollUtil.isEmpty(costRuleDistributeMap.get(BaseConstant.DISTRIBUTE))) {
            return Result.ofSucceed(true);
        }
        Result<AppUnit> appUnitResult = baseAdminClient.getSetByUserId(userId + "");
        log.info("========== the user information in the dispatchHomePileTariff function: {}", JSON.toJSON(appUnitResult));
        if (null != appUnitResult && HttpStatus.SC_OK == appUnitResult.getCode() && null != appUnitResult.getData()) {
            currencySign = appUnitResult.getData().getMonetaryUnit();
            unit = appUnitResult.getData().getMonetaryCode();
        }
        TimezoneUserDTO timezoneUserDTO = userTimezoneService.getTimeZoneInfoOfUser(userId);
        List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
//        costRuleDispatchPileSnDTO.setPileSn(homePileTariffDispatchDTO.getPileSn());
//        costRuleDispatchPileSnDTO.setTariffId(userChargePileVO.getTariffId());
//        costRuleDispatchPileSnDTO.setUserId(userId);
        costRuleDispatchPileSnDTO.setStationTimezone(timezoneUserDTO.getTimezone());
        costRuleDispatchPileSnDTO.setCurrencySign(currencySign);
        costRuleDispatchPileSnDTO.setUnit(unit);
        costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);

        return protocolFeignClient.dispatchHomePileTariffV2(costRuleBatchDispatchPileSnList);
    }

    @Override
    @Deprecated
    public Result<Boolean> batchDispatchTariffByCurrency(HomePileTariffBatchDispatchDTO homePileTariffBatchDispatchDTO) {
        log.info("============ the homePileTariffBatchDispatchDTO in the batchDispatchTariffByCurrency: {}", JSON.toJSONString(homePileTariffBatchDispatchDTO));
        if (null == homePileTariffBatchDispatchDTO || CollUtil.isEmpty(homePileTariffBatchDispatchDTO.getPileSn())) {
            return Result.ofSucceed(true);
        }
        List<String> pileSnList = new ArrayList<>();
        homePileTariffBatchDispatchDTO.getPileSn().forEach(pileSn -> {
            if (StrUtil.isNotBlank(pileSn)) {
                ChargePileDTO chargePileInfoVO = deviceServiceFeign.pileDetail(pileSn).getData();
                log.info("============ the chargePileInfoVO of the pile: {}", JSON.toJSONString(chargePileInfoVO));
                // 若为第三方桩，则不下发计费规则
                if (chargePileInfoVO == null || null != chargePileInfoVO.getThirdPart() && chargePileInfoVO.getThirdPart()) {
                    log.info("============ because the pile is third part in the batchDispatchTariffByCurrency, it's not needed to dispatch");
                    return;
                }
                // 充电桩已被共享，则不下发计费规则
                if (DeviceTypeEnum.NO_ATTRIBUTE_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())
                        || DeviceTypeEnum.SHARED_HOME_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())) {
                    log.info("============ because the usage of pile has been shared in the batchDispatchTariffByCurrency, it's not needed to dispatch");
                    return;
                } else {
                    pileSnList.add(pileSn);
                }
            }
        });
        log.info("=========== the pileSnList in the batchDispatchTariffByCurrency: {}", JSON.toJSONString(pileSnList));
        if (CollUtil.isEmpty(pileSnList)) {
            return Result.ofSucceed(true);
        }
        List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = new ArrayList<>();
        pileSnList.forEach(pileSn -> {
            CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
            costRuleDispatchPileSnDTO.setTariffId(homePileTariffBatchDispatchDTO.getUserId());
            costRuleDispatchPileSnDTO.setPileSn(pileSn);
            costRuleDispatchPileSnDTO.setUserId(homePileTariffBatchDispatchDTO.getUserId());
            costRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
        });
        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
        updateOpCostRuleDistribute(costRuleDistributeMap);
        List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get(BaseConstant.DISTRIBUTE);
        log.info("============ the distributeCostRuleList in the batchDispatchTariffByCurrency: {}", JSON.toJSONString(distributeCostRuleList));
        // 不存在需要下发的计费规则，直接返回
        if (CollUtil.isEmpty(distributeCostRuleList)) {
            return Result.ofSucceed(true);
        }
        Result<String> tariffRuleResult = billFeignClient.getHomePileTariffs();
        log.info("========= the tariffRule in getHomePileTariffs invoked: {} ", JSON.toJSONString(tariffRuleResult));
        List<CostRuleBatchDispatchHomePileDTO> costRuleBatchDispatchHomePileDTOList = new ArrayList<>();
        distributeCostRuleList.forEach(distributeDTO -> {
            String sn = distributeDTO.getPileSn();
            UserChargePileVO userChargePileVO;
            try {
                userChargePileVO = homePileFeignClient.getUserPile(sn).getData();
                log.info("======== the userChargePileVO: {}", userChargePileVO);
            } catch (Exception e) {
                log.error("用户未绑桩sn: {}", sn);
                return;
            }

            double price = 0D;
            if (userChargePileVO != null && null != userChargePileVO.getPriceEnable() && userChargePileVO.getPriceEnable()) {
                price = userChargePileVO.getPrice() == null ? 0 : userChargePileVO.getPrice();
            }

            String tariffRule = String.format(tariffRuleResult.getData(), price);
            StationRuleInfoDTO stationRuleInfoDTO = transferStationRuleInfo(tariffRule, homePileTariffBatchDispatchDTO.getCurrencySign(),
                    homePileTariffBatchDispatchDTO.getUnit(), homePileTariffBatchDispatchDTO.getUserId() + "");
            CostRuleBatchDispatchHomePileDTO costRuleBatchDispatchHomePileDTO = new CostRuleBatchDispatchHomePileDTO();
            costRuleBatchDispatchHomePileDTO.setStationRuleInfoDTO(stationRuleInfoDTO);
            costRuleBatchDispatchHomePileDTO.setPileSn(sn);
            costRuleBatchDispatchHomePileDTOList.add(costRuleBatchDispatchHomePileDTO);
        });
        return protocolFeignClient.dispatchHomePileTariff(costRuleBatchDispatchHomePileDTOList);
    }

    @Override
    public Result<Boolean> batchDispatchTariffByCurrencyV2(HomePileTariffBatchDispatchDTO homePileTariffBatchDispatchDTO) {
        log.info("============ the homePileTariffBatchDispatchDTO in the batchDispatchTariffByCurrencyV2: {}", JSON.toJSONString(homePileTariffBatchDispatchDTO));
        if (null == homePileTariffBatchDispatchDTO || CollUtil.isEmpty(homePileTariffBatchDispatchDTO.getPileSn())) {
            return Result.ofSucceed(true);
        }
        Long userId = homePileTariffBatchDispatchDTO.getUserId();
        List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
        TimezoneUserDTO timezoneUserDTO = userTimezoneService.getTimeZoneInfoOfUser(userId);
        homePileTariffBatchDispatchDTO.getPileSn().forEach(pileSn -> {
            if (StrUtil.isNotBlank(pileSn)) {
                ChargePileDTO chargePileInfoVO = deviceServiceFeign.pileDetail(pileSn).getData();
                log.info("============ the chargePileInfoVO of the pile: {}", JSON.toJSONString(chargePileInfoVO));
                // 若为第三方桩，则不下发计费规则
                if (chargePileInfoVO == null || null != chargePileInfoVO.getThirdPart() && chargePileInfoVO.getThirdPart()) {
                    log.info("============ because the pile is third part in the batchDispatchTariffByCurrencyV2, it's not needed to dispatch");
                    return;
                }
                // 充电桩已被共享，则不下发计费规则
                if (DeviceTypeEnum.NO_ATTRIBUTE_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())
                        || DeviceTypeEnum.SHARED_HOME_PILE.getValue().equals(chargePileInfoVO.getUsageScenario())) {
                    log.info("============ because the usage of pile has been shared in the batchDispatchTariffByCurrencyV2, it's not needed to dispatch");
                    return;
                } else {
                    UserChargePileVO userChargePileVO;
                    try {
                        userChargePileVO = homePileFeignClient.getUserPile(pileSn).getData();
                        log.info("======== the userChargePileVO: {}", userChargePileVO);
                    } catch (Exception e) {
                        log.error("用户未绑桩sn: {}", pileSn);
                        return;
                    }
                    if (null != userChargePileVO && null != userChargePileVO.getTariffId()) {
                        CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                        costRuleDispatchPileSnDTO.setPileSn(pileSn);
                        costRuleDispatchPileSnDTO.setTariffId(userChargePileVO.getTariffId());
                        costRuleDispatchPileSnDTO.setUserId(userId);
                        costRuleDispatchPileSnDTO.setStationTimezone(timezoneUserDTO.getTimezone());
                        costRuleDispatchPileSnDTO.setCurrencySign(homePileTariffBatchDispatchDTO.getCurrencySign());
                        costRuleDispatchPileSnDTO.setUnit(homePileTariffBatchDispatchDTO.getUnit());
                        costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
                    }
                }
            }
        });
        log.info("=========== the pileSnList in the batchDispatchTariffByCurrencyV2: {}", JSON.toJSONString(costRuleBatchDispatchPileSnList));
        if (CollUtil.isEmpty(costRuleBatchDispatchPileSnList)) {
            return Result.ofSucceed(true);
        }
        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleBatchDispatchPileSnList);
        updateOpCostRuleDistribute(costRuleDistributeMap);
        List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get(BaseConstant.DISTRIBUTE);
        log.info("============ the distributeCostRuleList in the batchDispatchTariffByCurrencyV2: {}", JSON.toJSONString(distributeCostRuleList));
        // 不存在需要下发的计费规则，直接返回
        if (CollUtil.isEmpty(distributeCostRuleList)) {
            return Result.ofSucceed(true);
        }
        List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchList = new ArrayList<>();
        costRuleBatchDispatchPileSnList.forEach(costRuleDispatchPileSnDTO -> {
            distributeCostRuleList.forEach(distributeDTO -> {
                if (costRuleDispatchPileSnDTO.getPileSn().equals(distributeDTO.getPileSn())) {
                    costRuleBatchDispatchList.add(costRuleDispatchPileSnDTO);
                }
            });
        });
        if (CollUtil.isNotEmpty(costRuleBatchDispatchList)) {
            return protocolFeignClient.dispatchHomePileTariffV2(costRuleBatchDispatchPileSnList);
        }
        return Result.ofSucceed(true);
    }

    @Override
    public Result<Boolean> dispatchTariffOfPublicPileByTariffId(Long tariffId, Long userId, Boolean newRelease) {

        log.info("============ in the dispatchTariffOfPublicPile of DistributeCostRuleServiceImpl, the tariffId: {}, userId: {}, newRelease : {}", JSON.toJSONString(tariffId), JSON.toJSONString(userId), JSON.toJSONString(newRelease));

        //根据计费规则查询绑定的充电桩
        OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO = new OpPileAssociatedRuleParamDTO();
        opPileAssociatedRuleParamDTO.setTariffId(tariffId);
        opPileAssociatedRuleParamDTO.setIsExcludedTariff(false);
        Result<List<OpEvseAssociatedRuleDTO>> opEvseAssociatedRuleDTOResult = opLocationEvseService.getEvseInfoForRules(opPileAssociatedRuleParamDTO);
        log.info("======= the opEvseAssociatedRuleDTOResult: {} ", opEvseAssociatedRuleDTOResult);
        List<String> pileSnList = new ArrayList<>();
        opEvseAssociatedRuleDTOResult.getData().stream().forEach(opEvseAssociatedRuleDTO -> {
            if (StrUtil.isNotBlank(opEvseAssociatedRuleDTO.getEvseSn())) {
                String pileSn = opEvseAssociatedRuleDTO.getEvseSn().split("_")[0];
                if (CollUtil.isEmpty(pileSnList) || !pileSnList.contains(pileSn)) {
                    pileSnList.add(pileSn);
                }
            }
        });
        if (CollUtil.isEmpty(pileSnList)) {
            return Result.ofSucceed(true);
        }
        List<CostRuleDispatchPileSnDTO> finalCostRuleDispatchPileSnDTOList = new ArrayList<>();
        pileSnList.forEach(pileSn -> {
            CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
            costRuleDispatchPileSnDTO.setTariffId(tariffId);
            costRuleDispatchPileSnDTO.setPileSn(pileSn);
            costRuleDispatchPileSnDTO.setUserId(userId);
            finalCostRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
        });
        List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = opCostRuleDistributeService.filterNonThirdPileCostRule(finalCostRuleDispatchPileSnDTOList);
        if (CollUtil.isEmpty(costRuleDispatchPileSnDTOList)) {
            log.info("========= because there are not needed dispatch pile, return");
            return Result.ofSucceed(true);
        }
        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
        List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get(BaseConstant.DISTRIBUTE);
        // 下发计费规则
        if (CollUtil.isNotEmpty(distributeCostRuleList)) {
            List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
            for (OpCostRuleDistributeDTO opCostRuleDistributeDTO : distributeCostRuleList) {
                String evseSn = opCostRuleDistributeDTO.getPileSn() + "_" + "1";
                OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseService.getLocationEvseVOBySnAndGunNo(evseSn).getData();
                CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                costRuleDispatchPileSnDTO.setPileSn(opCostRuleDistributeDTO.getPileSn());
                costRuleDispatchPileSnDTO.setTariffId(opCostRuleDistributeDTO.getRuleId());
                if (ocppLocationEVSEVO != null) {
                    costRuleDispatchPileSnDTO.setStationTimezone(ocppLocationEVSEVO.getTimeZone());
                }
                costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
            }

            log.info("========== the costRuleBatchDispatchPileSnList before costRuleBatchDispatch invoked: {}", JSON.toJSONString(costRuleBatchDispatchPileSnList));

            if (newRelease != null && newRelease && ObjectUtils.isNotEmpty(costRuleBatchDispatchPileSnList)) {
                // todo v1.9版本下发需要满足：计费规则为旧版计费规则, 5寸，8寸，固件版本低于指定版本都需要下发
                List<CostRuleDispatchPileSnDTO> newReleaseCostRuleBatchDispatchPileSnList = new ArrayList<>(costRuleBatchDispatchPileSnList);
                costRuleBatchDispatchPileSnList = new ArrayList<>();
                List<String> SNList = newReleaseCostRuleBatchDispatchPileSnList.stream().map(CostRuleDispatchPileSnDTO::getPileSn).collect(Collectors.toList());
                // 调用device服务，筛选出符合下发条件的桩
                Result<List<SimpleChargePileVO>> result = deviceServiceFeign.getSimpleChargePileInfoList(SNList);

                log.info("===>>> DistributeCostRuleServiceImpl.dispatchTariffOfPublicPileByTariffId result : {}", JSON.toJSONString(result));

                if (result != null && result.getCode() == 200) {
                    List<SimpleChargePileVO> data = result.getData();
                    if (ObjectUtils.isNotEmpty(data)) {
                        for (CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO : newReleaseCostRuleBatchDispatchPileSnList) {
                            String pileSn = costRuleDispatchPileSnDTO.getPileSn();
                            for (SimpleChargePileVO datum : data) {
                                String sn = datum.getSn();
                                Integer type = datum.getType();
                                if (StrUtil.isNotBlank(pileSn)
                                        && pileSn.equals(sn)
                                        && !PileTypeEnum.OTHER_PILES.getCode().equals(type)) {
                                    costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
                                    break;
                                }
                            }
                        }
                    }
                }

                log.info("========== the newCostRuleBatchDispatchPileSnList before costRuleBatchDispatch invoked: {}", JSON.toJSONString(costRuleBatchDispatchPileSnList));

            }

            Result<Boolean> dispatchResult = protocolFeignClient.costRuleBatchDispatch(costRuleBatchDispatchPileSnList);
            log.info("=========== the dispatchResult after costRuleBatchDispatch invoked: {}", dispatchResult);
        }
        updateOpCostRuleDistribute(costRuleDistributeMap);
        return Result.ofSucceed(true);
    }

    @Override
    public Result<Boolean> dispatchTariffOfPublicPile(Long tariffId, String pileSn, String timezone) {
        log.info("=========== the tariffId: {}, pileSn: {}, timezone: {} in the dispatchTariffOfPublicPile function", tariffId, pileSn, timezone);
        List<CostRuleDispatchPileSnDTO> finalCostRuleDispatchPileSnDTOList = new ArrayList<>();
        CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
        costRuleDispatchPileSnDTO.setTariffId(tariffId);
        costRuleDispatchPileSnDTO.setPileSn(pileSn);
        finalCostRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
        List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = opCostRuleDistributeService.filterNonThirdPileCostRule(finalCostRuleDispatchPileSnDTOList);
        if (CollUtil.isEmpty(costRuleDispatchPileSnDTOList)) {
            log.info("========= because there are not needed dispatch pile, return");
            return Result.ofSucceed(true);
        }
        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
        List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get("distribute");
        // 下发计费规则
        if (CollUtil.isNotEmpty(distributeCostRuleList)) {
            List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
            for (OpCostRuleDistributeDTO distributeDTO : distributeCostRuleList) {
                if (StrUtil.isBlank(timezone)) {
                    String evseSn = distributeDTO.getPileSn() + "_" + "1";
                    OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseService.getLocationEvseVOBySnAndGunNo(evseSn).getData();
                    timezone = ocppLocationEVSEVO.getTimeZone();
                }
                CostRuleDispatchPileSnDTO temp = new CostRuleDispatchPileSnDTO();
                temp.setPileSn(distributeDTO.getPileSn());
                temp.setTariffId(distributeDTO.getRuleId());
                temp.setStationTimezone(timezone);
                costRuleBatchDispatchPileSnList.add(temp);
            }
            log.info("========== the costRuleBatchDispatchPileSnList before costRuleBatchDispatch invoked: {}", JSON.toJSONString(costRuleBatchDispatchPileSnList));
            Result<Boolean> dispatchResult = protocolFeignClient.costRuleBatchDispatch(costRuleBatchDispatchPileSnList);
            log.info("=========== the dispatchResult after costRuleBatchDispatch invoked: {}", dispatchResult);
        }
        updateOpCostRuleDistribute(costRuleDistributeMap);
        return Result.ofSucceed(true);
    }

    private RuleDetail getRuleDetail(CostRuleWeeksDTO item) {
        RuleDetail ruleDetail = new RuleDetail();
        ruleDetail.setTime(Joiner.on(",").join(item.getWeeks()));
        List<StationRuleDetailDTO> collect = item.getWeeksRules().stream()
                .map(this::getStationRuleDetail)
                .collect(Collectors.toList());
        ruleDetail.setDetails(collect);
        return ruleDetail;
    }

    private StationRuleDetailDTO getStationRuleDetail(CostRulesDTO rule) {
        StationRuleDetailDTO stationRuleDetail = new StationRuleDetailDTO();
        if (null != rule.getStartPrice()) {
            stationRuleDetail.setConnectionFee(rule.getStartPrice().stripTrailingZeros());
        } else {
            stationRuleDetail.setConnectionFee(BigDecimal.ZERO);
        }
        if (null != rule.getTimePrice()) {
            stationRuleDetail.setChargeDurationFee(rule.getTimePrice().stripTrailingZeros());
        } else {
            stationRuleDetail.setChargeDurationFee(BigDecimal.ZERO);
        }
        if (null != rule.getUnitPrice()) {
            stationRuleDetail.setElectricFee(rule.getUnitPrice().stripTrailingZeros());
        } else {
            stationRuleDetail.setElectricFee(BigDecimal.ZERO);
        }
        stationRuleDetail.setStart(rule.getBeginTime());
        stationRuleDetail.setEnd(rule.getEndTime());
        return stationRuleDetail;
    }


    /**
     * 封装充电桩计费规则
     *
     * @param
     * @param currencySign
     * @param unit
     * @param tariffId
     * @return
     */
    private StationRuleInfoDTO transferStationRuleInfo(String tariffRule, String currencySign, Integer unit, String tariffId) {
        StationRuleInfoDTO stationRuleInfoDTO = new StationRuleInfoDTO();
        // 家桩计费规则ID设置为用户ID
        stationRuleInfoDTO.setId(String.valueOf(tariffId));
        stationRuleInfoDTO.setCurrencySign(currencySign);
        stationRuleInfoDTO.setUnit(unit);
        TariffRuleModelVO tariffRuleModelVO = JSON.parseObject(tariffRule, TariffRuleModelVO.class);
        stationRuleInfoDTO.setUpdateTime(System.currentTimeMillis());
        List<RuleDetail> ruleDetailList = tariffRuleModelVO.getRules().stream()
                .map(this::getRuleDetail)
                .collect(Collectors.toList());
        log.info("========== the ruleDetailList in transferStationRuleInfo: {}", ruleDetailList.toString());
        stationRuleInfoDTO.setRules(ruleDetailList);
        return stationRuleInfoDTO;
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

    @Override
    public Result<Boolean> retryDistributeHomeCostRule(String pileSn, Long tariffId, Long userId, Double price) {
        log.info("========== the pileSn: {}, tariffId: {}, userId: {} in the retryDistributeHomeCostRule", pileSn, tariffId, userId);
        // 家桩用户ID
        String userIdStr = userId + "";//userChargePileVO.getUserId();
        // 查询货币符号
        String currencySign = null;
        Integer unit = null;
        Result<AppUnit> appUnitResult = baseAdminClient.getSetByUserId(userIdStr);
        log.info("========== the user information in the retryDistributeHomeCostRule function: {}", JSON.toJSON(appUnitResult));
        if (null != appUnitResult && HttpStatus.SC_OK == appUnitResult.getCode() && null != appUnitResult.getData()) {
            currencySign = appUnitResult.getData().getMonetaryUnit();
            unit = appUnitResult.getData().getMonetaryCode();
        }
        /*
        Result<String> tariffRuleResult = billFeignClient.getHomePileTariffs();
        log.info("========= the tariffRule in retryDistributeHomeCostRule invoked: {} ", JSON.toJSONString(tariffRuleResult));
        String tariffRule = String.format(tariffRuleResult.getData(), price);
        log.info("========= the tariffRule after format: {}", JSON.toJSONString(tariffRule));
        StationRuleInfoDTO stationRuleInfoDTO = transferStationRuleInfo(tariffRule, currencySign, unit, userIdStr);
        CostRuleBatchDispatchHomePileDTO costRuleBatchDispatchHomePileDTO = new CostRuleBatchDispatchHomePileDTO();
        costRuleBatchDispatchHomePileDTO.setStationRuleInfoDTO(stationRuleInfoDTO);
        costRuleBatchDispatchHomePileDTO.setPileSn(pileSn);
        List<CostRuleBatchDispatchHomePileDTO> costRuleBatchDispatchHomePileDTOList = new ArrayList<>();
        costRuleBatchDispatchHomePileDTOList.add(costRuleBatchDispatchHomePileDTO);
        protocolFeignClient.dispatchHomePileTariff(costRuleBatchDispatchHomePileDTOList);
*/
        TimezoneUserDTO timezoneUserDTO = userTimezoneService.getTimeZoneInfoOfUser(userId);
        List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
        CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
        costRuleDispatchPileSnDTO.setPileSn(pileSn);
        costRuleDispatchPileSnDTO.setTariffId(tariffId);
        costRuleDispatchPileSnDTO.setUserId(userId);
        costRuleDispatchPileSnDTO.setStationTimezone(timezoneUserDTO.getTimezone());
        costRuleDispatchPileSnDTO.setCurrencySign(currencySign);
        costRuleDispatchPileSnDTO.setUnit(unit);
        costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
        protocolFeignClient.dispatchHomePileTariffV2(costRuleBatchDispatchPileSnList);

        // 更新计费规则下发状态
        List<OpCostRuleDistributeDTO> costRuleDistributeDTOList = new ArrayList<>();
        OpCostRuleDistributeDTO costRuleDistributeDTO = new OpCostRuleDistributeDTO();
        costRuleDistributeDTO.setRuleId(tariffId);
        costRuleDistributeDTO.setPileSn(pileSn);
        costRuleDistributeDTO.setDistributeTime(null);
        costRuleDistributeDTO.setCreateBy(userId);
        costRuleDistributeDTOList.add(costRuleDistributeDTO);
        Result<Boolean> result = opCostRuleDistributeService.addOrUpdateOpCostRuleDistributeByPileSn(costRuleDistributeDTOList);
        return result;
    }

    @Override
    public Result<Boolean> getHubjectByLocationId(Long locationId) {
        boolean hubjectCheck = false;
        Optional<OpLocationElasticDTO> locationOptional = opLocationElastic.findById(locationId);
        if (locationOptional.isPresent()) {
            OpLocationElasticDTO opLocationElasticDTO = locationOptional.get();
            hubjectCheck = opLocationElasticDTO.getHubjectCheck();
        }
        return Result.ofSucceed(hubjectCheck);
    }

    @Override
    public Result<LocationNameAndPileSnListVO> getByTariffId(Long tariffId) {
        Long currentUserId = UserUtil.getSellerId();
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        boolQueryBuilder.must(QueryBuilders.termQuery("operatorId", currentUserId));
        List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
        if (CollectionUtils.isEmpty(locationIdList)) {
            return null;
        }
        boolQueryBuilder.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIdList));
        Iterable<OpLocationPileEvseElasticDTO> iterable =
//                opLocationPileEvseElastic.search(boolQueryBuilder);
        elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());

        LocationNameAndPileSnListVO vo = new LocationNameAndPileSnListVO();
        List<String> locationNameList = new ArrayList<>();
        List<String> pileSnList = new ArrayList<>();
        if (iterable != null) {
            iterable.forEach(dto -> {
                locationNameList.add(dto.getLocationName());
                Result<Boolean> hubjectByLocationId = getHubjectByLocationId(dto.getLocationId());
                if (hubjectByLocationId != null && hubjectByLocationId.getData().equals(Boolean.FALSE)) {
                    if (StringUtils.isNotBlank(dto.getPileSn())) {
                        List<OpLocationEvseElasticDTO> allByPileSn = opLocationEvseElastic.findAllByPileSn(dto.getPileSn());
                        if (CollUtil.isNotEmpty(allByPileSn)) {
                            allByPileSn.forEach(evse -> {
                                pileSnList.add(evse.getEvseSn());
                            });
                        }
                    }
                } else {
                    pileSnList.add(dto.getPileSn());
                }

            });
        }
        vo.setLocationNameList(locationNameList);
        vo.setPileSnList(pileSnList);
        return Result.ofSucceed(vo);
    }

    /**
     * @param dispatchTariffOfPublicPileByPileSnListDTO 清空五寸桩计费规则 入参模型
     * @return 清空五寸桩计费规则结果
     * @function 清空五寸桩计费规则
     */
    @Override
    public Result<Boolean> dispatchTariffOfPublicPileByPileSnList(DispatchTariffOfPublicPileByPileSnListDTO dispatchTariffOfPublicPileByPileSnListDTO) {

        log.info("======>>>>>>>>>>> DistributeCostRuleServiceImpl.dispatchTariffOfPublicPileByPileSnList dispatchTariffOfPublicPileByPileSnListDTO : {}", JSON.toJSONString(dispatchTariffOfPublicPileByPileSnListDTO));

        List<String> fiveInchesPileSnList = dispatchTariffOfPublicPileByPileSnListDTO.getFiveInchesPileSnList();
        Long userId = dispatchTariffOfPublicPileByPileSnListDTO.getUserId();
        Long tariffId = dispatchTariffOfPublicPileByPileSnListDTO.getTariffId();

        if (CollUtil.isEmpty(fiveInchesPileSnList)) {
            // 五寸桩集合为空，说明不需要下发计费规则
            return Result.ofSucceed(true);
        }

        List<CostRuleDispatchPileSnDTO> finalCostRuleDispatchPileSnDTOList = new ArrayList<>();
        fiveInchesPileSnList.forEach(pileSn -> {
            CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
            costRuleDispatchPileSnDTO.setTariffId(tariffId);
            costRuleDispatchPileSnDTO.setPileSn(pileSn);
            costRuleDispatchPileSnDTO.setUserId(userId);
            finalCostRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
        });

        // 过滤掉第三方桩，第三方桩不下发计费规则
        List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = opCostRuleDistributeService.filterNonThirdPileCostRule(finalCostRuleDispatchPileSnDTOList);
        if (CollUtil.isEmpty(costRuleDispatchPileSnDTOList)) {
            // 要下发的桩的集合为空，说明不需要下发计费规则
            return Result.ofSucceed(true);
        }

        Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = opCostRuleDistributeService.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
        List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get(BaseConstant.DISTRIBUTE);

        // 下发计费规则
        if (CollUtil.isNotEmpty(distributeCostRuleList)) {
            List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
            distributeCostRuleList.stream().forEach(distributeDTO -> {
                String evseSn = distributeDTO.getPileSn() + "_" + "1";
                OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseService.getLocationEvseVOBySnAndGunNo(evseSn).getData();
                CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                costRuleDispatchPileSnDTO.setPileSn(distributeDTO.getPileSn());
                costRuleDispatchPileSnDTO.setTariffId(distributeDTO.getRuleId());
                costRuleDispatchPileSnDTO.setStationTimezone(ocppLocationEVSEVO.getTimeZone());
                costRuleDispatchPileSnDTO.setIsFiveInchesPileSn(true);
                costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
            });
            log.info("========== the costRuleBatchDispatchPileSnList before costRuleBatchDispatch invoked: {}", JSON.toJSONString(costRuleBatchDispatchPileSnList));
            Result<Boolean> dispatchResult = protocolFeignClient.costRuleBatchDispatch(costRuleBatchDispatchPileSnList);
            log.info("=========== the dispatchResult after costRuleBatchDispatch invoked: {}", dispatchResult);
        }
        updateOpCostRuleDistribute(costRuleDistributeMap);
        return Result.ofSucceed(true);
    }

    @Override
    public Boolean issueBillingRule(TariffIssuedDTO tariffIssuedDTO) {

        log.info("===>>> DistributeCostRuleServiceImpl.issueBillingRule tariffIssuedDTO : {}",
                JSON.toJSONString(tariffIssuedDTO));

        List<DispatchTariffPileInfoVO> dispatchTariffPileInfoVOList = opLocationEvseService.getDispatchTariffPileInfo(tariffIssuedDTO.getTariffId());
        if (ObjectUtils.isEmpty(dispatchTariffPileInfoVOList)) {
            return true;
        }

        List<DispatchTariffDTO> dispatchTariffDTOList = new ArrayList<>();
        for (DispatchTariffPileInfoVO dispatchTariffPileInfoVO : dispatchTariffPileInfoVOList) {

            if (dispatchTariffPileInfoVO == null
                    || org.apache.commons.lang3.StringUtils.isBlank(dispatchTariffPileInfoVO.getSn())) {
                continue;
            }

            DispatchTariffDTO dispatchTariffDTO = new DispatchTariffDTO();

            dispatchTariffDTO.setTariffId(tariffIssuedDTO.getTariffId());
            dispatchTariffDTO.setPileSn(dispatchTariffPileInfoVO.getSn());
            dispatchTariffDTO.setFlagIssue(tariffIssuedDTO.getFlagIssue());
            dispatchTariffDTO.setNewVersionCostModelRule(tariffIssuedDTO.getNewVersionCostModelRule());
            dispatchTariffDTO.setType(dispatchTariffPileInfoVO.getType());
            dispatchTariffDTO.setSupportIssuedIdleFee(dispatchTariffPileInfoVO.getSupportIssuedIdleFee());

            dispatchTariffDTOList.add(dispatchTariffDTO);
        }

        Map<String, List<DispatchTariffVO>> operateAndDispatchTariffVOListMap = TariffUtil.buildDispatchMap(dispatchTariffDTOList);
        if (ObjectUtils.isEmpty(operateAndDispatchTariffVOListMap)) {
            return true;
        }

        List<DispatchTariffVO> needDispatchTariffPileSnList = operateAndDispatchTariffVOListMap.get(DispatchConstant.ISSUED);
        if (ObjectUtils.isNotEmpty(needDispatchTariffPileSnList)) {
            this.batchDispatchCostRule(needDispatchTariffPileSnList);
        }

        List<DispatchTariffVO> needClearTariffPileSnList = operateAndDispatchTariffVOListMap.get(DispatchConstant.CLEAR);
        if (ObjectUtils.isNotEmpty(needClearTariffPileSnList)) {
            this.batchClearBillingRule(needClearTariffPileSnList
                    .stream()
                    .filter(var -> var != null && org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileSn()))
                    .map(DispatchTariffVO::getPileSn)
                    .distinct()
                    .collect(Collectors.toList()));
        }
        return true;
    }

    @Override
    public Boolean businessPileOnlineIssueBillingRule(String pileSn, boolean clearFlag) {

        log.info("===>>> DistributeCostRuleServiceImpl.businessPileOnlineIssueBillingRule pileSn : {} and clearFlag : {}",
                JSON.toJSONString(pileSn),
                JSON.toJSONString(clearFlag));

        if (org.apache.commons.lang3.StringUtils.isBlank(pileSn)) {
            return false;
        }

        OpLocationEvseEntity opLocationEvseEntity = opLocationEvseService.findOneByEvseSn(pileSn + "_1");
        if (opLocationEvseEntity == null
                || opLocationEvseEntity.getTariffId() == null) {
            return false;
        }

        Set<String> pileSnSet = new HashSet<>();
        pileSnSet.add(pileSn);

        Result<List<DispatchTariffPileInfoVO>> dispatchTariffPileInfoResult = deviceServiceFeign.dispatchTariffPileInfo(new ArrayList<>(pileSnSet));

        log.info("===>>> DistributeCostRuleServiceImpl.businessPileOnlineIssueBillingRule dispatchTariffPileInfoResult : {}",
                JSON.toJSONString(dispatchTariffPileInfoResult));

        if (dispatchTariffPileInfoResult == null
                || ObjectUtils.isEmpty(dispatchTariffPileInfoResult.getData())
                || dispatchTariffPileInfoResult.getData().get(0) == null) {
            return false;
        }

        List<Long> tariffIdList = new ArrayList<>();
        tariffIdList.add(opLocationEvseEntity.getTariffId());
        Result<Map<Long, TariffIssuedVO>> tariffWhetherToSupportIssuedResult = tariffFeignClient.tariffWhetherToSupportIssued(tariffIdList);

        log.info("===>>> DistributeCostRuleServiceImpl.businessPileOnlineIssueBillingRule tariffWhetherToSupportIssuedResult : {}",
                JSON.toJSONString(tariffWhetherToSupportIssuedResult));

        if (tariffWhetherToSupportIssuedResult == null
                || ObjectUtils.isEmpty(tariffWhetherToSupportIssuedResult.getData())
                || tariffWhetherToSupportIssuedResult.getData().get(opLocationEvseEntity.getTariffId()) == null) {
            return false;
        }

        TariffIssuedVO tariffIssuedVO = tariffWhetherToSupportIssuedResult.getData().get(opLocationEvseEntity.getTariffId());
        DispatchTariffPileInfoVO dispatchTariffPileInfoVO = dispatchTariffPileInfoResult.getData().get(0);

        List<DispatchTariffDTO> dispatchTariffDTOList = new ArrayList<>();

        DispatchTariffDTO dispatchTariffDTO = new DispatchTariffDTO();

        dispatchTariffDTO.setTariffId(tariffIssuedVO.getTariffId());
        dispatchTariffDTO.setPileSn(dispatchTariffPileInfoVO.getSn());
        dispatchTariffDTO.setFlagIssue(tariffIssuedVO.getFlagIssue());
        dispatchTariffDTO.setNewVersionCostModelRule(tariffIssuedVO.getNewVersionCostModelRule());
        dispatchTariffDTO.setType(dispatchTariffPileInfoVO.getType());
        dispatchTariffDTO.setSupportIssuedIdleFee(dispatchTariffPileInfoVO.getSupportIssuedIdleFee());

        dispatchTariffDTOList.add(dispatchTariffDTO);

        Map<String, List<DispatchTariffVO>> operateAndDispatchTariffVOListMap = TariffUtil.buildDispatchMap(dispatchTariffDTOList);
        if (ObjectUtils.isEmpty(operateAndDispatchTariffVOListMap)) {
            return true;
        }

        List<DispatchTariffVO> needDispatchTariffPileSnList = operateAndDispatchTariffVOListMap.get(DispatchConstant.ISSUED);
        if (ObjectUtils.isNotEmpty(needDispatchTariffPileSnList)) {
            this.batchDispatchCostRule(needDispatchTariffPileSnList);
        }

        List<DispatchTariffVO> needClearTariffPileSnList = operateAndDispatchTariffVOListMap.get(DispatchConstant.CLEAR);
        if (clearFlag
                && ObjectUtils.isNotEmpty(needClearTariffPileSnList)) {
            this.batchClearBillingRule(needClearTariffPileSnList
                    .stream()
                    .filter(var -> var != null && org.apache.commons.lang3.StringUtils.isNotBlank(var.getPileSn()))
                    .map(DispatchTariffVO::getPileSn)
                    .distinct()
                    .collect(Collectors.toList()));
        }
        return true;
    }

    @Override
    public Boolean issueBillingRule(List<String> pileSnList) {

        log.info("===>>> DistributeCostRuleServiceImpl.issueBillingRule pileSnList : {}",
                JSON.toJSONString(pileSnList));

        if (ObjectUtils.isEmpty(pileSnList)) {
            return false;
        }

        for (String pileSn : pileSnList) {
            this.businessPileOnlineIssueBillingRule(pileSn, true);
        }

        return null;
    }

    private void batchDispatchCostRule(List<DispatchTariffVO> needDispatchTariffPileSnList) {

        log.info("===>>> DistributeCostRuleServiceImpl.batchDispatchCostRule needDispatchTariffPileSnList : {}",
                JSON.toJSONString(needDispatchTariffPileSnList));

        if (ObjectUtils.isEmpty(needDispatchTariffPileSnList)) {
            return;
        }

        List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
        for (DispatchTariffVO dispatchTariffVO : needDispatchTariffPileSnList) {

            if (dispatchTariffVO == null
                    || StringUtils.isBlank(dispatchTariffVO.getPileSn())
                    || dispatchTariffVO.getTariffId() == null) {
                continue;
            }

            CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();

            costRuleDispatchPileSnDTO.setPileSn(dispatchTariffVO.getPileSn());
            costRuleDispatchPileSnDTO.setTariffId(dispatchTariffVO.getTariffId());

            String evseSn = dispatchTariffVO.getPileSn() + "_" + "1";
            OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseService.getLocationEvseVOBySnAndGunNo(evseSn).getData();
            if (ocppLocationEVSEVO != null) {
                costRuleDispatchPileSnDTO.setStationTimezone(ocppLocationEVSEVO.getTimeZone());
            }

            costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
        }

        Result<Boolean> costRuleBatchDispatchResult = protocolFeignClient.costRuleBatchDispatch(costRuleBatchDispatchPileSnList);

        log.info("===>>> DistributeCostRuleServiceImpl.batchDispatchCostRule costRuleBatchDispatchResult : {}",
                JSON.toJSONString(costRuleBatchDispatchResult));

    }

    private void batchClearBillingRule(List<String> pileSnList) {

        log.info("===>>> DistributeCostRuleServiceImpl.batchClearBillingRule pileSnList : {}",
                JSON.toJSONString(pileSnList));

        if (ObjectUtils.isEmpty(pileSnList)) {
            return;
        }

        Result<Map<String, Boolean>> pileSnAndSuccessSignMapResult = protocolFeignClient.batchClearBillingRule(pileSnList);

        log.info("===>>> DistributeCostRuleServiceImpl.batchClearBillingRule pileSnAndSuccessSignMapResult : {}",
                JSON.toJSONString(pileSnAndSuccessSignMapResult));

    }
}
