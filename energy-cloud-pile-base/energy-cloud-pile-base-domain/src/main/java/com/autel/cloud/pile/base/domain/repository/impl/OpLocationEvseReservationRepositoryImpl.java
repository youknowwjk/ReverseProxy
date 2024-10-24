package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.date.DateUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseReservationRepository;
import com.autel.cloud.pile.base.domain.service.CommonUtilService;
import com.autel.cloud.pile.base.dto.ReservationDetailDTO;
import com.autel.cloud.pile.base.dto.ReservationPriceDTO;
import com.autel.cloud.pile.base.enums.ConnectorGunTypeEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.util.TimeZoneUtil;
import com.autel.cloud.pile.base.vo.ReservationDetailVO;
import com.autel.cloud.tariff.dto.CostRuleWeeksDTO;
import com.autel.cloud.tariff.dto.CostRulesDTO;
import com.autel.cloud.tariff.enums.ChargeTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.rule.dto.*;
import com.autel.cloud.tariff.vo.CostModelBasicInfoVO;
import com.autel.cloud.tariff.vo.CostModelRuleEntityVO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.common.geo.GeoDistance;
import org.elasticsearch.common.unit.DistanceUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

/**
 * @author A22587
 */
@Service
@Slf4j
public class OpLocationEvseReservationRepositoryImpl implements OpLocationEvseReservationRepository {

    @Resource
    private OpLocationElastic opLocationElastic;

    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Resource
    private TariffFeignClient tariffFeignClient;

    @Autowired
    private CommonUtilService commonUtilService;

    private static final String MINUTE_STRING = "M";
    private static final String HOUR_STRING = "H";
    private static final Double MIN_POWER = 50D;

    @Override
    public Result<ReservationDetailVO> reservationInformationDisplay(ReservationDetailDTO reservationDetailDTO) {

        ReservationDetailVO reservationDetailVO = new ReservationDetailVO();
        Long stationId = reservationDetailDTO.getStationId();

        Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(stationId);
        log.info("reservationInformationDisplay.optionalOpLocationElasticDTO:{}", JSON.toJSONString(optionalOpLocationElasticDTO));
        if (optionalOpLocationElasticDTO.isPresent()
                && optionalOpLocationElasticDTO.get().getPlatform() != 2
//                && optionalOpLocationElasticDTO.get().getReservationEnabled()
        ) {

            OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();

            String sn = reservationDetailDTO.getEvseSn().split("_")[0];
            reservationDetailVO.setEvseSn(reservationDetailDTO.getEvseSn());
            reservationDetailVO.setGunNo(reservationDetailDTO.getGunNo());
            reservationDetailVO.setConnectorDisplayName(commonUtilService.getConnectorDisplayName(sn, reservationDetailDTO.getGunNo()));
            reservationDetailVO.setLocationName(opLocationElasticDTO.getName());
            reservationDetailVO.setOperatorId(opLocationElasticDTO.getOperatorId());
            reservationDetailVO.setZoneId(opLocationElasticDTO.getZoneId());
            //距离
            if (reservationDetailDTO.getLatitude() != null && reservationDetailDTO.getLongitude() != null) {
                try {
                    double calculateDistance = GeoDistance.ARC.calculate(Double.valueOf(opLocationElasticDTO.getLatitude()), Double.valueOf(opLocationElasticDTO.getLongitude()),
                            reservationDetailDTO.getLatitude(), reservationDetailDTO.getLongitude(), DistanceUnit.KILOMETERS);
                    reservationDetailVO.setDistance(Double.valueOf(String.format("%.2f", calculateDistance)));
                } catch (Exception ignored) {
                }
            }
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findByEvseSn(reservationDetailDTO.getEvseSn());
            if (ObjectUtils.isEmpty(opLocationEvseElasticDTO)) {
                return Result.ofSucceed(reservationDetailVO);
            }
            log.info("==>>reservationInformationDisplay.opLocationEvseElasticDTO:{}", JSON.toJSONString(opLocationEvseElasticDTO));
            reservationDetailVO.setStationId(opLocationEvseElasticDTO.getLocationId());
            reservationDetailVO.setPower(opLocationEvseElasticDTO.getPower());
            String pileName = reservationDetailDTO.getEvseSn().split("_")[0];
            if (!org.springframework.util.StringUtils.isEmpty(opLocationEvseElasticDTO.getPileName())) {
                pileName = opLocationEvseElasticDTO.getPileName();
            }
            reservationDetailVO.setPileSnName(pileName);
            reservationDetailVO.setGunType(opLocationEvseElasticDTO.getGunType());
            reservationDetailVO.setGunTypeName(ConnectorGunTypeEnum.getEnumByCode(opLocationEvseElasticDTO.getGunType()).getName());
            reservationDetailVO.setStateCode(EvseDeviceStatusEnum.getEnumByName(opLocationEvseElasticDTO.getState()).getCode());
            reservationDetailVO.setEvseId(opLocationEvseElasticDTO.getId().toString());
            reservationDetailVO.setTariffId(opLocationEvseElasticDTO.getTariffId());
            Result<CostModelBasicInfoVO> costModelBasicInfoVOResult = tariffFeignClient.queryTariffBasicInfoById(opLocationEvseElasticDTO.getTariffId());
            if (null != costModelBasicInfoVOResult && null != costModelBasicInfoVOResult.getData()) {
                CostModelBasicInfoVO costModelBasicInfoVO = costModelBasicInfoVOResult.getData();
                reservationDetailVO.setRuleModelType(costModelBasicInfoVO.getRuleModelType());
            }
            //经纬度、距离
            Double lat = opLocationElasticDTO.getLatitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLatitude());
            Double lon = opLocationElasticDTO.getLongitude() == null ? null : Double.parseDouble(opLocationElasticDTO.getLongitude());
            reservationDetailVO.setLatitude(lat);
            reservationDetailVO.setLongitude(lon);
            //预约信息
            Long tariffId = opLocationEvseElasticDTO.getTariffId();
            Result<CostModelRuleEntityVO> costModelRuleDetailResult = tariffFeignClient.getCostModelRuleDetail(tariffId);
            if (costModelRuleDetailResult != null && costModelRuleDetailResult.getData() != null) {
                CostModelRuleEntityVO costModelRuleEntityVO = costModelRuleDetailResult.getData();
                log.info("reservationInformationDisplay.costModelRuleEntityVO:{}", JSON.toJSONString(costModelRuleEntityVO));
                //电量费
                List<CostRuleWeeksDTO> rules = costModelRuleEntityVO.getRules();
                //时长费
                DurationDTO durationDTO = costModelRuleEntityVO.getDurationDTO();
                // 按时区转换开始时间对应周几以及小时
                String timezoneId = opLocationElasticDTO.getZoneId();
                Long startTimestamp = System.currentTimeMillis();
                LocalDateTime beginChargeTime = TimeZoneUtil.millsToLocalDateTime(startTimestamp, timezoneId);
                String chargeTime = DateUtil.format(beginChargeTime, "HH:mm");
                int beginChargeWeekDay = beginChargeTime.getDayOfWeek().getValue();

                //电量费当前时刻的单价
                BigDecimal electricityPrice = processingElectricityPrice(rules, chargeTime, beginChargeWeekDay);

                //时长费当前时刻的单价
                BigDecimal timePrice = BigDecimal.ZERO;
                if (!ObjectUtils.isEmpty(durationDTO) && ChargeTypeEnum.TIME.getCode().equals(durationDTO.getModel())) {
                    //按时段
                    timePrice = processingDurationThroughThePeriod(durationDTO, chargeTime, beginChargeWeekDay);
                } else if (!ObjectUtils.isEmpty(durationDTO) && ChargeTypeEnum.DURATION.getCode().equals(durationDTO.getModel())) {
                    //按时长
                    timePrice = processingDurationThroughDur(durationDTO, beginChargeWeekDay);
                }
                Double power = opLocationEvseElasticDTO.getPower();
                if (power > MIN_POWER) {
                    power = MIN_POWER;
                }
                log.info("reservationInformationDisplay.electricityPrice:{}, timePrice:{}", JSON.toJSONString(electricityPrice), JSON.toJSONString(timePrice));
                BigDecimal reservationPrice = electricityPrice.multiply(BigDecimal.valueOf(power)).divide(BigDecimal.valueOf(120), 5, BigDecimal.ROUND_HALF_UP)
                        .add(timePrice.divide(BigDecimal.valueOf(2), 5, BigDecimal.ROUND_HALF_UP))
                        .setScale(2, BigDecimal.ROUND_HALF_UP);
                ReservationPriceDTO reservationPriceDTO = new ReservationPriceDTO();
                reservationPriceDTO.setReservationPrice(reservationPrice);
                reservationPriceDTO.setCurrencySign(costModelRuleEntityVO.getCurrencySign());
                reservationPriceDTO.setTimeUnit(MINUTE_STRING);
                reservationPriceDTO.setReservationRuleModelType(2);
                reservationDetailVO.setReservationPriceDTO(reservationPriceDTO);
                reservationDetailVO.setReservationTime(20);
            }

        }
        return Result.ofSucceed(reservationDetailVO);
    }

    /**
     * 电量费处理
     */
    private BigDecimal processingElectricityPrice(List<CostRuleWeeksDTO> rules, String chargeTime, int beginChargeWeekDay) {
        BigDecimal unitPrice = BigDecimal.ZERO;
        AtomicReference<CostRuleWeeksDTO> costRuleWeeksDTO = new AtomicReference<>();
        if (!ObjectUtils.isEmpty(rules)) {
            rules.forEach(temp -> {
                if (temp.getWeeks().contains(beginChargeWeekDay)) {
                    costRuleWeeksDTO.set(temp);
                }
            });
            if (!ObjectUtils.isEmpty(costRuleWeeksDTO)) {
                List<CostRulesDTO> costRulesDTOS = costRuleWeeksDTO.get().getWeeksRules();
                for (CostRulesDTO costRulesDTO : costRulesDTOS) {
                    if (chargeTime.compareTo(costRulesDTO.getBeginTime()) >= 0 && chargeTime.compareTo(costRulesDTO.getEndTime()) < 0) {
                        unitPrice = costRulesDTO.getUnitPrice();
                        break;
                    }
                }
            }
        }
        return unitPrice;
    }

    /**
     * 时长费按照时段收费的处理
     */
    private BigDecimal processingDurationThroughThePeriod(DurationDTO durationDTO, String chargeTime, int beginChargeWeekDay){
        BigDecimal timePrice = BigDecimal.ZERO;
        List<DurationRuleDTO> durationRuleList = durationDTO.getDurationRuleList();
        if (!ObjectUtils.isEmpty(durationRuleList)) {
            DurationPriceDTO durationPriceDTO = new DurationPriceDTO();
            AtomicReference<DurationRuleDTO> durationRuleDTO = new AtomicReference<>();
            durationRuleList.forEach(temp -> {
                if (temp.getWeeks().contains(beginChargeWeekDay)) {
                    durationRuleDTO.set(temp);
                }
            });
            if (null != durationRuleDTO.get()) {
                List<DurationPriceDTO> durationPriceList = durationRuleDTO.get().getDurationPriceList();
                for (DurationPriceDTO temp : durationPriceList) {
                    if (chargeTime.compareTo(temp.getBeginTime()) >= 0 && chargeTime.compareTo(temp.getEndTime()) < 0) {
                        durationPriceDTO = temp;
                        break;
                    }
                }
            }
            String unit = durationPriceDTO.getUnit();
            if (StringUtils.isNotBlank(unit)) {
                timePrice = durationPriceDTO.getTimePrice();
                if (HOUR_STRING.equalsIgnoreCase(unit)) {
                    timePrice = timePrice.divide(BigDecimal.valueOf(60), 5, BigDecimal.ROUND_HALF_UP);
                }
            }
        }
        return timePrice;
    }

    /**
     * 时长费按照时长收费的处理
     */
    private BigDecimal processingDurationThroughDur(DurationDTO durationDTO, int beginChargeWeekDay) {
        BigDecimal timePrice = BigDecimal.ZERO;
        List<DurationRuleByDurDTO> durationRuleByDurList = durationDTO.getDurationRuleByDurList();
        if (!ObjectUtils.isEmpty(durationRuleByDurList)) {
            DurationPriceDTO durationPriceDTO = new DurationPriceDTO();
            AtomicReference<DurationRuleByDurDTO> durationRuleDTO = new AtomicReference<>();
            durationRuleByDurList.forEach(temp -> {
                if (temp.getWeeks().contains(beginChargeWeekDay)) {
                    durationRuleDTO.set(temp);
                }
            });
            if (null != durationRuleDTO.get()) {
                List<BasicPriceByDurationDTO> priceList = durationRuleDTO.get().getPriceList();
                BasicPriceByDurationDTO basicPriceByDurationDTO = priceList.get(0);
                durationPriceDTO.setTimePrice(basicPriceByDurationDTO.getPrice());
                durationPriceDTO.setUnit(basicPriceByDurationDTO.getUnit());
            }
            String unit = durationPriceDTO.getUnit();
            if (StringUtils.isNotBlank(unit)) {
                timePrice = durationPriceDTO.getTimePrice();
                if (HOUR_STRING.equalsIgnoreCase(unit)) {
                    timePrice = timePrice.divide(BigDecimal.valueOf(60), 5, BigDecimal.ROUND_HALF_UP);
                }
            }
        }
        return timePrice;
    }
}
