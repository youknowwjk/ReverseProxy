package com.autel.cloud.pile.base.domain.strategy.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.OncePerRequestConvert;
import com.autel.cloud.pile.base.domain.strategy.DeliveryStrategy;
import com.autel.cloud.pile.base.dto.DeliveryGroupDTO;
import com.autel.cloud.pile.base.dto.FirstDownThenUpDTO;
import com.autel.cloud.pile.base.enums.NumberPhasesEnum;
import com.autel.cloud.pile.base.util.DlbUtil;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import com.autel.cloud.pile.base.vo.PlanTimeVO;
import com.autel.cloud.smart.charge.dto.DeliveryParamDTO;
import com.autel.cloud.smart.charge.feign.SmartChargeFeign;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2023/2/7 10:27
 */
@Component(value = BaseConstant.IMMEDIATELY_STRATEGY)
@Slf4j
public class ImmediatelyStrategy implements DeliveryStrategy {
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private SmartChargeFeign smartChargeFeign;
    @Resource
    private RabbitTemplate rabbitTemplate;
    @Value("${spring.rabbitmq.version.suffix:}")
    private String rabbitmqVersionSuffix;
    @Resource
    private OncePerRequestConvert oncePerRequestConvert;

    @Override
    public void dealSafeStragety(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList) {
        log.info("ImmediatelyStrategy,before delivery deliveryVOList={}", JSON.toJSONString(deliveryVOList));
        if (!CollectionUtils.isEmpty(deliveryVOList)) {
            List<OpLocationPileGroupDeliveryVO> tempList = new ArrayList<>(deliveryVOList);
            tempList.stream().forEach(vo -> {
                if (vo.getChargingUp() != null) {
                    String chargingUpUnit = vo.getChargingUpUnit();
                    //最大电流是0A的桩过滤掉
                    if ("A".equals(chargingUpUnit) && vo.getAmperage() != null && BigDecimal.ZERO.compareTo(vo.getAmperage()) == 0) {
                        deliveryVOList.remove(vo);
                    }
                    //下发值小于0时特殊处理
                    BigDecimal chargingUp = vo.getChargingUp();
                    if (chargingUp.compareTo(BigDecimal.ZERO) < 0) {
                        DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                        tmpDto.setChargingUpUnit(vo.getChargingUpUnit());
                        tmpDto.setMinReserve(0);
                        tmpDto.setLoadType(vo.getLoadType());
                        tmpDto.setPowerEquipmentEnabled(vo.getPowerEquipmentEnabled());
                        vo.setChargingUp(DlbUtil.getMinValue(tmpDto));
                        log.info("ImmediatelyStrategy,before delivery chargingUp less than zero vo={}", JSON.toJSONString(vo));
                    }
                }
            });
            if (CollectionUtils.isEmpty(deliveryVOList)) {
                log.info("ImmediatelyStrategy,before delivery deliveryVOList is empty");
                return;
            }
            deliveryVOList.stream().forEach(vo -> {
                Boolean securityEnabled = vo.getSecurityEnabled();
                String zoneId = vo.getZoneId();
                LocalDateTime now = LocalDateTime.now(ZoneId.of(zoneId));
                LocalDateTime dateTime = now.atZone(ZoneId.of(zoneId)).withZoneSameInstant(ZoneId.of(BaseConstant.GMT)).toLocalDateTime();
                //功率优先
                if (vo.getChargingUp() != null && vo.getEnergyUseStrategy() != null && vo.getEnergyUseStrategy() == 0) {
                    PlanTimeVO planTimeVO = new PlanTimeVO();
                    planTimeVO.setBeginTime("00:00");
                    planTimeVO.setChargingUp(vo.getChargingUp());
                    planTimeVO.setStartPeriod(0);
                    vo.setStartSchedule(this.setStartSchedule(now.toLocalTime().format(DateTimeFormatter.ofPattern("HH:mm:ss")), zoneId));
                    vo.setPlanTimeList(Collections.singletonList(planTimeVO));
                    //是否开启安全充电，处理缓升速降逻辑分段逻辑
                    if (securityEnabled != null && securityEnabled && DlbUtil.isCharging(vo.getState())) {
                        if (vo.getUpload() == null || vo.getUpload().compareTo(BigDecimal.ZERO) <= 0) {
                            log.info("ImmediatelyStrategy,before upload is 0");
                            return;
                        }
                        BigDecimal lastValue = vo.getUpload();
                        DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                        tmpDto.setChargingUpUnit(vo.getChargingUpUnit());
                        tmpDto.setMinReserve(1);
                        tmpDto.setLoadType(vo.getLoadType());
                        tmpDto.setPowerEquipmentEnabled(vo.getPowerEquipmentEnabled());
                        BigDecimal minValue = DlbUtil.getMinValue(tmpDto);
                        //小于最小值跳过
                        if (lastValue == null || lastValue.compareTo(minValue) <= 0) {
                            return;
                        }
                        if (DlbUtil.powerRamp(vo.getChargingUpUnit(), vo.getChargingUp(), lastValue)) {
                            List<PlanTimeVO> planTmp = new ArrayList<>();
                            BigDecimal offset = vo.getChargingUp().subtract(lastValue);
                            Integer upDuration = vo.getUpDuration();
                            if (upDuration == null || upDuration.intValue() > 5 || upDuration.intValue() < 1) {
                                upDuration = 5;
                            }
                            int t = upDuration * 60 / 5;
                            for (int i = 0; i <= 5; i++) {
                                PlanTimeVO timeVO = new PlanTimeVO();
                                timeVO.setBeginTime(dateTime.plusMinutes(Long.valueOf(i)).format(DateTimeFormatter.ofPattern("HH:mm:ss")));
                                timeVO.setChargingUp(lastValue.add(offset.multiply(new BigDecimal("0.2")).multiply(BigDecimal.valueOf(i))));
                                timeVO.setStartPeriod(t * i);
                                planTmp.add(timeVO);
                            }
                            vo.setPlanTimeList(planTmp);
                            log.info("ImmediatelyStrategy,before delivery current={},last={},planTmp={}", vo.getChargingUp(), lastValue, JSON.toJSONString(planTmp));
                        }
                    }
                }
                //箱数设置
                vo.setPhaseNum(NumberPhasesEnum.getPhaseNum(vo.getPowerType()));
            });
            //是否先降后升，处理缓升速降逻辑
            for (OpLocationPileGroupDeliveryVO deliveryVO : deliveryVOList) {
                String state = deliveryVO.getState();
                String evseSn = deliveryVO.getEvseSn();
                if (EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
                    deliveryVO.setFirstDownThenUp(DlbUtil.firstDownThenUp(deliveryVO.getChargingUpUnit(), DlbUtil.getSettingValue(deliveryVO), deliveryVO.getUpload()));
                    String key = "energy:pile:base:charging:start:evse_sn:" + evseSn;
                    if (stringRedisTemplate.hasKey(key)) {
                        deliveryVO.setFirstDownThenUp(2);
                    }
                    Integer loadType = deliveryVO.getLoadType();
                    Boolean powerEquipmentEnabled = deliveryVO.getPowerEquipmentEnabled();
                    //多电源模式跳过
                    if (loadType != null && loadType == 1 && powerEquipmentEnabled != null && powerEquipmentEnabled) {
                        deliveryVO.setFirstDownThenUp(0);
                    }
                } else {
                    deliveryVO.setUpload(BigDecimal.ZERO);
                }
            }
        }
    }

    private String setStartSchedule(String beginTime, String zoneId) {
        LocalTime localTime = LocalTime.parse(beginTime).plusMinutes(-1L);
        LocalDateTime localDateTime = LocalDateTime.now(ZoneId.of(zoneId)).withHour(localTime.getHour()).withMinute(localTime.getMinute()).withSecond(localTime.getSecond()).withNano(0);
        LocalDateTime dateTime = localDateTime.atZone(ZoneId.of(zoneId)).withZoneSameInstant(ZoneId.of(BaseConstant.GMT)).toLocalDateTime();
        return dateTime.format(DateTimeFormatter.ofPattern(BaseConstant.DEFAULT_LONG_PATTERN));
    }

    @Override
    public void delivery(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList) {
        log.info("ImmediatelyStrategy,delivery deliveryVOList={}", JSON.toJSONString(deliveryVOList));
        if (groupId != null) {
            //处理安全策略
            this.dealSafeStragety(groupId, deliveryVOList);
            //设置更新标识
            String eventKey = RedisKeyConstant.getPileGroupDeliveryEventKey(groupId);
            stringRedisTemplate.opsForValue().set(eventKey, groupId.toString(), 11L, TimeUnit.SECONDS);
        }
        DeliveryParamDTO dto = new DeliveryParamDTO();
        dto.setGroupId(groupId);
        dto.setDeliveryVoList(deliveryVOList);
        if (groupId == null) {
            dto.setUsageScenario(2);
        } else {
            //是否先降后升
            this.checkFirstDown(dto);
        }
        //这里新增或者禁用子群组时会下发一份默认配置txDefaultProfile，以保证桩有一份默认配置
        if (!CollectionUtils.isEmpty(deliveryVOList) && groupId != null) {
            List<OpLocationPileGroupDeliveryVO> chargingList = deliveryVOList.stream().filter(vo -> EvseDeviceStatusEnum.CHARGING.getName().equals(vo.getState())).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(chargingList)) {
                List<OpLocationPileGroupDeliveryVO> defaultList = new ArrayList<>();
                chargingList.stream().forEach(vo -> {
                    Integer updateType = vo.getUpdateType();
                    if (updateType != null && (updateType == 1 || updateType == 6)) {
                        OpLocationPileGroupDeliveryVO copy = oncePerRequestConvert.copy(vo);
                        copy.setState(EvseDeviceStatusEnum.DEFAULT.getName());
                        PlanTimeVO planTimeVO = new PlanTimeVO();
                        planTimeVO.setBeginTime("00:00");
                        DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                        tmpDto.setChargingUpUnit(vo.getChargingUpUnit());
                        tmpDto.setMinReserve(vo.getMinReserve());
                        tmpDto.setLoadType(vo.getLoadType());
                        tmpDto.setPowerEquipmentEnabled(vo.getPowerEquipmentEnabled());
                        planTimeVO.setChargingUp(DlbUtil.getMinValue(tmpDto));
                        planTimeVO.setStartPeriod(0);
                        copy.setPlanTimeList(Collections.singletonList(planTimeVO));
                        defaultList.add(copy);
                    }
                });
                if (defaultList.size() > 0) {
                    log.info("ImmediatelyStrategy,delivery defaultList={}", JSON.toJSONString(defaultList));
                    DeliveryParamDTO defaultDto = new DeliveryParamDTO();
                    defaultDto.setGroupId(groupId);
                    defaultDto.setDeliveryVoList(defaultList);
                    Result<Boolean> defaultResult = smartChargeFeign.deliveryChargeProfile(defaultDto);
                    log.info("ImmediatelyStrategy,delivery defaultResult={}", defaultResult);
                }
            }
        }
        //ALM配置一样不下发
        if (this.checkEqual(deliveryVOList, groupId)) {
            return;
        }
        Result<Boolean> booleanResult = smartChargeFeign.deliveryChargeProfile(dto);
        this.updateDeliverCache(groupId, deliveryVOList);
        log.info("ImmediatelyStrategy,delivery booleanResult={}", booleanResult);
    }

    private Boolean checkEqual(List<OpLocationPileGroupDeliveryVO> deliveryVoList, Long groupId) {
        if (!this.checkLimit(groupId)) {
            return false;
        }
        String key = "energy:pile:base:alm:limit:deliveryList:" + groupId;
        String redisValue = stringRedisTemplate.opsForValue().get(key);
        if (!StringUtils.hasText(redisValue)) {
            return false;
        }
        List<OpLocationPileGroupDeliveryVO> lastList = JSON.parseArray(redisValue, OpLocationPileGroupDeliveryVO.class);
        if (lastList.size() != deliveryVoList.size()) {
            return false;
        }
        Map<String, OpLocationPileGroupDeliveryVO> lastMap = lastList.stream().collect(Collectors.toMap(OpLocationPileGroupDeliveryVO::getEvseSn, e -> e, (f, s) -> f));
        for (OpLocationPileGroupDeliveryVO vo : deliveryVoList) {
            String evseSn = vo.getEvseSn();
            OpLocationPileGroupDeliveryVO lastVo = lastMap.get(evseSn);
            if (lastVo == null) {
                return false;
            }
            if (vo.getUpdateType() != 8) {
                return false;
            }
            List<PlanTimeVO> planTimeList = vo.getPlanTimeList();
            List<PlanTimeVO> lastPlanTimeList = lastVo.getPlanTimeList();
            if (planTimeList.size() != lastPlanTimeList.size()) {
                return false;
            }
            Map<Integer, PlanTimeVO> map = lastPlanTimeList.stream().collect(Collectors.toMap(PlanTimeVO::getStartPeriod, e -> e, (f, s) -> f));
            for (PlanTimeVO planTimeVO : planTimeList) {
                Integer startPeriod = planTimeVO.getStartPeriod();
                PlanTimeVO lastPlanTimeVO = map.get(startPeriod);
                if (lastPlanTimeVO == null) {
                    return false;
                }
                if (!DlbUtil.checkEqual(planTimeVO.getChargingUp(), lastPlanTimeVO.getChargingUp(), vo.getChargingUpUnit())) {
                    return false;
                }
            }
        }
        log.info("ImmediatelyStrategy,delivery lastList={}", JSON.toJSONString(lastList));
        return true;
    }

    private void checkFirstDown(DeliveryParamDTO dto) {
        List<OpLocationPileGroupDeliveryVO> deliveryVoList = dto.getDeliveryVoList();
        if (!CollectionUtils.isEmpty(deliveryVoList)) {
            Long groupId = dto.getGroupId();
            //List<OpLocationPileGroupDeliveryVO> list = JSON.parseArray(JSON.toJSONString(deliveryVoList), OpLocationPileGroupDeliveryVO.class);
            List<OpLocationPileGroupDeliveryVO> firstDownList = deliveryVoList.stream().filter(vo -> vo.getFirstDownThenUp() == 1).collect(Collectors.toList());
            boolean flag = false;
            if (!CollectionUtils.isEmpty(firstDownList)) {
                log.info("ImmediatelyStrategy,delivery firstDownList={}", JSON.toJSONString(firstDownList));
                List<OpLocationPileGroupDeliveryVO> upList = deliveryVoList.stream().filter(vo -> vo.getFirstDownThenUp() == 2).collect(Collectors.toList());
                if (!CollectionUtils.isEmpty(upList)) {
                    log.info("ImmediatelyStrategy,delivery upList={}", JSON.toJSONString(upList));
                    upList.stream().forEach(vo -> {
                        if (EvseDeviceStatusEnum.CHARGING.getName().equals(vo.getState())) {
                            DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                            tmpDto.setChargingUpUnit(vo.getChargingUpUnit());
                            tmpDto.setMinReserve(vo.getMinReserve());
                            tmpDto.setLoadType(vo.getLoadType());
                            tmpDto.setPowerEquipmentEnabled(vo.getPowerEquipmentEnabled());
                            BigDecimal minValue = DlbUtil.getMinValue(tmpDto);
                            BigDecimal settingValue = DlbUtil.getSettingValue(vo);
                            String zoneId = vo.getZoneId();
                            //上升按默认配置值下发
                            if (settingValue.compareTo(minValue) >= 0) {
                                PlanTimeVO planTimeVO = new PlanTimeVO();
                                planTimeVO.setBeginTime("00:00");
                                planTimeVO.setChargingUp(minValue);
                                planTimeVO.setStartPeriod(0);
                                vo.setPlanTimeList(Collections.singletonList(planTimeVO));
                                LocalDateTime now = LocalDateTime.now(ZoneId.of(zoneId));
                                vo.setStartSchedule(this.setStartSchedule(now.toLocalTime().format(DateTimeFormatter.ofPattern("HH:mm:ss")), zoneId));
                            }
                        }
                    });
                    //设置缓存并发送MQ
                    flag = true;
                }
            }
            String key = RedisKeyConstant.getPileBaseFirstDownThenUpKey(groupId);
            if (flag) {
                FirstDownThenUpDTO firstDownThenUpDTO = new FirstDownThenUpDTO();
                String requestId = IdWorker.getIdStr();
                firstDownThenUpDTO.setGroupId(groupId);
                firstDownThenUpDTO.setRequestId(requestId);
                firstDownThenUpDTO.setDeliveryVOList(deliveryVoList);
                stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(firstDownThenUpDTO), 5L, TimeUnit.MINUTES);
                MessageProperties messageProperties = new MessageProperties();
                //延迟11秒
                messageProperties.setDelay(15000);
                FirstDownThenUpDTO queueDto = new FirstDownThenUpDTO();
                queueDto.setGroupId(groupId);
                queueDto.setRequestId(requestId);
                Message message = new Message((JSON.toJSONString(queueDto)).getBytes(StandardCharsets.UTF_8), messageProperties);
                rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_FIRST_DOWN_THEN_UP_EXCHANGE + rabbitmqVersionSuffix, AmqpConstant.PILE_BASE_FIRST_DOWN_THEN_UP_ROUTE, message);
            } else {
                if (stringRedisTemplate.hasKey(key)) {
                    stringRedisTemplate.delete(key);
                }
            }
        }
    }

    @Override
    public void updateDeliverCache(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList) {
        log.info("ImmediatelyStrategy,after delivery deliveryVOList={}", JSON.toJSONString(deliveryVOList));
        if (!CollectionUtils.isEmpty(deliveryVOList)) {
            deliveryVOList.stream().forEach(vo -> {
                String evseSn = vo.getEvseSn();
                if (DlbUtil.isCharging(vo.getState())) {
                    stringRedisTemplate.opsForValue().set(RedisKeyConstant.getChargeLastDeliveryKey(evseSn), JSON.toJSONString(vo), 30L, TimeUnit.DAYS);
                }
            });
            if (this.checkLimit(groupId)) {
                String key = "energy:pile:base:alm:limit:deliveryList:" + groupId;
                stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(deliveryVOList), 10L, TimeUnit.MINUTES);
            }
        }
    }

    private Boolean checkLimit(Long rootId) {
        String key = "energy:pile:base:alm:delivery:rooId:" + rootId;
        return stringRedisTemplate.hasKey(key);
    }
}
