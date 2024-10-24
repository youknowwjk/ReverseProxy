package com.autel.cloud.pile.base.domain.strategy.impl;

import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.strategy.DeliveryStrategy;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import com.autel.cloud.pile.base.vo.PlanTimeVO;
import com.autel.cloud.tariff.dto.CostRulesDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoField;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * 延迟下发
 *
 * @Author temp
 * @Date 2023/2/7 10:30
 */
@Component(value = BaseConstant.DELAY_STRATEGY)
@Slf4j
public class DelayStrategy implements DeliveryStrategy {
    @Resource
    private ImmediatelyStrategy immediatelyStrategy;

    @Override
    public void dealSafeStragety(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList) {

    }

    @Override
    public void delivery(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList) {
        //第一次下发
        List<OpLocationPileGroupDeliveryVO> minDeliveryVOList = new ArrayList<>(deliveryVOList.size());
        deliveryVOList.stream().forEach(vo -> {
            OpLocationPileGroupDeliveryVO pileGroupDeliveryVO = new OpLocationPileGroupDeliveryVO();
            BeanUtils.copyProperties(vo, pileGroupDeliveryVO);
            List<PlanTimeVO> planTimeList = vo.getPlanTimeList();
            pileGroupDeliveryVO.setPlanTimeList(this.setPlanTimeList(planTimeList));
            pileGroupDeliveryVO.setStartSchedule(this.setStartSchedule(pileGroupDeliveryVO.getPlanTimeList().get(0).getBeginTime(), vo.getZoneId()));
            minDeliveryVOList.add(pileGroupDeliveryVO);
        });
        immediatelyStrategy.delivery(groupId, minDeliveryVOList);
    }

    @Override
    public void updateDeliverCache(Long groupId, List<OpLocationPileGroupDeliveryVO> deliveryVOList) {

    }

    private String setStartSchedule(String beginTime, String zoneId) {
        LocalTime localTime = LocalTime.parse(beginTime);
        LocalDateTime localDateTime = LocalDateTime.now(ZoneId.of(zoneId)).withHour(localTime.getHour()).withMinute(localTime.getMinute()).withSecond(0).withNano(0);
        LocalDateTime dateTime = localDateTime.atZone(ZoneId.of(zoneId)).withZoneSameInstant(ZoneId.of(BaseConstant.GMT)).toLocalDateTime();
        return dateTime.format(DateTimeFormatter.ofPattern(BaseConstant.DEFAULT_LONG_PATTERN));
    }

    private List<PlanTimeVO> setPlanTimeList(List<PlanTimeVO> planTimeList) {
        //按充电计划排序
        List<PlanTimeVO> afterList = planTimeList.stream().sorted((f, s) -> (int) f.getStartPeriod().compareTo(s.getStartPeriod())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(afterList)) {
            PlanTimeVO firstVo = afterList.get(0);
            PlanTimeVO vo = new PlanTimeVO();
            vo.setChargingUp(firstVo.getChargingUp());
            vo.setFastValue(firstVo.getFastValue());
            vo.setStartPeriod(0);
            vo.setBeginTime("00:00");
            afterList.add(vo);
        }
        afterList.stream().filter(vo -> vo.getStartPeriod() == null).forEach(vo -> vo.setStartPeriod(Long.valueOf(LocalTime.parse(vo.getBeginTime()).getLong(ChronoField.SECOND_OF_DAY)).intValue()));
        List<PlanTimeVO> resultList = afterList.stream().sorted(Comparator.comparing(PlanTimeVO::getStartPeriod)).collect(Collectors.toList());
        return resultList;
    }

    private List<CostRulesDTO> getRuleDtoList(List<CostRulesDTO> rulesDtos, LocalDateTime now, String planTime) {
        if (CollectionUtils.isEmpty(rulesDtos)) {
            return null;
        }
        //格式化时间
        String time = now.toLocalTime().format(DateTimeFormatter.ofPattern("HH:mm"));
        LocalTime localTime = LocalTime.parse(time);
        //24:00处理
        rulesDtos.stream().forEach(dto -> {
            if ("24:00".equalsIgnoreCase(dto.getEndTime())) {
                dto.setEndTime("23:59");
            }
        });
        List<CostRulesDTO> tempList = rulesDtos.stream().filter(dto -> (LocalTime.parse(dto.getBeginTime()).compareTo(localTime) <= 0 && LocalTime.parse(dto.getEndTime()).compareTo(localTime) > 0) || LocalTime.parse(dto.getBeginTime()).compareTo(localTime) > 0).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(tempList)) {
            return null;
        }
        //截取有效时间
        tempList.stream().forEach(dto -> {
            if (LocalTime.parse(dto.getBeginTime()).compareTo(localTime) <= 0 && LocalTime.parse(dto.getEndTime()).compareTo(localTime) > 0) {
                dto.setBeginTime(localTime.toString());
            }
        });
        //按电价排序
        List<CostRulesDTO> dtoList = tempList.stream().sorted((f, s) -> f.getUnitPrice().subtract(s.getUnitPrice()).multiply(BigDecimal.valueOf(10000)).intValue()).collect(Collectors.toList());
        List<CostRulesDTO> dtoListTemp = new ArrayList<>();
        Long value = TimeUnit.HOURS.toMinutes(Integer.valueOf(planTime));
        for (CostRulesDTO dto : dtoList) {
            String beginTime = dto.getBeginTime();
            String endTime = dto.getEndTime();
            Long offset = LocalTime.parse(endTime).getLong(ChronoField.MINUTE_OF_DAY) - LocalTime.parse(beginTime).getLong(ChronoField.MINUTE_OF_DAY);
            dtoListTemp.add(dto);
            if (offset >= value) {
                break;
            } else {
                value = value - offset;
            }
        }
        //按时间排序
        List<CostRulesDTO> resultList = dtoListTemp.stream().sorted((f, s) -> (int) LocalTime.parse(f.getBeginTime()).compareTo(LocalTime.parse(s.getBeginTime()))).collect(Collectors.toList());
        return resultList;
    }
}
