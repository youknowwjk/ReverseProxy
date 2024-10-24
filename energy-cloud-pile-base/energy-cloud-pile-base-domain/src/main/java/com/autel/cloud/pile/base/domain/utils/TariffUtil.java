package com.autel.cloud.pile.base.domain.utils;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.constant.DispatchConstant;
import com.autel.cloud.pile.base.domain.convert.DozerConvert;
import com.autel.cloud.pile.base.domain.model.ParkingPriceCostRulesDTO;
import com.autel.cloud.pile.base.domain.model.SortedCostRuleWeeksDTO;
import com.autel.cloud.pile.base.domain.model.TimePriceCostRulesDTO;
import com.autel.cloud.pile.base.domain.model.UnitPriceCostRulesDTO;
import com.autel.cloud.pile.base.dto.tariff.DispatchTariffDTO;
import com.autel.cloud.pile.base.enums.device.PileTypeEnum;
import com.autel.cloud.pile.base.vo.DispatchTariffVO;
import com.autel.cloud.pile.base.vo.britishAct.DefaultChargingTimeVO;
import com.autel.cloud.pile.base.vo.britishAct.ThatDayDefaultChargingTimeVO;
import com.autel.cloud.tariff.dto.*;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;

/**
 * @Author MingLong A22599
 * @Date 2022/11/19
 * @Function 计费规则工具类
 */
@Slf4j
public class TariffUtil {

    /**
     * 构造方法私有化
     */
    private TariffUtil() {
    }

    /**
     * @param resourceCostModelRuleDTO
     * @return 针对相同费用的电量费时段，进行时段合并后的结果
     * @function 针对相同费用的电量费时段，进行时段合并
     */
    public static CostModelRuleDTO mergeUnitPrice(CostModelRuleDTO resourceCostModelRuleDTO) {

        log.info("==============>>>>>>>>> TariffUtil.mergeUnitPrice begin resourceCostModelRuleDTO : {}", JSON.toJSONString(resourceCostModelRuleDTO));

        // 不改变源对象的同时，构造出返回对象
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);

        log.info("==============>>>>>>>>> TariffUtil.mergeUnitPrice costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));

        // 获取费用规则
        List<CostRuleWeeksDTO> ruleList = costModelRuleDTO.getRules();

        // 先合并每一天的计费规则（一天中相邻时段的，计费单价相同的两段计费规则需要被合并）
        List<SortedCostRuleWeeksDTO<UnitPriceCostRulesDTO>> ruleListAfterCombine = new ArrayList<>();

        ruleList.forEach(ruleWeeksDTO -> {

            List<UnitPriceCostRulesDTO> ruleAfterCombine = new ArrayList<>();

            List<CostRulesDTO> collect = ruleWeeksDTO.getWeeksRules();

            //先按起始时间，再按结束时间正序排列
            Collections.sort(collect);

            for (int i = 0; i < collect.size(); i++) {
                if (i < collect.size() - 1) {
                    CostRulesDTO costRules = collect.get(i);
                    CostRulesDTO costRulesInfo = collect.get(i + 1);
                    Boolean neededCombine = TariffUtil.isNeededCombinePrice(costRules.getUnitPrice(), costRulesInfo.getUnitPrice());
                    if (neededCombine) {
                        CostRulesDTO costRulesDTO = new CostRulesDTO();
                        costRulesDTO.setBeginMinute(costRules.getBeginMinute());
                        costRulesDTO.setBeginHour(costRules.getBeginHour());
                        costRulesDTO.setBeginTime(costRules.getBeginTime());
                        costRulesDTO.setEndMinute(costRulesInfo.getEndMinute());
                        costRulesDTO.setEndHour(costRulesInfo.getEndHour());
                        costRulesDTO.setEndTime(costRulesInfo.getEndTime());
                        costRulesDTO.setUnitPrice(costRules.getUnitPrice());
                        collect.set(i, costRulesDTO);
                        collect.remove(i + 1);
                        i--;
                    }
                }
            }

            //先按起始时间，再按结束时间正序排列
            Collections.sort(collect);

            for (CostRulesDTO costRulesDTO : collect) {
                UnitPriceCostRulesDTO unitPriceCostRulesDTO = DozerConvert.map(costRulesDTO, UnitPriceCostRulesDTO.class);
                ruleAfterCombine.add(unitPriceCostRulesDTO);
            }

            ruleWeeksDTO.getWeeks().forEach(week -> {
                SortedCostRuleWeeksDTO<UnitPriceCostRulesDTO> temp = new SortedCostRuleWeeksDTO<>();
                temp.setWeeks(Collections.singletonList(week));
                temp.setWeeksRules(ruleAfterCombine);
                ruleListAfterCombine.add(temp);
            });
        });

        // todo 需要对ruleListAfterCombine对象进行排序，按照星期顺序
        Collections.sort(ruleListAfterCombine);

        List<CostRuleWeeksDTO> rules = new ArrayList<>();

        for (int i = 0; i < ruleListAfterCombine.size(); i++) {
            if (i < ruleListAfterCombine.size() - 1) {
                // 第i天的计费对象
                SortedCostRuleWeeksDTO<UnitPriceCostRulesDTO> costRuleWeeksDTO = ruleListAfterCombine.get(i);
                // 第i + 1天的计费对象
                SortedCostRuleWeeksDTO<UnitPriceCostRulesDTO> secCostRuleWeeksDTO = ruleListAfterCombine.get(i + 1);

                // 第i天的计费对象的电量费计费规则
                List<UnitPriceCostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                // 第i + 1天的计费对象的电量费计费规则
                List<UnitPriceCostRulesDTO> secWeeksRules = secCostRuleWeeksDTO.getWeeksRules();

                // 第i天的计费对象的适用星期数
                List<Integer> weeks = costRuleWeeksDTO.getWeeks();
                // 第i + 1天的计费对象的适用星期数
                List<Integer> secWeeks = secCostRuleWeeksDTO.getWeeks();

                Boolean neededCombine = weeksRules.hashCode() == secWeeksRules.hashCode();

                if (neededCombine) {

                    // 重新声明变量
                    List<Integer> result = new ArrayList<>();

                    result.addAll(weeks);
                    // 相邻的两天计费规则需要合并（第i天合并掉第i + 1天）
                    result.addAll(secWeeks);

                    // 第i天的计费对象的适用星期数需要重新赋值
                    costRuleWeeksDTO.setWeeks(result);

                    // 集合中第i天的计费对象需要重新赋值
                    ruleListAfterCombine.set(i, costRuleWeeksDTO);

                    // 移除掉被合并的第i + 1天的计费对象
                    ruleListAfterCombine.removeIf(var -> var.equals(secCostRuleWeeksDTO));

                    i--;
                }
            }
        }

        ruleListAfterCombine.forEach(resourceCostModelRule -> {
            CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
            costRuleWeeksDTO.setWeeks(resourceCostModelRule.getWeeks());

            List<UnitPriceCostRulesDTO> unitPriceCostRulesDTOList = resourceCostModelRule.getWeeksRules();
            List<CostRulesDTO> weeksRules = new ArrayList<>();

            for (UnitPriceCostRulesDTO unitPriceCostRulesDTO : unitPriceCostRulesDTOList) {
                CostRulesDTO costRulesDTO = DozerConvert.map(unitPriceCostRulesDTO, CostRulesDTO.class);
                weeksRules.add(costRulesDTO);
            }

            costRuleWeeksDTO.setWeeksRules(weeksRules);

            rules.add(costRuleWeeksDTO);
        });

        costModelRuleDTO.setRules(rules);

        // todo 如果七天的电量费都为0，不展示
        Integer type = 1;
        if (TariffUtil.oneWeekFree(costModelRuleDTO, type)) {
            costModelRuleDTO = new CostModelRuleDTO();
        }

        log.info("==============>>>>>>>>> TariffUtil.mergeUnitPrice after costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));

        return costModelRuleDTO;
    }

    /**
     * @param resourceCostModelRuleDTO
     * @return 针对相同费用的时长费时段，进行时段合并后的结果
     * @function 针对相同费用的时长费时段，进行时段合并
     */
    public static CostModelRuleDTO mergeTimePrice(CostModelRuleDTO resourceCostModelRuleDTO) {

        log.info("==============>>>>>>>>> TariffUtil.mergeTimePrice begin resourceCostModelRuleDTO : {}", JSON.toJSONString(resourceCostModelRuleDTO));

        // 不改变源对象的同时，构造出返回对象
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);

        // 获取费用规则
        List<CostRuleWeeksDTO> ruleList = costModelRuleDTO.getRules();

        // 先合并每一天的计费规则（一天中相邻时段的，计费单价相同的两段计费规则需要被合并）
        List<SortedCostRuleWeeksDTO<TimePriceCostRulesDTO>> ruleListAfterCombine = new ArrayList<>();

        ruleList.forEach(ruleWeeksDTO -> {

            List<TimePriceCostRulesDTO> ruleAfterCombine = new ArrayList<>();

            List<CostRulesDTO> collect = ruleWeeksDTO.getWeeksRules();

            //先按起始时间，再按结束时间正序排列
            Collections.sort(collect);

            for (int i = 0; i < collect.size(); i++) {
                if (i < collect.size() - 1) {
                    CostRulesDTO costRules = collect.get(i);
                    CostRulesDTO costRulesInfo = collect.get(i + 1);
                    Boolean neededCombine = TariffUtil.isNeededCombinePrice(costRules.getTimePrice(), costRulesInfo.getTimePrice());
                    if (neededCombine) {
                        CostRulesDTO costRulesDTO = new CostRulesDTO();
                        costRulesDTO.setBeginMinute(costRules.getBeginMinute());
                        costRulesDTO.setBeginHour(costRules.getBeginHour());
                        costRulesDTO.setBeginTime(costRules.getBeginTime());
                        costRulesDTO.setEndMinute(costRulesInfo.getEndMinute());
                        costRulesDTO.setEndHour(costRulesInfo.getEndHour());
                        costRulesDTO.setEndTime(costRulesInfo.getEndTime());
                        costRulesDTO.setTimePrice(costRules.getTimePrice());
                        collect.set(i, costRulesDTO);
                        collect.remove(i + 1);
                        i--;
                    }
                }
            }

            //先按起始时间，再按结束时间正序排列
            Collections.sort(collect);

            for (CostRulesDTO costRulesDTO : collect) {
                TimePriceCostRulesDTO timePriceCostRulesDTO = DozerConvert.map(costRulesDTO, TimePriceCostRulesDTO.class);
                ruleAfterCombine.add(timePriceCostRulesDTO);
            }

            ruleWeeksDTO.getWeeks().forEach(week -> {
                SortedCostRuleWeeksDTO<TimePriceCostRulesDTO> temp = new SortedCostRuleWeeksDTO<>();
                temp.setWeeks(Collections.singletonList(week));
                temp.setWeeksRules(ruleAfterCombine);
                ruleListAfterCombine.add(temp);
            });
        });

        // todo 需要对ruleListAfterCombine对象进行排序，按照星期顺序
        Collections.sort(ruleListAfterCombine);

        List<CostRuleWeeksDTO> rules = new ArrayList<>();

        for (int i = 0; i < ruleListAfterCombine.size(); i++) {
            if (i < ruleListAfterCombine.size() - 1) {
                // 第i天的计费对象
                SortedCostRuleWeeksDTO<TimePriceCostRulesDTO> costRuleWeeksDTO = ruleListAfterCombine.get(i);
                // 第i + 1天的计费对象
                SortedCostRuleWeeksDTO<TimePriceCostRulesDTO> secCostRuleWeeksDTO = ruleListAfterCombine.get(i + 1);

                // 第i天的计费对象的时长费计费规则
                List<TimePriceCostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                // 第i + 1天的计费对象的时长费计费规则
                List<TimePriceCostRulesDTO> secWeeksRules = secCostRuleWeeksDTO.getWeeksRules();

                // 第i天的计费对象的适用星期数
                List<Integer> weeks = costRuleWeeksDTO.getWeeks();
                // 第i + 1天的计费对象的适用星期数
                List<Integer> secWeeks = secCostRuleWeeksDTO.getWeeks();

                Boolean neededCombine = weeksRules.hashCode() == secWeeksRules.hashCode();

                if (neededCombine) {
                    // 重新声明变量
                    List<Integer> result = new ArrayList<>();

                    result.addAll(weeks);
                    // 相邻的两天计费规则需要合并（第i天合并掉第i + 1天）
                    result.addAll(secWeeks);

                    // 第i天的计费对象的适用星期数需要重新赋值
                    costRuleWeeksDTO.setWeeks(result);

                    // 集合中第i天的计费对象需要重新赋值
                    ruleListAfterCombine.set(i, costRuleWeeksDTO);

                    // 移除掉被合并的第i + 1天的计费对象
                    ruleListAfterCombine.removeIf(var -> var.equals(secCostRuleWeeksDTO));

                    i--;
                }
            }
        }

        ruleListAfterCombine.forEach(resourceCostModelRule -> {
            CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
            costRuleWeeksDTO.setWeeks(resourceCostModelRule.getWeeks());

            List<TimePriceCostRulesDTO> timePriceCostRulesDTOList = resourceCostModelRule.getWeeksRules();
            List<CostRulesDTO> weeksRules = new ArrayList<>();

            for (TimePriceCostRulesDTO timePriceCostRulesDTO : timePriceCostRulesDTOList) {
                CostRulesDTO costRulesDTO = DozerConvert.map(timePriceCostRulesDTO, CostRulesDTO.class);
                weeksRules.add(costRulesDTO);
            }

            costRuleWeeksDTO.setWeeksRules(weeksRules);

            rules.add(costRuleWeeksDTO);
        });

        costModelRuleDTO.setRules(rules);

        // todo 如果七天的时长费都为0，不展示
        Integer type = 2;
        if (TariffUtil.oneWeekFree(costModelRuleDTO, type)) {
            costModelRuleDTO = new CostModelRuleDTO();
        }

        log.info("==============>>>>>>>>> TariffUtil.mergeTimePrice after costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));

        return costModelRuleDTO;
    }

    /**
     * @param resourceCostModelRuleDTO
     * @return 针对相同费用的停车费时段，进行时段合并后的结果
     * @function 针对相同费用的停车费时段，进行时段合并
     */
    public static CostModelRuleDTO mergeParkPrice(CostModelRuleDTO resourceCostModelRuleDTO) {

        log.info("==============>>>>>>>>> TariffUtil.mergeParkPrice begin resourceCostModelRuleDTO : {}", JSON.toJSONString(resourceCostModelRuleDTO));

        // 不改变源对象的同时，构造出返回对象
        CostModelRuleDTO costModelRuleDTO = DozerConvert.map(resourceCostModelRuleDTO, CostModelRuleDTO.class);

        // 获取费用规则
        List<CostRuleWeeksDTO> ruleList = costModelRuleDTO.getRules();

        // 先合并每一天的计费规则（一天中相邻时段的，计费单价相同的两段计费规则需要被合并）
        List<SortedCostRuleWeeksDTO<ParkingPriceCostRulesDTO>> ruleListAfterCombine = new ArrayList<>();

        ruleList.forEach(ruleWeeksDTO -> {

            List<ParkingPriceCostRulesDTO> ruleAfterCombine = new ArrayList<>();

            List<CostRulesDTO> collect = ruleWeeksDTO.getWeeksRules();

            //先按起始时间，再按结束时间正序排列
            Collections.sort(collect);

            for (int i = 0; i < collect.size(); i++) {
                if (i < collect.size() - 1) {
                    CostRulesDTO costRules = collect.get(i);
                    CostRulesDTO costRulesInfo = collect.get(i + 1);
                    Boolean neededCombine = TariffUtil.isNeededCombinePrice(costRules.getParkingPrice(), costRulesInfo.getParkingPrice());
                    if (neededCombine) {
                        CostRulesDTO costRulesDTO = new CostRulesDTO();
                        costRulesDTO.setBeginMinute(costRules.getBeginMinute());
                        costRulesDTO.setBeginHour(costRules.getBeginHour());
                        costRulesDTO.setBeginTime(costRules.getBeginTime());
                        costRulesDTO.setEndMinute(costRulesInfo.getEndMinute());
                        costRulesDTO.setEndHour(costRulesInfo.getEndHour());
                        costRulesDTO.setEndTime(costRulesInfo.getEndTime());
                        costRulesDTO.setParkingPrice(costRules.getParkingPrice());
                        collect.set(i, costRulesDTO);
                        collect.remove(i + 1);
                        i--;
                    }
                }
            }

            //先按起始时间，再按结束时间正序排列
            Collections.sort(collect);

            for (CostRulesDTO costRulesDTO : collect) {
                ParkingPriceCostRulesDTO parkingPriceCostRulesDTO = DozerConvert.map(costRulesDTO, ParkingPriceCostRulesDTO.class);
                ruleAfterCombine.add(parkingPriceCostRulesDTO);
            }

            ruleWeeksDTO.getWeeks().forEach(week -> {
                SortedCostRuleWeeksDTO<ParkingPriceCostRulesDTO> temp = new SortedCostRuleWeeksDTO<>();
                temp.setWeeks(Collections.singletonList(week));
                temp.setWeeksRules(ruleAfterCombine);
                ruleListAfterCombine.add(temp);
            });
        });

        // todo 需要对ruleListAfterCombine对象进行排序，按照星期顺序
        Collections.sort(ruleListAfterCombine);

        List<CostRuleWeeksDTO> rules = new ArrayList<>();

        for (int i = 0; i < ruleListAfterCombine.size(); i++) {
            if (i < ruleListAfterCombine.size() - 1) {
                // 第i天的计费对象
                SortedCostRuleWeeksDTO<ParkingPriceCostRulesDTO> costRuleWeeksDTO = ruleListAfterCombine.get(i);
                // 第i + 1天的计费对象
                SortedCostRuleWeeksDTO<ParkingPriceCostRulesDTO> secCostRuleWeeksDTO = ruleListAfterCombine.get(i + 1);

                // 第i天的计费对象的停车费计费规则
                List<ParkingPriceCostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                // 第i + 1天的计费对象的停车费计费规则
                List<ParkingPriceCostRulesDTO> secWeeksRules = secCostRuleWeeksDTO.getWeeksRules();

                // 第i天的计费对象的适用星期数
                List<Integer> weeks = costRuleWeeksDTO.getWeeks();
                // 第i + 1天的计费对象的适用星期数
                List<Integer> secWeeks = secCostRuleWeeksDTO.getWeeks();

                Boolean neededCombine = weeksRules.hashCode() == secWeeksRules.hashCode();

                if (neededCombine) {
                    // 重新声明变量
                    List<Integer> result = new ArrayList<>();

                    result.addAll(weeks);
                    // 相邻的两天计费规则需要合并（第i天合并掉第i + 1天）
                    result.addAll(secWeeks);

                    // 第i天的计费对象的适用星期数需要重新赋值
                    costRuleWeeksDTO.setWeeks(result);

                    // 集合中第i天的计费对象需要重新赋值
                    ruleListAfterCombine.set(i, costRuleWeeksDTO);

                    // 移除掉被合并的第i + 1天的计费对象
                    ruleListAfterCombine.removeIf(var -> var.equals(secCostRuleWeeksDTO));

                    i--;
                }
            }
        }

        ruleListAfterCombine.forEach(resourceCostModelRule -> {
            CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
            costRuleWeeksDTO.setWeeks(resourceCostModelRule.getWeeks());

            List<ParkingPriceCostRulesDTO> parkingPriceCostRulesDTOList = resourceCostModelRule.getWeeksRules();
            List<CostRulesDTO> weeksRules = new ArrayList<>();

            for (ParkingPriceCostRulesDTO parkingPriceCostRulesDTO : parkingPriceCostRulesDTOList) {
                CostRulesDTO costRulesDTO = DozerConvert.map(parkingPriceCostRulesDTO, CostRulesDTO.class);
                weeksRules.add(costRulesDTO);
            }

            costRuleWeeksDTO.setWeeksRules(weeksRules);

            rules.add(costRuleWeeksDTO);
        });

        costModelRuleDTO.setRules(rules);

        // todo 如果七天的停车费都为0，不展示
        Integer type = 3;
        if (TariffUtil.oneWeekFree(costModelRuleDTO, type)) {
            costModelRuleDTO = new CostModelRuleDTO();
        }

        log.info("==============>>>>>>>>> TariffUtil.mergeParkPrice after costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));

        return costModelRuleDTO;
    }

    /**
     * @param srcPrice
     * @param desPrice
     * @return 费用项是否需要合并的结果
     * @function 合并费用项单价相同的两个计费
     */
    private static Boolean isNeededCombinePrice(BigDecimal srcPrice, BigDecimal desPrice) {
        if (srcPrice != null && srcPrice.doubleValue() != 0d && desPrice == null) {
            return false;
        }
        if (srcPrice == null && desPrice != null && desPrice.doubleValue() != 0d) {
            return false;
        }
        if (srcPrice != null && desPrice != null && srcPrice.doubleValue() != desPrice.doubleValue()) {
            return false;
        }
        return true;
    }

    /**
     * @param type
     * @param costModelRuleDTO
     * @return 是否需要展示的结果
     * @function 如果七天的费用都为0或者没有配置，就不展示
     */
    private static Boolean oneWeekFree(CostModelRuleDTO costModelRuleDTO, Integer type) {

        log.info("==============>>>>>>>>> TariffUtil.oneWeekFree costModelRuleDTO : {}, type : {}", JSON.toJSONString(costModelRuleDTO), JSON.toJSONString(type));

        // 默认为false
        Boolean flag = false;

        if (CollectionUtils.isNotEmpty(costModelRuleDTO.getRules()) && costModelRuleDTO.getRules().size() == 1) {
            CostRuleWeeksDTO rule = costModelRuleDTO.getRules().get(0);
            //只有一组，包含7天
            if (CollectionUtils.isNotEmpty(rule.getWeeks()) && rule.getWeeks().size() == 7) {
                List<CostRulesDTO> weeksRules = rule.getWeeksRules();
                //只有一个0点到24点
                if (CollectionUtils.isNotEmpty(weeksRules) && weeksRules.size() == 1) {
                    CostRulesDTO costRulesDTO = weeksRules.get(0);
                    switch (type) {
                        // 电量费分支
                        case 1: {
                            BigDecimal unitPrice = costRulesDTO.getUnitPrice();
                            if (unitPrice == null || unitPrice.doubleValue() == 0d) {
                                flag = true;
                            }
                            break;
                        }

                        // 时长费分支
                        case 2: {
                            BigDecimal timePrice = costRulesDTO.getTimePrice();
                            if (timePrice == null || timePrice.doubleValue() == 0d) {
                                flag = true;
                            }
                            break;
                        }

                        // 停车费分支
                        case 3: {
                            BigDecimal parkingPrice = costRulesDTO.getParkingPrice();
                            if (parkingPrice == null || parkingPrice.doubleValue() == 0d) {
                                flag = true;
                            }
                            break;
                        }
                    }
                }
            }
        }

        log.info("==============>>>>>>>>> TariffUtil.oneWeekFree flag : {}", JSON.toJSONString(flag));

        return flag;
    }

    /**
     * 999费用模板
     *
     * @param resourceCostModelRuleDTO
     * @return
     */
    public static CostModelRuleDTO chargingCost999Template(CostModelRuleDTO resourceCostModelRuleDTO) {

        log.info("==============>>>>>>>>> TariffUtil.chargingCost999Template resourceCostModelRuleDTO : {}", JSON.toJSONString(resourceCostModelRuleDTO));

        String currencySign = resourceCostModelRuleDTO.getCurrencySign();
        CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
        List<CostRuleWeeksDTO> rules = new ArrayList<>();
        CostRuleWeeksDTO costRuleWeeksDTO = new CostRuleWeeksDTO();
        costRuleWeeksDTO.setWeeks(Arrays.asList(1, 2, 3, 4, 5, 6, 7));
        List<CostRulesDTO> weeksRules = new ArrayList<>();
        CostRulesDTO costRulesDTO = new CostRulesDTO();
        costRulesDTO.setStartPrice(new BigDecimal("999"));
        costRulesDTO.setUnitPrice(new BigDecimal("999"));
        costRulesDTO.setTimePrice(new BigDecimal("999"));
        costRulesDTO.setParkingPrice(new BigDecimal("999"));
        costRulesDTO.setCostPrice(new BigDecimal("999"));
        costRulesDTO.setEnergyUnit(2);
        costRulesDTO.setParkingUnit(2);
        costRulesDTO.setBeginHour(0);
        costRulesDTO.setBeginMinute(0);
        costRulesDTO.setBeginTime("00:00");
        costRulesDTO.setEndHour(24);
        costRulesDTO.setEndMinute(0);
        costRulesDTO.setEndTime("24:00");
        weeksRules.add(costRulesDTO);
        costRuleWeeksDTO.setWeeksRules(weeksRules);
        rules.add(costRuleWeeksDTO);
        costModelRuleDTO.setCurrencySign(currencySign);
        costModelRuleDTO.setRules(rules);

        log.info("==============>>>>>>>>> TariffUtil.chargingCost999Template costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));

        return costModelRuleDTO;
    }

    /**
     * @return 默认充电时间模板
     * @function 提供默认充电时间模板：
     * （周一到周五：
     * 00:00-8:00
     * 11:00-16:00
     * 22:00-24:00
     * 周六到周日：
     * 00:00-24:00）
     */
    public static String defaultChargingTemplate() {
        List<DefaultChargingTimeVO> defaultChargingTimeVO = new ArrayList<>();

        // 工作日
        DefaultChargingTimeVO workingDayDefaultChargingTimeVO = new DefaultChargingTimeVO();

        // 工作日早上
        ThatDayDefaultChargingTimeVO morningThatDayDefaultChargingTimeVO = new ThatDayDefaultChargingTimeVO();
        morningThatDayDefaultChargingTimeVO.setBeginHour(0);
        morningThatDayDefaultChargingTimeVO.setBeginMinute(0);
        morningThatDayDefaultChargingTimeVO.setBeginTime("00:00");
        morningThatDayDefaultChargingTimeVO.setEndHour(8);
        morningThatDayDefaultChargingTimeVO.setEndMinute(0);
        morningThatDayDefaultChargingTimeVO.setEndTime("08:00");

        // 工作日下午
        ThatDayDefaultChargingTimeVO noonThatDayDefaultChargingTimeVO = new ThatDayDefaultChargingTimeVO();
        noonThatDayDefaultChargingTimeVO.setBeginHour(11);
        noonThatDayDefaultChargingTimeVO.setBeginMinute(0);
        noonThatDayDefaultChargingTimeVO.setBeginTime("11:00");
        noonThatDayDefaultChargingTimeVO.setEndHour(16);
        noonThatDayDefaultChargingTimeVO.setEndMinute(0);
        noonThatDayDefaultChargingTimeVO.setEndTime("16:00");

        // 工作日晚上
        ThatDayDefaultChargingTimeVO nightThatDayDefaultChargingTimeVO = new ThatDayDefaultChargingTimeVO();
        nightThatDayDefaultChargingTimeVO.setBeginHour(22);
        nightThatDayDefaultChargingTimeVO.setBeginMinute(0);
        nightThatDayDefaultChargingTimeVO.setBeginTime("22:00");
        nightThatDayDefaultChargingTimeVO.setEndHour(24);
        nightThatDayDefaultChargingTimeVO.setEndMinute(0);
        nightThatDayDefaultChargingTimeVO.setEndTime("24:00");

        List<ThatDayDefaultChargingTimeVO> workingDayWeeksRules = new ArrayList<>();

        workingDayWeeksRules.add(morningThatDayDefaultChargingTimeVO);
        workingDayWeeksRules.add(noonThatDayDefaultChargingTimeVO);
        workingDayWeeksRules.add(nightThatDayDefaultChargingTimeVO);

        workingDayDefaultChargingTimeVO.setWeeks(Arrays.asList(1, 2, 3, 4, 5));
        workingDayDefaultChargingTimeVO.setWeeksRules(workingDayWeeksRules);

        // 周末
        DefaultChargingTimeVO weekendDefaultChargingTimeVO = new DefaultChargingTimeVO();

        // 周末全天
        ThatDayDefaultChargingTimeVO weekendThatDayDefaultChargingTimeVO = new ThatDayDefaultChargingTimeVO();
        weekendThatDayDefaultChargingTimeVO.setBeginHour(0);
        weekendThatDayDefaultChargingTimeVO.setBeginMinute(0);
        weekendThatDayDefaultChargingTimeVO.setBeginTime("00:00");
        weekendThatDayDefaultChargingTimeVO.setEndHour(24);
        weekendThatDayDefaultChargingTimeVO.setEndMinute(0);
        weekendThatDayDefaultChargingTimeVO.setEndTime("24:00");

        List<ThatDayDefaultChargingTimeVO> weekendWeeksRules = new ArrayList<>();
        weekendWeeksRules.add(weekendThatDayDefaultChargingTimeVO);

        weekendDefaultChargingTimeVO.setWeeks(Arrays.asList(6, 7));
        weekendDefaultChargingTimeVO.setWeeksRules(weekendWeeksRules);

        defaultChargingTimeVO.add(workingDayDefaultChargingTimeVO);
        defaultChargingTimeVO.add(weekendDefaultChargingTimeVO);
        return JSON.toJSONString(defaultChargingTimeVO);
    }

    /**
     * @param zoneId               时区id名称
     * @param defaultChargingTime  默认充电时间
     * @param currentTimeTimestamp 当前时刻时间戳
     * @return 是否需要提醒
     * @funtion 英国法案认证：高峰期充电增加提醒
     */
    public static Boolean isPublicPileStartDuringPeakElectricityConsumption(String zoneId, String defaultChargingTime, Long currentTimeTimestamp) {

        log.info("===>>> TariffUtil.isPublicPileStartDuringPeakElectricityConsumption zoneId : {} and defaultChargingTime : {} and currentTimeTimestamp : {}", JSON.toJSONString(zoneId), JSON.toJSONString(defaultChargingTime), JSON.toJSONString(currentTimeTimestamp));

        Boolean flag = Boolean.FALSE;

        if (defaultChargingTime == null) {
            // 商家没有配置默认充电时间，可以认为所有时刻都是充电高峰期
            flag = Boolean.TRUE;
        }

        List<DefaultChargingTimeVO> defaultChargingTimeVOList = JSON.parseArray(defaultChargingTime, DefaultChargingTimeVO.class);
        if (ObjectUtils.isNotEmpty(defaultChargingTimeVOList)) {
            // 对当前时间戳进行时区转换
            LocalDateTime currentTime = TimeZoneUtil.millsToLocalDateTime(currentTimeTimestamp, zoneId);
            if (currentTime != null) {
                // 当前时刻的小时分钟部分 (以2022-12-28 14:07:23为例 取14:07)
                String hourMinuteFormatTime = DateUtil.format(currentTime, "HH:mm");
                // 当前时刻对应周几 (以2022-12-28 14:07:23(周三)为例 取3)
                Integer week = currentTime.getDayOfWeek().getValue();
                if (StrUtil.isNotBlank(hourMinuteFormatTime)
                        && week != null) {
                    Boolean reg = Boolean.FALSE;
                    Boolean endSign = Boolean.FALSE;
                    for (DefaultChargingTimeVO defaultChargingTimeVO : defaultChargingTimeVOList) {
                        List<Integer> weeks = defaultChargingTimeVO.getWeeks();
                        List<ThatDayDefaultChargingTimeVO> weeksRules = defaultChargingTimeVO.getWeeksRules();
                        if (ObjectUtils.isNotEmpty(weeks)
                                && ObjectUtils.isNotEmpty(weeksRules)
                                && weeks.contains(week)) {
                            for (ThatDayDefaultChargingTimeVO thatDayDefaultChargingTimeVO : weeksRules) {
                                String beginTime = thatDayDefaultChargingTimeVO.getBeginTime();
                                String endTime = thatDayDefaultChargingTimeVO.getEndTime();
                                if (hourMinuteFormatTime.compareTo(beginTime) >= 0
                                        && hourMinuteFormatTime.compareTo(endTime) < 0) {
                                    reg = Boolean.TRUE;
                                    endSign = Boolean.TRUE;
                                    break;
                                }
                            }
                            if (endSign) {
                                break;
                            }
                        }
                    }
                    if (!reg) {
                        flag = Boolean.TRUE;
                    }
                }
            }
        }
        return flag;
    }

    public static Map<String, List<DispatchTariffVO>> buildDispatchMap(List<DispatchTariffDTO> dispatchTariffDTOList) {

        if (ObjectUtils.isEmpty(dispatchTariffDTOList)) {
            return null;
        }

        List<DispatchTariffVO> needDispatchTariffPileSnList = new ArrayList<>();
        List<DispatchTariffVO> needClearTariffPileSnList = new ArrayList<>();
        for (DispatchTariffDTO dispatchTariffDTO : dispatchTariffDTOList) {

            if (dispatchTariffDTO == null
                    || StringUtils.isBlank(dispatchTariffDTO.getPileSn())
                    || dispatchTariffDTO.getTariffId() == null) {
                continue;
            }

            Long tariffId = dispatchTariffDTO.getTariffId();
            String pileSn = dispatchTariffDTO.getPileSn();
            boolean flagIssue = dispatchTariffDTO.getFlagIssue() == null ? false : dispatchTariffDTO.getFlagIssue();
            Integer newVersionCostModelRule = dispatchTariffDTO.getNewVersionCostModelRule();
            Integer type = dispatchTariffDTO.getType();
            boolean supportIssuedIdleFee = dispatchTariffDTO.getSupportIssuedIdleFee() == null ? false : dispatchTariffDTO.getSupportIssuedIdleFee();

            DispatchTariffVO dispatchTariffVO = new DispatchTariffVO();
            dispatchTariffVO.setPileSn(pileSn);
            dispatchTariffVO.setTariffId(tariffId);

            if (PileTypeEnum.FIVE_INCH_PILE.getCode().equals(type)) {
                if (newVersionCostModelRule == null
                        || Integer.valueOf(0).equals(newVersionCostModelRule)) {
                    needDispatchTariffPileSnList.add(dispatchTariffVO);
                } else if (flagIssue && supportIssuedIdleFee) {
                    needDispatchTariffPileSnList.add(dispatchTariffVO);
                } else {
                    needClearTariffPileSnList.add(dispatchTariffVO);
                }
            } else if (PileTypeEnum.EIGHT_INCH_PILE.getCode().equals(type)
                    || PileTypeEnum.FIRMWARE_VERSION_DOES_NOT_SUPPORT.getCode().equals(type)) {
                if (newVersionCostModelRule == null
                        || Integer.valueOf(0).equals(newVersionCostModelRule)) {
                    needDispatchTariffPileSnList.add(dispatchTariffVO);
                }
            } else {
                continue;
            }
        }

        Map<String, List<DispatchTariffVO>> operateAndDispatchTariffVOListMap = new HashMap<>();

        if (ObjectUtils.isNotEmpty(needDispatchTariffPileSnList)) {
            operateAndDispatchTariffVOListMap.put(DispatchConstant.ISSUED, needDispatchTariffPileSnList);
        }

        if (ObjectUtils.isNotEmpty(needClearTariffPileSnList)) {
            operateAndDispatchTariffVOListMap.put(DispatchConstant.CLEAR, needClearTariffPileSnList);
        }

        return operateAndDispatchTariffVOListMap;
    }
}
