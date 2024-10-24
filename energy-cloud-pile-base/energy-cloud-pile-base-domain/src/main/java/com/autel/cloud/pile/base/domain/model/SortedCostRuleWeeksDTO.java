package com.autel.cloud.pile.base.domain.model;

import cn.hutool.core.collection.CollUtil;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * @Author MingLong A22599
 * @Date 2022/11/23
 * @Function 有序的停车费计费模型对象
 */
@Data
@ApiModel
public class SortedCostRuleWeeksDTO<T> implements Serializable, Comparable<SortedCostRuleWeeksDTO<T>> {

    @ApiModelProperty(value = "ID")
    private String id;

    @ApiModelProperty(value = "适用星期数")
    private List<Integer> weeks;

    @ApiModelProperty(value = "费用模式(1:电量;2:时长)")
    private Integer feeModel = 1;

    @ApiModelProperty(value = "电价计费规则")
    private List<T> weeksRules;

    private Integer currentWeekDays;

    @Override
    public int compareTo(SortedCostRuleWeeksDTO<T> o) {
        if (CollUtil.isEmpty(this.weeks)) {
            return -1;
        }
        if (CollUtil.isEmpty(o.weeks)) {
            return 1;
        }
        return this.weeks.get(0) - o.weeks.get(0);
    }
}
