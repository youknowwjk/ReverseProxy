package com.autel.cloud.pile.base.domain.model;

import cn.hutool.core.util.StrUtil;
import com.autel.cloud.tariff.rule.dto.BasicPriceDTO;
import com.autel.cloud.tariff.utils.time.TimeFormatUtils;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.apache.commons.lang3.math.NumberUtils;

import java.io.Serializable;
import java.math.BigDecimal;


/**
 * @Author MingLong A22599
 * @Date 2022/11/19
 * @Function 基本费用模板
 */
@Data
@ApiModel
public class BaseCostRulesDTO implements Serializable, Comparable<BaseCostRulesDTO> {

    @ApiModelProperty(value = "Id")
    private String id;

    @ApiModelProperty(value = "开始时间-小时部分")
    private Integer beginHour = 0;

    @ApiModelProperty(value = "开始时间-分钟部分")
    private Integer beginMinute = 0;

    @ApiModelProperty(value = "结束时间-小时部分")
    private Integer endHour = 0;

    @ApiModelProperty(value = "结束时间-分钟部分")
    private Integer endMinute = 0;

    @ApiModelProperty(value = "开始时间(HH:mm)")
    private String beginTime;

    @ApiModelProperty(value = "结束时间(HH:mm)")
    private String endTime;

    @ApiModelProperty(value = "下限")
    private BigDecimal lower;

    @ApiModelProperty(value = "上限")
    private BigDecimal upper;

    public String getBeginTime() {
        if (StrUtil.isBlank(this.beginTime)) {
            return TimeFormatUtils.formatHour(getBeginHour()) + ":" + TimeFormatUtils.formatMinute(getBeginMinute());
        }
        String[] beginTimeArr = this.beginTime.split(":");
        int hour = this.beginHour;
        if (beginTimeArr.length > 0 && NumberUtils.isDigits(beginTimeArr[0])) {
            hour = Integer.parseInt(beginTimeArr[0]);
        }
        int minuter = 0;
        if (beginTimeArr.length > 1 && NumberUtils.isDigits(beginTimeArr[1])) {
            minuter = Integer.parseInt(beginTimeArr[1]);
        }
        setBeginHour(TimeFormatUtils.transferHour(hour));
        setBeginMinute(TimeFormatUtils.transferMinute(minuter));
        return this.beginTime;
    }

    public void setBeginTime(String beginTime) {
        this.beginTime = beginTime;
    }

    public String getEndTime() {
        if (StrUtil.isBlank(this.endTime)) {
            return TimeFormatUtils.formatHour(getEndHour()) + ":" + TimeFormatUtils.formatMinute(getEndMinute());
        }
        String[] endTimeArr = this.endTime.split(":");
        int hour = this.endHour;
        if (endTimeArr.length > 0 && NumberUtils.isDigits(endTimeArr[0])) {
            hour = Integer.parseInt(endTimeArr[0]);
        }
        int minuter = 0;
        if (endTimeArr.length > 1 && NumberUtils.isDigits(endTimeArr[1])) {
            minuter = Integer.parseInt(endTimeArr[1]);
        }
        setEndHour(TimeFormatUtils.transferHour(hour));
        setEndMinute(TimeFormatUtils.transferMinute(minuter));
        return this.endTime;
    }

    @Override
    public int compareTo(BaseCostRulesDTO o) {
        if (null == this.getBeginTime()) {
            return -1;
        }
        if (null == o.getBeginTime()) {
            return 1;
        }
        int compareFlag = this.getBeginTime().compareTo(o.getBeginTime());
        if (compareFlag != 0) {
            return compareFlag;
        }
        if (null == this.getEndTime()) {
            return 1;
        }
        if (null == o.getEndTime()) {
            return -1;
        }
        compareFlag = this.getEndTime().compareTo(o.getEndTime());
        return compareFlag;
    }
}
