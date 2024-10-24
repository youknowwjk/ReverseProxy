package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import java.math.BigDecimal;

import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableField;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * <p>
 * 功率限制分时上限设置
 * </p>
 *
 * @author A22121
 * @since 2022-07-02
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("op_power_limit_setting")
@ApiModel(value = "OpPowerLimitSettingEntity对象", description = "功率限制分时设置")
public class OpPowerLimitSettingEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @TableField("created_at")
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField("updated_at")
    private Long updatedAt;

    @ApiModelProperty(value = "是否删除")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "状态")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "开始时间")
    @TableField("start_time")
    private String startTime;

    @ApiModelProperty(value = "结束时间")
    @TableField("end_time")
    private String endTime;

    @ApiModelProperty(value = "充电设置")
    @TableField("power")
    private BigDecimal power;

    @ApiModelProperty(value = "充电设置单位（功率、电流）")
    @TableField("power_unit")
    private String powerUnit;

    @ApiModelProperty(value = "时区")
    @TableField("time_zone")
    private String timeZone;

    @ApiModelProperty(value = "星期几[1, 2]")
    @TableField("week_day")
    private String weekDay;

    @ApiModelProperty(value = "充电组合id")
    @TableField("combination_id")
    private Long combinationId;


}
