package com.autel.cloud.pile.base.infrastructure.mapper.entity;

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
 * 充电设备组合功率限制开启表
 * </p>
 *
 * @author A22121
 * @since 2022-07-02
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("op_power_limit")
@ApiModel(value = "OpPowerLimitEntity对象", description = "充电设备组合功率限制开启表")
public class OpPowerLimitEntity implements Serializable {

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

    @ApiModelProperty(value = "充电组合id")
    @TableField("combination_id")
    private Long combinationId;

    @ApiModelProperty(value = "是否开启，默认不开启")
    @TableField("turn_on")
    private Integer turnOn;

    @ApiModelProperty(value = "是否自定义设置，默认不开启")
    @TableField("advanced_setting")
    private Integer advancedSetting;

    @ApiModelProperty(value = "是否长期有效，默认长期有效")
    @TableField("long_term_effective")
    private Integer longTermEffective;

    @ApiModelProperty(value = "自定义有效期开始日期")
    @TableField("effective_start_date")
    private Long effectiveStartDate;

    @ApiModelProperty(value = "自定义有效期结束日期")
    @TableField("effective_end_date")
    private Long effectiveEndDate;


}
