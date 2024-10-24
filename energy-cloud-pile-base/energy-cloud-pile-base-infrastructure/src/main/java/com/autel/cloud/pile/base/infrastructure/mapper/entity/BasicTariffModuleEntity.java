package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author A22219
 */
@Data
@TableName(value = "tb_charge_card")
@ApiModel(value = "基础版计费规则")
public class BasicTariffModuleEntity implements Serializable {

    @TableId(type = IdType.AUTO)
    @ApiModelProperty(value = "主键id")
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @TableField(value = "created_at", fill = FieldFill.INSERT)
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField(value = "updated_at", fill = FieldFill.INSERT_UPDATE)
    private Long updatedAt;

    @ApiModelProperty(value = "是否删除")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "货币符合")
    @TableField(value = "currency_sign", fill = FieldFill.INSERT)
    private String currencySign;

    @ApiModelProperty(value = "充电费")
    @TableField(value = "unit_price")
    private BigDecimal unitPrice;

    @ApiModelProperty(value = "滞留费")
    @TableField(value = "idle_price")
    private BigDecimal idlePrice;

    @ApiModelProperty(value = "是否不收滞留费")
    @TableField(value = "currency_sign")
    private Boolean withOutIdlePrice;

    @ApiModelProperty(value = "不收滞留费开始时间")
    @TableField(value = "without_idle_start_time")
    private String withOutIdleStartTime;

    @ApiModelProperty(value = "不收滞留费结束时间")
    @TableField(value = "without_idle_end_time")
    private String withOutIdleEndTime;

    @ApiModelProperty(value = "最低费用")
    @TableField(value = "lowest_price")
    private BigDecimal lowestPrice;
}