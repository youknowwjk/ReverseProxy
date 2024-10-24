package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 成本电价表详情
 * @TableName op_location_price_detail
 */
@ApiModel(description = "成本电价表详情")
@TableName(value ="op_location_price_detail")
@Data
public class OpLocationPriceDetailEntity implements Serializable {
    /**
     * 主键ID
     */
    @ApiModelProperty("主键ID")
    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 成本电价表ID（op_location_price）
     */
    @ApiModelProperty("成本电价表ID（op_location_price）")
    private Long priceId;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @TableField(fill = FieldFill.INSERT)
    private Long createdAt;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updatedAt;

    /**
     * 货币ID（tb_currency）
     */
    @ApiModelProperty("货币ID（tb_currency）")
    private Long currencyId;

    /**
     * 电价
     */
    @ApiModelProperty("电价")
    private BigDecimal price;

    /**
     * 开始时间
     */
    @ApiModelProperty("开始时间")
    private String startTime;

    /**
     * 结束时间
     */
    @ApiModelProperty("结束时间")
    private String endTime;

    /**
     * 星期几（1-7），多天逗号分隔
     */
    @ApiModelProperty("星期几（1-7），多天逗号分隔")
    private String days;

    /**
     * 0：未删除，1：删除
     */
    @ApiModelProperty("0：未删除，1：删除")
    private Integer deleted;

    /**
     * 版本号
     */
    @ApiModelProperty("版本号")
    @Version
    private Long version;

    @ApiModelProperty("")
    @TableField(exist = false)
    private static final long serialVersionUID = 1L;
}