package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 成本电价表
 * @TableName op_location_price
 */
@ApiModel(description = "成本电价表")
@TableName(value ="op_location_price")
@Data
public class OpLocationPriceEntity implements Serializable {
    /**
     * 主键ID
     */
    @ApiModelProperty("主键ID")
    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

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
     * 0：未删除，1：删除
     */
    @ApiModelProperty("0：未删除，1：删除")
    private Integer deleted;

    /**
     * 场站ID
     */
    @ApiModelProperty("场站ID")
    private Long locationId;

    /**
     * 商家ID
     */
    @ApiModelProperty("商家ID")
    private Long sellerId;

    /**
     * 电价名称
     */
    @ApiModelProperty("电价名称")
    private String name;

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