package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 电表
 * @TableName op_location_meter
 */
@ApiModel(description = "电表")
@TableName(value ="op_location_meter")
@Data
public class LocationMeterEntity implements Serializable {
    /**
     * 
     */
    @ApiModelProperty("")
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
     * 是否删除
     */
    @ApiModelProperty("是否删除")
    private Integer deleted;

    /**
     * 电表名称
     */
    @ApiModelProperty("电表名称")
    private String name;

    /**
     * 版本号
     */
    @ApiModelProperty("版本号")
    @Version
    private Long version;

    @ApiModelProperty("场站ID")
    @Deprecated
    private Long locationId;

    @ApiModelProperty(value = "运营商id")
    @TableField("operator_id")
    private Long operatorId;

    /**
     * @see com.autel.cloud.pile.base.enums.meter.MeterbrandEnum
     */
    @ApiModelProperty(value = "品牌类型枚举值")
    @TableField("brand_enum")
    private Integer brandEnum;

    @ApiModelProperty(value = "SN码")
    @TableField("sn")
    private String sn;

    @ApiModelProperty(value = "PIN码")
    @TableField("pin")
    private String pin;

    @ApiModelProperty("")
    @TableField(exist = false)
    private static final long serialVersionUID = 1L;


    @ApiModelProperty("型号")
    private String category;

    @ApiModelProperty("通信方式:RS485、IP")
    private String callMode;

    @ApiModelProperty("通信地址")
    private String callAddress;

    @ApiModelProperty("电网模式")
    private String elecMode;

    @ApiModelProperty("CT量程")
    private String ctRange;

    @ApiModelProperty("变比")
    private String ctRatio;
    @ApiModelProperty("通信端口")
    private String callPort;

}