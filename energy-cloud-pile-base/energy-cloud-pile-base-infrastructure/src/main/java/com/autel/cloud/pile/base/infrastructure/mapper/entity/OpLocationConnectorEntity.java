package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 充电设备连接器
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_connector")
@ApiModel(value = "OpLocationConnectorEntity对象", description = "充电设备连接器")
public class OpLocationConnectorEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
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

    @ApiModelProperty(value = "状态")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "充电设备id")
    @TableField("location_evse_id")
    private Long locationEvseId;

    @ApiModelProperty(value = "标准（CHADEMO，家用）")
    @TableField("standard")
    private String standard;

    @ApiModelProperty(value = "设备格式（1：插座，2：电缆）")
    @TableField("format")
    private String format;

    @ApiModelProperty(value = "1: AC_1_PHASE, 2: AC_3_PHASE, 3: DC")
    @TableField("power_type")
    private String powerType;

    @ApiModelProperty(value = "电压(V)")
    @TableField("voltage")
    private BigDecimal voltage;

    @ApiModelProperty(value = "电流（A）")
    @TableField("amperage")
    private BigDecimal amperage;

    @ApiModelProperty(value = "当前收费费率的标识符")
    @TableField("tariff_id")
    private String tariffId;

    @ApiModelProperty(value = "运营商条款URL")
    @TableField("terms_and_conditions")
    private String termsAndConditions;

    @ApiModelProperty(value = "序列号")
    @TableField("connector_id")
    private String connectorId;

    @ApiModelProperty(value = "图片id")
    @TableField("image_id")
    private Long imageId;

    @ApiModelProperty(value = "枪类型1.CCS Combo 2、2.CHAdeMO、3.Type2、4.Type3、5.Type1、6.Wall（Euro）、7.CCS Combo 1、8.GB/T DC、9.GB/T AC")
    @TableField("gun_type")
    private Integer gunType;

    @ApiModelProperty(value = "功率(KW)")
    @TableField("power")
    private double power;


}
