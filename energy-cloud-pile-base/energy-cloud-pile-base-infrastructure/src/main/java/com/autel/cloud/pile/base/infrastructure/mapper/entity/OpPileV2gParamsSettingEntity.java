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
 * 桩V2G参数设置
 * </p>
 *
 * @author A22121
 * @since 2022-10-17
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("op_pile_v2g_params_setting")
@ApiModel(value = "OpPileV2gParamsSettingEntity对象", description = "桩V2G参数设置")
public class OpPileV2gParamsSettingEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.AUTO)
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

    @ApiModelProperty(value = "桩SN")
    @TableField("pile_sn")
    private String pileSn;

    @ApiModelProperty(value = "状态")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "是否放电：0 充电、1 放电")
    @TableField("charge_flag")
    private Integer chargeFlag;

    @ApiModelProperty(value = "最大充电soc")
    @TableField("import_max_soc")
    private Integer importMaxSoc;

    @ApiModelProperty(value = "最小充电soc")
    @TableField("import_min_soc")
    private Integer importMinSoc;

    @ApiModelProperty(value = "最大放电soc")
    @TableField("export_max_soc")
    private Integer exportMaxSoc;

    @ApiModelProperty(value = "最小放电soc")
    @TableField("export_min_soc")
    private Integer exportMinSoc;

    @ApiModelProperty(value = "设置放电功率限制 W")
    @TableField("export_limit_power")
    private BigDecimal exportLimitPower;

    @ApiModelProperty(value = "设置充电功率限制 W")
    @TableField("import_limit_power")
    private BigDecimal importLimitPower;

    @ApiModelProperty(value = "设置放电截至电压 V")
    @TableField("stop_voltage")
    private BigDecimal stopVoltage;

    @ApiModelProperty(value = "设置按键模式： 0 按键功能禁止、1 运行按键启停、2 运行启停与切换")
    @TableField("button_mode")
    private Integer buttonMode;


}
