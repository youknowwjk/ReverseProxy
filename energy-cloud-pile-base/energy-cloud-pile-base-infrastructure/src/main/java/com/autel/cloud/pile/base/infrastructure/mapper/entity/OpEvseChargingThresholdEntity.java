package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 充电设备充电阀值预警设置
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_evse_charging_threshold")
@ApiModel(value = "OpEvseChargingThresholdEntity对象", description = "充电设备充电阀值预警设置")
public class OpEvseChargingThresholdEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.AUTO)
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

    @ApiModelProperty(value = "桩id")
    @TableField("evse_id")
    private Long evseId;

    @ApiModelProperty(value = "汽车电池电流")
    @TableField("car_battery_current")
    private Double carBatteryCurrent;

    @ApiModelProperty(value = "汽车电池电压")
    @TableField("car_battery_voltage")
    private Double carBatteryVoltage;

    @ApiModelProperty(value = "汽车电池温度")
    @TableField("car_battery_temperature")
    private Double carBatteryTemperature;

    @ApiModelProperty(value = "充电设备电流")
    @TableField("evse_current")
    private Double evseCurrent;

    @ApiModelProperty(value = "充电设备电压")
    @TableField("evse_voltage")
    private Double evseVoltage;

    @ApiModelProperty(value = "充电设备温度")
    @TableField("evse_temperature")
    private Double evseTemperature;

    @ApiModelProperty(value = "连接器电流")
    @TableField("connector_current")
    private Double connectorCurrent;

    @ApiModelProperty(value = "连接器电压")
    @TableField("connector_voltage")
    private Double connectorVoltage;

    @ApiModelProperty(value = "连接器温度")
    @TableField("connector_temperature")
    private Double connectorTemperature;

    @ApiModelProperty(value = "是否达到电流自动关机")
    @TableField("auto_down_current")
    private Integer autoDownCurrent;

    @ApiModelProperty(value = "是否达到电压自动关机")
    @TableField("auto_down_voltage")
    private Integer autoDownVoltage;

    @ApiModelProperty(value = "是否达到温度自动关机")
    @TableField("auto_down_temperature")
    private Integer autoDownTemperature;


}
