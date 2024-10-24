package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.autel.cloud.pile.base.dto.ElectricityPrice;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.FastjsonTypeHandler;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * <p>
 * 充电桩群组
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName(value = "op_location_pile_group", autoResultMap = true)
@ApiModel(value = "OpLocationPileGroupEntity对象", description = "充电桩群组")
public class OpLocationPileGroupEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "从0开始  所在层级")
    @TableField(exist = false)
    private Integer level;

    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "父群组ID")
    private Long pid;


    @ApiModelProperty(value = "商家ID")
    private Long merchantId;

    @ApiModelProperty(value = "创建时间")
    @TableField("created_at")
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField("updated_at")
    private Long updatedAt;

    @ApiModelProperty(value = "是否删除")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "桩组状态")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "备注")
    @TableField("note")
    private String note;

    @ApiModelProperty(value = "桩组名称")
    @TableField("name")
    private String name;

    @ApiModelProperty(value = "是否启用智能充电")
    @TableField("use_smart_charging")
    @Deprecated
    private Integer useSmartCharging;

    @ApiModelProperty(value = "充电上限")
    @TableField("charging_up")
    private BigDecimal chargingUp;

    @ApiModelProperty(value = "EMS离线时电网负载上限")
    @TableField("charging_up_ems")
    private BigDecimal chargingUpEms;


    @ApiModelProperty(value = "电表位置，0：不包含充电桩，1：包含充电桩")
    private Integer meterLocation;

    @ApiModelProperty(value = "充电上限类型（功率、电流）")
    @TableField("charging_up_type")
    private String chargingUpType;


    @ApiModelProperty(value = "充电上限单位（KW）")
    @TableField("charging_up_unit")
    private String chargingUpUnit;

    /**
     * 废弃的字段
     */
    @ApiModelProperty(value = "智能充电模式（固定功率0/分时功率1）")
    @TableField("smart_charging_mode")
    @Deprecated
    private Integer smartChargingMode = 0;

    @ApiModelProperty(value = "分时设置模式（一星期 统一0/分开1）")
    @TableField("time_setting_mode")
    private Integer timeSettingMode;

    @ApiModelProperty(value = "分时设置详情")
    private String timeSettingDetail;

    @ApiModelProperty(value = "站点id")
    @TableField("location_id")
    private Long locationId;

    @ApiModelProperty(value = "相数（单相/三相）")
    @TableField("phase_num")
    private String phaseNum;

    @ApiModelProperty(value = "分配策略（平均分配）")
    @TableField("allocation_strategy")
    private String allocationStrategy;

    @ApiModelProperty(value = "负载管理类型，0：DLB，1：ALM,2：EMS")
    private Integer loadType;

    @ApiModelProperty(value = "用能策略，0：功率优先，1：成本优先")
    private Integer energyUseStrategy;

    @ApiModelProperty(value = "计费规则ID")
    @Deprecated
    private Long tariffId;

    @ApiModelProperty(value = "用能习惯，0：按充电时长，1：按充电结束时间")
    private Integer favor;

    @ApiModelProperty(value = "计划充电时长")
    private String planTime;

    @ApiModelProperty(value = "VIP优先，0：关闭，1：开启")
    private Integer priority;

    @ApiModelProperty(value = "电表ID")
    private Long meterId;

    @ApiModelProperty(value = "是否启用安全用电")
    @TableField("security_enabled")
    private Boolean securityEnabled;

    @ApiModelProperty(value = "是否预留安全限制")
    @Deprecated
    private Boolean offlineValue;

    @ApiModelProperty(value = "最低充电设置，0：不预留；1：每台枪预留；2：单枪预留")
    private Integer minReserve = 2;


    @ApiModelProperty(value = "电价")
    @TableField(typeHandler = FastjsonTypeHandler.class)
    private List<ElectricityPrice> prices;

    @ApiModelProperty(value = "缓升时长 单位min")
    private Integer upDuration;

    @ApiModelProperty(value = "过载熔断  0：关闭，1：开启   ")
    private Integer overloadFuse;

    @ApiModelProperty(value = "用能场景 0:场站经营;   1:企业场景;   2:车队运营; ")
    @TableField("scene")
    private int scene;

    @ApiModelProperty(value = "是否接入发电设备")
    @TableField("power_equipment_enabled")
    private Boolean powerEquipmentEnabled;

    @ApiModelProperty(value = "市电供电上限")
    @TableField("electric_up")
    private BigDecimal electricUp;

    @ApiModelProperty(value = "发电机供电上限")
    @TableField("power_equipment_up")
    private BigDecimal powerEquipmentUp;

    @ApiModelProperty(value = "发电机设备启动时间（单位：秒）")
    @TableField("power_equipment_start_time")
    private Long powerEquipmentStartTime;

    @ApiModelProperty(value = "电表电压")
    private BigDecimal meterVoltage;

    @ApiModelProperty(value = "群组类型：0，普通群组 1，需求控制费群组,2：边缘云 3：EMS")
    private Integer groupType;

}
