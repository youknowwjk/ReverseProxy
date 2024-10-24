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
 * SOC阀值设置
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_evse_soc_threshold")
@ApiModel(value = "OpEvseSocThresholdEntity对象", description = "SOC阀值设置")
public class OpEvseSocThresholdEntity implements Serializable {

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

    @ApiModelProperty(value = "商家ID")
    @TableField("seller_id")
    private Long sellerId;

    @ApiModelProperty(value = "桩ID")
    @TableField("evse_id")
    private Long evseId;

    @ApiModelProperty(value = "开始时段")
    @TableField("soc_start_time")
    private String socStartTime;

    @ApiModelProperty(value = "结束时段")
    @TableField("soc_finish_time")
    private String socFinishTime;

    @ApiModelProperty(value = "时间段SOC值")
    @TableField("time_interval_soc")
    private Integer timeIntervalSoc;

    @ApiModelProperty(value = "车辆电池组类型")
    @TableField("vehicle_battery")
    private Integer vehicleBattery;
}
