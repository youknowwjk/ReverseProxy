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
 * 充电设备
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_evse")
@ApiModel(value = "OpLocationEvseEntity对象", description = "充电设备")
public class OpLocationEvseEntity implements Serializable {

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

    @ApiModelProperty(value = "场站id")
    @TableField("location_id")
    private Long locationId;

    @ApiModelProperty(value = "枪状态")
    @TableField("state")
    private String state;

    @ApiModelProperty(value = "充电设备序列号")
    @TableField("evse_id")
    private String evseId;

    @ApiModelProperty(value = "楼层")
    @TableField("floor_level")
    private Integer floorLevel;

    @ApiModelProperty(value = "纬度")
    @TableField("latitude")
    private String latitude;

    @ApiModelProperty(value = "经度")
    @TableField("longitude")
    private String longitude;

    @ApiModelProperty(value = "EVSE外部展示字符")
    @TableField("physical_reference")
    private String physicalReference;

    @ApiModelProperty(value = "停车限制")
    @TableField("parking_restrictions")
    private String parkingRestrictions;

    @ApiModelProperty(value = "pin码")
    @TableField("pin_code")
    private String pinCode;

    /**
     * 充电枪(充电设备)绑定的计费规则组id
     */
    @ApiModelProperty(value = "充电枪(充电设备)绑定的计费规则组id")
    @TableField("tariff_id")
    private Long tariffId;

    @ApiModelProperty(value = "设备SN码")
    @TableField("evse_sn")
    private String evseSn;

    @ApiModelProperty(value = "删除时间")
    private Long deletedTime;
}
