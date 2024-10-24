package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.autel.cloud.pile.base.SmartChargeProfile;
import com.autel.cloud.pile.base.VehicleProfile;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.FastjsonTypeHandler;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "member_smart_charge_config", autoResultMap = true)
public class MemberSmartChargeConfigEntity {

    @TableId(value = "id", type = IdType.ASSIGN_ID)
    @ApiModelProperty(value = "Id")
    private Long id;

    @ApiModelProperty(value = "客户ID")
    private Long userId;

//
//    @ApiModelProperty(value = "车辆Id")
//    private Long vehicleId;

    @ApiModelProperty(value = "场站Id")
    private Long locationId;

    @ApiModelProperty(value = "是否在该场站自动开启智能充电")
    private int status = 1;

    @ApiModelProperty(value = "用户智能充电配置  信息当前数据行的所有信息(JSON 格式)")
    @TableField(typeHandler = FastjsonTypeHandler.class)
    private SmartChargeProfile profile;

    @ApiModelProperty(value = "创建时间")
    private Long createTime;

    @ApiModelProperty(value = "更新时间")
    private Long updateTime;
}
