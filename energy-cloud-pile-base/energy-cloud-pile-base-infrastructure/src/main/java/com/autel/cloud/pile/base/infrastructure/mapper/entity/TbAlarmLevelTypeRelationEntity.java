package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("tb_alarm_level_type_relation")
@ApiModel(value = "tb_alarm_level_type_relation表实体对象", description = "告警等级类型关联关系")
public class TbAlarmLevelTypeRelationEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @TableField("create_time")
    private Date createTime;

    @ApiModelProperty(value = "创建人ID")
    @TableField("create_id")
    private Long createId;

    @ApiModelProperty(value = "修改人ID")
    @TableField("update_id")
    private Long updateId;

    @ApiModelProperty(value = "更新时间")
    @TableField("update_time")
    private Long updateTime;

    @ApiModelProperty(value = "删除标识(1:已删除;0:未删除)")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "运营商id")
    @TableField("operator_id")
    private Long operatorId;

    @ApiModelProperty(value = "告警级别(1:正常停机,2:提示信息,3:提醒注意,4:告警,5:故障停机,6:设备严重故障)")
    @TableField("alarm_level")
    private Integer alarmLevel;

    @ApiModelProperty(value = "告警类型(ConnectorLockFailure;EVCommunicationError;GroundFailure;HighTemperature;InternalError;LocalListConflict;NoError;OtherError;OverCurrentFailure;OverVoltage;PowerMeterFailure;PowerSwitchFailure;ReaderFailure;ResetFailure;UnderVoltage;WeakSignal)")
    @TableField("alarm_type")
    private String alarmType;
}
