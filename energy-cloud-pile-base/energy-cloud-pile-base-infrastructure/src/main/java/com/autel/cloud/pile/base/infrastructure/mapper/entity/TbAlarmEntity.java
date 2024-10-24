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
@TableName("tb_alarm")
@ApiModel(value = "tb_alarm表实体对象", description = "告警信息")
public class TbAlarmEntity implements Serializable {

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

    @ApiModelProperty(value = "处理状态(1:待处理;2:处理中;3:处理完成)")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "处理人ID")
    @TableField("handler_id")
    private Long handlerId;

    @ApiModelProperty(value = "处理人名称")
    @TableField("handler_name")
    private String handlerName;

    @ApiModelProperty(value = "处理时间")
    @TableField("handler_time")
    private Date handlerTime;

    @ApiModelProperty(value = "运营商id")
    @TableField("operator_id")
    private Long operatorId;

    @ApiModelProperty(value = "场站ID")
    @TableField("location_id")
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    @TableField("location_name")
    private String locationName;

    @ApiModelProperty(value = "站点时间")
    @TableField("location_time")
    private Date locationTime;

    @ApiModelProperty(value = "充电桩ID")
    @TableField("evse_id")
    private Long evseId;

    @ApiModelProperty(value = "充电设备SN码")
    @TableField("evse_sn")
    private String evseSn;

    @ApiModelProperty(value = "站点时区")
    @TableField("time_zone")
    private String timeZone;

    @ApiModelProperty(value = "告警级别(1:正常停机,2:提示信息,3:提醒注意,4:告警,5:故障停机,6:设备严重故障)")
    @TableField("alarm_level")
    private Integer alarmLevel;

    @ApiModelProperty(value = "告警类型(ConnectorLockFailure;EVCommunicationError;GroundFailure;HighTemperature;InternalError;LocalListConflict;NoError;OtherError;OverCurrentFailure;OverVoltage;PowerMeterFailure;PowerSwitchFailure;ReaderFailure;ResetFailure;UnderVoltage;WeakSignal)")
    @TableField("alarm_type")
    private String alarmType;
}
