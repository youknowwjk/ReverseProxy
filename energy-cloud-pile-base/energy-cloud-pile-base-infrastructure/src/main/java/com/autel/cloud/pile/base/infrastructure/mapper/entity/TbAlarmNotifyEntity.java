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
@TableName("tb_alarm_notify")
@ApiModel(value = "tb_alarm_notify实体对象", description = "告警通知")
public class TbAlarmNotifyEntity implements Serializable {

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

    @ApiModelProperty(value = "推送方式(1:邮件;2:短信)")
    @TableField("push_type")
    private Integer pushType;

    @ApiModelProperty(value = "推送开关(0:不推送;1:推送)")
    @TableField("push_switch")
    private Integer pushSwitch;

    @ApiModelProperty(value = "联系人ID")
    @TableField("contact_id")
    private Long contactId;

    @ApiModelProperty(value = "联系人名称")
    @TableField("contact_name")
    private String contactName;

    @ApiModelProperty(value = "联系方式")
    @TableField("contact_info")
    private String contactInfo;

    @ApiModelProperty(value = "运营商id")
    @TableField("operator_id")
    private Long operatorId;

    @ApiModelProperty(value = "告警级别(1:正常停机,2:提示信息,3:提醒注意,4:告警,5:故障停机,6:设备严重故障)")
    @TableField("alarm_level")
    private Integer alarmLevel;
}
