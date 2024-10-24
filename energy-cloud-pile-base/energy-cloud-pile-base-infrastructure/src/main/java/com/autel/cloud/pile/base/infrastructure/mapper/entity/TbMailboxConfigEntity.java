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
@TableName("tb_mailbox_config")
@ApiModel(value = "tb_mailbox_config表实体对象", description = "邮箱配置")
public class TbMailboxConfigEntity implements Serializable {

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

    @ApiModelProperty(value = "邮件协议(SMTP;POP3;IMAP;MIME)")
    @TableField("protocol")
    private String protocol;

    @ApiModelProperty(value = "服务器地址")
    @TableField("host")
    private String host;

    @ApiModelProperty(value = "服务器端口")
    @TableField("port")
    private String port;

    @ApiModelProperty(value = "邮箱")
    @TableField("email")
    private String email;

    @ApiModelProperty(value = "授权码")
    @TableField("auth_code")
    private String authCode;

    @ApiModelProperty(value = "启用开关(0:关闭;1:启用)")
    @TableField("active_switch")
    private Integer activeSwitch;
}
