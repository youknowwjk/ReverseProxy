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
import java.time.LocalDateTime;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/17 15:07
 */

@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("tb_user_op_log")
@ApiModel(value = "tb_user_op_log表实体对象", description = "用户操作日志表")
public class TbUserOpLogEntity implements Serializable {

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "用户")
    @TableField("user_id")
    private Long userId;

    @ApiModelProperty(value = "商家")
    @TableField("seller_id")
    private Long sellerId;

    @ApiModelProperty(value = "IP")
    @TableField("ip")
    private String ip;

    @ApiModelProperty(value = "局域网IP")
    @TableField("ip_lan")
    private String ipLan;

    @ApiModelProperty(value = "逻辑删除，0-存在，1-已删除")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "操作记录，这里以前应该是为了其他目的准备的冗余字段")
    @TableField("record")
    private String record;

    @ApiModelProperty(value = "操作类型：登录（A）、新增（B）、修改（C）、删除（D）")
    @TableField("optype")
    private String opType;

    @ApiModelProperty(value = "操作日志")
    @TableField("detail")
    private String detail;

    @ApiModelProperty(value = "更新时间")
    @TableField("update_time")
    private LocalDateTime updateTime;

    @ApiModelProperty(value = "创建时间")
    @TableField("create_time")
    private LocalDateTime createTime;
}
