package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 进场控制规则表
 * @TableName tb_rule
 */
@ApiModel(description = "进场控制规则表")
@TableName(value ="tb_rule")
@Data
public class RuleEntity implements Serializable {
    /**
     * 主键ID
     */
    @ApiModelProperty("主键ID")
    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 规则名称
     */
    @ApiModelProperty("规则名称")
    private String name;

    /**
     * 商家ID
     */
    @ApiModelProperty("商家ID")
    private Long sellerId;

    /**
     * 规则类型
     */
    @ApiModelProperty("规则类型，0：白名单，1：黑名单")
    private Integer ruleType;

    /**
     * 创建人
     */
    @ApiModelProperty("创建人")
    private Long createBy;

    /**
     * 更新人
     */
    @ApiModelProperty("更新人")
    private Long updateBy;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @TableField(fill = FieldFill.INSERT)
    private Long createTime;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updateTime;

    /**
     * 0：未删除 1：已删除
     */
    @ApiModelProperty("0：未删除 1：已删除")
    private Integer deleted;

    /**
     * 版本号
     */
    @ApiModelProperty("版本号")
    @Version
    private Long version;

    @ApiModelProperty("客户组类型: 0-用户 1-车辆 2-充电卡")
    @TableField(value = "member_type")
    private Integer memberType;

    @ApiModelProperty("")
    @TableField(exist = false)
    private static final long serialVersionUID = 1L;

}