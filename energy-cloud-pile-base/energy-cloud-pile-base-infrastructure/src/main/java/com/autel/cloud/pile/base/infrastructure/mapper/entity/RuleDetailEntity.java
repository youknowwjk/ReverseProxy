package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 进场控制规则明细表
 * @TableName tb_rule_detail
 */
@ApiModel(description = "进场控制规则明细表")
@TableName(value ="tb_rule_detail")
@Data
public class RuleDetailEntity implements Serializable {
    /**
     * 主键ID
     */
    @ApiModelProperty("主键ID")
    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 规则ID
     */
    @ApiModelProperty("规则ID")
    private Long ruleId;

    /**
     * 星期几，多个值逗号分隔
     */
    @ApiModelProperty("星期几，多个值逗号分隔")
    private String days;

    /**
     * 开始时间
     */
    @ApiModelProperty("开始时间")
    private String startTime;

    /**
     * 结束时间
     */
    @ApiModelProperty("结束时间")
    private String endTime;

    /**
     * 会员分组ID，多值逗号分隔
     */
    @ApiModelProperty("会员分组ID，多值逗号分隔")
    private String memberGroupId;

    /**
     * 行动
     */
    @ApiModelProperty("行动")
    private Integer action;

    /**
     * 行动类型
     */
    @ApiModelProperty("行动类型")
    private Integer actionType;

    /**
     * 国家编码
     */
    @ApiModelProperty("国家编码")
    private String countryCode;

    /**
     * 联系人手机号
     */
    @ApiModelProperty("联系人手机号")
    private String phoneNumber;

    /**
     * 联系人邮箱
     */
    @ApiModelProperty("联系人邮箱")
    private String email;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    @TableField(fill = FieldFill.INSERT)
    private Long creatTime;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updateTime;

    /**
     * 0：未删除 1：删除
     */
    @ApiModelProperty("0：未删除 1：删除")
    private Integer deleted;

    /**
     * 版本号
     */
    @ApiModelProperty("版本号")
    @Version
    private Long version;

    @ApiModelProperty("")
    @TableField(exist = false)
    private static final long serialVersionUID = 1L;
}