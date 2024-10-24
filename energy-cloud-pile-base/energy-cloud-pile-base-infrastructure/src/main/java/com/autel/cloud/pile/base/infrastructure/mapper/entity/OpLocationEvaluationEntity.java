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

@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_evaluation")
@ApiModel(value = "OpLocationOpenTimeEntity对象", description = "场站评价")
public class OpLocationEvaluationEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    @ApiModelProperty(value = "用户id")
    @TableField("user_id")
    private Long userId;

    @ApiModelProperty(value = "用户名")
    @TableField("user_name")
    private String userName;

    @ApiModelProperty(value = "场站id")
    @TableField("location_id")
    private Long locationId;

    @ApiModelProperty(value = "评价内容")
    @TableField("content")
    private String content;

    @ApiModelProperty(value = "评价时间")
    @TableField("time")
    private Long time;

    @ApiModelProperty(value = "评分")
    @TableField("core")
    private Integer core;
}
