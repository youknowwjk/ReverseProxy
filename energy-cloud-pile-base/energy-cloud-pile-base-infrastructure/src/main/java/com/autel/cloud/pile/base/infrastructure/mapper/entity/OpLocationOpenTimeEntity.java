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

/**
 * <p>
 * 场站设施
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_open_time")
@ApiModel(value = "OpLocationOpenTimeEntity对象", description = "场站设施")
public class OpLocationOpenTimeEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @TableField("created_at")
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField("updated_at")
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

    @ApiModelProperty(value = "星期几(0-7)/2022-01-04")
    @TableField("date_value")
    private String dateValue;

    @ApiModelProperty(value = "开始时间")
    @TableField("period_begin")
    private String periodBegin;

    @ApiModelProperty(value = "结束时间")
    @TableField("period_end")
    private String periodEnd;

    @ApiModelProperty(value = "开放1/关闭0")
    @TableField("type")
    private Integer type;

    @ApiModelProperty(value = "weekly/daily")
    @TableField("date_type")
    private String dateType;


}
