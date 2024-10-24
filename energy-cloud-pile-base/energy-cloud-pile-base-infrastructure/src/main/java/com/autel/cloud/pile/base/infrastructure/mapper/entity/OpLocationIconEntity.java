package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 图标信息实体类
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_icon")
@ApiModel(value = "OpIconEntity对象", description = "图标信息")
public class OpLocationIconEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "编码")
    @TableField(value = "icon_code")
    private String iconCode;

    @ApiModelProperty(value = "名称")
    @TableField(value = "icon_name")
    private String iconName;

    @ApiModelProperty(value = "描述")
    @TableField(value = "icon_describe")
    private String iconDescribe;

    @ApiModelProperty(value = "语言")
    @TableField(value = "language")
    private String language;

    @ApiModelProperty(value = "url地址")
    @TableField(value = "icon_url")
    private String iconUrl;

    /**
     * 类别：1代表着周边设施
     */
    @ApiModelProperty(value = "类别(1:周边设施)")
    @TableField(value = "category")
    private String category;

    @ApiModelProperty(value = "创建时间")
    @TableField(value = "created_at")
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField(value = "updated_at")
    private Long updatedAt;

    @ApiModelProperty(value = "创建用户ID")
    @TableField(value = "created_by")
    private Long createdBy;

    @ApiModelProperty(value = "更新用户ID")
    @TableField(value = "updated_by")
    private Long updatedBy;

    /**
     * 0.代表未删除
     * 1.代表已删除
     */
    @ApiModelProperty(value = "是否删除")
    @TableField(value = "deleted")
    private Integer deleted;
}
