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
 * 场站人员
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_personnel")
@ApiModel(value = "OpLocationPersonnelEntity对象", description = "场站人员")
public class OpLocationPersonnelEntity implements Serializable {

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

    @ApiModelProperty(value = "名称")
    @TableField("name")
    private String name;

    @ApiModelProperty(value = "网站")
    @TableField("website")
    private String website;

    @ApiModelProperty(value = "logo图片id")
    @TableField("logo")
    private Long logo;

    @ApiModelProperty(value = "电话区号")
    @TableField("country_code")
    private String countryCode;

    @ApiModelProperty(value = "电话")
    @TableField("phone")
    private String phone;

    @ApiModelProperty(value = "描述")
    @TableField("text")
    private String text;

    @ApiModelProperty(value = "站点id")
    @TableField("location_id")
    private Long locationId;


}
