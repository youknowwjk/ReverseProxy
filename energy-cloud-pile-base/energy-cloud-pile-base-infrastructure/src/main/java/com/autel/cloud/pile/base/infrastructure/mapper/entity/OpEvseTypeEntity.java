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
 * 枪类型表
 * </p>
 *
 * @author A22327
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_evse_type")
@ApiModel(value = "opEvseType对象", description = "枪类型表")
public class OpEvseTypeEntity implements Serializable {

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

    @ApiModelProperty(value = "图片Id")
    @TableField("image_id")
    private Long imageId;

    @ApiModelProperty(value = "枪类型名")
    @TableField("name")
    private String name;

    @ApiModelProperty(value = "枪类型")
    @TableField("gun_type")
    private Integer gunType;

    @ApiModelProperty(value = "1.未选中大图 2.选中大图 3.小图")
    @TableField("type")
    private Integer type;

    @ApiModelProperty(value = "交直流类型 AC交流 DC直流")
    @TableField("ad_type")
    private String ADType;
}
