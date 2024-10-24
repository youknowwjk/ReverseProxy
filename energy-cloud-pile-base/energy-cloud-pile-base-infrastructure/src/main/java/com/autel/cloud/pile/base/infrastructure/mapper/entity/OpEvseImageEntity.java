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
 * 充电设备图片
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_evse_image")
@ApiModel(value = "OpEvseImageEntity对象", description = "充电设备图片")
public class OpEvseImageEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @TableField(value = "created_at", fill = FieldFill.INSERT)
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField(value = "updated_at", fill = FieldFill.INSERT_UPDATE)
    private Long updatedAt;

    @ApiModelProperty(value = "是否删除")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "状态")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "充电设备id")
    @TableField("location_evse_id")
    private Long locationEvseId;

    @ApiModelProperty(value = "图片id")
    @TableField("image_id")
    private String imageId;


}
