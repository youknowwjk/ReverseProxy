package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @author A22591
 * @since 2022-08-31
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_pile_share_white_list")
@ApiModel(value = "OpPileShareWhiteListEntity对象", description = "家桩共享白名单表")
public class OpPileShareWhiteListEntity implements Serializable {

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

    @ApiModelProperty("创建人")
    @TableField(value = "created_by")
    private Long createdBy;

    @ApiModelProperty(value = "充电设备组合（桩）序列号")
    @TableField("pile_sn")
    private String pileSn;
}
