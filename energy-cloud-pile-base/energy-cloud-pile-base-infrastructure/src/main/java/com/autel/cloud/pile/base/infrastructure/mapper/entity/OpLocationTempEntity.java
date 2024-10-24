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
 * 场站表
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_temp")
@ApiModel(value = "OpLocationTempEntity对象", description = "场站临时表")
public class OpLocationTempEntity implements Serializable {

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

    @ApiModelProperty(value = "用户ID")
    @TableField("user_id")
    private Long userId;

    @ApiModelProperty(value = "场站基本信息")
    @TableField("location_json")
    private String locationJson;

    @ApiModelProperty(value = "桩基本信息")
    @TableField("pile_json")
    private String pileJson;

    @ApiModelProperty(value = "计费规则ID")
    @TableField("tariff_id")
    private Long tariffId;

    @ApiModelProperty(value = "桩与计费规则映射关系")
    @TableField("pile_tariff_json")
    private String pileTariffJson;

}
