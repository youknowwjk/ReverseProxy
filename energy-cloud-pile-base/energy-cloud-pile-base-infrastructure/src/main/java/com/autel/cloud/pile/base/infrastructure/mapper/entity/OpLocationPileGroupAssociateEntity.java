package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

/**
 * <p>
 * 桩和桩组关系表
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("op_location_pile_group_associate")
@ApiModel(value = "OpLocationPileGroupAssociateEntity对象", description = "桩和桩组关系表")
public class OpLocationPileGroupAssociateEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "场站ID")
    private Long locationId;

    @ApiModelProperty(value = "创建时间")
    @TableField("created_at")
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField("updated_at")
    private Long updatedAt;

    @ApiModelProperty(value = "是否删除")
    @TableField("deleted")
    private Boolean deleted;

    @ApiModelProperty(value = "状态")
    @TableField("status")
    private Integer status;

    @ApiModelProperty(value = "充电桩id")
    @TableField("pile_id")
    private Long pileId;

    @ApiModelProperty(value = "充电桩sn码")
    @TableField("pile_sn")
    private String pileSn;

    @ApiModelProperty(value = "充电桩组id")
    @TableField("group_id")
    private Long groupId;


}
