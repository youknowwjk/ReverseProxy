package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * <p>
 * 充电站运营
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_location_operation")
@ApiModel(value = "OpLocationOperationEntity对象", description = "充电站运营")
public class OpLocationOperationEntity implements Serializable {

    private static final long serialVersionUID = 1L;

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

    @ApiModelProperty(value = "运营类型")
    @TableField("operation_type")
    private String operationType;

    @ApiModelProperty(value = "站点的状态(失效)")
    @TableField("state")
    private Integer state;

    @ApiModelProperty(value = "客服电话")
    @TableField("service_tel")
    private String serviceTel;

    @ApiModelProperty(value = "场站公告")
    @TableField("announcement")
    private String announcement;

    @ApiModelProperty(value = "公告时间")
    @TableField(value = "announcement_at", fill = FieldFill.INSERT_UPDATE)
    private Long announcementAt;

    @ApiModelProperty(value = "开放类型 1：对外开放；2：不对外开放")
    @TableField("open_type")
    private Integer openType;

    @ApiModelProperty(value = "是否在APP展示")
    @TableField("app_show")
    private Boolean appShow;

    @ApiModelProperty(value = "变压器功率")
    @TableField("trans_power")
    @Deprecated
    private BigDecimal transPower;

    @ApiModelProperty(value = "场站收费规则说明")
    @TableField("billing_rule")
    private String billingRule;

    @TableField("location_id")
    private Long locationId;

    @ApiModelProperty(value = "logo图片外键")
    @TableField(value = "logo_image_id")
    private Long logoImageId;

    @ApiModelProperty(value = "运营时间")
    @TableField(value = "operation_date")
    private Long operationDate;

    @ApiModelProperty(value = "组织结构ID")
    @TableField(value = "group_id")
    @Deprecated
    private Long groupId;

    @ApiModelProperty(value = "组织结构名称")
    @TableField(value = "group_name")
    @Deprecated
    private String groupName;


}
