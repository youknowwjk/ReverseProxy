package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

/**
 * <p>
 * 充电设备组合（桩）
 * </p>
 *
 * @author A22121
 * @since 2022-05-10
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("op_location_pile_evse")
@ApiModel(value = "OpLocationPileEvseEntity对象", description = "充电设备组合（桩）")
public class OpLocationPileEvseEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.AUTO)
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @TableField(value = "created_at",fill = FieldFill.INSERT)
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @TableField(value = "updated_at",fill = FieldFill.INSERT_UPDATE)
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

    @ApiModelProperty(value = "充电设备组合（桩）序列号")
    @TableField("pile_sn")
    private String pileSn;

    @ApiModelProperty(value = "包含充电设备id")
    @TableField("evse_list")
    private String evseList;

    @ApiModelProperty(value = "桩名")
    @TableField("name")
    private String name;


    @ApiModelProperty(value = "品牌外键ID")
    @TableField(value = "brand_id")
    private Long brandId;


    @ApiModelProperty(value = "产品型号")
    @TableField(value = "product_model")
    private String productModel;


    @ApiModelProperty(value = "供应商")
    @TableField(value = "vendor")
    private String vendor;

    /**
     * 默认充电时间(仅英标桩才有默认充电时间)
     */
    @ApiModelProperty(value = "默认充电时间(仅英标桩才有默认充电时间)")
    @TableField(value = "default_charging_time")
    private String defaultChargingTime;

    /**
     * 配置公开属性标志，可选项为（1:公开-Public;0:私有-Private）
     */
    @ApiModelProperty(value = "配置公开属性标志，可选项为（1:公开-Public;0:私有-Private）")
    @TableField(value = "public_mark")
    private Integer publicMark;

    /**
     * 桩是否开启了互联互通的标志（1:桩开启了互联互通;0:桩关闭了互联互通）
     */
    @ApiModelProperty(value = "桩是否开启了互联互通的标志（1:桩开启了互联互通;0:桩关闭了互联互通）")
    @TableField(value = "eroaming_enable")
    private Integer eroamingEnable;

    @ApiModelProperty(value = "充电桩是否设置了FreeVend启动方式(1:开启状态; 0:关闭状态)")
    @TableField(value = "free_vend_enable")
    private Integer freeVendEnable;

    @ApiModelProperty(value = "充电桩启动充电时的idTag")
    @TableField(value = "free_vend_id_tag")
    private String freeVendIdTag;
}
