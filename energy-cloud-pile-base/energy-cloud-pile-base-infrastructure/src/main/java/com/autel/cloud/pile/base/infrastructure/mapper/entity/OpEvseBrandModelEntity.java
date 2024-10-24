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
import java.math.BigDecimal;

/**
 * <p>
 * 桩品牌型号表
 * </p>
 *
 * @author A22587
 * @since 2022-09-13
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_evse_brand_model")
@ApiModel(value = "OpEvseBrandModelEntity对象", description = "桩品牌型号表")
public class OpEvseBrandModelEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "品牌id")
    @TableField(value = "brand_id")
    private Long brandId;

    @ApiModelProperty(value = "品牌名称")
    @TableField(value = "brand_name")
    private String brandName;

    @ApiModelProperty(value = "产品型号")
    @TableField(value = "product_model")
    private String productModel;

    @ApiModelProperty(value = "枪类型集合 1.CCS Combo 2、2.CHAdeMO、3.Type2、4.Type3、5.Type1、6.Wall（Euro）、7.CCS Combo 1、8.GB/T DC、9.GB/T AC")
    @TableField(value = "gun_type_list")
    private String gunTypeList;

    @ApiModelProperty(value = "电源类型")
    @TableField(value = "power_type")
    private String powerType;

    @ApiModelProperty(value = "额定功率(KW)")
    @TableField(value = "power")
    private Double power;

    @ApiModelProperty(value = "电压(V)")
    @TableField(value = "voltage")
    private BigDecimal voltage;

    @ApiModelProperty(value = "电流(A)")
    @TableField(value = "amperage")
    private BigDecimal amperage;

    @ApiModelProperty(value = "是否删除(1:已删除;0:未删除)")
    @TableField(value = "deleted")
    private Integer deleted;

    @ApiModelProperty(value = "创建时间")
    @TableField(value = "create_time")
    private Long creatTime;

    @ApiModelProperty(value = "创建人ID")
    @TableField(value = "created_by")
    private Long createdBy;

    @ApiModelProperty(value = "添加人名称")
    @TableField(value = "created_by_name")
    private String createdByName;

    @ApiModelProperty(value = "更新时间")
    @TableField(value = "update_time")
    private Long updateTime;

    @ApiModelProperty(value = "更新人ID")
    @TableField(value = "updated_by")
    private Long updatedBy;

    @ApiModelProperty(value = "最后操作人名称")
    @TableField(value = "updated_by_name")
    private String updatedByName;
}
