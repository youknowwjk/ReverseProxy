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
 * 国家区号表
 * </p>
 *
 * @author A22327
 * @since 2022-04-15
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("op_country")
@ApiModel(value = "OpCountry对象", description = "国家区号表")
public class OpCountryEntity implements Serializable {

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

    @ApiModelProperty(value = "国家缩写")
    @TableField("alpha_2_code")
    private String alpha2Code;

    @ApiModelProperty(value = "语言")
    @TableField("language")
    private String language;

    @ApiModelProperty(value = "名称")
    @TableField("name")
    private String name;

    @ApiModelProperty(value = "电话区号")
    @TableField("code")
    private String code;

    @ApiModelProperty(value = "国旗图片ID")
    @TableField("image_id")
    private Long imageId;

    @ApiModelProperty(value = "国旗货币符号")
    @TableField("currency_sign")
    private String currencySign;

    @ApiModelProperty(value = "货币信息ID")
    @TableField("currency_id")
    private Long currencyId;

}
