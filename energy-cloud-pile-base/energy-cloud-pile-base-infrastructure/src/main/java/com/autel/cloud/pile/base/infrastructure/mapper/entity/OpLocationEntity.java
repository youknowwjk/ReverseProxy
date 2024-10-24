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
@TableName("op_location")
@ApiModel(value = "OpLocationEntity对象", description = "场站表")
public class OpLocationEntity implements Serializable {

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

    @ApiModelProperty(value = "场站类型")
    @TableField("type")
    private String type;

    @ApiModelProperty(value = "场站名称")
    @TableField("name")
    private String name;

    @ApiModelProperty(value = "场站地址")
    @TableField("address")
    private String address;

    @ApiModelProperty(value = "场站所在城市")
    @TableField("city")
    private String city;

    @ApiModelProperty(value = "邮政编码")
    @TableField("postal_code")
    private String postalCode;

    @ApiModelProperty(value = "国家代码")
    @TableField("country")
    private String country;

    @ApiModelProperty(value = "省")
    @TableField("province")
    private String province;

    @ApiModelProperty(value = "纬度")
    @TableField("latitude")
    private String latitude;

    @ApiModelProperty(value = "经度")
    @TableField("longitude")
    private String longitude;

    @ApiModelProperty(value = "运营商id")
    @TableField("operator_id")
    private Long operatorId;

    @ApiModelProperty(value = "子运营商id")
    @TableField("sub_operator_id")
    private Long subOperatorId;

    @ApiModelProperty(value = "所有者id")
    @TableField("owner_id")
    private Long ownerId;

    @ApiModelProperty(value = "时区")
    @TableField("time_zone")
    @Deprecated
    private String timeZone;

    @ApiModelProperty(value = "时区id")
    @TableField("zone_id")
    private String zoneId;

    @ApiModelProperty(value = "关闭可否充电0/1")
    @TableField("charging_when_closed")
    private Integer chargingWhenClosed;

    @ApiModelProperty(value = "是否支持预约 ")
    @TableField("reservation_enabled")
    private Boolean reservationEnabled;

    @ApiModelProperty(value = "门牌号  ")
    private String houseNumber;

    @ApiModelProperty(value = "是否互联互通：true：开启互联互通 false: 关闭")
    private Boolean hubjectCheck;

    @Deprecated
    @ApiModelProperty(value = "场站进场条件:1 公共免费入场;2 公共收费入场;3 公共限制入场(注意：这个字段已经被废弃掉了，已经不再维护该字段了！)")
    private Integer conditionsValue;

    @ApiModelProperty(value = "是否支持OCPI")
    @TableField("ocpi_enabled")
    private Boolean ocpiEnabled;

    @ApiModelProperty(value = "删除时间")
    private Long deletedTime;

    /**
     * 位置类型
     */
    @ApiModelProperty(value = "位置类型(1:Onstreet;2:Parking Garage;3:UndergroundParkingGarage;4:Workplace;5:Retail;6:Hotel;7:Motorway Service Stop Charging;8:Fleet;9:Other)")
    @TableField("location_type")
    private Integer locationType;

    /**
     * 税费配置
     */
    @ApiModelProperty(value = "税费配置")
    @TableField("tax_configuration")
    private String taxConfiguration;

    /**
     * 付费模式（1：预支付， 2：后支付）
     */
    @TableField("pay_method")
    private Integer payMethod;

    /**
     * 预付金额档位列表
     */
    @TableField("prepayment_amount_tier")
    private String prepaymentAmountTier;

    @ApiModelProperty(value = "业务类型，1：运营；2：运营+广告；3：广告")
    private Integer businessType;
}
