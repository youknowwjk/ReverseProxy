package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.*;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName OpLocationElastic
 * @Author A22121
 * @Description
 * @Date 2022/4/15 16:27
 * @Version 0.0.1-SNAPSHOT
 */
@Data
@Document(indexName = "pile_base_op_location_index")
@Setting(settingPath = "setting/elasticsearch_setting.json")
public class OpLocationElasticDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @Id
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @Field(type = FieldType.Long)
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @Field(type = FieldType.Long)
    private Long updatedAt;

    @ApiModelProperty(value = "状态")
    @Field(type = FieldType.Integer)
    private Integer status;

    @ApiModelProperty(value = "场站类型")
    @Field(type = FieldType.Keyword)
    private String type;

    @ApiModelProperty(value = "场站名称")
    @Field(type = FieldType.Keyword)
    private String name;

    @ApiModelProperty(value = "场站地址")
    @Field(type = FieldType.Keyword)
    private String address;

    @ApiModelProperty(value = "成本电价标识，0：未开启，1：开启")
    @Field(type = FieldType.Integer)
    private Integer priceFlag = 0;

    @ApiModelProperty(value = "场站所在城市")
    @Field(type = FieldType.Keyword)
    private String city;

    @ApiModelProperty(value = "邮政编码")
    @Field(type = FieldType.Keyword)
    private String postalCode;

    @ApiModelProperty(value = "国家代码")
    @Field(type = FieldType.Keyword)
    private String country;

    @ApiModelProperty(value = "场站所在省份")
    @Field(type = FieldType.Keyword)
    private String province;

    @ApiModelProperty(value = "纬度")
    @Field(type = FieldType.Keyword)
    private String latitude;

    @ApiModelProperty(value = "经度")
    @Field(type = FieldType.Keyword)
    private String longitude;

    @ApiModelProperty(value = "地点")
    @GeoPointField
    private GeoPoint location;

    @ApiModelProperty(value = "运营商id")
    @Field(type = FieldType.Long)
    private Long operatorId;

    @ApiModelProperty(value = "子运营商id")
    @Field(type = FieldType.Long)
    private Long subOperatorId;

    @ApiModelProperty(value = "所有者id")
    @Field(type = FieldType.Long)
    private Long ownerId;

    @ApiModelProperty(value = "时区")
    @Field(type = FieldType.Keyword)
    private String timeZone;

    @ApiModelProperty(value = "时区名称id")
    @Field(type = FieldType.Keyword)
    private String zoneId;

    @ApiModelProperty(value = "关闭可否充电0/1")
    @Field(type = FieldType.Integer)
    private Integer chargingWhenClosed;

    @ApiModelProperty(value = "开放类型 1：对外开放；2：不对外开放")
    @Field(type = FieldType.Integer)
    private Integer openType;

    @ApiModelProperty(value = "运营类型")
    @Field(type = FieldType.Keyword)
    private String operationType;

    @ApiModelProperty(value = "站点的状态(失效)")
    @Field(type = FieldType.Integer)
    private Integer state;

    @ApiModelProperty(value = "客服电话")
    @Field(type = FieldType.Keyword)
    private String serviceTel;

    @ApiModelProperty(value = "场站公告")
    @Field(type = FieldType.Keyword)
    private String announcement;

    @ApiModelProperty(value = "公告时间")
    @Field(type = FieldType.Long)
    private Long announcementAt;

    @ApiModelProperty(value = "是否在APP展示")
    @Field(type = FieldType.Boolean)
    private Boolean appShow;

    @ApiModelProperty(value = "变压器功率")
    @Field(type = FieldType.Double)
    @Deprecated
    private BigDecimal transPower;

    @ApiModelProperty(value = "场站收费规则说明")
    @Field(type = FieldType.Keyword)
    private String billingRule;

    @ApiModelProperty(value = "运营时间")
    @Field(type = FieldType.Long)
    private Long operationDate;

    @ApiModelProperty(value = "组织机构外键ID")
    @Field(type = FieldType.Long)
    @Deprecated
    private Long groupId;

    @ApiModelProperty(value = "组织机构名称")
    @Field(type = FieldType.Keyword)
    @Deprecated
    private String groupName;

    @ApiModelProperty(value = "周边设施(1:餐饮;2:无线网络;3:卫生间;4:住宿;5:停车场;6:购物;7:娱乐)")
    @Field(type = FieldType.Keyword)
    private String facility;

    @ApiModelProperty(value = "Stripe账号信息")
    @Field(type = FieldType.Keyword)
    private String stripeAccount;

    @ApiModelProperty(value = "个人名称")
    @Field(type = FieldType.Keyword)
    private String personalName;

    @ApiModelProperty(value = "个人姓")
    @Field(type = FieldType.Keyword)
    private String personalSurName;

    @ApiModelProperty(value = "是否支持预约")
    @Field(type = FieldType.Boolean)
    private Boolean reservationEnabled;

    @ApiModelProperty(value = "门牌号")
    @Field(type = FieldType.Keyword)
    private String houseNumber;

    @ApiModelProperty(value = "是否互联互通：true：开启互联互通 false: 关闭")
    @Field(type = FieldType.Boolean)
    private Boolean hubjectCheck;

    @Deprecated
    @ApiModelProperty(value = "场站进场条件:1 公共免费入场;2 公共收费入场;3 公共限制入场(注意：这个字段已经被废弃掉了，已经不再维护该字段了！)")
    @Field(type = FieldType.Integer)
    private Integer conditionsValue;

    @ApiModelProperty(value = "平台类型，1：Autel, 2:Hubject, 3:OCPI-EMSP")
    @Field(type = FieldType.Integer)
    private Integer platform = 1;

    /**
     * 位置类型
     */
    @ApiModelProperty(value = "位置类型(1:Onstreet;2:Parking Garage;3:UndergroundParkingGarage;4:Workplace;5:Retail;6:Hotel;7:Motorway Service Stop Charging;8:Fleet;9:Other)")
    @Field(type = FieldType.Integer)
    private Integer locationType;

    /**
     * 税费配置
     */
    @ApiModelProperty(value = "税费配置")
    @Field(type = FieldType.Text)
    private String taxConfiguration;


    @ApiModelProperty(value = "希望开通互联互通的用户数量")
    @Field(type = FieldType.Long)
    private Long eRoamingNum;

    /**
     * 付费模式（1：预支付， 2：后支付）
     */
    @ApiModelProperty("payMethod")
    private Integer payMethod;

    /**
     * 预付金额档位列表
     */
    @ApiModelProperty("prepaymentAmountTier")
    private String prepaymentAmountTier;

    @ApiModelProperty(value = "业务类型，1：运营；2：运营+广告；3：广告")
    @Field(type = FieldType.Integer)
    private Integer businessType = 1;
}
