package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import com.autel.cloud.pile.base.constant.BaseConstant;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.*;
import org.springframework.data.elasticsearch.core.geo.GeoPoint;

import java.math.BigDecimal;

/**
 * 充电设备扩展类
 * @Author temp
 * @Date 2022/10/18 11:30
 */
@Data
@Setting(settingPath = "setting/setting.json")
@Document(indexName = BaseConstant.PILE_BASE_OP_LOCATION_EVSE_EXPAND_INDEX)
public class OpLocationEvseExpandElasticDTO {
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

    @ApiModelProperty(value = "场站id")
    @Field(type = FieldType.Long)
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    @Field(type = FieldType.Keyword)
    private String name;

    @ApiModelProperty(value = "场站地址")
    @Field(type = FieldType.Keyword)
    private String address;

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
    private BigDecimal transPower;

    @ApiModelProperty(value = "场站收费规则说明")
    @Field(type = FieldType.Keyword)
    private String billingRule;

    @ApiModelProperty(value = "运营时间")
    @Field(type = FieldType.Long)
    private Long operationDate;

    @ApiModelProperty(value = "组织机构外键ID")
    @Field(type = FieldType.Long)
    private Long groupId;

    @ApiModelProperty(value = "组织机构名称")
    @Field(type = FieldType.Keyword)
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
    @Deprecated
    private Boolean hubjectCheck;

    @ApiModelProperty(value = "场站进场条件:1 公共免费入场;2 公共收费入场;3 公共限制入场")
    @Field(type = FieldType.Integer)
    private Integer conditionsValue;

    @ApiModelProperty(value = "枪功率")
    @Field(type = FieldType.Double)
    private Double power;

    @ApiModelProperty(value = "枪状态")
    @Field(type = FieldType.Keyword)
    private String gunState;

    @ApiModelProperty(value = "枪类型1.CCS Combo 2、2.CHAdeMO、3.Type2、4.Type3、5.Type1、6.Wall（Euro）、7.CCS Combo 1、8.GB/T DC、9.GB/T AC")
    @Field(type = FieldType.Keyword)
    private Integer gunType;

    @ApiModelProperty(value = "平台类型，1：Autel, 2:Hubject, 3:...")
    @Field(type = FieldType.Integer)
    private Integer platform = 1;

    @ApiModelProperty(value = "计费规则id")
    @Field(type = FieldType.Long)
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long tariffId;

    @ApiModelProperty(value = "是否订阅到期：true：生效中 false: 已到期")
    @Field(type = FieldType.Boolean)
    private Boolean subscriptionCheck;

    @ApiModelProperty(value = "业务类型，1：运营；2：运营+广告；3：广告")
    private Integer businessType = 1;
}
