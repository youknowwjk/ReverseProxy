package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.annotations.Setting;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 充电设备
 * @ClassName OpLocationEvseElastic
 * @Author A22121
 * @Description
 * @Date 2022/4/15 17:03
 * @Version 0.0.1-SNAPSHOT
 */
@Data
@Document(indexName = "pile_base_op_location_evse_index")
@Setting(settingPath = "setting/setting.json")
public class OpLocationEvseElasticDTO implements Serializable {
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

    @ApiModelProperty(value = "场站id")
    @Field(type = FieldType.Long)
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    @Field(type = FieldType.Keyword)
    private String locationName;

    @ApiModelProperty(value = "枪状态")
    @Field(type = FieldType.Keyword)
    private String state;

    @ApiModelProperty(value = "充电设备序列号")
    @Field(type = FieldType.Keyword)
    private String evseId;

    @ApiModelProperty(value = "楼层")
    @Field(type = FieldType.Integer)
    private Integer floorLevel;

    @ApiModelProperty(value = "纬度")
    @Field(type = FieldType.Keyword)
    private String latitude;

    @ApiModelProperty(value = "经度")
    @Field(type = FieldType.Keyword)
    private String longitude;

    @ApiModelProperty(value = "EVSE外部展示字符")
    @Field(type = FieldType.Keyword)
    private String physicalReference;

    @ApiModelProperty(value = "停车限制")
    @Field(type = FieldType.Keyword)
    private String parkingRestrictions;

    @ApiModelProperty(value = "pin码")
    @Field(type = FieldType.Keyword)
    private String pinCode;

    /**
     * 充电枪(充电设备)绑定的计费规则组id
     */
    @ApiModelProperty(value = "充电枪(充电设备)绑定的计费规则组id")
    @Field(type = FieldType.Long)
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long tariffId;

    @ApiModelProperty(value = "充电桩设备序列号")
    @Field(type = FieldType.Keyword)
    private String evseSn;

    @ApiModelProperty(value = "枪类型1.CCS Combo 2、2.CHAdeMO、3.Type2、4.Type3、5.Type1、6.Wall（Euro）、7.CCS Combo 1、8.GB/T DC、9.GB/T AC")
    @Field(type = FieldType.Keyword)
    private Integer gunType;

    @ApiModelProperty(value = "1: AC_1_PHASE, 2: AC_3_PHASE, 3: DC")
    @Field(type = FieldType.Keyword)
    private String powerType;

    @ApiModelProperty(value = "电压")
    @Field(type = FieldType.Integer)
    private BigDecimal voltage;

    @ApiModelProperty(value = "电流（A）")
    @Field(type = FieldType.Integer)
    private BigDecimal amperage;

    @ApiModelProperty(value = "功率")
    @Field(type = FieldType.Double)
    private Double power;

    @ApiModelProperty(value = "品牌id")
    @Field(type = FieldType.Long)
    private Long brandId;

    @ApiModelProperty(value = "品牌名")
    @Field(type = FieldType.Keyword)
    private String brandName;

    @ApiModelProperty(value = "桩序列号")
    @Field(type = FieldType.Keyword)
    private String pileSn;

    @ApiModelProperty(value = "桩名")
    @Field(type = FieldType.Keyword)
    private String pileName;

    @ApiModelProperty(value = "运营商id")
    @Field(type = FieldType.Long)
    private Long operatorId;

    @ApiModelProperty(value = "是否支持预约")
    @Field(type = FieldType.Boolean)
    private Boolean reservationEnabled;

    @ApiModelProperty(value = "是否订阅到期：true：生效中 false: 已到期")
    @Field(type = FieldType.Boolean)
    private Boolean subscriptionCheck;

    @ApiModelProperty(value = "最新状态上报时间(非离线)")
    @Field(type = FieldType.Long)
    private Long stateUploadTime;

    @ApiModelProperty(value = "最新离线状态上报时间")
    @Field(type = FieldType.Long)
    private Long offlineUploadTime;

    @ApiModelProperty(value = "上一次枪状态")
    @Field(type = FieldType.Keyword)
    private String lastState;

    @ApiModelProperty(value = "最后一笔订单OrderSeq")
    @Field(type = FieldType.Long)
    private Long lastOrderSeq;

    @ApiModelProperty(value = "进场控制规则ID")
    @Field(type = FieldType.Long)
    private Long ruleId;

    @ApiModelProperty(value = "规则名称")
    @Field(type = FieldType.Text, analyzer = "ik_max_word")
    private String ruleName;

    @ApiModelProperty(value = "ocpi evse uid")
    @Field(type = FieldType.Keyword)
    private String evseUid;
}
