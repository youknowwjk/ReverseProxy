package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.annotations.Setting;

import java.math.BigDecimal;

/**
 * 充电设备组合
 */
@Data
@Document(indexName = "pile_base_op_location_pile_evse_index")
@Setting(settingPath = "setting/setting.json")
public class OpLocationPileEvseElasticDTO {

    @ApiModelProperty(value = "主键id")
    @Id
    private Long id;

    @ApiModelProperty(value = "创建时间")
    @Field(type = FieldType.Long)
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @Field(type = FieldType.Long)
    private Long updatedAt;

    @ApiModelProperty(value = "场站id")
    @Field(type = FieldType.Long)
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    @Field(type = FieldType.Keyword)
    private String locationName;

    @ApiModelProperty(value = "桩序列号")
    @Field(type = FieldType.Keyword)
    private String pileSn;

    @ApiModelProperty(value = "充电设备列表")
    @Field(type = FieldType.Keyword)
    private String evseList;

    @ApiModelProperty(value = "桩名称")
    @Field(type = FieldType.Keyword)
    private String name;

    @ApiModelProperty(value = "品牌id")
    @Field(type = FieldType.Long)
    private Long brandId;

    @ApiModelProperty(value = "品牌名称")
    @Field(type = FieldType.Keyword)
    private String brandName;

    @ApiModelProperty(value = "产品型号")
    @Field(type = FieldType.Keyword)
    private String productModel;

    @ApiModelProperty(value = "供应商")
    @Field(type = FieldType.Keyword)
    private String vendor;

    @ApiModelProperty(value = "功率")
    @Field(type = FieldType.Double)
    private Double power;

    @ApiModelProperty(value = "功率类型")
    @Field(type = FieldType.Keyword)
    private String powerType;

    @ApiModelProperty(value = "运营商id")
    @Field(type = FieldType.Long)
    private Long operatorId;

    /**
     * 这个字段已经不再维护(废弃)了，请不要使用这个字段，这个字段存储的信息可能有误！！！
     */
    @ApiModelProperty(value = "计费规则id")
    @Field(type = FieldType.Long)
    @Deprecated
    private Long tariffId;

    @ApiModelProperty(value = "进场控制规则ID")
    @Field(type = FieldType.Long)
    private Long ruleId;

    @ApiModelProperty(value = "规则名称")
    @Field(type = FieldType.Text, analyzer = "ik_max_word")
    private String ruleName;

    @ApiModelProperty(value = "计费规则")
    @Field(type = FieldType.Keyword)
    private String costRule;

    @ApiModelProperty(value = "状态")
    @Field(type = FieldType.Integer)
    private Integer status;

    @ApiModelProperty(value = "是否开启随机延迟")
    @Field(type = FieldType.Boolean)
    private Boolean randomDelaySwitch;

    @ApiModelProperty(value = "随机延迟时长")
    @Field(type = FieldType.Integer)
    private Integer randomDelayTime;

    /**
     * 英标桩标志: (true: 英标桩, false: 非英标桩)
     */
    @ApiModelProperty(value = "英标桩标志: (true: 英标桩, false: 非英标桩);")
    @Field(type = FieldType.Boolean)
    private Boolean britainStandPileMark;

    /**
     * 默认充电时间(仅英标桩才有默认充电时间)
     */
    @ApiModelProperty(value = "默认充电时间(仅英标桩才有默认充电时间)")
    @Field(type = FieldType.Text)
    private String defaultChargingTime;

    /**
     * 英国法案认证开关(true: 开启，false: 关闭)
     */
    @ApiModelProperty(value = "英国法案认证开关(true: 开启，false: 关闭)")
    @Field(type = FieldType.Boolean)
    private Boolean britainApproveSwitch = false;

    /**
     * 配置公开属性标志，可选项为（1:公开-Public;0:私有-Private）
     */
    @ApiModelProperty(value = "配置公开属性标志，可选项为（1:公开-Public;0:私有-Private）")
    @Field(type = FieldType.Byte)
    private Integer publicMark;

    /**
     * 桩是否开启了互联互通的标志（1:桩开启了互联互通;0:桩关闭了互联互通）
     */
    @ApiModelProperty(value = "桩是否开启了互联互通的标志（1:桩开启了互联互通;0:桩关闭了互联互通）")
    @Field(type = FieldType.Integer)
    private Integer eroamingEnable;

    @ApiModelProperty(value = "充电桩离线电流")
    @Field(type = FieldType.Double)
    private BigDecimal offlineChargeCurrent;

    @ApiModelProperty(value = "充电桩是否设置了FreeVend启动方式(1:开启状态; 0:关闭状态)")
    @Field(type = FieldType.Integer)
    private Integer freeVendEnable;

    @ApiModelProperty(value = "充电桩启动充电时的idTag")
    @Field(type = FieldType.Keyword)
    private String freeVendIdTag;
}
