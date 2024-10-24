package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.annotations.Setting;

/**
 * @Author temp
 * @Date 2022/7/19 20:24
 */
@Data
@Document(indexName = "pile_base_tb_rule_index", shards = 3)
@TypeAlias(value = "pile_base_tb_rule_index")
@Setting(settingPath = "setting/setting.json")
public class RuleElasticDTO{
    @ApiModelProperty("主键ID")
    @Id
    @Field(type = FieldType.Long)
    private Long id;

    @ApiModelProperty("规则名称")
    @Field(type = FieldType.Text,analyzer = "ik_max_word")
    private String name;

    @ApiModelProperty("商家ID")
    @Field(type = FieldType.Long)
    private Long sellerId;

    @ApiModelProperty("创建人")
    @Field(type = FieldType.Long)
    private Long createBy;

    @ApiModelProperty("更新人")
    @Field(type = FieldType.Long)
    private Long updateBy;

    @ApiModelProperty("规则明细列表")
    @Field(type = FieldType.Text, index = false)
    private String details;

    @ApiModelProperty("客户组类型: 0-用户 1-车辆 2-充电卡")
    @Field(type = FieldType.Long)
    private Integer memberType = 0;

    @ApiModelProperty("规则类型，0：白名单，1：黑名单")
    @Field(type = FieldType.Integer)
    private Integer ruleType = 0;
}
