package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.data.elasticsearch.annotations.Setting;

import java.io.Serializable;

/**
 * @ClassName OpLocationPileGroupElasticDTO
 * @Author A22121
 * @Description
 * @Date 2022/4/15 16:27
 * @Version 0.0.1-SNAPSHOT
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@Document(indexName = "pile_base_op_location_pile_group_associate_index")
@Setting(settingPath = "setting/setting.json")
public class OpLocationPileGroupAssociateElasticDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @Id
    private Long id;

    @ApiModelProperty(value = "桩组id")
    @Field(type = FieldType.Long)
    private Long groupId;

    @ApiModelProperty(value = "桩组名称")
    @Field(type = FieldType.Keyword)
    private String groupName;

    @ApiModelProperty(value = "创建时间")
    @Field(type = FieldType.Long)
    private Long createdAt;

    @ApiModelProperty(value = "更新时间")
    @Field(type = FieldType.Long)
    private Long updatedAt;

    @ApiModelProperty(value = "桩组状态")
    @Field(type = FieldType.Integer)
    private Integer groupStatus;

    @ApiModelProperty(value = "场站id")
    @Field(type = FieldType.Long)
    private Long locationId;

    @ApiModelProperty(value = "充电桩id")
    @Field(type = FieldType.Long)
    private Long pileId;

    @ApiModelProperty(value = "充电桩sn码")
    @Field(type = FieldType.Keyword)
    private String pileSn;

}
