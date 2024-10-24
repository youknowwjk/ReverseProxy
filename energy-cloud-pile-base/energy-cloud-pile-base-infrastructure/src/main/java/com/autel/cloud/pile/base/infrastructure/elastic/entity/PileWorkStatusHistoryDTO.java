package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import com.autel.cloud.pile.base.constant.BaseConstant;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

/**
 * @author A22581
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Accessors(chain = true)
@Document(indexName = BaseConstant.PILE_WORK_STATUS_HISTORY_INDEX + BaseConstant.ES_REAL_TIME_DYNAMIC_INDEX)
public class PileWorkStatusHistoryDTO {

    @Id
    @Field(type = FieldType.Keyword)
    private String id;
    /**
     * 桩序列号
     */
    @Field(type = FieldType.Keyword)
    private String sn;

    /**
     * 枪号
     */
    @Field(type = FieldType.Integer)
    private Integer connectorId;

    /**
     * 当前状态
     */
    @Field(type = FieldType.Integer)
    private Integer status;

    /**
     * 前一个状态
     */
    @Field(type = FieldType.Integer)
    private Integer prevStatus;

    /**
     * 枪状态下的子状态
     */
    @Field(type = FieldType.Integer)
    private Integer subStatus;

    /**
     * 运行状态
     */
    @Field(type = FieldType.Integer)
    private Integer runStatus;

    /**
     * 插枪状态
     */
    @Field(type = FieldType.Integer)
    private Integer cpStatus;

    /**
     * 更新时间
     */
    @Field(type = FieldType.Long)
    private Long createTime;

    /**
     * 报道至平台的时间
     */
    @Field(type = FieldType.Long)
    private Long reportTime;

}
