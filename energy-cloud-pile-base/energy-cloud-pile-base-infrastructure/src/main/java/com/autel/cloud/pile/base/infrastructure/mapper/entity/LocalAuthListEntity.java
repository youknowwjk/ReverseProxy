package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@TableName("local_auth_list")
public class LocalAuthListEntity {
    @ApiModelProperty("主键ID")
    @TableId
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;

    @ApiModelProperty("充电卡号")
    @TableField("card")
    private String card;

    @ApiModelProperty("状态(1:Accepted 2:Blocked 3:Expired 4:Invalid )")
    @TableField("status")
    private Integer status;

    @ApiModelProperty("创建时间")
    @TableField(value = "create_time")
    private Long createTime;

    @ApiModelProperty("更新时间")
    @TableField(value = "update_time",fill = FieldFill.INSERT_UPDATE)
    private Long updateTime;

    @ApiModelProperty("过期时间(-1为长期有效)")
    @TableField("expired_time")
    private Long expiredTime;

    @ApiModelProperty("商家id")
    @TableField("seller_id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long sellerId;

    @ApiModelProperty("桩sn")
    @TableField("sn")
    private String sn;
}
