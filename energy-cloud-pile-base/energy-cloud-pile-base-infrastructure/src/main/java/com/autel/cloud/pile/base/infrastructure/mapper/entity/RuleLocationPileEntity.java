package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 进场控制规则与桩关系表
 * @TableName tb_rule_location_pile
 */
@ApiModel(description = "进场控制规则与桩关系表")
@TableName(value ="tb_rule_location_pile")
@Data
public class RuleLocationPileEntity implements Serializable {
    /**
     * 主键ID
     */
    @ApiModelProperty("主键ID")
    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 规则ID
     */
    @ApiModelProperty("规则ID")
    private Long ruleId;

    /**
     * 场站ID
     */
    @ApiModelProperty("场站ID")
    private Long locationId;

    /**
     * 桩ID
     */
    @ApiModelProperty("桩ID")
    private Long pileId;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Long createTime;

    /**
     * 桩sn
     */
    @ApiModelProperty("桩sn")
    @TableField("pile_sn")
    private String pileSn;

    /**
     * 充电设备sn
     */
    @ApiModelProperty("充电设备sn")
    @TableField("evse_sn")
    private String evseSn;

    /**
     * 桩名称
     */
    @ApiModelProperty("桩名称")
    @TableField("pile_name")
    private String pileName;

    @ApiModelProperty("")
    @TableField(exist = false)
    private static final long serialVersionUID = 1L;
}