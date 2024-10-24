package com.autel.cloud.pile.base.domain.model;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * 需求费控制器表
 * @TableName demand_control_config
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class ControlContentDTO implements Serializable {


    @ApiModelProperty("id")
    private String id;
    /**
     * 功率上限
     */
    @ApiModelProperty("功率上限")
    private BigDecimal powerUp;

    /**
     * 季节  （1：夏季(6月-9月）、2：冬季(10月-5月)、0：全年）
     */
    @ApiModelProperty("季节  （1：夏季(6月-9月）、2：冬季(10月-5月)、0：全年）")
    private Integer season;

    /**
     * 工作日/周末  0：所有 1：周末 2：工作日
     */
    @ApiModelProperty("工作日/周末  0：所有 1：周末 2：工作日")
    private Integer weekDay;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    /**
     * 控制名称细分
     */
    @ApiModelProperty("控制名称细分")
    private String childControlName;

    public ControlContentDTO(Long rootId,String startTime, String endTime) {
        this.id = rootId.toString();
        this.startTime = startTime;
        this.endTime = endTime;
    }
}