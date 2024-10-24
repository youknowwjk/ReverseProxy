package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author 吴超
 * @date 2021/6/25 13:42
 */
@Data
@ApiModel(value = "StartTransactionDto", description = "开始充电请求对象")
public class StartTransactionDto {
    @ApiModelProperty(value = "充电桩SN号，长度32位",required = true)
    @NotEmpty
    private String chargePileSn;

    @ApiModelProperty(value = "枪编号，长度11位",example = "0",required = true)
    @NotNull
    private Integer gunNo;

    @ApiModelProperty(value = "vin码", required = false)
    private String vin;

    @ApiModelProperty(value = "车辆年份", required = false)
    private String year;

    @ApiModelProperty(value = "车辆品牌", required = false)
    private String make;

    @ApiModelProperty(value = "车辆型号", required = false)
    private String model;

    @ApiModelProperty(value = "是否配置smartCharging，1：配置，如不配置则不填", required = false)
    private Integer smart;

    @ApiModelProperty(value = "用户ID", required = false)
    private String userId;

}
