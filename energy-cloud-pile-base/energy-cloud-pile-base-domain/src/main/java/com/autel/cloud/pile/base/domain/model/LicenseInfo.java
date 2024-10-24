package com.autel.cloud.pile.base.domain.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * License详情
 *
 * @author A22598
 * @date 2023/06/15
 */
@Data
@ApiModel("License详情")
public class LicenseInfo {
    @ApiModelProperty(value = "订单Id", example = "00001")
    private String orderId;

    @ApiModelProperty(value = "License", example = "DE0240A1GM6C90075")
    private String licenseCode;

    @ApiModelProperty(value = "状态", example = "1", notes = "0:新建；1：有效，2：失效")
    private Integer status;

    @ApiModelProperty(value = "桩SN", example = "AE0022A1GM8E102006")
    private String pileSn;

    @ApiModelProperty(value = "关联桩名称", example = "我的桩")
    private String pileName;

    @ApiModelProperty(value = "订阅时长")
    private String serviceTime;

    @ApiModelProperty(value = "剩余有效宽限期", example = "90")
    private Integer extendedRemainingDays;

    @ApiModelProperty(value = "许可证关联日期")
    private Long bindTime;
}
