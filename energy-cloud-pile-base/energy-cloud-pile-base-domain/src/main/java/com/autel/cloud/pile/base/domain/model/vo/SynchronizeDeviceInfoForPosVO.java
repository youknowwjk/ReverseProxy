package com.autel.cloud.pile.base.domain.model.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
@ApiModel("为POS机同步设备数据 出参模型")
public class SynchronizeDeviceInfoForPosVO {

    @ApiModelProperty(value = "充电桩名称")
    private String pileName;

    @ApiModelProperty(value = "充电桩SN")
    private String pileSn;

    @ApiModelProperty(value = "终端名称")
    private String terminalName;

    @ApiModelProperty(value = "终端SN")
    private String terminalSn;

    @ApiModelProperty("超充桩标志(0:非超充桩; 1:超充桩)")
    private Integer overchargingPileFlag;
}
