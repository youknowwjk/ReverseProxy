package com.autel.cloud.pile.base.domain.model.dto;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel("设备数据 入参模型")
public class DeviceInfoDTO implements Serializable {

    @ApiModelProperty("超充桩标志(0:非超充桩; 1:超充桩)")
    private Integer overchargingPileFlag;

    @ApiModelProperty(value = "终端SN")
    private String terminalSn;

    @ApiModelProperty(value = "充电桩SN")
    private String pileSn;
}