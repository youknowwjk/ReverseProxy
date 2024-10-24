package com.autel.cloud.pile.base.infrastructure.feign.dto;

import com.autel.cloud.pile.base.ChargePointNoticeEvent;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ApiModel("为POS机同步设备信息 入参模型")
public class SyncPileInfoForPosDTO implements Serializable {

    @ApiModelProperty(value = "事件")
    private ChargePointNoticeEvent.Event event;

    @ApiModelProperty("设备类型(0:超充主机; 1:超充终端; 2:其它设备)")
    private Integer deviceType;

    @ApiModelProperty(value = "充电桩SN")
    private String pileSn;

    @ApiModelProperty(value = "充电桩名称")
    private String pileName;

    @ApiModelProperty(value = "终端SN")
    private String terminalSn;

    @ApiModelProperty(value = "终端名称")
    private String terminalName;

    @ApiModelProperty(value = "商家id")
    private Long sellerId;
}
