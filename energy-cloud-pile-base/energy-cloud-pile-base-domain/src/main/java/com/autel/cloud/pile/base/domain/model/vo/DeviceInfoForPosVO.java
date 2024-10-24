package com.autel.cloud.pile.base.domain.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
@ApiModel("为POS机查询桩信息 出参模型")
public class DeviceInfoForPosVO implements Serializable {

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty(value = "商家充电桩资产表id")
    private Long pileId;

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty(value = "商家终端资产表id")
    private Long terminalId;

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty(value = "场站id")
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    private String locationName;

    @ApiModelProperty(value = "充电桩名称")
    private String pileName;

    @ApiModelProperty(value = "充电桩SN")
    private String pileSn;

    @ApiModelProperty(value = "终端名称")
    private String terminalName;

    @ApiModelProperty(value = "终端SN")
    private String terminalSn;

    @ApiModelProperty(value = "枪号")
    private List<Integer> connectors;

    @ApiModelProperty(value = "桩类型")
    private String pileType;

    @ApiModelProperty("超充桩标志(0:非超充桩; 1:超充桩)")
    private Integer overchargingPileFlag;

    @ApiModelProperty(value = "场站更新时间")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationUpdatedAt;

    @ApiModelProperty(value = "设备更新时间")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long deviceUpdatedAt;

    @ApiModelProperty("是否有权限")
    private Boolean havePermission;
}
