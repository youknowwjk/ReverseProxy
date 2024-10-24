package com.autel.cloud.pile.base.domain.model;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

@Data
@ApiModel(value = "为POS机查询设备信息 出参模型")
public class SelectDeviceInfoForPosVO {

    @ApiModelProperty(value = "桩id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long pileId;

    @ApiModelProperty(value = "桩SN")
    private String pileSn;

    @ApiModelProperty(value = "桩名称")
    private String pileName;

    @ApiModelProperty(value = "枪号集合")
    private List<Integer> connectorList;

    @ApiModelProperty("所属场站id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationId;

    @ApiModelProperty("所属场站名称")
    private String locationName;

    @ApiModelProperty("是否有权限")
    private Boolean havePermission;
}
