package com.autel.cloud.pile.base.domain.model.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
@ApiModel("为POS机同步设备数据 入参模型")
public class SynchronizeDeviceInfoForPosDTO implements Serializable {

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty("商户id")
    private Long sellerId;

    @ApiModelProperty("设备数据")
    private List<DeviceInfoDTO> deviceInfoDTOList;
}
