package com.autel.cloud.pile.base.domain.model.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel("为POS机查询桩信息 入参模型")
public class DeviceInfoForPosDTO implements Serializable {

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty("商户id")
    private Long sellerId;

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty("用户id")
    private Long userId;
}
