package com.autel.cloud.pile.base.domain.model.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel(value = "为桩(开启或者关闭)互联互通 入参模型")
public class SetPileEroamingForPileDTO implements Serializable {

    @ApiModelProperty(value = "充电桩Sn")
    private String pileSn;

    @ApiModelProperty(value = "操作类型(关闭互联互通(设置为私有):0; 开启互联互通(设置为公开):1)")
    private Integer operateType;
}
