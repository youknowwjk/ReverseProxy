package com.autel.cloud.pile.base.domain.model.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel(value = "为场站(开启或者关闭)互联互通 入参模型")
public class SetLocationEroamingForLocationDTO implements Serializable {

    @ApiModelProperty(value = "场站id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationId;

    @ApiModelProperty(value = "操作类型(关闭互联互通:0; 开启互联互通:1)")
    private Integer operateType;
}
