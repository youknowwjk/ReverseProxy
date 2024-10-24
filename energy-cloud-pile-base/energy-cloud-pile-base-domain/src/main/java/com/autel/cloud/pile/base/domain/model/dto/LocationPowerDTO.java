package com.autel.cloud.pile.base.domain.model.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

@Data
@ApiModel("场站总功率")
public class LocationPowerDTO implements Serializable {

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty("场站id")
    private Long locationId;

    @JsonFormat(shape = JsonFormat.Shape.STRING)
    @ApiModelProperty("该场站下所有充电桩的额定功率之和")
    private BigDecimal totalPowerOfPile;
}
