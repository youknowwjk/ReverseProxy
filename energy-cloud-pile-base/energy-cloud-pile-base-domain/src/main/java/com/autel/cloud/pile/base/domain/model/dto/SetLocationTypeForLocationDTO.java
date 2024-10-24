package com.autel.cloud.pile.base.domain.model.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel(value = "为场站设置位置类型属性 入参模型")
public class SetLocationTypeForLocationDTO implements Serializable {

    @ApiModelProperty(value = "场站id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationId;

    @ApiModelProperty(value = "位置类型(1:Onstreet; 2:Parking Garage; 3:UndergroundParkingGarage; 4:Workplace; 5:Retail; 6:Hotel; 7:Motorway Service Stop Charging; 8:Fleet; 9:Other)")
    private Integer locationType;
}
