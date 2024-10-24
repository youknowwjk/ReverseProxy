package com.autel.cloud.pile.base.domain.model.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel(value = "为互联互通查询场站信息 出参模型")
public class SelectLocationInfoForEroamingVO implements Serializable {

    @ApiModelProperty(value = "场站id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    private String locationName;

    @ApiModelProperty(value = "场站地址")
    private String address;

    @ApiModelProperty(value = "位置类型(1:Onstreet; 2:Parking Garage; 3:UndergroundParkingGarage; 4:Workplace; 5:Retail; 6:Hotel; 7:Motorway Service Stop Charging; 8:Fleet; 9:Other)")
    private Integer locationType;

    @ApiModelProperty(value = "是否互联互通(true:开启; false:关闭)")
    private Boolean hubjectCheck;
}
