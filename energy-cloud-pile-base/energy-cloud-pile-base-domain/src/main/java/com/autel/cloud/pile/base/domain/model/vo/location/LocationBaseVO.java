package com.autel.cloud.pile.base.domain.model.vo.location;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@ApiModel("场站基础信息查询 出参模型")
@Data
public class LocationBaseVO implements Serializable {

    @ApiModelProperty(value = "场站id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationId;

    @ApiModelProperty(value = "场站名称")
    private String locationName;
}
