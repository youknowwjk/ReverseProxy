package com.autel.cloud.pile.base.domain.model;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

@Data
@ApiModel(value = "为POS机查询设备信息 入参模型")
public class SelectDeviceInfoForPosDTO implements Serializable {

    @ApiModelProperty(value = "商户id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long sellerId;

    @ApiModelProperty(value = "场站id集合")
    private List<Long> locationIdList;

    @ApiModelProperty(value = "搜索")
    private String searchValue;
}
