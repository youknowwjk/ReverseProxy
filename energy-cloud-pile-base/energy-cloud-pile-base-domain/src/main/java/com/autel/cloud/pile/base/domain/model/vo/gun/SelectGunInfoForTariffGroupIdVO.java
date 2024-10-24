package com.autel.cloud.pile.base.domain.model.vo.gun;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

@Data
@ApiModel("为计费规则组查询枪信息 出参模型")
public class SelectGunInfoForTariffGroupIdVO implements Serializable {

    @ApiModelProperty("枪id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long gunId;

    @ApiModelProperty(value = "桩名称")
    private String pileName;

    @ApiModelProperty(value = "桩SN")
    private String pileSn;

    @ApiModelProperty(value = "枪号")
    private Integer connectorId;

    @ApiModelProperty("所属场站id")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long locationId;

    @ApiModelProperty("所属场站名称")
    private String locationName;

    @ApiModelProperty("是否已被当前计费规则组所绑定")
    private Boolean haveBind;

    @ApiModelProperty("是否有权限")
    private Boolean havePermission;
}
