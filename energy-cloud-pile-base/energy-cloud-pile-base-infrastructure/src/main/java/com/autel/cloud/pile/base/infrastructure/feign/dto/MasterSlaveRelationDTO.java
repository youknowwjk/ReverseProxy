package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.Valid;
import javax.validation.constraints.NotEmpty;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class MasterSlaveRelationDTO {
    @NotEmpty
    @Valid
    @ApiModelProperty(value = "主充电桩序列号")
    private String mainSn;

    @ApiModelProperty(value = "从充电桩序列号")
    private String sn;

    @ApiModelProperty(value = "是否启用ALM")
    private Integer almEnabled = 0;

    @ApiModelProperty(value = "桩数量")
    private Integer pileNum = 0;
}
