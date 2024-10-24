package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotEmpty;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ChangePileAbilityDto {

    @NotEmpty(message = "sn cannot be empty")
    @ApiModelProperty(value = "充电桩序列号")
    private  String  sn;

    @NotEmpty(message = "connectortId cannot be empty")
    @ApiModelProperty(value = "枪号")
    private  String  connectorId;

    @NotEmpty(message = "type cannot be empty")
    @ApiModelProperty(value = "禁用 Inoperative  解禁  Operative")
    private  String  type;

}
