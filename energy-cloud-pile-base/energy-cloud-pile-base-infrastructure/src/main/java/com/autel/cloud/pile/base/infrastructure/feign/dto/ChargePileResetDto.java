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
public class ChargePileResetDto {

    @NotEmpty(message = "sn cannot be  empty")
    @ApiModelProperty(value = "充电桩序列号")
    private  String  sn;

    @NotEmpty(message = "sn cannot be  empty")
    @ApiModelProperty(value = "复位类型  Hard  Soft")
    private String  type;

}
