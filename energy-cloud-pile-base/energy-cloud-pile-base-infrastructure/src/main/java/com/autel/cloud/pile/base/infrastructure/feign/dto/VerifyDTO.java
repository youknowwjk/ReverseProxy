package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;

/**
 * <p>Describe Class...</p>
 *
 * @author A19284
 * @since 2021/12/16 15:11
 */
@Data
@Accessors(chain = true)
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(value = "VerifyDTO", description = "充电桩验证信息")
public class VerifyDTO {
    @ApiModelProperty(value = "充电桩序列号")
    @NotBlank
    private String pileSn;

    @ApiModelProperty(value = "充电桩pin码")
    @NotBlank
    private String pin;
}
