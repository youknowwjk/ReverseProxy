package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author A19061
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ApiModel(value = "FirmwareVersionDTO", description = "固件信息")
public class FirmwareVersionDTO {
    @ApiModelProperty(value = "固件编号")
    private String firmwareId;

    @ApiModelProperty(value = "固件版本")
    private String firmwareVersion;
}
