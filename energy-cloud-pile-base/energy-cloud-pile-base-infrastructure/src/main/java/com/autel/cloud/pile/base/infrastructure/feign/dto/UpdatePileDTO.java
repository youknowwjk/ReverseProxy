package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author Zoe Liu
 * @data 2022/3/14
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UpdatePileDTO {

    @ApiModelProperty("SN码")
    private String sn;

    @ApiModelProperty("商标")
    private String brand;

    @ApiModelProperty("枪类型id")
    private String gunTypeId;

    @ApiModelProperty("地址")
    private String address;

    @ApiModelProperty("经度")
    private String longitude;

    @ApiModelProperty("纬度")
    private String latitude;
}
