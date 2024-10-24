package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @author A22208
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UpdatePileLocationDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("场站id")
    private String locationId;

    @ApiModelProperty("商家id")
    private String merchantId;

    /**
     * 充电桩所在国家
     */
    @ApiModelProperty("充电桩所在国家")
    private String country;

    /**
     * 充电桩所在州
     */
    @ApiModelProperty("充电桩所在州")
    private String state;

    /**
     * 充电桩所在城市
     */
    @ApiModelProperty("充电桩所在城市")
    private String city;

    /**
     * 邮政编码
     */
    @ApiModelProperty("邮政编码")
    private String zipCode;

    /**
     * 地址
     */
    @ApiModelProperty("地址")
    private String address;

    /**
     * 经度
     */
    @ApiModelProperty("经度")
    private String longitude;

    /**
     * 纬度
     */
    @ApiModelProperty("纬度")
    private String latitude;

}
