package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * <p>Describe Class...</p>
 *
 * @author A19284
 * @since 2021/12/4 10:38
 */
@Data
public class TariffVO implements Serializable {
    private static final long serialVersionUID = 6239179628087263899L;

    @ApiModelProperty("产品(桩)ID")
    private Long id;

    @ApiModelProperty("计费规则详情")
    private String tariff;


}
