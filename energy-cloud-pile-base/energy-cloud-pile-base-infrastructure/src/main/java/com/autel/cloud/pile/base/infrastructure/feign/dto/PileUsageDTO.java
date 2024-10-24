package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * <p>Describe Class...</p>
 *
 * @author A19284
 * @since 2021/12/4 10:43
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel(value = "PileUsageDTO", description = "充电桩使用场景")
public class PileUsageDTO implements Serializable {
    private static final long serialVersionUID = 8176093825466043740L;

    @ApiModelProperty(value = "使用场景 0：无属性桩  1：商桩  2：家桩")
    private Integer usage;
}
