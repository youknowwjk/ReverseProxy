package com.autel.cloud.pile.base.domain.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @Author MingLong A22599
 * @Date 2022/11/19
 * @Function 时长费费用模板
 */
@Data
@ApiModel
public class TimePriceCostRulesDTO extends BaseCostRulesDTO implements Serializable {

    @ApiModelProperty(value = "时长费(分钟)")
    private BigDecimal timePrice = BigDecimal.ZERO;

}
