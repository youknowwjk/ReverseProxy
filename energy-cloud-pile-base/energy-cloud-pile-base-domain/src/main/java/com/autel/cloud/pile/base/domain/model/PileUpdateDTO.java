package com.autel.cloud.pile.base.domain.model;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;

@Data
@ApiModel
public class PileUpdateDTO {


    /**
     * 桩名称
     */
    @ApiModelProperty("桩名称")
    private String name;


    /**
     * sn编号
     */
    @ApiModelProperty("桩sn编号")
    private String sn;


    /**
     * AC  DC
     */
    @ApiModelProperty("AC  DC ")
    private String powerType;

    /**
     * 额定功率
     */
    @ApiModelProperty("额定功率")
    private BigDecimal ratedPower;

}
