package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.infrastructure.feign.dto.ConnectorDTO;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Data
@Accessors(chain = true)
public class AutelPileDTO {

    /**
     * 桩类型 0：交流 1：直流 2 交直流
     */
    @ApiModelProperty("桩类型 1：交流 2：直流 3 交直流")
    private Integer category;

    /**
     * 额定功率
     */
    @ApiModelProperty("额定输出功率")
    private Double ratedOutputPower;

    /**
     * 供电相数
     */
    @ApiModelProperty("供电相数")
    private Integer phases;



    /**
     * 产品型号
     */
    private String partProduct;

    /**
     * 产品系列
     */
    private String productNamePdm;

    /**
     * 供应商
     */
    @ApiModelProperty("供应商")
    private String vendor;


    @ApiModelProperty("枪数量")
    private Integer connectorNum;

    @ApiModelProperty("枪类型")
    private Integer connectorType;

    @ApiModelProperty("枪类型列表")
    private List<ConnectorDTO> connectorList;

}
