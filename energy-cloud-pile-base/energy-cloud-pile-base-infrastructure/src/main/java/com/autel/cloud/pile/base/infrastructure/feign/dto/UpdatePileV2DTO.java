package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author Zoe Liu
 * @data 2022/3/14
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UpdatePileV2DTO {

    @ApiModelProperty("sn")
    private String sn;

    @ApiModelProperty("充电枪名称")
    private String name;

    @ApiModelProperty("充电枪列表")
    List<ConnectorDTO> connectorList;
}
