package com.autel.cloud.pile.base.infrastructure.elastic.entity;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@ApiModel
@AllArgsConstructor
@NoArgsConstructor
public class OpLocationSavePileEsDTO {

    @ApiModelProperty("op_location Es对象")
    private List<OpLocationElasticDTO> opLocationElasticDTO;

    @ApiModelProperty("op_location_evse_expand Es对象")
    private List<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTO;
}
