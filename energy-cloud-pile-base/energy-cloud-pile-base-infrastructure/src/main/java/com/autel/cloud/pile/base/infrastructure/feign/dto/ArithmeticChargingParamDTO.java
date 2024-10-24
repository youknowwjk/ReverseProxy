package com.autel.cloud.pile.base.infrastructure.feign.dto;

import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingParam;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @author Zoe Liu
 * @data 2022/3/14
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArithmeticChargingParamDTO implements Serializable {

    private String ModuleFun;

    private ArithmeticChargingParam params;

}
