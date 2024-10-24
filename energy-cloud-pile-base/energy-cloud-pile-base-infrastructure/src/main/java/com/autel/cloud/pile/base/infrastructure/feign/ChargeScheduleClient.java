package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingParam;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingResult;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ArithmeticChargingParamDTO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

/**
 * @Author temp
 * @Date 2023/4/25 11:27
 */
@FeignClient(value = "charge-schedule", contextId = "ChargeScheduleClient")
public interface ChargeScheduleClient {

//    @PostMapping("/api/arithmetic")
//    Result<List<OpLocationEvsePlanForEmsVO>> arithmetic(@RequestBody ChargeScheduleDTO paramDTO);

    @PostMapping("/api/arithmetic")
    @ApiOperation(value = "获取充电计划", notes = "获取充电计划")
    Result<ArithmeticChargingResult>  queryArithmeticCharging(@RequestBody ArithmeticChargingParamDTO DTO);
}
