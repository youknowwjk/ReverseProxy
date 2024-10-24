package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.oicp.*;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @Author: X22010
 * @Date: 2022/08/22/17:31
 * @Description:
 */
@FeignClient(value = "oicp-app")
public interface OicpFeignClient {

    @PostMapping("/evse/v2/cpo/push/evse")
    @ApiOperation(value = "cpo 上报充电桩数据")
    Result<Boolean> pushEvseData(@RequestBody EroamingEvseData eroamingEvseData, @RequestParam("action") Integer action);

    @PostMapping("/evse/query/evse-data")
    @ApiOperation(value = "云端查询充电桩数据")
    Result<List<PullEvseDataRespDTO>> queryEvseData(@RequestBody PullEvseStatusDTO pullEvseStatue);

    @GetMapping("/evse/query/charging-data/{chargingId}")
    @ApiOperation(value = "云端查询充电场站数据")
    Result<List<PullEvseDataRespDTO>> queryChargingPointData(@PathVariable("chargingId") Long chargingId);

    /**
     * action: 0-fullLoad, 1-update, 2-insert, 3-delete
     *
     * @param msgDto msgDto
     * @param action action
     * @return
     */
    @PostMapping("/dynamicpricing/cpo/push/tariff/{action}")
    @ApiOperation(value = "cpo 绑定设备与价格")
    Result<Boolean> cpoPushPricingProductData(@RequestBody List<TariffEvse> msgDto, @PathVariable("action") Integer action);

    @GetMapping("/hubject/config")
    @ApiOperation(value = "是否导航到灰度服务")
    Result<Boolean> checkUserIsGray();

}
