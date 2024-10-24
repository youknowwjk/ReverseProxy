package com.autel.cloud.pile.base.infrastructure.feign;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.PileTimezoneDTO;
import com.autel.cloud.pile.base.dto.lockOrUnlockGun.LockOrUnlockGunDTO;
import com.autel.cloud.pile.base.dto.tariff.CostRuleBatchDispatchHomePileDTO;
import com.autel.cloud.pile.base.dto.tariff.CostRuleDispatchPileSnDTO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * Author:   A19011
 * Description: ProtocalServiceClient
 * Date:     2021/11/29 17:18
 *
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient(value = "protocol-service")
public interface ProtocolFeignClient {
    @GetMapping("/dispatch/batchClearOcppConfig")
    @ApiOperation(value = "批量清除配置命令下发")
    Result<List<String>> batchClearOcppConfig(@RequestBody Collection<String> sns);

    @GetMapping("/dispatch/clearOcppConfig")
    @ApiOperation(value = "清除配置命令下发")
    Result<Boolean> clearOcppConfig(@RequestParam("sn") String sn);

    @PostMapping("/costRule/dispatch")
    @ApiOperation(value = "计费规则下发")
    Result<Boolean> costRuleDispatch(@RequestBody CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO);

    @PostMapping("/costRule/dispatch/batch")
    @ApiOperation(value = "计费规则批量下发")
    Result<Boolean> costRuleBatchDispatch(@RequestBody List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList);

    @PostMapping(value = "/costRule/dispatch/batch/homePile")
    @ApiOperation(value = "家桩计费规则批量推送")
    Result<Boolean> dispatchHomePileTariff(@RequestBody List<CostRuleBatchDispatchHomePileDTO> costRuleBatchDispatchHomePileDTOList);

    @PostMapping(value = "/costRule/dispatch/batch/homePile/v2")
    @ApiOperation(value = "家桩计费规则批量推送(根据计费规则ID)")
    Result<Boolean> dispatchHomePileTariffV2(@RequestBody List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList);

    @ApiOperation(value = "时区下发")
    @PostMapping(value = "/timezone/dispatch")
    Result<Boolean> dispatchTimezone(@RequestBody PileTimezoneDTO pileTimezoneDTO);

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 下发充电枪锁枪或者不锁枪的命令
     */
    @PostMapping("/pileLockOrUnlockGun/setCableEnable")
    @ApiOperation(value = "下发充电枪锁枪或者不锁枪的命令", notes = "下发充电枪锁枪或者不锁枪的命令")
    Result<Boolean> setCableEnable(@RequestBody LockOrUnlockGunDTO lockOrUnlockGunDTO);

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 发送获取当前充电枪状态的命令（查看当前充电枪是处于锁枪状态还是解锁枪状态）
     */
    @PostMapping("/pileLockOrUnlockGun/getCableEnable")
    @ApiOperation(value = "发送获取当前充电枪状态的命令（查看当前充电枪是处于锁枪状态还是解锁枪状态）", notes = "发送获取当前充电枪状态的命令（查看当前充电枪是处于锁枪状态还是解锁枪状态）")
    Result<Boolean> getCableEnable(@RequestBody LockOrUnlockGunDTO lockOrUnlockGunDTO);

    @PostMapping("/costRule/dispatch/batchClearBillingRule")
    @ApiOperation(value = "批量清除充电桩屏幕上的计费规则(针对于5寸充电桩发送软重启命令)")
    Result<Map<String, Boolean>> batchClearBillingRule(@RequestBody List<String> pileSnList);
}
