package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.CurrencyDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.autel.cloud.tariff.vo.CostModelRuleEntityVO;
import com.autel.cloud.tariff.vo.CurrentMomentFeeRulesVO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(name = "tariff-app")
public interface TariffAPPFeign {

    @PostMapping("/costModelRule/queryList")
    Result<List<CostModelRuleDTO>> queryList(@Validated @RequestBody CostModelRuleDTO costModelRuleDTO);

    @PostMapping("/costModelRule/queryDetail")
    @ApiOperation(value = "查询规则详情")
    Result<CostModelRuleDTO> queryDetail(@Validated @RequestBody CostModelRuleDTO costModelRuleDTO);

    @PostMapping("/costModelRule/queryListByIds")
    @ApiOperation(value = "根据ID集合查询计费信息--临时加上")
    Result<List<CostModelRuleDTO>> queryListByIds(@Validated @RequestBody CostModelRuleDTO costModelRuleDTO);

    @PostMapping("/costModelRule/queryTariffByIds")
    @ApiOperation(value = "查詢计费规则通過id")
    Result<List<CostModelRuleDTO>> queryTariffByIds(@Validated @RequestBody List<Long> ruleIds);

    @PostMapping(value = "/currency/manager/detail")
    @ApiOperation(value = "货币详情")
    Result<CurrencyDTO> getCurrencyDetail(@RequestBody CurrencyDTO currencyDTO);

    @OperationActionLog(action = "query", object = "costRule")
    @PostMapping("/costModelRule/getCostModelRuleDetail")
    @ApiOperation(value = "查询规则详情")
    Result<CostModelRuleEntityVO> getCostModelRuleDetail(@RequestParam("id") Long id);

    /**
     * @param tariffRuleOfPileDTO 查询当前时刻的计费规则 入参模型
     * @return 返回当前时刻的计费规则
     * @function 查询当前时刻的计费规则
     */
    @PostMapping(value = "/costModelRule/getCurrentMomentFeeRules")
    @ApiOperation(value = "返回当前时刻的计费规则", notes = "返回当前时刻的计费规则")
    Result<CurrentMomentFeeRulesVO> getCurrentMomentFeeRules(@RequestBody TariffRuleOfPileDTO tariffRuleOfPileDTO);

    @PostMapping("/costModelRule/getCurrencyListBySellerId")
    @ApiOperation(value = "查询商户所有货币")
    Result<List<CurrencyDTO>> getCurrencyListBySellerId(@RequestParam(value = "sellerId") Long sellerId);
}
