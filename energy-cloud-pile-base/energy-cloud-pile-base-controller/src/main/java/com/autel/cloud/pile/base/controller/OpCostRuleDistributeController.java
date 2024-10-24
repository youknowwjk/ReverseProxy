package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpCostRuleDistributeService;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录 控制逻辑层
 */
@Api(value = "计费规则下发记录", tags = "计费规则下发记录")
@RequestMapping(value = "/opCostRuleDistribute")
@RestController
@Slf4j
@Validated
public class OpCostRuleDistributeController {

    @Autowired
    private OpCostRuleDistributeService opCostRuleDistributeService;

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发新增
     */
    @PostMapping
    @ApiOperation(value = "计费规则下发新增")
    public Result<Boolean> addOpCostRuleDistribute(@RequestBody OpCostRuleDistributeDTO opCostRuleDistributeDTO) {
        log.info("===========>>>>>> addOpCostRuleDistribute in the OpCostRuleDistributeController function : {}", JSON.toJSONString(opCostRuleDistributeDTO));
        return opCostRuleDistributeService.addOpCostRuleDistribute(opCostRuleDistributeDTO);
    }

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN码是否存在，批量新增或者修改计费规则下发记录
     */
    @PutMapping
    @ApiOperation(value = "根据桩SN码是否存在，批量新增或者修改计费规则下发记录")
    public Result<Boolean> addOrUpdateOpCostRuleDistributeByPileSn(@RequestBody List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList) {
        log.info("===========>>>>>>> addOrUpdateOpCostRuleDistributeByPileSn in the OpCostRuleDistributeController function : {}", JSON.toJSONString(opCostRuleDistributeDTOList));
        return opCostRuleDistributeService.addOrUpdateOpCostRuleDistributeByPileSn(opCostRuleDistributeDTOList);
    }

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发查询
     */
    @GetMapping
    @ApiOperation(value = "计费规则下发查询")
    public Result<List<OpCostRuleDistributeDTO>> selectOpCostRuleDistributeList(@RequestBody OpCostRuleDistributeDTO opCostRuleDistributeDTO) {
        log.info("===========>>>>>>> selectOpCostRuleDistributeList in the OpCostRuleDistributeController function : {}", JSON.toJSONString(opCostRuleDistributeDTO));
        return opCostRuleDistributeService.selectOpCostRuleDistributeList(opCostRuleDistributeDTO);
    }

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN动态地批量修改桩的下发状态
     */
    @PostMapping("/updateOpCostRuleDistributeByPileSn")
    @ApiOperation(value = "根据桩SN动态地批量修改桩的下发状态")
    public Result<Boolean> updateOpCostRuleDistributeByPileSn(@RequestBody List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList) {
        log.info("=================>>>>>>>>>>> updateOpCostRuleDistributeByPileSn in the OpCostRuleDistributeController function : {}", JSON.toJSONString(opCostRuleDistributeDTOList));
        return opCostRuleDistributeService.updateOpCostRuleDistributeByPileSn(opCostRuleDistributeDTOList);
    }

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN动态地批量修改桩的生效状态
     */
    @PostMapping("/updateOpCostRuleEfficientByPileSn")
    @ApiOperation(value = "根据桩SN动态地批量修改桩的生效状态")
    public Result<Boolean> updateOpCostRuleEfficientByPileSn(@RequestBody List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList) {
        log.info("=================>>>>>>>>>>> updateOpCostRuleEfficientByPileSn in the OpCostRuleDistributeController function : {}", JSON.toJSONString(opCostRuleDistributeDTOList));
        return opCostRuleDistributeService.updateOpCostRuleEfficientByPileSn(opCostRuleDistributeDTOList);
    }

    /**
     * @param pileSn
     * @return
     * @function 通过桩Sn重新下发计费规则
     */
    @GetMapping("/reIssueBillingRulesByPileSn")
    @ApiOperation(value = "通过桩Sn重新下发计费规则", notes = "通过桩Sn重新下发计费规则")
    Result<Boolean> reIssueBillingRulesByPileSn(@RequestParam("pileSn") String pileSn) {
        log.info("=================>>>>>>>>>>> reIssueBillingRulesByPileSn in the OpCostRuleDistributeController function : {}", JSON.toJSONString(pileSn));
        return opCostRuleDistributeService.reIssueBillingRulesByPileSn(pileSn);
    }

    /**
     * @param tariffId
     * @return
     * @function 通过计费规则ID重新下发计费规则
     */
    @GetMapping("/reIssueBillingRulesByTariffId")
    @ApiOperation(value = "通过计费规则ID重新下发计费规则", notes = "通过计费规则ID重新下发计费规则")
    Result<Boolean> reIssueBillingRulesByTariffId(@RequestParam("tariffId") Long tariffId) {
        log.info("=================>>>>>>>>>>> reIssueBillingRulesByTariffId in the OpCostRuleDistributeController function : {}", JSON.toJSONString(tariffId));
        return opCostRuleDistributeService.reIssueBillingRulesByTariffId(tariffId);
    }

    @PostMapping(value = "/retry")
    @ApiOperation(value = "重试下发计费规则", notes = "重试下发计费规则")
    Result<Boolean> retryDistributeTariff(@RequestParam("pileSn") String pileSn) {
        log.info("=================>>>>>>>>>>> retryDistributeTariff in the OpCostRuleDistributeController function : {}", JSON.toJSONString(pileSn));
        return opCostRuleDistributeService.retryDistributeTariff(pileSn);
    }
}
