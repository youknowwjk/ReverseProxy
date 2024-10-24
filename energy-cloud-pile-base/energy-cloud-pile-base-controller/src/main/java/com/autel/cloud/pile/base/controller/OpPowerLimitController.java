package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpPowerLimitService;
import com.autel.cloud.pile.base.dto.OpPowerLimitDTO;
import com.autel.cloud.pile.base.dto.OpPowerLimitSettingDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @ClassName OpPowerLimitController
 * @Author A22121
 * @Description
 * @Date 2022/7/2 10:58
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@RequestMapping("/powerLimit")
@Api(value = "OpPowerLimitController", tags = "功率限定")
@Validated
public class OpPowerLimitController {

    @Autowired
    private OpPowerLimitService opPowerLimitService;

    @GetMapping("/updateTurnOn")
    @ApiOperation(value = "更新组合是否开启功率限制")
    public Result<Boolean> updateTurnOn(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.updateTurnOn(combinationId);
    }

    @GetMapping("/getTurnOn")
    @ApiOperation(value = "查询组合是否开启功率限制")
    public Result<Boolean> getTurnOn(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.getTurnOn(combinationId);
    }

    @PostMapping("/updatePowerLimit")
    @ApiOperation(value = "更新设置方式")
    public Result<Boolean> updatePowerLimit(@RequestBody OpPowerLimitDTO opPowerLimitDTO) {
        return opPowerLimitService.updatePowerLimit(opPowerLimitDTO);
    }

    @GetMapping("/queryPowerLimit")
    @ApiOperation(value = "查询设置方式")
    public Result<OpPowerLimitDTO> queryPowerLimit(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.queryPowerLimit(combinationId);
    }

    @PostMapping("/addTimeSetting")
    @ApiOperation(value = "添加分时设置功率")
    public Result<Boolean> addTimeSetting(@RequestBody List<OpPowerLimitSettingDTO> opPowerLimitSettingDTOList) {
        return opPowerLimitService.addTimeSetting(opPowerLimitSettingDTOList);
    }

    @GetMapping("/queryTimeSettingForUpdate")
    @ApiOperation(value = "更新展示分时设置功率")
    public Result<List<OpPowerLimitSettingDTO>> queryTimeSettingForUpdate(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.queryTimeSettingForUpdate(combinationId);
    }

    @GetMapping("/queryTimeSetting")
    @ApiOperation(value = "展示分时设置功率")
    public Result<List<OpPowerLimitSettingDTO>> queryTimeSetting(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.queryTimeSetting(combinationId);
    }

    @GetMapping("/queryTimeSettingNoDataAuth")
    @ApiOperation(value = "展示分时设置功率(无数据权限控制，仅供服务端之间调用，不可暴露给终端)")
    public Result<List<OpPowerLimitSettingDTO>> queryTimeSettingNoDataAuth(@RequestParam("combinationId") Long combinationId) {
        return this.queryTimeSetting(combinationId);
    }

    @GetMapping("/deliverySetting")
    @ApiOperation(value = "下发配置")
    public Result<Boolean> deliverySetting(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.deliverySetting(combinationId);
    }

    @GetMapping("/clearSetting")
    @ApiOperation(value = "清除配置")
    public Result<Boolean> clearSetting(@RequestParam("combinationId") Long combinationId) {
        return opPowerLimitService.clearSetting(combinationId);
    }
}
