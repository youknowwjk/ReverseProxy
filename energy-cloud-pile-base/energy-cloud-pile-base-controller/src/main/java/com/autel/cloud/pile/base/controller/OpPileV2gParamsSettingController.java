package com.autel.cloud.pile.base.controller;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpPileV2gParamsSettingService;
import com.autel.cloud.pile.base.dto.OpPileV2gParamsSettingDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * <p>
 * 桩V2G参数设置 前端控制器
 * </p>
 *
 * @author A22121
 * @since 2022-10-17
 */
@Api(tags = "v2g接口控制器")
@RestController
@RequestMapping("/v2g")
@Validated
public class OpPileV2gParamsSettingController {

    private final OpPileV2gParamsSettingService opPileV2gParamsSettingService;

    public OpPileV2gParamsSettingController(OpPileV2gParamsSettingService opPileV2gParamsSettingService) {
        this.opPileV2gParamsSettingService = opPileV2gParamsSettingService;
    }

    @PostMapping("/addSetting")
    @ApiOperation(value = "添加配置")
    public Result<Boolean> addSetting(@RequestBody OpPileV2gParamsSettingDTO opPileV2gParamsSettingDTO) {
        return opPileV2gParamsSettingService.addSetting(opPileV2gParamsSettingDTO);
    }

    @GetMapping("/deletedSetting")
    @ApiOperation(value = "删除配置")
    public Result<Boolean> deletedSetting(@RequestParam("pileSn") String pileSn) {
        return opPileV2gParamsSettingService.deletedSetting(pileSn);
    }

    @GetMapping("/getSetting")
    @ApiOperation(value = "获取配置")
    public Result<OpPileV2gParamsSettingDTO> getSetting(@RequestParam("pileSn") String pileSn) {
        return opPileV2gParamsSettingService.getSetting(pileSn);
    }

    @GetMapping("/convertMode")
    @ApiOperation(value = "切换充放电")
    public Result<Boolean> convertMode(@RequestParam("pileSn") String pileSn) {
        return opPileV2gParamsSettingService.convertMode(pileSn);
    }

    @GetMapping("/getChargeMode")
    @ApiOperation(value = "获取桩充放电模式")
    public Result<Integer> getChargeMode(@RequestParam("pileSn") String pileSn) {
        return opPileV2gParamsSettingService.getChargeMode(pileSn);
    }

    @GetMapping("/setChargeMode")
    @ApiOperation(value = "更新桩充放电模式")
    public Result<Boolean> setChargeMode(@RequestParam("pileSn") String pileSn, @RequestParam("chargeFlag") Integer chargeFlag) {
        return opPileV2gParamsSettingService.setChargeMode(pileSn, chargeFlag);
    }

}

