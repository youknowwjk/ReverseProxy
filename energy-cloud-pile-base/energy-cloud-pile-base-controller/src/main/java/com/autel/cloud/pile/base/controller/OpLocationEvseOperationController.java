package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseOperationService;
import com.autel.cloud.pile.base.dto.OpEvseChargingThresholdDTO;
import com.autel.cloud.pile.base.dto.OpEvseSocThresholdDTO;
import com.autel.cloud.pile.base.vo.OpEvseEnableVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @ClassName OpLocationEvseOperationController
 * @Author A22121
 * @Description
 * @Date 2022/4/27 15:50
 * @Version 0.0.1-SNAPSHOT
 */
@Api(tags = "充电设备操作")
@RestController
@Validated
@RequestMapping("/opLocationEvseOperation")
public class OpLocationEvseOperationController {


    private final OpLocationEvseOperationService opLocationEvseOperationService;

    public OpLocationEvseOperationController(OpLocationEvseOperationService opLocationEvseOperationService) {
        this.opLocationEvseOperationService = opLocationEvseOperationService;
    }

    @Log(title = "版本更新(充电设备操作)", businessType = BusinessType.UPDATE, code = "50201431")
    @PostMapping("/updateVersion")
    @ApiOperation(value = "版本更新", notes = "版本更新")
    public Result<Boolean> updateVersion(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.updateVersion(evseSn);
    }

    @Log(title = "批量版本更新(充电设备操作)", businessType = BusinessType.UPDATE, code = "50201432")
    @PostMapping("/batchUpdateVersion")
    @ApiOperation(value = "批量版本更新", notes = "批量版本更新")
    public Result<Boolean> batchUpdateVersion(@RequestBody List<String> evseSnList) {
        return opLocationEvseOperationService.batchUpdateVersion(evseSnList);
    }

    @PostMapping("/backendStop")
    @ApiOperation(value = "远程停止充电", notes = "远程停止充电")
    public Result<Boolean> backendStop(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.backendStop(evseSn);
    }

    @PostMapping("/remoteStop")
    @ApiOperation(value = "远程停止充电", notes = "远程停止充电")
    public Result<Boolean> remoteStop(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.backendStop(evseSn);
    }

    @PostMapping("/disable")
    @ApiOperation(value = "禁用充电", notes = "禁用充电")
    public Result<Boolean> disable(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.disable(evseSn);
    }

    @PostMapping("/able")
    @ApiOperation(value = "解禁充电", notes = "解禁充电")
    public Result<Boolean> able(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.able(evseSn);
    }

    @PostMapping("/reset")
    @ApiOperation(value = "枪重启", notes = "枪重启")
    public Result<Boolean> reset(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.reset(evseSn);
    }

    @PostMapping("/unlock")
    @ApiOperation(value = "枪解锁", notes = "枪解锁")
    public Result<Boolean> unlock(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.unlock(evseSn);
    }

    @PostMapping("/getChargingThreshold")
    @ApiOperation(value = "获取告警阈值设置", notes = "获取告警阈值设置")
    public Result<OpEvseChargingThresholdDTO> getChargingThreshold(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.getChargingThreshold(evseSn);
    }

    @PostMapping("/setChargingThreshold")
    @ApiOperation(value = "设置告警阈值", notes = "设置告警阈值")
    public Result<Boolean> setChargingThreshold(@RequestBody @Valid OpEvseChargingThresholdDTO opEvseChargingThresholdDTO) {
        return opLocationEvseOperationService.setChargingThreshold(opEvseChargingThresholdDTO);
    }

    @PostMapping("/getSocThreshold")
    @ApiOperation(value = "获取SOC设置", notes = "获取SOC设置")
    public Result<OpEvseSocThresholdDTO> getSocThreshold(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseOperationService.getSocThreshold(evseSn);
    }

    @PostMapping("/setSocThreshold")
    @ApiOperation(value = "设置SOC", notes = "设置SOC")
    public Result<Boolean> setSocThreshold(@RequestBody @Valid OpEvseSocThresholdDTO opEvseSocThresholdDTO) {
        return opLocationEvseOperationService.setSocThreshold(opEvseSocThresholdDTO);
    }

    @GetMapping("/statusList")
    @ApiOperation(value = "桩详情-配置-其他信息-启用禁用列表查询", notes = "X22028")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "桩的sn", name = "pileSn", example = "A123131", paramType = "query")
    })
    public Result<List<OpEvseEnableVO>> getStatusList(@RequestParam("pileSn") String pileSn) {
        return opLocationEvseOperationService.getStatusList(pileSn);
    }
}
