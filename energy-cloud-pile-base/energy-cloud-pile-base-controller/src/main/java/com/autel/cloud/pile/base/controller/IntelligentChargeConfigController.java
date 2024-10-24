package com.autel.cloud.pile.base.controller;


import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.SmartChargeProfile;
import com.autel.cloud.pile.base.VehicleProfile;
import com.autel.cloud.pile.base.domain.service.MemberSmartChargeConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.SmartChargeConfigQueryDTO;
import com.autel.cloud.pile.base.dto.SmartChargeProfileDTO;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.VehicleServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.MemberSmartChargeConfigEntity;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

@RestController
@RequestMapping(value = "/smartChargeConfig")
@Api(tags = "公共智能充电配置")
@Slf4j
@Validated
public class IntelligentChargeConfigController {

    @Resource
    private MemberSmartChargeConfigService memberSmartChargeConfigService;

    @Resource
    private VehicleServiceAdapter vehicleServiceAdapter;

    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @GetMapping("/queryEnabledSmartChargeConfigByUserId")
    @ApiOperation(value = "查询用户开启了智能充电的配置", notes = "查询用户开启了智能充电的配置")
    public Result<Boolean> queryEnabledSmartChargeConfigByUserId(@RequestParam("userId") Long userId) {
        log.info("queryEnabledSmartChargeConfigByUserId {}", userId);
        int i = memberSmartChargeConfigService.queryEnabledSmartChargeConfigByUserId(userId);
        log.info("isEnabledSmartChargePile result {} : {}", i, userId);
        return Result.ofSucceed(i > 0);
    }

    @GetMapping("/disabledSmartChargeConfigByUserId")
    @ApiOperation(value = "禁用用户开启了智能充电的配置", notes = "禁用用户开启了智能充电的配置")
    public Result<Boolean> disabledSmartChargeConfigByUserId(@RequestParam("userId") Long userId) {
        log.info("disabledSmartChargeConfigByUserId {}", userId);
        int i = memberSmartChargeConfigService.disabledSmartChargeConfigByUserId(userId);
        log.info("disabledConfigByUserId result {} : {}", i, userId);
        return Result.ofSucceed(i > 0);
    }

    @GetMapping("/isEnabledSmartChargePile")
    @ApiOperation(value = "查询桩是否在能启用公共职能充电", notes = "查询桩是否在能启用公共职能充电")
    public Result<Boolean> isEnabledSmartChargePile(@RequestParam("pileSn") String pileSn) {
        log.info("isEnabledSmartChargePile {}", pileSn);
        boolean enableSmartChargePile = opLocationPileGroupService.isEnableSmartChargePile(pileSn);
        log.info("isEnabledSmartChargePile result {} : {}", pileSn, enableSmartChargePile);
        return Result.ofSucceed(enableSmartChargePile);
    }

    @GetMapping("/listVehicles")
    @ApiOperation(value = "获取用户在enode系统上的车辆信息", notes = "获取用户在enode系统上的车辆信息")
    public Result<JSONArray> listVehicles(@RequestParam("userId") long userId) {
        log.info("listVehicles");
        JSONArray jsonArray = vehicleServiceAdapter.getByUserId(userId);
        log.info("listVehicles result : {}", jsonArray);
        return Result.ofSucceed(jsonArray);
    }


    @PostMapping("/profile")
    @ApiOperation(value = "获取用户在该场站的智能充电配置", notes = "获取用户车辆的智能充电配置")
    public Result<SmartChargeProfile> profile(@RequestBody SmartChargeConfigQueryDTO smartChargeConfigQueryDTO) {
        log.info("LoginUserHolder.getLoginUser(): {}", LoginUserHolder.getLoginUser());
        smartChargeConfigQueryDTO.setUserId(LoginUserHolder.getLoginUser().getId());
        log.info("smartChargeConfigQueryDTO: {}", JSON.toJSONString(smartChargeConfigQueryDTO));
        MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = memberSmartChargeConfigService.queryByUserIdAndLocationId(smartChargeConfigQueryDTO);
        log.info("getProfile detail: {}", JSON.toJSONString(memberSmartChargeConfigEntity));
        return Result.ofSucceed(memberSmartChargeConfigEntity.getProfile());
    }

    @PostMapping("/update")
    @ApiOperation(value = "更新公共智能充电配置", notes = "更新公共智能充电配置")
    public Result<SmartChargeProfile> update(@RequestBody SmartChargeProfileDTO profile) {
        log.info("IntelligentChargeProfile update: {}", JSON.toJSONString(profile));
        MemberSmartChargeConfigEntity data = memberSmartChargeConfigService.updateById(profile);
        log.info("update IntelligentChargeConfig data: {}", JSON.toJSONString(data));
        return Result.ofSucceed(data.getProfile());
    }


    @GetMapping("/getSmartChargeProfile")
    @ApiOperation(value = "查询智能充电配置", notes = "查询公共智能充电配置")
    public Result<SmartChargeProfile> getVehicleProfile(@RequestParam("id") long id) {
        log.info("getVehicleProfile id: {}", id);
        SmartChargeProfile profile = memberSmartChargeConfigService.selectById(id).getProfile();
        log.info("getVehicleProfile: {}", JSON.toJSONString(profile));
        return Result.ofSucceed(profile);
    }

    @GetMapping("/getUserSmartChargeProfiles")
    @ApiOperation(value = "根据用户ID查询智能充电配置", notes = "查询公共智能充电配置")
    public Result<List<MemberSmartChargeConfigEntity>> getUserVehicleProfile(@RequestParam("userId") long userId) {
        log.info("getUserVehicleProfile userId: {}", userId);
        List<MemberSmartChargeConfigEntity> list = memberSmartChargeConfigService.getUserVehicleProfile(userId);
        log.info("getUserVehicleProfile: {}", JSON.toJSONString(list));
        return Result.ofSucceed(list);
    }
}
