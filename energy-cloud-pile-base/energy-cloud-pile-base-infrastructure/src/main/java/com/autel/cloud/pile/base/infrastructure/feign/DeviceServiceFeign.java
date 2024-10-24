package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.device.vo.PileDeviceRelationVO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.chargepoint.GetDeviceTypeDTO;
import com.autel.cloud.pile.base.dto.pos.SetPosAuthorizedAmountDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.PileUsageDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.SyncPileInfoForPosDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.VerifyDTO;
import com.autel.cloud.pile.base.infrastructure.feign.vo.GunTypeAndNumVO;
import com.autel.cloud.pile.base.infrastructure.feign.vo.LockOrUnlockGunSupportInfoVO;
import com.autel.cloud.pile.base.vo.DispatchTariffPileInfoVO;
import com.autel.cloud.pile.base.vo.PileConfigurationInfoVO;
import com.autel.cloud.pile.base.vo.SimpleChargePileVO;
import com.autel.cloud.pile.base.vo.lockOrUnlockGun.PileDeviceEvseStatusVO;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * @ClassName DeviceServiceFeign
 * @Author A22327
 * @Description
 * @Date 2022/4/27 17:04
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient("device")
public interface DeviceServiceFeign {

    @GetMapping("/dt-charger-info/get-delivery")
    @ApiOperation(value = "获取充电桩发货时间")
    Result<String> getTimeOfDelivery(@RequestParam("sn") String sn);


    @GetMapping("/device/relation/info")
    @ApiOperation(value = "根据序列号查询设备关联关系信息")
    Result<PileDeviceRelationVO> getDeviceRelationInfo(@RequestParam String pileSn);

    @GetMapping("/dt-charger-unified-param/getCountryCode")
    @ApiOperation(value = "查询国家码")
    Result<String> getCountry(@RequestParam("sn") String sn);

    @PostMapping("/pile/snList")
    @ApiOperation(value = "根据一组序列号查询充电桩列表信息")
    Result<List<ChargePileDTO>> queryPileList(@RequestBody @ApiParam(value = "sn列表") List<String> snList);

    @GetMapping("/card/list")
    @ApiOperation(value = "获取卡的经销商编号")
    Result<List<CardInfoDTO>> getCardList(@RequestParam(value = "cardList") List<String> cardList);

    @PostMapping("/pile/verify")
    Result<Boolean> verifyPile(@RequestBody VerifyDTO verifyDTO);

    @GetMapping("/pile/detail")
    @ApiOperation(value = "根据序列号查询充电桩信息")
    Result<ChargePileDTO> pileDetail(@RequestParam(value = "sn") String sn);

    @PostMapping("/pile/modify/usage/{sn}")
    @ApiOperation(value = "根据序列号修改充电桩的使用场景")
    Result<Boolean> updateUsage(@RequestBody PileUsageDTO pileUsageDTO, @PathVariable("sn") String sn);

    @PostMapping("/pile/saveThirdPile")
    @ApiOperation(value = "保存第三方桩")
    Result<Boolean> saveThirdPile(@RequestBody OpLocationEvseDTO opLocationThirdEvseDTO);


    @PostMapping("/pile/batchSaveThirdPile")
    @ApiOperation(value = "批量保存第三方桩")
    Result<Boolean> batchSaveThirdPile(@RequestBody List<OpLocationEvseDTO> opLocationThirdEvseDTOs);

    /**
     * 获取设备类型： 家桩、商桩
     */
    @GetMapping("/pile/stand/info")
    Result<PileStandInfoDTO> queryStandInfo(@RequestParam(value = "sn") String sn);

    /**
     * @param pileSnList 设备序列号集合
     * @return 充电桩基础信息
     * @function 获得充电桩基础信息
     */
    @PostMapping("/pile/getSimpleChargePileInfoList")
    @ApiOperation(value = "获得充电桩基础信息", notes = "获得充电桩基础信息")
    Result<List<SimpleChargePileVO>> getSimpleChargePileInfoList(@RequestBody List<String> pileSnList);

    /**
     * 批量查询充电桩配置信息
     */
    @PostMapping("/pile/batch/configurationInformation")
    Result<List<PileConfigurationInfoVO>> configurationInformation(@RequestBody @NotNull List<String> snList);

    /**
     * @param pileSn 充电桩序列号
     * @return 该充电桩是否支持锁枪或者不锁枪功能的判断结果
     * @function 根据充电桩序列号判断该充电桩是否支持锁枪或者不锁枪功能
     */
    @ApiImplicitParam(name = "pileSn", value = "充电桩序列号", dataType = "String", paramType = "query", required = true)
    @GetMapping(value = "/pile/judgePileSupportLockOrUnlockGun")
    @ApiOperation(value = "根据充电桩序列号判断该充电桩是否支持锁枪或者不锁枪功能", notes = "根据充电桩序列号判断该充电桩是否支持锁枪或者不锁枪功能")
    Result<Map<String, Boolean>> judgePileSupportLockOrUnlockGun(@RequestParam("pileSn") String pileSn);

    /**
     * @param pileSn 充电桩序列号
     * @return 该充电桩是否支持锁枪或者不锁枪功能的判断结果
     * @function 根据充电桩序列号判断该充电桩是否支持锁枪或者不锁枪功能
     */
    @ApiImplicitParam(name = "pileSn", value = "充电桩序列号", dataType = "String", paramType = "query", required = true)
    @GetMapping(value = "/pile/getLockOrUnlockGunSupportInfo")
    @ApiOperation(value = "根据充电桩序列号查询该充电桩是否支持锁枪或者不锁枪信息", notes = "根据充电桩序列号查询该充电桩是否支持锁枪或者不锁枪信息")
    Result<LockOrUnlockGunSupportInfoVO> getLockOrUnlockGunSupportInfo(@RequestParam("pileSn") String pileSn);

    /**
     * @param evseSn 充电枪序列号
     * @return 该充电枪的状态（处于锁枪状态还是处于解锁枪状态）
     * @function 根据充电枪序列号获得该充电枪的状态（处于锁枪状态还是处于解锁枪状态）
     */
    @ApiImplicitParam(name = "pileSn", value = "充电枪序列号", dataType = "String", paramType = "query", required = true)
    @GetMapping(value = "/pile/queryPileDeviceEvseStatus")
    @ApiOperation(value = "根据充电枪序列号获得该充电枪的状态（处于锁枪状态还是处于解锁枪状态）", notes = "根据充电枪序列号获得该充电枪的状态（处于锁枪状态还是处于解锁枪状态）")
    Result<PileDeviceEvseStatusVO> queryPileDeviceEvseStatus(@RequestParam("evseSn") String evseSn);

    /**
     * @param snList 充电桩序列号集合
     * @return 判断结果
     * @function 判断充电桩序列号是否为英标
     */
    @PostMapping("/pile/judgeBritainStand")
    @ApiOperation(value = "判断充电桩序列号是否为英标", notes = "判断充电桩序列号是否为英标")
    Result<Map<String, String>> judgeBritainStand(@RequestBody List<String> snList);

    /**
     * @param pileSn            充电桩序列号
     * @param operationPassword 为防止误操作，需要输入操作密令
     * @return 操作结果标志
     * @function 为方便测试英国法案需求(需要把充电桩修改为英标桩)。仅供测试使用！请勿随便调用！谢谢！
     */
    @ApiImplicitParams({
            @ApiImplicitParam(name = "pileSn", value = "充电桩序列号", dataType = "String", example = "DE0120B1GN7C00009J", required = true),
            @ApiImplicitParam(name = "operationPassword", value = "为防止误操作，需要输入操作密令", dataType = "Autel2023", example = "10", required = true)
    })
    @GetMapping("/pile/modifyDtChargerInfoTableForTest")
    @ApiOperation(value = "为方便测试英国法案需求(需要把充电桩修改为英标桩)。仅供测试使用！请勿随便调用！谢谢！", notes = "为方便测试英国法案需求(需要把充电桩修改为英标桩)。仅供测试使用！请勿随便调用！谢谢！")
    Result<Boolean> modifyDtChargerInfoTableForTest(@RequestParam(value = "pileSn") String pileSn,
                                                           @RequestParam(value = "operationPassword") String operationPassword);

    @PostMapping("/firmware/listByPileSnList")
    @ApiOperation(value = "批量获取充电桩的固件列表")
    Result<List<ChargePileFirmwareVersionDTO>> getFirmwareVersionByPileSnList(@RequestBody List<String> pileSnList);

    @PostMapping("/pile/updatePower")
    @ApiOperation(value = "更新桩功率")
    Result<Boolean> updatePower(@RequestBody UpdatePowerDTO updatePowerDTO);

    @GetMapping("/pile/getGunTypeAndNum")
    @ApiOperation(value = "获取枪类型和数量")
    Result<GunTypeAndNumVO> getGunTypeAndNum(@RequestParam("pileSn") String pileSn);

    @PostMapping("/pile/batchGetGunTypeAndNum")
    @ApiOperation(value = "批量获取枪类型和数量")
    Result<List<GunTypeAndNumVO>> batchGetGunTypeAndNum(@RequestBody List<String> snList);

    /**
     * @param setPosAuthorizedAmountDTO
     * @return
     * @function 修改本地POS预授权金额
     */
    @PostMapping("/payter-device/setPosAuthorizedAmount")
    @ApiOperation(value = "修改本地POS预授权金额", notes = "修改本地POS预授权金额")
    Result<Boolean> setPosAuthorizedAmount(@RequestBody SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTO);

    @PostMapping("/pile/batchJudgePileOfflineChargeCurrentEnable")
    @ApiOperation(value = "批量判断充电桩是否支持配置离线充电电流")
    Result<Map<String, Boolean>> batchJudgePileOfflineChargeCurrentEnable(@RequestBody List<String> pileSnList);

    @PostMapping("/pile/batchJudgePileFreeVendEnable")
    @ApiOperation(value = "批量判断充电桩是否支持设置FreeVend启动方式")
    Result<Map<String, Boolean>> batchJudgePileFreeVendEnable(@RequestBody List<String> pileSnList);

    @PostMapping("/pile/dispatchTariffPileInfo")
    @ApiOperation(value = "查询计费规则下发充电桩信息", notes = "查询计费规则下发充电桩信息")
    Result<List<DispatchTariffPileInfoVO>> dispatchTariffPileInfo(@RequestBody List<String> pileSnList);

    @PostMapping("/pile/batchGetDeviceType")
    @ApiOperation(value = "批量获得设备类型")
    Result<Map<String, Integer>> batchGetDeviceType(@RequestBody List<GetDeviceTypeDTO> getDeviceTypeDTOList);

    @PostMapping("/payter-device/syncPileInfoForPos")
    @ApiOperation("为POS机同步充电桩信息")
    Result<Boolean> syncPileInfoForPos(@RequestBody SyncPileInfoForPosDTO syncPileInfoForPosDTO);
}