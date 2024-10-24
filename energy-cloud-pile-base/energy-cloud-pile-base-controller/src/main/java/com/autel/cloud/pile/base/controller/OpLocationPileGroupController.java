package com.autel.cloud.pile.base.controller;


import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.ems.dto.UpdateGroupInfo;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingParam;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingResult;
import com.autel.cloud.pile.base.dto.pile.GroupTypeDto;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.smart.monitor.dto.RedeliveryDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

/**
 * <p>
 * 充电桩群组 前端控制器
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@RestController
@RequestMapping("/opLocationPileGroup")
@Api(tags = "群组")
@Slf4j
@Validated
public class OpLocationPileGroupController {

    private final OpLocationPileGroupService opLocationPileGroupService;

    public OpLocationPileGroupController(OpLocationPileGroupService opLocationPileGroupService) {
        this.opLocationPileGroupService = opLocationPileGroupService;
    }

    @PostMapping("/add")
//    @NoRepeatSubmit
    @ApiOperation(value = "新增群组", notes = "新增群组")
    public Result<Boolean> add(@RequestBody @Valid OpLocationPileGroupDTO opLocationPileGroupDTO) {
        return opLocationPileGroupService.add(opLocationPileGroupDTO);
    }

    @PostMapping("/add-v3")
    @ApiOperation(value = "新增群组V3", notes = "新增群组V3")
    public Result<Boolean> addV3(@RequestBody SmartChargeGroupConfigAddParamDTOcopy smartChargeGroupConfigAddParamDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        smartChargeGroupConfigAddParamDTO.setMerchantId(getMerchantId(merchantId));
        return opLocationPileGroupService.addV3(smartChargeGroupConfigAddParamDTO);
    }

    @GetMapping("/detail-v3/{groupId}")
    @ApiOperation(value = "查询群组信息", notes = "查询群组信息")
    public Result<PileGroupDetailV3VOcopy> detailV3(@PathVariable("groupId") Long groupId) {
        return Result.ofSucceed(opLocationPileGroupService.detailV3(groupId));

    }

    @PostMapping("/queryDetailsV3")
    @ApiOperation(value = "批量查询群组信息", notes = "批量查询群组信息")
    public Result<List<PileGroupDetailV3VOcopy>> queryDetailsV3(@RequestBody List<Long> groupIds) {
        return Result.ofSucceed(opLocationPileGroupService.queryDetailsV3(groupIds));
    }

    /**
     * https://pile-platform-vue-enedev.auteltech.cn/api-b/pile-base-app/opLocationPileGroup/update
     * @param smartChargeGroupConfigUpdateParamDTO
     * @param merchantId
     * @return
     */
    @PostMapping("/update-v3")
    @ApiOperation(value = "编辑群组V3", notes = "新增群组V3")
    public Result<Boolean> updateV3(@RequestBody SmartChargeGroupConfigUpdateParamDTOcopy smartChargeGroupConfigUpdateParamDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        if (smartChargeGroupConfigUpdateParamDTO == null || smartChargeGroupConfigUpdateParamDTO.getId() == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        smartChargeGroupConfigUpdateParamDTO.setMerchantId(getMerchantId(merchantId));
        return opLocationPileGroupService.updateV3(smartChargeGroupConfigUpdateParamDTO);
    }

    @DeleteMapping("/delete-v3/{groupId}")
    @ApiOperation(value = "删除群组V3", notes = "删除群组V3")
//    @NoRepeatSubmit
    public Result<Boolean> deleteV3(@PathVariable("groupId") Long groupId) {
        return opLocationPileGroupService.deleteV3(groupId);
    }



    @DeleteMapping("/delete/{groupId}")
    @ApiOperation(value = "删除群组", notes = "删除群组")
//    @NoRepeatSubmit
    public Result<Boolean> delete(@PathVariable("groupId") Long groupId) {
        return opLocationPileGroupService.delete(groupId);
    }

    @PutMapping("/update")
//    @NoRepeatSubmit
    @ApiOperation(value = "更新群组", notes = "更新群组")
    public Result<Boolean> update(@RequestBody @Valid OpLocationPileGroupDTO opLocationPileGroupDTO) {
        return opLocationPileGroupService.update(opLocationPileGroupDTO);
    }

    @GetMapping("/status")
    @ApiOperation(value = "启用禁用群组", notes = "启用禁用群组")
    public Result<Long> status(@RequestParam("groupId") Long groupId, @RequestParam("status") Integer status) {
        return Result.ofSucceed(opLocationPileGroupService.status(groupId, status));
    }

    /**
     * 桩(组)范围展示：
     * 随订阅上线，则
     * 仅展示商户下有licence订阅的桩列表
     * 可跨场站设置
     * 展示层级优先级为 组  → 桩，桩展示场站信息
     * 若订阅为上，则
     * 所选桩范围仍选择场站后，再在该场站下选择
     * 可选项控制：
     * 1、ALM仅可选AC桩
     * 2、DLB可选所有未被同一层级选择的桩(组)
     * 3、AC/DC桩混排规则：
     * 若选择DC桩，则组内需均为DC桩；
     * 若选择AC桩，则组内可AC & DC混排
     * 4、DLB若为多层组，则其所包含的组充电策略单位(kW/A)需相同
     *
     * @param dto
     * @return
     */
    @PostMapping("/queryPileInMerchant")
    @ApiOperation(value = "查询该商家下所有的桩", notes = "查询该商家下所有的桩")
    public Result<List<PileGroupTreeVOcopy>> queryPileInMerchant(@RequestBody PileEvseGroupParamDTO dto, @RequestHeader(required = false, value = "merchantId") Long merchantId,
                                                             @RequestHeader(required = false, value = "X-Timezone", defaultValue = "UTC") String zoneId) {
        merchantId = getMerchantId(merchantId);
        return opLocationPileGroupService.queryPileInMerchant(dto, merchantId, zoneId);
    }

    @PostMapping("/queryPileInLocation")
    @ApiOperation(value = "查询该场站下所有的桩", notes = "查询该场站下所有的桩")
    public Result<OpLocationPileEvseGroupListVO> queryPileInLocation(@RequestBody @Valid OpLocationPileEvseGroupParamDTO dto) {
        return opLocationPileGroupService.queryPileInLocation(dto);
    }

    @GetMapping("/detail/{groupId}")
    @ApiOperation(value = "查询群组信息", notes = "查询群组信息")
    public Result<OpLocationPileGroupV2VO> detail(@PathVariable("groupId") Long groupId,
                                                  @RequestParam(value = "page", required = false, defaultValue = "1") Integer page,
                                                  @RequestParam(value = "pageSize", required = false, defaultValue = "10") Integer pageSize) {
        return opLocationPileGroupService.detail(groupId, page, pageSize);
    }

    @GetMapping("/dataAuthCheckDetail/{groupId}")
    @ApiOperation(value = "查询群组信息(前端鉴权版本)", notes = "查询群组信息(前端鉴权版本)")
    public Result<OpLocationPileGroupV2VO> dataAuthCheckDetail(@PathVariable("groupId") Long groupId,
                                                  @RequestParam(value = "page", required = false, defaultValue = "1") Integer page,
                                                  @RequestParam(value = "pageSize", required = false, defaultValue = "10") Integer pageSize) {
        return opLocationPileGroupService.detail(groupId, page, pageSize);
    }

    @PostMapping("/historyDetail")
    @ApiOperation(value = "查询群组历史信息", notes = "查询群组历史信息")
    public Result<List<OpLocationPileGroupHistoryDetailVO>> historyDetail(@RequestBody @Valid OpLocationPileGroupHistoryParamDTO dto) {
        List<OpLocationPileGroupHistoryDetailVO> historyDetailVOList = opLocationPileGroupService.historyDetail(dto);
        if (historyDetailVOList != null ){
            historyDetailVOList.sort((f, s) -> {
                if (f.getCreateTime() == null) {
                    return -1;
                }
                if (s.getCreateTime() == null) {
                    return 1;
                }
                return f.getCreateTime().compareTo(s.getCreateTime());
            });
        }
        return Result.ofSucceed(historyDetailVOList);
    }

    @PostMapping("/estimatedChargeLine")
    @ApiOperation(value = "智能充电预估曲线", notes = "智能充电预估曲线")
    @Deprecated
    public Result<List<EstimatedChargeLineVO>> estimatedChargeLine(@RequestParam("evseSn") String evseSn) {
        return Result.ofSucceed(opLocationPileGroupService.estimatedChargeLine(evseSn));
    }

    @PostMapping("/chargeLine")
    @ApiOperation(value = "智能充电预估曲线", notes = "智能充电预估曲线")
    public Result<PileGroupChargeLineVO> chargeLine(@RequestParam("evseSn") String evseSn, @RequestParam(value = "transactionId", required = false) String transactionId) {
        return Result.ofSucceed(opLocationPileGroupService.chargeLine(evseSn, transactionId));
    }

    @GetMapping("/getByMeterId")
    @ApiOperation(value = "查询电表关联群组桩电流上报", notes = "查询电表关联群组桩电流上报")
    public Result<BigDecimal> getByMeterId(@RequestParam("meterId") Long meterId) {
        return Result.ofSucceed(opLocationPileGroupService.getByMeterId(meterId));
    }

    @GetMapping("/queryAssociateByPileSn/{pileSn}")
    @ApiOperation(value = "通过pileSn查询群组关联桩的信息", notes = "通过pileSn查询群组关联桩的信息")
    public Result<OpPileGroupAssociateVO> queryAssociateByPileSn(@PathVariable("pileSn") String pileSn) {
        return opLocationPileGroupService.queryAssociateByPileSn(pileSn);
    }

    @PostMapping("/query/v2")
    @ApiOperation(value = "查询群组列表", notes = "查询群组列表")
    public Result<List<OpLocationPileGroupV2VO>> queryV2(@RequestBody @Valid OpLocationPileGroupParamDTO opLocationPileGroupParamDTO) {
        return opLocationPileGroupService.queryV2(opLocationPileGroupParamDTO);
    }

    @PostMapping("/query/v3")
    @ApiOperation(value = "查询群组列表V3", notes = "查询群组列表V3")
    public Result<Page<PileGroupVOcopy>> queryV3(@RequestBody  PileGroupParamDTO pileGroupParamDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        pileGroupParamDTO.setMerchantId(getMerchantId(merchantId));
        return opLocationPileGroupService.queryV3(pileGroupParamDTO);
    }

    @PostMapping("/queryGroupBySns")
    @ApiOperation(value = "根据sn查询群组列表", notes = "根据sn查询群组列表")
    public Result<List<PileGroupDetailV3VOcopy>> queryGroupBySns(@RequestBody List<String> sns) {
        return opLocationPileGroupService.queryGroupBySns(sns);
    }

    @GetMapping("/deliveryByEvseSn")
    @ApiOperation(value = "单个evseSn触发配置下发", notes = "单个evseSn触发配置下发")
    public Result<Boolean> deliveryByEvseSn(@RequestParam("evseSn") String evseSn, @RequestParam("state") String state) {
        return Result.ofSucceed(opLocationPileGroupService.deliveryByEvseSn(evseSn, state));
    }

    @PostMapping("/deliveryByGroupId")
    @ApiOperation(value = "按群组重新触发", notes = "按群组重新触发")
    public Result<Boolean> deliveryByGroupId(@RequestBody CommonDeliveryDTO dto) {
        return Result.ofSucceed(opLocationPileGroupService.deliveryByGroupId(dto));
    }

    @PostMapping("/deliveryByUser")
    @ApiOperation(value = "用户设置触发", notes = "用户设置触发")
    public Result<Boolean> deliveryByUser(@RequestBody DeliveryByUserDTO dto) {
        Integer fast = dto.getFast();
        if (fast != null) {
            PileGroupFastDTO fastDto = new PileGroupFastDTO();
            fastDto.setEvseSn(dto.getEvseSn());
            fastDto.setClick(1);
            fastDto.setFast(dto.getFast());
            return Result.ofSucceed(opLocationPileGroupService.fastCharging(fastDto));
        }
        return Result.ofSucceed(opLocationPileGroupService.deliveryByUser(dto));
    }

    @PostMapping("/fastCharging")
    @ApiOperation(value = "优先充电或暂停充电", notes = "优先充电或暂停充电")
    public Result<Boolean> fastCharging(@RequestBody PileGroupFastDTO fastDTO) {
        Payload payload = LoginUserUtil.getPayload();
        fastDTO.setSellerId(payload.getSellerId());
        fastDTO.setUserId(payload.getUserId());
        return Result.ofSucceed(opLocationPileGroupService.fastCharging(fastDTO));
    }

    @PostMapping("/clearDelivery")
    @ApiOperation(value = "按SN清除配置", notes = "按SN清除配置")
    public Result<Long> clearDelivery(@RequestBody String pileSn) {
        return Result.ofSucceed(opLocationPileGroupService.clearDelivery(pileSn));
    }

    @PostMapping("/clear")
    @ApiOperation(value = "按SN下发清除配置", notes = "按SN下发清除配置")
    public Result<Long> clear(@RequestBody String pileSn) {
        return Result.ofSucceed(opLocationPileGroupService.clear(pileSn));
    }

    @GetMapping("/isAssociatePileGroup")
    @ApiOperation(value = "桩是否关联群组", notes = "桩是否关联群组")
    public Result<String> isAssociatePileGroup(@RequestParam("pileSn") String pileSn) {
        return opLocationPileGroupService.isAssociatePileGroup(pileSn);
    }

    @GetMapping("/loadAllTimeSetting")
    @ApiOperation(value = "查询分时设置群组", notes = "查询分时设置群组")
    public Result<List<OpPileGroupTimeSettingDTO>> loadAllTimeSetting(@RequestParam(value = "rootId", required = false) Long rootId) {
        return Result.ofSucceed(opLocationPileGroupService.loadAllTimeSetting(rootId));
    }

    @GetMapping("/setVip")
    @ApiOperation(value = "设置VIP客户", notes = "设置VIP客户")
    public Result<String> setVip(@RequestParam("evseSn") String evseSn, @RequestParam("userId") Long userId) {
        return opLocationPileGroupService.setVip(evseSn, userId);
    }

    @PostMapping("/almStart")
    @ApiOperation(value = "触发ALM", notes = "触发ALM")
    public Result<Boolean> almStart(@RequestBody MeterParamDTO dto) {
        return Result.ofSucceed(opLocationPileGroupService.almStart(dto));
    }

    @PostMapping("/homePile/update")
    @ApiOperation(value = "家桩更新配置下发", notes = "家桩更新配置下发")
    public Result<Boolean> homePileUpdate(@RequestParam("groupId") Long groupId, @RequestParam(value = "flag", defaultValue = "1：新增，2：更新，3：删除") Integer flag) {
        return Result.ofSucceed(opLocationPileGroupService.homePileUpdate(groupId, flag));
    }

    @GetMapping("/syncStatus")
    @ApiOperation(value = "同步群组状态", notes = "同步群组状态")
    public Result<Integer> syncStatus(@RequestParam(value = "status", required = false) Integer status, @RequestParam(value = "groupId", required = false) Long groupId) {
        return Result.ofSucceed(opLocationPileGroupService.syncStatus(status, groupId));
    }

    @GetMapping("/checkAlm")
    @ApiOperation(value = "ALM白名单校验", notes = "ALM白名单校验")
    public Result<Boolean> checkAlm() {
        Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        return Result.ofSucceed(opLocationPileGroupService.checkAlm(userId));
    }

    @GetMapping("/setAlm/{userId}")
    @ApiOperation(value = "ALM白名单设置", notes = "ALM白名单设置")
    public Result<Long> setAlm(@PathVariable("userId") Long userId) {
        return Result.ofSucceed(opLocationPileGroupService.setAlm(userId));
    }

    @GetMapping("/getLoadTypeWhiteList")
    @ApiOperation(value = "ALM白名单校验", notes = "ALM白名单校验")
    public Result<List<Integer>> getLoadTypeWhiteList() {
        Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        return Result.ofSucceed(opLocationPileGroupService.getLoadTypeWhiteList(userId));
    }


    @PostMapping("/setLoadTypeWhiteList")
    @ApiOperation(value = "ALM白名单设置", notes = "ALM白名单设置")
    public Result<Long> setLoadTypeWhiteList(@RequestParam("userId") Long userId, @RequestParam(value = "loadType", defaultValue = "0：DLB， 1：ALM， 2：EMS , 3:DEMAND") Integer loadType) {
        return Result.ofSucceed(opLocationPileGroupService.setLoadTypeWhiteList(userId,loadType));
    }

    @PostMapping("/removeLoadTypeWhiteList")
    @ApiOperation(value = "ALM白名单移除", notes = "ALM白名单移除")
    public Result<Boolean> removeLoadTypeWhiteList(@RequestParam("userId") Long userId, @RequestParam(value = "loadType", defaultValue = "0：DLB， 1：ALM， 2：EMS") Integer loadType) {
        return Result.ofSucceed(opLocationPileGroupService.removeLoadTypeWhiteList(userId,loadType));
    }



    @GetMapping("/checkCost")
    @ApiOperation(value = "成本优先白名单校验", notes = "成本优先白名单校验")
    public Result<Boolean> checkCost() {
        Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        return Result.ofSucceed(opLocationPileGroupService.checkCost(userId));
    }

    @GetMapping("/setCost/{userId}")
    @ApiOperation(value = "成本优先白名单设置", notes = "成本优先白名单设置")
    public Result<Long> setCost(@PathVariable("userId") Long userId) {
        return Result.ofSucceed(opLocationPileGroupService.setCost(userId));
    }

    @PostMapping("/loadAll")
    @ApiOperation(value = "查询所有", notes = "查询所有")
    public Result<List<OpLocationPileGroupVO>> loadAll() {
        return Result.ofSucceed(opLocationPileGroupService.loadAll(null));
    }

    @PostMapping("/loadById/{rootId}")
    @ApiOperation(value = "按ID查询群组树", notes = "按ID查询群组树")
    public Result<List<OpLocationPileGroupVO>> loadById(@PathVariable("rootId") Long rootId) {
        return Result.ofSucceed(opLocationPileGroupService.loadAll(rootId));
    }

    @PostMapping("/monitor/delivery")
    @ApiOperation(value = "监控调整", notes = "监控调整")
    public Result<Boolean> redelivery(@RequestBody RedeliveryDTO dto) {
        return Result.ofSucceed(opLocationPileGroupService.redelivery(dto));
    }

    @GetMapping("/groupPileList/{groupId}")
    @ApiOperation(value = "分页显示群组下的桩列表", notes = "分页显示群组下的桩列表")
    public Result<List<OpLocationPileGroupV2VO.GroupPileV2DTO>> groupPilePageList(@PathVariable("groupId") Long groupId) {
        return Result.ofSucceed(opLocationPileGroupService.groupPileList(groupId));
    }

    @GetMapping("/getRootId/{groupId}")
    @ApiOperation(value = "查询根群组ID", notes = "查询根群组ID")
    public Result<Long> getRootId(@PathVariable("groupId") Long groupId) {
        return Result.ofSucceed(opLocationPileGroupService.getRootId(groupId));
    }

    private Long getMerchantId(Long merchantId) {
        return Optional.ofNullable(merchantId).orElseGet(() -> LoginUserHolder.getLoginUser().getPayload().getSellerId());
    }

    @GetMapping(value = "/generateDefaultGroupName")
    @ApiOperation(value = "生成默认的群组名称", notes = "生成默认的群组名称")
    public Result<String> generateDefaultGroupName() {
        return Result.ofSucceed(opLocationPileGroupService.generateDefaultGroupName());
    }

    @PostMapping(value = "/syncMeterDataFromOp")
    @ApiOperation(value = "从运维平台同步电表数据", notes = "从运维平台同步电表数据")
    public Result<Boolean> syncMeterDataFromOp(@RequestBody MeterSyncDataDTO dto) {
        return Result.ofSucceed(opLocationPileGroupService.syncMeterDataFromOp(dto));
    }

    @PostMapping(value = "/queryArithmeticCharging")
    @ApiOperation(value = "获取充电计划", notes = "获取充电计划")
    public Result<ArithmeticChargingResult>  queryArithmeticCharging(@RequestBody ArithmeticChargingParam req) {
        return opLocationPileGroupService.queryArithmeticCharging(req);
    }

    @PostMapping("/updateGroupType")
    @ApiOperation(value = "充电桩群组GroupType更新", notes = "充电桩群组GroupType更新(groupType设为2)")
    public Result<Boolean> updateGroupType(@RequestBody GroupTypeDto dto){
        return Result.ofSucceed(opLocationPileGroupService.updateGroupType(dto));
    }
    @PostMapping("/updateGroupInfo")
    @ApiOperation(value = "下发群组更新信息", notes = "下发群组更新信息")
    public Result<Boolean> updateGroupInfo(@RequestBody UpdateGroupInfo groupInfo){
        return opLocationPileGroupService.updateGroupInfo(groupInfo);
    }

    @GetMapping("/queryGroupBySellerId")
    @ApiOperation(value = "根据SellerId查询群组列表(边缘云需求)", notes = "根据SellerId查询群组列表,过滤demand标签和电表为energic的")
    public Result<List<PileGroupDetailV3VOcopy>> queryGroupBySellerId(@RequestParam("sellerId") String sellerId) throws Exception{
        return Result.ofSucceed(opLocationPileGroupService.queryGroupBySellerId(sellerId));
    }
    @PostMapping("/queryGroupByGroupIds")
    @ApiOperation(value = "根据GroupIds查询群组列表", notes = "根据GroupIds查询群组列表")
    public Result<List<PileGroupDetailV3VOcopy>> queryGroupByGroupIds(@RequestBody List<Long> groupIds) throws Exception {
        return Result.ofSucceed(opLocationPileGroupService.queryGroupByGroupIds(groupIds));
    }

}

