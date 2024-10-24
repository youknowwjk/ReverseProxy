package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.pile.base.domain.model.PileUpdateDTO;
import com.autel.cloud.pile.base.domain.model.dto.gun.SelectGunInfoForTariffGroupIdDTO;
import com.autel.cloud.pile.base.domain.model.vo.gun.SelectGunInfoForTariffGroupIdVO;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.SellerAccountService;
import com.autel.cloud.pile.base.domain.service.impl.PileBaseAsync;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.fleet.GetDeviceInfoForFleetDTO;
import com.autel.cloud.pile.base.dto.lockOrUnlockGun.LockOrUnlockGunDTO;
import com.autel.cloud.pile.base.dto.tariff.BindCostModelRuleGroupForGunDTO;
import com.autel.cloud.pile.base.dto.tariff.CostRuleAssociatedAndSellerAccountDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.PileInfoDTO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.evse.EvseDataVO;
import com.autel.cloud.pile.base.vo.evse.EvseInfoVO;
import com.autel.cloud.pile.base.vo.evse.PileEvseInfoVO;
import com.autel.cloud.pile.base.vo.fleet.GetDeviceInfoForFleetVO;
import com.autel.cloud.pile.base.vo.evse.EvseTariffInfoVO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.info.SellerInfoVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @ClassName OpLocationEvseController
 * @Author A22121
 * @Description
 * @Date 2022/4/15 11:03
 * @Version 0.0.1-SNAPSHOT
 */
@Api(tags = "充电设备设置")
@RestController
@RequestMapping("/opLocationEvse")
@Validated
@Slf4j
public class OpLocationEvseController {

    private final OpLocationEvseService opLocationEvseService;

    @Autowired
    private SellerAccountService sellerAccountService;

    @Autowired
    private PileBaseAsync pileBaseAsync;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    public OpLocationEvseController(OpLocationEvseService opLocationEvseService) {
        this.opLocationEvseService = opLocationEvseService;
    }

    @GetMapping("/getEvseByEvseSn")
    @ApiOperation(value = "查询充电设备关联信息", notes = "查询充电设备关联信息")
    public Result<OpEvseInfoDTO> getEvseByEvseSn(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseService.getEvseByEvseSn(evseSn);
    }

    @GetMapping("/getEvseInfoByEvseSn")
    @ApiOperation(value = "查询充电设备关联和场站信息", notes = "查询充电设备关联和场站信息")
    public Result<OpEvseDTO> getEvseInfoByEvseSn(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseService.getEvseInfoByEvseSn(evseSn);
    }

    @GetMapping("/getLocaitonEvseVOBySnAndGunNo")
    @ApiOperation(value = "按桩枪查询场站桩数据", notes = "充电桩校验")
    public Result<OcppLocationEVSEVO> getLocationEvseVOBySnAndGunNo(@RequestParam("evseSn") String evseSn) {
        return opLocationEvseService.getLocationEvseVOBySnAndGunNo(evseSn);
    }

    @PostMapping("/getSnTimeZoneMap")
    @ApiOperation(value = "查询桩场站时区", notes = "查询桩场站时区")
    public Result<Map<String, String>> getSnTimeZoneMap(@Validated @RequestBody List<String> evseSnList) {
        return opLocationEvseService.getSnTimeZoneMap(evseSnList);
    }

    @GetMapping("/getLocationByPileSn")
    @ApiOperation(value = "按桩序列号查询场站信息", notes = "扫码校验")
    public Result<OcppLocationEVSEVO> getLocationByPileSn(@RequestParam("pileSn") String pileSn) {
        return opLocationEvseService.getLocationByPileSn(pileSn);
    }

    @GetMapping("/getLocationConnectorListByPileSn")
    @ApiOperation(value = "按照桩查询连接器列表", notes = "扫码校验")
    public Result<List<OplocationConnectorScanDTO>> getLocationConnectorListByPileSn(@RequestParam("pileSn") String pileSn) {
        return opLocationEvseService.getLocationConnectorListByPileSn(pileSn);
    }

    @GetMapping("/getEvseList")
    @ApiOperation(value = "根据SN查询充电设备列表", notes = "扫码校验")
    public Result<OpLocationInfoDTO> getEvseList(@RequestParam("pileSn") String pileSn) {
        return Result.ofSucceed(opLocationEvseService.getEvseList(pileSn));
    }

    @GetMapping("/getEvseListDeleted")
    @ApiOperation(value = "根据SN查询充电设备列表包含删除", notes = "根据SN查询充电设备列表包含删除")
    public Result<List<OpEvseInfoDTO>> getEvseListDeleted(@RequestParam("pileSn") String pileSn) {
        return Result.ofSucceed(opLocationEvseService.getEvseListDeleted(pileSn));
    }

    @ApiOperation("充电设备详情查询")
    @PostMapping("/details/{id}")
    public Result<OpLocationEvseDTO> details(@PathVariable Long id) {
        return opLocationEvseService.details(id);
    }

    @OperationActionLog(action = "query", object = "tariff")
    @ApiOperation(value = "批量查询计费规则组关联的充电设备的信息", notes = "批量查询计费规则组关联的充电设备的信息")
    @PostMapping("/getEvseNumByTariffIds")
    public Result<List<TariffsEvseNumDTO>> getEvseNumByTariffIds(@Validated @RequestBody List<Long> tariffGroupIds) {

        log.info("===>>>OpLocationEvseController.getEvseNumByTariffIds tariffGroupIds: {}",
                JSON.toJSONString(tariffGroupIds));

        return opLocationEvseService.getEvseNumByTariffIds(tariffGroupIds);
    }

    @PostMapping("/bindCostModelRuleGroupForGun")
    @ApiOperation(value = "为充电设备（充电枪）绑定计费规则组 (运营平台V2.0新版)", notes = "为充电设备（充电枪）绑定计费规则组 (运营平台V2.0新版)")
    public Result<Boolean> bindCostModelRuleGroupForGun(@Validated @RequestBody List<BindCostModelRuleGroupForGunDTO> bindCostModelRuleGroupForGunDTOList) {

        log.info("OpLocationEvseController.bindCostModelRuleGroupForGun bindCostModelRuleGroupForGunDTOList: {}", JSON.toJSONString(bindCostModelRuleGroupForGunDTOList));

        return opLocationEvseService.bindCostModelRuleGroupForGun(bindCostModelRuleGroupForGunDTOList);
    }

    @ApiOperation("为计费规则查询充电设备信息")
    @PostMapping("/getEvseInfoForRules")
    public Result<List<OpEvseAssociatedRuleDTO>> getEvseInfoForRules(@Validated @RequestBody OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {
        return opLocationEvseService.getEvseInfoForRules(opPileAssociatedRuleParamDTO);
    }

    @OperationActionLog(action = "query", object = "tariff")
    @ApiOperation("为计费规则分页查询桩列表")
    @PostMapping("/getPileListForRules")
    @Deprecated
    public Result<Page<OpPileAssociatedRuleDTO>> getPileListForRules(@Validated @RequestBody OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {

        log.info("===>>>OpLocationEvseController.getPileListForRules opPileAssociatedRuleParamDTO: {}", JSON.toJSONString(opPileAssociatedRuleParamDTO));

        return opLocationEvseService.getPileListForRules(opPileAssociatedRuleParamDTO);
    }

    @ApiOperation("为计费规则查询桩信息")
    @PostMapping("/getPileInfoForRules")
    @Deprecated
    public Result<List<OpPileAssociatedRuleDTO>> getPileInfoForRules(@Validated @RequestBody OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {
        return opLocationEvseService.getPileInfoForRules(opPileAssociatedRuleParamDTO);
    }

    @OperationActionLog(action = "query", object = "tariff")
    @ApiOperation("计费关联桩树列表")
    @PostMapping("/getEvseInfoTreeForRules")
    @Deprecated
    public Result<List<UnionPileTreeVO>> getEvseInfoTreeForRules(@Validated @RequestBody OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {

        log.info("===>>>OpLocationEvseController.getEvseInfoTreeForRules opPileAssociatedRuleParamDTO: {}", JSON.toJSONString(opPileAssociatedRuleParamDTO));

        return opLocationEvseService.getEvseInfoTreeForRules(opPileAssociatedRuleParamDTO);
    }

    @PostMapping("/importEvses")
    @ApiOperation(value = "批量导入", notes = "批量导入")
    public Result<Boolean> importEvses(@RequestParam("file") MultipartFile file) {
        return null;
    }

    @PostMapping("/downLocationXls")
    @ApiOperation(value = "场站导入模板下载", notes = "场站导入模板下载")
    public void downLocationXls(HttpServletRequest request, HttpServletResponse response) {
        opLocationEvseService.downLocationEvseXls(request, response);
    }

    @PostMapping("/searchSitesOrEvses")
    @ApiOperation(value = "根据场站名称或桩SN码搜索", notes = "根据场站名称或桩SN码搜索")
    public Result<Page<OpLocationsAndEvsesDTO>> searchSitesOrEvses(@Validated @RequestBody OpLocationQueryDTO opLocationQueryDTO) {
        return opLocationEvseService.searchSitesOrEvses(opLocationQueryDTO);
    }

    @GetMapping("/verifyPileSn")
    @ApiOperation(value = "校验pileSn是否准确", notes = "校验pileSn是否准确")
    public Result<Boolean> verifyPileSn(@RequestParam("pileSn") String pileSn) {
        return opLocationEvseService.verifyPileSn(pileSn);
    }

    @GetMapping("/verifyThirdPileSn")
    @ApiOperation(value = "校验三方pileSn是否准确", notes = "校验三方pileSn是否准确")
    public Result<Boolean> verifyThirdPileSn(@RequestParam("pileSn") String pileSn, @RequestParam("brandId") Long brandId) {
        return opLocationEvseService.verifyThirdPileSn(pileSn, brandId);
    }

    @GetMapping("/verifyPin")
    @ApiOperation(value = "校验pin是否准确", notes = "校验pin是否准确")
    public Result<Boolean> verifyPin(@RequestParam("pileSn") String pileSn, @RequestParam("pin") String pin) {
        return opLocationEvseService.verifyPin(pileSn, pin);
    }

    @GetMapping("/getDetailsFromEsById/{id}")
    @ApiOperation(value = "通过evseId查询es中的数据", notes = "通过evseId查询es中的数据")
    public Result<OpLocationEvseDTO> getDetailsFromEsById(@PathVariable Long id) {
        return opLocationEvseService.getDetailsFromEsById(id);
    }

    @GetMapping("/getDetailsFromDbById/{id}")
    @ApiOperation(value = "通过evseId查询DB中的数据", notes = "通过evseId查询DB中的数据")
    public Result<OpLocationEvseDetailVO> getDetailsFromDbById(@PathVariable Long id) {
        return Result.ofSucceed(opLocationEvseService.getDetailsFromDbById(id));
    }


    @OperationActionLog(action = "add", object = "pile")
    //@PostMapping("/createEvse") 废弃接口 先注释
    @ApiOperation(value = "新增充电设备", notes = "新增充电设备")
    @Deprecated
    public Result<List<PileVO>> createEvse(@Validated @RequestBody @Valid List<OpLocationEvseDTO> opLocationEvseDTOs) {

        log.info("OpLocationEvseController.createEvse opLocationEvseDTOs : {}", JSON.toJSONString(opLocationEvseDTOs));

        return opLocationEvseService.createEvse(opLocationEvseDTOs);
    }


    @OperationActionLog(action = "edit", object = "pile")
    @PostMapping("/updateEvse")
    @ApiOperation(value = "更新充电设备", notes = "更新充电设备")
    public Result<List<PileVO>> updateEvse(@Validated @RequestBody @Valid List<OpLocationEvseDTO> opLocationEvseDTOs) {

        log.info("OpLocationEvseController.updateEvse opLocationEvseDTOs : {}", JSON.toJSONString(opLocationEvseDTOs));
        try {
            for (OpLocationEvseDTO opLocationEvseDTO : opLocationEvseDTOs) {
                PileUpdateDTO dto = new PileUpdateDTO();
                dto.setSn(opLocationEvseDTO.getPileSN());
                dto.setName(opLocationEvseDTO.getPileName());
                if (StringUtils.isNotBlank(opLocationEvseDTO.getPowerType())) {
                    dto.setPowerType(opLocationEvseDTO.getPowerType().substring(0, 2));
                }
                dto.setRatedPower(BigDecimal.valueOf(opLocationEvseDTO.getPower()));
                chargePointMerchantRelationService.updatePile(dto, LoginUserHolder.getLoginUser().getPayload().getSellerId());
            }
        } catch (Exception e) {
            log.error("updatePile failed", e);
        }
        return opLocationEvseService.updateEvse(opLocationEvseDTOs);
    }

    @PostMapping("/updatePileName")
    @ApiOperation(value = "根据sn修改桩名称")
    public Result<Boolean> updatePileName(@Validated @RequestBody UpdatePileNameDTO updatePileNameDTO) {
        return Result.ofSucceed(opLocationEvseService.updatePileName(updatePileNameDTO));
    }

    @PostMapping("/updatePileName/v2")
    @ApiOperation(value = "根据sn修改桩名称-v2")
    public Result<Object> updatePileNameV2(@Validated @RequestBody UpdatePileNameDTO updatePileNameDTO) {
        return Result.ofSucceed(opLocationEvseService.updatePileNameV2(updatePileNameDTO));
    }

    @PostMapping("/updatePublicMark")
    @ApiOperation(value = "修改桩公开/私有")
    @Deprecated
    public Result<Boolean> updatePublicMark(@Validated @RequestBody UpdatePublicMarkDTO updatePublicMarkDTO) {
        return opLocationEvseService.updatePublicMark(updatePublicMarkDTO);
    }

    @PostMapping("/queryPileListByOperationId")
    @ApiOperation(value = "根据商户ID查询充电设备列表", notes = "根据商户ID查询充电设备列表")
    public Result<List<OpEvseInfoDTO>> queryPileListByOperationId(@Validated @RequestBody QueryOplocationDTO queryOplocationDTO) {
        return opLocationEvseService.queryPileListByOperationId(queryOplocationDTO);
    }

    @PostMapping("/batchQueryPileBindTime")
    @ApiOperation(value = "批量查询桩绑定时间", notes = "批量查询桩绑定时间")
    public Result<List<PileBindTimeVO>> batchQueryPileBindTime(@Validated @RequestBody List<String> snList) {
        return opLocationEvseService.batchQueryPileBindTime(snList);
    }

    @GetMapping("/getEvseListByLocationId/{locationId}")
    @ApiOperation(value = "通过locationId查询所有的充电设备列表", notes = "通过locationId查询所有的充电设备列表")
    public Result<List<OpLocationEvseDTO>> getEvseListByLocationId(@PathVariable Long locationId) {
        return opLocationEvseService.getEvseListByLocationId(locationId);
    }

    @GetMapping("/getEvseListIncludeDeletedByLocationId/{locationId}")
    @ApiOperation(value = "通过locationId查询所有的充电设备列表", notes = "通过locationId查询所有的充电设备列表")
    public Result<List<OpLocationEvseDTO>> getEvseListIncludeDeletedByLocationId(@PathVariable Long locationId) {
        return opLocationEvseService.getEvseListIncludeDeletedByLocationId(locationId);
    }

    @PostMapping("/findEvseList")
    @ApiOperation("根据场站ID查询场站设备")
    public Result<List<LocationEvseInfoVO>> findEvseList(@Validated @RequestBody List<Long> locationIds) {
        return opLocationEvseService.findEvseList(locationIds);
    }

    @PostMapping("/updateEvseState")
    @ApiOperation("更新充电设备状态")
    @Deprecated
    public Result<Boolean> updateEvseState(@RequestParam("evseSn") String evseSn, @RequestParam("state") String state) {
        return opLocationEvseService.updateEvseState(evseSn, state);
    }


    @PostMapping("/updateEvseStateByUpdatedAt")
    @ApiOperation("更新充电设备状态")
    public Result<Boolean> updateEvseStateByUpdatedAt(@RequestParam("evseSn") String evseSn, @RequestParam("state") String state, @RequestParam("updatedAt") Long updatedAt) {
        return opLocationEvseService.updateEvseStateByUpdateAt(evseSn, state, updatedAt);
    }

    @PostMapping("/batchUpdateEvseStateByUpdatedAt")
    @ApiOperation("批量更新充电设备状态")
    public Result<Boolean> batchUpdateEvseStateByUpdatedAt(@Validated @RequestBody Collection<EvseStateDTO> collection) {
        return opLocationEvseService.batchUpdateEvseStateByUpdateAt(collection);
    }

    @PostMapping("/queryLocationAndPileBySeller")
    @ApiOperation(value = "根据商家id查询充电充电设备复合信息", notes = "根据商家id查询充电充电设备复合信息")
    public Result<List<PileInfoDTO>> queryLocationAndPileBySeller(@RequestParam("sellerId") Long sellerId) {
        return opLocationEvseService.queryLocationAndPileBySeller(sellerId);
    }

    @PostMapping("/queryPileInfoByPileSn")
    @ApiOperation(value = "根据充电桩SN查询信息", notes = "根据充电桩SN查询信息")
    public Result<List<HubPileDTO>> queryPileInfoByPileSn(@Validated @RequestBody List<String> pileSnList) {
        return opLocationEvseService.queryPileInfoByPileSn(pileSnList);
    }

    @PostMapping("/syncEvseExpand")
    @ApiOperation(value = "同步充电设备ES数据到充电设备扩展类", notes = "同步充电设备ES数据到充电设备扩展类")
    public Result<Integer> syncEvseExpand(@Validated @RequestBody EvseExpandDTO dto) {
        return Result.ofSucceed(opLocationEvseService.syncEvseExpand(dto));
    }

    /**
     * @param costRuleAssociatedAndSellerAccountDTO 商家的stripe账号是否可用以及充电桩计费规则关联的充电设备数量 入参模型
     * @return 商家的stripe账号是否可用以及充电桩计费规则关联的充电设备数量
     * @function 查询商家的stripe账号是否可用以及充电桩计费规则关联的充电设备数量
     */
    @PostMapping("/queryCostRuleAssociatedAndSellerAccount")
    @ApiOperation(value = "查询商家的stripe账号是否可用以及充电桩计费规则关联的充电设备数量", notes = "查询商家的stripe账号是否可用以及充电桩计费规则关联的充电设备数量")
    public Result<CostRuleAssociatedAndSellerAccountVO> queryCostRuleAssociatedAndSellerAccount(@Validated @RequestBody CostRuleAssociatedAndSellerAccountDTO costRuleAssociatedAndSellerAccountDTO) {

        log.info("======>>>>>>>>> OpLocationEvseController.queryCostRuleAssociatedAndSellerAccount costRuleAssociatedAndSellerAccountDTO : {}", JSON.toJSONString(costRuleAssociatedAndSellerAccountDTO));

        CostRuleAssociatedAndSellerAccountVO costRuleAssociatedAndSellerAccountVO = new CostRuleAssociatedAndSellerAccountVO();

        if (ObjectUtils.isNotEmpty(costRuleAssociatedAndSellerAccountDTO)) {
            Long sellerId = costRuleAssociatedAndSellerAccountDTO.getSellerId();
            Long id = costRuleAssociatedAndSellerAccountDTO.getId();

            Boolean flag = sellerAccountService.isSellerStripeAccountCharged(sellerId);
            Integer number = opLocationEvseService.queryCostRuleAssociatedAndSellerAccount(id);

            costRuleAssociatedAndSellerAccountVO.setIsSellerStripeAccountCharged(flag);
            costRuleAssociatedAndSellerAccountVO.setCostRuleModelAssociatedPileNumber(number);
        }

        log.info("======>>>>>>>>> OpLocationEvseController.queryCostRuleAssociatedAndSellerAccount costRuleAssociatedAndSellerAccountVO : {}", JSON.toJSONString(costRuleAssociatedAndSellerAccountVO));

        return Result.ofSucceed(costRuleAssociatedAndSellerAccountVO);
    }

    @PostMapping("/getTatiffRuleByEvseSn")
    @ApiOperation("根据枪序列号获取计费规则")
    @Deprecated
    public Result<CostModelRuleDTO> getTatiffRuleByEvseSn(@Validated @RequestBody PileEvseDTO pileEvseDTO) {

        log.info("======>>>>>>>>> OpLocationEvseController.getTatiffRuleByEvseSn pileEvseDTO : {}", JSON.toJSONString(pileEvseDTO));

        return opLocationEvseService.getTatiffRuleByEvseSn(pileEvseDTO);
    }

    /**
     * @param tariffId 计费规则id
     * @return 某个计费规则id绑定的桩的类型
     * @function 查询某个计费规则id绑定的桩的类型
     */
    @GetMapping("/getSimpleChargePileInfoList")
    @ApiOperation(value = "查询某个计费规则id绑定的桩的类型", notes = "查询某个计费规则id绑定的桩的类型")
    public Result<List<SimpleChargePileVO>> getSimpleChargePileInfoList(@RequestParam("tariffId") Long tariffId) {

        log.info("======>>>>>>>>> OpLocationEvseController.getSimpleChargePileInfoList tariffId : {}", JSON.toJSONString(tariffId));

        return Result.ofSucceed(opLocationEvseService.getSimpleChargePileInfoList(tariffId));
    }

    @GetMapping("/syncGunType")
    @ApiOperation(value = "同步枪类型", notes = "同步枪类型")
    public Result<Boolean> syncGunType(@RequestParam("pileSn") String pileSn) {
        return opLocationEvseService.syncGunType(pileSn);
    }

    @PostMapping("/getZoneIdByPileAndEvse")
    @ApiOperation(value = "根据桩SN和枪SN查询枪计费规则id和zoneId", notes = "根据桩SN和枪SN查询枪计费规则id和zoneId")
    public Result<TariffRuleOfPileDTO> getZoneIdByPileAndEvse(@Validated @RequestBody TariffRuleOfPileDTO tariffRuleOfPileDTO) {

        log.info("======>>>>>>>>> OpLocationEvseController.getZoneIdByPileAndEvse tariffRuleOfPileDTO : {}", JSON.toJSONString(tariffRuleOfPileDTO));

        return opLocationEvseService.getZoneIdByPileAndEvse(tariffRuleOfPileDTO);
    }

    @PostMapping("/getTariffCountByPileSn")
    @ApiOperation("根据桩号判断计费规则是否一致")
    public Result<TariffCountVO> getTariffCountByPileSn(@Validated @RequestBody PileEvseTariffDTO pileEvseTariffDTO) {
        return opLocationEvseService.getTariffCountByPileSn(pileEvseTariffDTO);
    }

    /**
     * @param tariffId 计费规则id
     * @return 计费规则模型关联的充电设备数量
     * @function 查询计费规则模型关联的充电设备数量
     */
    @GetMapping("/queryCostRuleAssociatedPileCount")
    @ApiOperation(value = "查询计费规则模型关联的充电设备数量", notes = "查询计费规则模型关联的充电设备数量")
    public Result<CostRuleAssociatedAndSellerAccountVO> queryCostRuleAssociatedPileCount(@RequestParam("tariffId") Long tariffId) {

        log.info("======>>>>>>>>> OpLocationEvseController.queryCostRuleAssociatedPileCount tariffId : {}", JSON.toJSONString(tariffId));

        CostRuleAssociatedAndSellerAccountVO costRuleAssociatedAndSellerAccountVO = new CostRuleAssociatedAndSellerAccountVO();
        costRuleAssociatedAndSellerAccountVO.setCostRuleModelAssociatedPileNumber(opLocationEvseService.queryCostRuleAssociatedAndSellerAccount(tariffId));

        return Result.ofSucceed(costRuleAssociatedAndSellerAccountVO);
    }

    @PostMapping("/getPileEvseInfoByEvseSnList")
    @ApiOperation(value = "根据枪序列号查询桩信息", notes = "根据枪序列号查询桩信息")
    public Result<Map<String, PileEvseSimpleVO>> getPileEvseInfoByEvseSnList(@Validated @RequestBody List<String> evseSnList) {
        return opLocationEvseService.getPileEvseInfoByEvseSnList(evseSnList);
    }

    @PostMapping("/getOpenAndRuleInfoByEvseSn")
    @ApiOperation(value = "根据枪序列号查询进场控制时间和场站开放时间", notes = "根据枪序列号查询进场控制时间和场站开放时间")
    public Result<OpenAndRuleInfoVO> getOpenAndRuleInfoByEvseSn(@Validated @RequestBody OpenAndRuleInfoDTO openAndRuleInfoDTO) {
        return opLocationEvseService.getOpenAndRuleInfoByEvseSn(openAndRuleInfoDTO);
    }

    @PostMapping("/testMergeUnitPrice")
    @ApiOperation("测试接口：针对相同费用的电量费时段，进行时段合并")
    public Result<CostModelRuleDTO> mergeUnitPrice(@RequestBody CostModelRuleDTO costModelRuleDTO) {

        log.info("====>>>>OpLocationEvseController.mergeUnitPrice costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));

        return Result.ofSucceed(TariffUtil.mergeUnitPrice(costModelRuleDTO));
    }

    @GetMapping("/dataCheck")
    @ApiOperation(value = "枪数据不同步筛选", notes = "枪数据不同步筛选")
    public Result<List<Long>> dataCheck() {
        return opLocationEvseService.dataCheck();
    }

    /**
     * @param pileEvseDTO 序列号
     * @return 与场站（桩或枪）计费相关的信息
     * @function 查询与场站（桩或枪）计费相关的信息
     */
    @PostMapping("/queryTariffInfoVO")
    @ApiOperation(value = "查询与场站（桩或枪）计费相关的信息", notes = "查询与场站（桩或枪）计费相关的信息")
    public Result<TariffInfoVO> queryTariffInfoVO(@RequestBody PileEvseDTO pileEvseDTO) {

        log.info("=====>>>>>OpLocationEvseController.queryTariffInfoVO pileEvseDTO : {}", JSON.toJSONString(pileEvseDTO));

        return Result.ofSucceed(opLocationEvseService.queryTariffInfoVO(pileEvseDTO));
    }

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 下发充电枪锁枪或者不锁枪的命令
     */
    @PostMapping("/setCableEnable")
    @ApiOperation(value = "下发充电枪锁枪或者不锁枪的命令", notes = "下发充电枪锁枪或者不锁枪的命令")
    public Result<Boolean> setCableEnable(@RequestBody LockOrUnlockGunDTO lockOrUnlockGunDTO) {

        log.info("===>>>OpLocationEvseController.setCableEnable lockOrUnlockGunDTO : {}", JSON.toJSONString(lockOrUnlockGunDTO));

        return Result.ofSucceed(opLocationEvseService.setCableEnable(lockOrUnlockGunDTO));
    }

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 获得充电枪的状态（返回此时充电枪是否已经插入充电口的标志）
     */
    @PostMapping("/getEvseInPile")
    @ApiOperation(value = "获得充电枪的状态（返回此时充电枪是否已经插入充电口的标志）", notes = "获得充电枪的状态（返回此时充电枪是否已经插入充电口的标志）")
    public Result<Boolean> getEvseInPile(@RequestBody LockOrUnlockGunDTO lockOrUnlockGunDTO) {

        log.info("===>>>OpLocationEvseController.getEvseInPile lockOrUnlockGunDTO : {}", JSON.toJSONString(lockOrUnlockGunDTO));

        return Result.ofSucceed(opLocationEvseService.getEvseInPile(lockOrUnlockGunDTO));
    }

    /**
     * @return
     * @function 获得商家下所有的充电枪数据
     */
    @GetMapping("/getAllEvseInfoBySellerId")
    @ApiOperation(value = "获得商家下所有的充电枪数据", notes = "获得商家下所有的充电枪数据")
    public Result<List<EvseInfoVO>> getAllEvseInfoBySellerId() {

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        Long sellerId = jwtInfo.getPayload().getSellerId();

        log.info("===>>>OpLocationEvseController.getAllEvseInfoBySellerId sellerId : {}", JSON.toJSONString(sellerId));

        return Result.ofSucceed(opLocationEvseService.getAllEvseInfoBySellerId(sellerId));
    }

    /**
     * @param pileSnList
     * @return
     * @function 判断充电桩是否在美国加州
     */
    @PostMapping("/judgeUSCAPile")
    @ApiOperation(value = "判断充电桩是否在美国加州", notes = "判断充电桩是否在美国加州")
    public Result<Map<String, Boolean>> judgeUSCAPile(@RequestBody List<String> pileSnList) {

        log.info("===>>>OpLocationEvseController.judgeUSCAPile pileSnList : {}", JSON.toJSONString(pileSnList));

        return Result.ofSucceed(opLocationEvseService.judgeUSCAPile(pileSnList));
    }

    // @PostMapping(value = "/savePileListV2")
    @ApiOperation(value = "批量保存桩(用于场站添加桩接口)", notes = "批量保存桩(用于场站添加桩接口)")
    public Result<PileUploadSaveVO> savePileListV2(@RequestBody SavePileListV2DTO savePileListV2DTO) {

        log.info("批量保存桩(用于场站添加桩接口)，savePileListV2DTO: {}", JSON.toJSONString(savePileListV2DTO));
        savePileListV2DTO.setMerchantId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        return Result.ofSucceed(opLocationEvseService.savePileListV2(savePileListV2DTO));
    }

    @PostMapping(value = {"/savePileListV2", "/savePileListV3"})
    @ApiOperation(value = "批量保存桩(用于场站添加桩接口)", notes = "批量保存桩(用于场站添加桩接口)")
    public Result<PileUploadSaveVO> savePileListV3(@RequestBody SavePileListV3DTO savePileListV3DTO) {
        log.info("批量保存桩(用于场站添加桩接口)，savePileListV3DTO: {}", JSON.toJSONString(savePileListV3DTO));
        savePileListV3DTO.setMerchantId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        return Result.ofSucceed(opLocationEvseService.savePileListV3(savePileListV3DTO));
    }

    @GetMapping("/getTariffIdListBySn")
    @ApiOperation(value = "按设备序列号查询所有枪计费规则ID列表", notes = "按设备序列号查询所有枪计费规则ID列表")
    public Result<List<String>> getTariffIdListBySn(@RequestParam("sn") String sn) {
        return Result.ofSucceed(opLocationEvseService.getTariffIdListBySn(sn));
    }

    /**
     * @param eroamingPileList
     * @return
     * @function 同步枪数据到EMSP
     */
    @PostMapping("/syncEroamingPileList")
    @ApiOperation(value = "同步枪数据到EMSP", notes = "同步枪数据到EMSP")
    public Result<Boolean> syncEroamingPileList(@RequestBody List<EroamingPileVO> eroamingPileList) {

        log.info("===>>>同步计费规则关联的互联互通的枪数据到EMSP eroamingPileList : {}", JSON.toJSONString(eroamingPileList));
        pileBaseAsync.syncEroamingPileList(eroamingPileList);
        return Result.ofSucceed(true);
    }

    /**
     * @param tariffId
     * @return
     * @function 查询计费规则关联的互联互通的枪数据到EMSP
     */
    @PostMapping("/queryEroamingPileListByTariff")
    @ApiOperation(value = "查询计费规则关联的互联互通的枪数据", notes = "查询计费规则关联的互联互通的枪数据")
    public Result<List<EroamingPileVO>> queryEroamingPileListByTariff(@RequestParam("tariffId") Long tariffId) {

        log.info("===>>>查询计费规则关联的互联互通的枪数据， tariffId : {}", tariffId);
        return Result.ofSucceed(opLocationEvseService.queryEroamingPileListByTariff(tariffId));
    }

    /**
     * 根据桩SN获取桩的详细信息
     *
     * @param pileSN 桩SN
     * @return 桩信息
     */
    @GetMapping("/getPileInfoByPileSN")
    public Result<OpLocationPileEvseElasticDTO> getPileInfoByPileSN(@RequestParam String pileSN) {
        return Result.ofSucceed(opLocationEvseService.getPileInfoByPileSN(pileSN));
    }

    /**
     * @param pileSnList
     * @return
     * @function 批量根据充电桩序列号集合获取可用的充电枪数据集合
     */
    @PostMapping("/availableEvseList")
    @ApiOperation(value = "批量根据充电桩序列号集合获取可用的充电枪数据集合", notes = "批量根据充电桩序列号集合获取可用的充电枪数据集合")
    public Result<List<EvseInfoVO>> availableEvseList(@RequestBody List<String> pileSnList) {

        log.info("===>>>OpLocationEvseController.availableEvseList pileSnList : {}",
                JSON.toJSONString(pileSnList));

        return Result.ofSucceed(opLocationEvseService.availableEvseList(pileSnList));
    }

    @PostMapping("/searchEvseByNameOrSn")
    @ApiOperation("根据桩名称或者sn模糊搜索枪")
    public Result<IPage<SearchEvseByNameOrSnVO>> searchEvseByNameOrSn(@RequestBody SearchEvseByNameOrSnDTO dto) {
        Long sellerId = LoginUserUtil.getSellerId();
        return Result.ofSucceed(opLocationEvseService.searchEvseByNameOrSn(dto,sellerId));
    }


    @GetMapping("/getSellerEvseInfo")
    @ApiOperation(value = "获取商家的所有设备信息")
    public Result<List<OpLocationEvseElasticDTO>> getSellerEvseInfo(@RequestParam("pileSnList")List<String> pileSnList, @RequestParam("operatorId") Long operatorId) {
        return Result.ofSucceed(opLocationEvseService.getSellerEvseInfo(pileSnList, operatorId));
    }

    @PostMapping("/updateEvseLastOrderSeq")
    @ApiOperation(value = "更新枪最后一笔订单的orderSeq信息")
    public Result<Boolean> updateEvseLastOrderSeq(@RequestParam("evseSn") String evseSn, @RequestParam("orderSeq") Long orderSeq) {
        return Result.ofSucceed(opLocationEvseService.updateEvseLastOrderSeq(evseSn,orderSeq));
    }

    @PostMapping("/getAllEvseSnByLocationIds")
    @ApiOperation("通过场站id查找枪sn列表")
    public Result<List<String>> getAllEvseSnByLocationIds(@RequestBody List<Long> locationIds) {
        return Result.ofSucceed(opLocationEvseService.getAllEvseSnByLocationIds(locationIds));
    }

    /**
     * 根据桩SN获取桩的详细信息
     *
     * @param pileSNList 桩SN
     * @return 桩信息
     */
    @GetMapping("/getPileListByPileSNList")
    public Result<List<OpLocationPileEvseElasticDTO>> getPileListByPileSNList(@RequestParam List<String> pileSNList) {
        return Result.ofSucceed(opLocationEvseService.getPileListByPileSNList(pileSNList));
    }

    @PostMapping("/selectBindGunInfoForTariffGroupId")
    @ApiOperation(value = "为计费规则组查询当前已关联的枪信息")
    public Result<Page<SelectGunInfoForTariffGroupIdVO>> selectBindGunInfoForTariffGroupId(@RequestBody SelectGunInfoForTariffGroupIdDTO selectGunInfoForTariffGroupIdDTO) {

        log.info("===>>> OpLocationEvseController.selectBindGunInfoForTariffGroupId selectGunInfoForTariffGroupIdDTO : {}",
                JSON.toJSONString(selectGunInfoForTariffGroupIdDTO));

        return Result.ofSucceed(opLocationEvseService.selectBindGunInfoForTariffGroupId(selectGunInfoForTariffGroupIdDTO));
    }

    @PostMapping("/selectGunInfoForTariffGroupId")
    @ApiOperation(value = "为计费规则组查询枪信息")
    public Result<Page<SelectGunInfoForTariffGroupIdVO>> selectGunInfoForTariffGroupId(@RequestBody SelectGunInfoForTariffGroupIdDTO selectGunInfoForTariffGroupIdDTO) {

        log.info("===>>> OpLocationEvseController.selectGunInfoForTariffGroupId selectGunInfoForTariffGroupIdDTO : {}",
                JSON.toJSONString(selectGunInfoForTariffGroupIdDTO));

        return Result.ofSucceed(opLocationEvseService.selectGunInfoForTariffGroupId(selectGunInfoForTariffGroupIdDTO));
    }

    @ApiOperation("为车队根据条件查询设备(充电枪)数据")
    @PostMapping("/getDeviceInfoForFleet")
    Result<Page<GetDeviceInfoForFleetVO>> getDeviceInfoForFleet(@RequestBody GetDeviceInfoForFleetDTO getDeviceInfoForFleetDTO) {

        log.info("===>>>OpLocationEvseController.getDeviceInfoForFleet getDeviceInfoForFleetDTO : {}",
                JSON.toJSONString(getDeviceInfoForFleetDTO));

        return Result.ofSucceed(opLocationEvseService.getDeviceInfoForFleet(getDeviceInfoForFleetDTO));
    }

    @PostMapping("/batchGetEvseInfo")
    @ApiOperation(value = "批量根据枪sn查询枪信息(设备状态)", notes = "批量根据枪sn查询枪信息(设备状态)")
    Result<List<EvseDataVO>> batchGetEvseInfo(@RequestBody List<String> evseList) {

        log.info("===>>>OpLocationEvseController.batchGetEvseInfo evseList : {}",
                JSON.toJSONString(evseList));

        return Result.ofSucceed(opLocationEvseService.batchGetEvseInfo(evseList));
    }

    @PostMapping("/batchGetEvseData")
    @ApiOperation(value = "批量根据枪sn查询设备的信息(当前充电车辆的mac地址)", notes = "批量根据枪sn查询设备的信息(当前充电车辆的mac地址)")
    Result<List<PileEvseInfoVO>> batchGetEvseData(@RequestBody List<String> evseList){

        log.info("===>>>OpLocationEvseController.batchGetEvseData evseList : {}",
                JSON.toJSONString(evseList));

        return Result.ofSucceed(opLocationEvseService.batchGetEvseData(evseList));
    }

    @PostMapping("/batchGetEvseTariffInfo")
    @ApiOperation("批量根据枪序列号来获取枪的计费规则信息")
    public Result<List<EvseTariffInfoVO>> batchGetEvseTariffInfo(@RequestBody List<String> evseSnList) {

        log.info("===>>> OpLocationEvseController.batchGetEvseTariffInfo evseSnList : {}",
                JSON.toJSONString(evseSnList));

        return Result.ofSucceed(opLocationEvseService.batchGetEvseTariffInfo(evseSnList));
    }
}
