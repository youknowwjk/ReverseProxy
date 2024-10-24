package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.common.CustomizedDateTime;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.pile.base.domain.model.ChargePointDTO;
import com.autel.cloud.pile.base.domain.model.ImminentExpireChargePointDTO;
import com.autel.cloud.pile.base.domain.model.RemoveChargePointDTO;
import com.autel.cloud.pile.base.domain.model.dto.DeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.dto.SynchronizeDeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.vo.DeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.model.vo.SynchronizeDeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.chargepoint.ChargePointDetailDTO;
import com.autel.cloud.pile.base.dto.chargepoint.GetDeviceTypeDTO;
import com.autel.cloud.pile.base.dto.chargepoint.GetPileInfoByPileDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileListDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPilePageDTO;
import com.autel.cloud.pile.base.dto.pos.GetDeviceGunNumberDTO;
import com.autel.cloud.pile.base.enums.MerchantType;
import com.autel.cloud.pile.base.enums.chargepoint.OverchargingPileFlagEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantRelationEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.chargepoint.DeviceBriefInfoVO;
import com.autel.cloud.pile.base.vo.chargepoint.GetPileInfoByPileVO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.base.vo.fleet.GetPileInfoForFleetVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * opLocationEvse/createEvse
 * <p>
 * [{"locationId":"1609034092779336073","key":"fa07c551-1c26-4858-8734-a82755214d82",
 * "pinCode":"01234567","pinOrSnComplete":true,"thirdPinRequire":false,"pileManner":true,
 * "pileSN":"DE1240B1TEST00002J","pileName":"1w1","brandName":"Autel","brandId":1,"thirdPart":0,"chargeTypes":"DC",
 * "powerType":"DC","productModel":"","curRadio":1,"power":240,
 * "opLocationConnectorDTOs":[{"gunType":1,"connectorId":1,"name":"CCSCombo2"}],"PIN_SN_PASS":true,"publicMark":1}]
 */
@RestController
@RequestMapping("/chargePoint")
@Slf4j
@Validated
@Api(value = "统一桩管理", tags = "统一桩管理")
public class ChargePointMerchantRelationController {

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private OpLocationEvseService opLocationEvseService;

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    @ApiOperation(value = "查询商家类型", notes = "查询商家类型")
    @GetMapping("/merchantType")
    public Result<MerchantType> merchantType(@RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("getLoginUser {}", JSON.toJSONString(LoginUserHolder.getLoginUser()));
        return Result.ofSucceed(MerchantType.NONE);
    }

    @ApiOperation(value = "校验SN", notes = "校验SN")
    @GetMapping("/validate-sn")
    public Result<Boolean> validateSN(@RequestParam("sn") String sn, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        merchantId = getMerchantId(merchantId);
        log.info("merchantId = {}", merchantId);
        chargePointMerchantRelationService.validateSN(sn, merchantId);
        return opLocationEvseService.verifyPileSn(sn);
    }


    @ApiOperation(value = "查询道通充电桩", notes = "查询道通充电桩")
    @GetMapping("/autel")
    public Result<AutelChargePointVO> getPile(@RequestParam("sn") String sn, @RequestParam("pin") String pin) {
        return Result.ofSucceed(chargePointMerchantRelationService.getAutelChargePoint(sn, pin));
    }

    @ApiOperation(value = "为运维服务查询Autel设备基本信息", notes = "为运维服务查询Autel设备基本信息")
    @GetMapping("/selectAutelDeviceInfoForOpsMgmt")
    public Result<SelectAutelDeviceInfoForOpsMgmtVO> selectAutelDeviceInfoForOpsMgmt(@RequestParam("sn") String sn) {

        log.info("===>>> ChargePointMerchantRelationController.selectAutelDeviceInfoForOpsMgmt sn : {}",
                JSON.toJSONString(sn));

        return Result.ofSucceed(chargePointMerchantRelationService.selectAutelDeviceInfoForOpsMgmt(sn));
    }

    @ApiOperation(value = "添加充电桩", notes = "添加充电桩")
    @PostMapping({"/save"})
    public Result<ChargePointMerchantRelationEntity> save(@RequestBody ChargePointDTO chargePointDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId,
                                          @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        log.info("saveChargePoint {}", JSON.toJSONString(chargePointDTO));
        chargePointDTO.setZoneId(zoneId);
        log.info("getLoginUser {}", JSON.toJSONString(LoginUserHolder.getLoginUser()));
        log.info("SellerId {}", LoginUserHolder.getLoginUser().getPayload().getSellerId());
        log.info("TenantId {}", LoginUserHolder.getLoginUser().getTenantId());
        log.info("LoginUser().getId {}", LoginUserHolder.getLoginUser().getId());
        merchantId = getMerchantId(merchantId);
        ChargePointMerchantRelationEntity data = chargePointMerchantRelationService.save(chargePointDTO, merchantId);
        log.info("saveChargePoint result {}", JSON.toJSONString(data));
        return Result.ofSucceed(data);
    }

    @ApiOperation(value = "修改充电桩", notes = "修改充电桩")
    @PostMapping({"/update"})
    public Result<ChargePointMerchantRelationEntity> update(@RequestBody ChargePointDTO chargePointDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId,
                                                            @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        merchantId = getMerchantId(merchantId);

        log.info("===>>> ChargePointMerchantRelationController.update chargePointDTO : {} and merchantId : {} and zoneId : {}",
                JSON.toJSONString(chargePointDTO),
                JSON.toJSONString(merchantId),
                JSON.toJSON(zoneId));

        chargePointDTO.setZoneId(zoneId);
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationService.update(chargePointDTO, merchantId);

        log.info("===>>> ChargePointMerchantRelationController.update chargePointMerchantRelationEntity : {}",
                JSON.toJSONString(chargePointMerchantRelationEntity));

        UpdatePileNameDTO updatePileNameDTO = new UpdatePileNameDTO();

        updatePileNameDTO.setMerchantId(chargePointMerchantRelationEntity.getMerchantId());
        updatePileNameDTO.setPileSn(chargePointDTO.getSn());
        updatePileNameDTO.setPileName(chargePointDTO.getName());
        updatePileNameDTO.setPower(chargePointDTO.getRatedPower());
        updatePileNameDTO.setPowerType(chargePointDTO.getPowerType());
        updatePileNameDTO.setCategory(chargePointMerchantRelationEntity.getCategory());
        updatePileNameDTO.setPhase(chargePointDTO.getPhases());
        updatePileNameDTO.setPin(chargePointMerchantRelationEntity.getPin());

        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointMerchantRelationEntity.getOverchargingPileFlag())
                && ObjectUtils.isNotEmpty(chargePointMerchantRelationEntity.getConnectors())) {
            List<OpLocationConnectorDTO> opLocationConnectorDTOs = new ArrayList<>();
            for (Connector connector : chargePointMerchantRelationEntity.getConnectors()) {
                OpLocationConnectorDTO dto = new OpLocationConnectorDTO();
                BeanUtils.copyProperties(connector, dto);
                if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(connector.getPower())) {
                    dto.setPower(connector.getPower().doubleValue());

                }
                dto.setConnectorId(connector.getConnectorId().toString());
                dto.setGunType(connector.getConnectorType());
                opLocationConnectorDTOs.add(dto);
            }
            List<OpLocationConnectorDTO> collect = opLocationConnectorDTOs.stream().sorted(Comparator.comparing(OpLocationConnectorDTO::getConnectorId)).collect(Collectors.toList());
            updatePileNameDTO.setOpLocationConnectorDTOs(collect);
        }

        opLocationEvseService.updatePileName(updatePileNameDTO);
        return Result.ofSucceed(chargePointMerchantRelationEntity);
    }

    @ApiOperation(value = "删除充电桩", notes = "删除充电桩  删除的是关系")
    @PostMapping("/delete")
    public Result<Void> remove(@RequestBody RemoveChargePointDTO chargePointDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        merchantId = getMerchantId(merchantId);
        log.info("LoginUser().getRoles {}", LoginUserHolder.getLoginUser().getRoles());
        chargePointDTO.setMerchantType(pileUserServiceAdapter.getMerchantTypeById(merchantId));
        chargePointMerchantRelationService.remove(chargePointDTO, merchantId);
        return Result.ofSucceed();
    }

    @PostMapping(value = "/findByFuzzNameOrSN")
    @ApiOperation(value = "名称或sn 模糊查询")
    public Result<List<ChargePointVO>> findByFuzzNameOrSN(@RequestBody ChargePointFuzzQueryDTO chargePointQueryDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("findByFuzzNameOrSN {}", JSON.toJSONString(chargePointQueryDTO));
        merchantId = getMerchantId(merchantId);
        List<ChargePointVO> byFuzzNameOrSN = chargePointMerchantRelationService.findByFuzzNameOrSN(chargePointQueryDTO, merchantId);
        log.info("findByFuzzNameOrSN result {}", JSON.toJSONString(byFuzzNameOrSN));
        return Result.ofSucceed(byFuzzNameOrSN);
    }

    @PostMapping(value = "/findChargePointSource")
    @ApiOperation(value = "名称或sn 模糊查询")
    public Result<List<ChargePointSourceVO>> findChargePointSource(@Valid @RequestBody ChargePointSourceFuzzQueryDTO chargePointQueryDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("findChargePointSource {}", JSON.toJSONString(chargePointQueryDTO));
        merchantId = getMerchantId(merchantId);
//        chargePointQueryDTO.setRelationship(pileUserServiceAdapter.getMerchantTypeById(merchantId).getKey());
        List<ChargePointSourceVO> byFuzzNameOrSN = chargePointMerchantRelationService.findChargePointSource(chargePointQueryDTO, merchantId);
        log.info("findChargePointSource result {}", JSON.toJSONString(byFuzzNameOrSN));
        return Result.ofSucceed(byFuzzNameOrSN);
    }

    @GetMapping(value = "/modifyPileEvseId")
    @ApiOperation(value = "修改桩的id(脏数据id是字符串)")
    public Result<OpLocationPileEvseElasticDTO> modifyPileEvseId(@RequestParam(value = "pileEvseId")String pileEvseId,
                                                                 @RequestParam(value = "pileSn")String pileSn
    ) {
        return Result.ofSucceed(chargePointMerchantRelationService.modifyPileEvseId(pileEvseId,pileSn));
    }

    /**
     * 绑license分页查询桩
     */
    @PostMapping("/subscriptPage")
    @ApiOperation(value = "统一桩管理列表查询")
    @CustomizedDateTime
    public Result<IPage<ChargePointVO>> findPage(@RequestBody ChargePointFuzzQueryDTO chargePointQueryDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId,
                                                 @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        log.info("findPage {}", JSON.toJSONString(chargePointQueryDTO));
        merchantId = getMerchantId(merchantId);
        IPage<ChargePointVO> byPage = chargePointMerchantRelationService.findByPage(chargePointQueryDTO, merchantId, zoneId);
        log.info("findPage result {}", JSON.toJSONString(byPage));
        return Result.ofSucceed(byPage);
    }


    private Long getMerchantId(Long merchantId) {
        return Optional.ofNullable(merchantId).orElseGet(() -> LoginUserHolder.getLoginUser().getPayload().getSellerId());
//        return LoginUserHolder.getLoginUser().getPayload().getSellerId();
    }

    /**
     * 查询商家存在的充电桩交直流类型
     */
    @PostMapping("/existsPowerTypes")
    @ApiOperation(value = "查询商家存在的充电桩交直流类型")
    public Result<ChargePointPowerTypesVO> existsPowerTypes(@RequestHeader(required = false, value = "merchantId") Long merchantId,
                                                           @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        merchantId = getMerchantId(merchantId);
        ChargePointPowerTypesVO byPage = chargePointMerchantRelationService.existsPowerTypes(merchantId, zoneId);
        log.info("existsPowerTypes result {}", JSON.toJSONString(byPage));
        return Result.ofSucceed(byPage);
    }

    /**
     * 分页查询
     */
    @PostMapping("/page")
    @ApiOperation(value = "统一桩管理列表带过期时间查询")
    public Result<IPage<ChargePointAssetVO>> findChargePointsByPage(@RequestBody ChargePointFuzzQueryDTO chargePointQueryDTO, @RequestHeader(required = false, value = "merchantId") Long merchantId,
                                                                    @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        log.info("findChargePointsByPage {}", JSON.toJSONString(chargePointQueryDTO));
        merchantId = getMerchantId(merchantId);
        IPage<ChargePointAssetVO> byPage = chargePointMerchantRelationService.findChargePointsByPage(chargePointQueryDTO, merchantId, zoneId);
        log.info("findChargePointsByPage result {}", JSON.toJSONString(byPage));
        return Result.ofSucceed(byPage);
    }

    @PostMapping("/exportChargePoint")
    @ApiOperation(value = "充电桩导出")
    public void exportChargePointsByPage(@RequestBody ChargePointExportDTO chargePointExportDTO, HttpServletRequest request, HttpServletResponse response,
                                         @RequestHeader(required = false, value = "merchantId") Long merchantId,
                                         @RequestHeader(required = false, value = "X-Timezone") String zoneId) throws IOException {
        log.info("exportChargePointsByPage chargePointExportDTO: {}", JSON.toJSONString(chargePointExportDTO));
        merchantId = getMerchantId(merchantId);
        chargePointMerchantRelationService.exportChargePoint(chargePointExportDTO, request, response, merchantId, zoneId);
    }

    @ApiOperation(value = "桩记录")
    @GetMapping("/record/{id}")
    public Result<ChargePointRecordVO> record(@PathVariable("id") Long id, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("record {}", id);
        merchantId = getMerchantId(merchantId);
        ChargePointRecordVO record = chargePointMerchantRelationService.record(id, merchantId);
        log.info("record result {}", JSON.toJSONString(record));
        return Result.ofSucceed(record);
    }

    @Deprecated
    @GetMapping("/detail/{id}")
    @ApiOperation(value = "桩详情")
    public Result<ChargePointDetailVO> detail(@PathVariable("id") Long id, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("detail {}", id);
        merchantId = getMerchantId(merchantId);
        ChargePointDetailVO detail = chargePointMerchantRelationService.detail(id, merchantId);
        log.info("detail result {}", JSON.toJSONString(detail));
        return Result.ofSucceed(detail);
    }

    @Deprecated
    @GetMapping("/detailV2")
    @ApiOperation(value = "桩详情(V2版本，可根据终端SN查出终端信息)")
    @CustomizedDateTime
    public Result<ChargePointDetailVO> detailV2(@RequestParam("sn") String sn, @RequestHeader(required = false, value = "merchantId") Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationController.detailV2 sn : {} and merchantId : {}",
                JSON.toJSONString(sn),
                JSON.toJSONString(merchantId));

        merchantId = getMerchantId(merchantId);
        ChargePointDetailVO detail = chargePointMerchantRelationService.detailV2(sn, merchantId);

        log.info("===>>> ChargePointMerchantRelationController.detailV2 detail : {}",
                JSON.toJSONString(detail));

        return Result.ofSucceed(detail);
    }

    @PostMapping("/detailV3")
    @ApiOperation(value = "桩详情(V3版本，可根据终端SN查出终端信息)")
    @CustomizedDateTime
    public Result<ChargePointDetailVO> detailV3(@RequestBody ChargePointDetailDTO chargePointDetailDTO) {

        log.info("===>>> ChargePointMerchantRelationController.detailV3 chargePointDetailDTO : {}",
                JSON.toJSONString(chargePointDetailDTO));

        Long merchantId = getMerchantId(chargePointDetailDTO.getMerchantId());
        ChargePointDetailVO chargePointDetailVO = chargePointMerchantRelationService.detailV2(chargePointDetailDTO.getSn(), merchantId);

        log.info("===>>> ChargePointMerchantRelationController.detailV3 chargePointDetailVO : {}",
                JSON.toJSONString(chargePointDetailVO));

        return Result.ofSucceed(chargePointDetailVO);
    }

    @GetMapping("/detailBySn/{sn}")
    @ApiOperation(value = "桩详情-配置-常规信息查询")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "桩的sn", name = "sn", example = "A123131", paramType = "path")
    })
    public Result<ChargePointDetailVO> detailBySn(@PathVariable("sn") String pileSn) {
        log.info("detailBySn {}", pileSn);
        ChargePointDetailVO detail = chargePointMerchantRelationService.detailBySn(pileSn);
        log.info("detailBySn result {}", JSON.toJSONString(detail));
        return Result.ofSucceed(detail);
    }

    @ApiOperation(value = "导入充电桩（new）", notes = "导入充电桩(new)")
    @PostMapping("/import")
    @OperationActionLog(action = "import", object = "pile")
    public Object newImport(@RequestParam("file") MultipartFile file, @RequestHeader(required = false, value = "merchantId") Long merchantId, @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        log.info("newImport {} {}", merchantId, zoneId);
        merchantId = getMerchantId(merchantId);
        return Result.ofSucceed(chargePointMerchantRelationService.newImport(zoneId, merchantId, file));
    }

    @ApiOperation(value = "下载导入错误文件", notes = "下载导入错误文件")
    @PostMapping("/downloadImportErrorFile")
    @OperationActionLog(action = "export", object = "pile")
    public void downloadImportErrorFile(@RequestHeader(value = "Accept-Language", defaultValue = "en-US") String language, HttpServletResponse response) {
        chargePointMerchantRelationService.downloadImportErrorFile(language, response);
    }

    @ApiOperation(value = "下载excel模板", notes = "下载excel模板")
    @GetMapping("/template")
    public void downloadTempExcel(HttpServletResponse response, @RequestHeader(value = "Accept-Language", defaultValue = "en-US") String language, @RequestHeader(required = false, value = "merchantId") Long merchantId) throws IOException {
        chargePointMerchantRelationService.downloadTempExcel(response, language);
    }


    @ApiOperation(value = "桩绑定license", notes = "桩绑定license")
    @PostMapping("/license")
    public Result<Boolean> license(@RequestBody List<ChargePointLicenseDTO> chargePointLicenseDTOList, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        merchantId = getMerchantId(merchantId);
        log.info("license {} {}", merchantId, JSON.toJSONString(chargePointLicenseDTOList));
        return Result.ofSucceed(chargePointMerchantRelationService.license(chargePointLicenseDTOList, merchantId, LoginUserHolder.getLoginUser().getId()));
    }

    /*
    @ApiOperation(value = "查询道通充电桩许可证过期时间", notes = "查询道通充电桩许可证过期时间")
    @PostMapping("/findChargePointLicense")
    @ApiParam(value = "tb_agreement_function的service_id", example = "qy001")
    public Result<List<ChargePointLicenseVO>> findChargePointLicense(@RequestBody List<String> snList, @RequestParam(required = false, value = "serviceId", defaultValue = "") String serviceId, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        merchantId = getMerchantId(merchantId);
        log.info("findLastExpireTimeChargePointLicense {} {}", merchantId, JSON.toJSONString(snList));
        List<ChargePointLicenseVO> lastExpireBindRelations = chargePointService.findLastExpireTimeChargePointLicense(snList, serviceId, merchantId);
        log.info("findLastExpireTimeChargePointLicense  {} result {}", merchantId, JSON.toJSONString(lastExpireBindRelations));
        return Result.ofSucceed(lastExpireBindRelations);
    }
 */

    @PostMapping(value = "/findBySN")
    @ApiOperation(value = "根据sn，查询CPO、运维商、经销商名称及绑定时间接口")
    public Result<ChargePointVO> findBySN(@RequestParam("sn") String sn, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("findBySN {} {}", merchantId, sn);
        merchantId = Optional.ofNullable(merchantId).orElseGet(() -> LoginUserHolder.getLoginUser().getPayload().getSellerId()); // app 用户也会使用 不会哟sellerId
        ChargePointVO list = chargePointMerchantRelationService.findChargePointBySN(sn, merchantId);
        log.info("findBySN result {} {}", merchantId, JSON.toJSONString(list));
        return Result.ofSucceed(list);
    }


    @PostMapping(value = "/findBySNs")
    @ApiOperation(value = "根据sn列表，批量查询CPO、运维商、经销商名称及绑定时间接口")
    public Result<List<ChargePointVO>> findBySNs(@RequestBody Set<String> snSet, @RequestHeader(required = false, value = "merchantId") Long merchantId) {
        log.info("findBySNs  {} {}", merchantId, JSON.toJSONString(snSet));
        merchantId = Optional.ofNullable(merchantId).orElseGet(() -> LoginUserHolder.getLoginUser().getPayload().getSellerId()); // app 用户也会使用 不会哟sellerId
        List<ChargePointVO> list = chargePointMerchantRelationService.findChargePointBySNs(snSet, merchantId);
        log.info("findBySNs result {} {}", merchantId, JSON.toJSONString(list));
        return Result.ofSucceed(list);
    }

    @PostMapping(value = "/findSubRequiredPile")
    @ApiOperation(notes = "您有6台桩未订阅充电云计划，无法正常充电，请尽快完成订阅", value = "全局常驻未订阅/已过期提示")
    public Result<Integer> findSubRequiredPile(@RequestHeader(required = false, value = "merchantId") Long merchantId, @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        merchantId = getMerchantId(merchantId);
        log.info("findSubRequiredPile  {}", merchantId);
        List<ImminentExpireChargePointDTO> list = chargePointMerchantRelationService.findSubRequiredPile(merchantId, zoneId);
        log.info("findSubRequiredPile result {} {}", merchantId, JSON.toJSONString(list));
        return Result.ofSucceed(list.size());
    }

    @PostMapping("/subscriptPageBySellerId")
    @ApiOperation(value = "通过商家id查询商户充电桩信息(CRM调用 header必填)")
    public Result<IPage<ChargePointVO>> findPageBySellerId(@RequestBody ChargePointFuzzQueryDTO chargePointQueryDTO, @RequestHeader(value = "merchantId", required = false) Long merchantId,
                                                           @RequestHeader(required = false, value = "X-Timezone") String zoneId) {
        log.info("subscriptPageBySellerId {}", JSON.toJSONString(chargePointQueryDTO));
        IPage<ChargePointVO> byPage = chargePointMerchantRelationService.subscriptPageBySellerId(chargePointQueryDTO, merchantId, zoneId);
        log.info("subscriptPageBySellerId result {}", JSON.toJSONString(byPage));
        return Result.ofSucceed(byPage);
    }

    @PostMapping("/snSubscribeDisplaySet")
    @ApiOperation(value = "设置商户资产订阅概览显示配置")
    public Result<List<String>> snSubscribeDisplaySet(@RequestBody SnSubscribeDisplaySettingDto displaySettingDto) {
        log.info("snSubscribeDisplaySet {}", JSON.toJSONString(displaySettingDto));
        return chargePointMerchantRelationService.snSubscribeDisplaySet(displaySettingDto);
    }

    @GetMapping("/getSnSubscribeDisplaySetting")
    @ApiOperation(value = "查询商户资产订阅概览显示配置")
    public Result<List<String>> getSnSubscribeDisplaySetting() {
        return chargePointMerchantRelationService.getSnSubscribeDisplaySetting();
    }

    @GetMapping("/subscriptionStatusCount")
    @ApiOperation(value = "通过商户id查询充电桩的订阅状态数量(CRM调用)")
    public Result<SubscriptionStatusCountVO> subscriptionStatusCount(@RequestParam(value = "merchantId") Long merchantId) {
        log.info("subscriptionStatusCount {}", merchantId);
        SubscriptionStatusCountVO result = chargePointMerchantRelationService.subscriptionStatusCount(merchantId);
        log.info("subscriptionStatusCount result {}", JSON.toJSONString(result));
        return Result.ofSucceed(result);
    }

    @PostMapping("/findSellerByCount")
    @ApiOperation("根据桩数量筛选商家")
    public Set<String> findSellerByCount(@RequestBody RangeConditionDto rangeConditionDto) {
        return chargePointMerchantRelationService.findSellerByCount(rangeConditionDto);
    }

    @GetMapping("/getConnectorNumber")
    @ApiOperation(value = "查询枪数量")
    public Result<Integer> getConnectorNumber(@RequestParam(value = "sn") String sn) {
        return Result.ofSucceed(chargePointMerchantRelationService.getConnectorNumber(sn));
    }

    /**
     * @param getDeviceTypeDTO
     * @return
     * @see com.autel.cloud.pile.base.enums.chargepoint.DeviceTypeEnum
     */
    @PostMapping("/getDeviceType")
    @ApiOperation("获得设备类型")
    public Result<Integer> getDeviceType(@RequestBody GetDeviceTypeDTO getDeviceTypeDTO) {

        log.info("===>>> ChargePointMerchantRelationController.getDeviceType getDeviceTypeDTO : {}",
                JSON.toJSONString(getDeviceTypeDTO));

        return Result.ofSucceed(chargePointMerchantRelationService.getDeviceType(getDeviceTypeDTO));
    }

    @PostMapping("/getPileInfoByPile")
    @ApiOperation("获得桩侧上报的超充桩数据")
    public Result<GetPileInfoByPileVO> getPileInfoByPile(@RequestBody GetPileInfoByPileDTO getPileInfoByPileDTO) {

        log.info("===>>> ChargePointMerchantRelationController.getPileInfoByPile getPileInfoByPileDTO : {}",
                JSON.toJSONString(getPileInfoByPileDTO));

        return Result.ofSucceed(chargePointMerchantRelationService.getPileInfoByPile(getPileInfoByPileDTO));
    }

    @PostMapping("/getDeviceGunNumber")
    @ApiOperation("获得设备枪号信息")
    public Result<Map<String, List<Integer>>> getDeviceGunNumber(@RequestBody GetDeviceGunNumberDTO getDeviceGunNumberDTO) {

        log.info("===>>> ChargePointMerchantRelationController.getDeviceGunNumber getDeviceGunNumberDTO : {}",
                JSON.toJSONString(getDeviceGunNumberDTO));

        return Result.ofSucceed(chargePointMerchantRelationService.getDeviceGunNumber(getDeviceGunNumberDTO));
    }

    @GetMapping("/getDeviceBriefInfo")
    @ApiOperation(value = "获得设备简要信息")
    public Result<DeviceBriefInfoVO> getDeviceBriefInfo(@RequestParam(value = "sn") String sn,
                                                        @RequestHeader(required = false, value = "merchantId") Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationController.getDeviceBriefInfo sn : {}",
                JSON.toJSONString(sn));

        return Result.ofSucceed(chargePointMerchantRelationService.getDeviceBriefInfo(sn, this.getMerchantId(merchantId)));
    }

    @PostMapping("/getDeviceInfoForPos")
    @ApiOperation("为POS机查询桩信息——内部调用")
    public Result<List<DeviceInfoForPosVO>> getDeviceInfoForPos(@RequestBody DeviceInfoForPosDTO deviceInfoForPosDTO) {

        log.info("===>>> ChargePointMerchantRelationController.getDeviceInfoForPos deviceInfoForPosDTO : {}",
                JSON.toJSONString(deviceInfoForPosDTO));

        return Result.ofSucceed(chargePointMerchantRelationService.getDeviceInfoForPos(deviceInfoForPosDTO));
    }

    @PostMapping("/synchronizeDeviceInfoForPos")
    @ApiOperation("为POS机同步设备数据——内部调用")
    public Result<List<SynchronizeDeviceInfoForPosVO>> synchronizeDeviceInfoForPos(@RequestBody SynchronizeDeviceInfoForPosDTO synchronizeDeviceInfoForPosDTO){

        log.info("===>>> ChargePointMerchantRelationController.synchronizeDeviceInfoForPos synchronizeDeviceInfoForPosDTO : {}",
                JSON.toJSONString(synchronizeDeviceInfoForPosDTO));

        return Result.ofSucceed(chargePointMerchantRelationService.synchronizeDeviceInfoForPos(synchronizeDeviceInfoForPosDTO));
    }

    @ApiOperation(value = "订阅概览导出")
    @PostMapping("/exportLicenceView")
    @SneakyThrows
    public void exportExcel(HttpServletRequest request, HttpServletResponse response) {
        chargePointMerchantRelationService.exportExcel(request, response);
    }

    @PostMapping("/getPileInfoForFleet")
    @ApiOperation(value = "根据设备sn查询设备信息", notes = "根据设备sn查询设备信息")
    public Result<List<GetPileInfoForFleetVO>> getPileInfoForFleet(@RequestBody List<String> pileSns) {

        log.info("===>>> ChargePointMerchantRelationController.getPileInfoForFleet pileSns : {}",
                JSON.toJSONString(pileSns));

        return Result.ofSucceed(chargePointMerchantRelationService.getPileInfoForFleet(pileSns));
    }

    @GetMapping("/getTerminalDeviceData")
    @ApiOperation(value = "获得商家某个主机下辖的终端信息")
    public Result<List<TerminalDeviceDataVO>> getTerminalDeviceData(@RequestParam(value = "hostSn") String hostSn,
                                                                    @RequestParam(value = "sellerId") Long sellerId) {

        log.info("===>>> ChargePointMerchantRelationController.getTerminalDeviceData hostSn : {} and sellerId : {}",
                JSON.toJSONString(hostSn),
                JSON.toJSONString(sellerId));

        return Result.ofSucceed(chargePointMerchantRelationService.getTerminalDeviceData(hostSn, sellerId));
    }

    @PostMapping("/queryPagePileByPrivilege")
    @ApiOperation(value = "获取有权限的桩，分页查询", notes = "按运营商id,桩名、sn查询")
    public Result<Page<PileBaseVO>> queryPagePileByPrivilege(@RequestBody @Valid QueryPilePageDTO paramDTO){
        return Result.ofSucceed(chargePointMerchantRelationService.queryPagePileByPrivilege(paramDTO));
    }

    @PostMapping("/queryPileBySn")
    @ApiOperation(value = "获取桩列表", notes = "按运营商id,桩名、sn查询")
    public Result<List<PileBaseVO>> queryPileBySn(@RequestBody @Valid QueryPileListDTO paramDTO){

        log.info("===>>> ChargePointMerchantRelationController.queryPileBySn  QueryPileListDTO : {}",
                JSON.toJSONString(paramDTO));
        return Result.ofSucceed(chargePointMerchantRelationService.queryPileBySn(paramDTO));
    }
}
