package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.service.SubscribeBenefitFuncService;
import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.dto.AgreementFunctionDetailDto;
import com.autel.cloud.pile.base.dto.BusinessReqDto;
import com.autel.cloud.pile.base.dto.BusinessRespDto;
import com.autel.cloud.pile.base.dto.UnUsedLicenceInfoDto;
import com.autel.cloud.pile.base.dto.subscribe.*;
import com.autel.cloud.pile.base.infrastructure.mapper.TbOrderRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbOrderRecordEntity;
import com.autel.cloud.pile.base.dto.subscribe.LicenceGracePeriodExtendedDto;
import com.autel.cloud.pile.base.dto.subscribe.*;
import com.autel.cloud.pile.base.vo.AgreementFunctionReqVo;
import com.autel.cloud.pile.user.api.constant.Constant;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.util.List;

/**
 * @description 查询桩权益相关接口
 * @auther A23204
 * @datetime 2023/6/9 11:39
 */
@RestController
@RequestMapping("/rights")
@Slf4j
@Validated
@Api(tags = "桩权益相关接口")
public class PileRightsController {

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Autowired
    private SubscribeBenefitFuncService subscribeBenefitFuncService;


    @ApiOperation(value = "根据商户id：tenantId，桩列表：pileSn，status，查询桩信息，tenantId，pileSn两个参数二选一；status非必填，默认所有")
    @PostMapping("/pile/getPileRights")
    Result<List<AgreementFunctionDetailDto>> getHaveRightsPileList(@Valid @RequestBody AgreementFunctionReqVo agreementFunctionReqVo) {
        return Result.ofSucceed(subscribePileRightsService.getHaveRightsPileList(agreementFunctionReqVo));
    }


    /** 桩是否可用
     * @param pileSn
     * @return
     */
    @ApiOperation(value = "根据账号，查询桩是否可用")
    @GetMapping("/pile/pileIsAvailable")
    Result<Boolean> pileIsAvailable(@RequestParam("pileSn") String pileSn) {
        return subscribePileRightsService.pileIsAvailable(pileSn);
    }

    /** 桩是否可用 带枪号
     * @param pileSn
     * @return
     */
    @ApiOperation(value = "根据桩+枪号，查询枪是否可用")
    @GetMapping("/pile/pileWithGunIsAvailable")
    Result<Boolean> pileWithGunIsAvailable(@RequestParam("pileSn") String pileSn, @RequestParam("gunNum") Integer gunNum) {
        return subscribePileRightsService.pileWithGunIsAvailable(pileSn, gunNum);
    }

    /** 桩是否可用, 预约查询失效时间，增加续约的场景。
     * @param pileSn
     * @return Long 过期时间。
     */
    @ApiOperation(value = "根据账号，查询桩是否可用")
    @GetMapping("/pile/pileRightsWithExtension")
    Result<Long> pileRightsWithExtension(@RequestParam("pileSn") String pileSn) {
        return subscribePileRightsService.pileRightsWithExtension(pileSn);
    }

    /** 根据商户id，查询未使用licence明细信息列表
     * @return
     */
    @ApiOperation(value = "根据商户id，查询未使用licence明细信息列表")
    @GetMapping("/pile/queryUnUsedLicence")
    Result<List<UnUsedLicenceInfoDto>> queryUnUsedLicence() {
        return subscribePileRightsService.queryUnUsedLicence(null, null);
    }

    @ApiOperation("根据许可证数量查询商户")
    @PostMapping("/findSellerIdByLicenseNum")
    Result<BusinessRespDto> findSellerIdByLicenseNum(@RequestBody BusinessReqDto businessReqDto) {
        return Result.ofSucceed(subscribePileRightsService.findSellerIdByLicenseNum(businessReqDto));
    }

    @ApiOperation(value = "延长licence宽限期")
    @PostMapping("/licence/extend")
    Result<List<String>> extendGracePeriod(@Valid @RequestBody LicenceGracePeriodExtendedDto licenceGracePeriodExtendedDto) {
        return subscribePileRightsService.extendGracePeriod(licenceGracePeriodExtendedDto);
    }



    /** 服务权益配置元数据查询
     * @return
     */
    @ApiOperation(value = "服务权益配置元数据查询")
    @GetMapping("/benefit/list")
    Result<List<SubscribeBenefitMetaDto>> benefitConfigQuery() {
        return subscribePileRightsService.benefitConfigQuery();
    }

    /** 订阅功能点控制列表查询
     * @return
     */
    @ApiOperation(value = "功能点控制元数据查询")
    @GetMapping("/function/list")
    Result<List<SubscribeFuncConfDto>> functionConfigQuery() {
        return subscribePileRightsService.functionConfigQuery();
    }


    @ApiOperation(value = "功能点控制编辑")
    @PostMapping("/function/edit")
    Result<Boolean> functionEdit(@Valid @RequestBody FunctionModifyDto functionModifyDto) {
        return subscribePileRightsService.functionEdit(functionModifyDto);
    }

    @ApiOperation(value = "服务权益编辑")
    @PostMapping("/benefit/edit")
    Result<Boolean> benefitFuncEdit(@Valid @RequestBody BenefitEditDto benefitEditDto) {
        return subscribeBenefitFuncService.benefitFuncEdit(benefitEditDto);
    }

    /** 权益-功能点控制列表查询
     * @return
     */
    @ApiOperation(value = "权益-功能点控制配置列表查询")
    @GetMapping("/benefitFuncConfig/list")
    Result<List<BenefitFunctionRelationDto>> benefitFuncList() {
        return subscribePileRightsService.benefitFuncList();
    }

    @ApiOperation(value = "查询桩所具有的功能控制点")
    @PostMapping("/pile/pileFunction")
    Result<List<PileBenefitFunctionRespDto>> getPileFunction(@Valid @RequestBody BenefitFunctionReqDto benefitFunctionReqDto) {
        return subscribePileRightsService.getPileFunction(benefitFunctionReqDto);
    }


    @ApiOperation(value = "查询桩订阅生效中的功能控制点集合")
    @PostMapping("/pile/functionAvailableList")
    Result<List<BenefitAvailableFuncListRespDto>> functionAvailableList(@Valid @RequestBody BenefitAvailableFuncListReqDto benefitAvailableFuncListReqDto) {
        return subscribePileRightsService.functionAvailableList(benefitAvailableFuncListReqDto);
    }

    /** test 权益查询1
     * @return
     */
    //@ApiOperation(value = "权益id查询功能点列表")
    @GetMapping("/benefitFuncConfig/test1")
    Result<List<String>> test1() {
        List<String> functionListByBenefitId = subscribePileRightsService.getFunctionListByBenefitId("Benefits_Pro");
        return Result.ofSucceed(functionListByBenefitId);
    }

    /** test 权益查询2
     * @return
     */
    //@ApiOperation(value = "功能点查询权益列表")
    @GetMapping("/benefitFuncConfig/test2")
    Result<List<String>> test2() {
        List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.GUN_SEARCH);
        return Result.ofSucceed(functionBenefitList);
    }

    @ApiOperation(value = "订阅灰度商家修改服务权益id")
    @PostMapping("/serviceId/update")
    Result<Boolean> updateServiceIdBySellerId(@RequestParam("sellerId") String sellerId) {
        return subscribeBenefitFuncService.updateServiceIdBySellerId(sellerId);
    }

}
