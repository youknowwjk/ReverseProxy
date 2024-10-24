package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.LicenseManagementService;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.dto.GetLicenseDetailPageByOrderIdDTO;
import com.autel.cloud.pile.base.dto.UnUsedLicenceInfoDto;
import com.autel.cloud.pile.base.dto.license.DoNotRemindAgainDTO;
import com.autel.cloud.pile.base.dto.license.LicenseDetailDTO;
import com.autel.cloud.pile.base.vo.license.LicenseDetailVO;
import com.autel.cloud.pile.base.vo.license.LicenseGroupListVO;
import com.autel.cloud.pile.base.vo.license.LicenseStatusCountByOrderIdVO;
import com.autel.cloud.pile.base.vo.license.LicenseStatusCountVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/licenseManagement")
@Api(tags = "许可证管理")
@Slf4j
@Validated
public class LicenseManagementController {

    @Autowired
    private LicenseManagementService licenseManagementService;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @GetMapping("/doNotRemindAgainViewEnable")
    @ApiOperation(value = "是否需要给登录的用户弹出提醒页签(true:需要弹出; false:不需要弹出)")
    public Result<Boolean> doNotRemindAgainViewEnable() {
        return Result.ofSucceed(licenseManagementService.doNotRemindAgainViewEnable());
    }

    @PostMapping("/doNotPromptAgainOperation")
    @ApiOperation(value = "不再提示按钮接口")
    public Result<Boolean> doNotPromptAgainOperation(@Validated @RequestBody DoNotRemindAgainDTO doNotRemindAgainDTO) {

        log.info("===>>>LicenseManagementController.doNotPromptAgainOperation doNotRemindAgainDTO : {}",
                JSON.toJSONString(doNotRemindAgainDTO));

        return Result.ofSucceed(licenseManagementService.doNotPromptAgainOperation(doNotRemindAgainDTO));
    }

    @ApiOperation(value = "未使用licence明细信息列表")
    @GetMapping("/queryUnUsedLicenceBySkuCode")
    Result<List<UnUsedLicenceInfoDto>> queryUnUsedLicence(@RequestParam("skuCode") String skuCode, @RequestHeader(required = false, value = "merchantId") Long merchantId) {

        log.info("===>>>LicenseManagementController.queryUnUsedLicence skuCode : {}",
                JSON.toJSONString(skuCode));

        return subscribePileRightsService.queryUnUsedLicence(skuCode, merchantId);
    }


    @PostMapping("/getLicenseList")
    @ApiOperation(value = "许可证管理列表")
    public Result<List<LicenseGroupListVO>> getLicenseList(@Validated @RequestBody PageDTO pageDTO) {

        log.info("===>>>LicenseManagementController.getLicenseList pageDTO : {}",
                JSON.toJSONString(pageDTO));

        return Result.ofSucceed(licenseManagementService.getLicenseList(pageDTO));
    }

    @GetMapping("/getLicenseStatusCount")
    @ApiOperation(value = "查询某个商品sku下的许可证状态个数")
    public Result<LicenseStatusCountVO> getLicenseStatusCount(@RequestParam(value = "skuCode", required = false) String skuCode,@RequestParam(value = "sellerId",required = false) String sellerId) {

        log.info("===>>> LicenseManagementController.getLicenseStatusCount skuCode : {}",
                JSON.toJSONString(skuCode));

        return Result.ofSucceed(licenseManagementService.getLicenseStatusCount(skuCode,sellerId));
    }

    @GetMapping("/getStatusCountBySellerId")
    @ApiOperation(value = "查询商家的许可证状态个数")
    public Result<LicenseStatusCountVO> getStatusCountBySellerId() {
        return Result.ofSucceed(licenseManagementService.getStatusCountBySellerId());
    }

    @GetMapping("/getLicenseStatusCountByOrderId")
    @ApiOperation(value = "根据订单id 查询许可证状态数量")
    public Result<LicenseStatusCountByOrderIdVO> getLicenseStatusCountByOrderId(@RequestParam("orderId") String orderId) {

        log.info("getLicenseStatusCountByOrderId orderId : {}",
                orderId);

        return Result.ofSucceed(licenseManagementService.getLicenseStatusCountByOrderId(orderId));
    }

    @PostMapping("/getLicenseDetailPage")
    @ApiOperation(value = "分页查询许可证详情列表")
    public Result<Page<LicenseDetailVO>> getLicenseDetailPage(@Validated @RequestBody LicenseDetailDTO licenseDetailDTO) {

        log.info("===>>>LicenseManagementController.getLicenseDetailPage licenseDetailDTO : {}",
                JSON.toJSONString(licenseDetailDTO));

        return Result.ofSucceed(licenseManagementService.getLicenseDetailPage(licenseDetailDTO));
    }

    @PostMapping("/getLicenseDetailPageByOrderId")
    @ApiOperation(value = "根据订单id查询许可证信息")
    public Result<Page<LicenseDetailVO>> getLicenseDetailPageByOrderId(@Validated @RequestBody GetLicenseDetailPageByOrderIdDTO getLicenseDetailPageByOrderIdDTO) {

        log.info("getLicenseDetailPageByOrderId getLicenseDetailPageByOrderIdDTO : {}",
                JSON.toJSONString(getLicenseDetailPageByOrderIdDTO));

        return Result.ofSucceed(licenseManagementService.getLicenseDetailPageByOrderId(getLicenseDetailPageByOrderIdDTO));
    }

    @GetMapping("/restoreLicenseStatus")
    @ApiOperation(value = "修复license状态字段")
    public Result<Integer> restoreLicenseStatus(@RequestParam("secretKey") String secretKey) {

        log.info("===>>> LicenseManagementController.restoreLicenseStatus secretKey : {}",
                JSON.toJSONString(secretKey));

        if (!"Autel".equalsIgnoreCase(secretKey)) {
            return Result.ofSucceed(0);
        }
        return Result.ofSucceed(licenseManagementService.restoreLicenseStatus());
    }

    @GetMapping("/getLicenseListBySellerId")
    @ApiOperation(value = "通过商家id查询许可证列表")
    public Result<List<LicenseGroupListVO>> getLicenseListBySellerId(@RequestParam("sellerId") String sellerId) {

        log.info("getLicenseListBySellerId sellerId : {}",
                JSON.toJSONString(sellerId));

        return Result.ofSucceed(licenseManagementService.getLicenseListBySellerId(sellerId));
    }
}

