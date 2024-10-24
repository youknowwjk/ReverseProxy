package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ordercenter.vo.ReturnOrderRespVO;
import com.autel.cloud.pile.base.domain.service.LicenseManagementService;
import com.autel.cloud.pile.base.vo.license.LicenseDetailVO;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnLicenseVO;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnSkuCountVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/refund")
@Api(tags = "退货管理")
@Slf4j
@Validated
public class RefundController {

    @Autowired
    private LicenseManagementService licenseManagementService;

    @PostMapping("/sku/check")
    @ApiOperation(value = "sf发起退货时校验sku可退货许可证对应的数量")
    public Result<List<ReturnSkuCountVO>> getReturnSkuCountByOrderId(@RequestParam("orderId") Long orderId) {
        return Result.ofSucceed(licenseManagementService.getReturnSkuCountByOrderId(orderId));
    }

    @PostMapping
    @ApiOperation(value = "退回license")
    public Result<ReturnOrderRespVO> ReturnLicense(@RequestBody ReturnLicenseVO returnLicenseVO) {
        return Result.ofSucceed(licenseManagementService.ReturnLicense(returnLicenseVO));
    }


}

