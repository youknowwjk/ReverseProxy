package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.job.EvseDynamicPricingJob;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;

/**
 * Author:   A19011
 * Description: EvseDynamicPriceController
 * Date:     2023/7/13 8:42
 *
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@Api(tags = "互联互通计费")
@RequestMapping("/evsePrice")
@Slf4j
@Validated
public class EvseDynamicPriceController {
    @Autowired
    private EvseDynamicPricingJob evseDynamicPricingJob;


    @GetMapping("/ocpi")
    @ApiOperation(value = "OCPI计费同步", notes = "OCPI计费同步")
    public Result<Boolean> ocpiPriceSync() throws IOException {
        log.info("ocpiPriceSync start");
        evseDynamicPricingJob.executeOcpiDynamicPricingTask();
        log.info("ocpiPriceSync end");
        return Result.ofSucceed();
    }


    @GetMapping("/oicp")
    @ApiOperation(value = "OICP计费同步", notes = "OICP计费同步")
    public Result<Boolean> oicpPriceSync() throws IOException {
        log.info("oicpPriceSync start");
        evseDynamicPricingJob.executeOicpDynamicPricingTask();
        log.info("oicpPriceSync end");
        return Result.ofSucceed();
    }
}
