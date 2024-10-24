package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.SellerService;
import com.autel.cloud.pile.base.dto.SellerInfoDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Author:   A19011
 * Description: SellerController
 * Date:     2022/5/12 10:35
 *
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@RequestMapping(value = "/seller")
@Api(tags = "商家信息接口")
@Validated
public class SellerController {
    @Autowired
    SellerService sellerService;

    @GetMapping("/getSellerInfoBySn")
    @ApiOperation(value = "按桩查询商家名", notes = "按桩查询商家名")
    public Result<String> getSellerInfoBySn(@RequestParam("evseSn") String evseSn) {
        return Result.ofSucceed(sellerService.getSellerInfoBySn(evseSn));
    }

    @GetMapping("/getSellerIdInfoBySn")
    @ApiOperation(value = "按桩查询商家ID", notes = "按桩查询商家用户名")
    Result<String> getSellerIdInfoBySn(@RequestParam("evseSn") String evseSn) {
        return Result.ofSucceed(sellerService.getSellerIdInfoBySn(evseSn));
    }

    @GetMapping("/sellerIdByPileSn")
    @ApiOperation(value = "通过设备sn,查询商户id(不鉴权接口)", notes = "通过设备sn.查询商户id(不鉴权接口)")
    Result<String> getSellerIdByPileSn(@RequestParam("pileSn") String pileSn) {
        return Result.ofSucceed(sellerService.getSellerIdByPileSn(pileSn));
    }

    @GetMapping("/pileSnBySellId")
    @ApiOperation(value = "通过商户id,查询设备sn(不鉴权接口)", notes = "通过商户id.查询设备sn(不鉴权接口)")
    Result<List<String>> getPileSnBySellId(@RequestParam("sellId") String sellId) {
        return Result.ofSucceed(sellerService.getPileSnBySellId(sellId));
    }

    @GetMapping("/sellerInfoByPileSn")
    @ApiOperation(value = "通过设备sn,查询商户信息", notes = "通过设备sn,查询商户信息")
    Result<SellerInfoDTO> getSellerInfoByPileSn(@RequestParam("pileSn") String pileSn) {
        return Result.ofSucceed(sellerService.getSellerInfoByPileSn(pileSn));
    }
}
