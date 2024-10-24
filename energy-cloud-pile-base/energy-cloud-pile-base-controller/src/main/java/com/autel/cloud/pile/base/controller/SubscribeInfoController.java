package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.ordercenter.vo.ClientOrderListReqVo;
import com.autel.cloud.pile.base.domain.model.*;
import com.autel.cloud.pile.base.domain.service.SubscribeInfoService;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;

/**
 * 订阅信息
 *
 * @author A22598
 * @date 2023/06/13
 */
@Api(tags = "订阅信息")
@Slf4j
@RestController
@Validated
@RequestMapping("/subscribeInfo")
public class SubscribeInfoController {
    @Resource
    SubscribeInfoService subscribeInfoService;

    @PostMapping("/page")
    @ApiOperation("订阅信息分页查询")
    public Result<PageDTO<SubscribeInfo>> subscribeInfoPage(@RequestBody SubscribeInfoPageVO searchDTO, @RequestHeader("accept-language") String language) {
        log.info("subscribeInfoPage searchDTO: {}, language: {} ", JSON.toJSONString(searchDTO), language);
        PageDTO<SubscribeInfo> page = subscribeInfoService.subscribeInfoPage(searchDTO, language);
        log.info("subscribeInfoPage pageRes: {}", JSON.toJSONString(page));
        return Result.ofSucceed(page);
    }

    @GetMapping("/{orderId}")
    @ApiOperation("订阅订单详细信息")
    Result<SubscribeInfoDetail> subscribedOrderDetail(@PathVariable("orderId") Long orderId, @RequestHeader("accept-language") String language) {
        log.info("subscribedOrderDetail orderId: {}", orderId);
        SubscribeInfoDetail subscribeInfoDetail = subscribeInfoService.subscribedOrderDetail(orderId, language);
        log.info("subscribedOrderDetail subscribeInfoDetail: {}", JSON.toJSONString(subscribeInfoDetail));
        return Result.ofSucceed(subscribeInfoDetail);
    }

    @PostMapping("/licenseInfoOfOrder")
    @ApiOperation("license详情")
    Result<PageDTO<LicenseInfo>> licenseInfoOfOrder(@RequestBody LicenseSearchDTO licenseSearchDTO) {
        log.info("licenseInfoOfOrder licenseSearchDTO: {}", licenseSearchDTO);
        PageDTO<LicenseInfo> licenseInfo = subscribeInfoService.licenseInfoOfOrder(licenseSearchDTO);
        log.info("licenseInfoOfOrder licenseInfo: {}", JSON.toJSONString(licenseInfo));
        return Result.ofSucceed(licenseInfo);
    }

}
