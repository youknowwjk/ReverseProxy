package com.autel.cloud.pile.base.controller;

import cn.hutool.core.bean.BeanUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ordercenter.vo.*;
import com.autel.cloud.pile.base.domain.service.SubscribePlaceOrderService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @description 下单相关接口
 * @auther A23204
 * @datetime 2023/6/9 11:36
 */
@RestController
@RequestMapping("/order")
@Slf4j
@Api(tags = "下单相关接口")
@Validated
public class PlaceOrderController {

    @Autowired
    private SubscribePlaceOrderService placeOrderService;

    /** 可用商品列表查询
     * @param
     * @return
     */
    @ApiOperation(value = "可用商品列表查询")
    @PostMapping("/goods/list")
    Result<List<ClientCommodityRespVo>> queryGoodsList(@Valid ClientCommodityReqVo commodityReqVo) {
        return placeOrderService.queryGoodsList(commodityReqVo);
    }

    @ApiOperation(value = "支付结果查询：0：失败： 1： 成功， 2：暂未收到支付结果 ")
    @GetMapping("/goods/result")
    Result<String> queryPayResult(@RequestParam String orderId) {
        return placeOrderService.queryPayResult(orderId);
    }

    @ApiOperation(value = "商品详情查询")
    @PostMapping("/goods/detail")
    Result<ClientCommodityDetailRespVo> queryGoodsDetail(@Valid @RequestBody ClientCommodityDetailReqVo clientCommodityDetailReqVo) {
        return placeOrderService.queryGoodsDetail(clientCommodityDetailReqVo);
    }

    /** 下单接口
     * @return
     */
    @ApiOperation(value = "CRM等内部系统下单统一入口")
    @PostMapping("/goods/place")
    Result<ClientOrderCreateRespVo> placeOrder(@Valid @RequestBody OrderCreateReqVo orderCreateReqVo) {
        return placeOrderService.placeOrder(orderCreateReqVo);
    }

    /** 充电云平台下单入口
     * @return
     */
    @ApiOperation(value = "充电云平台下单入口")
    @PostMapping("/goods/charge/place")
    Result<ClientOrderCreateRespVo> placeOrderChargeBusiness(@Valid @RequestBody OrderCreateChargeBusinessReqVo orderCreateChargeBusinessReqVo) {
        OrderCreateReqVo orderCreateReqVo = new OrderCreateReqVo();
        BeanUtil.copyProperties(orderCreateChargeBusinessReqVo, orderCreateReqVo);
        return placeOrderService.placeOrder(orderCreateReqVo);
    }

    /** sap 下单入口
     * @return
     */

    @ApiOperation(value = "sap下单入口")
    @PostMapping("/goods/sap/place")
    Result<ClientOrderCreateRespVo> placeOrderSAP(@RequestHeader("autel-sign") String autelSign, @RequestHeader(value = "redirect-key", required = false) String redirectKey, @Valid @RequestBody OrderCreateSapReqVo orderCreateSapReqVo) {
        return placeOrderService.placeOrderSAP(autelSign, redirectKey, orderCreateSapReqVo);
    }

    /** sap 撤销订单
     * @return
     */

    @ApiOperation(value = "sap撤销订单")
    @PostMapping("/goods/sap/delete")
    Result<Boolean> orderDeleteSAP(@RequestHeader("autel-sign") String autelSign, @RequestHeader(value = "redirect-key", required = false) String redirectKey, @Valid @RequestBody OrderCancelReqVo orderCancelReqVo) {
        return placeOrderService.orderCancelSAP(autelSign, redirectKey,  orderCancelReqVo);
    }

    @ApiOperation(value = "sap下单地址查询,返回下单url.(urlType=create：创建订单；urlType=cancel:取消订单)")
    @GetMapping("/goods/sap/placeorderurl")
    Result<String> placeOrderUrlForSap(@RequestParam("countryAbbreviation") String countryAbbreviation, @RequestParam("urlType") String urlType) {
        String result = null;
        if ("create".equals(urlType)) {
            result = "https://autel-cloud-energy-gateway-enedev.auteltech.cn/api/pile-base-app/order/goods/sap/place";
        } else if("cancel".equals(urlType)) {
            result = "https://autel-cloud-energy-gateway-enedev.auteltech.cn/api/pile-base-app/order/goods/sap/delete";
        }
        return Result.ofSucceed(result);
        //return placeOrderService.queryPlaceOrderUrlForSap(countryAbbreviation);
    }

    /** 订单回调接口，接收订单产生的回调事件
     * @return
     */
    @ApiOperation(value = "订单回调接口，接收订单产生的回调事件, 接收成功，返回Boolean.TRUE, 失败：Boolean.FALSE")
    @PostMapping("/pay/callback")
    Result<Boolean> orderTransactionCallback (@Validated @RequestBody CallBackEventNoticeReqVo callBackEventNoticeReqVo) {
        return placeOrderService.orderTransactionCallback(callBackEventNoticeReqVo);
    }

    /** 充电卡licence 一键提取
     * @return
     */
    @ApiOperation(value = "充电卡licence 一键提取")
    @PostMapping("/goods/rfd/extract")
    Result<ClientOrderCreateRespVo> rdfExtract(@Valid @RequestBody RfdLicenceExtractVo rfdLicenseExtract) {
        return placeOrderService.rdfExtract(rfdLicenseExtract);
    }

}
