package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.SellerAccountService;
import com.autel.cloud.pile.base.dto.pay.*;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@RestController
@RequestMapping("/sellerAccount")
@Api(value = "sellerAccount", tags = "商家支付账户管理")
@Slf4j
@Validated
public class SellerAccountController {

    @Autowired
    private SellerAccountService sellerAccountService;

    @OperationActionLog(action = "query",object = "stripeAccount")
    @PostMapping("/queryStripeAccount")
    @ApiOperation("查询商家stripe账号")
    public Result<SellerAccountResp> queryStripeAccount(@RequestBody SellerAccountReq sellerAccountReq){
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        if(sellerAccountReq!=null &&payload!=null){
            sellerAccountReq.setSellerId(payload.getSellerId().toString());
        }

        // todo 数据安全(APP请求时需要检验)
        if (sellerAccountReq != null
                && payload == null
                && LoginUserHolder.getLoginUser().getId() != null) {
            sellerAccountReq.setSellerId(LoginUserHolder.getLoginUser().getId().toString());
        }
        return sellerAccountService.queryStripeAccount(sellerAccountReq);
    }

    @PostMapping("/queryStripeAccountV2")
    @ApiOperation("查询商家stripe账号")
    public Result<SellerAccountResp> queryStripeAccountV2(@RequestBody SellerAccountReq sellerAccountReq){
        return sellerAccountService.queryStripeAccount(sellerAccountReq);
    }

    @Log(title = "绑定Stripe账户(商家支付账户管理)", businessType = BusinessType.OTHER, code = "50201438")
    @OperationActionLog(action = "bind",object = "stripeAccount")
    @PostMapping("/bindStripeAccount")
    @ApiOperation("绑定Stripe账户")
    public Result<BindStripeResp> bindStripeAccount(@RequestBody SellerAccountReq sellerAccountReq){
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        if(sellerAccountReq!=null &&payload!=null){
            sellerAccountReq.setSellerId(payload.getSellerId().toString());
        }
        return sellerAccountService.bindStripeAccount(sellerAccountReq);
    }


    @Log(title = "绑定Stripe账户(私桩共享)", businessType = BusinessType.OTHER, code = "50201438")
    @PostMapping("/bindStripeAccountV2")
    @ApiOperation("绑定Stripe账户V2")
    public Result bindStripeAccountV2(@RequestBody SellerAccountReq sellerAccountReq){
        Result<BindStripeResp> bindStripeRespResult = sellerAccountService.bindStripeAccount(sellerAccountReq);
        Integer code = bindStripeRespResult.getCode();
        if(code==200){
            BindStripeResp data = bindStripeRespResult.getData();
            return Result.ofSucceed(data.getData().getUrl());
        }
        return bindStripeRespResult;
    }

    @PostMapping("/bindCallBack")
    @ApiOperation("绑定Stripe账户回调")
    public Result<Boolean> bindCallBack(@RequestBody SellerAccountReq sellerAccountReq){
        return sellerAccountService.bindCallBack(sellerAccountReq);
    }

    @Log(title = "解绑Stripe账户(商家支付账户管理)", businessType = BusinessType.OTHER, code = "50201439")
    @OperationActionLog(action = "unbind",object = "stripeAccount")
    @PostMapping("/unBindStripeAccount")
    @ApiOperation("解绑Stripe账户")
    public Result<UnBindStripeResp> unBindStripeAccount(@RequestBody SellerAccountReq sellerAccountReq){
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        if(sellerAccountReq!=null &&payload!=null){
            sellerAccountReq.setSellerId(payload.getSellerId().toString());
        }
        return sellerAccountService.unBindStripeAccount(sellerAccountReq);
    }

    @Log(title = "修改Stripe账户信息(商家支付账户管理)", businessType = BusinessType.OTHER, code = "50201440")
    @OperationActionLog(action = "edit",object = "stripeAccount")
    @PostMapping("/updateStripeAccount")
    @ApiOperation("修改Stripe账户信息")
    public Result<BindStripeResp> updateStripeAccount (@RequestBody SellerAccountReq sellerAccountReq){
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        if(sellerAccountReq!=null &&payload!=null){
            sellerAccountReq.setSellerId(payload.getSellerId().toString());
        }
        return sellerAccountService.updateStripeAccount(sellerAccountReq);
    }

    @PostMapping("/bindSellerWXAccount")
    @ApiOperation("绑定微信商户号")
    public Result<Boolean> bindWXAccount(@Valid @RequestBody SellerWXAccountReq sellerWXAccountReq) {
        return sellerAccountService.bindWXAccount(sellerWXAccountReq);
    }

    @PostMapping("/unBindSellerWXAccount")
    @ApiOperation("解除绑定微信商户号")
    public Result<WxPayDeleteReceiverResponse> unBindWXAccount(@Valid @RequestBody UnBindWxAccountReq unBindWxAccountReq) {
        return sellerAccountService.unBindWXAccount(unBindWxAccountReq);
    }


    @GetMapping("/queryBindWXAccount")
    @ApiOperation("查询商家微信账号")
    public Result<String> queryBindWXAccount(@RequestParam(value = "sellerId", required = false) Long sellerId){
        return sellerAccountService.queryBindWXAccount(sellerId);
    }

    @GetMapping("/queryBindWXAccountV2")
    @ApiOperation("查询商家微信账号V2")
    public Result<WxAccountInfoResp> queryBindWXAccountV2(@RequestParam(value = "sellerId", required = false) Long sellerId){
        return sellerAccountService.queryBindWXAccountV2(sellerId);
    }

    @GetMapping("/queryCurrentEnv")
    @ApiOperation("查询当前所属环境信息")
    public Result<String> queryCurrentEnv() {
        return sellerAccountService.queryCurrentEnv();
    }

    @GetMapping("/queryPayType")
    @ApiOperation("查询当前商家的收款方式 1:stripe 10:weChat")
    public Result<Integer> queryPayType() {
        return sellerAccountService.queryPayType();
    }


}
