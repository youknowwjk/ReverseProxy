package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.PaymentCardService;
import com.autel.cloud.pile.base.dto.CardManagerDTO;
import com.autel.cloud.pile.base.dto.PaymentCardManagerDTO;
import com.autel.cloud.pile.base.vo.CardManagerV2VO;
import com.autel.cloud.pile.base.vo.CardManagerVO;
//import com.autel.cloud.redis.annotation.NoRepeatSubmit;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

@RestController
@RequestMapping("/paymentCardManager")
@Api(tags = "支付卡管理")
@Validated
public class PaymentCardManageController {


    @Resource
    private PaymentCardService paymentCardService;

    @PostMapping("/queryPaymentCardDetail")
    @ApiOperation(value = "查询支付卡详情", tags = "查询支付卡详情")
    public Result<PaymentCardManagerDTO> queryPaymentCardDetail(@Validated @RequestBody PaymentCardManagerDTO cardManagerDTO) {
        return paymentCardService.queryPaymentCardDetail(cardManagerDTO);
    }

    @PutMapping("/updateRefuseCodeByPaymentId")
    @ApiOperation(value = "更新支付卡标识",tags = "更新支付卡标识")
    Result<Boolean> updateRefuseCodeByPaymentId(@RequestParam("paymentId") String paymentId,
                                                      @RequestParam(value = "refuseCode", required = false)  String refuseCode){
        return Result.ofSucceed(paymentCardService.updateRefuseCodeByPaymentId(paymentId, refuseCode));
    }

    @GetMapping("/getPaymentCardInfoByPaymentId")
    @ApiOperation(value = "按paymentId查询支付卡信息",tags = "按paymentId查询支付卡信息")
    Result<PaymentCardManagerDTO> getPaymentCardInfoByPaymentId(@RequestParam("paymentId") String paymentId){
        return Result.ofSucceed(paymentCardService.getPaymentCardInfoByPaymentId(paymentId));
    }


    @Log(title = "支付卡绑定(支付卡管理)", businessType = BusinessType.OTHER, code = "50201436")
    @PostMapping("/bindCard")
    @ApiOperation("支付卡绑定")
//    @NoRepeatSubmit
    public Result<Boolean> bindCard(@Validated (CardManagerDTO.Default.class) @RequestBody CardManagerDTO cardManagerDTO) {
        if(cardManagerDTO.getUserId() == null){
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            cardManagerDTO.setUserId(jwtInfo.getId());
        }
        return Result.ofSucceed(paymentCardService.bindCard(cardManagerDTO).getResult());
    }

    @Log(title = "支付卡绑定(支付卡管理)", businessType = BusinessType.OTHER, code = "50201436")
    @PostMapping("/bindCard/v2")
    @ApiOperation("支付卡绑定")
    public Result<CardManagerV2VO> bindCardV2(@Validated(CardManagerDTO.Default.class) @RequestBody CardManagerDTO cardManagerDTO) {
        if(cardManagerDTO.getUserId() == null){
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            cardManagerDTO.setUserId(jwtInfo.getId());
        }
        return paymentCardService.bindCardV2(cardManagerDTO);
    }

    @PostMapping("/bindCardList")
    @ApiOperation("查询绑卡列表")
    public Result<List<CardManagerVO>> bindCardList() {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        return paymentCardService.bindCardList(jwtInfo.getId());
    }

    @Log(title = "解除绑定卡(支付卡管理)", businessType = BusinessType.OTHER, code = "50201437")
    @PostMapping("/unbindCard")
    @ApiOperation("解除绑定卡")
    public Result<Boolean> unbindCard(@Validated(CardManagerDTO.UnbindCard.class) @RequestBody CardManagerDTO cardManagerDTO) {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        cardManagerDTO.setUserId(jwtInfo.getId());
        return paymentCardService.unbindCard(cardManagerDTO);
    }

    @PostMapping("/setDefaultCard")
    @ApiOperation("设置默认卡")
    public Result<Boolean> setDefaultCard(@RequestBody @Validated(CardManagerDTO.SetDefaultCard.class) CardManagerDTO cardManagerDTO) {
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        cardManagerDTO.setUserId(jwtInfo.getId());
        return paymentCardService.setDefaultCard(cardManagerDTO);
    }

    @PostMapping("/isBindingCard")
    @ApiOperation(value = "是否绑卡")
    public Result<Boolean> isBindingCard(@RequestParam("userId") String userId) {
        return paymentCardService.isBindingCard(userId);
    }

    @PostMapping("/isBindingCardCurrentUser")
    @ApiOperation(value = "当前用户是否绑stripe")
    public Result<Boolean> isBindingCardCurrentUser() {
        return paymentCardService.isBindingCardCurrentUser(LoginUserHolder.getLoginUser().getId());
    }

    @GetMapping("/defaultCardExpire")
    @ApiOperation(value = "默认卡是否过期")
    public Result<Boolean> defaultCardExpire(@RequestParam("userId") String userId) {
        return paymentCardService.defaultCardExpire(userId);
    }


}
