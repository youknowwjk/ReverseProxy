package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.PaymentService;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.PaymentEntity;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;

/**
 * 支付方式控制层
 *
 * @author Xzhi
 * @date 2021-12-07 19:52
 */
@RestController
@RequestMapping("/payment")
@Api(value = "paymentController", tags = "支付方式")
@Validated
public class PaymentController {

    @Autowired
    private PaymentService paymentService;

    @PostMapping("/selectByStationIdAndSellerId")
    @ApiOperation(value = "根据站点ID和商家ID查询支付方式")
    public Result<PaymentEntity> selectByStationIdAndSellerId(
            @NotNull @RequestParam("sellerId") Long sellerId, @NotNull @RequestParam("stationId")Long stationId) {
        return  Result.ofSucceed(paymentService.selectByStationIdAndSellerId(sellerId, stationId));
    }

    @PostMapping("/selectListBySellerId")
    @ApiOperation(value = "根据商家ID查询支付方式")
    public Result<List<PaymentEntity>> selectListBySellerId(@NotNull @RequestParam("sellerId") Long sellerId) {
        return Result.ofSucceed(paymentService.selectListBySellerId(sellerId));
    }

    @PostMapping("/selectAccountIdBySellerIdAndCode")
    @ApiOperation(value = "根据商家ID和支付方式查询商家支付账户id")
    public Result<PaymentEntity> selectAccountIdBySellerIdAndCode(@RequestParam("sellerId")Long  sellerId, @RequestParam("code") Integer code) {
        return Result.ofSucceed(paymentService.selectAccountIdBySellerIdAndCode(sellerId,code));
    }

    @PostMapping("/saveReturnPojo")
    @ApiOperation(value = "插入对象返回对象（主要是主键id）")
    public Result<PaymentEntity> saveReturnPojo(@RequestBody PaymentEntity payment) {
        return Result.ofSucceed(paymentService.saveReturnPojo(payment));
    }


    @PostMapping("/updateStripeStatusById")
    @ApiOperation(value = "根据主键id更新enable_flag字段，这里用来标识商家stripe账户审核状态")
    public Result<Boolean> updateStripeStatusById(@RequestBody PaymentEntity payment) {
        return Result.ofSucceed(paymentService.updateStripeStatusById(payment));
    }


}