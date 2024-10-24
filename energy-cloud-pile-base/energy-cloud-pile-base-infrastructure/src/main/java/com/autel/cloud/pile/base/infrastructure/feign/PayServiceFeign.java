package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.CustomerDTO;
import com.autel.cloud.pile.base.dto.pay.*;
import com.autel.cloud.pile.base.vo.CustomerVO;
import com.autel.cloud.pile.bill.dto.pay.DTStripeBodyDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

/**
 * @Author A22282
 * @Date 2022/5/13 16:42
 */
@FeignClient(name = "pay-server", contextId = "PayServiceFeign")
//@FeignClient(name = "pay-server",contextId = "PayServiceFeign",url = "http://autel-cloud-energy-gateway-enedev.auteltech.cn/api/pay-server")
public interface PayServiceFeign {

    @PutMapping({"/customer/add"})
    Result<CustomerVO> addCustomer(@RequestHeader("source") String source,
                                   @RequestHeader("pay-type") Integer payType,
                                   @RequestHeader(value = "code", required = false) String code,
                                   @RequestHeader(value = "country", required = false) String country,
                                   @RequestHeader("autel-sign") String autelSign,
                                   @RequestBody CustomerDTO CustomerDTO);

    @PostMapping({"/customer/attach"})
    Result<CustomerVO> attachPaymentMethod(@RequestHeader("source") String source,
                                           @RequestHeader("pay-type") Integer payType,
                                           @RequestHeader(value = "code", required = false) String code,
                                           @RequestHeader(value = "country", required = false) String country,
                                           @RequestHeader("autel-sign") String autelSign,
                                           @RequestBody CustomerDTO CustomerDTO);

    @DeleteMapping({"/customer/{paymentMethod}/detach"})
    @ResponseBody
    Result<Boolean> detachPaymentMethod(@PathVariable("paymentMethod") String paymentMethod,
                                        @RequestHeader("source") String source,
                                        @RequestHeader("pay-type") Integer payType,
                                        @RequestHeader("autel-sign") String autelSign,
                                        @RequestHeader(value = "code", required = false) String code,
                                        @RequestHeader(value = "country", required = false) String country);


    /**
     * 生成引导链接
     *
     * @param source
     * @param payType
     * @param code
     * @param language
     * @param autelSign
     * @param dtStripeBodyDTO
     * @return
     */
    @PostMapping("/unified/account-link")
    BindStripeResp stripeAccountCreateV2(@RequestHeader("source") String source,
                                         @RequestHeader("pay-type") Integer payType,
                                         @RequestHeader(value = "code", required = false) String code,
                                         @RequestHeader(value = "country", required = false) String country,
                                         @RequestHeader("Accept-Language") String language,
                                         @RequestHeader("autel-sign") String autelSign,
                                         @RequestBody DTStripeBodyDTO dtStripeBodyDTO);


    /**
     * 重新生成引导链接
     *
     * @param source
     * @param payType
     * @param code
     * @param autelSign
     * @param dtStripeBodyDTO
     * @return
     */
    @PostMapping(value = "/unified/account/link")
    BindStripeResp stripeAccountLinkCreateV2(@RequestHeader("source") String source,
                                             @RequestHeader("pay-type") Integer payType,
                                             @RequestHeader(value = "code", required = false) String code,
                                             @RequestHeader(value = "country", required = false) String country,
                                             @RequestHeader("autel-sign") String autelSign,
                                             @RequestBody DTStripeBodyDTO dtStripeBodyDTO);

    /**
     * 解绑子账户
     *
     * @param source
     * @param payType
     * @param code
     * @param autelSign
     * @param bindingId
     * @return
     */
    @DeleteMapping(value = "/unified/account")
    UnBindStripeResp stripeAccountDeleteV2(@RequestHeader("source") String source,
                                           @RequestHeader("pay-type") Integer payType,
                                           @RequestHeader(value = "code", required = false) String code,
                                           @RequestHeader(value = "country", required = false) String country,
                                           @RequestHeader("autel-sign") String autelSign,
                                           @RequestParam("bindingId") String bindingId);


    /**
     * 查询商户接口
     *
     * @param source
     * @param payType
     * @param code
     * @param language
     * @param autelSign
     * @param bindingId
     * @return
     */
    @GetMapping(value = "/unified/account")
    QueryStripeResp stripeAccountStatusQueryV2(@RequestHeader("source") String source,
                                               @RequestHeader("pay-type") Integer payType,
                                               @RequestHeader(value = "code", required = false) String code,
                                               @RequestHeader("Accept-Language") String language,
                                               @RequestHeader("autel-sign") String autelSign,
                                               @RequestParam("bindingId") String bindingId);


    /*
     * description: bindWxAccount 绑定微信商户号
     * version: 1.0
     * date: 2023/8/15 17:09 
     * author: A23204 
     * 
     * @param source
     * @param payType
     * @param code
     * @param country
     * @param bindWxAccountReq
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.String>
     */ 
    @PostMapping("/wx/pay/receivers/add")
    Result<WxPayAddReceiverResponse> bindWxAccount(@RequestHeader("source") String source,
                                 @RequestHeader("pay-type") Integer payType,
                                 @RequestHeader("autel-sign") String autelSign,
                                 @RequestHeader(value = "code", required = false) String code,
                                 @RequestHeader(value = "country", required = false) String country,
                                 @RequestBody BindWxAccountReq bindWxAccountReq);

    /*
     * description: unBindWxAccount  微信解绑
     * version: 1.0
     * date: 2023/8/15 17:09 
     * author: A23204 
     * 
     * @param source
     * @param payType
     * @param code
     * @param country
     * @param unBindWxAccountReq
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.String>
     */ 
    @PostMapping("/wx/pay/receivers/delete")
    Result<WxPayDeleteReceiverResponse> unBindWxAccount(@RequestHeader("source") String source,
                                   @RequestHeader("pay-type") Integer payType,
                                   @RequestHeader("autel-sign") String autelSign,
                                   @RequestHeader(value = "code", required = false) String code,
                                   @RequestHeader(value = "country", required = false) String country,
                                   @RequestBody UnBindWxAccountReq unBindWxAccountReq);

    /**
     * description: retrievePaymentMethod pay-server 查询支付卡信息
     * version: 1.0
     * date: 2023/10/11 13:54
     * author: A23204
     *
     * @param country
     * @param source
     * @param paymentMethodId
     * @return com.autel.cloud.base.http.pojo.Result<com.autel.cloud.pile.base.dto.pay.PaymentMethodResp>
     */
    @GetMapping("/stripe/pm")
    Result<PaymentMethodResp> retrievePaymentMethod(@RequestParam(value = "paymentMethodId", required = false) String paymentMethodId,
                                                    @RequestParam(value = "paymentMethod", required = false) String paymentMethod,
                                                    @RequestHeader("source") String source,
                                                    @RequestHeader("pay-type") Integer payType,
                                                    @RequestHeader(value = "code", required = false) String code,
                                                    @RequestHeader(value = "country", required = false) String country);

}
