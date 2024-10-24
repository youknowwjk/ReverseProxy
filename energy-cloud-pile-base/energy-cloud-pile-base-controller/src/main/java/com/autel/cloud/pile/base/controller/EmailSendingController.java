package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.SubscribedInvoiceDTO;
import com.autel.cloud.pile.base.domain.service.EmailSendingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;


/**
 * 邮件发送
 * @author A22598
 * @date 2023/06/13
 */
@Api(tags = "邮件发送")
@RestController
@RequestMapping("/emailSending")
@Validated
public class EmailSendingController {

    @Resource
    EmailSendingService emailSendingService;

    @GetMapping("/subscribedInvoice")
    public Result<Boolean> sendSubscribedInvoice (@RequestParam("orderId") String orderId, @RequestHeader("accept-language") String language) {
        emailSendingService.sendSubscribedInvoice(orderId, language);
        return Result.ofSucceed(true);
    }

    @GetMapping("/invoiceDownload")
    @ApiOperation("票据下载")
    public Result<SubscribedInvoiceDTO> invoiceDownload (@RequestParam("orderId") String orderId, @RequestHeader("accept-language") String language) {
        return Result.ofSucceed(emailSendingService.invoiceDownload(orderId, language));
    }

    @GetMapping("/test")
    public Result<Boolean> test(@RequestParam("orderId") String orderId, @RequestParam("tenantId") String tenantId, @RequestHeader("accept-language") String language) {
        emailSendingService.sendSubscriptionProductArrival(orderId, language, tenantId);
        return Result.ofSucceed(true);
    }

    @GetMapping("/test2")
    public Result<Boolean> test2(@RequestParam("orderId") String orderId, @RequestParam("returnId") String returnId, @RequestParam("tenantId") String tenantId, @RequestHeader("accept-language") String language) {
        emailSendingService.sendSubscriptionProductReturn(orderId, Long.valueOf(returnId), language, tenantId);
        return Result.ofSucceed(true);
    }
}
