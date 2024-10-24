package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.user.api.dto.BatchDiffEmailDTO;
import com.autel.cloud.pile.user.api.dto.EmailSendDTO;
import com.autel.cloud.pile.user.api.dto.SmsSendDTO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;

import javax.validation.Valid;

/**
 * @Author temp
 * @Date 2022/7/29 15:06
 */
@FeignClient(value = "common-service",contextId = "CommonServiceFeign")
public interface CommonServiceFeign {

    @ApiOperation(value = "发送单条短信")
    @PostMapping(value = "/message/sms")
    Result<Object> sendSms(@RequestBody @Valid SmsSendDTO smsSendDto);

    @ApiOperation(value = "用户中心注册专用 发送单条短信")
    @PostMapping(value = "/message/smsToUc")
    Result<Object> sendSmsToUc(@RequestBody @Valid SmsSendDTO smsSendUcDto);

    @ApiOperation(value = "发送邮件")
    @PostMapping("/message/email")
    @ResponseBody
    Result<Void> sendEmail(@RequestBody @Valid EmailSendDTO emailSendDto);

    @ApiOperation(value = "批量发送不同模板邮件")
    @PostMapping("/message/batchEmail")
    @ResponseBody
    Result<Void> sendBatchEmail(@RequestBody @Valid BatchDiffEmailDTO batchDiffEmailDTO);
}
