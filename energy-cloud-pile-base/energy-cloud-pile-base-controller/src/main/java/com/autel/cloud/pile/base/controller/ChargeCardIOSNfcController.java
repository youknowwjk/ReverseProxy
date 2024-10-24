package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.support.HttpRequestHandlerServlet;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletResponse;
import java.io.Serializable;
import java.util.Date;

@RestController
@RequestMapping(value = "/iosWallet")
@Api(tags = "ios钱包通行证更新支持")
@Validated
public class ChargeCardIOSNfcController {


    @GetMapping("/addWallet")
    @ResponseBody
    @ApiOperation("添加到苹果钱包中")
    public void addWallet(@ApiIgnore HttpRequestHandlerServlet httpRequest, @ApiIgnore HttpServletResponse httpResponse) {
        // 根据之前的创建通行证生成完善这个接口就可以；
    }

    //回调接口，用于保存设备的token，方便之后的更新
    @PostMapping("/v1/devices/{deviceLibraryIdentifier}/registrations/{passTypeIdentifier}/{serialNumber}")
    @ApiOperation("注册设备")
    public ResponseEntity registrationWallet(@PathVariable("deviceLibraryIdentifier") String deviceLibraryIdentifier,
                                             @PathVariable("passTypeIdentifier") String passTypeIdentifier,
                                             @PathVariable("serialNumber") String serialNumber,
                                             @RequestBody PushToken pushToken
    ) {
        // 这里的pushToken需要保存起来，然后推送更新时，将这个pushToken带上即可；
        System.err.println("time:"+ new Date());
        System.err.println("[暗余]. 注册设备");
        System.err.println("deviceLibraryIdentifier:"+deviceLibraryIdentifier);
        System.err.println("passTypeIdentifier:"+passTypeIdentifier);
        System.err.println("serialNumber:"+serialNumber);
        System.err.println("pushToken:"+ JSON.toJSONString(pushToken));
        return new ResponseEntity(HttpStatus.OK);
    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    static class PushToken implements Serializable {
        private String pushToken;
    }



}
