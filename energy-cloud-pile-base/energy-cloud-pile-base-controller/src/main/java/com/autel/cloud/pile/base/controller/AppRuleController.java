package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.pile.base.domain.service.AppRuleService;
import com.autel.cloud.pile.base.dto.app.BusinessTimeDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @Author A22591
 * @Date 2022/8/18 11:22
 */
@RestController
@RequestMapping("/appRuler")
@Slf4j
@Api(tags = "APP营业时间")
@Validated
public class AppRuleController {

    @Autowired
    private AppRuleService ruleService;

    @PostMapping("/addTime")
    @ApiOperation("营业时间")
    @ApiResponses(value = {
            @ApiResponse(code = 50201442, message = "规则明细不能为空或details数量超出2"),
            @ApiResponse(code = 50201443, message = "规则日期不能为空或days超出24"),
            @ApiResponse(code = 50201444, message = "手机号格式错误"),
            @ApiResponse(code = 50201445, message = "邮箱格式错误"),
            @ApiResponse(code = 50201447, message = "规则名称不能重复")
    })
    public Result<Boolean> addTime(@RequestBody @Validated BusinessTimeDTO businessTimeDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        businessTimeDTO.setSellerId(payload.getSellerId());
        businessTimeDTO.setUserId(payload.getUserId());
        return ruleService.addTime(businessTimeDTO);
    }
}
