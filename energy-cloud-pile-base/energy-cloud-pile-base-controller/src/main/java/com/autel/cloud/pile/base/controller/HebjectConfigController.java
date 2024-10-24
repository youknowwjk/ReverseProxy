package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.vo.HubjectConfigVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

/**
 * @Author temp
 * @Date 2022/11/17 19:43
 */
@RestController
@RequestMapping("/hubject")
@Api(tags = "hubject配置管理")
@Slf4j
@Validated
public class HebjectConfigController {

    @Resource
    private OpLocationService opLocationService;

    @GetMapping("/query")
    @ApiOperation(value = "查询huject配置")
    public Result<HubjectConfigVO> query() {
        return opLocationService.checkNeedHubject();
    }
}
