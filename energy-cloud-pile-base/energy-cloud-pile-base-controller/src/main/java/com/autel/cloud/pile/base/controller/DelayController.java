package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.DelayService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.context.annotation.Lazy;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

@RestController
@RequestMapping("/delay")
@Api(value = "延时队列",tags = "延时队列")
public class DelayController {

    @Resource
    private DelayService delayService;

    @Lazy
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @PostMapping("/create")
    @ApiOperation(value = "生成一条延时信息",notes = "生成一条延时信息")
    public Result<Boolean> create(){
        return Result.ofSucceed(delayService.sendDelayMessage());
    }

    @PostMapping("/test")
    @ApiOperation(value = "测试用例",notes = "测试用例")
    public Result<Boolean> test(){
        opLocationPileGroupService.loadEmsGroupData();
        return Result.ofSucceed(true);
    }
}
