package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.HomeService;
import com.autel.cloud.pile.base.dto.TimePowerDTO;
import com.autel.cloud.pile.bill.dto.ChargePowerDTO;
import com.autel.cloud.pile.bill.dto.EmsGroupPowerDTO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @Author A22282
 * @Date 2024/1/23 10:42
 */
@RestController
@RequestMapping("/home")
@Api(tags = "首页数据统计", value = "首页数据统计")
@Slf4j
@Validated
public class HomeController {

    private final HomeService homeService;

    public HomeController(HomeService homeService) {
        this.homeService = homeService;
    }

    @PostMapping("/queryTimePower")
    @ApiOperation(value = "实时充电功率", notes = "实时充电功率")
    public Result<Page<ChargePowerDTO>> queryTimePower(@RequestBody TimePowerDTO dto) {
        Long sellerId = LoginUserUtil.getSellerId();
        dto.setSellerId(sellerId);
        return Result.ofSucceed(this.homeService.queryTimePower(dto));
    }

}
