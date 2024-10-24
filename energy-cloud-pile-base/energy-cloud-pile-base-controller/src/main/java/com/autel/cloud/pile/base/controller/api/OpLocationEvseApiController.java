package com.autel.cloud.pile.base.controller.api;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseApiService;
import com.autel.cloud.pile.base.dto.api.LocationEvsePageQueryDto;
import com.autel.cloud.pile.base.vo.api.LocationEvseApiInfoVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 充电枪
 */
@Log4j2
@RestController
@RequestMapping("/api/opEvse")
@Api(tags = "api充电枪")
@Validated
public class OpLocationEvseApiController {

    private final OpLocationEvseApiService opLocationEvseApiService;

    public OpLocationEvseApiController(OpLocationEvseApiService opLocationEvseApiService) {
        this.opLocationEvseApiService = opLocationEvseApiService;
    }

    @PostMapping("/page")
    @ApiOperation(value = "api分页查询场站充电枪列表", notes = "api分页查询场站充电枪列表")
    public Result<OpenApiPagelVO<LocationEvseApiInfoVO>> queryEvselist(@RequestBody LocationEvsePageQueryDto dto) {
        return Result.ofSucceed( opLocationEvseApiService.list(dto) );
    }

    @GetMapping("/id")
    @ApiOperation(value = "api查询场站充电枪列表", notes = "api查询场站充电枪列表")
    public Result<LocationEvseApiInfoVO> queryEvseLocation(@RequestParam("id") Long id,@RequestParam("sellerId") Long sellerId) {
        return Result.ofSucceed( opLocationEvseApiService.queryOpLocationEvseById(id,sellerId) );
    }
}

