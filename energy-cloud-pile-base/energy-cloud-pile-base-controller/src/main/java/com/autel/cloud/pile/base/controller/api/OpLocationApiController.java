package com.autel.cloud.pile.base.controller.api;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.openapi.dto.OpenApiPageDto;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.domain.service.OpLocationApiService;
import com.autel.cloud.pile.base.dto.api.LocationQueryDto;
import com.autel.cloud.pile.base.vo.api.LocationApiInfoVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 * 场站表 前端控制器
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Log4j2
@RestController
@RequestMapping("/api/opLocation")
@Api(tags = "api场站设置")
@Validated
public class OpLocationApiController {

    private final OpLocationApiService opLocationApiService;

    public OpLocationApiController(OpLocationApiService opLocationApiService) {
        this.opLocationApiService = opLocationApiService;
    }

    @PostMapping("/page")
    @ApiOperation(value = "api分页查询场站列表", notes = "api分页查询场站列表")
    public Result<OpenApiPagelVO<LocationApiInfoVO>> list(@RequestBody OpenApiPageDto dto) {
        return Result.ofSucceed(opLocationApiService.list(dto));
    }

    @PostMapping("/condtion")
    @ApiOperation(value = "查询场站信息", notes = "查询场站信息")
    public Result<LocationApiInfoVO> queryLocation(@RequestBody LocationQueryDto dto) {
        return Result.ofSucceed(opLocationApiService.queryLocation(dto));
    }
}

