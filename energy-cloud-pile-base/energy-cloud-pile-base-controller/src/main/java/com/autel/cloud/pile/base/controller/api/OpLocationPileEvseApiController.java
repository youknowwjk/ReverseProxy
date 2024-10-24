package com.autel.cloud.pile.base.controller.api;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseApiService;
import com.autel.cloud.pile.base.dto.api.LocationPilePageQueryDto;
import com.autel.cloud.pile.base.dto.api.LocationPileQueryDto;
import com.autel.cloud.pile.base.vo.api.PileEvseApiInfoVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

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
@RequestMapping("/api/opPileEvse")
@Api(tags = "api设备")
@Validated
public class OpLocationPileEvseApiController {

    private final OpLocationPileEvseApiService opLocationPileEvseApiService;

    public OpLocationPileEvseApiController(OpLocationPileEvseApiService opLocationPileEvseApiService) {
        this.opLocationPileEvseApiService = opLocationPileEvseApiService;
    }

    @PostMapping("/page")
    @ApiOperation(value = "api分页查询场站设备列表", notes = "api分页查询场站设备列表")
    public Result<OpenApiPagelVO<PileEvseApiInfoVO>> queryPileEvselist(@RequestBody LocationPilePageQueryDto dto) {
        return Result.ofSucceed(opLocationPileEvseApiService.list(dto));
    }

    @PostMapping("/condtion")
    @ApiOperation(value = "查询设备信息", notes = "查询设备信息")
    public Result<List<PileEvseApiInfoVO>> queryPileEvseLocation(@RequestBody LocationPileQueryDto dto) {
        return Result.ofSucceed(opLocationPileEvseApiService.queryLocation(dto));
    }
}

