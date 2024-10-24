package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseOpenService;
import com.autel.cloud.pile.base.dto.OpLocationForAdvParamDTO;
import com.autel.cloud.pile.base.vo.OpLocationPileForAdvVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * @Author A22282
 * @Date 2023/12/12 20:33
 */
@RestController
@RequestMapping("/openPile")
@Api(tags = "桩信息接口-对外")
@Slf4j
@Validated
public class OpLocationPileEvseOpenController {

    private final OpLocationPileEvseOpenService opLocationPileEvseOpenService;

    public OpLocationPileEvseOpenController(OpLocationPileEvseOpenService opLocationPileEvseOpenService) {
        this.opLocationPileEvseOpenService = opLocationPileEvseOpenService;
    }

    @PostMapping("/getAdvList")
    @ApiOperation(value = "查询包含广告类型场站下的桩", notes = "查询包含广告类型场站下的桩")
    public Result<Page<OpLocationPileForAdvVO>> getAdvList(@Validated @RequestBody OpLocationForAdvParamDTO dto) {
        return Result.ofSucceed(this.opLocationPileEvseOpenService.getAdvList(dto));
    }

    @PostMapping("/history")
    @ApiOperation(value = "历史数据处理", notes = "历史数据处理")
    public Result<List<Long>> history(@Validated @RequestBody List<Long> sellerIdList) {
        return Result.ofSucceed(this.opLocationPileEvseOpenService.history(sellerIdList));
    }

    @PostMapping("/deletes")
    @ApiOperation(value = "脏数据处理", notes = "脏数据处理")
    public Result<Integer> deletes() {
        return Result.ofSucceed(this.opLocationPileEvseOpenService.deletes());
    }
}
