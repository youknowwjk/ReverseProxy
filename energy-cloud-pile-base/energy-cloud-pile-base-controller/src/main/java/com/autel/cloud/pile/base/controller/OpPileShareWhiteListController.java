package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.pile.base.domain.service.OpPileShareWhiteListService;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListDTO;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListPageDTO;
import com.autel.cloud.pile.base.vo.OpPileShareWhiteListPageVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

@Log4j2
@RestController
@RequestMapping("/opWhiteList")
@Api(tags = "私桩共享白名单设置")
@Validated
public class OpPileShareWhiteListController {

    private final OpPileShareWhiteListService opPileShareWhiteListService;

    public OpPileShareWhiteListController(OpPileShareWhiteListService opPileShareWhiteListService) {
        this.opPileShareWhiteListService = opPileShareWhiteListService;
    }

    @Log(title = "根据pileSn查询桩是否在白名单")
    @PostMapping("/isExistByPileSn/{pileSn}")
    @ApiOperation(value = "根据pileSn查询桩是否在白名单", notes = "根据pileSn查询桩是否在白名单")
    public Result<Boolean> isExistByPileSn(@PathVariable("pileSn") String pileSn) {
        return opPileShareWhiteListService.isExistByPileSn(pileSn);
    }

    @Log(title = "分页查询白名单")
    @PostMapping("/page")
    @ApiOperation(value = "分页查询白名单", notes = "分页查询白名单")
    public Result<Page<OpPileShareWhiteListPageVO>> getWhiteListPage(@Validated @RequestBody @Valid OpPileShareWhiteListPageDTO opPileShareWhiteListPageDTO) {
        return opPileShareWhiteListService.getWhiteListPage(opPileShareWhiteListPageDTO);
    }

    @Log(title = "增加白名单")
    @PostMapping("/add")
    @ApiOperation(value = "新增白名单", notes = "新增白名单")
    public Result<Long> create(@Validated @RequestBody @Valid OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        return opPileShareWhiteListService.create(opPileShareWhiteListDTO);
    }

    @Log(title = "根据pileSn删除白名单")
    @PostMapping("/delete")
    @ApiOperation(value = "根据pileSn删除白名单", notes = "根据pileSn删除白名单")
    public Result<Boolean> deleteByPileSn(@Validated @RequestBody @Valid OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        return opPileShareWhiteListService.deleteByPileSn(opPileShareWhiteListDTO);
    }
}
