package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.domain.service.OpLocationPriceService;
import com.autel.cloud.pile.base.dto.LocationPriceDTO;
import com.autel.cloud.pile.base.vo.OpLocationPriceInfoVo;
import com.autel.cloud.pile.base.vo.OpLocationPriceVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @Author temp
 * @Date 2023/3/7 15:37
 */
@Slf4j
@RestController
@RequestMapping("/locationPrice")
@Api(tags = "成本电价管理")
@Validated
public class OpLocationPriceController {

    private final OpLocationPriceService opLocationPriceService;

    public OpLocationPriceController(OpLocationPriceService opLocationPriceService) {
        this.opLocationPriceService = opLocationPriceService;
    }

    @PostMapping("/add")
    @ApiOperation(value = "新增成本电价", notes = "新增成本电价")
    public Result<Long> add(@Validated(LocationPriceDTO.Add.class) @RequestBody  LocationPriceDTO dto) {
        dto.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        return Result.ofSucceed(opLocationPriceService.add(dto));
    }

    @PostMapping("/edit")
    @ApiOperation(value = "编辑成本电价", notes = "编辑成本电价")
    public Result<Long> edit(@Validated(LocationPriceDTO.Update.class) @RequestBody LocationPriceDTO dto) {
        dto.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        return Result.ofSucceed(opLocationPriceService.edit(dto));
    }

    @GetMapping("/detail/{id}")
    @ApiOperation(value = "成本电价详情", notes = "成本电价详情")
    public Result<OpLocationPriceVO> detail(@PathVariable("id") Long id) {
        return Result.ofSucceed(opLocationPriceService.detail(id));
    }

    @GetMapping("/delete/{id}")
    @ApiOperation(value = "删除成本电价", notes = "删除成本电价")
    public Result<Long> delete(@PathVariable("id") Long id) {
        return Result.ofSucceed(opLocationPriceService.delete(id));
    }

    @GetMapping("/getPriceList")
    @ApiOperation(value = "成本电价列表包含场站", notes = "成本电价列表包含场站")
    public Result<OpLocationPriceInfoVo> getPriceList(@RequestParam("locationId") Long locationId) {
        return Result.ofSucceed(opLocationPriceService.getPriceList(locationId));
    }

    @GetMapping("/getList")
    @ApiOperation(value = "成本电价列表", notes = "成本电价列表")
    public Result<List<OpLocationPriceVO>> getList(@RequestParam("locationId") Long locationId) {
        return Result.ofSucceed(opLocationPriceService.getList(locationId));
    }
}
