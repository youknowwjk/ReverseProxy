package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpPosAuthorizedAmountDistributeService;
import com.autel.cloud.pile.base.dto.pos.SetPosAuthorizedAmountDTO;
import com.autel.cloud.pile.base.vo.pos.PileVO;
import com.autel.cloud.pile.base.vo.pos.PosAuthorizedAmountDistributeVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 控制逻辑层
 */
@RestController
@RequestMapping("/opPosAuthorizedAmountDistribute")
@Api(tags = "本地POS预授权金额修改工具设置", value = "本地POS预授权金额修改工具设置")
@Slf4j
@Validated
public class OpPosAuthorizedAmountDistributeController {

    @Autowired
    private OpPosAuthorizedAmountDistributeService opPosAuthorizedAmountDistributeService;

    /**
     * @param pageDTO
     * @return
     * @function 分页查询充电桩序列号下拉列表
     */
    @PostMapping("/getPileDropDownPageList")
    @ApiOperation(value = "分页查询充电桩序列号下拉列表", notes = "分页查询充电桩序列号下拉列表")
    public Result<Page<PileVO>> getPileDropDownPageList(@RequestBody PageDTO pageDTO) {

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeController.getPileDropDownPageList pageDTO : {}",
                JSON.toJSONString(pageDTO));

        return Result.ofSucceed(opPosAuthorizedAmountDistributeService.getPileDropDownPageList(pageDTO));
    }

    /**
     * @param setPosAuthorizedAmountDTO
     * @return
     * @function 修改本地POS预授权金额
     */
    @PostMapping("/setPosAuthorizedAmount")
    @ApiOperation(value = "修改本地POS预授权金额", notes = "修改本地POS预授权金额")
    public Result<Boolean> setPosAuthorizedAmount(@RequestBody SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTO) {

        log.info("===>>>OpPosAuthorizedAmountDistributeController.setPosAuthorizedAmount setPosAuthorizedAmountDTO : {}",
                JSON.toJSONString(setPosAuthorizedAmountDTO));

        return Result.ofSucceed(opPosAuthorizedAmountDistributeService.setPosAuthorizedAmount(setPosAuthorizedAmountDTO));
    }

    /**
     * @param pageDTO
     * @return
     * @function 分页查询本地POS预授权金额修改工具设置记录
     */
    @PostMapping("/pages")
    @ApiOperation(value = "分页查询本地POS预授权金额修改工具设置记录", notes = "分页查询本地POS预授权金额修改工具设置记录")
    public Result<Page<PosAuthorizedAmountDistributeVO>> queryPages(@RequestBody PageDTO pageDTO) {

        log.info("=====>>>>>OpPosAuthorizedAmountDistributeController.queryPages pageDTO : {}",
                JSON.toJSONString(pageDTO));

        return Result.ofSucceed(opPosAuthorizedAmountDistributeService.queryPages(pageDTO));
    }
}
