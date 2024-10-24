package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.dto.UserOpLogDTO;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.UserOpLogService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/18 10:09
 */
@Api(tags = "用户操作日志")
@RestController
@RequestMapping(value = "/userlog")
@Validated
public class UserOpLogController {

    @Autowired
    UserOpLogService userOpLogService;

    @PostMapping("/queryPages")
    @ApiOperation(value = "分页查询")
    public Result<PageVO<UserOpLogDTO>> queryPages(@Validated @RequestBody UserOpLogDTO requestDto) {
        PageVO<UserOpLogDTO> pages = userOpLogService.queryPages(requestDto);
        return Result.ofSucceed(pages);
    }

    @Log(title = "导出(用户操作日志)", businessType = BusinessType.EXPORT, code = "50201441")
    @ApiOperation(value = "导出", notes = "导出", httpMethod = "POST")
    @PostMapping("/export")
    public void export(@Validated @RequestBody UserOpLogDTO requestDto, HttpServletRequest request,
                       HttpServletResponse response) {
        userOpLogService.export(requestDto, request, response);
    }

    @PostMapping("/save")
    @ApiOperation(value = "保存")
    public Integer save(@Validated @RequestBody UserOpLogDTO userOpLogDto) {
        return userOpLogService.save(userOpLogDto);
    }
}
