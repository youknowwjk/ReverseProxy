package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.service.OpLocationIconService;
import com.autel.cloud.pile.base.dto.OpLocationIconDTO;
import com.autel.cloud.pile.base.vo.OpLocationIconVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * <p>
 * 图标信息（icon）控制类
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
@Api(tags = "图标信息（icon）控制类")
@RestController
@RequestMapping("/opLocationIcon")
@Slf4j
@Validated
public class OpLocationIconController {

    @Autowired
    private OpLocationIconService opLocationIconService;

    /**
     * 获得所有图标信息（icon）条件查询
     *
     * @param opLocationIconDTO
     * @param httpServletRequest
     * @return
     */
    @PostMapping("/getAllIcon")
    @ApiOperation(value = "获得所有图标信息（icon）")
    public Result<List<OpLocationIconVO>> getAllIcon(@Validated @RequestBody OpLocationIconDTO opLocationIconDTO, HttpServletRequest httpServletRequest) {
        String language = httpServletRequest.getHeader(BaseConstant.ACCEPT_LANGUAGE);
        opLocationIconDTO.setLanguage(language);
        return opLocationIconService.getAllIcon(opLocationIconDTO);
    }

    /**
     * 单个添加图标信息（icon）
     *
     * @param opLocationIconDTO
     * @param httpServletRequest
     * @return
     */
    @PostMapping("/addIcon")
    @ApiOperation(value = "新增图标信息（icon）")
    public Result<Boolean> addIcon(@Validated @RequestBody OpLocationIconDTO opLocationIconDTO, HttpServletRequest httpServletRequest) {
        String language = httpServletRequest.getHeader(BaseConstant.ACCEPT_LANGUAGE);
        opLocationIconDTO.setLanguage(language);
        return opLocationIconService.addIcon(opLocationIconDTO);
    }

    /**
     * 单个修改图标信息（icon）
     *
     * @param opLocationIconDTO
     * @param httpServletRequest
     * @return
     */
    @PostMapping("/editIcon")
    @ApiOperation(value = "编辑图标信息（icon）")
    public Result<Boolean> editIcon(@Validated @RequestBody OpLocationIconDTO opLocationIconDTO, HttpServletRequest httpServletRequest) {
        String language = httpServletRequest.getHeader(BaseConstant.ACCEPT_LANGUAGE);
        opLocationIconDTO.setLanguage(language);
        return opLocationIconService.editIcon(opLocationIconDTO);
    }

    /**
     * 初始化图标信息（icon）
     *
     * @return
     */
    @GetMapping("/initializeIconInformation")
    @ApiOperation(value = "初始化图标信息（icon）")
    public Result<Boolean> initializeIconInformation() {
        return opLocationIconService.initializeIconInformation();
    }
}
