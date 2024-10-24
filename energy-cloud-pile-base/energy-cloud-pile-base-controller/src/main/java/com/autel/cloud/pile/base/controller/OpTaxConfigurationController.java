package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpTaxConfigurationService;
import com.autel.cloud.pile.base.dto.tax.StationLocationDTO;
import com.autel.cloud.pile.base.vo.tax.OpTaxConfigurationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置 控制逻辑层
 */
@RestController
@RequestMapping("/opTaxConfiguration")
@Api(tags = "税费重构——Autel管理端-默认税率配置", value = "税费重构——Autel管理端-默认税率配置")
@Slf4j
@Validated
public class OpTaxConfigurationController {

    @Autowired
    private OpTaxConfigurationService opTaxConfigurationService;

    /**
     * @param stationLocationDTO 场站地理位置信息
     * @return Autel管理端的默认税率配置信息
     * @function 税费重构——根据场站地理位置信息获取Autel管理端的默认税率配置信息
     */
    @PostMapping(value = "/getOpTaxConfigurationByStationLocation")
    @ApiOperation(value = "税费重构——根据场站地理位置信息获取Autel管理端的默认税率配置信息", notes = "税费重构——根据场站地理位置信息获取Autel管理端的默认税率配置信息")
    public Result<OpTaxConfigurationVO> getOpTaxConfigurationByStationLocation(@Validated @RequestBody StationLocationDTO stationLocationDTO) {

        log.info("====>>>>OpTaxConfigurationController.getOpTaxConfigurationByStationLocation stationLocationDTO : {}", JSON.toJSONString(stationLocationDTO));

        return Result.ofSucceed(opTaxConfigurationService.getOpTaxConfigurationByStationLocation(stationLocationDTO));
    }

    /**
     * @param stationLocationDTO 场站地理位置信息
     * @return Autel管理端-默认税率配置数据列表
     * @function 税费重构——分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）
     */
    @PostMapping(value = "/getOpTaxConfigurationPageList")
    @ApiOperation(value = "税费重构——分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）", notes = "税费重构——分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）")
    public Result<Page<OpTaxConfigurationVO>> getOpTaxConfigurationPageList(@Validated @RequestBody StationLocationDTO stationLocationDTO) {

        log.info("====>>>>OpTaxConfigurationController.getOpTaxConfigurationPageList stationLocationDTO : {}", JSON.toJSONString(stationLocationDTO));

        return Result.ofSucceed(opTaxConfigurationService.getOpTaxConfigurationPageList(stationLocationDTO));
    }

    /**
     * @param request  请求对象
     * @param response 响应对象
     * @return 默认税率模版文件
     * @function 税费重构——下载默认税率模版文件
     */
    @GetMapping("/downloadDefaultTaxTemplateXLSX")
    @ApiOperation(value = "税费重构——下载默认税率模版文件", notes = "税费重构——下载默认税率模版文件")
    public Result<Void> downloadDefaultTaxTemplateXLSX(HttpServletRequest request, HttpServletResponse response) {
        return Result.ofSucceed(opTaxConfigurationService.downloadDefaultTaxTemplateXLSX(request, response));
    }

    /**
     * @param request  请求对象
     * @param response 响应对象
     * @return 默认税率数据文件
     * @function 税费重构——导出默认税率数据文件
     */
    @GetMapping("/exportDefaultTaxDataFileXLSX")
    @ApiOperation(value = "税费重构——导出默认税率数据文件", notes = "税费重构——导出默认税率数据文件")
    public Result<Void> exportDefaultTaxDataFileXLSX(HttpServletRequest request, HttpServletResponse response) {
        return Result.ofSucceed(opTaxConfigurationService.exportDefaultTaxDataFileXLSX(request, response));
    }

    /**
     * @param multipartFile 文件对象
     * @return 上传操作是否成功的标志
     * @function 税费重构——上传默认税率数据文件
     */
    @PostMapping(value = "/uploadDefaultTaxDataFileXLSX")
    @ApiOperation(value = "税费重构——上传默认税率数据文件", notes = "税费重构——上传默认税率数据文件")
    public Result<Boolean> uploadDefaultTaxDataFileXLSX(@RequestParam("file") MultipartFile multipartFile) {
        return Result.ofSucceed(opTaxConfigurationService.uploadDefaultTaxDataFileXLSX(multipartFile));
    }
}
