package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.OpEvseBrandModelService;
import com.autel.cloud.pile.base.dto.OpEvseBrandModelDTO;
import com.autel.cloud.pile.base.dto.VerifyBrandNameAndProductModelDTO;
import com.autel.cloud.pile.base.vo.OpEvseBrandModelVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 桩品牌型号表
 * </p>
 *
 * @author A22587
 * @since 2022-09-13
 */
@Api(tags = "桩品牌型号表")
@RestController
@RequestMapping(value = "/opEvseBrandModel")
@Validated
public class OpEvseBrandModelController {

    @Autowired
    private OpEvseBrandModelService opEvseBrandModelService;

    @Log(title = "产品信息录入")
    @PostMapping("/addOpEvseBrandModel")
    @ApiOperation(value = "产品信息录入", notes = "产品信息录入")
    public Result<OpEvseBrandModelVO> addOpEvseBrandModel(@RequestBody @Valid OpEvseBrandModelDTO opEvseBrandModelDTO) {
        return opEvseBrandModelService.addOpEvseBrandModel(opEvseBrandModelDTO);
    }

    @Log(title = "分页查询产品型号信息")
    @PostMapping("/pages")
    @ApiOperation(value = "分页查询产品型号信息", notes = "分页查询产品型号信息")
    public Result<Page<OpEvseBrandModelVO>> pages(@RequestBody OpEvseBrandModelDTO opEvseBrandModelDTO) {
        return opEvseBrandModelService.pages(opEvseBrandModelDTO);
    }

    @Log(title = "列表查询（根据品牌条件）")
    @GetMapping("/queryOpEvseBrandModelByBrand/{brandId}")
    @ApiOperation(value = "列表查询（根据品牌条件）", notes = "列表查询（根据品牌条件）")
    public Result<List<OpEvseBrandModelVO>> queryOpEvseBrandModelByBrand(@PathVariable("brandId") Long brandId) {
        return opEvseBrandModelService.queryOpEvseBrandModelByBrand(brandId);
    }

    @Log(title = "产品型号详情")
    @GetMapping("/opEvseBrandModelDetail/{id}")
    @ApiOperation(value = "产品型号详情", notes = "产品型号详情")
    public Result<OpEvseBrandModelVO> opEvseBrandModelDetail(@PathVariable("id") Long id) {
        return opEvseBrandModelService.opEvseBrandModelDetail(id);
    }

    @Log(title = "编辑产品型号信息")
    @PostMapping("/updateOpEvseBrandModel")
    @ApiOperation(value = "编辑产品型号信息", notes = "编辑产品型号信息")
    public Result<Boolean> updateOpEvseBrandModel(@RequestBody @Valid OpEvseBrandModelDTO opEvseBrandModelDTO) {
        return opEvseBrandModelService.updateOpEvseBrandModel(opEvseBrandModelDTO);
    }

    @Log(title = "删除产品型号信息")
    @PostMapping("/deleteOpEvseBrandModel")
    @ApiOperation(value = "删除产品型号信息", notes = "删除产品型号信息")
    public Result<Boolean> deleteOpEvseBrandModel(@RequestBody OpEvseBrandModelDTO opEvseBrandModelDTO) {
        return opEvseBrandModelService.deleteOpEvseBrandModel(opEvseBrandModelDTO);
    }

    @Log(title = "批量导入产品型号信息")
    @PostMapping("/saveOpEvseBrandModelList")
    @ApiOperation(value = "批量导入产品型号信息", notes = "批量导入产品型号信息")
    public Result<Boolean> saveOpEvseBrandModelList(@RequestBody List<OpEvseBrandModelDTO> opEvseBrandModelDTOList) {
        return opEvseBrandModelService.saveOpEvseBrandModelList(opEvseBrandModelDTOList);
    }

    @Log(title = "品牌与品牌型号的校验")
    @PostMapping("/verifyBrandNameAndProductModel")
    @ApiOperation(value = "品牌与品牌型号的校验", notes = "品牌与品牌型号的校验")
    public Result<Boolean> checkBrandNameAndProductModel(@RequestBody VerifyBrandNameAndProductModelDTO verifyBrandNameAndProductModelDTO) {
        return opEvseBrandModelService.verifyBrandNameAndProductModel(verifyBrandNameAndProductModelDTO);
    }

    @Log(title = "批量导入产品型号")
    @PostMapping("/uploadBrandModelModuleXls")
    @ApiOperation(value = "批量导入产品型号", notes = "批量导入产品型号")
    public Result<Boolean> uploadBrandModelModuleXls(@RequestParam("file") MultipartFile multipartFile) {
        return opEvseBrandModelService.uploadBrandModelModuleXls(multipartFile);
    }

    @Log(title = "三方桩导入模板下载", businessType = BusinessType.EXPORT, code = "50201434")
    @GetMapping("/downModuleXls")
    @ApiOperation(value = "三方桩导入模板下载", notes = "三方桩导入模板下载")
    public Result<Void> downModuleXls(HttpServletRequest request, HttpServletResponse response) {
        return opEvseBrandModelService.downModuleResourceXls(request, response);
    }

    @Log(title = "根据品牌和产品型号查询电流和电压")
    @PostMapping("/queryByBrandNameAndProductModel")
    @ApiOperation(value = "根据品牌和产品型号查询电流和电压", notes = "根据品牌和产品型号查询电流和电压")
    public Result<OpEvseBrandModelVO> queryByBrandNameAndProductModel(@RequestBody OpEvseBrandModelDTO opEvseBrandModelDTO) {
        return opEvseBrandModelService.queryByBrandNameAndProductModel(opEvseBrandModelDTO);
    }

}
