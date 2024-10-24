package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.alibaba.nacos.client.naming.utils.CollectionUtils;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OicpService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.oicp.BaseEsIdsDTO;
import com.autel.cloud.pile.base.dto.oicp.EroamingEvseData;
import com.autel.cloud.pile.base.dto.oicp.EvseSnStatusVO;
import com.autel.cloud.pile.base.dto.oicp.PileStatusToEsDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationSavePileEsDTO;
import com.autel.cloud.pile.base.vo.EvseDynamicPricingVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author X21216
 * @date 2023/3/23  17:58
 */
@RestController
@Api(tags = "OICP接口管理")
@RequestMapping("/oicp")
@Slf4j
@Validated
public class OicpPileBaseController {

    @Resource
    private OicpService oicpService;

    @Resource
    private OpLocationService opLocationService;

    @GetMapping("/eroamingEvseData/page")
    @ApiOperation(value = "分页查询互联互通桩信息")
    public Result<Page<EroamingEvseData>> queryEroamingEvseDatas(@RequestParam("page") Integer page,
                                                                 @RequestParam("pageSize") Integer pageSize) {
        Page<EroamingEvseData> eroamingEvseDataPage = oicpService.queryEvseDate(page, pageSize);
        return Result.ofSucceed(eroamingEvseDataPage);
    }

    @PostMapping("/savePileToEs")
    @ApiOperation(value = "存储hubject的桩数据到es", notes = "存储hubject的桩数据到es")
    public Result<Boolean> savePileToEs(@RequestBody OpLocationSavePileEsDTO opLocationSavePileEsDTO) {
        if (opLocationSavePileEsDTO == null) {
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
        return opLocationService.savePileToEs(opLocationSavePileEsDTO);
    }

    @PostMapping("/deletePileToEs")
    @ApiOperation(value = "删除hubject的es桩数据", notes = "删除hubject的es桩数据")
    public Result<Boolean> deletePileToEs(@RequestBody BaseEsIdsDTO baseEsIdsDTO,
                                          @RequestParam(value = "isAll", required = false) String isAll) {
        if (CollectionUtils.isEmpty(baseEsIdsDTO.getLocationIds())
                && CollectionUtils.isEmpty(baseEsIdsDTO.getOpLocationEvseIds())
                && !Boolean.parseBoolean(isAll)) {
            return Result.ofSucceed(Boolean.TRUE);
        }
        return opLocationService.deletePileToEs(baseEsIdsDTO, Boolean.parseBoolean(isAll));
    }

    @PostMapping("/savePileStatusToEs")
    @ApiOperation(value = "存储hubject的桩状态到es", notes = "存储hubject的桩状态到es")
    public Result<Boolean> savePileStatusToEs(@RequestBody List<PileStatusToEsDTO> pileStatusToEsDTOS) {
        if (CollectionUtils.isEmpty(pileStatusToEsDTOS)) {
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
        return opLocationService.savePileStatusToEs(pileStatusToEsDTOS);
    }

    @PostMapping("/statusByEvseSn")
    @ApiOperation(value = "根据桩sn查询桩状态")
    Result<List<EvseSnStatusVO>> gunStatusByEvseSn(@RequestBody List<String> EvseSnList) {
        return opLocationService.gunStatusByEvseSn(EvseSnList);
    }


    /**
     * @param pageDTO
     * @return
     * @function 提供清单页面供EMP拉取(OICP Hubject)
     */
    @PostMapping("/page")
    @ApiOperation(value = "提供清单页面供EMP拉取(OICP Hubject)", notes = "提供清单页面供EMP拉取(OICP Hubject)")
    public Result<Page<EvseDynamicPricingVO>> page(@RequestBody PageDTO pageDTO) {

        log.info("=====>>>>>OicpPileBaseController.page pageDTO : {}", JSON.toJSONString(pageDTO));

        return Result.ofSucceed(oicpService.page(pageDTO));
    }
}
