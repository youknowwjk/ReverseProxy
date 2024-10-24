package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpCountryService;
import com.autel.cloud.pile.base.dto.OpCountryDTO;
import com.autel.cloud.pile.base.dto.UserAlpha2CodeAndLanguage;
import com.autel.cloud.pile.base.vo.OpCountryInfoVO;
import com.autel.cloud.pile.base.vo.OpCountryVO;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping(value = "/country")
@Api(value = "国家区号", tags = "国家区号")
@Validated
public class OpCountryController {
    @Autowired
    private OpCountryService opCountryService;

    @GetMapping(value = "/list")
    @ApiOperation(value = "查询国家区号列表")
    public List<OpCountryVO> list() {
        return opCountryService.list();
    }


    @GetMapping("/info")
    @ApiOperation(value = "根据国家缩写获取国家信息")
    public Result<List<OpCountryInfoVO>> getCountryInfo(@RequestHeader("accept-language") String language, @RequestParam List<String> abbrCodeList) {
        return Result.ofSucceed(opCountryService.getCountryInfo(language, abbrCodeList));
    }

    @GetMapping(value = "/getAll")
    @ApiOperation(value = "获取国家列表信息")
    public Result<List<OpCountryVO>> getAll(@RequestHeader(value = "accept-language", required = false, defaultValue = "en-US") String language) {
        log.info("language:{}", language);
        return Result.ofSucceed(opCountryService.getAll(language));
    }

    @GetMapping(value = "/getAllCode")
    @ApiOperation(value = "获取国家列表区号信息")
    public Result<List<OpCountryVO>> getAllCode() {
        String language = "en-US";
        return Result.ofSucceed(opCountryService.getAllCode(language));
    }

    @GetMapping(value = "/getCountryInfoByCountryAbbre")
    @ApiOperation(value = "根据国家缩写获取国家信息")
    public Result<List<OpCountryVO>> getCountryInfoByCountryAbbre(@RequestParam("countryAbbre") String countryAbbre) {
        UserAlpha2CodeAndLanguage userAlpha2CodeAndLanguage = new UserAlpha2CodeAndLanguage();
        userAlpha2CodeAndLanguage.setAlpha2Code(countryAbbre);
        return Result.ofSucceed(opCountryService.getCountryInfoByLanguageAndAlpha2Code(userAlpha2CodeAndLanguage));
    }

    /**
     * @param userAlpha2CodeAndLanguage
     * @return
     * @function 根据国家缩写和语言获取国家信息
     */
    @PostMapping(value = "/getCountryInfoByLanguageAndAlpha2Code")
    @ApiOperation(value = "根据国家缩写和语言获取国家信息")
    public Result<OpCountryVO> getCountryInfoByLanguageAndAlpha2Code(@RequestBody UserAlpha2CodeAndLanguage userAlpha2CodeAndLanguage) {
        List<OpCountryVO> countryInfoByLanguageAndAlpha2Code = opCountryService.getCountryInfoByLanguageAndAlpha2Code(userAlpha2CodeAndLanguage);
        OpCountryVO opCountryVO = new OpCountryVO();
        if (CollectionUtils.isNotEmpty(countryInfoByLanguageAndAlpha2Code)) {
            //（根据国家缩写和语言作为筛选条件，获取到的国家信息只会有一条记录）
            opCountryVO = countryInfoByLanguageAndAlpha2Code.get(0);
        }
        return Result.ofSucceed(opCountryVO);
    }

    @PostMapping(value = "/saveCountryCurrencyRelation")
    @ApiOperation(value = "保存国家和货币信息")
    public Result<List<OpCountryVO>> saveCountryCurrencyRelation(@RequestBody List<OpCountryDTO> requestDTO) {
        return Result.ofSucceed(opCountryService.saveCountryCurrencyRelation(requestDTO));
    }


    @GetMapping(value = "/syncCountryCurrencyId")
    @ApiOperation(value = "同步国家对应货币id信息")
    public Result<Boolean> syncCountryCurrencyId() {
        return Result.ofSucceed(opCountryService.syncCountryCurrencyId());
    }


    @PostMapping(value = "/getCountryInfoListByCountryAbbreList")
    @ApiOperation(value = "根据国家缩写批量获取国家信息")
    public Result<List<OpCountryVO>> getCountryInfoListByCountryAbbreList(@RequestBody List<String> countryAbbreList) {
        return Result.ofSucceed(opCountryService.getCountryInfoListByCountryAbbreList(countryAbbreList));
    }

    @PostMapping(value = "/getCountryMap")
    @ApiOperation(value = "根据国家缩写批量获取国家名称")
    public Result<Map<String,String>> getCountryMap(@RequestBody List<String> countryAbbreList) {
        return Result.ofSucceed(opCountryService.getCountryMap(countryAbbreList));
    }
}
