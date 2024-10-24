package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpEvseBrandService;
import com.autel.cloud.pile.base.vo.OpEvseBrandVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@Api(tags = "品牌")
@RestController
@RequestMapping(value = "/opEvseBrand")
@Validated
public class OpEvseBrandController {
    @Autowired
    private OpEvseBrandService opEvseBrandService;

    @GetMapping(value = "/list")
    @ApiOperation(value = "查询品牌列表")
    public Result<List<OpEvseBrandVO>> list() {
        return opEvseBrandService.list();
    }

}
