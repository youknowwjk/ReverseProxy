package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationFacilityService;
import com.autel.cloud.pile.base.dto.OpLocationFacilityDTO;
import groovy.util.logging.Slf4j;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Api(tags = "场站周边设施控制类")
@RestController
@RequestMapping("/opLocationFacility")
@Slf4j
@Validated
public class OpLocationFacilityController {

    @Autowired
    private OpLocationFacilityService opLocationFacilityService;

    @ApiOperation(value = "新增周边设施")
    @PostMapping
    public Result<Long> addOpLocationFacility(@RequestBody OpLocationFacilityDTO opLocationFacilityDTO) {
        return opLocationFacilityService.addOpLocationFacility(opLocationFacilityDTO);
    }

    @ApiOperation(value = "修改周边设施")
    @PutMapping
    public Result<Boolean> updateOpLocationFacility(@RequestBody OpLocationFacilityDTO opLocationFacilityDTO) {
        return opLocationFacilityService.updateOpLocationFacility(opLocationFacilityDTO);
    }

    @ApiOperation(value = "删除周边设施")
    @DeleteMapping(value = "/{id}")
    public Result<Boolean> delOpLocationFacility(@PathVariable Long id) {
        return opLocationFacilityService.delOpLocationFacility(id);
    }

    @ApiOperation(value = "根据场站删除周边设施")
    @DeleteMapping(value = "/locationId/{locationId}")
    public Result<Boolean> delOpLocationFacilityByLocationId(@PathVariable Long locationId) {
        return opLocationFacilityService.delOpLocationFacilityByLocationId(locationId);
    }

    @ApiOperation(value = "查询场站周边设施")
    @GetMapping(value = "/list/locationId/{locationId}")
    public Result<List<OpLocationFacilityDTO>> selectOpLocationFacilityListByLocationId(@PathVariable Long locationId) {
        return opLocationFacilityService.selectOpLocationFacilityListByLocationId(locationId);
    }
}
