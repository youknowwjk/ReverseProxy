package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.StaticFieldConstant;
import com.autel.cloud.pile.base.domain.service.OpEvseTypeService;
import com.autel.cloud.pile.base.domain.service.OpLocationConnectorService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.dto.app.GunListDto;
import com.autel.cloud.pile.base.vo.app.GunListPageDto;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author: A22327
 * @CreateTime: 2022/5/23 9:36
 * @Description: app控制器
 */
@Api(tags = "app相关的api")
@RestController
@RequestMapping(value = "/api")
@Log4j2
@Validated
public class AppController {

    @Value("#{'${connector.type:}'.empty ? null : '${connector.type:}'.split(',')}")
    private List<String> connectorTypes;

    @Value("#{'${connector.powerRange:}'.empty ? null : '${connector.powerRange:}'.split(',')}")
    private List<Integer> powerRange;

    @Autowired
    private OpLocationEvseService opLocationEvseService;

    @Autowired
    private OpEvseTypeService opEvseTypeService;

    @Resource
    private OpLocationConnectorService opLocationConnectorService;


    @ApiOperation(value = "获取枪类型列表")
    @GetMapping(value = "/getGunType")
    public Result<List<GunTypeRespDTO>> getGunType(@RequestParam(value = "type",required = false,defaultValue = "") String type) {
        if (StringUtils.isNotEmpty(type)){
        return   Result.ofSucceed(opLocationConnectorService.getGunTypeByType(type));
        }
        return opEvseTypeService.getGunType();
    }

    @ApiOperation(value = "同步枪类型列表")
    @GetMapping(value = "/syncGunType")
    public Result<Boolean> syncGunType() {
        return opEvseTypeService.syncGunType();
    }

    @ApiOperation(value = "枪功率列表")
    @GetMapping(value = "/powerRange")
    public Result<List<Integer>> getPowerRange() {
        if (CollectionUtils.isEmpty(StaticFieldConstant.powerRangeList)) {
            StaticFieldConstant.powerRangeList = new ArrayList<>();
            StaticFieldConstant.powerRangeList.addAll(powerRange);
        }
        return Result.ofSucceed(StaticFieldConstant.powerRangeList);
    }
    @ApiOperation(value = "根据枪类型获取枪功率列表")
    @GetMapping(value = "/powerRangeByGunType")
    public Result<List<Double>> getPowerRangeByGunType(@RequestParam(value = "gunType",defaultValue = "3")int gunType) {
        return Result.ofSucceed(opLocationConnectorService.getPowerByGunType(gunType));
    }


    @ApiOperation(value = "根据站点ID分页获取枪列表")
    @PostMapping(value = "/pileList")
    public Result<PageDTO<GunListPageDto>> getGunListRulesByStationId(@RequestBody GunListDto gunListDto) {
        return opLocationEvseService.getGunListRulesByStationId(gunListDto);

    }
}
