package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.DataStatsService;
import com.autel.cloud.pile.base.vo.EvseForStatsVO;
import com.autel.cloud.pile.base.vo.GunStatsVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

/**
 * @Author A22282
 * @Date 2022/4/21 11:14
 */
@RestController
@Validated
@RequestMapping("/dataStats")
@Api(value = "数据概览",tags = "数据概览")
public class DataStatsController {
    @Autowired
    private DataStatsService dataStatsService;

    @PostMapping("/evseStats")
    @ApiOperation(value = "充电设施",notes = "充电设施")
    public Result<EvseForStatsVO> evseStats(@RequestParam(value = "operatorId",required = false) Long operatorId){
        if (Objects.isNull(operatorId)){
            return dataStatsService.getAllEvseStats();
        }
        return dataStatsService.evseStats(operatorId);
    }

    @PostMapping("/gunStats")
    @ApiOperation(value = "枪状态",notes = "枪状态")
    public Result<List<GunStatsVO>> gunStats(@RequestParam(value = "operatorId",required = false) Long operatorId){
        if (Objects.isNull(operatorId)){
            return dataStatsService.getAllGunStats();
        }
        return dataStatsService.gunStats(operatorId);
    }
}
