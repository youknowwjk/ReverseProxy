package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationStatisticsService;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseStatisticsDTO;
import com.autel.cloud.pile.base.dto.OpLocationOperationInfoDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * @ClassName OpLocationStatisticsController
 * @Author A22121
 * @Description
 * @Date 2022/4/18 14:24
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@RequestMapping("/opLocationStatistics")
@Api(tags = "场站运营数据")
@Validated
public class OpLocationStatisticsController {

    private final OpLocationStatisticsService opLocationStatisticsService;

    public OpLocationStatisticsController(OpLocationStatisticsService opLocationStatisticsService) {
        this.opLocationStatisticsService = opLocationStatisticsService;
    }


    @PostMapping("/getEvse")
    @ApiOperation(value = "查询充电设备-统计", notes = "查询充电设备-统计")
    public Result<OpLocationEvseStatisticsDTO> getEvse(@RequestBody @Valid OpLocationEvseDTO opLocationEvseDTO) {
        return opLocationStatisticsService.getEvse(opLocationEvseDTO);
    }

    @PostMapping("/locationOperationInfo")
    @ApiOperation(value = "查询场站运营情况", notes = "查询场站运营情况")
    public Result<OpLocationOperationInfoDTO> locationOperationInfo(@RequestParam Long locationId) {
        return opLocationStatisticsService.locationOperationInfo(locationId);
    }

    @PostMapping("/locationOperationInfoList")
    @ApiOperation(value = "查询场站运营情况折线图", notes = "查询场站运营情况折线图")
    public Result<Map<String, List<String>>> locationOperationInfoList(@RequestParam Long locationId) {
        return opLocationStatisticsService.locationOperationInfoList(locationId);
    }
}
