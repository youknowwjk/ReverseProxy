package com.autel.cloud.pile.base.controller;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.EvseStatisticService;
import com.autel.cloud.pile.base.dto.EvseStatisticDto;
import com.autel.cloud.pile.base.dto.statistics.GunStatusQueryDTO;
import com.autel.cloud.pile.base.dto.statistics.GunStatusStatisDTO;
import com.autel.cloud.pile.base.vo.EvsePowerStatisticVO;
import com.autel.cloud.pile.base.vo.EvseRealtimeDataVO;
import com.autel.cloud.pile.base.vo.EvseStatusStatisticVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Api(tags = "充电设备实时监控")
@RestController
@RequestMapping(value = "/statistic/evse")
@Log4j2
@Validated
public class EvseStatisticController {

    @Autowired
    private EvseStatisticService evseStatisticService;

    @ApiOperation(value = "充电枪状态统计")
    @GetMapping(value = "/evseStatus")
    public Result<EvseStatusStatisticVO> statisticEvseStatus(@Validated @RequestBody EvseStatisticDto evseStatisticDto) {
        return evseStatisticService.statisticEvseStatus(evseStatisticDto);
    }

    @ApiOperation(value = "充电枪实时数据")
    @GetMapping(value = "/realtimeData")
    public Result<List<EvseRealtimeDataVO>> getEvseRealtimeData(@Validated @RequestBody EvseStatisticDto evseStatisticDto) {
        return evseStatisticService.getEvseRealtimeData(evseStatisticDto);
    }

    @ApiOperation(value = "充电枪实时数据(详情)")
    @GetMapping(value = "/realtimeDataDetail")
    public Result<EvseRealtimeDataVO> getEvseRealtimeDataDetail(
            @ApiParam(value = "场站Id")
            @RequestParam Long locationUid,
            @ApiParam(value = "充电设备Id")
            @RequestParam Long evseUid) {
        return evseStatisticService.getEvseRealtimeDataDetail(locationUid, evseUid);
    }

    @ApiOperation(value = "充电枪功率统计")
    @GetMapping(value = "/statisticPower")
    public Result<List<EvsePowerStatisticVO>> getStatisticPower(@ApiParam(value = "场站Id")
                                                                @RequestParam Long locationUid,
                                                                @ApiParam(value = "充电设备Id")
                                                                @RequestParam Long evseUid) {
        return evseStatisticService.getStatisticPower(locationUid, evseUid);
    }

    @ApiOperation(value = "桩详情-运行信息-查询近7天桩的枪状态变化信息", notes = "查询近7天桩的枪状态信息")
    @PostMapping("/gunStatusInfo")
    public Result<GunStatusStatisDTO> getGun7DayStatusInfo(@Validated @RequestBody GunStatusQueryDTO gunStatusQueryDTO) {
        return Result.ofSucceed(evseStatisticService.getGunStatusStatis(gunStatusQueryDTO));
    }

}
