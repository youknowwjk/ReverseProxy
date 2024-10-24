package com.autel.cloud.pile.base.controller;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.data.JobQuery;
import com.autel.cloud.pile.base.domain.data.QuickChargingScheduleDTO;
import com.autel.cloud.pile.base.domain.service.impl.IntelligentChargingScheduling;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargingScheduleCountDownDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargingScheduleCountDownQueryDTO;
import com.autel.cloud.pile.bill.dto.ChargingScheduleTaskDTO;
import com.autel.cloud.smart.charge.dto.ChargingTaskDTO;
import com.autel.cloud.smart.charge.enums.SmartChargeStatusEnum;
import com.autel.cloud.smartbi.dto.ChargingSchedulePlanDTO;
import com.autel.cloud.smartbi.dto.SmartChargeStatusQueryDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.Set;

@RestController
@RequestMapping(value = "/IntelligentCharge")
@Api(tags = "公共智能充电调度")
@Slf4j
@Validated
public class IntelligentChargesScheduleController {

    @Resource
    private IntelligentChargingScheduling intelligentChargingScheduling;

    @PostMapping("/countDown")
    @ApiOperation(value = "调度智能充电倒计时", notes = "调度智能充电倒计时")
    public Result<ChargingScheduleCountDownDTO> countDown(@RequestBody ChargingScheduleCountDownQueryDTO chargingScheduleTaskDTO) {
        log.info("begin countDown {}", JSON.toJSONString(chargingScheduleTaskDTO));
        ChargingScheduleCountDownDTO chargingScheduleCountDownDTO = intelligentChargingScheduling.countDown(chargingScheduleTaskDTO);
        log.info("countDown result {}", JSON.toJSONString(chargingScheduleCountDownDTO));
        return Result.ofSucceed(chargingScheduleCountDownDTO);
    }

    @PostMapping("/schedule")
    @ApiOperation(value = "调度智能充电计划", notes = "调度智能充电计划")
    public Result<Boolean> schedule(@RequestBody ChargingScheduleTaskDTO chargingScheduleTaskDTO) {
        log.info("begin schedule {}", JSON.toJSONString(chargingScheduleTaskDTO));
        Boolean schedule = intelligentChargingScheduling.schedule(chargingScheduleTaskDTO);
        log.info("end schedule result {}", schedule);
        return Result.ofSucceed(schedule);
    }

    @PostMapping("/quickCharge")
    @ApiOperation(value = "立刻快充", notes = "立刻快充")
    public Result<Set<Long>> quickCharge(@RequestBody QuickChargingScheduleDTO chargingTaskDTO) {
        log.info("begin quickCharge: {}", JSON.toJSONString(chargingTaskDTO));
        // 立刻快充
        // 充电桩离线，无法切换为普通充电模式，建议您停止后重新启动
        // 充电桩网络连接不稳定，切换失败，建议您停止后重新启动

        // 切换成功

        Result<Set<Long>> result = intelligentChargingScheduling.quickCharge(chargingTaskDTO);
        log.info("end quickCharge: {}", result);
        return result;
    }

    @PostMapping("/schedulePlan")
    @ApiOperation(value = "调度智能充电计划", notes = "调度智能充电计划")
    public Result<ChargingSchedulePlanDTO> schedulePlan(@RequestBody ChargingTaskDTO chargingTaskDTO) {
        log.info("begin schedulePlan: {}", JSON.toJSONString(chargingTaskDTO));
        ChargingSchedulePlanDTO chargingSchedulePlanDTO = intelligentChargingScheduling.schedulePlan(chargingTaskDTO);
        log.info("end schedulePlan: {}", JSON.toJSONString(chargingSchedulePlanDTO));
        return Result.ofSucceed(chargingSchedulePlanDTO);
    }

    @PostMapping("/getSmartChargeStatus")
    @ApiOperation(value = "根据用户ID 和场站ID 查询智能充电状态", notes = "根据用户ID 和场站ID 查询智能充电状态")
    public Result<Integer> getSmartChargeStatus(@RequestBody SmartChargeStatusQueryDTO smartChargeStatusQueryDTO) {
        log.info("begin smartChargeStatusQueryDTO: {}", JSON.toJSONString(smartChargeStatusQueryDTO));
        int smartChargeStatus = intelligentChargingScheduling.getSmartChargeStatus(smartChargeStatusQueryDTO);
        log.info("end smartChargeStatusQueryDTO result: {}", smartChargeStatus);
        return Result.ofSucceed(smartChargeStatus);
    }

}
