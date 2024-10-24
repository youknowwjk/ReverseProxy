package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.AlarmService;
import com.autel.cloud.pile.base.dto.AlarmDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Api(tags = "告警数据")
@RestController
@RequestMapping(value = "/alarm")
@Validated
public class AlarmController {
    private final AlarmService alarmService;

    public AlarmController(AlarmService alarmService) {
        this.alarmService = alarmService;
    }

    @Log(title = "新增告警", businessType = BusinessType.INSERT,code = "50201401")
    @PostMapping
    @ApiOperation(value = "新增告警")
    public Boolean addAlarm(@Validated @RequestBody AlarmDTO alarmDTO) {
        return alarmService.addAlarm(alarmDTO);
    }

    @GetMapping(value = "/page")
    @ApiOperation(value = "分页查询告警")
    public PageVO<AlarmDTO> selectAlarmPage(@Validated @RequestBody AlarmDTO alarmDTO) {
        return alarmService.selectAlarmPage(alarmDTO);
    }

    @GetMapping(value = "/list")
    @ApiOperation(value = "查询告警列表")
    public List<AlarmDTO> selectAlarmList(@Validated @RequestBody AlarmDTO alarmDTO) {
        return alarmService.selectAlarmList(alarmDTO);
    }


    @DeleteMapping(value = "/{evseSn}")
    @ApiOperation(value = "查询告警列表")
    public Boolean delAlarmByEvseSn(@PathVariable String evseSn) {
        return alarmService.delAlarmByEvseSn(evseSn);
    }
}
