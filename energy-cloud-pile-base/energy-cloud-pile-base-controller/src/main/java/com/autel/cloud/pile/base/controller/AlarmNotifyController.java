package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.AlarmNotifyService;
import com.autel.cloud.pile.base.dto.AlarmNotifyDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Api(tags = "告警通知设置")
@RestController
@Validated
@RequestMapping(value = "/alarm/notify")
public class AlarmNotifyController {

    private final AlarmNotifyService alarmNotifyService;

    public AlarmNotifyController(AlarmNotifyService alarmNotifyService) {
        this.alarmNotifyService = alarmNotifyService;
    }

    @Log(title = "新增告警通知", businessType = BusinessType.INSERT,code = "50201405")
    @PostMapping
    @ApiOperation(value = "新增告警通知")
    public Boolean addAlarmNotify(@RequestBody AlarmNotifyDTO alarmNotifyDTO) {
        return alarmNotifyService.addAlarmNotify(alarmNotifyDTO);
    }

    @Log(title = "修改告警通知", businessType = BusinessType.UPDATE,code = "50201406")
    @PutMapping
    @ApiOperation(value = "修改告警通知")
    public Boolean updateAlarmNotify(@RequestBody AlarmNotifyDTO alarmNotifyDTO) {
        return alarmNotifyService.updateAlarmNotify(alarmNotifyDTO);
    }
    @Log(title = "删除告警通知", businessType = BusinessType.DELETE,code = "50201407")
    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除告警通知")
    public Boolean delAlarmNotify(@PathVariable Long id) {
        return alarmNotifyService.delAlarmNotify(id);
    }

    @GetMapping(value = "/page")
    @ApiOperation(value = "分页查询告警通知")
    public PageVO<AlarmNotifyDTO> selectAlarmNotifyPage(@RequestBody AlarmNotifyDTO alarmNotifyDTO) {
        return alarmNotifyService.selectAlarmNotifyPage(alarmNotifyDTO);
    }
}
