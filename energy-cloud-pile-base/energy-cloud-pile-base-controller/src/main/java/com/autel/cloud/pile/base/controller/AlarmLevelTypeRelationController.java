package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.AlarmLevelTypeRelationService;
import com.autel.cloud.pile.base.dto.AlarmLevelTypeRelationDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@Api(tags = "告警规则设置")
@RestController
@Validated
@RequestMapping(value = "/alarm/levelTypeRelation")
public class AlarmLevelTypeRelationController {

    private final AlarmLevelTypeRelationService alarmLevelTypeRelationService;

    public AlarmLevelTypeRelationController(AlarmLevelTypeRelationService alarmLevelTypeRelationService) {
        this.alarmLevelTypeRelationService = alarmLevelTypeRelationService;
    }

    @Log(title = "新增告警规则", businessType = BusinessType.INSERT,code = "50201402")
    @PostMapping
    @ApiOperation(value = "新增告警规则")
    public Boolean addAlarmLevelTypeRelation(@Validated @RequestBody AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        return alarmLevelTypeRelationService.addAlarmLevelTypeRelation(alarmLevelTypeRelationDTO);
    }

    @Log(title = "修改告警规则", businessType = BusinessType.UPDATE,code = "50201403")
    @PutMapping
    @ApiOperation(value = "修改告警规则")
    public Boolean updateAlarmLevelTypeRelation(@Validated @RequestBody AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        return alarmLevelTypeRelationService.updateAlarmLevelTypeRelation(alarmLevelTypeRelationDTO);
    }

    @Log(title = "删除告警规则", businessType = BusinessType.DELETE,code = "50201404")
    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除告警规则")
    public Boolean delAlarmLevelTypeRelation(@PathVariable Long id) {
        return alarmLevelTypeRelationService.delAlarmLevelTypeRelation(id);
    }

    @GetMapping(value = "/page")
    @ApiOperation(value = "分页查询告警规则")
    public PageVO<AlarmLevelTypeRelationDTO> selectAlarmLevelTypeRelationPage(@Validated @RequestBody AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        return alarmLevelTypeRelationService.selectAlarmLevelTypeRelationPage(alarmLevelTypeRelationDTO);
    }
}
