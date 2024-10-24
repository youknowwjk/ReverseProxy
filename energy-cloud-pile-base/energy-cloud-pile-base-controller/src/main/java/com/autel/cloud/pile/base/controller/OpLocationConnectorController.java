package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationConnectorService;
import com.autel.cloud.pile.base.dto.OpLocationConnectorPagingDTO;
import com.autel.cloud.pile.base.vo.OpLocationConnectorPagingVo;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

@SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
@Api(tags = "连接器相关")
@RestController
@RequestMapping(value = "/opConnector")
@Log4j2
@Validated
public class OpLocationConnectorController {

    private final OpLocationConnectorService opLocationConnectorService;

    public OpLocationConnectorController(OpLocationConnectorService opLocationConnectorService) {
        this.opLocationConnectorService = opLocationConnectorService;
    }


    //@ApiOperation(value = "连接器分页列表")
    //@GetMapping(value = "/page")
    public Result<Page<OpLocationConnectorPagingDTO>> connectorPaging(@RequestBody OpLocationConnectorPagingVo opLocationConnectorPagingVo) {
        return opLocationConnectorService.paging(opLocationConnectorPagingVo);
    }

    @ApiOperation(value = "根据枪ID查询枪详情")
    @PostMapping(value = "/getOpLocationConnectorDetail")
    public Result<OpLocationConnectorPagingDTO> connectorPaging(@RequestParam("connectorId") Long connectorId) {
        return opLocationConnectorService.getOpLocationConnectorDetail(connectorId);
    }

    @ApiOperation(value = "根据枪ID查询枪详情,包括已经删除的")
    @PostMapping(value = "/getOpLocationConnectorDetailDeleted")
    public Result<OpLocationConnectorPagingDTO> connectorPagingDeleted(@RequestParam("connectorId") Long connectorId) {
        return opLocationConnectorService.getOpLocationConnectorDetailDeleted(connectorId);
    }

    @ApiOperation(value = "根据桩的sn修改，修改连接器功率")
    @PostMapping(value = "/updateEvsePowerBySn")
    public Result<Boolean> updateEvsePowerBySn(@RequestParam("evseSn") String evseSn,
                                               @RequestParam("power") Double power){
        log.info("修改功率参数：{}，{}",evseSn,power);
        return opLocationConnectorService.updateEvsePowerBySn(evseSn,power);
    }
}
