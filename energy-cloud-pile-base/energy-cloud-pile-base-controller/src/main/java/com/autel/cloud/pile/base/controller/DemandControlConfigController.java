package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.DemandControlAddConfigDTO;
import com.autel.cloud.pile.base.domain.model.DemandControlUpdateConfigDTO;
import com.autel.cloud.pile.base.domain.service.DemandControlConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DemandControlConfigEntity;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author A20019
 * @since 2022/3/21 10:32
 */
@RestController
@RequestMapping("/demandControlConfig")
@Api(value = "需求费控制", tags = "需求费控制")
public class DemandControlConfigController {
    @Resource
    private DemandControlConfigService demandControlConfigService;
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;


    @PostMapping("/list")
    @ApiOperation(value = "查询需求费控制列表")
    public Result<List<DemandControlConfigEntity>> listDemandControlConfig() {
        DemandControlAddConfigDTO dto = new DemandControlAddConfigDTO();
        dto.setMerchantId(LoginUserUtil.getSellerId());
        //查询需求费控制配置
        return Result.ofSucceed(demandControlConfigService.listDemandControlConfig(dto));
    }


    @PostMapping("/detail")
    @ApiOperation(value = "查询需求费控制配置")
    public Result<DemandControlConfigEntity> queryDemandControlConfig(@RequestBody DemandControlAddConfigDTO dto) {
        //查询需求费控制配置
        DemandControlConfigEntity entity = demandControlConfigService.getById(dto.getId());
        return Result.ofSucceed(entity);
    }

    @PostMapping("/addDemandControlGroup")
    @ApiOperation(value = "新增需求控制群组", notes = "新增需求控制群组")
    public Result<Boolean> addDemandControlGroup(@RequestBody DemandControlAddConfigDTO dto) {
        dto.setMerchantId(LoginUserUtil.getSellerId());
        return opLocationPileGroupService.addDemandControlGroup(dto);
    }

    @PostMapping("/updateDemandControlGroup")
    @ApiOperation(value = "更新需求控制群组", notes = "更新需求控制群组")
    public Result<Boolean> updateDemandControlGroup(@RequestBody DemandControlUpdateConfigDTO dto) {
        dto.setMerchantId(LoginUserUtil.getSellerId());
//        dto.setMerchantId(1625305777309425665L);
        return opLocationPileGroupService.updateDemandControlGroup(dto);
    }

    @PostMapping(value = "/generateDefaultControlName")
    @ApiOperation(value = "生成默认的控制器名称", notes = "生成默认的控制器名称")
    public Result<String> generateDefaultControlName() {
        DemandControlAddConfigDTO dto = new DemandControlAddConfigDTO();
        dto.setMerchantId(LoginUserUtil.getSellerId());
        return Result.ofSucceed(opLocationPileGroupService.generateDefaultControlName(dto));
    }

    @PostMapping(value = "/status")
    @ApiOperation(value = "更新控制器状态  status：0：未启用，1：启用 deleted  0：未删除 1：已删除",
            notes = "更新控制器状态  status：0：未启用，1：启用 deleted  0：未删除 1：已删除")
    public Result<Boolean> updateDemandControlStatus(Integer status, Integer deleted,Long id) {
        return Result.ofSucceed(opLocationPileGroupService.updateDemandControlStatus(status, deleted, id));
    }

}
