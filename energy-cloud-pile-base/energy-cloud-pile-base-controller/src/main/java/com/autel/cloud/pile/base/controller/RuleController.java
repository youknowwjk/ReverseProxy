package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.common.CustomizedDateTime;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.pile.base.domain.repository.RuleRepository;
import com.autel.cloud.pile.base.domain.service.RuleDetailService;
import com.autel.cloud.pile.base.domain.service.RuleService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleDetailEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @Author temp
 * @Date 2022/7/6 12:07
 */
@RestController
@RequestMapping("/rule")
@Slf4j
@Api(tags = "进场控制-规则管理")
@Validated
public class RuleController {

    @Autowired
    private RuleService ruleService;


    @Autowired
    private RuleRepository repository;

    @Autowired
    private RuleDetailService ruleDetailService;

    @PostMapping("/sync2Es")
    public Result<Boolean> sync2Es() {
        List<RuleEntity> ruleList = repository.list(new LambdaQueryWrapper<RuleEntity>()
                .eq(RuleEntity::getDeleted, 0));
        for (RuleEntity ruleEntity : ruleList) {
            Long id = ruleEntity.getId();
            LambdaQueryWrapper<RuleDetailEntity> eq = new LambdaQueryWrapper<RuleDetailEntity>()
                    .eq(RuleDetailEntity::getDeleted, 0).eq(RuleDetailEntity::getRuleId, id);
            List<RuleDetailEntity> ruleDetailEntities = ruleDetailService.getBaseMapper().selectList(eq);
            repository.syncRuleToES(ruleEntity, ruleDetailEntities);
        }
        return Result.ofSucceed(Boolean.TRUE);
    }

    @OperationActionLog(action = "add", object = "approachControlRules")
    @PostMapping("/addRule")
    @ApiOperation("进场控制-添加规则")
    @ApiResponses(value = {
            @ApiResponse(code = 50201808, message = "规则名称格式不正确"),
            @ApiResponse(code = 50201442, message = "规则明细不能为空或details数量超出7"),
            @ApiResponse(code = 50201443, message = "规则日期不能为空或days超出24"),
            @ApiResponse(code = 50201444, message = "手机号格式错误"),
            @ApiResponse(code = 50201445, message = "邮箱格式错误"),
            @ApiResponse(code = 50201447, message = "规则名称不能重复")
    })
    public Result<String> addRule(@RequestBody @Validated RuleDTO ruleDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        ruleDTO.setSellerId(payload.getSellerId());
        ruleDTO.setUserId(payload.getUserId());
        return ruleService.addRule(ruleDTO);
    }

    @OperationActionLog(action = "edit", object = "approachControlRules")
    @PostMapping("/editRule")
    @ApiOperation("进场控制-编辑规则")
    @ApiResponses(value = {
            @ApiResponse(code = 50201501, message = "规则不存在")
    })
    public Result<Boolean> editRule(@RequestBody @Validated RuleDTO ruleDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        ruleDTO.setSellerId(payload.getSellerId());
        ruleDTO.setUserId(payload.getUserId());
        return ruleService.editRule(ruleDTO);
    }

    @OperationActionLog(action = "query", object = "approachControlRules")
    @GetMapping("/detail/{ruleId}")
    @ApiOperation("进场控制-规则详情")
    @ApiResponses(value = {
            @ApiResponse(code = 50201501, message = "规则不存在")
    })
    public Result<RuleVO> detail(@PathVariable("ruleId") Long ruleId) {
        return ruleService.detail(ruleId);
    }

    @OperationActionLog(action = "delete", object = "approachControlRules")
    @GetMapping("/deleteRule/{ruleId}")
    @ApiOperation("进场控制-删除规则")
    @ApiResponses(value = {
            @ApiResponse(code = 50201501, message = "规则不存在"),
            @ApiResponse(code = 50201446, message = "规则下有关联的桩，不能删除")
    })
    public Result<Boolean> deleteRule(@PathVariable("ruleId") Long ruleId) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        return ruleService.deleteRule(payload.getUserId(), ruleId);
    }

    @PostMapping("/locationList")
    @ApiOperation("进场控制-场站分页")
    @Deprecated
    public Result<List<RuleSiteVO>> locationList(@Validated @RequestBody RuleSiteDTO ruleSiteDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        ruleSiteDTO.setSellerId(payload.getSellerId());
        ruleSiteDTO.setUserId(payload.getUserId());
        return ruleService.locationList(ruleSiteDTO);
    }

    @PostMapping("/ruleList")
    @ApiOperation("进场控制-规则分页查询")
    @Deprecated
    public Result<List<RuleVO>> ruleList(@Valid @Validated @RequestBody RulePageDTO rulePageDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        rulePageDTO.setSellerId(payload.getSellerId());
        rulePageDTO.setUserId(payload.getUserId());
        return ruleService.ruleList(rulePageDTO);
    }

    @OperationActionLog(action = "query", object = "approachControlRules")
    @PostMapping("/ruleListNew")
    @ApiOperation("进场控制-规则分页查询(新)")
    public Result<IPage<RuleVO>> ruleListNew(@Valid @Validated @RequestBody SerachBindEvseDTO serachBindEvseDTO) {
        return Result.ofSucceed(this.ruleService.ruleListNew(serachBindEvseDTO));
    }

    @OperationActionLog(action = "query", object = "approachControlPiles")
    @PostMapping("/relateList")
    @ApiOperation("进场控制-关联桩分页")
    @Deprecated
    public Result<IPage<PileDetailVO>> relateList(@RequestBody @Validated RuleSitePageDTO ruleSitePageDTO) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        ruleSitePageDTO.setSellerId(payload.getSellerId());
        ruleSitePageDTO.setUserId(payload.getUserId());
        return ruleService.relateList(ruleSitePageDTO);
    }

    @PostMapping("/getSiteList")
    @ApiOperation("进场控制-所属场站列表")
    public Result<List<SiteInfoVo>> getSiteList(@RequestBody @Validated SiteDTO siteDTO) {
        return ruleService.getSiteList(siteDTO);
    }

    @OperationActionLog(action = "add", object = "approachControlPiles")
    @PostMapping("/relatePile")
    @ApiOperation("进场控制-添加桩")
    @Deprecated
    public Result<Boolean> relatePile(@RequestBody @Validated RelatePileDTO relatePileDTO) {
        return ruleService.relatePile(relatePileDTO);
    }

    @OperationActionLog(action = "add", object = "approachControlPiles")
    @PostMapping("/associatedGun")
    @ApiOperation("进场控制-关联枪")
    public Result<Boolean> associatedGun(@Valid @RequestBody AssociatedGunDTO associatedGunDTO) {
        return ruleService.associatedGun(associatedGunDTO);
    }

    @OperationActionLog(action = "delete", object = "approachControlPiles")
    @PostMapping("/removePile")
    @ApiOperation("进场控制-移除桩")
    @Deprecated
    public Result<Boolean> removePile(@RequestBody @Validated RemovePileDTO removePileDTO) {
        return ruleService.removePile(removePileDTO);
    }

    @OperationActionLog(action = "delete", object = "approachControlPiles")
    @PostMapping("/removeEvse")
    @ApiOperation("进场控制-移除枪")
    public Result<Boolean> removeEvse(@Validated @RequestBody RemoveGunDTO removeGunDTO) {
        return ruleService.removeEvse(removeGunDTO);
    }

    @ApiOperation("查询已关联的枪")
    @PostMapping("/serachBindEvse")
    public Result<IPage<SerachBindEvseVO>> serachBindEvse (@Valid @RequestBody SerachBindEvseDTO serachBindEvseDTO) {
        return ruleService.serachBindEvse(serachBindEvseDTO);
    }

    @ApiOperation("进场控制-关联枪列表")
    @PostMapping("/serachAllEvse")
    public Result<IPage<SerachBindEvseVO>> serachAllEvse (@Validated @RequestBody SerachBindEvseDTO dto) {
        Long sellerId = LoginUserUtil.getSellerId();
        return Result.ofSucceed(this.ruleService.serachAllEvse(dto,sellerId));
    }

    @GetMapping("/getAllRule")
    @ApiOperation("进场控制-获取所有规则列表")
    public Result<List<RuleVO>> getAllRule() {
        return ruleService.getAllRule();
    }

    @GetMapping("/getAllRule/{locationId}")
    @ApiOperation("进场控制-获取所有规则列表")
    public Result<List<RuleVO>> getAllRuleByLocationId(@PathVariable("locationId") Long locationId) {
        return ruleService.getAllRuleByLocationId(locationId);
    }

    @PostMapping("/getRulesByPileSn")
    @ApiOperation("进场控制-桩SN码获取所属场站下桩关联规则")
    @Deprecated
    public Result<List<PileRuleVO>> getRulesByPileSn(@RequestBody @Validated(PullRuleDTO.PileRule.class) PullRuleDTO pullRuleDTO) {
        return ruleService.getRulesByPileSn(pullRuleDTO);
    }

    @PostMapping("/getRulesByEvseSn")
    @ApiOperation("进场控制-充电设备SN码获取所属场站下桩关联规则")
    @Deprecated
    public Result<List<PileRuleVO>> getRulesByEvseSn(@Valid @RequestBody GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        return ruleService.getRulesByEvseSn(getRuleByEvseSnDTO);
    }

    @PostMapping("/getRuleByPileSn")
    @ApiOperation("进场控制-桩SN码获取桩关联规则")
    @Deprecated
    public Result<RuleVO> getRuleByPileSn(@RequestBody @Validated(PullRuleDTO.PileRule.class) PullRuleDTO pullRuleDTO) {
        return ruleService.getRuleByPileSn(pullRuleDTO);
    }

    @PostMapping("/getRuleByEvseSn")
    @ApiOperation("进场控制-充电设备sn获取关联规则")
    @CustomizedDateTime
    public Result<RuleVO> getRuleByEvseSn(@Valid @RequestBody GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        return Result.ofSucceed(this.ruleService.getRuleByEvseSn(getRuleByEvseSnDTO));
    }

    @PostMapping("/getRuleByLocationId")
    @ApiOperation("进场控制-根据用户ID和场站ID获取规则")
    @Deprecated
    public Result<List<PileRuleVO>> getRuleByLocationId(@RequestBody @Validated(PullRuleDTO.SiteRule.class) PullRuleDTO pullRuleDTO) {
        return ruleService.getRuleByLocationId(pullRuleDTO);
    }

    @PostMapping("/getEvseRulesByLocationId")
    @ApiOperation("进场控制-根据用户ID和场站ID获取充电设备规则")
    public Result<List<PileRuleVO>> getEvseRulesByLocationId(@Valid @RequestBody GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        return ruleService.getEvseRulesByLocationId(getRuleByEvseSnDTO);
    }

    @PostMapping("/getRuleForApp")
    @ApiOperation("进场控制-APP地图获取规则")
    public Result<RuleRelateForAppVO> getRuleForApp(@RequestBody @Validated RuleRelateForAppDTO ruleRelateForAppDTO) {
        return ruleService.getRuleForApp(ruleRelateForAppDTO);
    }

    @GetMapping("/getRuleNameByGroupId")
    @ApiOperation("根据客户组id查找进场控制规则名称")
    public Result<List<RuleGroupVO>> getRuleNameByGroupId (@RequestParam("groupIds") List<Long> groupIds, @RequestParam("sellerId") Long sellerId) {
        return Result.ofSucceed(ruleService.getRuleNameByGroupId(groupIds,sellerId));
    }

    @PostMapping("/syncRuleToEvse")
    @ApiOperation("同步进场控制规则到枪")
    public Result<Integer> syncRuleToEvse () {
        return Result.ofSucceed(ruleService.syncRuleToEvse());
    }

    @PostMapping("/syncMemberGroupId")
    @ApiOperation("同步新客户组ID")
    public Result<Integer> syncMemberGroupId () {
        return Result.ofSucceed(ruleService.syncMemberGroupId());
    }

    @PostMapping("/findUse")
    @ApiOperation("查找已使用的新客户组商家ID")
    public Result<List<Long>> findUse () {
        return Result.ofSucceed(ruleService.findUse());
    }
}
