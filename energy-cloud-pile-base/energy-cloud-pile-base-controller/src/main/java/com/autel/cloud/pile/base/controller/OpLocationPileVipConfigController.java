package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.VipPolicy;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileVipConfigTypeMapper;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileVipConfigRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileVipConfigEntity;
import com.autel.cloud.pile.user.api.dto.MemberCleanDTO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;

@Log4j2
@RestController
@RequestMapping("/smartChargeVIP")
@Api(tags = "VIP策略配置")
@Validated
public class OpLocationPileVipConfigController {


    @Resource
    private OpLocationPileVipConfigRepository opLocationPileVipConfigRepository;


    @GetMapping("/findVipPolicyRef")
    @ApiOperation(value = "根据客户组或者客户查询VIP策略配置", notes = "根据客户组或者客户查询VIP策略配置")
    public Result<List<VipPolicy>> findVipPolicyRef(@RequestParam("type") int type, @RequestParam("principalId") Long principalId) {
        List<OpLocationPileVipConfigEntity> byTypeAndPrincipalId = opLocationPileVipConfigRepository.findByTypeAndPrincipalId(type, principalId);
        List<VipPolicy> vipPolicies = OpLocationPileVipConfigTypeMapper.INSTANCE.mapVO(byTypeAndPrincipalId);
        return Result.ofSucceed(vipPolicies);
    }

    @PostMapping("/removeMembers")
    @ApiOperation(value = "移除智能充电关联客户", notes = "移除智能充电关联客户")
    public Result<Integer> removeMembers(@Validated @RequestBody MemberCleanDTO dto) {
        return Result.ofSucceed(this.opLocationPileVipConfigRepository.removeMembers(dto));
    }

    @PostMapping("/syncMemberGroupId")
    @ApiOperation("同步新客户组ID")
    public Result<Integer> syncMemberGroupId () {
        return Result.ofSucceed(opLocationPileVipConfigRepository.syncMemberGroupId());
    }

}
