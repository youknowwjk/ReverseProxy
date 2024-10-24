package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.RuleRepository;
import com.autel.cloud.pile.base.domain.service.RuleService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.vo.*;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author temp
 * @description 针对表【tb_rule(进场控制规则表)】的数据库操作Service实现
 * @createDate 2022-07-06 11:40:00
 */
@Service
@Slf4j
public class RuleServiceImpl implements RuleService {

    @Autowired
    private RuleRepository repository;

    @Override
    public Result<String> addRule(RuleDTO ruleDTO) {
        return repository.addRule(ruleDTO);
    }

    @Override
    public Result<Boolean> editRule(RuleDTO ruleDTO) {
        return repository.editRule(ruleDTO);
    }

    @Override
    public Result<Boolean> deleteRule(Long userId, Long ruleId) {
        return repository.deleteRule(userId,ruleId);
    }

    @Override
    public Result<List<RuleSiteVO>> locationList(RuleSiteDTO ruleSiteDTO) {
        return repository.locationList(ruleSiteDTO);
    }

    @Override
    public Result<List<RuleVO>> ruleList(RulePageDTO rulePageDTO) {
        return repository.ruleList(rulePageDTO);
    }

    @Override
    public IPage<RuleVO> ruleListNew(SerachBindEvseDTO serachBindEvseDTO) {
        return repository.ruleListNew(serachBindEvseDTO);
    }

    @Override
    public Result<IPage<SerachBindEvseVO>> serachBindEvse(SerachBindEvseDTO serachBindEvseDTO) {
        return repository.serachBindEvse(serachBindEvseDTO);
    }

    @Override
    public IPage<SerachBindEvseVO> serachAllEvse(SerachBindEvseDTO dto,Long sellerId) {
        return repository.serachAllEvse(dto,sellerId);
    }

    @Override
    public List<RuleGroupVO> getRuleNameByGroupId(List<Long> groupIds, Long sellerId) {
        return repository.getRuleNameByGroupId(groupIds,sellerId);
    }

    @Override
    public Integer syncRuleToEvse() {
        return repository.syncRuleToEvse();
    }

    @Override
    public Integer syncMemberGroupId() {
        return repository.syncMemberGroupId();
    }

    @Override
    public List<Long> findUse() {
        return repository.findUse();
    }

    @Override
    public Result<IPage<PileDetailVO>> relateList(RuleSitePageDTO ruleSitePageDTO) {
        return repository.relateList(ruleSitePageDTO);
    }

    @Override
    public Result<List<SiteInfoVo>> getSiteList(SiteDTO siteDTO) {
        return repository.getSiteList(siteDTO);
    }

    @Override
    public Result<Boolean> relatePile(RelatePileDTO relatePileDTO) {
        return repository.relatePile(relatePileDTO);
    }

    @Override
    public Result<Boolean> associatedGun(AssociatedGunDTO associatedGunDTO) {
        return repository.associatedGun(associatedGunDTO);
    }
    @Override
    public Result<Boolean> removePile(RemovePileDTO removePileDTO) {
        return repository.removePile(removePileDTO);
    }

    @Override
    public Result<Boolean> removeEvse(RemoveGunDTO removeGunDTO) {
        return repository.removeEvse(removeGunDTO);
    }

    @Override
    public Result<List<RuleVO>> getAllRule() {
        return repository.getAllRule();
    }

    @Override
    public Result<List<PileRuleVO>> getRulesByPileSn(PullRuleDTO pullRuleDTO) {
        return repository.getRulesByPileSn(pullRuleDTO);
    }

    @Override
    public Result<List<PileRuleVO>> getRulesByEvseSn(GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        return repository.getRulesByEvseSn(getRuleByEvseSnDTO);
    }

    @Override
    public Result<List<PileRuleVO>> getRuleByLocationId(PullRuleDTO pullRuleDTO) {
        return repository.getRuleByLocationId(pullRuleDTO);
    }

    @Override
    public Result<List<PileRuleVO>> getEvseRulesByLocationId(GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        return repository.getEvseRulesByLocationId(getRuleByEvseSnDTO);
    }

    @Override
    public RuleVO findById(Long ruleId) {
        return repository.findById(ruleId);
    }

    @Override
    public List<RuleVO> findAllByIds(List<Long> ruleIds) {
        return repository.findAllByIds(ruleIds);
    }

    @Override
    public Result<List<RuleVO>> getAllRuleByLocationId(Long locationId) {
        return repository.getAllRuleByLocationId(locationId);
    }

    @Override
    public Result<RuleVO> detail(Long ruleId) {
        return repository.detail(ruleId);
    }

    @Override
    public Result<RuleRelateForAppVO> getRuleForApp(RuleRelateForAppDTO ruleRelateForAppDTO) {
        return repository.getRuleForApp(ruleRelateForAppDTO);
    }

    @Override
    public Result<RuleVO> getRuleByPileSn(PullRuleDTO pullRuleDTO) {
        return repository.getRuleByPileSn(pullRuleDTO);
    }

    @Override
    public RuleVO getRuleByEvseSn(GetRuleByEvseSnDTO getRuleByEvseSnDTO) {
        return repository.getRuleByEvseSn(getRuleByEvseSnDTO);
    }
}




