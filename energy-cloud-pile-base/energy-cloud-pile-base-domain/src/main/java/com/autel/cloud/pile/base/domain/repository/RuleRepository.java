package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleDetailEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleEntity;
import com.autel.cloud.pile.base.vo.*;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @Author temp
 * @Date 2022/8/2 17:54
 */
public interface RuleRepository extends IService<RuleEntity> {
    Result<String> addRule(RuleDTO ruleDTO);

    void syncRuleToES(RuleEntity entity, List<RuleDetailEntity> ruleDetailEntities);

    Result<Boolean> editRule(RuleDTO ruleDTO);

    Result<Boolean> deleteRule(Long userId, Long ruleId);

    Result<List<RuleSiteVO>> locationList(RuleSiteDTO ruleSiteDTO);

    Result<List<RuleVO>> ruleList(RulePageDTO rulePageDTO);

    Result<IPage<PileDetailVO>> relateList(RuleSitePageDTO ruleSitePageDTO);

    Result<List<SiteInfoVo>> getSiteList(SiteDTO siteDTO);

    Result<Boolean> relatePile(RelatePileDTO relatePileDTO);

    Result<Boolean> removePile(RemovePileDTO removePileDTO);

    Result<List<RuleVO>> getAllRule();

    Result<List<PileRuleVO>> getRulesByPileSn(PullRuleDTO pullRuleDTO);

    Result<List<PileRuleVO>> getRuleByLocationId(PullRuleDTO pullRuleDTO);

    RuleVO findById(Long ruleId);

    List<RuleVO> findAllByIds(List<Long> ruleIds);

    Result<List<RuleVO>> getAllRuleByLocationId(Long locationId);

    Result<RuleVO> detail(Long ruleId);

    Result<RuleRelateForAppVO> getRuleForApp(RuleRelateForAppDTO ruleRelateForAppDTO);

    Result<RuleVO> getRuleByPileSn(PullRuleDTO pullRuleDTO);

    RuleVO getRuleByEvseSn(GetRuleByEvseSnDTO getRuleByEvseSnDTO);

    Result<Boolean> associatedGun(AssociatedGunDTO associatedGunDTO);

    Result<Boolean> removeEvse(RemoveGunDTO removeGunDTO);

    Result<List<PileRuleVO>> getRulesByEvseSn(GetRuleByEvseSnDTO getRuleByEvseSnDTO);

    Result<List<PileRuleVO>> getEvseRulesByLocationId(GetRuleByEvseSnDTO getRuleByEvseSnDTO);

    IPage<RuleVO> ruleListNew(SerachBindEvseDTO serachBindEvseDTO);

    Result<IPage<SerachBindEvseVO>> serachBindEvse(SerachBindEvseDTO serachBindEvseDTO);

    IPage<SerachBindEvseVO> serachAllEvse(SerachBindEvseDTO dto,Long sellerId);

    List<RuleGroupVO> getRuleNameByGroupId(List<Long> groupIds, Long sellerId);

    Integer syncMemberGroupId();
    Integer syncRuleToEvse();

    List<Long> findUse();
}
