package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.domain.service.RuleDetailService;
import com.autel.cloud.pile.base.dto.RuleDetailDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.RuleDetailMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleDetailEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author temp
 * @description 针对表【tb_rule_detail(进场控制规则明细表)】的数据库操作Service实现
 * @createDate 2022-07-06 16:24:07
 */
@Service
@Slf4j
public class RuleDetailServiceImpl extends ServiceImpl<RuleDetailMapper, RuleDetailEntity>
        implements RuleDetailService {
    @Autowired
    private RuleDetailMapper ruleDetailMapper;

    @Override
    public boolean insertBatchByRuleId(Long ruleId, List<RuleDetailDTO> details) {
        if (CollectionUtils.isEmpty(details)) {
            log.info("insertBatchByRuleId,ruleId={},details={}", ruleId, details);
            return true;
        }
        int removeCont = this.getBaseMapper().delete(new LambdaQueryWrapper<RuleDetailEntity>().eq(RuleDetailEntity::getRuleId, ruleId));
        if (removeCont > 0){
            log.info("insertBatchByRuleId,removeCont={}", removeCont);
        }
        List<RuleDetailEntity> insertList = new ArrayList<>();
        details.forEach(detail -> {
            RuleDetailEntity entity = new RuleDetailEntity();
            BeanUtils.copyProperties(detail, entity);
            entity.setId(IdWorker.getId());
            entity.setRuleId(ruleId);
            entity.setDays(detail.getDays().stream().map(String::valueOf).collect(Collectors.joining(",")));
            entity.setMemberGroupId(detail.getMemberGroupId().stream().map(String::valueOf).collect(Collectors.joining(",")));
            entity.setCreatTime(System.currentTimeMillis());
            insertList.add(entity);
        });
        return ruleDetailMapper.insertBatchByRuleId(insertList);
    }

    @Override
    public List<Long> findUse() {
        return this.ruleDetailMapper.findUse();
    }
}




