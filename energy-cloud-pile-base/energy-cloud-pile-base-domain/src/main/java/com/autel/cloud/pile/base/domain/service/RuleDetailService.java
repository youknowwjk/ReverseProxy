package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.RuleDetailDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleDetailEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
* @author temp
* @description 针对表【tb_rule_detail(进场控制规则明细表)】的数据库操作Service
* @createDate 2022-07-06 16:24:07
*/
public interface RuleDetailService extends IService<RuleDetailEntity> {

    boolean insertBatchByRuleId(Long ruleId, List<RuleDetailDTO> details);

    List<Long> findUse();
}
