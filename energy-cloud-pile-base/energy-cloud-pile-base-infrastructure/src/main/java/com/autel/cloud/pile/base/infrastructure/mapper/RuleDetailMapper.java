package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleDetailEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
* @author temp
* @description 针对表【tb_rule_detail(进场控制规则明细表)】的数据库操作Mapper
* @createDate 2022-07-06 16:24:07
* @Entity generator.domain.RuleDetailEntity
*/
@Mapper
public interface RuleDetailMapper extends BaseMapper<RuleDetailEntity> {

    boolean insertBatchByRuleId(@Param("insertList") List<RuleDetailEntity> insertList);

    List<Long> findUse();
}




