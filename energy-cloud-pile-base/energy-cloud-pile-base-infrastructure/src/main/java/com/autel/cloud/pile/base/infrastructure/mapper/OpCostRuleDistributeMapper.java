package com.autel.cloud.pile.base.infrastructure.mapper;


import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCostRuleDistributeEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import groovy.util.logging.Slf4j;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Update;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录 Mapper接口
 */
@Repository
@Mapper
@Slf4j
public interface OpCostRuleDistributeMapper extends BaseMapper<OpCostRuleDistributeEntity> {
    @Update({
       "<script>" +
            "<foreach collection=\"opCostRuleDistributeEntityList\" item=\"item\" separator=\";\"> " +
               "update op_cost_rule_distribute " +
               "set success_flag= #{item.successFlag}, " +
               "failure_reason= #{item.failureReason}, " +
               "distribute_time= #{item.distributeTime}, " +
               "update_time= #{item.updateTime} " +
               "where pile_sn=#{item.pileSn} and deleted=0 " +
             "</foreach>" +
       "</script>"
    })
    void batchUpdate(@Param("opCostRuleDistributeEntityList") List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList);
}
