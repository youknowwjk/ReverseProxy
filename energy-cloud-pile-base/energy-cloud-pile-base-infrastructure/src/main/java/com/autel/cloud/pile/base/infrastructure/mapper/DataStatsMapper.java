package com.autel.cloud.pile.base.infrastructure.mapper;

import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Map;

/**
 * @Author A22282
 * @Date 2022/4/27 17:39
 */
@Repository
public interface DataStatsMapper {
    List<Map<String, Object>> evseStats(@Param("operatorId") Long operatorId);

    Integer countByOperatorId(@Param("operatorId") Long operatorId);

    List<Map<String, Object>> statusCountByOperatorId(@Param("operatorId") Long operatorId);
}
