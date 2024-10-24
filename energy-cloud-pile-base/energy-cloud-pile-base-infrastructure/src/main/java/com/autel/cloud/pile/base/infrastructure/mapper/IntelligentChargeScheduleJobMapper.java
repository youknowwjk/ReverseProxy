package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * @author A21309
 * @date 2021-09-30 11:18
 */
public interface IntelligentChargeScheduleJobMapper extends BaseMapper<IntelligentChargeScheduleJob> {


    int updateFinishedJob(@Param("evseSn") String evseSn, @Param("orderSeq") String orderSeq);
}
