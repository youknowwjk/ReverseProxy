package com.autel.cloud.pile.base.domain.service;


import com.autel.cloud.pile.base.dto.batch.EvseBatchConfigDTO;
import com.autel.cloud.pile.base.dto.batch.EvseBatchConfigResponseDTO;
import com.autel.cloud.pile.base.dto.batch.EvseConfigQueryDTO;
import com.autel.cloud.pile.base.vo.batch.EvseConfigVO;
import com.autel.cloud.pile.base.vo.batch.EvseRuleVO;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

/**
 * 充电桩/枪批量配置服务
 */
public interface EvseBatchConfigService {

    IPage<EvseConfigVO> page(EvseConfigQueryDTO configQueryDTO);

    IPage<EvseConfigVO> queryEvseList(Integer page,Integer pageSize,String keyword);

    List<EvseRuleVO> getCostRuleList();

    List<EvseRuleVO> getMarketingRuleList();

    List<EvseRuleVO> getEntryControlList();

    EvseBatchConfigResponseDTO batchConfigEvse(EvseBatchConfigDTO evseBatchConfigDTO);
}
