package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.OpLocationForAdvParamDTO;
import com.autel.cloud.pile.base.vo.OpLocationPileForAdvVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * @Author A22282
 * @Date 2023/12/12 20:34
 */
public interface OpLocationPileEvseOpenService {
    Page<OpLocationPileForAdvVO> getAdvList(OpLocationForAdvParamDTO dto);

    List<Long> history(List<Long> sellerIdList);

    Integer deletes();
}
