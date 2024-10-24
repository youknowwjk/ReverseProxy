package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.dto.api.LocationPilePageQueryDto;
import com.autel.cloud.pile.base.dto.api.LocationPileQueryDto;
import com.autel.cloud.pile.base.vo.api.PileEvseApiInfoVO;

import java.util.List;

/**
 * 场站openapi服务暴露接口
 */
public interface OpLocationPileEvseApiService {

    OpenApiPagelVO<PileEvseApiInfoVO> list(LocationPilePageQueryDto dto);
    List<PileEvseApiInfoVO> queryLocation(LocationPileQueryDto dto);
}
