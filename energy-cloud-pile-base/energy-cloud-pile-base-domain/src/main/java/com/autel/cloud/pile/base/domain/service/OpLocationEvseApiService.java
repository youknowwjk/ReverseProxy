package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.dto.api.LocationEvsePageQueryDto;
import com.autel.cloud.pile.base.vo.api.LocationEvseApiInfoVO;

/**
 * 充电枪openapi服务暴露接口
 */
public interface OpLocationEvseApiService {
    OpenApiPagelVO<LocationEvseApiInfoVO> list(LocationEvsePageQueryDto dto);
    LocationEvseApiInfoVO  queryOpLocationEvseById( Long id , Long sellerId  );
}
