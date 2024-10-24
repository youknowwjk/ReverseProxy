package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.openapi.dto.OpenApiPageDto;
import com.autel.cloud.openapi.vo.OpenApiPagelVO;
import com.autel.cloud.pile.base.dto.api.LocationQueryDto;
import com.autel.cloud.pile.base.vo.api.LocationApiInfoVO;

/**
 * 场站openapi服务暴露接口
 */
public interface OpLocationApiService {
    OpenApiPagelVO<LocationApiInfoVO> list(OpenApiPageDto dto);
    LocationApiInfoVO  queryLocation( LocationQueryDto dto );
}
