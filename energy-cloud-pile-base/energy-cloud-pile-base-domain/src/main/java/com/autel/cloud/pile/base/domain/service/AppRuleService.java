package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.app.BusinessTimeDTO;

public interface AppRuleService {
    Result<Boolean> addTime(BusinessTimeDTO businessTimeDTO);
}
