package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.app.BusinessTimeDTO;

public interface AppRuleRepository {
    Result<Boolean> addTime(BusinessTimeDTO businessTimeDTO);
}
