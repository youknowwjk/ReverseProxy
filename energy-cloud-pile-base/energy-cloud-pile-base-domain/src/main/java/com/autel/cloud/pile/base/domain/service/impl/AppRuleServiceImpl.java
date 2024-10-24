package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.AppRuleRepository;
import com.autel.cloud.pile.base.domain.service.AppRuleService;
import com.autel.cloud.pile.base.dto.app.BusinessTimeDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class AppRuleServiceImpl implements AppRuleService {
    @Autowired
    private AppRuleRepository appRuleRepository;

    @Override
    public Result<Boolean> addTime(BusinessTimeDTO businessTimeDTO) {
        return appRuleRepository.addTime(businessTimeDTO);
    }



}
