package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.domain.service.DomainService;
import com.autel.cloud.pile.base.dto.DomainInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.DomainMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DomainEntity;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * @author A21309
 * @date 2022-06-09 10:37
 */
@Service
@Slf4j
public class DomainServiceImpl extends ServiceImpl<DomainMapper, DomainEntity> implements DomainService {
    @Override
    public String getDomainLogo(DomainInfoDTO domainInfo) {
        QueryWrapper<DomainEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("domain_url", domainInfo.getUrl());
        DomainEntity domain = baseMapper.selectOne(queryWrapper);
        if (domain != null) {
            return domain.getLogoPath();
        }
        return null;
    }

    @Override
    public Boolean addDomainInfo(DomainEntity domain) {
        return baseMapper.insert(domain) == 1;
    }
}

