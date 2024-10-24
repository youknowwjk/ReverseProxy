package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.DomainInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DomainEntity;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * @author A21309
 * @date 2022-06-09 10:37
 */
public interface DomainService extends IService<DomainEntity> {
    String getDomainLogo(DomainInfoDTO domainInfo);

    Boolean addDomainInfo(DomainEntity domain);
}
