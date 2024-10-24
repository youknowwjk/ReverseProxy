package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.domain.model.DemandControlAddConfigDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DemandControlConfigEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Set;

/**
* @author wang
* @description 针对表【demand_control_config(需求费控制器表)】的数据库操作Service
* @createDate 2024-04-30 10:16:41
*/
public interface DemandControlConfigService extends IService<DemandControlConfigEntity> {

    String findContentByGroupId(Long id);

    List<DemandControlConfigEntity> listDemandControlConfig(DemandControlAddConfigDTO dto);
    List<DemandControlConfigEntity> listAllDemandControlConfig(DemandControlAddConfigDTO dto);

    Boolean updateDemandControlStatus(Integer status, Integer deleted, Long id);

    int countDemandControlConfig(DemandControlAddConfigDTO dto);

    void deleteDemandControlById(Long rootId);

    void deleteByGroupIds(Set<Long> deleteIds);
}
