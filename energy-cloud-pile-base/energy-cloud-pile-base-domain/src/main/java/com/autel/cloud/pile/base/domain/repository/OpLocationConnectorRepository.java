package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpLocationConnectorPagingDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.vo.OpLocationConnectorPagingVo;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 * 充电设备连接器 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
public interface OpLocationConnectorRepository extends IService<OpLocationConnectorEntity> {

    Page<OpLocationConnectorPagingDTO> paging(OpLocationConnectorPagingVo opLocationConnectorPagingVo);


    OpLocationConnectorPagingDTO getOpLocationConnectorDetail(Long connectorId);

    OpLocationConnectorPagingDTO getOpLocationConnectorDetailDeleted(Long connectorId);

    Boolean updateEvsePowerBySn(String evseSn,Double power);
}
