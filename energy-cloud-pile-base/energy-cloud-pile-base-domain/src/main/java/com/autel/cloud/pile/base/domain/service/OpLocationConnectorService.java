package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpLocationConnectorPagingDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.vo.OpLocationConnectorPagingVo;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * @Author: A22327
 * @CreateTime: 2022/4/24 20:29
 * @Description: 连接器
 */
public interface OpLocationConnectorService {

    /*
     *
     * @author A22327
     * @date 2022/4/24 20:49
     * @param [opLocationConnectorPagingVo]
     * @return Page<OpLocationConnectorPagingDTO>
    **/
    Result<Page<OpLocationConnectorPagingDTO>> paging(OpLocationConnectorPagingVo opLocationConnectorPagingVo);


    Result<OpLocationConnectorPagingDTO> getOpLocationConnectorDetail(Long  connectorId);


    List<Double> getPowerByGunType(int gunType);

    List<GunTypeRespDTO> getGunTypeByType(String type);

    Result<OpLocationConnectorPagingDTO> getOpLocationConnectorDetailDeleted(Long  connectorId);

    Result<Boolean> updateEvsePowerBySn(String evseSn,Double power);

    List<OpLocationConnectorEntity> findByEvseId(List<Long> ids);

    boolean updateBatch(List<OpLocationConnectorEntity> connectorEntityList);
}
