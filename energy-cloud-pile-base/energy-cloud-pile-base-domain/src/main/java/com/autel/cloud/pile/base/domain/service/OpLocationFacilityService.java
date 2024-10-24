package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpLocationFacilityDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationFacilityEntity;

import java.util.List;

public interface OpLocationFacilityService {

    /**
     * 新增周边设施
     *
     * @param opLocationFacilityDTO
     * @return
     */
    Result<Long> addOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO);

    /**
     * 批量新增周边设施
     *
     * @param opLocationFacilityDTOList
     * @return
     */
    Result<List<OpLocationFacilityEntity>> addOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList);

    /**
     * 修改周边设施
     *
     * @param opLocationFacilityDTO
     * @return
     */
    Result<Boolean> updateOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO);

    /**
     * 批量修改周边设施
     *
     * @param opLocationFacilityDTOList
     * @return
     */
    Result<List<OpLocationFacilityEntity>> updateOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList);

    /**
     * 删除周边设施
     *
     * @param id
     * @return
     */
    Result<Boolean> delOpLocationFacility(Long id);

    /**
     * 根据场站删除周边设施
     *
     * @param locationId
     * @return
     */
    Result<Boolean> delOpLocationFacilityByLocationId(Long locationId);

    /**
     * 查询场站周边设施
     *
     * @param locationId
     * @return
     */
    Result<List<OpLocationFacilityDTO>> selectOpLocationFacilityListByLocationId(Long locationId);
}
