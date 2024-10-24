package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpLocationFacilityDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationFacilityEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 场站设施 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
public interface OpLocationFacilityRepository extends IService<OpLocationFacilityEntity> {
    /**
     * 新增周边设施
     *
     * @param opLocationFacilityDTO
     * @return
     */
    Long addOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO);

    /**
     * 批量新增周边设施
     *
     * @param opLocationFacilityDTOList
     * @return
     */
    List<OpLocationFacilityEntity> addOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList);

    /**
     * 修改周边设施
     *
     * @param opLocationFacilityDTO
     * @return
     */
    Boolean updateOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO);

    /**
     * 批量修改周边设施
     *
     * @param opLocationFacilityDTOList
     * @return
     */
    List<OpLocationFacilityEntity> updateOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList);

    /**
     * 删除周边设施
     *
     * @param id
     * @return
     */
    Boolean delOpLocationFacility(Long id);

    /**
     * 根据场站删除周边设施
     *
     * @param locationId
     * @return
     */
    Boolean delOpLocationFacilityByLocationId(Long locationId);

    /**
     * 查询场站周边设施
     *
     * @param locationId
     * @return
     */
    List<OpLocationFacilityDTO> selectOpLocationFacilityListByLocationId(Long locationId);

    /**
     * 批量查询场站周边设施
     *
     * @param locationIdList
     * @return
     */
    List<OpLocationFacilityDTO> selectOpLocationFacilityListByLocationIdList(List<Long> locationIdList);
}
