package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.OpLocationNameDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationOperationEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 充电站运营 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Mapper
@Repository
public interface OpLocationOperationMapper extends BaseMapper<OpLocationOperationEntity> {
    /**
     * 根据locationId删除
     *
     * @param locationId
     */
    void deleteByLocationId(Long locationId);

    /**
     * 根据updateId更新
     *
     * @param opLocationOperationEntity
     */
    void updateByLocationId(OpLocationOperationEntity opLocationOperationEntity);

    /**
     * 根据运营类型查询场站id
     *
     * @param operationType
     * @return
     */
    List<Long> selectLocationIdsByOperationType(String operationType);

    OpLocationOperationEntity existsByGroupIdIn(List<Long> groupIds);

    List<OpLocationNameDTO> getLocationByGroupId(@Param("list") List<Long> groupIds);
}
