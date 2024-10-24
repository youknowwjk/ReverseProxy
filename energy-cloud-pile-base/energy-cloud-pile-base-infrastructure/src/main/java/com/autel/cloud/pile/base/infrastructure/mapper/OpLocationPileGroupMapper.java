package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.dto.OpLocationPileGroupParamDTO;
import com.autel.cloud.pile.base.dto.PileGroupParamDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * 充电桩群组 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@Mapper
public interface OpLocationPileGroupMapper extends BaseMapper<OpLocationPileGroupEntity> {

    List<Long> findGroupId(@Param("dto") OpLocationPileGroupParamDTO dto);

    List<Long> findGroupIdV3(@Param("dto") PileGroupParamDTO dto);

    List<Long> findGroupIdWithPid(@Param("locationId") Long locationId, @Param("pid") Long pid, @Param("self") Long self);

    List<Long> findGroupIdByPidAndMerchantId(@Param("merchantId") Long merchantId, @Param("pid") Long pid, @Param("self") Long self);

    List<Long> findOrdinaryRoot(@Param("id") Long id);

    List<OpLocationPileGroupEntity> findChildren(@Param("id") Long id, @Param("status") Integer status);

    OpLocationPileGroupEntity findName(@Param("pileSn") String pileSn);

    boolean updateBatch(@Param("list") List<OpLocationPileGroupEntity> list);

    int updatePidByIds(@Param("set") Set<Long> removeGroupIds, @Param("pid") Long i, @Param("normalGroupType")Integer normalGroupType);

    Set<String> findRemovePileSns(@Param("merchantId") Long merchantId, @Param("groupId") Long groupId, @Param("set") Set<String> includeSubPileSns);

    Set<Long> findRemoveSubGroupIds(@Param("merchantId") Long merchantId,@Param("pid")  Long pid, @Param("set") Set<Long> includeSubGroupIds);

    void updateBatchGroupType(@Param("groupType") Integer groupType, @Param("set") Set<Long> updateIds);

    int countEdgeGroupId(@Param("groupId") Long groupId);

    List<Long> selectChildIds(@Param("groupId") Long groupId, @Param("groupTypes") List<Integer> groupTypes, @Param("hasRoot") Boolean hasRoot);
}
