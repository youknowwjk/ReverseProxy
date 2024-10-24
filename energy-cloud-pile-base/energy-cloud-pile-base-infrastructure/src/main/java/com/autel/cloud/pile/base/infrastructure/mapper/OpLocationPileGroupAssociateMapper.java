package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupAssociateVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Set;

/**
 * <p>
 * 桩和桩组关系表 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@Mapper
public interface OpLocationPileGroupAssociateMapper extends BaseMapper<OpLocationPileGroupAssociateEntity> {

    List<Long> findGroupIdByMerchantIdAndPileSnAndGroupName(@Param("merchantId")Long merchantId, @Param("pileSn") String pileSn, @Param("groupName") String groupName);

    List<Long> findGroupIdByPileSn(@Param("pileSn") String pileSn);

    List<Long> findGroupId(@Param("locationId")Long locationId,@Param("pileSn") String pileSn);

    List<OpLocationPileGroupAssociateEntity> findList(@Param("groupIds") Set<Long> groupIds);

    List<OpLocationPileGroupAssociateVO> findDetailList(@Param("pileSnList") List<String> pileSnList);

    List<OpLocationPileGroupAssociateEntity> findListDeleted( @Param("pileSnList") List<String> pileSnList,@Param("deleted") Integer deleted);

    List<OpLocationPileGroupAssociateEntity> findAssociateList(@Param("groupIdList") List<Long> groupIdList, @Param("pageSize") int pageSize);

    List<Long> findGroupIdByMerchantIdAndPileSn(Long merchantId, String pileSn);

    List<Long> findDemandGroupIds(long merchantId);
}
