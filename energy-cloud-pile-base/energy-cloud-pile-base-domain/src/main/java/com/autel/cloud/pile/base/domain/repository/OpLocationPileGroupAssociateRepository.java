package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupAssociateVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * 桩和桩组关系表 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
public interface OpLocationPileGroupAssociateRepository extends IService<OpLocationPileGroupAssociateEntity> {

    List<Long> findGroupId(Long locationId,String pileSn);

    List<Long> findGroupIdByMerchantIdAndPileSn(Long merchantId, String pileSn);

    List<OpLocationPileGroupAssociateEntity> findList(Set<Long> groupIds);
    List<OpLocationPileGroupAssociateEntity> findList(Long groupId);

    List<OpLocationPileGroupAssociateEntity> findAll();

    boolean updateBatch(List<OpLocationPileGroupAssociateEntity> list);

    List<OpLocationPileGroupAssociateVO> findDetailList(List<String> pileSnList);

    boolean insertBatch(List<OpLocationPileGroupAssociateEntity> associateEntityList);

    boolean deleteByGroupIds(Collection<Long> deleteIds);

    boolean deleteBatch(List<OpLocationPileGroupAssociateEntity> associateEntityList);

    List<OpLocationPileGroupAssociateEntity> findListDeleted(List<String> pileSnList);
    List<OpLocationPileGroupAssociateEntity> findList(List<String> pileSnList);

    OpLocationPileGroupAssociateEntity findOne(String pileSn);
    OpLocationPileGroupAssociateEntity findOne(Long pileId);

    Page<OpLocationPileGroupAssociateEntity> findPageList(Long groupId, int page, int pageSize);

    List<OpLocationPileGroupAssociateEntity> findPageListV3(Long groupId);

    List<OpLocationPileGroupAssociateEntity> findAssociateList(List<Long> groupIds, int defaultPageSize);

    List<Long> findGroupIdByMerchantIdAndPileSnAndGroupName(long merchantId, String pileSn, String groupName);

    List<Long> findDemandGroupIds(long merchantId);
}
