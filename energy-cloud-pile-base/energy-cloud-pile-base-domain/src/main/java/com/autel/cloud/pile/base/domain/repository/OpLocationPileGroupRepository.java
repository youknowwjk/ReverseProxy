package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpLocationPileGroupParamDTO;
import com.autel.cloud.pile.base.dto.PileGroupParamDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Collection;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * 充电桩群组 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
public interface OpLocationPileGroupRepository extends IService<OpLocationPileGroupEntity> {

    OpLocationPileGroupEntity getById(Long id);

    List<Long> findGroupId(OpLocationPileGroupParamDTO dto);

    List<Long> findGroupIdV3(PileGroupParamDTO dto);

    List<Long> findGroupId(Long locationId, Long pid, Long self);

    List<OpLocationPileGroupEntity> findByPidAndMerchantId(Long merchantId, Long pid, Long self);

    List<Long> findGroupIdByPidAndMerchantId(Long merchantId, Long pid, Long self);

    Long findOrdinaryRoot(Long id);

    Long findRootByPileSn(String pileSn);

    List<OpLocationPileGroupEntity> findChildren(Long id);

    /**
     * @param id     群组ID
     * @param status 0：未启用，1：启用
     * @return
     */
    List<OpLocationPileGroupEntity> findChildren(Long id, Integer status);

    List<OpLocationPileGroupEntity> findChildren(Long id,boolean includeParent);

    boolean updateBatch(List<OpLocationPileGroupEntity> pileGroupEntityList);

    boolean insert(OpLocationPileGroupEntity pileGroupEntity);

    List<OpLocationPileGroupEntity> findDetails(Set<Long> rootIds);

    boolean check(Long id);

    /**
     * 获取全部 EMS 类型的群组id
     * @return
     */
    List<Long> queryEmsGroupId();

    boolean updateEntity(OpLocationPileGroupEntity pileGroupEntity);

    OpLocationPileGroupEntity findOne(Long id);
    OpLocationPileGroupEntity findOne(Long locationId,Long meterId);
    List<OpLocationPileGroupEntity> findList(Long locationId,Long priceId);

    boolean deleteBatch(Set<Long> deleteIds);

    /**
     * 物理删除
     * @param deleteIds
     * @return
     */
    boolean deleteByIds(Collection<Long> deleteIds);

    OpLocationPileGroupEntity findName(String pileSn);

    List<OpLocationPileGroupEntity> findAllRoot(Long rootId);

    List<OpLocationPileGroupEntity> findByIds(List<Long> ids);

    List<OpLocationPileGroupEntity> findAll();

    List<OpLocationPileGroupEntity> findByLocationId(Long locationId);

    Page<OpLocationPileGroupEntity> findChildrenPageList(Long groupId, Integer page, Integer pageSize);

    List<OpLocationPileGroupEntity> findChildrenPageListV3(Long groupId);

    /**
     * @param meterIdList
     * @return
     * @function 根据电表id集合查询充电桩群组信息
     */
    List<OpLocationPileGroupEntity> findAllByMeterIdList(List<Long> meterIdList);

    Page<OpLocationPileGroupEntity> findRootGroup(PileGroupParamDTO pileGroupParamDTO);

    int updatePidByIds(Set<Long> removeGroupIds);

    Set<String> findRemovePileSns(Long merchantId, Long id, Set<String> includeSubPileSns);

    Set<Long> findRemoveSubGroupIds(Long merchantId, Long id, Set<Long> includeSubGroupIds);

    int countByMerchantId(Long merchantId);

    void updateBatchGroupType(Integer groupType, Set<Long> updateIds);

    boolean jugeEdgeGroupId(Long groupId);

    void updateSubGroupType(Long rootId);

    boolean hasEdgeEMs(Long rootId);
}
