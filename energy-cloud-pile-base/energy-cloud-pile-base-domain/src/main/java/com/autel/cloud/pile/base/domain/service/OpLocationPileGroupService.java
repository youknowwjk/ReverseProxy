package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ems.dto.UpdateGroupInfo;
import com.autel.cloud.pile.base.domain.model.DemandControlAddConfigDTO;
import com.autel.cloud.pile.base.domain.model.DemandControlUpdateConfigDTO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingParam;
import com.autel.cloud.pile.base.dto.arithmetic.ArithmeticChargingResult;
import com.autel.cloud.pile.base.dto.pile.GroupTypeDto;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.smart.monitor.dto.RedeliveryDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * @ClassName OpLocationPileGroupService
 * @Author A22121
 * @Description
 * @Date 2022/7/13 16:50
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationPileGroupService {

    Long SINGLE_LAYER = 0L;

    Long MULTI_LAYER = 1L;

    @Transactional(rollbackFor = Exception.class)
    Result<Boolean> addV3(SmartChargeGroupConfigAddParamDTOcopy smartChargeGroupConfigParamDTO);

    Result<Boolean> add(OpLocationPileGroupDTO opLocationPileGroupDTO);

    @Transactional(rollbackFor = Exception.class)
    Result<Boolean> deleteV3(Long groupId);

    @Transactional(rollbackFor = Exception.class)
    Result<Boolean> clearSettings(Long groupId);

    @Transactional(rollbackFor = Exception.class)
    Result<Boolean> distributeSettings(Long groupId);

    Result<Boolean> delete(Long groupId);

    Result<Boolean> update(OpLocationPileGroupDTO opLocationPileGroupDTO);

    Result<OpLocationPileGroupV2VO> detail(Long groupId, Integer page, Integer pageSize);

    PileGroupDetailV3VOcopy findGroupDetailByPileSn(String pileSn);

    boolean isEnableSmartChargePile(String pileSn);

    PileGroupDetailV3VOcopy detailV3(Long groupId);

    PileGroupDetailV3VOcopy queryDetailForEms(Long groupId);

    Result<List<PileGroupTreeVOcopy>> queryPileInMerchant(PileEvseGroupParamDTO dto, long  merchantId, String zoneId);

    Result<OpLocationPileEvseGroupListVO> queryPileInLocation(OpLocationPileEvseGroupParamDTO dto);

    Result<OpPileGroupAssociateVO> queryAssociateByPileSn(String pileSn);

    Result<Boolean> deletedAssociateByPileId(Long pileId);

    List<OpPileGroupTimeSettingDTO> loadAllTimeSetting(Long rootId);

    Result<List<OpLocationPileGroupV2VO>> queryV2(OpLocationPileGroupParamDTO dto);

    Result<Page<PileGroupVOcopy>> queryV3(PileGroupParamDTO dto);

    Result<String> isAssociatePileGroup(String pileSn);

    List<OpLocationPileGroupEntity> findAllRoot(Long rootId);

    Long updateRoot(OpLocationPileGroupEntity entity);

    Result<String> setVip(String evseSn, Long userId);

    Boolean almStart(MeterParamDTO dto);

    OpLocationPileGroupEntity getOne(Long id);

    Long status(Long groupId, Integer status);

    Long clearDelivery(String pileSn);

    List<OpLocationPileGroupHistoryDetailVO> historyDetail(OpLocationPileGroupHistoryParamDTO dto);

    String getZoneId(Long locationId);

    Long clear(String pileSn);

    boolean checkStatus(Long locationDTOId, Long priceId);

    Integer syncStatus(Integer status, Long groupId);

    Boolean checkAlm(Long userId);

    Long setAlm(Long userId);

    List<OpLocationPileGroupEntity> findByLocationId(Long locationId);

    Boolean homePileUpdate(Long groupId, Integer flag);

    Boolean checkCost(Long userId);

    Long setCost(Long userId);

    List<OpLocationPileGroupVO> loadAll(Long rootId);

    Boolean redelivery(RedeliveryDTO dto);

    List<EstimatedChargeLineVO> estimatedChargeLine(String evseSn);

    List<OpLocationPileGroupV2VO.GroupPileV2DTO> groupPileList(Long groupId);

    PileGroupChargeLineVO chargeLine(String evseSn, String transactionId);

    BigDecimal getByMeterId(Long meterId);

    Boolean fastCharging(PileGroupFastDTO fastDTO);

    Boolean deliveryByEvseSn(String evseSn, String state);

    Boolean deliveryByGroupId(CommonDeliveryDTO dto);

    Boolean deliveryByUser(DeliveryByUserDTO dto);

    Long getRootId(Long groupId);

    /**
     * @param meterIdList
     * @return
     * @function 获得电表和该电表所关联的充电桩群组之间的映射关系
     */
    Map<Long, List<OpLocationPileGroupEntity>> getMeterIdAndOpLocationPileGroupEntityListMap(List<Long> meterIdList);

    /**
     * @param idList
     * @return
     * @function 根据id批量查询
     */
    List<OpLocationPileGroupEntity> findAllByIdIn(List<Long> idList);

    Result<Boolean> updateV3(SmartChargeGroupConfigUpdateParamDTOcopy smartChargeGroupConfigUpdateParamDTO);

    String generateDefaultGroupName();

    Boolean syncMeterDataFromOp(MeterSyncDataDTO dto);

    Result<Boolean> addDemandControlGroup(DemandControlAddConfigDTO dto);

    Result<Boolean> updateDemandControlGroup(DemandControlUpdateConfigDTO dto);

    String generateDefaultControlName(DemandControlAddConfigDTO dto);

    Boolean updateDemandControlStatus(Integer status, Integer deleted, Long id);

    Result<List<PileGroupDetailV3VOcopy>> queryGroupBySns(List<String> sns);

    Result<ArithmeticChargingResult>  queryArithmeticCharging(ArithmeticChargingParam req);

    List<PileGroupForEmsVOcopy> getPileGroupForEms(Long groupId);

    List<PileGroupDetailV3VOcopy> queryDetailsV3(List<Long> groupIds);

    void deleteDemandControlById(Long rootId);

    Long setLoadTypeWhiteList(Long userId, Integer type);

    List<Integer> getLoadTypeWhiteList(Long userId);

    void handleEmsGroupOffLine(Long groupId);

    void handleEmsGroupOnLine(EnergyEmsPowerInfoVO ems);

    Boolean removeLoadTypeWhiteList(Long userId, Integer loadType);

    /**
     * 获取全部 EMS 类型的群组id
     * @return
     */
    List<Long> queryEmsGroupId();

    /**
     * 加载ems类型的桩跟枪对应关系
     */
    void loadEmsGroupData();

    boolean updateGroupType(GroupTypeDto dto);

    List<PileGroupDetailV3VOcopy> queryGroupBySellerId(String sellerId) throws Exception;

    List<PileGroupDetailV3VOcopy> queryGroupByGroupIds(List<Long> groupIds) throws Exception;

    Result<Boolean> updateGroupInfo(UpdateGroupInfo groupInfo);
}
