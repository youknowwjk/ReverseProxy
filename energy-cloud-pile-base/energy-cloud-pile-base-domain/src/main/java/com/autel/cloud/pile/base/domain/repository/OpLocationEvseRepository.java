package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.app.GunListDto;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.dto.pile.CheckPileNameDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.GunListPageDto;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <p>
 * 充电设备 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
public interface OpLocationEvseRepository extends IService<OpLocationEvseEntity> {

    /**
     * 查询场站的充电设备
     *
     * @param locationId 场站id
     * @return 充电设备集合
     */
    List<OpLocationEvseRealTimeDTO> getEvseByLocationId(Long locationId);

    /**
     * 查询场站的充电设备
     *
     * @param locationIds 场站id集合
     * @return 充电设备集合
     */
    Map<Long, List<OpLocationEvseRealTimeDTO>> getEvseByLocationIds(Set<Long> locationIds);

    /**
     * 删除场站的充电设备
     *
     * @param id
     */
    Boolean delete(Long id);

    /**
     * 更新充电设备计费规则
     *
     * @param opLocationEvseEntity
     * @return
     */
    Boolean associatedBillingRules(OpLocationEvseEntity opLocationEvseEntity, OpLocationPileEvseDTO opLocationPileEvseDTO);

    /**
     * 获取充电设备信息
     *
     * @param evseSn
     * @return
     */
    OpEvseInfoDTO getEvseByEvseSn(String evseSn);

    /**
     * 获取充电设备信息
     *
     * @param id
     * @return
     */
    OpEvseInfoDTO getEvseById(Long id);

    /**
     * 基于桩枪获取相关数据
     *
     * @param evseSN
     * @return
     */
    OcppLocationEVSEVO getLocationEvseVOBySnAndGunNo(String evseSN);

    /**
     * 基于pilesn查询产站桩信息
     *
     * @param pileSn
     * @return
     */
    OcppLocationEVSEVO getLocationByPileSn(String pileSn);

    /**
     * 基于pilesn查询连接器列表
     *
     * @param pileSn
     * @return
     */
    List<OplocationConnectorScanDTO> getLocationConnectorListByPileSn(String pileSn);

    /**
     * 获取充电设备详情
     *
     * @param id
     * @return
     */
    OpLocationEvseDTO details(Long id);

    /**
     * 根据站点ID分页获取枪列表 - app
     *
     * @param gunListDto
     * @return
     */
    PageDTO<GunListPageDto> getGunListRulesByStationId(GunListDto gunListDto);

    /**
     * 为计费规则查询桩信息
     *
     * @param tariffId
     * @param isExcludedTariff
     * @return
     */
    List<OpEvseAssociatedRuleDTO> queryEvseByTariffId(Long tariffId, Boolean isExcludedTariff);

    /**
     * 搜索场站及桩信息
     *
     * @param opLocationQueryDTO
     * @return
     */
    Page<OpLocationsAndEvsesDTO> searchSitesOrEvses(OpLocationQueryDTO opLocationQueryDTO);

    /**
     * 查询es
     *
     * @param id
     * @return
     */
    OpLocationEvseElasticDTO getDetailsFromEsById(Long id);

    /**
     * 新增Evse
     *
     * @param opLocationEvseDTOs
     * @return
     */
    List<PileVO> createEvse(List<OpLocationEvseDTO> opLocationEvseDTOs);

    /**
     * 更新Evse
     *
     * @param opLocationEvseDTOs
     * @return
     */
    List<PileVO> updateEvse(List<OpLocationEvseDTO> opLocationEvseDTOs);

    /**
     * 校验Evse
     *
     * @param opLocationEvseDTOs
     * @return
     */
    List<PileVO> validEvse(List<OpLocationEvseDTO> opLocationEvseDTOs);

    /**
     * 校验Evse
     *
     * @param opLocationEvseDTOs
     * @return
     */
    List<PileVO> onlyValidEvse(List<OpLocationEvseDTO> opLocationEvseDTOs);


    PileVO createEvse(OpLocationEvseDTO opLocationEvseDTO, OpLocationEntity opLocationEntity);

    /**
     * @param checkPileNameDTO
     * @return
     * @function 校验同一场站下的充电桩名称不能相同
     */
    boolean checkPileNameInLocationUnique(CheckPileNameDTO checkPileNameDTO);

    /**
     * 查询桩信息
     *
     * @param pileName   桩名称
     * @param locationId 场站主键
     * @return 查询桩信息
     */
    List<OpLocationPileEvseEntity> getPileInfoByPileNameAndLocationId(String pileName, Long locationId);


    List<OpEvseInfoDTO> queryPileListByOperationId(Long operatorId, List<Long> locationIds);


    /**
     * @param locationId
     * @return
     */
    List<OpLocationEvseDTO> queryEvseByLocationId(Long locationId);

    List<OpLocationEvseDTO> queryEvseIncludeDeletedByLocationId(Long locationId);

    List<LocationEvseInfoVO> findEvseList(List<Long> locationIds);

    /**
     * 为计费规则查询桩信息
     *
     * @param tariffId
     * @param currentUserId
     * @param isExcludedTariff
     * @return
     */
    List<OpPileAssociatedRuleDTO> queryPileByTariffId(Long tariffId, Long currentUserId, Boolean isExcludedTariff);

    /**
     * 分页为计费规则查询桩信息
     *
     * @param opPileAssociatedRuleParamDTO
     * @return
     */
    Page<OpPileAssociatedRuleDTO> queryPileByTariffIdForPage(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO);

    /**
     * 更新充电设备状态
     *
     * @param evseSn
     * @param state
     * @return
     */
    Boolean updateEvseState(String evseSn, String state);

    /**
     * 批量更新充电设备状态
     *
     * @param evseStateDTOs evseSn  state updateAt
     * @return true
     */
    Boolean batchUpdateEvseStateByUpdateAt(Collection<EvseStateDTO> evseStateDTOs);

    /**
     * 更新充电设备状态
     *
     * @param evseSn
     * @param state
     * @param updateAt
     * @return
     */
    Boolean updateEvseStateByUpdateAt(String evseSn, String state, Long updateAt);


    /**
     * @param pileSn
     * @return
     */
    Boolean verifyPileSn(String pileSn);

    /**
     * @param pileSn
     * @return
     */
    Boolean verifyThirdPileSn(String pileSn, Long brandId);

    OpLocationEvseDetailVO getDetailsFromDbById(Long id);

    /**
     * 根据 pileSnList 查询充电桩信息
     *
     * @param pileSnList
     * @return
     */
    List<HubPileDTO> getPileInfoByPileSn(List<String> pileSnList);

    Boolean syncEvseExpand(EvseExpandDTO expandDTO);

    /**
     * @param id 计费规则模型的主键Id
     * @return 计费规则模型关联的充电设备数量
     * @function 查询计费规则模型关联的充电设备数量
     */
    Integer getEvseNumberByTariffId(Long id);

    /**
     * @param tariffIdList 计费规则模型的主键Id集合
     * @return 根据计费规则模型的主键Id集合查询绑定的枪的Sn结果
     * @function 根据计费规则模型的主键Id集合查询绑定的枪的Sn
     */
    List<OpLocationEvseEntity> getPileSnListByTariffIdList(List<Long> tariffIdList);

    List<OpLocationEvseElasticDTO> findList(List<String> pileSnList);

    Boolean isHubject(Long locationId);

    boolean savBatchEs(List<OpLocationEvseElasticDTO> evseDtoList);

    CostModelRuleDTO getTatiffRuleByTariffId(Long tariffId);

    TariffRuleOfPileDTO getZoneIdByPileAndEvse(TariffRuleOfPileDTO tariffRuleOfPileDTO);

    TariffCountVO getTariffCountByPileSn(PileEvseTariffDTO pileEvseTariffDTO);

    /**
     * @param evseSnList 充电设备的sn集合
     * @return
     * @function 查询计费规则集合
     */
    List<OpLocationEvseEntity> queryTariffIdListByEvseSnList(List<String> evseSnList);

    List<OpLocationEvseElasticDTO> findAll();

    List<TariffsEvseNumDTO> getEvseNumByTariffIdsV2(List<Long> tariffGroupIds);

    List<OcppLocationEVSETimeZoneVO> getOcppLocationEVSETimeZoneVOList(List<String> evseSnList);

    /**
     * @param tariffId 计费规则id
     * @return 场站id集合
     * @function 根据计费规则id，查询充电设备表，获得该计费规则所使用的场站集合
     */
    List<OpLocationEvseEntity> getAllLocationIdByTariffId(Long tariffId);

    /**
     * @param opLocationEvseEntityList     充电设备实体集合(MySQL)
     * @param opLocationEvseElasticDTOList 充电设备实体集合(ES)
     * @return 更新结果
     * @function 为充电枪(充电设备)批量绑定计费规则
     */
    boolean bindCostModelRuleGroupForGun(List<OpLocationEvseEntity> opLocationEvseEntityList, List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList);

    /**
     * @param gunIdList 充电枪id集合
     * @return 充电设备（充电枪）的信息
     * @function 从MySQL数据库查询出充电设备（充电枪）的信息
     */
    List<OpLocationEvseEntity> getEvseInfoFromMySQL(List<Long> gunIdList);

    /**
     * @param gunIdList 充电枪id集合
     * @return 充电设备（充电枪）的信息
     * @function 从ES中查询出充电设备（充电枪）的信息
     */
    List<OpLocationEvseElasticDTO> getEvseInfoFromES(List<Long> gunIdList);

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的充电枪数据
     */
    List<OpLocationEvseElasticDTO> getAllEvseInfoBySellerId(Long sellerId);

    /**
     * @param evseInfoModifyDTO
     * @function 当充电设备（充电枪）的基本信息发生变化时，向外发出信息变动的通知，供车队项目使用！
     */
    void sendEvseInfoMQToFleet(EvseInfoModifyDTO evseInfoModifyDTO);


    /**
     * @function 当充电设备（充电枪）的基本信息发生变化时，向外发出信息变动的通知，供车队项目使用！
     * @param evseInfoModifyDTOList
     * @param locationId
     * @param userId
     */
    void sendEvseInfoMQToFleet(List<EvseInfoModifyDTO> evseInfoModifyDTOList, Long locationId, Long userId);

    /**
     * @param tariffGroupIdList
     * @param evseIdList
     * @return
     * @function 根据条件筛选出充电枪信息
     */
    List<OpLocationEvseEntity> findEvseInfoList(List<Long> tariffGroupIdList, List<Long> evseIdList);

    /**
     * @param pileSn
     * @return
     * @function 根据充电桩序列号查询充电设备信息
     */
    List<OpLocationEvseElasticDTO> findListByPileSn(String pileSn);

    Result<Boolean> updatePublicMark(UpdatePublicMarkDTO updatePublicMarkDTO);
    List<String> getTariffIdListByEvseSn(List<String> evseSnList);

    /**
     * 按ID查询，包含已删除
     *
     * @param ids
     * @return
     */
    List<OpLocationEvseEntity> findByIds(List<Long> ids);

    List<PileBindTimeVO> batchQueryPileBindTime(List<String> snList);

    List<OpEvseAssociatedRuleDTO> searchGunInformation(List<Long> evseIdList);

    OpLocationEvseElasticDTO findByEvseSn(String evseSn);

    boolean updateRuleId(AssociatedGunDTO associatedGunDTO);

    List<OpLocationEvseElasticDTO> findByLocationId(Long locationId);

    boolean syncRuleName(Long ruleId, String name);

    List<OpLocationEvseElasticDTO> finEvseListByOplocationIdAndSellerId(List<Long> locationIds, Long sellerId);

    IPage<SerachBindEvseVO> getEvseInformationByLocationIds(SerachBindEvseDTO dto, Long sellerId);

    List<OpLocationEvseElasticDTO> getSellerEvseInfo(Long operatorId, List<String> pileSnList);

    /**
     * 批量获取充电设备信息
     * @param idList
     * @return
     */
    List<OpEvseInfoDTO> getEvseByIdList(List<Long> idList);

    Boolean updateEvseLastOrderSeq(String evseSn, Long orderSeq);

    OpLocationEvseEntity findOneByEvseSn(String evseSn);

    List<String> getAllEvseSnByLocationIds(List<Long> locationIds);

    List<OpLocationEvseElasticDTO> getOpLocationEvseElasticDTOs(List<String> evseSns);
}
