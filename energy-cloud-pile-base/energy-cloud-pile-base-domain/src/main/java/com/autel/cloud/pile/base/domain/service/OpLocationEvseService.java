package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.domain.model.dto.gun.SelectGunInfoForTariffGroupIdDTO;
import com.autel.cloud.pile.base.domain.model.vo.gun.SelectGunInfoForTariffGroupIdVO;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.app.GunListDto;
import com.autel.cloud.pile.base.dto.fleet.GetDeviceInfoForFleetDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileDTO;
import com.autel.cloud.pile.base.dto.tariff.BindCostModelRuleGroupForGunDTO;
import com.autel.cloud.pile.base.dto.lockOrUnlockGun.LockOrUnlockGunDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.PileInfoDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.GunListPageDto;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.autel.cloud.pile.base.vo.evse.EvseDataVO;
import com.autel.cloud.pile.base.vo.evse.EvseInfoVO;
import com.autel.cloud.pile.base.vo.evse.PileEvseInfoVO;
import com.autel.cloud.pile.base.vo.fleet.GetDeviceInfoForFleetVO;
import com.autel.cloud.pile.base.vo.evse.EvseTariffInfoVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @ClassName OpLocationEvseService
 * @Author A22121
 * @Description
 * @Date 2022/4/15 11:14
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationEvseService {
    /**
     * 根据evseSn获取部分充电设备信息
     *
     * @param evseSn
     * @return
     */
    Result<OpEvseInfoDTO> getEvseByEvseSn(String evseSn);

    /**
     * 基于evseSn获取场站相关数据
     *
     * @param evseSn
     * @return
     */
    Result<OcppLocationEVSEVO> getLocationEvseVOBySnAndGunNo(String evseSn);

    /**
     * 基于pilesn查询产站桩信息
     *
     * @param pileSn
     * @return
     */
    Result<OcppLocationEVSEVO> getLocationByPileSn(String pileSn);

    /**
     * 基于pilesn查询连接器列表
     *
     * @param pileSn
     * @return
     */
    Result<List<OplocationConnectorScanDTO>> getLocationConnectorListByPileSn(String pileSn);

    /**
     * 根据站点ID分页获取枪列表 - app
     *
     * @param gunListDto
     * @return
     */
    Result<PageDTO<GunListPageDto>> getGunListRulesByStationId(GunListDto gunListDto);

    /**
     * 充电设备详情查询
     *
     * @param id
     * @return
     */
    Result<OpLocationEvseDTO> details(Long id);

    /**
     * 下载场站充电设备导入模板
     *
     * @param request
     * @param response
     */
    void downLocationEvseXls(HttpServletRequest request, HttpServletResponse response);

    Result<List<TariffsEvseNumDTO>> getEvseNumByTariffIds(List<Long> tariffGroupIds);

    /**
     * @param pileSnList
     * @return
     * @function 判断充电桩是否在美国加州
     */
    Map<String, Boolean> judgeUSCAPile(List<String> pileSnList);

    /**
     * 为计费规则查询桩信息
     *
     * @param opPileAssociatedRuleParamDTO
     * @return
     */
    Result<List<OpEvseAssociatedRuleDTO>> getEvseInfoForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO);

    /**
     * 计费关联桩树列表
     *
     * @param opPileAssociatedRuleParamDTO
     * @return
     */
    Result<List<UnionPileTreeVO>> getEvseInfoTreeForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO);

    /**
     * 根据场站名称或充电设备SN查出场站及桩信息
     *
     * @param opLocationQueryDTO
     * @return
     */
    Result<Page<OpLocationsAndEvsesDTO>> searchSitesOrEvses(OpLocationQueryDTO opLocationQueryDTO);

    /**
     * 校验pileSn是否准确
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> verifyPileSn(String pileSn);

    /**
     * 校验pileSn是否准确
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> verifyThirdPileSn(String pileSn, Long brandId);

    /**
     * 校验pin是否准确
     *
     * @param pileSn
     * @param pin
     * @return
     */
    Result<Boolean> verifyPin(String pileSn, String pin);

    /**
     * 通过id查询es充电设备数据
     *
     * @param id
     * @return
     */
    Result<OpLocationEvseDTO> getDetailsFromEsById(Long id);

    /**
     * 新增充电设备
     *
     * @param opLocationEvseDTOs
     * @return
     */
    Result<List<PileVO>> createEvse(List<OpLocationEvseDTO> opLocationEvseDTOs);

    /**
     * 更新充电设备
     *
     * @param opLocationEvseDTOs
     * @return
     */
    Result<List<PileVO>> updateEvse(List<OpLocationEvseDTO> opLocationEvseDTOs);

    /**
     * 根据商户ID查询充电设备列表
     *
     * @param queryOplocationDTO
     * @return
     */
    Result<List<OpEvseInfoDTO>> queryPileListByOperationId(QueryOplocationDTO queryOplocationDTO);

    /**
     * 通过locationId查询所有的充电设备列表
     *
     * @param locationId
     * @return
     */
    Result<List<OpLocationEvseDTO>> getEvseListByLocationId(Long locationId);

    Result<List<OpLocationEvseDTO>> getEvseListIncludeDeletedByLocationId(Long locationId);

    Result<List<LocationEvseInfoVO>> findEvseList(List<Long> locationIds);

    List<OpLocationEvseElasticDTO> findEvseList(Long locationId, String pileSn);

    /**
     * 为计费规则查询桩信息
     *
     * @param opPileAssociatedRuleParamDTO
     * @return
     */
    Result<List<OpPileAssociatedRuleDTO>> getPileInfoForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO);

    /**
     * 为计费规则分页查询桩列表
     *
     * @param opPileAssociatedRuleParamDTO
     * @return
     */
    Result<Page<OpPileAssociatedRuleDTO>> getPileListForRules(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO);

    /**
     * 更新充电设备状态
     *
     * @param evseSn
     * @param state
     * @return
     */
    Result<Boolean> updateEvseState(String evseSn, String state);

    Result<Boolean> updateEvseStateByUpdateAt(String evseSn, String state, Long updateAt);

    Result<Boolean> batchUpdateEvseStateByUpdateAt(Collection<EvseStateDTO> evseStateDTOs);

    OpLocationEvseDetailVO getDetailsFromDbById(Long id);

    /**
     * 根据商家id，查询该商家下所有桩及场站组织信息，用于新运维平台同步
     *
     * @return
     */
    Result<List<PileInfoDTO>> queryLocationAndPileBySeller(Long sellerId);

    /**
     * 根据 pileSnList 查询充电桩信息
     *
     * @param pileSnList
     * @return
     */
    Result<List<HubPileDTO>> queryPileInfoByPileSn(List<String> pileSnList);

    Integer syncEvseExpand(EvseExpandDTO dto);

    /**
     * @param id 计费规则模型的主键Id
     * @return 计费规则模型关联的充电设备数量
     * @function 查询计费规则模型关联的充电设备数量
     */
    Integer queryCostRuleAssociatedAndSellerAccount(Long id);

    Result<OpEvseDTO> getEvseInfoByEvseSn(String evseSn);

    OpLocationInfoDTO getEvseList(String pileSn);

    Result<CostModelRuleDTO> getTatiffRuleByEvseSn(PileEvseDTO pileEvseDTO);

    /**
     * @param tariffId 计费规则id
     * @return 某个计费规则id绑定的桩的类型
     * @function 查询某个计费规则id绑定的桩的类型
     */
    List<SimpleChargePileVO> getSimpleChargePileInfoList(Long tariffId);

    List<DispatchTariffPileInfoVO> getDispatchTariffPileInfo(Long tariffId);

    List<OpLocationEvseElasticDTO> findList(List<String> pileSnList);

    Result<Boolean> syncGunType(String pileSn);

    Result<TariffRuleOfPileDTO> getZoneIdByPileAndEvse(TariffRuleOfPileDTO tariffRuleOfPileDTO);

    Result<TariffCountVO> getTariffCountByPileSn(PileEvseTariffDTO pileEvseTariffDTO);

    Result<Map<String, PileEvseSimpleVO>> getPileEvseInfoByEvseSnList(List<String> evseSnList);

    Result<OpenAndRuleInfoVO> getOpenAndRuleInfoByEvseSn(OpenAndRuleInfoDTO openAndRuleInfoDTO);

    Result<List<Long>> dataCheck();

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 下发充电枪锁枪或者不锁枪的命令
     */
    Boolean setCableEnable(LockOrUnlockGunDTO lockOrUnlockGunDTO);

    /**
     * @param lockOrUnlockGunDTO 与充电枪锁枪或者不锁枪相关的功能 入参模型
     * @return 操作结果
     * @function 获得充电枪的状态（返回此时充电枪是否已经插入充电口的标志）
     */
    Boolean getEvseInPile(LockOrUnlockGunDTO lockOrUnlockGunDTO);

    Result<Map<String, String>> getSnTimeZoneMap(List<String> evseSnList);

    /**
     * @param tariffId 计费规则id
     * @return 场站id集合
     * @function 根据计费规则id，查询充电设备表，获得该计费规则所使用的场站集合
     */
    List<Long> getAllLocationIdByTariffId(Long tariffId);

    /**
     * @param pileEvseDTO 序列号
     * @return 与场站（桩或枪）计费相关的信息
     * @function 查询与场站（桩或枪）计费相关的信息
     */
    TariffInfoVO queryTariffInfoVO(PileEvseDTO pileEvseDTO);

    Result<Boolean> bindCostModelRuleGroupForGun(List<BindCostModelRuleGroupForGunDTO> bindCostModelRuleGroupForGunDTOList);

    /**
     * @function 获得商家下所有的充电枪数据
     * @param sellerId
     * @return
     */
    List<EvseInfoVO> getAllEvseInfoBySellerId(Long sellerId);

    /**
     * @param pileSn
     * @return
     * @function 根据充电桩序列号查询充电设备信息
     */
    List<OpLocationEvseElasticDTO> findListByPileSn(String pileSn);

    /**
     * @param opLocationEvseElasticDTO
     * @return
     * @function 保存充电设备信息
     */
    boolean updateEvseInfo(OpLocationEvseElasticDTO opLocationEvseElasticDTO);

    /**
     * @param tariffGroupIdList
     * @param evseIdList
     * @return
     * @function 根据条件筛选出充电枪信息
     */
    List<OpLocationEvseEntity> findEvseInfoList(List<Long> tariffGroupIdList, List<Long> evseIdList);

    PileUploadSaveVO savePileListV2(SavePileListV2DTO savePileListV2DTO);

    PileUploadSaveVO savePileListV3(SavePileListV3DTO savePileListV2DTO);

    Result<Boolean> updatePublicMark(UpdatePublicMarkDTO updatePublicMarkDTO);

    Boolean updatePileName(UpdatePileNameDTO updatePileNameDTO);

    Boolean updatePileNameV2(UpdatePileNameDTO updatePileNameDTO);

    List<OpEvseInfoDTO> getEvseListDeleted(String pileSn);

    List<String> getTariffIdListBySn(String sn);
    /**
     * @param locationId
     * @return
     * @function 批量开启某个场站下所有的属性为公开的充电桩的互联互通属性
     */
    boolean batchSetPileEroaming(Long locationId);

    List<EroamingPileVO> queryEroamingPileListByTariff(Long tariffId);

    OpLocationPileEvseElasticDTO getPileInfoByPileSN(String pileSN);

    Result<List<PileBindTimeVO>> batchQueryPileBindTime(List<String> snList);

    /**
     * @param pileSnList
     * @return
     * @function 批量根据充电桩序列号集合获取充电枪数据集合
     */
    List<EvseInfoVO> availableEvseList(List<String> pileSnList);

    IPage<SearchEvseByNameOrSnVO> searchEvseByNameOrSn(SearchEvseByNameOrSnDTO dto,Long sellerId);

    List<OpLocationEvseElasticDTO> getSellerEvseInfo(List<String> pileSnList, Long operatorId);

    Boolean updateEvseLastOrderSeq(String evseSn ,Long orderSeq);

    List<String> getAllEvseSnByLocationIds(List<Long> locationIds);

    OpLocationEvseEntity findOneByEvseSn(String evseSn);

    Map<String, Long> queryTariffByEvseSnList(List<String> evseSnList);

    /**
     * 通过商家/场站查找桩信息
     * @param
     * @return
     */
    List<OpLocationPileEvseDTO> searchPileListBySellerId(QueryPileDTO queryPileDTO);

    List<OpLocationPileEvseElasticDTO> getPileListByPileSNList(List<String> pileSNList);

    Page<GetDeviceInfoForFleetVO> getDeviceInfoForFleet(GetDeviceInfoForFleetDTO getDeviceInfoForFleetDTO);

    Boolean setPileEroamingForPile(SetPileEroamingForPileDTO setPileEroamingForPileDTO);

    Page<SelectGunInfoForTariffGroupIdVO> selectBindGunInfoForTariffGroupId(SelectGunInfoForTariffGroupIdDTO selectGunInfoForTariffGroupIdDTO);

    Page<SelectGunInfoForTariffGroupIdVO> selectGunInfoForTariffGroupId(SelectGunInfoForTariffGroupIdDTO selectGunInfoForTariffGroupIdDTO);

    List<String> queryPileListByLocations(List<Long> locationId);

    List<EvseDataVO> batchGetEvseInfo(List<String> evseList);

    List<PileEvseInfoVO> batchGetEvseData(List<String> evseList);

    List<EvseTariffInfoVO> batchGetEvseTariffInfo(List<String> evseSnList);
}
