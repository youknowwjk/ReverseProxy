package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ocpi.vo.location.LocationVO;
import com.autel.cloud.pile.base.OpLocationEvseInfoDTO;
import com.autel.cloud.pile.base.domain.model.OpLocationMenuQueryDto;
import com.autel.cloud.pile.base.domain.model.vo.location.LocationBaseVO;
import com.autel.cloud.pile.base.domain.model.dto.SetLocationEroamingForLocationDTO;
import com.autel.cloud.pile.base.domain.model.dto.SetLocationTypeForLocationDTO;
import com.autel.cloud.pile.base.domain.model.vo.SelectLocationInfoForEroamingVO;
import com.autel.cloud.pile.base.domain.model.vo.location.LocationBaseVO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.common.SearchDTO;
import com.autel.cloud.pile.base.dto.fleet.SelectLocationForFleetDTO;
import com.autel.cloud.pile.base.dto.location.LocationSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.oicp.BaseEsIdsDTO;
import com.autel.cloud.pile.base.dto.oicp.EvseSnStatusVO;
import com.autel.cloud.pile.base.dto.oicp.PileStatusToEsDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationSavePileEsDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.advancePayment.AppPrePaymentVO;
import com.autel.cloud.pile.base.vo.advancePayment.LocationInfoVO;
import com.autel.cloud.pile.base.vo.app.AggMapVO;
import com.autel.cloud.pile.base.vo.fleet.SelectLocationForFleetVO;
import com.autel.cloud.pile.base.vo.location.LocationBasicInfoPackageVO;
import com.autel.cloud.pile.base.vo.location.LocationBasicInfoVO;
import com.autel.cloud.pile.base.vo.location.LocationDataVO;
import com.autel.cloud.pile.base.vo.location.LocationSimpleInfoQueryVO;
import com.autel.cloud.pile.user.api.dto.QuerySubTreeDTO;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.pile.user.api.vo.NodeVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @ClassName OplocationService
 * @Author A22121
 * @Description
 * @Date 2022/4/14 11:59
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationService {

    /**
     * 插入新的场站
     *
     * @param opLocationDTO
     * @return
     */
    Result<Long> add(OpLocationDTO opLocationDTO);

    /**
     * 删除场站
     *
     * @param id
     * @return
     */
    Result<Boolean> delete(Long id);

    /**
     * 删除建站缓存表中的组织id和name
     *
     * @param userId
     * @return
     */
    Result<Boolean> deletedGroupIdAndGroupName(Long userId);

    /**
     * @param
     * @return
     * @description: 根据场站名称查询运营商id
     * @author: william
     * @date: 2022/9/21 10:00
     */
    List<String> getSellerIdList(String locationName);

    /**
     * 更新场站
     *
     * @param opLocationDTO
     * @return
     */
    Result<Boolean> update(OpLocationDTO opLocationDTO);

    /**
     * 查询场站（下拉菜单）
     *
     * @return
     */
    List<OpLocationMenuDTO> getStationMenu();

    /**
     * 查询EMSP场站（下拉菜单）
     *
     * @return
     */
    Result<List<OpLocationMenuDTO>> getEMSPStationMenu();

    /**
     * 查询场站（下拉菜单）
     * 如果是管理员请查询包含已经删除的
     *
     * @return
     */
    com.autel.cloud.base.model.Result<List<OpLocationMenuDTO>> getStationMenuIncludedDeleted();

    Page<OpLocationMenuDTO> getStationMenuIncludedDeletedWithoutAuthorize(PageDTO pageDTO,Boolean isIncludeDeleted);

    List<OpLocationDTO> getOpLocationBySellerId(QueryOplcationDTO queryOplcationDTO);


    /**
     * 场站是否存在
     *
     * @param groupIds 组织机构id集合
     * @return 场站是否存在
     */
    Result<Boolean> existsByGroupIdIn(List<Long> groupIds);

    Result<List<OpLocationOperationDTO>> getLocationList(List<String> groupIdList);

    /**
     * 分页查询场站
     *
     * @param opLocationQueryDTO
     * @return
     */
    Result<Page<OpLocationDTO>> pages(OpLocationQueryDTO opLocationQueryDTO);

    /**
     * 查询单个场站详情（所有字段）
     *
     * @param id
     * @return
     */
    Result<OpLocationDTO> details(Long id);

    /**
     * 查询场站地图信息
     *
     * @return
     */
    Result<List<OpLocationDTO>> getStationMapInfo();

    /**
     * 下载导入模板
     *
     * @param request
     * @param response
     */
    void downLocationXls(HttpServletRequest request, HttpServletResponse response);

    /**
     * 批量导入
     *
     * @param file
     * @return
     */
    Result<Boolean> importLocations(MultipartFile file);

    /**
     * 5月新版场站分页
     *
     * @param opLocationPageDTO 检索对象
     * @return 5月新版场站分页
     */
    Result<Page<OpLocationPageVO>> page(OpLocationPageDTO opLocationPageDTO);

    /**
     * 5月新版场站详情查询
     *
     * @param id 场站id
     * @return 场站详情
     */
    com.autel.cloud.base.model.Result<OpLocationDetailVO> detail(Long id);

    /**
     * APP地图搜索场站
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 场站数据
     */
    Result<List<OpLocationMapQueryVO>> mapQuery(OpLocationMapQueryDTO opLocationMapQueryDTO);

    /**
     * 地图找桩支持搜索
     *
     * @param opLocationMapQueryPageDTO 检索对象
     * @return 场站数据
     */
    Result<Page<OpLocationMapQueryVO>> mapQueryPile(OpLocationMapQueryPageDTO opLocationMapQueryPageDTO);

    /**
     * 地图找桩支持搜索
     *
     * @param opLocationMapQueryPageDTO 检索对象
     * @return 场站数据
     */
    Result<Page<OpLocationMapQueryVO>> mapQueryPileForOCPI(OpLocationMapQueryPageDTO opLocationMapQueryPageDTO);

    /**
     * 查询oicp-app服务，查看是否需要查询Hubject服务
     *
     * @return checkNeedHubject
     */
    Result<HubjectConfigVO> checkNeedHubject();

    /**
     * APP场站卡片
     *
     * @param opLocationCardDTO 检索对象
     * @return 返回对象
     */
    Result<OpLocationCardVO> stationCardData(OpLocationCardDTO opLocationCardDTO);

    /**
     * APP地图场站推荐
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 场站卡片列表
     */
    Result<List<OpLocationCardVO>> recommend(OpLocationMapQueryDTO opLocationMapQueryDTO);

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    Result<OpLocationAPPDetailVO> appDetail(OpLocationDetailDTO opLocationDetailDTO);

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    Result<OpLocationAPPDetailVO> appDetailForOCPI(OpLocationDetailDTO opLocationDetailDTO);

    Result<Integer> deleteOCPIEMSPData(List<Long> sellerIdList);

    /**
     * APP根据站点ID获取枪列表
     *
     * @param gunListPageDTO 检索对象
     * @return 枪列表
     */
    Result<Page<GunListPageVO>> getGunListByStationId(GunListPageDTO gunListPageDTO);

    /**
     * 处理场站是否开放
     *
     * @return
     */
    Result<Integer> JudgeOpenType();

    /**
     * 将数据库数据同步到es
     *
     * @return
     */
    Integer synchronizationData(List<Long> sellerIds);

    /**
     * 将数据库数据同步到es
     *
     * @return
     */
    Result<Boolean> synchronizationOnlyData();


    /**
     * 查询es数据
     *
     * @param id
     * @return
     */
    Result<OpLocationDTO> getDetailsFromEsById(Long id);

    /**
     * 对桩集合进行排序
     *
     * @param pileSortDTO 排序对象
     * @return 桩集合
     */
    Result<List<PilePageVO>> sortPile(PileSortDTO pileSortDTO);

    /**
     * 根据用户id查询用户所属的组织，然后通过组织id获取场站
     *
     * @return
     */
    Result<List<OpLocationDTO>> getLocationBySellerId();


    /**
     * 根据用户id查询用户所属的组织，然后通过组织id获取场站
     *
     * @return
     */
    Result<List<Long>> getLocationIdBySellerId();

    /**
     * 更新场站公告
     *
     * @param opLocationAnnouncementUpdateDTO 更新对象
     * @return 更新结果
     */
    Result<Void> updateAnnouncement(OpLocationAnnouncementUpdateDTO opLocationAnnouncementUpdateDTO);

    /**
     * 根据组织机构id获取场站信息
     */
    Result<List<OpLocationInfoVO>> getLocationByGroupId(List<Long> groupIdList);

    /**
     * 根据pileSn查询场站
     *
     * @param pileSn
     * @return
     */
    Result<OpLocationDTO> getLocationByPileSn(String pileSn);

    /**
     * 根据pileSn查询场站
     *
     * @param pileSnList
     * @return
     */
    Result<List<OpLocationListDTO>> getLocationByPileSnList(List<String> pileSnList);

    /**
     * 将场站数据库信息全部同步到es
     *
     * @param id
     * @return
     */
    Result<Boolean> synchronizationLocation(Long id);

    /**
     * 新手建站
     *
     * @param opLocationForNewDTO
     * @return
     */
    Result<OpLocationForNewVO> addForNew(OpLocationForNewDTO opLocationForNewDTO);

    /**
     * 根据新手缓存建站的信息新增场站
     *
     * @param pileTariffList
     * @return
     */
    Result<Long> addLocationByNew(List<PileTariffMapDTO> pileTariffList);

    /**
     * 获取新手建站信息
     *
     * @param
     * @return
     */
    Result<OpLocationForNewDTO> queryLocationForNewInfo();

    /**
     * 获取时区列表
     *
     * @return 时区列表
     */
    Result<List<TimeZoneVO>> getTimeZoneList();

    /**
     * 刷新场站更新时间
     *
     * @return
     */
    Result<Boolean> updateLocationUpdateTime(Long locationId);

    Result<List<GunStatusGroupVO>> stationGunStatusGroupVO(Long id);

    Result<Void> refreshGroupName();

    Result<Void> updateGroupName(Long groupId, String groupName);

    Result<List<EvseSnStatusVO>> gunStatusByEvseSn(List<String> EvseSnList);

    List<LocationInfoDTO> getLocationByKeyword(String keyword);

    IPage<PileDetailVO> getPilePageByKeyword(RuleSitePageDTO ruleSitePageDTO);

    List<SiteInfoVo> getSiteList(List<Long> locationIds, String keyword);

    Result<List<OpLocationMenuDTO>> getStationIncludedDeletedOrderByCreateAt();

    /**
     * 场站地址查询
     *
     * @param id
     * @return
     */
    Result<OpLocationAddressDTO> getLocationAddress(Long id);

    /**
     * 场站信息历史数据的批量更新（更新ZoneId）
     *
     * @param locationIds
     * @return
     */
    Result<Boolean> syncZoneId(List<Long> locationIds);

    List<OpLocationEntity> findAll();

    List<OpLocationEntity> populationZoneId(List<OpLocationEntity> list);

    List<OpLocationEntity> saveZoneId(List<OpLocationEntity> list);

    Result<Boolean> initializeOpLocationProvinceToES();

    /**
     * 三方桩激活文档下载
     *
     * @param response response
     * @return 返回对象
     */
    Result<Void> download(HttpServletResponse response, String brandName);

    /**
     * 搜索组织、场站、桩
     *
     * @param searchItemInfoDTO
     * @return
     */
    Result<SearchItemInfoVO> searchInfo(SearchItemInfoDTO searchItemInfoDTO);

    /**
     * 统计桩和枪的数量、功率、类型、状态
     *
     * @param id
     * @return
     */
    Result<OpStatisticsPileAndEvseVO> pileAndEvse(Long id);

    /**
     * 根据组织、场站、桩的id搜索场站
     *
     * @param opLocationPageDTO
     * @return
     */
    Page<OpLocationPageVO> searchByPage(OpLocationPageDTO opLocationPageDTO);
//    String searchByPage(OpLocationPageDTO opLocationPageDTO);

    /**
     * 根据组织id、场站名称、桩名称搜索场站
     *
     * @param opLocationPageDTO
     * @return
     */
    com.autel.cloud.base.model.Result<Page<OpLocationPageVO>> searchByPageV2(OpLocationPageDTO opLocationPageDTO);

    /***
     * 批量查询时区
     * @param pileSnList
     * @return
     */
    List<OpLocationDTO> getDetails(List<String> pileSnList);

    /***
     * 将hubject数据存储到es中
     * @param opLocationSavePileEsDTO
     * @return
     */
    Result<Boolean> savePileToEs(OpLocationSavePileEsDTO opLocationSavePileEsDTO);

    Result<Boolean> deletePileToEs(BaseEsIdsDTO baseEsIdsDTO, boolean isAll);

    Result<Boolean> savePileStatusToEs(List<PileStatusToEsDTO> pileStatusToEsDTOS);

    OpLocationElasticDTO findById(Long locationId);

    /**
     * 添加平台字段到场站和枪ES中
     *
     * @return
     */
    Boolean syncOpLocationESPlatform();

    Boolean updateOpLocationESPlatform();

    String getZoneIdByLocationId(Long locationId);

    String getZoneId(Long locationId);

    /**
     * 根据evse_sn获取其所在场站的ZoneId
     *
     * @param evseSn 充电设备
     * @return {@link String} zoneId
     */
    String getZonIdByEvseSn(String evseSn);

    OpLocationEvseInfoDTO getEvseInfo(String EvseSn);

    /**
     * 通过场站id查询所属组织id
     *
     * @return
     */
    Result<Long> detailById(Long id);

    Result<AggMapVO> mapAgg(OpLocationMapQueryDTO opLocationMapQueryDTO);

    Result<AggMapVO> mapAggForOCPI(OpLocationMapQueryDTO opLocationMapQueryDTO);

    /**
     * @return 迁移结果
     * @function 税费重构——计费规则的税率迁移
     */
    Boolean taxRateMigrate();

    /**
     * @param locationSimpleInfoQueryDTO 场站信息的简单查询实体 入参模型
     * @return 场站信息
     * @function 场站信息的简单查询
     */
    List<LocationSimpleInfoQueryVO> locationSimpleInfoQuery(LocationSimpleInfoQueryDTO locationSimpleInfoQueryDTO);

    GetSellerInfoAndLocationInfoVO getSellerInfoAndLocationInfo(String pileSn);

    LocationRoamingVO getLocationRoamingByEvseSn(String evseSn);

    Integer syncField();

    boolean deleteExceptionData(OpLocationExceptionDTO exceptionDTO);

    Boolean updatePower(UpdatePowerDTO updatePowerDTO);

    /**
     * @param currentPageLocationIdList
     * @return
     * @function 查询场站信息
     */
    List<OpLocationEntity> findLocationInfoByLocationIdList(List<Long> currentPageLocationIdList);

    List<EroamingPileVO> queryEroamingPileListByTariff(Long tariffId);

    /**
     * @param pileSimpleInfoQueryDTO
     * @return
     * @function 获得场站基础信息（供运维使用）
     */
    Page<LocationBasicInfoVO> getLocationBasicInfoVOPage(SearchDTO pileSimpleInfoQueryDTO);

    List<OpLocationElasticDTO> findByIds(List<Long> ids,String keyword);

    /**
     * @param sellerIdList
     * @return
     * @function 批量获得商家与该商家所拥有的所有的场站信息之间的映射关系 （供运维使用）
     */
    LocationBasicInfoPackageVO getLocationBasicInfoPackageVOBySellerIdList(List<String> sellerIdList);

    /**
     * @param pileSnList
     * @return
     * @function 根据传入的充电桩序列号集合，查询这些充电桩的信息及其所在的场站信息，并以商家id为映射条件返回这些信息（供运维使用）
     */
    LocationBasicInfoPackageVO getLocationBasicInfoPackageVOByPileSnList(List<String> pileSnList);


    LocationSimpleInfoQueryVO setLikeRoaming( Long locationId);

    LocationInfoVO getSimplyLocationInfoByLocationId(com.autel.cloud.pile.base.dto.advancePayment.LocationInfoDTO locationInfoDTO);

    Object ocpiPutLocation(Long operatorId, Map<String,Long> tariffIdMap, LocationVO locationVO);

    Boolean prePaymentEnabled(Long locationId);

    List<Long> getIdsByBusinessType(Collection<Long> locationIds, List<Integer> businessTypes);

    AppPrePaymentVO appPrePaymentVO(Long locationId);

    com.autel.cloud.base.model.Result<List<OpLocationMenuDTO>> getUserStationMenu();

    List<LocationListVO> locationListByOperatorId(LocationListDTO locationListDTO);

    Map<String, List<CommonVO>> getLocationIdSellerMap();

    List<NodeVO> querySubTree(QuerySubTreeDTO querySubTreeDTO);

    List<OpLocationDTO> getList(List<Long> ids);

    Result<List<OpLocationMenuDTO>> getStationMenuBySellerId(String merchantId);

    List<SelectLocationForFleetVO> selectLocationForFleet(SelectLocationForFleetDTO selectLocationForFleetDTO);

    Long getMaxPowerLocationId(Long sellerId);

    Page<SelectLocationInfoForEroamingVO> selectLocationInfoForEroaming(PageDTO pageDTO);

    Boolean setLocationTypeForLocation(SetLocationTypeForLocationDTO setLocationTypeForLocationDTO);

    Boolean setLocationEroamingForLocation(SetLocationEroamingForLocationDTO setLocationEroamingForLocationDTO);

    Map<Long, Boolean> batchGetSellerEroamingLocationFlag(List<Long> sellerIdList);

    Page<LocationBaseVO> getLocationDropDownPage(OpLocationMenuQueryDto pageDTO);

    AppPileDetailVO appPileDetail(String sn);

    H5PileDetailVO h5PileDetail(String sn, String encryptUserId);

    OperatorSupportInfoVO getOperatorSupportInfo(String sn);

    String getLocationIdBySn(String sn);

    List<LocationDataVO> getLocationData(Long userId);

    List<Long> getLocationIdBySellerId(Long sellerId);
}
