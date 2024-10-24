package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ocpi.vo.location.LocationVO;
import com.autel.cloud.pile.base.domain.model.OpLocationMenuQueryDto;
import com.autel.cloud.pile.base.domain.model.vo.location.LocationBaseVO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.oicp.BaseEsIdsDTO;
import com.autel.cloud.pile.base.dto.oicp.EvseSnStatusVO;
import com.autel.cloud.pile.base.dto.oicp.PileStatusToEsDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationSavePileEsDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.AggMapVO;
import com.autel.cloud.pile.base.vo.app.AppDetailPileListVO;
import com.autel.cloud.pile.base.vo.location.LocationSimpleInfoQueryVO;
import com.autel.cloud.pile.user.api.dto.QuerySubTreeDTO;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.pile.user.api.vo.NodeVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.scheduling.annotation.Async;

import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <p>
 * 场站表 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
public interface OpLocationRepository extends IService<OpLocationEntity> {

    /**
     * 添加场站
     *
     * @param oplocationDTO
     * @return
     */
    OpLocationEntity add(OpLocationDTO oplocationDTO);

    /**
     * 删除场站
     *
     * @param id
     * @return
     */
    Boolean delete(Long id);

    /**
     * 场站是否存在
     *
     * @param groupIds 组织机构id集合
     * @return 场站是否存在
     */
    Boolean existsByGroupIdIn(List<Long> groupIds);

    /**
     * 更新场站
     *
     * @param opLocationDTO
     * @return
     */
    Boolean update(OpLocationDTO opLocationDTO);

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
    List<OpLocationMenuDTO> getEMSPStationMenu();

    /**
     * 查询场站（下拉菜单） 包含已经删除的
     *
     * @return
     */
    List<OpLocationMenuDTO> getStationMenuIncludedDeleted();

    Page<OpLocationMenuDTO> getStationMenuIncludedDeletedWithoutAuthorize(PageDTO pageDTO,Boolean isIncludeDeleted);

    /**
     * 查询场站（下拉菜单） 包含已经删除的(根据创建时间降序)
     *
     * @return
     */
    List<OpLocationMenuDTO> getStationIncludedDeletedOrderByCreateAt();

    List<OpLocationDTO> getOpLocationBySellerId(QueryOplcationDTO queryOplcationDTO);

    /**
     * 获取场站运营数据
     *
     * @param locationId
     * @return
     */
    OpLocationOperationInfoDTO getLocationOperationInfo(Long locationId);

    /**
     * 获取场站运营数据 按日期
     *
     * @param dayNum
     * @param locationId
     * @return
     */
    List<OpLocationOperationInfoDTO> getLocationOperationInfoListForDay(Integer dayNum, Long locationId);

    /**
     * 分页查询场站
     *
     * @param opLocationQueryDTO
     * @return
     */
    Page<OpLocationDTO> pages(OpLocationQueryDTO opLocationQueryDTO, List<Long> opLocationId);

    /**
     * 根据名称查出id
     *
     * @param opLocationQueryDTO
     * @return
     */
    List<Long> selectByName(OpLocationQueryDTO opLocationQueryDTO);

    /**
     * 当个场站详情（返回所有字段）
     *
     * @param id
     * @return
     */
    OpLocationDTO details(Long id);

    /**
     * 查询场站地图信息
     *
     * @return
     */
    List<OpLocationDTO> getStationMapList();

    /**
     * 根据名称查出对象
     *
     * @param query
     * @return
     */
    List<OpLocationEntity> selectByName(LambdaQueryWrapper<OpLocationEntity> query);

    /**
     * 根据ID查询站点信息
     *
     * @param id
     * @return
     */
    OpLocationEntity selectOpLocationEntityById(Long id);

    /**
     * 5月新版场站分页
     *
     * @param opLocationPageDTO 检索对象
     * @return 5月新版场站分页
     */
    Page<OpLocationPageVO> stationPage(OpLocationPageDTO opLocationPageDTO);

    /**
     * 5月新版场站详情查询
     *
     * @param id 场站id
     * @return 场站详情
     */
    OpLocationDetailVO detail(Long id);

    /**
     * app地图上搜索场站
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 场站数据
     */
    List<OpLocationMapQueryVO> mapQuery(OpLocationMapQueryDTO opLocationMapQueryDTO);

    List<OpLocationMapQueryVO> mapQuery2(OpLocationMapQueryDTO opLocationMapQueryDTO);

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
     * APP场站卡片
     *
     * @param opLocationCardDTO 检索对象
     * @return 返回对象
     */
    OpLocationCardVO stationCardData(OpLocationCardDTO opLocationCardDTO);

    /**
     * APP场站推荐
     *
     * @param opLocationMapQueryDTO 检索对象
     * @return 推荐场站列表
     */
    List<OpLocationCardVO> recommend(OpLocationMapQueryDTO opLocationMapQueryDTO);

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    OpLocationAPPDetailVO appDetail(OpLocationDetailDTO opLocationDetailDTO);

    /**
     * APP场站详情
     *
     * @param opLocationDetailDTO 检索对象
     * @return 场站详情
     */
    OpLocationAPPDetailVO appDetailForOCPI(OpLocationDetailDTO opLocationDetailDTO);

    /**
     * APP根据站点ID获取枪列表
     *
     * @param gunListPageDTO 检索对象
     * @return 枪列表
     */
    Page<GunListPageVO> getGunListByStationId(GunListPageDTO gunListPageDTO);

    /**
     * 处理场站是否开放
     */
    Integer JudgeOpenType();


    /**
     * 同步场站
     *
     * @param opLocationEntity
     * @return
     */
    @Async
    void syncLocation(OpLocationEntity opLocationEntity);

    /**
     * 同步数据
     *
     * @return
     */
    Integer synchronizationData(List<Long> sellerIds);

    /**
     * 同步数据
     *
     * @return
     */
    Boolean synchronizationOnlyData();

    /**
     * 从es获取数据
     *
     * @param id
     * @return
     */
    OpLocationElasticDTO getDetailsFromEsById(Long id);

    /**
     * 场站列表对桩集合进行排序
     *
     * @param pileSortDTO 排序对象
     * @return 桩集合
     */
    List<PilePageVO> sortPile(PileSortDTO pileSortDTO);

    /**
     * 更新场站公告
     *
     * @param opLocationAnnouncementUpdateDTO 更新对象
     */
    void updateAnnouncement(OpLocationAnnouncementUpdateDTO opLocationAnnouncementUpdateDTO);

    /**
     * 根据组织机构id获取场站信息
     *
     * @param groupIdList
     * @return
     */
    List<OpLocationInfoVO> getLocationByGroupId(List<Long> groupIdList);

    /**
     * 国家简写转成全称
     *
     * @param country 国家简写
     * @return 国家全称
     */
    String changeCountry(String country);

    /**
     * 根据组织查询场站
     *
     * @param sellerId
     * @return
     */
    List<OpLocationDTO> getLocationBySellerId(Long sellerId);

    /**
     * 根据组织查询场站
     *
     * @param sellerId
     * @return
     */
    List<Long> getLocationIdBySellerId(Long sellerId);

    /**
     * 根据pileSn查询场站
     *
     * @param pileSn
     * @return
     */
    OpLocationDTO getLocationByPileSn(String pileSn);

    /**
     * 根据pileSn查询场站
     *
     * @param pileSnList
     * @return
     */
    List<OpLocationListDTO> getLocationByPileSnList(List<String> pileSnList);

    /**
     * 将场站数据库信息全部同步到es
     *
     * @param id
     * @return
     */
    Boolean synchronizationLocation(Long id);

    /**
     * 获取时区列表
     *
     * @return 时区列表
     */
    List<TimeZoneVO> getTimeZoneList();

    Boolean updateLocationUpdateTime(Long locationId);

    List<GunStatusGroupVO> stationGunStatusGroupVO(Long id);

    void refreshGroupName();

    List<EvseSnStatusVO> gunStatusByEvseSn(List<String> EvseSnList);


    void updateGroupName(Long groupId, String groupName);

    List<LocationInfoDTO> getLocationByKeyword(String keyword);

    IPage<PileDetailVO> getPilePageByKeyword(RuleSitePageDTO ruleSitePageDTO);

    List<SiteInfoVo> getSiteList(List<Long> locationIds, String keyword);


    List<OpLocationEntity> findAll();

    /**
     * 场站信息历史数据的批量更新（更新ZoneId）
     *
     * @param locationIds
     * @return
     */
    Boolean syncZoneId(List<Long> locationIds);


    List<OpLocationEntity> populationZoneId(List<OpLocationEntity> list);

    List<OpLocationEntity> saveZoneId(List<OpLocationEntity> list);

    /**
     * 场站地址查询
     *
     * @param id
     * @return
     */
    Result<OpLocationAddressDTO> getLocationAddress(Long id);

    Result<Boolean> initializeOpLocationProvinceToES();

    /**
     * 三方桩激活文档下载
     *
     * @param response response
     * @return 返回对象
     */
    void download(HttpServletResponse response, String brandName);

    /**
     * 搜索组织、场站、桩
     *
     * @param searchItemInfoDTO
     * @return
     */
    SearchItemInfoVO searchInfo(SearchItemInfoDTO searchItemInfoDTO);

    /**
     * 统计桩和枪的数量、功率、类型、状态
     *
     * @param id
     * @return
     */
    OpStatisticsPileAndEvseVO pileAndEvse(Long id);

    /**
     * 根据组织、场站、桩的id搜索场站
     *
     * @param opLocationPageDTO
     * @return
     */
    Page<OpLocationPageVO> searchByPage(OpLocationPageDTO opLocationPageDTO);

    /**
     * 根据组织id，场站名称，桩名称搜索场站
     *
     * @param opLocationPageDTO
     * @return
     */
    Page<OpLocationPageVO> searchByPageV2(OpLocationPageDTO opLocationPageDTO);

    /**
     * 获取枪对应计费规则
     *
     * @param esEVSEList
     * @return
     */
    List<AppDetailPileListVO> appDetailPileList(List<OpLocationEvseElasticDTO> esEVSEList);

    /**
     * 添加平台字段到场站和枪ES中
     *
     * @return
     */
    Boolean syncOpLocationESPlatform();

    Boolean updateOpLocationESPlatform();

    Result<Boolean> savePileToEs(OpLocationSavePileEsDTO opLocationSavePileEsDTO);

    Result<Boolean> deletePileToEs(BaseEsIdsDTO baseEsIdsDTO, boolean isAll);

    Result<Boolean> savePileStatusToEs(List<PileStatusToEsDTO> pileStatusToEsDTOS);

    String getZoneId(Long locationId);

    String getZonIdByEvseSn(String evseSn);

    OpLocationEvseElasticDTO getEvseInfo(String evseSn);

    /**
     * 通过场站id查询所属组织id
     *
     * @return
     */
    Long detailById(Long id);

    AggMapVO mapAgg(OpLocationMapQueryDTO opLocationMapQueryDTO);

    AggMapVO mapAggForOCPI(OpLocationMapQueryDTO opLocationMapQueryDTO);

    /**
     * @param sellerId 商家id
     * @return 场站信息
     * @function 根据商家id获取场站信息
     */
    List<OpLocationEntity> getLocationInfoBySellerId(Long sellerId);

    /**
     * @param id 场站id
     * @return 场站信息
     * @function 根据场站主键获取场站信息
     */
    OpLocationEntity getLocationInfoById(Long id);

    /**
     * @param opLocationEntity 场站实体
     * @return 执行结果
     * @function 修复场站的税率数据
     */
    boolean taxRateMigrate(OpLocationEntity opLocationEntity);

    GetSellerInfoAndLocationInfoVO getLocationInfo(String pileSn);

    List<OpLocationElasticDTO> findByIds(List<Long> ids,String keyword);

    LocationRoamingVO getLocationRoamingByEvseSn(String evseSn);
    /**
     * @param locationId
     * @param ocpiEnabled
     * @return
     * @function 场站ocpi启用设置
     */
    Boolean updateLocationOcpiEnalbed(Long locationId, Boolean ocpiEnabled);

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的场站数据
     */
    List<OpLocationElasticDTO> getAllLocationInfoBySellerId(Long sellerId);

    /**
     * @param locationIdList
     * @return
     * @function 根据场站id查询场站信息
     */
    List<OpLocationEntity> findLocationInfoInLocationList(List<Long> locationIdList);

    OpLocationEntity selectById(Long locationId);

    List<EroamingPileVO> getEroamingPileListByTariff(Long tariffId);


    LocationSimpleInfoQueryVO setLikeRoaming(Long locationId);

    List<OpLocationElasticDTO> findList(Long sellerId, List<String> pileSnList);

    Object ocpiPutLocation(Long operatorId, Map<String,Long> tariffIdMap, LocationVO locationVO);

    Integer deleteOCPIEMSPData(List<Long> sellerIdList);

    /**
     * 查询当前用户下场站列表
     * @return
     */
    List<OpLocationMenuDTO> getUserStationMenu();

    Set<Long> getAllNotAdvertisingStation(Long sellerId);

    List<OpLocationMenuDTO> getStationMenuBySellerId(String merchantId);

    List<LocationListVO> locationListByOperatorId(LocationListDTO locationListDTO);

    Map<String, List<CommonVO>> getLocationIdSellerMap();

    List<NodeVO> querySubTree(QuerySubTreeDTO querySubTreeDTO);

    Page<LocationBaseVO> getLocationDropDownPage(OpLocationMenuQueryDto pageDTO);

    void appAddEMSPCondition(String operatorId, PileBaseEnum pileBaseEnum);
}
