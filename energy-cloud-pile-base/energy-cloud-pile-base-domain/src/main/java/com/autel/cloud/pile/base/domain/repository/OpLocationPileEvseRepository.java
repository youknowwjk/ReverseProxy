package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.PileQrCodeDTO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.config.PileFreeVendInfoDTO;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * <p>
 * 充电设备组合（桩） 服务类
 * </p>
 *
 * @author A22121
 * @since 2022-05-10
 */
public interface OpLocationPileEvseRepository extends IService<OpLocationPileEvseEntity> {

    /**
     * 查询桩信息
     *
     * @param pileSn
     * @return
     */
    OpLocationPileEvseDTO queryByPileSn(String pileSn);

    /**
     * 查询桩信息列表
     *
     * @param pileSnList
     * @return
     */
    List<OpLocationPileEvseDTO> queryByPileSnList(List<String> pileSnList);

    /**
     * 查询桩信息
     *
     * @param pileId
     * @return
     */
    OpLocationPileEvseDTO queryByPileId(Long pileId);

    /**
     * 场站详情桩分页查询
     *
     * @param pilePageDTO 场站id
     * @return 桩分院
     */
    Page<PilePageVO> stationPilePage(PilePageDTO pilePageDTO);

    /**
     * 站点桩分页查询（场站列表用 ）
     *
     * @param pileByStationIdAndPileSNPageDTO 场站id 桩SN码
     * @param pageIndex                       当前页
     * @param pageSize                        页大小
     * @return 桩分院
     */
    Page<PilePageVO> stationPilePageByStationIdAndPileSN(PileByStationIdAndPileSNPageDTO pileByStationIdAndPileSNPageDTO, int pageIndex, int pageSize);

    /**
     * 桩集合搜索(场站列表用)
     *
     * @param stationIds 站点id集合
     * @param pileSN     桩SN
     * @return 桩集合
     */
    Map<Long, List<PilePageVO>> stationPileListByStationIdsAndPileSN(Set<Long> stationIds, String pileSN);

    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    Void downModuleGenerateXls(HttpServletRequest request, HttpServletResponse response);

    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    Result<Void> downModuleResourceXls(HttpServletRequest request, HttpServletResponse response);

    /**
     * 批量导入桩
     *
     * @param multipartFile 文件
     * @return Result
     */
    PileUploadCheckVO uploadModuleXls(String locationId, MultipartFile multipartFile);


    /**
     * 模拟批量导入桩
     *
     * @param locationId        站点id
     * @param pileUploadDTOList 文件对象
     * @return 校验结果
     */
    PileUploadCheckVO simulateUploadModuleXls(String locationId, List<PileUploadDTO> pileUploadDTOList);

    /**
     * 批量保存桩
     *
     * @param pileUploadSaveDTO 保存数据
     * @return 保存结果
     */
    PileUploadSaveVO savePileList(PileUploadSaveDTO pileUploadSaveDTO);



    List<EvseAndTariffIdVO> getEvseByPileId(PileUploadSaveVO pileUploadSaveVO);

    Result<List<CostModelRuleDTO>> queryTariffByPileSN(String pileSN);

    Result<Map<String, CostModelRuleDTO>> queryTariffByPileSN(String... pileSN);

    OpLocationPileEvseElasticDTO findByPileSn(String pileSn);

    List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId,String keyword);
    List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId);

    boolean updateRuleId(RelatePileDTO relatePileDTO);

    boolean syncRuleName(Long ruleId, String name);

    Page<LocationForAdvertiseDTO> syncToAdvertise(PileSyncForAdvertiseParamDTO paramDTO);

    Boolean syncPileOperatorIdCache(String sn);

    List<OpLocationPileEvseVO> getListByLocationIds(List<Long> locationIds);

    /**
     * 落地页设备信息展示
     *
     * @param scanPileEvseDTO
     * @return
     */
    Result<List<OpLocationPileEvseLandingPageVO>> scanPileEvceDetail(ScanPileEvseDTO scanPileEvseDTO);

    List<OpLocationPileEvseElasticDTO> findList(List<String> pileSnList);


    /**
     * @param sellerId    商家id
     * @param currentPage 当前页码
     * @param pageSize    每页数据量
     * @param keyword     关键词（仅支持桩SN的模糊搜索）
     * @return 英标桩列表
     * @function 分页查询英标桩列表
     */
    PageVO<OpLocationPileEvseElasticDTO> getBritishActPileListPage(Long sellerId, int currentPage, int pageSize, String keyword);

    /**
     * @param opLocationPileEvseEntity op_location_pile_evse表实体类
     * @return 是否编辑成功
     * @function 英国法案认证：编辑默认充电时间
     */
    Boolean updateBritishActPileDefaultChargingTime(OpLocationPileEvseEntity opLocationPileEvseEntity,Boolean britainApproveSwitch);

    Boolean synchronousBritishActPileDefaultChargingTime(OpLocationPileEvseEntity opLocationPileEvseEntity,Boolean britainApproveSwitch);
    /**
     * @param locationIdList 场站id集合
     * @return 充电桩信息集合
     * @function 根据场站id集合查询充电桩信息集合
     */
    List<OpLocationPileEvseEntity> getPileInfoListByLocationIdList(List<Long> locationIdList);

    /**
     * @param pileSn 充电桩序列号
     * @return 充电桩信息
     * @function 根据充电桩序列号查询充电桩信息
     */
    OpLocationPileEvseEntity getPileInfoByPileSn(String pileSn);


    List<OpLocationPileEvseEntity> selectPlieList();

    List<OpLocationPileEvseElasticDTO> findListByIds(List<Long> pileIds);

    Boolean randomDelay(RandomDelayDTO randomDelayDTO);
    int getGunCountBySn(String sn);

    /**
     * @param setPileEroamingDTO 设置充电桩的互联互通开关 入参模型
     * @return 操作结果
     * @function 设置充电桩的互联互通开关
     */
    boolean setPileEroaming(SetPileEroamingDTO setPileEroamingDTO);

    /**
     * @param opLocationPileEvseEntity
     * @return 同步结果
     * @function 根据MySQL数据库中的数据同步ES数据
     */
    boolean syncPilePublicPropertyInES(OpLocationPileEvseEntity opLocationPileEvseEntity);

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的充电桩数据
     */
    List<OpLocationPileEvseElasticDTO> getAllPileInfoBySellerId(Long sellerId);

    Object newImport(String locationId, MultipartFile file);

    void downloadImportErrorFile(String language, HttpServletResponse response);

    /**
     * @param sellerId
     * @param pileSnSearch
     * @return
     * @function 按照商家id和关键字查询充电桩列表
     */
    List<OpLocationPileEvseElasticDTO> findByCondition(Long sellerId, String pileSnSearch);

    Boolean checkExist(String pileSn);

    /**
     * @return
     * @function 查询出所有的开启了互联互通功能的充电桩信息
     */
    List<OpLocationPileEvseElasticDTO> findAllEroamingPile();

    /**
     * @param locationId
     * @return
     * @function 查询出当前场站下所有的需要开启互联互通属性的充电桩集合
     */
    List<OpLocationPileEvseEntity> findAllNeedToOpenEroamingEnablePileList(Long locationId);
    /**
     * @return
     * @function 根据充电桩id查询充电桩信息
     */
    OpLocationPileEvseElasticDTO getPileInfoByPileId(Long pileId);

    /**
     * @param opLocationPileEvseElasticDTO
     * @return
     * @function 保存充电桩信息
     */
    boolean updatePileInfo(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO);

    /**
     * @param pileSnList
     * @return
     * @function 根据充电桩序列号查询充电桩信息
     */
    List<OpLocationPileEvseEntity> findPileInfoInPileSnList(List<String> pileSnList);

    Result<Boolean> updateEsSubscriptionStatusByPileSnList(UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO);

    OpLocationPileEvseElasticDTO getOpLocationPileEvseElasticDTOByPileSnFromES(String pileSn);

    List<Long> findLocationIds(Long sellerId,List<String> pileSnList);

    /**
     * 按场站ID和关键字搜索（桩名称或SN）
     *
     * @param locationIds
     * @param keyword
     * @return
     */
    List<OpLocationPileEvseElasticDTO> findList(List<Long> locationIds, String keyword);

    OpLocationPileEvseEntity findLast(String pileSn);

    /**
     * 查询枪视图列表
     * @param opLocationLiveEvseViewDTO
     * @return
     */
    List<com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO> listEvse(OpLocationLiveEvseViewDTO opLocationLiveEvseViewDTO);

    /**
     * 查询枪视图统计查询列表
     * @param opLocationEvseStateCountDTO
     * @return
     */
    List<com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO> listEvse(OpLocationEvseStateCountDTO opLocationEvseStateCountDTO);

    /**
     *
     * @param opLocationLiveEvsePageDTO
     * @return
     */
    PageVO<OpLocationEvseElasticDTO> listEvse(OpLocationLiveEvsePageDTO opLocationLiveEvsePageDTO);

    List<OpLocationPileEvseElasticDTO> queryBetaTestPileInfo(List<Long> userIdList);

    Boolean updatePileFreeVendInfo(PileFreeVendInfoDTO pileFreeVendInfoDTO);

    Page<PileQrCodeDTO> getPileQrCodePage(String keyWord, String locationId, int page, int pageSize);

    Integer repairRedisOfOperatorIdByPileSN();

    Map<String, List<CommonVO>> getPileLocationMap();
}
