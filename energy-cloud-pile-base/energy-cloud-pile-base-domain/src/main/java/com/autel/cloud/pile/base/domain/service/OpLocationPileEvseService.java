package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.SelectDeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.SelectDeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.advertise.OpPileAdvertiseReq;
import com.autel.cloud.pile.base.dto.beta.QueryBetaTestPileInfoDTO;
import com.autel.cloud.pile.base.dto.common.SearchDTO;
import com.autel.cloud.pile.base.dto.config.ClearSmartChargingProfileDTO;
import com.autel.cloud.pile.base.dto.config.PileFreeVendInfoDTO;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.dto.fleet.SelectChargingInfoForFleetDTO;
import com.autel.cloud.pile.base.dto.pile.EvscpSettingDTO;
import com.autel.cloud.pile.base.dto.pile.PileSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseBaseInfoVO;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseListVO;
import com.autel.cloud.pile.base.vo.beta.BetaTestPileInfoVO;
import com.autel.cloud.pile.base.vo.britishAct.BritishActVO;
import com.autel.cloud.pile.base.vo.britishAct.ModifyCommonPileToBritainStandPileForTestVO;
import com.autel.cloud.pile.base.vo.fleet.SelectChargingInfoForFleetVO;
import com.autel.cloud.pile.base.vo.pile.OcppConfigurationViewEnableVO;
import com.autel.cloud.pile.base.vo.pile.PileBasicInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileSimpleInfoQueryVO;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

public interface OpLocationPileEvseService {
    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    Result<Void> downModuleGenerateXls(HttpServletRequest request, HttpServletResponse response);

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
    Result<PileUploadCheckVO> uploadModuleXls(String locationId, MultipartFile multipartFile);

    /**
     * 模拟批量导入桩
     *
     * @param locationId        站点id
     * @param pileUploadDTOList 文件对象
     * @return 校验结果
     */
    Result<PileUploadCheckVO> simulateUploadModuleXls(String locationId, List<PileUploadDTO> pileUploadDTOList);

    /**
     * 批量保存桩
     *
     * @param pileUploadSaveDTO 保存数据
     * @return 保存结果
     */
    Result<PileUploadSaveVO> savePileList(PileUploadSaveDTO pileUploadSaveDTO);


    Result<List<EvseAndTariffIdVO>> getEvseByPileId(@RequestBody @Valid PileUploadSaveVO pileUploadSaveVO);

    /**
     * 根据桩SN码查询计费规则
     *
     * @param pileSN
     * @return
     */
    Result<List<CostModelRuleDTO>> queryTariffByPileSN(String pileSN);

    Result<Map<String, CostModelRuleDTO>> queryTariffByPileSN(String... pileSN);

    OpLocationPileEvseElasticDTO findByPileSn(String pileId);

    List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId);
    List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId,String keyword);

    boolean updateRuleId(RelatePileDTO relatePileDTO);

    boolean syncRuleName(Long ruleId, String name);

    Integer syncRule();

    /**
     * 查询场站桩枪数据-广告同步
     *
     * @param paramDTO
     * @return
     */
    Result<Page<LocationForAdvertiseDTO>> syncToAdvertise(PileSyncForAdvertiseParamDTO paramDTO);

    Result<Boolean> syncPileOperatorIdCache(String sn);

    Result<List<OpLocationPileEvseVO>> getListByLocationIds(List<Long> locationIds);

    Long updatePile(OpLocationPileEvseDTO dto);

    /**
     * 落地页设备信息展示
     *
     * @param scanPileEvseDTO
     * @return
     */
    Result<List<OpLocationPileEvseLandingPageVO>> scanPileEvceDetail(ScanPileEvseDTO scanPileEvseDTO);

    List<OpLocationPileEvseElasticDTO> findList(List<String> pileSnList);

    /**
     * @param currentPage 当前页码
     * @param pageSize    每页数据量
     * @param keyword     关键词（仅支持桩SN的模糊搜索）
     * @return 英标桩列表
     * @function 英国法案认证：查询英标桩列表
     */
    Page<BritishActVO> getBritishActPileListPage(int currentPage, int pageSize, String keyword);

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 英标桩详情
     * @funtion 英国法案认证：查询英标桩详情
     */
    BritishActVO getBritishActPileDetail(BritishActVO britishActVO);

    void queryEvscp(String pileSn, Long userId);

    void queryOffPeakHour(String pileSn, Long userId);

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 英标桩详情
     * @funtion 英国法案认证：编辑默认充电时间
     */
    BritishActVO updateBritishActPileDefaultChargingTime(BritishActVO britishActVO);

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 是否需要提醒
     * @funtion 英国法案认证：高峰期充电增加提醒
     */
    Boolean isPublicPileStartDuringPeakElectricityConsumption(BritishActVO britishActVO);

    /**
     * @param pileSnList 充电桩序列号集合
     * @return 判断结果
     * @function 判断充电桩是不是英标桩
     */
    Map<String, Boolean> judgeBritainStand(List<String> pileSnList);

    /**
     * @param modifyCommonPileToBritainStandPileForTestVO 为方便测试英国法案需求(把充电桩修改为英标桩) 出参模型
     * @param operationPassword                           为防止误操作，需要输入操作密令
     * @return 修改结果
     * @function 为方便测试英国法案需求(需要把充电桩修改为英标桩)
     */
    Map<String, Boolean> modifyCommonPileToBritainStandPileForTest(ModifyCommonPileToBritainStandPileForTestVO modifyCommonPileToBritainStandPileForTestVO, String operationPassword);

    Boolean syncBritainPile();

    Result<Boolean> randomDelay(RandomDelayDTO randomDelayDTO);

    RemoteStartPileDataVO getRemoteStartPileData(String sn);

    /**
     * @paraString pileSnm pileSn 商桩序列号
     * @return 商家id
     * @function 根据商家id获取商桩序列号
     */
    Long getSellerIdByPileSn(String pileSn);

    /**
     * @param pileSimpleInfoQueryDTO 充电桩信息的简单查询实体 入参模型
     * @return 充电桩信息
     * @function 充电桩信息的简单查询
     */
    List<PileSimpleInfoQueryVO> pileSimpleInfoQuery(PileSimpleInfoQueryDTO pileSimpleInfoQueryDTO);

    /**
     * @param setPileEroamingDTO 设置充电桩的互联互通开关 入参模型
     * @return 操作结果
     * @function 设置充电桩的互联互通开关
     */
    boolean setPileEroaming(SetPileEroamingDTO setPileEroamingDTO);

    /**
     * @return 同步条数
     * @function 同步充电桩的公开属性标志
     */
    int syncPilePublicProperty();

    /**
     * @param pileSnList
     * @return
     * @function 根据充电桩序列号批量查询充电桩基础信息
     */
    List<com.autel.cloud.pile.base.vo.pile.PileInfoVO> getPileInfoListByPileSnList(List<String> pileSnList);

    Object newImport(String locationId, MultipartFile file);

    void downloadImportErrorFile(String language, HttpServletResponse response);

    /**
     * @return
     * @function 查询出所有的开启了互联互通功能的充电桩信息
     */
    List<OpLocationPileEvseElasticDTO> findAllEroamingPile();
    Result<Boolean> getConfigurations(String sn);

    Result<Boolean> changeConfiguration(ChangeConfigurationDTO changeConfigurationDTO);

    /**
     * @param locationId
     * @return
     * @function 批量开启某个场站下所有的属性为公开的充电桩的互联互通属性
     */
    boolean batchSetPileEroaming(Long locationId);
    /**
     * @return
     * @function 同步充电桩名称
     */
    Integer syncPileName();

    /**
     * @param pileSimpleInfoQueryDTOList
     * @return
     * @function 查询充电桩信息
     */
    List<PileInfoVO> queryPileInfo(List<PileSimpleInfoQueryDTO> pileSimpleInfoQueryDTOList);

    Result<Boolean> updateEsSubscriptionStatusByPileSnList(UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO);

    Boolean handleEvscpSettingDTO(EvscpSettingDTO evscpSettingDTO);

    /**
     * 查询最新记录包含已删除
     *
     * @param pileSn
     * @return
     */
    OpLocationPileEvseEntity findLast(String pileSn);

    List<OpLocationPileEvseElasticDTO> findList(Long operatorId, List<String> includeField);

    /**
     * @param pileSimpleInfoQueryDTO
     * @return
     * @function 获得充电桩基础信息（供运维使用）
     */
    Page<PileBasicInfoVO> getPileBasicInfoVOPage(SearchDTO pileSimpleInfoQueryDTO);

    List<String> getPileSnList();

    OcppConfigurationViewEnableVO ocppConfigurationViewEnable(String pileSn);

    Result<Boolean> sendLocalList(LocalListInformationDTO localListInformationDTO);

    List<UnionPileTreeVO> searchPileBySellerId(Long operatorId, Long locationId);

    Boolean clearPileSmartChargingProfile(ClearSmartChargingProfileDTO clearSmartChargingProfileDTO);

    IPage<SearchPileByNameOrSnVO> searchPileByNameOrSn(SearchPileByNameOrSnDTO searchEvseByNameOrSnDTO);

    String getZoneIdByPileSn(String pileSn);

    Result<AdvertiseBaseInfoVO> queryAdvertiseBaseInfo(String pileSn);

    Result<Page<AdvertiseListVO>> queryAdvertiseList(OpPileAdvertiseReq advertiseReq);

    List<BetaTestPileInfoVO> queryBetaTestPileInfo(QueryBetaTestPileInfoDTO queryBetaTestPileInfoDTO);

    Boolean updatePileFreeVendInfo(PileFreeVendInfoDTO pileFreeVendInfoDTO);

    PileInfoVO getPileBriefInformation(String pileSn);

    OpLocationEvseElasticDTO getEvseSnByIdAndGunCode(String id, String gunCode);

    /**
     * 通过商家/场站查找桩信息
     * @param
     * @return
     */
    List<OpLocationPileEvseDTO> searchPileListBySellerId(QueryPileDTO queryPileDTO);

    Boolean setPileEroamingForPile(SetPileEroamingForPileDTO setPileEroamingForPileDTO);

    List<SelectDeviceInfoForPosVO> selectDeviceInfoForPos(SelectDeviceInfoForPosDTO selectDeviceInfoForPosDTO);

    Result<Boolean> batchSendLocalList(LocalListDTO localListDTO);

    Map<String, List<CommonVO>> getPileLocationMap();

    List<SelectChargingInfoForFleetVO> selectChargingInfoForFleet(SelectChargingInfoForFleetDTO selectChargingInfoForFleetDTO);
}
