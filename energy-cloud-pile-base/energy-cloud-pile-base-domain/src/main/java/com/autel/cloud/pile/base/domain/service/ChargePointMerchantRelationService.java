package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.ChargePointDTO;
import com.autel.cloud.pile.base.domain.model.ImminentExpireChargePointDTO;
import com.autel.cloud.pile.base.domain.model.PileUpdateDTO;
import com.autel.cloud.pile.base.domain.model.RemoveChargePointDTO;
import com.autel.cloud.pile.base.domain.model.dto.DeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.dto.SynchronizeDeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.vo.DeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.model.vo.SynchronizeDeviceInfoForPosVO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.chargepoint.GetDeviceTypeDTO;
import com.autel.cloud.pile.base.dto.chargepoint.GetPileInfoByPileDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileListDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPilePageDTO;
import com.autel.cloud.pile.base.dto.pos.GetDeviceGunNumberDTO;
import com.autel.cloud.pile.base.enums.SubStatus;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantRelationEntity;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.chargepoint.DeviceBriefInfoVO;
import com.autel.cloud.pile.base.vo.chargepoint.GetPileInfoByPileVO;
import com.autel.cloud.pile.base.vo.fleet.GetPileInfoForFleetVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.context.event.EventListener;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface ChargePointMerchantRelationService {
    List<SubStatus> SUB_REQUIRED_SUBSCRIPTION_STATUS = Arrays.asList(SubStatus.INACTIVITY, SubStatus.INVALIDITY);
    DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
    DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
    String phaseConstant = "PHASE";
    String AC_1_PHASE = "AC_1_PHASE";
    String AC_3_PHASE = "AC_3_PHASE";
    String DC = "DC";
    String AC = "AC";
    String UTF_8 = "UTF-8";

    long ONE_DAY_MILLIS = 3600 * 24 * 1000L;


    // `status` TINYINT(1) DEFAULT NULL COMMENT '0:新建；1：有效，2：失效',

    /**
     * 未生效 也就是未绑定
     */
    int NEW_LICENSE = 0;

    /**
     * 生效中
     */
    int EFFECTIVE_LICENSE = 1;

    /**
     * 失效
     */
    int INVALIDITY_LICENSE = 2;

    String DAY = "day";

    String MONTH = "month";

    String YEAR = "year";

    String asterisk = "*";

    Boolean validatePin(String sn, String pin);

    ChargePointMerchantRelationEntity save(ChargePointDTO chargePointDTO, Long merchantId);

    ChargePointMerchantRelationEntity updatePile(PileUpdateDTO pileUpdateDTO, Long merchantId);

    void scanImminentExpireChargePoint(Long... offsetDays);

    void validateSN(String sn, Long merchantId);

    AutelChargePointVO getAutelChargePoint(String sn, String pin);

    ChargePointMerchantRelationEntity update(ChargePointDTO chargePointDTO, Long merchantId);

    @EventListener
    ChargePointMerchantRelationEntity updatePileName(UpdatePileNameDTO updatePileNameDTO);

    void remove(RemoveChargePointDTO chargePointDTO, Long merchantId);

    IPage<ChargePointVO> findByPage(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId, String zoneId);

    List<ImminentExpireChargePointDTO> findSubRequiredPile(Long merchantId, String zoneId);

    void downloadTempExcel(HttpServletResponse response, String language) throws IOException;

    void downloadImportErrorFile(String language, HttpServletResponse response);

    Object newImport(String zoneId, Long merchantId, MultipartFile file);

    IPage<ChargePointAssetVO> findChargePointsByPage(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId, String zoneId);

    ChargePointRecordVO record(Long id, Long merchantId);

    ChargePointDetailVO detail(Long id, Long merchantId);

    ChargePointDetailVO detailV2(String sn, Long merchantId);

    Boolean license(List<ChargePointLicenseDTO> chargePointLicenseDTOList, Long merchantId, Long id);

    List<ChargePointSourceVO> findChargePointSource(ChargePointSourceFuzzQueryDTO chargePointQueryDTO, Long merchantId);

    OpLocationPileEvseElasticDTO modifyPileEvseId(String pileEvseId,String pileSn);

    List<ChargePointLicenseVO> findLastExpireTimeChargePointLicense(List<String> sns, List<String> serviceId, Long sellerId);

    List<ChargePointVO> findBySNs(Set<String> pileSnSet, Long valueOf);

    ChargePointVO findBySN(String pileSn, Long sellerId);

    ChargePointMerchantRelationEntity queryBySN(String sn, Long sellerId);

    IPage<ChargePointVO> subscriptPageBySellerId(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId, String zoneId);

    SubscriptionStatusCountVO subscriptionStatusCount(Long merchantId);

    Set<String> findSellerByCount(RangeConditionDto rangeConditionDto);

    List<ChargePointVO> findByFuzzNameOrSN(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId);

    List<ChargePointVO> findChargePointBySNs(Set<String> snSet, Long sellerId);

    ChargePointVO findChargePointBySN(String sn, Long sellerId);

    ChargePointDetailVO detailBySn(String pileSn);

    ChargePointPowerTypesVO existsPowerTypes(Long merchantId, String zoneId);

    Integer getConnectorNumber(String sn);

    Integer getDeviceType(GetDeviceTypeDTO getDeviceTypeDTO);

    GetPileInfoByPileVO getPileInfoByPile(GetPileInfoByPileDTO getPileInfoByPileDTO);

    Map<String, List<Integer>> getDeviceGunNumber(GetDeviceGunNumberDTO getDeviceGunNumberDTO);

    DeviceBriefInfoVO getDeviceBriefInfo(String sn, Long merchantId);

    List<DeviceInfoForPosVO> getDeviceInfoForPos(DeviceInfoForPosDTO deviceInfoForPosDTO);

    List<SynchronizeDeviceInfoForPosVO> synchronizeDeviceInfoForPos(SynchronizeDeviceInfoForPosDTO synchronizeDeviceInfoForPosDTO);

    SelectAutelDeviceInfoForOpsMgmtVO selectAutelDeviceInfoForOpsMgmt(String sn);

    List<TerminalDeviceDataVO> getTerminalDeviceData(String hostSn, Long sellerId);

    List<GetPileInfoForFleetVO> getPileInfoForFleet(List<String> pileSns);

    /**
     * description: snSubscribeDisplaySet 资产订阅概览显示配置
     * version: 1.0
     * date: 2024/5/13 9:50
     * author: A23204
     *
     * @param displaySettingDto
     * @return com.autel.cloud.base.http.pojo.Result<java.util.List<java.lang.String>>
     */
    Result<List<String>> snSubscribeDisplaySet(SnSubscribeDisplaySettingDto displaySettingDto);

    /**
     * description: getSnSubscribeDisplaySetting 查询订阅显示配置
     * version: 1.0
     * date: 2024/5/13 10:09
     * author: A23204
     *
     * @param
     * @return com.autel.cloud.base.http.pojo.Result<java.util.List<java.lang.String>>
     */
    Result<List<String>> getSnSubscribeDisplaySetting();

    void exportExcel(HttpServletRequest request, HttpServletResponse response) throws IOException;

    Page<PileBaseVO> queryPagePileByPrivilege(QueryPilePageDTO paramDTO);

    List<PileBaseVO> queryPileBySn(QueryPileListDTO paramDTO);

    void exportChargePoint(ChargePointExportDTO chargePointExportDTO, HttpServletRequest request, HttpServletResponse response, Long merchantId, String zoneId ) throws IOException;
}
