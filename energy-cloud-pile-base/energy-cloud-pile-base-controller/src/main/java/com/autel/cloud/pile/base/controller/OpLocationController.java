package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.ocpi.dto.PutLocationToAutelDTO;
import com.autel.cloud.pile.base.OpLocationEvseInfoDTO;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.model.OpLocationMenuQueryDto;
import com.autel.cloud.pile.base.domain.model.dto.SetLocationEroamingForLocationDTO;
import com.autel.cloud.pile.base.domain.model.dto.SetLocationTypeForLocationDTO;
import com.autel.cloud.pile.base.domain.model.vo.SelectLocationInfoForEroamingVO;
import com.autel.cloud.pile.base.domain.model.vo.location.LocationBaseVO;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.advancePayment.LocationInfoDTO;
import com.autel.cloud.pile.base.dto.common.SearchDTO;
import com.autel.cloud.pile.base.dto.fleet.SelectLocationForFleetDTO;
import com.autel.cloud.pile.base.dto.location.LocationSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.oicp.EvseSnStatusVO;
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
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.elasticsearch.ElasticsearchException;
import org.elasticsearch.action.bulk.BulkItemResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.core.TimeValue;
import org.elasticsearch.index.reindex.BulkByScrollResponse;
import org.elasticsearch.index.reindex.ReindexRequest;
import org.elasticsearch.index.reindex.ScrollableHitSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;
import javax.validation.constraints.Max;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 * 场站表 前端控制器
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Log4j2
@RestController
@RequestMapping("/opLocation")
@Api(tags = "场站设置")
@Validated
public class OpLocationController {
    private final OpLocationService opLocationService;
    @Autowired
    private RestHighLevelClient restHighLevelClient;
    @Autowired
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private OpLocationEvseService opLocationEvseService;

    public OpLocationController(OpLocationService opLocationService) {
        this.opLocationService = opLocationService;
    }

    @OperationActionLog(action = "add", object = "location")
    @PostMapping("/add")
    @ApiOperation(value = "新增场站", notes = "新增场站")
    public Result<Long> add(@RequestBody @Valid OpLocationDTO locationDTO) {
        locationDTO.setOperatorId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        if (locationDTO.getBusinessType() == 3) {
            locationDTO.setTaxDTO(null);
        }
        return opLocationService.add(locationDTO);
    }


    @PostMapping("/addForNew")
    @ApiOperation(value = "新手建站", notes = "新手建站")
    @Deprecated
    public Result<OpLocationForNewVO> addForNew(@RequestBody OpLocationForNewDTO opLocationForNewDTO) {

        log.info("===>>>OpLocationController.addForNew opLocationForNewDTO: {}", JSON.toJSONString(opLocationForNewDTO));

        Long userId = UserUtil.getUserId();
        String key = "energy:pile_base:location:rock:" + userId;
        Boolean flag = stringRedisTemplate.opsForValue().setIfAbsent(key, userId.toString(), 30L, TimeUnit.SECONDS);
        if (flag) {
            try {
                return opLocationService.addForNew(opLocationForNewDTO);
            } finally {
                stringRedisTemplate.delete(key);
            }
        }
        return Result.ofSucceed(null);
    }

    @PostMapping("/addLocationByNew")
    @ApiOperation(value = "根据新手缓存建站的信息新增场站", notes = "根据新手缓存建站的信息新增场站")
    @Deprecated
    public Result<Long> addLocationByNew(@RequestBody List<PileTariffMapDTO> pileTariffList) {
        Long userId = LoginUserHolder.getLoginUser().getId();
        String key = RedisKeyConstant.getAddLocationLock(userId);
        String deleteKey = RedisKeyConstant.getStringAddLocationException(userId);
        Boolean lock = stringRedisTemplate.opsForValue().setIfAbsent(key, String.valueOf(System.currentTimeMillis()), 1, TimeUnit.MINUTES);
        if (lock != null && lock) {
            try {
                return opLocationService.addLocationByNew(pileTariffList);
            } finally {
                String value = stringRedisTemplate.opsForValue().get(deleteKey);
                if (StringUtils.hasText(value)) {
                    OpLocationExceptionDTO exceptionDTO = JSON.parseObject(value, OpLocationExceptionDTO.class);
                    opLocationService.deleteExceptionData(exceptionDTO);
                    stringRedisTemplate.delete(deleteKey);
                }
                stringRedisTemplate.delete(key);
            }
        }
        return Result.ofFailed(HttpCodeEnum.FORBIDDEN, "repeated submit.");
    }

    @GetMapping("/queryLocationForNewInfo")
    @ApiOperation(value = "获取用户新手建站数据", notes = "获取用户新手建站数据")
    @Deprecated
    public Result<OpLocationForNewDTO> queryLocationForNewInfo() {
        return opLocationService.queryLocationForNewInfo();
    }

    @PostMapping("/deletedGroupIdAndGroupName")
    @ApiOperation(value = "删除建站缓存表中的组织id和name", notes = "删除建站缓存表中的组织id和name")
    public Result<Boolean> deletedGroupIdAndGroupName(@RequestParam(value = "userId") Long userId) {
        return opLocationService.deletedGroupIdAndGroupName(userId);
    }

    @OperationActionLog(action = "delete", object = "location")
    @PostMapping("/delete/{id}")
    @ApiOperation(value = "删除场站", notes = "删除场站")
    public Result<Boolean> delete(@PathVariable Long id) {
        return opLocationService.delete(id);
    }

    @OperationActionLog(action = "edit", object = "location")
    @PostMapping("/update")
    @ApiOperation(value = "更新场站", notes = "更新场站")
    public Result<Boolean> update(@RequestBody @Valid OpLocationDTO opLocationDTO) {
        opLocationDTO.setOperatorId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        return opLocationService.update(opLocationDTO);
    }

    @PostMapping("/getStationMenu")
    @ApiOperation(value = "查询场站（下拉菜单）", notes = "查询场站（下拉菜单）")
    public Result<List<OpLocationMenuDTO>> getStationMenu() {
        return Result.ofSucceed(opLocationService.getStationMenu());
    }

    @PostMapping("/getStationMenuBySellerId")
    @ApiOperation(value = "查询场站非运营平台调用（下拉菜单）", notes = "查询场站非运营平台调用（下拉菜单）")
    public Result<List<OpLocationMenuDTO>> getStationMenuBySellerId(@RequestHeader(value = "merchantId") String merchantId) {
        return opLocationService.getStationMenuBySellerId(merchantId);
    }

    @PostMapping("/getEMSPStationMenu")
    @ApiOperation(value = "查询emsp场站（下拉菜单）", notes = "查询场站emsp（下拉菜单）")
    public Result<List<OpLocationMenuDTO>> getEMSPStationMenu() {
        return opLocationService.getEMSPStationMenu();
    }

    @PostMapping("/getUserStationMenu")
    @ApiOperation(value = "查询用户下场站（下拉菜单）", notes = "查询用户下场站（下拉菜单）")
    public com.autel.cloud.base.model.Result<List<OpLocationMenuDTO>> getUserStationMenu() {
        return opLocationService.getUserStationMenu();
    }

    @PostMapping("/getStationMenuIncludedDeleted")
    @ApiOperation(value = "查询场站包含已经删除的（下拉菜单）", notes = "查询场站（下拉菜单）")
    public com.autel.cloud.base.model.Result<List<OpLocationMenuDTO>> getStationMenuIncludedDeleted(@RequestParam(value = "isIncludeDeleted",required = false) Boolean isIncludeDeleted) {
        if (!ObjectUtils.isEmpty(isIncludeDeleted) && Boolean.TRUE.equals(isIncludeDeleted)) {
            return opLocationService.getStationMenuIncludedDeleted();
        }
        return opLocationService.getUserStationMenu();
    }

    @PostMapping("/getStationMenuIncludedDeleted/v2")
    @ApiOperation(value = "查询场站包含已经删除的（下拉菜单,不过滤权限）", notes = "查询场站（下拉菜单）")
    public Result<Page<OpLocationMenuDTO>> getStationMenuIncludedDeletedWithoutAuthorize(@RequestBody PageDTO pageDTO,@RequestParam(value = "isIncludeDeleted",required = false) Boolean isIncludeDeleted) {
        return Result.ofSucceed(opLocationService.getStationMenuIncludedDeletedWithoutAuthorize(pageDTO,isIncludeDeleted));
    }

    @PostMapping("/getStationIncludedDeletedOrderByCreateAt")
    @ApiOperation(value = "查询场站包含已经删除的（下拉菜单）", notes = "查询场站（下拉菜单）")
    public Result<List<OpLocationMenuDTO>> getStationIncludedDeletedOrderByCreateAt(@RequestParam(value = "isIncludeDeleted",required = false) Boolean isIncludeDeleted) {
        if (!ObjectUtils.isEmpty(isIncludeDeleted) && Boolean.TRUE.equals(isIncludeDeleted)) {
            return opLocationService.getStationIncludedDeletedOrderByCreateAt();
        }
        return Result.ofSucceed(opLocationService.getStationMenu());
    }

    @PostMapping("/getOpLocationBySellerId")
    @ApiOperation(value = "根据商户ID查询所有的场站", notes = "根据商户ID查询所有的场站")
    public Result<List<OpLocationDTO>> getOpLocationBySellerId(@RequestBody QueryOplcationDTO queryOplcationDTO) {
        return Result.ofSucceed(opLocationService.getOpLocationBySellerId(queryOplcationDTO));
    }

    /**
     * @param id
     * @return
     * @deprecated
     */
    @PostMapping("/details/{id}")
    @ApiOperation(value = "场站详情查询", notes = "场站详情查询")
    @Deprecated
    public Result<OpLocationDTO> details(@PathVariable Long id) {
        return opLocationService.details(id);
    }

    @GetMapping("/getLocationAddress/{id}")
    @ApiOperation(value = "场站地址查询", notes = "场站地址查询")
    public Result<OpLocationAddressDTO> getLocationAddress(@PathVariable("id") Long id) {
        return opLocationService.getLocationAddress(id);
    }

    @PostMapping("/existsByGroupIdIn")
    @ApiOperation(value = "根据组织机构id判断场站是否存在", notes = "根据组织机构id判断场站是否存在")
    public Result<Boolean> existsByGroupIdIn(@RequestBody List<Long> groupIds) {
        return opLocationService.existsByGroupIdIn(groupIds);
    }

    @PostMapping("/getLocationList")
    @ApiOperation(value = "根据组织机构id查询场站", notes = "根据组织机构id判断场站是否存在")
    public Result<List<OpLocationOperationDTO>> getLocationList(@RequestBody List<String> groupIdList) {
        return opLocationService.getLocationList(groupIdList);
    }

    @PostMapping("/downLocationXls")
    @ApiOperation(value = "场站导入模板下载", notes = "场站导入模板下载")
    public void downLocationXls(HttpServletRequest request, HttpServletResponse response) {
        opLocationService.downLocationXls(request, response);
    }

    @PostMapping("/getStationMapInfo")
    @ApiOperation(value = "查询场站地图信息", notes = "查询场站地图信息")
    public Result<List<OpLocationDTO>> getStationMapInfo() {
        return opLocationService.getStationMapInfo();
    }

    @PostMapping("/importLocations")
    @ApiOperation(value = "批量导入", notes = "批量导入")
    public Result<Boolean> importLocations(@RequestParam("file") MultipartFile file) {
        return opLocationService.importLocations(file);
    }

    @OperationActionLog(action = "query", object = "location")
    @PostMapping("/page")
    @ApiOperation(value = "5月新版场站列表", notes = "5月新版场站列表")
    public Result<Page<OpLocationPageVO>> page(@Valid @RequestBody OpLocationPageDTO opLocationPageDTO) {
        return opLocationService.page(opLocationPageDTO);
    }

    @GetMapping("/pileAndEvse/{id}")
    @ApiOperation(value = "统计桩和枪的数量、功率、类型、状态", notes = "统计桩和枪的数量、功率、类型、状态")
    public Result<OpStatisticsPileAndEvseVO> pileAndEvse(@PathVariable("id") Long id) {
        return opLocationService.pileAndEvse(id);
    }

    @OperationActionLog(action = "query", object = "location")
    @GetMapping("/detail/{id}")
    @ApiOperation(value = "5月新版场站详情查询", notes = "5月新版场站详情查询")
    public com.autel.cloud.base.model.Result<OpLocationDetailVO> detail(@PathVariable(value = "id", name = "id") Long id) {
        return opLocationService.detail(id);
    }

    @GetMapping("/stationGunStatusGroupCount/{id}")
    @ApiOperation(value = "场站不同枪状态统计", notes = "场站不同枪状态统计")
    public Result<List<GunStatusGroupVO>> stationGunStatusGroupCount(@PathVariable(value = "id", name = "id") Long id) {
        return opLocationService.stationGunStatusGroupVO(id);
    }

    @PostMapping("/StatusByEvseSn")
    @ApiOperation(value = "根据桩sn查询桩状态")
    @Deprecated
    Result<List<EvseSnStatusVO>> gunStatusByEvseSn(@RequestBody List<String> EvseSnList) {
        return opLocationService.gunStatusByEvseSn(EvseSnList);
    }

    @PostMapping("/sortPile")
    @ApiOperation(value = "场站列表对桩集合进行排序", notes = "场站列表对桩集合进行排序")
    Result<List<PilePageVO>> sortPile(@RequestBody PileSortDTO pileSortDTO) {
        return opLocationService.sortPile(pileSortDTO);
    }

    @PostMapping("/mapQuery")
    @ApiOperation(value = "APP地图搜索场站", notes = "APP地图搜索场站")
    public Result<List<OpLocationMapQueryVO>> mapQuery(@RequestBody OpLocationMapQueryDTO opLocationMapQueryDTO) {
        return opLocationService.mapQuery(opLocationMapQueryDTO);
    }

    @PostMapping("/mapQueryPile")
    @ApiOperation(value = "地图找桩支持搜索", notes = "地图找桩支持搜索")
    public Result<Page<OpLocationMapQueryVO>> mapQueryPile(@RequestBody OpLocationMapQueryPageDTO opLocationMapQueryPageDTO) {
        return opLocationService.mapQueryPile(opLocationMapQueryPageDTO);
    }

    @PostMapping("/mapQueryPileForOCPI")
    @ApiOperation(value = "地图找桩支持搜索", notes = "地图找桩支持搜索")
    public Result<Page<OpLocationMapQueryVO>> mapQueryPileForOCPI(@RequestBody OpLocationMapQueryPageDTO opLocationMapQueryPageDTO) {
        return opLocationService.mapQueryPileForOCPI(opLocationMapQueryPageDTO);
    }

    @PostMapping("/cardData")
    @ApiOperation(value = "APP场站卡片", notes = "APP场站卡片")
    public Result<OpLocationCardVO> stationCardData(@RequestBody @Valid OpLocationCardDTO opLocationCardDTO) {
        return opLocationService.stationCardData(opLocationCardDTO);
    }

    @PostMapping("/recommend")
    @ApiOperation(value = "APP场站推荐", notes = "APP场站推荐")
    public Result<List<OpLocationCardVO>> recommend(@RequestBody @Valid OpLocationMapQueryDTO opLocationMapQueryDTO) {
        return opLocationService.recommend(opLocationMapQueryDTO);
    }

    @PostMapping("/appDetail")
    @ApiOperation(value = "APP充电站详情", notes = "APP充电站详情")
    public Result<OpLocationAPPDetailVO> appDetail(@Valid @RequestBody OpLocationDetailDTO opLocationDetailDTO) {
        log.info("opLocationDetailDTO:{}", JSON.toJSONString(opLocationDetailDTO));

        Result<OpLocationAPPDetailVO> result = opLocationService.appDetail(opLocationDetailDTO);

        log.info("===========>>>>>>>>>> OpLocationController.appDetail result : {}", JSON.toJSONString(result));

        return result;
    }

    @GetMapping("/h5PileDetail")
    @ApiOperation(value = "APP充电站详情", notes = "APP充电站详情")
    public Result<H5PileDetailVO> h5PileDetail(@RequestParam("sn") String sn,
                                               @RequestParam(value = "encryptUserId", required = false) String encryptUserId) {
        return Result.ofSucceed(opLocationService.h5PileDetail(sn, encryptUserId));
    }

    @GetMapping("/appPileDetail")
    @ApiOperation(value = "APP充电站详情", notes = "APP充电站详情")
    public Result<AppPileDetailVO> appPileDetail(@RequestParam("sn") String sn) {
        return Result.ofSucceed(opLocationService.appPileDetail(sn));
    }

    @PostMapping("/appDetailForOCPI")
    @ApiOperation(value = "APP充电站详情", notes = "APP充电站详情")
    public Result<OpLocationAPPDetailVO> appDetailForOCPI(@Valid @RequestBody OpLocationDetailDTO opLocationDetailDTO) {
        log.info("opLocationDetailDTO:{}", JSON.toJSONString(opLocationDetailDTO));

        Result<OpLocationAPPDetailVO> result = opLocationService.appDetailForOCPI(opLocationDetailDTO);

        log.info("===========>>>>>>>>>> OpLocationController.appDetail result : {}", JSON.toJSONString(result));

        return result;
    }

    @PostMapping("/deleteOCPIEMSPData")
    @ApiOperation(value = "删除ocpi-emsp场站相关数据", notes = "删除ocpi-emsp场站相关数据")
    public Result<Integer> deleteOCPIEMSPData(@RequestBody List<Long> operatorIdList) {
        log.info("operatorIdList:{}", JSONArray.toJSONString(operatorIdList));
        return opLocationService.deleteOCPIEMSPData(operatorIdList);
    }

    @PostMapping("/getGunListByStationId")
    @ApiOperation(value = "APP根据站点ID获取枪列表")
    public Result<Page<GunListPageVO>> getGunListByStationId(@Valid @RequestBody GunListPageDTO gunListPageDTO) {
        return opLocationService.getGunListByStationId(gunListPageDTO);
    }

    @PostMapping("/synchronizationData")
    @ApiOperation(value = "将数据库信息全部同步到es")
    public Result<Integer> synchronizationData(@RequestBody List<Long> sellerIds) {
        return Result.ofSucceed(opLocationService.synchronizationData(sellerIds));
    }

    @PostMapping("/synchronizationOnlyData")
    @ApiOperation(value = "将数据库信息全部同步到es")
    public Result<Boolean> synchronizationOnlyData() {
        return opLocationService.synchronizationOnlyData();
    }

    @PostMapping("/synchronizationLocation/{id}")
    @ApiOperation(value = "将场站数据库信息全部同步到es")
    public Result<Boolean> synchronizationLocation(@PathVariable("id") Long id) {
        return opLocationService.synchronizationLocation(id);
    }

    @PostMapping("/synchronizationLocations")
    @ApiOperation(value = "将场站数据库信息全部同步到es")
    public Result<Boolean> synchronizationLocation(@RequestBody List<Long> locationIds) {
        for (Long id : locationIds) {
            opLocationService.synchronizationLocation(id);
            opLocationEvseService.syncEvseExpand(EvseExpandDTO.builder().locationIds(locationIds).build());
        }
        return Result.ofSucceed();
    }

    @PostMapping("/syncField")
    @ApiOperation(value = "从数据库同步场站指定字段到es")
    public Result<Integer> syncField() {
        return Result.ofSucceed(this.opLocationService.syncField());
    }


    @PostMapping("/syncZoneId")
    @ApiOperation(value = "场站历史数据批量更新")
    public Result<Boolean> syncZoneId(@RequestBody List<Long> locationIds) {
        return opLocationService.syncZoneId(locationIds);
    }

    @GetMapping("/findAll")
    @ApiOperation(value = "全部场站查询")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public Result<List<OpLocationEntity>> findAll() {
        return Result.ofSucceed(opLocationService.findAll());
    }

    @JsonInclude(JsonInclude.Include.NON_NULL)
    @PostMapping("/populationZoneId")
    @ApiOperation(value = "本地环境调用Google填充zoneId")
    public List<OpLocationEntity> populationZoneId(@RequestBody List<OpLocationEntity> list) {
        return opLocationService.populationZoneId(list);
    }

    @PostMapping("/saveZoneId")
    @ApiOperation(value = "本环境调用保存")
    @JsonInclude(JsonInclude.Include.NON_NULL)
    public List<OpLocationEntity> saveZoneId(@RequestBody List<OpLocationEntity> list) {
        return opLocationService.saveZoneId(list);
    }

    @GetMapping("/getDetailsFromEsById/{id}")
    @ApiOperation(value = "通过LocationId查询es中的数据", notes = "通过LocationId查询es中的数据")
    public Result<OpLocationDTO> getDetailsFromEsById(@PathVariable Long id) {
        return opLocationService.getDetailsFromEsById(id);
    }

    @GetMapping("/getLocationBySellerId")
    @ApiOperation(value = "根据商家获取场站", notes = "根据商家获取场站")
    public Result<List<OpLocationDTO>> getLocationBySellerId() {
        return opLocationService.getLocationBySellerId();
    }

    @GetMapping("/getLocationIdBySellerId")
    @ApiOperation(value = "根据商家获取场站id", notes = "根据商家获取场站id")
    public Result<List<Long>> getLocationIdBySellerId() {
        return opLocationService.getLocationIdBySellerId();
    }

    @GetMapping("/getLocationByPileSn")
    @ApiOperation(value = "根据pileSn查询场站", notes = "根据pileSn查询场站")
    public Result<OpLocationDTO> getLocationByPileSn(@RequestParam("pileSn") String pileSn) {
        return opLocationService.getLocationByPileSn(pileSn);
    }

    @PostMapping("/getLocationByPileSnList")
    @ApiOperation(value = "根据pileSnList查询场站", notes = "根据pileSnList查询场站")
    public Result<List<OpLocationListDTO>> getLocationByPileSnList(@RequestBody List<String> pileSnList) {
        if (pileSnList.isEmpty()) {
            return Result.ofSucceed();
            //return CompetenceUtil.encapsulation(new ArrayList<>(),new ArrayList<>());
        }
        return opLocationService.getLocationByPileSnList(pileSnList);
    }

    @PostMapping("/updateAnnouncement")
    @ApiOperation(value = "更新场站公告", notes = "更新场站公告")
    public Result<Void> updateAnnouncement(@RequestBody OpLocationAnnouncementUpdateDTO opLocationAnnouncementUpdateDTO) {
        return opLocationService.updateAnnouncement(opLocationAnnouncementUpdateDTO);
    }

    @PostMapping("/infoByGroupId")
    @ApiOperation(value = "根据组织机构id获取场站信息", notes = "根据组织机构id获取场站信息")
    public Result<List<OpLocationInfoVO>> getLocationByGroupId(@RequestBody List<Long> groupIdList) {
        return opLocationService.getLocationByGroupId(groupIdList);
    }

    @GetMapping("/getTimeZoneList")
    @ApiOperation(value = "获取时区列表", notes = "获取时区列表")
    public Result<List<TimeZoneVO>> getTimeZoneList() {
        return opLocationService.getTimeZoneList();
    }

    @GetMapping("/updateLocationUpdateTime/{locationId}")
    @ApiOperation(value = "刷新场站更新时间", notes = "刷新场站更新时间")
    Result<Boolean> updateLocationUpdateTime(@PathVariable("locationId") Long locationId) {
        return opLocationService.updateLocationUpdateTime(locationId);
    }

    @GetMapping("/refreshGroupName")
    @ApiOperation(value = "刷新组织机构名称", notes = "刷新组织机构名称")
    Result<Void> refreshGroupName() {
        return opLocationService.refreshGroupName();
    }

    @GetMapping("/updateGroupName")
    @ApiOperation(value = "更新组织机构名称", notes = "更新组织机构名称")
    Result<Void> updateGroupName(@RequestParam("groupId") Long groupId, @RequestParam("groupName") String groupName) {
        return opLocationService.updateGroupName(groupId, groupName);
    }

    @GetMapping("/initializeOpLocationProvinceToES")
    @ApiOperation(value = "初始化ES中的省份字段信息", notes = "初始化ES中的省份字段信息")
    public Result<Boolean> initializeOpLocationProvinceToES() {
        return opLocationService.initializeOpLocationProvinceToES();
    }

    @PostMapping("/reindex")
    @ApiOperation(value = "同步迁移数据")
    public BulkByScrollResponse reindex(@RequestParam("sourceIndices") String[] sourceIndices,
                                        @RequestParam("destIndex") String destIndex) throws IOException {
        try {
            ReindexRequest request = new ReindexRequest();
            request.setSourceIndices(sourceIndices);
            request.setDestIndex(destIndex);
            request.setDestOpType("index");
            request.setMaxRetries(1);
            request.setSourceBatchSize(30);
            BulkByScrollResponse bulkResponse = restHighLevelClient.reindex(request, RequestOptions.DEFAULT);
            //输出
            TimeValue timeTaken = bulkResponse.getTook();
            boolean timedOut = bulkResponse.isTimedOut();
            long totalDocs = bulkResponse.getTotal();
            long updatedDocs = bulkResponse.getUpdated();
            long createdDocs = bulkResponse.getCreated();
            long deletedDocs = bulkResponse.getDeleted();
            long batches = bulkResponse.getBatches();
            long noops = bulkResponse.getNoops();
            long versionConflicts = bulkResponse.getVersionConflicts();
            long bulkRetries = bulkResponse.getBulkRetries();
            long searchRetries = bulkResponse.getSearchRetries();
            TimeValue throttledMillis = bulkResponse.getStatus().getThrottled();
            TimeValue throttledUntilMillis =
                    bulkResponse.getStatus().getThrottledUntil();
            List<ScrollableHitSource.SearchFailure> searchFailures =
                    bulkResponse.getSearchFailures();
            List<BulkItemResponse.Failure> bulkFailures = bulkResponse.getBulkFailures();
            return bulkResponse;
        } catch (ElasticsearchException | IOException exception) {
            log.error("reIndex failed", exception);
            throw exception;
        }
    }

    @GetMapping(value = "/activeFile/download")
    @ApiOperation(value = "三方桩激活文档下载", notes = "三方桩激活文档下载")
    public Result<Void> queryTariffByPileSN(HttpServletResponse response, @RequestParam(value = "brandName", required = false) String brandName) {
        return opLocationService.download(response, brandName);
    }

    @OperationActionLog(action = "query", object = "location")
    @PostMapping("/searchInfo")
    @ApiOperation(value = "搜索组织、场站、桩", notes = "搜索组织、场站、桩")
    Result<SearchItemInfoVO> searchInfo(@RequestBody SearchItemInfoDTO searchItemInfoDTO) {
        return opLocationService.searchInfo(searchItemInfoDTO);
    }

    @PostMapping("/searchByPage")
    @ApiOperation(value = "根据组织、场站、桩的id搜索场站", notes = "根据组织、场站、桩的id搜索场站")
    Page<OpLocationPageVO> searchByPage(@Valid @RequestBody OpLocationPageDTO opLocationPageDTO) {
        return opLocationService.searchByPage(opLocationPageDTO);
    }

    @PostMapping("/searchByPage/v2")
    @ApiOperation(value = "根据组织id，场站名称，桩名称搜索场站-V2接口", notes = "根据组织id,场站名称，桩名称搜索")
    public com.autel.cloud.base.model.Result<Page<OpLocationPageVO>> searchByPageV2(@Valid @RequestBody OpLocationPageDTO opLocationPageDTO) {
        return opLocationService.searchByPageV2(opLocationPageDTO);
    }

    @GetMapping(value = "/getSellerIdList")
    @ApiOperation(value = "获取商家ID列表", notes = "获取商家ID列表")
    public Result<List<String>> getSellerIdList(@RequestParam("locationName") String locationName) {
        return Result.ofSucceed(opLocationService.getSellerIdList(locationName));
    }

    @PostMapping("/getDetails")
    @ApiOperation(value = "批量查询时区", notes = "批量查询时区")
    Result<List<OpLocationDTO>> getDetails(@RequestBody List<String> pileSnList) {
        return Result.ofSucceed(opLocationService.getDetails(pileSnList));
    }

    @PostMapping("/searchByPageForAlarm")
    @ApiOperation(value = "feign调用查询场站所有sn", notes = "feign调用查询场站所有sn")
    Result<Page<OpLocationPageVO>> searchByPageForAlarm(@Valid @RequestBody OpLocationPageDTO opLocationPageDTO) {
        return Result.ofSucceed(opLocationService.searchByPage(opLocationPageDTO));
    }

    @GetMapping("/syncOpLocationESPlatform")
    @ApiOperation(value = "添加平台字段到场站和枪ES中", notes = "添加平台字段到场站和枪ES中")
    public Result<Boolean> syncOpLocationESPlatform() {
        return Result.ofSucceed(opLocationService.syncOpLocationESPlatform());
    }

    @GetMapping("/updateOpLocationESPlatform")
    @ApiOperation(value = "修改平台字段到场站和枪ES中", notes = "添加平台字段到场站和枪ES中")
    public Result<Boolean> updateOpLocationESPlatform() {
        return Result.ofSucceed(opLocationService.updateOpLocationESPlatform());
    }

    @GetMapping("/detailById/{id}")
    @ApiOperation(value = "通过场站id查询所属组织id", notes = "通过场站id查询所属组织id")
    public Result<Long> detailById(@PathVariable(value = "id", name = "id") Long id) {
        return opLocationService.detailById(id);
    }

    @ApiOperation(value = "app地图聚合", notes = "app地图聚合")
    @PostMapping("/mapAgg")
    public Result<AggMapVO> mapAgg(@RequestBody OpLocationMapQueryDTO opLocationMapQueryDTO) {
        return opLocationService.mapAgg(opLocationMapQueryDTO);
    }

    @ApiOperation(value = "app地图聚合", notes = "app地图聚合")
    @PostMapping("/mapAggForOCPI")
    public Result<AggMapVO> mapAggForOCPI(@RequestBody OpLocationMapQueryDTO opLocationMapQueryDTO) {
        return opLocationService.mapAggForOCPI(opLocationMapQueryDTO);
    }

    @GetMapping(value = "/getZonIdByEvseSn")
    @ApiOperation(value = "根据evse_sn获取其所在场站的ZoneId", notes = "根据evse_sn获取其所在场站的ZoneId")
    public Result<String> getZonIdByEvseSn(@RequestParam("EvseSn") String evseSn) {
        return Result.ofSucceed(opLocationService.getZonIdByEvseSn(evseSn));
    }

    @PostMapping(value = "/getEvseInfo")
    @ApiOperation(value = "查询桩和枪的类型", notes = "查询桩和枪的类型")
    public Result<OpLocationEvseInfoDTO> getEvseInfo(@RequestParam("evseSn") String evseSn) {
        return Result.ofSucceed(opLocationService.getEvseInfo(evseSn));
    }

    /**
     * @param id 场站id
     * @return 场站信息（包含场站税费配置信息等）
     * @function 获取场站信息（包含场站税费配置信息等）
     */
    @GetMapping(value = "/queryLocationInfo")
    @ApiOperation(value = "获取场站信息（包含场站税费配置信息等）", notes = "获取场站信息（包含场站税费配置信息等）")
    public Result<OpLocationDTO> queryLocationInfo(@RequestParam("id") Long id) {

        log.info("===========>>>>>>>>>> OpLocationController.queryLocationInfo id : {}", JSON.toJSONString(id));

        return opLocationService.getDetailsFromEsById(id);
    }

    /**
     * @return 迁移结果
     * @function 税费重构——计费规则的税率迁移
     */
    @GetMapping("/taxRateMigrate")
    @ApiOperation(value = "税费重构——计费规则的税率迁移", notes = "税费重构——计费规则的税率迁移")
    public Result<Boolean> taxRateMigrate() {

        log.info("====>>>>税费重构——计费规则的税率迁移 开始！");

        return Result.ofSucceed(opLocationService.taxRateMigrate());
    }

    /**
     * @param locationSimpleInfoQueryDTO 场站信息的简单查询实体 入参模型
     * @return 场站信息
     * @function 场站信息的简单查询
     */
    @RequestMapping(method = {RequestMethod.POST}, value = "/locationSimpleInfoQuery")
    @ApiOperation(value = "场站信息的简单查询", notes = "场站信息的简单查询")
    public Result<List<LocationSimpleInfoQueryVO>> locationSimpleInfoQuery(@RequestBody LocationSimpleInfoQueryDTO locationSimpleInfoQueryDTO) {

        log.info("===>>>OpLocationController.locationSimpleInfoQuery locationSimpleInfoQueryDTO : {}", JSON.toJSONString(locationSimpleInfoQueryDTO));

        return Result.ofSucceed(opLocationService.locationSimpleInfoQuery(locationSimpleInfoQueryDTO));
    }

    @GetMapping("/getSellerInfoAndLocationInfo")
    @ApiOperation(value = "根据sn获取商家信息和场站信息")
    public Result<GetSellerInfoAndLocationInfoVO> getSellerInfoAndLocationInfo(@RequestParam("pileSn") String pileSn) {
        return Result.ofSucceed(opLocationService.getSellerInfoAndLocationInfo(pileSn));
    }


    @GetMapping("/getLocationRoamingByEvseSn")
    @ApiOperation(value = "按evseSn查询场站漫游数据", notes = "按evseSn查询场站漫游数据")
    public Result<LocationRoamingVO> getLocationRoamingByEvseSn(@RequestParam("evseSn") String evseSn) {
        return Result.ofSucceed(opLocationService.getLocationRoamingByEvseSn(evseSn));
    }

    @PostMapping("/update/power")
    @ApiOperation(value = "修改功率")
    public Result<Boolean> updatePower(@RequestBody UpdatePowerDTO updatePowerDTO) {
        return Result.ofSucceed(opLocationService.updatePower(updatePowerDTO));
    }

    /**
     * @param pileSimpleInfoQueryDTO
     * @return
     * @function 获得场站基础信息（供运维使用）
     */
    @RequestMapping(method = {RequestMethod.POST}, value = "/getLocationBasicInfoVOPage")
    @ApiOperation(value = "获得场站基础信息（供运维使用）", notes = "获得场站基础信息（供运维使用）")
    public Result<Page<LocationBasicInfoVO>> getLocationBasicInfoVOPage(@RequestBody SearchDTO pileSimpleInfoQueryDTO) {

        log.info("===>>> OpLocationController.getLocationBasicInfoVOPage pileSimpleInfoQueryDTO : {}",
                JSON.toJSONString(pileSimpleInfoQueryDTO));

        return Result.ofSucceed(opLocationService.getLocationBasicInfoVOPage(pileSimpleInfoQueryDTO));
    }

    /**
     * @param sellerIdList
     * @return
     * @function 批量获得商家与该商家所拥有的所有的场站信息之间的映射关系 （供运维使用）
     */
    @PostMapping("/getLocationBasicInfoPackageVOBySellerIdList")
    @ApiOperation(value = "批量获得商家与该商家所拥有的所有的场站信息之间的映射关系 （供运维使用）", notes = "批量获得商家与该商家所拥有的所有的场站信息之间的映射关系 （供运维使用）")
    public Result<LocationBasicInfoPackageVO> getLocationBasicInfoPackageVOBySellerIdList(@RequestBody List<String> sellerIdList) {

        log.info("===>>>OpLocationController.getLocationBasicInfoPackageVOBySellerIdList sellerIdList : {}", JSON.toJSONString(sellerIdList));

        return Result.ofSucceed(opLocationService.getLocationBasicInfoPackageVOBySellerIdList(sellerIdList));
    }

    /**
     * @param pileSnList
     * @return
     * @function 根据传入的充电桩序列号集合，查询这些充电桩的信息及其所在的场站信息，并以商家id为映射条件返回这些信息（供运维使用）
     */
    @PostMapping("/getLocationBasicInfoPackageVOByPileSnList")
    @ApiOperation(value = "根据传入的充电桩序列号集合，查询这些充电桩的信息及其所在的场站信息，并以商家id为映射条件返回这些信息（供运维使用）", notes = "根据传入的充电桩序列号集合，查询这些充电桩的信息及其所在的场站信息，并以商家id为映射条件返回这些信息（供运维使用）")
    public Result<LocationBasicInfoPackageVO> getLocationBasicInfoPackageVOByPileSnList(@RequestBody List<String> pileSnList) {

        log.info("===>>>OpLocationController.getLocationBasicInfoPackageVOByPileSnList pileSnList : {}", JSON.toJSONString(pileSnList));

        return Result.ofSucceed(opLocationService.getLocationBasicInfoPackageVOByPileSnList(pileSnList));
    }

    @PutMapping("/supportERoaming/{locationId}")
    @ApiOperation(value = "修改是否希望场站支持互联互通")
    public Result<LocationSimpleInfoQueryVO> setLikeRoaming(@PathVariable("locationId") Long locationId){
        return Result.ofSucceed(opLocationService.setLikeRoaming(locationId));
    }

    @PostMapping("/getLocationSimplyInfoByLocationId")
    @ApiOperation(value = "通过场站id获取场站信息")
    public Result<LocationInfoVO> getSimplyLocationInfoByLocationId(@RequestBody LocationInfoDTO locationInfoDTO) {
        return Result.ofSucceed(opLocationService.getSimplyLocationInfoByLocationId(locationInfoDTO));
    }

    @PutMapping("/ocpiPutLocation")
    @ApiOperation(value = "ocpi添加场站")
    public Result ocpiPutLocation(@RequestBody PutLocationToAutelDTO putLocationToAutelDTO) {
        log.info("====ocpiPutLocation:{}", JSONObject.toJSONString(putLocationToAutelDTO));
        return Result.ofSucceed(opLocationService.ocpiPutLocation(putLocationToAutelDTO.getLocationId(),putLocationToAutelDTO.getTariffIdMap(),putLocationToAutelDTO.getLocationVO()));
    }

    @GetMapping("/prePaymentEnabled")
    @ApiOperation(value = "是否支持预支付", notes = "是否支持预支付")
    public Result<Boolean> prePaymentEnabled(@RequestParam("locationId") Long locationId){
        return Result.ofSucceed(opLocationService.prePaymentEnabled(locationId));
    }

    @GetMapping("/appPrePayment")
    @ApiOperation(value = "APP预支付信息", notes = "APP预支付信息")
    public Result<AppPrePaymentVO> appPrePaymentVO(@RequestParam("locationId") Long locationId){
        return Result.ofSucceed(opLocationService.appPrePaymentVO(locationId));
    }

    @PostMapping("/locationListByOperatorId")
    @ApiOperation("根据商家id和关键字查询场站列表")
    public Result<List<LocationListVO>> locationListByOperatorId(@RequestBody LocationListDTO locationListDTO) {
        return Result.ofSucceed(opLocationService.locationListByOperatorId(locationListDTO));
    }

    @GetMapping("/getLocationIdSellerMap")
    @ApiOperation(value = "查询所有商家和场站关系",notes = "基础服务权限树维护调用")
    public Result<Map<String, List<CommonVO>>> getLocationIdSellerMap() {
        return Result.ofSucceed(opLocationService.getLocationIdSellerMap());
    }

    @PostMapping("/querySubTree")
    @ApiOperation(value = "查询某个节点的子树数据")
    public Result<List<NodeVO>> querySubTree(@RequestBody QuerySubTreeDTO querySubTreeDTO) {
        return Result.ofSucceed(opLocationService.querySubTree(querySubTreeDTO));
    }

    @PostMapping("/getList")
    @ApiOperation(value = "批量查询场站名称", notes = "批量查询场站名称")
    public Result<List<OpLocationDTO>> getList(@RequestBody @Max(value = 500) List<Long> ids){
        return Result.ofSucceed(opLocationService.getList(ids));
    }

    @PostMapping("/selectLocationForFleet")
    @ApiOperation(value = "为车队查询所有场站位置信息", notes = "为车队查询所有场站位置信息")
    public Result<List<SelectLocationForFleetVO>> selectLocationForFleet(@RequestBody SelectLocationForFleetDTO selectLocationForFleetDTO) {

        log.info("===>>> OpLocationController.selectLocationForFleet selectLocationForFleetDTO : {}",
                JSON.toJSONString(selectLocationForFleetDTO));

        return Result.ofSucceed(opLocationService.selectLocationForFleet(selectLocationForFleetDTO));
    }

    @GetMapping("/getMaxPowerLocationId")
    @ApiOperation(value = "根据商户id获取功率最高的场站id", notes = "根据商户id获取功率最高的场站id")
    public Result<Long> getMaxPowerLocationId(@RequestParam("sellerId") Long sellerId) {

        log.info("===>>> OpLocationController.getMaxPowerLocationId sellerId : {}",
                JSON.toJSONString(sellerId));

        return Result.ofSucceed(opLocationService.getMaxPowerLocationId(sellerId));
    }

    @ApiOperation("为互联互通查询场站信息(分页)")
    @PostMapping("/selectLocationInfoForEroaming")
    public Result<Page<SelectLocationInfoForEroamingVO>> selectLocationInfoForEroaming(@RequestBody PageDTO pageDTO) {

        log.info("===>>>OpLocationController.selectLocationInfoForEroaming pageDTO : {}",
                JSON.toJSONString(pageDTO));

        return Result.ofSucceed(opLocationService.selectLocationInfoForEroaming(pageDTO));
    }

    @PostMapping("/setLocationTypeForLocation")
    @ApiOperation(value = "为场站设置位置类型属性")
    public Result<Boolean> setLocationTypeForLocation(@RequestBody SetLocationTypeForLocationDTO setLocationTypeForLocationDTO) {

        log.info("===>>> OpLocationController.setLocationTypeForLocation setLocationTypeForLocationDTO : {}",
                JSON.toJSONString(setLocationTypeForLocationDTO));

        return Result.ofSucceed(opLocationService.setLocationTypeForLocation(setLocationTypeForLocationDTO));
    }

    @PostMapping("/setLocationEroamingForLocation")
    @ApiOperation(value = "为场站(开启或者关闭)互联互通")
    public Result<Boolean> setLocationEroamingForLocation(@RequestBody SetLocationEroamingForLocationDTO setLocationEroamingForLocationDTO) {

        log.info("===>>> OpLocationController.setLocationEroamingForLocation setLocationEroamingForLocationDTO : {}",
                JSON.toJSONString(setLocationEroamingForLocationDTO));

        return Result.ofSucceed(opLocationService.setLocationEroamingForLocation(setLocationEroamingForLocationDTO));
    }

    @PostMapping("/batchGetSellerEroamingLocationFlag")
    @ApiOperation(value = "判断商户下是否有开启了互联互通的场站(批量)")
    public Result<Map<Long, Boolean>> batchGetSellerEroamingLocationFlag(@RequestBody List<Long> sellerIdList) {

        log.info("===>>> OpLocationController.batchGetSellerEroamingLocationFlag sellerIdList : {}",
                JSON.toJSONString(sellerIdList));

        return Result.ofSucceed(opLocationService.batchGetSellerEroamingLocationFlag(sellerIdList));
    }

    @ApiOperation("场站列表(下拉分页列表)")
    @PostMapping("/getLocationDropDownPage")
    public Result<Page<LocationBaseVO>> getLocationDropDownPage(@RequestBody OpLocationMenuQueryDto pageDTO) {

        log.info("===>>>OpLocationController.getLocationDropDownPage pageDTO : {}",
                JSON.toJSONString(pageDTO));

        return Result.ofSucceed(opLocationService.getLocationDropDownPage(pageDTO));
    }

    @GetMapping("/getOperatorSupportInfo")
    @ApiOperation(value = "获取商家求助支持信息", notes = "获取商家求助支持信息")
    public Result<OperatorSupportInfoVO> getOperatorSupportInfo(@RequestParam("sn") String sn){
        return Result.ofSucceed(opLocationService.getOperatorSupportInfo(sn));
    }


    @GetMapping("/getLocationIdBySn")
    @ApiOperation(value = "按sn获取场站ID", notes = "按sn获取场站ID")
    public Result<String> getLocationIdBySn(@RequestParam("sn") String sn){
        return Result.ofSucceed(opLocationService.getLocationIdBySn(sn));
    }


    @GetMapping("/getLocationData")
    @ApiOperation(value = "查询场站信息(获取可见场站的额定功率)", notes = "查询场站信息(获取可见场站的额定功率)")
    Result<List<LocationDataVO>> getLocationData(@RequestParam("userId") Long userId) {

        log.info("===>>> OpLocationController.getLocationData userId : {}",
                JSON.toJSONString(userId));

        return Result.ofSucceed(opLocationService.getLocationData(userId));
    }

    @GetMapping("/getLocationId")
    @ApiOperation(value = "根据商家id,查询场站信息", notes = "根据商家id,查询场站信息")
    Result<List<Long>> getLocationIdBySellerId(@RequestParam("sellerId") Long sellerId) {
        log.info("===>>> OpLocationController.getLocationIdBySellerId sellerId : {}",JSON.toJSONString(sellerId));
        return Result.ofSucceed(opLocationService.getLocationIdBySellerId(sellerId));
    }

}

