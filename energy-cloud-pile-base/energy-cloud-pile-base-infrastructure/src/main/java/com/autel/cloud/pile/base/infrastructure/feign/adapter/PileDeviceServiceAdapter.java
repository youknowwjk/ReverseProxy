package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.device.dto.GunTypeAndNumVO;
import com.autel.cloud.device.feign.DeviceClient;
import com.autel.cloud.device.vo.PileDeviceRelationVO;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.chargepoint.GetDeviceTypeDTO;
import com.autel.cloud.pile.base.enums.chargepoint.DeviceTypeEnum;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.SyncPileInfoForPosDTO;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class PileDeviceServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private DeviceServiceFeign deviceServiceClient;

    @Resource
    private DeviceClient deviceClient;

    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;


    private final LoadingCache<String, ChargePileDTO> pileDetailCache = CacheBuilder.newBuilder()
            .expireAfterWrite(8, TimeUnit.HOURS)
            .initialCapacity(10)
            .maximumSize(100)
            .build(new CacheLoader<String, ChargePileDTO>() {
                @Override
                public ChargePileDTO load(String id) {
                    return loadPileDetail(id);
                }
            });

    public GunTypeAndNumVO getGunTypeAndNumVO(String sn) {
        try {
            log.info("getGunTypeAndNumVO: {}", sn);
            Result<GunTypeAndNumVO> gunTypeAndNum = deviceClient.getGunTypeAndNum(sn);
            log.info("getGunTypeAndNumVO result: {}", JSON.toJSONString(gunTypeAndNum));
            GunTypeAndNumVO handle = nullableHandle(gunTypeAndNum);
            return Optional.ofNullable(handle).orElse(new GunTypeAndNumVO());
        } catch (Exception e) {
            log.error("getGunTypeAndNumVO,调用device查询枪类型异常：", e);
        }
        return new GunTypeAndNumVO();
    }

    public Boolean saveThirdPile(OpLocationEvseDTO opLocationEvseDTO) {
        log.info("saveThirdPile {}", JSON.toJSONString(opLocationEvseDTO));
        Result<Boolean> booleanResult = deviceServiceClient.saveThirdPile(opLocationEvseDTO);
        log.info("saveThirdPile result {}", JSON.toJSONString(booleanResult));
        return nullableHandle(booleanResult);
    }
    public Boolean batchSaveThirdPile(List<OpLocationEvseDTO>  opLocationEvseDTO) {
        log.info("batchSaveThirdPile {}", JSON.toJSONString(opLocationEvseDTO));
        if (CollectionUtils.isEmpty(opLocationEvseDTO)) {
            return Boolean.TRUE;
        }

        try {
            Result<Boolean> booleanResult = deviceServiceClient.batchSaveThirdPile(opLocationEvseDTO);
            log.info("batchSaveThirdPile result {}", JSON.toJSONString(booleanResult));
            return nullableHandle(booleanResult);
        } catch (Exception e) {
            log.error("batchSaveThirdPile failed", e);
            throw e;
        }
    }

    /**
     * 获取充电桩发货时间
     */
    public String getTimeOfDelivery(String sn) {
        log.info("getTimeOfDelivery {}", sn);
        Result<String> timeOfDelivery = deviceServiceClient.getTimeOfDelivery(sn);
        log.info("getTimeOfDelivery result {}", JSON.toJSONString(timeOfDelivery));
        return Optional.ofNullable(nullableHandle(timeOfDelivery)).orElse("--");
    }

    public List<com.autel.cloud.device.dto.ChargePileDTO> queryPileList(List<String> snList) {
        log.info("queryPileList {}", JSON.toJSONString(snList));
        Result<List<com.autel.cloud.device.dto.ChargePileDTO>> result = deviceClient.queryPileList(snList);
        log.info("queryPileList result {}", JSON.toJSONString(result));
        return Optional.ofNullable(nullableHandle(result)).orElse(Collections.emptyList());
    }

    /**
     * 获取充电桩发货时间
     */
    public String getCountry(String sn) {
        log.info("getCountry {}", sn);
        Result<String> timeOfDelivery = deviceServiceClient.getCountry(sn);
        log.info("getCountry result {}", JSON.toJSONString(timeOfDelivery));
        return Optional.ofNullable(nullableHandle(timeOfDelivery)).orElse("--");
    }


    /**
     * 获取ChargePileInfo
     *
     * @return 场站IDs  至少会有一个不存在的值 防止出错
     */
    public ChargePileDTO getChargePileInfo(String pileSn) {
        try {
            return pileDetailCache.get(pileSn);
        } catch (ExecutionException e) {
            log.error("getLocationIdsByUserId", e);
        }
        return loadPileDetail(pileSn);
    }

    /**
     * 查询最新充电上报信息
     */
    public ChargePileDTO loadPileDetail(String pileSn) {
        log.info("loadPileDetail: {}", pileSn);
        Result<ChargePileDTO> chargePileDTOResult = deviceServiceClient.pileDetail(pileSn);
        log.info("loadPileDetail result: {}", JSON.toJSONString(chargePileDTOResult));
        ChargePileDTO handle = nullableHandle(chargePileDTOResult);
        if (Objects.isNull(handle)) {
            String content = String.format("调用服务(device/pile/detai)没有查询该桩 \n pileSn=%s", pileSn);
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            ChargePileDTO chargePileInfo = new ChargePileDTO();
            chargePileInfo.setPhase(1);// 默认1相电
            return chargePileInfo;
        }
        return handle;
    }

    @ApiOperation(value = "根据序列号查询设备关联关系信息")
    public PileDeviceRelationVO getDeviceRelationInfo(String pileSn) {
        log.info("getDeviceRelationInfo {}", pileSn);
        Result<PileDeviceRelationVO> deviceRelationInfo = deviceServiceClient.getDeviceRelationInfo(pileSn);
        log.info("getDeviceRelationInfo result {}", pileSn);
        PileDeviceRelationVO handle = nullableHandle(deviceRelationInfo);
        return handle;
    }


    @ApiOperation(value = "根据序列号查询设备关联关系信息")
    public String getDeviceMac(String pileSn) {
        log.info("getDeviceMac {}", pileSn);
        PileDeviceRelationVO deviceRelationInfo = getDeviceRelationInfo(pileSn);
        String mac = Optional.ofNullable(deviceRelationInfo).map(PileDeviceRelationVO::getMac).orElse("--");
        log.info("getDeviceMac result pileSn = {} mac = {}", pileSn, mac);
        return mac;

    }

    /**
     * @param getDeviceTypeDTO
     * @return
     * @see com.autel.cloud.pile.base.enums.chargepoint.DeviceTypeEnum
     */
    @ApiOperation(value = "获得设备类型")
    public Integer getDeviceType(GetDeviceTypeDTO getDeviceTypeDTO) {

        log.info("===>>> PileDeviceServiceAdapter.getDeviceType getDeviceTypeDTO : {}",
                JSON.toJSONString(getDeviceTypeDTO));

        List<GetDeviceTypeDTO> getDeviceTypeDTOList = new ArrayList<>();
        getDeviceTypeDTOList.add(getDeviceTypeDTO);

        Result<Map<String, Integer>> result = deviceServiceClient.batchGetDeviceType(getDeviceTypeDTOList);

        log.info("===>>> PileDeviceServiceAdapter.getDeviceType result : {}",
                JSON.toJSONString(result));

        if (result != null
                && ObjectUtils.isNotEmpty(result.getData())) {
            return result.getData().get(getDeviceTypeDTO.getSn());
        }
        return DeviceTypeEnum.OTHER_DEVICE.getCode();
    }

    @ApiOperation(value = "根据一组序列号查询充电桩列表信息")
    public Map<String, ChargePileDTO> getSnAndChargePileDTOMap(List<String> snList) {
        Map<String, ChargePileDTO> snAndChargePileDTOMap = new HashMap<>();
        if (ObjectUtils.isEmpty(snList)) {
            return snAndChargePileDTOMap;
        }

        Result<List<ChargePileDTO>> result = deviceServiceClient.queryPileList(snList);

        log.info("===>>> PileDeviceServiceAdapter.getSnAndChargePileDTOMap result : {}",
                JSON.toJSONString(result));

        if (result != null
                && ObjectUtils.isNotEmpty(result.getData())) {
            result.getData().forEach(val -> snAndChargePileDTOMap.put(val.getSn(), val));
        }
        return snAndChargePileDTOMap;
    }

    @ApiOperation(value = "为POS机同步充电桩信息")
    public Boolean syncPileInfoForPos(SyncPileInfoForPosDTO syncPileInfoForPosDTO) {

        log.info("===>>> PileDeviceServiceAdapter.syncPileInfoForPos syncPileInfoForPosDTO : {}",
                JSON.toJSONString(syncPileInfoForPosDTO));

        Result<Boolean> syncPileInfoForPosResult = deviceServiceClient.syncPileInfoForPos(syncPileInfoForPosDTO);
        if (syncPileInfoForPosResult != null
                && syncPileInfoForPosResult.getData() != null) {
            return syncPileInfoForPosResult.getData();
        }
        return false;
    }
}
