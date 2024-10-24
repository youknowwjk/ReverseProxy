package com.autel.cloud.pile.base.infrastructure.feign.impl;

import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.mgnt.vo.DnmsgLatestValueVO;
import com.autel.cloud.pile.base.config.ops.content.sim.SimCardInfoDTO;
import com.autel.cloud.pile.base.config.ops.content.wifi.WifiInfoDTO;
import com.autel.cloud.pile.base.infrastructure.feign.OpsMgmtClient;
import lombok.extern.slf4j.Slf4j;
import org.assertj.core.util.Lists;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author X20010
 */
@Component
@Slf4j
public class OpsMgntFeignClientProxy {

    private static final String SERVICE_ID_NETWORK_SETTING = "networkSettings";
    private static final String WIFI_KEY = "wifiInfo";
    private static final String WIFI_LIST_KEY = "wifiList";
    private static final String SIM_CARD_LIST_KEY = "simCardList";
    private static final String ETHERNET_ENABLE_KEY = "ethernetEnable";

    @Resource
    private OpsMgmtClient opsMgmtClient;

    public DnmsgLatestValueVO getPileNetworkSetting(String pileSn) {
        try {
            log.info("Ops mgnt getPileNetworkSetting feign request: pileSn = {}", pileSn);
            Result<DnmsgLatestValueVO> result = opsMgmtClient.getLatestSetting(SERVICE_ID_NETWORK_SETTING, pileSn);
            log.info("Ops mgnt getPileNetworkSetting feign response: result = {}", JSON.toJSONString(result));
            if (Objects.nonNull(result.getData()) && Objects.nonNull(result.getData().getValue())) {
                return result.getData();
            }
        } catch (Exception e) {
            log.error("Ops mgnt getPileNetworkSetting feign error!", e);
        }
        return null;
    }


    public boolean hasWifiConfig(DnmsgLatestValueVO dnmsgLatestValueVO) {
        if (Objects.nonNull(dnmsgLatestValueVO)) {
            List<WifiInfoDTO>  wifiList = getWifiList(dnmsgLatestValueVO);
            if (CollectionUtil.isNotEmpty(wifiList)) {
                return true;
            }
        }
        return false;
    }

    public boolean hasSimCardConfig(DnmsgLatestValueVO dnmsgLatestValueVO) {
        if (Objects.nonNull(dnmsgLatestValueVO)) {
            Object object = dnmsgLatestValueVO.getValue();
            JSONObject valueObj = JSON.parseObject(JSON.toJSONString(object));
            if (valueObj.containsKey(SIM_CARD_LIST_KEY)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 供应商的芯片请求数据第一次
     * @param valueObj
     * @return
     */
    public boolean isFirstNetSetting(JSONObject valueObj) {
        return valueObj.containsKey(WIFI_KEY) && !valueObj.getJSONObject(WIFI_KEY).containsKey(WIFI_LIST_KEY);
    }

    public List<WifiInfoDTO> getWifiList(DnmsgLatestValueVO dnmsgLatestValueVO) {
        try {
            if (Objects.nonNull(dnmsgLatestValueVO)) {
                Object object = dnmsgLatestValueVO.getValue();
                JSONObject valueObj = JSON.parseObject(JSON.toJSONString(object));
                if (valueObj.containsKey(WIFI_KEY)) {
                    JSONObject wifiObj = valueObj.getJSONObject(WIFI_KEY);
                    if (isFirstNetSetting(valueObj)) {
                        WifiInfoDTO wifiInfoDTO = JSONObject.parseObject(wifiObj.toJSONString(), WifiInfoDTO.class);
                        return Lists.newArrayList(wifiInfoDTO);
                    } else {
                        List<WifiInfoDTO> list = JSONArray.parseArray(wifiObj.getJSONArray(WIFI_LIST_KEY).toJSONString(), WifiInfoDTO.class);
                        return list.stream()
                                .filter(WifiInfoDTO::getIsConnected)
                                .collect(Collectors.toList());
                    }

                }
            }
        } catch (Exception e) {
            log.error("getWifiList error!", e);
        }
        return null;
    }

    public List<SimCardInfoDTO> getSimCardList(DnmsgLatestValueVO dnmsgLatestValueVO) {
        try {
            if (Objects.nonNull(dnmsgLatestValueVO)) {
                Object object = dnmsgLatestValueVO.getValue();
                JSONObject valueObj = JSON.parseObject(JSON.toJSONString(object));
                if (valueObj.containsKey(SIM_CARD_LIST_KEY) && valueObj.getJSONArray(SIM_CARD_LIST_KEY).size() > 0) {
                    List<SimCardInfoDTO> list = JSONArray.parseArray(valueObj.getJSONArray(SIM_CARD_LIST_KEY).toJSONString(), SimCardInfoDTO.class);
                    return list.stream()
                            .filter(SimCardInfoDTO::getMobileDataEnable)
                            .collect(Collectors.toList());
                }
            }
        } catch (Exception e) {
            log.error("getSimCardList error!", e);
        }
        return null;
    }

    public Boolean getEthernetEnable(DnmsgLatestValueVO dnmsgLatestValueVO){
         try{
             if (Objects.nonNull(dnmsgLatestValueVO)) {
                 Object object = dnmsgLatestValueVO.getValue();
                 JSONObject valueObj = JSON.parseObject(JSON.toJSONString(object));
                 if (valueObj.containsKey(ETHERNET_ENABLE_KEY) && valueObj.getBoolean(ETHERNET_ENABLE_KEY) != null) {
                     return valueObj.getBoolean(ETHERNET_ENABLE_KEY);
                 }
             }
         }catch (Exception e){
             log.error("getSimCardList error!", e);
         }
         return false;
    }
}
