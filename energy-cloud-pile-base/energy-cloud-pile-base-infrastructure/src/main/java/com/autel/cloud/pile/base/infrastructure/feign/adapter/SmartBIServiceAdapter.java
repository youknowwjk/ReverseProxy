package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.SmartBiClient;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.*;

@Slf4j
@Component
public class SmartBIServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private SmartBiClient smartBiClient;

    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;

    /**
     * 获取充电桩发货时间
     */
    public Long getTimeOfFirstConnectToInternet(String sn) {
        try {
            log.info("getTimeOfFirstConnectToInternet {}", sn);
            Result<Map<String, Object>> mapResult = smartBiClient.pileLabel(SmartBiClient.LabelVO.builder().pileSn(sn).build());
            log.info("getTimeOfFirstConnectToInternet result {}", JSON.toJSONString(mapResult));
            Map<String, Object> map = Optional.ofNullable(nullableHandle(mapResult)).orElse(new HashMap<>());
            if (map.containsKey("firstLogAfterShipmentTime")) {
                return (Long) map.get("firstLogAfterShipmentTime");
            }
        } catch (Exception e) {
            log.error("getTimeOfFirstConnectToInternet", e);
        }
        return null;
    }

    /**
     * 桩属性配置
     *
     * @param sn 桩SN
     * @return imei
     */
    public String getIMEI(String sn) {
        String result="";
        try {
            SmartBiClient.PileAttributeConfigVo pileAttributeConfigVo = new SmartBiClient.PileAttributeConfigVo();
            List<String> fieldList = new ArrayList<>();
            fieldList.add("imei");
            List<String> pileSnList = new ArrayList<>();
            pileSnList.add(sn);
            pileAttributeConfigVo.setFieldList(fieldList);
            pileAttributeConfigVo.setPileSnList(pileSnList);
            Result<List<Map<String, Object>>> listResult = smartBiClient.pileAttributeConfig(pileAttributeConfigVo);
            if (null != listResult.getData() && CollUtil.isNotEmpty(listResult.getData())) {
                Map<String, Object> map = listResult.getData().get(0);
                Object imei = map.get("imei");
                result = Optional.ofNullable(imei).map(Object::toString).orElse(null);
            }
        } catch (Exception e) {
            log.info("imei:查询错误：", e);
        }
        return result;
    }
}
