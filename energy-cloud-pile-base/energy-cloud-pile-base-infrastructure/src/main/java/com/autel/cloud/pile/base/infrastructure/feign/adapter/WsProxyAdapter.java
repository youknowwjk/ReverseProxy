package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ws.dto.ConnectInfo;
import com.autel.cloud.ws.dto.QueryConnectDto;
import com.autel.cloud.ws.feign.WsCoreClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;


@Slf4j
@Component
public class WsProxyAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private WsCoreClient wsCoreClient;


    public List<ConnectInfo> queryDeviceConnectInfo(QueryConnectDto queryConnectDto) {
        long now = System.currentTimeMillis();
        log.info("queryDeviceConnectInfo {}", JSON.toJSONString(queryConnectDto));
        Result<List<ConnectInfo>> listResult = wsCoreClient.queryDeviceConnectInfo(queryConnectDto);
        log.info("queryDeviceConnectInfo result {}", JSON.toJSONString(listResult));
        return nullableHandle(listResult);
    }


    public List<ConnectInfo> queryDeviceConnectInfo(Collection<String> pileSNs) {
        if (CollectionUtils.isEmpty(pileSNs)) {
            return Collections.emptyList();
        }
        QueryConnectDto queryConnectDto = new QueryConnectDto();
        queryConnectDto.setIds(new ArrayList<>(pileSNs));
        queryConnectDto.setProto("OCPP");
        return queryDeviceConnectInfo(queryConnectDto);
    }

}
