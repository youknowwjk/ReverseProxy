package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.fleet.FleetFeignClient;
import com.autel.cloud.fleet.vo.smartChargingPlan.ChargePlanVo;
import com.autel.cloud.ws.dto.ConnectInfo;
import com.autel.cloud.ws.dto.QueryConnectDto;
import com.autel.cloud.ws.feign.WsCoreClient;
import lombok.extern.slf4j.Slf4j;
import org.assertj.core.util.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;


@Slf4j
@Component
public class FleetAdapter  {

    @Autowired
    private FleetFeignClient fleetFeignClient;


    public List<ChargePlanVo> queryPileBindSmartChargingPlan(String pileSn) {
        log.info("queryPileBindSmartChargingPlan is {}",JSON.toJSONString(pileSn));
        List<ChargePlanVo> result = Lists.newArrayList();
        try {
            Result<List<ChargePlanVo>> listResult = fleetFeignClient.queryPileBindSmartChargingPlan(pileSn);
            log.info("queryPileBindSmartChargingPlan result is {}", JSON.toJSONString(listResult));
            result = listResult.getData();
        }catch (Exception e){
            log.error("queryPileBindSmartChargingPlan error is {}",e.getMessage());
        }
        return result;
    }



}
