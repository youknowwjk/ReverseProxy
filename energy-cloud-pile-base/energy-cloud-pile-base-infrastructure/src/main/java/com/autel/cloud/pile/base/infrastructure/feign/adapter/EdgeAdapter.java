package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.edge.EdgeFeignClient;
import com.autel.cloud.edge.vo.edge.PileBindVO;
import com.autel.cloud.fleet.FleetFeignClient;
import com.autel.cloud.fleet.vo.smartChargingPlan.ChargePlanVo;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.assertj.core.util.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;


@Slf4j
@Component
public class EdgeAdapter {

    @Autowired
    private EdgeFeignClient edgeFeignClient;


    public List<PileBindVO> queryPileBindEdgeGateway(String pileSn) {
        log.info("queryPileBindEdgeGateway is {}",JSON.toJSONString(pileSn));
        List<PileBindVO> result = Lists.newArrayList();
        try {
            Result<List<PileBindVO>> binds = edgeFeignClient.queryPileBindEdgeGateway(pileSn);
            log.info("queryPileBindEdgeGateway result is {}", JSON.toJSONString(binds));
            if(CollectionUtils.isNotEmpty( binds.getData() )){
                result =  binds.getData();
            }
        }catch (Exception e){
            log.error("queryPileBindSmartChargingPlan error is {}",e.getMessage());
        }
        return result;
    }

    public void delPileLinkedEdgeGatWay(String pileSn) {
        log.info("delPileLinkedEdgeGatWay is {}",JSON.toJSONString(pileSn));
        try {
            Result<Boolean> result = edgeFeignClient.delPileLinkedEdgeGatWay(pileSn);
            log.info("delPileLinkedEdgeGatWay result is {}", JSON.toJSONString(result));
        }catch (Exception e){
            log.error("delPileLinkedEdgeGatWay error is {}",e.getMessage());
        }
    }



}
