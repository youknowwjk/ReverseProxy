package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.EnergyBillDetailVO;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.autel.cloud.pile.bill.vo.bill.BillCDRInfoVO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.assertj.core.util.Lists;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Slf4j
@Component
public class PileBillServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private IBillFeignClient billFeignClient;

    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;

    /**
     * 查询最新充电上报信息
     */
    public EnergyBillDetailVO selectBillBaseByBusId(String busId) {
        try {
            return billFeignClient.selectBillBaseByBusId(busId);
        } catch (Exception e) {
            log.error("selectBillBaseByBusId", e);
            String content = String.format("调用服务(pile-bill-app//bill/billBase) %s", busId);
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return new EnergyBillDetailVO();
        }

    }

    public List<EnergyBillVO> findLastBillInfoByEvseList( List<String> evseSnList ) {
        log.info("findLastBillInfoByEvseList param is {}", JSON.toJSONString(evseSnList));
        if(CollectionUtils.isEmpty(evseSnList)){
            return Lists.newArrayList();
        }
        List<EnergyBillVO> ret = Lists.newArrayList();
        try {
            //Result<List<BillCDRInfoVO>> lastBillInfos = billFeignClient.findLastBillInfoByEvseList(evseSnList);
            Set<String> evseSnSet = new HashSet<>(evseSnList);
            Result<List<EnergyBillVO>> lastBillInfos = billFeignClient.findLastBillByEvses(evseSnSet);
            log.info("findLastBillInfoByEvseList result is {}",JSON.toJSONString(lastBillInfos));
            ret = lastBillInfos.getData();
        } catch (Exception e) {
            log.error("findLastBillInfoByEvseList", e);
            String content = String.format("调用服务(pile-bill-app//bill/findLastBillInfoByEvseList) %s", JSON.toJSONString(evseSnList));
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
        }
        return ret;
    }
}
