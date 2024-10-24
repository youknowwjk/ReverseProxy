package com.autel.cloud.pile.base.infrastructure.feign.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.oicp.*;
import com.autel.cloud.pile.base.infrastructure.feign.OicpFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;

/**
 * @author X21216
 * @date 2023/3/21  20:29
 */
@Component
@Slf4j
public class OicpFeignClientProxy {

    @Resource
    private OicpFeignClient oicpFeignClient;

    @Value("${hubject.enable:false}")
    private String hubjectEnable;

    public Result<Boolean> pushEvseData(EroamingEvseData eroamingEvseData, Integer action) {
        return oicpFeignClient.pushEvseData(eroamingEvseData, action);
    }


    public Result<List<PullEvseDataRespDTO>> queryEvseData(PullEvseStatusDTO pullEvseStatue) {
        return oicpFeignClient.queryEvseData(pullEvseStatue);
    }

    public Result<List<PullEvseDataRespDTO>> queryChargingPointData(Long chargingId) {
        return oicpFeignClient.queryChargingPointData(chargingId);
    }

    /**
     * action: 0-fullLoad, 1-update, 2-insert, 3-delete
     *
     * @param msgDto msgDto
     * @param action action
     * @return
     */
    public Result<Boolean> cpoPushPricingProductData(List<TariffEvse> msgDto, Integer action) {
        return oicpFeignClient.cpoPushPricingProductData(msgDto, action);
    }

    /**
     * 查询oicp-app服务，查看是否需要查询Hubject服务
     *
     * @return checkNeedHubject
     */
    public boolean checkUserIsGray() {
        boolean needHubject = false;
        try {
            if (Boolean.parseBoolean(hubjectEnable)) {
                Result<Boolean> result = oicpFeignClient.checkUserIsGray();
                needHubject = result.getData() != null && result.getData();
            }
        } catch (Exception e) {
            log.error("查询是否搜索hubject桩信息异常");
            needHubject = false;
        }
        log.info("是否需要查询Hubject: {}", needHubject);
        return needHubject;
    }
}
