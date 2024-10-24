package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.commodityCenter.feign.CommodityFeignClient;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;


@Slf4j
@Component
public class CommodityCenterServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private CommodityFeignClient commodityFeignClient;


    public String getCommodityCategory(String skuCode) {
        try {
            log.info("commodityFeignClient.getCommodityCategory request: {}", skuCode);
            Result<String> result = commodityFeignClient.categoryRelation(skuCode);
            log.info("commodityFeignClient.getCommodityCategory response: {}", JSON.toJSONString(result));
            return result.getData();
        } catch (Exception e) {
            log.error("commodityFeignClient.getCommodityCategory exception:", e);
            throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
        }

    }


}
