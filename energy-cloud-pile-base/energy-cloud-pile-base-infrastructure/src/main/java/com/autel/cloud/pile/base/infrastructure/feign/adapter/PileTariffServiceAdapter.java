package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.TariffAPPFeign;
import com.autel.cloud.tariff.dto.CurrencyDTO;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class PileTariffServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private TariffAPPFeign tariffAPPFeign;

    @Value("${webhook.wechat.key.pile-user:66ae591a-4e15-4b57-9a1c-56978ba6152b}")
    protected String webhookWechatKey;

    private final LoadingCache<Long, String> sellerCache = CacheBuilder.newBuilder()
            .expireAfterWrite(30, TimeUnit.MINUTES)
            .initialCapacity(100)
            .maximumSize(1000)
            .build(new CacheLoader<Long, String>() {
                @Override
                public String load(Long id) {
                    return loadCurrencyListBySellerId(id);
                }
            });

    public String getCurrencyListBySellerId(Long sellerId) {
        try {
            return sellerCache.get(sellerId);
        } catch (ExecutionException e) {
            log.error("getCurrencyListBySellerId" + sellerId, e);
            return loadCurrencyListBySellerId(sellerId);
        }
    }

    public String loadCurrencyListBySellerId(Long sellerId) {
        log.info("loadCurrencyListBySellerId: {}", sellerId);
        Result<List<CurrencyDTO>> listResult = tariffAPPFeign.getCurrencyListBySellerId(sellerId);
        log.info("loadCurrencyListBySellerId result: {}", JSON.toJSONString(listResult));
        List<CurrencyDTO> handle = nullableHandle(listResult);
        if (CollectionUtils.isEmpty(handle)) {
            String content = String.format("调用服务(pile-user-app/seller/getCurrencyListBySellerId)没有查询该商家 \n sellerId=%s", sellerId);
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return "￥";
        }
        return handle.get(0).getCurrencySign();
    }


    public void refreshSeller(Long sellerId) {
        sellerCache.refresh(sellerId);
    }

    public void cleanUp() {
        sellerCache.cleanUp();
    }
}
