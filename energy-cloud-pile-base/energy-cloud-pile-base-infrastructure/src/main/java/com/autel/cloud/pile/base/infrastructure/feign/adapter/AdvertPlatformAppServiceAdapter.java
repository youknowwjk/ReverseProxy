package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.AdvertPlatformAppFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.MgmtFeignClient;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Slf4j
@Component
public class AdvertPlatformAppServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private AdvertPlatformAppFeignClient creativePlatformAppFeignClient;

    @Resource
    private MgmtFeignClient mgmtFeignClient;


    @Value("${webhook.wechat.key:95a17fbd-4a05-495b-8c17-9253a5cd03d8}")
    protected String webhookWechatKey;



    /**
     * 查询家桩是否绑定
     */
    public List<AdvertPlatformAppFeignClient.ResponseData> sync() {
        log.info("CreativePlatformAppFeignClient sync ");
        Result<List<AdvertPlatformAppFeignClient.ResponseData>> responseDataResult = creativePlatformAppFeignClient.sync();
        log.info("CreativePlatformAppFeignClient result {}", JSON.toJSONString(responseDataResult));
        return Optional.ofNullable(nullableHandle(responseDataResult)).orElse(Collections.emptyList());
    }


    /**
     * 查询家桩是否绑定
     */
    public Page<MgmtFeignClient.PileInfoPO> history(MgmtFeignClient.PileHistoryDTO pileDto) {
        log.info("mgmtFeignClient.history  {}", JSON.toJSONString(pileDto));

        Result<Page<MgmtFeignClient.PileInfoPO>> pageResult = mgmtFeignClient.history(pileDto);
        log.info("mgmtFeignClient.history result {}", JSON.toJSONString(pageResult));
        return Optional.ofNullable(nullableHandle(pageResult)).orElse(null);
    }

}
