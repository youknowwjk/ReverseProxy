package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import io.swagger.annotations.ApiOperation;
import lombok.Data;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;

import java.util.List;

@FeignClient(name = "advert-platform-app")
public interface AdvertPlatformAppFeignClient {


    @ApiOperation(value = "同步广告添加设备到统一桩管理", notes = "同步广告添加设备到统一桩管理", httpMethod = "POST")
    @PostMapping("/device/sync")
    Result<List<ResponseData>> sync();


    @Data
    class ResponseData {

        private String brandId;
        private String brandName;
        private String name;
        private String pin;
        private String sellerId;
        private String sn;
        private String string;

    }
}
