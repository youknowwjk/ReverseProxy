package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;
import java.util.Map;

/**
 * @ClassName ConfigServiceFeignClient
 * @Author A22121
 * @Description
 * @Date 2022/6/8 14:58
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient(name = "config-service")
public interface ConfigServiceFeignClient {
    @ApiOperation(value = "分页查询", notes = "分页查询", httpMethod = "POST")
    @PostMapping("/station/stationPage")
    Result<Page<Map<String, Object>>> stationPage(@RequestBody Map<String, Object> param);

    @ApiOperation(value = "获取场站桩枪数据", notes = "获取场站桩枪数据", httpMethod = "POST")
    @PostMapping("/pile/getPileDTOByStationId")
    Result<List<Map<String, Object>>> getPileDTOByStationId(@RequestParam("stationId") Long stationId);
}
