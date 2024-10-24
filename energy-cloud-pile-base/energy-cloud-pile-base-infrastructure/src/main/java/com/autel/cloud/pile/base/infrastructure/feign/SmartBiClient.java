package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.UserFeatureDTO;
import io.swagger.annotations.ApiOperation;
import lombok.Builder;
import lombok.Data;
import lombok.ToString;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.Map;

/**
 * Author:   A19011
 * Description: SaasAccessFeignClient
 * Date:     2022/5/12 11:14
 *
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient(value = "smart-bi",contextId = "SmartBiClient")
public interface SmartBiClient {

    @PostMapping("/smartCharge/queryUserChargeFeatureById")
    @ApiOperation(value = "通过用户id获取用户充电属性")
    Result<Map<Long,List<UserFeatureDTO>>> queryUserChargeFeatureById(@Validated @RequestBody List<Long> userIds);

    @ApiOperation(value = "获取首次联网时间", notes = "桩首次上报日志的时间")
    @PostMapping("/ops/pileLabel")
    Result<Map<String, Object>> pileLabel(LabelVO labelVo);

    @ApiOperation(value = "桩属性配置")
    @PostMapping(value = "/mainData/pileAttributeConfig", produces = {"application/json"})
    @ResponseBody
    public Result<List<Map<String, Object>>> pileAttributeConfig(@Validated @RequestBody PileAttributeConfigVo vo);
    /**
     * @author A22581
     */
    @Data
    @Builder
    class LabelVO {

        private String pileSn;
    }

    @Data
    @ToString
    class PileAttributeConfigVo {
        private List<String> fieldList;
        private List<String> pileSnList;
    }
}
