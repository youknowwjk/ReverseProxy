package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.SmartChargeCalculateDTO;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

/**
 * @Author temp
 * @Date 2023/4/25 11:27
 */
@FeignClient(value = "ai-smart-charging",contextId = "AiSmartChargingClient")
public interface AiSmartChargingClient {

    @PostMapping("/api/arithmetic")
    Result<List<OpLocationPileGroupDeliveryVO>> arithmetic(@RequestBody SmartChargeCalculateDTO paramDTO);
}
