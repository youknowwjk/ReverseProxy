package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.bill.dto.KeywordPeriodSearchDTO;
import com.autel.cloud.pile.bill.dto.OrderWeeklyInfoDTO;
import com.autel.cloud.pile.bill.vo.EnergyBillDetailVO;
import com.autel.cloud.pile.bill.vo.emsp.TbWalletConfigVO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(value = "pile-bill-app", contextId = "PileBillClientInPileBase")
public interface PileBillClient {

    @GetMapping("/bill/lastBillExcludeCurrentOrderSeq")
    Result<EnergyBillDetailVO> findLastBillByEvseExcludeCurrentOrderSeq(@RequestParam(value = "evseSn") String evseSn, @RequestParam(value = "orderSeq", required = false) String orderSeq);

    @GetMapping("/wallet/config")
    @ApiOperation(value = "钱包配置详情", notes = "钱包配置详情")
    Result<TbWalletConfigVO> getConfig();

}
