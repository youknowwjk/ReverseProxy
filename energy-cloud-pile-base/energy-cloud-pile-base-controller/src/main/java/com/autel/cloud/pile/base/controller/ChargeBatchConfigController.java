package com.autel.cloud.pile.base.controller;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.EvseBatchConfigService;
import com.autel.cloud.pile.base.dto.batch.EvseBatchConfigDTO;
import com.autel.cloud.pile.base.dto.batch.EvseBatchConfigResponseDTO;
import com.autel.cloud.pile.base.dto.batch.EvseConfigQueryDTO;
import com.autel.cloud.pile.base.vo.batch.EvseConfigVO;
import com.autel.cloud.pile.base.vo.batch.EvseRuleVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/batchConfig")
@Slf4j
@Validated
@Api(value = "批量配置", tags = "批量配置")
public class ChargeBatchConfigController {

   @Autowired
   private EvseBatchConfigService evseBatchConfigService;

   @ApiOperation("查询充电枪配置")
   @PostMapping("/serachEvseConfig")
   public Result<IPage<EvseConfigVO>> getEvseConfigPage (@Valid @RequestBody EvseConfigQueryDTO evseConfigQueryDTO) {
      log.info("==========> ChargeBatchConfigController EvseConfigQueryDTO : {}", evseConfigQueryDTO);
       return Result.ofSucceed(evseBatchConfigService.page(evseConfigQueryDTO));
   }


   @ApiOperation("查询充电枪及其配置下拉列表")
   @GetMapping("/evseList")
   public Result<IPage<EvseConfigVO>> getEvseConfigList(@RequestParam("page") Integer page,
                                                       @RequestParam("pageSize") Integer pageSize,
                                                       @RequestParam(value = "keyWord", required = false) String keyWord){
      return Result.ofSucceed(evseBatchConfigService.queryEvseList(page,pageSize,keyWord));
   }

   @ApiOperation("查询商户计费规则列表")
   @GetMapping("/costRuleList")
   public Result<List<EvseRuleVO>> getCostRuleList(){
      return Result.ofSucceed(evseBatchConfigService.getCostRuleList());
   }

   @ApiOperation("查询商户营销列表")
   @GetMapping("/marketingRuleList")
   public Result<List<EvseRuleVO>> getMarketingRuleList(){
      return Result.ofSucceed(evseBatchConfigService.getMarketingRuleList());
   }

   @ApiOperation("查询商户入场控制列表")
   @GetMapping("/entryControlList")
   public Result<List<EvseRuleVO>> getEntryControlList(){
      return Result.ofSucceed(evseBatchConfigService.getEntryControlList());
   }

   @ApiOperation("批量配置充电枪")
   @PostMapping("/policy")
   public Result<EvseBatchConfigResponseDTO> batchConfigEvse(@Valid @RequestBody EvseBatchConfigDTO evseBatchConfigDTO){
      return Result.ofSucceed(evseBatchConfigService.batchConfigEvse(evseBatchConfigDTO));
   }
}
