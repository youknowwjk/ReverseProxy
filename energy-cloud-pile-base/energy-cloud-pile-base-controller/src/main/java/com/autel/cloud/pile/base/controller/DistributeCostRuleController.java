package com.autel.cloud.pile.base.controller;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.DistributeCostRuleService;
import com.autel.cloud.pile.base.dto.TariffIssuedDTO;
import com.autel.cloud.pile.base.dto.tariff.DispatchTariffOfPublicPileByPileSnListDTO;
import com.autel.cloud.pile.base.dto.tariff.HomePileTariffBatchDispatchDTO;
import com.autel.cloud.pile.base.dto.tariff.HomePileTariffDispatchDTO;
import com.autel.cloud.pile.base.vo.LocationNameAndPileSnListVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;

@Api(value = "计费规则下发")
@RequestMapping(value = "/costRule/distribute")
@RestController
@Slf4j
@Validated
public class DistributeCostRuleController {

    @Autowired
    private DistributeCostRuleService dispatchTariffByPrice;

    @PostMapping(value = "/homePile/dispatchByPrice")
    @ApiOperation(value = "修改电价推送计费模板", notes = "家桩计费模板")
    @Deprecated
    public Result<Boolean> dispatchTariffByPrice(@RequestBody HomePileTariffDispatchDTO homePileTariffDispatchDTO) {
        log.info("============ the homePileTariffDispatchDTO in the HomePileTariffController:{}", JSON.toJSONString(homePileTariffDispatchDTO));
        return dispatchTariffByPrice.dispatchTariffByPrice(homePileTariffDispatchDTO);
    }

    @PostMapping(value = "/homePile/dispatchHomePileTariff")
    @ApiOperation(value = "修改家桩计费规则推送计费模板", notes = "家桩计费模板")
    public Result<Boolean> dispatchHomePileTariff(@RequestBody HomePileTariffDispatchDTO homePileTariffDispatchDTO) {
        log.info("============ the homePileTariffDispatchDTO in the HomePileTariffController:{}", JSON.toJSONString(homePileTariffDispatchDTO));
        return dispatchTariffByPrice.dispatchHomePileTariff(homePileTariffDispatchDTO);
    }

    /**
     * 修改电价推送计费模板(兼容请求头为"x-www-form-urlencoded"的请求)
     *
     * @param energyPrice
     * @param pileSn
     * @return
     */
    @PostMapping(value = "/homePile/dispatchByPrice2")
    @ApiOperation(value = "修改电价推送计费模板", notes = "家桩计费模板")
    @Deprecated
    public Result<Boolean> dispatchTariffByPrice2(@RequestParam("energyPrice") String energyPrice, @RequestParam("pileSn") String pileSn) {
        log.info("============ the homePileTariffDispatchDTO in the HomePileTariffController:{}, pileSn:{}", energyPrice, pileSn);
        if (StrUtil.isBlank(energyPrice) || StrUtil.isBlank(pileSn)) {
            log.error("============ because of energyPrice == null or pileSn == null, return, energyPrice:{}, pileSn:{}", energyPrice, pileSn);
            return Result.ofSucceed(true);
        }
        HomePileTariffDispatchDTO homePileTariffDispatchDTO = new HomePileTariffDispatchDTO();
        homePileTariffDispatchDTO.setPileSn(pileSn);
        homePileTariffDispatchDTO.setEnergyPrice(new BigDecimal(energyPrice));
        return dispatchTariffByPrice.dispatchTariffByPrice(homePileTariffDispatchDTO);
    }

    @PostMapping(value = "/homePile/dispatchByCurrency")
    @ApiOperation(value = "修改货币符号推送计费模板", notes = "家桩计费模板")
    @Deprecated
    public Result<Boolean> batchDispatchTariffByCurrency(@RequestBody HomePileTariffBatchDispatchDTO homePileTariffBatchDispatchDTO) {
        log.info("============ the homePileTariffBatchDispatchDTO in the HomePileTariffController:{}", JSON.toJSONString(homePileTariffBatchDispatchDTO));
        return dispatchTariffByPrice.batchDispatchTariffByCurrencyV2(homePileTariffBatchDispatchDTO);
    }

    @PostMapping(value = "/publicPile")
    @ApiOperation(value = "根据计费规则ID下发商桩计费模板", notes = "商桩计费模板")
    public Result<Boolean> dispatchTariffOfPublicPileByTariffId(@RequestParam("tariffId") Long tariffId, @RequestParam("userId") Long userId) {
        log.info("============ in the dispatchTariffOfPublicPile, the tariffId:{}, userId:{}", tariffId, userId);
        return dispatchTariffByPrice.dispatchTariffOfPublicPileByTariffId(tariffId, userId, null);
    }

    /**
     * @param dispatchTariffOfPublicPileByPileSnListDTO v1.9版本下发计费规则 入参对象
     * @return
     * @function v1.9版本下发需要满足：计费规则为旧版计费规则, 5寸，8寸，固件版本低于指定版本都需要下发
     */
    @PostMapping(value = "/newRelease")
    @ApiOperation(value = "v1.9版本下发计费规则", notes = "v1.9版本下发计费规则")
    public Result<Boolean> newRelease(@RequestBody DispatchTariffOfPublicPileByPileSnListDTO dispatchTariffOfPublicPileByPileSnListDTO) {

        log.info("===>>> DistributeCostRuleController.newRelease dispatchTariffOfPublicPileByPileSnListDTO : {}", JSON.toJSONString(dispatchTariffOfPublicPileByPileSnListDTO));

        Long tariffId = dispatchTariffOfPublicPileByPileSnListDTO.getTariffId();
        Long userId = dispatchTariffOfPublicPileByPileSnListDTO.getUserId();
        Boolean newRelease = dispatchTariffOfPublicPileByPileSnListDTO.getNewRelease();

        return dispatchTariffByPrice.dispatchTariffOfPublicPileByTariffId(tariffId, userId, newRelease);
    }

    @GetMapping(value = "/getHubjectByLocationId")
    @ApiOperation(value = "根据场站id查询场站的hubject是否打开", notes = "根据场站id查询场站的hubject是否打开")
    public Result<Boolean> getHubjectByLocationId(@RequestParam("locationId") Long locationId) {
        return dispatchTariffByPrice.getHubjectByLocationId(locationId);
    }

    @GetMapping(value = "/getByTariffId")
    @ApiOperation(value = "根据计费规则id查询其关联的场站和桩、枪", notes = "根据计费规则id查询其关联的场站和桩、枪")
    public Result<LocationNameAndPileSnListVO> getByTariffId(@RequestParam("tariffId") Long tariffId) {
        return dispatchTariffByPrice.getByTariffId(tariffId);
    }

    /**
     * @param dispatchTariffOfPublicPileByPileSnListDTO 清空五寸桩计费规则 入参模型
     * @return 清空五寸桩计费规则结果
     * @function 清空五寸桩计费规则
     */
    @PostMapping(value = "/dispatchTariffOfPublicPileByPileSnList")
    @ApiOperation(value = "清空五寸桩计费规则", notes = "清空五寸桩计费规则")
    public Result<Boolean> dispatchTariffOfPublicPileByPileSnList(@RequestBody DispatchTariffOfPublicPileByPileSnListDTO dispatchTariffOfPublicPileByPileSnListDTO) {

        log.info("======>>>>>>>>>>> DistributeCostRuleController.dispatchTariffOfPublicPileByPileSnList fiveInchesPileSnList : {}", JSON.toJSONString(dispatchTariffOfPublicPileByPileSnListDTO));

        return dispatchTariffByPrice.dispatchTariffOfPublicPileByPileSnList(dispatchTariffOfPublicPileByPileSnListDTO);
    }

    @PostMapping(value = "/issueBillingRule")
    @ApiOperation(value = "下发计费规则相关逻辑", notes = "下发计费规则相关逻辑")
    public Result<Boolean> issueBillingRule(@RequestBody TariffIssuedDTO tariffIssuedDTO) {

        log.info("===>>> DistributeCostRuleController.issueBillingRule tariffIssuedDTO : {}",
                JSON.toJSONString(tariffIssuedDTO));

        return Result.ofSucceed(dispatchTariffByPrice.issueBillingRule(tariffIssuedDTO));
    }
}
