package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.TariffIssuedDTO;
import com.autel.cloud.pile.base.dto.tariff.DispatchTariffOfPublicPileByPileSnListDTO;
import com.autel.cloud.pile.base.dto.tariff.HomePileTariffBatchDispatchDTO;
import com.autel.cloud.pile.base.dto.tariff.HomePileTariffDispatchDTO;
import com.autel.cloud.pile.base.vo.LocationNameAndPileSnListVO;

import java.util.List;

public interface DistributeCostRuleService {

    /**
     * 修改电价推送计费模板
     *
     * @param homePileTariffDispatchDTO
     * @return
     */
    @Deprecated
    Result<Boolean> dispatchTariffByPrice(HomePileTariffDispatchDTO homePileTariffDispatchDTO);

    /**
     * 修改家桩计费规则推送计费模板
     *
     * @param homePileTariffDispatchDTO
     * @return
     */
    Result<Boolean> dispatchHomePileTariff(HomePileTariffDispatchDTO homePileTariffDispatchDTO);

    /**
     * 修改货币符号推送计费模板
     *
     * @param homePileTariffBatchDispatchDTO
     * @return
     */
    @Deprecated
    Result<Boolean> batchDispatchTariffByCurrency(HomePileTariffBatchDispatchDTO homePileTariffBatchDispatchDTO);

    /**
     * 修改货币符号推送计费模板
     *
     * @param homePileTariffBatchDispatchDTO
     * @return
     */
    Result<Boolean> batchDispatchTariffByCurrencyV2(HomePileTariffBatchDispatchDTO homePileTariffBatchDispatchDTO);

    /**
     * 根据计费规则ID下发商桩计费模板
     *
     * @param tariffId
     * @return
     */
    Result<Boolean> dispatchTariffOfPublicPileByTariffId(Long tariffId, Long userId, Boolean newRelease);

    /**
     * 下发商桩计费规则
     *
     * @param tariffId
     * @param pileSn
     * @return
     */
    Result<Boolean> dispatchTariffOfPublicPile(Long tariffId, String pileSn, String timezone);

    /**
     * 重试下发家桩充电桩计费规则
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> retryDistributeHomeCostRule(String pileSn, Long tariffId, Long userId, Double price);

    Result<Boolean> getHubjectByLocationId(Long locationId);

    Result<LocationNameAndPileSnListVO> getByTariffId(Long tariffId);

    /**
     * @param dispatchTariffOfPublicPileByPileSnListDTO 清空五寸桩计费规则 入参模型
     * @return 清空五寸桩计费规则结果
     * @function 清空五寸桩计费规则
     */
    Result<Boolean> dispatchTariffOfPublicPileByPileSnList(DispatchTariffOfPublicPileByPileSnListDTO dispatchTariffOfPublicPileByPileSnListDTO);

    Boolean issueBillingRule(TariffIssuedDTO tariffIssuedDTO);

    Boolean businessPileOnlineIssueBillingRule(String pileSn, boolean clearFlag);

    Boolean issueBillingRule(List<String> pileSnList);
}
