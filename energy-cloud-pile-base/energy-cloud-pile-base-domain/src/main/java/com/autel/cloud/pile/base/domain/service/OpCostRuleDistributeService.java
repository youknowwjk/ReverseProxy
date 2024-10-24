package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.dto.SendEmailDownLoadHistoryDataDTO;
import com.autel.cloud.pile.base.dto.tariff.CostRuleDispatchPileSnDTO;

import java.util.List;
import java.util.Map;

/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录 业务逻辑层接口
 */
public interface OpCostRuleDistributeService {

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发新增
     */
    Result<Boolean> addOpCostRuleDistribute(OpCostRuleDistributeDTO opCostRuleDistributeDTO);

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN码是否存在，批量新增或者修改计费规则下发记录
     */
    Result<Boolean> addOrUpdateOpCostRuleDistributeByPileSn(List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList);

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发查询
     */
    Result<List<OpCostRuleDistributeDTO>> selectOpCostRuleDistributeList(OpCostRuleDistributeDTO opCostRuleDistributeDTO);

    /**
     * @param pileSn
     * @return
     * @function 通过桩Sn重新下发计费规则
     */
    Result<Boolean> reIssueBillingRulesByPileSn(String pileSn);

    /**
     * @param tariffId
     * @return
     * @function 通过计费规则ID重新下发计费规则
     */
    Result<Boolean> reIssueBillingRulesByTariffId(Long tariffId);

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN动态地批量修改桩的下发状态
     */
    Result<Boolean> updateOpCostRuleDistributeByPileSn(List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList);

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN动态地批量修改桩的生效状态
     */
    Result<Boolean> updateOpCostRuleEfficientByPileSn(List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList);

    /**
     * @param pileSn
     * @return
     * @function 根据桩SN查询计费规则下发记录
     */
    Result<OpCostRuleDistributeDTO> selectOpCostRuleDistributeByPileSn(String pileSn);

    /**
     * 过滤非第三方充电桩的计费规则下发记录
     *
     * @param costRuleDispatchPileSnDTOList
     * @return
     */
    List<CostRuleDispatchPileSnDTO> filterNonThirdPileCostRule(List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList);

    /**
     * 封装计费规则下发记录
     *
     * @param
     * @return
     */
    Map<String, List<OpCostRuleDistributeDTO>> wrapCostRuleDistribute(List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList);

    /**
     * 重试下发公共桩充电桩计费规则
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> retryDistributePublicCostRule(String pileSn, Long tariffId, Long userId);

    /**
     * 重试下发计费规则
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> retryDistributeTariff(String pileSn);

    Result<Boolean> sendEmailDownLoadHistoryData(SendEmailDownLoadHistoryDataDTO sendEmailDownLoadHistoryDataDTO);
}
