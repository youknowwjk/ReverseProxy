package com.autel.cloud.pile.base.domain.repository;


import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCostRuleDistributeEntity;

import java.util.List;
import java.util.Set;

/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录 服务接口（数据库存储服务）
 */
public interface OpCostRuleDistributeRepository {

    /**
     * @param opCostRuleDistributeEntity
     * @return
     * @function 计费规则下发新增
     */
    Boolean addOpCostRuleDistribute(OpCostRuleDistributeEntity opCostRuleDistributeEntity);

    /**
     * @param tariffId
     * @return
     * @function 计费规则下发物理删除（根据计费规则id）
     */
    Integer physicalDelOpCostRuleDistributeByRuleId(Long tariffId);

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发查询
     */
    List<OpCostRuleDistributeEntity> selectOpCostRuleDistributeList(OpCostRuleDistributeDTO opCostRuleDistributeDTO);

    /**
     * @param pileSnList 充电设备sn的集合
     * @return 充电桩计费规则下发记录集合
     * @function 根据充电设备sn的集合去查询充电桩计费规则下发记录集合
     */
    List<OpCostRuleDistributeEntity> selectOpCostRuleDistributeListByPileSnSet(Set<String> pileSnList);

    /**
     * @param opCostRuleDistributeEntitySet
     * @return
     * @function 批量新增或者修改充电桩计费规则下发记录
     */
    Boolean saveOrUpdateBatchByEntitySet(Set<OpCostRuleDistributeEntity> opCostRuleDistributeEntitySet);

    /**
     * @param opCostRuleDistributeEntitySet
     * @return 新增结果
     * @function 批量新增充电桩计费规则下发记录
     */
    Boolean saveBatchByEntitySet(Set<OpCostRuleDistributeEntity> opCostRuleDistributeEntitySet);

    /**
     * 下发失败或者桩生效失败的下发记录
     *
     * @param opCostRuleDistributeDTO
     * @return
     */
    List<OpCostRuleDistributeEntity> selectFailedDistributeList(OpCostRuleDistributeDTO opCostRuleDistributeDTO);

    /**
     * @param pileSn
     * @return
     * @function 根据桩SN查询该桩所绑定的计费规则下发记录
     */
    OpCostRuleDistributeEntity selectByPileSn(String pileSn);

    /**
     * @param opCostRuleDistributeEntity
     * @param pileSn
     * @return
     * @function 桩已经绑定过计费规则，此时需要替换新的计费规则，根据桩SN来修改
     */
    Boolean updateOpCostRuleDistributeByPileSn(OpCostRuleDistributeEntity opCostRuleDistributeEntity, String pileSn);

    /**
     * @param opCostRuleDistributeEntity
     * @return 更新结果
     * @function 根据主键id更新该桩的计费规则下发记录
     */
    Boolean updateOpCostRuleDistributeById(OpCostRuleDistributeEntity opCostRuleDistributeEntity);

    /**
     * @param opCostRuleDistributeEntityList
     * @return
     * @function 根据桩SN动态地批量修改桩的下发状态
     */
    Boolean updateOpCostRuleDistributeItemsByPileSn(List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList);

    /**
     * @param opCostRuleDistributeEntityList
     * @return
     * @function 根据桩SN动态地批量修改桩的生效状态
     */
    Boolean updateOpCostRuleEfficientByPileSn(List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList);
}
