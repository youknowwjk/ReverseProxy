package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.repository.OpCostRuleDistributeRepository;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpCostRuleDistributeMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCostRuleDistributeEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.List;
import java.util.Set;

import static com.autel.cloud.pile.base.constant.BaseConstant.LIMIT_1;

/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录 服务实现类（数据库存储服务）
 */
@Slf4j
@Service
public class OpCostRuleDistributeRepositoryImpl extends ServiceImpl<OpCostRuleDistributeMapper, OpCostRuleDistributeEntity> implements OpCostRuleDistributeRepository {

    @Resource
    private OpCostRuleDistributeMapper opCostRuleDistributeMapper;

    /**
     * @param opCostRuleDistributeEntity
     * @return
     * @function 计费规则下发新增
     */
    @Override
    public Boolean addOpCostRuleDistribute(OpCostRuleDistributeEntity opCostRuleDistributeEntity) {

        log.info("========== addOpCostRuleDistribute in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(opCostRuleDistributeEntity));

        return this.save(opCostRuleDistributeEntity);
    }

    /**
     * @param tariffId
     * @return
     * @function 计费规则下发物理删除（根据计费规则id）
     */
    @Override
    public Integer physicalDelOpCostRuleDistributeByRuleId(Long tariffId) {

        log.info("========== physicalDelOpCostRuleDistributeByRuleId in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(tariffId));

        LambdaQueryWrapper<OpCostRuleDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();

        lambdaQueryWrapper.eq(OpCostRuleDistributeEntity::getRuleId, tariffId);

        return opCostRuleDistributeMapper.delete(lambdaQueryWrapper);
    }

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发查询
     */
    @Override
    public List<OpCostRuleDistributeEntity> selectOpCostRuleDistributeList(OpCostRuleDistributeDTO opCostRuleDistributeDTO) {

        log.info("========== selectOpCostRuleDistributeList in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTO));

        LambdaQueryWrapper<OpCostRuleDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();

        lambdaQueryWrapper.eq(null != opCostRuleDistributeDTO.getId(), OpCostRuleDistributeEntity::getId, opCostRuleDistributeDTO.getId());
        lambdaQueryWrapper.eq(StrUtil.isNotBlank(opCostRuleDistributeDTO.getPileSn()), OpCostRuleDistributeEntity::getPileSn, opCostRuleDistributeDTO.getPileSn());
        lambdaQueryWrapper.eq(null != opCostRuleDistributeDTO.getRuleId(), OpCostRuleDistributeEntity::getRuleId, opCostRuleDistributeDTO.getRuleId());
        lambdaQueryWrapper.eq(null != opCostRuleDistributeDTO.getSuccessFlag(), OpCostRuleDistributeEntity::getSuccessFlag, opCostRuleDistributeDTO.getSuccessFlag());
        lambdaQueryWrapper.eq(null != opCostRuleDistributeDTO.getEfficientFlag(), OpCostRuleDistributeEntity::getEfficientFlag, opCostRuleDistributeDTO.getEfficientFlag());
        lambdaQueryWrapper.eq(OpCostRuleDistributeEntity::getDeleted, 0);
        lambdaQueryWrapper.orderByAsc(OpCostRuleDistributeEntity::getId);

        return this.list(lambdaQueryWrapper);
    }

    /**
     * @param pileSnList 充电设备sn的集合
     * @return 充电桩计费规则下发记录集合
     * @function 根据充电设备sn的集合去查询充电桩计费规则下发记录集合
     */
    @Override
    public List<OpCostRuleDistributeEntity> selectOpCostRuleDistributeListByPileSnSet(Set<String> pileSnList) {
        if (ObjectUtils.isNotEmpty(pileSnList)) {
            LambdaQueryWrapper<OpCostRuleDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
            lambdaQueryWrapper
                    .select(OpCostRuleDistributeEntity::getId, OpCostRuleDistributeEntity::getPileSn)
                    .in(OpCostRuleDistributeEntity::getPileSn, pileSnList)
                    .eq(OpCostRuleDistributeEntity::getDeleted, 0);
            return opCostRuleDistributeMapper.selectList(lambdaQueryWrapper);
        }
        return null;
    }

    /**
     * @param opCostRuleDistributeEntitySet
     * @return
     * @function 批量新增或者修改充电桩计费规则下发记录
     */
    @Override
    public Boolean saveOrUpdateBatchByEntitySet(Set<OpCostRuleDistributeEntity> opCostRuleDistributeEntitySet) {
        if (ObjectUtils.isNotEmpty(opCostRuleDistributeEntitySet)) {
            return this.saveOrUpdateBatch(opCostRuleDistributeEntitySet);
        }
        return false;
    }

    /**
     * @param opCostRuleDistributeEntitySet
     * @return 新增结果
     * @function 批量新增充电桩计费规则下发记录
     */
    @Override
    public Boolean saveBatchByEntitySet(Set<OpCostRuleDistributeEntity> opCostRuleDistributeEntitySet) {
        if (ObjectUtils.isNotEmpty(opCostRuleDistributeEntitySet)) {
            return this.saveBatch(opCostRuleDistributeEntitySet);
        }
        return false;
    }


    @Override
    public List<OpCostRuleDistributeEntity> selectFailedDistributeList(OpCostRuleDistributeDTO opCostRuleDistributeDTO) {

        log.info("========== selectFailedDistributeList in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTO));

        LambdaQueryWrapper<OpCostRuleDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();

        lambdaQueryWrapper.nested(wrapper ->
                wrapper.eq(OpCostRuleDistributeEntity::getSuccessFlag, 0)
                        .or()
                        .isNull(OpCostRuleDistributeEntity::getSuccessFlag)
                        .or()
                        .eq(OpCostRuleDistributeEntity::getEfficientFlag, 0)
                        .or()
                        .isNull(OpCostRuleDistributeEntity::getSuccessFlag)
        );
        lambdaQueryWrapper.eq(StrUtil.isNotBlank(opCostRuleDistributeDTO.getPileSn()), OpCostRuleDistributeEntity::getPileSn, opCostRuleDistributeDTO.getPileSn());
        lambdaQueryWrapper.eq(null != opCostRuleDistributeDTO.getRuleId(), OpCostRuleDistributeEntity::getRuleId, opCostRuleDistributeDTO.getRuleId());
        lambdaQueryWrapper.eq(OpCostRuleDistributeEntity::getDeleted, 0);
        lambdaQueryWrapper.orderByAsc(OpCostRuleDistributeEntity::getId);
        return this.list(lambdaQueryWrapper);
    }

    /**
     * @param pileSn
     * @return
     * @function 根据桩SN查询该桩所绑定的计费规则下发记录
     */
    @Override
    public OpCostRuleDistributeEntity selectByPileSn(String pileSn) {
        log.info("========== selectByPileSn in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(pileSn));
        LambdaQueryWrapper<OpCostRuleDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(OpCostRuleDistributeEntity::getPileSn, pileSn);
        lambdaQueryWrapper.eq(OpCostRuleDistributeEntity::getDeleted, 0);
        lambdaQueryWrapper.last(LIMIT_1);
        return opCostRuleDistributeMapper.selectOne(lambdaQueryWrapper);
    }

    /**
     * @param opCostRuleDistributeEntity
     * @param pileSn
     * @return
     * @function 桩已经绑定过计费规则，此时需要替换新的计费规则，根据桩SN来修改
     */
    @Override
    public Boolean updateOpCostRuleDistributeByPileSn(OpCostRuleDistributeEntity opCostRuleDistributeEntity, String pileSn) {

        log.info("========== updateOpCostRuleDistributeByPileSn in the OpCostRuleDistributeRepositoryImpl function : opCostRuleDistributeEntity : {}, pileSn : {}", JSON.toJSONString(opCostRuleDistributeEntity), JSON.toJSONString(pileSn));

        LambdaUpdateWrapper<OpCostRuleDistributeEntity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();

        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getRuleId, opCostRuleDistributeEntity.getRuleId());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getSuccessFlag, opCostRuleDistributeEntity.getSuccessFlag());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getFailureReason, opCostRuleDistributeEntity.getFailureReason());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getDistributeTime, opCostRuleDistributeEntity.getDistributeTime());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientFlag, opCostRuleDistributeEntity.getEfficientFlag());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientFailureReason, opCostRuleDistributeEntity.getEfficientFailureReason());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientTime, opCostRuleDistributeEntity.getEfficientTime());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getUpdateBy, opCostRuleDistributeEntity.getUpdateBy());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getUpdateTime, opCostRuleDistributeEntity.getUpdateTime());
        lambdaUpdateWrapper.eq(OpCostRuleDistributeEntity::getPileSn, pileSn);
        lambdaUpdateWrapper.eq(OpCostRuleDistributeEntity::getDeleted, 0);

        return this.update(lambdaUpdateWrapper);
    }

    /**
     * @param opCostRuleDistributeEntity
     * @return 更新结果
     * @function 根据主键id更新该桩的计费规则下发记录
     */
    @Override
    public Boolean updateOpCostRuleDistributeById(OpCostRuleDistributeEntity opCostRuleDistributeEntity) {

        log.info("========== updateOpCostRuleDistributeByPileSn in the OpCostRuleDistributeRepositoryImpl function : opCostRuleDistributeEntity : {}", JSON.toJSONString(opCostRuleDistributeEntity));

        LambdaUpdateWrapper<OpCostRuleDistributeEntity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();

        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getRuleId, opCostRuleDistributeEntity.getRuleId());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getSuccessFlag, opCostRuleDistributeEntity.getSuccessFlag());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getFailureReason, opCostRuleDistributeEntity.getFailureReason());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getDistributeTime, opCostRuleDistributeEntity.getDistributeTime());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientFlag, opCostRuleDistributeEntity.getEfficientFlag());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientFailureReason, opCostRuleDistributeEntity.getEfficientFailureReason());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientTime, opCostRuleDistributeEntity.getEfficientTime());
        lambdaUpdateWrapper.set(null != opCostRuleDistributeEntity.getCreateBy(), OpCostRuleDistributeEntity::getCreateBy, opCostRuleDistributeEntity.getCreateBy());
        lambdaUpdateWrapper.set(null != opCostRuleDistributeEntity.getCreateTime(), OpCostRuleDistributeEntity::getCreateTime, opCostRuleDistributeEntity.getCreateTime());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getUpdateBy, opCostRuleDistributeEntity.getUpdateBy());
        lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getUpdateTime, opCostRuleDistributeEntity.getUpdateTime());
        lambdaUpdateWrapper.eq(OpCostRuleDistributeEntity::getId, opCostRuleDistributeEntity.getId());
        lambdaUpdateWrapper.eq(OpCostRuleDistributeEntity::getDeleted, 0);

        return this.update(lambdaUpdateWrapper);
    }

    /**
     * @param opCostRuleDistributeEntityList
     * @return
     * @function 根据桩SN动态地批量修改桩的下发状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateOpCostRuleDistributeItemsByPileSn(List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList) {
        log.info("========== updateOpCostRuleDistributeItemsByPileSn in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(opCostRuleDistributeEntityList));
        try {
            if (CollUtil.isNotEmpty(opCostRuleDistributeEntityList)) {
                opCostRuleDistributeMapper.batchUpdate(opCostRuleDistributeEntityList);
            }
        } catch (Exception e) {
            log.error("========== updateOpCostRuleDistributeItemsByPileSn in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(e));
            return false;
        }
        return true;
    }

    /**
     * @param opCostRuleDistributeEntityList
     * @return
     * @function 根据桩SN动态地批量修改桩的生效状态
     */
    @Override
    public Boolean updateOpCostRuleEfficientByPileSn(List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList) {
        log.info("========== updateOpCostRuleEfficientByPileSn in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(opCostRuleDistributeEntityList));
        try {
            if (CollUtil.isNotEmpty(opCostRuleDistributeEntityList)) {
                for (OpCostRuleDistributeEntity opCostRuleDistributeEntity : opCostRuleDistributeEntityList) {
                    LambdaUpdateWrapper<OpCostRuleDistributeEntity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();
                    lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientFlag, opCostRuleDistributeEntity.getEfficientFlag());
                    lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientFailureReason, opCostRuleDistributeEntity.getEfficientFailureReason());
                    lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getEfficientTime, opCostRuleDistributeEntity.getEfficientTime());
                    lambdaUpdateWrapper.set(OpCostRuleDistributeEntity::getUpdateTime, opCostRuleDistributeEntity.getUpdateTime());
                    lambdaUpdateWrapper.set(opCostRuleDistributeEntity.getSuccessFlag() != null, OpCostRuleDistributeEntity::getSuccessFlag, opCostRuleDistributeEntity.getSuccessFlag());
                    lambdaUpdateWrapper.eq(null != opCostRuleDistributeEntity.getRuleId(), OpCostRuleDistributeEntity::getRuleId, opCostRuleDistributeEntity.getRuleId());
                    lambdaUpdateWrapper.eq(OpCostRuleDistributeEntity::getPileSn, opCostRuleDistributeEntity.getPileSn());
                    lambdaUpdateWrapper.eq(OpCostRuleDistributeEntity::getDeleted, 0);
                    boolean result = this.update(lambdaUpdateWrapper);
                    log.info("================= the result of updateOpCostRuleEfficientByPileSn in the OpCostRuleDistributeRepositoryImpl function, the result:{}, the pileSn:{}", result, opCostRuleDistributeEntity.getPileSn());
                }
            }
        } catch (Exception e) {
            log.error("========== updateOpCostRuleEfficientByPileSn in the OpCostRuleDistributeRepositoryImpl function : {}", JSON.toJSONString(e));
            return false;
        }
        return true;
    }
}
