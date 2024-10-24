package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.repository.OpCostRuleDistributeRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.service.DistributeCostRuleService;
import com.autel.cloud.pile.base.domain.service.DistributeTimezoneService;
import com.autel.cloud.pile.base.domain.service.OpCostRuleDistributeService;
import com.autel.cloud.pile.base.domain.utils.CheckUtil;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.dto.PileStandInfoDTO;
import com.autel.cloud.pile.base.dto.SendEmailDownLoadHistoryDataDTO;
import com.autel.cloud.pile.base.dto.tariff.CostRuleDispatchPileSnDTO;
import com.autel.cloud.pile.base.enums.CostRuleDistributeEfficientFailureReason;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.device.PileTypeEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.exception.MessageSourceUtil;
import com.autel.cloud.pile.base.infrastructure.feign.CommonServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.ProtocolFeignClient;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpCostRuleDistributeEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.OcppLocationEVSEVO;
import com.autel.cloud.pile.base.vo.SimpleChargePileVO;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.vo.UserChargePileVO;
import com.autel.cloud.pile.user.api.dto.EmailSendDTO;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.SimpleInformationAboutCostModelRuleVO;
import com.autel.cloud.tariff.vo.TariffIssuedVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.StringUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;


/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录 业务逻辑层实现类
 */
@Service
@Slf4j
public class OpCostRuleDistributeServiceImpl implements OpCostRuleDistributeService {

    @Autowired
    private OpCostRuleDistributeRepository opCostRuleDistributeRepository;

    @Autowired
    @Lazy
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private ProtocolFeignClient protocolFeignClient;

    @Autowired
    private MonitorFeignClient monitorFeignClient;

    @Resource
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private HomePileFeignClient homePileFeignClient;

    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;

    @Autowired
    private DistributeCostRuleService distributeCostRuleService;

    @Autowired
    private DistributeTimezoneService distributeTimezoneService;

    /**
     * 引入线程池
     */
    @Autowired
    private ThreadPoolTaskExecutor threadPoolTaskExecutor;

    @Autowired
    private CommonServiceFeign commonServiceFeign;

    @Autowired
    private MessageSourceUtil messageSourceUtil;

    /**
     * 执行的步长
     */
    private final Integer STEP_SIZE = 100;


    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发新增
     */
    @Override
    public Result<Boolean> addOpCostRuleDistribute(OpCostRuleDistributeDTO opCostRuleDistributeDTO) {
        log.info("=========== addOpCostRuleDistribute in the OpCostRuleDistributeServiceImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTO));
        if (opCostRuleDistributeDTO == null || opCostRuleDistributeDTO.getRuleId() == null || StrUtil.isBlank(opCostRuleDistributeDTO.getPileSn())) {
            return Result.ofSucceed(false);
        }
        OpCostRuleDistributeEntity opCostRuleDistributeEntity = new OpCostRuleDistributeEntity();
        BeanUtil.copyProperties(opCostRuleDistributeDTO, opCostRuleDistributeEntity);
        Long userId = UserUtil.getUserId();
        Long currentTimeMillis = System.currentTimeMillis();
        opCostRuleDistributeEntity.setCreateBy(userId);
        opCostRuleDistributeEntity.setCreateTime(currentTimeMillis);
        opCostRuleDistributeEntity.setUpdateBy(userId);
        opCostRuleDistributeEntity.setUpdateTime(currentTimeMillis);
        opCostRuleDistributeEntity.setDeleted(0);
        return Result.ofSucceed(opCostRuleDistributeRepository.addOpCostRuleDistribute(opCostRuleDistributeEntity));
    }

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN码是否存在，批量新增或者修改计费规则下发记录
     */
    @Override
    public Result<Boolean> addOrUpdateOpCostRuleDistributeByPileSn(List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList) {

        log.info("=========== addOrUpdateOpCostRuleDistributeByPileSn in the OpCostRuleDistributeServiceImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTOList));

        Long currentTimeMillis = System.currentTimeMillis();
        AtomicReference<Long> userId = new AtomicReference<>(UserUtil.getUserId());
        if (CollUtil.isNotEmpty(opCostRuleDistributeDTOList)) {

            // 执行次数
            int count = opCostRuleDistributeDTOList.size() / this.STEP_SIZE + ((opCostRuleDistributeDTOList.size() % this.STEP_SIZE) == 0 ? 0 : 1);

            log.info("=========== 入参的集合的大小opCostRuleDistributeDTOList.size : {}, 执行次数count : {}", opCostRuleDistributeDTOList.size(), count);

            for (int i = 0; i < count; i++) {
                List<OpCostRuleDistributeDTO> opCostRuleDistributePage = opCostRuleDistributeDTOList.subList(i * this.STEP_SIZE, ((i + 1) * this.STEP_SIZE >= opCostRuleDistributeDTOList.size() ? (opCostRuleDistributeDTOList.size()) : ((i + 1) * this.STEP_SIZE)));
                Set<String> pileSnSet = opCostRuleDistributePage.stream().map(OpCostRuleDistributeDTO::getPileSn).collect(Collectors.toSet());
                // 通过桩SN的set集合来查询op_cost_rule_distribute表
                List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList = opCostRuleDistributeRepository.selectOpCostRuleDistributeListByPileSnSet(pileSnSet);

                threadPoolTaskExecutor.execute(() -> {

                    // 批量新增集合
                    Set<OpCostRuleDistributeEntity> opCostRuleDistributeEntitySet = new HashSet<>();

                    log.info("=======>>>>>> 当前线程：{}执行了", Thread.currentThread().getName());

                    opCostRuleDistributePage.forEach(opCostRuleDistributeDTO -> {
                        if (opCostRuleDistributeDTO == null || opCostRuleDistributeDTO.getRuleId() == null || StrUtil.isBlank(opCostRuleDistributeDTO.getPileSn())) {
                            return;
                        }
                        if (opCostRuleDistributeDTO.getCreateBy() != null) {
                            userId.set(opCostRuleDistributeDTO.getCreateBy());
                        }

                        OpCostRuleDistributeEntity saveOrUpdateBatchByEntity = new OpCostRuleDistributeEntity();

                        OpCostRuleDistributeEntity opCostRuleDistributeEntity = null;
                        if (CollUtil.isNotEmpty(opCostRuleDistributeEntityList)) {
                            opCostRuleDistributeEntity = opCostRuleDistributeEntityList.stream().filter(temp -> temp.getPileSn().equals(opCostRuleDistributeDTO.getPileSn())).findAny().orElse(null);
                        }
                        if (null == opCostRuleDistributeEntity) {
                            // 说明是新的桩绑定新的计费规则，此时需要插入新的记录
                            BeanUtil.copyProperties(opCostRuleDistributeDTO, saveOrUpdateBatchByEntity);
                            saveOrUpdateBatchByEntity.setCreateBy(userId.get());
                            saveOrUpdateBatchByEntity.setCreateTime(currentTimeMillis);
                            saveOrUpdateBatchByEntity.setUpdateBy(userId.get());
                            saveOrUpdateBatchByEntity.setUpdateTime(currentTimeMillis);
                            saveOrUpdateBatchByEntity.setDeleted(0);
                            opCostRuleDistributeEntitySet.add(saveOrUpdateBatchByEntity);
                        } else {
                            // 说明该桩已经绑定过计费规则，此时需要替换新的计费规则，执行更新语句
                            saveOrUpdateBatchByEntity.setId(opCostRuleDistributeEntity.getId());
                            saveOrUpdateBatchByEntity.setRuleId(opCostRuleDistributeDTO.getRuleId());
                            saveOrUpdateBatchByEntity.setSuccessFlag(opCostRuleDistributeDTO.getSuccessFlag());
                            saveOrUpdateBatchByEntity.setFailureReason(opCostRuleDistributeDTO.getFailureReason());
                            saveOrUpdateBatchByEntity.setDistributeTime(opCostRuleDistributeDTO.getDistributeTime());
                            saveOrUpdateBatchByEntity.setEfficientFailureReason(opCostRuleDistributeDTO.getEfficientFailureReason());
                            saveOrUpdateBatchByEntity.setEfficientFlag(opCostRuleDistributeDTO.getEfficientFlag());
                            saveOrUpdateBatchByEntity.setEfficientTime(opCostRuleDistributeDTO.getEfficientTime());
                            saveOrUpdateBatchByEntity.setCreateBy(userId.get());
                            saveOrUpdateBatchByEntity.setCreateTime(currentTimeMillis);
                            saveOrUpdateBatchByEntity.setUpdateBy(userId.get());
                            saveOrUpdateBatchByEntity.setUpdateTime(currentTimeMillis);

                            // 执行单个更新操作
                            opCostRuleDistributeRepository.updateOpCostRuleDistributeById(saveOrUpdateBatchByEntity);
                        }
                    });

                    log.info("=======>>>>>> opCostRuleDistributeEntitySet : {}", JSON.toJSONString(opCostRuleDistributeEntitySet));

                    // 执行批量新增操作
                    if (ObjectUtils.isNotEmpty(opCostRuleDistributeEntitySet)) {
                        opCostRuleDistributeRepository.saveBatchByEntitySet(opCostRuleDistributeEntitySet);
                    }
                });
            }
            return Result.ofSucceed(true);
        }
        return Result.ofSucceed(false);
    }

    /**
     * @param opCostRuleDistributeDTO
     * @return
     * @function 计费规则下发查询
     */
    @Override
    public Result<List<OpCostRuleDistributeDTO>> selectOpCostRuleDistributeList(OpCostRuleDistributeDTO opCostRuleDistributeDTO) {
        log.info("=========== selectOpCostRuleDistributeList in the OpCostRuleDistributeServiceImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTO));
        List<OpCostRuleDistributeDTO> response = new ArrayList<>();
        List<OpCostRuleDistributeEntity> costRuleDistributeEntityList = opCostRuleDistributeRepository.selectOpCostRuleDistributeList(opCostRuleDistributeDTO);
        costRuleDistributeEntityList.stream().forEach(opCostRuleDistributeEntity -> {
            OpCostRuleDistributeDTO temp = new OpCostRuleDistributeDTO();
            BeanUtil.copyProperties(opCostRuleDistributeEntity, temp);
            response.add(temp);
        });
        return Result.ofSucceed(response);
    }

    /**
     * @param pileSn
     * @return
     * @function 通过桩Sn重新下发计费规则
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> reIssueBillingRulesByPileSn(String pileSn) {
        // 根据桩SN查询桩的生效标识和生效失败的原因
        OpCostRuleDistributeDTO opCostRuleDistributeDTO = new OpCostRuleDistributeDTO();
        opCostRuleDistributeDTO.setPileSn(pileSn);
        List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList = opCostRuleDistributeRepository.selectFailedDistributeList(opCostRuleDistributeDTO);
        log.info("===========>>>>>>>>> reIssueBillingRulesByPileSn in the OpCostRuleDistributeServiceImpl function opCostRuleDistributeEntity : {}", JSON.toJSONString(opCostRuleDistributeEntityList));
        if (CollUtil.isNotEmpty(opCostRuleDistributeEntityList)) {
            List<CostRuleDispatchPileSnDTO> finalCostRuleDispatchPileSnDTOList = new ArrayList<>();
            opCostRuleDistributeEntityList.forEach(opCostRuleDistributeEntity -> {
                CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                costRuleDispatchPileSnDTO.setTariffId(opCostRuleDistributeEntity.getRuleId());
                costRuleDispatchPileSnDTO.setPileSn(opCostRuleDistributeEntity.getPileSn());
                finalCostRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
            });
            List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = this.filterNonThirdPileCostRule(finalCostRuleDispatchPileSnDTOList);
            if (CollUtil.isEmpty(costRuleDispatchPileSnDTOList)) {
                log.info("========= because there are not needed dispatch pile, return");
                return Result.ofSucceed(true);
            }
            Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = this.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
            List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get(BaseConstant.DISTRIBUTE);
            // 下发计费规则
            if (CollUtil.isNotEmpty(distributeCostRuleList)) {
                String evseSn = pileSn + "_" + "1";
                OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseRepository.getLocationEvseVOBySnAndGunNo(evseSn);
                CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                costRuleDispatchPileSnDTO.setPileSn(pileSn);
                costRuleDispatchPileSnDTO.setTariffId(distributeCostRuleList.get(0).getRuleId());
                costRuleDispatchPileSnDTO.setStationTimezone(ocppLocationEVSEVO.getTimeZone());
                // 远程调用计费规则下发接口，尝试重新进行计费规则下发的操作
                Result<Boolean> costRuleDispatchResult = protocolFeignClient.costRuleDispatch(costRuleDispatchPileSnDTO);
                log.info("===========>>>>>>>> reIssueBillingRulesByPileSn in the OpCostRuleDistributeServiceImpl function costRuleDispatchResult : {}", JSON.toJSONString(costRuleDispatchResult));
            }
            updateOpCostRuleDistribute(costRuleDistributeMap);
            return Result.ofSucceed(true);
        }
        return Result.ofSucceed(false);
    }

    /**
     * @param tariffId
     * @return
     * @function 通过计费规则ID重新下发计费规则
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<Boolean> reIssueBillingRulesByTariffId(Long tariffId) {
        // 根据计费规则ID查询桩的生效标识和生效失败的原因
        OpCostRuleDistributeDTO opCostRuleDistributeDTO = new OpCostRuleDistributeDTO();
        opCostRuleDistributeDTO.setRuleId(tariffId);
        List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList = opCostRuleDistributeRepository.selectFailedDistributeList(opCostRuleDistributeDTO);
        log.info("===========>>>>>>>>> reIssueBillingRulesByTariffId in the OpCostRuleDistributeServiceImpl function opCostRuleDistributeEntityList : {}", JSON.toJSONString(opCostRuleDistributeEntityList));
        // 要重新进行计费规则下发操作的桩的集合
        if (CollUtil.isNotEmpty(opCostRuleDistributeEntityList)) {
            List<CostRuleDispatchPileSnDTO> finalCostRuleDispatchPileSnDTOList = new ArrayList<>();
            opCostRuleDistributeEntityList.forEach(opCostRuleDistributeEntity -> {
                CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                costRuleDispatchPileSnDTO.setTariffId(tariffId);
                costRuleDispatchPileSnDTO.setPileSn(opCostRuleDistributeEntity.getPileSn());
                finalCostRuleDispatchPileSnDTOList.add(costRuleDispatchPileSnDTO);
            });
            List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList = this.filterNonThirdPileCostRule(finalCostRuleDispatchPileSnDTOList);
            if (CollUtil.isEmpty(costRuleDispatchPileSnDTOList)) {
                log.info("========= because there are not needed dispatch pile, return");
                return Result.ofSucceed(true);
            }
            Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap = this.wrapCostRuleDistribute(costRuleDispatchPileSnDTOList);
            List<OpCostRuleDistributeDTO> distributeCostRuleList = costRuleDistributeMap.get(BaseConstant.DISTRIBUTE);
            // 下发计费规则
            if (CollUtil.isNotEmpty(distributeCostRuleList)) {
                List<CostRuleDispatchPileSnDTO> costRuleBatchDispatchPileSnList = new ArrayList<>();
                distributeCostRuleList.stream().forEach(distributeDTO -> {
                    String evseSn = distributeDTO.getPileSn() + "_" + "1";
                    OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseRepository.getLocationEvseVOBySnAndGunNo(evseSn);
                    CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
                    costRuleDispatchPileSnDTO.setPileSn(distributeDTO.getPileSn());
                    costRuleDispatchPileSnDTO.setTariffId(distributeDTO.getRuleId());
                    costRuleDispatchPileSnDTO.setStationTimezone(ocppLocationEVSEVO.getTimeZone());
                    costRuleBatchDispatchPileSnList.add(costRuleDispatchPileSnDTO);
                });
                log.info("========== the costRuleBatchDispatchPileSnList before costRuleBatchDispatch invoked:{}", JSON.toJSONString(costRuleBatchDispatchPileSnList));
                Result<Boolean> dispatchResult = protocolFeignClient.costRuleBatchDispatch(costRuleBatchDispatchPileSnList);
                log.info("=========== the dispatchResult after costRuleBatchDispatch invoked:{}", dispatchResult);
            }
            updateOpCostRuleDistribute(costRuleDistributeMap);
            return Result.ofSucceed(true);
        }
        return Result.ofSucceed(false);
    }

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN动态地批量修改桩的下发状态
     */
    @Override
    public Result<Boolean> updateOpCostRuleDistributeByPileSn(List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList) {
        log.info("=========== updateOpCostRuleDistributeByPileSn in the OpCostRuleDistributeServiceImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTOList));
        Long currentTimeMillis = System.currentTimeMillis();
        List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList = new ArrayList<>();
        if (CollUtil.isNotEmpty(opCostRuleDistributeDTOList)) {
            for (OpCostRuleDistributeDTO opCostRuleDistributeDTO : opCostRuleDistributeDTOList) {
                if (opCostRuleDistributeDTO == null || opCostRuleDistributeDTO.getRuleId() == null || StrUtil.isBlank(opCostRuleDistributeDTO.getPileSn())) {
                    break;
                }
                OpCostRuleDistributeEntity opCostRuleDistributeEntity = new OpCostRuleDistributeEntity();
                BeanUtil.copyProperties(opCostRuleDistributeDTO, opCostRuleDistributeEntity);
                opCostRuleDistributeEntity.setUpdateTime(currentTimeMillis);
                opCostRuleDistributeEntityList.add(opCostRuleDistributeEntity);
            }
        }
        if (Boolean.TRUE.equals(opCostRuleDistributeRepository.updateOpCostRuleDistributeItemsByPileSn(opCostRuleDistributeEntityList))) {
            return Result.ofSucceed(true);
        } else {
            throw new MessageCodeException(PileBaseEnum.FAILED_TO_DELIVER_BILLING_RULES);
        }
    }

    /**
     * @param opCostRuleDistributeDTOList
     * @return
     * @function 根据桩SN动态地批量修改桩的生效状态
     */
    @Override
    public Result<Boolean> updateOpCostRuleEfficientByPileSn(List<OpCostRuleDistributeDTO> opCostRuleDistributeDTOList) {
        log.info("=========== updateOpCostRuleEfficientByPileSn in the OpCostRuleDistributeServiceImpl function : {}", JSON.toJSONString(opCostRuleDistributeDTOList));
        Long currentTimeMillis = System.currentTimeMillis();
        List<OpCostRuleDistributeEntity> opCostRuleDistributeEntityList = new ArrayList<>();
        if (CollUtil.isNotEmpty(opCostRuleDistributeDTOList)) {
            for (OpCostRuleDistributeDTO opCostRuleDistributeDTO : opCostRuleDistributeDTOList) {
                if (opCostRuleDistributeDTO == null || opCostRuleDistributeDTO.getRuleId() == null || StrUtil.isBlank(opCostRuleDistributeDTO.getPileSn())) {
                    break;
                }
                // todo 计费规则下发记录生效时，说明该计费规则一定是下发成功了，所以需要强制改变下发成功标识，将其设置为成功状态
                if (opCostRuleDistributeDTO != null
                        && opCostRuleDistributeDTO.getEfficientFlag() != null
                        && opCostRuleDistributeDTO.getEfficientFlag() == 1) {
                    opCostRuleDistributeDTO.setSuccessFlag(1);
                }

                OpCostRuleDistributeEntity opCostRuleDistributeEntity = new OpCostRuleDistributeEntity();
                BeanUtil.copyProperties(opCostRuleDistributeDTO, opCostRuleDistributeEntity);
                opCostRuleDistributeEntity.setUpdateTime(currentTimeMillis);
                opCostRuleDistributeEntityList.add(opCostRuleDistributeEntity);
            }
        }
        if (Boolean.TRUE.equals(opCostRuleDistributeRepository.updateOpCostRuleEfficientByPileSn(opCostRuleDistributeEntityList))) {
            return Result.ofSucceed(true);
        } else {
            throw new MessageCodeException(PileBaseEnum.FAILED_TO_TAKE_EFFECT_OF_BILLING_RULES);
        }
    }

    /**
     * @param pileSn
     * @return
     * @function 根据桩SN查询计费规则下发记录表，获得该桩的计费规则下发记录
     * （如果查不到说明是历史数据，此时认为该桩所绑定的计费规则是生效的）
     */
    @Override
    public Result<OpCostRuleDistributeDTO> selectOpCostRuleDistributeByPileSn(String pileSn) {
        log.info("=========== selectOpCostRuleDistributeByPileSn in the OpCostRuleDistributeServiceImpl function : {}", JSON.toJSONString(pileSn));
        OpCostRuleDistributeDTO opCostRuleDistributeDTO = new OpCostRuleDistributeDTO();
        OpCostRuleDistributeEntity opCostRuleDistributeEntity = opCostRuleDistributeRepository.selectByPileSn(pileSn);
        if (!Objects.isNull(opCostRuleDistributeEntity)) {
            opCostRuleDistributeDTO.setId(opCostRuleDistributeEntity.getId());
            opCostRuleDistributeDTO.setRuleId(opCostRuleDistributeEntity.getRuleId());
            opCostRuleDistributeDTO.setPileSn(pileSn);
            opCostRuleDistributeDTO.setSuccessFlag(opCostRuleDistributeEntity.getSuccessFlag());
            opCostRuleDistributeDTO.setFailureReason(opCostRuleDistributeEntity.getFailureReason());
            opCostRuleDistributeDTO.setDistributeTime(opCostRuleDistributeEntity.getDistributeTime());
            opCostRuleDistributeDTO.setEfficientFlag(opCostRuleDistributeEntity.getEfficientFlag());
            opCostRuleDistributeDTO.setEfficientFailureReason(opCostRuleDistributeEntity.getEfficientFailureReason());
            opCostRuleDistributeDTO.setEfficientTime(opCostRuleDistributeEntity.getEfficientTime());
        } else {
            // 查不到说明是历史数据，此时默认该桩绑定的计费规则是生效的
            opCostRuleDistributeDTO.setPileSn(pileSn);
            opCostRuleDistributeDTO.setSuccessFlag(1);
            opCostRuleDistributeDTO.setEfficientFlag(1);
        }
        return Result.ofSucceed(opCostRuleDistributeDTO);
    }


    /**
     * 过滤非第三方充电桩的计费规则下发记录
     *
     * @param costRuleDispatchPileSnDTOList
     * @return
     */
    @Override
    public List<CostRuleDispatchPileSnDTO> filterNonThirdPileCostRule(List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList) {
        log.info("=========== the costRuleDispatchPileSnDTOList in the filterNonThirdPileCostRule function:{}", JSON.toJSONString(costRuleDispatchPileSnDTOList));
        if (CollUtil.isEmpty(costRuleDispatchPileSnDTOList)) {
            return costRuleDispatchPileSnDTOList;
        }
        List<String> pileSnList = costRuleDispatchPileSnDTOList.stream().map(CostRuleDispatchPileSnDTO::getPileSn).collect(Collectors.toList());
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQuery = Wrappers.lambdaQuery();
        lambdaQuery.select(OpLocationPileEvseEntity::getId, OpLocationPileEvseEntity::getPileSn, OpLocationPileEvseEntity::getBrandId)
                .in(OpLocationPileEvseEntity::getPileSn, pileSnList)
                .eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> pileEvseEntityList = opLocationPileEvseRepository.list(lambdaQuery);
        log.info("=========== the pileEvseEntityList in the filterNonThirdPileCostRule function:{}", JSON.toJSONString(pileEvseEntityList));
        pileEvseEntityList.forEach(pileEvseEntity -> {
            // brand != 1 表示第三方桩，不下发计费规则
            if (null != pileEvseEntity.getBrandId() && 1 != pileEvseEntity.getBrandId()) {
                CostRuleDispatchPileSnDTO thirdPileCostRule = costRuleDispatchPileSnDTOList.stream().filter(costRuleDispatchPileSnDTO -> {
                    return costRuleDispatchPileSnDTO.getPileSn().equals(pileEvseEntity.getPileSn());
                }).findAny().orElse(null);
                if (null != thirdPileCostRule) {
                    costRuleDispatchPileSnDTOList.remove(thirdPileCostRule);
                }
            }
        });
        log.info("=========== the costRuleDispatchPileSnDTOList after filter in the filterNonThirdPileCostRule function:{}", JSON.toJSONString(costRuleDispatchPileSnDTOList));
        return costRuleDispatchPileSnDTOList;
    }

    /**
     * 封装计费规则下发记录
     *
     * @param
     * @param
     * @return
     */
    @Override
    public Map<String, List<OpCostRuleDistributeDTO>> wrapCostRuleDistribute(List<CostRuleDispatchPileSnDTO> costRuleDispatchPileSnDTOList) {
        Map<String, List<OpCostRuleDistributeDTO>> result = new HashMap<>();
        // 离线
        List<OpCostRuleDistributeDTO> offlineList = new ArrayList<>();
        // 需下发的充电记录
        List<OpCostRuleDistributeDTO> distributeList = new ArrayList<>();
        costRuleDispatchPileSnDTOList.forEach(costRuleDispatchPileSnDTO -> {
            String pileSn = costRuleDispatchPileSnDTO.getPileSn();
            Long tariffId = costRuleDispatchPileSnDTO.getTariffId();
            Result<String> pileStatusResult = monitorFeignClient.queryStatusByEvseSn(pileSn + "_" + "1");
            log.info("=========== the pileStatusResult:{}", pileStatusResult);
            // 充电桩未离线
            if (null != pileStatusResult && HttpStatus.SC_OK == pileStatusResult.getCode() && !EvseDeviceStatusEnum.DEFAULT.getName().equals(pileStatusResult.getData())) {
                OpCostRuleDistributeDTO costRuleDistributeDTO = new OpCostRuleDistributeDTO();
                costRuleDistributeDTO.setRuleId(tariffId);
                costRuleDistributeDTO.setPileSn(pileSn);
                costRuleDistributeDTO.setCreateBy(costRuleDispatchPileSnDTO.getUserId());
                costRuleDistributeDTO.setDistributeTime(null);
                distributeList.add(costRuleDistributeDTO);
            } else {
                OpCostRuleDistributeDTO costRuleDistributeDTO = new OpCostRuleDistributeDTO();
                costRuleDistributeDTO.setRuleId(tariffId);
                costRuleDistributeDTO.setPileSn(pileSn);
                costRuleDistributeDTO.setSuccessFlag(0);
                costRuleDistributeDTO.setFailureReason(CostRuleDistributeEfficientFailureReason.THE_PILE_IS_OFFLINE.getCode() + "");
                costRuleDistributeDTO.setDistributeTime(null);
                costRuleDistributeDTO.setCreateBy(costRuleDispatchPileSnDTO.getUserId());
                offlineList.add(costRuleDistributeDTO);
            }
        });
        result.put("offline", offlineList);
        result.put(BaseConstant.DISTRIBUTE, distributeList);
        log.info("============ the result of wrapCostRuleDistribute function:{}", JSON.toJSONString(result));
        return result;
    }

    @Override
    public Result<Boolean> retryDistributePublicCostRule(String pileSn, Long tariffId, Long userId) {
        log.info("============ the pileSn:{}, tariffId:{} in the retryDistributePublicCostRule", pileSn, tariffId);
        List<OpCostRuleDistributeDTO> costRuleDistributeDTOList = new ArrayList<>();
        OpCostRuleDistributeDTO costRuleDistributeDTO = new OpCostRuleDistributeDTO();
        costRuleDistributeDTO.setRuleId(tariffId);
        costRuleDistributeDTO.setPileSn(pileSn);
        costRuleDistributeDTO.setDistributeTime(null);
        costRuleDistributeDTO.setCreateBy(userId);
        costRuleDistributeDTOList.add(costRuleDistributeDTO);
        String evseSn = pileSn + "_" + "1";
        OcppLocationEVSEVO ocppLocationEVSEVO = opLocationEvseRepository.getLocationEvseVOBySnAndGunNo(evseSn);
        CostRuleDispatchPileSnDTO costRuleDispatchPileSnDTO = new CostRuleDispatchPileSnDTO();
        costRuleDispatchPileSnDTO.setPileSn(pileSn);
        costRuleDispatchPileSnDTO.setTariffId(tariffId);
        if (ocppLocationEVSEVO != null) {
            costRuleDispatchPileSnDTO.setStationTimezone(ocppLocationEVSEVO.getTimeZone());
        }
        // 远程调用计费规则下发接口，尝试重新进行计费规则下发的操作
        Result<Boolean> costRuleDispatchResult = protocolFeignClient.costRuleDispatch(costRuleDispatchPileSnDTO);
        log.info("===========>>>>>>>> retryDistributeCostRule in the OpCostRuleDistributeServiceImpl function costRuleDispatchResult : {}", JSON.toJSONString(costRuleDispatchResult));
        Result<Boolean> result = addOrUpdateOpCostRuleDistributeByPileSn(costRuleDistributeDTOList);
        return result;
    }

    @Override
    public Result<Boolean> retryDistributeTariff(String pileSn) {
        log.info("========= the pileSn in the retryDistributeTariff of OpCostRuleDistributeServiceImpl:{}", pileSn);
        try {
            Result<PileStandInfoDTO> pileStandInfoDTOResult = deviceServiceFeign.queryStandInfo(pileSn);
            log.info("============ the pileStandInfoDTOResult of the pile:{}", JSON.toJSONString(pileStandInfoDTOResult));
            if (null != pileStandInfoDTOResult && HttpStatus.SC_OK == pileStandInfoDTOResult.getCode() && null != pileStandInfoDTOResult.getData()) {
                PileStandInfoDTO pileStandInfoDTO = pileStandInfoDTOResult.getData();
//                ChargePileDTO chargePileInfoVO = deviceServiceFeign.pileDetail(pileSn).getData();
//                log.info("============ the chargePileInfoVO of the pile:{}", JSON.toJSONString(chargePileInfoVO));
                if (null == pileStandInfoDTO.getThirdPart() || pileStandInfoDTO.getThirdPart()) {
                    log.info("========== because the pile is thirdPart in the retryDistributeTariff function, return. the pileSn:{}", pileSn);
                    return Result.ofSucceed(Boolean.TRUE);
                }
                if (null == pileStandInfoDTO.getUsage() || DeviceTypeEnum.NO_ATTRIBUTE_PILE.getValue().equals(pileStandInfoDTO.getUsage())) {
                    log.info("========== because the pile‘s usage is null or 0 in the retryDistributeTariff function, return. the pileSn:{}", pileSn);
                    return Result.ofSucceed(Boolean.TRUE);
                }
                //todo 是否需要过滤只有计费规则下发记录的充电桩？
                OpCostRuleDistributeDTO opCostRuleDistributeDTO = new OpCostRuleDistributeDTO();
                opCostRuleDistributeDTO.setPileSn(pileSn);
                List<OpCostRuleDistributeDTO> costRuleDistributeList = this.selectOpCostRuleDistributeList(opCostRuleDistributeDTO).getData();
                log.info("=================>>>>>>>>>>> costRuleDistributeList in the OpCostRuleDistributeController function : {}", JSON.toJSONString(costRuleDistributeList));
                OpCostRuleDistributeDTO costRuleDistributeDTO = null;
                if (CollUtil.isNotEmpty(costRuleDistributeList)) {
                    costRuleDistributeDTO = costRuleDistributeList.get(0);
                }
                // 桩离线/超时才需重试
//            if (CostRuleDistributeEfficientFailureReason.THE_PILE_IS_OFFLINE.getCode().toString().equals(costRuleDistributeDTO.getEfficientFailureReason())
//                    || CostRuleDistributeEfficientFailureReason.THE_PILE_IS_OFFLINE.getCode().toString().equals(costRuleDistributeDTO.getFailureReason())
//                    || CostRuleDistributeEfficientFailureReason.TIME_OUT.getCode().toString().equals(costRuleDistributeDTO.getEfficientFailureReason())
//                    || CostRuleDistributeEfficientFailureReason.TIME_OUT.getCode().toString().equals(costRuleDistributeDTO.getFailureReason())) {
                // 家桩
                if (DeviceTypeEnum.HOME_PILE.getValue().equals(pileStandInfoDTO.getUsage())) {
                    UserChargePileVO userChargePileVO;
                    try {
                        userChargePileVO = homePileFeignClient.getUserPile(pileSn).getData();//todo CodeReview TTL 缓存 + mq 删除
                        log.info("========== the userChargePileVO in the retryDistributeTariff:{}", userChargePileVO);
                    } catch (Exception ex) {
                        log.error("========== invoke getUserPile exception in them retryDistributeTariff", ex);
                        return Result.ofSucceed(Boolean.FALSE);
                    }
                    double price = 0D;
                    if (userChargePileVO != null && userChargePileVO.getPriceEnable() != null
                            && userChargePileVO.getPriceEnable()) {
                        price = userChargePileVO.getPrice() == null ? 0 : userChargePileVO.getPrice();
                    }
                    if (null != userChargePileVO && StrUtil.isNotBlank(userChargePileVO.getUserId())) {
                        Long userId = Long.parseLong(userChargePileVO.getUserId());
                        distributeTimezoneService.distributeHomePileTimezone(pileSn, userId);
                    }
                    if (null != costRuleDistributeDTO) {
//                        return distributeCostRuleService.retryDistributeHomeCostRule(pileSn, costRuleDistributeDTO.getRuleId(), costRuleDistributeDTO.getCreateBy(), price);
                        return distributeCostRuleService.retryDistributeHomeCostRule(pileSn, userChargePileVO.getTariffId(), costRuleDistributeDTO.getCreateBy(), price);
                    }
                } else {
                    distributeTimezoneService.distributePublicPileTimezone(pileSn);
                    return Result.ofSucceed(distributeCostRuleService.businessPileOnlineIssueBillingRule(pileSn, false));
                }
            }
        } catch (Exception ex) {
            log.error("============ 获取充电桩类型异常 in the retryDistributeTariff function", ex);
        }
//            }

        return Result.ofSucceed(Boolean.TRUE);
    }

    private Boolean updateOpCostRuleDistribute(Map<String, List<OpCostRuleDistributeDTO>> costRuleDistributeMap) {
        List<OpCostRuleDistributeDTO> costRuleDistributeDTOList = new ArrayList<>();
        costRuleDistributeMap.forEach((tempKey, tempValue) -> {
            if (CollUtil.isNotEmpty(tempValue)) {
                costRuleDistributeDTOList.addAll(tempValue);
            }
        });
        Result<Boolean> result = addOrUpdateOpCostRuleDistributeByPileSn(costRuleDistributeDTOList);
        return result.getData();
    }

    @Override
    public Result<Boolean> sendEmailDownLoadHistoryData(SendEmailDownLoadHistoryDataDTO sendEmailDownLoadHistoryDataDTO) {
        if (!CheckUtil.checkEmail(sendEmailDownLoadHistoryDataDTO.getEmail())) {
            throw new MessageCodeException(PileBaseEnum.EMAIL_FORMAT_ERROR);
        }
        return Result.ofSucceed(sendDownLoadHistory(sendEmailDownLoadHistoryDataDTO));
    }

    private Boolean sendDownLoadHistory(SendEmailDownLoadHistoryDataDTO sendEmailDownLoadHistoryDataDTO) {
        if (StringUtils.isBlank(sendEmailDownLoadHistoryDataDTO.getPileSn())) {
            return false;
        }
        //暂时随便写一个网址，后续更改
        String url = "baidu.com";
        String content = String.format(messageSourceUtil.getMessage("DOWNLOAD_HISTORYDATA_EMAIL"),sendEmailDownLoadHistoryDataDTO.getPileSn()) + url;;
        EmailSendDTO emailSendDto = new EmailSendDTO();
        emailSendDto.setEmail(sendEmailDownLoadHistoryDataDTO.getEmail());
        emailSendDto.setContent(content);
        emailSendDto.setSubject("Historical data download");
        commonServiceFeign.sendEmail(emailSendDto);
        return true;
    }

}
