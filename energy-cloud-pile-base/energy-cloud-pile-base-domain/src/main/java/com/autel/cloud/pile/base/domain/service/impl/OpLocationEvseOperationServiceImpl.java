package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.common.util.LocaleResultUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.convert.OpLocationEvseConvert;
import com.autel.cloud.pile.base.domain.repository.OpEvseChargingThresholdRepository;
import com.autel.cloud.pile.base.domain.repository.OpEvseSocThresholdRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseOperationRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationEvse;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseOperationService;
import com.autel.cloud.pile.base.dto.OpEvseChargingThresholdDTO;
import com.autel.cloud.pile.base.dto.OpEvseInfoDTO;
import com.autel.cloud.pile.base.dto.OpEvseSocThresholdDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseChargingThresholdEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseSocThresholdEntity;
import com.autel.cloud.pile.base.vo.OpEvseEnableVO;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.feign.IOrderServiceFeign;
import com.autel.cloud.pile.bill.feign.OCPPFeignClient;
import com.autel.cloud.pile.bill.feign.OcpiSessionFeignClient;
import com.autel.cloud.pile.bill.vo.OrderVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.List;

/**
 * @ClassName OpLocationEvseOperationServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/4/27 16:27
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class OpLocationEvseOperationServiceImpl implements OpLocationEvseOperationService {

    private final OpLocationEvseOperationRepository opLocationEvseOperationRepository;

    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;

    private final OpEvseChargingThresholdRepository opEvseChargingThresholdRepository;

    private final OpEvseSocThresholdRepository opEvseSocThresholdRepository;

    @Resource
    private IOrderServiceFeign orderServiceFeign;

    @Resource
    private OcpiSessionFeignClient ocpiSessionFeignClient;

    @Resource
    private MonitorFeignClient monitorFeignClient;

    @Resource
    private OCPPFeignClient ocppFeignClient;

    @Autowired
    private OpLocationEvse opLocationEvse;


    public OpLocationEvseOperationServiceImpl(OpLocationEvseOperationRepository opLocationEvseOperationRepository, OpEvseChargingThresholdRepository opEvseChargingThresholdRepository, OpEvseSocThresholdRepository opEvseSocThresholdRepository) {
        this.opLocationEvseOperationRepository = opLocationEvseOperationRepository;
        this.opEvseChargingThresholdRepository = opEvseChargingThresholdRepository;
        this.opEvseSocThresholdRepository = opEvseSocThresholdRepository;
    }

    @Override
    public Result<Boolean> updateVersion(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        if (opLocationEvseRepository.getEvseByEvseSn(evseSn) == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        Boolean ret = opLocationEvseOperationRepository.updateVersion(evseSn);
        if (Boolean.TRUE.equals(ret)) {
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<Boolean> batchUpdateVersion(List<String> evseSnList) {
        if (CollectionUtils.isEmpty(evseSnList)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        Boolean ret = opLocationEvseOperationRepository.batchUpdateVersion(evseSnList);
        if (Boolean.TRUE.equals(ret)) {
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<Boolean> backendStop(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(evseSn);
        if (opEvseInfoDTO == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        // 获取正在充电的订单号
        // 获取当前充电信息
        Result<OrderVO> getCurrentOrderInfoRet = orderServiceFeign.findLatestOrderByEvseSn(evseSn);
        log.info("OpLocationEvseOperationServiceImpl.backendStop getCurrentOrderInfoRet {}",
                JSON.toJSONString(getCurrentOrderInfoRet));
        if (getCurrentOrderInfoRet == null || getCurrentOrderInfoRet.getData() == null
                || !OrderStatusEnum.START_SUCCESS.getValue().equals(getCurrentOrderInfoRet.getData().getOrderStatus())) {
            log.error("OpLocationEvseOperationServiceImpl.backendStop getCurrentOrderInfoRet BillInfoForEvseVO {}",
                    JSON.toJSONString(getCurrentOrderInfoRet));
            return Result.ofFailed(LocaleResultUtil.result("evse.operation.notCharging"));
        }

        OrderVO orderVO = getCurrentOrderInfoRet.getData();
        String busId = orderVO.getOrderSeq();
        Long userId = orderVO.getUserId();
        if (StringUtils.isBlank(busId)) {
            log.error("OpLocationEvseOperationServiceImpl.backendStop busId is blank");
            return Result.ofFailed(LocaleResultUtil.result("evse.operation.notCharging"));
        }

        // 充电枪状态
        Result<String> pileStatusResult = monitorFeignClient.queryStatusByEvseSn(evseSn);
        log.info("=========== the evse status: {} of evseSn: {}", pileStatusResult, evseSn);
        if (null != pileStatusResult && HttpStatus.SC_OK == pileStatusResult.getCode()) {
            String evseStatus = pileStatusResult.getData();
            // 充电桩不在"充电中"，强制关闭充电订单
            if (!(EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(evseStatus)
                    || EvseDeviceStatusEnum.SUSPENDED_EVSE.getName().equalsIgnoreCase(evseStatus)
                    || EvseDeviceStatusEnum.SUSPENDED_EV.getName().equalsIgnoreCase(evseStatus))) {
                Result<Boolean> closeResult = ocppFeignClient.closeBill(busId);
                log.info("========== the close result: {} of orderSeq: {} on evseSn: {}", closeResult, busId, evseSn);
                return closeResult;
            }
        }

        Boolean ret = opLocationEvseOperationRepository.backendStop(evseSn, busId, userId);
        if (Boolean.TRUE.equals(ret)) {
            log.error("OpLocationEvseOperationServiceImpl.backendStop call backendStop is failed");
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<Boolean> disable(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }

        // 增加判断，如果是禁用桩，则其所有枪的状态为ocpp状态的Available 或者Faulted 才可以禁用，否则提示充电设备操作异常
        if(evseSn.contains("_")&&"0".equals(evseSn.split("_")[1])){
            if(!opLocationEvse.checkAllEvseEnable(evseSn.split("_")[0])){
                return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
            }
        }


        Boolean ret = opLocationEvseOperationRepository.disable(evseSn);
        if (Boolean.TRUE.equals(ret)) {
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<Boolean> able(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        Boolean ret = opLocationEvseOperationRepository.able(evseSn);
        if (Boolean.TRUE.equals(ret)) {
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<Boolean> reset(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        if (opLocationEvseRepository.getEvseByEvseSn(evseSn) == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        Boolean ret = opLocationEvseOperationRepository.reset(evseSn);
        if (Boolean.TRUE.equals(ret)) {
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<Boolean> unlock(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        if (opLocationEvseRepository.getEvseByEvseSn(evseSn) == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        Boolean ret = opLocationEvseOperationRepository.unlock(evseSn);
        if (Boolean.TRUE.equals(ret)) {
            return Result.ofSucceed(true);
        }
        return Result.ofFailed(LocaleResultUtil.result(BaseConstant.EVSE_OPERATION_EXCEPTION));
    }

    @Override
    public Result<OpEvseChargingThresholdDTO> getChargingThreshold(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(evseSn);
        if (opEvseInfoDTO == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        LambdaQueryWrapper<OpEvseChargingThresholdEntity> queryWrapper = Wrappers.lambdaQuery(OpEvseChargingThresholdEntity.class)
                .eq(OpEvseChargingThresholdEntity::getDeleted, Boolean.FALSE)
                .eq(OpEvseChargingThresholdEntity::getEvseId, opEvseInfoDTO.getId());
        OpEvseChargingThresholdEntity entity = opEvseChargingThresholdRepository.getOne(queryWrapper);
        return Result.ofSucceed(OpLocationEvseConvert.toOpEvseChargingThresholdDTO(entity));
    }

    @Override
    public Result<Boolean> setChargingThreshold(OpEvseChargingThresholdDTO opEvseChargingThresholdDTO) {
        if (opEvseChargingThresholdDTO == null) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        LambdaQueryWrapper<OpEvseChargingThresholdEntity> queryWrapper = Wrappers.lambdaQuery(OpEvseChargingThresholdEntity.class)
                .eq(OpEvseChargingThresholdEntity::getDeleted, Boolean.FALSE)
                .eq(OpEvseChargingThresholdEntity::getEvseId, opEvseChargingThresholdDTO.getEvseId());
        OpEvseChargingThresholdEntity entity = opEvseChargingThresholdRepository.getOne(queryWrapper);
        OpLocationEvseConvert.toOpEvseChargingThresholdEntity(entity, opEvseChargingThresholdDTO);
        return Result.ofSucceed(opEvseChargingThresholdRepository.saveOrUpdate(entity));
    }

    @Override
    public Result<OpEvseSocThresholdDTO> getSocThreshold(String evseSn) {
        if (StringUtils.isBlank(evseSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(evseSn);
        if (opEvseInfoDTO == null) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        LambdaQueryWrapper<OpEvseSocThresholdEntity> queryWrapper = Wrappers.lambdaQuery(OpEvseSocThresholdEntity.class)
                .eq(OpEvseSocThresholdEntity::getDeleted, Boolean.FALSE)
                .eq(OpEvseSocThresholdEntity::getEvseId, opEvseInfoDTO.getId());
        OpEvseSocThresholdEntity entity = opEvseSocThresholdRepository.getOne(queryWrapper);
        return Result.ofSucceed(OpLocationEvseConvert.toOpEvseSocThresholdDTO(entity));
    }

    @Override
    public Result<Boolean> setSocThreshold(OpEvseSocThresholdDTO opEvseSocThresholdDTO) {
        if (opEvseSocThresholdDTO == null) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        LambdaQueryWrapper<OpEvseSocThresholdEntity> queryWrapper = Wrappers.lambdaQuery(OpEvseSocThresholdEntity.class)
                .eq(OpEvseSocThresholdEntity::getDeleted, Boolean.FALSE)
                .eq(OpEvseSocThresholdEntity::getEvseId, opEvseSocThresholdDTO.getEvseId());
        OpEvseSocThresholdEntity entity = opEvseSocThresholdRepository.getOne(queryWrapper);
        OpLocationEvseConvert.toOpEvseSocThresholdEntity(entity, opEvseSocThresholdDTO);
        return Result.ofSucceed(opEvseSocThresholdRepository.saveOrUpdate(entity));
    }

    @Override
    public Result<List<OpEvseEnableVO>> getStatusList(String pileSn) {
        return Result.ofSucceed(opLocationEvse.getAbleDisAbleList(pileSn));
    }
}
