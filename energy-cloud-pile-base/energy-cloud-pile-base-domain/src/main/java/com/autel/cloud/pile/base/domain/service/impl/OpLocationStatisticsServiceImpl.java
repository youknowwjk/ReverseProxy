package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationStatisticsService;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseRealTimeDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseStatisticsDTO;
import com.autel.cloud.pile.base.dto.OpLocationOperationInfoDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * @ClassName OpLocationStatisticsServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/4/18 20:48
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class OpLocationStatisticsServiceImpl implements OpLocationStatisticsService {

    private final OpLocationEvseRepository opLocationEvseRepository;
    private final OpLocationRepository opLocationRepository;
    public OpLocationStatisticsServiceImpl(OpLocationEvseRepository opLocationEvseRepository,
                                           OpLocationRepository opLocationRepository) {
        this.opLocationEvseRepository = opLocationEvseRepository;
        this.opLocationRepository = opLocationRepository;
    }

    @Override
    public Result<OpLocationEvseStatisticsDTO> getEvse(OpLocationEvseDTO opLocationEvseDTO) {
        try {
            if (opLocationEvseDTO == null) {
                return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("param is null"));
            }
            OpLocationEvseStatisticsDTO opLocationEvseStatisticsDTO = new OpLocationEvseStatisticsDTO();
            List<OpLocationEvseRealTimeDTO> opLocationEvseRealTimeDTOList = opLocationEvseRepository
                    .getEvseByLocationId(opLocationEvseDTO.getLocationId());
            opLocationEvseStatisticsDTO.setOpLocationEvseDTOList(opLocationEvseRealTimeDTOList);
            // 接下来进行统计
            Set<String> pileSnSet = new TreeSet<>();
            opLocationEvseRealTimeDTOList.forEach(evseDTO -> {
                calcValue(evseDTO, opLocationEvseStatisticsDTO);
                pileSnSet.add(evseDTO.getEvseSn().split("_")[0]);
            });
            opLocationEvseStatisticsDTO.setEvseTotal(opLocationEvseRealTimeDTOList.size());
            opLocationEvseStatisticsDTO.setEvsePileTotal(pileSnSet.size());
            return Result.ofSucceed(opLocationEvseStatisticsDTO);
        } catch (Exception e) {
            log.error("OpLocationStatisticsServiceImpl getEvse exception = ", e);
        }
        return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
    }

    /**
     * 计算状态数值
     * @param evseDTO
     * @param opLocationEvseStatisticsDTO
     */
    private void calcValue(OpLocationEvseRealTimeDTO evseDTO, OpLocationEvseStatisticsDTO opLocationEvseStatisticsDTO) {
        switch (EvseDeviceStatusEnum.getEnumByName(evseDTO.getState())) {
            case AVAILABLE:
            case PREPARING:
                opLocationEvseStatisticsDTO.increaseFree();
                break;
            case SUSPENDED_EVSE:
            case SUSPENDED_EV:
            case UNAVAILABLE:
            case FINISHING:
                opLocationEvseStatisticsDTO.increaseProhibit();
                break;
            case CHARGING:
                opLocationEvseStatisticsDTO.increaseCharging();
                break;
            case FAULTED:
                opLocationEvseStatisticsDTO.increaseProblem();
                break;
            case RESERVED:
                opLocationEvseStatisticsDTO.increaseAppoint();
                break;
            case DEFAULT:
                opLocationEvseStatisticsDTO.increaseOffLine();
                break;
        }
    }

    @Override
    public Result<OpLocationOperationInfoDTO> locationOperationInfo(Long locationId) {
        try {
            if(locationId == null || locationId < 1) {
                return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
            }
            // 获取场站运营数据
            OpLocationOperationInfoDTO infoDTO = opLocationRepository.getLocationOperationInfo(locationId);
            return Result.ofSucceed(infoDTO);
        } catch (Exception e) {
            log.error("OpLocationStatisticsServiceImpl locationOperationInfo exception = ", e);
        }
        return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
    }

    @Override
    public Result<Map<String, List<String>>> locationOperationInfoList(Long locationId) {
        try {
            List<OpLocationOperationInfoDTO> infoDTOList = opLocationRepository.getLocationOperationInfoListForDay(30, locationId);
            Map<String, List<String>> chartMap = new HashMap<>(16);
            List<String> chargeTimesList = new ArrayList<>();
            List<String> chargeCapacityList = new ArrayList<>();
            List<String> chargeOrderList = new ArrayList<>();
            List<String> chargeIncomeList = new ArrayList<>();
            List<String> operationDateList = new ArrayList<>();
            infoDTOList.forEach(info -> {
                chargeTimesList.add(info.getChargeTimes().toString());
                chargeCapacityList.add(info.getChargeCapacity().toString());
                chargeOrderList.add(info.getChargeOrder());
                chargeIncomeList.add(info.getChargeIncome().toString());
                operationDateList.add(info.getOperationDate());
            });
            chartMap.put("chargeTimesList", chargeTimesList);
            chartMap.put("chargeCapacityList", chargeCapacityList);
            chartMap.put("chargeOrderList", chargeOrderList);
            chartMap.put("chargeIncomeList", chargeIncomeList);
            chartMap.put("operationDateList", operationDateList);
            return Result.ofSucceed(chartMap);
        } catch (Exception e) {
            log.error("OpLocationStatisticsServiceImpl locationOperationInfoList exception = ", e);
            return Result.ofFailed(ResultCodeEnum.INTERNAL_SERVER_ERROR);
        }
    }
}








