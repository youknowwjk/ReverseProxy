package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseOperationRepository;
import com.autel.cloud.pile.base.infrastructure.feign.DataServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChangePileAbilityDto;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileResetDto;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileUnlockDto;
import com.autel.cloud.pile.base.infrastructure.feign.dto.TransactionDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @ClassName OpLocationEvseOperationRepositoryImpl
 * @Author A22121
 * @Description
 * @Date 2022/4/27 20:03
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
@Slf4j
public class OpLocationEvseOperationRepositoryImpl implements OpLocationEvseOperationRepository {

    private final DataServiceFeign dataServiceFeign;

    public OpLocationEvseOperationRepositoryImpl(DataServiceFeign dataServiceFeign) {
        this.dataServiceFeign = dataServiceFeign;
    }

    @Override
    public Boolean updateVersion(String evseSn) {
        try {
            String pileSn = evseSn.split("_")[0];
            Result<Boolean> ret = dataServiceFeign.remoteUpdate(pileSn);
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository updateVersion exception = ", e);
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean batchUpdateVersion(List<String> evseSnList) {
        try {
            List<String> pileSn = evseSnList.stream()
                    .map(evseSn -> evseSn.split("_")[0]).collect(Collectors.toList());
            Result<Boolean> ret = dataServiceFeign.remoteUpdateBatch(pileSn);
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository batchUpdateVersion exception = ", e);
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean backendStop(String evseSn, String busId, Long userId) {
        try {
            String pileSn = evseSn.split("_")[0];
            String connectorId = evseSn.split("_")[1];
            TransactionDto dto = TransactionDto.builder().transactionId(busId)
                    .chargePileSn(pileSn).gunNo(Integer.valueOf(connectorId)).userId(userId.toString()).build();
            Result<Boolean> ret = dataServiceFeign.backendStop(dto);
            log.info("OpLocationEvseOperationRepositoryImpl backendStop TransactionDto = " + JSON.toJSONString(dto));
            log.info("OpLocationEvseOperationRepositoryImpl backendStop result = " + JSON.toJSONString(ret));
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository backendStop exception = ", e);
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean disable(String evseSn) {
        try {
            String pileSn = evseSn.split("_")[0];
            String connectorId = evseSn.split("_")[1];
            ChangePileAbilityDto dto = ChangePileAbilityDto.builder().sn(pileSn)
                    .connectorId(connectorId).type("Inoperative").build();
            Result<Boolean> ret = dataServiceFeign.changeAbility(dto);
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository disable exception = ", e);
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean able(String evseSn) {
        try {
            String pileSn = evseSn.split("_")[0];
            String connectorId = evseSn.split("_")[1];
            ChangePileAbilityDto dto = ChangePileAbilityDto.builder().sn(pileSn)
                    .connectorId(connectorId).type("Operative").build();
            Result<Boolean> ret = dataServiceFeign.changeAbility(dto);
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository able exception = ", e);
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean reset(String evseSn) {
        try {
            String pileSn = evseSn.split("_")[0];
            ChargePileResetDto dto = ChargePileResetDto.builder().sn(pileSn).type("Hard").build();
            Result<Boolean> ret = dataServiceFeign.reset(dto);
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository reset exception = ", e);
        }
        return Boolean.FALSE;
    }

    @Override
    public Boolean unlock(String evseSn) {
        try {
            String pileSn = evseSn.split("_")[0];
            String connectorId = evseSn.split("_")[1];
            ChargePileUnlockDto dto = ChargePileUnlockDto.builder().sn(pileSn).connectorId(connectorId).build();
            Result<Boolean> ret = dataServiceFeign.unlockPile(dto);
            return ret.getData();
        } catch (Exception e) {
            log.error("OpLocationEvseOperationRepository unlock exception = ", e);
        }
        return Boolean.FALSE;
    }
}
