package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.data.IntelligentChargeJobQuery;
import com.autel.cloud.pile.base.domain.data.SmartChargeJobQuery;
import com.autel.cloud.pile.base.domain.service.IntelligentChargeScheduleJobService;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleJobMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.MemberSmartChargeConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.MemberSmartChargeConfigEntity;
import com.autel.cloud.pile.bill.dto.IntelligentChargeVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestBody;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.Objects;

@Slf4j
@Service
public class IntelligentChargeScheduleJobServiceImpl implements IntelligentChargeScheduleJobService {

    @Resource
    private IntelligentChargeScheduleJobMapper intelligentChargeScheduleJobMapper;

    @Resource
    private MemberSmartChargeConfigMapper memberSmartChargeConfigMapper;

    @Resource
    private IntelligentChargingScheduling intelligentChargingScheduling;


    @Override
    public IntelligentChargeVO jobStatus(@RequestBody SmartChargeJobQuery smartChargeJobQuery) {
        log.info("SmartChargeJobQuery jobStatus: {}", JSON.toJSONString(smartChargeJobQuery));
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.parseLong(smartChargeJobQuery.getTransactionId()));
        if (Objects.nonNull(intelligentChargeScheduleJob) && intelligentChargeScheduleJob.getEvseSn().equalsIgnoreCase(smartChargeJobQuery.getEvseSn())) {
            log.info("SmartChargeJobQuery jobStatus: {}", intelligentChargeScheduleJob);
            IntelligentChargeVO intelligentChargeVO = new IntelligentChargeVO();
            intelligentChargeVO.setJobStatus(intelligentChargeScheduleJob.getStatus());
            intelligentChargeVO.setSmartChargeStatus(intelligentChargeScheduleJob.getSmartChargeStatus());
            Long profileId = intelligentChargeScheduleJob.getProfileId();
            MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = memberSmartChargeConfigMapper.selectById(profileId);
            if (Objects.nonNull(memberSmartChargeConfigEntity)) {
                intelligentChargeVO.setTargetSOC(memberSmartChargeConfigEntity.getProfile().getTargetSOC());
                intelligentChargeVO.setStartSOC(memberSmartChargeConfigEntity.getProfile().getStartSOC());
            }
            BigDecimal soc = intelligentChargingScheduling.pushAppSOCFromMonitor(intelligentChargeScheduleJob);
            intelligentChargeVO.setSoc(soc);// 桩和车上都没有读到soc 那么从算法预估的soc 获取一个
            return intelligentChargeVO;
        }
        return null;
    }
}
