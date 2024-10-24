package com.autel.cloud.pile.base.domain.job;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.service.OpPowerLimitService;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPowerLimitEntity;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import com.xxl.job.core.log.XxlJobLogger;
import com.xxl.job.core.util.ShardingUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @ClassName PowerLimitJob
 * @Author A22121
 * @Description
 * @Date 2022/7/11 9:14
 * @Version 0.0.1-SNAPSHOT
 */
@Slf4j
@Component
public class PowerLimitJob {

    @Autowired
    private OpPowerLimitService opPowerLimitService;

    /**
     * 12小时运行一次，循环所有的桩
     *
     * @param param
     * @return
     */
    @XxlJob(value = "pilePowerLimitTask")
    public ReturnT<String> pilePowerLimitTask(String param) {
        XxlJobLogger.log("每日功率控制下发开始");
        List<OpPowerLimitEntity> entityList = opPowerLimitService.findAllTurnOn();
        XxlJobLogger.log("entityList={}", JSON.toJSONString(entityList));
        if (CollectionUtils.isEmpty(entityList)) {
            XxlJobLogger.log("当前无开启功率控制下发的桩");
            return ReturnT.SUCCESS;
        }
        // 分片参数
        int shardIndex = ShardingUtil.getShardingVo().getIndex();
        int shardTotal = ShardingUtil.getShardingVo().getTotal();
        XxlJobLogger.log("分片参数：当前分片序号 = {}, 总分片数 = {}", shardIndex, shardTotal);
        List<Long> pileIds = new ArrayList<>();
        entityList.stream().forEach(entity -> {
            if (entity.getCombinationId() % shardTotal == shardIndex) {
                pileIds.add(entity.getCombinationId());
            }
        });
        XxlJobLogger.log("当前分片序号 = {}, 命中分片桩数 = {}", shardIndex, pileIds.size());
        if (pileIds.isEmpty()) {
            XxlJobLogger.log("当前分片无命中的桩");
            return ReturnT.SUCCESS;
        }
        pileIds.stream().forEach(pileId -> opPowerLimitService.deliverySetting(pileId));
        XxlJobLogger.log("每日功率控制下发结束");
        return ReturnT.SUCCESS;
    }
}
