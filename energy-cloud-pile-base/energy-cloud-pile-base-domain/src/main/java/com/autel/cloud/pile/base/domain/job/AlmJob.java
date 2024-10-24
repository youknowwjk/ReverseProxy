package com.autel.cloud.pile.base.domain.job;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.context.DistributeContext;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.List;
import java.util.Random;

/**
 * @Author temp
 * @Date 2023/2/1 14:27
 */
@Component
@Slf4j
public class AlmJob {
    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;
    @Resource
    private DistributeContext distributeContext;
    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @XxlJob("almTask")
    public ReturnT<String> almTask(String param) {
        XxlJobLogger.log("触发ALM功率调整开始");
        List<OpLocationPileGroupEntity> almEntityList = opLocationPileGroupService.findAllRoot(null);
        XxlJobLogger.log("almEntityList=" + JSON.toJSONString(almEntityList));
        if (CollectionUtils.isEmpty(almEntityList)) {
            return ReturnT.SUCCESS;
        }
        //群组上限50%-100%之间随机
        almEntityList.stream().forEach(entity -> {
            //redis读取
            String key = "ENERGY:SMART:CHARGING_ALM:CACHE:%s";
            String redisValue = stringRedisTemplate.opsForValue().get(String.format(key, entity.getId()));
            BigDecimal chargingUp;
            if (StringUtils.isEmpty(redisValue)) {
                chargingUp = entity.getChargingUp();
                stringRedisTemplate.opsForValue().set(String.format(key, entity.getId()), chargingUp.toString());
            } else {
                chargingUp = BigDecimal.valueOf(Long.valueOf(redisValue));
            }
            //计算随机值
            Random random = new Random();
            int bound = random.nextInt(50);
            BigDecimal newValue = (BigDecimal.ONE.subtract(BigDecimal.valueOf(bound).divide(BigDecimal.valueOf(100)))).multiply(chargingUp);
            XxlJobLogger.log("newValue=" + newValue + ",entity=" + JSON.toJSONString(entity));
            //更新随机值
            entity.setChargingUp(newValue);
            entity.setUpdatedAt(System.currentTimeMillis());
            Long result = opLocationPileGroupService.updateRoot(entity);
            //下发配置
            //OpLocationPileGroupStrategyDTO params = new OpLocationPileGroupStrategyDTO();
            //params.setRootId(result);
            //distributeContext.getStrategy(entity.getAllocationStrategy()).execute(params);
        });
        return ReturnT.SUCCESS;
    }
}
