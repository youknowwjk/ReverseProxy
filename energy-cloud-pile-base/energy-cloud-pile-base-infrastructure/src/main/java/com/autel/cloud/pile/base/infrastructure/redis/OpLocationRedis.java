package com.autel.cloud.pile.base.infrastructure.redis;

import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import org.springframework.stereotype.Repository;

import java.util.HashMap;
import java.util.Map;

/**
 * @ClassName OpLocationRedis
 * @Author A22121
 * @Description
 * @Date 2022/4/15 11:05
 * @Version 0.0.1-SNAPSHOT
 */
@Repository
public class OpLocationRedis extends AbstractRedis {

    /**
     * 查询场站运营信息
     *
     * @param date
     * @param locationId
     * @return
     */
    public Map<String, String> getLocationChargeInfo(String date, Long locationId) {
        String key = RedisKeyConstant.getHashPileBaseLocationChargeInfo(date, locationId);
        Map<String, String> chargeInfoMap = new HashMap<>(8);
        buildMap(key, chargeInfoMap);
        return chargeInfoMap;
    }
}
