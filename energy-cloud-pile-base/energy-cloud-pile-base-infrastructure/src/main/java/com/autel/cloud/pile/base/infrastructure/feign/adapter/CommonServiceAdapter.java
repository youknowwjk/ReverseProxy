package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.commonserver.dto.LarkRobotNotifyDto;
import com.autel.cloud.commonserver.feign.CommonServiceFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;

/**
 * @Author: x20025(liujinxing)
 * @CreateTime: 2024-02-29  15:33
 * @Description:
 */
@Slf4j
@Component
public class CommonServiceAdapter {
    @Autowired
    private CommonServiceFeignClient commonServiceFeignClient;

    public void robotFeiShuNotify(LarkRobotNotifyDto larkRobotNotifyDto) {
        try {
            log.info("commonServiceFeignClient.robotFeiShuNotify request is {}", JSON.toJSONString(larkRobotNotifyDto));
            Result<Map> result = commonServiceFeignClient.robotFeiShuNotify(larkRobotNotifyDto);
            log.info("commonServiceFeignClient.robotFeiShuNotify response is {}", JSON.toJSONString(result));
        }catch (Exception e){
            log.error("commonServiceFeignClient.robotFeiShuNotify error is:",e);
        }
    }


}
