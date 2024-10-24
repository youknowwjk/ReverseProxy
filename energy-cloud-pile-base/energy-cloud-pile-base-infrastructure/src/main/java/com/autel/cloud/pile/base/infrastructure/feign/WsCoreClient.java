package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.feign.dto.SendMsgDTO;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

/**
 * ws client接口类
 *
 * @author A19061
 */
@FeignClient(value = "ws-proxy")
public interface WsCoreClient {

    /**
     * 发送消息
     *
     * @param sendMsgDTO
     * @return
     */
    @PostMapping(value = "/message/send")
    Result<Boolean> sendMsg(@RequestBody SendMsgDTO sendMsgDTO);

    /**
     * 获取用户是否在线
     * @param userId
     * @return
     */
    @GetMapping(value = "/user/query-online/{userId}")
    Result<Boolean>  userIsOnline(@PathVariable String userId);


    @ApiOperation(value = "按协议查询用户是否在线", notes = "proto字段默认为 OCPP")
    @GetMapping("/user/query-online/{userId}")
    Result<Boolean> userIsOnline(@PathVariable(value = "userId") String userId,
                                 @RequestParam(value = "proto", defaultValue = "OCPP") String proto);
}
