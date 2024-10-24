package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.bill.dto.BatchSendMsgV2DTO;
import com.autel.cloud.pile.bill.dto.SendMsgDto;
import com.autel.cloud.ws.dto.BatchSendMsgDTO;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * @ClassName WxProxyClient
 * @Author A22121
 * @Description
 * @Date 2022/3/9 16:50
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient(value = "ws-proxy")
public interface WxProxyClient {
    /**
     * 发送消息
     * @param sendMsgDTO
     * @return
     */
    @PostMapping(value="/message/send")
    Result<Boolean> sendMsg(@RequestBody SendMsgDto sendMsgDTO);
    /**
     * 批量发送消息
     * @param batchSendMsgV2DTO
     * @return
     */
    @PostMapping("/message/batch/send/v2")
    Result<Boolean> batchSendMsgV2(@RequestBody BatchSendMsgV2DTO batchSendMsgV2DTO);
}
