package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.RemoteStopDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.*;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import javax.validation.Valid;
import java.util.List;

/**
 * 调用老系统的接口可以这样写，新系统直接引用api包就可以
 *
 * @ClassName DataServiceFeign
 * @Author A22121
 * @Description
 * @Date 2022/4/27 17:04
 * @Version 0.0.1-SNAPSHOT
 */
@FeignClient("data-service")
public interface DataServiceFeign {
    @PostMapping("/transaction/backend-start")
    @ApiOperation(value = "后台远程开始充电", notes = "临时start接口")
    Result<Object> backendStart(@RequestBody @Valid StartTransactionDto startTransactionDto);

    @PostMapping("/transaction/backend-stop")
    @ApiOperation(value = "后台远程停止充电", notes = "临时stop接口")
    Result<Boolean> backendStop(@RequestBody @Valid TransactionDto transactionDto);

    @PostMapping("/transaction/start")
    @ApiOperation(value = "开始充电", notes = "开始充电")
    Result<Object> start(@RequestBody @Valid StartTransactionDto startTransactionDto);

    @PostMapping("/manager/device/pile/unlock")
    @ApiOperation(value = "解、锁枪", notes = "解、锁枪")
    Result<Boolean> unlockPile(@RequestBody @Valid ChargePileUnlockDto chargePileUnlockDto);

    @PostMapping("/manager/device/pile/changeAbility")
    @ApiOperation(value = "禁用、解禁", notes = "禁用、解禁")
    Result<Boolean> changeAbility(@RequestBody @Valid ChangePileAbilityDto changePileAbilityDto);

    @PostMapping("/manager/device/pile/reset")
    @ApiOperation(value = "复位、重启", notes = "复位、重启")
    Result<Boolean> reset(@RequestBody @Valid ChargePileResetDto chargePileResetDto);

    @PostMapping("/manager/device/pile/firmware/update")
    @ApiOperation(value = "根据桩序列号进行远程固件升级", notes = "根据桩序列号进行远程固件升级")
    Result<Boolean> remoteUpdate(@RequestParam(value = "sn") String sn);

    @PostMapping("/firmware/update/batch")
    @ApiOperation(value = "根据桩序列号列表进行远程固件升级", notes = "根据桩序列号列表进行远程固件升级")
    Result<Boolean> remoteUpdateBatch(@RequestBody List<String> snList);

    @PostMapping({"/transaction/backend-stop"})
    Result<Boolean> stop(@RequestBody RemoteStopDTO remoteStopDTO);

    @PostMapping({"/device/pile/{sn}/modify/business"})
    Result<Object> addPile(@PathVariable("sn") String var1, @RequestParam("pin") String var2);

    @PostMapping({"/device/pile/{sn}/modify/home"})
    Result<Boolean> delPile(@PathVariable("sn") String var1);
}
