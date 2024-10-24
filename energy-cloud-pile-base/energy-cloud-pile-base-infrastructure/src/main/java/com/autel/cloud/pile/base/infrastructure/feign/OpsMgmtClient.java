package com.autel.cloud.pile.base.infrastructure.feign;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.OperationActionLog;
import com.autel.cloud.mgnt.vo.DnmsgLatestValueVO;
import com.autel.cloud.pile.base.dto.PileConfigDTO;
import com.autel.cloud.pile.base.dto.PileRandomDelayTimeDTO;
import com.autel.cloud.pile.base.dto.britishAct.QueryDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.*;
import com.autel.cloud.pile.base.vo.ConfigBritishActVO;
import com.autel.cloud.pile.base.vo.ConfigOffPeakVO;
import com.autel.cloud.pile.base.vo.FirmwareRecordDetailsVO;
import com.autel.cloud.pile.base.vo.britishAct.ops.EVSCPParamVO;
import com.autel.cloud.pile.base.vo.britishAct.ops.EVSCPSecurityLogListVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.ApiOperation;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * @author Zoe Liu
 * @data 2022/7/11
 */
@FeignClient("ops-mgmt")
public interface OpsMgmtClient {

    @PostMapping("/pile/ops/save")
    Result<Boolean> saveOpsPile(@RequestBody PileInfoDTO pileInfoDTO);

    @PostMapping("/pile/base/edit")
    Result<Boolean> editPile(@RequestBody UpdatePileV2DTO updatePileV2DTO);

    @DeleteMapping("/pile/deleteOpsPile")
    Result<Boolean> deleteOpsPile(@RequestBody PileDeleteDTO pileDeleteDTO);

    @PostMapping("/pile/base/editPileLocation")
    Result<Boolean> baseEditPileLocation(@RequestBody UpdatePileLocationDTO locationDTO);

    @PostMapping("/pile/ops/batchSaveOpsPile")
    Result<Boolean> batchSaveOpsPile(@RequestBody List<PileInfoDTO> pileInfoDTOs);

    @PostMapping("/remoteConfig/send-britishAct")
    Result<Boolean> sendBritishAct(@RequestBody ConfigBritishActVO configBritishActVO);

   /* @ApiOperation("根据key和sn，查询桩的对应配置信息")
    @PostMapping("/pileConfig/queryConfigBySn")
    Result<PileRandomDelayTimeDTO> queryConfigBySn(@RequestParam("sn") String sn, @RequestParam("key") String key);

    @ApiOperation("根据key和sn，查询桩的对应配置信息")
    @PostMapping("/pileConfig/queryConfigBySn")
    Result<PileOffPeakHourParamDTO> queryConfigBySnforDefaultCharging(@RequestParam("sn") String sn, @RequestParam("key") String key);*/

    @PostMapping(value = "/remoteConfig/send-OffPeak")
    @ApiOperation(value = "英国法案-高峰时间")
    Result<Boolean> sendOffPeakHourParam(@RequestBody ConfigOffPeakVO configOffPeakVO);

    @GetMapping(value = "/common/dnmsg/value")
    @ApiOperation(value = "获取保存在云端的最后一次设置记录")
    Result<DnmsgLatestValueVO> getLatestSetting(@RequestParam("serviceId") String serviceId, @RequestParam("sn") String sn);

    @GetMapping("/firmware/page/pile")
    @ApiOperation(value = "分页查询版本比较", notes = "分页查询版本比较")
    Result<Page<FirmwareRecordDetailsVO>> recordDetailPage(
            @RequestParam(value = "sn", required = false) String sn
            , @Min(value = 1, message = "页数不能小于1") @RequestParam(value = "page", defaultValue = "1") Integer page
            , @Max(value = 50, message = "每页条数不能大于50") @RequestParam(value = "pageSize", defaultValue = "10") Integer pageSize
            , @RequestParam(value = "update", required = false) Integer update);

    @ApiOperation("根据key和sn，查询桩的对应配置信息")
    @PostMapping("/pileConfig/queryConfigBySnNoCheck")
    Result<PileRandomDelayTimeDTO> queryConfigBySn(@RequestParam("sn") String sn, @RequestParam("key") String key);

    @ApiOperation("根据key和sn，查询桩的对应配置信息")
    @PostMapping("/pileConfig/queryConfigBySnNoCheck")
    Result<PileOffPeakHourParamDTO> queryConfigBySnforDefaultCharging(@RequestParam("sn") String sn, @RequestParam("key") String key);

    @ApiOperation("英国法案-查询相关功能使能与否指令下发")
    @PostMapping("/remoteConfig/commandEvscp")
    Result<Boolean> commandEvscp(@RequestBody QueryDTO queryDTO);

    @ApiOperation("英国法案-查询相关功能使能与否")
    @PostMapping("/remoteConfig/queryEvscp")
    Result<EVSCPParamVO> queryEvscp(@RequestParam("sn") String sn);

    @ApiOperation("英国法案-查询桩非高峰时段（默认充电时段）指令下发")
    @PostMapping("/remoteConfig/commandOffPeakHour")
    Result<Boolean> commandOffPeakHour(@RequestBody QueryDTO queryDTO);

    @ApiOperation("英国法案-查询桩非高峰时段（默认充电时段）")
    @PostMapping("/remoteConfig/queryOffPeakHour")
    Result<PileOffPeakHourParamDTO> queryOffPeakHour(@RequestParam("sn") String sn);

    @ApiOperation("英国法案-获取所有安全事件日志应答指令下发")
    @PostMapping("/remoteConfig/commandSecurityLogList")
    Result<Boolean> commandSecurityLogList(@RequestBody QueryDTO queryDTO);

    @ApiOperation("英国法案-查询所有安全事件日志")
    @PostMapping("/remoteConfig/querySecurityLogList")
    Result<EVSCPSecurityLogListVO> querySecurityLogList(@RequestParam("sn") String sn);
}
