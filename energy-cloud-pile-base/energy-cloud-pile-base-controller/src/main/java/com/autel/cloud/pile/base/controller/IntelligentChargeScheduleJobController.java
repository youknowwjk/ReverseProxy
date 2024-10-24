package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.data.IntelligentChargeJobQuery;
import com.autel.cloud.pile.base.domain.data.JobQuery;
import com.autel.cloud.pile.base.domain.service.impl.IntelligentChargingScheduling;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileBillServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileHomeServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.SmartChargeServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.WebHookServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleJobMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.IntelligentChargeScheduleRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.MemberSmartChargeConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.MemberSmartChargeConfigEntity;
import com.autel.cloud.pile.base.vo.ChargingDetailVO;
import com.autel.cloud.pile.base.vo.UserVehicle;
import com.autel.cloud.pile.bill.dto.IntelligentChargeVO;
import com.autel.cloud.pile.bill.enums.OrderStatusEnum;
import com.autel.cloud.pile.bill.event.OrderCompletedEvent;
import com.autel.cloud.pile.bill.vo.EnergyBillDetailVO;
import com.autel.cloud.push.enums.JobStatus;
import com.autel.cloud.smart.charge.constant.SmartChargeStatus;
import com.autel.cloud.smart.charge.dto.IntelligentChargeOrderDTO;
import com.autel.cloud.smart.charge.vo.ChargingGraphVO;
import com.autel.cloud.smart.charge.vo.ChargingProfileSettingVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import static com.autel.cloud.pile.bill.constant.RedisKeys.BUSINESS_INTELLIGENT_CHARGE_SCHEDULE;


@RestController
@RequestMapping(value = "/IntelligentChargeJob")
@Api(tags = "公共智能充电配置JOB")
@Slf4j
@Validated
public class IntelligentChargeScheduleJobController {

    @Resource
    private IntelligentChargingScheduling intelligentChargingScheduling;

    @Resource
    private IntelligentChargeScheduleJobMapper intelligentChargeScheduleJobMapper;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private PileHomeServiceAdapter pileHomeServiceAdapter;

    @Resource
    private MemberSmartChargeConfigMapper memberSmartChargeConfigMapper;

    @Resource
    private WebHookServiceAdapter webHookServiceAdapter;

    @Resource
    private SmartChargeServiceAdapter smartChargeServiceAdapter;

    @Resource
    private PileBillServiceAdapter pileBillServiceAdapter;

    @PostMapping("/close")
    @ApiOperation(value = "停止充电关闭智能充电job", notes = "停止充电关闭智能充电job")
    public Result<IntelligentChargeOrderDTO> closeJob(@RequestBody OrderCompletedEvent stopChargeEvent) {
        log.info("closeJob stopChargeEvent: {}", JSON.toJSONString(stopChargeEvent));
        IntelligentChargeOrderDTO intelligentChargeOrderDTO = new IntelligentChargeOrderDTO();
        try {
            String format = String.format(BUSINESS_INTELLIGENT_CHARGE_SCHEDULE, stopChargeEvent.getOrderSeq());
            stringRedisTemplate.expire(format, 1, TimeUnit.SECONDS);
            IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.valueOf(stopChargeEvent.getOrderSeq()));
            if (Objects.nonNull(intelligentChargeScheduleJob)) {
                intelligentChargeOrderDTO.setEnable(1 == intelligentChargeScheduleJob.getSmartChargeSwitch());
                intelligentChargeScheduleJob.setStatus(JobStatus.FINISH.getStatus());
                intelligentChargeScheduleJobMapper.updateById(intelligentChargeScheduleJob);
                // 如果激活过 那么及时一下 节省费用
                if (intelligentChargeOrderDTO.isEnable()) {
                    ChargingGraphVO chargingProfile = smartChargeServiceAdapter.graphList(intelligentChargeScheduleJob.getEvseSn(), intelligentChargeScheduleJob.getOrderSeq());

                    List<ChargingDetailVO> socList = chargingProfile.getSocList();
                    if (!CollectionUtils.isEmpty(socList)) {
                        BigDecimal savedEnergyFee = BigDecimal.ZERO;
                        for (ChargingDetailVO chargingDetailVO : socList) {
                            long millis = System.currentTimeMillis();
                            if (chargingDetailVO.getStartPeriod() <= millis) {
                                savedEnergyFee = chargingDetailVO.getSavedEnergyFee();
                            } else {
                                break;
                            }
                        }
                        intelligentChargeOrderDTO.setSavedEnergyFee(savedEnergyFee);
                    }
                }
                String msg = String.format("订单 %s 节省费用: %s", stopChargeEvent.getOrderNumber(), intelligentChargeOrderDTO.getSavedEnergyFee());
                webHookServiceAdapter.sendMessage(stopChargeEvent.getOrderSeq(), stopChargeEvent.getEvseSn(), stopChargeEvent.getOrderNumber(), intelligentChargeScheduleJob.getVehicleId(), msg);
            }
        } catch (Exception e) {
            log.error("close intelligentChargeScheduleJob", e);
        }
        intelligentChargeOrderDTO.setBillId(stopChargeEvent.getBillId());
        intelligentChargeOrderDTO.setOrderSeq(stopChargeEvent.getOrderSeq());
        intelligentChargeOrderDTO.setOrderNumber(stopChargeEvent.getOrderNumber());
        log.info("closeJob result : {}", JSON.toJSONString(intelligentChargeOrderDTO));
        return Result.ofSucceed(intelligentChargeOrderDTO);
    }

    @PostMapping("/running")
    @ApiOperation(value = "判断桩是否在充电中", notes = "判断桩是否在充电中")
    public Result<Integer> running(@RequestBody IntelligentChargeJobQuery intelligentChargeJobQuery) {
        log.info("intelligentChargeJobQuery running: {}", JSON.toJSONString(intelligentChargeJobQuery));
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.parseLong(intelligentChargeJobQuery.getTransactionId()));
        if (Objects.nonNull(intelligentChargeScheduleJob)) {
            log.info("intelligentChargeJobQuery running: {}", intelligentChargeScheduleJob);
            return Result.ofSucceed(intelligentChargeScheduleJob.getStatus());
        }
        return Result.ofSucceed(JobStatus.FAILURE_VEHICLE.getStatus());
    }

    @PostMapping("/jobStatus")
    @ApiOperation(value = "查看订单job状态", notes = "查看订单job状态")
    public Result<IntelligentChargeVO> jobStatus(@RequestBody IntelligentChargeJobQuery intelligentChargeJobQuery) {
        log.info("intelligentChargeJobQuery jobStatus: {}", JSON.toJSONString(intelligentChargeJobQuery));
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectById(Long.parseLong(intelligentChargeJobQuery.getTransactionId()));
        if (Objects.nonNull(intelligentChargeScheduleJob)) {
            log.info("intelligentChargeJobQuery jobStatus: {}", intelligentChargeScheduleJob);
            IntelligentChargeVO intelligentChargeVO = new IntelligentChargeVO();
            intelligentChargeVO.setJobStatus(intelligentChargeScheduleJob.getStatus());
            intelligentChargeVO.setSmartChargeStatus(intelligentChargeScheduleJob.getSmartChargeStatus());
            Long profileId = intelligentChargeScheduleJob.getProfileId();
            MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = memberSmartChargeConfigMapper.selectById(profileId);
            if (Objects.nonNull(memberSmartChargeConfigEntity)) {
                intelligentChargeVO.setTargetSOC(memberSmartChargeConfigEntity.getProfile().getTargetSOC());
            }
            BigDecimal soc = intelligentChargingScheduling.pushAppSOCFromMonitor(intelligentChargeScheduleJob);
            intelligentChargeVO.setSoc(soc);// 桩和车上都没有读到soc 那么从算法预估的soc 获取一个
            return Result.ofSucceed(intelligentChargeVO);
        }
        return Result.ofSucceed(null);
    }


    @PostMapping("/lastTriggerJob")
    @ApiOperation(value = "倒计时结束最后一次触发job", notes = "倒计时结束最后一次触发job")
    public Result<IntelligentChargeScheduleJob> lastTriggerJob(@RequestBody JobQuery jobQuery) {
        log.info("lastTriggerJob jobStatus: {}", JSON.toJSONString(jobQuery));
        LambdaQueryWrapper<IntelligentChargeScheduleJob> wrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class);
        wrapper.eq(IntelligentChargeScheduleJob::getOrderSeq, jobQuery.getTransactionId());
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectOne(wrapper);
        Optional.ofNullable(intelligentChargeScheduleJob).ifPresent(job -> {
            if (JobStatus.TRY_LINKED.getStatus() == job.getStatus()) {
                intelligentChargingScheduling.doIdentify(job);
            }
        });
        // to do other
        log.info("timeout lastTriggerJob IntelligentChargeScheduleJob: {}", JSON.toJSONString(intelligentChargeScheduleJob));
        return Result.ofSucceed(intelligentChargeScheduleJob);
    }

    @PostMapping("/switchSmartCharge")
    @ApiOperation(value = "开启/关闭 公共智能充电", notes = "开启/关闭 公共智能充电")
    public Result<Boolean> switchSmartCharge(@RequestBody JobQuery jobQuery) {
        intelligentChargingScheduling.switchSmartCharge(jobQuery);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @PostMapping("/queryUserVehicleByOrderSeq")
    @ApiOperation(value = "根据订单transactionId查询该订单正在充电的车辆", notes = "根据订单transactionId查询该订单正在充电的车辆")
    public Result<UserVehicle> queryUserVehicleByOrderSeq(@RequestBody JobQuery jobQuery) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> wrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class);
        wrapper.eq(IntelligentChargeScheduleJob::getOrderSeq, jobQuery.getTransactionId());
        IntelligentChargeScheduleJob intelligentChargeScheduleJob = intelligentChargeScheduleJobMapper.selectOne(wrapper);
        if (Objects.nonNull(intelligentChargeScheduleJob) && Objects.nonNull(intelligentChargeScheduleJob.getProfileId()) && intelligentChargeScheduleJob.getProfileId() > 0) {
            MemberSmartChargeConfigEntity memberSmartChargeConfigEntity = memberSmartChargeConfigMapper.selectById(intelligentChargeScheduleJob.getProfileId());
            if (Objects.nonNull(memberSmartChargeConfigEntity) && Objects.nonNull(memberSmartChargeConfigEntity.getProfile()) && Objects.nonNull(memberSmartChargeConfigEntity.getProfile().getVehicleId())) {
                UserVehicle userVehicle = pileHomeServiceAdapter.getUserVehicleByUserVehicleId(memberSmartChargeConfigEntity.getProfile().getVehicleId());
                return Result.ofSucceed(userVehicle);
            }
        }
        return Result.ofSucceed();
    }

    @GetMapping("/haveSmartChargingJob")
    @ApiOperation(value = "根据UserId查询是否存在公共智能充电", notes = "根据UserId查询是否存在公共智能充电")
    public Result<Boolean> haveSmartChargingJob(@RequestParam("userId") Long userId) {
        LambdaQueryWrapper<IntelligentChargeScheduleJob> wrapper = Wrappers.lambdaQuery(IntelligentChargeScheduleJob.class);
        wrapper.eq(IntelligentChargeScheduleJob::getUserId, userId)
                .eq(IntelligentChargeScheduleJob::getSmartChargeSwitch, 1)
                .ne(IntelligentChargeScheduleJob::getStatus, JobStatus.FINISH.getStatus());// 该订单还在充电
        //.eq(IntelligentChargeScheduleJob::getSmartChargeStatus, SmartChargeStatus.SWITCH_SUCCESS_STATUS); // 切换成功的
        List<IntelligentChargeScheduleJob> intelligentChargeScheduleJobs = intelligentChargeScheduleJobMapper.selectList(wrapper);
        log.info("haveSmartChargingJob {}", JSON.toJSONString(intelligentChargeScheduleJobs));
        if (CollectionUtils.isEmpty(intelligentChargeScheduleJobs)) {
            return Result.ofSucceed(false);// 没有充电订单那么 返回false 没有智能充电订单订单
        }
        for (IntelligentChargeScheduleJob job : intelligentChargeScheduleJobs) {
            EnergyBillDetailVO energyBillDetailVO = pileBillServiceAdapter.selectBillBaseByBusId(job.getOrderSeq());
            if (energyBillDetailVO.getOrderStatus() == OrderStatusEnum.START_SUCCESS.getValue()) {
                return Result.ofSucceed(true);
            }
        }
        return Result.ofSucceed(false);
    }
}
