package com.autel.cloud.pile.base.controller;

import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.LocalDateTimeUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.HomeService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.TimePowerDTO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.bill.dto.ChargePowerDTO;
import com.autel.cloud.pile.bill.dto.EmsGroupPowerDTO;
import com.autel.cloud.pile.bill.dto.SumEnergyDTO;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.smartbi.feign.SmartBiFeignClient;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.Valid;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * @Author A22282
 * @Date 2024/1/23 10:42
 */
@RestController
@RequestMapping("/home")
@Api(tags = "首页数据统计", value = "首页数据统计")
@Slf4j
@Validated
public class EmsScreenInfoController {

    @Autowired
    HomeService homeService;

    @Autowired
    SmartBiFeignClient smartBiFeignClient;

    @Autowired
    OpLocationPileGroupService opLocationPileGroupService;

    @Resource
    private IBillFeignClient billFeignClient;

    @PostMapping("/queryEmsTimePower")
    @ApiOperation(value = "Ems群组实时充电功率", notes = "Ems群组实时充电功率")
    public Result<EmsGroupPowerDTO> queryEmsTimePower(Long groupId) {
        return Result.ofSucceed(this.homeService.queryEmsTimePower(groupId));
    }

    @PostMapping("/queryEmsEvseStateCount")
    @ApiOperation(value = "Ems群组实时枪状态", notes = "Ems群组实时枪状态")
    public Result<OpLocationEvseStateCountResultVO> queryEmsEvseStateCount(Long groupId) {
        List<OpLocationEvseStateCountVO> vos = this.homeService.queryEmsEvseStateCount(groupId);
        OpLocationEvseStateCountResultVO result = new OpLocationEvseStateCountResultVO();
        result.setGroupId(groupId);
        result.setList(vos);
        return Result.ofSucceed(result);
    }

    @PostMapping("/lastWeekIncome")
    @ApiOperation(value = "最近一周收入和手续费")
    public Result<EMSPowerUsageVo> lastWeekIncome(@RequestParam(value = "groupId", required = false, defaultValue = "") String groupId) {
        if (ObjectUtils.isEmpty(groupId)){
            return null;
        }
        EMSPowerUsageVo vo = new EMSPowerUsageVo();
        //获取桩数据
        PileGroupDetailV3VOcopy detail = opLocationPileGroupService.queryDetailForEms(Long.valueOf(groupId));
        //赋值
        List<String> pileSns=getAllPileSns(detail);
        vo.setPileSnList(pileSns);
        vo.setGroupId(groupId);
        log.info("周年统计的列表:{}", JSON.toJSONString(vo.getPileSnList()));
        Result<EMSPowerUsageVo> lastYearIncome= smartBiFeignClient.lastWeekIncome(vo);
//        //计算7天之前的数据
//        LocalDateTime now = LocalDateTime.now();
//        String endTime = LocalDateTimeUtil.format(now, DatePattern.NORM_DATETIME_FORMATTER);
//        String startTime = LocalDateTimeUtil.format(now.minusDays(7), DatePattern.NORM_DATETIME_FORMATTER);
//        SumEnergyDTO sumEnergyDTO = new SumEnergyDTO();
//        sumEnergyDTO.setEndTime(endTime);
//        sumEnergyDTO.setStartTime(startTime);
//        sumEnergyDTO.setPileSns(pileSns);
//        Result<EnergyBillVO> energyBillVO=billFeignClient.getSumEnergy(sumEnergyDTO);
//        if(energyBillVO.getCode() == 200 && energyBillVO.getData() != null){
//            // TODO 减去一周的成本
//            EnergyBillVO data = energyBillVO.getData();
//        }
        //解析返回结果并赋值
        EMSPowerUsageVo result = JSON.parseObject(JSON.toJSONString(lastYearIncome.getData()), EMSPowerUsageVo.class);
        result.setExtendInfo(JSON.toJSONString(result));
        return Result.ofSucceed(result);
    }

    @PostMapping("/lastYearIncome")
    @ApiOperation(value = "最近一年收入和手续费")
    Result<EMSPowerUsageVo> lastYearIncome(@RequestParam(value = "groupId", required = false, defaultValue = "") String groupId) {
        if (ObjectUtils.isEmpty(groupId)){
            return null;
        }
        EMSPowerUsageVo vo = new EMSPowerUsageVo();
        //获取桩数据
        PileGroupDetailV3VOcopy detail = opLocationPileGroupService.queryDetailForEms(Long.valueOf(groupId));
        //赋值
        vo.setPileSnList(getAllPileSns(detail));
        vo.setGroupId(groupId);
        Result<EMSPowerUsageVo> lastYearIncome= smartBiFeignClient.lastYearIncome(vo);
        //解析返回结果并赋值
        EMSPowerUsageVo result = JSON.parseObject(JSON.toJSONString(lastYearIncome.getData()), EMSPowerUsageVo.class);
        result.setExtendInfo(JSON.toJSONString(result));
        return Result.ofSucceed(result);
    }

    // 递归方法，获取所有pileSn
    public List<String> getAllPileSns(PileGroupDetailV3VOcopy detail) {
        List<String> pileSns = new ArrayList<>();
        if (detail != null) {
            // 遍历当前对象的groupPiles列表并添加pileSn
            if(CollectionUtils.isNotEmpty(detail.getGroupPileDTOList())){
                //遍历
                for (GroupPileV3DTO groupPile : detail.getGroupPileDTOList()) {
                    if (groupPile.getPileSn() != null) {
                        pileSns.add(groupPile.getPileSn());
                    }
                }
            }
            //处理下级
            if (CollectionUtils.isNotEmpty(detail.getChildrenList())){
                //遍历
                for (PileGroupDetailV3VOcopy child : detail.getChildrenList()) {
                    pileSns.addAll(getAllPileSns(child));
                }
            }
        }
        return pileSns;
    }


}
