package com.autel.cloud.pile.base.controller;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.infrastructure.sysconfig.log.annotation.Log;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.service.HomePileShareService;
import com.autel.cloud.pile.base.dto.HomeShareAddDTO;
import com.autel.cloud.pile.base.dto.HomeShareUpdateDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.dto.RandomDelayDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.vo.OpLimitFreeVO;
import com.autel.cloud.pile.base.vo.RandomDelayVO;
import com.autel.cloud.pile.base.vo.app.HomeShareDetailVO;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiModelProperty;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * @author A22219
 */
@Log4j2
@RestController
@RequestMapping("/pile/share")
@Api(tags = "家桩共享")
@Validated
public class HomePileShareController {

    @Autowired
    private HomePileShareService homePileShareService;

    @Log(title = "新建家桩共享", businessType = BusinessType.INSERT, code = "50201419")
    @PostMapping("/add")
    @ApiOperation(value = "新建家桩共享", notes = "新建家桩共享")
    public Result<OpLocationEntity> add(@Validated @RequestBody @Valid HomeShareAddDTO homeShareAddDTO) {
        log.info("新建家桩共享");
        return homePileShareService.add(homeShareAddDTO);
    }

    @Log(title = "用户是否有共享场站", businessType = BusinessType.INSERT, code = "50201419")
    @GetMapping("/userHaveShare")
    @ApiOperation(value = "用户是否有共享场站", notes = "用户是否有共享场站")
    public Result<Boolean> userHaveShare() {
        return homePileShareService.userHaveShare();
    }

    @Log(title = "桩是否已共享", businessType = BusinessType.INSERT, code = "50201419")
    @GetMapping("/pileHaveShare/{pileSN}")
    @ApiOperation(value = "桩是否已共享", notes = "桩是否已共享")
    public Result<Boolean> pileHaveShare(@PathVariable("pileSN") String pileSN) {
        return homePileShareService.pileHaveShare(pileSN);
    }

    @Log(title = "用户所有已共享的桩", businessType = BusinessType.INSERT, code = "50201419")
    @GetMapping("/haveSharePileList")
    @ApiOperation(value = "用户所有已共享的桩", notes = "用户所有已共享的桩 ")
    public Result<List<String>> userHaveSharePileList() {
        return homePileShareService.userHaveSharePileList();
    }

    @Log(title = "家桩共享详情", businessType = BusinessType.INSERT, code = "50201419")
    @GetMapping("/detail")
    @ApiOperation(value = "家桩共享详情", notes = "家桩共享详情")
    public Result<HomeShareDetailVO> detail(@RequestParam(value = "pileSN", required = false) String pileSN) {
        return homePileShareService.detail(pileSN);
    }

    @Log(title = "桩停止家桩共享", businessType = BusinessType.INSERT, code = "50201419")
    @GetMapping("/stop/{pileSN}")
    @ApiOperation(value = "桩停止家桩共享", notes = "桩停止家桩共享")
    public Result<Boolean> pileStop(@PathVariable("pileSN") String pileSN) {
        return homePileShareService.pileStop(pileSN);
    }

    @Log(title = "用户停止家桩共享", businessType = BusinessType.INSERT, code = "50201419")
    @GetMapping("/user/stop")
    @ApiOperation(value = "用户停止家桩共享", notes = "用户停止家桩共享")
    public Result<Boolean> userStop() {
        return homePileShareService.userStop();
    }

    @GetMapping("/user/stop/v2")
    @ApiOperation(value = "用户停止家桩共享", notes = "用户停止家桩共享")
    public Result<Boolean> userStopV2(@RequestParam("userId") Long userId) {
        return homePileShareService.userStopV2(userId);
    }

    /**
     * @deprecated
     * @param homeShareUpdateDTO
     * @return
     */
    @Deprecated
    @Log(title = "编辑家桩共享信息", businessType = BusinessType.UPDATE, code = "50201419")
    @PostMapping("/update")
    @ApiOperation(value = "编辑家桩共享信息", notes = "编辑家桩共享信息")
    public Result<OpLocationEntity> update(@Validated @RequestBody @Valid HomeShareUpdateDTO homeShareUpdateDTO) {
        return homePileShareService.update(homeShareUpdateDTO);
    }

    @Log(title = "编辑地址信息", businessType = BusinessType.UPDATE, code = "50201419")
    @PostMapping("/updateAddress")
    @ApiOperation(value = "编辑地址信息", notes = "编辑地址信息")
    public Result<Boolean> updateAddress(@Validated @RequestBody @Valid HomeShareUpdateDTO homeShareUpdateDTO) {
        return homePileShareService.updateAddress(homeShareUpdateDTO);
    }

    @Log(title = "编辑个人信息", businessType = BusinessType.UPDATE, code = "50201419")
    @PostMapping("/updatePersonalInfo")
    @ApiOperation(value = "编辑个人信息", notes = "编辑个人信息")
    public Result<Boolean> updatePersonalInfo(@RequestBody @Valid HomeShareUpdateDTO homeShareUpdateDTO) {
        return homePileShareService.updatePersonalInfo(homeShareUpdateDTO);
    }

    @Log(title = "编辑价格信息", businessType = BusinessType.UPDATE, code = "50201419")
    @PostMapping("/updateCostRule")
    @ApiOperation(value = "编辑价格信息", notes = "编辑价格信息")
    public Result<Boolean> updateCostRule(@RequestBody @Valid HomeShareUpdateDTO homeShareUpdateDTO) {
        return homePileShareService.updateCostRule(homeShareUpdateDTO);
    }

    @Log(title = "限时免费日期查询", businessType = BusinessType.UPDATE, code = "50201419")
    @GetMapping("/limit/free")
    @ApiOperation(value = "限时免费日期查询", notes = "限时免费日期查询")
    public Result<OpLimitFreeVO> limitFree() {
        return homePileShareService.limitFree();
    }

    @Log(title = "修改限时免费日期", businessType = BusinessType.UPDATE, code = "50201419")
    @GetMapping("/limit/free/update/{freeDate}")
    @ApiOperation(value = "修改限时免费日期", notes = "修改限时免费日期")
    public Result<OpLimitFreeVO> updateLimitFree(@PathVariable("freeDate") Long freeDate) {
        return homePileShareService.updateLimitFree(freeDate);
    }

    @Log(title = "新增限时免费日期", businessType = BusinessType.UPDATE, code = "50201419")
    @GetMapping("/limit/free/add/{freeDate}")
    @ApiOperation(value = "新增限时免费日期", notes = "新增限时免费日期")
    public Result<OpLimitFreeVO> addLimitFree(@PathVariable("freeDate") Long freeDate) {
        return homePileShareService.addLimitFree(freeDate);
    }

    @Log(title = "设置随机延迟开关", businessType = BusinessType.UPDATE, code = "50201419")
    @PostMapping("/randomDelay")
    @ApiOperation(value = "设置随机延迟开关", notes = "设置随机延迟开关")
    public Result<Boolean> randomDelay(@RequestBody RandomDelayDTO randomDelayDTO) {
        return homePileShareService.randomDelay(randomDelayDTO);
    }

    @Log(title = "查询是否开启随机延迟", businessType = BusinessType.UPDATE, code = "50201419")
    @GetMapping("/randomDelay/query/{pileSN}")
    @ApiOperation(value = "查询是否开启随机延迟", notes = "查询是否开启随机延迟")
    public Result<RandomDelayVO> randomDelayQuery(@PathVariable("pileSN") String pileSN) {
        return homePileShareService.randomDelayQuery(pileSN);
    }

    @PostMapping(value = "/updateHomePileInfo")
    @ApiOperation(value = "更新家桩共享桩信息", notes = "更新家桩共享桩信息")
    public Result<Boolean> updateHomePileInfo(@RequestBody OpLocationPileEvseDTO opLocationPileEvseDTO) {
        return homePileShareService.updateHomePileInfo(opLocationPileEvseDTO);
    }
}

