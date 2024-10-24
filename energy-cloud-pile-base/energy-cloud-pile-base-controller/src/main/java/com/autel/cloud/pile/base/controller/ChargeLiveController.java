package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.ChargeLiveServie;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.OpLocationEvseStateCountDTO;
import com.autel.cloud.pile.base.dto.OpLocationLiveEvsePageDTO;
import com.autel.cloud.pile.base.dto.OpLocationLiveEvseViewDTO;
import com.autel.cloud.pile.base.dto.pos.DeviceInfoDTO;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.pos.DeviceInfoVO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.CollectionUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author X20010
 */
@Api(tags = "充电实况")
@RestController
@RequestMapping(value = "/chargeLive")
@Log4j2
@Validated
public class ChargeLiveController {


    @Autowired
    private ChargeLiveServie chargeLiveServie;
    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    private OpLocationService opLocationService;

    @PostMapping("/countAllEvseByState")
    @ApiOperation(value = "按枪状态维度统计所有场站")
    public Result<List<OpLocationEvseStateCountVO>> countAllEvseByState(@RequestBody(required = false) List<Long> locationIds) {
        return Result.ofSucceed(chargeLiveServie.countAllEvseByState(locationIds));
    }

    @PostMapping("/countEvseByState")
    @ApiOperation(value = "按枪状态维度统计")
    public Result<List<OpLocationEvseStateCountVO>> countEvseByState(@RequestBody @Validated OpLocationEvseStateCountDTO opLocationEvseStateCountDTO) {
        List<Long> locationIds = this.getLocationIds(opLocationEvseStateCountDTO.getLocationIds());
        if (CollectionUtils.isEmpty(locationIds)) {
            OpLocationEvseStateCountVO totalVo = new OpLocationEvseStateCountVO();
            totalVo.setState(-1);
            totalVo.setCount(0);
            return Result.ofSucceed(Arrays.asList(totalVo));
        }
        opLocationEvseStateCountDTO.setLocationIds(locationIds);
        return Result.ofSucceed(chargeLiveServie.countEvseByState(opLocationEvseStateCountDTO));
    }

    @PostMapping("/listLiveEvseView")
    @ApiOperation(value = "按（状态/桩SN/桩名称模糊）条件查询当前场站下枪视图")
    public Result<List<OpLocationLiveEvseViewVO>> listLiveEvseView(@RequestBody @Validated OpLocationLiveEvseViewDTO opLocationLiveEvseViewDTO) {
        Long locationId = opLocationLiveEvseViewDTO.getLocationId();
        if (locationId == null) {
            return Result.ofSucceed(new ArrayList<>());
        }
        List<Long> locationIds = this.getLocationIds(Collections.singletonList(locationId));
        if (CollectionUtils.isEmpty(locationIds)) {
            return Result.ofSucceed(new ArrayList<>());
        }
        opLocationLiveEvseViewDTO.setLocationId(locationIds.get(0));
        return Result.ofSucceed(chargeLiveServie.listLiveEvseView(opLocationLiveEvseViewDTO));
    }


    @GetMapping("/getEvseLiveInfo")
    @ApiOperation(value = "查询指定枪更多实况信息")
    public Result<OpLocationLiveEvseMoreVO> getEvseLiveInfo(@RequestParam(value = "evseSn") String evseSn) {
        return Result.ofSucceed(chargeLiveServie.getEvseLiveInfo(evseSn));
    }

    @PostMapping("/listLiveEvseByPage")
    @ApiOperation(value = "分页查询枪实况信息列表，支持指定场站下，状态，桩SN/桩名称模糊搜索")
    public Result<PageVO<OpLocationLiveEvseVO>> listLiveEvseByPage(@Valid @RequestBody @Validated OpLocationLiveEvsePageDTO opLocationLiveEvsePageDTO) {
        List<Long> locationIds = this.getLocationIds(opLocationLiveEvsePageDTO.getLocationIds());
        if (CollectionUtils.isEmpty(locationIds)) {
            PageVO<OpLocationLiveEvseVO> pageVO = new PageVO<>();
            pageVO.setPage(opLocationLiveEvsePageDTO.getPage());
            pageVO.setPageSize(opLocationLiveEvsePageDTO.getPageSize());
            pageVO.setTotalPages(0L);
            pageVO.setTotalRows(0L);
            return Result.ofSucceed(pageVO);
        }
        opLocationLiveEvsePageDTO.setLocationIds(locationIds);
        return Result.ofSucceed(chargeLiveServie.listLiveEvseByPage(opLocationLiveEvsePageDTO));
    }

    @ApiOperation(value = "桩详情--运行信息-充电实况")
    @GetMapping(value = "/currentStatus")
    @ApiImplicitParams({
            @ApiImplicitParam(value = "枪的sn", name = "evseSn", example = "A123131_01", paramType = "query")
    })
    public Result<OpLocationLiveEvseCurrentVO> getCurrentStatus(@RequestParam(value = "evseSn") String evseSn) {
        return Result.ofSucceed(chargeLiveServie.getCurrentStatus(evseSn));
    }

    @PostMapping("/selectDeviceInfo")
    @ApiOperation(value = "查询指定设备的基础信息")
    public Result<List<DeviceInfoVO>> selectDeviceInfo(@RequestBody List<DeviceInfoDTO> deviceInfoDTOList) {

        log.info("===>>> ChargeLiveController.selectDeviceInfo deviceInfoDTOList : {}",
                JSON.toJSONString(deviceInfoDTOList));

        return Result.ofSucceed(chargeLiveServie.selectDeviceInfo(deviceInfoDTOList));
    }

    /**
     * 过滤场站权限
     *
     * @param ids
     * @return
     */
    private List<Long> getLocationIds(List<Long> ids) {
        List<Long> tmpList;
        if (LoginUserUtil.isSellerAdmin()) {
            tmpList = this.opLocationService.getLocationIdBySellerId().getData();
        } else {
            tmpList = this.pileUserFeign.getLocationIds().getData();
        }
        if (CollectionUtils.isEmpty(tmpList)) {
            log.info("getLocationIds,tmpList is null");
            return null;
        }
        if (CollectionUtils.isEmpty(ids)) {
            return tmpList;
        }
        List<Long> resultList = ids.stream().filter(tmpList::contains).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(resultList)) {
            log.info("getLocationIds,resultList is null");
        }
        return resultList;
    }
}
