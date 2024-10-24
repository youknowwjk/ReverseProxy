package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.LocationMeterService;
import com.autel.cloud.pile.base.dto.LocationMeterDTO;
import com.autel.cloud.pile.base.dto.meter.MeterIdsQueryDTO;
import com.autel.cloud.pile.base.dto.meter.MeterQueryDTO;
import com.autel.cloud.pile.base.dto.meter.PageParamDTO;
import com.autel.cloud.pile.base.dto.meter.PageQueryDTO;
import com.autel.cloud.pile.base.vo.LocationMeterVO;
import com.autel.cloud.pile.base.vo.MeterVO;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 电表设置
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Log4j2
@RestController
@RequestMapping("/meter")
@Api(tags = "电表设置")
@Validated
public class LocationMeterController {
    @Autowired
    @Lazy
    LocationMeterService locationMeterService;


    /**
     * @param locationMeterDTO
     * @return
     * @function 新增电表
     */
    @PostMapping(value = "/add")
    @ApiOperation(value = "新增电表", notes = "新增电表")
    public Result<String> add(@RequestBody @Validated(value = LocationMeterDTO.Add.class) LocationMeterDTO locationMeterDTO) {

        log.info("===>>>LocationMeterController.add locationMeterDTO : {}",
                JSON.toJSONString(locationMeterDTO));

        return Result.ofSucceed(String.valueOf(locationMeterService.add(locationMeterDTO)));
    }

    /**
     * @param id
     * @return
     * @function 查询电表信息详情
     */
    @GetMapping(value = "/detail/{id}")
    @ApiOperation(value = "查询电表信息详情", notes = "查询电表信息详情")
    public Result<LocationMeterVO> detail(@PathVariable("id") Long id) {

        log.info("===>>>LocationMeterController.detail id : {}",
                JSON.toJSONString(id));

        return Result.ofSucceed(locationMeterService.detail(id));
    }

    /**
     * @param locationMeterDTO
     * @return
     * @function 更新电表
     */
    @PostMapping(value = "/update")
    @ApiOperation(value = "更新电表", notes = "更新电表")
    public Result<Boolean> update(@Validated(value = LocationMeterDTO.Update.class) @RequestBody LocationMeterDTO locationMeterDTO) {

        log.info("===>>>LocationMeterController.update locationMeterDTO : {}",
                JSON.toJSONString(locationMeterDTO));

        return Result.ofSucceed(locationMeterService.update(locationMeterDTO));
    }

    /**
     * @param pageQueryDTO
     * @return
     * @function 分页查询商家下的电表数据
     */
    @PostMapping("/pages")
    @ApiOperation(value = "分页查询商家下的电表数据", notes = "分页查询商家下的电表数据")
    public Result<Page<LocationMeterVO>> queryPages(@Validated @RequestBody PageQueryDTO pageQueryDTO) {

        log.info("=====>>>>>LocationMeterController.queryPages pageQueryDTO : {}",
                JSON.toJSONString(pageQueryDTO));

        return Result.ofSucceed(locationMeterService.queryPages(pageQueryDTO));
    }

    /**
     * @param id
     * @return
     * @function 删除电表
     */
    @GetMapping(value = "/delete/{id}")
    @ApiOperation(value = "删除电表", notes = "删除电表")
    public Result<Boolean> delete(@PathVariable("id") Long id) {

        log.info("===>>>LocationMeterController.delete id : {}",
                JSON.toJSONString(id));

        return Result.ofSucceed(locationMeterService.delete(id));
    }

    /**
     * @return
     * @function 查询商家可以给智能充电群组绑定的电表列表(过滤掉已经被绑定过的)
     */
    @PostMapping(value = "/getList")
    @ApiOperation(value = "查询商家可以给智能充电群组绑定的电表列表(过滤掉已经被绑定过的)", notes = "查询商家可以给智能充电群组绑定的电表列表(过滤掉已经被绑定过的)")
    public Result<List<LocationMeterVO>> getList(@Validated @RequestBody MeterQueryDTO meterQueryDTO) {

        log.info("===>>>LocationMeterController.getList meterQueryDTO : {}",
                JSON.toJSONString(meterQueryDTO));

        List<Long> groupIdList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(meterQueryDTO.getGroupIdList())) {
            meterQueryDTO.getGroupIdList().forEach(var -> {
                if (var != null) {
                    groupIdList.add(var);
                }
            });
        }

        meterQueryDTO.setGroupIdList(groupIdList);
        return Result.ofSucceed(locationMeterService.getList(meterQueryDTO));
    }

    @ApiImplicitParam(name = "brandEnum", value = "电表品牌类型枚举值(0: Enegic)", dataType = "Integer", paramType = "query", required = true)
    @GetMapping(value = "/generateDefaultMeterName")
    @ApiOperation(value = "生成默认的电表名称", notes = "生成默认的电表名称")
    public Result<String> generateDefaultMeterName(@RequestParam("brandEnum") Integer brandEnum) {
        return Result.ofSucceed(locationMeterService.generateDefaultMeterName(brandEnum));
    }

    @PostMapping(value = "/getIds")
    @ApiOperation(value = "通过SN查询ID", notes = "通过SN查询ID")
    public Result<List<LocationMeterVO>> getIds(@Validated @RequestBody List<String> sns) {
        return Result.ofSucceed(locationMeterService.getIds(sns));
    }

    /**
     * 供边缘网关关联电表时  调用
     */
    @PostMapping("/queryByPage")
    @ApiOperation(value = "分页查询商家下的电表数据", notes = "分页查询商家下的电表数据")
    public Result<Page<MeterVO>> queryByPage(@RequestBody PageParamDTO paramDTO) {
        log.info("=====>>>>>LocationMeterController.queryByPage pageQueryDTO : {}",
                JSON.toJSONString(paramDTO));
        return Result.ofSucceed(locationMeterService.queryByPage(paramDTO));
    }

    @PostMapping(value = "/ids")
    @ApiOperation(value = "通过SN查询ID", notes = "通过SN查询ID")
    public Result<List<MeterVO>> queryMetetByMetetIds(@RequestBody @Validated MeterIdsQueryDTO paramDTO){
        log.info("=====>>>>>LocationMeterController.queryMetetByMetetIds pageQueryDTO : {}",
                JSON.toJSONString(paramDTO));
        return Result.ofSucceed(locationMeterService.queryMetetByMetetIds(paramDTO));
    }
}

