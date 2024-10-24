package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.DataChecker;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.enums.SceneEnums;
import com.autel.cloud.pile.base.infrastructure.mapper.TbSceneDefaultStrategyConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbSceneDefaultStrategyConfigEntity;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import io.swagger.annotations.*;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.List;
import java.util.Objects;

@RestController
@RequestMapping("/SceneDefaultStrategyConfig")
@Slf4j
@Validated
@Api(tags = "场景默认策略配置表")
public class TbSceneDefaultStrategyConfigController {


    @Resource
    private TbSceneDefaultStrategyConfigMapper tbSceneDefaultStrategyConfigMapper;

    @GetMapping("/findById")
    @ApiOperation(value = "场景默认策略配置详情")
    @ApiResponse(code = 800222, message = "SceneDefaultStrategyConfig")
    public Result<TbSceneDefaultStrategyConfigEntity> findById(@RequestParam("id") Long id) {
        return Result.ofSucceed(tbSceneDefaultStrategyConfigMapper.selectById(id));
    }

    @GetMapping("/findByScene")
    @ApiOperation(value = "查询场景默认策略配置详情")
    @ApiResponse(code = 800222, message = "SceneDefaultStrategyConfig")
    public Result<TbSceneDefaultStrategyConfigEntity> findByScene(@RequestParam(value = "scene", defaultValue = "0") int scene) {
        Wrapper<TbSceneDefaultStrategyConfigEntity> lambdaQuery = Wrappers.lambdaQuery(TbSceneDefaultStrategyConfigEntity.class).eq(TbSceneDefaultStrategyConfigEntity::getScene, scene);
        return Result.ofSucceed(tbSceneDefaultStrategyConfigMapper.selectOne(lambdaQuery));
    }


    @PostMapping(value = "/page")
    @ApiOperation(value = "场景默认策略配置分页查询")
    public Result<List<TbSceneDefaultStrategyConfigEntity>> selectTbSceneDefaultStrategyConfigList(@Validated @RequestBody TbSceneDefaultStrategyConfigFuzzQueryDTO queryDTO) {
        LambdaQueryWrapper<TbSceneDefaultStrategyConfigEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(StringUtils.hasText(queryDTO.getKeyword()), TbSceneDefaultStrategyConfigEntity::getScene, SceneEnums.getCodesByMatchName(queryDTO.getKeyword()));
        List<TbSceneDefaultStrategyConfigEntity> tbSceneDefaultStrategyConfigEntities = tbSceneDefaultStrategyConfigMapper.selectList(queryWrapper);
        return Result.ofSucceed(tbSceneDefaultStrategyConfigEntities);
    }

    @PostMapping(value = "/showToggle")
    @ApiOperation(value = "场景默认策略配置展示与隐藏")
    public Result<TbSceneDefaultStrategyConfigEntity> showToggle(@RequestParam("id") Long id) {
        TbSceneDefaultStrategyConfigEntity tbSceneDefaultStrategyConfigEntity = tbSceneDefaultStrategyConfigMapper.selectById(id);
        tbSceneDefaultStrategyConfigEntity.setShowStatus((tbSceneDefaultStrategyConfigEntity.getShowStatus() + 1) % 2);
        tbSceneDefaultStrategyConfigMapper.updateById(tbSceneDefaultStrategyConfigEntity);
        return Result.ofSucceed(tbSceneDefaultStrategyConfigEntity);
    }

    @PostMapping(value = "/update")
    @ApiOperation(value = "场景默认策略配置编辑")
    public Result<TbSceneDefaultStrategyConfigEntity> update(@Validated @RequestBody TbSceneDefaultStrategyConfigEntity tbSceneDefaultStrategyConfigEntity) {
        Assert.notNull(tbSceneDefaultStrategyConfigMapper.selectById(tbSceneDefaultStrategyConfigEntity.getId()), "Data not exit");
        int allocationStrategy = tbSceneDefaultStrategyConfigEntity.getAllocationStrategy();
        int energyUseStrategy = tbSceneDefaultStrategyConfigEntity.getEnergyUseStrategy();
        int scene = tbSceneDefaultStrategyConfigEntity.getScene();
        DataChecker.check(0 < allocationStrategy && allocationStrategy  <= 6, "请输入合法的默认分配策略",
                () -> log.error("tbSceneDefaultStrategyConfigEntity:{}", JSON.toJSONString(tbSceneDefaultStrategyConfigEntity)));
        DataChecker.check(0 <= energyUseStrategy && energyUseStrategy  <= 1, "请输入合法的默认用能策略",
                () -> log.error("tbSceneDefaultStrategyConfigEntity:{}", JSON.toJSONString(tbSceneDefaultStrategyConfigEntity)));
        DataChecker.check(0 <= scene && scene  <= 2, "请输入合法的默认用能场景",
                () -> log.error("tbSceneDefaultStrategyConfigEntity:{}", JSON.toJSONString(tbSceneDefaultStrategyConfigEntity)));
        tbSceneDefaultStrategyConfigMapper.updateById(tbSceneDefaultStrategyConfigEntity);
        return Result.ofSucceed(tbSceneDefaultStrategyConfigEntity);
    }

    @PostMapping(value = "/add")
    @ApiOperation(value = "场景默认策略配置新增")
    public Result<TbSceneDefaultStrategyConfigEntity> add(@Validated @RequestBody TbSceneDefaultStrategyConfigEntity tbSceneDefaultStrategyConfigEntity) {
        tbSceneDefaultStrategyConfigMapper.insert(tbSceneDefaultStrategyConfigEntity);
        return Result.ofSucceed(tbSceneDefaultStrategyConfigEntity);
    }


    @EqualsAndHashCode(callSuper = true)
    @ApiModel("场景默认策略配置查询对象")
    @Data
    public static class TbSceneDefaultStrategyConfigFuzzQueryDTO extends PageDTO {

        @ApiModelProperty(value = "场景")
        private String keyword;

    }

}
