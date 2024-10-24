package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * @author A22136
 * @description 场景默认策略配置表
 * @date 2023-08-18
 */
@Data
@TableName("tb_scene_default_strategy_config")
public class TbSceneDefaultStrategyConfigEntity {

    @TableId(type = IdType.ASSIGN_ID)
    @ApiModelProperty("ID")
    private Long id;

    /**
     * 名称
     */
    @ApiModelProperty("名称 0:场站经营;   1:企业场景;   2:车队运营")
    private int scene;

    /**
     * 默认用能策略 0：效率模式  1：经济模式
     */
    @ApiModelProperty(value = "默认用能策略", allowableValues = "0, 1", example = "1", notes = "默认用能策略 0：效率模式  1：经济模式")
    private int energyUseStrategy;

    /**
     * 默认分配策略 0：缺省值 1：智能分配 2：平均分配 3：加权平均 4:先进先出 5:轮流充电 6车队排班
     */
    @ApiModelProperty(value = "默认分配策略", allowableValues = "1, 2,3,4,5,6", example = "1", notes = "默认分配策略 0：缺省值 1：智能分配 2：平均分配 3：加权平均 4:先进先出 5:轮流充电 6车队排班")
    private int allocationStrategy;

    /**
     * 显示状态 0：隐藏 1：显示
     */
    @ApiModelProperty(value = "显示状态", allowableValues = "0,1, ", example = "1", notes = "显示状态 0：隐藏 1：显示")
    private int showStatus;

}