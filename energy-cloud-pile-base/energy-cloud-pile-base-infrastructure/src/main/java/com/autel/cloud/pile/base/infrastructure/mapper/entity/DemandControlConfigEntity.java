package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 需求费控制器表
 * @TableName demand_control_config
 */
@AllArgsConstructor
@NoArgsConstructor
@TableName(value ="demand_control_config")
@Data
public class DemandControlConfigEntity  {
    /**
     * 
     */
    @TableId
    private Long id;

    @ApiModelProperty("桩序列号列表")
    private String sns;

    private Long merchantId;

    /**
     * 控制器名称
     */
    private String controllerName;

    /**
     * 时间间隔
     */
    private Long intervalTime;

    /**
     * 控制范围
     */
    private Long groupId;

    /**
     * 控制器内容
     */
    private String controllerContent;

    /**
     * 0：未启用，1：启用
     */
    private Integer status;

    /**
     * 是否删除
     */
    private Integer deleted;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 更新时间
     */
    private Long updatedAt;

    @TableField(exist = false)
    private static final long serialVersionUID = 1L;

    /**
     * 控制器范围
     */
    @ApiModelProperty(value = "控制器范围")
    private String controlRange;
}