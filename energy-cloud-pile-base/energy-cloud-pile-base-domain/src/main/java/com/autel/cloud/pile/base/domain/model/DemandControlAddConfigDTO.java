package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.dto.SmartChargeGroupConfigAddParamDTOcopy;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 需求费控制器表
 * @TableName demand_control_config
 */
@AllArgsConstructor
@NoArgsConstructor
@Data
public class DemandControlAddConfigDTO implements Serializable {

    @ApiModelProperty("主键")
    private Long id;

    @ApiModelProperty("商户id")
    private Long merchantId;

    @ApiModelProperty("桩序列号列表")
    private List<String> pileSNList;
    /**
     * 控制器名称
     */
    @ApiModelProperty("控制器名称")
    private String controllerName;

    /**
     * 时间间隔
     */
    @ApiModelProperty("时间间隔")
    private Long intervalTime;

    /**
     * 控制范围
     */
    @ApiModelProperty("控制范围")
    private Long groupId;

    /**
     * 控制器内容
     */
    @ApiModelProperty("控制器内容")
    private List<ControlContentDTO> controllerContent;

    /**
     * 0：未启用，1：启用
     */
    @ApiModelProperty("0：未启用，1：启用")
    private Integer status;

    /**
     * 是否删除
     */
    @ApiModelProperty("是否删除 0：未删除 1：已删除")
    private Integer deleted;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Long createdAt;

    /**
     * 更新时间
     */
    @ApiModelProperty("更新时间")
    private Long updatedAt;

    /**
     * 控制器范围
     */
    @ApiModelProperty(value = "控制器范围")
    private String controlRange;


    private SmartChargeGroupConfigAddParamDTOcopy smartChargeGroupConfigAddParamDTO;

}