package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.dto.SmartChargeGroupConfigUpdateParamDTOcopy;
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
public class DemandControlUpdateConfigDTO implements Serializable {

    private Long id;

    private Long merchantId;

    @ApiModelProperty("桩序列号列表")
    private List<String> pileSNList;
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
    private List<ControlContentDTO> controllerContent;

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

    /**
     * 控制器范围
     */
    @ApiModelProperty(value = "控制器范围")
    private String controlRange;


    private SmartChargeGroupConfigUpdateParamDTOcopy smartChargeGroupConfigUpdateParamDTO;

}