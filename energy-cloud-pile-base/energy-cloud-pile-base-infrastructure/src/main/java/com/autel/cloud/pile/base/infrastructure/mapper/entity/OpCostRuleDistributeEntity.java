package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @Author MingLong A22599
 * @Date 2022.09.12
 * @Function 计费规则下发记录表对应的实体类
 */
@Accessors(chain = true)
@TableName(value = "op_cost_rule_distribute")
@Data
public class OpCostRuleDistributeEntity implements Serializable {

    private static final long serialVersionUID = 1537863453767845L;

    /**
     * 主键ID
     */
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 计费规则ID
     */
    @TableField(value = "rule_id")
    private Long ruleId;

    /**
     * 充电设备SN码
     */
    @TableField(value = "pile_sn")
    private String pileSn;

    /**
     * 下发成功标识(0:失败;1:成功)
     */
    @TableField(value = "success_flag")
    private Integer successFlag;

    /**
     * 下发失败的原因有 1:(桩离线), 2:(桩固件版本不支持)
     */
    @TableField(value = "failure_reason")
    private String failureReason;

    /**
     * 分发时间
     */
    @TableField(value = "distribute_time")
    private Long distributeTime;

    /**
     * 生效标识(0:失败;1:成功)
     */
    @TableField(value = "efficient_flag")
    private Integer efficientFlag;

    /**
     * 生效失败的原因有 1:(桩离线), 2:(桩固件版本不支持), 3:(桩使用中)
     */
    @TableField(value = "efficient_failure_reason")
    private String efficientFailureReason;

    /**
     * 生效时间
     */
    @TableField(value = "efficient_time")
    private Long efficientTime;

    /**
     * 修改者
     */
    @TableField(value = "update_by")
    private Long updateBy;

    /**
     * 创建者
     */
    @TableField(value = "create_by")
    private Long createBy;

    /**
     * 是否删除 1删除 0不删除
     */
    @TableField(value = "deleted")
    private Integer deleted;

    /**
     * 创建时间
     */
    @TableField(value = "create_time")
    private Long createTime;

    /**
     * 更新时间
     */
    @TableField(value = "update_time")
    private Long updateTime;
}