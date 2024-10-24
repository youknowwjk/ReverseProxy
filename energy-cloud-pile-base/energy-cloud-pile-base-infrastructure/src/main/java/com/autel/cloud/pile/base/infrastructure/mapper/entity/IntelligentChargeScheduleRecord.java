package com.autel.cloud.pile.base.infrastructure.mapper.entity;


import com.autel.cloud.pile.base.dto.DeliveryByUserDTO;
import com.autel.cloud.smart.charge.vo.ChargingProfileSettingVO;
import com.autel.cloud.smartbi.dto.ChargingSchedulePlanDTO;
import com.autel.cloud.smartbi.dto.ScheduleArithmeticRequest;
import com.autel.cloud.smartbi.dto.ScheduleArithmeticResponse;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.extension.handlers.FastjsonTypeHandler;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.ibatis.type.LocalDateTimeTypeHandler;

import java.time.LocalDateTime;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "intelligent_charge_schedule_record", autoResultMap = true)
public class IntelligentChargeScheduleRecord {


    /**
     * 记录id
     */
    @TableId(type = IdType.ASSIGN_ID)
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long id;

    /**
     * 智能充电调度作业id
     */
    private Long jobId;

    /**
     * 智能充电相关配置
     */
    @TableField(typeHandler = FastjsonTypeHandler.class)
    private DeliveryByUserDTO scheduleRequest;

    /**
     * 智能充电相关配置
     */
    @TableField(typeHandler = FastjsonTypeHandler.class)
    private ChargingProfileSettingVO scheduleResponse;

    /**
     * getcompositeschedule.req‌
     */
    private String compositeSchedule;

    /**
     * setchargingprofile.req‌
     */
    private String chargingProfile;

    /**
     * 根据算法充电计划曲线
     */
    @TableField(typeHandler = FastjsonTypeHandler.class)
    private ChargingSchedulePlanDTO chargingPlan;

    /**
     * 调度时间
     */
    @TableField(typeHandler = LocalDateTimeTypeHandler.class)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime scheduleTime;

    /**
     * 调度状态
     * -1 失败
     * 1 成功
     */
    private Integer scheduleStatus;

    /**
     * 触发调度事件: 0、尝试切换智能充电首次触发; 1、定时任务触发; 2、配置修改触发
     */
    private int triggerEvent;


}
