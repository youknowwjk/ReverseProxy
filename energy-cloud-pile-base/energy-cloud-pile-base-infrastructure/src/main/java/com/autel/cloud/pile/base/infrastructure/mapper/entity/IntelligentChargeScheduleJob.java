package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.autel.cloud.pile.base.infrastructure.feign.dto.JobStatus;
import com.autel.cloud.smart.charge.constant.SmartChargeStatus;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.ibatis.type.LocalDateTimeTypeHandler;

import java.time.LocalDateTime;

/**
 * @author A22136
 * @date 2023-03-23
 * @description 智能充电调度作业表
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
@TableName(value = "intelligent_charge_schedule_job", autoResultMap = true)
public class IntelligentChargeScheduleJob {


    /**
     * 调度作业表id目前用transactionId填充
     */
    private Long id;

    /**
     * 充电设备id
     */
    private Long evseId;

    /**
     * 充电设备id
     */
    private Integer evseType;


    /**
     * 充电设备id
     */
    private Integer chargingType;

    /**
     * locationId 场站ID
     */
    private Long locationId;

    /**
     * 智能充电相关配置id
     */
    private Long profileId;

    /**
     * 识别出的车辆ID
     */
    private Long vehicleId;


    /**
     " 智能充电开关 0：禁用； 1：启用
     */
    private Integer smartChargeSwitch;

    /**
     * 充电用户id
     */
    private Long userId;

    /**
     * 订单流水号 busid
     */
    private String orderSeq;

    /**
     * 订单编号
     */
    private String orderNumber;

    /**
     * 订单启动充电时间
     */
    private String openingZonedTime;

    /**
     * 桩_枪
     */
    private String evseSn;

    /**
     * 桩_枪
     */
    private String zoneId;

    /**
     * @see JobStatus
     */
    private int status;

    /**
     * @see SmartChargeStatus
     */
    private int smartChargeStatus;

    /**
     * 创建时间
     * queryWrapper.ge(IntelligentChargeScheduleJob::getCreateTime, LocalDateTime.now().minusDays(parseInt).toString());
     */
    @TableField(typeHandler = LocalDateTimeTypeHandler.class)
    private LocalDateTime createTime;

    /**
     * 创建时间毫秒 便于定时任务
     * queryWrapper.ge(IntelligentChargeScheduleJob::getCreateTime, LocalDateTime.now().minusDays(parseInt).toString());
     */
    private Long timestamp;

    /**
     * 这个时间只是在识别车辆之后 记录当前的时间 其他地方不要修改这个时间 修改时间
     */
    @TableField(typeHandler = LocalDateTimeTypeHandler.class)
    private LocalDateTime updateTime;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Long getEvseId() {
        return evseId;
    }

    public void setEvseId(Long evseId) {
        this.evseId = evseId;
    }

    public Long getProfileId() {
        return profileId;
    }

    public void setProfileId(Long profileId) {
        this.profileId = profileId;
    }

    public Long getUserId() {
        return userId;
    }

    public void setUserId(Long userId) {
        this.userId = userId;
    }

    public String getOrderSeq() {
        return orderSeq;
    }

    public void setOrderSeq(String orderSeq) {
        this.orderSeq = orderSeq;
    }

    public String getOrderNumber() {
        return orderNumber;
    }

    public void setOrderNumber(String orderNumber) {
        this.orderNumber = orderNumber;
    }

    public String getOpeningZonedTime() {
        return openingZonedTime;
    }

    public void setOpeningZonedTime(String openingZonedTime) {
        this.openingZonedTime = openingZonedTime;
    }

    public String getEvseSn() {
        return evseSn;
    }

    public void setEvseSn(String evseSn) {
        this.evseSn = evseSn;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }


}