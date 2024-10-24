package com.autel.cloud.pile.base.infrastructure.feign.dto;

import com.autel.cloud.base.common.precision.SOCFormatIntSerializer;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class ChargingScheduleCountDownDTO {
    @ApiModelProperty(value = "transactionId")
    private String transactionId;
    @ApiModelProperty(value = "00:30")
    private String remainingTime;
    @ApiModelProperty(value = "调度信息")
    private String message;
    @ApiModelProperty(value = "30")
    private Long expire;
    /**
     * @see com.autel.cloud.smart.charge.enums.SmartChargeStatusEnum
     */
    private Integer smartChargeStatus;
    /**
     * @see JobStatus
     */
    private Integer jobStatus;

    @ApiModelProperty(value = "目标SOC")
    @JsonSerialize(using = SOCFormatIntSerializer.class)
    private BigDecimal targetSOC;


    @ApiModelProperty("车当前电量")
    @JsonSerialize(using = SOCFormatIntSerializer.class)
    private BigDecimal soc;

    /**
     " 智能充电开关 0：禁用； 1：启用
     */
    private Integer smartChargeSwitch;


    private String driveTime;

    private String sn;

    private int deviceType = DeviceTypeEnum.BUSINESS_PILE.getValue();

}
