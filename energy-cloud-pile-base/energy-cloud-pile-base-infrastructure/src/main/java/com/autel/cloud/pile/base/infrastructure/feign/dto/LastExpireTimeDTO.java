package com.autel.cloud.pile.base.infrastructure.feign.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class LastExpireTimeDTO {

    private String pileSn;

    private Long tenantId;

    @ApiModelProperty("订阅到期日期时间戳")
    private Long unavailableTime;

    @ApiModelProperty("订阅到期日期")
    private String expireDate;

    @ApiModelProperty("订阅到期时间戳")
    private Long expireTimestamp;

    @ApiModelProperty("显示状态")
    private Integer subscriptionStatus;


    public LastExpireTimeDTO(String pileSn, Long tenantId, Integer subscriptionStatus) {
        this.pileSn = pileSn;
        this.tenantId = tenantId;
        this.subscriptionStatus = subscriptionStatus;
    }
}