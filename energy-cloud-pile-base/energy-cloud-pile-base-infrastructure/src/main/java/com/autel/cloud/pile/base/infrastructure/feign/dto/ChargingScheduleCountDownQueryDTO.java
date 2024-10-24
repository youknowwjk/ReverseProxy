package com.autel.cloud.pile.base.infrastructure.feign.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Locale;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ChargingScheduleCountDownQueryDTO {

    private String transactionId;

    private String sn;

    private String gunNo;

    private Long userId;

    private Locale locale;
}
