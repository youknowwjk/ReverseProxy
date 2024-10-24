package com.autel.cloud.pile.base.domain.data;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class QuickChargingScheduleDTO {

    @ApiModelProperty("busId  orderSeq 订单ID")
    private String transactionId;

    @ApiModelProperty("桩SN")
    private String sn;

    @ApiModelProperty("枪号")
    private String gunNo;
}
