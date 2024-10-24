package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.enums.SubStatus;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Builder
@Data
@AllArgsConstructor
@NoArgsConstructor
@ApiModel("过期充电桩")
public class ImminentExpireChargePointDTO {

//    充电桩SN	充电桩名称	所属场站	计划名称	订阅状态	剩余时间	订阅到期日期

    @ApiModelProperty("商家ID")
    private Long merchantId;

    @ApiModelProperty("充电桩SN")
    private String sn;

    @ApiModelProperty("充电桩名称")
    private String name;

    @ApiModelProperty("所属场站")
    private String locationName;

    @ApiModelProperty("计划名称")
    private String goodsName;

    @ApiModelProperty("订阅状态")
    private SubStatus subStatus;

    @ApiModelProperty("剩余时间")
    private Long remainDays;

    @ApiModelProperty("到期时间")
    private Long expireEpochMilli;

    @ApiModelProperty("订阅到期日期")
    private String expireDate;

}
