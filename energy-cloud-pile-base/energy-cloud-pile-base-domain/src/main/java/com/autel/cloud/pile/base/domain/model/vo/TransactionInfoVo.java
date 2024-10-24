package com.autel.cloud.pile.base.domain.model.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author A20019
 * @since 2021/12/4 16:50
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TransactionInfoVo {
    @ApiModelProperty(value = "充电桩序列号")
    private String deviceNum;

    @ApiModelProperty(value = "充电桩枪号")
    private String gunNo;

    @ApiModelProperty(value = "充电流水号")
    private String transactionId;

    @ApiModelProperty(value = "充电用户")
    private String userId;

    @ApiModelProperty(value = "家庭成员用户id集合")
    private String familyUserIds;

    @ApiModelProperty(value = "充电类型(1.刷卡;2.扫码;3.即插即充;4.POS机)")
    private Integer chargeMod;

    @ApiModelProperty(value = "卡号")
    private String cardno;

    @ApiModelProperty(value = "充电桩类型(1：商桩，2家桩)")
    private Integer evseType;

    @ApiModelProperty(value = "是否第三方桩")
    private Boolean thirdPart;

    /**
     * OCPI 设备会话Id
     */
    @ApiModelProperty(value = "OCPI 设备会话Id")
    private String sessionId;

    @ApiModelProperty(value = "OCPP2.0桩上报的tracId")
    private String octtTraceId;

    /**
     * 银行卡ID
     */
    @ApiModelProperty("银行卡ID")
    private String cardId;

    private String emspId;

    @ApiModelProperty(name = "emspOrderId", notes = "emspOrderId"    )
    private String emspOrderId;

    @ApiModelProperty(name = "订单ID", notes = "订单ID")
    private Long billId;

}
