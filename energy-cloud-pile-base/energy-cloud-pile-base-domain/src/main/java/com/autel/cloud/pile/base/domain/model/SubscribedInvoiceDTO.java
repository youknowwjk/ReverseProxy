package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.domain.serializer.BigDecimalListSerializer;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * 订阅发票
 * @author A22598
 * @date 2023/06/13
 */
@Data
public class SubscribedInvoiceDTO  {
//    /**
//     * logo地址
//     */
//    private String logoPath;
    @ApiModelProperty(value = "订单Id", example = "156456112313")
    String orderId;

    @ApiModelProperty(value = "发票编号", example = "1630500746154967047")
    private String invoiceNo;

    @ApiModelProperty(value = "发票日期", example = "2023-06-17")
    private String invoiceDate;

    @ApiModelProperty(value = "增值税率列表", example = "[0.22,0.33]")
    @JsonSerialize(using = BigDecimalListSerializer.class)
    private List<BigDecimal> vatRates;

    @ApiModelProperty(value = "消费税率列表", example = "[0.22,0.33]")
    @JsonSerialize(using = BigDecimalListSerializer.class)
    private List<BigDecimal> salesTaxRates;

    @ApiModelProperty(value = "货币符号", example = "$")
    private String currencySymbol;

    /**
     * 时区信息
     */
    @ApiModelProperty(value = "时区信息", example = "UTC+8")
    private String timeZone;

    //------------------------商户信息------------------------

    @ApiModelProperty(value = "客户ID", example = "1549033082797285858")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private Long customerId;

    @ApiModelProperty(value = "客户名称", example = "EVgo")
    private String customerName;

    @ApiModelProperty(value = "客户地址", example = "Zhongshan Road 31, Los Angeles, California, America")
    private String customerAddress;

    @ApiModelProperty(value = "客户国家编码", example = "China")
    private String customerCountryCode;

    @ApiModelProperty(value = "租户税号", example = "0000111111")
    private String vatNumber;

    //------------------------订单信息------------------------

    /**
     * 订单ID 在父类
     */

    @ApiModelProperty(value = "订单商品信息及费用信息")
    SubscribeInfoDetail subscribeInfoDetail;

    /**
     * 支付时间
     */
    private String paymentDate;
    private String createOrderTime;

    //------------------------公司信息------------------------
    /**
     * 公司名称
     */
    private String companyName;

    /**
     * 公司地址
     */
    private String companyAddress;

    private Long payTime;

//    /**
//     * 公司电话
//     */
//    private String companyPhone;
}
