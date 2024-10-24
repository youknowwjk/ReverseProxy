package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.domain.serializer.BigDecimalListSerializer;
import com.autel.cloud.pile.base.domain.serializer.BigDecimalSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * 订阅信息详细信息
 *
 * @author A22598
 * @date 2023/06/13
 */
@Api("订阅信息详情")
@Data
public class SubscribeInfoDetail extends SubscribeInfo {
    /**
     * ----基本信息----
     */
    @ApiModelProperty(value = "商品名称")
    private String produceName;

    @ApiModelProperty(value = "订单商品详情列表")
    private List<OrderCommodityDetail> orderCommodityDetails;

    /**
     * -----金额相关-----
     */
    @ApiModelProperty(value = "货币符号", example = "$")
    private String currencySymbol;

    @ApiModelProperty(value = "商品金额", example = "100.56")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal subTotal;

    @ApiModelProperty(value = "总优惠金额", example = "20.50")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal totalDiscount;

    @ApiModelProperty(value = "增值税", example = "6.56")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal vat;

    @ApiModelProperty(value = "消费税", example = "6.56")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal salesTax;

    @ApiModelProperty(value = "总税额", example = "6.56")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal totalTax;

    @ApiModelProperty(value = "折后税后金额", example = "73.50")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal totalAmount;

    @ApiModelProperty(
            value = "增值税率列表",
            example = "[0.22,0.33]"
    )
    @JsonSerialize(using = BigDecimalListSerializer.class)
    private List<BigDecimal> vatRates;

    @ApiModelProperty(
            value = "消费税率列表",
            example = "[0.22,0.33]"
    )
    @JsonSerialize(using = BigDecimalListSerializer.class)
    private List<BigDecimal> salesTaxRates;

    @ApiModelProperty(value = "支付链接", example = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
    private String payUrl;

    private Long createOrderTime;
}
