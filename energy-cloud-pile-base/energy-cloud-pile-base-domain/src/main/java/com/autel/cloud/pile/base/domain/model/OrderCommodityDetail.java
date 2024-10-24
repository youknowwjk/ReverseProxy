package com.autel.cloud.pile.base.domain.model;

import com.autel.cloud.pile.base.domain.serializer.BigDecimalSerializer;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * 订单商品明细
 *
 * @author A22598
 * @date 2023/06/14
 */
@Data
public class OrderCommodityDetail {

    @ApiModelProperty("产品名称")
    private String commodityName;

    @ApiModelProperty("订阅时长")
    private String subscriptionDuration;

    @ApiModelProperty("枪类型")
    private String portOrGunType;

    @ApiModelProperty(value = "数量", example = "10")
    private Integer quantity;

    @ApiModelProperty(value = "数量单位,枪,桩", example = "枪")
    private String quantityUnit;

    @ApiModelProperty(value = "货币符号", example = "$")
    private String currencySymbol;

    @ApiModelProperty(value = "单价", example = "99.50")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal unitPrice;

    @ApiModelProperty(value = "商品小计", example = "99.50")
    @JsonSerialize(using = BigDecimalSerializer.class)
    private BigDecimal subTotal;

    @ApiModelProperty("许可证编码")
    private List<String> licenseCodeList;

    @ApiModelProperty(value = "商品退货金额", example = "99.50")
    private BigDecimal subscriptionReturnAmount;
}
