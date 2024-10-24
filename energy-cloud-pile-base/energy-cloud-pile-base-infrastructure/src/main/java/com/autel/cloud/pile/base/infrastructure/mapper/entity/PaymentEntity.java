package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;


/**
 * 支付方式
 * @author Xzhi
 * @date 2021-12-07 19:02
 */
@Data
@TableName(value = "t_payment")
public class PaymentEntity implements Serializable {

    @JsonFormat(
            shape = JsonFormat.Shape.STRING
    )
    private Long id;

    @ApiModelProperty(value = "商家ID")
    private Long sellerId;

    @ApiModelProperty(value = "支付类型的名称")
    private String name;

    @ApiModelProperty(value = "支付小图标的http路径")
    private String icon;

    @ApiModelProperty(value = "支付类型编码：1 Stripe; 2 Paypal; 3 Google; 4 Apple")
    private Integer code;

    @ApiModelProperty(value = "支付APP id")
    private String payAppId;

    @ApiModelProperty(value = "商户号")
    private String merchantNumber;

    @ApiModelProperty(value = "商户key")
    private String merchantKey;

    @ApiModelProperty(value = "支付成功跳转URL")
    private String successJumpUrl;

    @ApiModelProperty(value = "支付失败跳转url（预留）")
    private String failJumpUrl;

    @ApiModelProperty(value = "退款回调url")
    private String refundCallBackUrl;

    @ApiModelProperty(value = "支付通知回调url")
    private String notifyCallBackUrl;

    @ApiModelProperty(value = "是否启用该支付:true表示启用，false不启用")
    private Boolean enableFlag;

    @TableField(fill = FieldFill.INSERT)
    private Integer deleted;

    private Long createTime;

    private Long updateTime;
}