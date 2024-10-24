package com.autel.cloud.pile.base.domain.model;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 订阅信息
 *
 * @author A22598
 * @date 2023/06/13
 */
@ApiModel("订阅信息")
@Data
public class SubscribeInfo {

    @ApiModelProperty("订单编号")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    Long orderId;

    @ApiModelProperty("订单状态")
    Integer orderStatus;

    @ApiModelProperty("商品名称")
    String productName;

    @ApiModelProperty("枪类型")
    String pileOrGunType;

    @ApiModelProperty("License数量")
    Integer licenseNumber;

    @ApiModelProperty("订阅时长")
    String subscriptionDuration;

    @ApiModelProperty("订阅日期")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    Long subscriptionTime;

    @ApiModelProperty("生效License数量")
    Integer effectiveLicenseNumber;

    @ApiModelProperty("可用License数量")
    Integer availableLicenseNumber;

    @ApiModelProperty(value = "选购方式, 1:购买并使用许可证, 2:购买许可证", example = "1")
    private Integer purchaseMethod;

    @ApiModelProperty(value = "是否需要支付", example = "true")
    private Boolean payRequired;

    @ApiModelProperty(value = "操作人", example = "1453302022528233474")
    @JsonFormat(shape = JsonFormat.Shape.STRING)
    private String operatorName;

    @ApiModelProperty(value = "支付连接", example = "https://checkout.stripe.com/c/pay/cs_test_a17fjyZRAY1zFVKBG177sxveSgwKXG8fVNLepwIn3XtU4krZKGkSjzYrUF#fidkdWxOYHwnPyd1blpxYHZxWjA0TzBVQXNCN0dyZ0xOYHN9cXBTfGxJY1VcVl98dFdVVXdrdVFrVlNBdnJ%2FSjxAcFBBY2B9c3dAYn9qVXQwM3xzNXBGNXdNcjxrTHVWQn1Lc25OM01yb1dwNTVVdV91bEh0ZicpJ2N3amhWYHdzYHcnP3F3cGApJ2lkfGpwcVF8dWAnPyd2bGtiaWBabHFgaCcpJ2BrZGdpYFVpZGZgbWppYWB3dic%2FcXdwYHgl")
    private String payUrl;

}
