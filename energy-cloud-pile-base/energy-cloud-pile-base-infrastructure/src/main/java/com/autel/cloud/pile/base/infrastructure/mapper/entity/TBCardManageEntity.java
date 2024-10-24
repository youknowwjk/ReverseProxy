package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 
 * </p>
 * @since 2021-12-27
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@TableName("tb_card_manager")
@ApiModel(value = "支付卡对象", description = "支付卡")
public class TBCardManageEntity {

    private static final long serialVersionUID = 1L;

    @TableId(type = IdType.AUTO)
    private Long id;

    private Integer deleted;
    
    @TableField(value = "create_time",fill = FieldFill.INSERT)
    private Long createTime;

    @TableField(value = "update_time",fill = FieldFill.INSERT_UPDATE)
    private Long updateTime;

    @TableField("card_type")
    private String cardType;

    @TableField("default_card")
    private Integer defaultCard;  //  1 默认卡 ，其他 非默认卡

    @TableField("card_last_four_no")
    private String cardLastFourNo;

    @TableField("user_id")
    private Long userId;

    @TableField("email")
    private String email;

    @TableField("mobile")
    private String mobile;

    @TableField("payment_id")
    private Long paymentId;

    @TableField("exp_year")
    private Long expYear;

    @TableField("exp_month")
    private Long expMonth;

    @TableField("fingerprint")
    private String fingerprint;


    @TableField("cc")
    private String cc;

    @TableField("phone_number")
    private String phoneNumber;

    @TableField("address")
    private String address;

    @TableField("refuse_code")
    @ApiModelProperty("拒付码（01: 卡信息错误  02: 卡失效）")
    private String refuseCode;
}