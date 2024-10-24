package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * @Author A22282
 * @Date 2022/5/9 16:42
 */
@Data
@TableName(value ="tb_charge_card")
@ApiModel(value = "充电卡表")
public class ChargeCardEntity implements Serializable {
    @TableId(type = IdType.AUTO)
    @ApiModelProperty(value = "主键id")
    private Long id;

    @ApiModelProperty(value = "充电卡号")
    private String cardNumber;

    @ApiModelProperty(value = "商家名称")
    private String operatorName;

    @ApiModelProperty(value = "卡类型；0：RFID，1：其他，2：公司卡")
    private Integer cardType = 0;

    @ApiModelProperty(value = "卡品牌；1：Autel，2：EVBox，3：Monta，4：Others")
    private Integer cardBrand = 1;

    @ApiModelProperty(value = "充电次数")
    private Integer chargeTimes = 0;

    @ApiModelProperty(value = "归属商家id")
    private Long operatorId;

    @ApiModelProperty(value = "卡属性;0.表示通用型1.表示指定场站")
    private Integer cardAttribute = 0;

    @ApiModelProperty(value = "绑定状态：0表示未绑定，1表示已绑定")
    private Integer bindStatus = 0;

    @ApiModelProperty(value = "绑定用户id")
    private Long userId;

    @ApiModelProperty(value = "绑定时间")
    private Long bindTime;

    @ApiModelProperty(value = "充电卡名称")
    private String cardAlias;

    @ApiModelProperty(value = "充电场站，-1表示不限")
    private String limitLocationId = "-1";

    @ApiModelProperty(value = "充电桩，-1表示不限")
    private String limitEvseId = "-1";

    @ApiModelProperty(value = "是否删除;0表示未删除,1表示已删除")
    private Boolean deleted;

    @ApiModelProperty(value = "创建时间")
    @TableField(fill = FieldFill.INSERT)
    private Long createTime;

    @ApiModelProperty(value = "修改时间")
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updateTime;

    @ApiModelProperty(value = "卡经销商类型： 1：AUTEL, 2:sevadisCard, 3: Other")
    private Integer cardPartyType;

    @ApiModelProperty(value = "是否可以同时启动多笔订单")
    private Boolean startMultipleOrdersEnabled;

    @ApiModelProperty(value = "版本号")
    @Version
    private Long version;

    @TableField(exist = false)
    private static final long serialVersionUID = 1L;
}
