package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * @author A22587
 */
@Data
@TableName(value ="tb_virtual_card_nfc")
@ApiModel(value = "nfc虚拟卡号表")
public class TbVirtualCardNfcEntity implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "主键id")
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    @ApiModelProperty(value = "用户ID")
    @TableField(value = "user_id")
    private Long userId;

    @ApiModelProperty(value = "虚拟充电卡号")
    @TableField(value = "card_number")
    private String cardNumber;

    @ApiModelProperty(value = "nfc类型（1：安卓，2：IOS）")
    @TableField(value = "nfc_type")
    private Integer nfcType;

    @ApiModelProperty(value = "IOS文件下载地址")
    @TableField(value = "ios_download_url")
    private String iosDownloadUrl;

    @ApiModelProperty(value = "是否删除")
    @TableField("deleted")
    private Integer deleted;

    @ApiModelProperty(value = "创建时间")
    @TableField(value = "create_time")
    private Long createTime;

    @ApiModelProperty(value = "更新时间")
    @TableField(value = "update_time")
    private Long updateTime;

}
