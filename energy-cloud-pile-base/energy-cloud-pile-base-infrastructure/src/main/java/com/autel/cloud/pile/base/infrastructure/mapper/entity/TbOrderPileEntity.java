package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 9:06
 */
@Data
@TableName("tb_order_pile")
public class TbOrderPileEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private String serialId;

    private String orderId;

    private String pileSn;

    /*
        sap sku映射唯一识别码
    * */
    private String uniqueSkuCode;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;
}