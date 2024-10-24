package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 9:00
 */
@TableName("tb_order_record")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TbOrderRecordEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private String serialId;

    private String orderId;

    private String outerOrderNo;

    private String acceptLanguage;

    private String tenantId;

    private Integer licenceCount;

    private Integer purchaseType;

    private Integer status;

    private Integer needPay;

    private String timeZone;

    private String zoneId;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

    private String sourceId;

    public TbOrderRecordEntity(Integer needPay) {
        this.needPay = needPay;
    }

}