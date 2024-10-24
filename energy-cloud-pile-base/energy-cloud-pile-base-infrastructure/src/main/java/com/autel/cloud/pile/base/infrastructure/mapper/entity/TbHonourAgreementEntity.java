package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * @description 服务权益履约表 实体对象
 * @auther A23204
 * @datetime 2023/6/13 15:41
 */
@Data
@TableName("tb_honour_agreement")
public class TbHonourAgreementEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private String orderId;

    private String goodsId;

    private String agreementId;

    private String tenantId;

    private String serviceId;

    private Integer licenceCount;

    private String measureUnit;

    private Integer serviceTime;

    private String timeUnit;

    private Integer agreementStatus;

    private String remark;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

}