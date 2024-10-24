package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 14:08
 */
@Data
@TableName("tb_agreement_function")
public class TbAgreementFunctionEntity implements Serializable {

    private Long id;

    private String serviceId;

    private String serviceName;

    private String sourceId;

    private String functionId;

    private String functionName;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

}