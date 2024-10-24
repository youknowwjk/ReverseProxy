package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/12/6 13:56
 */
@Data
@TableName("tb_benefit_config")
public class TbBenefitConfigEntity implements Serializable {
    private static final long serialVersionUID = -4892991600892809366L;

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 服务权益id
     */
    private String serviceId;

    /**
     * 服务权益名称
     */
    private String serviceName;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

}
