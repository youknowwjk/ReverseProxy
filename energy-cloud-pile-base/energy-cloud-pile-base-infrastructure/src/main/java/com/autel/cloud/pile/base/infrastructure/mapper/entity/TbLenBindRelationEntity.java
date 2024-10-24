package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 15:50
 */
@Data
@NoArgsConstructor
@TableName("tb_len_bind_relation")
public class TbLenBindRelationEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private String tenantId;

    private String orderId;

    private String goodsId;

    /*
     * V1.1 新增 sku
     * */
    private String skuCode;

    private String goodsName;

    private String agreementId;

    /**
     * 权益ID	        含义
     * biz:ops_official	运维运营权限官方授权的权益
     * biz:ops_trial	运维运营权限官方试用的权益
     * ads_official   	广告官方授权的权益
     * 权益ID 命名规则
     * 权益范围[:权益范围:权益范围]_权益类型
     * <p>
     * 权益范围	含义
     * ads	    广告
     * biz 	    运营
     * ops 	    运维
     * <p>
     * 权益类型	含义
     * trial	官方试用
     * official	官方正式
     * <p>
     * 说明
     * 1、权益范围不止一个的按字母顺序升序  例如  biz:ops_trial    biz:ops_official
     * 2、续期规则
     * 权益范围[:权益范围:权益范围] 不一样的 立即生效 即生效时间就是当前时间
     * 权益范围[:权益范围:权益范围] 一样的   找到最后失效时间作为 当前许可证的生效时间
     */
    private String serviceId;

    private String licenceCode;

    private String pileSn;

    private String gunNo;

    private Integer serviceTime;

    private String timeUnit;

    private String measureUnit;

    private String chargeType;

    private Long availableTime;

    private Long unavailableTime;

    private Long bindTime;

    /**
     * '0:新建；1：有效，2：失效，5：已退货
     */
    private Integer status;

    private String remark;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

    /**
     * 附赠时长 value
     * since from crm v1.3
     */
    private String bonusDurationValue;

    /**
     * 额外增加的宽限期天数
     * added @Date: 20231219
     */
    private Integer extendedGracePeriod;

    public TbLenBindRelationEntity(Integer status) {
        this.updateTime = System.currentTimeMillis();
        this.status = status;

    }
}