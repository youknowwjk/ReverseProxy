package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.apache.ibatis.type.LocalDateTimeTypeHandler;
import org.springframework.format.annotation.DateTimeFormat;

import java.time.LocalDateTime;

/**
 * @author zhengkai.blog.csdn.net
 * @description vip客户配置表
 * @date 2023-08-21
 */
@Data
@TableName("op_location_pile_vip_config")
public class OpLocationPileVipConfigEntity {


    @TableId(type = IdType.ASSIGN_ID)
    /**
     * id
     */
    private Long id;

    /**
     * 智能充电配置id
     */
    private Long groupId;

    /**
     * 1 tb_member_group; 2  tb_member
     */
    private int type;

    /**
     * type=1 时 客户组 type=2 时 用户组
     */
    private Long principalId;

    private Long pkId;

    /**
     * 当前时间
     */
    @ApiModelProperty("生效时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(typeHandler = LocalDateTimeTypeHandler.class)
    private LocalDateTime effectiveDate;

    /**
     * 失效时间
     */
    @ApiModelProperty("失效时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(typeHandler = LocalDateTimeTypeHandler.class)
    private LocalDateTime expirationDate;

}