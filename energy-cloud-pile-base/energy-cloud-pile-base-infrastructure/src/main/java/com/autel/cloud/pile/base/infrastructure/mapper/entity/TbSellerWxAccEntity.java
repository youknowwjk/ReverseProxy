package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/7/3 19:53
 */
@TableName(value ="tb_seller_wx_acc")
@Data
public class TbSellerWxAccEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private Long sellerId;

    private String account;

    private String accountType;

    private String accountName;

    /*
    * 0: 新建
    * 1: 可用
    * 2：解绑，不可用
    * */
    private Integer status;

    private String remark;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

}
