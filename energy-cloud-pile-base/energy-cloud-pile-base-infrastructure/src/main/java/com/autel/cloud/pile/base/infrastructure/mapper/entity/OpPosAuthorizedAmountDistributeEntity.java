package com.autel.cloud.pile.base.infrastructure.mapper.entity;


import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置记录 实体类
 */
@Accessors(chain = true)
@TableName(value = "op_pos_authorized_amount_distribute")
@Data
public class OpPosAuthorizedAmountDistributeEntity implements Serializable {

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 充电桩序列号
     */
    @TableField(value = "pile_sn")
    private String pileSn;

    /**
     * 下发状态(0:下发失败; 1:成功)
     */
    @TableField(value = "status")
    private Integer status;

    /**
     * 商家(运营商)id
     */
    @TableField(value = "seller_id")
    private Long sellerId;

    /**
     * 修改者
     */
    @TableField(value = "update_by")
    private Long updateBy;

    /**
     * 创建者
     */
    @TableField(value = "create_by")
    private Long createBy;

    /**
     * 这条记录是否已经被删除(0:这条记录未被删除; 1:这条记录已被删除)
     */
    @TableField(value = "deleted")
    private Integer deleted;

    /**
     * 创建时间
     */
    @TableField(value = "create_time")
    private Long createTime;

    /**
     * 修改时间
     */
    @TableField(value = "update_time")
    private Long updateTime;
}
