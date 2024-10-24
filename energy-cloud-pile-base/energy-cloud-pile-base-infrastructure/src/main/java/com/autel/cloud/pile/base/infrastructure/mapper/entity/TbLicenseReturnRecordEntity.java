package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 15:50
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@TableName("tb_license_return_record")
public class TbLicenseReturnRecordEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private String orderId;

    private Long returnId;

    private String licenseCode;


    /**
     * 退货状态, 0:待退货,  1:退货成功,  2:退货失败
     */
    private Integer status;


    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

    public TbLicenseReturnRecordEntity(Integer status) {
        this.status = status;
    }


}