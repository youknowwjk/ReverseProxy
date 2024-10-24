package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * 提示用户license状态 实体类
 */
@Data
@TableName("tb_remind_user_license_status")
public class RemindUserLicenseStatusEntity implements Serializable {

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    private String userId;

    /**
     * @see com.autel.cloud.pile.base.enums.license.RemindEnableEnum
     */
    private Integer remindEnable;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;
}
