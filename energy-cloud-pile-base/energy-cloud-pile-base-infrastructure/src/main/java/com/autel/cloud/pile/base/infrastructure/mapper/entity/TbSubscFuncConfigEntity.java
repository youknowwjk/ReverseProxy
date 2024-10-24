package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serializable;

/**
 * @description
 * @auther A23204
 * @datetime 2023/12/6 14:02
 */
@Data
@TableName("tb_subsc_func_config")
public class TbSubscFuncConfigEntity implements Serializable {

    private static final long serialVersionUID = 6982885502552007983L;

    @TableId(type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 控制功能id
     */
    private String functionId;

    /**
     * 控制功能描述
     */
    private String functionDesc;

    /**
     * 控制功能id
     */
    private String sourceId;

    /**
     * 功能控制点所属业务系统id
     */
    private String sourceName;

    private String createBy;

    private String updateBy;

    private Long createTime;

    private Long updateTime;

}
