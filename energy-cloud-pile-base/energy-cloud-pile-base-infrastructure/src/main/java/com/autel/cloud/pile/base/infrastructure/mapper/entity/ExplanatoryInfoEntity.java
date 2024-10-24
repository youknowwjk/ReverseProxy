package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@TableName("tb_explanatory_information")
public class ExplanatoryInfoEntity {

    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 菜单ID
     */
    private Long menuId;

    /**
     * 标签Key
     */
    private String labelKey;

    /**
     * 国际化key
     */
    @TableField("label_i18n_key")
    private String labelI18nKey;

    /**
     * 语言
     */
    private String language;

    /**
     * 说明文本内容
     */
    private String content;

    @TableField(fill = FieldFill.INSERT)
    private Long createTime;

    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updateTime;
}
