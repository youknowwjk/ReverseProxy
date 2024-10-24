package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置表 实体类
 */
@Accessors(chain = true)
@TableName(value = "op_tax_configuration")
@Data
public class OpTaxConfigurationEntity {

    private static final long serialVersionUID = 1607863453767845L;

    /**
     * 主键id
     */
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 国家名称
     */
    @TableField(value = "country_name")
    private String countryName;

    /**
     * 国家简码
     */
    @TableField(value = "country_short_code")
    private String countryShortCode;

    /**
     * 州/省名称
     */
    @TableField(value = "province_name")
    private String provinceName;

    /**
     * 州/省简码
     */
    @TableField(value = "province_short_code")
    private String provinceShortCode;

    /**
     * 城市名称
     */
    @TableField(value = "city_name")
    private String cityName;

    /**
     * 邮政编码
     */
    @TableField(value = "zip_code")
    private String zipCode;

    /**
     * 默认税率名称
     */
    @TableField(value = "default_tax_rate_name")
    private String defaultTaxRateName;

    /**
     * 默认税率
     */
    @TableField(value = "default_tax")
    private BigDecimal defaultTax;

    /**
     * 地方税率名称
     */
    @TableField(value = "local_tax_rate_name")
    private String localTaxRateName;

    /**
     * 地方税率
     */
    @TableField(value = "local_tax")
    private BigDecimal localTax;

    /**
     * 复合方式(1:添加;2:替代;3:结合)
     */
    @TableField(value = "composite_method")
    private Integer compositeMethod;

    /**
     * 创建者
     */
    @TableField(value = "create_by")
    private Long createBy;

    /**
     * 修改者
     */
    @TableField(value = "update_by")
    private Long updateBy;

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

    /**
     * 是否删除 1删除 0不删除
     */
    @TableField(value = "deleted")
    private Integer deleted;

    /**
     * 去重标志字段
     */
    @TableField(exist = false)
    private String deduplicationFlagField;

    /**
     * @return 返回deduplicationFlagField字段的值
     * @function 重写get方法
     */
    public String getDeduplicationFlagField() {
        String countryShortCode = "";
        if (this.countryShortCode != null) {
            countryShortCode = this.countryShortCode;
        }
        String provinceShortCode = "";
        if (this.provinceShortCode != null) {
            provinceShortCode = this.provinceShortCode;
        }
        String zipCode = "";
        if (this.zipCode != null) {
            zipCode = this.zipCode;
        }
        return countryShortCode + provinceShortCode + zipCode;
    }
}
