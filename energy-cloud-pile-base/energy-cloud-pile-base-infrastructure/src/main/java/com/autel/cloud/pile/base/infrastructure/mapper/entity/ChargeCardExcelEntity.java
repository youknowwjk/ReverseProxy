package com.autel.cloud.pile.base.infrastructure.mapper.entity;

import com.alibaba.excel.annotation.ExcelProperty;
import lombok.Data;

/**
 * @Author A22282
 * @Date 2022/5/30 20:37
 */
@Data
public class ChargeCardExcelEntity {

    @ExcelProperty(value = "充电卡名称",index = 0)
    private String cardAlias;

    @ExcelProperty(value = "卡号（必填）",index = 1)
    private String cardNumber;

    @ExcelProperty(value = "充电卡品牌",index = 2)
    private String cardBrand;

    @ExcelProperty(value = "是否可以同时启动多笔订单",index = 3)
    private String startMultipleOrdersEnabled;

    @ExcelProperty(value = "客户组",index = 4)
    private String memberGroupName;

    @ExcelProperty(value = "关联车主",index = 5)
    private String driverName;

    @ExcelProperty(value = "卡类型（必填）",index = 10)
    private String cardType;
}
