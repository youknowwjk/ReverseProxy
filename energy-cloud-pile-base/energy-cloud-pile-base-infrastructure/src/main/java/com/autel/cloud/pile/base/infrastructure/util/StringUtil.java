package com.autel.cloud.pile.base.infrastructure.util;

import org.apache.commons.lang3.StringUtils;

import java.math.BigDecimal;
import java.text.NumberFormat;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @Author A22599
 * @Date 2022/02/06
 * @Function 字符串工具类
 */
public class StringUtil {
    public static final String HEAD_NAME = "Content-disposition";
    public static final String HEAD_VALUE = "attachment;filename*=utf-8''";
    public static final String XLSX = "xlsx";
    public static final String XLS = "xls";
    public static final String CSV = "csv";
    private StringUtil() {
    }

    public static Integer formatStringToInteger(String str) {
        try {
            return Integer.valueOf(str);
        }catch (Exception e){
            return null;
        }
    }

    /**
     * 去除小数后面多余的0 (返回String类型，注意返回格式为：1,200.300)
     *
     * @param s
     * @return
     */
    public static String FormatNumber(String s) {
        //格式化数字
        NumberFormat nf = NumberFormat.getInstance();
        if (s.indexOf(".") >= 1) {
            //去掉多余的0
            s = s.replaceAll("0+$", "");
            //如最后一位是.则去掉
            s = s.replaceAll("[.]$", "");
        }
        //避免科学计数
        s = new BigDecimal(s).toPlainString();
        //去掉  值为 0 的所有小数
        s = nf.format(Double.parseDouble(s));
        return s;
    }

    /**
     * 去除小数后面多余的0 (返回String类型)
     *
     * @param value
     * @return
     */
    public static String convertByString(String value) {
        String regex = "^(-?\\d*)(\\.?0*)$";
        Matcher matcher = Pattern.compile(regex).matcher(value);
        if (matcher.find()) {
            return matcher.group(1);
        }
        String regex1 = "^(-?\\d*\\.\\d*[1-9])(0*)$";
        Matcher matcher1 = Pattern.compile(regex1).matcher(value);
        if (matcher1.find()) {
            return matcher1.group(1);
        }
        return null;
    }

    /**
     * 去除小数后面多余的0 （返回BigDecimal类型）
     *
     * @param value
     * @return
     */
    public static BigDecimal convertByBigDecimal(BigDecimal value) {
        return value.stripTrailingZeros();
    }

    /**
     * function: mysql的模糊查询时特殊字符转义
     *
     * @param before
     * @return
     */
    public static String escapeChar(String before) {
        if (StringUtils.isNotBlank(before)) {
            // 第一步：将源字符串中的所有的\转换成\\
            before = before.replace("\\", "\\\\");
            // 第二步：将第一步处理完的字符串中的所有的_转换成\_
            before = before.replace("_", "\\_");
            // 第三步：将第二步处理完的字符串中的所有的%转换成\%
            before = before.replace("%", "\\%");
        }
        return before;
    }
}