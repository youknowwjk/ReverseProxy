package com.autel.cloud.pile.base.domain.utils;


import org.apache.commons.lang3.StringUtils;

/**
 * @author X21147
 * @Description
 * @date 2023/4/22 21:23
 */
public class CharUtils {

    /**
     * 处理模糊搜索的百分号
     */
    public static final String PERCENT_SIGN = "%";

    /**
     * 处理模糊搜索的下划线
     */
    public static final String UNDERSCORE = "_";

    /**
     * 处理模糊搜索的双斜杠
     */
    public static final String SLASH = "\\";

    /**
     * 处理模糊搜索的 % 和 _ 特殊字符
     * @param keyWord
     * @return
     */
    public static String handleChar(String keyWord) {
        if (StringUtils.isBlank(keyWord)) {
            return null;
        }

        if (keyWord.contains(CharUtils.SLASH)){
            keyWord = keyWord.replaceAll("\\\\", "\\\\\\\\");
        }
        if (keyWord.contains(CharUtils.PERCENT_SIGN)) {
            keyWord = keyWord.replace("%", "\\%");
        }
        if (keyWord.contains(CharUtils.UNDERSCORE)) {
            keyWord = keyWord.replace("_", "\\_");
        }

        return keyWord;
    }
}
