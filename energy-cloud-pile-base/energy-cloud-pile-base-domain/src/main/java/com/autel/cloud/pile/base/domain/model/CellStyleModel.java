package com.autel.cloud.pile.base.domain.model;

import lombok.Data;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.xssf.usermodel.XSSFColor;

/**
 * 样式信息类
 * @author A22588
 */
@Data
public class CellStyleModel {
    /**
     * sheet名称
     */
    private String sheetName;
    /**
     * 列索引
     */
    private int colIndex;
    /**
     * 行索引
     */
    private int rowIndex;
    /**
     * 字体名称
     */
    private String fontName;
    /**
     * 字体大小
     */
    private Double fontHeight;
    /**
     * 字体颜色
     */
    private Object fontColor;
    /**
     * 字体加粗
     */
    private Boolean fontBold;
    /**
     * 字体斜体
     */
    private Boolean fontItalic;
    /**
     * 字体下划线
     */
    private Byte fontUnderLine;
    /**
     * 字体上标下标
     */
    private Short fontTypeOffset;
    /**
     * 字体删除线
     */
    private Boolean fontStrikeout;
    /**
     * 背景颜色
     */
    private Object backgroundColor;

    /**
     * 上边框线条类型
     */
    private BorderStyle borderTop;
    /**
     * 右边框线条类型
     */
    private BorderStyle borderRight;
    /**
     * 下边框线条类型
     */
    private BorderStyle borderBottom;
    /**
     * 左边框线条类型
     */
    private BorderStyle borderLeft;
    /**
     * 上边框线条颜色
     */
    private Object topBorderColor;
    /**
     * 上边框线条颜色
     */
    private Object rightBorderColor;
    /**
     * 下边框线条颜色
     */
    private Object bottomBorderColor;
    /**
     */
    private Object leftBorderColor;
    /**
     * 水平对齐方式
     */
    private HorizontalAlignment horizontalAlignment;
    /**
     * 垂直对齐方式
     */
    private VerticalAlignment verticalAlignment;
    /**
     * 自动换行方式
     */
    private Boolean wrapText;

    /**
     * 生成样式信息
     *
     * @param sheetName
     * @param rowIndex
     * @param columnIndex
     * @param fontHeight
     * @param fontColor
     * @param fontBold
     * @param backgroundColor
     * @return
     */
    public static CellStyleModel createCellStyleModel(String sheetName, int rowIndex, int columnIndex
            , Double fontHeight, Object fontColor, Boolean fontBold, Object backgroundColor) {
        CellStyleModel cellStyleModel = new CellStyleModel();
        //sheet页名称
        cellStyleModel.setSheetName(sheetName);
        //行号
        cellStyleModel.setRowIndex(rowIndex);
        //列号
        cellStyleModel.setColIndex(columnIndex);

        //设置字体样式
        //字体大小
        fontHeight = fontHeight != null && fontHeight <= 0 ? null : fontHeight;
        cellStyleModel.setFontHeight(fontHeight);
        //字体颜色
        fontColor = fontColor != null && (!(fontColor instanceof IndexedColors) && !(fontColor instanceof XSSFColor))
                ? null : fontColor;
        cellStyleModel.setFontColor(fontColor);
        //字体加粗
        cellStyleModel.setFontBold(fontBold);
        //背景颜色
        backgroundColor = backgroundColor != null && (!(backgroundColor instanceof IndexedColors) && !(backgroundColor instanceof XSSFColor))
                ? null : backgroundColor;
        cellStyleModel.setBackgroundColor(backgroundColor);
        return cellStyleModel;
    }
}

