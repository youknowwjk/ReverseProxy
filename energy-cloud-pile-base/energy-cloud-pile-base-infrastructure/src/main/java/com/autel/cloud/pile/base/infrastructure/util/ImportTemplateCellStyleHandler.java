package com.autel.cloud.pile.base.infrastructure.util;

import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.handler.context.CellWriteHandlerContext;
import com.alibaba.excel.write.metadata.holder.WriteSheetHolder;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.*;

/**
 * @author A22208
 */
@Slf4j
public class ImportTemplateCellStyleHandler implements CellWriteHandler {

    @Override
    public void afterCellDispose(CellWriteHandlerContext context) {
        Boolean head = context.getHead();
        if (head) {
            Integer columnIndex = context.getColumnIndex();
            Row row = context.getRow();
            int rowNum = row.getRowNum();
            WriteSheetHolder writeSheetHolder = context.getWriteSheetHolder();
            Sheet sheet = writeSheetHolder.getSheet();
            sheet.setColumnWidth(context.getCell().getColumnIndex(), 4000);
            if (rowNum == 0 || rowNum == 1 || rowNum == 2) {
                WriteCellData<?> cellData = context.getFirstCellData();
                WriteCellStyle writeCellStyle = cellData.getOrCreateStyle();
                WriteFont writeFont = new WriteFont();
                writeFont.setColor(IndexedColors.RED.getIndex());
                writeCellStyle.setWriteFont(writeFont);
                writeCellStyle.setHorizontalAlignment(HorizontalAlignment.LEFT);
            }
            if (columnIndex >= 4) {
                WriteCellData<?> cellData = context.getFirstCellData();
                WriteCellStyle writeCellStyle = cellData.getOrCreateStyle();
                writeCellStyle.setFillForegroundColor(IndexedColors.YELLOW.getIndex());
                // 这里需要指定 FillPatternType 为FillPatternType.SOLID_FOREGROUND
                // 要不然背景色不会生效
                writeCellStyle.setFillPatternType(FillPatternType.SOLID_FOREGROUND);
            }
            if (columnIndex == 1) {
                sheet.setColumnWidth(context.getCell().getColumnIndex(), 6800);
            }
        }
    }

}
