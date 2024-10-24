package com.autel.cloud.pile.base.infrastructure.util;

import com.alibaba.excel.write.handler.SheetWriteHandler;
import com.alibaba.excel.write.metadata.holder.WriteSheetHolder;
import com.alibaba.excel.write.metadata.holder.WriteWorkbookHolder;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.xssf.streaming.SXSSFSheet;

/**
 * @author A22208
 */
public class CustomSheetWriteHandler implements SheetWriteHandler {

    @Override
    public void beforeSheetCreate(WriteWorkbookHolder writeWorkbookHolder, WriteSheetHolder writeSheetHolder) {
    }

    @Override
    public void afterSheetCreate(WriteWorkbookHolder writeWorkbookHolder, WriteSheetHolder writeSheetHolder) {
        for (int i = 0; i < writeSheetHolder.getHead().size(); i++) {
            // 设置为文本格式
            SXSSFSheet sxssfSheet = (SXSSFSheet) writeSheetHolder.getSheet();
            CellStyle cellStyle = writeWorkbookHolder.getCachedWorkbook().createCellStyle();
            // 49为文本格式
            cellStyle.setDataFormat((short) 49);
            // i为列，一整列设置为文本格式
            sxssfSheet.setDefaultColumnStyle(i, cellStyle);
        }
    }

}
