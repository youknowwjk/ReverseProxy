package com.autel.cloud.pile.base.domain.hanlder;

import com.alibaba.excel.metadata.Head;
import com.alibaba.excel.metadata.data.WriteCellData;
import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.metadata.holder.WriteSheetHolder;
import com.alibaba.excel.write.metadata.holder.WriteTableHolder;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;

import java.util.List;

public class EmptyCellWriteHandler implements CellWriteHandler {
    private final String defaultValue;

    public EmptyCellWriteHandler(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    @Override
    public void afterCellDispose(WriteSheetHolder writeSheetHolder, WriteTableHolder writeTableHolder, List<WriteCellData<?>> cellDataList, Cell cell, Head head, Integer relativeRowIndex, Boolean isHead) {
        if (!isHead) {
            if (cell.getCellType() == CellType.BLANK || cell.getStringCellValue() == null || cell.getStringCellValue().trim().isEmpty()) {
                cell.setCellValue(defaultValue);
            }
        }
    }

}
