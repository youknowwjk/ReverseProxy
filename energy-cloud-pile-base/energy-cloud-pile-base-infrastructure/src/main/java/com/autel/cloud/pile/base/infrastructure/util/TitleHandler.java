package com.autel.cloud.pile.base.infrastructure.util;

import com.alibaba.excel.write.handler.SheetWriteHandler;
import com.alibaba.excel.write.metadata.holder.WriteSheetHolder;
import com.alibaba.excel.write.metadata.holder.WriteWorkbookHolder;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.util.CellRangeAddressList;

import java.util.Map;

@Slf4j
public class TitleHandler implements SheetWriteHandler {

    /**
     * 下拉框值
     */
    private Map<Integer,String[]> dropDownMap;

    /**
     * 多少行有下拉
     */
    private static final Integer ROW_SIZE = 200;

    private static final int START_LINE = 4;

    public TitleHandler(Map<Integer,String[]> dropDownMap) {
        this.dropDownMap = dropDownMap;
    }

    @Override
    public void beforeSheetCreate(WriteWorkbookHolder writeWorkbookHolder, WriteSheetHolder writeSheetHolder) {
        // do nothing

    }

    @Override
    public void afterSheetCreate(WriteWorkbookHolder writeWorkbookHolder, WriteSheetHolder writeSheetHolder) {
        Sheet sheet = writeSheetHolder.getSheet();
        DataValidationHelper helper = sheet.getDataValidationHelper();

        dropDownMap.forEach((celIndex, strings) -> {
            // 区间设置
            CellRangeAddressList cellRangeAddressList = new CellRangeAddressList(START_LINE, ROW_SIZE, celIndex, celIndex);
            // 下拉内容
            DataValidationConstraint constraint = helper.createExplicitListConstraint(strings);
            DataValidation dataValidation = helper.createValidation(constraint, cellRangeAddressList);
            sheet.addValidationData(dataValidation);
        });
    }
}