package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import com.alibaba.excel.exception.ExcelDataConvertException;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardExcelEntity;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author A22282
 * @Date 2022/5/30 20:49
 */
@Slf4j
public class ChargeCardServiceImportExcelListener extends AnalysisEventListener<ChargeCardExcelEntity> {

    private Long sellerId;

    public ChargeCardServiceImportExcelListener(Long sellerId) {
        this.sellerId = sellerId;
    }

    /**
     * 用于暂时存储数据
     */
    private List<ChargeCardExcelEntity> dataList = new ArrayList<>(10000);

    /**
     * 记录行数 第一行为表头
     */
    int row = 1;

    @Override
    public void invoke(ChargeCardExcelEntity entity, AnalysisContext analysisContext) {
        if (sellerId.longValue() == 0L){
            String cardAlias = entity.getCardAlias();
            String cardNumber = entity.getCardNumber();
            entity.setCardNumber(cardAlias);
            entity.setCardType(cardNumber);
        }
        //第一行为表头
        if (StringUtils.isEmpty(entity.getCardNumber())){
            log.info("Card number is null.");
        }else {
            row++;
            dataList.add(entity);
            if (row >= 10000){
                throw new IllegalArgumentException("Excel table has more than 10,000 rows");
            }
        }
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext analysisContext) {

    }

    /**
     * 在转换异常 获取其他异常下会调用本接口。抛出异常则停止读取。如果这里不抛出异常则 继续读取下一行。
     *
     * @param exception
     * @param context
     * @throws Exception ExcelAnalysisException
     */
    @Override
    public void onException(Exception exception, AnalysisContext context) throws Exception {
        //如果是某一个单元格的转换异常 能获取到具体行号,如果要获取头的信息 配合invokeHeadMap使用
        if (exception instanceof ExcelDataConvertException) {
            ExcelDataConvertException excelDataConvertException = (ExcelDataConvertException) exception;
            throw new MessageCodeException("第 " + excelDataConvertException.getRowIndex() + " 行，第 " + excelDataConvertException.getColumnIndex() + " 列解析异常");
        }
        //若不抛异常 则会继续解析下一行
        throw exception;
    }

    public List<ChargeCardExcelEntity> getDataList() {
        return dataList;
    }
}
