package com.autel.cloud.pile.base.controller.listener;


import com.alibaba.excel.context.AnalysisContext;
import com.alibaba.excel.event.AnalysisEventListener;
import com.autel.cloud.pile.base.domain.repository.OpCountryRepository;
import com.autel.cloud.pile.base.dto.CountryExcelDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * @Author: A22327
 * @CreateTime: 2022/5/18 20:08
 * @Description: Excel监听器
 */
@Slf4j
@Component
public class ExcelListener extends AnalysisEventListener<CountryExcelDTO> {

    private OpCountryRepository opCountryRepository;

    public ExcelListener(OpCountryRepository opCountryRepository) {
        this.opCountryRepository = opCountryRepository;
    }

    List<CountryExcelDTO> list = new ArrayList<>();

    @Override
    public void invoke(CountryExcelDTO countryExcelDTO, AnalysisContext analysisContext) {
        list.add(countryExcelDTO);
    }

    @Override
    public void doAfterAllAnalysed(AnalysisContext analysisContext) {
        log.info("数据={}", list);

    }
}
