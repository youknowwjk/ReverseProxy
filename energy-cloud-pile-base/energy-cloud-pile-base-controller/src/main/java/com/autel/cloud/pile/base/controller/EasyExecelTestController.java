package com.autel.cloud.pile.base.controller;


import com.alibaba.excel.EasyExcelFactory;
import com.autel.cloud.pile.base.controller.listener.ExcelListener;
import com.autel.cloud.pile.base.domain.repository.OpCountryRepository;
import com.autel.cloud.pile.base.dto.CountryExcelDTO;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;

@RestController
@Api(value = "测试AMQP", hidden = false)
@Slf4j
@Validated
public class EasyExecelTestController {

    @Autowired
    private OpCountryRepository opCountryRepository;

    @PostMapping("/importExcel")
    public void importExcel(MultipartFile file) throws Exception {
        InputStream inputStream = file.getInputStream();
        EasyExcelFactory.read(inputStream, CountryExcelDTO.class, new ExcelListener(opCountryRepository)).sheet(0).doRead();
    }

}
