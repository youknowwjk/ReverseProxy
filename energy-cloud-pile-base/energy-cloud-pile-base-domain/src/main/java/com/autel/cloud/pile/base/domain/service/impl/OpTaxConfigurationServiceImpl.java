package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import com.alibaba.excel.util.FileUtils;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.convert.OpTaxConfigurationConvert;
import com.autel.cloud.pile.base.domain.repository.OpTaxConfigurationRepository;
import com.autel.cloud.pile.base.domain.service.OpTaxConfigurationService;
import com.autel.cloud.pile.base.dto.OpCostRuleDistributeDTO;
import com.autel.cloud.pile.base.dto.tax.StationLocationDTO;
import com.autel.cloud.pile.base.enums.FileExtensionEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardExcelEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpTaxConfigurationEntity;
import com.autel.cloud.pile.base.vo.tax.OpTaxConfigurationVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.math.BigDecimal;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * @Author A22599
 * @Date 2023/02/06
 * @Function Autel管理端-默认税率配置 业务逻辑层 实现类
 */
@Service
@Slf4j
public class OpTaxConfigurationServiceImpl implements OpTaxConfigurationService {

    @Autowired
    private OpTaxConfigurationRepository opTaxConfigurationRepository;

    @Autowired
    private ThreadPoolTaskExecutor threadPoolTaskExecutor;

    /**
     * 执行的步长
     */
    private final Integer STEP_SIZE = 500;

    /**
     * @param stationLocationDTO 场站地理位置信息
     * @return Autel管理端的默认税率配置信息
     * @function 税费重构——根据场站地理位置信息获取Autel管理端的默认税率配置信息
     */
    @Override
    public OpTaxConfigurationVO getOpTaxConfigurationByStationLocation(StationLocationDTO stationLocationDTO) {

        log.info("====>>>>OpTaxConfigurationServiceImpl.getOpTaxConfigurationByStationLocation stationLocationDTO : {}", JSON.toJSONString(stationLocationDTO));

        if (stationLocationDTO == null) {
            return null;
        }
        String countryShortCode = stationLocationDTO.getCountryShortCode();
        String provinceShortCode = stationLocationDTO.getProvinceShortCode();
        String zipCode = stationLocationDTO.getZipCode();
        return OpTaxConfigurationConvert.OpTaxConfigurationEntityToOpTaxConfigurationVO(opTaxConfigurationRepository.getOpTaxConfigurationByStationLocation(countryShortCode, provinceShortCode, zipCode));
    }

    /**
     * @param stationLocationDTO 场站地理位置信息
     * @return Autel管理端-默认税率配置数据列表
     * @function 税费重构——分页获取Autel管理端-默认税率配置数据列表（支持国家，州/省名称，邮政编码的模糊搜索）
     */
    @Override
    public Page<OpTaxConfigurationVO> getOpTaxConfigurationPageList(StationLocationDTO stationLocationDTO) {

        log.info("====>>>>OpTaxConfigurationServiceImpl.getOpTaxConfigurationPageList stationLocationDTO : {}", JSON.toJSONString(stationLocationDTO));

        if (stationLocationDTO == null) {
            return null;
        }
        Integer currentPage = stationLocationDTO.getCurrentPage();
        Integer pageSize = stationLocationDTO.getPageSize();
        String keyword = stationLocationDTO.getKeyword();
        Page<OpTaxConfigurationVO> page = new Page<>(currentPage, pageSize);
        Page<OpTaxConfigurationEntity> opTaxConfigurationEntityPage = opTaxConfigurationRepository.getOpTaxConfigurationPageList(currentPage, pageSize, keyword);
        if (opTaxConfigurationEntityPage != null) {
            List<OpTaxConfigurationEntity> opTaxConfigurationEntityList = opTaxConfigurationEntityPage.getRecords();
            if (ObjectUtils.isNotEmpty(opTaxConfigurationEntityList)) {
                List<OpTaxConfigurationVO> opTaxConfigurationVOList = new ArrayList<>();
                for (OpTaxConfigurationEntity opTaxConfigurationEntity : opTaxConfigurationEntityList) {
                    opTaxConfigurationVOList.add(OpTaxConfigurationConvert.OpTaxConfigurationEntityToOpTaxConfigurationVO(opTaxConfigurationEntity));
                }
                page.setRecords(opTaxConfigurationVOList);
                page.setTotal(opTaxConfigurationEntityPage.getTotal());
            }
        }
        return page;
    }

    /**
     * @param request  请求对象
     * @param response 响应对象
     * @return 默认税率模版文件
     * @function 税费重构——下载默认税率模版文件
     */
    @Override
    public Void downloadDefaultTaxTemplateXLSX(HttpServletRequest request, HttpServletResponse response) {
        InputStream inputStream = null;
        try {
            String resourceFileName = BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX;
            String resourceFilePath = "xls/" + resourceFileName;
            inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(resourceFilePath);
            File tempFile = File.createTempFile("test", ".xlsx");
            if (inputStream != null) {
                FileUtils.writeToFile(tempFile, inputStream);
            }
            //下载
            response.setContentType("application/vnd.ms-excel");
            response.setCharacterEncoding("utf-8");
            String downLoadFileName = URLEncoder.encode(BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX.split("\\.")[0], "UTF-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + downLoadFileName + ".xlsx");
            byte[] fileToByteArray = FileUtils.readFileToByteArray(tempFile);
            response.getOutputStream().write(fileToByteArray);
        } catch (IOException e) {
            log.error("downModuleResourceXls:" + e);
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    log.error("OpTaxConfigurationServiceImpl.downloadDefaultTaxTemplateXLSX close inputStream exception = ", e);
                }
            }
        }
        return null;
    }

    /**
     * @param request  请求对象
     * @param response 响应对象
     * @return 默认税率数据文件
     * @function 税费重构——导出默认税率数据文件
     */
    @Override
    public Void exportDefaultTaxDataFileXLSX(HttpServletRequest request, HttpServletResponse response) {
        FileInputStream fileInputStream = null;
        Workbook workbook = null;
        OutputStream outputStream = null;
        InputStream inputStream = null;
        try {
            String resourceFileName = BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX;
            String resourceFilePath = "xls/" + resourceFileName;
            inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(resourceFilePath);
            File tempFile = File.createTempFile("test", ".xlsx");
            if (inputStream != null) {
                FileUtils.writeToFile(tempFile, inputStream);
            }
            List<OpTaxConfigurationEntity> opTaxConfigurationEntityList = opTaxConfigurationRepository.getAllOpTaxConfiguration();
            if (ObjectUtils.isNotEmpty(opTaxConfigurationEntityList)) {
                // 写入数据
                fileInputStream = new FileInputStream(tempFile.getPath());
                workbook = new XSSFWorkbook(fileInputStream);
                outputStream = new FileOutputStream(tempFile.getPath());
                Sheet sheet = workbook.getSheet(BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX.split("\\.")[0]);
                for (int i = 0; i < opTaxConfigurationEntityList.size(); i++) {
                    OpTaxConfigurationEntity opTaxConfigurationEntity = opTaxConfigurationEntityList.get(i);
                    Row row = sheet.createRow(i + 2);
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getCountryName())) {
                        row.createCell(0).setCellValue(opTaxConfigurationEntity.getCountryName());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getCountryShortCode())) {
                        row.createCell(1).setCellValue(opTaxConfigurationEntity.getCountryShortCode());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getProvinceName())) {
                        row.createCell(2).setCellValue(opTaxConfigurationEntity.getProvinceName());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getProvinceShortCode())) {
                        row.createCell(3).setCellValue(opTaxConfigurationEntity.getProvinceShortCode());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getCityName())) {
                        row.createCell(4).setCellValue(opTaxConfigurationEntity.getCityName());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getZipCode())) {
                        row.createCell(5).setCellValue(opTaxConfigurationEntity.getZipCode());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getDefaultTaxRateName())) {
                        row.createCell(6).setCellValue(opTaxConfigurationEntity.getDefaultTaxRateName());
                    }
                    if (opTaxConfigurationEntity.getDefaultTax() != null) {
                        row.createCell(7).setCellValue(opTaxConfigurationEntity.getDefaultTax().stripTrailingZeros().toPlainString());
                    }
                    if (StringUtils.isNotBlank(opTaxConfigurationEntity.getLocalTaxRateName())) {
                        row.createCell(8).setCellValue(opTaxConfigurationEntity.getLocalTaxRateName());
                    }
                    if (opTaxConfigurationEntity.getLocalTax() != null) {
                        row.createCell(9).setCellValue(opTaxConfigurationEntity.getLocalTax().stripTrailingZeros().toPlainString());
                    }
                    if (opTaxConfigurationEntity.getCompositeMethod() != null) {
                        row.createCell(10).setCellValue(String.valueOf(opTaxConfigurationEntity.getCompositeMethod()));
                    }
                }
                workbook.write(outputStream);
            }
            //下载
            response.setContentType("application/vnd.ms-excel");
            response.setCharacterEncoding("utf-8");
            String downLoadFileName = URLEncoder.encode(BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX.split("\\.")[0], "UTF-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + downLoadFileName + ".xlsx");
            byte[] fileToByteArray = FileUtils.readFileToByteArray(tempFile);
            //加上设置大小 下载下来的excel文件才不会在打开前提示修复
            response.addHeader("Content-Length", String.valueOf(tempFile.length()));
            response.getOutputStream().write(fileToByteArray);
        } catch (IOException e) {
            log.error("downModuleResourceXls:" + e);
        } finally {
            if (workbook != null) {
                try {
                    workbook.close();
                } catch (IOException e) {
                    log.error("OpTaxConfigurationServiceImpl.exportDefaultTaxDataFileXLSX close workbook exception = ", e);
                }
            }
            if (fileInputStream != null) {
                try {
                    fileInputStream.close();
                } catch (IOException e) {
                    log.error("OpTaxConfigurationServiceImpl.exportDefaultTaxDataFileXLSX close fileInputStream exception = ", e);
                }
            }
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    log.error("OpTaxConfigurationServiceImpl.exportDefaultTaxDataFileXLSX close inputStream exception = ", e);
                }
            }
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    log.error("OpTaxConfigurationServiceImpl.exportDefaultTaxDataFileXLSX close outputStream exception = ", e);
                }
            }
        }
        return null;
    }

    /**
     * @param multipartFile 文件对象
     * @return 上传操作是否成功的标志
     * @function 税费重构——上传默认税率数据文件
     */
    @Override
    public Boolean uploadDefaultTaxDataFileXLSX(MultipartFile multipartFile) {
        // 非空校验
        if (multipartFile == null || multipartFile.isEmpty()) {
            throw new MessageCodeException(PileBaseEnum.CHARGE_CARD_EXCEL_IMPORT_EXPLAIN_FAIL);
        }
        // 校验文件名
        String originalFilename = multipartFile.getOriginalFilename();

        log.info("====>>>>OpTaxConfigurationServiceImpl.uploadDefaultTaxDataFileXLSX originalFilename : {}", JSON.toJSONString(originalFilename));

        if (StringUtils.isBlank(originalFilename)
                || !originalFilename.startsWith(BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX.split("\\.")[0])) {
            throw new MessageCodeException(PileBaseEnum.NOT_A_TEMPLATE_FILE);
        }
        // 校验文件后缀
        String extension = Objects.requireNonNull(FilenameUtils.getExtension(originalFilename)).toLowerCase();
        if (!Objects.equals(extension, FileExtensionEnum.XLSX.getName())
                && !Objects.equals(extension, FileExtensionEnum.XLS.getName())
                && !Objects.equals(extension, FileExtensionEnum.CSV.getName())) {
            throw new MessageCodeException(PileBaseEnum.WRONG_FILE_FORMAT);
        }
        // 解析Excel
        // 1.获取上传文件输入流
        List<List<Object>> read;
        InputStream inputStream = null;
        try {
            inputStream = multipartFile.getInputStream();
            // 2.应用HUtool ExcelUtil获取ExcelReader指定输入流和sheet
            ExcelReader excelReader = ExcelUtil.getReader(inputStream, BaseConstant.DEFAULT_TAX_TEMPLATE_XLSX.split("\\.")[0]);
            // 3.读取每一行数据
            read = excelReader.read();
        } catch (Exception e) {
            log.error("读取文件发生错误！e : {}", e);
            throw new MessageCodeException(PileBaseEnum.NOT_A_TEMPLATE_FILE);
        } finally {
            if (null != inputStream) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    log.error("关闭文件流发生错误！e : {}", e);
                }
            }
        }
        if (ObjectUtils.isEmpty(read) || read.size() <= 2) {
            throw new MessageCodeException(PileBaseEnum.TEMPLATE_FILE_FORMAT_ERROR);
        }

        log.info("====>>>>OpTaxConfigurationServiceImpl.uploadDefaultTaxDataFileXLSX 共 : {}条数据", JSON.toJSONString(read.size()));

        List<OpTaxConfigurationEntity> opTaxConfigurationEntityList = new LinkedList<>();
        int errorLines = 0;
        try {
            Long sellerId = UserUtil.getSellerId();
            for (int i = 2; i < read.size(); i++) {
                errorLines = i + 1;
                List<Object> row = read.get(i);
                if (ObjectUtils.isNotEmpty(row)) {
                    OpTaxConfigurationEntity opTaxConfigurationEntity = new OpTaxConfigurationEntity();
                    if (row.size() >= 1
                            && row.get(0) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(0)))) {
                        opTaxConfigurationEntity.setCountryName(String.valueOf(row.get(0)));
                    }
                    if (row.size() >= 2
                            && row.get(1) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(1)))) {
                        opTaxConfigurationEntity.setCountryShortCode(String.valueOf(row.get(1)));
                    }
                    if (row.size() >= 3
                            && row.get(2) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(2)))) {
                        opTaxConfigurationEntity.setProvinceName(String.valueOf(row.get(2)));
                    }
                    if (row.size() >= 4
                            && row.get(3) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(3)))) {
                        opTaxConfigurationEntity.setProvinceShortCode(String.valueOf(row.get(3)));
                    }
                    if (row.size() >= 5
                            && row.get(4) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(4)))) {
                        opTaxConfigurationEntity.setCityName(String.valueOf(row.get(4)));
                    }
                    if (row.size() >= 6
                            && row.get(5) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(5)))) {
                        opTaxConfigurationEntity.setZipCode(String.valueOf(row.get(5)));
                    }
                    if (row.size() >= 7
                            && row.get(6) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(6)))) {
                        opTaxConfigurationEntity.setDefaultTaxRateName(String.valueOf(row.get(6)));
                    }
                    if (row.size() >= 8
                            && row.get(7) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(7)))
                            && NumberUtils.isCreatable(String.valueOf(row.get(7)))) {
                        opTaxConfigurationEntity.setDefaultTax(new BigDecimal(String.valueOf(row.get(7))));
                    }
                    if (row.size() >= 9
                            && row.get(8) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(8)))) {
                        opTaxConfigurationEntity.setLocalTaxRateName(String.valueOf(row.get(8)));
                    }
                    if (row.size() >= 10
                            && row.get(9) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(9)))
                            && NumberUtils.isCreatable(String.valueOf(row.get(9)))) {
                        opTaxConfigurationEntity.setLocalTax(new BigDecimal(String.valueOf(row.get(9))));
                    }
                    if (row.size() >= 11
                            && row.get(10) != null
                            && StringUtils.isNotBlank(String.valueOf(row.get(10)))
                            && NumberUtils.isDigits(String.valueOf(row.get(10)))) {
                        opTaxConfigurationEntity.setCompositeMethod(Integer.valueOf(String.valueOf(row.get(10))));
                    }
                    opTaxConfigurationEntity.setCreateBy(sellerId);
                    opTaxConfigurationEntity.setUpdateBy(sellerId);
                    opTaxConfigurationEntity.setCreateTime(System.currentTimeMillis());
                    opTaxConfigurationEntity.setUpdateTime(System.currentTimeMillis());
                    opTaxConfigurationEntity.setDeleted(0);
                    opTaxConfigurationEntity.setId(IdWorker.getId());
                    opTaxConfigurationEntityList.add(opTaxConfigurationEntity);
                }
            }
        } catch (NumberFormatException e) {

            log.info("====>>>>OpTaxConfigurationServiceImpl.uploadDefaultTaxDataFileXLSX 出现了数字转换异常错误！ e : {}", e);

            throw new MessageCodeException(PileBaseEnum.NUMBERS_ARE_REQUIRED, new Object[]{errorLines});
        } catch (Exception e) {

            log.info("====>>>>OpTaxConfigurationServiceImpl.uploadDefaultTaxDataFileXLSX 出现了其他错误！ e : {}", e);

            throw new MessageCodeException(PileBaseEnum.PLEASE_FILL_IN_THE_DATA);
        }
        // 校验数据的合法性
        if (ObjectUtils.isNotEmpty(opTaxConfigurationEntityList)) {
            // 默认税率 地方税率必须大于等于0，小于等于100，即[0, 100],复合方式必须为1,2,3
            for (OpTaxConfigurationEntity opTaxConfigurationEntity : opTaxConfigurationEntityList) {
                BigDecimal defaultTax = opTaxConfigurationEntity.getDefaultTax();
                BigDecimal localTax = opTaxConfigurationEntity.getLocalTax();
                Integer compositeMethod = opTaxConfigurationEntity.getCompositeMethod();
                if ((defaultTax != null
                        && (defaultTax.doubleValue() < 0d
                        || defaultTax.doubleValue() > 100d))
                        || (localTax != null
                        && (localTax.doubleValue() < 0d
                        || localTax.doubleValue() > 100d))) {
                    throw new MessageCodeException(PileBaseEnum.DATA_FORMAT_ERROR);
                }
                if (compositeMethod != null
                        && (compositeMethod.compareTo(1) < 0
                        || compositeMethod.compareTo(3) > 0)) {
                    throw new MessageCodeException(PileBaseEnum.WRONG_COMBINATION);
                }
            }
        }
        // 校验是否有重复数据
        if (ObjectUtils.isNotEmpty(opTaxConfigurationEntityList) && opTaxConfigurationEntityList.size() > 1) {
            int size = opTaxConfigurationEntityList
                    .stream()
                    .collect(Collectors.groupingBy(OpTaxConfigurationEntity::getDeduplicationFlagField))
                    .size();
            if (size != opTaxConfigurationEntityList.size()) {
                throw new MessageCodeException(PileBaseEnum.THE_UPLOADED_FILE_HAS_DUPLICATE_DATA);
            }
        }
        // 删除所有数据
        opTaxConfigurationRepository
                .remove(new LambdaQueryWrapper<OpTaxConfigurationEntity>()
                        .eq(OpTaxConfigurationEntity::getDeleted, 0));
        // 多线程处理保存
        if (CollUtil.isNotEmpty(opTaxConfigurationEntityList)) {
            // 执行次数
            int count = opTaxConfigurationEntityList.size() / this.STEP_SIZE + ((opTaxConfigurationEntityList.size() % this.STEP_SIZE) == 0 ? 0 : 1);

            log.info("=========== 入参的集合的大小opCostRuleDistributeDTOList.size : {}, 执行次数count : {}", opTaxConfigurationEntityList.size(), count);

            for (int i = 0; i < count; i++) {
                List<OpTaxConfigurationEntity> opCostRuleDistributePage = opTaxConfigurationEntityList.subList(i * this.STEP_SIZE, ((i + 1) * this.STEP_SIZE >= opTaxConfigurationEntityList.size() ? (opTaxConfigurationEntityList.size()) : ((i + 1) * this.STEP_SIZE)));
                threadPoolTaskExecutor.execute(() -> opTaxConfigurationRepository.saveBatch(opCostRuleDistributePage));
            }
        }
        return Boolean.TRUE;
    }
}

