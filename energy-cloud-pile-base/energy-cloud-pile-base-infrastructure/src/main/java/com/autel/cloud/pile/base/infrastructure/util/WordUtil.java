package com.autel.cloud.pile.base.infrastructure.util;

import com.alibaba.excel.util.FileUtils;
import com.autel.cloud.pile.base.constant.I18nConstant;
import com.autel.cloud.pile.base.infrastructure.exception.MessageSourceUtil;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardExcelEntity;
import freemarker.template.Configuration;
import freemarker.template.Template;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Component
public class WordUtil {

    private Template template;
    private Configuration configuration;

    @Value("${server.doMain:}")
    private String serverDomain;

    @Autowired
    private MessageSourceUtil messageSourceUtil;


    @PostConstruct
    public void initWordTemplate() {
        try {
            //2、获取模板文件
            configuration = new Configuration();
            configuration.setClassForTemplateLoading(this.getClass(), "");
            String resourceFileName = "template.ftl";
            String resourceFilePath = "word/" + resourceFileName;
            InputStream inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(resourceFilePath);
            File templateFile = File.createTempFile("template", ".ftl");
            assert inputStream != null;
            FileUtils.writeToFile(templateFile, inputStream);
            configuration.setDirectoryForTemplateLoading(templateFile.getParentFile());
            template = configuration.getTemplate(templateFile.getName());
        } catch (IOException e) {
            log.info("初始化word模板失败：" + e);
        }
    }

    public void createWord(HttpServletResponse response) {
        //1、构造参数
        Map<String, Object> dataMap = new HashMap<>();
        getData(dataMap);
        //3、将填充数据填入模板文件并输出到目标文件
        File outFile = null;
        try {
            outFile = File.createTempFile("test", ".doc");
            Writer out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile)));
            assert template != null;
            template.process(dataMap, out);
            log.info("生成成功:{}", outFile.getPath());
        } catch (Exception e) {
            log.info("将填充数据填入模板文件并输出到目标文件失败:" + e);
        }
        //4、文件下载
        assert outFile != null;
        if (!outFile.exists()) {
            log.info("找不到服务器指定文件:{}", outFile.getPath());
            return;
        }
        response.setCharacterEncoding("utf-8");
        String downLoadFileName = null;
        try {
            //防止文件中文名乱码
            downLoadFileName = URLEncoder.encode(messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.TITLE), "UTF-8").replaceAll("\\+", "%20");
        } catch (UnsupportedEncodingException e) {
            log.info("编码错误");
        }
        response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + downLoadFileName + ".doc");
        byte[] buffer = new byte[1024];
        FileInputStream fileInputStream = null;
        BufferedInputStream bufferedInputStream = null;
        OutputStream outputStream = null;
        try {
            fileInputStream = new FileInputStream(outFile);
            bufferedInputStream = new BufferedInputStream(fileInputStream);
            outputStream = response.getOutputStream();
            int i = bufferedInputStream.read(buffer);
            while (i != -1) {
                outputStream.write(buffer, 0, i);
                i = bufferedInputStream.read(buffer);
            }
        } catch (Exception e) {
            log.info("");
        } finally {
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
            if (bufferedInputStream != null) {
                try {
                    bufferedInputStream.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
            if (fileInputStream != null) {
                try {
                    fileInputStream.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
        }
    }

    public void createWallBoxWord(HttpServletResponse response) {
        String resourceFileName = "WallBox桩激活指引.doc";
        String resourceFilePath = "word/" + resourceFileName;
        InputStream inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(resourceFilePath);
        File outFile = null;
        try {
            outFile = File.createTempFile("test", "xlsx");
        } catch (IOException ignore) {
        }
        assert outFile != null;
        assert inputStream != null;
        FileUtils.writeToFile(outFile, inputStream);
        //4、文件下载
        if (!outFile.exists()) {
            log.info("找不到服务器指定文件:{}", outFile.getPath());
            return;
        }
        response.setCharacterEncoding("utf-8");
        String downLoadFileName = null;
        try {
            //防止文件中文名乱码
            downLoadFileName = URLEncoder.encode(resourceFileName, "UTF-8").replaceAll("\\+", "%20");
        } catch (UnsupportedEncodingException e) {
            log.info("编码错误");
        }
        response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + downLoadFileName);
        byte[] buffer = new byte[1024];
        FileInputStream fileInputStream = null;
        BufferedInputStream bufferedInputStream = null;
        OutputStream outputStream = null;
        try {
            fileInputStream = new FileInputStream(outFile);
            bufferedInputStream = new BufferedInputStream(fileInputStream);
            outputStream = response.getOutputStream();
            int i = bufferedInputStream.read(buffer);
            while (i != -1) {
                outputStream.write(buffer, 0, i);
                i = bufferedInputStream.read(buffer);
            }
        } catch (Exception e) {
            log.info("");
        } finally {
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
            if (bufferedInputStream != null) {
                try {
                    bufferedInputStream.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
            if (fileInputStream != null) {
                try {
                    fileInputStream.close();
                } catch (IOException e) {
                    log.info("");
                }
            }
        }
    }

    private void getData(Map<String, Object> dataMap) {
        String serverUrl = serverDomain == null ? "gateway-eneprodeu.autel.com" : serverDomain;
        dataMap.put("Title", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.TITLE));
        dataMap.put("SmallTitle", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.SMALL_TITLE));
        dataMap.put("Line1", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE1));
        dataMap.put("Line2", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE2));
        dataMap.put("Line3", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE3));
        dataMap.put("Line4", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE4));
        dataMap.put("Line5", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE5));
        dataMap.put("Line6", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE6));
        dataMap.put("Line7", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE7));
        dataMap.put("Line8", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE8));
        dataMap.put("Line9", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE9));
        dataMap.put("Line10", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE10));
        dataMap.put("Line11", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE11));
        dataMap.put("Line12", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE12));
        dataMap.put("Line13", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE13));
        dataMap.put("Line14", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE14));
        dataMap.put("Line15", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE15));
        dataMap.put("Line16", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE16));
        dataMap.put("Line17", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE17));
        dataMap.put("Line18", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE18));
        dataMap.put("Line19", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE19));
        dataMap.put("Line20", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE20));
        dataMap.put("Line22", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE22));
        dataMap.put("Line222", serverUrl);
        dataMap.put("Line23", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE23));
        dataMap.put("Line24", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE24));
        dataMap.put("Line25", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE25));
        dataMap.put("Line26", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE26));
        dataMap.put("Line27", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE27));
        dataMap.put("Line28", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE28));
        dataMap.put("Line29", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE29));
        dataMap.put("Line30", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE30));
        dataMap.put("Line31", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE31));
        dataMap.put("Line32", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE32));
        dataMap.put("Line33", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE33));
        dataMap.put("Line34", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE34));
        dataMap.put("Line35", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE35));
        dataMap.put("Line36", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE36));
        dataMap.put("Line37", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE37));
        dataMap.put("Line38", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE38));
        dataMap.put("Line39", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE39));
        dataMap.put("Line40", messageSourceUtil.getMessage(I18nConstant.PileImportModule.thirdPartyStubActivationDocumentation.LINE40));
    }

    public void setConfiguration(Configuration configuration) {
        this.configuration = configuration;
    }
}
