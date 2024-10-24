package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.excel.EasyExcelFactory;
import com.alibaba.excel.support.ExcelTypeEnum;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.infrastructure.sysconfig.log.dto.UserOpLogDTO;
import com.autel.cloud.infrastructure.sysconfig.log.enums.BusinessType;
import com.autel.cloud.pile.base.domain.data.UserOpLogData;
import com.autel.cloud.pile.base.domain.repository.UserOpLogRepository;
import com.autel.cloud.pile.base.domain.service.UserOpLogService;
import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

/**
 * @author A22203
 * @Description
 * @Date 2022/5/17 16:34
 */
@Slf4j
@Service
public class UserOpLogServiceImpl implements UserOpLogService {

    @Autowired
    UserOpLogRepository userOpLogRepository;


    @Override
    public PageVO<UserOpLogDTO> queryPages(UserOpLogDTO requestDto) {
        return userOpLogRepository.selectUserOpLogPage(requestDto);
    }

    @SneakyThrows
    @Override
    public void export(UserOpLogDTO requestDto, HttpServletRequest request,
                       HttpServletResponse response) {
        // 获取日志信息
        List<UserOpLogDTO> logs = getLogs(requestDto);
        if (CollectionUtils.isEmpty(logs)) {
            log.error("分页查询日志信息为空，入参：{}", JSON.toJSONString(requestDto));
            return;
        }
        // 设置excel下载响应头属性
        setExcelRespProp(response, MessageSourceHolder.getMessage("userlog.export.file.name"),
                "XLSX");
        // 封装Excel数据集合
        List<UserOpLogData> logDataColl = getLogDataColl(logs);
        EasyExcelFactory.write(response.getOutputStream())
                .head(UserOpLogData.class)
                .excelType(ExcelTypeEnum.XLSX)
                .sheet(MessageSourceHolder.getMessage("userlog.export.file.name"))
                .doWrite(logDataColl);
    }

    /**
     * 封装Excel数据集合
     *
     * @param logs
     * @return
     */
    private List<UserOpLogData> getLogDataColl(List<UserOpLogDTO> logs) {
        List<UserOpLogData> dataColl = Lists.newArrayList();
        for (UserOpLogDTO log : logs) {
            UserOpLogData logData = new UserOpLogData();
            dataColl.add(logData);
            BeanUtils.copyProperties(log, logData);
            Long operTime = log.getOperTime();
            if (operTime != null && operTime != 0) {
                logData.setOperTime(DateUtil.localDateTimeToString(
                        DateUtil.timestampToLocalDatetime(operTime), DateUtil.DEFAULT_PATTERN));
            }
            Long createTime = log.getCreateTime();
            if (createTime != null && createTime != 0) {
                logData.setCreateTime(DateUtil.localDateTimeToString(
                        DateUtil.timestampToLocalDatetime(createTime), DateUtil.DEFAULT_PATTERN));
            }
            logData.setJsonResult(JSON.toJSONString(log.getJsonResult()));
            logData.setOperName(log.getOperName());
            logData.setBusinessType(BusinessType.foreach(log.getBusinessType()).getMessage());
        }
        return dataColl;
    }

    private List<UserOpLogDTO> getLogs(UserOpLogDTO requestDto) {
        PageVO<UserOpLogDTO> logPage = userOpLogRepository.selectUserOpLogPage(requestDto);
        List<UserOpLogDTO> logs = logPage.getContent();
        log.info("查询操作日志数据 入参:{}，出参:{}", requestDto, logPage);
        return logs;
    }

    @Override
    public Integer save(UserOpLogDTO userOpLogDto) {
        return userOpLogRepository.save(userOpLogDto);
    }

    /**
     * 设置excel下载响应头属性
     *
     * @param response
     * @param rawFileName
     * @param fileType
     * @throws UnsupportedEncodingException
     */
    private void setExcelRespProp(HttpServletResponse response, String rawFileName, String fileType) throws UnsupportedEncodingException {
        response.setContentType("application/stream");
        response.setCharacterEncoding("utf-8");
        String fileName = URLEncoder.encode(rawFileName, "UTF-8");
        response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + fileName + "." + fileType);
    }
}
