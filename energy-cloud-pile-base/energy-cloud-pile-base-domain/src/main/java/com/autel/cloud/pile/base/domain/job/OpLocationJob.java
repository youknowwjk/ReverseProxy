package com.autel.cloud.pile.base.domain.job;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.text.ParseException;
import java.util.concurrent.ExecutionException;

/**
 * @Author A22282
 * @Date 2022/4/20 11:24
 */
@Component
@Slf4j
public class OpLocationJob {

    @Autowired
    private OpLocationService opLocationService;

    /**
     * 处理场站是否到运营时间
     */
    @XxlJob("locationOpenTypeTask")
    public ReturnT<String> locationOpenTypeTask(String param) {
        XxlJobLogger.log("每日场站是否开放处理服务开始");
        Result<Integer> integerResult = opLocationService.JudgeOpenType();
        XxlJobLogger.log("更新了{}条数据",integerResult.getData());
        XxlJobLogger.log("每日场站是否开放处理服务结束");
        return ReturnT.SUCCESS;
    }


}
