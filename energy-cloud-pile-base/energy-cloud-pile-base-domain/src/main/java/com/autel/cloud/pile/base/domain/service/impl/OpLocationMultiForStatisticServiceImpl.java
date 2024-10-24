package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.bill.feign.PileBillStationInterfaceFeign;
import com.autel.cloud.pile.bill.vo.MonthReportVO;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

@Service
@Slf4j
@Data
public class OpLocationMultiForStatisticServiceImpl implements Callable<Map<Long, MonthReportVO>> {

    private List<Long> stationIds;

    @Autowired
    private PileBillStationInterfaceFeign pileBillStationInterfaceFeign;

    private Map<Long, MonthReportVO> getLocationListStatistic(List<Long> stationIds) {
        Map<Long, MonthReportVO> statisticsMap = new HashMap<>();
        try {
            Result<List<MonthReportVO>> statisticsResult = pileBillStationInterfaceFeign.monthReport(stationIds);
            log.info("远程调用站点统计当月收入和新增用户数：{}", statisticsResult);
            if (statisticsResult.getCode() == HttpStatus.OK.value()) {
                List<MonthReportVO> statisticsList = statisticsResult.getData();
                log.info("站点统计当月收入和新增用户数： {}   {}", JSON.toJSONString(stationIds), JSON.toJSONString(statisticsList));
                if (CollectionUtils.isNotEmpty(statisticsList)) {
                    for (MonthReportVO monthReportVO : statisticsList) {
                        statisticsMap.put(monthReportVO.getLocationId(), monthReportVO);
                    }
                }
            }
        } catch (Exception ignore) {
        }
        return statisticsMap;
    }

    @Override
    public Map<Long, MonthReportVO> call() {
        return getLocationListStatistic(stationIds);
    }
}
