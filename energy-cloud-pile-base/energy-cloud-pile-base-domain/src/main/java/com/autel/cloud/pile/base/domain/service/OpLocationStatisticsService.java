package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseStatisticsDTO;
import com.autel.cloud.pile.base.dto.OpLocationOperationInfoDTO;

import java.util.List;
import java.util.Map;

/**
 * @ClassName OpLocationStatisticsService
 * @Author A22121
 * @Description
 * @Date 2022/4/18 20:48
 * @Version 0.0.1-SNAPSHOT
 */
public interface OpLocationStatisticsService {
    /**
     * 查询充电设备-统计
     *
     * @param opLocationEvseDTO
     * @return
     */
    Result<OpLocationEvseStatisticsDTO> getEvse(OpLocationEvseDTO opLocationEvseDTO);

    /**
     * 查询场站运营情况
     * @param locationId
     * @return
     */
    Result<OpLocationOperationInfoDTO> locationOperationInfo(Long locationId);

    /**
     * 查询场站运营情况折线图
     * @param locationId
     * @return
     */
    Result<Map<String, List<String>>> locationOperationInfoList(Long locationId);
}
