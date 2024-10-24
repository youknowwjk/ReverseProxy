package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.vo.EvseForStatsVO;
import com.autel.cloud.pile.base.vo.EvseStatusStatisticVO;
import com.autel.cloud.pile.base.vo.GunStatsVO;

import java.util.List;

/**
 * @Author A22282
 * @Date 2022/4/27 16:46
 */
public interface DataStatsService {
    Result<EvseForStatsVO> evseStats(Long operatorId);

    Result<List<GunStatsVO>> gunStats(Long operatorId);

    Result<List<GunStatsVO>> getAllGunStats();

    Result<EvseForStatsVO> getAllEvseStats();

}
