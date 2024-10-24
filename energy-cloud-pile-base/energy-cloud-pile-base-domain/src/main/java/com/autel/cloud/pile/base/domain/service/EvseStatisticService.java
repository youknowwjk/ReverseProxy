package com.autel.cloud.pile.base.domain.service;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.EvseStatisticDto;
import com.autel.cloud.pile.base.dto.statistics.GunStatusQueryDTO;
import com.autel.cloud.pile.base.dto.statistics.GunStatusStatisDTO;
import com.autel.cloud.pile.base.vo.EvsePowerStatisticVO;
import com.autel.cloud.pile.base.vo.EvseRealtimeDataVO;
import com.autel.cloud.pile.base.vo.EvseStatusStatisticVO;

import java.util.List;

;

public interface EvseStatisticService {
    Result<EvseStatusStatisticVO> statisticEvseStatus(EvseStatisticDto evseStatisticDto);

    Result<List<EvseRealtimeDataVO>> getEvseRealtimeData(EvseStatisticDto evseStatisticDto);

    Result<EvseRealtimeDataVO> getEvseRealtimeDataDetail(Long locationUid, Long evseUid);

    Result<List<EvsePowerStatisticVO>> getStatisticPower(Long locationUid, Long evseUid);

    GunStatusStatisDTO getGunStatusStatis(GunStatusQueryDTO gunStatusQueryDTO);
}
