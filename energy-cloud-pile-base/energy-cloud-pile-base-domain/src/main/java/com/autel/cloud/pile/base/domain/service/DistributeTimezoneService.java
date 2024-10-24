package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;

public interface DistributeTimezoneService {
    /**
     * 公共桩时区下发
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> distributePublicPileTimezone(String pileSn);

    /**
     * 私桩桩时区下发
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> distributeHomePileTimezone(String pileSn, Long userId);
}
