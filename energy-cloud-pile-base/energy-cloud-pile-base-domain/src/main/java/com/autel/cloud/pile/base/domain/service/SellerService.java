package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.SellerInfoDTO;

import java.util.List;

/**
 * Author:   A19011
 * Description: SellerService
 * Date:     2022/5/12 10:39
 *
 * @Version 0.0.1-SNAPSHOT
 */
public interface SellerService {
    /**
     * 按桩查询商家ID
     * @param evseSn
     * @return
     */
    String getSellerInfoBySn(String evseSn);

    /**
     * 按桩查询商家ID
     * @param evseSn
     * @return
     */
    String getSellerIdInfoBySn(String evseSn);

    /**
     * 通过设备sn.查询sellId。
     * @param pileSn
     * @return
     */
    String getSellerIdByPileSn(String pileSn);

    /**
     * 商户id 查询设备sn.
     * @param sellId
     * @return
     */
    List<String> getPileSnBySellId(String sellId);

    SellerInfoDTO getSellerInfoByPileSn(String pileSn);
}
