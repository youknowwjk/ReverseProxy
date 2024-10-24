package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.bill.dto.PileTypeVO;
import com.autel.cloud.pile.bill.feign.DeviceClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Author:   A19011
 * Description: CommonUtilService
 * Date:     2024/1/19 18:55
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Slf4j
@Component
public class CommonUtilService {
    @Autowired
    private DeviceClient deviceClient;

    public String getConnectorDisplayName(String sn, String gunNo){
        List<String> pileSnList = new ArrayList<>();
        pileSnList.add(sn);
        try{
            List<PileTypeVO> pileTypeList = deviceClient.getProductTypeByPileSnList(pileSnList).getData();
            if(!CollectionUtils.isEmpty(pileTypeList)){
                PileTypeVO pileTypeVO = pileTypeList.get(0);
                if(pileTypeVO.getPileProductType() != null && pileTypeVO.getPileProductType().equals(2)){
                    return CommonUtil.getConnectorDisplayName(gunNo);
                }
            }
        }catch (Exception e){
            log.error("getConnectorDisplayName 异常", e);
        }
        return CommonUtil.getGunNoLetter(Integer.parseInt(gunNo));
    }
}
