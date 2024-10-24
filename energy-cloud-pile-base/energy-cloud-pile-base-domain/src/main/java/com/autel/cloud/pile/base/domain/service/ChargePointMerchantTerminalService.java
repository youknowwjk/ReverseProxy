package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantTerminalEntity;
import com.autel.cloud.pile.base.vo.chargepoint.TerminalBriefInfoVO;

import java.util.List;

public interface ChargePointMerchantTerminalService {

    List<ChargePointMerchantTerminalEntity> getTerminalList(List<String> terminalSnList, Long merchantId);

    ChargePointMerchantTerminalEntity queryBySN(String sn, Long sellerId);

    List<TerminalBriefInfoVO> getTerminalBriefInfoVOList(String hostSn, Long merchantId);

    List<ChargePointMerchantTerminalEntity> getTerminalEntityList(String hostSn, Long merchantId);

    boolean batchAdd(List<ChargePointMerchantTerminalEntity> terminalEntityList);

    boolean updateTerminal(ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity);

    boolean batchDelete(String hostSn, Long merchantId);

    boolean deleteByCondition(List<String> terminalSnList, Long merchantId);
}
