package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.user.api.vo.SellerDetailVO;

public interface PileBaseCommonService {

    Integer getSellerAssociatedPlatform(String operationId);

    SellerDetailVO getSellerDetail(String operationId);
}
