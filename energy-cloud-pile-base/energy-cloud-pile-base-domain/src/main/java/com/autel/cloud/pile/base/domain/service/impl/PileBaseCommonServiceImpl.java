package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.service.PileBaseCommonService;
import com.autel.cloud.pile.user.api.enums.SellerPlatformEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class PileBaseCommonServiceImpl implements PileBaseCommonService {

    @Autowired
    private PileUserFeign pileUserFeign;

    @Override
    public Integer getSellerAssociatedPlatform(String operationId) {
        SellerDetailVO sellerDetailVO = this.getSellerDetail(operationId);
        if (sellerDetailVO != null && sellerDetailVO.getCollabStatus() != null && sellerDetailVO.getCollabStatus() == 1
                && SellerPlatformEnum.CSMS.getCode().equalsIgnoreCase(sellerDetailVO.getAppId())) {
            return sellerDetailVO.getAssociatedPlatform();
        }
        return null;
    }

    @Override
    public SellerDetailVO getSellerDetail(String operationId) {
        // 获取商户信息
        if (StrUtil.isNotBlank(operationId)) {
            Result<SellerDetailVO> sellerDetailResult = pileUserFeign.detail(Long.parseLong(operationId));
            log.info("============ get sellerDetailResult: {}", JSON.toJSONString(sellerDetailResult));
            if (sellerDetailResult != null && sellerDetailResult.getCode() == HttpStatus.SC_OK && sellerDetailResult.getData() != null) {
                SellerDetailVO sellerDetailVO = sellerDetailResult.getData();
                return sellerDetailVO;
            }
        }
        return null;
    }
}
