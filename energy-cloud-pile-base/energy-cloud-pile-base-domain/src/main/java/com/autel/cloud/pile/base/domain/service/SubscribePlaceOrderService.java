package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.ordercenter.vo.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbHonourAgreementEntity;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/12 14:27
 */
public interface SubscribePlaceOrderService {

    Result<ClientOrderCreateRespVo> placeOrder(@RequestBody OrderCreateReqVo orderCreateReqVo);

    Result<List<ClientCommodityRespVo>> queryGoodsList(ClientCommodityReqVo commodityReqVo);

    Result<String> queryPayResult(String orderId);

    Result<ClientCommodityDetailRespVo> queryGoodsDetail(ClientCommodityDetailReqVo clientCommodityDetailReqVo);

    Result<Boolean> orderTransactionCallback(CallBackEventNoticeReqVo callBackEventNoticeReqVo);

    void getAndBindLicence(TbHonourAgreementEntity tbHonourAgreementEntity);

    Result<ClientOrderCreateRespVo> placeOrderSAP(String autelSign, String redirectKey, OrderCreateSapReqVo orderCreateSapReqVo);

    Result<Boolean> orderCancelSAP(String autelSign, String redirectKey, OrderCancelReqVo orderCancelReqVo);

    Result<String> queryPlaceOrderUrlForSap(String countryAbbreviation, String urlType);

    Result<ClientOrderCreateRespVo> rdfExtract(RfdLicenceExtractVo rfdLicenseExtract);
}
