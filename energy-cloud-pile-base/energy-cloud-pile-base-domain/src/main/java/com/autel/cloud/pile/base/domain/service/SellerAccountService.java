package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.pay.*;

/**
 * @author A21309
 * @date 2022-05-24 10:35
 */
public interface SellerAccountService {
    Result<SellerAccountResp> queryStripeAccount(SellerAccountReq sellerAccountReq);

    Result<BindStripeResp> bindStripeAccount(SellerAccountReq sellerAccountReq);

    Result<Boolean> bindCallBack(SellerAccountReq sellerAccountReq);

    Result<UnBindStripeResp> unBindStripeAccount(SellerAccountReq sellerAccountReq);

    Result<BindStripeResp> updateStripeAccount(SellerAccountReq sellerAccountReq);

    /**
     * @param sellerId 商家Id
     * @return 商家的Stripe账户是否可以进行收费的结果
     * @function 查询商家的Stripe账户是否可以进行收费
     */
    Boolean isSellerStripeAccountCharged(Long sellerId);

    /**
     *
     * @param id 计费规则模型的主键Id
     * @param sellerId 商家Id
     * @return 商家是否可以绑定某个充电桩的计费规则模型的标志
     * @function 商家是否可以绑定某个充电桩的计费规则模型
     */
    Boolean isSellerBindChargingPile(Long id, Long sellerId);

    Result<Boolean> bindWXAccount(SellerWXAccountReq sellerWXAccountReq);

    Result<String> queryBindWXAccount(Long sellerId);

    Result<String> queryCurrentEnv();

    Result<WxPayDeleteReceiverResponse> unBindWXAccount(UnBindWxAccountReq unBindWxAccountReq);

    Result<WxAccountInfoResp> queryBindWXAccountV2(Long sellerId);

    Result<Integer> queryPayType();
}
