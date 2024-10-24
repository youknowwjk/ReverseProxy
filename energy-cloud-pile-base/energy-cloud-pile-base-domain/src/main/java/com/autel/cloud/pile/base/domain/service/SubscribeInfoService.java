package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.ordercenter.vo.ClientOrderListReqVo;
import com.autel.cloud.pile.base.domain.model.*;
import com.autel.cloud.pile.base.vo.app.PageDTO;

/**
 * 订阅信息服务
 *
 * @author A22598
 * @date 2023/06/14
 */
public interface SubscribeInfoService {

    /**
     * 订阅信息分页查询
     *
     * @param searchDTO 搜索DTO
     * @param language  语言
     * @return {@link PageDTO}<{@link SubscribeInfo}>
     */
    PageDTO<SubscribeInfo> subscribeInfoPage(SubscribeInfoPageVO searchDTO, String language);

    /**
     * 订阅订单明细
     *
     * @param orderId 订单ID
     * @param language 语言
     * @return {@link SubscribeInfoDetail}
     */
    SubscribeInfoDetail subscribedOrderDetail(Long orderId, String language);

    void orderDetail(SubscribedInvoiceDTO invoiceDTO, String language);


    /**
     * 订单License信息
     *
     * @param licenseSearchDTO
     * @return {@link LicenseInfo}
     */
    PageDTO<LicenseInfo> licenseInfoOfOrder(LicenseSearchDTO licenseSearchDTO);
}
