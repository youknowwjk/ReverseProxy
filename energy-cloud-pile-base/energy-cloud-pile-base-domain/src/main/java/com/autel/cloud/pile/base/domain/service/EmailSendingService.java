package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.domain.model.ImminentExpireChargePointDTO;
import com.autel.cloud.pile.base.domain.model.SubscribedInvoiceDTO;

import java.util.List;
import java.util.Locale;

/**
 * 电子邮件发送服务
 *
 * @author A22598
 * @date 2023/06/13
 */
public interface EmailSendingService {

    /**
     * 发送已订阅发票
     *
     * @return {@link Boolean}
     */
    Boolean sendSubscribedInvoice(String orderId, String language);

    /**
     * @param orderId 订单id
     * @param language 语言
     * @return {@link SubscribedInvoiceDTO}
     */
    SubscribedInvoiceDTO invoiceDownload(String orderId, String language);

    /**
     * 过期提醒电子邮件
     *
     * @param imminentExpireChargePointDTO 即将到期dto
     * @param email                        邮箱
     * @param jumpLink                     前往充电云的跳转链接
     * @param language                     语言
     * @return {@link Boolean}
     */
    Boolean expirationReminderEmail(List<ImminentExpireChargePointDTO> imminentExpireChargePointDTO, String jumpLink, String email, Locale locale);

    /**
     * 商品到货通知
     *
     * @param orderId  orderId
     * @param language language
     * @return java.lang.Boolean
     * @author X22024
     * @since 2024/3/1 14:35
     **/
    Boolean sendSubscriptionProductArrival(String orderId, String language, String tenantId);

    /**
     * 商品退货通知
     *
     * @param orderId  orderId
     * @param returnId returnId
     * @param language language
     * @return java.lang.Boolean
     * @author X22024
     * @since 2024/3/1 19:39
     **/
    Boolean sendSubscriptionProductReturn(String orderId, Long returnId, String language, String tenantId);
}
