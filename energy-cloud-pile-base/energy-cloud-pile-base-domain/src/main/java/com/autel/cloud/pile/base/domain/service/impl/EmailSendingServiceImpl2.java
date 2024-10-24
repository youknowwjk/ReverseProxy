package com.autel.cloud.pile.base.domain.service.impl;

//import cn.hutool.core.collection.CollUtil;
//import cn.hutool.core.util.ObjectUtil;
//import com.alibaba.fastjson.JSON;
//import com.autel.cloud.base.common.MessageSourceHolder;
//import com.autel.cloud.base.data.domain.R;
//import com.autel.cloud.base.exception.MessageCodeException;
//import com.autel.cloud.base.http.pojo.Result;
//import com.autel.cloud.base.webhook.WeChatClient;
//import com.autel.cloud.base.webhook.dto.Text;
//import com.autel.cloud.base.webhook.message.TextMessage;
//import com.autel.cloud.base.webhook.message.WeChatMessage;
//import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
//import com.autel.cloud.ordercenter.vo.OrderInvoiceRespVo;
//import com.autel.cloud.pile.base.domain.model.ImminentExpireChargePointDTO;
//import com.autel.cloud.pile.base.domain.model.OrderCommodityDetail;
//import com.autel.cloud.pile.base.domain.model.SubscribeInfoDetail;
//import com.autel.cloud.pile.base.domain.model.SubscribedInvoiceDTO;
//import com.autel.cloud.pile.base.domain.properties.InvoiceProperties;
//import com.autel.cloud.pile.base.domain.service.EmailSendingService;
//import com.autel.cloud.pile.base.domain.service.OpCountryService;
//import com.autel.cloud.pile.base.domain.service.SubscribeInfoService;
//import com.autel.cloud.pile.base.domain.utils.TimeZoneUtil;
//import com.autel.cloud.pile.base.enums.SubStatus;
//import com.autel.cloud.pile.base.infrastructure.feign.CommonServiceFeign;
//import com.autel.cloud.pile.base.infrastructure.mapper.TbOrderRecordMapper;
//import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbOrderRecordEntity;
//import com.autel.cloud.pile.bill.enums.PileBillExceptionEnum;
//import com.autel.cloud.pile.bill.feign.BaseAdminFeign;
//import com.autel.cloud.pile.bill.vo.CountryVo;
//import com.autel.cloud.pile.user.api.constant.Constant;
//import com.autel.cloud.pile.user.api.dto.EmailSendDTO;
//import com.autel.cloud.pile.user.api.feign.PileUserFeign;
//import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
//import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
//import lombok.extern.slf4j.Slf4j;
//import org.apache.commons.lang3.StringUtils;
//import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
//import org.jsoup.Jsoup;
//import org.jsoup.nodes.Document;
//import org.jsoup.nodes.Element;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Qualifier;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.core.io.ClassPathResource;
//import org.springframework.http.HttpStatus;
//import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
//import org.springframework.stereotype.Service;
//import org.thymeleaf.TemplateEngine;
//import org.thymeleaf.context.Context;
//import org.thymeleaf.templatemode.TemplateMode;
//import org.thymeleaf.templateresolver.ClassLoaderTemplateResolver;
//
//import javax.annotation.Resource;
//import java.io.IOException;
//import java.io.InputStream;
//import java.math.BigDecimal;
//import java.util.List;
//import java.util.Locale;
//import java.util.Map;
//import java.util.Objects;
//import java.util.stream.Collectors;
//
//import static com.autel.cloud.pile.base.domain.constant.ReminderEmailConstant.*;
//import static com.autel.cloud.pile.base.domain.constant.SubscribeInvoiceConstant.*;
//
///**
// * 邮件发送服务实现类
// *
// * @author A22598
// * @date 2023/06/13
// */
//@Service
//@Slf4j
//public class EmailSendingServiceImpl2 implements EmailSendingService {
//
//    @Resource
//    TbOrderRecordMapper tbOrderRecordMapper;
//
//    @Resource
//    OrderCenterFeignClient orderCenterFeignClient;
//
//    @Resource
//    PileUserFeign pileUserFeign;
//
//    @Resource
//    SubscribeInfoService subscribeInfoService;
//
//    @Autowired
//    @Qualifier(value = "threadPoolTaskExecutor")
//    ThreadPoolTaskExecutor threadPoolTaskExecutor;
//
//    @Resource
//    CommonServiceFeign commonService;
//
//    @Resource
//    InvoiceProperties invoiceProperties;
//
//    @Resource
//    private WeChatClient weChatClient;
//
//    @Resource
//    private BaseAdminFeign baseAdminFeign;
//
//    @Autowired
//    private OpCountryService opCountryService;
//
//    public static final String CHARSET_NAME = "UTF-8";
//
//    public static final String SPAN_S = "<span>";
//    public static final String SOON_EXPIRE_SPAN_S = "<span style=\"\n" +
//            "                    background: rgba(255,149,0,0.1);\n" +
//            "                    color: #FF9500;\n" +
//            "                    border-radius: 12px;\n" +
//            "                    height: 18px;\n" +
//            "                    display: inline-block;\n" +
//            "                    padding: 0 8px;\n" +
//            "                    \">";
//    public static final String EXPIRED_SPAN_S = "<span style=\"\n" +
//            "                        background: rgba(0,0,0,0.1);\n" +
//            "                        color: rgba(0,0,0,0.6);\n" +
//            "                        border-radius: 12px;\n" +
//            "                        height: 18px;\n" +
//            "                        display: inline-block;\n" +
//            "                        padding: 0 8px;\n" +
//            "                        \">";
//    public static final String SPAN_E = "</span>";
//
//    public static final String TD_S = "<td>";
//    public static final String ORDER_DETAIL_TD_S = "<td style=\"border-bottom: 1px solid #e5e5e5;\">";
//    public static final String REMINDER_EMAIL_TD_S = "<td style=\"border-bottom: 1px solid #e5e5e5;\">";
//    public static final String TD_E = "</td>";
//
//    public static final String TH_E = "</th>";
//    public static final String DIV_E = "</div>";
//
//    public static final String TBODY_S = "<tbody>";
//    public static final String TBODY_E = "</tbody>";
//
//    public static final String COMMON_TR_S = "<tr><td colspan=\"3\"></td><td colspan=\"3\"><div class=\"other-fee\">";
//    public static final String TOTAL_AMOUNT_TR_S = "<tr><td colspan=\"3\"></td><td colspan=\"3\"><div class=\"other-fee total-fee\" id=\"totalAmount\">";
//    public static final String HEADER_TR_S = "<tr class=\"item-wrapper\" id=\"remainderHeader\">";
//    public static final String TR_E = "</tr>";
//
//    @Value("${webhook.wechat.key:4c3bae39-9bff-4365-8dfe-08a7feaa002d}")
//    private String webhookWechatKey;
//    private String nacosNs = System.getenv("NACOS_NS");
//
//
//    @Override
//    public Boolean sendSubscribedInvoice(String orderId, String language) {
//        final SubscribedInvoiceDTO subscribedInvoiceDTO = invoiceDownload(orderId, language);
//        log.info("SubscribedInvoiceDTO : {}", JSON.toJSONString(subscribedInvoiceDTO));
//        //查询商家的邮箱信息
//        final Result<SellerDetailVO> detail = pileUserFeign.detail(subscribedInvoiceDTO.getCustomerId());
//        if (ObjectUtil.isNull(detail) || !detail.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(detail.getData())) {
//            sendMessage("未查询到商家邮箱信息！邮件发送失败！");
//            return false;
//        }
//        final SellerDetailVO sellerDetailVO = detail.getData();
//        String html = "";
//        org.springframework.core.io.Resource resource = null;
//        // evota&autel 兼容处理
//        boolean evota = isEvotaSeller(orderId);
//        if (evota) {
//            resource = new ClassPathResource("template/subscribe_invoice_evota.html");
//        } else {
//            resource = new ClassPathResource("template/subscribe_invoice.html");
//        }
//
//        //加载模板
//        try (InputStream resourceAsStream = resource.getInputStream()) {
//            Document document = Jsoup.parse(resourceAsStream, CHARSET_NAME, "");
//            Element body = document.body();
//            StringBuilder htmlText = new StringBuilder();
//            String invoiceText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(INVOICE, "Invoice")).append("#").append(COLON).append(SPAN_E)
//                    .append(SPAN_S).append(subscribedInvoiceDTO.getInvoiceNo()).append(SPAN_E).toString();
//            htmlText = new StringBuilder();
//            String invoiceDateText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(INVOICE_DATE, "Invoice Date")).append(COLON).append(SPAN_E)
//                    .append(SPAN_S).append(subscribedInvoiceDTO.getInvoiceDate()).append(SPAN_E).toString();
//            htmlText = new StringBuilder();
//            String billToText = htmlText.append(MessageSourceHolder.getMessage(BILL_TO, "Bill To")).append(COLON).toString();
//            htmlText = new StringBuilder();
//            String customerNameText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(CUSTOMER_NAME, "Customer Name")).append(COLON).append(SPAN_E)
//                    .append(SPAN_S).append(subscribedInvoiceDTO.getCustomerName()).append(SPAN_E).toString();
//            htmlText = new StringBuilder();
//            String customerAddressText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(CUSTOMER_ADDRESS, "Customer Address")).append(COLON).append(SPAN_E)
//                    .append(SPAN_S).append(subscribedInvoiceDTO.getCustomerAddress()).append(SPAN_E).toString();
//            htmlText = new StringBuilder();
//            String customerCountryCodeText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(CUSTOMER_COUNTRY_CODE, "Country")).append(COLON).append(SPAN_E)
//                    .append(SPAN_S).append(subscribedInvoiceDTO.getCustomerCountryCode()).append(SPAN_E).toString();
//            htmlText = new StringBuilder();
//            String vatNumberText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(VAT_NUMBER, "Vat Number")).append(COLON).append(SPAN_E)
//                    .append(SPAN_S).append(subscribedInvoiceDTO.getVatNumber()).append(SPAN_E).toString();
//            htmlText = new StringBuilder();
//            String orderIdText = htmlText.append(MessageSourceHolder.getMessage(ORDER_ID, "Order Id")).append(COLON).append(subscribedInvoiceDTO.getOrderId()).toString();
//            htmlText = new StringBuilder();
//            String paymentDateText = htmlText.append(MessageSourceHolder.getMessage(PAYMENT_DATE, "Payment Date")).append(COLON).append(subscribedInvoiceDTO.getPaymentDate()).toString();
//
//            //表头
//            htmlText = new StringBuilder();
//            String commodityNameText = htmlText.append("<th class=\"p-0-16\" style=\"width: '25%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(COMMODITY_NAME, "Commodity Name")).append(SPAN_E).append(TH_E).toString();
//            htmlText = new StringBuilder();
//            String subscriptionDurationText = htmlText.append("<th class=\"p-0-16\" style=\"width: '15%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(SUBSCRIPTION_DURATION, "Subscription Duration")).append(SPAN_E).append(TH_E).toString();
//            htmlText = new StringBuilder();
//            String portTypeText = htmlText.append("<th class=\"p-0-16\" style=\"width: '15%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(PORT_TYPE, "Port Type")).append(SPAN_E).append(TH_E).toString();
//            htmlText = new StringBuilder();
//            String quantityText = htmlText.append("<th class=\"p-0-16\" style=\"width: '20%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(QUANTITY, "Quantity")).append(SPAN_E).append(TH_E).toString();
//            htmlText = new StringBuilder();
//            String unitPriceText = htmlText.append("<th class=\"p-0-16\" style=\" width: 100px \">").append(SPAN_S).append(MessageSourceHolder.getMessage(UNIT_PRICE, "Unit Price")).append(SPAN_E).append(TH_E).toString();
//            htmlText = new StringBuilder();
//            String subTotalText = htmlText.append("<th class=\"p-0-16\" id=\"subTotal\" >").append(SPAN_S).append(MessageSourceHolder.getMessage(SUB_TOTAL, "SubTotal")).append(SPAN_E).append(TH_E).toString();
//            String tableTitle = "<tr class=\"item-wrapper\" id=\"tableTitle\">" + commodityNameText + subscriptionDurationText + portTypeText + quantityText + unitPriceText + subTotalText + TR_E;
//
//            //订单明细信息
//            htmlText = new StringBuilder();
//            final SubscribeInfoDetail subscribeInfoDetail = subscribedInvoiceDTO.getSubscribeInfoDetail();
//            final List<OrderCommodityDetail> orderCommodityDetails = subscribeInfoDetail.getOrderCommodityDetails();
//            for (OrderCommodityDetail orderDetail : orderCommodityDetails) {
//                htmlText.append("<tr class=\"t-li\" id=\"orderDetail\">");
//                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getCommodityName()).append(TD_E);
//                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getSubscriptionDuration()).append(TD_E);
//                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getPortOrGunType() == null ? "--" : orderDetail.getPortOrGunType()).append(TD_E);
//                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getQuantity()).append(TD_E);
//                htmlText.append("<td style=\"border-bottom: 1px solid #e5e5e5;width: 100px\">").append(subscribeInfoDetail.getCurrencySymbol()).append(orderDetail.getUnitPrice()).append(TD_E);
//                htmlText.append(ORDER_DETAIL_TD_S).append(subscribeInfoDetail.getCurrencySymbol()).append(orderDetail.getSubTotal()).append(TD_E);
//                htmlText.append(TR_E);
//            }
//            String orderDetailText = htmlText.toString();
//            //小计
//            htmlText = new StringBuilder();
//            final String currencySymbol = subscribedInvoiceDTO.getCurrencySymbol();
//            String totalText = htmlText.append(MessageSourceHolder.getMessage(TOTAL, "total")).toString();
//            htmlText = new StringBuilder();
//            String totalValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getSubTotal()).toString();
//            //折扣
//            htmlText = new StringBuilder();
//            String discountText = htmlText.append(MessageSourceHolder.getMessage(DISCOUNT_AMOUNT, "discount")).toString();
//            htmlText = new StringBuilder();
//            String discountValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getTotalDiscount()).toString();
//            //销售税
//            String salesTaxText = null;
//            String salesTaxValueText = null;
//            if (Objects.nonNull(subscribeInfoDetail.getSalesTax())) {
//                htmlText = new StringBuilder();
//                salesTaxText = htmlText.append(MessageSourceHolder.getMessage(SALES_TAX, "Sales Tax")).toString();
//                htmlText = new StringBuilder();
//                htmlText.append(currencySymbol).append(subscribeInfoDetail.getSalesTax());
//                final List<BigDecimal> salesTaxRates = subscribedInvoiceDTO.getSalesTaxRates();
//                addTaxLabel(htmlText, salesTaxRates);
//                salesTaxValueText = htmlText.toString();
//            }
//
//            //增值税
//            String vatText = null;
//            String vatValueText = null;
//            if (Objects.nonNull(subscribeInfoDetail.getVat())) {
//                htmlText = new StringBuilder();
//                vatText = htmlText.append(MessageSourceHolder.getMessage(VAT, "Vat")).toString();
//                htmlText = new StringBuilder();
//                htmlText.append(currencySymbol).append(subscribeInfoDetail.getVat());
//                final List<BigDecimal> vatRates = subscribedInvoiceDTO.getVatRates();
//                addTaxLabel(htmlText, vatRates);
//                vatValueText = htmlText.toString();
//            }
//
//            //总税额
//            htmlText = new StringBuilder();
//            String totalTaxText = htmlText.append(MessageSourceHolder.getMessage(TOTAL_TAX, "Total Tax")).toString();
//            htmlText = new StringBuilder();
//            final String totalTaxValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getTotalTax()).toString();
//
//            //总计
//            htmlText = new StringBuilder();
//            String totalAmountText = htmlText.append(MessageSourceHolder.getMessage(TOTAL_AMOUNT, "Total Amount")).toString();
//            htmlText = new StringBuilder();
//            final String totalAmountValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getTotalAmount()).toString();
//
//            //公司名称
//            htmlText = new StringBuilder();
//            String companyNameText = htmlText.append(subscribedInvoiceDTO.getCompanyName()).toString();
//            //公司地址
//            htmlText = new StringBuilder();
//            String companyAddrText = htmlText.append(subscribedInvoiceDTO.getCompanyAddress()).toString();
//            if (evota) {
//                companyNameText = "Autel Netherlands B.V.";
//                companyAddrText = "Laan van Waalhaven 217, 2497 GL, Den Haag, Netherlands";
//            }
//
//            //将拼接好的内容放入标签
//            fillLabels(body, "invoiceNo", invoiceText);
//            fillLabels(body, "invoiceDate", invoiceDateText);
//            fillLabels(body, "billTo", billToText);
//            fillLabels(body, "customerName", customerNameText);
//            fillLabels(body, "customerAddress", customerAddressText);
//            fillLabels(body, "customerCountryCode", customerCountryCodeText);
//            fillLabels(body, "vatNumber", vatNumberText);
//            if (Objects.isNull(subscribedInvoiceDTO.getVatNumber())) {
//                //税号没有则不显示该标签
//                Objects.requireNonNull(body.getElementById("vatNumber")).remove();
//            }
//            fillLabels(body, "orderId", orderIdText);
//            fillLabels(body, "paymentDate", paymentDateText);
//
//            //清空表体
//            Objects.requireNonNull(body.getElementById("dataTable")).empty();
//            fillLabels(body, "dataTable", TBODY_S + tableTitle + orderDetailText + TBODY_E);
//
//            fillLabels(body, "total", totalText);
//            fillLabels(body, "totalValue", totalValueText);
//
//            fillLabels(body, "discount", discountText);
//            fillLabels(body, "discountValue", discountValueText);
//            if (StringUtils.isNotBlank(salesTaxText)) {
//                fillLabels(body, "salesTax", salesTaxText);
//                fillLabels(body, "salesTaxValue", salesTaxValueText);
//            } else {
//                removeLabels(body,"salesTaxLabel");
//            }
//
//            if (StringUtils.isNotBlank(vatText)) {
//                fillLabels(body, "vat", vatText);
//                fillLabels(body, "vatTaxValue", vatValueText);
//            } else {
//                removeLabels(body,"vatLabel");
//            }
//
//            fillLabels(body, "totalTax", totalTaxText);
//            fillLabels(body, "totalTaxValue", totalTaxValueText);
//
//            fillLabels(body, "totalAmount", totalAmountText);
//            fillLabels(body, "totalAmountValue", totalAmountValueText);
//
//            fillLabels(body, "companyName", companyNameText);
//            fillLabels(body, "companyAddress", companyAddrText);
//            html = document.html();
//        } catch (IOException e) {
//            log.error("IoException", e);
//            sendMessage(nacosNs + "发送发票失败，订单ID：" + orderId + e.getMessage());
//            throw new MessageCodeException(PileBillExceptionEnum.INVOICE_PDF);
//        }
//
//
//
//        // 创建模板解析器
//        ClassLoaderTemplateResolver templateResolver = new ClassLoaderTemplateResolver();
//        templateResolver.setPrefix("template/");
//        templateResolver.setSuffix(".html");
//        templateResolver.setTemplateMode(TemplateMode.HTML);
//
//
//        // 创建模板引擎
//        TemplateEngine templateEngine = new TemplateEngine();
//        templateEngine.setTemplateResolver(templateResolver);
//
//        // 创建上下文并添加需要替换的数据
//        Context context = new Context();
//        context.setVariable(AUTEL_LOGO, Boolean.TRUE.equals(evota) ? EVOTA_LOGO_VALUE : AUTEL_LOGO_VALUE);
//        context.setVariable(BILL_TO, MessageSourceHolder.getMessage(BILL_TO, "Bill To") + COLON);
//        context.setVariable(CUSTOMER_NAME, MessageSourceHolder.getMessage(CUSTOMER_NAME, "Customer Name") + COLON);
//        context.setVariable(CUSTOMER_NAME_VALUE, subscribedInvoiceDTO.getCustomerName());
//        context.setVariable(CUSTOMER_ADDRESS, MessageSourceHolder.getMessage(CUSTOMER_ADDRESS, "Customer Address") + COLON);
//        context.setVariable(CUSTOMER_ADDRESS_VALUE, subscribedInvoiceDTO.getCustomerAddress());
//        context.setVariable(CUSTOMER_COUNTRY_CODE, MessageSourceHolder.getMessage(CUSTOMER_COUNTRY_CODE, "Country") + COLON);
//        context.setVariable(CUSTOMER_COUNTRY_CODE_VALUE, subscribedInvoiceDTO.getCustomerCountryCode());
//        context.setVariable(HELLO_CUSTOMER, MessageSourceHolder.getMessage(HELLO_CUSTOMER, ""));
//        context.setVariable(SUBSCRIBE_INVOICE_CHECK, String.format(MessageSourceHolder.getMessage(SUBSCRIBE_INVOICE_CHECK, ""), subscribedInvoiceDTO.getOrderId()));
//        context.setVariable(INVOICE, MessageSourceHolder.getMessage(INVOICE, "") + COLON + POUND + subscribedInvoiceDTO.getInvoiceNo());
//        context.setVariable(INVOICE_DATE, MessageSourceHolder.getMessage(INVOICE_DATE, "") + COLON + subscribedInvoiceDTO.getInvoiceDate());
//        context.setVariable(ORDER_ID, MessageSourceHolder.getMessage(ORDER_ID, "") + COLON + subscribedInvoiceDTO.getOrderId());
//        context.setVariable(PAYMENT_DATE, MessageSourceHolder.getMessage(PAYMENT_DATE, "") + COLON + subscribedInvoiceDTO.getPaymentDate());
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//        context.setVariable("message", "Hello");
//
//
//        // 处理模板并输出结果
//        String html2 = templateEngine.process("subscribe_pay_invoice.html", context);
//
//        EmailSendDTO emailSendDto = new EmailSendDTO();
//        emailSendDto.setContent(html2);
//        emailSendDto.setEmail(sellerDetailVO.getEmail());
//        emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBJECT, "Invoice"));
//        emailSendDto.setSenderName(SENDER);
//        if (evota) {
//            emailSendDto.setSenderName(SENDER_EVOTA);
//            emailSendDto.setBrand("evota");
//            emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBJECT, "Invoice").replaceAll("Autel", "Evota"));
//        }
//        threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
//            try {
//                commonService.sendEmail(emailSendDto);
//            } catch (Exception e) {
//                log.info("邮件发送失败", e);
//            }
//        }));
//        return true;
//    }
//
//    private boolean isEvotaSeller(String orderId) {
//        try {
//            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
//            queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
//            TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
//            if (tbOrderRecordEntity != null && tbOrderRecordEntity.getTenantId() != null) {
//                // 查询商家
//                Result<SellerDetailVO> detail = pileUserFeign.detail(Long.valueOf(tbOrderRecordEntity.getTenantId()));
//                log.info("isEvotaSeller, detail: {}", JSON.toJSONString(detail));
//                return detail.getData() != null && Constant.SUBJECT_EVOTA.equalsIgnoreCase(detail.getData().getSellerSubject());
//            }
//        } catch (Exception e) {
//            log.error("isEvotaSeller method error.", e);
//        }
//        return false;
//    }
//
//    /**
//     * 添加税率标签
//     *
//     * @param htmlText HTML文本
//     * @param taxRates 税率列表
//     */
//    private void addTaxLabel(StringBuilder htmlText, List<BigDecimal> taxRates) {
//        for (int i = 0; i < taxRates.size(); i++) {
//            if (i == 0) {
//                htmlText.append("(");
//            }
//            if (i == taxRates.size() - 1) {
//                htmlText.append(taxRates.get(i).multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP)).append("%").append(")").append(SPAN_E);
//                break;
//            }
//            htmlText.append(taxRates.get(i).multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP)).append("%").append(";");
//        }
//    }
//
//    private void fillLabels(Element body, String labelId, String invoiceText) {
//        Objects.requireNonNull(body.getElementById(labelId)).html(invoiceText);
//    }
//
//    private void removeLabels(Element body , String labelId) {
//        Objects.requireNonNull(body.getElementById(labelId)).remove();
//    }
//
//
//    @Override
//    public SubscribedInvoiceDTO invoiceDownload(String orderId, String language) {
//        SubscribedInvoiceDTO invoiceDTO = new SubscribedInvoiceDTO();
//        //查询下单的时区
//        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
//        queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
//        final TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
//        String timeZone = tbOrderRecordEntity.getTimeZone();
//        if (StringUtils.isEmpty(timeZone)) {
//            timeZone = "UTC";
//        }
//        //调用订单中心
//        Result<OrderInvoiceRespVo> result;
//        try {
//            result = orderCenterFeignClient.getInvoice(orderId, language);
//            //异常返回
//            if (ObjectUtil.isNull(result) || !result.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(result.getData())) {
//                log.info("subscribeInfoPage Feign结果异常");
//                return null;
//            }
//        } catch (Exception e) {
//            log.info("subscribeInfoPage Feign调用失败！", e);
//            return null;
//        }
//        String timeZoneString = " (" + timeZone + ")";
//        final OrderInvoiceRespVo orderInvoice = result.getData();
//        log.info("subscribeInfoPage orderInvoice:{}", JSON.toJSONString(orderInvoice));
//        invoiceDTO.setInvoiceNo(orderInvoice.getInvoiceNo());
//        invoiceDTO.setInvoiceDate(TimeZoneUtil.format(orderInvoice.getInvoiceDate(), timeZone, TimeZoneUtil.DATE_PATTERN) + timeZoneString);
//        invoiceDTO.setCustomerId(orderInvoice.getCustomerId());
//        //公司信息
//        //查询商家的邮箱信息
//        //默认使用美国地址
//        invoiceDTO.setCompanyName(US_COMPANY_NAME);
//        invoiceDTO.setCompanyAddress(US_COMPANY_ADDR);
//        Result<SellerDetailVO> detail = null;
//        try {
//            detail = pileUserFeign.detail(invoiceDTO.getCustomerId());
//        } catch (Exception e) {
//            log.info("查询商家信息失败:", e);
//        }
//        String countryAbbreviation = null;
//        if (!Objects.isNull(detail) && !Objects.isNull(detail.getData())) {
//            countryAbbreviation = detail.getData().getCountryAbbreviation();
//        }
//        if (StringUtils.isNotEmpty(countryAbbreviation) && invoiceProperties.getEuInfoCountryList().contains(countryAbbreviation)) {
//            invoiceDTO.setCompanyName(EU_COMPANY_NAME);
//            invoiceDTO.setCustomerAddress(EU_COMPANY_ADDR);
//        }
//        invoiceDTO.setCustomerName(orderInvoice.getCustomerName());
//        invoiceDTO.setCustomerAddress(orderInvoice.getCustomerAddress());
//        //根据国家编码查国家
//        try{
//            R<List<CountryVo>> countryVosRes = baseAdminFeign.getCountryList(language,null);
//            final List<CountryVo> countryVos = countryVosRes.getData();
//            Map<String, CountryVo> countryVoMap = countryVos.stream().collect(Collectors.toMap(CountryVo::getAbbreviation, v -> v, (v1, v2) -> v1));
//            if (CollUtil.isNotEmpty(countryVoMap)) {
//                log.info("CountryInfo:{}",JSON.toJSONString(countryVoMap.get(countryAbbreviation)));
//                invoiceDTO.setCustomerCountryCode(countryVoMap.get(countryAbbreviation).getCountryName());
//            }
//        }catch (Exception e){
//            log.info("获取国家编码失败",e);
//            invoiceDTO.setCustomerCountryCode("--");
//        }
//
//        invoiceDTO.setVatNumber(orderInvoice.getVatNumber());
//        invoiceDTO.setOrderId(orderInvoice.getOrderId());
//        invoiceDTO.setVatRates(orderInvoice.getVatRates());
//        invoiceDTO.setSalesTaxRates(orderInvoice.getSalesTaxRates());
//        invoiceDTO.setCurrencySymbol(orderInvoice.getCurrencySymbol());
//        if (orderInvoice.getPayTime() != null) {
//            invoiceDTO.setPaymentDate(TimeZoneUtil.format(orderInvoice.getPayTime(), timeZone, TimeZoneUtil.DATE_PATTERN) + timeZoneString);
//        } else {
//            invoiceDTO.setPaymentDate("--");
//        }
//        //查询订单商品信息及费用信息
//        SubscribeInfoDetail subscribeInfoDetail = subscribeInfoService.subscribedOrderDetail(Long.valueOf(orderId), language);
//        invoiceDTO.setSubscribeInfoDetail(subscribeInfoDetail);
//
//        return invoiceDTO;
//    }
//
//    @Override
//    public Boolean expirationReminderEmail(List<ImminentExpireChargePointDTO> imminentExpireChargePointDTO, String jumpLink, String email, Locale locale) {
//        String html;
//        //加载模板
//        org.springframework.core.io.Resource resource = new ClassPathResource("template/expiration_reminder_email.html");
//        try (InputStream resourceAsStream = resource.getInputStream()) {
//            Document document = Jsoup.parse(resourceAsStream, CHARSET_NAME, "");
//            Element body = document.body();
//            //状态
//            final SubStatus subStatus = imminentExpireChargePointDTO.get(0).getSubStatus();
//            //数量
//            final int size = imminentExpireChargePointDTO.size();
//            //正文
//            String contentText;
//            String pileSN;
//            String pileName;
//            String locationName;
//            String produceName;
//            String status;
//            String remainingTime;
//            String expireTime;
//            String tableText;
//            //共同表头
//            fillLabels(body, "pileSN", MessageSourceHolder.getMessage(CHARGER_SN,null,locale));
//            fillLabels(body, "pileName", MessageSourceHolder.getMessage(CHARGER_NAME,null,locale));
//            fillLabels(body, "locationName", MessageSourceHolder.getMessage(LOCATION_NAME,null,locale));
//            fillLabels(body, "produceName",MessageSourceHolder.getMessage(PRODUCT_NAME,null,locale));
//            fillLabels(body, "status", MessageSourceHolder.getMessage(SUBSCRIPTION_STATUS,null,locale));
//            if (subStatus.equals(SubStatus.SOON_TO_EXPIRE)) {
//                fillLabels(body, "remainingTime", MessageSourceHolder.getMessage(REMAINING_TIME, null, locale));
//            } else {
//                Objects.requireNonNull(body.getElementById("remainingTh")).remove();
//            }
//            fillLabels(body, "expireTime", MessageSourceHolder.getMessage(SUBSCRIPTION_EXPIRATION_DATE,null,locale));
//            final String remainderHeader = HEADER_TR_S + Objects.requireNonNull(body.getElementById("remainderHeader")).html() + TR_E;
//            //正文
//            contentText = MessageSourceHolder.getMessage(LICENSE_EXPIRED_EMAIL_CONTENT, null,"Your subscription plan for %s chargers has expired. Thus, charging and remote start/stop cannot be conducted properly. Please renew as soon as possible to avoid any disruption to your daily operations.",locale);
//            if (subStatus.equals(SubStatus.SOON_TO_EXPIRE)) {
//                contentText = MessageSourceHolder.getMessage(LICENSE_WILL_EXPIRE_EMAIL_CONTENT, null,"Your subscription plan for %s chargers is about to expire. After the expiration, charging cannot be conducted properly. Please renew your subscription plan in time to avoid any disruption to your daily operations.",locale);
//            }
//            contentText = String.format(contentText, size);
//            fillLabels(body, "contentText", contentText);
//
//            //表格
//            StringBuilder htmlText = new StringBuilder();
//            for (ImminentExpireChargePointDTO icp:imminentExpireChargePointDTO) {
//                htmlText.append("<tr class=\"t-li\">");
//                pileSN = icp.getSn();
//                pileName = icp.getName();
//                locationName = icp.getLocationName();
//                produceName = icp.getGoodsName();
//                status = MessageSourceHolder.getMessage(SOON_TO_EXPIRE,null,"Expiring Soon",locale);
//                remainingTime = icp.getRemainDays() + MessageSourceHolder.getMessage(DAY, null,"days",locale);
//                expireTime = icp.getExpireDate();
//                htmlText.append(REMINDER_EMAIL_TD_S).append(pileSN).append(TD_E);
//                htmlText.append(REMINDER_EMAIL_TD_S).append(pileName).append(TD_E);
//                htmlText.append(REMINDER_EMAIL_TD_S).append(locationName).append(TD_E);
//                htmlText.append(REMINDER_EMAIL_TD_S).append(produceName).append(TD_E);
//                if (subStatus.equals(SubStatus.SOON_TO_EXPIRE)) {
//                    htmlText.append(REMINDER_EMAIL_TD_S).append(SOON_EXPIRE_SPAN_S).append(status).append(SPAN_E).append(TD_E);
//                    htmlText.append(REMINDER_EMAIL_TD_S).append(remainingTime).append(TD_E);
//                } else {
//                    status = MessageSourceHolder.getMessage(INVALIDITY,null,"Expired",locale);
//                    htmlText.append(REMINDER_EMAIL_TD_S).append(EXPIRED_SPAN_S).append(status).append(SPAN_E).append(TD_E);
//                }
//                htmlText.append(REMINDER_EMAIL_TD_S).append(expireTime).append(TD_E);
//                htmlText.append(TR_E);
//            }
//            //清空Table
//            body.getElementById("remainderTable").empty();
//            //头+数据
//            tableText = remainderHeader + htmlText.toString();
//            fillLabels(body, "remainderTable", tableText);
//            //前往充电云
//            body.getElementById("goForRenew").attr("href", jumpLink);
//            fillLabels(body, "goForRenew", MessageSourceHolder.getMessage(GO_FOR_RENEWAL,null,"Go to Charging Cloud and Renew",locale));
//            //发送邮件
//            html = document.html();
//            log.info("html:\n {}", html);
//            EmailSendDTO emailSendDto = new EmailSendDTO();
//            emailSendDto.setContent(html);
//            emailSendDto.setEmail(email);
//            final String subject = MessageSourceHolder.getMessage(subStatus.equals(SubStatus.SOON_TO_EXPIRE) ? LICENSE_WILL_EXPIRE_EMAIL_SUBJECT : LICENSE_EXPIRED_EMAIL_SUBJECT, null, "Expiration Reminder", locale);
//            emailSendDto.setSubject(String.format(subject, size));
//            emailSendDto.setSenderName(SENDER);
//            threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
//                try {
//                    commonService.sendEmail(emailSendDto);
//                } catch (Exception e) {
//                    log.info("邮件发送失败", e);
//                }
//            }));
//            return true;
//        } catch (IOException e) {
//            log.error("IoException", e);
//            sendMessage(nacosNs + "发送过期提醒邮件失败，邮箱：" + email + ", 商家："+ imminentExpireChargePointDTO.get(0).getMerchantId() + e.getMessage());
//            throw new MessageCodeException(PileBillExceptionEnum.INVOICE_PDF);
//        }
//    }
//
//    final Text text = new Text();
//    final WeChatMessage weChatMessage = new TextMessage(text);
//
//    private void sendMessage(String format) {
//        log.info(format);
//        text.setContent(format);
//        weChatClient.sendMessage(weChatMessage, webhookWechatKey);
//    }
//}
