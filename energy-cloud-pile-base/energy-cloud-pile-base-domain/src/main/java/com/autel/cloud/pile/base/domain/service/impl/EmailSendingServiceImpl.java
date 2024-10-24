package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.data.domain.R;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.webhook.WeChatClient;
import com.autel.cloud.base.webhook.dto.Text;
import com.autel.cloud.base.webhook.message.TextMessage;
import com.autel.cloud.base.webhook.message.WeChatMessage;
import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
import com.autel.cloud.ordercenter.vo.OrderInvoiceRespVo;
import com.autel.cloud.ordercenter.vo.OrderReturnDetailRespVo;
import com.autel.cloud.pile.base.domain.model.ImminentExpireChargePointDTO;
import com.autel.cloud.pile.base.domain.model.OrderCommodityDetail;
import com.autel.cloud.pile.base.domain.model.SubscribeInfoDetail;
import com.autel.cloud.pile.base.domain.model.SubscribedInvoiceDTO;
import com.autel.cloud.pile.base.domain.properties.InvoiceProperties;
import com.autel.cloud.pile.base.domain.service.EmailSendingService;
import com.autel.cloud.pile.base.domain.service.OpCountryService;
import com.autel.cloud.pile.base.domain.service.SubscribeInfoService;
import com.autel.cloud.pile.base.domain.utils.TimeZoneUtil;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.SubStatus;
import com.autel.cloud.pile.base.enums.license.LicenceReturnStatusEnum;
import com.autel.cloud.pile.base.infrastructure.feign.CommonServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLicenseReturnRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbOrderRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLicenseReturnRecordEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbOrderRecordEntity;
import com.autel.cloud.pile.bill.enums.PileBillExceptionEnum;
import com.autel.cloud.pile.bill.feign.BaseAdminFeign;
import com.autel.cloud.pile.bill.vo.CountryVo;
import com.autel.cloud.pile.user.api.constant.Constant;
import com.autel.cloud.pile.user.api.dto.EmailSendDTO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpStatus;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.Context;
import org.thymeleaf.templatemode.TemplateMode;
import org.thymeleaf.templateresolver.ClassLoaderTemplateResolver;

import javax.annotation.Resource;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.domain.constant.ReminderEmailConstant.*;
import static com.autel.cloud.pile.base.domain.constant.SubscribeInvoiceConstant.*;

/**
 * 邮件发送服务实现类
 *
 * @author A22598
 * @date 2023/06/13
 */
@Service
@Slf4j
public class EmailSendingServiceImpl implements EmailSendingService {

    @Resource
    TbOrderRecordMapper tbOrderRecordMapper;

    @Resource
    OrderCenterFeignClient orderCenterFeignClient;

    @Resource
    PileUserFeign pileUserFeign;

    @Resource
    SubscribeInfoService subscribeInfoService;

    @Resource
    private TbLicenseReturnRecordMapper tbLicenseReturnRecordMapper;

    @Autowired
    @Qualifier(value = "threadPoolTaskExecutor")
    ThreadPoolTaskExecutor threadPoolTaskExecutor;

    @Resource
    CommonServiceFeign commonService;

    @Resource
    InvoiceProperties invoiceProperties;

    @Resource
    private WeChatClient weChatClient;

    @Resource
    private BaseAdminFeign baseAdminFeign;

    @Autowired
    private OpCountryService opCountryService;

    public static final String CHARSET_NAME = "UTF-8";
    private static final String DEFAULT_TIMEZONE = "UTC";
    private static final String DATE_FORMAT_PATTERN = TimeZoneUtil.DATE_PATTERN;
    private static final String DEFAULT_PAYMENT_DATE = "--";

    public static final String SPAN_S = "<span>";
    public static final String SOON_EXPIRE_SPAN_S = "<span style=\"\n" +
            "                    background: rgba(255,149,0,0.1);\n" +
            "                    color: #FF9500;\n" +
            "                    border-radius: 12px;\n" +
            "                    height: 18px;\n" +
            "                    display: inline-block;\n" +
            "                    padding: 0 8px;\n" +
            "                    \">";
    public static final String EXPIRED_SPAN_S = "<span style=\"\n" +
            "                        background: rgba(0,0,0,0.1);\n" +
            "                        color: rgba(0,0,0,0.6);\n" +
            "                        border-radius: 12px;\n" +
            "                        height: 18px;\n" +
            "                        display: inline-block;\n" +
            "                        padding: 0 8px;\n" +
            "                        \">";
    public static final String SPAN_E = "</span>";

    public static final String TD_S = "<td>";
    public static final String ORDER_DETAIL_TD_S = "<td style=\"border-bottom: 1px solid #e5e5e5;\">";
    public static final String REMINDER_EMAIL_TD_S = "<td style=\"border-bottom: 1px solid #e5e5e5;\">";
    public static final String TD_E = "</td>";

    public static final String TH_E = "</th>";
    public static final String DIV_E = "</div>";

    public static final String TBODY_S = "<tbody>";
    public static final String TBODY_E = "</tbody>";

    public static final String COMMON_TR_S = "<tr><td colspan=\"3\"></td><td colspan=\"3\"><div class=\"other-fee\">";
    public static final String TOTAL_AMOUNT_TR_S = "<tr><td colspan=\"3\"></td><td colspan=\"3\"><div class=\"other-fee total-fee\" id=\"totalAmount\">";
    public static final String HEADER_TR_S = "<tr class=\"item-wrapper\" id=\"remainderHeader\">";
    public static final String TR_E = "</tr>";

    @Value("${webhook.wechat.key:4c3bae39-9bff-4365-8dfe-08a7feaa002d}")
    private String webhookWechatKey;
    private String nacosNs = System.getenv("NACOS_NS");


    @Override
    public Boolean sendSubscribedInvoice(String orderId, String language) {
        final SubscribedInvoiceDTO subscribedInvoiceDTO = invoiceDownload(orderId, language);
        log.info("SubscribedInvoiceDTO : {}", JSON.toJSONString(subscribedInvoiceDTO));
        //查询商家的邮箱信息
        final Result<SellerDetailVO> detail = pileUserFeign.detail(subscribedInvoiceDTO.getCustomerId());
        if (ObjectUtil.isNull(detail) || !detail.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(detail.getData())) {
            sendMessage("未查询到商家邮箱信息！邮件发送失败！");
            return false;
        }
        final SellerDetailVO sellerDetailVO = detail.getData();
        final SubscribeInfoDetail subscribeInfoDetail = subscribedInvoiceDTO.getSubscribeInfoDetail();
        final List<OrderCommodityDetail> orderCommodityDetails = subscribeInfoDetail.getOrderCommodityDetails();
        // evota&autel 兼容处理
        boolean evota = isEvotaSeller(orderId);

        // 创建模板解析器
        ClassLoaderTemplateResolver templateResolver = new ClassLoaderTemplateResolver();
        templateResolver.setPrefix("template/");
        templateResolver.setSuffix(".html");
        templateResolver.setTemplateMode(TemplateMode.HTML);

        // 创建模板引擎
        TemplateEngine templateEngine = new TemplateEngine();
        templateEngine.setTemplateResolver(templateResolver);

        // 创建上下文并添加需要替换的数据
        Context context = new Context();
        context.setVariable(AUTEL_LOGO, Boolean.TRUE.equals(evota) ? EVOTA_LOGO_VALUE : AUTEL_LOGO_VALUE);
        context.setVariable(BILL_TO, MessageSourceHolder.getMessage(BILL_TO, "Bill To") + COLON);
        context.setVariable(CUSTOMER_NAME, MessageSourceHolder.getMessage(CUSTOMER_NAME, "Customer Name") + COLON);
        context.setVariable(CUSTOMER_NAME_VALUE, subscribedInvoiceDTO.getCustomerName());
        context.setVariable(CUSTOMER_ADDRESS, MessageSourceHolder.getMessage(CUSTOMER_ADDRESS, "Customer Address") + COLON);
        context.setVariable(CUSTOMER_ADDRESS_VALUE, subscribedInvoiceDTO.getCustomerAddress());
        context.setVariable(CUSTOMER_COUNTRY_CODE, MessageSourceHolder.getMessage(CUSTOMER_COUNTRY_CODE, "Country") + COLON);
        context.setVariable(CUSTOMER_COUNTRY_CODE_VALUE, subscribedInvoiceDTO.getCustomerCountryCode());
        context.setVariable(HELLO_CUSTOMER, MessageSourceHolder.getMessage(HELLO_CUSTOMER, "尊敬的客户,您好!"));
        String subscribeInvoiceCheck = String.format(MessageSourceHolder.getMessage(SUBSCRIBE_INVOICE_CHECK, "道通云订阅服务订单%s发票,请查收!"), subscribedInvoiceDTO.getOrderId());
        if (evota) {
            subscribeInvoiceCheck = subscribeInvoiceCheck.replace("Autel", "").replace("道通", "");
        }
        context.setVariable(SUBSCRIBE_INVOICE_CHECK, subscribeInvoiceCheck);
        context.setVariable(INVOICE, MessageSourceHolder.getMessage(INVOICE, "Invoice") + COLON + POUND + subscribedInvoiceDTO.getInvoiceNo());
        context.setVariable(INVOICE_DATE, MessageSourceHolder.getMessage(INVOICE_DATE, "Invoice Date") + COLON + subscribedInvoiceDTO.getInvoiceDate());
        context.setVariable(ORDER_ID, MessageSourceHolder.getMessage(ORDER_ID, "Order Id") + COLON + subscribedInvoiceDTO.getOrderId());
        context.setVariable(PAYMENT_DATE, MessageSourceHolder.getMessage(PAYMENT_DATE, "Payment Date") + COLON + subscribedInvoiceDTO.getPaymentDate());
        context.setVariable(COMMODITY_NAME, MessageSourceHolder.getMessage(COMMODITY_NAME, "Commodity Name"));
        context.setVariable(SUBSCRIPTION_DURATION, MessageSourceHolder.getMessage(SUBSCRIPTION_DURATION, "Subscription Duration"));
        context.setVariable(PORT_TYPE, MessageSourceHolder.getMessage(PORT_TYPE, "Port Type"));
        context.setVariable(QUANTITY, MessageSourceHolder.getMessage(QUANTITY, "Quantity"));
        context.setVariable(UNIT_PRICE, MessageSourceHolder.getMessage(UNIT_PRICE, "Unit Price"));
        context.setVariable(SUB_TOTAL, MessageSourceHolder.getMessage(SUB_TOTAL, "SubTotal"));
        context.setVariable(ORDER_COMMODITY_DETAILS, orderCommodityDetails);
        context.setVariable(CURRENCY, subscribeInfoDetail.getCurrencySymbol());
        context.setVariable(TOTAL, MessageSourceHolder.getMessage(TOTAL, "total"));
        context.setVariable(TOTAL_VALUE, subscribeInfoDetail.getSubTotal());
        context.setVariable(DISCOUNT_AMOUNT, MessageSourceHolder.getMessage(DISCOUNT_AMOUNT, "discount"));
        context.setVariable(DISCOUNT_AMOUNT_VALUE, subscribeInfoDetail.getTotalDiscount());
        context.setVariable(SALE_TAX_CONTAIN, Objects.nonNull(subscribeInfoDetail.getSalesTax()));
        context.setVariable(VAT_TAX_CONTAIN, Objects.nonNull(subscribeInfoDetail.getVat()));
        if (Objects.nonNull(subscribeInfoDetail.getSalesTax())) {
            final List<BigDecimal> salesTaxRates = subscribedInvoiceDTO.getSalesTaxRates();
            String rate = ratesHandle(salesTaxRates);
            context.setVariable(SALES_TAX, MessageSourceHolder.getMessage(SALES_TAX, "Sales Tax"));
            context.setVariable(SALES_TAX_VALUE, subscribeInfoDetail.getSalesTax() + rate);

        }
        if (Objects.nonNull(subscribeInfoDetail.getVat())) {
            final List<BigDecimal> vatRates = subscribedInvoiceDTO.getVatRates();
            String rate = ratesHandle(vatRates);
            context.setVariable(VAT, MessageSourceHolder.getMessage(VAT, "Vat"));
            context.setVariable(VAT_VALUE, subscribeInfoDetail.getVat() + rate);
        }
        context.setVariable(TOTAL_TAX, MessageSourceHolder.getMessage(TOTAL_TAX, "Total Tax"));
        context.setVariable(TOTAL_TAX_VALUE, subscribeInfoDetail.getTotalTax());
        context.setVariable(TOTAL_AMOUNT, MessageSourceHolder.getMessage(TOTAL_AMOUNT, "Total Amount"));
        context.setVariable(TOTAL_AMOUNT_VALUE, subscribeInfoDetail.getTotalAmount());
        context.setVariable(COMPANY_NAME, Boolean.TRUE.equals(evota) ? "Autel Netherlands B.V." : subscribedInvoiceDTO.getCompanyName());
        context.setVariable(COMPANY_ADDR, Boolean.TRUE.equals(evota) ? "Laan van Waalhaven 217, 2497 GL, Den Haag, Netherlands" : subscribedInvoiceDTO.getCompanyAddress());


        // 处理模板并输出结果
        String html = templateEngine.process("subscribe_pay_invoice.html", context);

        EmailSendDTO emailSendDto = new EmailSendDTO();
        emailSendDto.setContent(html);
        emailSendDto.setEmail(sellerDetailVO.getEmail());
        emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBJECT_V2, "Invoice"));
        emailSendDto.setSenderName(SENDER);
        if (evota) {
            emailSendDto.setSenderName(SENDER_EVOTA);
            emailSendDto.setBrand("evota");
            emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBJECT_V2, "Invoice").replaceAll("Autel", "Evota"));
        }
        threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
            try {
                commonService.sendEmail(emailSendDto);
            } catch (Exception e) {
                log.info("邮件发送失败", e);
            }
        }));
        return true;
    }

    private boolean isEvotaSeller(String orderId) {
        try {
            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
            TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
            if (tbOrderRecordEntity != null && tbOrderRecordEntity.getTenantId() != null) {
                // 查询商家
                Result<SellerDetailVO> detail = pileUserFeign.detail(Long.valueOf(tbOrderRecordEntity.getTenantId()));
                log.info("isEvotaSeller, detail: {}", JSON.toJSONString(detail));
                return detail.getData() != null && Constant.SUBJECT_EVOTA.equalsIgnoreCase(detail.getData().getSellerSubject());
            }
        } catch (Exception e) {
            log.error("isEvotaSeller method error.", e);
        }
        return false;
    }

    /**
     * 添加税率标签
     *
     * @param htmlText HTML文本
     * @param taxRates 税率列表
     */
    private void addTaxLabel(StringBuilder htmlText, List<BigDecimal> taxRates) {
        for (int i = 0; i < taxRates.size(); i++) {
            if (i == 0) {
                htmlText.append("(");
            }
            if (i == taxRates.size() - 1) {
                htmlText.append(taxRates.get(i).multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP)).append("%").append(")").append(SPAN_E);
                break;
            }
            htmlText.append(taxRates.get(i).multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP)).append("%").append(";");
        }
    }
    private String ratesHandle(List<BigDecimal> rates) {
        StringBuilder htmlText = new StringBuilder();
        for (int i = 0; i < rates.size(); i++) {
            if (i == 0) {
                htmlText.append("(");
            }
            if (i == rates.size() - 1) {
                htmlText.append(rates.get(i).multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP)).append("%").append(")");
                break;
            }
            htmlText.append(rates.get(i).multiply(new BigDecimal(100)).setScale(2,BigDecimal.ROUND_HALF_UP)).append("%").append(";");
        }
        return htmlText.toString();
    }

    private void fillLabels(Element body, String labelId, String invoiceText) {
        Objects.requireNonNull(body.getElementById(labelId)).html(invoiceText);
    }

    private void removeLabels(Element body , String labelId) {
        Objects.requireNonNull(body.getElementById(labelId)).remove();
    }


    @Override
    public SubscribedInvoiceDTO invoiceDownload(String orderId, String language) {
        SubscribedInvoiceDTO invoiceDTO = new SubscribedInvoiceDTO();
        //查询下单的时区
        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
        final TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
        String timeZone = tbOrderRecordEntity.getTimeZone();
        if (StringUtils.isEmpty(timeZone)) {
            timeZone = "UTC";
        }
        //调用订单中心
        Result<OrderInvoiceRespVo> result;
        try {
            result = orderCenterFeignClient.getInvoice(orderId, language);
            //异常返回
            if (ObjectUtil.isNull(result) || !result.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(result.getData())) {
                log.info("subscribeInfoPage Feign结果异常");
                return null;
            }
        } catch (Exception e) {
            log.info("subscribeInfoPage Feign调用失败！", e);
            return null;
        }
        String timeZoneString = " (" + timeZone + ")";
        final OrderInvoiceRespVo orderInvoice = result.getData();
        log.info("subscribeInfoPage orderInvoice:{}", JSON.toJSONString(orderInvoice));
        invoiceDTO.setInvoiceNo(orderInvoice.getInvoiceNo());
        invoiceDTO.setInvoiceDate(TimeZoneUtil.format(orderInvoice.getInvoiceDate(), timeZone, TimeZoneUtil.DATE_PATTERN) + timeZoneString);
        invoiceDTO.setCustomerId(orderInvoice.getCustomerId());
        //公司信息
        //查询商家的邮箱信息
        //默认使用美国地址
        invoiceDTO.setCompanyName(US_COMPANY_NAME);
        invoiceDTO.setCompanyAddress(US_COMPANY_ADDR);
        Result<SellerDetailVO> detail = null;
        try {
            detail = pileUserFeign.detail(invoiceDTO.getCustomerId());
        } catch (Exception e) {
            log.info("查询商家信息失败:", e);
        }
        String countryAbbreviation = null;
        if (!Objects.isNull(detail) && !Objects.isNull(detail.getData())) {
            countryAbbreviation = detail.getData().getCountryAbbreviation();
        }
        if (StringUtils.isNotEmpty(countryAbbreviation) && invoiceProperties.getEuInfoCountryList().contains(countryAbbreviation)) {
            invoiceDTO.setCompanyName(EU_COMPANY_NAME);
            invoiceDTO.setCustomerAddress(EU_COMPANY_ADDR);
        }
        invoiceDTO.setCustomerName(orderInvoice.getCustomerName());
        invoiceDTO.setCustomerAddress(orderInvoice.getCustomerAddress());
        //根据国家编码查国家
        try{
            R<List<CountryVo>> countryVosRes = baseAdminFeign.getCountryList(language,null);
            final List<CountryVo> countryVos = countryVosRes.getData();
            Map<String, CountryVo> countryVoMap = countryVos.stream().collect(Collectors.toMap(CountryVo::getAbbreviation, v -> v, (v1, v2) -> v1));
            if (CollUtil.isNotEmpty(countryVoMap)) {
                log.info("CountryInfo:{}",JSON.toJSONString(countryVoMap.get(countryAbbreviation)));
                invoiceDTO.setCustomerCountryCode(countryVoMap.get(countryAbbreviation).getCountryName());
            }
        }catch (Exception e){
            log.info("获取国家编码失败",e);
            invoiceDTO.setCustomerCountryCode("--");
        }

        invoiceDTO.setVatNumber(orderInvoice.getVatNumber());
        invoiceDTO.setOrderId(orderInvoice.getOrderId());
        invoiceDTO.setVatRates(orderInvoice.getVatRates());
        invoiceDTO.setSalesTaxRates(orderInvoice.getSalesTaxRates());
        invoiceDTO.setCurrencySymbol(orderInvoice.getCurrencySymbol());
        if (orderInvoice.getPayTime() != null) {
            invoiceDTO.setPaymentDate(TimeZoneUtil.format(orderInvoice.getPayTime(), timeZone, TimeZoneUtil.DATE_PATTERN) + timeZoneString);
        } else {
            invoiceDTO.setPaymentDate("--");
        }
        //查询订单商品信息及费用信息
        SubscribeInfoDetail subscribeInfoDetail = subscribeInfoService.subscribedOrderDetail(Long.valueOf(orderId), language);
        invoiceDTO.setSubscribeInfoDetail(subscribeInfoDetail);
        invoiceDTO.setCreateOrderTime(TimeZoneUtil.format(subscribeInfoDetail.getCreateOrderTime(), timeZone, TimeZoneUtil.DATE_PATTERN) + timeZoneString);

        return invoiceDTO;
    }

    public SubscribedInvoiceDTO invoiceV2Download(String orderId, String language, String tenantId) {
        SubscribedInvoiceDTO invoiceDTO = new SubscribedInvoiceDTO();
        //公司信息
        //查询商家的邮箱信息
        //默认使用美国地址
        invoiceDTO.setCompanyName(US_COMPANY_NAME);
        invoiceDTO.setCompanyAddress(US_COMPANY_ADDR);
        invoiceDTO.setOrderId(orderId);
        Result<SellerDetailVO> detail = null;
        try {
            detail = pileUserFeign.detail(Long.valueOf(tenantId));
        } catch (Exception e) {
            log.info("查询商家信息失败:", e);
        }
        String countryAbbreviation = null;
        if (!Objects.isNull(detail) && !Objects.isNull(detail.getData())) {
            countryAbbreviation = detail.getData().getCountryAbbreviation();
        }
        if (StringUtils.isNotEmpty(countryAbbreviation) && invoiceProperties.getEuInfoCountryList().contains(countryAbbreviation)) {
            invoiceDTO.setCompanyName(EU_COMPANY_NAME);
            invoiceDTO.setCustomerAddress(EU_COMPANY_ADDR);
        }
        //根据国家编码查国家
        try{
            R<List<CountryVo>> countryVosRes = baseAdminFeign.getCountryList(language,null);
            final List<CountryVo> countryVos = countryVosRes.getData();
            Map<String, CountryVo> countryVoMap = countryVos.stream().collect(Collectors.toMap(CountryVo::getAbbreviation, v -> v, (v1, v2) -> v1));
            if (CollUtil.isNotEmpty(countryVoMap)) {
                log.info("CountryInfo:{}",JSON.toJSONString(countryVoMap.get(countryAbbreviation)));
                invoiceDTO.setCustomerCountryCode(countryVoMap.get(countryAbbreviation).getCountryName());
            }
        }catch (Exception e){
            log.info("获取国家编码失败",e);
            invoiceDTO.setCustomerCountryCode("--");
        }
        //查询订单商品信息及费用信息
        subscribeInfoService.orderDetail(invoiceDTO, language);
        //查询下单的时区
        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
        final TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
        String timeZone = StringUtils.defaultIfEmpty(tbOrderRecordEntity.getTimeZone(), DEFAULT_TIMEZONE);
        String timeZoneString = " (" + timeZone + ")";
        if (invoiceDTO.getPayTime() != null) {
            String paymentDate = TimeZoneUtil.format(invoiceDTO.getPayTime(), timeZone, DATE_FORMAT_PATTERN);
            invoiceDTO.setPaymentDate(paymentDate + timeZoneString);
        } else {
            invoiceDTO.setPaymentDate(DEFAULT_PAYMENT_DATE);
        }
        invoiceDTO.setCreateOrderTime(TimeZoneUtil.format(Long.valueOf(invoiceDTO.getCreateOrderTime()), timeZone, TimeZoneUtil.DATE_PATTERN) + timeZoneString);

        //SubscribeInfoDetail subscribeInfoDetail = subscribeInfoService.subscribedOrderDetail(Long.valueOf(orderId), language);
        //invoiceDTO.setSubscribeInfoDetail(subscribeInfoDetail);

        return invoiceDTO;
    }

    @Override
    public Boolean expirationReminderEmail(List<ImminentExpireChargePointDTO> imminentExpireChargePointDTO, String jumpLink, String email, Locale locale) {
        String html;
        //加载模板
        org.springframework.core.io.Resource resource = new ClassPathResource("template/expiration_reminder_email.html");
        try (InputStream resourceAsStream = resource.getInputStream()) {
            Document document = Jsoup.parse(resourceAsStream, CHARSET_NAME, "");
            Element body = document.body();
            //状态
            final SubStatus subStatus = imminentExpireChargePointDTO.get(0).getSubStatus();
            //数量
            final int size = imminentExpireChargePointDTO.size();
            //正文
            String contentText;
            String pileSN;
            String pileName;
            String locationName;
            String produceName;
            String status;
            String remainingTime;
            String expireTime;
            String tableText;
            //共同表头
            fillLabels(body, "pileSN", MessageSourceHolder.getMessage(CHARGER_SN,null,locale));
            fillLabels(body, "pileName", MessageSourceHolder.getMessage(CHARGER_NAME,null,locale));
            fillLabels(body, "locationName", MessageSourceHolder.getMessage(LOCATION_NAME,null,locale));
            fillLabels(body, "produceName",MessageSourceHolder.getMessage(PRODUCT_NAME,null,locale));
            fillLabels(body, "status", MessageSourceHolder.getMessage(SUBSCRIPTION_STATUS,null,locale));
            if (subStatus.equals(SubStatus.SOON_TO_EXPIRE)) {
                fillLabels(body, "remainingTime", MessageSourceHolder.getMessage(REMAINING_TIME, null, locale));
            } else {
                Objects.requireNonNull(body.getElementById("remainingTh")).remove();
            }
            fillLabels(body, "expireTime", MessageSourceHolder.getMessage(SUBSCRIPTION_EXPIRATION_DATE,null,locale));
            final String remainderHeader = HEADER_TR_S + Objects.requireNonNull(body.getElementById("remainderHeader")).html() + TR_E;
            //正文
            contentText = MessageSourceHolder.getMessage(LICENSE_EXPIRED_EMAIL_CONTENT, null,"Your subscription plan for %s chargers has expired. Thus, charging and remote start/stop cannot be conducted properly. Please renew as soon as possible to avoid any disruption to your daily operations.",locale);
            if (subStatus.equals(SubStatus.SOON_TO_EXPIRE)) {
                contentText = MessageSourceHolder.getMessage(LICENSE_WILL_EXPIRE_EMAIL_CONTENT, null,"Your subscription plan for %s chargers is about to expire. After the expiration, charging cannot be conducted properly. Please renew your subscription plan in time to avoid any disruption to your daily operations.",locale);
            }
            contentText = String.format(contentText, size);
            fillLabels(body, "contentText", contentText);

            //表格
            StringBuilder htmlText = new StringBuilder();
            for (ImminentExpireChargePointDTO icp:imminentExpireChargePointDTO) {
                htmlText.append("<tr class=\"t-li\">");
                pileSN = icp.getSn();
                pileName = icp.getName();
                locationName = icp.getLocationName();
                produceName = icp.getGoodsName();
                status = MessageSourceHolder.getMessage(SOON_TO_EXPIRE,null,"Expiring Soon",locale);
                remainingTime = icp.getRemainDays() + MessageSourceHolder.getMessage(DAY, null,"days",locale);
                expireTime = icp.getExpireDate();
                htmlText.append(REMINDER_EMAIL_TD_S).append(pileSN).append(TD_E);
                htmlText.append(REMINDER_EMAIL_TD_S).append(pileName).append(TD_E);
                htmlText.append(REMINDER_EMAIL_TD_S).append(locationName).append(TD_E);
                htmlText.append(REMINDER_EMAIL_TD_S).append(produceName).append(TD_E);
                if (subStatus.equals(SubStatus.SOON_TO_EXPIRE)) {
                    htmlText.append(REMINDER_EMAIL_TD_S).append(SOON_EXPIRE_SPAN_S).append(status).append(SPAN_E).append(TD_E);
                    htmlText.append(REMINDER_EMAIL_TD_S).append(remainingTime).append(TD_E);
                } else {
                    status = MessageSourceHolder.getMessage(INVALIDITY,null,"Expired",locale);
                    htmlText.append(REMINDER_EMAIL_TD_S).append(EXPIRED_SPAN_S).append(status).append(SPAN_E).append(TD_E);
                }
                htmlText.append(REMINDER_EMAIL_TD_S).append(expireTime).append(TD_E);
                htmlText.append(TR_E);
            }
            //清空Table
            body.getElementById("remainderTable").empty();
            //头+数据
            tableText = remainderHeader + htmlText.toString();
            fillLabels(body, "remainderTable", tableText);
            //前往充电云
            body.getElementById("goForRenew").attr("href", jumpLink);
            fillLabels(body, "goForRenew", MessageSourceHolder.getMessage(GO_FOR_RENEWAL,null,"Go to Charging Cloud and Renew",locale));
            //发送邮件
            html = document.html();
            log.info("html:\n {}", html);
            EmailSendDTO emailSendDto = new EmailSendDTO();
            emailSendDto.setContent(html);
            emailSendDto.setEmail(email);
            final String subject = MessageSourceHolder.getMessage(subStatus.equals(SubStatus.SOON_TO_EXPIRE) ? LICENSE_WILL_EXPIRE_EMAIL_SUBJECT : LICENSE_EXPIRED_EMAIL_SUBJECT, null, "Expiration Reminder", locale);
            emailSendDto.setSubject(String.format(subject, size));
            emailSendDto.setSenderName(SENDER);
            threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
                try {
                    commonService.sendEmail(emailSendDto);
                } catch (Exception e) {
                    log.info("邮件发送失败", e);
                }
            }));
            return true;
        } catch (IOException e) {
            log.error("IoException", e);
            sendMessage(nacosNs + "发送过期提醒邮件失败，邮箱：" + email + ", 商家："+ imminentExpireChargePointDTO.get(0).getMerchantId() + e.getMessage());
            throw new MessageCodeException(PileBillExceptionEnum.INVOICE_PDF);
        }
    }

    @Override
    public Boolean sendSubscriptionProductArrival(String orderId, String language,String tenantId) {
        final SubscribedInvoiceDTO subscribedInvoiceDTO = invoiceV2Download(orderId, language,tenantId);
        log.info("SubscribedInvoiceDTO : {}", JSON.toJSONString(subscribedInvoiceDTO));
        //查询商家的邮箱信息
        final Result<SellerDetailVO> detail = pileUserFeign.detail(Long.valueOf(tenantId));
        if (ObjectUtil.isNull(detail) || !detail.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(detail.getData())) {
            sendMessage("未查询到商家邮箱信息！邮件发送失败！");
            return false;
        }
        final SellerDetailVO sellerDetailVO = detail.getData();
        final SubscribeInfoDetail subscribeInfoDetail = subscribedInvoiceDTO.getSubscribeInfoDetail();
        final List<OrderCommodityDetail> orderCommodityDetails = subscribeInfoDetail.getOrderCommodityDetails();
        // evota&autel 兼容处理
        boolean evota = isEvotaSeller(orderId);

        // 创建模板解析器
        ClassLoaderTemplateResolver templateResolver = new ClassLoaderTemplateResolver();
        templateResolver.setPrefix("template/");
        templateResolver.setSuffix(".html");
        templateResolver.setTemplateMode(TemplateMode.HTML);

        // 创建模板引擎
        TemplateEngine templateEngine = new TemplateEngine();
        templateEngine.setTemplateResolver(templateResolver);

        // 创建上下文并添加需要替换的数据
        Context context = new Context();
        context.setVariable(AUTEL_LOGO, Boolean.TRUE.equals(evota) ? EVOTA_LOGO_VALUE : AUTEL_LOGO_VALUE);
        context.setVariable(BILL_TO, MessageSourceHolder.getMessage(BILL_TO, "Bill To") + COLON);
        context.setVariable(CUSTOMER_NAME, MessageSourceHolder.getMessage(CUSTOMER_NAME, "Customer Name") + COLON);
        context.setVariable(CUSTOMER_NAME_VALUE, subscribedInvoiceDTO.getCustomerName());
        context.setVariable(CUSTOMER_ADDRESS, MessageSourceHolder.getMessage(CUSTOMER_ADDRESS, "Customer Address") + COLON);
        context.setVariable(CUSTOMER_ADDRESS_VALUE, subscribedInvoiceDTO.getCustomerAddress());
        context.setVariable(CUSTOMER_COUNTRY_CODE, MessageSourceHolder.getMessage(CUSTOMER_COUNTRY_CODE, "Country") + COLON);
        context.setVariable(CUSTOMER_COUNTRY_CODE_VALUE, subscribedInvoiceDTO.getCustomerCountryCode());
        context.setVariable(HELLO_CUSTOMER, MessageSourceHolder.getMessage(HELLO_CUSTOMER, "尊敬的客户:"));
        String subscribeSuccessCheck = String.format(MessageSourceHolder.getMessage(SUBSCRIBE_SUCCESS_CHECK, "您好!道通云订阅服务订单%s，商品已下发至您的账号(%s)下,请登录道通云服务平台CSMS，在许可证管理中查看使用,商品信息如下："), subscribedInvoiceDTO.getOrderId(), subscribedInvoiceDTO.getCustomerName());
        if (evota) {
            subscribeSuccessCheck = subscribeSuccessCheck.replace("Autel", "").replace("道通", "");
        }
        context.setVariable(SUBSCRIBE_SUCCESS_CHECK, subscribeSuccessCheck);
        context.setVariable(ORDER_ID, MessageSourceHolder.getMessage(ORDER_ID, "Order Id") + COLON + subscribedInvoiceDTO.getOrderId());
        context.setVariable(PAYMENT_DATE, MessageSourceHolder.getMessage(PAYMENT_DATE, "Payment Date") + COLON + subscribedInvoiceDTO.getPaymentDate());
        context.setVariable(COMMODITY_NAME, MessageSourceHolder.getMessage(COMMODITY_NAME, "Commodity Name"));
        context.setVariable(SUBSCRIPTION_DURATION, MessageSourceHolder.getMessage(SUBSCRIPTION_DURATION, "Subscription Duration"));
        context.setVariable(PORT_TYPE, MessageSourceHolder.getMessage(PORT_TYPE, "Port Type"));
        context.setVariable(QUANTITY, MessageSourceHolder.getMessage(QUANTITY, "Quantity"));
        context.setVariable(LICENSE_NUMBER, MessageSourceHolder.getMessage(LICENSE_NUMBER, "license编码"));
        context.setVariable(ORDER_COMMODITY_DETAILS, orderCommodityDetails);
        context.setVariable(COMPANY_NAME, Boolean.TRUE.equals(evota) ? "Autel Netherlands B.V." : subscribedInvoiceDTO.getCompanyName());
        context.setVariable(COMPANY_ADDR, Boolean.TRUE.equals(evota) ? "Laan van Waalhaven 217, 2497 GL, Den Haag, Netherlands" : subscribedInvoiceDTO.getCompanyAddress());
        context.setVariable(SUBSCRIBE_CREATE_ORDER_DATE, MessageSourceHolder.getMessage(SUBSCRIBE_CREATE_ORDER_DATE, "Order Date") + COLON + subscribedInvoiceDTO.getCreateOrderTime());


        // 处理模板并输出结果
        String html = templateEngine.process("subscription_product_arrival.html", context);

        EmailSendDTO emailSendDto = new EmailSendDTO();
        emailSendDto.setContent(html);
        emailSendDto.setEmail(sellerDetailVO.getEmail());
        emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBSCRIBE_SUCCESS_SUBJECT, "道通云服务订阅成功通知"));
        emailSendDto.setSenderName(SENDER);
        if (evota) {
            emailSendDto.setSenderName(SENDER_EVOTA);
            emailSendDto.setBrand("evota");
            emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBSCRIBE_SUCCESS_SUBJECT, "道通云服务订阅成功通知").replaceAll("Autel", "Evota"));
        }
        threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
            try {
                commonService.sendEmail(emailSendDto);
            } catch (Exception e) {
                log.info("邮件发送失败", e);
            }
        }));
        return true;
    }

    @Override
    public Boolean sendSubscriptionProductReturn(String orderId, Long returnId, String language, String tenantId) {
        final SubscribedInvoiceDTO subscribedInvoiceDTO = invoiceV2Download(orderId, language, tenantId);
        log.info("SubscribedInvoiceDTO : {}", JSON.toJSONString(subscribedInvoiceDTO));
        //查询商家的邮箱信息
        final Result<SellerDetailVO> detail = pileUserFeign.detail(Long.valueOf(tenantId));
        if (ObjectUtil.isNull(detail) || !detail.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(detail.getData())) {
            sendMessage("未查询到商家邮箱信息！邮件发送失败！");
            return false;
        }
        final SellerDetailVO sellerDetailVO = detail.getData();
        final SubscribeInfoDetail subscribeInfoDetail = subscribedInvoiceDTO.getSubscribeInfoDetail();
        final List<OrderCommodityDetail> orderCommodityDetails = subscribeInfoDetail.getOrderCommodityDetails();
        List<OrderCommodityDetail> orderCommodityDetailList = queryReturnLicense(orderCommodityDetails, returnId,subscribedInvoiceDTO.getCurrencySymbol());

        // evota&autel 兼容处理
        boolean evota = isEvotaSeller(orderId);

        // 创建模板解析器
        ClassLoaderTemplateResolver templateResolver = new ClassLoaderTemplateResolver();
        templateResolver.setPrefix("template/");
        templateResolver.setSuffix(".html");
        templateResolver.setTemplateMode(TemplateMode.HTML);

        // 创建模板引擎
        TemplateEngine templateEngine = new TemplateEngine();
        templateEngine.setTemplateResolver(templateResolver);

        // 创建上下文并添加需要替换的数据
        Context context = new Context();
        context.setVariable(AUTEL_LOGO, Boolean.TRUE.equals(evota) ? EVOTA_LOGO_VALUE : AUTEL_LOGO_VALUE);
        context.setVariable(BILL_TO, MessageSourceHolder.getMessage(BILL_TO, "Bill To") + COLON);
        context.setVariable(CUSTOMER_NAME, MessageSourceHolder.getMessage(CUSTOMER_NAME, "Customer Name") + COLON);
        context.setVariable(CUSTOMER_NAME_VALUE, subscribedInvoiceDTO.getCustomerName());
        context.setVariable(CUSTOMER_ADDRESS, MessageSourceHolder.getMessage(CUSTOMER_ADDRESS, "Customer Address") + COLON);
        context.setVariable(CUSTOMER_ADDRESS_VALUE, subscribedInvoiceDTO.getCustomerAddress());
        context.setVariable(CUSTOMER_COUNTRY_CODE, MessageSourceHolder.getMessage(CUSTOMER_COUNTRY_CODE, "Country") + COLON);
        context.setVariable(CUSTOMER_COUNTRY_CODE_VALUE, subscribedInvoiceDTO.getCustomerCountryCode());
        context.setVariable(HELLO_CUSTOMER, MessageSourceHolder.getMessage(HELLO_CUSTOMER, "尊敬的客户:"));
        String subscribeReturnSuccessCheck = String.format(MessageSourceHolder.getMessage(SUBSCRIBE_RETURN_SUCCESS_CHECK, "您好!道通云订阅服务订单%s以下商品已完成退货，商品已失效。"), subscribedInvoiceDTO.getOrderId());
        if (evota) {
            subscribeReturnSuccessCheck = subscribeReturnSuccessCheck.replace("Autel", "").replace("道通", "");
        }
        context.setVariable(SUBSCRIBE_RETURN_SUCCESS_CHECK, subscribeReturnSuccessCheck);
        context.setVariable(SUBSCRIBE_QUESTIONS_CONSULTATION, MessageSourceHolder.getMessage(SUBSCRIBE_QUESTIONS_CONSULTATION, "如对此订单有疑问,您可咨询您的销售代表"));
        context.setVariable(ORDER_ID, MessageSourceHolder.getMessage(ORDER_ID, "Order Id") + COLON + subscribedInvoiceDTO.getOrderId());
        context.setVariable(PAYMENT_DATE, MessageSourceHolder.getMessage(PAYMENT_DATE, "Payment Date") + COLON + subscribedInvoiceDTO.getPaymentDate());

        context.setVariable(COMMODITY_NAME, MessageSourceHolder.getMessage(COMMODITY_NAME, "Commodity Name"));
        context.setVariable(SUBSCRIPTION_DURATION, MessageSourceHolder.getMessage(SUBSCRIPTION_DURATION, "Subscription Duration"));
        context.setVariable(PORT_TYPE, MessageSourceHolder.getMessage(PORT_TYPE, "Port Type"));
        context.setVariable(QUANTITY, MessageSourceHolder.getMessage(QUANTITY, "Quantity"));
        context.setVariable(LICENSE_NUMBER, MessageSourceHolder.getMessage(LICENSE_NUMBER, "license编码"));
        context.setVariable(SUBSCRIPTION_RETURN_AMOUNT, MessageSourceHolder.getMessage(SUBSCRIPTION_RETURN_AMOUNT, "Return Amount"));
        context.setVariable(ORDER_COMMODITY_DETAILS, orderCommodityDetailList);
        context.setVariable(COMPANY_NAME, Boolean.TRUE.equals(evota) ? "Autel Netherlands B.V." : subscribedInvoiceDTO.getCompanyName());
        context.setVariable(COMPANY_ADDR, Boolean.TRUE.equals(evota) ? "Laan van Waalhaven 217, 2497 GL, Den Haag, Netherlands" : subscribedInvoiceDTO.getCompanyAddress());
        context.setVariable(CURRENCY, subscribeInfoDetail.getCurrencySymbol());

        // 处理模板并输出结果
        String html = templateEngine.process("subscription_product_return.html", context);

        EmailSendDTO emailSendDto = new EmailSendDTO();
        emailSendDto.setContent(html);
        emailSendDto.setEmail(sellerDetailVO.getEmail());
        emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBSCRIBE_RETURN_SUCCESS_SUBJECT, "道通云服务商品订阅订单退货成功通知"));
        emailSendDto.setSenderName(SENDER);
        if (evota) {
            emailSendDto.setSenderName(SENDER_EVOTA);
            emailSendDto.setBrand("evota");
            emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBSCRIBE_RETURN_SUCCESS_SUBJECT, "道通云服务商品订阅订单退货成功通知").replaceAll("Autel", "Evota"));
        }
        threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
            try {
                commonService.sendEmail(emailSendDto);
            } catch (Exception e) {
                log.info("邮件发送失败", e);
            }
        }));
        return true;
    }

    private List<OrderCommodityDetail> queryReturnLicense(List<OrderCommodityDetail> orderCommodityDetails, Long returnId, String currencySymbol) {
        LambdaQueryWrapper<TbLicenseReturnRecordEntity> queryWrapper = new LambdaQueryWrapper<TbLicenseReturnRecordEntity>()
                .in(TbLicenseReturnRecordEntity::getReturnId, returnId);
        List<TbLicenseReturnRecordEntity> tbLicenseReturnRecordEntityList = tbLicenseReturnRecordMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(orderCommodityDetails)) {
            return Collections.emptyList();
        }
        Result<OrderReturnDetailRespVo> result;
        try {
            result = orderCenterFeignClient.returnDetail(returnId);
            //异常返回
            if (ObjectUtil.isNull(result) || !result.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(result.getData())) {
                log.info("subscribeInfoPage Feign结果异常");
                return null;
            }
        } catch (Exception e) {
            log.info("subscribeInfoPage Feign调用失败！", e);
            return null;
        }

        Map<String, List<TbLicenseReturnRecordEntity>> recordMap = tbLicenseReturnRecordEntityList.stream()
                .collect(Collectors.groupingBy(TbLicenseReturnRecordEntity::getLicenseCode));
        List<OrderCommodityDetail> orderCommodityDetailList = new ArrayList<>();
        orderCommodityDetails.get(0).setSubscriptionReturnAmount(Optional.ofNullable(result.getData().getRefundedAmount()).orElse(BigDecimal.ZERO));
        orderCommodityDetails.get(0).setCurrencySymbol(currencySymbol);
        for (OrderCommodityDetail orderCommodityDetail : orderCommodityDetails) {
            List<String> licenseCodeList = orderCommodityDetail.getLicenseCodeList();
            if (licenseCodeList != null) {
                List<String> licenses = new ArrayList<>();
                Integer quantity = 0;
                for (String licenseCode : licenseCodeList) {
                    List<TbLicenseReturnRecordEntity> records = recordMap != null ? recordMap.get(licenseCode) : null;
                    if (records != null) {
                        quantity += records.size();
                        licenses.add(licenseCode);
                    }
                }
                if (quantity > 0) {
                    orderCommodityDetail.setQuantity(quantity);
                    orderCommodityDetail.setLicenseCodeList(licenses);
                    orderCommodityDetailList.add(orderCommodityDetail);
                }
            }
        }
        return orderCommodityDetailList;
    }




    final Text text = new Text();
    final WeChatMessage weChatMessage = new TextMessage(text);

    private void sendMessage(String format) {
        log.info(format);
        text.setContent(format);
        weChatClient.sendMessage(weChatMessage, webhookWechatKey);
    }
}
