package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.http.vo.PageReqVO;
import com.autel.cloud.base.http.vo.PageRespVO;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.ordercenter.CommodityAttributeEnum;
import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
import com.autel.cloud.ordercenter.vo.*;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.convert.license.LicenseAboutConvert;
import com.autel.cloud.pile.base.domain.model.LicenseInfo;
import com.autel.cloud.pile.base.domain.model.*;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantTerminalService;
import com.autel.cloud.pile.base.domain.service.SubscribeInfoService;
import com.autel.cloud.pile.base.domain.utils.TimeZoneUtil;
import com.autel.cloud.pile.base.enums.license.LicenceStatusEnum;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantTerminalEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.vo.ChargePointVO;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.autel.cloud.pile.user.api.vo.UserDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

@Slf4j
@Service
public class SubscribeInfoServiceImpl implements SubscribeInfoService {
    @Resource
    OrderCenterFeignClient orderCenterFeignClient;

    @Resource
    private PileUserServiceFeign pileUserServiceFeign;

    @Resource
    TbLenBindRelationMapper tbLenBindRelationMapper;

    @Resource
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Resource
    private ChargePointMerchantTerminalService chargePointMerchantTerminalService;

    @Resource
    private RedisUtil redisUtil;

    /**
     * 1705248000000: 北京时间 2024/1/15 00:00:00
     */
    @Value("${subscribe.boundaryMillisecond:1705248000000}")
    private Long boundaryMillisecond;

    @Override
    public PageDTO<SubscribeInfo> subscribeInfoPage(SubscribeInfoPageVO searchDTO, String language) {
        //调用订单中心
        Result<PageRespVO<ClientOrderListRespVo>> result;
        try {
            searchDTO.getData().setOrderBy("subscribeTime");
            searchDTO.getData().setOrderType(searchDTO.getOrderType());

            log.info("call subscribedOrderPage, searchDTO: {}", JSON.toJSONString(searchDTO));
            result = orderCenterFeignClient.subscribedOrderPage(searchDTO, language);
            log.info("Subscribed order page result: {}", JSON.toJSONString(result));
            //异常返回
            if (ObjectUtil.isNull(result) || !result.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(result.getData()) || CollUtil.isEmpty(result.getData().getResult())) {
                log.info("subscribeInfoPage Feign结果异常,result : {}",result);
                return null;
            }
        } catch (Exception e) {
            log.info("subscribeInfoPage Feign调用失败！", e);
            return null;
        }
        final PageRespVO<ClientOrderListRespVo> orderData = result.getData();
        log.info("orderData:{}", JSON.toJSONString(orderData));
        //返回值
        final PageDTO<SubscribeInfo> subscribeInfoPageDTO = new PageDTO<>();
        subscribeInfoPageDTO.setPageNo(orderData.getPage());
        subscribeInfoPageDTO.setPageSize(orderData.getPageSize());
        subscribeInfoPageDTO.setTotalCount(orderData.getTotal());
        //总页数
        long pages;
        if (orderData.getTotal() % orderData.getPageSize() == 0) {
            pages = orderData.getTotal() / orderData.getPageSize();
        } else {
            pages = orderData.getTotal() / orderData.getPageSize() + 1;
        }
        subscribeInfoPageDTO.setPages(pages);
        //填充data
        List<SubscribeInfo> subscribeInfos = encapsulationSubscribeInfo(orderData.getResult());
        subscribeInfoPageDTO.setData(subscribeInfos);

        return subscribeInfoPageDTO;
    }

    @Override
    public SubscribeInfoDetail subscribedOrderDetail(Long orderId, String language) {
        //调用订单中心
        Result<ClientOrderDetailRespVo> result;
        try {
            result = orderCenterFeignClient.subscribedOrderDetail(orderId, language);
            log.info("subscribed order detail result:{}", JSON.toJSONString(result));
            //异常返回
            if (ObjectUtil.isNull(result) || !result.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(result.getData())) {
                log.info("subscribeInfoPage Feign结果异常");
                return null;
            }
        } catch (Exception e) {
            log.info("subscribeInfoPage Feign调用失败！", e);
            return null;
        }
        final ClientOrderDetailRespVo orderDetail = result.getData();
        final SubscribeInfoDetail subscribeInfoDetail = encapsulationSubscribeOrderDetail(orderDetail);
        if (Integer.valueOf(1).equals(subscribeInfoDetail.getOrderStatus())) {
            //订单未支付，查询此订单的缓存链接并返回
            final String payUrl = (String) redisUtil.get(BaseConstant.PLACE_URL_LINK + orderId.toString());
            subscribeInfoDetail.setPayUrl(payUrl);
        }
        subscribeInfoDetail.setCreateOrderTime(orderDetail.getCreatTime());
        return subscribeInfoDetail;
    }

    @Override
    public void orderDetail(SubscribedInvoiceDTO invoiceDTO, String language) {
        //调用订单中心
        Result<ClientOrderDetailRespVo> result;
        try {
            result = orderCenterFeignClient.subscribedOrderDetail(Long.valueOf(invoiceDTO.getOrderId()), language);
            log.info("subscribed order detail result:{}", JSON.toJSONString(result));
            //异常返回
            if (ObjectUtil.isNull(result) || !result.getCode().equals(HttpStatus.OK.value()) || ObjectUtil.isNull(result.getData())) {
                log.info("subscribeInfoPage Feign结果异常");
                return;
            }
        } catch (Exception e) {
            log.info("subscribeInfoPage Feign调用失败！", e);
            return;
        }
        final ClientOrderDetailRespVo orderDetail = result.getData();
        final SubscribeInfoDetail subscribeInfoDetail = encapsulationSubscribeOrderDetail(orderDetail);
        if (Integer.valueOf(1).equals(subscribeInfoDetail.getOrderStatus())) {
            //订单未支付，查询此订单的缓存链接并返回
            final String payUrl = (String) redisUtil.get(BaseConstant.PLACE_URL_LINK + invoiceDTO.getOrderId());
            subscribeInfoDetail.setPayUrl(payUrl);
        }
        OrderReceiverVo orderReceiver = orderDetail.getOrderReceiver();
        if (orderReceiver != null) {
            invoiceDTO.setCustomerName(orderReceiver.getMerchantName());
            invoiceDTO.setCustomerAddress(orderReceiver.getAddress());
            invoiceDTO.setCustomerCountryCode(orderReceiver.getCountryCode());
            invoiceDTO.setCustomerId(orderReceiver.getMerchantId());
        }
        OrderPayRespVo orderPay = orderDetail.getOrderPay();
        if (orderPay != null) {
            invoiceDTO.setPayTime(orderPay.getPayTime());
        }
        invoiceDTO.setCreateOrderTime(orderDetail.getCreatTime().toString());
        invoiceDTO.setSubscribeInfoDetail(subscribeInfoDetail);
    }

    @Override
    public PageDTO<LicenseInfo> licenseInfoOfOrder(LicenseSearchDTO licenseSearchDTO) {
        //查询订单的License信息
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbLenBindRelationEntity::getOrderId, licenseSearchDTO.getOrderId());
        Optional.ofNullable(licenseSearchDTO.getPileOrGunType()).ifPresent(type -> queryWrapper.eq(TbLenBindRelationEntity::getChargeType, type));
        queryWrapper.orderByAsc("asc".equalsIgnoreCase(licenseSearchDTO.getOrderBy()), TbLenBindRelationEntity::getBindTime);
        queryWrapper.orderByDesc("desc".equalsIgnoreCase(licenseSearchDTO.getOrderBy()), TbLenBindRelationEntity::getBindTime);


        Page<TbLenBindRelationEntity> page = new Page<>();
        page.setCurrent(licenseSearchDTO.getPage());
        page.setSize(licenseSearchDTO.getPageSize());
        final Page<TbLenBindRelationEntity> tbLenBindRelationEntityIPage = tbLenBindRelationMapper.selectPage(page, queryWrapper);
        final List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationEntityIPage.getRecords();
        List<LicenseInfo> licenseInfos = new ArrayList<>();
        tbLenBindRelationEntities.forEach(item -> {
            //封装License信息
            LicenseInfo licenseInfo = new LicenseInfo();
            licenseInfo.setOrderId(item.getOrderId());
            licenseInfo.setLicenseCode(item.getLicenceCode());
            // 处理状态映射
            Integer finalStatus = licenceStatusMapping(item);
            licenseInfo.setStatus(finalStatus);
            licenseInfo.setPileSn(item.getPileSn());
            licenseInfo.setBindTime(item.getBindTime());
            licenseInfo.setServiceTime(item.getServiceTime() + " " + item.getTimeUnit());
            //查询桩名称
            final ChargePointVO pileInfoByPileSn = chargePointMerchantRelationService.findBySN(item.getPileSn(), LoginUserHolder.getLoginUser().getPayload().getSellerId());
            if (ObjectUtil.isNotNull(pileInfoByPileSn)) {
                licenseInfo.setPileName(pileInfoByPileSn.getName());
            } else {
                List<ChargePointMerchantTerminalEntity> terminalList = chargePointMerchantTerminalService.getTerminalList(Collections.singletonList(item.getPileSn()), LoginUserHolder.getLoginUser().getPayload().getSellerId());
                if (CollUtil.isNotEmpty(terminalList)) {
                    licenseInfo.setPileName(terminalList.get(0).getTerminalName());
                }
            }

            // 设置剩余有效宽限期 剩余宽限期天数 =  额外增加天数 + 90 - （当前日期 - 创建日期 ）相差天数
            Long createTime = item.getCreateTime();
            int baseDays = createTime < boundaryMillisecond ? 180 : 90;
            Integer extended = Optional.ofNullable(item.getExtendedGracePeriod()).orElse(0);
            int extendedRemainingDays =  (int)Math.floor(baseDays + extended -(((double)(System.currentTimeMillis() - createTime))/(3600000 * 24)));
            if (item.getStatus() == null || item.getStatus() == 0) {
                licenseInfo.setExtendedRemainingDays(extendedRemainingDays);
            }
            licenseInfos.add(licenseInfo);
        });
        PageDTO<LicenseInfo> pageResult = new PageDTO<>();
        pageResult.setData(licenseInfos);
        pageResult.setPages(tbLenBindRelationEntityIPage.getPages());
        pageResult.setTotalCount(tbLenBindRelationEntityIPage.getTotal());
        pageResult.setPageNo((int) tbLenBindRelationEntityIPage.getCurrent());
        return pageResult;
    }

    private Integer licenceStatusMapping(TbLenBindRelationEntity entity) {
        Integer finalStatus;
        if (LicenceStatusEnum.UNUSED.getCode().equals(entity.getStatus()) && Optional.ofNullable(entity.getAvailableTime()).orElse(0L) > 0L) {
            finalStatus =  LicenceStatusEnum.AUTO_ACTIVE.getCode();
        } else if (LicenceStatusEnum.EXPIRED.getCode().equals(entity.getStatus())) {
            finalStatus =  LicenceStatusEnum.USED.getCode();
        } else {
            finalStatus = entity.getStatus();
        }

        return finalStatus;
    }

    private SubscribeInfoDetail encapsulationSubscribeOrderDetail(ClientOrderDetailRespVo orderDetail) {
        SubscribeInfoDetail subscribeInfoDetail = new SubscribeInfoDetail();
        //商品信息列表
        final ArrayList<OrderCommodityDetail> orderCommodityDetails = new ArrayList<>();
        List<BigDecimal> vatRates = new ArrayList<>();
        List<BigDecimal> salesTaxRates = new ArrayList<>();
        orderDetail.getOrderCommodityDetails().forEach(item -> {
            OrderCommodityDetail orderCommodityDetail = new OrderCommodityDetail();
            orderCommodityDetail.setCommodityName(item.getCommodityName());
            //订阅时长及枪类型
            for (OrderCommodityAttrRespVo orderCommodityAttrRespVo : item.getCommodityAttrs()) {
                if (orderCommodityAttrRespVo.getCode().equals(CommodityAttributeEnum.SUBSCRIPTION_CYCLE.getAttributeCode())) {
                    orderCommodityDetail.setSubscriptionDuration(orderCommodityAttrRespVo.getValue());
                    continue;
                }
                if ((orderCommodityAttrRespVo.getCode().equals(CommodityAttributeEnum.CHARGING_GUN_TYPE.getAttributeCode()) || orderCommodityAttrRespVo.getCode().equals(CommodityAttributeEnum.CHARGING_PILE_TYPE.getAttributeCode()))
                        && StringUtils.isNotEmpty(orderCommodityAttrRespVo.getValue())) {
                    orderCommodityDetail.setPortOrGunType(orderCommodityAttrRespVo.getValue());
                }
            }
            //附加时长处理
            String subscriptionDuration = orderCommodityDetail.getSubscriptionDuration();
            if (subscriptionDuration != null && StringUtils.isNotBlank(item.getBonusDurationValue())) {
                final String s = subscriptionDuration + "+" + item.getBonusDurationValue();
                orderCommodityDetail.setSubscriptionDuration(s);
            }
            //数量
            orderCommodityDetail.setQuantity(item.getQuantity());
            //单价
            orderCommodityDetail.setUnitPrice(item.getUnitPrice().setScale(2,BigDecimal.ROUND_HALF_UP));
            //总价
            orderCommodityDetail.setSubTotal(item.getSubTotal().setScale(2,BigDecimal.ROUND_HALF_UP));
            Optional.ofNullable(item.getCommodityPerformance()).map(OrderCommodityPerformanceRespVo::getLicenses)
                    .filter(CollectionUtils::isNotEmpty)
                    .ifPresent(licenses -> {
                        List<String> collect = licenses.stream()
                                .map(OrderCommodityLicenseRespVo::getLicenseCode)
                                .collect(Collectors.toList());
                        orderCommodityDetail.setLicenseCodeList(collect);
                    });
            orderCommodityDetails.add(orderCommodityDetail);
            //税率
            final List<OrderCommodityTaxRespVo> commodityTaxes = item.getCommodityTaxes();
            for (OrderCommodityTaxRespVo vo : commodityTaxes) {
                BigDecimal taxRate = vo.getTaxRate();
                if (taxRate != null) {
                    taxRate = taxRate.setScale(2,BigDecimal.ROUND_HALF_UP);
                }
                if ("valueAddedTax".equals(vo.getTaxCode())) {
                    vatRates.add(taxRate);
                }
                if ("salesTax".equals(vo.getTaxCode())) {
                    salesTaxRates.add(taxRate);
                }
            }
        });
        //------商品列表------
        subscribeInfoDetail.setOrderCommodityDetails(orderCommodityDetails);
        //------基本信息------
        subscribeInfoDetail.setProduceName(orderDetail.getOrderCommodityDetails().get(0).getCommodityName());
        subscribeInfoDetail.setSubscriptionTime(orderDetail.getCreatTime());
        subscribeInfoDetail.setOrderStatus(orderDetail.getStatus());
        //------以下金额相关------
        subscribeInfoDetail.setCurrencySymbol(orderDetail.getCurrencySymbol());
        Optional.ofNullable(orderDetail.getSubTotal()).ifPresent(subTotal -> subscribeInfoDetail.setSubTotal(subTotal.setScale(2,BigDecimal.ROUND_HALF_UP)));
        Optional.ofNullable(orderDetail.getVat()).ifPresent(vat -> subscribeInfoDetail.setVat(vat.setScale(2,BigDecimal.ROUND_HALF_UP)));
        Optional.ofNullable(orderDetail.getSalesTax()).ifPresent(salesTax -> subscribeInfoDetail.setSalesTax(salesTax.setScale(2,BigDecimal.ROUND_HALF_UP)));
        Optional.ofNullable(orderDetail.getTotalTax()).ifPresent(totalTax -> subscribeInfoDetail.setTotalTax(totalTax.setScale(2,BigDecimal.ROUND_HALF_UP)));
        Optional.ofNullable(orderDetail.getTotalDiscount()).ifPresent(totalDiscount -> subscribeInfoDetail.setTotalDiscount(totalDiscount.setScale(2,BigDecimal.ROUND_HALF_UP)));
        Optional.ofNullable(orderDetail.getTotalAmount()).ifPresent(totalAmount -> subscribeInfoDetail.setTotalAmount(totalAmount.setScale(2,BigDecimal.ROUND_HALF_UP)));
        subscribeInfoDetail.setVatRates(vatRates);
        subscribeInfoDetail.setSalesTaxRates(salesTaxRates);
        subscribeInfoDetail.setPurchaseMethod(orderDetail.getPurchaseMethod());

        Long createBy = orderDetail.getCreateBy();
        Result<UserDetailVO> userDetail = pileUserServiceFeign.getUserDetail(createBy);
        if (userDetail.getData() != null) {
            String name = userDetail.getData().getName();
            subscribeInfoDetail.setOperatorName(name);
        }
        return subscribeInfoDetail;
    }

    private List<SubscribeInfo> encapsulationSubscribeInfo(List<ClientOrderListRespVo> result) {
        List<SubscribeInfo> subscribeInfos = new ArrayList<>();
        result.forEach(item -> {
            SubscribeInfo subscribeInfo = new SubscribeInfo();
            subscribeInfo.setOrderId(item.getOrderId());
            subscribeInfo.setOrderStatus(item.getStatus());
            subscribeInfo.setProductName(item.getCommodityName());
            subscribeInfo.setPurchaseMethod(item.getPurchaseMethod());
            subscribeInfo.setLicenseNumber(item.getLicenseCount());
            subscribeInfo.setPayRequired(item.getPayRequired());
            subscribeInfo.setPayUrl(item.getPayUrl());

            //订阅日期（ms）
            subscribeInfo.setSubscriptionTime(item.getSubscribeTime());
            Long createBy = item.getCreateBy();
            Result<UserDetailVO> userDetail = pileUserServiceFeign.getUserDetail(createBy);
            if (userDetail.getData() != null) {
                String name = userDetail.getData().getName();
                subscribeInfo.setOperatorName(name);
            }
            subscribeInfos.add(subscribeInfo);
        });
        return subscribeInfos;
    }
}
