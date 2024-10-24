package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.Method;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.crm.dto.OrderTypeNoticeDTO;
import com.autel.cloud.crm.dto.RfdNoticeDto;
import com.autel.cloud.ordercenter.feign.OrderCenterFeignClient;
import com.autel.cloud.ordercenter.vo.*;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.repository.OpCountryRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.service.*;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.dto.UpdateEsSubscriptionStatusDTO;
import com.autel.cloud.pile.base.enums.OrderSourceIdEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.UnitOfQuantityCodeEnum;
import com.autel.cloud.pile.base.feign.SubscribeGoodsFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.BaseAdminClient;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.CrmServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.TbHonourAgreementMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbOrderPileMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbOrderRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.infrastructure.util.AutelSignUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.Connector;
import com.autel.cloud.pile.user.api.dto.SellerAddDTO;
import com.autel.cloud.pile.user.api.enums.CountryDomainEnum;
import com.autel.cloud.pile.user.api.enums.EnvironmentEnum;
import com.autel.cloud.pile.user.api.enums.PileUserEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.sql.Timestamp;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/12 14:29
 */
@Service
@Slf4j
public class SubscribePlaceOrderServiceImpl implements SubscribePlaceOrderService {

    @Value("${subscribe.subscribeCallBack}")
    private String subscribeCallBack;

    @Autowired
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Autowired
    private ChargePointMerchantTerminalService chargePointMerchantTerminalService;

    @Autowired
    private TbOrderRecordMapper tbOrderRecordMapper;

    @Autowired
    private TbOrderPileMapper tbOrderPileMapper;

    @Autowired
    private TbHonourAgreementMapper honourAgreementMapper;

    @Autowired
    private TbLenBindRelationMapper lenBindRelationMapper;

    @Autowired
    private TbOrderPileMapper orderPileMapper;

    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private OrderCenterFeignClient orderCenterFeignClient;

    @Autowired
    private PileUserFeign pileUserFeign;

    @Autowired
    private PileUserServiceAdapter pileUserServiceAdapter;

    @Autowired
    private SubscribeGoodsFeignClient subscribeGoodsFeignClient;

    @Autowired
    private RedisUtil redisUtil;

    @Autowired
    private EmailSendingServiceImpl sendSubscribedInvoice;

    @Autowired
    private OpCountryRepository countryRepository;

    @Autowired
    private BaseAdminClient baseAdminClient;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Autowired
    private CrmServiceAdapter crmServiceAdapter;
    @Autowired
    private LicenseManagementService licenseManagementService;

    @Resource
    private EmailSendingService emailSendingService;

    /*
     * 获取当前nacos 启动空间
     * */
    private String nacosNs = System.getenv("NACOS_NS");

    /*
     * 加载当前配置支持的国际化语言
     * */
    @Value("${i18n.messageproperties}")
    private String langs;

    @Value("${subscribe.sapOfflineFlag:true}")
    private Boolean sapOfflineFlag;
    /*
     * 防止发生循环调用
     * */
    private static final String REDIRECT_KEY = "HaqEo$19&b7pW8!b9Z0rR#21*2aqO09&46^iLsT51wBOce3";

    @Override
    @Transactional
    public Result<ClientOrderCreateRespVo> placeOrder(OrderCreateReqVo orderCreateReqVo) {

        log.info("--->>> placeOrder request param: {}", JSON.toJSONString(orderCreateReqVo));

        // param check
        paramCheck(orderCreateReqVo);

        // request param build
        orderCreateReqVoBuild(orderCreateReqVo);

        // save order
        saveOrderAndSns(orderCreateReqVo);

        // call order-center
        log.info("placeOrder orderCreateReqVo {}", JSON.toJSONString(orderCreateReqVo));
        Result<ClientOrderCreateRespVo> placeOrderResult = orderCenterFeignClient.placeOrder(orderCreateReqVo);

        log.info("--->>> placeOrderResult : {}", JSON.toJSONString(placeOrderResult));

        // update order id
        updateOrderId(placeOrderResult);

        if (placeOrderResult.getData() != null) {
            // 缓存支付链接24小时
            redisUtil.set(BaseConstant.PLACE_URL_LINK + placeOrderResult.getData().getOrderId(), placeOrderResult.getData().getPayUrl(), 3600 * 24);
        }

        return placeOrderResult;
    }

    @Override
    public Result<List<ClientCommodityRespVo>> queryGoodsList(ClientCommodityReqVo commodityReqVo) {
        Long tenantId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        log.info("--->>> queryGoodsList, tenantId:{}", tenantId);

        //如果国家2字码空，提示异常信息
        SellerDetailVO merchantByIdSupportFallback = pileUserServiceAdapter.findMerchantById(tenantId);
        if (merchantByIdSupportFallback == null || StringUtils.isBlank(merchantByIdSupportFallback.getCountryAbbreviation())) {
            throw new MessageCodeException(PileBaseEnum.SELLER_COUNTRY_ABBREVIATION_IS_NULL);
        }
        commodityReqVo.setTenantId(String.valueOf(tenantId));
        commodityReqVo.setAlpha2Code(merchantByIdSupportFallback.getCountryAbbreviation());
        commodityReqVo.setZipCode(merchantByIdSupportFallback.getZipCode());
        Result<List<ClientCommodityRespVo>> listResult = subscribeGoodsFeignClient.queryGoodsList(commodityReqVo);
        log.info("--->>> queryGoodsList, call commodity-center returns: {}", JSON.toJSONString(listResult));
        return listResult;
    }

    @Override
    public Result<String> queryPayResult(String orderId) {
        String payResult = redisUtil.getString(BaseConstant.ORDER_PAY_RESULT + orderId);
        log.info("--->>> get payResult from redis, orderId:{}, payResult:{}", orderId, payResult);
        payResult = payResult == null ? "2" : payResult;
        // 判断支付状态，如果支付状态是1，支付成功，再查下licence是否生成成功。
        if ("1".equals(payResult)) {
            // 查下licence是否成功
            LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.eq(TbLenBindRelationEntity::getOrderId, orderId);
            List<TbLenBindRelationEntity> tbLenBindRelationEntities = lenBindRelationMapper.selectList(queryWrapper);
            if (!CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
                payResult = "3";
            }
        }
        return Result.ofSucceed(payResult);
    }

    @Override
    public Result<ClientCommodityDetailRespVo> queryGoodsDetail(ClientCommodityDetailReqVo clientCommodityDetailReqVo) {
        Long tenantId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        SellerDetailVO merchantByIdSupportFallback = pileUserServiceAdapter.findMerchantById(tenantId);
        if (merchantByIdSupportFallback == null || StringUtils.isBlank(merchantByIdSupportFallback.getCountryAbbreviation())) {
            throw new MessageCodeException(PileBaseEnum.SELLER_COUNTRY_ABBREVIATION_IS_NULL);
        }
        clientCommodityDetailReqVo.setTenantId(String.valueOf(tenantId));
        clientCommodityDetailReqVo.setAlpha2Code(merchantByIdSupportFallback.getCountryAbbreviation());
        clientCommodityDetailReqVo.setZipCode(merchantByIdSupportFallback.getZipCode());
        log.info("--->>> queryGoodsDetail request param: {}", JSON.toJSONString(clientCommodityDetailReqVo));
        Result<ClientCommodityDetailRespVo> clientCommodityDetailRespVoResult = subscribeGoodsFeignClient.queryGoodsDetail(clientCommodityDetailReqVo);
        log.info("--->>> queryGoodsDetail, call commodity-center returns: {}", JSON.toJSONString(clientCommodityDetailRespVoResult));
        return clientCommodityDetailRespVoResult;
    }

    @Override
    public Result<Boolean> orderTransactionCallback(CallBackEventNoticeReqVo callBackEventNoticeReqVo) {
        log.info("--->>> orderTransactionCallback receive from order-center, request param:{}", JSON.toJSONString(callBackEventNoticeReqVo));
        String event = callBackEventNoticeReqVo.getEvent();
        LinkedHashMap<String, Object> mapData = (LinkedHashMap) callBackEventNoticeReqVo.getData();
        if (BaseConstant.CALLBACK_EVENT_PAY.equals(event)) {
            PlaceOrderNotifyReqVo orderNotifyReqVo = BeanUtil.mapToBean(mapData, PlaceOrderNotifyReqVo.class, Boolean.TRUE, new CopyOptions());
            log.info("--->>> set pay result redis cache, orderNotifyReqVo:{}", orderNotifyReqVo);
            redisUtil.set(BaseConstant.ORDER_PAY_RESULT + orderNotifyReqVo.getOrderId(), orderNotifyReqVo.getPayStatus(), 3600);
            log.info("--->>> get redis pay result: " + redisUtil.getString(BaseConstant.ORDER_PAY_RESULT + orderNotifyReqVo.getOrderId()));

            //更新订单表needPay字段
            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new QueryWrapper<TbOrderRecordEntity>().lambda()
                    .eq(TbOrderRecordEntity::getOrderId, orderNotifyReqVo.getOrderId());
            /*int needPay = Boolean.TRUE.equals(orderNotifyReqVo.getPayRequired()) ? 1 : 0;
            tbOrderRecordMapper.update(new TbOrderRecordEntity(needPay), queryWrapper);*/

            //发送支付成功通知邮件
            List<TbOrderRecordEntity> tbOrderRecordEntityList = tbOrderRecordMapper.selectList(queryWrapper);
            if (CollectionUtil.isNotEmpty(tbOrderRecordEntityList)
                    && !OrderSourceIdEnum.ORDER_SOURCE_SAP.getKey().equals(tbOrderRecordEntityList.get(0).getSourceId())) {
                // 调用发送邮件方法，crm与云平台订单
                AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> sendSubscribedInvoice(orderNotifyReqVo.getOrderId(), tbOrderRecordEntityList.get(0).getTenantId())));
            }
        } else if (BaseConstant.CALLBACK_EVENT_DELIVERY.equals(event)) {
            LicenseDeliveryNotifyReqVo licenseDelivery = BeanUtil.mapToBean(mapData, LicenseDeliveryNotifyReqVo.class, Boolean.TRUE, new CopyOptions());
            if (StringUtils.isBlank(licenseDelivery.getOrderId()) || CollectionUtils.isEmpty(licenseDelivery.getPerformanceIdList())) {
                throw new IllegalArgumentException("param error");
            }
            // 订单真实性校验
            orderExistsCheck(licenseDelivery);

            // 订单是否已经生成履约单，如果订单id已经存在，直接返回。
            Boolean exist = callBackIdempotentCheck(licenseDelivery);
            if (exist) {
                return Result.ofSucceed(Boolean.TRUE);
            }

            // 插入履约表；获取licence，并且完成绑定，通知订单模块履约完成，发送mq桩绑定，通过定时任务完成。
            List<TbHonourAgreementEntity> honourAgreementEntityList = honourAgreementBuild(licenseDelivery);
            honourAgreementEntityList.forEach(item -> honourAgreementMapper.insert(item));

            /*
             * V1.1 优化，改成通过线程池异步处理。
             *
             * */
            //getAndBindLicence();
            log.info("--->>> orderTransactionCallback  getAndBindLicence execute.");
            getAndBindLicence(honourAgreementEntityList.get(0));
            //AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> getAndBindLicence(honourAgreementEntityList.get(0))));
           /* AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> {
                try {
                    getAndBindLicence(honourAgreementEntityList.get(0));
                }  catch (Exception e) {
                    log.error("orderTransactionCallback, call getAndBindLicence error.", e);
                }
            }));*/

            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> sendSubscriptionProductArrival(licenseDelivery.getOrderId())));

        } else if (BaseConstant.CALLBACK_EVENT_RETURN.equals(event)) {
            ReturnResultNotifyReqVo returnResultNotifyReqVo = BeanUtil.mapToBean(mapData, ReturnResultNotifyReqVo.class, Boolean.TRUE, new CopyOptions());
            if (returnResultNotifyReqVo.getReturnStatus() == 1) {
                licenseManagementService.returnResultSuccessHandle(returnResultNotifyReqVo.getOrderId(),
                        returnResultNotifyReqVo.getReturnId(), returnResultNotifyReqVo.getReturnStatus());
            } else {
                log.warn("return status:{}", returnResultNotifyReqVo.getReturnStatus());
            }
        } else {
            // unknown event
            log.info("--->>> orderTransactionCallback unknown call event");
            return Result.ofSucceed(Boolean.FALSE);
        }
        return Result.ofSucceed(Boolean.TRUE);
    }

    public void sendSubscriptionProductArrival(String orderId) {
        try {
            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
            final TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
            String language = getSellerLanguage(orderId, tbOrderRecordEntity.getTenantId());
            emailSendingService.sendSubscriptionProductArrival(orderId, language, tbOrderRecordEntity.getTenantId());
        } catch (Exception e) {
            log.error("--->>> send Subscription Product Arrival email error.", e);
        }
    }

    private Boolean callBackIdempotentCheck(LicenseDeliveryNotifyReqVo licenseDelivery) {
        LambdaQueryWrapper<TbHonourAgreementEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbHonourAgreementEntity::getOrderId, licenseDelivery.getOrderId());
        List<TbHonourAgreementEntity> tbHonourAgreementEntities = honourAgreementMapper.selectList(queryWrapper);
        if (!CollectionUtils.isEmpty(tbHonourAgreementEntities)) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    private void orderExistsCheck(LicenseDeliveryNotifyReqVo licenseDelivery) {
        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbOrderRecordEntity::getOrderId, licenseDelivery.getOrderId());
        queryWrapper.or().eq(TbOrderRecordEntity::getSerialId, licenseDelivery.getBusinessNumber());
        List<TbOrderRecordEntity> tbOrderRecordEntities = tbOrderRecordMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbOrderRecordEntities)) {
            log.info("--->>> orderTransactionCallback, orderId:{} not exists", licenseDelivery.getOrderId());
            throw new IllegalArgumentException("orderId not exists.");
        }
        licenseDelivery.setTenant(tbOrderRecordEntities.get(0).getTenantId());
    }

    public void sendSubscribedInvoice(String orderId, String tenantId) {
        try {
            String language = getSellerLanguage(orderId, tenantId);
            sendSubscribedInvoice.sendSubscribedInvoice(orderId, language);
        } catch (Exception e) {
            log.error("--->>> send subscribe email error.", e);
        }
    }

    @Override
    @Transactional
    public void getAndBindLicence(TbHonourAgreementEntity tbHonourAgreementEntity) {
        // 根据orderId，查询order-center， get licence。
        Result<List<LicenseInfo>> licence = orderCenterFeignClient.getLicence(tbHonourAgreementEntity.getOrderId(), 1L);
        List<LicenseInfo> licenseInfoList = licence.getData();

        //List<LicenseInfo> licenseInfoList = JSON.parseArray("[{\"commodityCode\":\"cdyp003\",\"commodityName\":\"Pro\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cdyp003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Pro\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp02\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_Pro\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"isDuration\":false,\"specificationCode\":\"chargingGunType\",\"specificationName\":\"Connector Type\",\"specificationValue\":\"AC\"},{\"duration\":24,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"24 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"GUN\"}],\"createTime\":1715241725183,\"licenseCode\":\"7GyWr0mgaLuFohYe\",\"orderCommodityId\":1788479474699145218,\"orderId\":\"1788479474699145217\",\"performanceId\":\"1788479474699145217a\",\"rightId\":\"Benefits_Pro\",\"sku\":\"cdyp003ac200001\"},{\"commodityCode\":\"cdyp003\",\"commodityName\":\"Pro\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cdyp003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Pro\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp02\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_Pro\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"isDuration\":false,\"specificationCode\":\"chargingGunType\",\"specificationName\":\"Connector Type\",\"specificationValue\":\"AC\"},{\"duration\":24,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"24 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"GUN\"}],\"createTime\":1715241725185,\"licenseCode\":\"ldLtVTvVsUgUGwZf\",\"orderCommodityId\":1788479474699145218,\"orderId\":\"1788479474699145217\",\"performanceId\":\"1788479474699145217a\",\"rightId\":\"Benefits_Pro\",\"sku\":\"cdyp003ac200001\"},{\"commodityCode\":\"cdyp003\",\"commodityName\":\"Pro\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cdyp003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Pro\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp02\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_Pro\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"isDuration\":false,\"specificationCode\":\"chargingGunType\",\"specificationName\":\"Connector Type\",\"specificationValue\":\"AC\"},{\"duration\":24,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"24 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"GUN\"}],\"createTime\":1715241725186,\"licenseCode\":\"zPlyxZt4BmoAqXcM\",\"orderCommodityId\":1788479474699145218,\"orderId\":\"1788479474699145217\",\"performanceId\":\"1788479474699145217a\",\"rightId\":\"Benefits_Pro\",\"sku\":\"cdyp003ac200001\"},{\"commodityCode\":\"cdyp003\",\"commodityName\":\"Pro\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cdyp003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Pro\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp02\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_Pro\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"isDuration\":false,\"specificationCode\":\"chargingGunType\",\"specificationName\":\"Connector Type\",\"specificationValue\":\"AC\"},{\"duration\":24,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"24 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"GUN\"}],\"createTime\":1715241725188,\"licenseCode\":\"T0YuWQeJ4RQzd6fN\",\"orderCommodityId\":1788479474699145218,\"orderId\":\"1788479474699145217\",\"performanceId\":\"1788479474699145217a\",\"rightId\":\"Benefits_Pro\",\"sku\":\"cdyp003ac200001\"},{\"commodityCode\":\"cdyp003\",\"commodityName\":\"Pro\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cdyp003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Pro\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp02\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_Pro\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"isDuration\":false,\"specificationCode\":\"chargingGunType\",\"specificationName\":\"Connector Type\",\"specificationValue\":\"DC\"},{\"duration\":24,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"24 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"GUN\"}],\"createTime\":1715241725190,\"licenseCode\":\"vr5idxgIPaK1jRPX\",\"orderCommodityId\":1788479474703339525,\"orderId\":\"1788479474699145217\",\"performanceId\":\"1788479474699145217b\",\"rightId\":\"Benefits_Pro\",\"sku\":\"cdyp003dc200001\"}]", LicenseInfo.class);
        //List<LicenseInfo> licenseInfoList = JSON.parseArray("[{\"commodityCode\":\"mp003\",\"commodityName\":\"Maintenance\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"mp003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Maintenance\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp04\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_ops\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"duration\":36,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"36 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"chargingPileType\",\"specificationName\":\"Charger Type\",\"specificationValue\":\"AC\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"PILE\"}],\"createTime\":1716014855531,\"licenseCode\":\"08JSyRIwsf09elXN\",\"orderCommodityId\":1791722225353097218,\"orderId\":\"1791722225353097217\",\"performanceId\":\"1791722225353097217a\",\"rightId\":\"Benefits_ops\",\"sku\":\"mp003ac300001\"}]", LicenseInfo.class);
        //List<LicenseInfo> licenseInfoList = JSON.parseArray("[{\"commodityCode\":\"cms003\",\"commodityName\":\"Advertising\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cms003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Advertising\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp03\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_ads\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"duration\":36,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"36 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"PILE\"}],\"createTime\":1716019240556,\"licenseCode\":\"0xzhFzGxbCP4O9sa\",\"orderCommodityId\":1791740614658228226,\"orderId\":\"1791740614658228225\",\"performanceId\":\"1791740614658228225a\",\"rightId\":\"Benefits_ads\",\"sku\":\"cms003300001\"}]", LicenseInfo.class);
        //List<LicenseInfo> licenseInfoList = JSON.parseArray("[{\"commodityCode\":\"cms003\",\"commodityName\":\"Advertising\",\"commoditySpecificationList\":[{\"isDuration\":false,\"specificationCode\":\"commodityCode\",\"specificationName\":\"商品ID\",\"specificationValue\":\"cms003\"},{\"isDuration\":false,\"specificationCode\":\"commodityName\",\"specificationName\":\"商品名称\",\"specificationValue\":\"Advertising\"},{\"isDuration\":false,\"specificationCode\":\"commodityDetail\",\"specificationName\":\"商品详情\",\"specificationValue\":\"0\"},{\"isDuration\":false,\"specificationCode\":\"applyApp\",\"specificationName\":\"适用应用\",\"specificationValue\":\"Chargebusi,crmsystem1,Chargefirm\"},{\"isDuration\":false,\"specificationCode\":\"permissionProduct\",\"specificationName\":\"权限产品\",\"specificationValue\":\"pp03\"},{\"isDuration\":false,\"specificationCode\":\"performanceApp\",\"specificationName\":\"履约应用\",\"specificationValue\":\"Chargebusi,crmsystem1\"},{\"isDuration\":false,\"specificationCode\":\"serviceInterest\",\"specificationName\":\"服务权益\",\"specificationValue\":\"Benefits_ads\"},{\"isDuration\":false,\"specificationCode\":\"applyArea\",\"specificationName\":\"适用地区\",\"specificationValue\":\"CHARGING_CLOUD_ASIC_PACIFIC_REGION\"},{\"isDuration\":false,\"specificationCode\":\"currency\",\"specificationName\":\"币种\",\"specificationValue\":\"USD\"},{\"isDuration\":false,\"specificationCode\":\"taxesRates\",\"specificationName\":\"税费及税率\",\"specificationValue\":\"CHARGE_CLOUD_TAXES\"},{\"duration\":36,\"isDuration\":true,\"specificationCode\":\"subscriptionCycle\",\"specificationName\":\"Subscription Period\",\"specificationValue\":\"36 MONTH\",\"unit\":\"MONTH\"},{\"isDuration\":false,\"specificationCode\":\"unitOfQuantityCode\",\"specificationName\":\"数量单位\",\"specificationValue\":\"PILE\"}],\"createTime\":1716347676274,\"licenseCode\":\"jmStFJTh99aUME2r\",\"orderCommodityId\":1793118169244106755,\"orderId\":\"1793118169244106754\",\"performanceId\":\"1793118169244106754a\",\"rightId\":\"Benefits_ads\",\"sku\":\"cms003300001\"}]", LicenseInfo.class);
        if (CollectionUtils.isEmpty(licenseInfoList)) {
            log.info("--->>> getAndBindLicence, get licence from order-center returns empty licences, orderId:{}", tbHonourAgreementEntity.getOrderId());
            return;
        }

        log.info("--->>> getAndBindLicence return:{}", JSON.toJSONString(licenseInfoList));

        List<LicenseInfo> originLicenseList = new ArrayList<>(licenseInfoList);

        // 获取licence，并且完成绑定，通知订单模块履约完成，发送mq桩绑定
        // 查询订单下的桩，并且如果是枪维度的licence，需要查询每个桩几把枪。
        // 同一个order id 下面的licence 创建时间一致
        Long createTime = System.currentTimeMillis();

        LambdaQueryWrapper<TbOrderPileEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbOrderPileEntity::getOrderId, tbHonourAgreementEntity.getOrderId());
        List<TbOrderPileEntity> tbOrderPileEntities = orderPileMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbOrderPileEntities)) {
            // crm & 云平台无设备下单，直接保存licence即可。
            log.info("getAndBindLicence, there is no pile selected.");
            licenseInfoList.forEach(licenseInfo -> buildAndInsertLicence(licenseInfo, null, tbHonourAgreementEntity, createTime));
        } else {
            // 优化绑定逻辑
            List<CommoditySpecification> collect = licenseInfoList.get(0).getCommoditySpecificationList().stream().filter(item -> "unitOfQuantityCode".equalsIgnoreCase(item.getSpecificationCode())).collect(Collectors.toList());
            String unitOfQuantityCode = collect.get(0).getSpecificationValue();

            boolean gunFlag = UnitOfQuantityCodeEnum.GUN.getCode().equalsIgnoreCase(unitOfQuantityCode);

            boolean noPowerType = false;
            Map<String, List<LicenseInfo>> groupedLicenseInfo = null;

            List<CommoditySpecification> chargingPileTypeSpecList = licenseInfoList.stream()
                    .flatMap(licenseInfo -> licenseInfo.getCommoditySpecificationList().stream())
                    .filter(spec -> "chargingPileType".equalsIgnoreCase(spec.getSpecificationCode()) || "chargingGunType".equalsIgnoreCase(spec.getSpecificationCode()))
                    .collect(Collectors.toList());

            if (CollUtil.isEmpty(chargingPileTypeSpecList)) {
                noPowerType = true;
            } else {
                groupedLicenseInfo = licenseInfoList.stream()
                        .collect(Collectors.groupingBy(
                                licenseInfo -> licenseInfo.getCommoditySpecificationList().stream()
                                        .filter(spec -> "chargingGunType".equalsIgnoreCase(spec.getSpecificationCode()) || "chargingPileType".equalsIgnoreCase(spec.getSpecificationCode()))
                                        .map(CommoditySpecification::getSpecificationValue)
                                        .findFirst()
                                        .orElse(null)
                        ));
            }

        /*    // 对licenseInfoList进行分组
            Map<String, List<LicenseInfo>> groupedLicenseInfo = licenseInfoList.stream()
                    .collect(Collectors.groupingBy(
                            licenseInfo -> licenseInfo.getCommoditySpecificationList().stream()
                                    .filter(spec -> "chargingGunType".equalsIgnoreCase(spec.getSpecificationCode()) || "chargingPileType".equalsIgnoreCase(spec.getSpecificationCode()))
                                    .map(CommoditySpecification::getSpecificationValue)
                                    .findFirst()
                                    .orElse(null)
                    ));*/

            boolean finalNoPowerType = noPowerType;
            Map<String, List<LicenseInfo>> finalGroupedLicenseInfo = groupedLicenseInfo;
            tbOrderPileEntities.forEach(tbOrderPileEntity -> {
                ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationService.queryBySN(tbOrderPileEntity.getPileSn(), Long.parseLong(tbHonourAgreementEntity.getTenantId()));
                ChargePointMerchantRelationEntity chargePointEntityHost = null;
                ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity = null;
                // 增加null校验。
                if (chargePointEntity == null) {
                    log.info("--->>> getAndBindLicence, chargePointEntity queryBySN return null.");
                    // 可能是超充终端
                    chargePointMerchantTerminalEntity = chargePointMerchantTerminalService.queryBySN(tbOrderPileEntity.getPileSn(), Long.parseLong(tbHonourAgreementEntity.getTenantId()));
                    if (chargePointMerchantTerminalEntity == null) {
                        log.info("--->>> getAndBindLicence, chargePointMerchantTerminalEntity queryBySN return null.");
                        return;
                    }
                    chargePointEntityHost = chargePointMerchantRelationService.queryBySN(chargePointMerchantTerminalEntity.getHostSn(), Long.parseLong(tbHonourAgreementEntity.getTenantId()));
                    if (chargePointEntityHost == null) {
                        log.info("--->>> getAndBindLicence, chargePointEntityHost queryBySN return null.");
                        return;
                    }
                }

                if (finalNoPowerType) {
                    LicenseInfo licenseInfo = licenseInfoList.get(0);
                    if (licenseInfo != null) {
                        buildAndInsertLicence(licenseInfo, tbOrderPileEntity, tbHonourAgreementEntity, createTime);
                        // 每次取一个，移除一个
                        licenseInfoList.remove(0);
                    }
                } else {
                    // ac/dc/acdc
                    String type = chargePointEntity != null ? chargePointEntity.getPowerType() : chargePointEntityHost.getPowerType();
                    if (gunFlag) {
                        List<Connector> connectors = chargePointEntity != null ? chargePointEntity.getConnectors() : chargePointMerchantTerminalEntity.getConnectorsList();
                        log.info("--->>> getAndBindLicence, queryBySN, sn:{}, count:{}", tbOrderPileEntity.getPileSn(), CollectionUtil.size(connectors));
                        int pileCount = connectors.size();
                        for (int i=0; i<pileCount; i++) {
                            LicenseInfo licenseInfo = finalGroupedLicenseInfo.get(type).get(0);
                            if (licenseInfo != null) {
                                buildAndInsertLicence(licenseInfo, tbOrderPileEntity, tbHonourAgreementEntity, createTime);
                                // 每次取一个，移除一个
                                finalGroupedLicenseInfo.get(type).remove(0);
                            }
                        }
                    } else {
                        LicenseInfo licenseInfo = finalGroupedLicenseInfo.get(type).get(0);
                        if (licenseInfo != null) {
                            buildAndInsertLicence(licenseInfo, tbOrderPileEntity, tbHonourAgreementEntity, createTime);
                            // 每次取一个，移除一个
                            finalGroupedLicenseInfo.get(type).remove(0);
                        }
                    }
                }
            });

            if (finalGroupedLicenseInfo != null ) {
                groupedLicenseInfo.forEach((type, licenseListRemain) -> {
                    if (CollUtil.isNotEmpty(licenseListRemain)) {
                        log.info("--->>> There is still having remaining licences after binding piles.");
                        licenseListRemain.forEach(licenseInfo -> buildAndInsertLicence(licenseInfo, null, tbHonourAgreementEntity, createTime));
                    }
                });
            }

            if (noPowerType && CollUtil.isNotEmpty(licenseInfoList)) {
                log.info("--->>> noPowerType, There is still having remaining licences after binding piles.");
                licenseInfoList.forEach(licenseInfo -> buildAndInsertLicence(licenseInfo, null, tbHonourAgreementEntity, createTime));
            }

            log.info("getAndBindLicence, have finish insert licence, orderId: {}", tbHonourAgreementEntity.getOrderId());
            // 发送绑定事件通知 ES
            // 判断当前的权益id，是否含有 004功能点，有004 app才能搜索。
            try {
                List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.GUN_SEARCH);
                if (CollectionUtil.isNotEmpty(functionBenefitList) && functionBenefitList.contains(originLicenseList.get(0).getRightId())) {
                    UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO = new UpdateEsSubscriptionStatusDTO();
                    List<String> pileList = tbOrderPileEntities.stream().map(TbOrderPileEntity::getPileSn).collect(Collectors.toList());
                    updateEsSubscriptionStatusDTO.setPileSnList(pileList);
                    updateEsSubscriptionStatusDTO.setStatus(Boolean.TRUE);
                    opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);
                }
            } catch (Exception e) {
                log.error("getAndBindLicence, notice ES error, orderId: {}", tbHonourAgreementEntity.getOrderId(), e);
            }
            log.info("getAndBindLicence, call es update end, orderId: {}", tbHonourAgreementEntity.getOrderId());
        }

        // 调用发送邮件方法，crm与云平台订单
/*        try {
            String language = getSellerLanguage(tbHonourAgreementEntity);
            sendSubscribedInvoice.sendSubscribedInvoice(tbHonourAgreementEntity.getOrderId(), language);
        } catch (Exception e) {
            log.error("--->>> send subscribe email error.", e);
        }*/

        // licence生成后，更新本地履约单状态
        updateTbHonourAgreementStatus(tbHonourAgreementEntity);

        // 通知订单模块履约完成
        UpdatePerformanceStatusReqVo updatePerformanceStatusReqVo = new UpdatePerformanceStatusReqVo();
        updatePerformanceStatusReqVo.setOrderId(tbHonourAgreementEntity.getOrderId());
        updatePerformanceStatusReqVo.setOperatorId(1L);
        orderCenterFeignClient.updateOrderStatus(updatePerformanceStatusReqVo);

        log.info("getAndBindLicence, notice order-center finish, orderId: {}", tbHonourAgreementEntity.getOrderId());

        // 该订单通知给crm让其在sf下单（crm中会筛选出非sf来源的、非试用的订单在sf下单）
        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper2 = new QueryWrapper<TbOrderRecordEntity>().lambda()
                .eq(TbOrderRecordEntity::getOrderId, tbHonourAgreementEntity.getOrderId());
        List<TbOrderRecordEntity> tbOrderRecordEntities = tbOrderRecordMapper.selectList(queryWrapper2);

        // 通知crm
        if (CollUtil.isNotEmpty(tbOrderRecordEntities)) {
            TbOrderRecordEntity recordEntity = tbOrderRecordEntities.get(0);
            if (StringUtils.isNotBlank(recordEntity.getOuterOrderNo())) {
                // 调用crm 接口
                RfdNoticeDto dto = new RfdNoticeDto();
                dto.setAccount(recordEntity.getOuterOrderNo());
                dto.setMerchantId(tbHonourAgreementEntity.getTenantId());
                dto.setLicenseNumber(licenseInfoList.get(0).getLicenseCode());

                crmServiceAdapter.extractCRM(dto);
                log.info("rfd card notice crm success.");
            }
        }

        if (CollUtil.isNotEmpty(tbOrderRecordEntities)
                && Objects.nonNull(tbOrderRecordEntities.get(0).getNeedPay())
                && tbOrderRecordEntities.get(0).getNeedPay() == 1) {
            OrderTypeNoticeDTO orderTypeNoticeDTO = new OrderTypeNoticeDTO(BaseConstant.CALLBACK_EVENT_DELIVERY
                    , tbHonourAgreementEntity.getOrderId());
            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> crmServiceAdapter.orderTypeNotice(orderTypeNoticeDTO)));
        }
    }

    /**
     * description: getSellerLanguage
     * 获取商户下单时的语言，没有根据商户国家选择，英文兜底。
     * version: 1.0
     * date: 2023/7/27 9:38
     * author: A23204
     *
     * @param tbHonourAgreementEntity
     * @return java.lang.String
     */
    private String getSellerLanguage(TbHonourAgreementEntity tbHonourAgreementEntity) {
        String language;
        try {
            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.eq(TbOrderRecordEntity::getOrderId, tbHonourAgreementEntity.getOrderId());
            List<TbOrderRecordEntity> tbOrderRecordEntities = tbOrderRecordMapper.selectList(queryWrapper);
            language = tbOrderRecordEntities.get(0).getAcceptLanguage();
            if (language == null) {
                SellerDetailVO merchantByIdSupportFallback = pileUserServiceAdapter.findMerchantById(Long.valueOf(tbHonourAgreementEntity.getTenantId()));
                Locale locale = CommonUtil.language(merchantByIdSupportFallback.getCountryAbbreviation(), langs);
                language = locale.toLanguageTag();
            }
        } catch (Exception ex) {
            log.error("--->>> getSellerLanguage error, use default language:[en-US].", ex);
            language = "en-US";
        }
        return language;
    }

    private String getSellerLanguage(String orderId, String tenantId) {
        String language;
        try {
            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
            List<TbOrderRecordEntity> tbOrderRecordEntities = tbOrderRecordMapper.selectList(queryWrapper);
            language = tbOrderRecordEntities.get(0).getAcceptLanguage();
            if (language == null) {
                SellerDetailVO merchantByIdSupportFallback = pileUserServiceAdapter.findMerchantById(Long.valueOf(tenantId));
                Locale locale = CommonUtil.language(merchantByIdSupportFallback.getCountryAbbreviation(), langs);
                language = locale.toLanguageTag();
            }
        } catch (Exception ex) {
            log.error("--->>> getSellerLanguage error, use default language:[en-US].", ex);
            language = "en-US";
        }
        return language;
    }

    private void updateTbHonourAgreementStatus(TbHonourAgreementEntity tbHonourAgreementEntity) {
        LambdaUpdateWrapper<TbHonourAgreementEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.eq(TbHonourAgreementEntity::getOrderId, tbHonourAgreementEntity.getOrderId());

        TbHonourAgreementEntity entity = new TbHonourAgreementEntity();
        entity.setAgreementStatus(1);
        entity.setUpdateTime(System.currentTimeMillis());

        honourAgreementMapper.update(entity, updateWrapper);
    }

    /**
     * description: buildAndInsertLicence
     * 构建 licence 并保存
     * version: 1.0
     * date: 2023/8/2 10:22
     * author: A23204
     *
     * @param licenseInfo
     * @param pileEntity
     * @param honourAgreementEntity
     * @param createTime
     * @return void
     */
    private void buildAndInsertLicence(LicenseInfo licenseInfo, TbOrderPileEntity pileEntity, TbHonourAgreementEntity honourAgreementEntity, Long createTime) {
        //先判断，这个桩，同一个权益id（qy001、qy002,算同一个权益），做续期。 如果当前是 qy001，qy002，判断是否已经存

        // beta期间兼容，将旧的转换成新的服务权益id。
        if (licenseInfo.getSku().startsWith("cdyp")) {
            licenseInfo.setRightId("Benefits_Pro");
        } else if (licenseInfo.getSku().startsWith("cdyl")) {
            licenseInfo.setRightId("Benefits_Lite");
        } else if (licenseInfo.getSku().startsWith("mp")) {
            licenseInfo.setRightId("Benefits_ops");
        } else if (licenseInfo.getSku().startsWith("cms")) {
            licenseInfo.setRightId("Benefits_ads");
        }

        /*
        update tb_len_bind_relation set service_id = 'Benefits_Pro' where goods_id like 'cdyp%';
        update tb_len_bind_relation set service_id = 'Benefits_Lite' where goods_id like 'cdyl%';
        update tb_len_bind_relation set service_id = 'Benefits_ops' where goods_id like 'mp%';
        update tb_len_bind_relation set service_id = 'Benefits_ads' where goods_id like 'cms%';
         */

        log.info("buildAndInsertLicence start.");
        TbLenBindRelationEntity bindRelationEntity = new TbLenBindRelationEntity();
        bindRelationEntity.setTenantId(honourAgreementEntity.getTenantId());
        bindRelationEntity.setOrderId(licenseInfo.getOrderId());
        bindRelationEntity.setGoodsId(licenseInfo.getCommodityCode());
        bindRelationEntity.setGoodsName(licenseInfo.getCommodityName());
        bindRelationEntity.setAgreementId(licenseInfo.getPerformanceId());
        bindRelationEntity.setServiceId(licenseInfo.getRightId());
        bindRelationEntity.setLicenceCode(licenseInfo.getLicenseCode());
        bindRelationEntity.setPileSn(pileEntity != null ? pileEntity.getPileSn() : "");
        bindRelationEntity.setCreateTime(createTime);
        bindRelationEntity.setUpdateTime(createTime);
        bindRelationEntity.setSkuCode(licenseInfo.getSku());
        bindRelationEntity.setBonusDurationValue(licenseInfo.getBonusDurationValue());
        if (pileEntity != null) {
            bindRelationEntity.setBindTime(createTime);
        }

        // 套餐时常等属性；通过空格分隔，硬编码。
        List<CommoditySpecification> collect = licenseInfo.getCommoditySpecificationList().stream().filter(attribute -> BaseConstant.SUBSCRIPTION_CYCLE.equals(attribute.getSpecificationCode())).collect(Collectors.toList());
        CommoditySpecification commoditySpecification = collect.get(0);
        String[] s = commoditySpecification.getSpecificationValue().split(" ");

        /*
         * V1.1 优化，服务时长拆分成2个字段。
         * */
        if (commoditySpecification.getUnit() != null && commoditySpecification.getDuration() != null) {
            bindRelationEntity.setServiceTime(commoditySpecification.getDuration());
            bindRelationEntity.setTimeUnit(commoditySpecification.getUnit());
        } else {
            bindRelationEntity.setServiceTime(Integer.valueOf(s[0]));
            bindRelationEntity.setTimeUnit(s[1]);
        }


        /*
         * 说明
         * 1、权益范围不止一个的按字母顺序升序 例如 biz:ops_trial biz:ops_official
         * 2、 续期规则
         *       权益范围[:权益范围:权益范围] 不一样的 立即生效 即生效时间就是当前时间
         *       权益范围[:权益范围:权益范围] 一样的 找到最后失效时间作为 当前许可证的生效时间
         *
         * */

        if (pileEntity != null) {
            String rightIdLeft = licenseInfo.getRightId();

            LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.eq(TbLenBindRelationEntity::getTenantId, honourAgreementEntity.getTenantId());
            queryWrapper.eq(TbLenBindRelationEntity::getPileSn, pileEntity.getPileSn());
            queryWrapper.ne(TbLenBindRelationEntity::getOrderId, licenseInfo.getOrderId());
            // 未过期的。
            queryWrapper.ne(TbLenBindRelationEntity::getStatus, 2);
            queryWrapper.eq(TbLenBindRelationEntity::getServiceId, rightIdLeft);
            queryWrapper.gt(TbLenBindRelationEntity::getAvailableTime, 0L);
            queryWrapper.orderByDesc(TbLenBindRelationEntity::getUnavailableTime);
            queryWrapper.last("limit 1");
            // 如果 bindRelationEntityTemp ！=null 续期， == null， 立即激活。
            TbLenBindRelationEntity bindRelationEntityTemp = lenBindRelationMapper.selectOne(queryWrapper);

            createTime = bindRelationEntityTemp == null ? createTime : bindRelationEntityTemp.getUnavailableTime();
            Timestamp timestampCurr = new Timestamp(createTime);
            Calendar c = Calendar.getInstance();
            c.setTime(timestampCurr);
            //int unit = s[1].startsWith("year") ? Calendar.YEAR : Calendar.MARCH;
            //int unit = s[1].startsWith("year") ? Calendar.YEAR : s[1].startsWith("month") ? Calendar.MARCH : Calendar.DAY_OF_MONTH;

            /*
             * V1.1 优化
             * */
            int unit = bindRelationEntity.getTimeUnit().toLowerCase().startsWith("year") ? Calendar.YEAR : bindRelationEntity.getTimeUnit().toLowerCase().startsWith("month") ? Calendar.MONTH : Calendar.DAY_OF_YEAR;
            c.add(unit, bindRelationEntity.getServiceTime());
            // 统一切齐到 23:59:59:999
            c.set(Calendar.MILLISECOND, 999);
            c.set(Calendar.SECOND, 59); //这是将当天的【秒】设置为59
            c.set(Calendar.MINUTE, 59); //这是将当天的【分】设置为59
            c.set(Calendar.HOUR_OF_DAY, 23); //这是将当天的【时】设置为23

            bindRelationEntity.setStatus(1);
            if (bindRelationEntityTemp == null) {
                bindRelationEntity.setAvailableTime(createTime);
                bindRelationEntity.setUnavailableTime(c.getTimeInMillis());
            } else {
                bindRelationEntity.setAvailableTime(bindRelationEntityTemp.getUnavailableTime());
                bindRelationEntity.setUnavailableTime(c.getTimeInMillis());
            }
        } else {
            bindRelationEntity.setStatus(0);
        }

        // V1.1  set measureUnit 需要优化，待处理。
        List<CommoditySpecification> measureUnitList = licenseInfo.getCommoditySpecificationList().stream().filter(attribute -> BaseConstant.UNIT_OF_QUANTITY_CODE.equals(attribute.getSpecificationCode())).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(measureUnitList)) {
            log.info("--->>> orderId:{},get licenseInfo return empty unitOfQuantityCode.", licenseInfo.getOrderId());
            throw new RuntimeException("unitOfQuantityCode is empty");
        }

        if (BaseConstant.GOODS_TYPE_GUN.equalsIgnoreCase(measureUnitList.get(0).getSpecificationValue())) {
            bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_GUN);
        } else if (BaseConstant.GOODS_TYPE_PILE.equalsIgnoreCase(measureUnitList.get(0).getSpecificationValue())) {
            bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_PILE);
        }

        List<CommoditySpecification> goodsType = licenseInfo.getCommoditySpecificationList().stream().filter(attribute -> BaseConstant.CHARGING_PILE_TYPE.equals(attribute.getSpecificationCode())).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(goodsType)) {
            bindRelationEntity.setChargeType(goodsType.get(0).getSpecificationValue());
            //bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_PILE);
        } else {
            List<CommoditySpecification> goodsGunType = licenseInfo.getCommoditySpecificationList().stream().filter(attribute -> BaseConstant.CHARGING_GUN_TYPE.equals(attribute.getSpecificationCode())).collect(Collectors.toList());
            if (!CollectionUtils.isEmpty(goodsGunType)) {
                bindRelationEntity.setChargeType(goodsGunType.get(0).getSpecificationValue());
                //bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_GUN);
            }
        }

        // 处理默认值
        if (bindRelationEntity.getChargeType() == null) {
            bindRelationEntity.setChargeType("NULL");
        }
        if (bindRelationEntity.getMeasureUnit() == null) {
            bindRelationEntity.setMeasureUnit(BaseConstant.GOODS_TYPE_PILE);
        }

        // save
        int effectRows = lenBindRelationMapper.insert(bindRelationEntity);
        log.info("buildAndInsertLicence, licenceCode:{} insert effect rows:{}", licenseInfo.getLicenseCode(), effectRows);
    }

    /**
     * description: honourAgreementBuild
     * 构造履约对象
     * version: 1.0
     * date: 2023/8/2 10:23
     * author: A23204
     *
     * @param licenseDelivery
     * @return java.util.List<com.autel.cloud.pile.base.infrastructure.mapper.entity.TbHonourAgreementEntity>
     */
    private List<TbHonourAgreementEntity> honourAgreementBuild(LicenseDeliveryNotifyReqVo licenseDelivery) {
        List<TbHonourAgreementEntity> honourAgreementEntityList = new ArrayList<>();
        Long createTime = System.currentTimeMillis();
        licenseDelivery.getPerformanceIdList().forEach(performanceId -> {
            TbHonourAgreementEntity honourAgreementEntity = new TbHonourAgreementEntity();
            honourAgreementEntity.setOrderId(licenseDelivery.getOrderId());
            honourAgreementEntity.setTenantId(licenseDelivery.getTenant());
            honourAgreementEntity.setAgreementId(performanceId);
            honourAgreementEntity.setCreateTime(createTime);
            honourAgreementEntity.setUpdateTime(createTime);
            honourAgreementEntity.setAgreementStatus(0);

            // 暂时设置默认值。
            honourAgreementEntity.setGoodsId("NULL");
            honourAgreementEntity.setServiceId("NULL");
            honourAgreementEntity.setLicenceCount(0);
            honourAgreementEntity.setMeasureUnit("NULL");
            honourAgreementEntity.setServiceTime(0);
            honourAgreementEntity.setTimeUnit("NULL");
            honourAgreementEntityList.add(honourAgreementEntity);
        });

        return honourAgreementEntityList;
    }

    /**
     * description: orderCreateReqVoBuild
     * 构造订单请求参数
     * version: 1.0
     * date: 2023/8/2 10:23
     * author: A23204
     *
     * @param orderCreateReqVo
     * @return void
     */
    private void orderCreateReqVoBuild(OrderCreateReqVo orderCreateReqVo) {
        orderCreateReqVo.setBusinessNumber(IdWorker.getIdStr());
        orderCreateReqVo.setNotifyUrl(subscribeCallBack);
        // set OrderReceiverReqVo tenantId tenantName countryCode
        if (OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey().equals(orderCreateReqVo.getAppId())) {
            Long tenantId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
            SellerDetailVO merchantByIdSupportFallback = pileUserServiceAdapter.findMerchantById(tenantId);
            if (merchantByIdSupportFallback == null || StringUtils.isBlank(merchantByIdSupportFallback.getCountryAbbreviation())) {
                throw new MessageCodeException(PileBaseEnum.SELLER_COUNTRY_ABBREVIATION_IS_NULL);
            }
            orderCreateReqVo.setOperatorId(LoginUserUtil.getUserId());
            orderCreateReqVo.setCustomerId(tenantId);
            orderCreateReqVo.setCustomerName(Optional.ofNullable(merchantByIdSupportFallback.getName()).orElse("--"));
            OrderReceiverVo orderReceiverVo = new OrderReceiverVo();
            orderReceiverVo.setReceiverName(orderCreateReqVo.getCustomerName());
            orderReceiverVo.setAlpha2Code(Optional.ofNullable(merchantByIdSupportFallback.getCountryAbbreviation()).orElse("--"));
            orderReceiverVo.setMerchantId(tenantId);
            orderReceiverVo.setMerchantName(Optional.ofNullable(merchantByIdSupportFallback.getName()).orElse("--"));
            if (StringUtils.isNotBlank(merchantByIdSupportFallback.getZipCode())) {
                orderReceiverVo.setZipCode(merchantByIdSupportFallback.getZipCode());
            }
            orderCreateReqVo.setOrderReceiverReqVo(orderReceiverVo);

            // set invoice info
            OrderCreateReqVo.Invoice invoice = new OrderCreateReqVo.Invoice();
            invoice.setCustomerId(Long.valueOf(merchantByIdSupportFallback.getId()));
            invoice.setCustomerName(Optional.ofNullable(merchantByIdSupportFallback.getName()).orElse("--"));
            invoice.setCustomerAddress(merchantByIdSupportFallback.getAddress());
            invoice.setCustomerCountryCode(merchantByIdSupportFallback.getCountryAbbreviation());
            if (StringUtils.isNotBlank(merchantByIdSupportFallback.getDutyParagraph())) {
                invoice.setVatNumber(merchantByIdSupportFallback.getDutyParagraph());
            }
            orderCreateReqVo.setInvoice(invoice);
        }
    }

    private void updateOrderId(Result<ClientOrderCreateRespVo> placeOrderResult) {
        log.info("--->>> updateOrderId, param:{}", placeOrderResult);
        // 调用下单接口成功后，更新orderId
        if (placeOrderResult.getCode() == HttpStatus.SC_OK) {
            try {
                String businessNumber = placeOrderResult.getData().getBusinessNumber();
                String orderId = placeOrderResult.getData().getOrderId();
                int needPay = Boolean.TRUE.equals(placeOrderResult.getData().getPayRequired()) ? 1 : 0;
                // 根据 businessNumber 更新 orderId
                LambdaUpdateWrapper<TbOrderRecordEntity> updateWrapper = Wrappers.lambdaUpdate();
                updateWrapper.eq(TbOrderRecordEntity::getSerialId, businessNumber);

                TbOrderRecordEntity recordEntity = new TbOrderRecordEntity();
                recordEntity.setOrderId(orderId);
                recordEntity.setStatus(0);
                recordEntity.setNeedPay(needPay);
                recordEntity.setUpdateTime(System.currentTimeMillis());

                tbOrderRecordMapper.update(recordEntity, updateWrapper);

                // 同步更新 tb_order_pile
                LambdaUpdateWrapper<TbOrderPileEntity> pileEntityLambdaUpdateWrapper = Wrappers.lambdaUpdate();
                pileEntityLambdaUpdateWrapper.eq(TbOrderPileEntity::getSerialId, businessNumber);

                TbOrderPileEntity tbOrderPileEntity = new TbOrderPileEntity();
                tbOrderPileEntity.setOrderId(orderId);
                tbOrderPileEntity.setUpdateTime(System.currentTimeMillis());

                tbOrderPileMapper.update(tbOrderPileEntity, pileEntityLambdaUpdateWrapper);

            } catch (Exception e) {
                log.error("--->>> SubscribePlaceOrderServiceImpl.updateOrderId error, order-center response: {}", JSON.toJSONString(placeOrderResult), e);
            }
        } else {
            log.error("--->>> updateOrderId error, call order-center create order failed, returns:{} ", JSON.toJSONString(placeOrderResult));
        }
    }

    /**
     * description: paramCheck
     * 增加下单接口校验，验证购买数量是否匹配
     * version: 1.1
     * date: 2023/8/2 10:24
     * author: A23204
     *
     * @param orderCreateReqVo
     * @return void
     */
    private void paramCheck(OrderCreateReqVo orderCreateReqVo) {
        List<String> pileSns = orderCreateReqVo.getPileSns();
        if (OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey().equals(orderCreateReqVo.getAppId()) && !CollectionUtils.isEmpty(pileSns)) {
            log.info("--->>> place order paramCheck, pileSns is not empty.");

            // 云平台下单，允许无设备无设备下单。 当选择了设备时，检查购买licence数据与枪、桩数量是否匹配

            List<OrderCreateReqVo.CommodityItem> commodityItems = orderCreateReqVo.getCommodityItems();

            // 查询商品的 计量单位
            ClientCommodityDetailReqVo clientCommodityDetailReqVo = new ClientCommodityDetailReqVo();
            String commodityCode = commodityItems.get(0).getCommodityCode();
            clientCommodityDetailReqVo.setCommodityCode(commodityCode);
            clientCommodityDetailReqVo.setAppId("Chargebusi");
            Result<ClientCommodityDetailRespVo> clientCommodityDetailRespVoResult = queryGoodsDetail(clientCommodityDetailReqVo);
            List<CommonRespVo> commodityAttrs = clientCommodityDetailRespVoResult.getData().getCommodityAttrs();
            List<CommonRespVo> collect = commodityAttrs.stream().filter(item -> "unitOfQuantityCode".equalsIgnoreCase(item.getCode())).collect(Collectors.toList());
            String unitOfQuantityCode = collect.get(0).getValue();
            log.info("--->>> place order paramCheck, unitOfQuantityCode: {}", unitOfQuantityCode);

            // 分别统计AC、DC 的总数量
            int ac = 0, dc = 0;
            Map<String , Integer> typeMap = new HashMap<>();

            Integer total = commodityItems.stream().mapToInt(OrderCreateReqVo.CommodityItem::getQuantity).sum();

            boolean exists = commodityItems.stream().anyMatch(item -> StringUtils.isNotBlank(item.getPowerType()));
            if (exists) {
                for (OrderCreateReqVo.CommodityItem commodityItem : commodityItems) {
                    typeMap.merge(commodityItem.getPowerType(), commodityItem.getQuantity(), Integer::sum);
                /*if (commodityItem.getSku().contains("ac")) {
                    ac += commodityItem.getQuantity();
                } else if (commodityItem.getSku().contains("dc")) {
                    dc += commodityItem.getQuantity();
                }*/
                }
            }

            // ac/dc 数量计算完毕。
            // 开始计算实际需要的 ac/dc数量
            int acActualNeed = 0, dcActualNeed = 0, acPile = 0, dcPile = 0;

            Map<String , Integer> typeMapNeedGun = new HashMap<>();
            Map<String , Integer> typeMapNeedPile = new HashMap<>();

            for (String pileSn : pileSns) {
                ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationService.queryBySN(pileSn, LoginUserHolder.getLoginUser().getPayload().getSellerId());
                if (chargePointEntity != null) {
                    typeMapNeedGun.merge(chargePointEntity.getPowerType(), chargePointEntity.getConnectors().size(), Integer::sum);
                    typeMapNeedPile.merge(chargePointEntity.getPowerType(), 1, Integer::sum);
                   /* if ("AC".equalsIgnoreCase(chargePointEntity.getPowerType())) {
                        acActualNeed += chargePointEntity.getConnectors().size();
                        acPile++;
                    } else if ("DC".equalsIgnoreCase(chargePointEntity.getPowerType())) {
                        dcActualNeed += chargePointEntity.getConnectors().size();
                        dcPile++;
                    }*/
                } else {
                    // 可能是超充终端
                    ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity = chargePointMerchantTerminalService.queryBySN(pileSn, LoginUserHolder.getLoginUser().getPayload().getSellerId());
                    ChargePointMerchantRelationEntity chargePointEntityHost = chargePointMerchantRelationService.queryBySN(chargePointMerchantTerminalEntity.getHostSn(), LoginUserHolder.getLoginUser().getPayload().getSellerId());
                    /*if ("AC".equalsIgnoreCase(chargePointEntityHost.getPowerType())) {
                        acActualNeed += chargePointMerchantTerminalEntity.getConnectorsList().size();
                        acPile++;
                    } else if ("DC".equalsIgnoreCase(chargePointEntityHost.getPowerType())) {
                        dcActualNeed += chargePointMerchantTerminalEntity.getConnectorsList().size();
                        dcPile++;
                    }*/
                    typeMapNeedGun.merge(chargePointEntityHost.getPowerType(), chargePointMerchantTerminalEntity.getConnectorsList().size(), Integer::sum);
                    typeMapNeedPile.merge(chargePointEntityHost.getPowerType(), 1, Integer::sum);
                }
            }

            log.info("--->>> place order paramCheck, ac: {}, dc: {}, acActualNeed: {}, dcActualNeed: {}, acPile: {}, dcPile: {}", ac, dc, acActualNeed, dcActualNeed, acPile, dcPile);

            if (UnitOfQuantityCodeEnum.PILE.getCode().equalsIgnoreCase(unitOfQuantityCode)) {
                typeMap.forEach((k, v) -> {
                    Integer needCount = typeMapNeedPile.get(k);
                    if (!v.equals(needCount)) {
                        log.info("quantityUnit pile, ac or dc licence quantity error.");
                        throw new MessageCodeException(PileBaseEnum.LICENCE_QUANTITY_NOT_ENOUGH);
                    }
                });
                if (!exists && !total.equals(pileSns.size())) {
                    // 比较总
                    log.info("pile unit, exists = false, quantityUnit pile, ac or dc licence quantity error.");
                    throw new MessageCodeException(PileBaseEnum.LICENCE_QUANTITY_NOT_ENOUGH);
                }
               /* // 桩维度校验
                if (ac != acPile || dc != dcPile) {
                    log.info("quantityUnit pile, ac or dc licence quantity error.");
                    throw new MessageCodeException(PileBaseEnum.LICENCE_QUANTITY_NOT_ENOUGH);
                }*/
            } else if (UnitOfQuantityCodeEnum.GUN.getCode().equalsIgnoreCase(unitOfQuantityCode)) {
                // 枪维度
                typeMap.forEach((k, v) -> {
                    Integer needCount = typeMapNeedGun.get(k);
                    if (!v.equals(needCount)) {
                        log.info("quantityUnit gun, ac or dc licence quantity error.");
                        throw new MessageCodeException(PileBaseEnum.LICENCE_QUANTITY_NOT_ENOUGH);
                    }
                });

                if (!exists) {
                    // 比较总
                    int sum = typeMapNeedGun.values().stream().mapToInt(Integer::intValue).sum();
                    if (!total.equals(sum)) {
                        log.info("gun unit, exists = false, quantityUnit pile, ac or dc licence quantity error.");
                        throw new MessageCodeException(PileBaseEnum.LICENCE_QUANTITY_NOT_ENOUGH);
                    }
                }

                /*if (ac != acActualNeed || dc != dcActualNeed) {
                    log.info("quantityUnit gun, ac or dc licence quantity error.");
                    throw new MessageCodeException(PileBaseEnum.LICENCE_QUANTITY_NOT_ENOUGH);
                }*/
            }

            log.info("--->>> place order paramCheck success.");
        }
    }

    private void saveOrderAndSns(OrderCreateReqVo orderCreateReqVo) {
        TbOrderRecordEntity recordEntity = new TbOrderRecordEntity();
        recordEntity.setSerialId(orderCreateReqVo.getBusinessNumber());
        recordEntity.setTenantId(String.valueOf(orderCreateReqVo.getCustomerId()));
        recordEntity.setCreateTime(System.currentTimeMillis());
        recordEntity.setUpdateTime(recordEntity.getCreateTime());
        recordEntity.setTimeZone(orderCreateReqVo.getTimeZone());
        recordEntity.setSourceId(orderCreateReqVo.getAppId());
        recordEntity.setPurchaseType(Optional.ofNullable(orderCreateReqVo.getPurchaseMethod()).orElse(1));
        recordEntity.setCreateBy(String.valueOf(orderCreateReqVo.getOperatorId()));
        recordEntity.setOuterOrderNo(orderCreateReqVo.getOuterOrderNo());

        List<OrderCreateReqVo.CommodityItem> commodityItems = orderCreateReqVo.getCommodityItems();
        if (!CollectionUtils.isEmpty(commodityItems)) {
            recordEntity.setLicenceCount(commodityItems.stream().mapToInt(OrderCreateReqVo.CommodityItem::getQuantity).sum());
        }

        if (OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey().equals(orderCreateReqVo.getAppId())) {
            try {
                HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
                String language = request.getHeader("accept-language");
                recordEntity.setAcceptLanguage(language);
            } catch (Exception ex) {
                log.info("--->>> saveOrderAndSns set acceptLanguage error.", ex);
            }
        }

        tbOrderRecordMapper.insert(recordEntity);

        // 如果是云平台下单，
        if (!CollectionUtils.isEmpty(orderCreateReqVo.getPileSns())) {
            orderCreateReqVo.getPileSns().forEach(pile -> {
                TbOrderPileEntity pileEntity = new TbOrderPileEntity();
                pileEntity.setSerialId(orderCreateReqVo.getBusinessNumber());
                pileEntity.setPileSn(pile);
                pileEntity.setCreateTime(System.currentTimeMillis());
                pileEntity.setUpdateTime(pileEntity.getCreateTime());
                tbOrderPileMapper.insert(pileEntity);
            });
        }
    }

    /**
     * description: placeOrderSAP
     * SAP 渠道订单同步
     * version: 1.0
     * date: 2023/8/2 10:25
     * author: A23204
     *
     * @param autelSign
     * @param redirectKey
     * @param orderCreateSapReqVo
     * @return com.autel.cloud.base.http.pojo.Result<com.autel.cloud.ordercenter.vo.ClientOrderCreateRespVo>
     */
    @Override
    public Result<ClientOrderCreateRespVo> placeOrderSAP(String autelSign, String redirectKey, OrderCreateSapReqVo orderCreateSapReqVo) {

        if (sapOfflineFlag) {
            return Result.ofSucceed();
        }

        log.info("--->>> placeOrderSAP request param:{}, redirectKey:{}", JSON.toJSONString(orderCreateSapReqVo), redirectKey);

        // address为空处理.

        if (StringUtils.isBlank(orderCreateSapReqVo.getAddress())) {
            orderCreateSapReqVo.setAddress(orderCreateSapReqVo.getCity() + ", " + orderCreateSapReqVo.getArea() + " " + orderCreateSapReqVo.getZipCode() + ", " + "countryAbbreviation");
        }
        /*
        * 如果address为空，格式： city, area zipCode,
        * {
            "accountCountryAbbreviation": "US",
            "accountCountryCode": "+1",
            "accountName": "George Burnett",
            "address": "4420 S Park Loop Rd, Jackson, WY 83001, USA",
            "area": "WY",
            "businessLicense": "",
            "certificatePath": "",
            "city": "South Park",
            "contact": "George Burnett",
            "contactEmail": "george@acmeev.com",
            "countryAbbreviation": "US",
            "countryName": "United States",
            "describe": "",
            "dutyParagraph": "0",
            "email": "george@acmeev.com",
            "idCard": "",
            "latitude": 43.4166989,
            "logoPath": "",
            "longitude": -110.805225,
            "name": "ACME Fleet Inc",
            "phoneNumber": "8585313083",
            "postalAddress": "",
            "postalCode": "",
            "roleList": ["1524683495125372929"],
            "subContact": "",
            "subContactPhone": "",
            "tagList": [{
                "name": "Customer"
            }, {
                "name": "Commercial"
            }],
            "type": 1,
            "webAddress": "",
            "zipCode": "83001"
        }
        *
        * */

        /*
            增加 redirectKey header，保证请求转发只发送一次，防止异常情况发生死循环。
         */
        String targetDomain = needRedirectRequest(orderCreateSapReqVo.getCountryAbbreviation());
        if (targetDomain != null && !REDIRECT_KEY.equals(redirectKey)) {
            log.info("--->>> placeOrderSAP redirect request to targetDomain:{}", targetDomain);
            // 不是当前环境的请求， 转发至目标环境
            String placeUrl = "https://" + targetDomain + "/api/pile-base-app/order/goods/sap/place";

            HttpRequest httpRequest = new HttpRequest(placeUrl);
            httpRequest.header("autel-sign", autelSign);
            httpRequest.header("redirect-key", REDIRECT_KEY);
            httpRequest.body(JSON.toJSONString(orderCreateSapReqVo));
            httpRequest.setMethod(Method.POST);

            // try一下http请求，如果5S没有收到，则重试
            HttpResponse httpResponse = null;
            int retryTime = 10000;
            for (int attempt = 0; attempt < 4; attempt++) {
                httpRequest.setReadTimeout(retryTime);
                httpRequest.setConnectionTimeout(retryTime);
                try {
                    httpResponse = httpRequest.execute();
                    break;
                } catch (Exception e) {
                    //增加超时时长，进行重试
                    log.info("request to target domain error. url is {}.times {}.", placeUrl, attempt, e);
                    retryTime += 2000;
                }
            }
            if (httpResponse == null) {
                log.info("many times error, url {}", placeUrl);
                return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage("request timeout after many times retrying."));
            }

            String body = httpResponse.body();
            //String body2 = "{\"code\":200,\"data\":{\"businessNumber\":\"1681565149879730177\",\"orderId\":\"1681565150326468609\",\"payUrl\":\"https://checkout.stripe.com/c/pay/cs_test_a1evEyq65yri0JmqHd2ujGAu9sSWAPNYvVT05x435Rj9edEnK6VAKdaL30#fidkdWxOYHwnPyd1blpxYHZxWjA0TzBVQXNCN0dyZ0xOYHN9cXBTfGxJY1VcVl98dFdVVXdrdVFrVlNBdnJ%2FSjxAcFBBY2B9c3dAYn9qVXQwM3xzNXBGNXdNcjxrTHVWQn1Lc25OM01yb1dwNTVVdV91bEh0ZicpJ2N3amhWYHdzYHcnP3F3cGApJ2lkfGpwcVF8dWAnPyd2bGtiaWBabHFgaCcpJ2BrZGdpYFVpZGZgbWppYWB3dic%2FcXdwYHgl\",\"status\":1,\"statusName\":\"待支付\"},\"message\":\"OK\"}";
            JSONObject jsonObject = JSON.parseObject(body);
            Result<ClientOrderCreateRespVo> placeOrderResult = new Result<>();
            placeOrderResult.setCode(jsonObject.getInteger("code"));
            placeOrderResult.setMessage(jsonObject.getString("message"));
            JSONObject data = jsonObject.getJSONObject("data");
            ClientOrderCreateRespVo respVo = JSON.toJavaObject(data, ClientOrderCreateRespVo.class);
            placeOrderResult.setData(respVo);

            log.info("--->>> redirect request returns:{}", JSON.toJSONString(placeOrderResult));

            return placeOrderResult;
        }

        // 是当前环境
        log.info("--->>> go to current env business.");

        // 签名校验，检验时间戳，与当前时间超过60秒，认为请求超时。
        String localSign = AutelSignUtil.getSigns(null, orderCreateSapReqVo);
        //checkSignature(autelSign, localSign, orderCreateSapReqVo.getCurrentMilTimeStamp());

        /*
         * EnvironmentEnum.queryEnvEnumByNs(nacosNs)
         * 新增校验，只有欧洲国家，税号必填
         *
         * */

        String serverCode = EnvironmentEnum.queryEnvEnumByNs(nacosNs).getServerCode();
        if (serverCode.contains("EU") && StringUtils.isBlank(orderCreateSapReqVo.getDutyParagraph())) {
            throw new IllegalArgumentException("bad request, dutyParagraph can not be empty.");
        }

        // 1. 判断当前outerOrderNo,status=0, 查询是否已经存在，存在就返回 orderID
        String orderId = orderExists(orderCreateSapReqVo);
        if (orderId != null) {
            ClientOrderCreateRespVo respVo = new ClientOrderCreateRespVo();
            respVo.setOrderId(orderId);
            respVo.setStatus(2);
            return Result.ofSucceed(respVo);
        }

        // 2. 判断商家是否存在。 根据超管邮箱查询  api,pile-user 新增接口，根据邮箱查询。
        SellerDetailVO sellerDetail = getSellerDetailByEmail(orderCreateSapReqVo.getEmail());

        Long sellerId;
        // 2. 不存在，新建商家 接口返回的是sellerId
        if (sellerDetail == null) {
            sellerId = doCreateSeller(orderCreateSapReqVo);
        } else {
            sellerId = Long.parseLong(sellerDetail.getId());
        }

        if (sellerId == null) {
            log.info("--->>> placeOrderSAP, can not get sellerId, request param{}", JSON.toJSONString(orderCreateSapReqVo));
            throw new IllegalArgumentException("bad request, can not get or get sellerId, please try it again later.");
        }

        orderCreateSapReqVo.setSellerId(sellerId);

        // 3. 创建本地订单
        TbOrderRecordEntity recordEntity = createLocalOrderRecord(orderCreateSapReqVo);

        // added @2023/08/07, sap下单增加桩号列表
        savePileSns(orderCreateSapReqVo.getPileSns(), recordEntity.getSerialId());

        // 4. 调用order-center
        OrderCreateReqVo orderCreateReq = orderCreateSapReqVoBuild(orderCreateSapReqVo, recordEntity.getSerialId());
        Result<ClientOrderCreateRespVo> placeOrderResult = orderCenterFeignClient.placeOrder(orderCreateReq);

        log.info("--->>> placeOrderResult : {}", JSON.toJSONString(placeOrderResult));

        // update order id
        updateOrderId(placeOrderResult);

        return placeOrderResult;
    }

    /*
     * description: savePileSns
     * sap 渠道下单保存桩号信息
     * version: 1.0
     * date: 2023/8/7 19:42
     * author: A23204
     *
     * @param pileSns
     * @param serialId
     * @return void
     */
    private void savePileSns(List<String> pileSns, String serialId) {
        /*
        *    TbOrderPileEntity pileEntity = new TbOrderPileEntity();
                pileEntity.setSerialId(orderCreateReqVo.getBusinessNumber());
                pileEntity.setPileSn(pile);
                pileEntity.setCreateTime(System.currentTimeMillis());
                pileEntity.setUpdateTime(pileEntity.getCreateTime());
                tbOrderPileMapper.insert(pileEntity);
        * */
        if (CollectionUtils.isEmpty(pileSns)) {
            log.info("--->>>savePileSns, pileSns size is 0.");
            return;
        }
        log.info("save sap pile sns, list.size:{}", pileSns.size());

        // 保存sap订单。 只保存sn。
        Long createTime = System.currentTimeMillis();
        pileSns.forEach(sn -> {
            TbOrderPileEntity pileEntity = new TbOrderPileEntity();
            pileEntity.setSerialId(serialId);
            pileEntity.setPileSn(sn);
            pileEntity.setCreateTime(createTime);
            pileEntity.setUpdateTime(createTime);
            tbOrderPileMapper.insert(pileEntity);
        });
    }

    private String orderExists(OrderCreateSapReqVo orderCreateSapReqVo) {
        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbOrderRecordEntity::getOuterOrderNo, orderCreateSapReqVo.getOuterOrderNo());
        queryWrapper.eq(TbOrderRecordEntity::getStatus, 0);
        List<TbOrderRecordEntity> tbOrderRecordEntities = tbOrderRecordMapper.selectList(queryWrapper);
        if (!CollectionUtils.isEmpty(tbOrderRecordEntities)) {
            return tbOrderRecordEntities.get(0).getOrderId();
        }

        return null;
    }

    /**
     * description: orderCreateSapReqVoBuild
     * sap 下单参数构造
     * version: 1.0
     * date: 2023/8/2 10:25
     * author: A23204
     *
     * @param orderCreateSapReqVo
     * @param serialId
     * @return com.autel.cloud.ordercenter.vo.OrderCreateReqVo
     */
    private OrderCreateReqVo orderCreateSapReqVoBuild(OrderCreateSapReqVo orderCreateSapReqVo, String serialId) {
        OrderCreateReqVo orderCreateReqVo = new OrderCreateReqVo();
        orderCreateReqVo.setAppId(orderCreateSapReqVo.getAppId());
        orderCreateReqVo.setAppName(orderCreateSapReqVo.getAppName());
        orderCreateReqVo.setType(1);
        orderCreateReqVo.setNotifyUrl(subscribeCallBack);
        orderCreateReqVo.setOperatorId(-1L);
        orderCreateReqVo.setCustomerId(orderCreateSapReqVo.getSellerId());
        orderCreateReqVo.setCustomerName(orderCreateSapReqVo.getName());
        orderCreateReqVo.setBusinessNumber(serialId);

        OrderReceiverVo orderReceiverReqVo = new OrderReceiverReqVo();
        orderReceiverReqVo.setMerchantId(orderCreateSapReqVo.getSellerId());
        orderReceiverReqVo.setMerchantName(orderCreateSapReqVo.getName());
        orderReceiverReqVo.setReceiverName(orderCreateSapReqVo.getName());
        orderReceiverReqVo.setAlpha2Code(orderCreateSapReqVo.getCountryAbbreviation());
        orderCreateReqVo.setOrderReceiverReqVo(orderReceiverReqVo);

        List<OrderCreateReqVo.CommodityItem> commodityItems = new ArrayList<>();

        Map<String, Integer> collectSkuCode = orderCreateSapReqVo.getList().stream().collect(Collectors.groupingBy(UniqueSkuAttribute::getUniqueSkuCode, Collectors.summingInt(UniqueSkuAttribute::getCount)));
        collectSkuCode.forEach((skuCode, quantity) -> {
            OrderCreateReqVo.CommodityItem commodityItem = new OrderCreateReqVo.CommodityItem();
            commodityItem.setSku(skuCode);
            commodityItem.setQuantity(quantity);
            commodityItems.add(commodityItem);
        });
/*        orderCreateSapReqVo.getList().forEach(uniqueSkuAttribute -> {
            OrderCreateReqVo.CommodityItem commodityItem = new OrderCreateReqVo.CommodityItem();
            commodityItem.setSku(uniqueSkuAttribute.getUniqueSkuCode());
            commodityItem.setQuantity(uniqueSkuAttribute.getCount());
            commodityItems.add(commodityItem);
        });*/


        orderCreateReqVo.setCommodityItems(commodityItems);

        return orderCreateReqVo;
    }

    private TbOrderRecordEntity createLocalOrderRecord(OrderCreateSapReqVo orderCreateSapReqVo) {
        TbOrderRecordEntity recordEntity = new TbOrderRecordEntity();
        recordEntity.setSerialId(IdWorker.getIdStr());
        recordEntity.setTenantId(String.valueOf(orderCreateSapReqVo.getSellerId()));
        recordEntity.setCreateTime(System.currentTimeMillis());
        recordEntity.setUpdateTime(recordEntity.getCreateTime());
        recordEntity.setLicenceCount(orderCreateSapReqVo.getList().stream().mapToInt(UniqueSkuAttribute::getCount).sum());
        recordEntity.setOuterOrderNo(orderCreateSapReqVo.getOuterOrderNo());

        tbOrderRecordMapper.insert(recordEntity);

        return recordEntity;
    }

    /**
     * description: doCreateSeller
     * sap 订单同步，自动创建商家
     * version: 1.0
     * date: 2023/8/2 10:26
     * author: A23204
     *
     * @param orderCreateSapReqVo
     * @return java.lang.Long
     */
    private Long doCreateSeller(OrderCreateSapReqVo orderCreateSapReqVo) {
        SellerAddDTO sellerAddDTO = new SellerAddDTO();
        BeanUtil.copyProperties(orderCreateSapReqVo, sellerAddDTO);
        sellerAddDTO.setSellerId(0L);
        sellerAddDTO.setType(3);

        List<OpCountryEntity> countryInfoByCountryAbbre = countryRepository.getCountryInfoByCountryAbbre(orderCreateSapReqVo.getCountryAbbreviation());
        // 非空校验

        if (!CollectionUtils.isEmpty(countryInfoByCountryAbbre)) {
            sellerAddDTO.setAccountCountryCode(countryInfoByCountryAbbre.get(0).getCode());
        }

        Result<Long> saveResult = pileUserFeign.save(null, sellerAddDTO);
        log.info("--->>> auto create seller returns:{}", JSON.toJSONString(saveResult));
        return saveResult.getData();
    }

    /**
     * description: orderCancelSAP
     * sap 订单取消接口
     * version: 1.0
     * date: 2023/8/2 10:26
     * author: A23204
     *
     * @param autelSign
     * @param redirectKey
     * @param orderCancelReqVo
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    @Override
    public Result<Boolean> orderCancelSAP(String autelSign, String redirectKey, OrderCancelReqVo orderCancelReqVo) {

        if (sapOfflineFlag) {
            return Result.ofSucceed(Boolean.TRUE);
        }

        log.info("--->>> orderCancelSAP request param:{}, redirectKey:{}", JSON.toJSONString(orderCancelReqVo), redirectKey);
        String targetDomain = needRedirectRequest(orderCancelReqVo.getCountryAbbreviation());
        if (targetDomain != null && !REDIRECT_KEY.equals(redirectKey)) {
            // 转发取消订单接口。
            log.info("--->>> orderCancelSAP redirect request to targetDomain:{}", targetDomain);
            // 不是当前环境的请求， 转发至目标环境
            String placeUrl = "https://" + targetDomain + "/api/pile-base-app/order/goods/sap/delete";

            HttpRequest httpRequest = new HttpRequest(placeUrl);
            httpRequest.header("autel-sign", autelSign);
            httpRequest.header("redirect-key", REDIRECT_KEY);
            httpRequest.body(JSON.toJSONString(orderCancelReqVo));
            httpRequest.setMethod(Method.POST);

            // try一下http请求，如果5S没有收到，则重试
            HttpResponse httpResponse = null;
            int retryTime = 10000;
            for (int attempt = 0; attempt < 4; attempt++) {
                httpRequest.setReadTimeout(retryTime);
                httpRequest.setConnectionTimeout(retryTime);
                try {
                    httpResponse = httpRequest.execute();
                    break;
                } catch (Exception e) {
                    //增加超时时长，进行重试
                    log.info("orderCancelSAP request to target domain error. url is {}.times {}.", placeUrl, attempt, e);
                    retryTime += 2000;
                }
            }
            if (httpResponse == null) {
                log.info("many times error, url {}", placeUrl);
                return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage("orderCancelSAP request timeout after many times retrying."));
            }

            String body = httpResponse.body();
            JSONObject jsonObject = JSON.parseObject(body);
            Integer code = jsonObject.getInteger("code");
            if (HttpCodeEnum.OK.getCode() == code) {
                return Result.ofSucceed(Boolean.TRUE);
            }
            return Result.ofFailed(ResultCodeEnum.BAD_REQUEST.setMessage("orderCancelSAP request failed."));
        }

        // 1. 签名校验，检验时间戳，与当前时间超过60秒，认为超时。
        String localSign = AutelSignUtil.getSigns(null, orderCancelReqVo);
        //checkSignature(autelSign, localSign, orderCancelReqVo.getCurrentMilTimeStamp());

        /*
         * sap 传入外部订单号，先根据外部订单号查询出订单，在更新。
         * */
        LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbOrderRecordEntity::getOuterOrderNo, orderCancelReqVo.getOrderId());
        List<TbOrderRecordEntity> tbOrderRecordEntities = tbOrderRecordMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbOrderRecordEntities)) {
            throw new IllegalArgumentException("bad request, orderId not exists.");
        }

        String orderId = tbOrderRecordEntities.get(0).getOrderId();
        log.info("--->>> sap orderCancelSAP, orderId:{}", orderId);


        // 2. licence 设置失效
        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.eq(TbLenBindRelationEntity::getOrderId, orderId);

        lenBindRelationMapper.delete(updateWrapper);

        // 同步删除记录表、桩列表
        deleteOrderRecordAndOrderPile(orderId);

        // 3. 通知order-center，cancel order.
        orderCancelReqVo.setOperatorId("-1");
        orderCancelReqVo.setOrderId(orderId);

        return orderCenterFeignClient.orderCancel(orderCancelReqVo);
    }

    /*
     * description: deleteOrderRecordAndOrderPile  删除购买记录和桩记录
     * version: 1.0
     * date: 2023/8/10 14:21
     * author: A23204
     *
     * @param orderId
     * @return void
     */
    private void deleteOrderRecordAndOrderPile(String orderId) {
        // 1. 删除记录
        LambdaUpdateWrapper<TbOrderRecordEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);

        tbOrderRecordMapper.delete(updateWrapper);

        // 2. 删除桩列表
        LambdaUpdateWrapper<TbOrderPileEntity> updateWrapperPile = Wrappers.lambdaUpdate();
        updateWrapperPile.eq(TbOrderPileEntity::getOrderId, orderId);

        tbOrderPileMapper.delete(updateWrapperPile);

    }


    private void checkSignature(String autelSign, String localSign, Long currentMilTimeStamp) {
        log.info("--->>> checkSignature, autelSign:{}, localSign:{}", autelSign, localSign);
        if (!localSign.equals(autelSign) || (System.currentTimeMillis() - currentMilTimeStamp) > 60000) {
            log.info("--->>> checkSignature, signature verification failed.");
            throw new IllegalArgumentException("bad request, signature verification failed.");
        }
    }

    private SellerDetailVO getSellerDetailByEmail(String email) {
        Result<SellerDetailVO> result = pileUserFeign.getSellerByEmail(email);
        return result == null ? null : result.getData();
    }

    /**
     * description: queryPlaceOrderUrlForSap
     * 获取请求地址
     * version: 1.0
     * date: 2023/8/2 10:28
     * author: A23204
     *
     * @param countryAbbreviation
     * @param urlType
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.String>
     */
    @Override
    public Result<String> queryPlaceOrderUrlForSap(String countryAbbreviation, String urlType) {
        log.info("--->>> queryPlaceOrderUrlForSap, request countryAbbreviation:{}, urlType:{}", countryAbbreviation, urlType);
        Result<String> countryInfoByAlpha2Code = baseAdminClient.getCountryInfoByAlpha2Code(countryAbbreviation);
        String targetDomain = CountryDomainEnum.queryByEnvAndCode(EnvironmentEnum.queryEnvEnumByNs(nacosNs).getEnv(), countryInfoByAlpha2Code.getData()).getDomain();
        String placeUrl = "https://" + targetDomain + "/api/pile-base-app/order/goods/sap/place";
        if ("create".equals(urlType)) {
            placeUrl = "https://" + targetDomain + "/api/pile-base-app/order/goods/sap/place";
        } else if ("cancel".equals(urlType)) {
            placeUrl = "https://" + targetDomain + "/api/pile-base-app/order/goods/sap/delete";
        }
        log.info("--->>> queryPlaceOrderUrlForSap, countryAbbreviation{}, placeUrl:{}", countryAbbreviation, placeUrl);

        return Result.ofSucceed(placeUrl);
    }

    /**
     * description: needRedirectRequest
     * 判断是否需要转发请求。
     * version: 1.0
     * date: 2023/7/19 14:23
     * author: A23204
     *
     * @param
     * @return boolean
     */
    private String needRedirectRequest(String countryAbbreviation) {
        //根据国家码拿到区号
        Result<String> serverCodeResult = baseAdminClient.getCountryInfoByAlpha2Code(countryAbbreviation);
        if (serverCodeResult == null || com.baomidou.mybatisplus.core.toolkit.StringUtils.isBlank(serverCodeResult.getData())) {
            throw new MessageCodeException(PileUserEnum.CC_ERROR);
        }

        // mock start
     /*   Result<String> serverCodeResult = new Result<>();
        serverCodeResult.setData("CN");*/

        // mock end
        String domain = EnvironmentEnum.queryEnvEnumByNs(nacosNs).getDomain();
        String targetDomain = CountryDomainEnum.queryByEnvAndCode(EnvironmentEnum.queryEnvEnumByNs(nacosNs).getEnv(), serverCodeResult.getData()).getDomain();

        if (org.apache.commons.lang3.StringUtils.equalsIgnoreCase(targetDomain, domain)) {
            return null;
        } else {
            return targetDomain;
        }
    }

    @Override
    public Result<ClientOrderCreateRespVo> rdfExtract(RfdLicenceExtractVo rfdLicenseExtract) {
        log.info("--->>> placeOrder request param: {}", JSON.toJSONString(rfdLicenseExtract));
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String timezone = request.getHeader("Timezone");
        rfdLicenseExtract.setTimeZone(timezone);

        Long tenantId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        OrderCreateReqVo orderCreateReqVo = new OrderCreateReqVo();
        BeanUtil.copyProperties(rfdLicenseExtract, orderCreateReqVo);
        orderCreateReqVo.setBusinessNumber(IdWorker.getIdStr());
        orderCreateReqVo.setNotifyUrl(subscribeCallBack);
        orderCreateReqVo.setOperatorId(tenantId);

        SellerDetailVO merchantByIdSupportFallback = pileUserServiceAdapter.findMerchantById(tenantId);
        if (merchantByIdSupportFallback == null || StringUtils.isBlank(merchantByIdSupportFallback.getCountryAbbreviation())) {
            throw new MessageCodeException(PileBaseEnum.SELLER_COUNTRY_ABBREVIATION_IS_NULL);
        }
        orderCreateReqVo.setCustomerId(tenantId);
        orderCreateReqVo.setCustomerName(Optional.ofNullable(merchantByIdSupportFallback.getName()).orElse("--"));
        OrderReceiverVo orderReceiverVo = new OrderReceiverVo();
        orderReceiverVo.setReceiverName(orderCreateReqVo.getCustomerName());
        orderReceiverVo.setAlpha2Code(Optional.ofNullable(merchantByIdSupportFallback.getCountryAbbreviation()).orElse("--"));
        orderReceiverVo.setMerchantId(tenantId);
        orderReceiverVo.setMerchantName(Optional.ofNullable(merchantByIdSupportFallback.getName()).orElse("--"));
        if (StringUtils.isNotBlank(merchantByIdSupportFallback.getZipCode())) {
            orderReceiverVo.setZipCode(merchantByIdSupportFallback.getZipCode());
        }
        orderCreateReqVo.setOrderReceiverReqVo(orderReceiverVo);

        // save order
        saveOrderAndSns(orderCreateReqVo);

        // call order-center
        log.info("rdfExtract orderCreateReqVo {}", JSON.toJSONString(orderCreateReqVo));
        Result<ClientOrderCreateRespVo> placeOrderResult = orderCenterFeignClient.placeOrder(orderCreateReqVo);

        log.info("--->>> rdfExtract placeOrderResult : {}", JSON.toJSONString(placeOrderResult));

        // update order id
        updateOrderId(placeOrderResult);

        return placeOrderResult;
    }
}