package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpException;
import cn.hutool.json.JSONUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.common.util.LocaleResultUtil;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.pile.base.domain.service.PaymentService;
import com.autel.cloud.pile.base.domain.service.SellerAccountService;
import com.autel.cloud.pile.base.dto.app.UserCountryUnitInfoDto;
import com.autel.cloud.pile.base.dto.pay.*;
import com.autel.cloud.pile.base.enums.OrderSourceIdEnum;
import com.autel.cloud.pile.base.infrastructure.feign.PayServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.UserCenterFeign;
import com.autel.cloud.pile.base.infrastructure.feign.WsCoreClient;
import com.autel.cloud.pile.base.infrastructure.feign.dto.MessageDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.SendMsgDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.TbSellerWxAccMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.PaymentEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbSellerWxAccEntity;
import com.autel.cloud.pile.base.infrastructure.util.AutelSignUtil;
import com.autel.cloud.pile.bill.dto.pay.DTStripeBodyDTO;
import com.autel.cloud.pile.bill.dto.pay.PayHeaderDTO;
import com.autel.cloud.pile.bill.enums.PayTypeEnum;
import com.autel.cloud.pile.bill.feign.BaseAdminFeign;
import com.autel.cloud.pile.user.api.enums.EnvironmentEnum;
import com.autel.cloud.pile.user.api.feign.PileMerchantUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.SimpleInformationAboutCostModelRuleVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.*;

/**
 * @author A21309
 * @date 2022-05-24 10:46
 */
@Slf4j
@Service
public class SellerAccountServiceImpl implements SellerAccountService {

    @Autowired
    private PaymentService paymentService;

    @Autowired
    private PayServiceFeign payServerFeign;

    @Resource
    private PileMerchantUserFeign pileMerchantUserFeign;

    @Value("${stripe.source}")
    private String source;

    @Value("${stripe.pay-type}")
    private String payType;

    @Value("${stripe.account-type}")
    private String accountType;

    @Value("${stripe.return-url}")
    private String returnUrl;

    @Value("${stripe.blank-return-url}")
    private String blankReturnUrl;

    @Value("${stripe.stripe-code}")
    private Integer stripeCode;


    @Value("${stripe.callback}")
    private String callBack;

    @Autowired
    private WsCoreClient wsCoreClient;

    @Autowired
    private TariffFeignClient tariffFeignClient;

    @Autowired
    private TbSellerWxAccMapper sellerWxAccMapper;

    @Autowired
    private BaseAdminFeign baseAdminFeign;

    @Autowired
    private UserCenterFeign userCenterFeign;

    @Value("${advance.payment.switch:false}")
    private Boolean advancePaymentEnable;

    private String nacosNs = System.getenv("NACOS_NS");

    /**
     * @param id       计费规则模型的主键id
     * @param sellerId 商家id
     * @return 商家是否可以绑定某个充电桩的计费规则模型的标志
     * @function 商家是否可以绑定某个充电桩的计费规则模型
     */
    @Override
    public Boolean isSellerBindChargingPile(Long id, Long sellerId) {
        // 默认是可以绑定的
        Boolean flag = true;

        log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsSellerBindTheChargingPile id : {} and sellerId : {}", JSON.toJSONString(id), JSON.toJSONString(sellerId));

        // 微信支付修改start
        Result<String> envResult = queryCurrentEnv();
        if (envResult.getData() != null && "CN".equalsIgnoreCase(envResult.getData())) {
            // 如果是中国区，判断是否有添加微信商户号, 如果添加了，返回true。
            Result<String> accountResult = queryBindWXAccount(null);
            if (!this.isCostModelRuleFree(id) && accountResult.getData() == null) {
                return false;
            }
            return true;
        }
        // 微信支付修改end

        // 如果商家要绑定的某个充电桩的计费规则是收费的，并且商家自己的Stripe账户不能进行收费，那么此时就不能对这个充电桩配置该收费的计费规则
        if (!this.isCostModelRuleFree(id) && !this.isSellerStripeAccountCharged(sellerId)) {
            flag = false;
        }

        log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsSellerBindTheChargingPile flag : {}", JSON.toJSONString(flag));

        return flag;
    }

    /**
     * @param id 计费规则模型的主键id
     * @return 充电桩要绑定的计费规则模型是不是免费的标志
     * @function 判断充电桩要绑定的计费规则模型是不是免费的
     */
    private Boolean isCostModelRuleFree(Long id) {

        log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsCostModelRuleFree id : {}", JSON.toJSONString(id));

        // 充电桩要绑定的计费规则模型是不是免费的标志
        Boolean flag = false;

        if (ObjectUtils.isNotEmpty(id)) {
            List<Long> idList = new ArrayList<>();
            idList.add(id);

            Result<List<SimpleInformationAboutCostModelRuleVO>> result = tariffFeignClient.querySimpleInformationAboutCostModelRuleVOByIds(idList);

            log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsCostModelRuleFree result : {}", JSON.toJSONString(result));

            if (ObjectUtils.isNotEmpty(result) && result.getCode() == HttpStatus.SC_OK) {
                List<SimpleInformationAboutCostModelRuleVO> simpleInformationAboutCostModelRuleVOS = result.getData();
                if (ObjectUtils.isNotEmpty(simpleInformationAboutCostModelRuleVOS)) {
                    SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO = simpleInformationAboutCostModelRuleVOS.get(0);
                    if (ObjectUtils.isNotEmpty(simpleInformationAboutCostModelRuleVO)) {
                        Integer ruleModelType = simpleInformationAboutCostModelRuleVO.getRuleModelType();
                        if (ObjectUtils.isNotEmpty(ruleModelType) && (ruleModelType.equals(RuleModelTypeEnum.FREE.getCode()))) {
                            flag = true;
                        }
                    }
                }
            }
        }

        log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsCostModelRuleFree flag : {}", JSON.toJSONString(flag));

        return flag;
    }

    /**
     * @param sellerId 商家Id
     * @return 商家的Stripe账户是否可以进行收费的结果
     * @function 查询商家的Stripe账户是否可以进行收费
     */
    @Override
    public Boolean isSellerStripeAccountCharged(Long sellerId) {

        log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsSellerStripeAccountCharged sellerId : {}", JSON.toJSONString(sellerId));

        // 商家的Stripe账户是否可以进行收费的标志 （默认是不可以收费的）
        Boolean flag = false;

        if (ObjectUtils.isNotEmpty(sellerId)) {
            // 查询t_payment表
            PaymentEntity payment = paymentService.selectAccountIdBySellerIdAndCode(sellerId, 1);

            log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsSellerStripeAccountCharged payment : {}", JSON.toJSONString(payment));

            if (ObjectUtils.isNotEmpty(payment)) {
                // 远程调用pay-server服务的查询商户接口
                // todo 构造请求参数
                HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
                String lgHeader = request.getHeader("Accept-Language");
                Long paymentId = payment.getId();
                //获取sign
                DTStripeHeadDTO headDto = new DTStripeHeadDTO();
                headDto.setSource(source);
                headDto.setPayType(payType);
                DTStripeBodyDTO bodyDto = new DTStripeBodyDTO();
                bodyDto.setBindingId(paymentId);
                String sign = AutelSignUtil.getSigns(headDto, bodyDto);

                // 远程调用pay-server服务的查询商户接口并获得返回结果
                QueryStripeResp queryStripeResp = payServerFeign.stripeAccountStatusQueryV2(source, Integer.valueOf(payType), null, lgHeader, sign, String.valueOf(paymentId));

                log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsSellerStripeAccountCharged queryStripeResp : {}", JSON.toJSONString(queryStripeResp));

                if (ObjectUtils.isNotEmpty(queryStripeResp) && queryStripeResp.getCode() == 0) {
                    SellerAccountResp sellerAccountResp = queryStripeResp.getData();
                    if (ObjectUtils.isNotEmpty(sellerAccountResp)) {
                        SpecialParamResp specialParam = sellerAccountResp.getSpecialParam();
                        if (ObjectUtils.isNotEmpty(specialParam)) {

                            // todo 商家的Stripe账户是否可以进行收费的判断条件是否只有这两个？

                            // 账户是否可以进行收费：0-不能，1-可以
                            Integer chargesEnabled = specialParam.getChargesEnabled();

                            // 关联子账户时是否详情信息提交完成：0-未完全填写，1-已详情填写
                            Integer detailsSubmitted = specialParam.getDetailsSubmitted();

                            if (ObjectUtils.isNotEmpty(chargesEnabled)
                                    && chargesEnabled == 1
                                    && ObjectUtils.isNotEmpty(detailsSubmitted)
                                    && detailsSubmitted == 1) {

                                // 修改判断标志，此时商家的Stripe账户是可以进行收费的
                                flag = true;

                            }
                        }
                    }
                }
            }
        }

        log.info("==========>>>>>>>>>SellerAccountServiceImpl.IsSellerStripeAccountCharged flag : {}", JSON.toJSONString(flag));

        return flag;
    }


    @Override
    public Result<SellerAccountResp> queryStripeAccount(SellerAccountReq sellerAccountReq) {
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        log.info("queryStripeAccount##sellerAccountReq is {}", JSONUtil.toJsonStr(sellerAccountReq));
        String lgHeader = request.getHeader("Accept-Language");
        String sellerId = sellerAccountReq.getSellerId();
        if (StrUtil.isBlank(sellerId)) {
            return Result.ofFailed(LocaleResultUtil.result("400"));
        }
        PaymentEntity payment = paymentService.selectAccountIdBySellerIdAndCode(Long.valueOf(sellerId), stripeCode);
        log.info("queryStripeAccount##payment is {}", JSONUtil.toJsonStr(payment));
        //如果没有插入payment表该商家信息
        if (payment == null) {
            payment = savePayment(sellerId);
        }
        QueryStripeResp queryStripeResp = null;
        try {
            Long paymentId = payment.getId();
        /*
          GET  /api/base-pay/unified /account? bindingId=
         */
            //获取sign
            DTStripeHeadDTO headDto = new DTStripeHeadDTO();
            headDto.setSource(source);
            headDto.setPayType(payType);
            DTStripeBodyDTO bodyDto = new DTStripeBodyDTO();
            bodyDto.setBindingId(paymentId);
            String sign = AutelSignUtil.getSigns(headDto, bodyDto);

            queryStripeResp = payServerFeign.stripeAccountStatusQueryV2(source, Integer.valueOf(payType), null, lgHeader, sign, String.valueOf(paymentId));
        } catch (HttpException e) {
            log.error("queryStripeAccount##error found", e);
        }
        log.info("queryStripeAccount##body is {}", queryStripeResp);

        //处理审核状态，和审核信息
        assert queryStripeResp != null;
        SellerAccountResp respData = queryStripeResp.getData();
        //是否已绑定关联（仅仅只是关联了子账户，并不代表能正常收费）：0-否，1-是,
        //TODO 如果验签失败，直接返回
        SpecialParamResp specialParam = respData.getSpecialParam();

        //是否可接受付款
        Integer chargesEnabled = specialParam.getChargesEnabled();
        //是否提交注册信息
        Integer detailsSubmitted = specialParam.getDetailsSubmitted();
        //缺失信息-账户即将被限制，需要提供这些信息，否则账户将会被限制
        Set<String> needProvidewarnrmationSet = specialParam.getNeedProvideInformationSet();
        //缺失信息-账户已经被限制了，必须提供这些信息才能恢复正常
        Set<String> mustProvidewarnrmationSet = specialParam.getMustProvideInformationSet();

        if (Boolean.FALSE.equals(payment.getEnableFlag())) {
            if (detailsSubmitted.equals(0)) {
                respData.setBindingStatus(0);
            } else {
                respData.setCheckStatus(2);
            }
        } else {
            if (chargesEnabled.compareTo(1) == 0 && detailsSubmitted.compareTo(1) == 0) {
                respData.setCheckStatus(1);
                Locale locale = LocaleContextHolder.getLocale();
                log.info("local_message:{}", locale.getLanguage());
                String ok = MessageSourceHolder.getMessage("OK", new Object[]{}, locale);
                log.info("ok:{}", ok);
                respData.setAccountStatus(ok);
                //账户状态
                if (CollUtil.isNotEmpty(needProvidewarnrmationSet)) {
                    respData.setCheckComment(needProvidewarnrmationSet.toString());
                }
            } else {
                respData.setCheckStatus(0);
                if (CollUtil.isNotEmpty(mustProvidewarnrmationSet)) {
                    respData.setCheckComment(mustProvidewarnrmationSet.toString());
                }
            }
        }
        return Result.ofSucceed(respData);
    }

    @Override
    public Result<BindStripeResp> bindStripeAccount(SellerAccountReq sellerAccountReq) {
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String authorization = request.getHeader("Authorization");
        String sellerId = sellerAccountReq.getSellerId();
        if (StrUtil.isBlank(sellerId)) {
            return Result.ofFailed(LocaleResultUtil.result("400"));
        }
        PaymentEntity payment = paymentService.selectAccountIdBySellerIdAndCode(Long.valueOf(sellerId), stripeCode);
        if (payment == null) {
            payment = savePayment(sellerId);
        }
        log.info("bindStripeAccount##payment is {}", JSONUtil.toJsonStr(payment));
        BindStripeResp bindStripeResp = null;
        try {
            Long paymentId = payment.getId();
            DTStripeHeadDTO headDto = new DTStripeHeadDTO();
            headDto.setSource(source);
            headDto.setPayType(payType);
            DTStripeBodyDTO bodyDto = new DTStripeBodyDTO();
            bodyDto.setBindingId(paymentId);
            bodyDto.setAccountType(accountType);
            bodyDto.setReturnUrl(returnUrl);
            bodyDto.setCallBack(callBack);
            Map<Object, Object> map = new HashMap<>();
            Integer bindingType = sellerAccountReq.getBindingType();
            if (bindingType != null && bindingType.equals(1)) {
                map.put("bindingType", bindingType);
                String urlTemp = blankReturnUrl.substring(0, blankReturnUrl.lastIndexOf("/") + 1) + sellerId + "?Authorization=" + authorization;
                bodyDto.setReturnUrl(urlTemp);
            }
            String countryCode = sellerAccountReq.getCountryCode();
            if (!StringUtils.isEmpty(countryCode)) {
                map.put("countryCode", countryCode);
            }
            String countryAbbreviation = null;
            Result<SellerDetailVO> sellerDetail = pileMerchantUserFeign.detail(Long.valueOf(sellerId));
            if (sellerDetail != null && sellerDetail.getData() != null) {
                log.info("bindStripeAccount sellerDetail:{}", JSON.toJSONString(sellerDetail.getData()));
                countryAbbreviation = sellerDetail.getData().getCountryAbbreviation();
            }
            if (StringUtils.isEmpty(countryAbbreviation)) {
                UserCountryUnitInfoDto userCountryUnitInfoDto = userCenterFeign.getUnit(sellerId).getData();
                if (userCountryUnitInfoDto != null) {
                    countryAbbreviation = userCountryUnitInfoDto.getAlpha2Code();
                }
            }

            headDto.setCountry(countryAbbreviation);
            bodyDto.setParams(map);
            String sign = AutelSignUtil.getSigns(headDto, bodyDto);
            log.info("bodyDto:{}", JSON.toJSONString(bodyDto));
            bindStripeResp = payServerFeign.stripeAccountCreateV2(source, Integer.valueOf(payType), null, countryAbbreviation, request.getHeader("Accept-Language"), sign, bodyDto);
            if (bindStripeResp.getData() == null) {
                return Result.ofFailed(LocaleResultUtil.result("500"), bindStripeResp.getMessage());
            }
        } catch (HttpException e) {
            log.error("bindStripeAccount##error found", e);
        }
        log.info("bindStripeAccount##body is {}", bindStripeResp);
        payment.setEnableFlag(false);
        String jsonStr = JSONUtil.toJsonStr(bindStripeResp);
        log.info("bindStripeAccount##bindStripeResp is {}", jsonStr);
        return Result.ofSucceed(bindStripeResp);
    }

    private PaymentEntity savePayment(String sellerId) {
        PaymentEntity savePayment = new PaymentEntity();
        savePayment.setSellerId(Long.valueOf(sellerId));
        savePayment.setCode(1);
        savePayment.setEnableFlag(false);
        savePayment.setName("Stripe");
        return paymentService.saveReturnPojo(savePayment);
    }

    @Override
    public Result<Boolean> bindCallBack(SellerAccountReq
                                                sellerAccountReq) {
        log.info("bindCallBack" +
                "##sellerAccountReq is {}", JSONUtil.toJsonStr(sellerAccountReq));
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String requestSign = request.getHeader("autel-sign");
        log.info("##bindCallBack## requestSign is {}", requestSign);
        String sign = AutelSignUtil.getSigns(null, sellerAccountReq);
        log.info("##bindCallBack##sign## is {}", sign);
        assert sign != null;
        if (!sign.equals(requestSign)) {
            return Result.ofFailed(LocaleResultUtil.result("500"));
        }
        log.info("bindCallBack##回调成功");
        if (sellerAccountReq.getSellerId() != null) {
            PaymentEntity payment = new PaymentEntity();
            payment.setId(Long.valueOf(sellerAccountReq.getSellerId()));
            payment.setEnableFlag(sellerAccountReq.getChargesEnabled() > 0);
            Boolean booleanResult = paymentService.updateStripeStatusById(payment);
            log.info("bindCallBack##booleanResult is {}", booleanResult);
            PaymentEntity paymentEntity = paymentService.getById(Long.valueOf(sellerAccountReq.getSellerId()));
            Long sellerId = paymentEntity.getSellerId();
            SellerAccountReq req = new SellerAccountReq();
            req.setSellerId(String.valueOf(sellerId));
            Result<SellerAccountResp> sellerAccountRespResult = queryStripeAccount(req);
            MessageDTO<SellerAccountResp> msg = new MessageDTO<>();
            msg.setSeq(System.currentTimeMillis() + "");
            SellerAccountResp resp = sellerAccountRespResult.getData();
            Integer checkStatus = resp.getCheckStatus();
            if (checkStatus == null) {
                resp.setCheckStatus(4);
            }
            msg.setData(resp);
            msg.setCmd(MessageDTO.CmdEnum.STRIPE_BIND_INFO.getValue());
            SendMsgDTO sendMsgDto = new SendMsgDTO();
            sendMsgDto.setReceiver(String.valueOf(sellerId));
            sendMsgDto.setMsg(msg.toString());
            wsCoreClient.sendMsg(sendMsgDto);
            log.info("sendMsgDto:{}", JSON.toJSONString(sendMsgDto));
        } else {
            return Result.ofFailed(LocaleResultUtil.result("500"));
        }
        return Result.ofSucceed(true);
    }

    @Override
    public Result<UnBindStripeResp> unBindStripeAccount(SellerAccountReq sellerAccountReq) {

        String sellerId = sellerAccountReq.getSellerId();
        if (StrUtil.isBlank(sellerId)) {
            return Result.ofFailed(LocaleResultUtil.result("400"));
        }
        PaymentEntity payment = paymentService.selectAccountIdBySellerIdAndCode(Long.valueOf(sellerId), stripeCode);
        log.info("queryStripeAccount##payment is {}", JSONUtil.toJsonStr(payment));
        UnBindStripeResp unBindStripeResp = null;
        try {
            Long paymentId = payment.getId();
        /*
        DELETE /api/base-pay/unified /account? bindingId=
         */
            String countryAbbreviation = null;
            Result<SellerDetailVO> sellerDetail = pileMerchantUserFeign.detail(Long.valueOf(sellerId));
            if (sellerDetail != null && sellerDetail.getData() != null) {
                log.info("bindStripeAccount sellerDetail:{}", JSON.toJSONString(sellerDetail.getData()));
                countryAbbreviation = sellerDetail.getData().getCountryAbbreviation();
            }
            //获取sign
            DTStripeHeadDTO headDto = new DTStripeHeadDTO();
            headDto.setSource(source);
            headDto.setPayType(payType);
            headDto.setCountry(countryAbbreviation);
            DTStripeBodyDTO bodyDto = new DTStripeBodyDTO();
            bodyDto.setBindingId(paymentId);
            String sign = AutelSignUtil.getSigns(headDto, bodyDto);

            unBindStripeResp = payServerFeign.stripeAccountDeleteV2(source, Integer.valueOf(payType), null, countryAbbreviation, sign, String.valueOf(paymentId));
        } catch (HttpException e) {
            log.error("unBindStripeAccount##error found", e);
        }
        String jsonStr = JSONUtil.toJsonStr(unBindStripeResp);
        log.info("unBindStripeAccount##unBindStripeResp is {}", jsonStr);
        if (!"true".equals(unBindStripeResp.getData().toString())) {
            Result r = new Result();
            r.setCode(unBindStripeResp.getCode());
            r.setMessage(unBindStripeResp.getMessage());
            return r;
        }
        return Result.ofSucceed(unBindStripeResp);
    }


    @Override
    public Result<BindStripeResp> updateStripeAccount(SellerAccountReq sellerAccountReq) {
        String sellerId = sellerAccountReq.getSellerId();
        if (StrUtil.isBlank(sellerId)) {
            return Result.ofFailed(LocaleResultUtil.result("400"));
        }
        PaymentEntity payment = paymentService.selectAccountIdBySellerIdAndCode(Long.valueOf(sellerId), stripeCode);
        log.info("updateStripeAccount##payment is {}", JSONUtil.toJsonStr(payment));
        BindStripeResp bindStripeResp = null;
        try {
            Long paymentId = payment.getId();
        /*
            POST /api/base-pay/unified/account/link
         */

            String countryAbbreviation = null;
            Result<SellerDetailVO> sellerDetail = pileMerchantUserFeign.detail(Long.valueOf(sellerId));
            if (sellerDetail != null && sellerDetail.getData() != null) {
                log.info("bindStripeAccount sellerDetail:{}", JSON.toJSONString(sellerDetail.getData()));
                countryAbbreviation = sellerDetail.getData().getCountryAbbreviation();
            }

            //获取sign
            DTStripeHeadDTO headDto = new DTStripeHeadDTO();
            headDto.setSource(source);
            headDto.setPayType(payType);
            headDto.setCountry(countryAbbreviation);
            DTStripeBodyDTO bodyDto = new DTStripeBodyDTO();
            bodyDto.setBindingId(paymentId);
            bodyDto.setReturnUrl(returnUrl);
            String sign = AutelSignUtil.getSigns(headDto, bodyDto);
            //todo 添加用户国家
            bindStripeResp = payServerFeign.stripeAccountLinkCreateV2(source, Integer.valueOf(payType), null, countryAbbreviation, sign, bodyDto);
        } catch (HttpException e) {
            log.error("updateStripeAccount##error found", e);
        }
        log.info("updateStripeAccount##bindStripeResp is {}", JSONUtil.toJsonStr(bindStripeResp));
        String jsonStr = JSONUtil.toJsonStr(bindStripeResp);
        log.info("updateStripeAccount##bindStripeResp is {}", jsonStr);
        return Result.ofSucceed(bindStripeResp);
    }

    @Override
    public Result<Boolean> bindWXAccount(SellerWXAccountReq sellerWXAccountReq) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        if (payload == null || payload.getSellerId() == null) {
            throw new IllegalArgumentException("bad request, can not get sellerId.");
        }

        Long sellerId = payload.getSellerId();
        Long creator = payload.getUserId();

        /*
         * V2.4 版本需求
         * 1. 判断当前账号是否存在，如果不存在，直接绑定；
         * 2. 如果已经存在，先解绑原有商户号，在绑定新的商户号；
         * */

        // 先查询是否存在，如果存在，更新；不存在，insert
        LambdaQueryWrapper<TbSellerWxAccEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbSellerWxAccEntity::getSellerId, sellerId);
        queryWrapper.eq(TbSellerWxAccEntity::getStatus, 1);
        //queryWrapper.ne(TbSellerWxAccEntity::getAccount, sellerWXAccountReq.getAccount());

        List<TbSellerWxAccEntity> tbSellerWxAccEntities = sellerWxAccMapper.selectList(queryWrapper);

        if (CollectionUtils.isEmpty(tbSellerWxAccEntities)) {
            // 未绑定过，直接绑定
            // 1. 调用微信绑定接口
            BindWxAccountReq bindWxAccountReq = new BindWxAccountReq();
            bindWxAccountReq.setAccount(sellerWXAccountReq.getAccount());
            bindWxAccountReq.setAppId(OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey());
            bindWxAccountReq.setRelationType("CUSTOM");
            bindWxAccountReq.setCustomRelation("场站主");
            bindWxAccountReq.setName(sellerWXAccountReq.getName());
            bindWxAccountReq.setType("MERCHANT_ID");

            PayHeaderDTO payHeaderDTO = new PayHeaderDTO();
            payHeaderDTO.setPayType("6");
            payHeaderDTO.setSource("energy");

            String autelSign = AutelSignUtil.getSign(payHeaderDTO, bindWxAccountReq);
            log.info("--->>> bindWXAccount sign：autelSign-：{}, 支付请求体header:{},支付请求体：jsonObject-：{}", autelSign, JSON.toJSONString(payHeaderDTO), JSON.toJSONString(bindWxAccountReq));

            Result<WxPayAddReceiverResponse> result = payServerFeign.bindWxAccount("energy", 6, autelSign, null, null, bindWxAccountReq);
            log.info("--->>> bindWXAccount, current active is zero, accountNo:{}, bind result:{}", sellerWXAccountReq.getAccount(), result);

            if (result.getCode() == 0) {
                // 成功，插入
                log.info("--->>>bindWXAccount, accountNo:{} bind successful.", sellerWXAccountReq.getAccount());

                TbSellerWxAccEntity tbSellerWxAccEntity = new TbSellerWxAccEntity();
                tbSellerWxAccEntity.setId(IdWorker.getId());
                tbSellerWxAccEntity.setSellerId(sellerId);
                tbSellerWxAccEntity.setAccount(sellerWXAccountReq.getAccount());
                tbSellerWxAccEntity.setAccountType("MERCHANT_ID");
                tbSellerWxAccEntity.setAccountName(sellerWXAccountReq.getName());
                tbSellerWxAccEntity.setStatus(1);
                tbSellerWxAccEntity.setRemark("场站主");
                tbSellerWxAccEntity.setCreateBy(String.valueOf(creator));
                tbSellerWxAccEntity.setCreateTime(System.currentTimeMillis());
                tbSellerWxAccEntity.setUpdateTime(tbSellerWxAccEntity.getCreateTime());

                sellerWxAccMapper.insert(tbSellerWxAccEntity);
            } else {
                // 失败了。
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST);
            }
        } else {
            //  先判断需要绑定的微信号，是否是当前生效中的微信号，如果是，直接返回。
            TbSellerWxAccEntity tbSellerWxAccEntityBefore = tbSellerWxAccEntities.get(0);
            if (tbSellerWxAccEntityBefore.getAccount().equals(sellerWXAccountReq.getAccount())) {
                log.info("--->>>> bindWXAccount, current bind account:{} is active.", sellerWXAccountReq.getAccount());
                return Result.ofSucceed(Boolean.TRUE);
            }


            // 1. 调用微信绑定接口
            BindWxAccountReq bindWxAccountReq = new BindWxAccountReq();
            bindWxAccountReq.setAccount(sellerWXAccountReq.getAccount());
            bindWxAccountReq.setAppId(OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey());
            bindWxAccountReq.setRelationType("CUSTOM");
            bindWxAccountReq.setCustomRelation("场站主");
            bindWxAccountReq.setName(sellerWXAccountReq.getName());
            bindWxAccountReq.setType("MERCHANT_ID");

            PayHeaderDTO payHeaderDTO = new PayHeaderDTO();
            payHeaderDTO.setPayType("6");
            payHeaderDTO.setSource("energy");

            String autelSign = AutelSignUtil.getSign(payHeaderDTO, bindWxAccountReq);
            log.info("--->>> bindWXAccount sign：autelSign-：{}, 支付请求体header:{},支付请求体：jsonObject-：{}", autelSign, JSON.toJSONString(payHeaderDTO), JSON.toJSONString(bindWxAccountReq));

            Result<WxPayAddReceiverResponse> result = payServerFeign.bindWxAccount("energy", 6, autelSign, null, null, bindWxAccountReq);
            log.info("--->>> bindWXAccount, current active is not null, create new first, accountNo:{}, bind result:{}", sellerWXAccountReq.getAccount(), result);

            // 3. 绑定成功，将当前账号 set status = 1， 其他账号 set status = 0；
            if (result.getCode() == 0) {
                // 1. 先添加 status=0 的记录
                TbSellerWxAccEntity tbSellerWxAccEntity = new TbSellerWxAccEntity();
                tbSellerWxAccEntity.setId(IdWorker.getId());
                tbSellerWxAccEntity.setSellerId(sellerId);
                tbSellerWxAccEntity.setAccount(sellerWXAccountReq.getAccount());
                tbSellerWxAccEntity.setAccountType("MERCHANT_ID");
                tbSellerWxAccEntity.setAccountName(sellerWXAccountReq.getName());
                tbSellerWxAccEntity.setRemark("场站主");
                tbSellerWxAccEntity.setStatus(1);
                tbSellerWxAccEntity.setCreateBy(String.valueOf(creator));
                tbSellerWxAccEntity.setCreateTime(System.currentTimeMillis());
                tbSellerWxAccEntity.setUpdateTime(tbSellerWxAccEntity.getCreateTime());

                sellerWxAccMapper.insert(tbSellerWxAccEntity);

                /*
                 * 0: 新建
                 * 1: 可用
                 * 2：解绑，不可用
                 * */
                // 更新之前的状态为2
                LambdaUpdateWrapper<TbSellerWxAccEntity> updateWrapper = Wrappers.lambdaUpdate();
                updateWrapper.eq(TbSellerWxAccEntity::getSellerId, sellerId);
                updateWrapper.ne(TbSellerWxAccEntity::getAccount, sellerWXAccountReq.getAccount());

                TbSellerWxAccEntity updateEntity = new TbSellerWxAccEntity();
                updateEntity.setStatus(2);
                updateEntity.setUpdateTime(System.currentTimeMillis());

                sellerWxAccMapper.update(updateEntity, updateWrapper);

                // 4. 绑定成功，解绑之前的账号。每个商户，生效中的只有一个


                UnBindWxAccountReq unBindWxAccountReq = new UnBindWxAccountReq();
                unBindWxAccountReq.setAccount(tbSellerWxAccEntityBefore.getAccount());
                unBindWxAccountReq.setType("MERCHANT_ID");
                unBindWxAccountReq.setAppId(OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey());

                PayHeaderDTO payHeaderDTOUnbind = new PayHeaderDTO();
                payHeaderDTOUnbind.setPayType("6");
                payHeaderDTOUnbind.setSource("energy");

                String autelSignUnbind = AutelSignUtil.getSign(payHeaderDTOUnbind, unBindWxAccountReq);
                log.info("--->>> unBindWXAccount sign：autelSign-：{}, 支付请求体header:{},支付请求体：jsonObject-：{}", autelSignUnbind, JSON.toJSONString(payHeaderDTOUnbind), JSON.toJSONString(unBindWxAccountReq));

                Result<WxPayDeleteReceiverResponse> resultUnbind = payServerFeign.unBindWxAccount("energy", 6, autelSignUnbind, null, null, unBindWxAccountReq);
                log.info("--->>> bind success, unBindWXAccount, accountNo:{}, unBind result:{}", unBindWxAccountReq.getAccount(), resultUnbind);

                if (resultUnbind.getCode() != 0) {
                    log.warn("--->>> bindWXAccount, accountNo:{} call pay-server unBind interface failed.", unBindWxAccountReq.getAccount());
                }

            } else {
                log.info("--->>> --->>> bindWXAccount, accountNo:{} bind failed. ", sellerWXAccountReq.getAccount());
                // 返回修改失败
                return Result.ofFailed(HttpCodeEnum.BAD_REQUEST, result.getMessage());
            }

          /*  TbSellerWxAccEntity tbSellerWxAccEntity = tbSellerWxAccEntities.get(0);
            tbSellerWxAccEntity.setAccount(sellerWXAccountReq.getAccount());
            tbSellerWxAccEntity.setUpdateBy(String.valueOf(creator));
            tbSellerWxAccEntity.setUpdateTime(System.currentTimeMillis());
            sellerWxAccMapper.updateById(tbSellerWxAccEntity);*/
        }

        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<String> queryBindWXAccount(Long sellerId) {

        if (sellerId == null) {
            if (LoginUserHolder.getLoginUser() == null || LoginUserHolder.getLoginUser().getPayload() == null || LoginUserHolder.getLoginUser().getPayload().getSellerId() == null) {
                throw new IllegalArgumentException("bad request, can not get sellerId.");
            }
            sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        }

        LambdaQueryWrapper<TbSellerWxAccEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbSellerWxAccEntity::getSellerId, sellerId);
        queryWrapper.eq(TbSellerWxAccEntity::getStatus, 1);

        List<TbSellerWxAccEntity> tbSellerWxAccEntities = sellerWxAccMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbSellerWxAccEntities)) {
            return Result.ofSucceed(null);
        } else {
            return Result.ofSucceed(tbSellerWxAccEntities.get(0).getAccount());
        }
    }

    @Override
    public Result<String> queryCurrentEnv() {
        EnvironmentEnum environmentEnum = EnvironmentEnum.queryEnvEnumByNs(nacosNs);
        if (environmentEnum != null) {
            String serverCode = environmentEnum.getServerCode();
            String code = serverCode.split("-")[0];
            return Result.ofSucceed(code);
        } else {
            return Result.ofSucceed();
        }

       /* if (LoginUserHolder.getLoginUser() != null
                && LoginUserHolder.getLoginUser().getPayload() != null
                && LoginUserHolder.getLoginUser().getPayload().getSellerId() != null) {
            Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();

            Result<SellerDetailVO> detail = pileMerchantUserFeign.detail(sellerId);

            if (detail == null || detail.getData() == null || org.apache.commons.lang.StringUtils.isEmpty(detail.getData().getCountryAbbreviation())) {
                throw new MessageCodeException(PileUserEnum.CC_ERROR);
            }

            //根据国家码拿到区号
            Result<String> serverCodeResult = baseAdminFeign.getCountryInfoByAlpha2Code(detail.getData().getCountryAbbreviation());
            if(serverCodeResult == null || com.baomidou.mybatisplus.core.toolkit.StringUtils.isBlank(serverCodeResult.getData())) {
                throw new MessageCodeException(PileUserEnum.CC_ERROR);
            }

            return Result.ofSucceed(serverCodeResult.getData());
        } else {
            return Result.ofSucceed();
        }*/
    }

    @Override
    public Result<WxPayDeleteReceiverResponse> unBindWXAccount(UnBindWxAccountReq unBindWxAccountReq) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        if (payload == null || payload.getSellerId() == null) {
            throw new IllegalArgumentException("bad request, can not get sellerId.");
        }

        Long sellerId = payload.getSellerId();

        unBindWxAccountReq.setAppId(OrderSourceIdEnum.ORDER_SOURCE_PLATFORM_CHARGE.getKey());
        unBindWxAccountReq.setType("MERCHANT_ID");

        PayHeaderDTO payHeaderDTOUnbind = new PayHeaderDTO();
        payHeaderDTOUnbind.setPayType("6");
        payHeaderDTOUnbind.setSource("energy");

        String autelSignUnbind = AutelSignUtil.getSign(payHeaderDTOUnbind, unBindWxAccountReq);
        log.info("--->>> unBindWXAccount sign：autelSign-：{}, 支付请求体header:{},支付请求体：jsonObject-：{}", autelSignUnbind, JSON.toJSONString(payHeaderDTOUnbind), JSON.toJSONString(unBindWxAccountReq));


        Result<WxPayDeleteReceiverResponse> resultUnbind = payServerFeign.unBindWxAccount("energy", 6, autelSignUnbind, null, null, unBindWxAccountReq);
        log.info("--->>> unBindWXAccount, accountNo:{}, unBind result:{}", unBindWxAccountReq.getAccount(), resultUnbind);

        if (resultUnbind.getCode() == 0) {
            LambdaUpdateWrapper<TbSellerWxAccEntity> updateWrapper = Wrappers.lambdaUpdate();
            updateWrapper.eq(TbSellerWxAccEntity::getSellerId, sellerId);
            updateWrapper.eq(TbSellerWxAccEntity::getAccount, unBindWxAccountReq.getAccount());

            TbSellerWxAccEntity updateEntity = new TbSellerWxAccEntity();
            updateEntity.setStatus(2);
            updateEntity.setUpdateTime(System.currentTimeMillis());

            sellerWxAccMapper.update(updateEntity, updateWrapper);
            resultUnbind.setCode(com.autel.cloud.tariff.constant.HttpStatus.SUCCESS);
        }
        return resultUnbind;
    }

    @Override
    public Result<WxAccountInfoResp> queryBindWXAccountV2(Long sellerId) {
        if (sellerId == null) {
            if (LoginUserHolder.getLoginUser() == null || LoginUserHolder.getLoginUser().getPayload() == null || LoginUserHolder.getLoginUser().getPayload().getSellerId() == null) {
                throw new IllegalArgumentException("bad request, can not get sellerId.");
            }
            sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        }

        LambdaQueryWrapper<TbSellerWxAccEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbSellerWxAccEntity::getSellerId, sellerId);
        queryWrapper.eq(TbSellerWxAccEntity::getStatus, 1);

        List<TbSellerWxAccEntity> tbSellerWxAccEntities = sellerWxAccMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbSellerWxAccEntities)) {
            return Result.ofSucceed(null);
        } else {
            WxAccountInfoResp resp = new WxAccountInfoResp();
            resp.setName(tbSellerWxAccEntities.get(0).getAccountName());
            resp.setAccount(tbSellerWxAccEntities.get(0).getAccount());
            return Result.ofSucceed(resp);
        }
    }

    @Override
    public Result<Integer> queryPayType() {
        Integer payType = PayTypeEnum.STRIPE.getValue();

        if (advancePaymentEnable) {
            payType = PayTypeEnum.WE_CHAT.getValue();
        }
        return Result.ofSucceed(payType);
    }
}
