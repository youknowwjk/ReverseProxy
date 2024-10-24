package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.constant.BindCardConstant;
import com.autel.cloud.pile.base.domain.convert.PaymentCardConvert;
import com.autel.cloud.pile.base.domain.repository.PaymentCardRepository;
import com.autel.cloud.pile.base.domain.service.PaymentCardService;
import com.autel.cloud.pile.base.dto.CardManagerDTO;
import com.autel.cloud.pile.base.dto.CustomerDTO;
import com.autel.cloud.pile.base.dto.MemberCustomerDTO;
import com.autel.cloud.pile.base.dto.PaymentCardManagerDTO;
import com.autel.cloud.pile.base.dto.app.UserCountryUnitInfoDto;
import com.autel.cloud.pile.base.dto.pay.PaymentMethodResp;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.feign.BaseAdminClient;
import com.autel.cloud.pile.base.infrastructure.feign.PayServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.UserCenterFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TBCardManageEntity;
import com.autel.cloud.pile.base.infrastructure.util.AutelSignUtil;
import com.autel.cloud.pile.base.vo.CardManagerV2VO;
import com.autel.cloud.pile.base.vo.CardManagerVO;
import com.autel.cloud.pile.base.vo.CustomerVO;
import com.autel.cloud.pile.bill.enums.CurrencyTypeEnum;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.EnergyBillVO;
import com.autel.cloud.pile.bill.vo.TbReservationEntityVO;
import com.autel.cloud.tariff.dto.CurrencyDTO;
import com.autel.cloud.tariff.feign.CurrencyFeignClient;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpStatus;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;


@Service
@Slf4j
public class PaymentCardServiceImpl implements PaymentCardService {

    @Autowired
    private PileUserServiceFeign pileUserServiceFeign;
    @Autowired
    private AutelSignUtil autelSignUtil;
    @Autowired
    private PayServiceFeign payServiceFeign;
    @Autowired
    private IBillFeignClient billServiceFeign;
    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String, Object> redisTemplate;

    private final PaymentCardRepository paymentCardRepository;

    @Resource
    private BaseAdminClient baseAdminClient;

    @Resource
    private CurrencyFeignClient currencyFeignClient;

    @Resource
    private UserCenterFeign userCenterFeign;

    private String nacosNameSpace = System.getenv("NACOS_NS");

    public PaymentCardServiceImpl(PaymentCardRepository paymentCardRepository) {
        this.paymentCardRepository = paymentCardRepository;
    }

    @Override
    public Result<PaymentCardManagerDTO> queryPaymentCardDetail(PaymentCardManagerDTO cardManagerDTO) {
        TBCardManageEntity tbCardManageEntity = paymentCardRepository.queryPaymentCardDetail(cardManagerDTO);
        return Result.ofSucceed(PaymentCardConvert.paymentCardManagerDTO(tbCardManageEntity));
    }

    @Override
    @Transactional
    public CardManagerV2VO bindCard(CardManagerDTO cardManagerDTO) {
        log.info("bindCard,cardManagerDTO={}", cardManagerDTO);
        CardManagerV2VO cardManagerV2VO = new CardManagerV2VO();

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        Boolean isChargMinAp = jwtInfo != null && !StringUtils.isEmpty(jwtInfo.getAppId()) && "ChargMinAp".equals(jwtInfo.getAppId());
        log.info("bindCard isChargMinAp : {}", isChargMinAp);

        //判断卡是否已被绑定
        List<TBCardManageEntity> cardList = paymentCardRepository.list(new LambdaQueryWrapper<TBCardManageEntity>()
                .eq(TBCardManageEntity::getUserId, cardManagerDTO.getUserId())
                .eq(TBCardManageEntity::getCardLastFourNo, cardManagerDTO.getCardLastFourNo())
                .eq(TBCardManageEntity::getDeleted, 0));
        log.info("bindCard,cardList={}", JSON.toJSONString(cardList));
        if (org.apache.commons.collections.CollectionUtils.isNotEmpty(cardList)) {
            if(isChargMinAp){
                cardManagerV2VO.setPaymentId(cardList.get(0).getPaymentId());
                cardManagerV2VO.setPaymentIdStr(String.valueOf(cardList.get(0).getPaymentId()));
                cardManagerV2VO.setResult(true);
                return cardManagerV2VO;
            }

            throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_EXIST);
        }


        if(!isChargMinAp){
            /**
             *  1. 是否存在未支付的订单；
             *  2. 当前已经绑定的客户是否达到5个；
             *  3. 调用冻结接口，回调后写入redis查询；
             *  4. 冻结成功后，继续后面绑定逻辑；
             *  5. 异步解冻；
             */
            bindConditionCheck(cardManagerDTO);
        }

        TBCardManageEntity insertEntity = new TBCardManageEntity();
        BeanUtils.copyProperties(cardManagerDTO, insertEntity);
        insertEntity.setCc(cardManagerDTO.getCountryCode());
        insertEntity.setPhoneNumber(cardManagerDTO.getPhone());
        insertEntity.setAddress(cardManagerDTO.getAddress());
        int bindCount = paymentCardRepository.count(new LambdaQueryWrapper<TBCardManageEntity>()
                .eq(TBCardManageEntity::getUserId, cardManagerDTO.getUserId())
                .eq(TBCardManageEntity::getDefaultCard, 1)
                .eq(TBCardManageEntity::getDeleted, 0));
        log.info("bindCard,bindCount={}", bindCount);
        if (bindCount == 0) {
            //设置默认卡
            cardManagerDTO.setDefaultCard(1);
        }
        String alpha2Code = cardManagerDTO.getAlpha2Code();
        if(StringUtils.isEmpty(alpha2Code) && LoginUserHolder.getLoginUser() != null){
            alpha2Code = LoginUserHolder.getLoginUser().getAlpha2Code();
        }
        if(StringUtils.isEmpty(alpha2Code)){
            alpha2Code = "US";
        }
        MemberCustomerDTO memberCustomerDTO = pileUserServiceFeign.findCustomer(cardManagerDTO.getUserId()).getData();
        log.info("bindCard,memberCustomerDTO={}", memberCustomerDTO);
        if (ObjectUtils.isEmpty(memberCustomerDTO) || ObjectUtils.isEmpty(memberCustomerDTO.getCustomerId())) {
            //当前用户不存在customer
            //生成加密信息
            Map<String, Object> message = new HashMap<>();
            message.put("paymentMethodId", cardManagerDTO.getPaymentMethodId());
            message.put(BaseConstant.SOURCE, BaseConstant.ENERGY);
            message.put(BaseConstant.PAY_TYPE, 4);
            message.put(BaseConstant.COUNTRY, alpha2Code);
            message.put("phone", cardManagerDTO.getPhone());
            String signMessage = AutelSignUtil.getSign(message);
            CustomerDTO customerDTO = new CustomerDTO();
            customerDTO.setPaymentMethodId(cardManagerDTO.getPaymentMethodId());
            customerDTO.setPhone(cardManagerDTO.getPhone());
            log.info("bindCard,signMessage={},customerDTO={}", signMessage, JSON.toJSONString(customerDTO));
            //调pay服务添加客户及绑定支付方法
            CustomerVO customerVO = payServiceFeign.addCustomer(BaseConstant.ENERGY, 4, null, alpha2Code, signMessage, customerDTO).getData();
            log.info("bindCard,customerVO={}", customerVO);
            if (Objects.isNull(customerVO) || ObjectUtils.isEmpty(customerVO.getCustomer()) || ObjectUtils.isEmpty(customerVO.getPaymentMethod())) {
                throw new MessageCodeException(PileBaseEnum.ADD_MEMBER_CUSTOMER_FAIL);
            }
            if (ObjectUtils.isEmpty(memberCustomerDTO)) {
                memberCustomerDTO = new MemberCustomerDTO();
            }
            Long customer = customerVO.getCustomer();
            Long paymentMethod = customerVO.getPaymentMethod();
            memberCustomerDTO.setUserId(cardManagerDTO.getUserId());
            memberCustomerDTO.setCustomerId(customer);
            log.info("bindCard,memberCustomerDTO={}", JSON.toJSONString(memberCustomerDTO));
            //user服务保存用户与客户记录
            Boolean insertCustomer = pileUserServiceFeign.insert(memberCustomerDTO).getData();
            log.info("bindCard,insertCustomer={}", insertCustomer);
            insertEntity.setPaymentId(paymentMethod);
            cardManagerV2VO.setPaymentId(paymentMethod);
            cardManagerV2VO.setPaymentIdStr(String.valueOf(paymentMethod));
            //本地保存绑定支付方法
            boolean save = paymentCardRepository.save(insertEntity);
            log.info("bindCard,save={}", save);
            if (cardManagerDTO.getDefaultCard() == 1) {
                //设置默认卡
                cardManagerDTO.setPaymentId(paymentMethod);
                this.setDefaultCard(cardManagerDTO);
            }
        } else {
            //客户已存在，调pay绑定支付方法
            //生成加密信息
            Map<String, Object> params = new HashMap<>();
            params.put("customer", memberCustomerDTO.getCustomerId());
            params.put("paymentMethodId", cardManagerDTO.getPaymentMethodId());
            params.put(BaseConstant.PAY_TYPE, 4);
            params.put(BaseConstant.SOURCE, BaseConstant.ENERGY);
            params.put(BaseConstant.COUNTRY, alpha2Code);
            String signMessage = AutelSignUtil.getSign(params);
            CustomerDTO customerDTO = new CustomerDTO();
            customerDTO.setCustomer(memberCustomerDTO.getCustomerId());
            customerDTO.setPaymentMethodId(cardManagerDTO.getPaymentMethodId());
            log.info("executeBindCard,signMessage={},customerDTO={}", signMessage, customerDTO);
            CustomerVO customerVO = payServiceFeign.attachPaymentMethod(BaseConstant.ENERGY, 4, null, alpha2Code, signMessage, customerDTO).getData();
            log.info("executeBindCard,customerVO={}", customerVO);
            if (ObjectUtils.isEmpty(customerVO) || ObjectUtils.isEmpty(customerVO.getPaymentMethod())) {
                throw new MessageCodeException(PileBaseEnum.ATTACH_PAYMENTMETHOD_FAIL);
            }
            Long paymentMethod = customerVO.getPaymentMethod();
            insertEntity.setPaymentId(paymentMethod);
            cardManagerV2VO.setPaymentId(paymentMethod);
            cardManagerV2VO.setPaymentIdStr(String.valueOf(paymentMethod));
            //本地保存绑定支付方法
            boolean save = paymentCardRepository.save(insertEntity);
            log.info("executeBindCard,save={}", save);
            if (cardManagerDTO.getDefaultCard() == 1 && save) {
                //设置默认卡
                cardManagerDTO.setPaymentId(paymentMethod);
                this.setDefaultCard(cardManagerDTO);
            }
        }
        cardManagerV2VO.setResult(true);
        return cardManagerV2VO;
    }

    @Override
    public Result<CardManagerV2VO> bindCardV2(CardManagerDTO cardManagerDTO) {
        Long userId = LoginUserHolder.getLoginUser().getId();

        CardManagerV2VO cardManagerV2VO = new CardManagerV2VO();
        CardManagerV2VO bindResult = bindCard(cardManagerDTO);
        cardManagerV2VO.setResult(bindResult.getResult());
        cardManagerV2VO.setPaymentId(bindResult.getPaymentId());
        cardManagerV2VO.setPaymentIdStr(String.valueOf(bindResult.getPaymentId()));
        //获取用户的货币信息
        UserCountryUnitInfoDto userCountryUnitInfoDto = userCenterFeign.getUnit(String.valueOf(userId)).getData();

        String currencySign = "€";
        String currencyType = "EUR";
        Integer currencyCode = 978;

        if (!ObjectUtils.isEmpty(userCountryUnitInfoDto)) {
            if (null != userCountryUnitInfoDto.getMonetaryCode()) {
                CurrencyDTO currencyDTO = new CurrencyDTO();
                currencyDTO.setCode(Integer.valueOf(userCountryUnitInfoDto.getMonetaryCode()));
                Result<CurrencyDTO> currencyDTOResult = currencyFeignClient.getCurrencyDetail(currencyDTO);
                log.info("========== the currencyDTOResult of the getCurrencyDetail invoked:{}", JSON.toJSONString(currencyDTOResult));
                if (null != currencyDTOResult && HttpStatus.SC_OK == currencyDTOResult.getCode()) {
                    currencySign = currencyDTOResult.getData().getCurrencySign();
                    currencyType = currencyDTOResult.getData().getCurrencyType();
                    currencyCode = currencyDTOResult.getData().getCode();
                }
            }
        }
        cardManagerV2VO.setCurrencyCode(currencyCode);
        cardManagerV2VO.setCurrencySign(currencySign);
        cardManagerV2VO.setCurrencyType(currencyType);
        BigDecimal minimumPrice = BigDecimal.valueOf(CurrencyTypeEnum.getCurrencyInfo(currencyType));
        if (BigDecimal.ZERO.compareTo(minimumPrice) >= 0) {
            minimumPrice = BigDecimal.valueOf(1);
        }
        cardManagerV2VO.setMinimumPrice(minimumPrice.multiply(new BigDecimal("2")));
        return Result.ofSucceed(cardManagerV2VO);
    }

    @Override
    public Result<List<CardManagerVO>> bindCardList(Long userId) {
        log.info("bindCardList,userId={}", userId);
        List<CardManagerVO> datas = new ArrayList<>();
        List<TBCardManageEntity> resultList = paymentCardRepository.list(new LambdaQueryWrapper<TBCardManageEntity>()
                .eq(TBCardManageEntity::getUserId, userId)
                .eq(TBCardManageEntity::getDeleted, 0)
                .orderByDesc(TBCardManageEntity::getDefaultCard)
                .orderByAsc(TBCardManageEntity::getCreateTime));
        log.info("bindCardList,resultList={}", resultList);
        if (!CollectionUtils.isEmpty(resultList)) {
            LocalDate date = LocalDate.now();
            int year = date.getYear();
            int month = date.getMonthValue();
            resultList.stream().forEach(tbCardManageEntity -> {
                CardManagerVO cardManagerVO = new CardManagerVO();
                BeanUtils.copyProperties(tbCardManageEntity, cardManagerVO);
                // 触发当前接口时，查询fingerprint
                if (org.apache.commons.lang3.StringUtils.isBlank(tbCardManageEntity.getFingerprint())) {
                    log.info("--->>> bindCardList, paymentId:{} fingerprint is null. ", tbCardManageEntity.getPaymentId());
                    try {
                        Result<PaymentMethodResp> paymentMethodResult = payServiceFeign.retrievePaymentMethod(String.valueOf(tbCardManageEntity.getPaymentId()), null, "energy", 4, null, null);
                        if (paymentMethodResult.getCode() == 0) {
                            PaymentMethodResp paymentMethodResp = paymentMethodResult.getData();
                            TBCardManageEntity updateEntity = new TBCardManageEntity();
                            updateEntity.setId(tbCardManageEntity.getId());
                            updateEntity.setUpdateTime(System.currentTimeMillis());
                            updateEntity.setFingerprint(paymentMethodResp.getFingerprint());
                            updateEntity.setExpYear(paymentMethodResp.getExpYear());
                            updateEntity.setExpMonth(paymentMethodResp.getExpMonth());
                            paymentCardRepository.updateById(updateEntity);

                            tbCardManageEntity.setExpMonth(paymentMethodResp.getExpMonth());
                            tbCardManageEntity.setExpYear(paymentMethodResp.getExpYear());
                        }
                    } catch (Exception e) {
                        log.info("--->>> retrievePaymentMethod error, error paymentId:{}", tbCardManageEntity.getPaymentId(), e);
                    }
                }

                // 设置过期标识
                if (tbCardManageEntity.getExpYear() != null && tbCardManageEntity.getExpMonth() != null
                        && (year > tbCardManageEntity.getExpYear() || (year == tbCardManageEntity.getExpYear() && month >= tbCardManageEntity.getExpMonth()))) {
                    cardManagerVO.setExpired(Boolean.TRUE);
                } else {
                    cardManagerVO.setExpired(Boolean.FALSE);
                }

                datas.add(cardManagerVO);
            });
        }
        log.info("bindCardList,datas={}", datas);
        return Result.ofSucceed(datas);
    }

    @Override
    @Transactional
    public Result<Boolean> unbindCard(CardManagerDTO cardManagerDTO) {
        log.info("unbindCard,cardManagerDTO={}", cardManagerDTO);
        //判断卡是否存在
        TBCardManageEntity oneEntity = paymentCardRepository.getOne(new LambdaQueryWrapper<TBCardManageEntity>()
                .select(TBCardManageEntity::getId, TBCardManageEntity::getDefaultCard)
                .eq(TBCardManageEntity::getPaymentId, cardManagerDTO.getPaymentId())
                .eq(TBCardManageEntity::getUserId, cardManagerDTO.getUserId())
                .eq(TBCardManageEntity::getDeleted, 0)
                .last("limit 1"));
        log.info("unbindCard,oneEntity={}", oneEntity);
        if (Objects.isNull(oneEntity)) {
            throw new MessageCodeException(PileBaseEnum.CARD_NOT_EXIST);
        }
        //是否存在未完成订单，充电中默认卡不能删除
        EnergyBillVO billStatusVO = billServiceFeign.getUnfinishedBillByUserId(cardManagerDTO.getUserId().toString(), DeviceTypeEnum.BUSINESS_PILE.getValue()).getData();
        log.info("unbindCard,billStatusVO={}", JSON.toJSONString(billStatusVO));
        if (Objects.nonNull(billStatusVO)) {
            throw new MessageCodeException(PileBaseEnum.NOT_DONE_ORDER_EXIST);
        }

        Result<String>  unfinishAdvancePaymentResult = billServiceFeign.getUnfinishAdvancePayment(cardManagerDTO.getUserId());
        if (org.apache.commons.lang3.ObjectUtils.isNotEmpty(unfinishAdvancePaymentResult.getData())) {
            throw new MessageCodeException(PileBaseEnum.NOT_DONE_ORDER_EXIST);
        }

        //是否存在未完成预约订单

        List<TbReservationEntityVO> reservationEntityVOS = billServiceFeign.getUnfinishedOrUnpaidReserveBillByUserId(cardManagerDTO.getUserId().toString()).getData();
        log.info("==>>unbindCard.reservationEntityVOS:{}", JSON.toJSONString(reservationEntityVOS));
        if (CollUtil.isNotEmpty(reservationEntityVOS)) {
            throw new MessageCodeException(PileBaseEnum.NOT_DONE_ORDER_EXIST);
        }
        //本地解除
        boolean removeCount = paymentCardRepository.removeById(oneEntity.getId());
        log.info("unbindCard,removeCount={}", removeCount);
        //解除的是默认卡，需重新设置默认卡
        if (oneEntity.getDefaultCard() == 1) {
            TBCardManageEntity paymentCardRepositoryOne = paymentCardRepository.getOne(new LambdaQueryWrapper<TBCardManageEntity>()
                    .select(TBCardManageEntity::getId, TBCardManageEntity::getDefaultCard)
                    .eq(TBCardManageEntity::getUserId, cardManagerDTO.getUserId())
                    .eq(TBCardManageEntity::getDeleted, 0)
                    .orderByDesc(TBCardManageEntity::getCreateTime)
                    .last("limit 1"));
            log.info("unbindCard,paymentCardRepositoryOne={}", paymentCardRepositoryOne);
            if (Objects.nonNull(paymentCardRepositoryOne)) {
                boolean updateCount = paymentCardRepository.update(new LambdaUpdateWrapper<TBCardManageEntity>()
                        .set(TBCardManageEntity::getDefaultCard, 1)
                        .eq(TBCardManageEntity::getId, paymentCardRepositoryOne.getId()));
                log.info("unbindCard,updateCount={}", updateCount);
            }
        }
        //远程服务解除
        Map<String, Object> params = new HashMap<>();
        params.put(BaseConstant.SOURCE, BaseConstant.ENERGY);
        params.put(BaseConstant.PAY_TYPE, "4");
        params.put(BaseConstant.COUNTRY, LoginUserHolder.getLoginUser().getAlpha2Code());
        params.put("paymentMethod", cardManagerDTO.getPaymentId().toString());
        String signMessage = AutelSignUtil.getSign(params);
        log.info("unbindCard,signMessage={}", signMessage);
        Boolean remoteCount = payServiceFeign.detachPaymentMethod(cardManagerDTO.getPaymentId().toString(), BaseConstant.ENERGY, 4, signMessage, null, LoginUserHolder.getLoginUser().getAlpha2Code()).getData();
        log.info("unbindCard,remoteCount={}", remoteCount);
        if (Boolean.FALSE.equals(remoteCount)) {
            throw new MessageCodeException(PileBaseEnum.REMOTE_DELETE_CARD_FAIL);
        }
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<Boolean> setDefaultCard(CardManagerDTO cardManagerDTO) {
        log.info("setDefaultCard,cardManagerDTO={}", cardManagerDTO);
        //解除默认卡
        boolean unbindFlag = paymentCardRepository.update(new LambdaUpdateWrapper<TBCardManageEntity>()
                .set(TBCardManageEntity::getDefaultCard, 0)
                .eq(TBCardManageEntity::getUserId, cardManagerDTO.getUserId())
                .eq(TBCardManageEntity::getDefaultCard, 1)
                .eq(TBCardManageEntity::getDeleted, 0));
        log.info("setDefaultCard,unbindFlag={}", unbindFlag);
        //绑定默认卡
        boolean bindFlag = paymentCardRepository.update(new LambdaUpdateWrapper<TBCardManageEntity>()
                .set(TBCardManageEntity::getDefaultCard, 1)
                .eq(TBCardManageEntity::getPaymentId, cardManagerDTO.getPaymentId())
                .eq(TBCardManageEntity::getUserId, cardManagerDTO.getUserId())
                .eq(TBCardManageEntity::getDefaultCard, 0)
                .eq(TBCardManageEntity::getDeleted, 0));
        log.info("setDefaultCard,bindFlag={}", bindFlag);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<Boolean> isBindingCard(String userId) {
        log.info("isBindingCard,userId={}", userId);
        int bindCount = paymentCardRepository.count(new LambdaQueryWrapper<TBCardManageEntity>()
                .eq(TBCardManageEntity::getUserId, userId)
                .eq(TBCardManageEntity::getDeleted, 0));
        log.info("isBindingCard,bindCount={}", bindCount);
        return Result.ofSucceed(bindCount > 0);
    }

    @Override
    public Result<Boolean> isBindingCardCurrentUser(Long userId) {
        log.info("isBindingCardCurrentUser,userId={}", userId);
        MemberCustomerDTO memberCustomerDTO = pileUserServiceFeign.findCustomer(userId).getData();
        log.info("isBindingCardCurrentUser,memberCustomerDTO={}", memberCustomerDTO);
        return Result.ofSucceed(memberCustomerDTO != null);
    }

    /**
     * description: bindConditionCheck 校验是否满足绑定条件
     * version: 1.0
     * date: 2023/10/10 10:27
     * author: A23204
     *
     * @param cardManagerDTO
     * @return boolean
     */
    private void bindConditionCheck(CardManagerDTO cardManagerDTO) {
        String fingerprint = cardManagerDTO.getFingerprint();

        if (org.apache.commons.lang3.StringUtils.isBlank(fingerprint)) {
            log.info("--->>> bindConditionCheck, fingerprint is blank, can not check.");
            // TODO: 根据 paymentMethodId 查询出 fingerprint
            Result<PaymentMethodResp> paymentMethodResult = payServiceFeign.retrievePaymentMethod(null, cardManagerDTO.getPaymentMethodId(), "energy", 4, null, null);
            if (paymentMethodResult.getCode() == 0) {
                cardManagerDTO.setFingerprint(paymentMethodResult.getData().getFingerprint());
                if (cardManagerDTO.getExpYear() == null) {
                    cardManagerDTO.setExpYear(paymentMethodResult.getData().getExpYear());
                }
                if (cardManagerDTO.getExpMonth() == null) {
                    cardManagerDTO.setExpMonth(paymentMethodResult.getData().getExpMonth());
                }
            } else {
                return;
            }
        }

        if (BindCardConstant.WHITE_CARD_LIST.contains(cardManagerDTO.getFingerprint()) || nacosNameSpace.contains("enetest")) {
            // 4242 的卡不校验
            log.info("--->>>bindConditionCheck, current fingerprint:{}", cardManagerDTO.getFingerprint());
            return;
        }

        // 查询绑定个数；
        List<TBCardManageEntity> list = paymentCardRepository.list(new LambdaQueryWrapper<TBCardManageEntity>()
                .eq(TBCardManageEntity::getFingerprint, cardManagerDTO.getFingerprint())
                .eq(TBCardManageEntity::getDeleted, 0));

        if (!CollectionUtils.isEmpty(list) && list.size() >= 5) {
            throw new MessageCodeException(PileBaseEnum.CARD_BIND_UP_LIMIT);
        }

        // 查询是否存在未支付的订单
        if (!CollectionUtils.isEmpty(list)) {
            List<Long> userIdsCollect = list.stream().map(TBCardManageEntity::getUserId).collect(Collectors.toList());
            log.info("--->>>userIdsCollect:{}", userIdsCollect);
            Result<Boolean> booleanResult = billServiceFeign.existUnPaidBill(userIdsCollect);
            Boolean exist = booleanResult.getData();
            if (Boolean.TRUE.equals(exist)) {
                throw new MessageCodeException(PileBaseEnum.NOT_PAY_ORDER_EXIST);
            }
        }

        //支付性校验，调用冻结接口

    }

    /**
     * description: defaultCardExpire 查询默认卡是否过期 true：过期；false：未过期
     * version: 1.0
     * date: 2023/10/10 21:05
     * author: A23204
     *
     * @param userId
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    @Override
    public Result<Boolean> defaultCardExpire(String userId) {
        log.info("--->>> defaultCardExpire， userId: {}", userId);
        LambdaQueryWrapper<TBCardManageEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TBCardManageEntity::getUserId, userId);
        queryWrapper.eq(TBCardManageEntity::getDefaultCard, 1);
        queryWrapper.eq(TBCardManageEntity::getDeleted, 0);

        List<TBCardManageEntity> list = paymentCardRepository.list(queryWrapper);
        if (!CollectionUtils.isEmpty(list)) {
            log.info("--->>> defaultCardExpire, expired.");
            TBCardManageEntity tbCardManageEntity = list.get(0);
            LocalDate date = LocalDate.now();
            int year = date.getYear();
            int month = date.getMonthValue();

            if (tbCardManageEntity.getExpYear() != null && tbCardManageEntity.getExpMonth() != null
                    && (year > tbCardManageEntity.getExpYear() || (year == tbCardManageEntity.getExpYear() && month >= tbCardManageEntity.getExpMonth()))) {
                return Result.ofSucceed(Boolean.TRUE);
            }
        }

        return Result.ofSucceed(Boolean.FALSE);
    }

    @Override
    public void payCardHistoryInit() {
        // 加密了
        log.info("--->>> payCardHistoryInit, start init pay card info.");

        //只查询30分钟之前的
        Long retrieveTime = System.currentTimeMillis() - 30*60*1000;

        int batch;

        for (int i=0; i<10; i++) {
            // 查询card ，单次1000
            LambdaQueryWrapper<TBCardManageEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.isNull(TBCardManageEntity::getFingerprint);
            queryWrapper.le(TBCardManageEntity::getUpdateTime, retrieveTime);
            queryWrapper.last("limit 1000");

            List<TBCardManageEntity> list = paymentCardRepository.list(queryWrapper);
            if (CollectionUtils.isEmpty(list)) {
                log.info("--->>> payCardHistoryInit, list size=0, return.");
                return;
            }

            batch = list.size();
            log.info("--- payCardHistoryInit, current size:{}", batch);

            list.forEach(tbCardManageEntity -> {
                try {
                    Result<PaymentMethodResp> paymentMethodResult = payServiceFeign.retrievePaymentMethod(String.valueOf(tbCardManageEntity.getPaymentId()), null, "energy", 4, null, null);
                    if (paymentMethodResult.getCode() == 0) {
                        PaymentMethodResp paymentMethodResp = paymentMethodResult.getData();
                        TBCardManageEntity updateEntity = new TBCardManageEntity();
                        updateEntity.setId(tbCardManageEntity.getId());
                        updateEntity.setUpdateTime(System.currentTimeMillis());
                        updateEntity.setFingerprint(paymentMethodResp.getFingerprint());
                        updateEntity.setExpYear(paymentMethodResp.getExpYear());
                        updateEntity.setExpMonth(paymentMethodResp.getExpMonth());
                        paymentCardRepository.updateById(updateEntity);

                    } else {
                        log.info("--->>>current paymentId:{} return:{}, updated failed.", tbCardManageEntity.getPaymentId(), JSON.toJSONString(paymentMethodResult));
                    }
                } catch (Exception e) {
                    log.info("--->>> retrievePaymentMethod error, error paymentId:{}", tbCardManageEntity.getPaymentId(), e);
                }
            });
            if (batch < 1000) {
                break;
            }
        }

        log.info("--->>> payCardHistoryInit, init pay card info finish.");
    }

    @Override
    public Boolean updateRefuseCodeByPaymentId(String paymentId, String refuseCode) {
        return paymentCardRepository.update(new LambdaUpdateWrapper<TBCardManageEntity>()
                .set(TBCardManageEntity::getRefuseCode, refuseCode)
                .eq(TBCardManageEntity::getPaymentId, Long.parseLong(paymentId)));
    }

    @Override
    public PaymentCardManagerDTO getPaymentCardInfoByPaymentId(String paymentId) {
        return paymentCardRepository.getPaymentCardInfoByPaymentId(paymentId);
    }

}
