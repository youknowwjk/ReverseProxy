package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcelFactory;
import com.alibaba.excel.support.ExcelTypeEnum;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.service.ChargeCardService;
import com.autel.cloud.pile.base.dto.MemberCleanDTO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.infrastructure.feign.BaseAdminClient;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.HomePileFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargeCardMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardExcelEntity;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.vo.UserSetVO;
import com.autel.cloud.pile.user.api.dto.*;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.DriverVO;
import com.autel.cloud.pile.user.api.vo.GroupVO;
import com.autel.cloud.pile.user.api.vo.MemberGroupVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.enums.SqlMethod;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.apache.ibatis.binding.MapperMethod;
import org.apache.poi.hssf.usermodel.HSSFDataValidation;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.net.URLEncoder;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @Author A22282
 * @Date 2022/5/9 17:19
 */
@Service
@Slf4j
@RefreshScope
public class ChargeCardServiceImpl extends ServiceImpl<ChargeCardMapper, ChargeCardEntity> implements ChargeCardService {

    private static final String SEVADIS_NO = "201384";

    @Value("${member.sevadisCardPath}")
    private String sevadisCardPath;
    @Value("${member.sevadisCardSmallPath}")
    private String sevadisCardSmallPath;
    @Value("${member.autelCardPath}")
    private String autelCardPath;
    @Value("${member.autelCardSmallPath}")
    private String autelCardSmallPath;
    @Value("${member.otherCardPath}")
    private String otherCardPath;
    @Value("${member.otherCardSmallPath}")
    private String otherCardSmallPath;

    @Autowired
    private ChargeCardMapper chargeCardMapper;

    @Autowired
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private PileUserServiceFeign pileUserServiceFeign;

    @Autowired
    private PileUserFeign pileUserFeign;

    @Autowired
    private MessageSource messageSource;

    @Autowired
    private HomePileFeignClient homePileFeignClient;

    @Resource
    private BaseAdminClient baseAdminClient;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String, Object> redisTemplates;

    @Value("${autel.admin:0}")
    private Long admin;

    @Override
    public Result<Boolean> addCard(ChargeCardDTO chargeCardDTO) {
        log.info("addCard,chargeCardDTO={}", chargeCardDTO);
        chargeCardDTO.setNumber(chargeCardDTO.getNumber().toUpperCase());
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .select(ChargeCardEntity::getUserId, ChargeCardEntity::getId, ChargeCardEntity::getVersion)
                .eq(ChargeCardEntity::getCardNumber, chargeCardDTO.getNumber())
                .eq(ChargeCardEntity::getDeleted, false)
                .last(BaseConstant.LIMIT_1));
        log.info("addCard,cardEntity={}", cardEntity);
        if (cardEntity != null) {
            Long userId = cardEntity.getUserId();
            if (Objects.nonNull(userId)) {
                if (chargeCardDTO.getUserId().equals(userId)) {
                    throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_BIND_YOURSELF);
                }
                throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_BIND_OTHERS);
            }
            int count = this.count(new LambdaQueryWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getUserId, chargeCardDTO.getUserId())
                    .eq(ChargeCardEntity::getDeleted, false));
            log.info("bindCard,count={}", count);
            if (count >= 100) {
                throw new MessageCodeException(PileBaseEnum.CARD_NUMBER_REACHED_UPPER_LIMIT);
            }

            // 卡名称校验
            validateCardName(chargeCardDTO);

            // 设置卡默认名称
            setDefaultCardName(chargeCardDTO);

            Integer cardPartyType = cardEntity.getCardPartyType();
            if (cardPartyType == null) {
                cardPartyType = CardPartyTypeEnum.OTHER.getValue();
            }

            log.info("更新公用卡名称");
            update(new LambdaUpdateWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getId, cardEntity.getId())
                    .set(ChargeCardEntity::getVersion, cardEntity.getVersion())
                    .set(ChargeCardEntity::getUserId, chargeCardDTO.getUserId())
                    .set(ChargeCardEntity::getCardPartyType, cardPartyType)
                    .set(ChargeCardEntity::getBindTime, System.currentTimeMillis())
                    .set(ChargeCardEntity::getCardAlias, chargeCardDTO.getName())
                    .set(ChargeCardEntity::getUpdateTime, System.currentTimeMillis())
                    .set(ChargeCardEntity::getBindStatus, true));
        } else {
            // 卡名称校验
            validateCardName(chargeCardDTO);

            // 设置卡默认名称
            setDefaultCardName(chargeCardDTO);

            cardEntity = new ChargeCardEntity();
            cardEntity.setCardAlias(chargeCardDTO.getName());
            cardEntity.setCardNumber(chargeCardDTO.getNumber());
            cardEntity.setUserId(chargeCardDTO.getUserId());
            cardEntity.setBindTime(System.currentTimeMillis());
            cardEntity.setUpdateTime(System.currentTimeMillis());
            cardEntity.setCardPartyType(CardPartyTypeEnum.OTHER.getValue());
            cardEntity.setBindStatus(1);
            chargeCardMapper.insert(cardEntity);
        }

        log.info("更新家桩卡名称");
        homePileFeignClient.syncCardName(chargeCardDTO.getUserId(), chargeCardDTO.getName(), chargeCardDTO.getNumber());

        return Result.ofSucceed(Boolean.TRUE);
    }

    private void setDefaultCardName(ChargeCardDTO chargeCardDTO) {
        log.info("chargeCardDTO:{}", JSON.toJSONString(chargeCardDTO));
        //充电卡命名规则：充电卡+卡号后四位，如有同一用户卡名有重复则末尾增加一位1，再有重复则增加，增加的1变为2，以此类推
        String cardNumber = chargeCardDTO.getNumber();
        String cardSLastFourDigit = cardNumber.substring(cardNumber.length() - 4);
        //传参卡名称是否为空
        if (org.apache.commons.lang3.StringUtils.isBlank(chargeCardDTO.getName())) {
            String privateCardName = homePileFeignClient.getCardNameByCardNumber(chargeCardDTO.getNumber(),
                    chargeCardDTO.getUserId().toString()).getData();
            log.info("privateCardName:{}", privateCardName);
            //数据库中的该卡号对应名称是否为空
            if (org.apache.commons.lang3.StringUtils.isBlank(privateCardName)) {
                List<String> publicCardNumberList = getCardNumberListByUser(chargeCardDTO.getUserId().toString());
                List<String> privateCardNumberList = homePileFeignClient.getCardNumberListByUser(chargeCardDTO.getUserId().toString()).getData();
                log.info("privateCardNumberList:{}", JSON.toJSONString(privateCardNumberList));

                publicCardNumberList = publicCardNumberList.stream().filter(o -> o != null && org.apache.commons.lang3.StringUtils.isNotBlank(o.toString())).distinct().collect(Collectors.toList());
                privateCardNumberList = privateCardNumberList.stream().filter(o -> o != null && org.apache.commons.lang3.StringUtils.isNotBlank(o.toString())).distinct().collect(Collectors.toList());
                //获取卡名命名规格默认且有相同前缀的卡集合
                Set<String> cardNumberSet = new HashSet<>();

                privateCardName = messageSource.getMessage("charge.card.name.default", null, Locale.forLanguageTag(getUserLanguage(chargeCardDTO.getUserId().toString())));
                log.info("国际化privateCardName：{}", privateCardName);

                //卡号前缀
                privateCardName = privateCardName + " " + cardSLastFourDigit;

                if (!CollectionUtils.isEmpty(publicCardNumberList)) {
                    for (String cardName : publicCardNumberList) {
                        if (cardName.contains(privateCardName)) {
                            cardNumberSet.add(cardName);
                        }
                    }
                }

                if (!CollectionUtils.isEmpty(privateCardNumberList)) {
                    for (String cardName : privateCardNumberList) {
                        if (cardName.contains(privateCardName)) {
                            cardNumberSet.add(cardName);
                        }
                    }
                }
                if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(cardNumberSet)) {
                    //充电卡后缀数字整理
                    int count = 2;
                    while (cardNumberSet.contains(privateCardName + " " + count)) {
                        count++;
                        if (count > 100) {
                            break;
                        }
                    }
                    privateCardName = privateCardName + " " + count;
                    log.info("===>>>默认卡号名称:{}", privateCardName);
                }
            }
            chargeCardDTO.setName(privateCardName);
        }
    }

    /**
     * 获取用户语言
     *
     * @param userId
     * @return
     */
    private String getUserLanguage(String userId) {
        List<String> userIdList = new ArrayList<>();
        userIdList.add(userId);
        List<UserSetVO> userSetList = baseAdminClient.queryUserSet(userIdList).getData();
        String language = "en-us";
        if (!CollectionUtils.isEmpty(userSetList)) {
            language = userSetList.get(0).getLanguage();
        }
        log.info("========== 用户语言Id:{}, 用户语言：{}", userId, language);
        return language;
    }


    /**
     * 卡名称校验
     *
     * @param chargeCardDTO
     */
    private void validateCardName(ChargeCardDTO chargeCardDTO) {
        if (!StringUtils.isEmpty(chargeCardDTO.getName())) {
            Integer publicCount = getSameNameCard(chargeCardDTO.getUserId(), chargeCardDTO.getName(), chargeCardDTO.getNumber());
            if (publicCount > 0) {
                throw new MessageCodeException(PileBaseEnum.CARD_NAME_HAS_EXIST);
            }

            Integer privateCount = homePileFeignClient.getSameNameCard(chargeCardDTO.getUserId(), chargeCardDTO.getName(), chargeCardDTO.getNumber()).getData();
            if (privateCount > 0) {
                throw new MessageCodeException(PileBaseEnum.CARD_NAME_HAS_EXIST);
            }
        }
    }

    @Override
    public Result<Boolean> updateCard(ChargeCardDTO chargeCardDTO) {
        log.info("updateCard,chargeCardDTO={}", chargeCardDTO);
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .select(ChargeCardEntity::getUserId, ChargeCardEntity::getId, ChargeCardEntity::getVersion)
                .eq(ChargeCardEntity::getCardNumber, chargeCardDTO.getNumber())
                .eq(ChargeCardEntity::getUserId, chargeCardDTO.getUserId())
                .eq(ChargeCardEntity::getDeleted, false)
                .last(BaseConstant.LIMIT_1));
        log.info("updateCard,cardEntity={}", cardEntity);
        if (Objects.isNull(cardEntity)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        Long userId = cardEntity.getUserId();
        if (Objects.isNull(userId) || !userId.equals(chargeCardDTO.getUserId())) {
            throw new MessageCodeException(PileBaseEnum.NOT_OWNER_UPDATE_NOT_ALLOW);
        }
        // 卡名称校验
        validateCardName(chargeCardDTO);

        this.update(new LambdaUpdateWrapper<ChargeCardEntity>()
                .eq(ChargeCardEntity::getId, cardEntity.getId())
                .set(ChargeCardEntity::getVersion, cardEntity.getVersion())
                .set(ChargeCardEntity::getUpdateTime, System.currentTimeMillis())
                .set(ChargeCardEntity::getCardAlias, chargeCardDTO.getName()));

        log.info("更新家桩卡名称");
        homePileFeignClient.syncCardName(userId, chargeCardDTO.getName(), chargeCardDTO.getNumber());

        return Result.ofSucceed(Boolean.TRUE);
    }

    @Transactional
    @Override
    public Result<Boolean> removeCard(ChargeCardDTO chargeCardDTO) {
        log.info("removeCard,chargeCardDTO={}", chargeCardDTO);
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .select(ChargeCardEntity::getUserId, ChargeCardEntity::getId, ChargeCardEntity::getVersion)
                .eq(ChargeCardEntity::getCardNumber, chargeCardDTO.getNumber())
                .eq(ChargeCardEntity::getUserId, chargeCardDTO.getUserId())
                .eq(ChargeCardEntity::getDeleted, false)
                .last(BaseConstant.LIMIT_1));
        log.info("removeCard,cardEntity={}", cardEntity);
        if (Objects.isNull(cardEntity)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        Long userId = cardEntity.getUserId();
        if (Objects.isNull(userId) || !userId.equals(chargeCardDTO.getUserId())) {
            throw new MessageCodeException(PileBaseEnum.NOT_OWNER_UPDATE_NOT_ALLOW);
        }

        Integer cardPartyType = cardEntity.getCardPartyType();
        if (cardPartyType == null) {
            cardPartyType = CardPartyTypeEnum.OTHER.getValue();
        }
        int count = 0;
        try {
            count = chargeCardMapper.deleteById(cardEntity);
        } catch (Exception e) {
            log.error("removeCard,exception={}", e.getMessage());
        }
        log.info("removeCard,count={}", count);

        if (count <= 0) {
            return Result.ofSucceed(false);
        }
        Boolean update = false;
        if (chargeCardDTO != null && chargeCardDTO.getUnBindPrivateCard()) {
            UnbindCardDTO unbindCardDTO = UnbindCardDTO.builder().cardNumber(chargeCardDTO.getNumber())
                    .userId(userId.toString()).build();
            update = homePileFeignClient.unbindCard(unbindCardDTO).getData();
        }
        return Result.ofSucceed(update);
    }

    @Override
    public Result<List<ChargeCardVO>> cardList(ChargeCardDTO chargeCardDTO) {
        log.info("cardList,chargeCardDTO={}", chargeCardDTO);
        LambdaQueryWrapper<ChargeCardEntity> params = new LambdaQueryWrapper<>();
        if (Objects.nonNull(chargeCardDTO.getSellerId())) {
            params.eq(ChargeCardEntity::getOperatorId, chargeCardDTO.getSellerId());
        }
        if (Objects.nonNull(chargeCardDTO.getUserId())) {
            params.eq(ChargeCardEntity::getUserId, chargeCardDTO.getUserId());
            params.orderByAsc(ChargeCardEntity::getBindTime);
        }
        List<ChargeCardEntity> resultList = this.list(params);
        log.info("cardList,resultList={}", resultList);
        List<ChargeCardVO> dataList = null;
        if (resultList != null && !resultList.isEmpty()) {
            dataList = new ArrayList<>(resultList.size());
            List<String> cardNumberList = resultList.stream().map(c -> c.getCardNumber().toUpperCase()).collect(Collectors.toList());
            List<CardInfoDTO> cardInfoList = deviceServiceFeign.getCardList(cardNumberList).getData();
            Map<String, Long> chargeCardMap = getChargeCardMap(cardNumberList, chargeCardDTO.getUserId());
            List<SimpleChargeCardVO> simpleChargeCardVOList = batchQueryCardList(cardNumberList);
            Map<String, Integer> cardTypeMap = new HashMap<>();
            if (!CollectionUtils.isEmpty(simpleChargeCardVOList)) {
                if (Objects.nonNull(chargeCardDTO.getUserId())){
                    cardTypeMap = simpleChargeCardVOList.stream().filter(c->c.getUserId().equals(chargeCardDTO.getUserId()))
                            .collect(Collectors.toMap(k -> k.getCardNumber().toUpperCase(), v -> v.getCardPartyType() == null ? CardPartyTypeEnum.AUTEL.getValue() : v.getCardPartyType()));
                }else{
                    cardTypeMap = simpleChargeCardVOList.stream()
                            .collect(Collectors.toMap(k -> k.getCardNumber().toUpperCase(), v -> v.getCardPartyType() == null ? CardPartyTypeEnum.AUTEL.getValue() : v.getCardPartyType()));
                }
            }

            List<ChargeCardVO> finalDataList = dataList;
            Map<String, Integer> finalCardTypeMap = cardTypeMap;
            resultList.forEach(chargeCardEntity -> {
                ChargeCardVO entity = new ChargeCardVO();
                BeanUtils.copyProperties(chargeCardEntity, entity);
                entity.setNumber(chargeCardEntity.getCardNumber().toUpperCase());
                entity.setName(chargeCardEntity.getCardAlias());
                if (cardInfoList != null && !cardInfoList.isEmpty()) {
                    cardInfoList.forEach(cardInfoDTO -> {
                        if (chargeCardEntity.getCardNumber().equalsIgnoreCase(cardInfoDTO.getCardNumber())) {
                            if (Objects.nonNull(cardInfoDTO.getSealerno())) {
                                entity.setSealerno(cardInfoDTO.getSealerno());
                            }
                            if (SEVADIS_NO.equalsIgnoreCase(entity.getSealerno())) {
                                entity.setCardPath(sevadisCardPath);
                                entity.setCardSmallPath(sevadisCardSmallPath);
                                entity.setCardPartyType(CardPartyTypeEnum.SEVADIS.getValue());
                            }
                        }
                    });
                }
                if (StringUtils.isEmpty(entity.getSealerno()) || !SEVADIS_NO.equalsIgnoreCase(entity.getSealerno())) {
                    Integer cardPartyType = finalCardTypeMap.get(chargeCardEntity.getCardNumber().toUpperCase());
                    entity.setSealerno(cardPartyType.toString());
                    if (CardPartyTypeEnum.AUTEL.getValue().equals(cardPartyType)) {
                        entity.setCardPath(autelCardPath);
                        entity.setCardSmallPath(autelCardSmallPath);
                    } else {
                        entity.setCardPath(otherCardPath);
                        entity.setCardSmallPath(otherCardSmallPath);
                    }
                    entity.setCardPartyType(cardPartyType);
                }

                Long privateCardUserId = chargeCardMap.get(chargeCardEntity.getCardNumber().toUpperCase());
                if (privateCardUserId != null) {
                    entity.setCardBelongTo(CardBelongToEnum.PUBLIC_PRIVATE.getValue());
                } else {
                    entity.setCardBelongTo(CardBelongToEnum.PUBLIC.getValue());
                }
                finalDataList.add(entity);
            });
        }

        log.info("cardList,dataList={}", dataList);
        return Result.ofSucceed(dataList);
    }

    // 查询卡号用户映射
    private Map<String, Long> getChargeCardMap(List<String> cardNumberList, Long userId) {
        Map<String, Long> chargeCardMap = new HashMap<>();
        List<SimpleChargeCardVO> simpleChargeCardVOList = homePileFeignClient.batchQueryCardList(cardNumberList, userId.toString()).getData();
        if (!CollectionUtils.isEmpty(simpleChargeCardVOList)) {
            simpleChargeCardVOList.forEach(c -> {
                chargeCardMap.put(c.getCardNumber().toUpperCase(), c.getUserId());
            });
        }
        return chargeCardMap;
    }

    @Override
    public String validCardNo(String cardNumber, String locationId, String evseId, Long operatorId) {
        log.info("校验卡和绑定用户是否存在,卡号：{},用户id:{}", cardNumber, operatorId);
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(ChargeCardEntity::getCardNumber, cardNumber)
                .eq(ChargeCardEntity::getDeleted, false)
                .eq(!ObjectUtils.isEmpty(operatorId), ChargeCardEntity::getOperatorId, operatorId)
                .last(BaseConstant.LIMIT_1));
        log.info("validCardNo,cardEntityByOperatorId={}", JSON.toJSONString(cardEntity));
        if (cardEntity == null) {
            cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getCardNumber, cardNumber)
                    .eq(ChargeCardEntity::getDeleted, false)
                    .eq(!ObjectUtils.isEmpty(operatorId), ChargeCardEntity::getUserId, operatorId)
                    .or(m -> m.isNull(ChargeCardEntity::getOperatorId).eq(ChargeCardEntity::getDeleted, false).eq(ChargeCardEntity::getCardNumber, cardNumber))
                    .last(BaseConstant.LIMIT_1));
            log.info("validCardNo,cardEntityByUserId={}", JSON.toJSONString(cardEntity));
            if (ObjectUtils.isEmpty(cardEntity)) {
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
            }
        }

        if (cardEntity.getBindStatus() == null || cardEntity.getBindStatus() == 0) {
            //充电卡已被禁用
            throw new MessageCodeException(PileBaseEnum.CHARGE_CARD_DISABLED);
        }

        if (StringUtils.isEmpty(cardEntity.getUserId())) {
            return "";
        }

        return cardEntity.getUserId().toString();
    }

    @Override
    public Result<String> addForBusiness(ChargeCardBusinessDTO cardBusinessDTO) {
        log.info("addForBusiness,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        this.checkForAddForBusiness(cardBusinessDTO);
        ChargeCardEntity entity = new ChargeCardEntity();
        entity.setCardAlias(cardBusinessDTO.getCardAlias());
        entity.setCardType(ChargeCardTypeEnum.COMPANY.getType());
        entity.setOperatorId(cardBusinessDTO.getOperatorId());
        entity.setUserId(IdWorker.getId());
        entity.setBindStatus(1);
        entity.setBindTime(System.currentTimeMillis());
        if (!CommonUtil.checkChargeCard(cardBusinessDTO.getCardNumber())) {
            throw new MessageCodeException(PileBaseEnum.CARD_NUMBER_ERROR);
        }
        entity.setCardBrand(cardBusinessDTO.getCardBrand());
        entity.setCardPartyType(CardPartyTypeEnum.AUTEL.getValue());

        Boolean startMultipleOrdersEnabled = cardBusinessDTO.getStartMultipleOrdersEnabled() == null ? false : cardBusinessDTO.getStartMultipleOrdersEnabled();
        String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(cardBusinessDTO.getCardNumber());
        if (startMultipleOrdersEnabled) {
            stringRedisTemplate.opsForValue().set(startMultipleOrdersEnabledCardKey, entity.getUserId().toString());
        } else {
            stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
        }
        entity.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
        Result<SellerInfoVO> detail = pileUserServiceFeign.detail(cardBusinessDTO.getOperatorId());
        log.info("addForBusiness,detail={}", JSON.toJSONString(detail));
        if (!ObjectUtils.isEmpty(detail) && detail.getCode() == 200 && !ObjectUtils.isEmpty(detail.getData())) {
            //设置商家名称
            entity.setOperatorName(detail.getData().getName());
        }
        //autel品牌校验卡号是否存在
        int existCount = this.check(cardBusinessDTO.getCardNumber(), null, null);
        if (existCount > 0) {
            //卡号已存在
            ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getCardNumber, cardBusinessDTO.getCardNumber())
                    .eq(ChargeCardEntity::getOperatorId, cardBusinessDTO.getOperatorId())
                    .eq(ChargeCardEntity::getDeleted, 0));
            if (!ObjectUtils.isEmpty(cardEntity) && Objects.equals(cardEntity.getCardBrand(), CardBrandEnum.AUTEL.getCode()) && cardEntity.getOperatorId() != null && cardEntity.getOperatorId().longValue() == cardBusinessDTO.getOperatorId().longValue()) {
                throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_BIND_YOURSELF);
            }
            //校验是否被app绑定
            int count = this.checkBindApp(cardBusinessDTO.getCardNumber());
            if (count > 0) {
                throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_BIND_OTHERS);
            }
        }
        entity.setCardNumber(cardBusinessDTO.getCardNumber().toUpperCase());
        log.info("addForBusiness,after set entity={}", JSON.toJSONString(entity));
        boolean save = this.saveOrUpdate(entity);
        log.info("addForBusiness,save={}", save);
        //生成默认C端用户
        MemberForChargeCardDTO dto = new MemberForChargeCardDTO();
        MemberInfoDTO params = new MemberInfoDTO();
        params.setUserId(entity.getUserId());
        params.setCardNumber(entity.getCardNumber());
        params.setMemberGroupIds(cardBusinessDTO.getMemberGroupIds());
        params.setDriverId(cardBusinessDTO.getDriverId());
        dto.setSellerId(cardBusinessDTO.getOperatorId());
        dto.setMemberList(Collections.singletonList(params));
        Result<Boolean> memberResult = pileUserFeign.addForChargeCard(dto);
        log.info("addForBusiness,memberResult={}", JSON.toJSONString(memberResult));
        if (memberResult == null || memberResult.getCode() != 200 || memberResult.getData() == null) {
            throw new IllegalArgumentException("Remote add member fail.");
        }
        return Result.ofSucceed(entity.getId().toString());
    }

    private int checkBindApp(String cardNumber) {
        return this.count(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(StringUtils.hasText(cardNumber), ChargeCardEntity::getCardNumber, cardNumber)
                .isNull(ChargeCardEntity::getOperatorId)
                .isNotNull(ChargeCardEntity::getUserId)
                .eq(ChargeCardEntity::getDeleted, 0));
    }

    private void checkForAddForBusiness(ChargeCardBusinessDTO cardBusinessDTO) {
        if (StringUtils.isEmpty(cardBusinessDTO.getCardAlias())) {
            throw new MessageCodeException(PileBaseEnum.CARD_NAME_ERROR);
        }
        if (cardBusinessDTO.getCardBrand() == null) {
            throw new IllegalArgumentException("Card brand should not null.");
        }
        if (this.check(null, cardBusinessDTO.getCardAlias(), cardBusinessDTO.getOperatorId()) > 0) {
            throw new MessageCodeException(PileBaseEnum.CARD_NAME_HAS_EXIST);
        }
    }

    @Override
    @Transactional
    public Result<Boolean> updateForBusiness(ChargeCardBusinessDTO cardBusinessDTO) {
        log.info("updateForBusiness,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        Long sellerId = cardBusinessDTO.getOperatorId();
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(ChargeCardEntity::getId, cardBusinessDTO.getId()));
        log.info("updateForBusiness,select cardEntity={}", JSON.toJSONString(cardEntity));
        if (ObjectUtils.isEmpty(cardEntity)) {
            throw new MessageCodeException(PileBaseEnum.CHARGE_CARD_NOT_EXIST);
        }

        Boolean startMultipleOrdersEnabled = cardBusinessDTO.getStartMultipleOrdersEnabled() == null ? false : cardBusinessDTO.getStartMultipleOrdersEnabled();
        //非admin
        if (admin.longValue() != cardBusinessDTO.getOperatorId().longValue()) {
            if (StringUtils.isEmpty(cardBusinessDTO.getCardAlias())) {
                throw new MessageCodeException(PileBaseEnum.CARD_NAME_ERROR);
            }
            if (this.checkChargeCard(cardBusinessDTO.getOperatorId(), cardBusinessDTO.getCardAlias(), cardBusinessDTO.getId())) {
                throw new MessageCodeException(PileBaseEnum.CARD_NAME_HAS_EXIST);
            }
            cardEntity.setCardAlias(cardBusinessDTO.getCardAlias());
            cardEntity.setCardType(cardBusinessDTO.getCardType());
            cardEntity.setCardNumber(cardBusinessDTO.getCardNumber());

            if (!startMultipleOrdersEnabled && cardEntity.getUserId() != null) {
                String idTagKey = String.format("ENERGY:CHARGE:IDTAG:%s:%s", DeviceTypeEnum.BUSINESS_PILE.getValue(), cardEntity.getUserId().toString());
                String idTagValue = stringRedisTemplate.opsForValue().get(idTagKey);
                if (!StringUtils.isEmpty(idTagValue)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGING_CARD_UPDATE_NOT_SUPPORT);
                }
            }
        } else {
            if (!StringUtils.isEmpty(cardBusinessDTO.getCardNumber())
                    && !cardBusinessDTO.getCardNumber().equals(cardEntity.getCardNumber())
                    && this.check(cardBusinessDTO.getCardNumber(), null, null) > 0) {
                throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_EXIST);
            }
            cardEntity.setCardType(cardBusinessDTO.getCardType());
            cardEntity.setCardNumber(cardBusinessDTO.getCardNumber());
        }

        String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(cardBusinessDTO.getCardNumber());
        cardEntity.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
        log.info("updateForBusiness,after set cardEntity={}", JSON.toJSONString(cardEntity));
        MemberGroupRelationDTO dto = new MemberGroupRelationDTO();
        List<MemberGroupRelationDTO.UserRelationDTO> relationList = new ArrayList<>();
        dto.setSellerId(sellerId);
        MemberGroupRelationDTO.UserRelationDTO relationDto = new MemberGroupRelationDTO.UserRelationDTO();
        relationDto.setUserId(cardEntity.getUserId());
        relationDto.setMemberGroupIds(cardBusinessDTO.getMemberGroupIds());
        relationDto.setDriverId(cardBusinessDTO.getDriverId());
        relationList.add(relationDto);
        dto.setRelationList(relationList);
        Integer count = this.pileUserFeign.update(dto).getData();
        log.info("updateForBusiness,count={}", count);
        boolean update = this.updateById(cardEntity);
        if (update) {
            if (startMultipleOrdersEnabled && cardEntity.getBindStatus() == 1) {
                stringRedisTemplate.opsForValue().set(startMultipleOrdersEnabledCardKey, cardEntity.getUserId().toString());
            } else {
                stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
            }
        }
        log.info("updateForBusiness,update={}", update);
        return Result.ofSucceed(update);
    }

    @Override
    public Result<IPage<ChargeCardBusinessVO>> pagesForBusiness(ChargeCardBusinessPageDTO cardBusinessDTO) {
        log.info("pagesForBusiness,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        IPage<ChargeCardBusinessVO> resultList = chargeCardMapper.pagesForBusiness(cardBusinessDTO, cardBusinessDTO);
        if (!ObjectUtils.isEmpty(resultList) && !CollectionUtils.isEmpty(resultList.getRecords())) {
            List<ChargeCardBusinessVO> records = resultList.getRecords();
            records.forEach(e -> {
                if (!ObjectUtils.isEmpty(e.getUserId())) {
                    Result<UserInfoVO> user = pileUserServiceFeign.getUser(null, e.getUserId());
                    if (!ObjectUtils.isEmpty(user) && user.getCode() == 200 && !ObjectUtils.isEmpty(user.getData())) {
                        UserInfoVO data = user.getData();
                        e.setUserName(data.getUsername());
                        //手机号和邮箱脱敏处理
                        String telephone = data.getPhoneNumber();
                        if (StrUtil.isNotBlank(telephone)) {
                            e.setPhoneNumber(StrUtil.hide(telephone, 3, telephone.length() - 4));
                        }
                        String email = data.getEmail();
                        if (StrUtil.isNotBlank(email)) {
                            int index = StrUtil.indexOf(email, '@');
                            e.setEmail(index <= 1 ? email : StrUtil.hide(email, 1, index));
                        }
                    }
                }
            });
        }
        log.info("pagesForBusiness,resultList={}", JSON.toJSONString(resultList));
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<IPage<ChargeCardPageVO>> pagesV2(ChargeCardPageDTO chargeCardPageDTO) {
        Long groupId = chargeCardPageDTO.getGroupId();
        Long sellerId = chargeCardPageDTO.getOperatorId();
        List<Long> list = new ArrayList<>();
        if (!ObjectUtils.isEmpty(groupId)) {
            //根据客户组id查询所有充电卡
            Result<List<Long>> cardByGroupId = pileUserServiceFeign.getCardByGroupId(groupId);
            if (ObjectUtils.isEmpty(cardByGroupId) || CollectionUtils.isEmpty(cardByGroupId.getData())) {
                return Result.ofSucceed(new Page<ChargeCardPageVO>(chargeCardPageDTO.getCurrent(), chargeCardPageDTO.getSize()));
            }
            list = cardByGroupId.getData();
        }

        // 判断排序类型和排序方法是否为空
        if (chargeCardPageDTO.getOrderBy() == null || chargeCardPageDTO.getOrderBy().isEmpty()) {
            chargeCardPageDTO.setOrderBy("create_time");
        }
        if ("bindTime".equals(chargeCardPageDTO.getOrderBy())) {
            chargeCardPageDTO.setOrderBy("bind_time");
        }
        if ("cardAlias".equals(chargeCardPageDTO.getOrderBy())) {
            chargeCardPageDTO.setOrderBy("card_alias");
        }
        if (chargeCardPageDTO.getOrderType() == null || chargeCardPageDTO.getOrderType().isEmpty()) {
            chargeCardPageDTO.setOrderType("ASC");
        }
        IPage<ChargeCardPageVO> resultList = chargeCardMapper.pagesV2(chargeCardPageDTO, list);
        //查询充电卡所属客户组信息
        if (!ObjectUtils.isEmpty(resultList) && !CollectionUtils.isEmpty(resultList.getRecords())) {
            List<Long> userIds = resultList.getRecords().stream().map(ChargeCardPageVO::getUserId).collect(Collectors.toList());
            log.info("充电卡分页,userIds={}", userIds);
            SearchGroupDTO searchGroupDTO = new SearchGroupDTO();
            searchGroupDTO.setSellerId(sellerId);
            searchGroupDTO.setUserIds(userIds);
            List<GroupVO> listResult = pileUserServiceFeign.searchGroupByUserId(searchGroupDTO).getData();
            log.info("充电卡分页,listResult={}", JSON.toJSONString(listResult));
            if (!ObjectUtils.isEmpty(listResult)) {
                Map<Long, GroupVO> tmpMap = listResult.stream().collect(Collectors.toMap(GroupVO::getUserId, e -> e, (f, s) -> f));
                for (ChargeCardPageVO record : resultList.getRecords()) {
                    Long userId = record.getUserId();
                    GroupVO groupVO = tmpMap.get(userId);
                    if (groupVO != null) {
                        record.setGroupInformationVOS(groupVO.getGroupInformationVOList());
                        record.setDriverList(groupVO.getDriverList());
                    }
                }
            }
        }
        return Result.ofSucceed(resultList);
    }

    @Override
    public Result<List<Long>> searchBindCard(Long groupId) {
        Long sellerId = LoginUserUtil.getSellerId();
        //查询充电卡所属客户组信息
        SearchGroupDTO searchGroupDTO = new SearchGroupDTO();
        searchGroupDTO.setSellerId(sellerId);
        searchGroupDTO.setGroupId(groupId);
        Result<List<GroupVO>> listResult = pileUserServiceFeign.searchGroupByUserId(searchGroupDTO);
        if (!ObjectUtils.isEmpty(listResult) && !CollectionUtils.isEmpty(listResult.getData())) {
            return Result.ofSucceed(listResult.getData().get(0).getMemberIds());
        }
        return Result.ofSucceed(null);
    }

    @Override
    public Boolean deletedAllDisPlaySetting() {
        Set<String> keys = stringRedisTemplate.keys("pile-base:chargeCardService:chargeCardDisplaySetting" + "*");
        Long delete = stringRedisTemplate.delete(keys);
        log.info("deletedAllDisPlaySetting,delete={}", delete);
        return true;
    }

    @Override
    public Result<IPage<CardOptionsPageVO>> cardOptionPageList(CardOptionsDTO cardOptionsDTO) {
        IPage<CardOptionsPageVO> resultList = chargeCardMapper.cardOptionPageList(cardOptionsDTO, cardOptionsDTO.getKeyWord(), cardOptionsDTO.getOperatorId());
        return Result.ofSucceed(resultList);
    }

    @Override
    public List<ChargeCardEntity> findListV2(List<String> cardNumbers, Long sellerId) {
        List<ChargeCardEntity> result = new ArrayList<>();
        if (CollectionUtils.isEmpty(cardNumbers)) return result;
        int batch = 1000;
        int row = 1;
        int size = cardNumbers.size();
        List<String> list = new ArrayList<>(batch);
        for (String cardNumber : cardNumbers) {
            list.add(cardNumber);
            if (row % batch == 0 || row == size) {
                result.addAll(this.list(new LambdaQueryWrapper<ChargeCardEntity>()
                        .in(ChargeCardEntity::getCardNumber, list)
                        .eq(ChargeCardEntity::getOperatorId, sellerId)
                        .eq(ChargeCardEntity::getDeleted, 0)));
                list.clear();
            }
            row++;
        }
        return result;
    }

    @Override
    public List<ChargeCardEntity> findList(List<String> cardAlias, Long sellerId) {
        List<ChargeCardEntity> result = new ArrayList<>();
        if (CollectionUtils.isEmpty(cardAlias)) return result;
        int batch = 1000;
        int row = 1;
        int size = cardAlias.size();
        List<String> list = new ArrayList<>(batch);
        for (String card : cardAlias) {
            list.add(card);
            if (row % batch == 0 || row == size) {
                result.addAll(this.list(new LambdaQueryWrapper<ChargeCardEntity>()
                        .in(ChargeCardEntity::getCardAlias, list)
                        .eq(ChargeCardEntity::getDeleted, 0)
                        .eq(ChargeCardEntity::getOperatorId, sellerId)));
                list.clear();
            }
            row++;
        }
        return result;
    }

    @Override
    public List<ChargeCardBusinessVO> findList(Long sellerId, Long driverId) {
        DriverParamDTO paramDto = new DriverParamDTO();
        paramDto.setSellerId(sellerId);

        LambdaQueryWrapper<ChargeCardEntity> query = Wrappers.lambdaQuery();
        query.eq(ChargeCardEntity::getOperatorId, sellerId);
        query.eq(ChargeCardEntity::getDeleted, 0);

        List<DriverVO> driverList = this.pileUserFeign.driverFindList(paramDto).getData();
        Set<Long> checkIds = new HashSet<>();
        if (!CollectionUtils.isEmpty(driverList)) {
            Map<Boolean, Set<Long>> ids = driverList.stream()
                    .flatMap(driverVO -> driverVO.getAuthList().stream()
                            .map(DriverAuthDTO::getAuthObject)
                            .filter(Objects::nonNull)
                            .map(Long::parseLong)
                            .map(userId -> new AbstractMap.SimpleEntry<>(
                                    driverId != null && driverId.equals(driverVO.getId()),
                                    userId)))
                    .collect(Collectors.groupingBy(
                            AbstractMap.SimpleEntry::getKey,
                            Collectors.mapping(AbstractMap.SimpleEntry::getValue, Collectors.toSet())));
            checkIds = ids.getOrDefault(true, new HashSet<>());
            Set<Long> userIds = ids.get(false);
            if (!CollectionUtils.isEmpty(userIds)) {
                query.notIn(ChargeCardEntity::getUserId, userIds);
            }
        }

        List<ChargeCardEntity> entityList = this.list(query);
        if (CollectionUtils.isEmpty(entityList)) {
            return null;
        }
        Set<Long> finalCheckIds = checkIds;
        List<ChargeCardBusinessVO> resultList = entityList.stream()
                .map(e -> {
                    ChargeCardBusinessVO vo = new ChargeCardBusinessVO();
                    vo.setId(e.getId());
                    vo.setCardAlias(e.getCardAlias());
                    vo.setCardNumber(e.getCardNumber());
                    vo.setUserId(e.getUserId());
                    Integer bindStatus = finalCheckIds.contains(e.getUserId()) ? 1 : 0;
                    vo.setBindStatus(bindStatus);
                    return vo;
                })
                .collect(Collectors.toList());
        return resultList;
    }

    @Override
    public Result<Boolean> deleteForBusiness(Long id) {
        log.info("deleteForBusiness,id={}", id);
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .select(ChargeCardEntity::getId, ChargeCardEntity::getVersion)
                .eq(ChargeCardEntity::getId, id)
                .eq(ChargeCardEntity::getDeleted, 0));
        log.info("deleteForBusiness,cardEntity={}", JSON.toJSONString(cardEntity));
        if (ObjectUtils.isEmpty(cardEntity)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        if (cardEntity.getUserId() != null) {
            String idTagKey = String.format("ENERGY:CHARGE:IDTAG:%s:%s", DeviceTypeEnum.BUSINESS_PILE.getValue(), cardEntity.getUserId().toString());
            String idTagValue = stringRedisTemplate.opsForValue().get(idTagKey);
            if (!StringUtils.isEmpty(idTagValue)) {
                throw new MessageCodeException(PileBaseEnum.CHARGING_CARD_UPDATE_NOT_SUPPORT);
            }
        }

        int delete = chargeCardMapper.deleteById(id);
        log.info("deleteForBusiness,delete={}", delete);

        if (delete > 0) {
            String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(cardEntity.getCardNumber());
            stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
        }

        return Result.ofSucceed(delete > 0);
    }

    @Override
    @Transactional
    public Result<Boolean> batchRemovalOfChargingCards(List<Long> ids) {
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        Long sellerId = payload.getSellerId();
        if (ObjectUtils.isEmpty(ids)) {
            return Result.ofSucceed(false);
        }
        MemberCleanDTO memberCleanDTO = new MemberCleanDTO();
        memberCleanDTO.setSellerId(sellerId);
        List<ChargeCardEntity> cardIds = chargeCardMapper.selectList(new LambdaQueryWrapper<ChargeCardEntity>()
                .in(ChargeCardEntity::getId, ids)
                .eq(ChargeCardEntity::getDeleted, 0));
        if (ObjectUtils.isEmpty(cardIds)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        cardIds.stream().forEach(c -> {
            if (c.getUserId() != null) {
                String idTagKey = String.format("ENERGY:CHARGE:IDTAG:%s:%s", DeviceTypeEnum.BUSINESS_PILE.getValue(), c.getUserId().toString());
                String idTagValue = stringRedisTemplate.opsForValue().get(idTagKey);
                if (!StringUtils.isEmpty(idTagValue)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGING_CARD_UPDATE_NOT_SUPPORT);
                }
            }
        });

        List<Long> autelCardIds = cardIds.stream().filter(m -> m.getCardBrand() == 1).map(ChargeCardEntity::getId).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(autelCardIds)) {
            Boolean update = this.update(new LambdaUpdateWrapper<ChargeCardEntity>()
                    .set(ChargeCardEntity::getOperatorId, null)
                    .set(ChargeCardEntity::getOperatorName, null)
                    .set(ChargeCardEntity::getLimitEvseId, "-1")
                    .set(ChargeCardEntity::getLimitLocationId, "-1")
                    .set(ChargeCardEntity::getCardAttribute, ChargeCardAttributeEnum.COMMON.getType())
                    .set(ChargeCardEntity::getChargeTimes, 0)
                    .set(ChargeCardEntity::getUserId, null)
                    .set(ChargeCardEntity::getBindTime, 0)
                    .set(ChargeCardEntity::getBindStatus, 0)
                    .in(ChargeCardEntity::getId, autelCardIds));
            if (update) {
                cardIds.stream().forEach(c -> {
                    String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(c.getCardNumber());
                    stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
                });
            }
        }
        List<Long> userIds = cardIds.stream().map(ChargeCardEntity::getUserId).collect(Collectors.toList());
        memberCleanDTO.setUserId(userIds);
        List<Long> list1 = cardIds.stream().filter(m -> m.getCardBrand() != 1).map(ChargeCardEntity::getId).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(list1)) {
            chargeCardMapper.deleteBatchIds(list1);
        }
        chargeCardMapper.deleteBatchIds(ids);
        pileUserServiceFeign.clean(memberCleanDTO);
        return Result.ofSucceed(true);
    }


    @Override
    public Result<Boolean> enableDisableChargingCard(EnableDisableChargingCardDTO enableDisableChargingCardDTO) {
        ChargeCardEntity chargeCardEntity = chargeCardMapper.selectOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(ChargeCardEntity::getCardNumber, enableDisableChargingCardDTO.getCardNumber())
                .eq(ChargeCardEntity::getOperatorId, LoginUserHolder.getLoginUser().getPayload().getSellerId())
                .eq(ChargeCardEntity::getDeleted, 0));
        if (enableDisableChargingCardDTO.getType().equals(0)) {
            chargeCardEntity.setBindStatus(0);

            if (chargeCardEntity.getUserId() != null) {
                String idTagKey = String.format("ENERGY:CHARGE:IDTAG:%s:%s", DeviceTypeEnum.BUSINESS_PILE.getValue(), chargeCardEntity.getUserId().toString());
                String idTagValue = stringRedisTemplate.opsForValue().get(idTagKey);
                if (!StringUtils.isEmpty(idTagValue)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGING_CARD_UPDATE_NOT_SUPPORT);
                }
            }

            Boolean update = this.updateById(chargeCardEntity);
            if (update) {
                String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(chargeCardEntity.getCardNumber());
                stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
            }
        }
        if (enableDisableChargingCardDTO.getType().equals(1)) {
            chargeCardEntity.setBindStatus(1);
            Boolean update = this.updateById(chargeCardEntity);

            if (update && chargeCardEntity.getStartMultipleOrdersEnabled() != null && chargeCardEntity.getStartMultipleOrdersEnabled()) {
                String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(chargeCardEntity.getCardNumber());
                stringRedisTemplate.opsForValue().set(startMultipleOrdersEnabledCardKey, chargeCardEntity.getUserId().toString());
            }
        }
        chargeCardMapper.updateById(chargeCardEntity);
        return Result.ofSucceed(true);
    }

    @Override
    public Map<String, Object> getDisplaySetting(Long operatorId, Long userId) {
        log.info("getDisplaySetting,operatorId={},userId={}", operatorId, userId);
        Map<String, Object> resultMap = new HashMap<>();
        String showKey = RedisUtil.buildKey(RedisDataEnum.CHARGE_CARD_DISPLAY_SETTING_SHOW.getPrefix(), operatorId, userId);
        String settings = stringRedisTemplate.opsForValue().get(showKey);
        if (StringUtils.hasText(settings)) {
            ChargeCardDisPlayV2DTO resultData = JSON.parseObject(settings, ChargeCardDisPlayV2DTO.class);
            resultMap.put("displaySetting", resultData);
            return resultMap;
        }

        ChargeCardDisPlayV2DTO resultData = ChargeCardDisPlayV2DTO.getDefaultInstance();
        stringRedisTemplate.opsForValue().set(showKey, JSON.toJSONString(resultData));
        resultMap.put("displaySetting", resultData);
        return resultMap;
    }

    @Override
    public Map<String, Object> setDisplaySetting(ChargeCardDisPlayV2DTO ChargeCardDisPlayV2DTO) {
        Map<String, Object> resultMap = new HashMap<>();
        log.info("setDisplaySetting, ChargeCardDisPlayV2DTO={}", JSON.toJSONString(ChargeCardDisPlayV2DTO));
        try {
            Long operatorId = ChargeCardDisPlayV2DTO.getOperatorId();
            Long userId = ChargeCardDisPlayV2DTO.getUserId();

            String showKey = RedisUtil.buildKey(RedisDataEnum.CHARGE_CARD_DISPLAY_SETTING_SHOW.getPrefix(), operatorId, userId);
            stringRedisTemplate.opsForValue().set(showKey, JSON.toJSONString(ChargeCardDisPlayV2DTO));
            resultMap.put("displaySetting", ChargeCardDisPlayV2DTO);
            return resultMap;
        } catch (Exception e) {
            log.error("setDisplaySetting", e);
            return getDisplaySetting(ChargeCardDisPlayV2DTO.getOperatorId(), ChargeCardDisPlayV2DTO.getUserId());
        }
    }

    @Override
    public Map<String, Object> exchangeDisplayColumn(Long operatorId, Long userId, List<String> dataIndexList) {
        String sortKey = RedisUtil.buildKey(RedisDataEnum.CHARGE_CARD_DISPLAY_SETTING_SORT.getPrefix(), operatorId, userId);
        String settings = JSON.toJSONString(dataIndexList);
        log.info("exchangeDisplayColumn,operatorId={},userId={}, settings={}", operatorId, userId, settings);
        stringRedisTemplate.opsForValue().set(sortKey, settings);
        return getDisplaySetting(operatorId, userId);
    }


    @Override
    public Result<Boolean> deleteUserData(Long userId) {
        List<String> cardNumberList = this.getCardNumberListByUser(userId.toString());

        boolean update = this.update(new LambdaUpdateWrapper<ChargeCardEntity>()
                .set(ChargeCardEntity::getBindStatus, false)
                .set(ChargeCardEntity::getBindTime, null)
                .set(ChargeCardEntity::getCardAlias, null)
                .set(ChargeCardEntity::getUserId, null)
                .eq(ChargeCardEntity::getUserId, userId)
                .eq(ChargeCardEntity::getDeleted, 0));
        log.info("deleteUserData,update={}", update);

        if (update && !CollectionUtils.isEmpty(cardNumberList)) {
            cardNumberList.forEach(c -> {
                String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(c);
                stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
            });
        }
        return Result.ofSucceed(update);
    }

    @Override
    public void download(HttpServletRequest request, HttpServletResponse response, Long sellerId) {
        log.info("download,sellerId={}", sellerId);
        if (ObjectUtils.isEmpty(sellerId)) {
            throw new MessageCodeException("sellerId should not be null");
        }
        /*String fileName = "electricCard_import.xlsx";
        if (!sellerId.equals(admin)) {
            fileName = "electricCard_seller_import.xlsm";
        }
        String filePath = "xls/" + fileName;*/
        InputStream fis = null;
        OutputStream os = null;
        try {
            final Locale locale = LocaleContextHolder.getLocale();
            log.info("locale context information ==>language:{},country:{},locale:{}", locale.getLanguage(), locale.getCountry(), locale);
            //InputStream inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(filePath);
            Workbook wb = new XSSFWorkbook();
            //Sheet sheet = wb.getSheetAt(0);
            Sheet sheet = wb.createSheet();

            CellStyle cs = wb.createCellStyle();
            Font font = wb.createFont();
            font.setColor(Font.COLOR_RED);
            cs.setFont(font);

            CellStyle gray = wb.createCellStyle();
            gray.setFillForegroundColor(IndexedColors.GREY_40_PERCENT.getIndex());
            gray.setFillPattern(FillPatternType.SOLID_FOREGROUND);

            if (sellerId.longValue() == admin.longValue()) {
                Row r0 = sheet.createRow(0);

                Cell c0 = r0.createCell(0);
                c0.setCellStyle(cs);
                c0.setCellValue(messageSource.getMessage("xlsRemark", null, locale));

                Row r1 = sheet.createRow(1);

                Cell c10 = r1.createCell(0);
                c10.setCellStyle(gray);
                c10.setCellValue(messageSource.getMessage("xlsCardNumber", null, locale));

                Cell c11 =r1.createCell(1);
                c11.setCellStyle(gray);
                c11.setCellValue(messageSource.getMessage("xlsCardType", null, locale));

                this.createDropDownList(sheet, new String[]{"RFID", messageSource.getMessage("Others", null, locale)}, 2, 20000, 1, 1);
            } else {
                Row r0 = sheet.createRow(0);

                Cell c0 = r0.createCell(0);
                c0.setCellStyle(cs);
                c0.setCellValue(messageSource.getMessage("xlsRemarkSeller", null, locale));

                Row r1 = sheet.createRow(1);

                Cell c10 = r1.createCell(0);
                c10.setCellStyle(gray);
                c10.setCellValue(messageSource.getMessage("xlsCardName", null, locale));

                Cell c11 = r1.createCell(1);
                c11.setCellStyle(gray);
                c11.setCellValue(messageSource.getMessage("xlsCardNumberSeller", null, locale));

                Cell c12 = r1.createCell(2);
                c12.setCellStyle(gray);
                c12.setCellValue(messageSource.getMessage("xlsCardBrand", null, locale));
                Cell c13 = r1.createCell(3);
                c13.setCellStyle(gray);
                c13.setCellValue(messageSource.getMessage("xlsStartMultipleOrdersEnabled", null, locale));

                Cell c14 = r1.createCell(4);
                c14.setCellStyle(gray);
                c14.setCellValue(messageSource.getMessage("_n2rnE3LPQYWs", null, locale));

                Cell c15 = r1.createCell(5);
                c15.setCellValue(messageSource.getMessage("_t7ueeWKWpu4W", null, locale));
                c15.setCellStyle(gray);

                //sheet.getRow(1).getCell(4).setCellValue(messageSource.getMessage("xlsIsFleetCard", null, LocaleContextHolder.getLocale()));

                this.createDropDownList(sheet, new String[]{"Autel", "EVBox", "Monta", "Others"}, 2, 20000, 2, 2);
                this.createDropDownList(sheet, new String[]{"0", "1"}, 2, 20000, 3, 3);
                //设置客户组下拉框
                List<MemberGroupVO> memberGroupVoList = this.pileUserFeign.getMemberGroupBySellerId(sellerId).getData();
                if (!CollectionUtils.isEmpty(memberGroupVoList)) {
                    //过滤充电卡客户组
                    memberGroupVoList = memberGroupVoList.stream().filter(e -> e.getMemberType().equals(2)).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(memberGroupVoList)) {
                        List<String> memberGroupNames = memberGroupVoList.stream().map(MemberGroupVO::getName).collect(Collectors.toList());
                        this.createDropDownList(sheet, memberGroupNames.toArray(new String[]{}), 2, 20000, 4, 4);
                    }
                }
                //设置车主下拉框
                DriverParamDTO paramDto = new DriverParamDTO();
                paramDto.setSellerId(sellerId);
                List<DriverVO> driverList = this.pileUserFeign.driverFindList(paramDto).getData();
                if (!CollectionUtils.isEmpty(driverList)) {
                    List<DriverVO> tmpList = driverList.stream().filter(e -> !e.getRelations()).collect(Collectors.toList());
                    if (!CollectionUtils.isEmpty(tmpList)) {
                        List<String> driverNames = tmpList.stream().map(DriverVO::getName).collect(Collectors.toList());
                        this.createDropDownList(sheet, driverNames.toArray(new String[]{}), 2, 20000, 5, 5);
                    }
                }
            }
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(1024);
            wb.write(byteArrayOutputStream);
            byte[] bytes = byteArrayOutputStream.toByteArray();
            fis = new ByteArrayInputStream(bytes);
            // 判断浏览器的类型
            String agent = request.getHeader("user-agent");
            String targetName;
            if (sellerId.equals(admin)) {
                targetName = messageSource.getMessage("electricCard.import", null, locale);
            } else {
                targetName = messageSource.getMessage("electricCard.seller.import", null, locale);
            }
            if (agent.toLowerCase().indexOf("firefox") != -1) {
                targetName = new String(targetName.getBytes(), "iso8859-1");
            } else {
                targetName = URLEncoder.encode(targetName, "utf-8");
            }
            if (StringUtils.hasText(targetName)) {
                targetName = targetName.replace("+", "%20");
            }
            response.setContentType("application/excel;charset=utf-8");
            response.setContentLength(bytes.length);
            response.setHeader("content-Disposition", "attachment;filename=" + targetName + ".xlsx");
            // 最后输出
            os = response.getOutputStream();
            IOUtils.copy(fis, os);
            os.flush();
        } catch (Exception e) {
            log.error("download:", e);
        } finally {
            try {
                if (fis != null) {
                    fis.close();
                }
                if (os != null) {
                    os.close();
                }
            } catch (IOException e) {
                log.error("download:", e);
            }
        }
    }

    @Override
    public void errorDetail(HttpServletRequest request, HttpServletResponse response, Long sellerId, Long code) {
        log.info("errorDetail,sellerId={},code={}", sellerId, code);
        if (ObjectUtils.isEmpty(sellerId) || ObjectUtils.isEmpty(code)) {
            throw new MessageCodeException("sellerId or code is null");
        }
        String key = String.format(RedisKeyConstant.getStringPileBaseChargeCardError(), sellerId, code);
        List<ChargeCardErrorDTO> redisList = (List<ChargeCardErrorDTO>) redisTemplates.opsForValue().get(key);
        if (CollectionUtils.isEmpty(redisList)) {
            return;
        }
        log.info("errorDetail,redisList={}", redisList.size());
        InputStream fis = null;
        OutputStream os = null;
        Workbook wb = null;
        try {
            wb = new XSSFWorkbook();
            Sheet sheet = wb.createSheet();
            createHeader(wb,sheet);
            Row row0 = sheet.getRow(0);
            log.info("{} ==== the row cell number: {}", row0.getFirstCellNum(), row0.getLastCellNum());
            if (sellerId.longValue() == admin.longValue()) {
                int row = 1;
                for (ChargeCardErrorDTO dto : redisList) {
                    Row r = sheet.createRow(row);
                    CellStyle cs = wb.createCellStyle();
                    Font font = wb.createFont();
                    font.setColor(Font.COLOR_RED);
                    if (dto.getErrorType().equals(ChargeCardErrorEnum.SUCCESS.getCode())) {
                        font.setColor(IndexedColors.GREEN.getIndex());
                    }
                    cs.setFont(font);
                    r.createCell(0).setCellValue(dto.getCardNumber());
                    Cell cell = r.createCell(1);
                    cell.setCellStyle(cs);
                    cell.setCellValue(messageSource.getMessage(ChargeCardErrorEnum.getDescByCode(dto.getErrorType()), null, LocaleContextHolder.getLocale()));
                    row++;
                }
                this.createDropDownList(sheet, new String[]{"RFID", messageSource.getMessage("Others", null, LocaleContextHolder.getLocale())}, 2, 10000, 1, 1);
            } else {
                int row = 1;
                for (ChargeCardErrorDTO dto : redisList) {
                    Row r = sheet.createRow(row);
                    CellStyle cs = wb.createCellStyle();
                    Font font = wb.createFont();
                    font.setColor(Font.COLOR_RED);
                    if (dto.getErrorType().equals(ChargeCardErrorEnum.SUCCESS.getCode())) {
                        font.setColor(IndexedColors.GREEN.getIndex());
                    }
                    cs.setFont(font);
                    r.createCell(0).setCellValue(dto.getCardAlias());
                    r.createCell(1).setCellValue(dto.getCardNumber());
                    r.createCell(2).setCellValue(dto.getCardBrand());
                    r.createCell(3).setCellValue(dto.getStartMultipleOrdersEnabled());
                    Cell cell = r.createCell(4);
                    cell.setCellStyle(cs);
                    cell.setCellValue(messageSource.getMessage(ChargeCardErrorEnum.getDescByCode(dto.getErrorType()), null, LocaleContextHolder.getLocale()));
                    log.info("{}====the row cell number: {},the dto is:{}", r.getFirstCellNum(), r.getLastCellNum(), JSON.toJSONString(dto));
                    row++;
                }
                this.createDropDownList(sheet, new String[]{"Autel", "EVBox", "Monta", "Others"}, 1, 20000, 2, 2);
                this.createDropDownList(sheet, new String[]{"0", "1"}, 1, 20000, 3, 3);
            }
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream(1024);
            wb.write(byteArrayOutputStream);
            byte[] bytes = byteArrayOutputStream.toByteArray();
            fis = new ByteArrayInputStream(bytes);
            // 判断浏览器的类型
            String agent = request.getHeader("user-agent");
            String targetName = "electricCard_import_detail";
            if (agent.toLowerCase().indexOf("firefox") != -1) {
                targetName = new String(targetName.getBytes(), "iso8859-1");
            } else {
                targetName = URLEncoder.encode(targetName, "utf-8");
            }
            response.setContentType("application/excel;charset=utf-8");
            response.setContentLength(bytes.length);
            response.setHeader("content-Disposition", "attachment;filename=" + targetName + ".xlsx");
            // 最后输出
            os = response.getOutputStream();
            IOUtils.copy(fis, os);
            os.flush();
        } catch (Exception e) {
            log.error("errorDetail" , e);
        } finally {
            try {
                if (fis != null) {
                    fis.close();
                }
                if (os != null) {
                    os.close();
                }
                if (wb != null) {
                    wb.close();
                }
            } catch (IOException e) {
                log.error("errorDetail:" + e);
            }
        }
    }

    private void createHeader(Workbook wb, Sheet sheet){
        Row r0 = sheet.createRow(0);
        CellStyle headerStyle = wb.createCellStyle();
        headerStyle.setFillForegroundColor(IndexedColors.GREY_40_PERCENT.getIndex());
        headerStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        Cell c0 = r0.createCell(0);
        c0.setCellStyle(headerStyle);
        c0.setCellValue(messageSource.getMessage("charge.card.import.detail.name", null, LocaleContextHolder.getLocale()));
        Cell c1 = r0.createCell(1);
        c1.setCellStyle(headerStyle);
        c1.setCellValue(messageSource.getMessage("charge.card.import.detail.number", null, LocaleContextHolder.getLocale()));
        Cell c2 =r0.createCell(2);
        c2.setCellStyle(headerStyle);
        c2.setCellValue(messageSource.getMessage("charge.card.import.detail.brand", null, LocaleContextHolder.getLocale()));
        Cell c3 = r0.createCell(3);
        c3.setCellStyle(headerStyle);
        c3.setCellValue(messageSource.getMessage("xlsStartMultipleOrdersEnabled", null, LocaleContextHolder.getLocale()));
        Cell c4 = r0.createCell(4);
        c4.setCellStyle(headerStyle);
        c4.setCellValue(messageSource.getMessage("charge.card.import.detail.result", null, LocaleContextHolder.getLocale()));
    }

    @Override
    public boolean checkChargeCard(Long operatorId, String cardAlias, Long id) {
        return this.count(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(operatorId != null, ChargeCardEntity::getOperatorId, operatorId)
                .eq(StringUtils.hasText(cardAlias), ChargeCardEntity::getCardAlias, cardAlias)
                .ne(id != null, ChargeCardEntity::getId, id)
                .eq(ChargeCardEntity::getDeleted, 0)) > 0;
    }

    @Override
    public List<SimpleChargeCardVO> batchQueryCardListByUserIds(List<String> userIds) {
        List<SimpleChargeCardVO> result = new ArrayList<>();
        LambdaQueryWrapper<ChargeCardEntity> params = new LambdaQueryWrapper<>();
        params.in(ChargeCardEntity::getUserId, userIds);
        params.eq(ChargeCardEntity::getDeleted, 0);
        List<ChargeCardEntity> cardList = chargeCardMapper.selectList(params);
        log.info("batchQueryCardListByUserIds.cardList:{}", JSON.toJSONString(cardList));
        if (CollectionUtils.isEmpty(cardList)) {
            return new ArrayList<>();
        }
        for (ChargeCardEntity chargeCardEntity : cardList) {
            SimpleChargeCardVO simpleChargeCardVO = new SimpleChargeCardVO();
            simpleChargeCardVO.setCardNumber(chargeCardEntity.getCardNumber());
            simpleChargeCardVO.setUserId(chargeCardEntity.getUserId());
            result.add(simpleChargeCardVO);
        }

        return result;
    }

    @Override
    public List<String> getAllCardIdBySellerId(Long sellerId) {
        if (ObjectUtils.isEmpty(sellerId)) {
            return new ArrayList<>();
        }
        List<ChargeCardEntity> chargeCardEntityList = chargeCardMapper.selectList(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(ChargeCardEntity::getOperatorId, sellerId)
                .eq(ChargeCardEntity::getDeleted, 0));
        if (CollectionUtils.isEmpty(chargeCardEntityList)) {
            return new ArrayList<>();
        }
        List<String> resultList = chargeCardEntityList.stream().map(ChargeCardEntity::getCardNumber).collect(Collectors.toList());
        return resultList;
    }

    @Override
    @Transactional
    public ChargeCardImportVO imports(HttpServletRequest request, MultipartFile file, Long sellerId) {
        log.info("imports,sellerId={}", sellerId);
        String filename = file.getOriginalFilename();
        ChargeCardImportVO resultVo = new ChargeCardImportVO();
        if (!StringUtils.hasText(filename)) {
            log.info("imports,filename is empty.");
            return resultVo;
        }
        //文件格式错误
        if (!filename.toLowerCase().endsWith("xlsx") && !filename.toLowerCase().endsWith("xlsm")) {
            throw new MessageCodeException(PileBaseEnum.FILE_TYPE_ERROR);
        }
        ChargeCardServiceImportExcelListener listener = new ChargeCardServiceImportExcelListener(sellerId);
        try {
            EasyExcelFactory.read(file.getInputStream(), ChargeCardExcelEntity.class, listener).excelType(ExcelTypeEnum.XLSX).sheet(0).headRowNumber(2).doRead();
        } catch (IOException e) {
            log.info("imports,exception={}", e);
            //导入异常
            throw new MessageCodeException(PileBaseEnum.EXCEL_IMPORT_ERROR);
        } catch (MessageCodeException e) {
            log.info("imports,exception1={}", e);
            //Excel解释失败
            throw new MessageCodeException(PileBaseEnum.CHARGE_CARD_EXCEL_IMPORT_EXPLAIN_FAIL);
        }
        List<ChargeCardExcelEntity> dataList = listener.getDataList();
        //Excel解释结果为空
        if (CollectionUtils.isEmpty(dataList)) {
            throw new MessageCodeException(PileBaseEnum.CHARGE_CARD_EXCEL_IMPORT_NULL);
        }
        log.info("imports,readSize={}", dataList.size());
        if (admin.longValue() == sellerId.longValue()) {
            this.dealWithAdmin(dataList, resultVo);
        } else {
            this.dealWithSeller(dataList, resultVo, sellerId);
        }
        return resultVo;
    }

    private void dealWithSeller(List<ChargeCardExcelEntity> dataList, ChargeCardImportVO resultVo, Long sellerId) {
        Result<SellerInfoVO> detail = pileUserServiceFeign.detail(sellerId);
        log.info("imports,detail={}", JSON.toJSONString(detail));
        String operatorName = null;
        if (!ObjectUtils.isEmpty(detail) && detail.getCode() == 200 && !ObjectUtils.isEmpty(detail.getData())) {
            //设置商家名称
            operatorName = detail.getData().getName();
        }
        List<ChargeCardErrorDTO> errorList = new ArrayList<>();
        List<ChargeCardExcelEntity> autelList = new ArrayList<>();
        List<ChargeCardExcelEntity> othersList = new ArrayList<>();
        //道通充电卡和第三方充电卡校验
        dataList.forEach(c -> {
            String cardNumber = c.getCardNumber();
            String cardBrand = c.getCardBrand();
            String cardAlias = c.getCardAlias();
            String startMultipleOrdersEnabled = c.getStartMultipleOrdersEnabled();
            if (StringUtils.isEmpty(cardBrand)) {
                ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.CARD_BRAND_NULL.getCode()).build();
                chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                errorList.add(chargeCardErrorDTO);
            } else if (CardBrandEnum.AUTEL.getDesc().equals(cardBrand)) {
                autelList.add(c);
            } else {
                othersList.add(c);
            }
        });
        //第三方卡校验
        List<ChargeCardEntity> insertList = new ArrayList<>(othersList.size());
        //道通卡校验
        List<ChargeCardEntity> updateList = new ArrayList<>(autelList.size());
        //客户组
        Map<Long, ChargeCardExcelEntity> convertMap = new HashMap<>();
        List<String> nameList = this.validateAutel(autelList, errorList, updateList, sellerId, operatorName, convertMap);
        this.validateOthers(othersList, errorList, insertList, nameList, sellerId, operatorName, convertMap);
        log.info("imports,seller insertSize={},errorSize={},updateList={}", insertList.size(), errorList.size(), updateList.size());
        List<ChargeCardEntity> memberList = new ArrayList<>();
        if (!insertList.isEmpty()) {
            this.saveBatch(insertList);
            updateStartMultipleOrdersEnabled(insertList);
            memberList.addAll(insertList);
        }
        if (!updateList.isEmpty()) {
            this.saveOrUpdateBatch(updateList);
            updateStartMultipleOrdersEnabled(updateList);
            memberList.addAll(updateList);
        }
        resultVo.setSuccess(memberList.size());
        if (!memberList.isEmpty()) {
            //批量生成C端用户
            this.batchInsertMember(memberList, sellerId, convertMap);
        }
        if (!errorList.isEmpty()) {
            resultVo.setFail(errorList.size());
            resultVo.setCode(IdWorker.getIdStr());
            //保存错误信息redis
            this.setRedisChargeCard(memberList, errorList, resultVo.getCode(), sellerId);
        }
        log.info("imports,seller end.");
    }

    private void updateStartMultipleOrdersEnabled(List<ChargeCardEntity> chargeCardList) {
        if (!CollectionUtils.isEmpty(chargeCardList)) {
            chargeCardList.forEach(c -> {
                Boolean startMultipleOrdersEnabled = c.getStartMultipleOrdersEnabled() == null ? false : c.getStartMultipleOrdersEnabled();
                String startMultipleOrdersEnabledCardKey = RedisKeyConstant.getStartMultipleOrdersEnabledCard(c.getCardNumber());
                if (startMultipleOrdersEnabled && c.getBindStatus() == 1) {
                    stringRedisTemplate.opsForValue().set(startMultipleOrdersEnabledCardKey, c.getUserId().toString());
                } else {
                    stringRedisTemplate.delete(startMultipleOrdersEnabledCardKey);
                }
            });
        }
    }

    private void validateOthers(List<ChargeCardExcelEntity> othersList, List<ChargeCardErrorDTO> errorList, List<ChargeCardEntity> insertList, List<String> list, Long sellerId, String operatorName, Map<Long, ChargeCardExcelEntity> convertMap) {
        if (CollectionUtils.isEmpty(othersList)) {
            return;
        }
        //列表数据校验
        this.validateChargeCard(othersList, errorList, insertList, list, sellerId, operatorName, convertMap);
        List<ChargeCardEntity> checkList = new ArrayList<>(insertList);
        insertList.clear();
        //数据库数据校验
        if (!CollectionUtils.isEmpty(checkList)) {
            //卡名称已存在
            List<ChargeCardEntity> nameList = this.findList(checkList.stream().map(ChargeCardEntity::getCardAlias).collect(Collectors.toList()), sellerId);
            List<String> nameMap = null;
            if (!CollectionUtils.isEmpty(nameList)) {
                nameMap = nameList.stream().map(ChargeCardEntity::getCardAlias).collect(Collectors.toList());
            }
            //卡号已存在
            List<ChargeCardEntity> existList = this.findListV2(checkList.stream().map(ChargeCardEntity::getCardNumber).collect(Collectors.toList()), sellerId);
            List<String> existMap = null;
            Map<String, ChargeCardEntity> existEntity = null;
            if (!CollectionUtils.isEmpty(existList)) {
                existMap = existList.stream().map(ChargeCardEntity::getCardNumber).collect(Collectors.toList());
                existEntity = existList.stream().collect(Collectors.toMap(ChargeCardEntity::getCardNumber, e -> e, (e1, e2) -> e1));
            }
            List<String> finalExistMap = existMap;
            List<String> finalNameMap = nameMap;
            Map<String, ChargeCardEntity> finalExistEntity = existEntity;
            checkList.forEach(check -> {
                String cardNumber = check.getCardNumber();
                String cardAlias = check.getCardAlias();
                Integer cardBrand = check.getCardBrand();
                String startMultipleOrdersEnabled = "0";
                if (check.getStartMultipleOrdersEnabled() != null) {
                    if (check.getStartMultipleOrdersEnabled()) {
                        startMultipleOrdersEnabled = "1";
                    } else {
                        startMultipleOrdersEnabled = "0";
                    }
                }
                if (!CollectionUtils.isEmpty(finalNameMap) && finalNameMap.contains(cardAlias)) {
                    ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(CardBrandEnum.getDescByCode(cardBrand)).errorType(ChargeCardErrorEnum.NAME_REPEAT.getCode()).build();
                    chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                    errorList.add(chargeCardErrorDTO);
                } else {
                    if (!CollectionUtils.isEmpty(finalExistMap) && finalExistMap.contains(cardNumber)) {
                        ChargeCardEntity entity = finalExistEntity.get(cardNumber);
                        if (!entity.getCardBrand().equals(cardBrand) && entity.getCardBrand().equals(CardBrandEnum.AUTEL.getCode())) {
                            ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(CardBrandEnum.getDescByCode(cardBrand)).errorType(ChargeCardErrorEnum.CHARGE_CARD_BRAND_ERROR.getCode()).build();
                            chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                            errorList.add(chargeCardErrorDTO);
                        } else {
                            ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(CardBrandEnum.getDescByCode(cardBrand)).errorType(ChargeCardErrorEnum.CARD_NUMBER_EXIST.getCode()).build();
                            chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                            errorList.add(chargeCardErrorDTO);
                        }
                    } else {
                        insertList.add(check);
                    }
                }
            });
        }
    }

    private List<String> validateAutel(List<ChargeCardExcelEntity> autelList, List<ChargeCardErrorDTO> errorList, List<ChargeCardEntity> updateList, Long sellerId, String operatorName, Map<Long, ChargeCardExcelEntity> convertMap) {
        if (CollectionUtils.isEmpty(autelList)) {
            return new ArrayList<>();
        }
        //列表数据校验
        List<String> nameList = this.validateChargeCard(autelList, errorList, updateList, null, sellerId, operatorName, convertMap);
        List<ChargeCardEntity> checkList = new ArrayList<>(updateList);
        updateList.clear();
        //数据库数据校验
        if (!CollectionUtils.isEmpty(checkList)) {
            //卡号已存在
            List<ChargeCardEntity> existList = this.findListV2(checkList.stream().map(ChargeCardEntity::getCardNumber).collect(Collectors.toList()), sellerId);
            Map<String, ChargeCardEntity> existMap = null;
            if (!CollectionUtils.isEmpty(existList)) {
                existMap = existList.stream().collect(Collectors.toMap(ChargeCardEntity::getCardNumber, e -> e, (e1, e2) -> e1, ConcurrentHashMap::new));
            }
            Map<String, ChargeCardEntity> finalExistMap = existMap;
            checkList.stream().forEach(check -> {
                String cardNumber = check.getCardNumber();
                String cardAlias = check.getCardAlias();
                Integer cardBrand = check.getCardBrand();
                String startMultipleOrdersEnabled = "0";
                if (check.getStartMultipleOrdersEnabled() != null) {
                    if (check.getStartMultipleOrdersEnabled()) {
                        startMultipleOrdersEnabled = "1";
                    } else {
                        startMultipleOrdersEnabled = "0";
                    }
                }

                if (!CollectionUtils.isEmpty(finalExistMap)
                        && finalExistMap.get(cardNumber) != null) {
                    if (finalExistMap.get(cardNumber).getUserId() != null) {
                        ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(CardBrandEnum.getDescByCode(cardBrand)).errorType(ChargeCardErrorEnum.CARD_NUMBER_EXIST.getCode()).build();
                        chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                        //卡号已被绑定
                        errorList.add(chargeCardErrorDTO);
                    } else {
                        check.setId(finalExistMap.get(cardNumber).getId());
                        updateList.add(check);
                    }
                } else {
                    updateList.add(check);
                }
            });
        }
        return nameList;
    }

    private List<String> validateChargeCard(List<ChargeCardExcelEntity> autelList, List<ChargeCardErrorDTO> errorList, List<ChargeCardEntity> updateList, List<String> list, Long sellerId, String operatorName, Map<Long, ChargeCardExcelEntity> convertMap) {
        List<String> existNameList = new Vector<>();
        if (!CollectionUtils.isEmpty(list)) {
            existNameList.addAll(list);
        }
        List<String> existCardNoList = new Vector<>();
        autelList.forEach(entity -> {
            String cardNumber = entity.getCardNumber();
            String cardBrand = entity.getCardBrand();
            String cardAlias = entity.getCardAlias();
            String startMultipleOrdersEnabled = entity.getStartMultipleOrdersEnabled();
            //卡号异常处理
            if (StringUtils.isEmpty(cardNumber) || cardNumber.length() > 50) {
                if (StringUtils.isEmpty(cardNumber)) {
                    errorList.add(ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.CARD_NUMBER_EMPTY.getCode()).build());
                } else {
                    ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardAlias(cardAlias).cardNumber(cardNumber).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.CARD_NUMBER_LIMIT.getCode()).build();
                    chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                    errorList.add(chargeCardErrorDTO);
                }
                return;
            }
            //充电卡名称为空
            if (StringUtils.isEmpty(cardAlias) || cardAlias.length() > 100) {
                if (StringUtils.isEmpty(cardAlias)) {
                    errorList.add(ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.NAME_NULL.getCode()).build());
                } else {
                    ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardAlias(cardAlias).cardNumber(cardNumber).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.CARD_NAME_LIMIT.getCode()).build();
                    chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                    errorList.add(chargeCardErrorDTO);
                }
            } else {
                //充电卡名称重复
                if (!CollectionUtils.isEmpty(existNameList) && existNameList.contains(cardAlias)) {
                    ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardBrand(cardBrand).cardAlias(cardAlias).errorType(ChargeCardErrorEnum.NAME_REPEAT.getCode()).build();
                    chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                    errorList.add(chargeCardErrorDTO);
                } else {
                    //卡号错误
                    if (!CommonUtil.checkChargeCard(cardNumber)) {
                        ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardBrand(cardBrand).cardAlias(cardAlias).errorType(ChargeCardErrorEnum.CARD_NUMBER_ERROR.getCode()).build();
                        chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                        errorList.add(chargeCardErrorDTO);
                    } else {
                        //充电卡号重复
                        if (!CollectionUtils.isEmpty(existCardNoList) && existCardNoList.contains(cardNumber)) {
                            ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.CARD_NUMBER_EXIST.getCode()).build();
                            chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                            errorList.add(chargeCardErrorDTO);
                        } else {
                            if (StringUtils.isEmpty(cardBrand)) {
                                ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder().cardNumber(cardNumber).cardAlias(cardAlias).cardBrand(cardBrand).errorType(ChargeCardErrorEnum.CARD_BRAND_NULL.getCode()).build();
                                chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                                errorList.add(chargeCardErrorDTO);
                            } else {
                                ChargeCardEntity e = new ChargeCardEntity();
                                e.setCardNumber(cardNumber.toUpperCase());
                                e.setCardType(ChargeCardTypeEnum.COMPANY.getType());
                                e.setCardBrand(CardBrandEnum.getCodeByDesc(cardBrand));
                                e.setOperatorId(sellerId);
                                e.setCardAlias(cardAlias);
                                e.setCardPartyType(CardPartyTypeEnum.OTHER.getValue());
                                e.setOperatorName(operatorName);
                                e.setBindStatus(1);
                                e.setBindTime(System.currentTimeMillis());
                                e.setUserId(IdWorker.getId());
                                e.setStartMultipleOrdersEnabled(false);
                                if (!StringUtils.isEmpty(startMultipleOrdersEnabled) && "1".equals(startMultipleOrdersEnabled)) {
                                    e.setStartMultipleOrdersEnabled(true);
                                }
                                String memberGroupName = entity.getMemberGroupName();
                                String driverName = entity.getDriverName();
                                if (StringUtils.hasText(memberGroupName) || StringUtils.hasText(driverName)) {
                                    ChargeCardExcelEntity convert = new ChargeCardExcelEntity();
                                    convert.setMemberGroupName(memberGroupName);
                                    convert.setDriverName(driverName);
                                    convertMap.put(e.getUserId(), convert);
                                }
                                updateList.add(e);
                                existNameList.add(cardAlias);
                                existCardNoList.add(cardNumber);
                            }
                        }
                    }
                }
            }
        });
        return existNameList;
    }

    private void batchInsertMember(List<ChargeCardEntity> insertList, Long sellerId, Map<Long, ChargeCardExcelEntity> convertMap) {
        if (CollectionUtils.isEmpty(insertList)) {
            return;
        }
        Map<String, Long> mapToUse = new HashMap<>();
        Map<String, Long> driverNameMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(convertMap)) {
            List<MemberGroupVO> tmpList = this.pileUserFeign.getMemberGroupBySellerId(sellerId).getData();
            if (!CollectionUtils.isEmpty(tmpList)) {
                tmpList.stream().forEach(e -> mapToUse.put(e.getName(), e.getId()));
            }
            DriverParamDTO paramDto = new DriverParamDTO();
            paramDto.setSellerId(sellerId);
            List<DriverVO> driverList = this.pileUserFeign.driverFindList(paramDto).getData();
            if (!CollectionUtils.isEmpty(driverList)) {
                driverList.stream().forEach(e -> driverNameMap.put(e.getName(), e.getId()));
            }
        }

        MemberForChargeCardDTO dto = new MemberForChargeCardDTO();
        dto.setSellerId(sellerId);
        List<MemberInfoDTO> dtoList = new ArrayList<>();
        int batch = 1000;
        int row = 1;
        int size = insertList.size();
        for (ChargeCardEntity entity : insertList) {
            MemberInfoDTO infoDTO = new MemberInfoDTO();
            infoDTO.setCardNumber(entity.getCardNumber());
            Long userId = entity.getUserId();
            infoDTO.setUserId(userId);
            if (!CollectionUtils.isEmpty(convertMap)) {
                ChargeCardExcelEntity convert = convertMap.get(userId);
                //设置客户组
                if (!CollectionUtils.isEmpty(mapToUse) && convert != null) {
                    String tmp = convert.getMemberGroupName();
                    if (StringUtils.hasText(tmp)) {
                        List<String> tmpList = Arrays.stream(tmp.split(",")).collect(Collectors.toList());
                        List<Long> memberGroupIds = new ArrayList<>();
                        tmpList.stream().forEach(n -> {
                            Long memberGroupId = mapToUse.get(n);
                            if (memberGroupId == null) {
                                return;
                            }
                            memberGroupIds.add(memberGroupId);
                        });
                        infoDTO.setMemberGroupIds(memberGroupIds);
                    }
                }
                //关联车主
                if (!CollectionUtils.isEmpty(driverNameMap) && convert != null) {
                    String driverName = convert.getDriverName();
                    if (StringUtils.hasText(driverName)) {
                        Long driverId = driverNameMap.get(driverName);
                        infoDTO.setDriverId(driverId);
                    }
                }
            }
            dtoList.add(infoDTO);
            if (row % batch == 0 || row == size) {
                dto.setMemberList(dtoList);
                Result<Boolean> batchResult = pileUserFeign.addForChargeCard(dto);
                log.info("imports,row={},batchResult={}", row, JSON.toJSONString(batchResult));
                if (batchResult == null || batchResult.getCode() != 200 || batchResult.getData() == null) {
                    throw new MessageCodeException(PileBaseEnum.EXCEL_IMPORT_ERROR);
                }
                dtoList.clear();
                dto.setMemberList(null);
            }
            row++;
        }
    }

    private void setRedisChargeCard(List<ChargeCardEntity> insertList, List<ChargeCardErrorDTO> errorList, String code, Long sellerId) {
        List<ChargeCardErrorDTO> redisList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(insertList)) {
            insertList.forEach(entity -> {
                ChargeCardErrorDTO chargeCardErrorDTO = ChargeCardErrorDTO.builder()
                        .cardAlias(entity.getCardAlias())
                        .cardBrand(CardBrandEnum.getDescByCode(entity.getCardBrand()))
                        .cardNumber(entity.getCardNumber())
                        .errorType(ChargeCardErrorEnum.SUCCESS.getCode()).build();
                String startMultipleOrdersEnabled = "0";
                if (entity.getStartMultipleOrdersEnabled() != null) {
                    if (entity.getStartMultipleOrdersEnabled()) {
                        startMultipleOrdersEnabled = "1";
                    } else {
                        startMultipleOrdersEnabled = "0";
                    }
                }
                chargeCardErrorDTO.setStartMultipleOrdersEnabled(startMultipleOrdersEnabled);
                redisList.add(chargeCardErrorDTO);
            });
        }
        redisList.addAll(errorList);
        if (redisList.isEmpty()) {
            return;
        }
        log.info("imports,redis insertSize={},errorSize={}", insertList.size(), errorList.size());
        //暂存一天
        redisTemplates.opsForValue().set(String.format(RedisKeyConstant.getStringPileBaseChargeCardError(), sellerId, code), redisList, 1, TimeUnit.DAYS);
        log.info("imports,redis end.");
    }

    private void dealWithAdmin(List<ChargeCardExcelEntity> dataList, ChargeCardImportVO resultVo) {
        List<ChargeCardErrorDTO> errorList = new ArrayList<>();
        List<ChargeCardEntity> checkList = new ArrayList<>();
        List<String> existCardNoList = new ArrayList<>();
        dataList.forEach(entity -> {
            String cardNumber = entity.getCardNumber();
            String cardType = entity.getCardType();
            //充电卡号重复
            if (!CollectionUtils.isEmpty(existCardNoList) && existCardNoList.contains(cardNumber)) {
                errorList.add(ChargeCardErrorDTO.builder().cardNumber(cardNumber).errorType(ChargeCardErrorEnum.CARD_NUMBER_EXIST.getCode()).build());
            } else {
                ChargeCardEntity e = new ChargeCardEntity();
                e.setCardNumber(cardNumber.toUpperCase());
                if (StringUtils.hasText(cardType)) {
                    if (ChargeCardTypeEnum.RFID.equals(cardType)) {
                        e.setCardType(ChargeCardTypeEnum.RFID.getType());
                    } else {
                        e.setCardType(ChargeCardTypeEnum.getTypeByDesc(cardType, messageSource));
                    }
                } else {
                    e.setCardType(ChargeCardTypeEnum.RFID.getType());
                }
                e.setCardBrand(CardBrandEnum.AUTEL.getCode());
                e.setCardPartyType(CardPartyTypeEnum.AUTEL.getValue());
                checkList.add(e);
                existCardNoList.add(cardNumber);
            }
        });
        List<ChargeCardEntity> insertList = new ArrayList<>(checkList);
        if (!CollectionUtils.isEmpty(checkList)) {
            //卡号已存在
            List<ChargeCardEntity> existList = this.findList(checkList.stream().map(ChargeCardEntity::getCardNumber).collect(Collectors.toList()), admin);
            if (!CollectionUtils.isEmpty(existList)) {
                Map<String, ChargeCardEntity> existMap = existList.stream().collect(Collectors.toMap(ChargeCardEntity::getCardNumber, e -> e, (e1, e2) -> e1, ConcurrentHashMap::new));
                checkList.forEach(check -> {
                    String cardNumber = check.getCardNumber();
                    if (existMap.get(cardNumber) != null) {
                        errorList.add(ChargeCardErrorDTO.builder().cardNumber(cardNumber).errorType(ChargeCardErrorEnum.CARD_NUMBER_EXIST.getCode()).build());
                        insertList.remove(check);
                    }
                });
            }
        }
        log.info("imports,admin insertSize={},errorSize={}", insertList.size(), errorList.size());
        if (!insertList.isEmpty()) {
            this.saveBatch(insertList);
        }
        resultVo.setSuccess(insertList.size());
        if (!errorList.isEmpty()) {
            resultVo.setFail(errorList.size());
            resultVo.setCode(IdWorker.getIdStr());
            this.setRedisChargeCard(insertList, errorList, resultVo.getCode(), admin);
        }
        log.info("imports,admin end.");
    }

    @Override
    public Integer updateBatchByCardNumber(List<ChargeCardEntity> datas) {
        return chargeCardMapper.updateBatchByCardNumber(datas);
    }

    @Override
    public Result<ChargeCardInfoVO> findCard(String cardNumber, Long operatorId) {
        log.info("findCard,cardNumber={}", cardNumber);
        ChargeCardEntity chargeCardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(ChargeCardEntity::getCardNumber, cardNumber)
                .eq(!ObjectUtils.isEmpty(operatorId), ChargeCardEntity::getOperatorId, operatorId)
                .eq(ChargeCardEntity::getDeleted, 0));
        log.info("findCard,chargeCardEntity={}", JSON.toJSONString(chargeCardEntity));
        if (chargeCardEntity == null) {
            chargeCardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getCardNumber, cardNumber)
                    .eq(!ObjectUtils.isEmpty(operatorId), ChargeCardEntity::getUserId, operatorId)
                    .or(m -> m.isNull(ChargeCardEntity::getOperatorId).eq(ChargeCardEntity::getDeleted, 0).eq(ChargeCardEntity::getCardNumber, cardNumber))
                    .eq(ChargeCardEntity::getDeleted, 0));
            if (ObjectUtils.isEmpty(chargeCardEntity)) {
                return Result.ofSucceed(null);
            }
        }
        ChargeCardInfoVO chargeCardVO = new ChargeCardInfoVO();
        BeanUtils.copyProperties(chargeCardEntity, chargeCardVO);
        log.info("findCard,chargeCardVO={}", JSON.toJSONString(chargeCardVO));
        return Result.ofSucceed(chargeCardVO);
    }

    @Override
    public boolean insertBatchByCardNumber(List<ChargeCardEntity> datas, boolean cover) {
        TableInfo tableInfo = TableInfoHelper.getTableInfo(this.entityClass);
        return this.executeBatch(datas, ((sqlSession, entity) -> {
            ChargeCardEntity cardEntity = this.getBaseMapper().selectOne(new LambdaQueryWrapper<ChargeCardEntity>()
                    .select(ChargeCardEntity::getId, ChargeCardEntity::getVersion)
                    .eq(ChargeCardEntity::getCardNumber, entity.getCardNumber())
                    .eq(ChargeCardEntity::getDeleted, 0));
            //数据已存在是否更新
            if (cardEntity != null) {
                if (cover) {
                    entity.setId(cardEntity.getId());
                    entity.setVersion(cardEntity.getVersion());
                    MapperMethod.ParamMap<ChargeCardEntity> param = new MapperMethod.ParamMap<>();
                    param.put("et", entity);
                    sqlSession.update(tableInfo.getSqlStatement(SqlMethod.UPDATE_BY_ID.getMethod()), param);
                } else {
                    log.info("insertBatchByCardNumber,skip cardNumber={}", entity.getCardNumber());
                }
            } else {
                sqlSession.insert(tableInfo.getSqlStatement(SqlMethod.INSERT_ONE.getMethod()), entity);
            }
        }));
    }

    @Override
    public Result<Boolean> syncCard(List<String> cardNumbers) {
        int batch = 1000;
        log.info("syncCard,openIds={}", JSON.toJSONString(cardNumbers));
        //数据较多多线程处理
        Integer count;
        if (CollectionUtils.isEmpty(cardNumbers)) {
            count = chargeCardMapper.countAll();
        } else {
            count = cardNumbers.size();
        }
        log.info("syncCard,count={}", count);
        if (count != null && count > 0) {
            for (int i = 0; i < count; i++) {
                if (i % batch == 0) {
                    log.info("syncCard,i={}", i);
                    int finalI = i;
                    ThreadPoolUtil.getExecutor().execute(() -> {
                        List<CardOldDTO> list = chargeCardMapper.selectByBatch(finalI, batch, cardNumbers);
                        if (!CollectionUtils.isEmpty(list)) {
                            List<CardOldDTO> insertList = new ArrayList<>(list);
                            List<ChargeCardEntity> existList = this.list(new LambdaQueryWrapper<ChargeCardEntity>().select(ChargeCardEntity::getId, ChargeCardEntity::getCardNumber)
                                    .in(ChargeCardEntity::getCardNumber, list.stream().filter(m -> StringUtils.hasText(m.getCardNumber())).map(CardOldDTO::getCardNumber).collect(Collectors.toList()))
                                    .eq(ChargeCardEntity::getDeleted, 0));
                            if (!CollectionUtils.isEmpty(existList)) {
                                for (CardOldDTO m : list) {
                                    for (ChargeCardEntity o : existList) {
                                        if (StringUtils.isEmpty(m.getCardNumber()) || m.getCardNumber().equals(o.getCardNumber())) {
                                            insertList.remove(m);
                                            log.info("syncCard,m={}", JSON.toJSONString(m));
                                            break;
                                        }
                                    }
                                }
                            }
                            log.info("syncCard,insertList={}", insertList.size());
                            if (!CollectionUtils.isEmpty(insertList)) {
                                boolean insert = chargeCardMapper.saveBatch(insertList);
                                log.info("syncCard,insert={}", insert);
                            }
                        }
                    });
                }
            }
        }
        return Result.ofSucceed(true);
    }

    @Override
    public Result<ChargeCardPageVO> getCardInfoByNumber(String cardNumber) {
        final ChargeCardPageVO cardInfoByNumber = chargeCardMapper.getCardInfoByNumber(cardNumber);
        return Result.ofSucceed(cardInfoByNumber);
    }

    @Override
    public Result<ChargeCardPageVO> getCardInfoByNumberAndUser(String cardNumber, Long userId, Long operatorId) {
        ChargeCardPageVO chargeCardPageVO = null;

        //查询
        LambdaQueryWrapper<ChargeCardEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(ChargeCardEntity.class)
                .select(ChargeCardEntity::getCardAlias, ChargeCardEntity::getCardBrand, ChargeCardEntity::getOperatorId)
                .eq(ChargeCardEntity::getCardNumber, cardNumber)
                .orderByDesc(ChargeCardEntity::getId);
        List<ChargeCardEntity> chargeCardEntities = chargeCardMapper.selectList(lambdaQueryWrapper);
        log.info("====chargeCardEntities:{}", JSONArray.toJSONString(chargeCardEntities));
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(chargeCardEntities)) {
            return Result.ofSucceed(chargeCardPageVO);
        }
        ChargeCardEntity chargeCardEntity = chargeCardEntities.get(0);
        if (operatorId != null && chargeCardEntities.size() > 1) {
            for (ChargeCardEntity temp : chargeCardEntities) {
                if (operatorId.equals(temp.getOperatorId())) {
                    chargeCardEntity = temp;
                    break;
                }
            }
        }

        //封装返回对象
        if (chargeCardEntity != null) {
            chargeCardPageVO = new ChargeCardPageVO();
            chargeCardPageVO.setCardAlias(chargeCardEntity.getCardAlias());
            chargeCardPageVO.setCardBrand(chargeCardEntity.getCardBrand());
        }
        log.info("====chargeCardEntity:{}", JSONObject.toJSONString(chargeCardEntity));
        log.info("====chargeCardPageVO:{}", JSONObject.toJSONString(chargeCardPageVO));
        return Result.ofSucceed(chargeCardPageVO);
    }

    @Override
    public Result<List<ChargeCardPageVO>> batchGetCardInfoByNumber(BatchQueryCardInfoParamDTO batchQueryCardInfoParamDTO) {

        //查询
        LambdaQueryWrapper<ChargeCardEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(ChargeCardEntity.class)
                .select(ChargeCardEntity::getCardAlias, ChargeCardEntity::getCardBrand, ChargeCardEntity::getOperatorId, ChargeCardEntity::getCardNumber)
                .in(ChargeCardEntity::getCardNumber, batchQueryCardInfoParamDTO.getCardNum())
                .eq(ChargeCardEntity::getOperatorId, batchQueryCardInfoParamDTO.getSellerId())
                .orderByDesc(ChargeCardEntity::getId);
        List<ChargeCardEntity> chargeCardEntities = chargeCardMapper.selectList(lambdaQueryWrapper);
        List<ChargeCardPageVO> voList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(chargeCardEntities)) {
            chargeCardEntities.stream().forEach(item -> {
                ChargeCardPageVO chargeCardPageVO = new ChargeCardPageVO();
                chargeCardPageVO.setCardNumber(item.getCardNumber());
                chargeCardPageVO.setCardAlias(item.getCardAlias());
                chargeCardPageVO.setCardBrand(item.getCardBrand());
                chargeCardPageVO.setCardType(item.getCardType());
                voList.add(chargeCardPageVO);
            });
        }
        return Result.ofSucceed(voList);
    }

    @Override
    public List<SimpleChargeCardVO> batchQueryCardList(List<String> cardNumberList) {
        List<SimpleChargeCardVO> result = new ArrayList<>();
        QueryWrapper<ChargeCardEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.select("card_number", "user_id", "card_party_type")
                .in("card_number", cardNumberList)
                .isNotNull("user_id")
                .eq("deleted", 0);
        List<ChargeCardEntity> cardList = chargeCardMapper.selectList(queryWrapper);

        if (!CollectionUtils.isEmpty(cardList)) {
            cardList.forEach(c -> {
                SimpleChargeCardVO chargeCardVO = SimpleChargeCardVO.builder().cardNumber(c.getCardNumber().toUpperCase()).userId(c.getUserId()).cardPartyType(c.getCardPartyType()).build();
                result.add(chargeCardVO);
            });
        }

        return result;
    }

    @Override
    public Page<ChargeCardEntity> batchQueryCardListForHubject(HashMap<String, Long> lastUpdate) {
        Long value = lastUpdate.get("lastUpdate");
        Long size = lastUpdate.getOrDefault("size", 100L);
        Long current = lastUpdate.getOrDefault("current", 1L);
        LambdaQueryWrapper<ChargeCardEntity> eq = Wrappers.lambdaQuery(ChargeCardEntity.class)
                .ge(value != null, ChargeCardEntity::getUpdateTime, value)
                .isNotNull(ChargeCardEntity::getUserId)
                .eq(ChargeCardEntity::getLimitEvseId, "-1")
                .eq(ChargeCardEntity::getLimitLocationId, "-1")
                .or(value != null, (wrapper) -> wrapper.ge(ChargeCardEntity::getCreateTime, value).
                        isNotNull(ChargeCardEntity::getUserId)
                        .eq(ChargeCardEntity::getLimitEvseId, "-1")
                        .eq(ChargeCardEntity::getLimitLocationId, "-1"));

        Page<ChargeCardEntity> page = new Page<>(current, size);
        return chargeCardMapper.selectPage(page, eq);
    }

    public void createDropDownList(Sheet sheet, String[] values, int firstRow, int lastRow, int firstCol, int lastCol) {
        DataValidationHelper helper = sheet.getDataValidationHelper();
        CellRangeAddressList addressList = new CellRangeAddressList(firstRow, lastRow, firstCol, lastCol);
        DataValidationConstraint constraint = helper.createExplicitListConstraint(values);
        DataValidation dataValidation = helper.createValidation(constraint, addressList);
        if (dataValidation instanceof HSSFDataValidation) {
            dataValidation.setSuppressDropDownArrow(false);
        } else {
            dataValidation.setSuppressDropDownArrow(true);
            dataValidation.setShowErrorBox(true);
        }

        sheet.addValidationData(dataValidation);
    }


    private boolean isAllowCharge(String locationId, String evseId, ChargeCardEntity card) {
        boolean
                allowAllStation = StrUtil.isEmpty(card.getLimitLocationId()) || Objects.equals("-1", card.getLimitLocationId()),
                allowAllPile = StrUtil.isEmpty(card.getLimitEvseId()) || Objects.equals("-1", card.getLimitEvseId());
        if (allowAllStation && allowAllPile) {
            return true;
        } else if (Objects.equals(false, allowAllStation) && allowAllPile) {
            return in(locationId, card.getLimitLocationId());
        } else if (allowAllStation) {
            return in(evseId, card.getLimitEvseId());
        } else {
            return in(locationId, card.getLimitLocationId())
                    && in(evseId, card.getLimitEvseId());
        }
    }

    private boolean in(String id, String limit) {
        Optional<String> first =
                Arrays.stream(limit.split(",")).filter(s -> Objects.equals(id, s)).findFirst();
        return first.isPresent();
    }

    @Override
    public Integer getSameNameCard(Long userId, String name, String cardNumber) {
        LambdaQueryWrapper<ChargeCardEntity> params = new LambdaQueryWrapper<>();
        params.eq(ChargeCardEntity::getUserId, userId);
        params.eq(ChargeCardEntity::getCardAlias, name);
        params.ne(ChargeCardEntity::getCardNumber, cardNumber);
        return chargeCardMapper.selectCount(params);
    }

    @Override
    public Boolean syncCardName(Long userId, String name, String cardNumber) {
        LambdaQueryWrapper<ChargeCardEntity> params = new LambdaQueryWrapper<>();
        params.eq(ChargeCardEntity::getCardNumber, cardNumber);
        params.eq(ChargeCardEntity::getUserId, userId);
        params.last(BaseConstant.LIMIT_1);
        ChargeCardEntity chargeCard = chargeCardMapper.selectOne(params);
        if (chargeCard != null) {
            chargeCard.setCardAlias(name);
            chargeCardMapper.updateById(chargeCard);
        }
        return true;
    }

    @Override
    public List<String> getCardNumberListByUser(String userId) {
        LambdaQueryWrapper<ChargeCardEntity> params = new LambdaQueryWrapper<>();
        params.eq(ChargeCardEntity::getUserId, userId);
        params.eq(ChargeCardEntity::getDeleted, false);
        List<ChargeCardEntity> cardList = chargeCardMapper.selectList(params);
        if (CollectionUtils.isEmpty(cardList)) {
            return new ArrayList<>();
        }

        return cardList.stream().map(ChargeCardEntity::getCardAlias).collect(Collectors.toList());
    }

    @Override
    public String getCardNameByCardNumber(String cardNumber, String userId) {
        LambdaQueryWrapper<ChargeCardEntity> params = new LambdaQueryWrapper<>();
        params.eq(ChargeCardEntity::getCardNumber, cardNumber);
        params.eq(ChargeCardEntity::getUserId, userId);
        params.last(BaseConstant.LIMIT_1);
        ChargeCardEntity chargeCard = chargeCardMapper.selectOne(params);
        if (chargeCard == null) {
            return "";
        }

        return chargeCard.getCardAlias();
    }

    @Override
    public Result<Boolean> bindCard(ChargeCardDTO chargeCardDTO) {
        log.info("bindCard,chargeCardDTO={}", chargeCardDTO);
        chargeCardDTO.setNumber(chargeCardDTO.getNumber().toUpperCase());
        ChargeCardEntity cardEntity = this.getOne(new LambdaQueryWrapper<ChargeCardEntity>()
                .select(ChargeCardEntity::getUserId, ChargeCardEntity::getId, ChargeCardEntity::getVersion)
                .eq(ChargeCardEntity::getCardNumber, chargeCardDTO.getNumber())
                .eq(ChargeCardEntity::getDeleted, false)
                .last(BaseConstant.LIMIT_1));
        log.info("bindCard,cardEntity={}", cardEntity);
        if (cardEntity != null) {
            Long userId = cardEntity.getUserId();
            if (Objects.nonNull(userId) && !chargeCardDTO.getUserId().equals(userId)) {
                return Result.ofSucceed(false);
            }
            int count = this.count(new LambdaQueryWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getUserId, chargeCardDTO.getUserId())
                    .eq(ChargeCardEntity::getDeleted, false));
            log.info("bindCard,count={}", count);
            if (count >= 100) {
                return Result.ofSucceed(false);
            }

            Integer cardPartyType = cardEntity.getCardPartyType();
            if (cardPartyType == null) {
                cardPartyType = CardPartyTypeEnum.OTHER.getValue();
            }

            log.info("更新公用卡名称");
            update(new LambdaUpdateWrapper<ChargeCardEntity>()
                    .eq(ChargeCardEntity::getId, cardEntity.getId())
                    .set(ChargeCardEntity::getVersion, cardEntity.getVersion())
                    .set(ChargeCardEntity::getUserId, chargeCardDTO.getUserId())
                    .set(ChargeCardEntity::getBindTime, System.currentTimeMillis())
                    .set(ChargeCardEntity::getCardAlias, chargeCardDTO.getName())
                    .set(ChargeCardEntity::getCardPartyType, cardPartyType)
                    .set(ChargeCardEntity::getUpdateTime, System.currentTimeMillis())
                    .set(ChargeCardEntity::getBindStatus, true));
        } else {
            cardEntity = new ChargeCardEntity();
            cardEntity.setCardAlias(chargeCardDTO.getName());
            cardEntity.setCardNumber(chargeCardDTO.getNumber());
            cardEntity.setUserId(chargeCardDTO.getUserId());
            cardEntity.setBindTime(System.currentTimeMillis());
            cardEntity.setUpdateTime(System.currentTimeMillis());
            cardEntity.setCardType(ChargeCardTypeEnum.OTHER.getType());
            cardEntity.setCardPartyType(CardPartyTypeEnum.OTHER.getValue());
            cardEntity.setBindStatus(1);
            chargeCardMapper.insert(cardEntity);
            log.info("自动绑定充电卡");
        }

        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<String> addForBusinessAdmin(ChargeCardBusinessDTO cardBusinessDTO) {
        log.info("addForBusinessAdmin,cardBusinessDTO={}", JSON.toJSONString(cardBusinessDTO));
        Long operatorId = cardBusinessDTO.getOperatorId();
        if (operatorId == null || operatorId.longValue() != admin.longValue()) {
            throw new IllegalArgumentException("User is not admin,not allowed to add card.");
        }
        //查询卡号是否存在，admin新增
        int existCount = this.check(cardBusinessDTO.getCardNumber(), null, 0L);
        if (existCount > 0) {
            throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_EXIST);
        }
        ChargeCardEntity cardEntity = BeanUtils.instantiateClass(ChargeCardEntity.class);
        cardEntity.setCardPartyType(CardPartyTypeEnum.AUTEL.getValue());
        cardEntity.setCardNumber(cardBusinessDTO.getCardNumber().toUpperCase());
        cardEntity.setCardType(cardBusinessDTO.getCardType());
        log.info("addForBusinessAdmin,after set cardEntity={}", JSON.toJSONString(cardEntity));
        boolean save = this.save(cardEntity);
        log.info("addForBusinessAdmin,save={}", save);
        return Result.ofSucceed(cardEntity.getId().toString());
    }

    @Override
    public int check(String cardNumber, String name, Long sellerId) {
        return this.count(new LambdaQueryWrapper<ChargeCardEntity>()
                .eq(StringUtils.hasText(cardNumber), ChargeCardEntity::getCardNumber, cardNumber)
                .eq(sellerId != null, ChargeCardEntity::getOperatorId, sellerId)
                .eq(StringUtils.hasText(name), ChargeCardEntity::getCardAlias, name)
                .eq(ChargeCardEntity::getDeleted, 0));
    }

    @Override
    public Result<ChargeCardBusinessVO> detail(Long id) {
        ChargeCardEntity entity = this.getById(id);
        ChargeCardBusinessVO vo = new ChargeCardBusinessVO();
        if (entity != null) {
            BeanUtils.copyProperties(entity, vo);
        }
        return Result.ofSucceed(vo);
    }

    @Override
    public Result<Boolean> syncChargeTimes(ChargeCardOrderDTO dto) {
        log.info("syncChargeTimes,dto={}", JSON.toJSONString(dto));
        String cardNumber = dto.getCardNumber();
//        int check = this.check(cardNumber, null, dto.getOperatorId());
        boolean update;
        if (dto.getOperatorId() != null) {
            update = this.update(new LambdaUpdateWrapper<ChargeCardEntity>()
                    .set(ChargeCardEntity::getChargeTimes, dto.getChargeTimes())
                    .set(ChargeCardEntity::getUpdateTime, System.currentTimeMillis())
                    .eq(ChargeCardEntity::getCardNumber, cardNumber)
                    .eq(ChargeCardEntity::getOperatorId, dto.getOperatorId())
                    .eq(ChargeCardEntity::getDeleted, 0));
        } else {
            update = this.update(new LambdaUpdateWrapper<ChargeCardEntity>()
                    .set(ChargeCardEntity::getChargeTimes, dto.getChargeTimes())
                    .set(ChargeCardEntity::getUpdateTime, System.currentTimeMillis())
                    .eq(ChargeCardEntity::getCardNumber, cardNumber)
                    .isNull(ChargeCardEntity::getOperatorId)
                    .eq(ChargeCardEntity::getDeleted, 0));
        }
        log.info("syncChargeTimes,update={}", update);
        return Result.ofSucceed(update);
    }
}
