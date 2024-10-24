package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.convert.license.LicenseAboutConvert;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.subscribe.*;
import com.autel.cloud.pile.base.enums.FunctionBelongsSourceEnum;
import com.autel.cloud.pile.base.enums.NormalWhiteBrandEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.license.LicenceStatusEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.vo.AgreementFunctionReqVo;
import com.autel.cloud.pile.base.vo.ChargePointVO;
import com.autel.cloud.pile.base.vo.Connector;
import com.autel.cloud.pile.base.vo.license.LicenseDetailVO;
import com.autel.cloud.pile.user.api.enums.PileUserEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.domain.constant.PileChargingRights.SERVICE_ID_LIST_ALL;

/**
 * @description
 * @auther A23204
 * @datetime 2023/6/13 16:14
 */
@Service
@Slf4j
@RefreshScope
public class SubscribePileRightsServiceImpl implements SubscribePileRightsService {

    @Autowired
    private PileRightsMapper pileRightsMapper;

    @Autowired
    private TbLenBindRelationMapper tbLenBindRelationMapper;

    @Autowired
    private RedisUtil redisUtil;
    @Resource
    private ChargePointMerchantRelationServiceImpl chargePointMerchantRelationServiceImpl;

    @Value("${subscribe.filterFlag:false}")
    private Boolean filterFlag;

    @Autowired
    private TbBenefitConfigMapper tbBenefitConfigMapper;

    @Autowired
    private TbBenefitFuncConfigMapper tbBenefitFuncConfigMapper;

    @Autowired
    private TbSubscFuncConfigMapper tbSubscFuncConfigMapper;

    @Autowired
    private ChargePointMerchantTerminalMapper chargePointMerchantTerminalMapper;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    PileUserFeign pileUserFeign;

    @Value("${subscribe.startTimingDays:180}")
    private Integer startTimeAfterNDays;

    @Value("${subscribe.startTimeAfter90Days:90}")
    private Integer startTimeAfter90Days;

    /**
     * 1705248000000: 北京时间 2024/1/15 00:00:00
     */
    @Value("${subscribe.boundaryMillisecond:1705248000000}")
    private Long boundaryMillisecond;

    /**
     * description: getHaveRightsPileList
     * 权益查询
     * version: 1.0
     * date: 2023/8/2 10:39 
     * author: A23204 
     * 
     * @param agreementFunctionReqVo
     * @return java.util.List<com.autel.cloud.pile.base.dto.AgreementFunctionDetailDto>
     */ 
    @Override
    public List<AgreementFunctionDetailDto> getHaveRightsPileList(AgreementFunctionReqVo agreementFunctionReqVo) {

        boolean whiteBrand = false;
        if (StringUtils.isNotBlank(agreementFunctionReqVo.getTenantId())) {
            whiteBrand = isWhiteBrand(Long.valueOf(agreementFunctionReqVo.getTenantId()));
        }

        if (filterFlag || whiteBrand) {
            List<AgreementFunctionDetailDto> result = new ArrayList<>();

            if (CollectionUtils.isEmpty(agreementFunctionReqVo.getPileSn())) {
                SERVICE_ID_LIST_ALL.forEach(right -> {
                    AgreementFunctionDetailDto detailDto = new AgreementFunctionDetailDto();
                    detailDto.setTenantId(agreementFunctionReqVo.getTenantId());
                    detailDto.setStatus(1);
                    detailDto.setServiceId(right);
                    detailDto.setAvailableTime(0L);
                    detailDto.setUnavailableTime(Long.MAX_VALUE);
                    if (agreementFunctionReqVo.getStatus() == null || agreementFunctionReqVo.getStatus() == 1) {
                        result.add(detailDto);
                    }
                });
            } else {
                // 桩有所有权益
                agreementFunctionReqVo.getPileSn().forEach(pile -> SERVICE_ID_LIST_ALL.forEach(right -> {
                    AgreementFunctionDetailDto detailDto = new AgreementFunctionDetailDto();
                    detailDto.setTenantId(agreementFunctionReqVo.getTenantId());
                    detailDto.setStatus(1);
                    detailDto.setServiceId(right);
                    detailDto.setAvailableTime(0L);
                    detailDto.setUnavailableTime(Long.MAX_VALUE);
                    detailDto.setPileSn(pile);
                    if (agreementFunctionReqVo.getStatus() == null || agreementFunctionReqVo.getStatus() == 1) {
                        result.add(detailDto);
                    }
                }));
            }
            return result;
        }

        agreementFunctionReqVo.setCurrentTimeStamp(System.currentTimeMillis());
        log.info("--->>>SubscribePileRightsServiceImpl.getHaveRightsPileList, agreementFunctionReqVo:{}", JSON.toJSONString(agreementFunctionReqVo));

        List<AgreementFunctionDetailDto> result = pileRightsMapper.getHaveRightsPileList(agreementFunctionReqVo);

        log.info("--->>>SubscribePileRightsServiceImpl.getHaveRightsPileList, resultSize:{}", CollectionUtils.size(result));

        return result;
    }

    /**
     * description: pileIsAvailable
     * 查询桩是否可用，有缓存。
     * version: 1.0
     * date: 2023/8/2 10:40
     * author: A23204
     *
     * @param pileSn
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    @Override
    public Result<Boolean> pileIsAvailable(String pileSn) {

        if (filterFlag) {
            return Result.ofSucceed(Boolean.TRUE);
        }

        String operationId = "";
        try {
            String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
            operationId = stringRedisTemplate.opsForValue().get(snOperatorIdKey);
        } catch (Exception e) {
            log.error("pileIsAvailable, get sellerId error.");
        }

        if (StringUtils.isNotBlank(operationId) && isWhiteBrand(Long.valueOf(operationId))) {
            return Result.ofSucceed(Boolean.TRUE);
        }

        Boolean result = (Boolean) redisUtil.get(HAVE_CHARGE_RIGHTS_KEY + pileSn + operationId);
        log.info("--->>> pileIsAvailable from redis, result:{}", result);
        if (result != null) {
            return Result.ofSucceed(result);
        }


        //是否有主机sn，如果有主机sn，则sn增加终端sn
        List<String> snList = new ArrayList<>();

        if (StringUtils.isNotBlank(operationId)) {
            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda().eq(ChargePointMerchantTerminalEntity::getMerchantId, operationId).eq(ChargePointMerchantTerminalEntity::getHostSn, pileSn);

            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
            if (CollectionUtil.isNotEmpty(terminalEntities)) {
                snList = terminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());
            }
        }

        if (CollUtil.isEmpty(snList)) {
            // 只是普通桩
            snList.add(pileSn);
            log.info("pileSn: {} is a normal pile.", pileSn);
        } else {
            log.info("pileSn: {} is a overcharging pile, snList: {}", pileSn, JSON.toJSONString(snList));
        }

        // 当前查询接口是启动充电查询接口，查询启动充电的功能控制点对应的权益id
        List<String> functionBenefitList = getFunctionBenefitList(PileChargingRights.START_CHARGE);

        // redis cache expire
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.in(TbLenBindRelationEntity::getPileSn, snList);
        queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());
        queryWrapper.in(TbLenBindRelationEntity::getServiceId, functionBenefitList);
        if (StringUtils.isNotBlank(operationId)) {
            queryWrapper.eq(TbLenBindRelationEntity::getTenantId, operationId);
        }

        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = tbLenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isNotEmpty(tbLenBindRelationEntityList)) {
            // 可以充电, 24小时过期。每次缓存时，判断过期时间，取最小
            long min = Math.min(24*3600, (tbLenBindRelationEntityList.get(0).getUnavailableTime() - System.currentTimeMillis()) / 1000);
            redisUtil.set(HAVE_CHARGE_RIGHTS_KEY + pileSn + operationId, Boolean.TRUE, min);
            result = Boolean.TRUE;
        } else {
            // 无权限充电
            result = Boolean.FALSE;
        }
        return Result.ofSucceed(result);
    }

    /**
     * description: pileWithGunIsAvailable  根据桩+枪，判断是否有充电权限
     * version: 1.0
     * date: 2024/6/5 17:07
     * author: A23204
     *
     * @param pileSn
     * @param gunNum
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    @Override
    public Result<Boolean> pileWithGunIsAvailable(String pileSn, Integer gunNum) {
        if (filterFlag) {
            return Result.ofSucceed(Boolean.TRUE);
        }

        String operationId = "";
        try {
            String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
            operationId = stringRedisTemplate.opsForValue().get(snOperatorIdKey);
        } catch (Exception e) {
            log.error("pileIsAvailable, get sellerId error.");
        }

        if (StringUtils.isNotBlank(operationId) && isWhiteBrand(Long.valueOf(operationId))) {
            return Result.ofSucceed(Boolean.TRUE);
        }

        Boolean result = (Boolean) redisUtil.get(HAVE_CHARGE_RIGHTS_KEY + "_" + pileSn + operationId);
        log.info("--->>> pileIsAvailable from redis, result:{}", result);
        if (result != null) {
            return Result.ofSucceed(result);
        }

        // 在查询 主机+枪号 的缓存
        result = (Boolean) redisUtil.get(HAVE_CHARGE_RIGHTS_KEY + "_" + pileSn + "_" + gunNum + operationId);
        if (result != null) {
            return Result.ofSucceed(result);
        }

        //是否有主机sn，如果有主机sn，则sn增加终端sn
        String finalSn = null;
        if (StringUtils.isNotBlank(operationId)) {
            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda().eq(ChargePointMerchantTerminalEntity::getMerchantId, operationId).eq(ChargePointMerchantTerminalEntity::getHostSn, pileSn);

            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
            if (CollectionUtil.isNotEmpty(terminalEntities)) {
                //snList = terminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());
                boolean stop = false;
                for (ChargePointMerchantTerminalEntity entity : terminalEntities) {
                    // 查询 枪号是 gunNum的终端 sn
                    List<Connector> connectorsList = entity.getConnectorsList();
                    for (Connector connector : connectorsList) {
                        if (connector.getConnectorId().equals(gunNum)) {
                            finalSn = entity.getTerminalSn();
                            stop = true;
                            break;
                        }
                    }
                    if (stop) {
                        break;
                    }
                }
            }
        }

        if (finalSn == null) {
            // 只是普通桩
            finalSn = pileSn;
            log.info("pileWithGunIsAvailable, pileSn: {} is a normal pile.", pileSn);
        } else {
            log.info("pileWithGunIsAvailable, pileSn: {} is a overcharging pile, terminalSn: {}", pileSn, finalSn);
        }

        // 当前查询接口是启动充电查询接口，查询启动充电的功能控制点对应的权益id
        List<String> functionBenefitList = getFunctionBenefitList(PileChargingRights.START_CHARGE);

        // redis cache expire
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbLenBindRelationEntity::getPileSn, finalSn);
        queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());
        queryWrapper.in(TbLenBindRelationEntity::getServiceId, functionBenefitList);
        if (StringUtils.isNotBlank(operationId)) {
            queryWrapper.eq(TbLenBindRelationEntity::getTenantId, operationId);
        }

        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = tbLenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isNotEmpty(tbLenBindRelationEntityList)) {
            // 可以充电, 24小时过期。每次缓存时，判断过期时间，取最小
            long min = Math.min(24*3600, (tbLenBindRelationEntityList.get(0).getUnavailableTime() - System.currentTimeMillis()) / 1000);
            if (pileSn.equals(finalSn)) {
                // 普通桩
                redisUtil.set(HAVE_CHARGE_RIGHTS_KEY + "_" + pileSn + operationId, Boolean.TRUE, min);
            } else {
                // 超充终端
                redisUtil.set(HAVE_CHARGE_RIGHTS_KEY + "_" + pileSn + "_" + gunNum + operationId, Boolean.TRUE, min);
            }
            result = Boolean.TRUE;
        } else {
            // 无权限充电
            result = Boolean.FALSE;
        }
        return Result.ofSucceed(result);
    }

    /**
     * description: getFunctionBenefitList 查询启动充电的功能控制点对应的权益id 集合
     * version: 1.0
     * date: 2023/12/8 16:46
     * author: A23204
     *
     * @param functionId
     * @return java.util.List<java.lang.String>
     */
    @Override
    public List<String> getFunctionBenefitList(String functionId) {
        // get from redis
        List<String> benefitList = (List<String>) redisUtil.get(RedisKeyConstant.FUNCTION_ID_BENEFIT_LIST_PREFIX + functionId);
        if (benefitList == null) {
            LambdaQueryWrapper<TbBenefitFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
            queryWrapper.eq(TbBenefitFuncConfigEntity::getFunctionId, functionId);
            List<TbBenefitFuncConfigEntity> benefitFuncConfigEntityList  = tbBenefitFuncConfigMapper.selectList(queryWrapper);
            benefitList = benefitFuncConfigEntityList.stream().map(TbBenefitFuncConfigEntity::getServiceId).collect(Collectors.toList());
            redisUtil.set(RedisKeyConstant.FUNCTION_ID_BENEFIT_LIST_PREFIX + functionId, benefitList);
        }
        return benefitList;
    }

    /**
     * description: queryUnUsedLicence
     * 查询未使用licence
     * version: 1.0
     * date: 2023/8/2 10:40
     * author: A23204
     *
     * @param
     * @return com.autel.cloud.base.http.pojo.Result<java.util.List<com.autel.cloud.pile.base.dto.UnUsedLicenceInfoDto>>
     */
    @Override
    public Result<List<UnUsedLicenceInfoDto>> queryUnUsedLicence(String skuCode, Long merchantId) {

        log.info("===>>>SubscribePileRightsServiceImpl.queryUnUsedLicence skuCode : {}",
                JSON.toJSONString(skuCode));

        Long tenantId = Optional.ofNullable(merchantId).orElseGet(() -> LoginUserHolder.getLoginUser().getPayload().getSellerId());
        //Long tenantId = LoginUserHolder.getLoginUser().getPayload().getSellerId();

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, tenantId);
        queryWrapper.eq(TbLenBindRelationEntity::getStatus, 0);
        queryWrapper.eq(StringUtils.isNotBlank(skuCode), TbLenBindRelationEntity::getSkuCode, skuCode);
        queryWrapper.isNotNull(TbLenBindRelationEntity::getSkuCode);

        List<UnUsedLicenceInfoDto> unUsedLicenceInfoDtoList = new ArrayList<>();
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isNotEmpty(tbLenBindRelationEntities)) {
            tbLenBindRelationEntities.forEach(var -> unUsedLicenceInfoDtoList.add(LicenseAboutConvert.buildUnUsedLicenceInfoDto(var)));
        }
        return Result.ofSucceed(unUsedLicenceInfoDtoList);
    }

    /**
     * description: pileRightsWithExtension
     * 返回桩权益过期时间，考虑续期的场景。
     * version: 1.0
     * date: 2023/8/2 10:41
     * author: A23204
     *
     * @param pileSn
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Long>
     */
    @Override
    public Result<Long> pileRightsWithExtension(String pileSn) {

        if (filterFlag) {
            return Result.ofSucceed(Long.MAX_VALUE);
        }

        String operationId = "";
        try {
            String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
            operationId = stringRedisTemplate.opsForValue().get(snOperatorIdKey);
        } catch (Exception e) {
            log.error("pileIsAvailable, get sellerId error.");
        }

        if (StringUtils.isNotBlank(operationId) && isWhiteBrand(Long.valueOf(operationId))) {
            return Result.ofSucceed(Long.MAX_VALUE);
        }

        // 当前查询接口是启动充电查询接口，查询启动充电的功能控制点对应的权益id
        List<String> functionBenefitList = getFunctionBenefitList(PileChargingRights.START_CHARGE);

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbLenBindRelationEntity::getPileSn, pileSn);
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, operationId);
        queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());
        queryWrapper.in(TbLenBindRelationEntity::getServiceId, functionBenefitList);
        queryWrapper.orderByDesc(TbLenBindRelationEntity::getUnavailableTime);
        queryWrapper.last("limit 1");

        TbLenBindRelationEntity tbLenBindRelationEntity = tbLenBindRelationMapper.selectOne(queryWrapper);
        if (tbLenBindRelationEntity != null) {
            return Result.ofSucceed(tbLenBindRelationEntity.getUnavailableTime());
        }

        return Result.ofSucceed(0L);
    }

    /**
     * description: queryLicence  查询某个商家下所有的licnece
     * version: 1.0
     * date: 2023/8/24 19:44 
     * author: A23204 
     * 
     * @param sellerId
     * @return com.autel.cloud.base.http.pojo.Result<java.util.List<com.autel.cloud.pile.base.dto.UnUsedLicenceInfoDto>>
     */ 
    @Override
    public Result<List<UnUsedLicenceInfoDto>> queryLicence(String sellerId) {
        log.info("--->>> queryLicence, sellerId:{}", sellerId);
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, sellerId);

        List<UnUsedLicenceInfoDto> unUsedLicenceInfoDtoList = new ArrayList<>();
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isNotEmpty(tbLenBindRelationEntities)) {
            tbLenBindRelationEntities.forEach(var -> unUsedLicenceInfoDtoList.add(LicenseAboutConvert.buildUnUsedLicenceInfoDto(var)));
        }
        return Result.ofSucceed(unUsedLicenceInfoDtoList);
    }

    @Override
    public BusinessRespDto findSellerIdByLicenseNum(BusinessReqDto businessReqDto) {
        if (ObjectUtils.isEmpty(businessReqDto)) {
            return null;
        }
        BusinessRespDto result = new BusinessRespDto();
        if (!ObjectUtils.isEmpty(businessReqDto.getPurchasedLicenseNumParam())) {
            Set<String> purchasedLicenseNumSet = searchPurchasedLicenseNum(businessReqDto.getPurchasedLicenseNumParam());
            //过滤道通端
            purchasedLicenseNumSet.remove("0");
            result.setPurchasedLicenseNumSellers(purchasedLicenseNumSet);
        }
        if (!ObjectUtils.isEmpty(businessReqDto.getNotUseLicenseNumParam())) {
            Set<String> notUseLicenseNumSet = searchNotUseLicenseNum(businessReqDto.getNotUseLicenseNumParam());
            //过滤道通端
            notUseLicenseNumSet.remove("0");
            result.setNotUseLicenseNumSellers(notUseLicenseNumSet);
        }
        if (!ObjectUtils.isEmpty(businessReqDto.getAddedPileNumParam())) {
            Set<String> addedPileNumSet = chargePointMerchantRelationServiceImpl.findSellerByCount(businessReqDto.getAddedPileNumParam());
            result.setAddedPileNumSellers(addedPileNumSet);
        }
        return result;
    }

    @Override
    public Result<List<SubscribeBenefitMetaDto>> benefitConfigQuery() {
        List<SubscribeBenefitMetaDto> result = new ArrayList<>();
        LambdaQueryWrapper<TbBenefitConfigEntity> queryWrapper = Wrappers.lambdaQuery();
        List<TbBenefitConfigEntity> tbBenefitConfigEntities = tbBenefitConfigMapper.selectList(queryWrapper);
        if (CollectionUtil.isNotEmpty(tbBenefitConfigEntities)) {
            tbBenefitConfigEntities.forEach(tbBenefitConfigEntity -> {
                SubscribeBenefitMetaDto benefitMetaDto = new SubscribeBenefitMetaDto();
                BeanUtils.copyProperties(tbBenefitConfigEntity, benefitMetaDto);
                result.add(benefitMetaDto);
            });
        }

        return Result.ofSucceed(result);
    }

    @Override
    public Result<List<SubscribeFuncConfDto>> functionConfigQuery() {
        List<SubscribeFuncConfDto> result = new ArrayList<>();
        LambdaQueryWrapper<TbSubscFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
        List<TbSubscFuncConfigEntity> subscFuncConfigEntityList = tbSubscFuncConfigMapper.selectList(queryWrapper);
        if (CollectionUtil.isNotEmpty(subscFuncConfigEntityList)) {
            subscFuncConfigEntityList.forEach(tbSubscFuncConfigEntity -> {
                SubscribeFuncConfDto subscribeFuncConfDto = new SubscribeFuncConfDto();
                BeanUtils.copyProperties(tbSubscFuncConfigEntity, subscribeFuncConfDto);
                result.add(subscribeFuncConfDto);
            });
        }

        return Result.ofSucceed(result);
    }

    @Override
    public Result<List<PileBenefitFunctionRespDto>> getPileFunction(BenefitFunctionReqDto benefitFunctionReqDto) {
        // 商家、桩，不能同时为空。
        if (StringUtils.isBlank(benefitFunctionReqDto.getTenantId()) && CollectionUtil.isEmpty(benefitFunctionReqDto.getPileSn())) {
            log.info(">>> getPileFunction, both of tenantId and pileSn are empty.");
            throw new IllegalArgumentException(" both of tenantId and pileSn are empty");
        }

        boolean whiteBrand = false;
        try{
            if(LoginUserHolder.getLoginUser() != null && LoginUserHolder.getLoginUser().getPayload() != null && LoginUserHolder.getLoginUser().getPayload().getSellerId() != null){
                whiteBrand = isWhiteBrand(Long.valueOf(LoginUserHolder.getLoginUser().getPayload().getSellerId()));
            }
        }catch (Exception e){
            log.error("whiteBrand 获取用户失败",e);
        }

        /*if (StringUtils.isNotBlank(benefitFunctionReqDto.getTenantId())) {
            whiteBrand = isWhiteBrand(Long.valueOf(benefitFunctionReqDto.getTenantId()));
        }*/

        // 开关控制
        if (filterFlag || whiteBrand) {

            List<PileBenefitFunctionRespDto> result = new ArrayList<>();
            // 查询控制点
            LambdaQueryWrapper<TbSubscFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
            List<TbSubscFuncConfigEntity> subscFuncConfigEntityList = tbSubscFuncConfigMapper.selectList(queryWrapper);
            List<String> funcIdList = subscFuncConfigEntityList.stream().map(TbSubscFuncConfigEntity::getFunctionId).collect(Collectors.toList());

            if (CollectionUtils.isEmpty(benefitFunctionReqDto.getPileSn())) {
                if (benefitFunctionReqDto.getStatus() == null || benefitFunctionReqDto.getStatus() == 1) {
                    PileBenefitFunctionRespDto detailDto = new PileBenefitFunctionRespDto();
                    detailDto.setTenantId(benefitFunctionReqDto.getTenantId());
                    detailDto.setAvailableTime(0L);
                    detailDto.setUnavailableTime(Long.MAX_VALUE);
                    detailDto.setFunctionIdList(funcIdList);
                    detailDto.setStatus(1);
                    result.add(detailDto);
                }
            } else {
                // 桩有所有权益
                if (benefitFunctionReqDto.getStatus() == null || benefitFunctionReqDto.getStatus() == 1) {
                    benefitFunctionReqDto.getPileSn().forEach(pile -> {
                        PileBenefitFunctionRespDto detailDto = new PileBenefitFunctionRespDto();
                        detailDto.setTenantId(benefitFunctionReqDto.getTenantId());
                        detailDto.setStatus(1);
                        detailDto.setFunctionIdList(funcIdList);
                        detailDto.setAvailableTime(0L);
                        detailDto.setUnavailableTime(Long.MAX_VALUE);
                        detailDto.setPileSn(pile);
                        result.add(detailDto);
                    });
                }
            }
            return Result.ofSucceed(result);
        }

        benefitFunctionReqDto.setCurrentTimeStamp(System.currentTimeMillis());
        log.info("--->>>SubscribePileRightsServiceImpl.getPileFunction, benefitFunctionReqDto:{}", JSON.toJSONString(benefitFunctionReqDto));

        List<PileBenefitFunctionRespDto> result = new ArrayList<>();

        AgreementFunctionReqVo agreementFunctionReqVo = new AgreementFunctionReqVo();
        BeanUtils.copyProperties(benefitFunctionReqDto, agreementFunctionReqVo);

        try{
            if(LoginUserHolder.getLoginUser() != null && LoginUserHolder.getLoginUser().getPayload() != null && LoginUserHolder.getLoginUser().getPayload().getSellerId() != null){
                agreementFunctionReqVo.setTenantId(String.valueOf(LoginUserHolder.getLoginUser().getPayload().getSellerId()));
            }
        }catch (Exception e){
            log.error("获取用户失败",e);
        }

        //是否有主机sn，如果有主机sn，则sn增加终端sn
        List<String> snList;

        if (StringUtils.isNotBlank(agreementFunctionReqVo.getTenantId()) && CollUtil.isNotEmpty(agreementFunctionReqVo.getPileSn())) {
            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda()
                    .eq(ChargePointMerchantTerminalEntity::getMerchantId, agreementFunctionReqVo.getTenantId()).in(ChargePointMerchantTerminalEntity::getHostSn, agreementFunctionReqVo.getPileSn());

            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
            if (CollectionUtil.isNotEmpty(terminalEntities)) {
                snList = terminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());
                agreementFunctionReqVo.getPileSn().addAll(snList);
            }
        }

        List<AgreementFunctionDetailDto> origin = pileRightsMapper.getHaveRightsPileList(agreementFunctionReqVo);

        if (CollectionUtils.isNotEmpty(origin)) {
            origin.forEach(item -> {
                // 查询缓存的 service id -> function list
                PileBenefitFunctionRespDto dto = new PileBenefitFunctionRespDto();
                BeanUtils.copyProperties(item, dto);
                List<String> functionListByBenefitId = getFunctionListByBenefitId(item.getServiceId());
                dto.setFunctionIdList(functionListByBenefitId);
                result.add(dto);
            });
        }

        log.info("--->>>SubscribePileRightsServiceImpl.getHaveRightsPileList, resultSize:{}", CollectionUtils.size(result));

        return Result.ofSucceed(result);
    }

    /**
     * description: functionAvailableList 查询桩生效中的功能控制点
     * version: 1.0
     * date: 2024/1/23 9:47
     * author: A23204
     *
     * @param benefitAvailableFuncListReqDto
     * @return com.autel.cloud.base.http.pojo.Result<BenefitAvailableFuncListRespDto>
     */
    @Override
    public Result<List<BenefitAvailableFuncListRespDto>> functionAvailableList(BenefitAvailableFuncListReqDto benefitAvailableFuncListReqDto) {
        // 商家、桩，不能同时为空。
        if (CollectionUtil.isEmpty(benefitAvailableFuncListReqDto.getPileSn())) {
            log.info(">>> getPileFunction, pileSn is empty.");
            throw new MessageCodeException(PileUserEnum.REQUEST_PARAMETER_ERROR);
        }

        boolean whiteBrand = isWhiteBrand(null);

        // 开关控制
        if (filterFlag || whiteBrand) {
            List<BenefitAvailableFuncListRespDto> result = new ArrayList<>();
            // 查询控制点
            LambdaQueryWrapper<TbSubscFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
            List<TbSubscFuncConfigEntity> subscFuncConfigEntityList = tbSubscFuncConfigMapper.selectList(queryWrapper);
            List<String> funcIdList = subscFuncConfigEntityList.stream().map(TbSubscFuncConfigEntity::getFunctionId).collect(Collectors.toList());

            benefitAvailableFuncListReqDto.getPileSn().forEach(pile -> {
                BenefitAvailableFuncListRespDto detailDto = new BenefitAvailableFuncListRespDto();
                detailDto.setPileSn(pile);
                detailDto.setFunctionIdList(funcIdList);
                result.add(detailDto);
            });
            return Result.ofSucceed(result);
        }

        // 走订阅业务逻辑
        benefitAvailableFuncListReqDto.setCurrentTimeStamp(System.currentTimeMillis());
        benefitAvailableFuncListReqDto.setStatus(1);
        log.info("--->>>SubscribePileRightsServiceImpl.functionAvailableList, benefitAvailableFuncListReqDto:{}", JSON.toJSONString(benefitAvailableFuncListReqDto));

        List<BenefitAvailableFuncListRespDto> result = new ArrayList<>();

        AgreementFunctionReqVo agreementFunctionReqVo = new AgreementFunctionReqVo();
        BeanUtils.copyProperties(benefitAvailableFuncListReqDto, agreementFunctionReqVo);
        agreementFunctionReqVo.setTenantId(String.valueOf(LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        List<AgreementFunctionDetailDto> origin = pileRightsMapper.getHaveRightsPileList(agreementFunctionReqVo);

        if (CollectionUtils.isNotEmpty(origin)) {
            origin.forEach(item -> {
                // 查询缓存的 service id -> function list
                BenefitAvailableFuncListRespDto dto = new BenefitAvailableFuncListRespDto();
                BeanUtils.copyProperties(item, dto);
                List<String> functionListByBenefitId = getFunctionListByBenefitId(item.getServiceId());
                dto.setFunctionIdList(functionListByBenefitId);
                result.add(dto);
            });
        }

        // result 按照桩合并
        if (CollectionUtil.isNotEmpty(result)) {
            List<BenefitAvailableFuncListRespDto> finalResult = new ArrayList<>();
            Map<String, List<BenefitAvailableFuncListRespDto>> groupMap = result.stream().collect(Collectors.groupingBy(BenefitAvailableFuncListRespDto::getPileSn));
            groupMap.forEach((key, value) -> {
                List<String> collect = value.stream().flatMap(func -> func.getFunctionIdList().stream()).collect(Collectors.toList());
                List<String> funcList = collect.stream().distinct().collect(Collectors.toList());

                BenefitAvailableFuncListRespDto dto = new BenefitAvailableFuncListRespDto();
                dto.setPileSn(key);
                dto.setFunctionIdList(funcList);
                finalResult.add(dto);
            });
            log.info("--->>>SubscribePileRightsServiceImpl.functionAvailableList, resultSize:{}", CollectionUtils.size(finalResult));
            return Result.ofSucceed(finalResult);
        }

        return Result.ofSucceed(new ArrayList<>());
    }


    @Override
    public List<String> getFunctionListByBenefitId(String benefitId) {
        try {
            List<String> functionList = (List<String>) redisUtil.get(RedisKeyConstant.BENEFIT_ID_FUNCTION_LIST_PREFIX + benefitId);
            if (CollectionUtils.isEmpty(functionList)) {
                // 查询数据库
                LambdaQueryWrapper<TbBenefitFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
                queryWrapper.eq(TbBenefitFuncConfigEntity::getServiceId, benefitId);

                List<TbBenefitFuncConfigEntity> benefitFuncConfigEntityList = tbBenefitFuncConfigMapper.selectList(queryWrapper);
                if (CollectionUtils.isNotEmpty(benefitFuncConfigEntityList)) {
                    functionList = benefitFuncConfigEntityList.stream().map(TbBenefitFuncConfigEntity::getFunctionId).collect(Collectors.toList());
                    redisUtil.set(RedisKeyConstant.BENEFIT_ID_FUNCTION_LIST_PREFIX + benefitId, functionList);
                }
            }
            return functionList;
        } catch (Exception e) {
            log.error("getFunctionListByBenefitId error, benefitId: {}", benefitId, e);
        }
        return Collections.emptyList();
    }

    @Override
    public void reSetFunctionIdBenefitList() {
        // 重新设置缓存
        LambdaQueryWrapper<TbBenefitFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
        List<TbBenefitFuncConfigEntity> benefitFuncConfigEntityList = tbBenefitFuncConfigMapper.selectList(queryWrapper);
        if (CollectionUtil.isNotEmpty(benefitFuncConfigEntityList)) {
            Map<String, List<TbBenefitFuncConfigEntity>> countryGroupMap = benefitFuncConfigEntityList.stream().collect(Collectors.groupingBy(TbBenefitFuncConfigEntity::getFunctionId));
            countryGroupMap.forEach((k, v) -> {
                List<String> benefitList = v.stream().map(TbBenefitFuncConfigEntity::getServiceId).collect(Collectors.toList());
                redisUtil.set(RedisKeyConstant.FUNCTION_ID_BENEFIT_LIST_PREFIX + k, benefitList);
            });
        }
    }

    @Override
    public Result<Boolean> functionEdit(FunctionModifyDto functionModifyDto) {
        // 根据功能id，更新 描述，业务类型
        LambdaUpdateWrapper<TbSubscFuncConfigEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.eq(TbSubscFuncConfigEntity::getFunctionId, functionModifyDto.getFunctionId());
        TbSubscFuncConfigEntity updateEntity = new TbSubscFuncConfigEntity();
        updateEntity.setFunctionDesc(functionModifyDto.getFunctionDesc());
        // 定义业务范围枚举类
        updateEntity.setSourceName(FunctionBelongsSourceEnum.getValueByKey(functionModifyDto.getSourceId()));
        updateEntity.setSourceId(functionModifyDto.getSourceId());
        updateEntity.setUpdateTime(System.currentTimeMillis());
        tbSubscFuncConfigMapper.update(updateEntity, updateWrapper);

        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    @Transactional
    public Result<Boolean> benefitEdit(BenefitEditDto benefitEditDto) {

        //
        LambdaQueryWrapper<TbBenefitConfigEntity> queryWrapperFirst = Wrappers.lambdaQuery();
        queryWrapperFirst.eq(TbBenefitConfigEntity::getServiceName, benefitEditDto.getServiceName());
        queryWrapperFirst.ne(TbBenefitConfigEntity::getServiceId, benefitEditDto.getServiceId());
        List<TbBenefitConfigEntity> tbBenefitConfigEntities = tbBenefitConfigMapper.selectList(queryWrapperFirst);
        if (CollUtil.isNotEmpty(tbBenefitConfigEntities)) {
            // 权益id已经存在
            throw new MessageCodeException(PileBaseEnum.SERVICE_NAME_EXISTS);
        }


        // update serviceName
        TbBenefitConfigEntity updateEntity = new TbBenefitConfigEntity();
        updateEntity.setServiceName(benefitEditDto.getServiceName());

        LambdaUpdateWrapper<TbBenefitConfigEntity> updateWrapperBenefit = Wrappers.lambdaUpdate();
        updateWrapperBenefit.eq(TbBenefitConfigEntity::getServiceId, benefitEditDto.getServiceId());

        tbBenefitConfigMapper.update(updateEntity, updateWrapperBenefit);



        // update tb_benefit_func_config
        // 先更新数据库，再删除缓存
        // 查询缓存时，缓存不存在，查询数据库
        LambdaUpdateWrapper<TbBenefitFuncConfigEntity> updateWrapperOld = Wrappers.lambdaUpdate();
        updateWrapperOld.eq(TbBenefitFuncConfigEntity::getServiceId, benefitEditDto.getServiceId());

        // 先删除旧的
        tbBenefitFuncConfigMapper.delete(updateWrapperOld);

        // 查询新的
        LambdaQueryWrapper<TbSubscFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.in(TbSubscFuncConfigEntity::getFunctionId, benefitEditDto.getFunctionIdList());
        List<TbSubscFuncConfigEntity> subscFuncConfigEntityList = tbSubscFuncConfigMapper.selectList(queryWrapper);


        //  添加新的
        subscFuncConfigEntityList.forEach(tbSubscFuncConfigEntity -> {
            // 构建新的 TbBenefitFuncConfigEntity
            TbBenefitFuncConfigEntity newEntity = new TbBenefitFuncConfigEntity();
            newEntity.setServiceId(benefitEditDto.getServiceId());
            newEntity.setServiceName(benefitEditDto.getServiceName());
            newEntity.setFunctionId(tbSubscFuncConfigEntity.getFunctionId());
            newEntity.setFunctionDesc(tbSubscFuncConfigEntity.getFunctionDesc());
            newEntity.setSourceId(tbSubscFuncConfigEntity.getSourceId());
            newEntity.setSourceName(tbSubscFuncConfigEntity.getSourceName());
            newEntity.setCreateTime(System.currentTimeMillis());
            newEntity.setUpdateTime(newEntity.getCreateTime());
            newEntity.setCreateBy(String.valueOf(LoginUserHolder.getLoginUser().getId()));
            newEntity.setUpdateBy(String.valueOf(LoginUserHolder.getLoginUser().getId()));
            tbBenefitFuncConfigMapper.insert(newEntity);
        });

        return Result.ofSucceed(Boolean.TRUE);
    }

    @Override
    public Result<List<BenefitFunctionRelationDto>> benefitFuncList() {
        List<BenefitFunctionRelationDto> result = new ArrayList<>();
        LambdaQueryWrapper<TbBenefitFuncConfigEntity> queryWrapper = Wrappers.lambdaQuery();
        List<TbBenefitFuncConfigEntity> benefitFuncConfigEntityList = tbBenefitFuncConfigMapper.selectList(queryWrapper);
        if (CollectionUtil.isNotEmpty(benefitFuncConfigEntityList)) {
            Map<String, List<TbBenefitFuncConfigEntity>> countryGroupMap = benefitFuncConfigEntityList.stream().collect(Collectors.groupingBy(TbBenefitFuncConfigEntity::getServiceId));
            result = countryGroupMap.entrySet().stream().map(entry -> {
                BenefitFunctionRelationDto dto = new BenefitFunctionRelationDto();
                dto.setServiceId(entry.getKey());
                dto.setServiceName(entry.getValue().get(0).getServiceName());
                List<SubscribeFuncConfDto> funcDtoList = new ArrayList<>();
                List<TbBenefitFuncConfigEntity> value = entry.getValue();
                value.forEach(tbBenefitFuncConfigEntity -> {
                    SubscribeFuncConfDto subscribeFuncConfDto = new SubscribeFuncConfDto();
                    BeanUtils.copyProperties(tbBenefitFuncConfigEntity, subscribeFuncConfDto);
                    funcDtoList.add(subscribeFuncConfDto);
                });
                dto.setFuncDtoList(funcDtoList);
                return dto;
            }).collect(Collectors.toList());
        }

        return Result.ofSucceed(result);
    }

    private Set<String> searchPurchasedLicenseNum(RangeConditionDto rangeConditionDto) {
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(TbLenBindRelationEntity::getTenantId,TbLenBindRelationEntity::getId);
        queryWrapper.groupBy(TbLenBindRelationEntity::getTenantId);
        if (rangeConditionDto.getRange()) {
            if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) >="+rangeConditionDto.getLowerLimit()+")");
            }else if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && !ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) >="+rangeConditionDto.getLowerLimit()+" and count(id) <="+rangeConditionDto.getUpperLimit()+")");
            }
        }else {
            if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) <"+rangeConditionDto.getLowerLimit()+")");
            }else if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && !ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) <"+rangeConditionDto.getLowerLimit()+" or count(id) >"+rangeConditionDto.getUpperLimit()+")");
            }
        }
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
            return new HashSet<>();
        }
        return tbLenBindRelationEntities.stream().filter(k->!ObjectUtils.isEmpty(k) && !ObjectUtils.isEmpty(k.getTenantId())).map(m->String.valueOf(m.getTenantId())).collect(Collectors.toSet());
    }

    private Set<String> searchNotUseLicenseNum(RangeConditionDto rangeConditionDto) {
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(TbLenBindRelationEntity::getTenantId,TbLenBindRelationEntity::getId);
        queryWrapper.eq(TbLenBindRelationEntity::getStatus,0);
        queryWrapper.groupBy(TbLenBindRelationEntity::getTenantId);
        if (rangeConditionDto.getRange()) {
            if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) >="+rangeConditionDto.getLowerLimit()+")");
            }else if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && !ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) >="+rangeConditionDto.getLowerLimit()+" and count(id) <="+rangeConditionDto.getUpperLimit()+")");
            }
        }else {
            if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) <"+rangeConditionDto.getLowerLimit()+")");
            }else if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && !ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())){
                queryWrapper.last("HAVING (COUNT(id) <"+rangeConditionDto.getLowerLimit()+" or count(id) >"+rangeConditionDto.getUpperLimit()+")");
            }
        }
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
            return new HashSet<>();
        }
        return tbLenBindRelationEntities.stream().filter(k->!ObjectUtils.isEmpty(k) && !ObjectUtils.isEmpty(k.getTenantId())).map(m->String.valueOf(m.getTenantId())).collect(Collectors.toSet());
    }

    /**
     * description: extendGracePeriod 统一延长licence 宽限期
     * version: 1.0
     * date: 2023/12/19 10:24
     * author: A23204
     *
     * @param licenceGracePeriodExtendedDto
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    @Override
    @Transactional
    public Result<List<String>> extendGracePeriod(LicenceGracePeriodExtendedDto licenceGracePeriodExtendedDto) {
        // 计算天数
        List<TbLenBindRelationEntity> updateEntityList = extendDaysCalculate(licenceGracePeriodExtendedDto);

        // 判断是否有不满足条件的数据
        if (updateEntityList.size() < licenceGracePeriodExtendedDto.getLicenceCode().size()) {

            List<String> collect = updateEntityList.stream().map(TbLenBindRelationEntity::getLicenceCode).collect(Collectors.toList());

            // 直接返回不满足条件的列表
            List<String> differenceSet = licenceGracePeriodExtendedDto.getLicenceCode().stream().filter(e -> !collect.contains(e)).collect(Collectors.toList());

            return Result.ofSucceed(differenceSet);
        }

        updateEntityList.forEach(item -> tbLenBindRelationMapper.updateById(item));

        return Result.ofSucceed(new ArrayList<>());
    }

    /**
     * description: extendDaysCalculate 计算需要增加的额外宽限期天数
     * version: 1.0
     * date: 2023/12/19 16:59
     * author: A23204
     *
     * @return java.util.List<com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity>
     */
    private List<TbLenBindRelationEntity> extendDaysCalculate(LicenceGracePeriodExtendedDto licenceGracePeriodExtendedDto) {
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.in(TbLenBindRelationEntity::getLicenceCode, licenceGracePeriodExtendedDto.getLicenceCode());

        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);

        List<TbLenBindRelationEntity> updateEntityList = new ArrayList<>();

        tbLenBindRelationEntities.forEach(tbLenBindRelationEntity -> {
            //基本宽限期，取旧值，还是新值判断
            Long createTime = tbLenBindRelationEntity.getCreateTime();
            int extendedBefore = Optional.ofNullable(tbLenBindRelationEntity.getExtendedGracePeriod()).orElse(0);
            Long currentTime = System.currentTimeMillis();
            if (createTime < boundaryMillisecond) {
                // 按照 180天
                int createDays = (int) ((currentTime - createTime) / (3600*24*1000));
                int addedDays = licenceGracePeriodExtendedDto.getExtendedDays() - (startTimeAfterNDays - createDays);
                if (addedDays > 0 && addedDays > extendedBefore) {
                    // 只延长，不减少
                    TbLenBindRelationEntity updateEntity = new TbLenBindRelationEntity();
                    updateEntity.setId(tbLenBindRelationEntity.getId());
                    updateEntity.setUpdateTime(currentTime);
                    updateEntity.setUpdateBy(String.valueOf(licenceGracePeriodExtendedDto.getOperatorId()));
                    updateEntity.setExtendedGracePeriod(addedDays);
                    updateEntityList.add(updateEntity);
                }
            } else {
                // 待优化，需要提取公共方法，与上面代码重复
                // 按照 90天
                int createDays = (int) ((currentTime - createTime) / (3600*24*1000));
                int addedDays = licenceGracePeriodExtendedDto.getExtendedDays() - (startTimeAfter90Days - createDays);
                if (addedDays > 0 && addedDays > extendedBefore) {
                    // 只延长，不减少
                    TbLenBindRelationEntity updateEntity = new TbLenBindRelationEntity();
                    updateEntity.setId(tbLenBindRelationEntity.getId());
                    updateEntity.setUpdateTime(currentTime);
                    updateEntity.setUpdateBy(String.valueOf(licenceGracePeriodExtendedDto.getOperatorId()));
                    updateEntity.setExtendedGracePeriod(addedDays);
                    updateEntityList.add(updateEntity);
                }
            }
        });

        return updateEntityList;
    }

    @Override
    public void updateServiceIdBySellerId(String sellerId) {

        if (StringUtils.isBlank(sellerId)) {
            return;
        }

        // 更新当前商家的 serviceId
        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper1 = Wrappers.lambdaUpdate();
        updateWrapper1.eq(TbLenBindRelationEntity::getTenantId, sellerId);
        updateWrapper1.likeRight(TbLenBindRelationEntity::getGoodsId, "cdyp");

        TbLenBindRelationEntity updateEntity1 = new TbLenBindRelationEntity();
        updateEntity1.setServiceId("Benefits_Pro");

        tbLenBindRelationMapper.update(updateEntity1, updateWrapper1);

        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper2 = Wrappers.lambdaUpdate();
        updateWrapper2.eq(TbLenBindRelationEntity::getTenantId, sellerId);
        updateWrapper2.likeRight(TbLenBindRelationEntity::getGoodsId, "cdyl");

        TbLenBindRelationEntity updateEntity2 = new TbLenBindRelationEntity();
        updateEntity2.setServiceId("Benefits_Lite");

        tbLenBindRelationMapper.update(updateEntity2, updateWrapper2);

        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper3 = Wrappers.lambdaUpdate();
        updateWrapper3.eq(TbLenBindRelationEntity::getTenantId, sellerId);
        updateWrapper3.likeRight(TbLenBindRelationEntity::getGoodsId, "mp");

        TbLenBindRelationEntity updateEntity3 = new TbLenBindRelationEntity();
        updateEntity3.setServiceId("Benefits_ops");

        tbLenBindRelationMapper.update(updateEntity3, updateWrapper3);


        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper4 = Wrappers.lambdaUpdate();
        updateWrapper4.eq(TbLenBindRelationEntity::getTenantId, sellerId);
        updateWrapper4.likeRight(TbLenBindRelationEntity::getGoodsId, "cms");

        TbLenBindRelationEntity updateEntity4 = new TbLenBindRelationEntity();
        updateEntity4.setServiceId("Benefits_ads");

        tbLenBindRelationMapper.update(updateEntity4, updateWrapper4);

    }

    /**
     * description: isWhiteBrand  如果 sellerId == null， 登录态调用
     * version: 1.0
     * date: 2024/6/25 11:11 
     * author: A23204 
     * 
     * @param 
     * @return boolean
     */ 
    private boolean isWhiteBrand(Long sellerId) {
        if (sellerId == null) {
            sellerId = LoginUserUtil.getSellerId();
        }
        Result<SellerDetailVO> detail = pileUserFeign.detail(sellerId);
        if (detail.getData() != null) {
            return NormalWhiteBrandEnum.whiteBrand(detail.getData().getSellerSubject());
        }
        return false;
    }
}
