package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.crm.dto.OrderTypeNoticeDTO;
import com.autel.cloud.ordercenter.vo.ReturnOrderReqVO;
import com.autel.cloud.ordercenter.vo.ReturnOrderRespVO;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.convert.license.LicenseAboutConvert;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantTerminalService;
import com.autel.cloud.pile.base.domain.service.EmailSendingService;
import com.autel.cloud.pile.base.domain.service.LicenseManagementService;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.dto.GetLicenseDetailPageByOrderIdDTO;
import com.autel.cloud.pile.base.dto.license.DoNotRemindAgainDTO;
import com.autel.cloud.pile.base.dto.license.LicenseDetailDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.license.LicenceReturnStatusEnum;
import com.autel.cloud.pile.base.enums.license.LicenceStatusEnum;
import com.autel.cloud.pile.base.enums.license.RemindEnableEnum;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.CrmServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.OrderCenterServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.RemindUserLicenseStatusMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLicenseReturnRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbOrderRecordMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.ChargePointVO;
import com.autel.cloud.pile.base.vo.license.LicenseDetailVO;
import com.autel.cloud.pile.base.vo.license.LicenseGroupListVO;
import com.autel.cloud.pile.base.vo.license.LicenseStatusCountByOrderIdVO;
import com.autel.cloud.pile.base.vo.license.LicenseStatusCountVO;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnLicenseVO;
import com.autel.cloud.pile.base.vo.subscribe.refund.ReturnSkuCountVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * 许可证管理
 */
@Service
@Slf4j
public class LicenseManagementServiceImpl implements LicenseManagementService {

    @Value("${subscribe.subscribeCallBack}")
    private String subscribeCallBack;

    @Autowired
    private TbLenBindRelationMapper lenBindRelationMapper;

    @Autowired
    private RemindUserLicenseStatusMapper remindUserLicenseStatusMapper;

    @Resource
    private EmailSendingService emailSendingService;

    @Resource
    private TbOrderRecordMapper tbOrderRecordMapper;

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    /*
     * 加载当前配置支持的国际化语言
     * */
    @Value("${i18n.messageproperties}")
    private String langs;

    @Autowired
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Autowired
    private ChargePointMerchantTerminalService chargePointMerchantTerminalService;

    @Autowired
    private OrderCenterServiceAdapter orderCenterServiceAdapter;

    @Autowired
    private TbLicenseReturnRecordMapper tbLicenseReturnRecordMapper;
    @Autowired
    private CrmServiceAdapter crmServiceAdapter;

    private static final Long _150_DAT = 150 * 24 * 60 * 60 * 1000L;

    private static final Long _180_DAT = 180 * 24 * 60 * 60 * 1000L;

    @Value("${subscribe.startTimingDays:180}")
    private Integer startTimeAfterNDays;

    @Value("${subscribe.startTimeAfter90Days:90}")
    private Integer startTimeAfter90Days;

    /**
     * 1705248000000: 北京时间 2024/1/15 00:00:00
     */
    @Value("${subscribe.boundaryMillisecond:1705248000000}")
    private Long boundaryMillisecond;

    @Override
    public boolean doNotRemindAgainViewEnable() {

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        log.info("===>>>LicenseManagementServiceImpl.doNotRemindAgainViewEnable jwtInfo : {}",
                JSON.toJSONString(jwtInfo));

        if (jwtInfo == null
                || jwtInfo.getPayload() == null
                || jwtInfo.getPayload().getUserId() == null
                || jwtInfo.getPayload().getSellerId() == null) {

            log.info("===>>>LicenseManagementServiceImpl.doNotRemindAgainViewEnable 获取登录信息有误！");

            return false;
        }

        boolean sellerUnusedLicenseMark = this.sellerUnusedLicenseMark(jwtInfo.getPayload().getSellerId());
        if (!sellerUnusedLicenseMark) {
            return false;
        }
        return !this.userDoNotRemindAgainMark(jwtInfo.getPayload().getUserId());
    }

    @Override
    public boolean doNotPromptAgainOperation(DoNotRemindAgainDTO doNotRemindAgainDTO) {

        log.info("===>>>LicenseManagementServiceImpl.doNotPromptAgainOperation doNotRemindAgainDTO : {}",
                JSON.toJSONString(doNotRemindAgainDTO));

        if (doNotRemindAgainDTO == null
                || doNotRemindAgainDTO.getNoLongerPromptMark() == null) {

            log.info("===>>>LicenseManagementServiceImpl.doNotPromptAgainOperation 参数有误！");

            return true;
        }

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        log.info("===>>>LicenseManagementServiceImpl.doNotPromptAgainOperation jwtInfo : {}",
                JSON.toJSONString(jwtInfo));

        if (jwtInfo == null
                || jwtInfo.getPayload() == null
                || jwtInfo.getPayload().getUserId() == null) {

            log.info("===>>>LicenseManagementServiceImpl.doNotPromptAgainOperation 获取登录信息有误！");

            return true;
        }

        return this.insertOrUpdateRemindUserLicenseStatusEntity(String.valueOf(jwtInfo.getPayload().getUserId().longValue()), doNotRemindAgainDTO.getNoLongerPromptMark() ? Integer.valueOf(0) : Integer.valueOf(1));
    }

    @Override
    public LicenseStatusCountVO getLicenseStatusCount(String skuCode, String sellerId) {

        log.info("===>>>LicenseManagementServiceImpl.getLicenseStatusCount skuCode : {}",
                JSON.toJSONString(skuCode));

        /*if (StringUtils.isBlank(skuCode)) {
            return null;
        }*/


        if (StringUtils.isBlank(sellerId)) {
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

            log.info("===>>>LicenseManagementServiceImpl.getLicenseStatusCount jwtInfo : {}",
                    JSON.toJSONString(jwtInfo));

            if (jwtInfo == null
                    || jwtInfo.getPayload() == null
                    || jwtInfo.getPayload().getSellerId() == null) {

                log.info("===>>>LicenseManagementServiceImpl.getLicenseStatusCount 获取登录信息有误！");

                return null;
            }
            sellerId = String.valueOf(jwtInfo.getPayload().getSellerId().longValue());
        }

        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getTenantId, sellerId)
                .eq(StringUtils.isNotBlank(skuCode), TbLenBindRelationEntity::getSkuCode, skuCode);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(lqw);

        LicenseStatusCountVO licenseStatusCountVO = new LicenseStatusCountVO();
        licenseStatusCountVO.setSkuCode(skuCode);
        if (ObjectUtils.isEmpty(tbLenBindRelationEntityList)) {
            licenseStatusCountVO.setWholeLicenseCount(0);
            licenseStatusCountVO.setUnusedLicenseCount(0);
            licenseStatusCountVO.setUsedLicenseCount(0);
            licenseStatusCountVO.setExpiredLicenseCount(0);
        } else {
            Map<Integer, List<TbLenBindRelationEntity>> licenseStatusAndTbLenBindRelationEntityListMap = tbLenBindRelationEntityList
                    .stream()
                    .collect(Collectors.groupingBy(TbLenBindRelationEntity::getStatus));
            licenseStatusCountVO.setWholeLicenseCount(tbLenBindRelationEntityList.size());
            licenseStatusCountVO.setReturned(ObjectUtils.isEmpty(licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.REFUND.getCode())) ? 0 : licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.REFUND.getCode()).size());

            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesUsed = licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.USED.getCode());
            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesExprired = licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.EXPIRED.getCode());
            int usedAll = CollUtil.size(tbLenBindRelationEntitiesUsed) + CollUtil.size(tbLenBindRelationEntitiesExprired);
            licenseStatusCountVO.setUsedLicenseCount(usedAll);

            // 未使用的 (包括自动激活的)
            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesUnused = licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.UNUSED.getCode());
            int unUsedSize = CollUtil.size(tbLenBindRelationEntitiesUnused);
            if (unUsedSize > 0) {
                List<TbLenBindRelationEntity> collectAutoActive = tbLenBindRelationEntitiesUnused.stream().filter(item -> Optional.ofNullable(item.getAvailableTime()).orElse(0L) > 0).collect(Collectors.toList());
                licenseStatusCountVO.setAutoActivate(CollUtil.size(collectAutoActive));
                licenseStatusCountVO.setUnusedLicenseCount(unUsedSize - CollUtil.size(collectAutoActive));
            } else {
                licenseStatusCountVO.setUnusedLicenseCount(0);
                licenseStatusCountVO.setAutoActivate(0);
            }
        }
        return licenseStatusCountVO;
    }

    @Override
    public LicenseStatusCountByOrderIdVO getLicenseStatusCountByOrderId(String orderId) {

        if (StringUtils.isBlank(orderId)) {
            return null;
        }

        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getOrderId, orderId);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(lqw);

        LicenseStatusCountByOrderIdVO licenseStatusCountByOrderIdVO = new LicenseStatusCountByOrderIdVO();
        licenseStatusCountByOrderIdVO.setOrderId(orderId);
        if (ObjectUtils.isEmpty(tbLenBindRelationEntityList)) {
            licenseStatusCountByOrderIdVO.setWholeLicenseCount(0);
            licenseStatusCountByOrderIdVO.setUnusedLicenseCount(0);
            licenseStatusCountByOrderIdVO.setUsedLicenseCount(0);
            licenseStatusCountByOrderIdVO.setExpiredLicenseCount(0);
            licenseStatusCountByOrderIdVO.setAutoActivate(0);
            licenseStatusCountByOrderIdVO.setReturned(0);
        } else {
            Map<Integer, List<TbLenBindRelationEntity>> licenseStatusAndTbLenBindRelationEntityListMap = tbLenBindRelationEntityList
                    .stream()
                    .collect(Collectors.groupingBy(TbLenBindRelationEntity::getStatus));

            // 0: 未使用，1：已使用，2：过期， 5：退货
            /**
             * 对外状态
             *    UNUSED(0, "未使用"),
             *
             *     USED(1, "已使用"),
             *
             *     AUTO_ACTIVE(4, "自动激活"),
             *
             *     REFUND(5, "退货"),
             */

            licenseStatusCountByOrderIdVO.setWholeLicenseCount(tbLenBindRelationEntityList.size());
            licenseStatusCountByOrderIdVO.setReturned(ObjectUtils.isEmpty(licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.REFUND.getCode())) ? 0 : licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.REFUND.getCode()).size());

            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesUsed = licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.USED.getCode());
            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesExprired = licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.EXPIRED.getCode());
            int usedAll = CollUtil.size(tbLenBindRelationEntitiesUsed) + CollUtil.size(tbLenBindRelationEntitiesExprired);
            licenseStatusCountByOrderIdVO.setUsedLicenseCount(usedAll);

            // 未使用的 (包括自动激活的)
            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesUnused = licenseStatusAndTbLenBindRelationEntityListMap.get(LicenceStatusEnum.UNUSED.getCode());
            int unUsedSize = CollUtil.size(tbLenBindRelationEntitiesUnused);
            if (unUsedSize > 0) {
                List<TbLenBindRelationEntity> collectAutoActive = tbLenBindRelationEntitiesUnused.stream().filter(item -> Optional.ofNullable(item.getAvailableTime()).orElse(0L) > 0).collect(Collectors.toList());
                licenseStatusCountByOrderIdVO.setAutoActivate(CollUtil.size(collectAutoActive));
                licenseStatusCountByOrderIdVO.setUnusedLicenseCount(unUsedSize - CollUtil.size(collectAutoActive));
            } else {
                licenseStatusCountByOrderIdVO.setUnusedLicenseCount(0);
                licenseStatusCountByOrderIdVO.setAutoActivate(0);
            }
        }
        return licenseStatusCountByOrderIdVO;
    }

    @Override
    public List<LicenseGroupListVO> getLicenseList(PageDTO pageDTO) {

        log.info("===>>>LicenseManagementServiceImpl.getLicenseList pageDTO : {}",
                JSON.toJSONString(pageDTO));

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        log.info("===>>>LicenseManagementServiceImpl.getLicenseList jwtInfo : {}",
                JSON.toJSONString(jwtInfo));

        if (jwtInfo == null
                || jwtInfo.getPayload() == null
                || jwtInfo.getPayload().getSellerId() == null) {

            log.info("===>>>LicenseManagementServiceImpl.getLicenseList 获取登录信息有误！");

            return Collections.emptyList();
        }

        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getTenantId, String.valueOf(jwtInfo.getPayload().getSellerId().longValue()))
                .isNotNull(TbLenBindRelationEntity::getSkuCode);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(lqw);

        if (ObjectUtils.isEmpty(tbLenBindRelationEntityList)) {
            return Collections.emptyList();
        }

        Map<String, List<TbLenBindRelationEntity>> skuAndTbLenBindRelationEntityListMap = tbLenBindRelationEntityList
                .stream()
                .collect(Collectors.groupingBy(TbLenBindRelationEntity::getSkuCode));

        List<LicenseGroupListVO> licenseGroupListVOList = new ArrayList<>();
        skuAndTbLenBindRelationEntityListMap.forEach((key, value) -> {
            LicenseGroupListVO licenseGroupListVO = new LicenseGroupListVO();
            AtomicInteger usedCount = new AtomicInteger();
            licenseGroupListVO.setGoodsName(value.get(0).getGoodsName());
            licenseGroupListVO.setMeasureUnit(LicenseAboutConvert.getMeasureUnit(value.get(0).getMeasureUnit()));
            licenseGroupListVO.setPileType(LicenseAboutConvert.getPileType(value.get(0).getChargeType()));
            licenseGroupListVO.setServiceTime(value.get(0).getServiceTime());
            licenseGroupListVO.setTimeUnit(value.get(0).getTimeUnit());
            List<TbLenBindRelationEntity> unUsedTbLenBindRelationEntityList = new ArrayList<>();
            value.forEach(var -> {
                if (LicenceStatusEnum.UNUSED.getCode().equals(var.getStatus())) {
                    unUsedTbLenBindRelationEntityList.add(var);
                }
                if (LicenceStatusEnum.USED.getCode().equals(var.getStatus()) || LicenceStatusEnum.EXPIRED.getCode().equals(var.getStatus())) {
                    usedCount.getAndIncrement();
                }
            });
            if (ObjectUtils.isNotEmpty(unUsedTbLenBindRelationEntityList)) {
                licenseGroupListVO.setUnUsedLicenceInfoDtoList(LicenseAboutConvert.batchBuildUnUsedLicenceInfoDto(unUsedTbLenBindRelationEntityList));
                licenseGroupListVO.setUnusedLicenseCount(licenseGroupListVO.getUnUsedLicenceInfoDtoList().size());
            } else {
                licenseGroupListVO.setUnUsedLicenceInfoDtoList(null);
                licenseGroupListVO.setUnusedLicenseCount(0);
            }
            licenseGroupListVO.setLicenseTotalCount(value.size());
            licenseGroupListVO.setUsedLicenseCount(usedCount.intValue());
            licenseGroupListVO.setLastPurchaseDate(LicenseAboutConvert.getLastPurchaseDate(value));
            licenseGroupListVO.setSkuCode(key);
            licenseGroupListVO.setCommodityCode(value.get(0).getGoodsId());
            licenseGroupListVOList.add(licenseGroupListVO);
        });
        if (licenseGroupListVOList.size() > 1) {
            // 新增排序规则 商品名称（升序）> AC/DC（升序） > 订阅时长（降序）
            Comparator<LicenseGroupListVO> comparator = (o1, o2) -> {
                // 按GoodsName升序
                int result = o1.getGoodsName().compareTo(o2.getGoodsName());
                if (result != 0) {
                    return result;
                }

                // GoodsName 相同则按PileType升序
                result = o1.getPileType().compareTo(o2.getPileType());
                if (result != 0) {
                    return result;
                }

                // PileType也相同则按ServiceTime降序
                return o2.getServiceTime().compareTo(o1.getServiceTime());
            };
            licenseGroupListVOList.sort(comparator);
        }
        AtomicInteger i = new AtomicInteger(1);
        licenseGroupListVOList.forEach(var -> var.setSerialNumber(i.getAndIncrement()));
        return licenseGroupListVOList;
    }

    @Override
    public List<LicenseGroupListVO> getLicenseListBySellerId(String sellerId) {

        if (StringUtils.isBlank(sellerId)) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getTenantId, sellerId)
                .isNotNull(TbLenBindRelationEntity::getSkuCode);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(lqw);
        log.info("getLicenseListBySellerId,tbLenBindRelationEntityList={}", JSON.toJSONString(tbLenBindRelationEntityList));

        if (CollectionUtils.isEmpty(tbLenBindRelationEntityList)) {
            return Collections.emptyList();
        }

        Map<String, List<TbLenBindRelationEntity>> skuAndTbLenBindRelationEntityListMap = tbLenBindRelationEntityList
                .stream()
                .collect(Collectors.groupingBy(TbLenBindRelationEntity::getSkuCode));

        List<LicenseGroupListVO> licenseGroupListVOList = new ArrayList<>();
        skuAndTbLenBindRelationEntityListMap.forEach((key, value) -> {
            LicenseGroupListVO licenseGroupListVO = new LicenseGroupListVO();
            licenseGroupListVO.setGoodsName(value.get(0).getGoodsName());
            licenseGroupListVO.setCommodityCode(value.get(0).getGoodsName());
            licenseGroupListVO.setMeasureUnit(LicenseAboutConvert.getMeasureUnit(value.get(0).getMeasureUnit()));
            licenseGroupListVO.setPileType(LicenseAboutConvert.getPileType(value.get(0).getChargeType()));
            licenseGroupListVO.setServiceTime(value.get(0).getServiceTime());
            licenseGroupListVO.setTimeUnit(value.get(0).getTimeUnit());
            List<TbLenBindRelationEntity> unUsedTbLenBindRelationEntityList = new ArrayList<>();
            value.forEach(var -> {
                if (LicenceStatusEnum.UNUSED.getCode().equals(var.getStatus())) {
                    unUsedTbLenBindRelationEntityList.add(var);
                }
            });
            if (ObjectUtils.isNotEmpty(unUsedTbLenBindRelationEntityList)) {
                licenseGroupListVO.setUnUsedLicenceInfoDtoList(LicenseAboutConvert.batchBuildUnUsedLicenceInfoDto(unUsedTbLenBindRelationEntityList));
                licenseGroupListVO.setUnusedLicenseCount(licenseGroupListVO.getUnUsedLicenceInfoDtoList().size());
            } else {
                licenseGroupListVO.setUnUsedLicenceInfoDtoList(null);
                licenseGroupListVO.setUnusedLicenseCount(0);
            }
            licenseGroupListVO.setLicenseTotalCount(value.size());
            licenseGroupListVO.setLastPurchaseDate(LicenseAboutConvert.getLastPurchaseDate(value));
            licenseGroupListVO.setSkuCode(key);
            licenseGroupListVOList.add(licenseGroupListVO);
        });
        if (licenseGroupListVOList.size() > 1) {
            Collections.sort(licenseGroupListVOList);
        }
        AtomicInteger i = new AtomicInteger(1);
        licenseGroupListVOList.forEach(var -> var.setSerialNumber(i.getAndIncrement()));
        return licenseGroupListVOList;
    }

    @Override
    public Page<LicenseDetailVO> getLicenseDetailPage(LicenseDetailDTO licenseDetailDTO) {

        log.info("===>>>LicenseManagementServiceImpl.getLicenseDetailPage licenseDetailDTO : {}",
                JSON.toJSONString(licenseDetailDTO));

        final Integer page = licenseDetailDTO.getPage();
        final Integer pageSize = licenseDetailDTO.getPageSize();
        Page<LicenseDetailVO> resultPage = new Page<>(page, pageSize);

        if (StringUtils.isBlank(licenseDetailDTO.getSkuCode())) {
            return resultPage;
        }

        String sellerId = licenseDetailDTO.getSellerId();
        if (StringUtils.isBlank(sellerId)) {
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

            log.info("===>>>LicenseManagementServiceImpl.getLicenseDetailPage jwtInfo : {}",
                    JSON.toJSONString(jwtInfo));

            if (jwtInfo == null
                    || jwtInfo.getPayload() == null
                    || jwtInfo.getPayload().getSellerId() == null) {

                log.info("===>>>LicenseManagementServiceImpl.getLicenseDetailPage 获取登录信息有误！");

                return resultPage;
            }
            sellerId = String.valueOf(jwtInfo.getPayload().getSellerId().longValue());
        }

        List<Integer> licenseStatusList = new ArrayList<>();
        if (licenseDetailDTO.getLicenceStatus() != null) {
            if (LicenceStatusEnum.UNUSED.getCode().equals(licenseDetailDTO.getLicenceStatus())) {
                licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
            } else if (LicenceStatusEnum.USED.getCode().equals(licenseDetailDTO.getLicenceStatus())) {
                licenseStatusList.add(LicenceStatusEnum.USED.getCode());
                licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
            } else if (LicenceStatusEnum.EXPIRED.getCode().equals(licenseDetailDTO.getLicenceStatus())) {
                licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
            } else if (LicenceStatusEnum.REFUND.getCode().equals(licenseDetailDTO.getLicenceStatus())) {
                licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
            } else if (LicenceStatusEnum.AUTO_ACTIVE.getCode().equals(licenseDetailDTO.getLicenceStatus())) {
                // 自动激活的，状态仍然是 0 ， 需要特殊处理
                licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
            } else {
                licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
                licenseStatusList.add(LicenceStatusEnum.USED.getCode());
                licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
                licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
            }
        } else if (CollUtil.isNotEmpty(licenseDetailDTO.getLicenceStatusList())) {
            licenseDetailDTO.getLicenceStatusList().forEach(item -> {
                if (LicenceStatusEnum.UNUSED.getCode().equals(item)) {
                    licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
                } else if (LicenceStatusEnum.USED.getCode().equals(item)) {
                    licenseStatusList.add(LicenceStatusEnum.USED.getCode());
                    licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
                } else if (LicenceStatusEnum.EXPIRED.getCode().equals(item)) {
                    licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
                } else if (LicenceStatusEnum.REFUND.getCode().equals(item)) {
                    licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
                } else if (LicenceStatusEnum.AUTO_ACTIVE.getCode().equals(item)) {
                    // 自动激活的，状态仍然是 0 ， 需要特殊处理
                    licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
                } else {
                    licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
                    licenseStatusList.add(LicenceStatusEnum.USED.getCode());
                    licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
                    licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
                }
            });
        } else {
            licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
            licenseStatusList.add(LicenceStatusEnum.USED.getCode());
            licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
            licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
        }

        Page<TbLenBindRelationEntity> conditionPage = new Page<>(page, pageSize);
        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getTenantId, sellerId)
                .eq(TbLenBindRelationEntity::getSkuCode, licenseDetailDTO.getSkuCode())
                .in(TbLenBindRelationEntity::getStatus, licenseStatusList)
                .gt(LicenceStatusEnum.AUTO_ACTIVE.getCode().equals(licenseDetailDTO.getLicenceStatus()), TbLenBindRelationEntity::getAvailableTime, 0L )
                .isNull(LicenceStatusEnum.UNUSED.getCode().equals(licenseDetailDTO.getLicenceStatus()), TbLenBindRelationEntity::getAvailableTime);
                /*.orderByAsc(TbLenBindRelationEntity::getStatus)
                .orderByDesc(TbLenBindRelationEntity::getUnavailableTime, TbLenBindRelationEntity::getCreateTime);*/

        // licenseStatusList 处理
        if (CollUtil.isNotEmpty(licenseDetailDTO.getLicenceStatusList())) {
            //
            if (licenseDetailDTO.getLicenceStatusList().contains(0) && !licenseDetailDTO.getLicenceStatusList().contains(4)) {
                lqw.isNull(TbLenBindRelationEntity::getAvailableTime);
            }
            if (licenseDetailDTO.getLicenceStatusList().contains(4) && !licenseDetailDTO.getLicenceStatusList().contains(0)) {
                lqw.gt(TbLenBindRelationEntity::getAvailableTime, 0L );
            }
        }

        if (StringUtils.isNotBlank(licenseDetailDTO.getLicenceCode())) {
            lqw.and(tbLenBindRelationEntityLambdaQueryWrapper -> tbLenBindRelationEntityLambdaQueryWrapper.like(TbLenBindRelationEntity::getLicenceCode, licenseDetailDTO.getLicenceCode())
                .or().like(TbLenBindRelationEntity::getPileSn, licenseDetailDTO.getLicenceCode()));
        }

        lqw.isNull(Boolean.TRUE == licenseDetailDTO.getExcludeAutoCountDown(), TbLenBindRelationEntity::getAvailableTime);

        if (StringUtils.isNotBlank(licenseDetailDTO.getOrderBy()) && StringUtils.isNotBlank(licenseDetailDTO.getOrderType())) {
            if ("purchaseDate".equalsIgnoreCase(licenseDetailDTO.getOrderBy())) {
                // asc
                if ("asc".equalsIgnoreCase(licenseDetailDTO.getOrderType())) {
                    lqw.orderByAsc(TbLenBindRelationEntity::getCreateTime);
                } else {
                    lqw.orderByDesc(TbLenBindRelationEntity::getCreateTime);
                }
            } else if ("bindTime".equalsIgnoreCase(licenseDetailDTO.getOrderBy())) {
                if ("asc".equalsIgnoreCase(licenseDetailDTO.getOrderType())) {
                    lqw.orderByAsc(TbLenBindRelationEntity::getBindTime);
                } else {
                    lqw.orderByDesc(TbLenBindRelationEntity::getBindTime);
                }
            }
        } else {
            lqw.orderByAsc(TbLenBindRelationEntity::getStatus).orderByDesc(TbLenBindRelationEntity::getUnavailableTime, TbLenBindRelationEntity::getCreateTime);
        }

        Page<TbLenBindRelationEntity> tbLenBindRelationEntityPage = lenBindRelationMapper.selectPage(conditionPage, lqw);

        if (tbLenBindRelationEntityPage == null
                || ObjectUtils.isEmpty(tbLenBindRelationEntityPage.getRecords())) {
            return resultPage;
        }

        // 排序状态： 自动生效、未使用，已使用，已退货
        // 排序2： 创建时间倒序
       /* tbLenBindRelationEntityPage.getRecords().sort((o1, o2) -> {
            // 建立状态值到排序权重的映射，状态1和2映射到同一权重
            Map<Integer, Integer> priorityMap = new HashMap<>();
            priorityMap.put(0, 1);
            priorityMap.put(1, 2);
            priorityMap.put(2, 2);
            priorityMap.put(5, 3);

            Integer rank1 = priorityMap.get(o1.getStatus());
            Integer rank2 = priorityMap.get(o2.getStatus());

            // 如果状态权重不同，则根据权重排序
            if (!rank1.equals(rank2)) {
                return rank1.compareTo(rank2);
            } else if (o1.getStatus() == 1 || o1.getStatus() == 2) {
                // 状态1和2视为同一级别，继续比较createTime来确定顺序
                // 使用equals比较创建时间，如果它们不相等，那么执行降序排序
                if (!o1.getCreateTime().equals(o2.getCreateTime())) {
                    return o2.getCreateTime().compareTo(o1.getCreateTime());
                } else {
                    return 0; // createTime也相同，保持原始顺序
                }
            } else {
                return 0; // 状态相同但不是2或3，保持原始顺序
            }
        });
*/
        List<LicenseDetailVO> licenseDetailVOList = new ArrayList<>();
        Set<String> pileSnSet = new HashSet<>();
        tbLenBindRelationEntityPage.getRecords().forEach(var -> {
            LicenseDetailVO licenseDetailVO = new LicenseDetailVO();
            licenseDetailVO.setId(var.getId());
            licenseDetailVO.setSkuCode(var.getSkuCode());
            licenseDetailVO.setLicenceCode(var.getLicenceCode());
            licenseDetailVO.setBindTime(var.getBindTime());

            // 设置剩余有效宽限期 剩余宽限期天数 =  额外增加天数 + 90 - （当前日期 - 创建日期 ）相差天数
            Long createTime = var.getCreateTime();
            int baseDays = createTime < boundaryMillisecond ? 180 : 90;
            Integer extended = Optional.ofNullable(var.getExtendedGracePeriod()).orElse(0);
            int extendedRemainingDays =  (int)Math.floor(baseDays + extended -(((double)(System.currentTimeMillis() - createTime))/(3600000 * 24)));
            if (var.getStatus() == null || var.getStatus() == 0) {
                licenseDetailVO.setExtendedRemainingDays(extendedRemainingDays);
            }

            // 处理状态映射
            Integer finalStatus = licenceStatusMapping(var);
            licenseDetailVO.setLicenceStatus(finalStatus);
            licenseDetailVO.setMeasureUnit(LicenseAboutConvert.getMeasureUnit(var.getMeasureUnit()));
            licenseDetailVO.setPileType(LicenseAboutConvert.getPileType(var.getChargeType()));
            licenseDetailVO.setServiceTime(var.getServiceTime());
            licenseDetailVO.setTimeUnit(var.getTimeUnit());
            licenseDetailVO.setLicenseEffectiveDate(var.getAvailableTime());
            long currentTimeMillis = System.currentTimeMillis();
            if (licenseDetailVO.getLicenseEffectiveDate() == null
                    && LicenceStatusEnum.UNUSED.getCode().equals(licenseDetailVO.getLicenceStatus())
                    && var.getCreateTime() != null) {
                // 倒计时时间判断修改
                Long nDaysMilliSec = var.getCreateTime() < boundaryMillisecond ? startTimeAfterNDays * 24*3600*1000L : startTimeAfter90Days * 24*3600*1000L;
                Long totalTime = var.getCreateTime() + nDaysMilliSec + (var.getExtendedGracePeriod() == null ? 0L : var.getExtendedGracePeriod()) *  24*3600*1000L;
                Long condition = totalTime - 30*24*3600*1000L - currentTimeMillis;
                // 小于30 天，需要展示
                if (condition <= 0) {
                    int licenseEffectiveCountdownDay = LicenseAboutConvert.getNaturalDays(currentTimeMillis, totalTime) + 1;
                    licenseDetailVO.setLicenseEffectiveCountdownDay(licenseEffectiveCountdownDay <= 0 ? null : licenseEffectiveCountdownDay);
                }
            }
            licenseDetailVO.setDaysRemaining(LicenseAboutConvert.getDaysRemaining(var));
            licenseDetailVO.setLicenseExpirationDate(var.getUnavailableTime());
            licenseDetailVO.setPurchaseDate(var.getCreateTime());
            licenseDetailVO.setAssociatePileSn(var.getPileSn());
            licenseDetailVO.setBonusDurationValue(var.getBonusDurationValue());
            if (StringUtils.isNotBlank(licenseDetailVO.getAssociatePileSn())) {
                pileSnSet.add(licenseDetailVO.getAssociatePileSn());
            }
            licenseDetailVOList.add(licenseDetailVO);
        });

        Map<String, ChargePointVO> pileSnAndChargePointVOMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(pileSnSet)) {
            List<ChargePointVO> chargePointVOList = chargePointMerchantRelationService.findBySNs(pileSnSet, Long.valueOf(sellerId));
            if (ObjectUtils.isNotEmpty(chargePointVOList)) {
                chargePointVOList.forEach(var -> pileSnAndChargePointVOMap.put(var.getSn(), var));
            }

            // 同步查询终端表
            List<ChargePointMerchantTerminalEntity> terminalList = chargePointMerchantTerminalService.getTerminalList(new ArrayList<>(pileSnSet), Long.valueOf(sellerId));
            if (ObjectUtils.isNotEmpty(terminalList)) {
                terminalList.forEach(item -> {
                    ChargePointVO chargePointVO = new ChargePointVO();
                    chargePointVO.setSn(item.getTerminalSn());
                    chargePointVO.setName(item.getTerminalName());
                    pileSnAndChargePointVOMap.put(item.getTerminalSn(), chargePointVO);
                });
            }
        }

        AtomicInteger i = new AtomicInteger(1);
        licenseDetailVOList.forEach(var -> {
            if (StringUtils.isNotBlank(var.getAssociatePileSn())) {
                var.setAssociatePileName(pileSnAndChargePointVOMap.get(var.getAssociatePileSn()) != null ? pileSnAndChargePointVOMap.get(var.getAssociatePileSn()).getName() : null);
            }
            var.setSerialNumber((page - 1) * pageSize + i.getAndIncrement());
        });

        resultPage.setTotal(tbLenBindRelationEntityPage.getTotal());
        resultPage.setRecords(licenseDetailVOList);
        return resultPage;
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

    @Override
    public Page<LicenseDetailVO> getLicenseDetailPageByOrderId(GetLicenseDetailPageByOrderIdDTO getLicenseDetailPageByOrderIdDTO) {

        log.info("getLicenseDetailPageByOrderId licenseDetailDTO : {}",
                JSON.toJSONString(getLicenseDetailPageByOrderIdDTO));

        final Integer page = getLicenseDetailPageByOrderIdDTO.getPage();
        final Integer pageSize = getLicenseDetailPageByOrderIdDTO.getPageSize();
        Page<LicenseDetailVO> resultPage = new Page<>(page, pageSize);

        if (StringUtils.isBlank(getLicenseDetailPageByOrderIdDTO.getOrderId())
                || getLicenseDetailPageByOrderIdDTO.getLicenceStatus() == null) {
            return resultPage;
        }

        List<Integer> licenseStatusList = new ArrayList<>();
        if (LicenceStatusEnum.UNUSED.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus())) {
            licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
        } else if (LicenceStatusEnum.USED.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus())) {
            licenseStatusList.add(LicenceStatusEnum.USED.getCode());
            licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
        } else if (LicenceStatusEnum.EXPIRED.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus())) {
            licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
        } else if (LicenceStatusEnum.REFUND.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus())) {
            licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
        } else if (LicenceStatusEnum.AUTO_ACTIVE.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus())) {
            // 自动激活的，状态仍然是 0 ， 需要特殊处理
            licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
        } else {
            licenseStatusList.add(LicenceStatusEnum.UNUSED.getCode());
            licenseStatusList.add(LicenceStatusEnum.USED.getCode());
            licenseStatusList.add(LicenceStatusEnum.EXPIRED.getCode());
            licenseStatusList.add(LicenceStatusEnum.REFUND.getCode());
        }


        Page<TbLenBindRelationEntity> conditionPage = new Page<>(page, pageSize);
        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getOrderId, getLicenseDetailPageByOrderIdDTO.getOrderId())
                .in(TbLenBindRelationEntity::getStatus, licenseStatusList)
                .gt(LicenceStatusEnum.AUTO_ACTIVE.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus()), TbLenBindRelationEntity::getAvailableTime, 0L )
                .isNull(LicenceStatusEnum.UNUSED.getCode().equals(getLicenseDetailPageByOrderIdDTO.getLicenceStatus()), TbLenBindRelationEntity::getAvailableTime)
                .orderByAsc(TbLenBindRelationEntity::getStatus, TbLenBindRelationEntity::getUnavailableTime, TbLenBindRelationEntity::getId);
        Page<TbLenBindRelationEntity> tbLenBindRelationEntityPage = lenBindRelationMapper.selectPage(conditionPage, lqw);

        if (tbLenBindRelationEntityPage == null
                || ObjectUtils.isEmpty(tbLenBindRelationEntityPage.getRecords())) {
            return resultPage;
        }
        List<LicenseDetailVO> licenseDetailVOList = new ArrayList<>();
        Set<String> pileSnSet = new HashSet<>();
        tbLenBindRelationEntityPage.getRecords().forEach(var -> {
            LicenseDetailVO licenseDetailVO = new LicenseDetailVO();
            licenseDetailVO.setId(var.getId());
            licenseDetailVO.setSkuCode(var.getSkuCode());
            licenseDetailVO.setLicenceCode(var.getLicenceCode());
            // 处理状态映射
            Integer finalStatus = licenceStatusMapping(var);
            licenseDetailVO.setLicenceStatus(finalStatus);
            licenseDetailVO.setMeasureUnit(LicenseAboutConvert.getMeasureUnit(var.getMeasureUnit()));
            licenseDetailVO.setPileType(LicenseAboutConvert.getPileType(var.getChargeType()));
            licenseDetailVO.setServiceTime(var.getServiceTime());
            licenseDetailVO.setTimeUnit(var.getTimeUnit());
            licenseDetailVO.setLicenseEffectiveDate(var.getAvailableTime());
            long currentTimeMillis = System.currentTimeMillis();
            if (licenseDetailVO.getLicenseEffectiveDate() == null
                    && LicenceStatusEnum.UNUSED.getCode().equals(licenseDetailVO.getLicenceStatus())
                    && var.getCreateTime() != null) {
                // 倒计时时间判断修改
                Long nDaysMilliSec = var.getCreateTime() < boundaryMillisecond ? startTimeAfterNDays * 24*3600*1000L : startTimeAfter90Days * 24*3600*1000L;
                Long totalTime = var.getCreateTime() + nDaysMilliSec + (var.getExtendedGracePeriod() == null ? 0L : var.getExtendedGracePeriod()) *  24*3600*1000L;
                Long condition = totalTime - 30*24*3600*1000L - currentTimeMillis;
                // 小于30 天，需要展示
                if (condition <= 0) {
                    int licenseEffectiveCountdownDay = LicenseAboutConvert.getNaturalDays(currentTimeMillis, totalTime) + 1;
                    licenseDetailVO.setLicenseEffectiveCountdownDay(licenseEffectiveCountdownDay <= 0 ? null : licenseEffectiveCountdownDay);
                }
            }
            licenseDetailVO.setDaysRemaining(LicenseAboutConvert.getDaysRemaining(var));
            licenseDetailVO.setLicenseExpirationDate(var.getUnavailableTime());
            licenseDetailVO.setPurchaseDate(var.getCreateTime());
            licenseDetailVO.setAssociatePileSn(var.getPileSn());
            licenseDetailVO.setBonusDurationValue(var.getBonusDurationValue());
            if (StringUtils.isNotBlank(licenseDetailVO.getAssociatePileSn())) {
                pileSnSet.add(licenseDetailVO.getAssociatePileSn());
            }
            licenseDetailVOList.add(licenseDetailVO);
        });
        String sellerId = tbLenBindRelationEntityPage.getRecords().get(0).getTenantId();
        Map<String, ChargePointVO> pileSnAndChargePointVOMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(pileSnSet)) {
            List<ChargePointVO> chargePointVOList = chargePointMerchantRelationService.findBySNs(pileSnSet, Long.valueOf(sellerId));
            if (ObjectUtils.isNotEmpty(chargePointVOList)) {
                chargePointVOList.forEach(var -> pileSnAndChargePointVOMap.put(var.getSn(), var));
            }
        }

        AtomicInteger i = new AtomicInteger(1);
        licenseDetailVOList.forEach(var -> {
            if (StringUtils.isNotBlank(var.getAssociatePileSn())) {
                var.setAssociatePileName(pileSnAndChargePointVOMap.get(var.getAssociatePileSn()) != null ? pileSnAndChargePointVOMap.get(var.getAssociatePileSn()).getName() : null);
            }
            var.setSerialNumber((page - 1) * pageSize + i.getAndIncrement());
        });

        resultPage.setTotal(tbLenBindRelationEntityPage.getTotal());
        resultPage.setRecords(licenseDetailVOList);
        return resultPage;
    }

    @Override
    public Integer restoreLicenseStatus() {

        Long currentTimeMillis = System.currentTimeMillis();

        log.info("===>>>LicenseManagementServiceImpl.restoreLicenseStatus currentTimeMillis : {}",
                JSON.toJSONString(currentTimeMillis));

        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getStatus, 3);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(lqw);
        if (ObjectUtils.isEmpty(tbLenBindRelationEntityList)) {
            return 0;
        }
        AtomicInteger count = new AtomicInteger();
        tbLenBindRelationEntityList.forEach(var -> {
            var.setStatus(1);
            var.setUpdateTime(currentTimeMillis);
            int flag = lenBindRelationMapper.updateById(var);
            if (flag > 0) {
                count.getAndIncrement();
            }
        });
        return count.get();
    }

    @Override
    public List<ReturnSkuCountVO> getReturnSkuCountByOrderId(Long orderId) {
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = new LambdaQueryWrapper<TbLenBindRelationEntity>()
                .eq(TbLenBindRelationEntity::getOrderId, orderId);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(queryWrapper);

        HashMap<String, Integer> sku2CountMapping = new HashMap<>();
        tbLenBindRelationEntityList.forEach(item -> {
            if (sku2CountMapping.containsKey(item.getSkuCode())) {
                if (LicenceStatusEnum.UNUSED.getCode().equals(item.getStatus())) {
                    sku2CountMapping.put(item.getSkuCode(), sku2CountMapping.get(item.getSkuCode()) + 1);
                }
            } else {
                if (LicenceStatusEnum.UNUSED.getCode().equals(item.getStatus())) {
                    sku2CountMapping.put(item.getSkuCode(), 1);
                }
            }
        });

        return sku2CountMapping.entrySet().stream().map(item -> new ReturnSkuCountVO(item.getKey(), item.getValue())).collect(Collectors.toList());
    }

    @Override
    public ReturnOrderRespVO ReturnLicense(ReturnLicenseVO returnLicenseVO) {
        //1.1校验-只有未使用的license才能退货
        List<String> licenseList = new ArrayList<>();
        returnLicenseVO.getSkuReturns().forEach(item ->licenseList.addAll(item.getLicenses()));

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = new LambdaQueryWrapper<TbLenBindRelationEntity>()
                .in(TbLenBindRelationEntity::getLicenceCode, licenseList);
        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(queryWrapper);
        for (TbLenBindRelationEntity item : tbLenBindRelationEntityList) {
            if (!LicenceStatusEnum.UNUSED.getCode().equals(item.getStatus())) {
                log.warn("license={} status not unused", item.getLicenceCode());
                throw new MessageCodeException(PileBaseEnum.CONTAIN_NOT_UNUSED_LICENSE);
            }
        }
        //1.2校验-发起过退货的license，并且不是退货失败的不能重复退货
        LambdaQueryWrapper<TbLicenseReturnRecordEntity> queryWrapper2 = new LambdaQueryWrapper<TbLicenseReturnRecordEntity>()
                .in(TbLicenseReturnRecordEntity::getLicenseCode, licenseList);
        List<TbLicenseReturnRecordEntity> tbLicenseReturnRecordEntityList = tbLicenseReturnRecordMapper.selectList(queryWrapper2);
        for (TbLicenseReturnRecordEntity item : tbLicenseReturnRecordEntityList) {
            if (LicenceReturnStatusEnum.RETURNING.getCode().equals(item.getStatus())
            || LicenceReturnStatusEnum.SUCCESS.getCode().equals(item.getStatus())) {
                log.warn("license={} repeat return", item.getLicenseCode());
                throw new MessageCodeException(PileBaseEnum.CONTAIN_REPEAT_RETURN_LICENSE);
            }
        }


        //2.调订单中心退货
        ReturnOrderReqVO returnOrderReqVO = ReturnOrderReqVO.builder()
                .appId(returnLicenseVO.getAppId())
                .appName(returnLicenseVO.getAppName())
                .operatorId(returnLicenseVO.getOperatorId())
                .orderId(returnLicenseVO.getOrderId())
                .refundAmount(returnLicenseVO.getReturnAmount())
                .returnReason(returnLicenseVO.getReturnReason())
                .callbackUrl(subscribeCallBack)
                .currency(returnLicenseVO.getCurrency())
                .build();
        ReturnOrderRespVO returnOrderRespVO = orderCenterServiceAdapter.orderReturn(returnOrderReqVO);

        //3.保存退货记录
        TbLicenseReturnRecordEntity returnRecordEntity = TbLicenseReturnRecordEntity.builder()
                .orderId(returnLicenseVO.getOrderId())
                .returnId(returnOrderRespVO.getReturnId())
                .status(returnOrderRespVO.getReturnStatus())
                .createBy(String.valueOf(returnLicenseVO.getOperatorId()))
                .updateBy(String.valueOf(returnLicenseVO.getOperatorId()))
                .createTime(System.currentTimeMillis())
                .updateTime(System.currentTimeMillis())
                .build();
        licenseList.forEach(item -> {
            returnRecordEntity.setId(IdWorker.getId());
            returnRecordEntity.setLicenseCode(item);
            tbLicenseReturnRecordMapper.insert(returnRecordEntity);
        });

        if (returnOrderRespVO.getReturnStatus() == 1) {
            returnResultSuccessHandle(String.valueOf(returnOrderRespVO.getOrderId()), returnOrderRespVO.getReturnId(),
                    returnOrderRespVO.getReturnStatus());
        }

        return returnOrderRespVO;
    }

    @Override
    public void returnResultSuccessHandle(String orderId, Long returnId, Integer returnStatus) {
        //更新license退货记录表状态
        LambdaQueryWrapper<TbLicenseReturnRecordEntity> returnWrapper = new QueryWrapper<TbLicenseReturnRecordEntity>().lambda()
                .eq(TbLicenseReturnRecordEntity::getReturnId, returnId);
        tbLicenseReturnRecordMapper.update(new TbLicenseReturnRecordEntity(returnStatus), returnWrapper);

        //更新tb_len_bind_relation表状态改为失效
        List<TbLicenseReturnRecordEntity> returnRecordEntities = tbLicenseReturnRecordMapper.selectList(returnWrapper);
        List<String> licenseCodeList = returnRecordEntities.stream().map(item -> item.getLicenseCode()).collect(Collectors.toList());
        LambdaQueryWrapper<TbLenBindRelationEntity> lenBindWrapper = new QueryWrapper<TbLenBindRelationEntity>().lambda()
                .in(TbLenBindRelationEntity::getLicenceCode, licenseCodeList);
        lenBindRelationMapper.update(new TbLenBindRelationEntity(5), lenBindWrapper);

        // 该订单通知给crm让其在sf退货
        OrderTypeNoticeDTO orderTypeNoticeDTO = new OrderTypeNoticeDTO(BaseConstant.CALLBACK_EVENT_RETURN, orderId, returnId);
        AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> crmServiceAdapter.orderTypeNotice(orderTypeNoticeDTO)));


        AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> sendSubscriptionProductReturn(orderId,returnId)));
    }

    public void sendSubscriptionProductReturn(String orderId, Long returnId) {
        try {
            LambdaQueryWrapper<TbOrderRecordEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(TbOrderRecordEntity::getOrderId, orderId);
            final TbOrderRecordEntity tbOrderRecordEntity = tbOrderRecordMapper.selectOne(queryWrapper);
            String language = getSellerLanguage(orderId, tbOrderRecordEntity.getTenantId());
            emailSendingService.sendSubscriptionProductReturn(orderId, returnId, language, tbOrderRecordEntity.getTenantId());
        } catch (Exception e) {
            log.error("--->>> send Subscription Product Return email error.", e);
        }
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

    private boolean userDoNotRemindAgainMark(Long userId) {

        log.info("===>>>LicenseManagementServiceImpl.userDoNotRemindAgainMark userId : {}",
                JSON.toJSONString(userId));

        LambdaQueryWrapper<RemindUserLicenseStatusEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(RemindUserLicenseStatusEntity::getUserId, String.valueOf(userId.longValue()));
        RemindUserLicenseStatusEntity remindUserLicenseStatusEntity = remindUserLicenseStatusMapper.selectOne(lqw);

        if (remindUserLicenseStatusEntity == null
                || RemindEnableEnum.NEED_REMIND.getCode().equals(remindUserLicenseStatusEntity.getRemindEnable())) {
            if (remindUserLicenseStatusEntity == null) {
                this.insertOrUpdateRemindUserLicenseStatusEntity(String.valueOf(userId.longValue()), RemindEnableEnum.NEED_REMIND.getCode());
            }
            return false;
        }
        return true;
    }

    private boolean sellerUnusedLicenseMark(Long sellerId) {

        log.info("===>>>LicenseManagementServiceImpl.sellerUnusedLicenseMark sellerId : {}",
                JSON.toJSONString(sellerId));

        LambdaQueryWrapper<TbLenBindRelationEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(TbLenBindRelationEntity::getTenantId, String.valueOf(sellerId.longValue()))
                .isNotNull(TbLenBindRelationEntity::getSkuCode)
                .eq(TbLenBindRelationEntity::getStatus, LicenceStatusEnum.UNUSED.getCode());

        List<TbLenBindRelationEntity> tbLenBindRelationEntityList = lenBindRelationMapper.selectList(lqw);
        return ObjectUtils.isNotEmpty(tbLenBindRelationEntityList);
    }

    private boolean insertOrUpdateRemindUserLicenseStatusEntity(String userId, Integer remindEnable) {

        log.info("===>>>LicenseManagementServiceImpl.insertOrUpdateRemindUserLicenseStatusEntity userId : {} and remindEnable : {}",
                JSON.toJSONString(userId),
                JSON.toJSONString(remindEnable));

        LambdaQueryWrapper<RemindUserLicenseStatusEntity> lwq = new LambdaQueryWrapper<>();
        lwq.eq(RemindUserLicenseStatusEntity::getUserId, userId);
        RemindUserLicenseStatusEntity remindUserLicenseStatusEntity = remindUserLicenseStatusMapper.selectOne(lwq);
        long currentTimeMillis = System.currentTimeMillis();
        if (remindUserLicenseStatusEntity == null) {
            RemindUserLicenseStatusEntity insertEntity = new RemindUserLicenseStatusEntity();
            insertEntity.setUserId(userId);
            insertEntity.setRemindEnable(remindEnable);
            insertEntity.setCreateBy(userId);
            insertEntity.setUpdateBy(userId);
            insertEntity.setCreateTime(currentTimeMillis);
            insertEntity.setUpdateTime(currentTimeMillis);
            return remindUserLicenseStatusMapper.insert(insertEntity) > 0;
        } else {
            remindUserLicenseStatusEntity.setRemindEnable(remindEnable);
            remindUserLicenseStatusEntity.setUpdateBy(userId);
            remindUserLicenseStatusEntity.setUpdateTime(currentTimeMillis);
            return remindUserLicenseStatusMapper.updateById(remindUserLicenseStatusEntity) > 0;
        }
    }

    @Override
    public LicenseStatusCountVO getStatusCountBySellerId() {

        return null;
    }
}
