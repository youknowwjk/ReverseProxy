package com.autel.cloud.pile.base.domain.job;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.domain.service.SubscribePlaceOrderService;
import com.autel.cloud.pile.base.dto.AgreementFunctionDetailDto;
import com.autel.cloud.pile.base.dto.UpdateEsSubscriptionStatusDTO;
import com.autel.cloud.pile.base.dto.subscribe.BenefitFunctionReqDto;
import com.autel.cloud.pile.base.dto.subscribe.PileBenefitFunctionRespDto;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.dto.subscribe.UnusedLicenceQueryParam;
import com.autel.cloud.pile.base.infrastructure.mapper.PileRightsMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbHonourAgreementMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.TbLenBindRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbHonourAgreementEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbLenBindRelationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbOrderPileEntity;
import com.autel.cloud.pile.base.vo.AgreementFunctionReqVo;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @description 订阅需求，拿到履约单后，定时任务获取licence，并且完成绑定，通知订单模块履约完成，发送mq桩绑定
 * @auther A23204
 * @datetime 2023/6/17 14:55
 */
@Slf4j
@Component
@RefreshScope
public class SubscribeGetLicenceJob {

    @Autowired
    private TbHonourAgreementMapper honourAgreementMapper;

    @Autowired
    private TbLenBindRelationMapper lenBindRelationMapper;

    @Autowired
    private PileRightsMapper pileRightsMapper;

    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private SubscribePlaceOrderService subscribePlaceOrderService;

    @Value("${subscribe.startTimingDays:180}")
    private Integer startTimeAfterNDays;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Value("${subscribe.startTimeAfter90Days:90}")
    private Integer startTimeAfter90Days;

    /**
     * 1705248000000: 北京时间 2024/1/15 00:00:00
     */
    @Value("${subscribe.boundaryMillisecond:1705248000000}")
    private Long boundaryMillisecond;


    /** 开始执行履约任务
     * @param param
     * @return
     */
    @XxlJob("honourAgreementStart")
    public ReturnT<String> honourAgreementStart(String param) {
        int orderLimit = 50;
        /*try {
            if (param != null && Integer.parseInt(param) > 0) {
                orderLimit = Integer.parseInt(param);
            }
        } catch (Exception e) {
            log.error("--->>> honourAgreementStart param config error, use default order limit 50.");
        }*/

        Long timeDiff = System.currentTimeMillis() - 30*60*1000L;

        QueryWrapper<TbHonourAgreementEntity> wrapper = Wrappers.query();
        wrapper.select("DISTINCT tenant_id, order_id").lambda().eq(TbHonourAgreementEntity::getAgreementStatus, 0).lt(TbHonourAgreementEntity::getCreateTime, timeDiff)
                .apply("IFNULL(remark,0) < 4").last("limit " + orderLimit);

        List<TbHonourAgreementEntity> tbHonourAgreementEntities = honourAgreementMapper.selectList(wrapper);
        if (CollectionUtils.isEmpty(tbHonourAgreementEntities)) {
            log.info("--->>> honourAgreementStart query order have to deal returns empty, there is no order have to deal.");
            return ReturnT.SUCCESS;
        }

        tbHonourAgreementEntities.forEach(tbHonourAgreementEntity -> {
            try {
                subscribePlaceOrderService.getAndBindLicence(tbHonourAgreementEntity);
            } catch (Exception e) {
                log.error("--->>> honourAgreementStart, deal orderId:{} exception, it will be done next time ", tbHonourAgreementEntity.getOrderId(), e);
                pileRightsMapper.agreementStatus(tbHonourAgreementEntity.getOrderId());
            }
        });

        return ReturnT.SUCCESS;

    }

    /** 维护licence表过期状态。
     * @param param
     * @return
     */
    @XxlJob("batchUpdateLicenceStatus")
    public ReturnT<String> batchUpdateLicenceStatus (String param) {
        // 批量处理过期、续期的状态。
        int batchSize = 200;

        // 先处理过期，并发送过期通知给ES
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbLenBindRelationEntity::getStatus, 1);
        //  这里使用启动充电控制点查询；
        List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.GUN_SEARCH);

        queryWrapper.in(TbLenBindRelationEntity::getServiceId,functionBenefitList);
        queryWrapper.lt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());
        queryWrapper.last("limit " + batchSize);

        List<TbLenBindRelationEntity> tbLenBindRelationEntities = lenBindRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
            log.info("--->>> batchUpdateLicenceStatus job there is no expired licence have to update. ");
        } else {

            List<String> pileSns = tbLenBindRelationEntities.stream().map(TbLenBindRelationEntity::getPileSn).collect(Collectors.toList());
            // 先要过滤当前是生效中的。
            LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper2 = Wrappers.lambdaQuery();
            queryWrapper2.in(TbLenBindRelationEntity::getStatus, 1,3);
            queryWrapper2.gt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());
            queryWrapper2.in(TbLenBindRelationEntity::getServiceId, functionBenefitList);
            queryWrapper2.in(TbLenBindRelationEntity::getPileSn, pileSns);
            List<TbLenBindRelationEntity> tbLenBindRelationEntitiesNotExpired = lenBindRelationMapper.selectList(queryWrapper2);
            List<String> haveToNoticeESList;
            if (!CollectionUtils.isEmpty(tbLenBindRelationEntitiesNotExpired)) {
                List<String> pileSnsNotExpired = tbLenBindRelationEntitiesNotExpired.stream().map(TbLenBindRelationEntity::getPileSn).collect(Collectors.toList());
                haveToNoticeESList = pileSns.stream().filter(item -> !pileSnsNotExpired.contains(item)).collect(Collectors.toList());
            } else {
                haveToNoticeESList = pileSns;
            }

            // 拿出过期, 并通知 ES
            log.info("--->>> haveToNoticeESList:{}", JSON.toJSONString(haveToNoticeESList));
            UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO = new UpdateEsSubscriptionStatusDTO();
            updateEsSubscriptionStatusDTO.setPileSnList(haveToNoticeESList);
            updateEsSubscriptionStatusDTO.setStatus(Boolean.FALSE);
            opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);

            // update status
            List<Long> pileIds = tbLenBindRelationEntities.stream().map(TbLenBindRelationEntity::getId).collect(Collectors.toList());
            LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper = Wrappers.lambdaUpdate();
            updateWrapper.in(TbLenBindRelationEntity::getId, pileIds);
            TbLenBindRelationEntity updateEntity = new TbLenBindRelationEntity();
            updateEntity.setStatus(2);
            updateEntity.setUpdateTime(System.currentTimeMillis());
            lenBindRelationMapper.update(updateEntity, updateWrapper);
        }

        // 处理其他权益过期，过期不需要发送ES.
        Long currentTimeStamp = System.currentTimeMillis();

        pileRightsMapper.updateExpiredOtherRights(currentTimeStamp, functionBenefitList);

        return ReturnT.SUCCESS;
    }

    /*
     * description: dealUnBindLicences  180尚未绑定的licence，设置生效时间、失效时间  1小时一次即可。
     * version: 1.0
     * date: 2023/7/27 17:11
     * author: A23204
     *
     * @param currentTimeStamp
     * @return void
     */
    @XxlJob("dealUnBindLicences")
    public ReturnT<String> dealUnBindLicences(String param) {
        // 一次只查询 200条。 因为超过180天仍未使用的很少。

        // 获取当天0点时间
      /*  Calendar today = Calendar.getInstance();
        today.set(Calendar.MILLISECOND,0);
        today.set(Calendar.SECOND,0);
        today.set(Calendar.MINUTE,0);
        today.set(Calendar.HOUR_OF_DAY,0);

        // 获取当天0点时间戳
        long currentZeroTimeStamp = today.getTimeInMillis();

        Calendar instance = Calendar.getInstance();

        instance.add(Calendar.DAY_OF_YEAR, -startTimeAfterNDays);

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(TbLenBindRelationEntity::getStatus, 0);
        queryWrapper.isNull(TbLenBindRelationEntity::getAvailableTime);
        queryWrapper.lt(TbLenBindRelationEntity::getCreateTime, instance.getTimeInMillis());
        queryWrapper.last("limit 20");*/

        // 改成从sql查询
        UnusedLicenceQueryParam unusedLicenceQueryParam = new UnusedLicenceQueryParam();
        unusedLicenceQueryParam.setCurrentTimeMilliSec(System.currentTimeMillis());
        unusedLicenceQueryParam.setExtend180DaysMilliSec(startTimeAfterNDays*24*3600*1000L);
        unusedLicenceQueryParam.setExtend90DaysMilliSec(startTimeAfter90Days*24*3600*1000L);
        unusedLicenceQueryParam.setBoundaryMilliSec(boundaryMillisecond);

        List<TbLenBindRelationEntity> tbLenBindRelationEntities = pileRightsMapper.selectUnBindLicence(unusedLicenceQueryParam);
        if (CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
            log.info("--->>> dealUnBindLicences, tbLenBindRelationEntities size is zero, end. ");
        }

        log.info("--->>>dealUnBindLicences, tbLenBindRelationEntities size is:{}", tbLenBindRelationEntities.size());

        if (!CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
            tbLenBindRelationEntities.forEach(tbLenBindRelationEntity -> {
                try {

                    // 获取当天0点时间
                    Calendar today = Calendar.getInstance();
                    today.set(Calendar.MILLISECOND,0);
                    today.set(Calendar.SECOND,0);
                    today.set(Calendar.MINUTE,0);
                    today.set(Calendar.HOUR_OF_DAY,0);

                    // 获取当天0点时间戳
                    long currentZeroTimeStamp = today.getTimeInMillis();

                    String timeUnit = tbLenBindRelationEntity.getTimeUnit();
                    int unit = timeUnit.toLowerCase().startsWith("year") ? Calendar.YEAR : timeUnit.toLowerCase().startsWith("month") ? Calendar.MARCH : Calendar.DAY_OF_MONTH;
                    today.add(unit, tbLenBindRelationEntity.getServiceTime());
                    // 判断是否有附赠时长
                    String bonusDurationValue = tbLenBindRelationEntity.getBonusDurationValue();
                    if (bonusDurationValue != null) {
                        String[] s = bonusDurationValue.split(" ");
                        int bonusDuration = Integer.parseInt(s[0]);
                        today.add(Calendar.MONTH, bonusDuration);
                    }
                    today.set(Calendar.MILLISECOND,999); //这是将当天的【毫秒】设置为999
                    today.set(Calendar.SECOND,59); //这是将当天的【秒】设置为59
                    today.set(Calendar.MINUTE,59); //这是将当天的【分】设置为59
                    today.set(Calendar.HOUR_OF_DAY,23); //这是将当天的【时】设置为23
                    TbLenBindRelationEntity updateEntity = new TbLenBindRelationEntity();
                    updateEntity.setAvailableTime(currentZeroTimeStamp);
                    updateEntity.setUnavailableTime(today.getTimeInMillis());
                    updateEntity.setId(tbLenBindRelationEntity.getId());
                    updateEntity.setUpdateTime(System.currentTimeMillis());

                    log.info("--->>> update id:{}, available:{}, unavailable:{}", updateEntity.getId(), updateEntity.getAvailableTime(), updateEntity.getUnavailableTime());
                    lenBindRelationMapper.updateById(updateEntity);

                } catch (Exception e) {
                    log.error("--->>>dealUnBindLicences, licence:{} has been over 180 days, but update error.", tbLenBindRelationEntity.getLicenceCode(), e);
                }
            });

        }

        // 处理未绑定过期
        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.eq(TbLenBindRelationEntity::getStatus, 0);
        updateWrapper.lt(TbLenBindRelationEntity::getUnavailableTime, System.currentTimeMillis());

        TbLenBindRelationEntity update = new TbLenBindRelationEntity();
        update.setStatus(2);
        update.setUpdateTime(System.currentTimeMillis());

        lenBindRelationMapper.update(update, updateWrapper);

        return ReturnT.SUCCESS;
    }

    /**
     * 定时同步ES中枪订阅状态
     */
    @XxlJob("syncPileSnSubscriptionStatus")
    public ReturnT<String> syncPileSnSubscriptionStatus(String param) {
        //查询ES中未订阅桩
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.mustNot(QueryBuilders.termsQuery("subscriptionCheck", true));

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(0, 100))
                .build();
        Iterable<OpLocationEvseElasticDTO> search =
                elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        Iterator<OpLocationEvseElasticDTO> it = search.iterator();
        Set<String> checkPileSnList = new HashSet<>();
        while (it.hasNext()) {
            OpLocationEvseElasticDTO next = it.next();
            checkPileSnList.add(next.getPileSn());
        }
        log.info("syncPileSnSubscriptionStatus.checkPileSnList: {}", checkPileSnList);
        Set<String> updatePileSnSet = new HashSet<>();
        if (checkPileSnList.size() > 0) {
            int pageSize = 300;
            int total = checkPileSnList.size();
            for (int pageNo = 1; pageNo <= ceilDiv(total, pageSize); pageNo++) {
                List<String> snList = checkPileSnList.stream().skip((pageNo - 1) * pageSize * 1l).limit(pageSize).collect(Collectors.toList());
                //查询桩的实际订阅状态
                BenefitFunctionReqDto agreementFunctionReqVo = new BenefitFunctionReqDto();
                agreementFunctionReqVo.setPileSn(Lists.newArrayList(snList));
                log.info("syncPileSnSubscriptionStatus,agreementFunctionReqVo:{}", JSON.toJSONString(agreementFunctionReqVo));
                // 修改接口 added @Date: 20231225
                Result<List<PileBenefitFunctionRespDto>> haveRightsPileList = subscribePileRightsService.getPileFunction(agreementFunctionReqVo);
                log.info("syncPileSnSubscriptionStatus，haveRightsPileList：{}", JSON.toJSONString(haveRightsPileList));

                if (CollUtil.isNotEmpty(haveRightsPileList.getData())) {
                    //List<String> functionBenefitList = subscribePileRightsService.getFunctionBenefitList(PileChargingRights.GUN_SEARCH);
                    for (PileBenefitFunctionRespDto agreementFunctionDetailDto : haveRightsPileList.getData()) {
                        if (agreementFunctionDetailDto.getUnavailableTime() != null && agreementFunctionDetailDto.getUnavailableTime() > System.currentTimeMillis()
                                &&  CollUtil.isNotEmpty(agreementFunctionDetailDto.getFunctionIdList()) && agreementFunctionDetailDto.getFunctionIdList().contains(PileChargingRights.GUN_SEARCH)) {
                            updatePileSnSet.add(agreementFunctionDetailDto.getPileSn());
                        }
                    }
                }
            }
            log.info("实际为订阅生效中的桩：{}", JSON.toJSONString(updatePileSnSet));
            if (updatePileSnSet.size() > 0) {
                //将实际订阅了的桩的状态进行更新
                UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO = new UpdateEsSubscriptionStatusDTO();
                updateEsSubscriptionStatusDTO.setPileSnList(Lists.newArrayList(updatePileSnSet));
                updateEsSubscriptionStatusDTO.setStatus(true);
                opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);
            }
        }

        return ReturnT.SUCCESS;
    }

    static int ceilDiv(int dividend, int divisor) {
        double quotient = (double) dividend / (double) divisor;
        return (int) Math.ceil(quotient);
    }
}
