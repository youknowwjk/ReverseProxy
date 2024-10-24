package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import com.alibaba.excel.EasyExcelFactory;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.support.ExcelTypeEnum;
import com.alibaba.excel.write.builder.ExcelWriterBuilder;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.style.column.SimpleColumnWidthStyleStrategy;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.common.util.ExcelResponseUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.exception.BusinessException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.device.dto.ChargePileDTO;
import com.autel.cloud.device.dto.ConnectorDTO;
import com.autel.cloud.device.dto.GunTypeAndNumVO;
import com.autel.cloud.device.dto.VerifyDTO;
import com.autel.cloud.device.enums.MgmtErrorCodeEnum;
import com.autel.cloud.device.feign.DeviceClient;
import com.autel.cloud.edge.EdgeFeignClient;
import com.autel.cloud.pile.base.ChargePointNoticeEvent;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.PileExcelHeadConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.convert.ChargePointMerchantRelationTypeMapper;
import com.autel.cloud.pile.base.domain.hanlder.CustomCellStyleHandler;
import com.autel.cloud.pile.base.domain.hanlder.EmptyCellWriteHandler;
import com.autel.cloud.pile.base.domain.listener.BatchPageReadListener;
import com.autel.cloud.pile.base.domain.model.*;
import com.autel.cloud.pile.base.domain.model.dto.DeviceInfoDTO;
import com.autel.cloud.pile.base.domain.model.dto.DeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.dto.SynchronizeDeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.vo.DeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.model.vo.SynchronizeDeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.repository.OpEvseBrandModelRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.*;
import com.autel.cloud.pile.base.domain.utils.*;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.chargepoint.GetDeviceTypeDTO;
import com.autel.cloud.pile.base.dto.chargepoint.GetPileInfoByPileDTO;
import com.autel.cloud.pile.base.dto.overcharging.OverchargingPileDTO;
import com.autel.cloud.pile.base.dto.overcharging.TerminalInfoDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileListDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPilePageDTO;
import com.autel.cloud.pile.base.dto.pos.GetDeviceGunNumberDTO;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.enums.chargepoint.DeviceTypeEnum;
import com.autel.cloud.pile.base.enums.chargepoint.OverchargingPileFlagEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic2;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElasticService;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO2;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.OpsMgmtClient;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.*;
import com.autel.cloud.pile.base.infrastructure.feign.dto.SyncPileInfoForPosDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.util.CustomSheetWriteHandler;
import com.autel.cloud.pile.base.infrastructure.util.ImportTemplateCellStyleHandler;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.infrastructure.util.TitleHandler;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;
import com.autel.cloud.pile.base.vo.chargepoint.DeviceBriefInfoVO;
import com.autel.cloud.pile.base.vo.chargepoint.GetPileInfoByPileVO;
import com.autel.cloud.pile.base.vo.chargepoint.TerminalInfoVO;
import com.autel.cloud.pile.base.vo.fleet.GetPileInfoForFleetVO;
import com.autel.cloud.pile.user.api.enums.PileUserEnum;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.google.common.collect.Lists;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.lucene.search.TotalHits;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.SearchHit;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StopWatch;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriUtils;

import javax.annotation.Resource;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLEncoder;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.TOPIC_EXCHANGE_PILE_BASE;

@Slf4j
@Service
public class ChargePointMerchantRelationServiceImpl implements ChargePointMerchantRelationService {
    private Random random = new Random();

    /**
     * License 过期时间提醒
     */
    @Value("${license.expire.notice.countDownDays:60}")
    private long countDownDays;

    @Resource
    private OpLocationEvseService opLocationEvseService;

    @Resource
    private PileHomeServiceAdapter pileHomeServiceAdapter;

    @Resource
    private RestHighLevelClient restHighLevelClient;

    @Resource
    private SmartBIServiceAdapter smartBIServiceAdapter;

    @Resource
    private Redis2Util redis2Util;

    @Resource
    private OpEvseTypeMapper opEvseTypeMapper;

    @Resource
    private OpEvseTypeService opEvseTypeService;

    @Resource
    private OpLocationPileEvseElastic opLocationPileEvseElastic;

    @Resource
    private OpLocationPileEvseElastic2 opLocationPileEvseElastic2;

    @Resource
    private ChargePointMerchantRelationMapper chargePointMerchantRelationMapper;

    @Resource
    private PileDeviceServiceAdapter pileDeviceServiceAdapter;

    @Resource
    private OpLocationRepository opLocationRepository;

    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;

    @Resource
    private OpEvseTypeServiceImpl opEvseTypeServiceImpl;

    @Resource
    private OpEvseBrandMapper opEvseBrandMapper;

    @Resource
    private DeviceClient deviceServiceFeign;

    @Resource
    private OpsMgmtClient opsMgmtClient;

    @Resource
    private RabbitTemplate rabbitTemplate;

    @Resource
    private DeviceClient deviceClient;

    @Resource
    private OpLocationPileEvseMapper opLocationPileEvseMapper;

    @Resource
    private OpLocationEvseMapper opLocationEvseMapper;

    @Resource
    private TbLenBindRelationMapper tbLenBindRelationMapper;

    @Resource(name = "pileBaseTaskExecutor")
    private Executor pileBaseTaskExecutor;

    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Resource
    private EmailSendingService emailSendingService;

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    @Value("${i18n.messageproperties}")
    private String langs;

    @Autowired
    private SubscribePileRightsService subscribePileRightsService;

    @Resource
    private ChargePointMerchantTerminalService chargePointMerchantTerminalService;

    @Autowired
    private ChargePointMerchantTerminalMapper chargePointMerchantTerminalMapper;

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private CommodityCenterServiceAdapter commodityCenterServiceAdapter;

    @Autowired
    private MessageSource messageSource;

    /**
     * （X=满足此次即将过期通知条件的充电桩数量；按钮跳转充电云-商城首页）
     * （X=满足此次已过期通知条件的充电桩数量；按钮跳转充电云-商城首页）
     */
    @Value("${mall.home: https://pile-platform-vue-enedev.auteltech.cn/mallManage}")
    private String url;

    @Autowired
    private OpEvseBrandModelRepository opEvseBrandModelRepository;

    public static final String HOST = "host";
    public static final String TERMINAL = "terminal";

    private static final Integer MYBATIS_PLUS_PAGE_MAX_LIMIT = 500;
    private static final String EMPTY_CELL_DEFAULT_VALUE = "--";

    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    private OpLocationPileEvseElasticService opLocationPileEvseElasticService;
    @Autowired
    private EdgeFeignClient edgeFeignClient;

    @Override
    public List<ChargePointVO> findBySNs(Set<String> snSet, Long merchantId) {
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(snSet)) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.in(ChargePointMerchantRelationEntity::getSn, snSet);
        queryWrapper.orderByDesc(ChargePointMerchantRelationEntity::getId);
        List<ChargePointMerchantRelationEntity> chargePointEntityList = chargePointMerchantRelationMapper.selectList(queryWrapper);
        return ChargePointMerchantRelationTypeMapper.INSTANCE.entityList2VO(chargePointEntityList);
    }

    @Override
    public List<ChargePointVO> findChargePointBySNs(Set<String> snSet, Long sellerId) {
        if (CollectionUtils.isEmpty(snSet)) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.in(ChargePointMerchantRelationEntity::getSn, snSet);
        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(chargePointMerchantRelationEntities)) {
            return Collections.emptyList();
        }
        Map<String, List<ChargePointMerchantRelationEntity>> collect = chargePointMerchantRelationEntities.stream().collect(Collectors.groupingBy(ChargePointMerchantRelationEntity::getSn));
        List<ChargePointVO> result = new ArrayList<>();
        for (Map.Entry<String, List<ChargePointMerchantRelationEntity>> entry : collect.entrySet()) {

            List<ChargePointMerchantRelationEntity> chargePointEntities = entry.getValue();
            ChargePointVO chargePointVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2ChargePointVO(chargePointEntities.get(0));
            for (ChargePointMerchantRelationEntity chargePointMerchantRelationEntity : chargePointEntities) {
                chargePointVO.setSellerId(sellerId);
                if (MerchantChargePointRelationEnum.OWNER.equals(MerchantChargePointRelationEnum.keyOf(chargePointMerchantRelationEntity.getRelation()))) {
                    if (chargePointMerchantRelationEntity.getMerchantId().equals(sellerId)) {
                        chargePointVO.setRelationship(RelationshipEnum.OWNER);
                    }
                    chargePointVO.setOwner(chargePointMerchantRelationEntity.getMerchantId());
                    chargePointVO.setOwnerName(pileUserServiceAdapter.findMerchantById(chargePointMerchantRelationEntity.getMerchantId()).getName());
                    chargePointVO.setOwnerBindTime(chargePointMerchantRelationEntity.getBindTime());
                }
                if (MerchantChargePointRelationEnum.MAINTENANCE.equals(MerchantChargePointRelationEnum.keyOf(chargePointMerchantRelationEntity.getRelation()))) {
                    if (chargePointMerchantRelationEntity.getMerchantId().equals(sellerId)) {
                        chargePointVO.setRelationship(RelationshipEnum.MAINTENANCE);
                    }
                    chargePointVO.setMaintenance(chargePointMerchantRelationEntity.getMerchantId());
                    chargePointVO.setMaintenanceName(pileUserServiceAdapter.findMerchantById(chargePointMerchantRelationEntity.getMerchantId()).getName());
                    chargePointVO.setMaintenanceBindTime(chargePointMerchantRelationEntity.getBindTime());
                }
            }
            result.add(chargePointVO);
        }
        return result;
    }

    @Override
    public ChargePointVO findChargePointBySN(String sn, Long sellerId) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, sn);
        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
        if (CollectionUtils.isEmpty(chargePointMerchantRelationEntities)) {
            return null;
        }
        ChargePointVO chargePointVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2ChargePointVO(chargePointMerchantRelationEntities.get(0));
        for (ChargePointMerchantRelationEntity chargePointMerchantRelationEntity : chargePointMerchantRelationEntities) {
            chargePointVO.setSellerId(sellerId);
            if (MerchantChargePointRelationEnum.OWNER.equals(MerchantChargePointRelationEnum.keyOf(chargePointMerchantRelationEntity.getRelation()))) {
                if (chargePointMerchantRelationEntity.getMerchantId().equals(sellerId)) {
                    chargePointVO.setRelationship(RelationshipEnum.OWNER);
                }
                chargePointVO.setOwner(chargePointMerchantRelationEntity.getMerchantId());
                chargePointVO.setOwnerName(pileUserServiceAdapter.findMerchantById(chargePointMerchantRelationEntity.getMerchantId()).getName());
                chargePointVO.setOwnerBindTime(chargePointMerchantRelationEntity.getBindTime());
            }
            if (MerchantChargePointRelationEnum.MAINTENANCE.equals(MerchantChargePointRelationEnum.keyOf(chargePointMerchantRelationEntity.getRelation()))) {
                if (chargePointMerchantRelationEntity.getMerchantId().equals(sellerId)) {
                    chargePointVO.setRelationship(RelationshipEnum.MAINTENANCE);
                }
                chargePointVO.setMaintenance(chargePointMerchantRelationEntity.getMerchantId());
                chargePointVO.setMaintenanceName(pileUserServiceAdapter.findMerchantById(chargePointMerchantRelationEntity.getMerchantId()).getName());
                chargePointVO.setMaintenanceBindTime(chargePointMerchantRelationEntity.getBindTime());
            }
        }
        return chargePointVO;
    }

    @Override
    public ChargePointVO findBySN(String pileSn, Long sellerId) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, pileSn).eq(ChargePointMerchantRelationEntity::getMerchantId, sellerId);
        return ChargePointMerchantRelationTypeMapper.INSTANCE.entity2ChargePointVO(chargePointMerchantRelationMapper.selectOne(queryWrapper));
    }

    @Override
    public ChargePointMerchantRelationEntity queryBySN(String sn, Long sellerId) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, sn).eq(ChargePointMerchantRelationEntity::getMerchantId, sellerId);
        return chargePointMerchantRelationMapper.selectOne(queryWrapper);
    }

    @Override
    public IPage<ChargePointVO> subscriptPageBySellerId(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId, String zoneId) {
        if (merchantId == null) {
            log.info("subscriptPageBySellerId, merchantId is null, get from token.");
            merchantId = LoginUserUtil.getSellerId();
        }

        StopWatch stopWatch = new StopWatch("统一桩管理列表");
        stopWatch.start("查询统一桩列表");

        chargePointQueryDTO.setKeyword(StringUtil.escapeChar(chargePointQueryDTO.getKeyword()));

        // 2.8版本优化 根据启动充电功能点来判断
        List<Long> locationIds = chargePointQueryDTO.getLocations();
        List<String> snByLocationIdList = getSnByLocationId(locationIds);

        Page<ChargePointMerchantRelationEntity> pageInfo = new Page<>(chargePointQueryDTO.getPage(), chargePointQueryDTO.getPageSize());
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        String keyword = chargePointQueryDTO.getKeyword();
        boolean condition = org.apache.commons.lang3.StringUtils.isNotBlank(keyword);
        List<String> matchFields = Collections.singletonList("ProductNamePdm");
        boolean productNamePdmCondition = matchFields.contains("ProductNamePdm") && condition;

        List<String> hostSnList = new ArrayList<>();
       /* if (Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus())) {
            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>()
                    .lambda().eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
                    .eq(ChargePointMerchantTerminalEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus());
            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
            if (CollectionUtil.isNotEmpty(terminalEntities)) {
                hostSnList = terminalEntities.stream().map(item -> item.getHostSn()).collect(Collectors.toList());
            }
        }*/
        if (condition) {
            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>()
                    .lambda().eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
                    .eq(Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()),ChargePointMerchantTerminalEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus())
                    .and(wrapper -> wrapper.like(ChargePointMerchantTerminalEntity::getTerminalSn, keyword).or().like(ChargePointMerchantTerminalEntity::getTerminalName, keyword));
            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
            if (CollectionUtil.isNotEmpty(terminalEntities)) {
                hostSnList = terminalEntities.stream().map(item -> item.getHostSn()).collect(Collectors.toList());
            }
        }

        List<String> finalHostSnListSnList = hostSnList;

        queryWrapper.and(condition || Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()) || CollectionUtil.isNotEmpty(finalHostSnListSnList),
                chargePointEntityLambdaQueryWrapper -> chargePointEntityLambdaQueryWrapper
                        .eq(Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()), ChargePointMerchantRelationEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus())
                        .or( condition || CollectionUtil.isNotEmpty(finalHostSnListSnList),
                        chargePointEntityLambdaQueryWrapper2 -> chargePointEntityLambdaQueryWrapper2.like(condition, ChargePointMerchantRelationEntity::getSn, keyword)
                                .or().like(condition, ChargePointMerchantRelationEntity::getName, keyword)
                                .or().like(condition && productNamePdmCondition, ChargePointMerchantRelationEntity::getProductNamePdm, keyword)
                                .or().in(CollectionUtil.isNotEmpty(finalHostSnListSnList), ChargePointMerchantRelationEntity::getSn, finalHostSnListSnList))
                 );
        queryWrapper.eq(StringUtils.isNotBlank(chargePointQueryDTO.getPowerType()), ChargePointMerchantRelationEntity::getPowerType, chargePointQueryDTO.getPowerType());
        queryWrapper.eq(Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()), ChargePointMerchantRelationEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus());
        queryWrapper.in(CollUtil.isNotEmpty(locationIds), ChargePointMerchantRelationEntity::getSn, snByLocationIdList);
        queryWrapper.orderByDesc(ChargePointMerchantRelationEntity::getBindTime).orderByDesc(ChargePointMerchantRelationEntity::getId);
        //Page<ChargePointMerchantRelationEntity> chargePointEntityPage = chargePointMerchantRelationMapper.selectPage(pageInfo, queryWrapper);

        chargePointQueryDTO.setMerchantId(merchantId);
        chargePointQueryDTO.setProductNamePdmCondition(productNamePdmCondition);
        chargePointQueryDTO.setHostSnList(hostSnList);
        chargePointQueryDTO.setSnByLocationIdList(snByLocationIdList);

        Page<ChargePointMerchantRelationEntity> chargePointEntityPage = chargePointMerchantRelationMapper.selectPageByCustomer(pageInfo, chargePointQueryDTO);

        stopWatch.stop();
        Page<ChargePointVO> chargePointVOPage = populateAttr(merchantId, chargePointEntityPage, null, chargePointQueryDTO.getSubscriptionStatus(), chargePointQueryDTO.getCommodityCode(), true);
        if (!ObjectUtils.isEmpty(chargePointVOPage) && !org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointVOPage.getRecords())) {
            List<String> snList = chargePointVOPage.getRecords().stream().map(ChargePointVO::getSn).collect(Collectors.toList());
            //加入终端sn
            chargePointVOPage.getRecords().forEach(item -> {
                if(CollectionUtil.isNotEmpty(item.getOverchargingTerminals())) {
                    item.getOverchargingTerminals().forEach(terminal -> snList.add(terminal.getTerminalSn()));
                }
            });
            //查询订阅商品名
            // 2.8版本优化
            LambdaQueryWrapper<TbLenBindRelationEntity> lenQueryWrapper = Wrappers.lambdaQuery();
            lenQueryWrapper.in(TbLenBindRelationEntity::getPileSn, snList);
            lenQueryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
            List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(lenQueryWrapper);

            for (ChargePointVO chargePointVO : chargePointVOPage.getRecords()) {
                List<CommodityBaseInfoVO> subscriptionCommodityList = new ArrayList<>();
                // 过滤sn
                List<TbLenBindRelationEntity> snCollect = tbLenBindRelationEntities.stream().filter(item -> chargePointVO.getSn().equalsIgnoreCase(item.getPileSn())).collect(Collectors.toList());
                Map<String, TbLenBindRelationEntity> maxUnavailableTimeByGoodsId = snCollect.stream().collect(Collectors.toMap(TbLenBindRelationEntity::getServiceId, Function.identity(), BinaryOperator.maxBy(Comparator.comparing(TbLenBindRelationEntity::getUnavailableTime))));

                handleGoodsDetail(maxUnavailableTimeByGoodsId, subscriptionCommodityList);

                if (!org.apache.commons.collections4.CollectionUtils.isEmpty(subscriptionCommodityList)) {
                    chargePointVO.setSubscriptionCommodityList(subscriptionCommodityList.stream().distinct().collect(Collectors.toList()));
                }

                //超冲终端添加订阅商品
                if (Objects.nonNull(chargePointVO.getOverchargingPileFlag())
                        && chargePointVO.getOverchargingPileFlag() == 1
                        && CollectionUtil.isNotEmpty(chargePointVO.getOverchargingTerminals())) {
                    for (OverchargingTerminal overchargingTerminal : chargePointVO.getOverchargingTerminals()) {
                        List<CommodityBaseInfoVO> subscriptionCommodityList2 = new ArrayList<>();

                        // 过滤sn
                        List<TbLenBindRelationEntity> snTerminalCollect = tbLenBindRelationEntities.stream().filter(item -> overchargingTerminal.getSn().equalsIgnoreCase(item.getPileSn())).collect(Collectors.toList());
                        // 分组
                        Map<String, TbLenBindRelationEntity> bindRelationEntityMap = snTerminalCollect.stream().collect(Collectors.toMap(TbLenBindRelationEntity::getServiceId, Function.identity(), BinaryOperator.maxBy(Comparator.comparing(TbLenBindRelationEntity::getUnavailableTime))));
                        handleGoodsDetail(bindRelationEntityMap, subscriptionCommodityList2);

                        overchargingTerminal.setSubscriptionCommodityList(subscriptionCommodityList2.stream().distinct().collect(Collectors.toList()));
                    }
                }
            }
        }
        log.info(stopWatch.prettyPrint());
        return chargePointVOPage;
    }

    /**
     * description: handleGoodsDetail 处理订阅套餐
     * version: 1.0
     * date: 2024/4/26 14:23
     * author: A23204
     *
     * @param maxUnavailableTimeByGoodsId
     * @param subscriptionCommodityList
     * @return void
     */
    private void handleGoodsDetail(Map<String, TbLenBindRelationEntity> maxUnavailableTimeByGoodsId, List<CommodityBaseInfoVO> subscriptionCommodityList) {
        AtomicBoolean pro = new AtomicBoolean(false);
        AtomicBoolean lite = new AtomicBoolean(false);
        AtomicBoolean cms = new AtomicBoolean(false);
        AtomicBoolean mp = new AtomicBoolean(false);

        maxUnavailableTimeByGoodsId.forEach((key, item) -> {
            if (PileChargingRights.SERVICE_ID_ALL.contains(key)) {
                CommodityBaseInfoVO vo = new CommodityBaseInfoVO();
                vo.setCommodityCode(item.getGoodsId());
                vo.setExpiredTime(item.getUnavailableTime());
                if (!Integer.valueOf(2).equals(item.getStatus())) {
                    vo.setStatus(System.currentTimeMillis() > Optional.ofNullable(item.getUnavailableTime()).orElse(0L) ? 0 : 1);
                } else {
                    vo.setStatus(0);
                    // 包含正常过期、直接删除桩的过期
                    if (System.currentTimeMillis() < Optional.ofNullable(item.getUnavailableTime()).orElse(0L)) {
                        // 直接删除桩的过期
                        vo.setExpiredTime(null);
                    }
                }

                if (key.endsWith("Pro")) {
                    pro.set(true);
                    vo.setCommodityName("Pro");
                } else if (key.endsWith("Lite")) {
                    lite.set(true);
                    vo.setCommodityName("Lite");
                } else if (key.endsWith("ads")) {
                    cms.set(true);
                    vo.setCommodityName("Advertising");
                } else if (key.endsWith("ops")) {
                    mp.set(true);
                    vo.setCommodityName("Maintenance");
                }
                subscriptionCommodityList.add(vo);
            }
        });

        // 补齐未订阅的
        if (!pro.get()) {
            CommodityBaseInfoVO vo = new CommodityBaseInfoVO();
            vo.setCommodityName("Pro");
            vo.setStatus(0);
            subscriptionCommodityList.add(vo);
        }
        if (!lite.get()) {
            CommodityBaseInfoVO vo = new CommodityBaseInfoVO();
            vo.setCommodityName("Lite");
            vo.setStatus(0);
            subscriptionCommodityList.add(vo);
        }
        if (!cms.get()) {
            CommodityBaseInfoVO vo = new CommodityBaseInfoVO();
            vo.setCommodityName("Advertising");
            vo.setStatus(0);
            subscriptionCommodityList.add(vo);
        }
        if (!mp.get()) {
            CommodityBaseInfoVO vo = new CommodityBaseInfoVO();
            vo.setCommodityName("Maintenance");
            vo.setStatus(0);
            subscriptionCommodityList.add(vo);
        }
    }

    /**
     * description: getSnByLocationId 根据场站id 查询 sn
     * version: 1.0
     * date: 2024/4/23 14:29
     * author: A23204
     *
     * @param locationId
     * @return java.util.List<java.lang.String>
     */
    private List<String> getSnByLocationId(List<Long> locationId) {
        if (CollUtil.isEmpty(locationId)) {
            return Collections.emptyList();
        }
        //  queryPileListByOperationId
        return opLocationEvseService.queryPileListByLocations(locationId);
    }

    private Page<ChargePointVO> populateAttr(Long merchantId, Page<ChargePointMerchantRelationEntity> chargePointEntityPage, String commodityCategory, Integer subStatus, String commodityCode, boolean sorted) {
        Page<ChargePointVO> chargePointVOPage = ChargePointMerchantRelationTypeMapper.INSTANCE.pageEntityList2PageVO(chargePointEntityPage);
        if (!CollectionUtils.isEmpty(chargePointEntityPage.getRecords())) {
            Map<String, ChargePointMerchantRelationEntity> chargePointEntityMap = chargePointEntityPage.getRecords().stream().collect(Collectors.toMap(ChargePointMerchantRelationEntity::getSn, Function.identity(), (oldValue, newValue) -> oldValue));
            List<String> sns = chargePointEntityPage.getRecords().stream().map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
            List<OpLocationPileEvseElasticDTO> locationPiles = opLocationPileEvseElastic.findByPileSnIn(sns);
            Map<String, String> pileLocationMap = new HashMap<>();
            if (Objects.nonNull(locationPiles) && !CollectionUtils.isEmpty(locationPiles)) {
                for (OpLocationPileEvseElasticDTO pileEvseElasticDTO : locationPiles) {
                    if (pileLocationMap.containsKey(pileEvseElasticDTO.getPileSn())) {
                        log.warn(" Duplicate key {}", pileEvseElasticDTO.getPileSn());
                    }
                    if (merchantId.equals(pileEvseElasticDTO.getOperatorId())) { // AB商家同时添加了同一个桩到资产，A添加该桩到场站，在B商家的账号能看到A商家的场站名称
                        pileLocationMap.put(pileEvseElasticDTO.getPileSn(), pileEvseElasticDTO.getLocationName());
                    }
                }
            }

            List<String> terminalSnList = new ArrayList<>();
            Map<String, ChargePointVO> terminalSnAndHostChargePointVOMap = new HashMap<>();
            for (ChargePointVO chargePointVO : chargePointVOPage.getRecords()) {
                if (chargePointVO.getConnectorsString() != null) {
                    List<Connector> connectors = JSON.parseArray(chargePointVO.getConnectorsString(), Connector.class);
                    chargePointVO.setCountGun(connectors.size());
                }
                //chargePointVO.setLocationName(pileLocationMap.get(chargePointVO.getSn()));
                if (!sorted) {
                    chargePointVO.setLocationName(pileLocationMap.get(chargePointVO.getSn()));
                }
                ImminentExpireChargePointDTO imminentExpireChargePointDTO = updateSubStatus(chargePointEntityMap.get(chargePointVO.getSn()), merchantId, commodityCode);
                chargePointVO.setSubscriptionStatus(imminentExpireChargePointDTO.getSubStatus().getStatus());
                chargePointVO.setExpireDate(imminentExpireChargePointDTO.getExpireDate());
                chargePointVO.setExpireTimestamp(imminentExpireChargePointDTO.getExpireEpochMilli());



                if (Objects.nonNull(chargePointVO.getOverchargingPileFlag()) && chargePointVO.getOverchargingPileFlag() == 1) {
                    log.info("populateAttr sn: {},  OverchargingPileFlag: {}, commodityCategory: {}", chargePointVO.getSn(), chargePointVO.getOverchargingPileFlag(), commodityCategory);
                    //运维商品主机可以选,运营、广告商品不能选主机
                    if (CommodityCategoryEnum.MAINTENANCE.getCode().equals(commodityCategory)) {
                        chargePointVO.setDisable(0);
                    } else {
                        chargePointVO.setDisable(1);
                    }

                    //查询主机下的终端集合
                    LambdaQueryWrapper<ChargePointMerchantTerminalEntity> queryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>()
                            .lambda()
                            .eq(merchantId != null, ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
                            .eq(ChargePointMerchantTerminalEntity::getHostSn, chargePointVO.getSn())
                            .eq(Objects.nonNull(subStatus), ChargePointMerchantTerminalEntity::getSubStatus, subStatus);
                    List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(queryWrapper);
                    //所有商品都可以选择终端
                    if (CollectionUtil.isNotEmpty(terminalEntities)) {
                        terminalEntities.forEach(val -> {
                            terminalSnList.add(val.getTerminalSn());
                            terminalSnAndHostChargePointVOMap.put(val.getTerminalSn(), chargePointVO);
                        });

                        List<OverchargingTerminal> overchargingTerminals = terminalEntities.stream().map(item -> {
                            OverchargingTerminal overchargingTerminal = new OverchargingTerminal(item.getTerminalSn(),
                                    item.getTerminalPin(), item.getTerminalName(), item.getConnectorsList());
                            overchargingTerminal.setId(item.getId());
                            overchargingTerminal.setDisable(0);
                            overchargingTerminal.setLocationName(chargePointVO.getLocationName());
                            overchargingTerminal.setPowerType(chargePointVO.getPowerType());

                            ImminentExpireChargePointDTO overchargingImminentExpireChargePointDTO = getAndUpdateTerminalInfo(item, merchantId, commodityCode);
                            overchargingTerminal.setSubscriptionStatus(overchargingImminentExpireChargePointDTO.getSubStatus().getStatus());
                            overchargingTerminal.setExpireDate(overchargingImminentExpireChargePointDTO.getExpireDate());
                            overchargingTerminal.setExpireTimestamp(overchargingImminentExpireChargePointDTO.getExpireEpochMilli());
                            return overchargingTerminal;
                        }).collect(Collectors.toList());
                        chargePointVO.setOverchargingTerminals(overchargingTerminals);
                    }
                } else {
                    chargePointVO.setDisable(0);
                }
            }

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalSnList)) {
                Map<String, com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO> snAndChargePileDTOMap = pileDeviceServiceAdapter.getSnAndChargePileDTOMap(terminalSnList);
                chargePointVOPage.getRecords().forEach(val -> {
                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(val.getOverchargingTerminals())) {
                        val.getOverchargingTerminals().forEach(item -> {
                            item.setName(item.getTerminalName());
                            item.setSn(item.getTerminalSn());
                            if (terminalSnAndHostChargePointVOMap.get(item.getTerminalSn()) != null) {
                                ChargePointVO chargePointVO = terminalSnAndHostChargePointVOMap.get(item.getTerminalSn());
                                item.setSellerId(chargePointVO.getSellerId());
                                item.setBrandId(chargePointVO.getBrandId());
                                item.setBrandName(chargePointVO.getBrandName());
                                item.setZoneId(chargePointVO.getZoneId());
                                item.setThirdPart(!Long.valueOf(BrandEnum.AUTEL.getCode()).equals(chargePointVO.getBrandId()));
                                item.setOwner(chargePointVO.getOwner());
                                item.setOwnerName(chargePointVO.getOwnerName());
                            }
                            if (snAndChargePileDTOMap.get(item.getTerminalSn()) != null) {
                                com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileDTO = snAndChargePileDTOMap.get(item.getTerminalSn());
                                item.setScreen1Size(chargePileDTO.getScreen1Size());
                                item.setScreen1Pixel(chargePileDTO.getScreen1Pixel());
                                item.setPartProduct(chargePileDTO.getPartProduct());
                            }
                        });
                    }
                });
            }
        }
        return chargePointVOPage;
    }

    private Page<ChargePointVO> populateAttr(Long merchantId, Page<ChargePointMerchantRelationEntity> chargePointEntityPage) {
        return populateAttr(merchantId, chargePointEntityPage, null, null, null, false);
    }
    @Override
    public SubscriptionStatusCountVO subscriptionStatusCount(Long merchantId) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryChargePointEntityWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryChargePointEntityWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(queryChargePointEntityWrapper);
        int valid = 0, unSubscription = 0, expired = 0, expiringSoon = 0;
        for (ChargePointMerchantRelationEntity entity : chargePointEntities) {
            if (SubStatus.INACTIVITY.getStatus().equals(entity.getSubStatus())) {
                unSubscription++; //0-未订阅，1-过期，2-即将过期，3-有效
            }
            if (SubStatus.INVALIDITY.getStatus().equals(entity.getSubStatus())) {
                expired++;
            }
            if (SubStatus.SOON_TO_EXPIRE.getStatus().equals(entity.getSubStatus())) {
                expiringSoon++;
            }
            if (SubStatus.EFFECTIVE.getStatus().equals(entity.getSubStatus())) {
                valid++;
            }
        }

        LambdaQueryWrapper<ChargePointMerchantTerminalEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantTerminalEntity.class);
        queryWrapper.eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId);
        List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(queryWrapper);
        for (ChargePointMerchantTerminalEntity entity : terminalEntities) {
            if (SubStatus.INACTIVITY.getStatus().equals(entity.getSubStatus())) {
                unSubscription++;
            }
            if (SubStatus.INVALIDITY.getStatus().equals(entity.getSubStatus())) {
                expired++;
            }
            if (SubStatus.SOON_TO_EXPIRE.getStatus().equals(entity.getSubStatus())) {
                expiringSoon++;
            }
            if (SubStatus.EFFECTIVE.getStatus().equals(entity.getSubStatus())) {
                valid++;
            }
        }


//        List<String> overchargingPileSn = chargePointEntities.stream()
//                .filter(item -> Objects.nonNull(item.getOverchargingPileFlag()) && item.getOverchargingPileFlag() == 1)
//                .map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
//
//        if (CollectionUtil.isNotEmpty(overchargingPileSn)) {
//            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> queryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda()
//                    .eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
//                    .in(ChargePointMerchantTerminalEntity::getHostSn, overchargingPileSn);
//            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(queryWrapper);
//
//            if (CollectionUtil.isNotEmpty(terminalEntities)) {
//                List<String> terminalSnList = terminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());
//
//                LambdaQueryWrapper<TbLenBindRelationEntity> lenQueryWrapper = new QueryWrapper<TbLenBindRelationEntity>().lambda()
//                        .eq(TbLenBindRelationEntity::getTenantId, merchantId)
//                        .in(TbLenBindRelationEntity::getPileSn, terminalSnList)
//                        .gt(TbLenBindRelationEntity::getUnavailableTime, 0);
//                List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(lenQueryWrapper);
//                List<TbLenBindRelationEntity> tbLenBindRelationEntities2 = tbLenBindRelationEntities.stream()
//                        .collect(Collectors.toMap(TbLenBindRelationEntity::getPileSn, entity -> entity,
//                                (existing, replacement) -> existing.getUnavailableTime() > replacement.getUnavailableTime() ? existing : replacement))
//                        .values()
//                        .stream()
//                        .collect(Collectors.toList());
//                for (TbLenBindRelationEntity entity : tbLenBindRelationEntities2) {
//                    long effectiveDays = (BigDecimal.valueOf(entity.getUnavailableTime() - System.currentTimeMillis())).divide(BigDecimal.valueOf(ONE_DAY_MILLIS), RoundingMode.UP).longValue();
//                    if (effectiveDays <= 0) {
//                        expired++;
//                    } else if (effectiveDays <= countDownDays) {
//                        expiringSoon++;
//                    } else {
//                        valid++;
//                    }
//                }
//                int inactivityCount = terminalSnList.size() - tbLenBindRelationEntities.size();
//                unSubscription = unSubscription + inactivityCount;
//            }
//        }


        SubscriptionStatusCountVO subscriptionStatusCountVO = new SubscriptionStatusCountVO();
        subscriptionStatusCountVO.setExpired(expired);
        subscriptionStatusCountVO.setUnSubscription(unSubscription);
        subscriptionStatusCountVO.setExpiringSoon(expiringSoon);
        subscriptionStatusCountVO.setValid(valid);
        subscriptionStatusCountVO.setTotal(chargePointEntities.size() + terminalEntities.size());
        return subscriptionStatusCountVO;
    }

    @Override
    public List<ChargePointLicenseVO> findLastExpireTimeChargePointLicense(List<String> chargePointSNList, List<String> serviceId, Long merchantId) {
        StopWatch stopWatch = new StopWatch("查询桩的权益的失效期");
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointSNList)) {
            return Collections.emptyList();
        }
        stopWatch.start("查询桩数据");
        Map<String, ChargePointLicenseVO> map = new HashMap<>();
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryChargePointEntityWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryChargePointEntityWrapper.in(ChargePointMerchantRelationEntity::getSn, chargePointSNList);
        queryChargePointEntityWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(queryChargePointEntityWrapper);
        stopWatch.stop();
        stopWatch.start("构造的权益的查询条件");
        Map<String, ChargePointMerchantRelationEntity> chargePointEntityMap = chargePointEntities.stream().collect(Collectors.toMap(ChargePointMerchantRelationEntity::getSn, Function.identity()));
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
        queryWrapper.in(TbLenBindRelationEntity::getPileSn, chargePointSNList);
        // note: serviceId 当前接口都是通过biz来调用的，实际是通过 桩详情功能点 触发。
        queryWrapper.in(CollectionUtil.isNotEmpty(serviceId), TbLenBindRelationEntity::getServiceId, serviceId);
        queryWrapper.orderByAsc(TbLenBindRelationEntity::getUnavailableTime);
        stopWatch.stop();
        stopWatch.start("查询所有关联桩的License权益的失效期");
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        stopWatch.stop();
        stopWatch.start("查询桩的最后License权益的失效期");
        List<TbLenBindRelationEntity> maxUnavailableTimeBindRelationList = new ArrayList<>();
        Map<String, List<TbLenBindRelationEntity>> collect = tbLenBindRelationEntities.stream().collect(Collectors.groupingBy(TbLenBindRelationEntity::getPileSn));
        for (List<TbLenBindRelationEntity> list : collect.values()) {
            list.stream().max(Comparator.comparing(TbLenBindRelationEntity::getUnavailableTime)).ifPresent(maxUnavailableTimeBindRelationList::add);
        }
        stopWatch.stop();
        stopWatch.start("如有变化更新统一桩的订阅失效期");
        for (TbLenBindRelationEntity entity : maxUnavailableTimeBindRelationList) {
            ChargePointLicenseVO chargePointLicenseVO = new ChargePointLicenseVO();
            chargePointLicenseVO.setServiceId(entity.getServiceId());
            chargePointLicenseVO.setLicense(entity.getLicenceCode());
            chargePointLicenseVO.setSn(entity.getPileSn());
            String zoneId = ZoneId.systemDefault().getId();
            if (chargePointEntityMap.containsKey(entity.getPileSn())) {
                zoneId = Optional.ofNullable(chargePointEntityMap.get(entity.getPileSn()).getZoneId()).orElse(ZoneId.systemDefault().getId());
            }
            chargePointLicenseVO.setZoneId(zoneId);
            chargePointLicenseVO.setExpireDateTime(LocalDateTime.ofInstant(Instant.ofEpochMilli(entity.getUnavailableTime()), ZoneId.of(chargePointLicenseVO.getZoneId())));
            chargePointLicenseVO.setEffectiveDateTime(LocalDateTime.ofInstant(Instant.ofEpochMilli(entity.getAvailableTime()), ZoneId.of(chargePointLicenseVO.getZoneId())));
            chargePointLicenseVO.setExpireTimestamp(entity.getUnavailableTime());
            chargePointLicenseVO.setEffectiveTimestamp(entity.getAvailableTime());
            chargePointLicenseVO.setExpireDate(LocalDateTime.ofInstant(Instant.ofEpochMilli(entity.getUnavailableTime()), ZoneId.systemDefault()).format(dateFormatter));
            chargePointLicenseVO.setEffectiveDays((BigDecimal.valueOf(chargePointLicenseVO.getExpireTimestamp() - System.currentTimeMillis())).divide(BigDecimal.valueOf(ONE_DAY_MILLIS), RoundingMode.DOWN).longValue());
            chargePointLicenseVO.setSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
            if (chargePointLicenseVO.getEffectiveDays() <= countDownDays && chargePointLicenseVO.getEffectiveDays() > 0) {
                chargePointLicenseVO.setSubscriptionStatus(SubStatus.SOON_TO_EXPIRE.getStatus());
                // 这里不要更新 统一桩 里面的状态 因为这里可以传serviceId 而桩管理里面 是不分 serviecId 的
                chargePointLicenseVO.setDisplay(true);
            }
            if (chargePointLicenseVO.getEffectiveDays() <= 0) {
                chargePointLicenseVO.setEffectiveDays(0L);
                chargePointLicenseVO.setSubscriptionStatus(SubStatus.INVALIDITY.getStatus());
                // 这里不要更新 统一桩 里面的状态 因为这里可以传serviceId 而桩管理里面 是不分 serviecId 的
            }
            if (SubStatus.EFFECTIVE.getStatus().equals(chargePointLicenseVO.getSubscriptionStatus())) {
                chargePointLicenseVO.setSubscriptionStatus(SubStatus.EFFECTIVE.getStatus());
                // 这里不要更新 统一桩 里面的状态 因为这里可以传serviceId 而桩管理里面 是不分 serviecId 的
            }
            map.put(entity.getPileSn(), chargePointLicenseVO);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return new ArrayList<>(map.values());
    }

    @Override
    public ChargePointMerchantRelationEntity updatePile(PileUpdateDTO pileUpdateDTO, Long merchantId) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, pileUpdateDTO.getSn());

        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
        chargePointMerchantRelationEntity.setName(pileUpdateDTO.getName());
        if (!chargePointMerchantRelationEntity.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
            if (Objects.nonNull(pileUpdateDTO.getPowerType())) {
                chargePointMerchantRelationEntity.setPowerType(pileUpdateDTO.getPowerType());
            }
            if (Objects.nonNull(pileUpdateDTO.getRatedPower())) {
                chargePointMerchantRelationEntity.setRatedPower(pileUpdateDTO.getRatedPower().doubleValue());
            }
        }
        chargePointMerchantRelationEntity.setUpdateTime(System.currentTimeMillis());
        chargePointMerchantRelationMapper.updateById(chargePointMerchantRelationEntity);
        return chargePointMerchantRelationEntity;
    }


    @Override
    public void scanImminentExpireChargePoint(Long... offsetDays) {
        log.info("scanImminentExpireChargePoint offsetDays ={}", JSON.toJSONString(offsetDays));
        LambdaQueryWrapper<TbLenBindRelationEntity> queryLicenseWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryLicenseWrapper.select(TbLenBindRelationEntity::getTenantId);
        queryLicenseWrapper.isNotNull(TbLenBindRelationEntity::getPileSn);
        queryLicenseWrapper.isNotNull(TbLenBindRelationEntity::getTenantId);
        queryLicenseWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, 0);
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryLicenseWrapper);
        Set<Long> merchantIds = tbLenBindRelationEntities.stream().mapToLong((item -> Long.parseLong(item.getTenantId()))).boxed().collect(Collectors.toSet());
        List<Long> longList = Arrays.asList(offsetDays);
        for (Long merchantId : merchantIds) {
            try {
                doProcess(longList, merchantId);
            } catch (Exception e) {
                log.error("merchant id failed " + merchantId, e);
                XxlJobLogger.log("merchant id failed {}", merchantId);
            }
        }
    }


    private void doProcess(List<Long> longList, Long merchantId) {
        AtomicBoolean emailNoticeImminentExpire = new AtomicBoolean(false);
        AtomicBoolean emailNoticeExpired = new AtomicBoolean(false);
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        Page<ChargePointMerchantRelationEntity> page = new Page<>(1, 30);
        List<ImminentExpireChargePointDTO> imminentExpire = new ArrayList<>(); // 即将失效
        List<ImminentExpireChargePointDTO> expired = new ArrayList<>();   // 当天失效的
        AtomicInteger atomicInteger = new AtomicInteger(0);
        XxlJobLogger.log("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ 开始执行 商家ID={}  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓", merchantId);
        do {
            page = chargePointMerchantRelationMapper.selectPage(page, queryWrapper);
            if (Objects.nonNull(page)) {
                List<ChargePointMerchantRelationEntity> records = page.getRecords();
                List<String> sns = page.getRecords().stream().map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
                List<OpLocationPileEvseElasticDTO> locationPiles = opLocationPileEvseElastic.findByPileSnIn(sns);
                Map<String, String> pileLocationMap = new HashMap<>();
                if (Objects.nonNull(locationPiles) && !org.springframework.util.CollectionUtils.isEmpty(locationPiles)) {
                    for (OpLocationPileEvseElasticDTO pileEvseElasticDTO : locationPiles) {
                        if (pileLocationMap.containsKey(pileEvseElasticDTO.getPileSn())) {
                            log.warn(" Duplicate key {}", pileEvseElasticDTO.getPileSn());
                        }
                        if (merchantId.equals(pileEvseElasticDTO.getOperatorId())) { // AB商家同时添加了同一个桩到资产，A添加该桩到场站，在B商家的账号能看到A商家的场站名称
                            pileLocationMap.put(pileEvseElasticDTO.getPileSn(), pileEvseElasticDTO.getLocationName());
                        }
                    }
                }
                for (ChargePointMerchantRelationEntity chargePointEntity : records) {
                    try {
                        atomicInteger.incrementAndGet();
                        ImminentExpireChargePointDTO chargePointDTO = updateSubStatus(chargePointEntity, merchantId, null);
                        chargePointDTO.setLocationName(Optional.ofNullable(pileLocationMap.get(chargePointDTO.getSn())).orElse("--"));
                        chargePointDTO.setGoodsName(Optional.ofNullable(chargePointDTO.getGoodsName()).orElse("--"));
                        chargePointDTO.setName(Optional.ofNullable(chargePointDTO.getName()).orElse("--"));
                        emailNoticeImminentExpire.compareAndSet(false, longList.contains(chargePointDTO.getRemainDays()));
                        emailNoticeExpired.compareAndSet(false, Objects.nonNull(chargePointDTO.getRemainDays()) && (chargePointDTO.getRemainDays().equals(0L) || chargePointDTO.getRemainDays().equals(-1L)));
                        if (SubStatus.SOON_TO_EXPIRE.equals(chargePointDTO.getSubStatus())) {
                            imminentExpire.add(chargePointDTO);
                            XxlJobLogger.log("即将失效 {}", JSON.toJSONString(chargePointDTO));
                        } else if (SubStatus.INVALIDITY.equals(chargePointDTO.getSubStatus())) {
                            expired.add(chargePointDTO);
                            XxlJobLogger.log("已经失效 {}", JSON.toJSONString(chargePointDTO));
                        } else {
                            XxlJobLogger.log("其他状态 {}", JSON.toJSONString(chargePointDTO));
                        }
                    } catch (Exception e) {
                        log.error("computeSubStatusByLastExpireLicense " + chargePointEntity.getSn(), e);
                        XxlJobLogger.log("computeSubStatusByLastExpireLicense failed {}", chargePointEntity.getSn());
                    }
                }
                page.setCurrent(page.getCurrent() + 1);
            }
        } while (Objects.nonNull(page) && page.getCurrent() <= page.getPages());

        SellerDetailVO merchant = pileUserServiceAdapter.getMerchantByIdSupportFallback(merchantId);
        String email = merchant.getEmail();
        Locale locale = CommonUtil.language(merchant.getCountryAbbreviation(), langs);
        String language = locale.toLanguageTag();
        //  发即将过期邮件提醒
        if (emailNoticeImminentExpire.get() && !imminentExpire.isEmpty()) {
            imminentExpire.sort(Comparator.comparing(ImminentExpireChargePointDTO::getRemainDays));
            Boolean sendEmailResult1 = emailSendingService.expirationReminderEmail(imminentExpire, url, email, locale);
            XxlJobLogger.log("即将失效 merchant {} {} send email language={} {}", merchantId, merchant.getEmail(), language, sendEmailResult1);
        }

        //  发今天失效的邮件提醒
        if (emailNoticeExpired.get() && !expired.isEmpty()) {
            Boolean sendEmailResult2 = emailSendingService.expirationReminderEmail(expired, url, email, locale);
            XxlJobLogger.log("今天失效 merchant {} {} send email language={} {}", merchantId, merchant.getEmail(), language, sendEmailResult2);
        }
        XxlJobLogger.log("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑结束 商家ID={}  总计: {}  ↑↑↑↑↑↑↑↑↑↑↑↑↑↑\n", merchantId, atomicInteger.get());
    }

    @Override
    public void validateSN(String sn, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.validateSN sn : {} and merchantId : {}",
                JSON.toJSONString(sn),
                JSON.toJSONString(merchantId));

        if (org.apache.commons.lang3.StringUtils.isBlank(sn)) {
            throw new MessageCodeException(PileBaseEnum.THE_DEVICE_SN_OR_PIN_IS_INCORRECT);
        }

        if (merchantId == null) {
            throw new MessageCodeException(PileUserEnum.MERCHANT_NOT_EXIST);
        }

        GetDeviceTypeDTO getDeviceTypeDTO = new GetDeviceTypeDTO();
        getDeviceTypeDTO.setBrandId(Long.valueOf(BrandEnum.AUTEL.getCode()));
        getDeviceTypeDTO.setSn(sn);
        Integer deviceType = this.getDeviceType(getDeviceTypeDTO);
        if (!DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceType)) {
            LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, sn);
            List<ChargePointMerchantRelationEntity> relationEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
            if (!CollectionUtils.isEmpty(relationEntities)) {
                List<Integer> existRelations = new ArrayList<>();
                for (ChargePointMerchantRelationEntity relationEntity : relationEntities) {
                    if (merchantId.equals(relationEntity.getMerchantId())) {

                        log.warn("merchantId={} already bound sn: {}", merchantId, sn);

                        throw new MessageCodeException(PileBaseEnum.CHARGE_POINT_BOUND_BY_MYSELF);
                    }
                    log.warn("merchantId={} already bound sn: {}  remove Relation {}", merchantId, sn, relationEntity.getRelation());
                    existRelations.add(relationEntity.getRelation());
                }
                List<MerchantChargePointRelationEnum> remainingRelations = MerchantChargePointRelationEnum.remainingRelations(existRelations);
                if (CollectionUtils.isEmpty(remainingRelations)) {
                    log.warn("merchantId={}  sn: {} already bound by other merchants: {}", merchantId, sn, JSON.toJSONString(relationEntities));
                    throw new MessageCodeException(PileBaseEnum.SN_BOUND_BY_OTHER_MERCHANT);
                }
                log.info("current merchant try bind {}", remainingRelations.get(0));
            }
        } else {
            List<String> terminalSnList = new ArrayList<>();
            terminalSnList.add(sn);
            List<ChargePointMerchantTerminalEntity> terminalEntitys = chargePointMerchantTerminalService.getTerminalList(terminalSnList, null);
            if (ObjectUtils.isNotEmpty(terminalEntitys)
                    && (terminalEntitys.size() >= 2)) {
                throw new MessageCodeException(PileBaseEnum.HAS_BEEN_ADDED_BY_OTHER_MERCHANTS, new Object[]{sn});
            }
        }
    }

    @Override
    public AutelChargePointVO getAutelChargePoint(String sn, String pin) {
        Boolean result = validatePin(sn, pin);
        if (Boolean.FALSE.equals(result)) {
            throw new BusinessException(MgmtErrorCodeEnum.PIN_SN_NOT_MAPPING);
        }
        Result<ChargePileDTO> pileResult = deviceClient.query(sn);
        log.info("pileResult:{}", JSON.toJSONString(pileResult));
        if (pileResult == null) {
            return null;
        }
        ChargePileDTO chargePileDTO = pileResult.getData();
        if (chargePileDTO == null) {
            return null;
        }
        AutelChargePointVO autelPileDTO = new AutelChargePointVO();
        autelPileDTO.setModel(chargePileDTO.getProductModel());
        autelPileDTO.setCategory(chargePileDTO.getCategory());
        if (Objects.isNull(autelPileDTO.getCategory())) {
            if (sn.startsWith("D")) {
                autelPileDTO.setCategory(CategoryEnum.DC.getCode());
            } else {
                autelPileDTO.setCategory(CategoryEnum.AC.getCode());
            }
        }
        autelPileDTO.setRatedPower(chargePileDTO.getOutputPower());
        autelPileDTO.setPhase(chargePileDTO.getPhase());
        autelPileDTO.setVendor(chargePileDTO.getManufacturer());
        autelPileDTO.setProductNamePdm(chargePileDTO.getProductNamePdm());
        autelPileDTO.setPartProduct(chargePileDTO.getPartProduct());

        // 填充枪类型和数量的信息
        if (ObjectUtils.isNotEmpty(chargePileDTO.getConnectorNum())) {
            int connectorNum = chargePileDTO.getConnectorNum();
            int connectorType;

            if (ObjectUtils.isNotEmpty(chargePileDTO.getConnectorType())) {
                connectorType = chargePileDTO.getConnectorType();
            } else {
                log.info("Device数据库中无枪类型数据,赋默认值: 1");
                connectorType = 1;
            }
            autelPileDTO.setConnectorNum(connectorNum);
            autelPileDTO.setConnectorType(connectorType);
        } else {
            autelPileDTO.setConnectorNum(1);
            autelPileDTO.setConnectorType(1);
        }

        List<ConnectorDTO> connectorList = getConnectorDTOS(sn);
        autelPileDTO.setConnectorList(connectorList);

        return autelPileDTO;
    }

    @Override
    public SelectAutelDeviceInfoForOpsMgmtVO selectAutelDeviceInfoForOpsMgmt(String sn) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.selectAutelDeviceInfoForOpsMgmt sn : {}",
                JSON.toJSONString(sn));

        Result<ChargePileDTO> queryResult = deviceClient.query(sn);

        log.info("===>>> ChargePointMerchantRelationServiceImpl.selectAutelDeviceInfoForOpsMgmt queryResult : {}",
                JSON.toJSONString(queryResult));

        if (queryResult == null
                || queryResult.getData() == null) {
            return null;
        }

        ChargePileDTO chargePileDTO = queryResult.getData();
        SelectAutelDeviceInfoForOpsMgmtVO selectAutelDeviceInfoForOpsMgmtVO = new SelectAutelDeviceInfoForOpsMgmtVO();
        selectAutelDeviceInfoForOpsMgmtVO.setBrandName(chargePileDTO.getBrandName());
        selectAutelDeviceInfoForOpsMgmtVO.setCategory(chargePileDTO.getCategory());
        selectAutelDeviceInfoForOpsMgmtVO.setPartProduct(chargePileDTO.getPartProduct());
        selectAutelDeviceInfoForOpsMgmtVO.setPin(chargePileDTO.getPin());
        selectAutelDeviceInfoForOpsMgmtVO.setProductNamePdm(chargePileDTO.getProductNamePdm());
        selectAutelDeviceInfoForOpsMgmtVO.setRatedPower(chargePileDTO.getOutputPower());
        selectAutelDeviceInfoForOpsMgmtVO.setSn(sn);

        GunTypeAndNumVO gunTypeAndNumVO = pileDeviceServiceAdapter.getGunTypeAndNumVO(sn);

        log.info("===>>> ChargePointMerchantRelationServiceImpl.selectAutelDeviceInfoForOpsMgmt gunTypeAndNumVO : {}",
                JSON.toJSONString(gunTypeAndNumVO));


        if (gunTypeAndNumVO != null
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(gunTypeAndNumVO.getGunTypeMap())) {

            List<ConnectorVO> connectorDTOList = new ArrayList<>();

            for (Map.Entry<String, String> next : gunTypeAndNumVO.getGunTypeMap().entrySet()) {
                ConnectorVO connectorVO = new ConnectorVO();
                Integer connectorSequence = StringUtil.formatStringToInteger(next.getKey());
                Integer connectorType = StringUtil.formatStringToInteger(next.getValue());
                connectorVO.setConnectorId(connectorSequence);
                connectorVO.setConnectorType(connectorType);
                connectorDTOList.add(connectorVO);
            }
            selectAutelDeviceInfoForOpsMgmtVO.setConnectorDTOList(connectorDTOList);
        }

        return selectAutelDeviceInfoForOpsMgmtVO;
    }

    @Override
    public List<GetPileInfoForFleetVO> getPileInfoForFleet(List<String> pileSns) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getPileInfoForFleet pileSns : {}",
                JSON.toJSONString(pileSns));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileSns)) {
            return null;
        }

        Long sellerId = LoginUserUtil.getSellerId();
        if (sellerId == null) {
            return null;
        }

        List<GetPileInfoForFleetVO> getPileInfoForFleetVOList = new ArrayList<>();
        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntityList = this.getPileList(pileSns, sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantRelationEntityList)) {
            chargePointMerchantRelationEntityList.forEach(val -> {
                GetPileInfoForFleetVO getPileInfoForFleetVO = new GetPileInfoForFleetVO();
                getPileInfoForFleetVO.setPileSn(val.getSn());
                getPileInfoForFleetVO.setPileName(val.getName());
                getPileInfoForFleetVOList.add(getPileInfoForFleetVO);
            });
        }

        List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntityList = chargePointMerchantTerminalService.getTerminalList(pileSns, sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntityList)) {
            chargePointMerchantTerminalEntityList.forEach(val -> {
                GetPileInfoForFleetVO getPileInfoForFleetVO = new GetPileInfoForFleetVO();
                getPileInfoForFleetVO.setPileSn(val.getTerminalSn());
                getPileInfoForFleetVO.setPileName(val.getTerminalName());
                getPileInfoForFleetVOList.add(getPileInfoForFleetVO);
            });
        }

        return com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getPileInfoForFleetVOList)
                ? null
                : getPileInfoForFleetVOList;
    }

    /**
     * description: snSubscribeDisplaySet 资产订阅概览显示配置
     * version: 1.0
     * date: 2024/5/13 9:50
     * author: A23204
     *
     * @param displaySettingDto
     * @return com.autel.cloud.base.http.pojo.Result<java.util.List < java.lang.String>>
     */
    @Override
    public Result<List<String>> snSubscribeDisplaySet(SnSubscribeDisplaySettingDto displaySettingDto) {
        Long userId = LoginUserUtil.getUserId();
        String key = RedisKeyConstant.getSubscribeUserDisplaySetting(userId);

        stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(displaySettingDto));

        return Result.ofSucceed(displaySettingDto.getList());
    }

    /**
     * description: getSnSubscribeDisplaySetting 查询订阅显示配置
     * version: 1.0
     * date: 2024/5/13 10:09
     * author: A23204
     *
     * @return com.autel.cloud.base.http.pojo.Result<java.util.List < java.lang.String>>
     */
    @Override
    public Result<List<String>> getSnSubscribeDisplaySetting() {
        List<String> result = new ArrayList<>();

        Long userId = LoginUserUtil.getUserId();
        String key = RedisKeyConstant.getSubscribeUserDisplaySetting(userId);
        String setting = stringRedisTemplate.opsForValue().get(key);
        log.info("getSnSubscribeDisplaySetting, setting: {}", setting);
        if (setting != null) {
            SnSubscribeDisplaySettingDto displaySettingDto = JSON.parseObject(setting, SnSubscribeDisplaySettingDto.class);
            result = displaySettingDto.getList();
        } else {
            result.add("Pro");
            result.add("Lite");
            result.add("Advertising");
            result.add("Maintenance");
        }

        return Result.ofSucceed(result);
    }

    @Override
    public List<TerminalDeviceDataVO> getTerminalDeviceData(String hostSn, Long sellerId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getTerminalDeviceData hostSn : {} and sellerId : {}",
                JSON.toJSONString(hostSn),
                JSON.toJSONString(sellerId));

        if (StringUtils.isBlank(hostSn)
                || sellerId == null) {
            return null;
        }

        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .eq(ChargePointMerchantRelationEntity::getSn, hostSn)
                .eq(ChargePointMerchantRelationEntity::getMerchantId, sellerId));

        if (chargePointMerchantRelationEntity == null) {
            return null;
        }

        List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntities = chargePointMerchantTerminalService.getTerminalEntityList(hostSn, sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePointMerchantTerminalEntities)) {
            return null;
        }

        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(hostSn);
        OpLocationElasticDTO opLocationElasticDTO = null;
        if (opLocationPileEvseElasticDTO != null
                && opLocationPileEvseElasticDTO.getLocationId() != null) {
            opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(opLocationPileEvseElasticDTO.getLocationId());

        }

        Map<String, com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO> snAndChargePileDTOMap = pileDeviceServiceAdapter.getSnAndChargePileDTOMap(chargePointMerchantTerminalEntities
                .stream()
                .map(ChargePointMerchantTerminalEntity::getTerminalSn)
                .collect(Collectors.toList()));

        List<TerminalDeviceDataVO> terminalDeviceDataVOS = new ArrayList<>();
        for (ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity : chargePointMerchantTerminalEntities) {
            TerminalDeviceDataVO terminalDeviceDataVO = new TerminalDeviceDataVO();

            if (opLocationElasticDTO != null) {
                terminalDeviceDataVO.setLocationId(opLocationElasticDTO.getId());
                terminalDeviceDataVO.setLocationName(opLocationElasticDTO.getName());
                terminalDeviceDataVO.setTimeZone(opLocationElasticDTO.getTimeZone());
                terminalDeviceDataVO.setZoneId(opLocationElasticDTO.getZoneId());
            }

            terminalDeviceDataVO.setTerminalSn(chargePointMerchantTerminalEntity.getTerminalSn());
            terminalDeviceDataVO.setTerminalName(chargePointMerchantTerminalEntity.getTerminalName());
            terminalDeviceDataVO.setTerminalPin(chargePointMerchantTerminalEntity.getTerminalPin());
            terminalDeviceDataVO.setSellerId(sellerId);

            com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileDTO = snAndChargePileDTOMap.get(chargePointMerchantTerminalEntity.getTerminalSn());
            if (chargePileDTO != null) {
                terminalDeviceDataVO.setScreen1Pixel(chargePileDTO.getScreen1Pixel());
                terminalDeviceDataVO.setScreen1Size(chargePileDTO.getScreen1Size());
                if (chargePileDTO.getThirdPart() != null
                        && chargePileDTO.getThirdPart()) {
                    terminalDeviceDataVO.setThirdPart(1);
                } else {
                    terminalDeviceDataVO.setThirdPart(0);
                }

                terminalDeviceDataVO.setBrandName(chargePileDTO.getBrandName());
            }

            terminalDeviceDataVOS.add(terminalDeviceDataVO);
        }

        return terminalDeviceDataVOS;
    }

    @Override
    public Boolean validatePin(String sn, String pin) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.validatePin sn : {} and pin : {}",
                JSON.toJSONString(sn),
                JSON.toJSONString(pin));

        VerifyDTO verifyDTO = new VerifyDTO();
        verifyDTO.setPileSn(sn);
        verifyDTO.setPin(pin);
        Result<Boolean> result = deviceServiceFeign.verifyPile(verifyDTO);

        log.info("===>>> ChargePointMerchantRelationServiceImpl.validatePin result : {}",
                JSON.toJSONString(result));

        return result != null
                && result.getData() != null
                && result.getData();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ChargePointMerchantRelationEntity update(ChargePointDTO chargePointDTO, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.update chargePointDTO : {} and merchantId : {}",
                JSON.toJSONString(chargePointDTO),
                JSON.toJSONString(merchantId));

        String sn = chargePointDTO.getSn();
        if (org.apache.commons.lang3.StringUtils.isBlank(sn)) {
            throw new MessageCodeException(PileBaseEnum.THE_DEVICE_SN_OR_PIN_IS_INCORRECT);
        }

        if (merchantId == null) {
            throw new MessageCodeException(PileUserEnum.MERCHANT_NOT_EXIST);
        }

        this.checkConnectors(chargePointDTO);

        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, sn).eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);

        if (chargePointMerchantRelationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.CAN_NOT_FIND_SN_INFO);
        }

        List<Terminal> terminals = chargePointDTO.getTerminals();
        if (Long.valueOf(BrandEnum.AUTEL.getCode()).equals(chargePointDTO.getBrandId())
                || BrandEnum.AUTEL.getName().equalsIgnoreCase(chargePointDTO.getBrandName())) {
            //判断是否美标极简版充电桩  美标极简版充电桩不能添加到平台作为商桩(要排在校验桩是否被绑定前，不然桩被改成商桩后，即不能添加在平台作为商桩，也不能添加到app作为家桩)
            Result<ChargePileDTO> chargePileDetailResult = null;
            try {
                chargePileDetailResult = deviceServiceFeign.query(sn);
            } catch (Exception e) {
                log.error("OpLocationEvseRepositoryImplValidEvse", e);
            }
            if (chargePileDetailResult != null && chargePileDetailResult.getData() != null) {
                String productModel = chargePileDetailResult.getData().getProductModel();
                if (StringUtils.isNotBlank(productModel)) {
                    for (AmericaPileProductModelEnum americaPileProductModelEnum : AmericaPileProductModelEnum.values()) {
                        if (Objects.equals(productModel, americaPileProductModelEnum.getValue())) {
                            throw new MessageCodeException(PileBaseEnum.AMERICA_PILE_NOT_ALLOW_ADD);
                        }
                    }
                }
            }

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminals)) {
                List<String> terminalSnList = new ArrayList<>();
                Set<String> terminalSnSet = new HashSet<>();
                for (Terminal terminal : terminals) {

                    String terminalSn = terminal.getTerminalSn();
                    terminalSnList.add(terminalSn);
                    terminalSnSet.add(terminalSn);
                }

                if (terminalSnList.size() != terminalSnSet.size()) {
                    throw new MessageCodeException(PileBaseEnum.THE_DEVICE_SN_IS_DUPLICATED_PLEASE_CONFIRM);
                }

                List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(sn, merchantId);
                List<String> toBeUpdateTerminalSnList = new ArrayList<>();
                if (ObjectUtils.isNotEmpty(terminalEntityList)) {
                    for (Terminal terminal : terminals) {
                        for (ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity : terminalEntityList) {
                            if (terminal.getTerminalSn().equalsIgnoreCase(chargePointMerchantTerminalEntity.getTerminalSn())) {
                                toBeUpdateTerminalSnList.add(terminal.getTerminalSn());
                                break;
                            }
                        }
                    }
                }

                List<String> toBeInsertTerminalSnList = new ArrayList<>();
                terminals.forEach(val -> {
                    if (!toBeUpdateTerminalSnList.contains(val.getTerminalSn())) {
                        toBeInsertTerminalSnList.add(val.getTerminalSn());
                    }
                });

                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(toBeInsertTerminalSnList)) {
                    for (String toBeInsertTerminalSn : toBeInsertTerminalSnList) {

                        GetDeviceTypeDTO deviceTypeDTO = new GetDeviceTypeDTO();
                        deviceTypeDTO.setSn(toBeInsertTerminalSn);
                        deviceTypeDTO.setBrandId(chargePointDTO.getBrandId());

                        if (!this.checkSn(deviceTypeDTO, ChargePointMerchantRelationServiceImpl.TERMINAL)) {
                            throw new MessageCodeException(PileBaseEnum.THE_DEVICE_CANNOT_BE_USED_AS_A_TERMINAL, new Object[]{toBeInsertTerminalSn});
                        }

                        if (!this.checkDeviceUnique(toBeInsertTerminalSn, merchantId)) {
                            throw new MessageCodeException(PileBaseEnum.DO_NOT_ADD_DUPLICATES, new Object[]{toBeInsertTerminalSn});
                        }

                        List<String> terminalSns = new ArrayList<>();
                        terminalSns.add(toBeInsertTerminalSn);
                        List<ChargePointMerchantTerminalEntity> terminalEntitys = chargePointMerchantTerminalService.getTerminalList(terminalSns, null);
                        if (ObjectUtils.isNotEmpty(terminalEntitys)) {
                            for (ChargePointMerchantTerminalEntity terminalEntity : terminalEntitys) {
                                if (merchantId.equals(terminalEntity.getMerchantId())) {
                                    throw new MessageCodeException(PileBaseEnum.DO_NOT_ADD_DUPLICATES, new Object[]{toBeInsertTerminalSn});
                                }
                            }

                            if (terminalEntitys.size() >= 2) {
                                throw new MessageCodeException(PileBaseEnum.HAS_BEEN_ADDED_BY_OTHER_MERCHANTS, new Object[]{toBeInsertTerminalSn});
                            }
                        }
                    }
                }
            }

            List<VerifyDTO> verifyDTOList = new ArrayList<>();
            VerifyDTO verifyDTO = new VerifyDTO();
            verifyDTO.setPileSn(sn);
            verifyDTO.setPin(chargePointDTO.getPin());
            verifyDTOList.add(verifyDTO);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminals)) {
                terminals.forEach(val -> {
                    VerifyDTO item = new VerifyDTO();
                    item.setPileSn(val.getTerminalSn());
                    item.setPin(val.getPin());
                    verifyDTOList.add(item);
                });
            }
            for (VerifyDTO dto : verifyDTOList) {
                Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(dto);
                if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.SN_AND_PIN_CODES_DO_NOT_MATCH);
                }
            }
        } else {   //三方桩的校验
            log.info("三方桩");
            OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
            opLocationEvseDTO.setPileSN(sn);
            opLocationEvseDTO.setEvseSn(sn);
            opLocationEvseDTO.setPower(chargePointDTO.getRatedPower().doubleValue());
            opLocationEvseDTO.setThirdPart(1);
            opLocationEvseDTO.setPileName(chargePointDTO.getName());
            opLocationEvseDTO.setPowerType(chargePointDTO.getPowerType());
            opLocationEvseDTO.setPinCode(chargePointDTO.getPin());
            opLocationEvseDTO.setBrandName(chargePointDTO.getBrandName());
            opLocationEvseDTO.setBrandId(chargePointDTO.getBrandId());
            List<OpLocationConnectorDTO> list = new ArrayList<>();
            for (Connector connector : chargePointDTO.getConnectors()) {
                OpLocationConnectorDTO connectorDTO = new OpLocationConnectorDTO();
                connectorDTO.setConnectorId(connector.getConnectorId().toString());
                connectorDTO.setGunType(connector.getConnectorType());
                list.add(connectorDTO);
            }
            opLocationEvseDTO.setProductModel(chargePointDTO.getPartProduct());
            opLocationEvseDTO.setProductNamePdm(chargePointDTO.getProductNamePdm());
            if (Objects.nonNull(chargePointDTO.getPhases())) {
                String phase = chargePointDTO.getPhases().contains("3") ? "3" : "1";
                opLocationEvseDTO.setPhase(phase);
            }
            opLocationEvseDTO.setCategory("DC".equalsIgnoreCase(chargePointDTO.getPowerType()) ? 2 : 1);
            opLocationEvseDTO.setOpLocationConnectorDTOs(list);
            pileDeviceServiceAdapter.saveThirdPile(opLocationEvseDTO);
        }

        // 桩类型 充电功率  桩名称
        if (!chargePointMerchantRelationEntity.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
            if (Objects.nonNull(chargePointDTO.getPowerType())) {
                chargePointMerchantRelationEntity.setPowerType(chargePointDTO.getPowerType());
            }
            if (Objects.nonNull(chargePointDTO.getRatedPower())) {
                chargePointMerchantRelationEntity.setRatedPower(chargePointDTO.getRatedPower().doubleValue());
            }
        }
        chargePointMerchantRelationEntity.setName(chargePointDTO.getName());
        chargePointMerchantRelationEntity.setZoneId(chargePointDTO.getZoneId());
        chargePointMerchantRelationEntity.setUpdateTime(System.currentTimeMillis());
        if (chargePointMerchantRelationEntity.getBrandId() > 1) {
            chargePointMerchantRelationEntity.setPhases(chargePointDTO.getPhases());
        }

        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointMerchantRelationEntity.getOverchargingPileFlag())) {
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePointDTO.getConnectors())) {
                chargePointDTO.setConnectors(this.buildConnectors(terminals));
            }
            chargePointMerchantRelationEntity.setConnectors(chargePointDTO.getConnectors());
        } else {
            chargePointDTO.setTerminals(null);
        }

        log.info("update chargePointMerchantRelationEntity = {}", JSON.toJSONString(chargePointMerchantRelationEntity));

        chargePointMerchantRelationMapper.updateById(chargePointMerchantRelationEntity);

        Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap = new HashMap<>();
        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointMerchantRelationEntity.getOverchargingPileFlag())) {
            chargePointMerchantTerminalEntityAndEventMap = this.processingTerminal(terminals, chargePointMerchantRelationEntity);
        }

        this.syncBusinessMessage(chargePointMerchantRelationEntity, ChargePointNoticeEvent.Event.UPDATE, chargePointMerchantTerminalEntityAndEventMap);
        Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> finalChargePointMerchantTerminalEntityAndEventMap = chargePointMerchantTerminalEntityAndEventMap;
        AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> this.syncDeviceInfoForPos(chargePointMerchantRelationEntity, ChargePointNoticeEvent.Event.UPDATE, finalChargePointMerchantTerminalEntityAndEventMap)));
        return chargePointMerchantRelationEntity;
    }

    private void syncDeviceInfoForPos(ChargePointMerchantRelationEntity chargePointMerchantRelationEntity,
                                      ChargePointNoticeEvent.Event event,
                                      Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap) {

        if (chargePointMerchantRelationEntity == null
                || org.apache.commons.lang3.StringUtils.isBlank(chargePointMerchantRelationEntity.getSn())) {
            return;
        }

        String sn = chargePointMerchantRelationEntity.getSn();
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.getOpLocationPileEvseElasticDTOByPileSnFromES(sn);
        if (opLocationPileEvseElasticDTO == null) {
            return;
        }

        List<SyncPileInfoForPosDTO> syncPileInfoForPosDTOList = new ArrayList<>();
        Integer overchargingPileFlag = chargePointMerchantRelationEntity.getOverchargingPileFlag();
        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(overchargingPileFlag)) {
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntityAndEventMap)) {
                chargePointMerchantTerminalEntityAndEventMap.forEach((key, value) -> {
                    SyncPileInfoForPosDTO syncPileInfoForPosDTO = new SyncPileInfoForPosDTO();

                    syncPileInfoForPosDTO.setEvent(value);
                    syncPileInfoForPosDTO.setDeviceType(1);
                    syncPileInfoForPosDTO.setPileSn(chargePointMerchantRelationEntity.getSn());
                    syncPileInfoForPosDTO.setPileName(chargePointMerchantRelationEntity.getName());
                    syncPileInfoForPosDTO.setTerminalSn(key.getTerminalSn());
                    syncPileInfoForPosDTO.setTerminalName(key.getTerminalName());
                    syncPileInfoForPosDTO.setSellerId(chargePointMerchantRelationEntity.getMerchantId());

                    syncPileInfoForPosDTOList.add(syncPileInfoForPosDTO);
                });
            }
        } else {
            SyncPileInfoForPosDTO syncPileInfoForPosDTO = new SyncPileInfoForPosDTO();
            syncPileInfoForPosDTO.setEvent(event);
            syncPileInfoForPosDTO.setDeviceType(2);
            syncPileInfoForPosDTO.setPileSn(chargePointMerchantRelationEntity.getSn());
            syncPileInfoForPosDTO.setPileName(chargePointMerchantRelationEntity.getName());
            syncPileInfoForPosDTO.setSellerId(chargePointMerchantRelationEntity.getMerchantId());
            syncPileInfoForPosDTOList.add(syncPileInfoForPosDTO);
        }
        syncPileInfoForPosDTOList.forEach(val -> pileDeviceServiceAdapter.syncPileInfoForPos(val));
    }

    private Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> processingTerminal(List<Terminal> terminals, ChargePointMerchantRelationEntity chargePointMerchantRelationEntity) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.processingTerminal terminals : {} and chargePointMerchantRelationEntity : {}",
                JSON.toJSONString(terminals),
                JSON.toJSONString(chargePointMerchantRelationEntity));

        if (ObjectUtils.isEmpty(terminals)
                || chargePointMerchantRelationEntity == null
                || chargePointMerchantRelationEntity.getMerchantId() == null) {
            return null;
        }

        Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap = new HashMap<>();

        List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(chargePointMerchantRelationEntity.getSn(), chargePointMerchantRelationEntity.getMerchantId());
        List<String> toBeUpdateTerminalSnList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(terminalEntityList)) {
            for (Terminal terminal : terminals) {
                for (ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity : terminalEntityList) {
                    if (terminal.getTerminalSn().equalsIgnoreCase(chargePointMerchantTerminalEntity.getTerminalSn())) {
                        toBeUpdateTerminalSnList.add(terminal.getTerminalSn());
                        break;
                    }
                }
            }
        }

        List<String> toBeDeleteTerminalSnList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(terminalEntityList)) {
            terminalEntityList.forEach(val -> {
                if (!toBeUpdateTerminalSnList.contains(val.getTerminalSn())) {
                    toBeDeleteTerminalSnList.add(val.getTerminalSn());
                    chargePointMerchantTerminalEntityAndEventMap.put(val, ChargePointNoticeEvent.Event.DELETE);
                }
            });
        }

        List<String> toBeInsertTerminalSnList = new ArrayList<>();
        terminals.forEach(val -> {
            if (!toBeUpdateTerminalSnList.contains(val.getTerminalSn())) {
                toBeInsertTerminalSnList.add(val.getTerminalSn());
            }
        });

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(toBeDeleteTerminalSnList)) {
            chargePointMerchantTerminalService.deleteByCondition(toBeDeleteTerminalSnList, chargePointMerchantRelationEntity.getMerchantId());
        }

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(toBeInsertTerminalSnList)) {
            List<Terminal> toBeInsertedTerminalList = new ArrayList<>();
            terminals.forEach(val -> {
                if (toBeInsertTerminalSnList.contains(val.getTerminalSn())) {
                    toBeInsertedTerminalList.add(val);
                }
            });

            List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntities = this.addTerminals(toBeInsertedTerminalList, chargePointMerchantRelationEntity);
            chargePointMerchantTerminalEntities.forEach(val -> chargePointMerchantTerminalEntityAndEventMap.put(val, ChargePointNoticeEvent.Event.INSERT));
        }

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(toBeUpdateTerminalSnList)) {
            terminals.forEach(val -> {
                if (toBeUpdateTerminalSnList.contains(val.getTerminalSn())) {
                    List<String> terminalSnList = new ArrayList<>();
                    terminalSnList.add(val.getTerminalSn());
                    List<ChargePointMerchantTerminalEntity> terminalList = chargePointMerchantTerminalService.getTerminalList(terminalSnList, chargePointMerchantRelationEntity.getMerchantId());
                    ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity = terminalList.get(0);
                    chargePointMerchantTerminalEntity.setTerminalName(val.getTerminalName());
                    chargePointMerchantTerminalEntity.setJsonFormatConnectors(val.getConnectors());
                    chargePointMerchantTerminalEntity.setUpdateTime(chargePointMerchantRelationEntity.getUpdateTime());
                    chargePointMerchantTerminalService.updateTerminal(chargePointMerchantTerminalEntity);
                    chargePointMerchantTerminalEntityAndEventMap.put(chargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event.UPDATE);
                }
            });
        }
        return chargePointMerchantTerminalEntityAndEventMap;
    }

    private void syncBusinessMessage(ChargePointMerchantRelationEntity chargePointMerchantRelationEntity,
                                     ChargePointNoticeEvent.Event event,
                                     Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap) {
        ChargePointNoticeEvent chargePointNoticeEvent = ChargePointMerchantRelationTypeMapper.INSTANCE.chargePointMerchantRelationEntityToChargePointNoticeEvent(chargePointMerchantRelationEntity);
        chargePointNoticeEvent.setEvent(event);

        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointMerchantRelationEntity.getOverchargingPileFlag())) {
            chargePointNoticeEvent.setDeviceType(DeviceTypeEnum.OVERCHARGING_THE_HOST.getCode());
        } else {
            chargePointNoticeEvent.setDeviceType(DeviceTypeEnum.OTHER_DEVICE.getCode());
        }

        if (Objects.isNull(chargePointNoticeEvent.getOwner())) {
            ChargePointMerchantRelationEntity relation = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.OWNER);
            Optional.ofNullable(relation).ifPresent(target -> chargePointNoticeEvent.setOwner(target.getMerchantId()));
        }
        if (Objects.isNull(chargePointNoticeEvent.getMaintenance())) {
            ChargePointMerchantRelationEntity relation = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.MAINTENANCE);
            Optional.ofNullable(relation).ifPresent(target -> chargePointNoticeEvent.setMaintenance(target.getMerchantId()));
        }
        Optional.ofNullable(chargePointNoticeEvent.getOwner()).ifPresent(id -> chargePointNoticeEvent.setOwnerName(pileUserServiceAdapter.findMerchantById(id).getName()));
        Optional.ofNullable(chargePointNoticeEvent.getMaintenance()).ifPresent(id -> chargePointNoticeEvent.setMaintenanceName(pileUserServiceAdapter.findMerchantById(id).getName()));

        log.info("chargePointNoticeEvent=  {}", JSON.toJSONString(chargePointNoticeEvent));
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, ChargePointNoticeEvent.class.getSimpleName(), JSON.toJSONString(chargePointNoticeEvent));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntityAndEventMap)) {
            List<String> terminalSnList = chargePointMerchantTerminalEntityAndEventMap
                    .keySet()
                    .stream()
                    .map(ChargePointMerchantTerminalEntity::getTerminalSn)
                    .distinct()
                    .collect(Collectors.toList());
            Map<String, com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO> snAndChargePileDTOMap = pileDeviceServiceAdapter.getSnAndChargePileDTOMap(terminalSnList);
            chargePointMerchantTerminalEntityAndEventMap.forEach((key, value) -> rabbitTemplate.convertAndSend(
                    TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX,
                    ChargePointNoticeEvent.class.getSimpleName(),
                    JSON.toJSONString(this.buildTerminalEvent(
                            key,
                            value,
                            chargePointMerchantRelationEntity,
                            chargePointNoticeEvent,
                            snAndChargePileDTOMap.get(key.getTerminalSn())
                            ))));
        }
    }

    private ChargePointNoticeEvent buildTerminalEvent(
            ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity,
            ChargePointNoticeEvent.Event event,
            ChargePointMerchantRelationEntity chargePointMerchantHostRelationEntity,
            ChargePointNoticeEvent chargePointNoticeHostEvent,
            com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileDTO) {

        ChargePointNoticeEvent chargePointNoticeTerminalEvent = new ChargePointNoticeEvent();
        if (event == null) {
            return chargePointNoticeTerminalEvent;
        }

        if (event == ChargePointNoticeEvent.Event.DELETE) {
            chargePointNoticeTerminalEvent.setEvent(ChargePointNoticeEvent.Event.DELETE);
            chargePointNoticeTerminalEvent.setId(chargePointMerchantTerminalEntity.getId());
            chargePointNoticeTerminalEvent.setSn(chargePointMerchantTerminalEntity.getTerminalSn());
            chargePointNoticeTerminalEvent.setDeviceType(DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode());
        } else {
            chargePointNoticeTerminalEvent.setEvent(event);
            chargePointNoticeTerminalEvent.setId(chargePointMerchantTerminalEntity.getId());
            chargePointNoticeTerminalEvent.setBrandId(chargePointNoticeHostEvent.getBrandId());
            chargePointNoticeTerminalEvent.setBrandName(chargePointNoticeHostEvent.getBrandName());
            chargePointNoticeTerminalEvent.setName(chargePointMerchantTerminalEntity.getTerminalName());
            chargePointNoticeTerminalEvent.setSn(chargePointMerchantTerminalEntity.getTerminalSn());
            chargePointNoticeTerminalEvent.setPin(chargePointMerchantTerminalEntity.getTerminalPin());
            if (MerchantChargePointRelationEnum.MAINTENANCE.equals(MerchantChargePointRelationEnum.keyOf(chargePointMerchantHostRelationEntity.getRelation()))) {
                chargePointNoticeTerminalEvent.setName_1(chargePointMerchantTerminalEntity.getTerminalName());
                chargePointNoticeTerminalEvent.setMaintenance(chargePointMerchantHostRelationEntity.getMerchantId());
                chargePointNoticeTerminalEvent.setMaintenanceBindTime(chargePointMerchantHostRelationEntity.getBindTime());
            }
            if (MerchantChargePointRelationEnum.OWNER.equals(MerchantChargePointRelationEnum.keyOf(chargePointMerchantHostRelationEntity.getRelation()))) {
                chargePointNoticeTerminalEvent.setName_3(chargePointMerchantTerminalEntity.getTerminalName());
                chargePointNoticeTerminalEvent.setOwner(chargePointMerchantHostRelationEntity.getMerchantId());
                chargePointNoticeTerminalEvent.setOwnerBindTime(chargePointMerchantHostRelationEntity.getBindTime());
            }
            chargePointNoticeTerminalEvent.setThirdPart(!Long.valueOf(BrandEnum.AUTEL.getCode()).equals(chargePointNoticeTerminalEvent.getBrandId()));
            chargePointNoticeTerminalEvent.setOwnerName(chargePointNoticeHostEvent.getOwnerName());
            chargePointNoticeTerminalEvent.setMaintenanceName(chargePointNoticeHostEvent.getMaintenanceName());
            chargePointNoticeTerminalEvent.setCategory(chargePointMerchantHostRelationEntity.getCategory());
            chargePointNoticeTerminalEvent.setPowerType(chargePointMerchantHostRelationEntity.getPowerType());
            chargePointNoticeTerminalEvent.setPhases(chargePointMerchantHostRelationEntity.getPhases());
            chargePointNoticeTerminalEvent.setZoneId(chargePointMerchantHostRelationEntity.getZoneId());
            chargePointNoticeTerminalEvent.setDeviceType(DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode());
            chargePointNoticeTerminalEvent.setProductNamePdm(chargePileDTO.getProductNamePdm());
            chargePointNoticeTerminalEvent.setPartProduct(chargePileDTO.getPartProduct());
            chargePointNoticeTerminalEvent.setRatedPower(chargePileDTO.getOutputPower());
            chargePointNoticeTerminalEvent.setScreen1Size(chargePileDTO.getScreen1Size());
            chargePointNoticeTerminalEvent.setScreen1Pixel(chargePileDTO.getScreen1Pixel());
            chargePointNoticeTerminalEvent.setSubscriptionStatus(chargePointMerchantTerminalEntity.getSubStatus());
            chargePointNoticeTerminalEvent.setCreateTime(chargePointMerchantTerminalEntity.getCreateTime());
            chargePointNoticeTerminalEvent.setUpdateTime(chargePointMerchantTerminalEntity.getUpdateTime());
            chargePointNoticeTerminalEvent.setConnectors(chargePointMerchantTerminalEntity.getConnectorsList());
            chargePointNoticeTerminalEvent.setMac(pileDeviceServiceAdapter.getDeviceMac(chargePointMerchantTerminalEntity.getTerminalSn()));
        }
        return chargePointNoticeTerminalEvent;
    }

    @Override
    public ChargePointMerchantRelationEntity updatePileName(UpdatePileNameDTO  pileNameUpdateEvent) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        String pileSn = pileNameUpdateEvent.getPileSn();
        String pileName = pileNameUpdateEvent.getPileName();
        Long merchantId = pileNameUpdateEvent.getMerchantId();
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, pileSn).eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
        if (Objects.nonNull(chargePointMerchantRelationEntity)){
            chargePointMerchantRelationEntity.setName(pileName);
            chargePointMerchantRelationEntity.setUpdateTime(System.currentTimeMillis());
            log.info("update chargePointMerchantRelationEntity = {}", JSON.toJSONString(chargePointMerchantRelationEntity));
            chargePointMerchantRelationMapper.updateById(chargePointMerchantRelationEntity);
            this.syncBusinessMessage(chargePointMerchantRelationEntity, ChargePointNoticeEvent.Event.UPDATE, null);
            Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap = new HashMap<>();
            if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointMerchantRelationEntity.getOverchargingPileFlag())) {
                List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(pileSn, merchantId);
                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalEntityList)) {
                    terminalEntityList.forEach(val -> chargePointMerchantTerminalEntityAndEventMap.put(val, ChargePointNoticeEvent.Event.UPDATE));
                }
            }
            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> this.syncDeviceInfoForPos(chargePointMerchantRelationEntity, ChargePointNoticeEvent.Event.UPDATE, chargePointMerchantTerminalEntityAndEventMap)));
        }
        return null;
    }

    @Override
    public IPage<ChargePointVO> findByPage(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId, String zoneId) {
        StopWatch stopWatch = new StopWatch("统一桩管理列表");

        stopWatch.start("查询统一桩列表");

        // 2.8版本优化 根据启动充电功能点来判断
        List<Long> locationIds = chargePointQueryDTO.getLocations();
        List<String> snByLocationIdList = getSnByLocationId(locationIds);

        Page<ChargePointMerchantRelationEntity> pageInfo = new Page<>(chargePointQueryDTO.getPage(), chargePointQueryDTO.getPageSize());
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        queryWrapper.in(CollUtil.isNotEmpty(locationIds), ChargePointMerchantRelationEntity::getSn, snByLocationIdList);
        String keyword = chargePointQueryDTO.getKeyword();
        boolean condition = org.apache.commons.lang3.StringUtils.isNotBlank(keyword);
        List<String> matchFields = Collections.singletonList("ProductNamePdm");
        boolean productNamePdmCondition = matchFields.contains("ProductNamePdm") && condition;

        List<String> hostSnList = new ArrayList<>();
        if (condition) {
            LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda()
                    .eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
                    .and (
                            chargePointEntityLambdaQueryWrapper -> chargePointEntityLambdaQueryWrapper
                                    .like(ChargePointMerchantTerminalEntity::getTerminalSn, keyword)
                                    .or()
                                    .like(ChargePointMerchantTerminalEntity::getTerminalName, keyword)
                    );
            List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
            if (CollectionUtil.isNotEmpty(terminalEntities)) {
                hostSnList = terminalEntities.stream().map(item -> item.getHostSn()).collect(Collectors.toList());
            }
        }

        List<String> finalHostSnListSnList = hostSnList;
        queryWrapper.and(condition,
                chargePointEntityLambdaQueryWrapper -> chargePointEntityLambdaQueryWrapper.like(condition, ChargePointMerchantRelationEntity::getSn, keyword)
                        .or().like(condition, ChargePointMerchantRelationEntity::getName, keyword)
                        .or().like(productNamePdmCondition, ChargePointMerchantRelationEntity::getProductNamePdm, keyword)
                        .or().in(CollectionUtil.isNotEmpty(finalHostSnListSnList), ChargePointMerchantRelationEntity::getSn, finalHostSnListSnList));

        queryWrapper.eq(StringUtils.isNotBlank(chargePointQueryDTO.getPowerType()), ChargePointMerchantRelationEntity::getPowerType, chargePointQueryDTO.getPowerType());
        queryWrapper.eq(Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()), ChargePointMerchantRelationEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus());
        queryWrapper.orderByAsc(ChargePointMerchantRelationEntity::getSubStatus).orderByAsc(ChargePointMerchantRelationEntity::getSubExpire).orderByAsc(ChargePointMerchantRelationEntity::getId);

        //Page<ChargePointMerchantRelationEntity> chargePointEntityPage = chargePointMerchantRelationMapper.selectPage(pageInfo, queryWrapper);

        // TODO: 因为要排序，需要自定义查询sql；
        chargePointQueryDTO.setMerchantId(merchantId);
        chargePointQueryDTO.setProductNamePdmCondition(productNamePdmCondition);
        chargePointQueryDTO.setHostSnList(hostSnList);
        chargePointQueryDTO.setSnByLocationIdList(snByLocationIdList);

        Page<ChargePointMerchantRelationEntity> chargePointEntityPage = chargePointMerchantRelationMapper.selectByOrder(pageInfo, chargePointQueryDTO);



        stopWatch.stop();

        String commodityCategory = null;
        if (StringUtils.isNotBlank(chargePointQueryDTO.getSkuCode())) {
            commodityCategory = commodityCenterServiceAdapter.getCommodityCategory(chargePointQueryDTO.getSkuCode());
        }
        Page<ChargePointVO> chargePointVOPage = populateAttr(merchantId, chargePointEntityPage, commodityCategory, chargePointQueryDTO.getSubscriptionStatus(), chargePointQueryDTO.getCommodityCode(), false);
        log.info(stopWatch.prettyPrint());
        return chargePointVOPage;
    }

    @Override
    public ChargePointPowerTypesVO existsPowerTypes(Long merchantId, String zoneId) {
        StopWatch stopWatch = new StopWatch("统一桩管理桩");
        ChargePointPowerTypesVO chargePointPowerTypesVO = new ChargePointPowerTypesVO();
        stopWatch.start("查询统一桩列表");
        List<String> powerTypes = chargePointMerchantRelationMapper.existsPowerTypes(merchantId);
        log.info("powerTypes {}", JSON.toJSONString(powerTypes));
        chargePointPowerTypesVO.setPowerTypes(powerTypes);
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return chargePointPowerTypesVO;
    }

    @Override
    public Integer getConnectorNumber(String sn) {
        String connectorNumberKey = RedisKeyConstant.getPileConnectorNumber(sn);
        String connectorNumberValue = stringRedisTemplate.opsForValue().get(connectorNumberKey);
        Integer connectorNumber = 1;
        if(StringUtils.isBlank(connectorNumberValue)){
            String connectors = chargePointMerchantRelationMapper.getConnectors(sn);
            if(StringUtils.isNotBlank(connectors)){
                List<String> list = JSONArray.parseArray(connectors, String.class);
                connectorNumber = list.size();
                stringRedisTemplate.opsForValue().set(connectorNumberKey, connectorNumber.toString(), random.nextInt(3) + 1L, TimeUnit.DAYS);
            }
        }else{
            connectorNumber = Integer.parseInt(connectorNumberValue);
        }
        return connectorNumber;
    }

    @Override
    public Integer getDeviceType(GetDeviceTypeDTO getDeviceTypeDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceType getDeviceTypeDTO : {}",
                JSON.toJSONString(getDeviceTypeDTO));

        if (getDeviceTypeDTO == null
                || org.apache.commons.lang3.StringUtils.isBlank(getDeviceTypeDTO.getSn())) {
            throw new MessageCodeException(PileBaseEnum.THE_DEVICE_SN_OR_PIN_IS_INCORRECT);
        }

        if (getDeviceTypeDTO.getBrandId() == null
                || !getDeviceTypeDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
            return DeviceTypeEnum.OTHER_DEVICE.getCode();
        }

        return pileDeviceServiceAdapter.getDeviceType(getDeviceTypeDTO);
    }

    @Override
    public GetPileInfoByPileVO getPileInfoByPile(GetPileInfoByPileDTO getPileInfoByPileDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getPileInfoByPile getPileInfoByPileDTO : {}",
                JSON.toJSONString(getPileInfoByPileDTO));

        if (getPileInfoByPileDTO == null
                || org.apache.commons.lang3.StringUtils.isBlank(getPileInfoByPileDTO.getHostSn())
                || org.apache.commons.lang3.StringUtils.isBlank(getPileInfoByPileDTO.getPin())) {
            throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
        }

        Boolean result = this.validatePin(getPileInfoByPileDTO.getHostSn(), getPileInfoByPileDTO.getPin());
        if (Boolean.FALSE.equals(result)) {
            throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
        }

        GetPileInfoByPileVO getPileInfoByPileVO = new GetPileInfoByPileVO();
        getPileInfoByPileVO.setHostSn(getPileInfoByPileDTO.getHostSn());
        List<TerminalInfoVO> terminals = new ArrayList<>();
        String redisResult = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getPileSideReportsOverchargedPileDataKey(getPileInfoByPileDTO.getHostSn()));
        if (org.apache.commons.lang3.StringUtils.isNotBlank(redisResult)) {
            OverchargingPileDTO overchargingPileDTO = JSON.parseObject(redisResult, OverchargingPileDTO.class);

            log.info("===>>> ChargePointMerchantRelationServiceImpl.getPileInfoByPile overchargingPileDTO : {}",
                    JSON.toJSONString(overchargingPileDTO));

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(overchargingPileDTO.getTerminalInfo())) {
                Map<String, List<Integer>> terminalSnAndConnectorIdListMap = new HashMap<>();
                overchargingPileDTO.getTerminalInfo().forEach(val -> terminalSnAndConnectorIdListMap.put(val.getTerminalSn(), val.getConnectorId()));
                Map<String, ConnectorDTO> evseSnAndConnectorDTOMap = new HashMap<>();
                terminalSnAndConnectorIdListMap.forEach((key, value) -> {
                    List<ConnectorDTO> connectorDTOList = this.getConnectorDTOS(key);
                    value.forEach(val -> {
                        ConnectorDTO connectorDTO = this.containsConnectorDTO(connectorDTOList, val);
                        if (connectorDTO != null) {
                            evseSnAndConnectorDTOMap.put(key + "_" + val, connectorDTO);
                        } else {
                            evseSnAndConnectorDTOMap.put(key + "_" + val, ConnectorDTO.builder().connectorNo(val).connectorType(1).build());
                        }
                    });
                });


                overchargingPileDTO.getTerminalInfo().forEach(val -> {
                    TerminalInfoVO terminalInfoVO = new TerminalInfoVO();
                    terminalInfoVO.setTerminalSn(val.getTerminalSn());
                    List<ConnectorDTO> connectorList = new ArrayList<>();
                    val.getConnectorId().forEach(connectorId -> connectorList.add(evseSnAndConnectorDTOMap.get(val.getTerminalSn() + "_" + connectorId)));
                    terminalInfoVO.setConnectorList(connectorList);
                    terminals.add(terminalInfoVO);
                });
            }
        }
        getPileInfoByPileVO.setTerminals(com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(terminals) ? null : terminals);
        return getPileInfoByPileVO;
    }

    private ConnectorDTO containsConnectorDTO(List<ConnectorDTO> connectorDTOList, Integer connectorId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.containsConnectorDTO connectorDTOList : {} and connectorId : {}",
                JSON.toJSONString(connectorDTOList),
                JSON.toJSONString(connectorId));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(connectorDTOList)
                || connectorId == null) {
            return null;
        }

        for (ConnectorDTO connectorDTO : connectorDTOList) {
            if (connectorDTO.getConnectorNo().equals(connectorId)) {
                return connectorDTO;
            }
        }
        return null;
    }

    @Override
    public Map<String, List<Integer>> getDeviceGunNumber(GetDeviceGunNumberDTO getDeviceGunNumberDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceGunNumber getDeviceGunNumberDTO : {}",
                JSON.toJSONString(getDeviceGunNumberDTO));

        if (getDeviceGunNumberDTO == null
                || getDeviceGunNumberDTO.getSellerId() == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(getDeviceGunNumberDTO.getDeviceSnSet())) {
            return null;
        }

        Map<String, List<Integer>> deviceSnAndConnectorIdListMap = new HashMap<>();
        Map<String, Integer> deviceSnAndDeviceTypeEnumMap = this.getDeviceSnAndDeviceTypeEnumMap(getDeviceGunNumberDTO.getDeviceSnSet(), getDeviceGunNumberDTO.getSellerId());
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(deviceSnAndDeviceTypeEnumMap)) {
            Set<String> pileSnSet = new HashSet<>();
            Set<String> terminalSet = new HashSet<>();
            deviceSnAndDeviceTypeEnumMap.forEach((key, value) -> {
                if (DeviceTypeEnum.OTHER_DEVICE.getCode().equals(value)) {
                    pileSnSet.add(key);
                } else {
                    terminalSet.add(key);
                }
            });

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileSnSet)) {
                List<ChargePointMerchantRelationEntity> pileList = this.getPileList(new ArrayList<>(pileSnSet), getDeviceGunNumberDTO.getSellerId());
                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileList)) {
                    pileList.forEach(val -> {
                        List<Integer> connectorIdList = new ArrayList<>();
                        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(val.getConnectors())) {

                            log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceGunNumber connectors : {}",
                                    JSON.toJSONString(val.getConnectors()));

                            List<Connector> connectorList = JSON.parseArray(val.getConnectors().toString(), Connector.class);
                            connectorList.forEach(item -> connectorIdList.add(item.getConnectorId()));
                        }
                        deviceSnAndConnectorIdListMap.put(val.getSn(), connectorIdList);
                    });
                }
            }

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalSet)) {
                List<ChargePointMerchantTerminalEntity> terminalList = chargePointMerchantTerminalService.getTerminalList(new ArrayList<>(terminalSet), getDeviceGunNumberDTO.getSellerId());
                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalList)) {
                    terminalList.forEach(val -> {
                        List<Integer> connectorIdList = new ArrayList<>();
                        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(val.getConnectorsList())) {
                            val.getConnectorsList().forEach(item -> connectorIdList.add(item.getConnectorId()));
                        }
                        deviceSnAndConnectorIdListMap.put(val.getTerminalSn(), connectorIdList);
                    });
                }
            }
        }


        return deviceSnAndConnectorIdListMap;
    }

    @Override
    public DeviceBriefInfoVO getDeviceBriefInfo(String sn, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceBriefInfo sn : {} and merchantId : {}",
                JSON.toJSONString(sn),
                JSON.toJSONString(merchantId));

        if (org.apache.commons.lang3.StringUtils.isBlank(sn)
                || merchantId == null) {
            throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
        }

        Set<String> snSet = new HashSet<>();
        snSet.add(sn);
        Map<String, Integer> deviceSnAndDeviceTypeEnumMap = this.getDeviceSnAndDeviceTypeEnumMap(snSet, merchantId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(deviceSnAndDeviceTypeEnumMap)
                || deviceSnAndDeviceTypeEnumMap.get(sn) == null) {
            throw new MessageCodeException(PileBaseEnum.CAN_NOT_FIND_SN_INFO);
        }

        DeviceBriefInfoVO deviceBriefInfoVO = new DeviceBriefInfoVO();
        deviceBriefInfoVO.setDeviceSn(sn);
        deviceBriefInfoVO.setDeviceType(deviceSnAndDeviceTypeEnumMap.get(sn));

        if (DeviceTypeEnum.OVERCHARGING_THE_HOST.getCode().equals(deviceBriefInfoVO.getDeviceType())) {
            List<ChargePointMerchantRelationEntity> pileList = this.getPileList(new ArrayList<>(snSet), merchantId);
            if (ObjectUtils.isNotEmpty(pileList)) {
                ChargePointMerchantRelationEntity entity = pileList.get(0);
                deviceBriefInfoVO.setDeviceName(entity.getName());
                deviceBriefInfoVO.setTerminals(chargePointMerchantTerminalService.getTerminalBriefInfoVOList(entity.getSn(), merchantId));
            }
        } else if (DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceBriefInfoVO.getDeviceType())) {
            List<ChargePointMerchantTerminalEntity> terminalList = chargePointMerchantTerminalService.getTerminalList(new ArrayList<>(snSet), merchantId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalList)) {
                deviceBriefInfoVO.setDeviceName(terminalList.get(0).getTerminalName());
            }
        } else {
            List<ChargePointMerchantRelationEntity> pileList = this.getPileList(new ArrayList<>(snSet), merchantId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileList)) {
                deviceBriefInfoVO.setDeviceName(pileList.get(0).getName());
            }
        }

        return deviceBriefInfoVO;
    }

    @Override
    public List<DeviceInfoForPosVO> getDeviceInfoForPos(DeviceInfoForPosDTO deviceInfoForPosDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceInfoForPos deviceInfoForPosDTO : {}",
                JSON.toJSONString(deviceInfoForPosDTO));

        if (deviceInfoForPosDTO == null
                || deviceInfoForPosDTO.getSellerId() == null) {
            return null;
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceInfoForPos getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        Set<Long> havePermissionLocationIdSet = new HashSet<>();
        if (getLocationIdsResult != null
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(getLocationIdsResult.getData())) {
            havePermissionLocationIdSet.addAll(getLocationIdsResult.getData());
        }

        Long sellerId = deviceInfoForPosDTO.getSellerId();
        List<OpLocationEntity> opLocationEntityList = opLocationRepository.getLocationInfoBySellerId(sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEntityList)) {
            return null;
        }

        Map<Long, List<OpLocationEntity>> locationIdAndOpLocationEntityMap = new HashMap<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEntityList)) {
            locationIdAndOpLocationEntityMap = opLocationEntityList
                    .stream()
                    .collect(Collectors.groupingBy(OpLocationEntity::getId));
        }

        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.getPileInfoListByLocationIdList(opLocationEntityList
                .stream()
                .map(OpLocationEntity::getId)
                .collect(Collectors.toList()));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileEvseEntityList)) {
            return null;
        }

        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntityList = this.getPileList(opLocationPileEvseEntityList
                        .stream()
                .filter(val -> org.apache.commons.lang3.StringUtils.isNotBlank(val.getPileSn()))
                .map(OpLocationPileEvseEntity::getPileSn)
                .distinct()
                .collect(Collectors.toList()),
                sellerId);
        Map<String, List<ChargePointMerchantRelationEntity>> pileSnAndChargePointMerchantRelationEntityListMap = new HashMap<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantRelationEntityList)) {
            pileSnAndChargePointMerchantRelationEntityListMap = chargePointMerchantRelationEntityList
                    .stream()
                    .collect(Collectors.groupingBy(ChargePointMerchantRelationEntity::getSn));
        }

        List<DeviceInfoForPosVO> deviceInfoForPosVOList = new ArrayList<>();
        for (OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntityList) {

            String pileSn = opLocationPileEvseEntity.getPileSn();
            Long locationId = opLocationPileEvseEntity.getLocationId();

            if (org.apache.commons.lang3.StringUtils.isBlank(pileSn)
                    || locationId == null) {
                continue;
            }

            List<OpLocationEntity> opLocationEntities = locationIdAndOpLocationEntityMap.get(locationId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEntities)) {
                continue;
            }

            OpLocationEntity opLocationEntity = opLocationEntities.get(0);

            List<ChargePointMerchantRelationEntity> relationEntityList = pileSnAndChargePointMerchantRelationEntityListMap.get(pileSn);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(relationEntityList)) {
                continue;
            }

            ChargePointMerchantRelationEntity relationEntity = relationEntityList.get(0);
            if (!OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(relationEntity.getOverchargingPileFlag())) {
                DeviceInfoForPosVO deviceInfoForPosVO = new DeviceInfoForPosVO();
                deviceInfoForPosVO.setPileId(relationEntity.getId());
                deviceInfoForPosVO.setLocationId(opLocationEntity.getId());
                deviceInfoForPosVO.setLocationName(opLocationEntity.getName());
                deviceInfoForPosVO.setPileName(relationEntity.getName());
                deviceInfoForPosVO.setPileSn(pileSn);
                deviceInfoForPosVO.setTerminalName(null);
                deviceInfoForPosVO.setTerminalSn(null);

                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(relationEntity.getConnectors())) {
                    List<Integer> connectorIdList = new ArrayList<>();
                    List<Connector> connectorList = JSON.parseArray(relationEntity.getConnectors().toString(), Connector.class);
                    connectorList.forEach(item -> connectorIdList.add(item.getConnectorId()));
                    Collections.sort(connectorIdList);
                    deviceInfoForPosVO.setConnectors(connectorIdList.stream().distinct().collect(Collectors.toList()));
                }

                deviceInfoForPosVO.setPileType(relationEntity.getPowerType());
                deviceInfoForPosVO.setOverchargingPileFlag(OverchargingPileFlagEnum.NON_OVERCHARGING_PILE.getCode());
                deviceInfoForPosVO.setLocationUpdatedAt(opLocationEntity.getUpdatedAt());
                deviceInfoForPosVO.setDeviceUpdatedAt(relationEntity.getUpdateTime());
                deviceInfoForPosVO.setHavePermission(havePermissionLocationIdSet.contains(locationId));
                deviceInfoForPosVOList.add(deviceInfoForPosVO);
            } else {
                List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(relationEntity.getSn(), sellerId);
                if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(terminalEntityList)) {
                    continue;
                }

                for (ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity : terminalEntityList) {
                    DeviceInfoForPosVO deviceInfoForPosVO = new DeviceInfoForPosVO();
                    deviceInfoForPosVO.setPileId(relationEntity.getId());
                    deviceInfoForPosVO.setTerminalId(chargePointMerchantTerminalEntity.getId());
                    deviceInfoForPosVO.setLocationId(opLocationEntity.getId());
                    deviceInfoForPosVO.setLocationName(opLocationEntity.getName());
                    deviceInfoForPosVO.setPileName(relationEntity.getName());
                    deviceInfoForPosVO.setPileSn(pileSn);
                    deviceInfoForPosVO.setTerminalName(chargePointMerchantTerminalEntity.getTerminalName());
                    deviceInfoForPosVO.setTerminalSn(chargePointMerchantTerminalEntity.getTerminalSn());

                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntity.getConnectorsList())) {
                        List<Connector> connectors = chargePointMerchantTerminalEntity.getConnectorsList();
                        List<Integer> connectorIdList = new ArrayList<>();
                        connectors.forEach(item -> connectorIdList.add(item.getConnectorId()));
                        deviceInfoForPosVO.setConnectors(connectorIdList.stream().distinct().collect(Collectors.toList()));
                    }

                    deviceInfoForPosVO.setPileType(relationEntity.getPowerType());
                    deviceInfoForPosVO.setOverchargingPileFlag(OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode());
                    deviceInfoForPosVO.setLocationUpdatedAt(opLocationEntity.getUpdatedAt());
                    deviceInfoForPosVO.setDeviceUpdatedAt(chargePointMerchantTerminalEntity.getUpdateTime());
                    deviceInfoForPosVO.setHavePermission(havePermissionLocationIdSet.contains(locationId));
                    deviceInfoForPosVOList.add(deviceInfoForPosVO);
                }
            }
        }
        return deviceInfoForPosVOList;
    }

    @Override
    public List<SynchronizeDeviceInfoForPosVO> synchronizeDeviceInfoForPos(SynchronizeDeviceInfoForPosDTO synchronizeDeviceInfoForPosDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.synchronizeDeviceInfoForPos synchronizeDeviceInfoForPosDTO : {}",
                JSON.toJSONString(synchronizeDeviceInfoForPosDTO));

        if (synchronizeDeviceInfoForPosDTO == null) {
            return null;
        }

        Long sellerId = synchronizeDeviceInfoForPosDTO.getSellerId();
        List<DeviceInfoDTO> deviceInfoDTOList = synchronizeDeviceInfoForPosDTO.getDeviceInfoDTOList();
        if (sellerId == null
                || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(deviceInfoDTOList)) {
            return null;
        }

        List<String> pileSnList = deviceInfoDTOList
                .stream()
                .map(DeviceInfoDTO::getPileSn)
                .distinct()
                .collect(Collectors.toList());

        List<ChargePointMerchantRelationEntity> pileList = this.getPileList(pileSnList, sellerId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileList)) {
            return null;
        }

        Map<String, List<ChargePointMerchantRelationEntity>> pileSnAndChargePointMerchantRelationEntityListMap = pileList
                .stream()
                .collect(Collectors.groupingBy(ChargePointMerchantRelationEntity::getSn));

        List<String> terminalSnList = deviceInfoDTOList
                .stream()
                .filter(val -> org.apache.commons.lang3.StringUtils.isNotBlank(val.getTerminalSn()))
                .map(DeviceInfoDTO::getTerminalSn)
                .distinct()
                .collect(Collectors.toList());
        Map<String, List<ChargePointMerchantTerminalEntity>> terminalSnAndChargePointMerchantTerminalEntityListMap = new HashMap<>();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalSnList)) {
            List<ChargePointMerchantTerminalEntity> terminalList = chargePointMerchantTerminalService.getTerminalList(terminalSnList, sellerId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalList)) {
                terminalSnAndChargePointMerchantTerminalEntityListMap = terminalList
                        .stream()
                        .collect(Collectors.groupingBy(ChargePointMerchantTerminalEntity::getTerminalSn));
            }
        }

        List<SynchronizeDeviceInfoForPosVO> resultList = new ArrayList<>();
        for (DeviceInfoDTO deviceInfoDTO : deviceInfoDTOList) {

            if (deviceInfoDTO == null
                    || org.apache.commons.lang3.StringUtils.isBlank(deviceInfoDTO.getPileSn())) {
                continue;
            }

            String pileSn = deviceInfoDTO.getPileSn();
            String terminalSn = deviceInfoDTO.getTerminalSn();

            SynchronizeDeviceInfoForPosVO synchronizeDeviceInfoForPosVO = new SynchronizeDeviceInfoForPosVO();

            synchronizeDeviceInfoForPosVO.setPileSn(pileSn);
            synchronizeDeviceInfoForPosVO.setOverchargingPileFlag(deviceInfoDTO.getOverchargingPileFlag());
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileSnAndChargePointMerchantRelationEntityListMap.get(pileSn))) {
                synchronizeDeviceInfoForPosVO.setPileName(pileSnAndChargePointMerchantRelationEntityListMap.get(pileSn).get(0).getName());
            }

            synchronizeDeviceInfoForPosVO.setTerminalSn(terminalSn);
            if (org.apache.commons.lang3.StringUtils.isNotBlank(terminalSn)
                    && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalSnAndChargePointMerchantTerminalEntityListMap.get(terminalSn))) {
                synchronizeDeviceInfoForPosVO.setTerminalName(terminalSnAndChargePointMerchantTerminalEntityListMap.get(terminalSn).get(0).getTerminalName());
            }

            resultList.add(synchronizeDeviceInfoForPosVO);
        }
        return resultList;
    }


    @Override
    public List<ChargePointVO> findByFuzzNameOrSN(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId) {

        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        String keyword = chargePointQueryDTO.getKeyword();
        boolean condition = org.apache.commons.lang3.StringUtils.isNotBlank(keyword);
        List<String> matchFields = Collections.singletonList("ProductNamePdm");
        boolean productNamePdmCondition = matchFields.contains("ProductNamePdm") && condition;
        queryWrapper.and(condition,
                chargePointEntityLambdaQueryWrapper -> chargePointEntityLambdaQueryWrapper.like(condition, ChargePointMerchantRelationEntity::getSn, keyword)
                        .or().like(condition, ChargePointMerchantRelationEntity::getName, keyword)
                        .or().like(productNamePdmCondition, ChargePointMerchantRelationEntity::getProductNamePdm, keyword));
        queryWrapper.orderByDesc(ChargePointMerchantRelationEntity::getId);
        queryWrapper.eq(StringUtils.isNotBlank(chargePointQueryDTO.getPowerType()), ChargePointMerchantRelationEntity::getPowerType, chargePointQueryDTO.getPowerType());
        queryWrapper.eq(Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()), ChargePointMerchantRelationEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus());
        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
        List<ChargePointVO> chargePointVOS = ChargePointMerchantRelationTypeMapper.INSTANCE.entityList2VO(chargePointEntities);
        for (ChargePointVO chargePointVO : chargePointVOS) {
            chargePointVO.setSellerId(merchantId);
            ChargePointMerchantRelationEntity relationOwner = findRelation(chargePointVO.getSn(), MerchantChargePointRelationEnum.OWNER);
            if (Objects.nonNull(relationOwner)) {
                chargePointVO.setOwnerName(pileUserServiceAdapter.getMerchantByIdSupportFallback(relationOwner.getMerchantId()).getName());
                chargePointVO.setOwner(relationOwner.getMerchantId());
            }
            ChargePointMerchantRelationEntity relationMaintenance = findRelation(chargePointVO.getSn(), MerchantChargePointRelationEnum.MAINTENANCE);
            if (Objects.nonNull(relationMaintenance)) {
                chargePointVO.setMaintenanceName(pileUserServiceAdapter.getMerchantByIdSupportFallback(relationMaintenance.getMerchantId()).getName());
                chargePointVO.setMaintenance(relationMaintenance.getMerchantId());
            }
            chargePointVO.setThirdPart(!chargePointVO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode())));
        }
        return chargePointVOS;
    }

    @Override
    public ChargePointRecordVO record(Long id, Long merchantId) {
        ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectById(id);
        if (Objects.isNull(chargePointEntity)) {
            return null;
        }
        ChargePointRecordVO chargePointRecordVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2RecordVO(chargePointEntity);
        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointEntity.getOverchargingPileFlag())) {
            List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(chargePointEntity.getSn(), chargePointEntity.getMerchantId());
            chargePointRecordVO.setTerminals(this.buildTerminals(terminalEntityList));
        }
        return chargePointRecordVO;
    }

    private List<Terminal> buildTerminals(List<ChargePointMerchantTerminalEntity> terminalEntityList) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.buildTerminals terminalEntityList : {}",
                JSON.toJSONString(terminalEntityList));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(terminalEntityList)) {
            return null;
        }

        List<Terminal> terminals = new ArrayList<>();
        terminalEntityList.forEach(val -> {
            Terminal terminal = new Terminal();
            terminal.setTerminalSn(val.getTerminalSn());
            terminal.setPin(val.getTerminalPin());
            terminal.setTerminalName(val.getTerminalName());
            terminal.setConnectors(val.getConnectorsList());
            terminals.add(terminal);
        });

        Collections.sort(terminals);
        return terminals;
    }

    @Override
    public ChargePointDetailVO detail(Long id, Long merchantId) {
        ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectById(id);
        if (Objects.isNull(chargePointEntity)) {
            return null;
        }
        ChargePointDetailVO chargePointDetailVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2VO(chargePointEntity);

        String timeOfDelivery = pileDeviceServiceAdapter.getTimeOfDelivery(chargePointEntity.getSn());
        chargePointDetailVO.setDeliverTime(timeOfDelivery);

        String bindTimeDateFormat = TimeZoneUtil.format(chargePointEntity.getBindTime(), chargePointEntity.getZoneId(), TimeZoneUtil.DATE_TIME_PATTERN);
        chargePointDetailVO.setBoundTime(org.apache.commons.lang3.StringUtils.isBlank(bindTimeDateFormat) ? "--" : bindTimeDateFormat);

        chargePointDetailVO.setTimeOfFirstConnectToInternet(smartBIServiceAdapter.getTimeOfFirstConnectToInternet(chargePointEntity.getSn()));
        chargePointDetailVO.setBoundStatus(queryBoundStatus(chargePointEntity.getSn()));
        chargePointDetailVO.setImei(smartBIServiceAdapter.getIMEI(chargePointEntity.getSn()));
        chargePointDetailVO.setMac(pileDeviceServiceAdapter.getDeviceMac(chargePointEntity.getSn()));
        com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileInfo = pileDeviceServiceAdapter.loadPileDetail(chargePointEntity.getSn());
        String businessAttr = Optional.ofNullable(chargePileInfo).map(info -> String.valueOf(info.getUsageScenario())).orElse("--");
        chargePointDetailVO.setBusinessAttributes(businessAttr);
        OpLocationDTO locationByPileSn = opLocationRepository.getLocationByPileSn(chargePointEntity.getSn());
        log.info("opLocationRepository.getLocationByPileSn {}", JSON.toJSONString(locationByPileSn));
        if (Objects.nonNull(locationByPileSn)) {
            chargePointDetailVO.setCountry(locationByPileSn.getCountry());
            chargePointDetailVO.setBusinessAttributes("1");
        }
        return chargePointDetailVO;
    }

    @Override
    public ChargePointDetailVO detailV2(String sn, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.detailV2 sn : {} and merchantId : {}",
                JSON.toJSONString(sn),
                JSON.toJSONString(merchantId));

        if (merchantId == null
                || org.apache.commons.lang3.StringUtils.isBlank(sn)) {
            return null;
        }

        Set<String> deviceSnSet = new HashSet<>();
        deviceSnSet.add(sn);
        Map<String, Integer> deviceSnAndDeviceTypeEnumMap = this.getDeviceSnAndDeviceTypeEnumMap(deviceSnSet, merchantId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(deviceSnAndDeviceTypeEnumMap)
                || deviceSnAndDeviceTypeEnumMap.get(sn) == null) {
            return null;
        }

        String hostSn = null;
        String terminalPin = null;
        Integer deviceTypeEnum = deviceSnAndDeviceTypeEnumMap.get(sn);
        if (!DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceTypeEnum)) {
            hostSn = sn;
        } else {
            List<String> terminalSnList = new ArrayList<>();
            terminalSnList.add(sn);
            List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalList(terminalSnList, merchantId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalEntityList)) {
                hostSn = terminalEntityList.get(0).getHostSn();
                terminalPin = terminalEntityList.get(0).getTerminalPin();
            }
        }

        if (org.apache.commons.lang3.StringUtils.isBlank(hostSn)) {
            return null;
        }

        ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectOne(new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .eq(ChargePointMerchantRelationEntity::getSn, hostSn)
                .eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId));

        if (chargePointEntity == null) {
            return null;
        }

        ChargePointDetailVO chargePointDetailVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2VO(chargePointEntity);
        if (DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceTypeEnum)) {
            chargePointDetailVO.setPin(terminalPin);
        }

        String timeOfDelivery = pileDeviceServiceAdapter.getTimeOfDelivery(sn);
        chargePointDetailVO.setDeliverTime(timeOfDelivery);

        String bindTimeDateFormat = TimeZoneUtil.format(chargePointEntity.getBindTime(), chargePointEntity.getZoneId(), TimeZoneUtil.DATE_TIME_PATTERN);
        chargePointDetailVO.setBoundTime(org.apache.commons.lang3.StringUtils.isBlank(bindTimeDateFormat) ? "--" : bindTimeDateFormat);

        chargePointDetailVO.setTimeOfFirstConnectToInternet(smartBIServiceAdapter.getTimeOfFirstConnectToInternet(sn));
        chargePointDetailVO.setBoundStatus(queryBoundStatus(chargePointEntity.getSn()));
        chargePointDetailVO.setImei(smartBIServiceAdapter.getIMEI(sn));
        chargePointDetailVO.setMac(pileDeviceServiceAdapter.getDeviceMac(sn));
        com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileInfo = pileDeviceServiceAdapter.loadPileDetail(chargePointEntity.getSn());
        String businessAttr = Optional.ofNullable(chargePileInfo).map(info -> String.valueOf(info.getUsageScenario())).orElse("--");
        chargePointDetailVO.setBusinessAttributes(businessAttr);
        OpLocationDTO locationByPileSn = opLocationRepository.getLocationByPileSn(chargePointEntity.getSn());

        log.info("===>>> ChargePointMerchantRelationServiceImpl.detailV2 locationByPileSn : {}",
                JSON.toJSONString(locationByPileSn));

        if (Objects.nonNull(locationByPileSn)) {
            chargePointDetailVO.setCountry(locationByPileSn.getCountry());
            chargePointDetailVO.setBusinessAttributes("1");
        }
        return chargePointDetailVO;
    }

    @Override
    public ChargePointDetailVO detailBySn(String pileSn) {
        Long merchantId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                .eq(ChargePointMerchantRelationEntity::getSn, pileSn)
                .eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);

        ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
        if (Objects.isNull(chargePointEntity)) {
            return null;
        }
        ChargePointDetailVO chargePointDetailVO = ChargePointMerchantRelationTypeMapper.INSTANCE.entity2VO(chargePointEntity);

        // IMEI 缓存一天 云端没有修改入口，只有录入信息
        chargePointDetailVO.setImei(getPileIMEI(chargePointEntity.getSn()));

        // 设备mac地址 缓存一天 云端没有修改入口，只有录入信息
        chargePointDetailVO.setMac(getPileMac(chargePointEntity.getSn()));

        // 固件版本，以及是否可升级
        chargePointDetailVO.setFirmwareUpdateStatus(0);
        Result<Page<FirmwareRecordDetailsVO>> pageResult = opsMgmtClient.recordDetailPage(chargePointEntity.getSn(), 1, 10, 0);
        if (Objects.nonNull(pageResult) && Objects.nonNull(pageResult.getData()) && Integer.valueOf(cn.hutool.http.HttpStatus.HTTP_OK).equals(pageResult.getCode()) && !CollectionUtils.isEmpty(pageResult.getData().getRecords())) {
            FirmwareRecordDetailsVO firmwareRecordDetailsVO = pageResult.getData().getRecords().get(0);
            chargePointDetailVO.setFirmwareVersion(firmwareRecordDetailsVO.getInitialVersion());
            chargePointDetailVO.setFirmwareUpdateStatus(firmwareRecordDetailsVO.getFirmwareUpdateStatus());
        }

        OpLocationDTO locationByPileSn = opLocationRepository.getLocationByPileSn(chargePointEntity.getSn());
        log.info("opLocationRepository.getLocationByPileSn {}", JSON.toJSONString(locationByPileSn));
        if (Objects.nonNull(locationByPileSn)) {
            chargePointDetailVO.setCountry(locationByPileSn.getCountry());
            chargePointDetailVO.setBusinessAttributes("1");
        }
        return chargePointDetailVO;
    }

    /**
     * 获取桩详情的imei
     * @param sn
     * @return
     */
    private String getPileIMEI(String sn) {
        String imei;
        String redisIMEI = redis2Util.get(String.format(RedisKeyConstant.PILE_DETAILS_IMEI_KEY, sn));
        log.info("get imei {}--->{}",sn,redisIMEI);
        if (StringUtils.isBlank(redisIMEI)) {
            imei = smartBIServiceAdapter.getIMEI(sn);
            redis2Util.set(String.format(RedisKeyConstant.PILE_DETAILS_IMEI_KEY, sn), "imei:" + (StringUtils.isBlank(imei) ? "" : imei), 1L, TimeUnit.DAYS);
            log.info("set imei {}--->{}",sn,redisIMEI);
        } else {
            imei = redisIMEI.substring(5);
        }
        return imei;
    }

    /**
     * 获取桩详情的mac地址
     * @param sn
     * @return
     */
    private String getPileMac(String sn) {
        String mac;
        String redidMac = redis2Util.get(String.format(RedisKeyConstant.PILE_DETAILS_MAC_KEY, sn));
        log.info("get mac {}--->{}",sn,redidMac);
        if (StringUtils.isBlank(redidMac)) {
            mac = pileDeviceServiceAdapter.getDeviceMac(sn);
            redis2Util.set(String.format(RedisKeyConstant.PILE_DETAILS_MAC_KEY, sn), "mac:" + (StringUtils.isBlank(mac) ? "" : mac), 1L, TimeUnit.DAYS);
            log.info("set mac {}--->{}",sn,redidMac);
        } else {
            mac = redidMac.substring(4);
        }
        return mac;
    }

    @Override
    public OpLocationPileEvseElasticDTO modifyPileEvseId(String pileEvseId,String pileSn){
        try {
            OpLocationPileEvseElasticDTO2 opLocationPileEvseElasticDTO2 = opLocationPileEvseElastic2.findByPileSn(pileSn);
            log.info("===>>>opLocationPileEvseElasticDTO2:{}", JSONObject.toJSONString(opLocationPileEvseElasticDTO2));
            if ((opLocationPileEvseElasticDTO2 != null) && opLocationPileEvseElasticDTO2.getId().equals(pileEvseId)) {
                //再根据场站id和桩sn查这个桩的id
                LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                        .eq(OpLocationPileEvseEntity::getLocationId, opLocationPileEvseElasticDTO2.getLocationId())
                        .eq(OpLocationPileEvseEntity::getDeleted, false)
                        .eq(OpLocationPileEvseEntity::getPileSn, opLocationPileEvseElasticDTO2.getPileSn());
                List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
                log.info("===>>>opLocationPileEvseEntities:{}", opLocationPileEvseEntities.size());
                if (!CollectionUtils.isEmpty(opLocationPileEvseEntities)) {
                    Long id = opLocationPileEvseEntities.get(0).getId();
                    OpLocationPileEvseElasticDTO dto = new OpLocationPileEvseElasticDTO();
                    BeanUtils.copyProperties(opLocationPileEvseElasticDTO2, dto);
                    dto.setId(id);
                    log.info("===>>>dto:{}", JSONObject.toJSONString(dto));
                    if (StringUtils.isNotBlank(dto.getPileSn()) && (dto.getLocationId() != null)) {
                        opLocationPileEvseElastic2.deleteById(pileEvseId);
                        opLocationPileEvseElastic.save(dto);
                        return dto;
                    }
                }
            }

            return null;
        } catch (Exception e) {
            e.printStackTrace();
            throw new MessageCodeException(PileBaseEnum.LOCATION_NAME_REPEATED);
        }
    }

    @Override
    public List<ChargePointSourceVO> findChargePointSource(ChargePointSourceFuzzQueryDTO chargePointQueryDTO, Long merchantId) {
        String value = redis2Util.get(RedisKeyConstant.getPileBaseAddLocationKey(merchantId));
        if (org.springframework.util.StringUtils.hasText(value)) {
            log.error( "lock by other {}={}", RedisKeyConstant.getPileBaseAddLocationKey(merchantId), value);
//            throw new MessageCodeException(PileBaseEnum.ADDING_PILE_TO_LOCATION_WAIT_TO_ADD);
            throw new MessageCodeException("response.failure.message");// 产品 蔺成成 说给操作失败
        }
        StopWatch stopWatch = new StopWatch("查询可加入场站的桩性能分析");
        stopWatch.start("查询统一桩管理本商家的桩");
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        String keyword = chargePointQueryDTO.getKeyword();
        boolean condition = org.apache.commons.lang3.StringUtils.isNotBlank(keyword);
        List<String> matchFields = Collections.singletonList("ProductNamePdm");
        boolean productNamePdmCondition = matchFields.contains("ProductNamePdm") && condition;
        queryWrapper.orderByDesc(ChargePointMerchantRelationEntity::getBindTime);
        queryWrapper.and(condition,
                chargePointEntityLambdaQueryWrapper -> chargePointEntityLambdaQueryWrapper.like(condition, ChargePointMerchantRelationEntity::getSn, keyword)
                        .or().like(condition, ChargePointMerchantRelationEntity::getName, keyword)
                        .or().like(productNamePdmCondition, ChargePointMerchantRelationEntity::getProductNamePdm, keyword));

        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);// 统一桩管理里面的桩结果
        if (org.springframework.util.CollectionUtils.isEmpty(chargePointEntities)) {
            return Collections.emptyList();
        }
        stopWatch.stop();
        stopWatch.start("查询已经进入场站的桩");
        List<String> collect = chargePointEntities.stream().map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());
        // pile_base_op_location_pile_evse_index    对应    op_location_pile_evse    桩信息
        //  加入场站的桩不再显示
        LambdaQueryWrapper<OpLocationPileEvseEntity> query = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        query.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
        query.in(OpLocationPileEvseEntity::getPileSn, collect);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(query);
        Set<String> joinLocationPiles = opLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toSet());

        // pile_base_op_location_evse_index       对应  op_location_evse             枪信息
        stopWatch.stop();
        stopWatch.start("检查并同步ES桩信息");
        AtomicBoolean sync = checkAndSync(opLocationPileEvseEntities);
        stopWatch.stop();
        stopWatch.start("是否有同步:" + sync.get());
        if (sync.get()) {
            joinLocationPiles.clear();
            List<OpLocationPileEvseEntity>  syncedOpLocationPileEvseEntities = opLocationPileEvseMapper.selectList(query);
            joinLocationPiles.addAll(syncedOpLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toSet()));
        }
        stopWatch.stop();

        stopWatch.start("过滤调家装绑定的sn");
        Set<String> bindByHomeSNs = pileHomeServiceAdapter.batchQueryBind(collect);
        joinLocationPiles.addAll(bindByHomeSNs);
        stopWatch.stop();
        stopWatch.start("转换对象并移除已经进入场站的sn:" + sync.get());
        List<ChargePointSourceVO> chargePointVOS = ChargePointMerchantRelationTypeMapper.INSTANCE.entities2SourcesVO(chargePointEntities);
        chargePointVOS.removeIf(chargePointVO -> joinLocationPiles.contains(chargePointVO.getSn()));
        stopWatch.stop();


        log.info(stopWatch.prettyPrint());
        return chargePointVOS;
    }

    public OpLocationPileEvseElasticDTO modifyPileEvseId(String pileEvseId){
        SearchRequest request = new SearchRequest("pile_base_op_location_pile_evse_index");
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.matchPhraseQuery(BaseConstant.ID, pileEvseId));
        searchSourceBuilder.query(boolQueryBuilder);

        request.source(searchSourceBuilder);
        try {
            SearchResponse response = restHighLevelClient.search(request, RequestOptions.DEFAULT);
            org.elasticsearch.search.SearchHit[] hits = response.getHits().getHits();
            if((hits != null) && (hits.length > 0)){
                OpLocationPileEvseElasticDTO2 opLocationPileEvseElasticDTO2 = JSON.parseObject(hits[0].getSourceAsString(), OpLocationPileEvseElasticDTO2.class);
                log.info("===>>>opLocationPileEvseElasticDTO2:{}", JSONObject.toJSONString(opLocationPileEvseElasticDTO2));
                if (opLocationPileEvseElasticDTO2 != null){
                    //再根据场站id和桩sn查这个桩的id
                    LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                            .eq(OpLocationPileEvseEntity::getLocationId, opLocationPileEvseElasticDTO2.getLocationId())
                            .eq(OpLocationPileEvseEntity::getPileSn, opLocationPileEvseElasticDTO2.getPileSn());
                    List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
                    log.info("===>>>opLocationPileEvseEntities:{}",opLocationPileEvseEntities.size());
                    if (!CollectionUtils.isEmpty(opLocationPileEvseEntities)){
                        Long id = opLocationPileEvseEntities.get(0).getId();
                        OpLocationPileEvseElasticDTO dto = new OpLocationPileEvseElasticDTO();
                        BeanUtils.copyProperties(opLocationPileEvseElasticDTO2,dto);
                        dto.setId(id);
                        log.info("===>>>dto:{}", JSONObject.toJSONString(dto));
                        if(StringUtils.isNotBlank(dto.getPileSn()) && (dto.getLocationId() != null)) {
                            opLocationPileEvseElastic.save(dto);
                            opLocationPileEvseElastic2.deleteById(pileEvseId);
                            return dto;
                        }
                    }
                }
            }
            return null;
        } catch (Exception e) {
            e.printStackTrace();
//            throw new BusinessException(ES_REQUEST_FAILED);
            throw new MessageCodeException(PileBaseEnum.LOCATION_NAME_REPEATED);
        }

    }

    /**
     *
     * @param opLocationPileEvseEntities  已经存在场站里面的桩（MySQL中的数据）
     * @return
     */
    private AtomicBoolean checkAndSync(List<OpLocationPileEvseEntity> opLocationPileEvseEntities) {
        AtomicBoolean sync = new AtomicBoolean(Boolean.FALSE);
        Set<Long> collect = opLocationPileEvseEntities.stream().map(OpLocationPileEvseEntity::getLocationId).collect(Collectors.toSet());
        List<OpLocationPileEvseElasticDTO2> allByLocationIdIn = opLocationPileEvseElastic2.findAllByLocationIdIn(collect);
        Map<Long, List<OpLocationPileEvseElasticDTO2>> locationPiles = allByLocationIdIn.stream().collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO2::getLocationId));
        for (OpLocationPileEvseEntity pileEvse : opLocationPileEvseEntities) {
            if (!locationPiles.containsKey(pileEvse.getLocationId())) { // es 中没有 但是 mysql中有数据 需要同步
                doSync(sync, pileEvse);
            } else {
                Set<String> pileSNs = locationPiles.get(pileEvse.getLocationId()).stream().map(OpLocationPileEvseElasticDTO2::getPileSn).collect(Collectors.toSet());
                if (!pileSNs.contains(pileEvse.getPileSn())) {   // es 中没有 但是 mysql中有数据 需要同步
                    doSync(sync, pileEvse);
                }
            }
        }
        return sync;
    }

    private void doSync(AtomicBoolean sync, OpLocationPileEvseEntity pileEvse) {
        log.warn("mysql es 数据不同步 直接删除mysql 桩数据: {}", JSON.toJSONString(pileEvse));
        sync.compareAndSet(Boolean.FALSE, Boolean.TRUE);
        OpLocationPileEvseEntity entity = opLocationPileEvseMapper.selectById(pileEvse.getId());
        if (Objects.isNull(entity)) {
            return;
        }
        entity.setUpdatedAt(System.currentTimeMillis());
        entity.setDeleted(1);
        opLocationPileEvseMapper.updateById(entity);
        opLocationPileEvseElastic.deleteById(entity.getId());
        String evseList = pileEvse.getEvseList();
        if (org.springframework.util.StringUtils.hasText(evseList)) {
            List<Long> evseIds = JSON.parseArray(evseList, Long.class);
            for (Long evseId : evseIds) {
                log.warn("mysql es 数据不同步 直接删除mysql 枪数据: {}", JSON.toJSONString(pileEvse));
                opLocationEvseMapper.deleteByEvseId(evseId);
                opLocationEvseElastic.deleteById(evseId);
            }
        }
    }

    @Override
    @Transactional
    public Boolean license(List<ChargePointLicenseDTO> chargePointLicenseDTOList, Long merchantId, Long userId) {
        if (org.springframework.util.CollectionUtils.isEmpty(chargePointLicenseDTOList)) {
            log.warn("{} list is empty !", JSON.toJSONString(chargePointLicenseDTOList));
            return Boolean.FALSE;
        }
        Map<String, List<ChargePointLicenseDTO>> snLicenseGroups = chargePointLicenseDTOList.stream().collect(Collectors.groupingBy(ChargePointLicenseDTO::getSn));

        Map<String, String> collect = chargePointLicenseDTOList.stream().collect(Collectors.toMap(ChargePointLicenseDTO::getLicense, ChargePointLicenseDTO::getSn));
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
        queryWrapper.in(TbLenBindRelationEntity::getLicenceCode, collect.keySet());
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        Set<String> licenses = tbLenBindRelationEntities.stream().map(TbLenBindRelationEntity::getLicenceCode).collect(Collectors.toSet());
        if (collect.keySet().size() > licenses.size()) {
            Set<String> notExistLicenseCodeSet = collect.keySet().stream().filter(license -> !licenses.contains(license)).collect(Collectors.toSet());
            String notExistLicenseCodes = String.join(", ", notExistLicenseCodeSet);
            throw new MessageCodeException(PileBaseEnum.LICENSE_CODE_NOT_EXIST, new Object[]{notExistLicenseCodes});
        }

        // 2.8 优化参数校验 绑定时，如果licence有类型，判断是否与桩的类型匹配
        chargeTypeCheck(chargePointLicenseDTOList, tbLenBindRelationEntities, merchantId);


        //  当桩有两个枪时 不应该在本次绑定license 累加 时长
        Map<String, TbLenBindRelationEntity> entityMap = new HashMap<>();
        for (TbLenBindRelationEntity entity : tbLenBindRelationEntities) {


            if (entity.getStatus().equals(EFFECTIVE_LICENSE) || org.apache.commons.lang3.StringUtils.isNotBlank(entity.getPileSn())) {
                throw new MessageCodeException(PileBaseEnum.LICENSE_USED, new Object[]{entity.getLicenceCode()});
            }
            if (entity.getStatus().equals(INVALIDITY_LICENSE)) {
                throw new MessageCodeException(PileBaseEnum.LICENSE_EXPIRE, new Object[]{entity.getLicenceCode()});
            }
            entity.setPileSn(collect.get(entity.getLicenceCode()));
            entity.setStatus(EFFECTIVE_LICENSE);
            TbLenBindRelationEntity calculatedTimeEntity = entityMap.get(entity.getPileSn());// 本次有没有计算过生效时间如果计算过那么直接使用
            if (Objects.nonNull(calculatedTimeEntity)) {
                entity.setAvailableTime(calculatedTimeEntity.getAvailableTime());
                entity.setUnavailableTime(calculatedTimeEntity.getUnavailableTime());
            } else {
                entity.setAvailableTime(calculateAvailableTime(entity));
                entity.setUnavailableTime(calculateUnavailableTime(entity));
            }
            entity.setUpdateTime(System.currentTimeMillis());
            entity.setBindTime(entity.getUpdateTime());
            Optional.ofNullable(userId).ifPresent(aLong -> entity.setUpdateBy(aLong.toString()));
            LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryCpWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
            queryCpWrapper.eq(ChargePointMerchantRelationEntity::getSn, entity.getPileSn());
            queryCpWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
            ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectOne(queryCpWrapper);

            ChargePointMerchantTerminalEntity chargePointTerminalEntity = null;
            if (Objects.isNull(chargePointEntity)) {
                //绑定的设备是终端
                LambdaQueryWrapper<ChargePointMerchantTerminalEntity> queryTerminalWrapper = Wrappers.lambdaQuery(ChargePointMerchantTerminalEntity.class);
                queryTerminalWrapper.eq(ChargePointMerchantTerminalEntity::getTerminalSn, entity.getPileSn());
                queryTerminalWrapper.eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId);
                chargePointTerminalEntity = chargePointMerchantTerminalMapper.selectOne(queryTerminalWrapper);
            }
            if ("gun".equalsIgnoreCase(entity.getMeasureUnit())) {
                List<Connector> connectors;
                if (Objects.nonNull(chargePointEntity)) {
                    connectors = chargePointEntity.getConnectors();
                } else {
                    connectors = chargePointTerminalEntity.getConnectorsList();
                }
                // 校验 该桩的枪数量是否满足
                String sn = collect.get(entity.getLicenceCode());
                if (CollectionUtil.isNotEmpty(connectors)) {
                    int size = connectors.size();
                    if (size != snLicenseGroups.get(sn).size()) {
                        log.error("sn: {} has {} guns but licence amount= {} {}", sn, size, snLicenseGroups.get(sn).size(), JSON.toJSONString(snLicenseGroups.get(sn)));
                        throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
                    }
                }
            }
            tbLenBindRelationMapper.updateById(entity);
            // 更新 license 绑定完之后才能更新状态
            if (Objects.nonNull(chargePointEntity)) {
                updateSubStatus(chargePointEntity, merchantId, entity.getGoodsId());
            } else {
                updateSubStatus(chargePointTerminalEntity, merchantId);
            }
            // updateSubStatus(entity.getUnavailableTime(), SubStatus.EFFECTIVE.getStatus(), merchantId, chargePointEntity);
            entityMap.putIfAbsent(entity.getPileSn(), entity);
        }
        // 这里不会混用，判断一个即可
        TbLenBindRelationEntity tbLenBindRelationEntity = tbLenBindRelationEntities.get(0);
        String serviceId = tbLenBindRelationEntity.getServiceId();
        List<String> functionListByBenefitId = subscribePileRightsService.getFunctionListByBenefitId(serviceId);
        if (CollectionUtil.isNotEmpty(functionListByBenefitId) && functionListByBenefitId.contains(PileChargingRights.GUN_SEARCH)) {
            UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO = new UpdateEsSubscriptionStatusDTO();
            List<String> sns = tbLenBindRelationEntities.stream().map(TbLenBindRelationEntity::getPileSn).collect(Collectors.toList());
            updateEsSubscriptionStatusDTO.setPileSnList(sns);
            updateEsSubscriptionStatusDTO.setStatus(true);
            opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);
        }

        return Boolean.TRUE;
    }

    /**
     * description: chargeTypeCheck 如果参数
     * version: 1.0
     * date: 2024/5/21 14:49
     * author: A23204
     *
     * @param chargePointLicenseDTOList
     * @param tbLenBindRelationEntities
     * @return void
     */
    private void chargeTypeCheck(List<ChargePointLicenseDTO> chargePointLicenseDTOList, List<TbLenBindRelationEntity> tbLenBindRelationEntities, Long merchantId) {
        Set<String> collect = chargePointLicenseDTOList.stream().map(ChargePointLicenseDTO::getSn).collect(Collectors.toSet());

        // 查询桩
        List<ChargePointVO> bySNs = findBySNs(collect, merchantId);

        Map<String, ChargePointVO> snMap = bySNs.stream().collect(Collectors.toMap(ChargePointVO::getSn, Function.identity(), (oldValue, newValue) -> oldValue));
        Map<String, TbLenBindRelationEntity> licenceMap = tbLenBindRelationEntities.stream().collect(Collectors.toMap(TbLenBindRelationEntity::getLicenceCode, Function.identity(), (oldValue, newValue) -> oldValue));

        chargePointLicenseDTOList.forEach(item -> {
            String license = item.getLicense();
            String sn = item.getSn();
            ChargePointVO chargePointVO = snMap.get(sn);
            TbLenBindRelationEntity tbLenBindRelationEntity = licenceMap.get(license);
            if (chargePointVO != null && tbLenBindRelationEntity != null && !"NULL".equals(tbLenBindRelationEntity.getChargeType())) {
                if (!chargePointVO.getPowerType().equals(tbLenBindRelationEntity.getChargeType())) {
                    log.info("chargeTypeCheck failed. licence: {}, pileSn: {}", license, sn);
                    // LICENCE_PILE_NOT_MATCH
                    throw new MessageCodeException(PileBaseEnum.LICENCE_PILE_NOT_MATCH);
                }
            }
        });
    }

    private Long calculateAvailableTime(TbLenBindRelationEntity entity) {
        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryWrapper.eq(TbLenBindRelationEntity::getPileSn, entity.getPileSn());
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, entity.getTenantId());
        queryWrapper.eq(TbLenBindRelationEntity::getServiceId, entity.getServiceId());
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(queryWrapper);
        if (org.springframework.util.CollectionUtils.isEmpty(tbLenBindRelationEntities)) {
            return Objects.nonNull(entity.getAvailableTime()) ? entity.getAvailableTime() : System.currentTimeMillis();
        } else {
            Optional<TbLenBindRelationEntity> optional = tbLenBindRelationEntities.stream().max(Comparator.comparing(TbLenBindRelationEntity::getUnavailableTime));
            return optional.map(TbLenBindRelationEntity::getUnavailableTime).filter(t -> t > System.currentTimeMillis()).orElse(System.currentTimeMillis());
        }
    }

    private Long calculateUnavailableTime(TbLenBindRelationEntity entity) {
        Long availableTime = entity.getAvailableTime();
        Long unavailableTime = entity.getUnavailableTime();
        Integer serviceTime = entity.getServiceTime();
        String timeUnit = entity.getTimeUnit();
        String bonusDurationValue = entity.getBonusDurationValue();
        Integer bonusDurationTime = null;
        if (Objects.nonNull(bonusDurationValue)) {
            String[] s = bonusDurationValue.split(" ");
            bonusDurationTime = Integer.valueOf(s[0]);
        }
        if (Objects.nonNull(unavailableTime)) {
            if (availableTime < System.currentTimeMillis()) {
                return unavailableTime;
            } else {
                return availableTime + (unavailableTime - System.currentTimeMillis());
            }
        }
        if (Objects.isNull(timeUnit)) {
            log.error("timeUnit is null");
            return availableTime + serviceTime;
        } else if (timeUnit.toLowerCase().startsWith(YEAR)) {
            LocalDateTime localDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(availableTime), ZoneId.systemDefault());
            long l;
            if (bonusDurationTime != null) {
                l = localDateTime.plusYears(serviceTime).plusMonths(bonusDurationTime).plusDays(1).toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
            } else {
                l = localDateTime.plusYears(serviceTime).plusDays(1).toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
            }
            return l - 1;
        } else if (timeUnit.toLowerCase().startsWith(MONTH)) {
            LocalDateTime localDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(availableTime), ZoneId.systemDefault());
            long l;
            if (bonusDurationTime != null) {
                l = localDateTime.plusMonths(serviceTime).plusMonths(bonusDurationTime).plusDays(1).toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
            } else {
                l = localDateTime.plusMonths(serviceTime).plusDays(1).toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
            }
            return l - 1;
        } else if (timeUnit.toLowerCase().startsWith(DAY)) {
            LocalDateTime localDateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(availableTime), ZoneId.systemDefault());
            long l;
            if (bonusDurationTime != null) {
                l = localDateTime.plusMonths(bonusDurationTime).plusDays(serviceTime).plusDays(1).toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
            } else {
                l = localDateTime.plusDays(serviceTime).plusDays(1).toLocalDate().atStartOfDay().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
            }
            return l - 1;
        } else {
            return availableTime;
        }
    }

    /**
     * 查询绑定时间
     * 家桩有绑定状态   已绑定是指这个桩被某APP用户绑定了
     * 商桩是进场站的时间录入场站的时间如果大于0 代表绑定了场站
     */
    public Boolean queryBoundStatus(String sn) {
        try {
            com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO deviceDetail = pileDeviceServiceAdapter.getChargePileInfo(sn);
            if (UsageScenarioEnum.HOME_PILE.getCode().equals(deviceDetail.getUsageScenario())) {
                return pileHomeServiceAdapter.queryBind(sn);
            } else { // 非家桩绑定时间为添加到运营场站的时间
                Result<OpEvseInfoDTO> evseByEvseSn = opLocationEvseService.getEvseByEvseSn(sn + "_1");
                return ObjectUtils.isNotEmpty(evseByEvseSn)
                        && ObjectUtils.isNotEmpty(evseByEvseSn.getData())
                        && evseByEvseSn.getData().getCreatedAt() > 0L;
            }
        } catch (Exception e) {
            log.info("查询绑定时间出错", e);
        }
        return Boolean.FALSE;
    }


    @Override
    public IPage<ChargePointAssetVO> findChargePointsByPage(ChargePointFuzzQueryDTO chargePointQueryDTO, Long merchantId, String zoneId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.findChargePointsByPage chargePointQueryDTO : {} and merchantId : {} and zoneId : {}",
                JSON.toJSONString(chargePointQueryDTO),
                JSON.toJSONString(merchantId),
                JSON.toJSONString(zoneId));

        StopWatch stopWatch = new StopWatch("统一桩管理列表");

        stopWatch.start("查询统一桩列表");

        /*
        Page<ChargePointMerchantRelationEntity> pageInfo = new Page<>(chargePointQueryDTO.getPage(), chargePointQueryDTO.getPageSize());
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        String keyword = chargePointQueryDTO.getKeyword();
        boolean condition = org.apache.commons.lang3.StringUtils.isNotBlank(keyword);
        List<String> matchFields = Collections.singletonList("ProductNamePdm");
        boolean productNamePdmCondition = matchFields.contains("ProductNamePdm") && condition;
        queryWrapper.and(condition,
                chargePointEntityLambdaQueryWrapper -> chargePointEntityLambdaQueryWrapper.like(condition, ChargePointMerchantRelationEntity::getSn, keyword)
                        .or().like(condition, ChargePointMerchantRelationEntity::getName, keyword)
                        .or().like(productNamePdmCondition, ChargePointMerchantRelationEntity::getProductNamePdm, keyword));
        queryWrapper.eq(StringUtils.isNotBlank(chargePointQueryDTO.getPowerType()), ChargePointMerchantRelationEntity::getPowerType, chargePointQueryDTO.getPowerType());
        queryWrapper.eq(Objects.nonNull(chargePointQueryDTO.getSubscriptionStatus()), ChargePointMerchantRelationEntity::getSubStatus, chargePointQueryDTO.getSubscriptionStatus());
        //排序
        String orderBy = chargePointQueryDTO.getOrderBy();
        String orderType = chargePointQueryDTO.getOrderType();
        if (StringUtils.isNotEmpty(orderBy) && StringUtils.isNotEmpty(orderType) && "name".equals(orderBy)) {
            queryWrapper.orderBy(true, "ASC".equalsIgnoreCase(orderType), ChargePointMerchantRelationEntity::getName);
        } else {
            queryWrapper.orderByDesc(ChargePointMerchantRelationEntity::getBindTime).orderByDesc(ChargePointMerchantRelationEntity::getId);
        }
        Page<ChargePointMerchantRelationEntity> chargePointEntityPage = chargePointMerchantRelationMapper.selectPage(pageInfo, queryWrapper);
        */

        if (org.apache.commons.lang3.StringUtils.isNotBlank(chargePointQueryDTO.getKeyword())) {
            chargePointQueryDTO.setKeyword(CharUtils.handleChar(chargePointQueryDTO.getKeyword()));
        }

        Page<ChargePointAssetVO> chargePointVOPage = chargePointMerchantRelationMapper.getPage(new Page<>(chargePointQueryDTO.getPage(), chargePointQueryDTO.getPageSize()), chargePointQueryDTO, merchantId);

        stopWatch.stop();

        /*
        stopWatch.start("对象转换");
        Page<ChargePointAssetVO> chargePointVOPage = ChargePointMerchantRelationTypeMapper.INSTANCE.pageEntityList2AssetPageVO(chargePointEntityPage);
        stopWatch.stop();
        */

        stopWatch.start("查询桩的所属场站名称");
        if (!org.springframework.util.CollectionUtils.isEmpty(chargePointVOPage.getRecords())) {
            List<String> sns = chargePointVOPage.getRecords().stream().map(ChargePointAssetVO::getSn).collect(Collectors.toList());
            log.info("sns {}", JSON.toJSONString(sns));
            List<OpLocationPileEvseElasticDTO> locationPiles = opLocationPileEvseElastic.findByPileSnIn(sns);
            log.info("locationPiles {}", JSON.toJSONString(locationPiles));
            Map<String, String> pileLocationMap = new HashMap<>();
            if (Objects.nonNull(locationPiles) && !org.springframework.util.CollectionUtils.isEmpty(locationPiles)) {
                for (OpLocationPileEvseElasticDTO pileEvseElasticDTO : locationPiles) {
                    if (pileLocationMap.containsKey(pileEvseElasticDTO.getPileSn())) {
                        log.warn(" Duplicate key {}", pileEvseElasticDTO.getPileSn());
                    }
                    if (merchantId.equals(pileEvseElasticDTO.getOperatorId())) { // AB商家同时添加了同一个桩到资产，A添加该桩到场站，在B商家的账号能看到A商家的场站名称
                        pileLocationMap.put(pileEvseElasticDTO.getPileSn(), pileEvseElasticDTO.getLocationName());
                    }
                }
            }
            for (ChargePointAssetVO chargePointVO : chargePointVOPage.getRecords()) {
                chargePointVO.setLocationName(pileLocationMap.get(chargePointVO.getSn()));
                if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointVO.getOverchargingPileFlag())) {
                    List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(chargePointVO.getSn(), merchantId);
                    chargePointVO.setTerminals(this.buildTerminals(terminalEntityList));
                } else {
                    chargePointVO.setTerminals(null);
                }
                chargePointVO.setExceptionPromptFlag(this.getExceptionPromptFlag(chargePointVO));
            }
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return chargePointVOPage;
    }

    private Integer getExceptionPromptFlag(ChargePointAssetVO chargePointAssetVO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getExceptionPromptFlag chargePointAssetVO : {}",
                JSON.toJSONString(chargePointAssetVO));

        if (!OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointAssetVO.getOverchargingPileFlag())) {
            return 0;
        }

        String redisResult = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getPileSideReportsOverchargedPileDataKey(chargePointAssetVO.getSn()));
        if (org.apache.commons.lang3.StringUtils.isBlank(redisResult)) {
            return 0;
        }

        OverchargingPileDTO overchargingPileDTO = JSON.parseObject(redisResult, OverchargingPileDTO.class);

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getExceptionPromptFlag overchargingPileDTO : {}",
                JSON.toJSONString(overchargingPileDTO));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(overchargingPileDTO.getTerminalInfo())
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePointAssetVO.getTerminals())) {
            return 0;
        }

        if ((com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(overchargingPileDTO.getTerminalInfo())
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointAssetVO.getTerminals()))
                || (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(overchargingPileDTO.getTerminalInfo())
                && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePointAssetVO.getTerminals()))) {
            return 1;
        }

        List<Terminal> terminals = chargePointAssetVO.getTerminals();
        List<TerminalInfoDTO> terminalInfo = overchargingPileDTO.getTerminalInfo();
        if (terminals.size() != terminalInfo.size()) {
            return 1;
        }

        for (Terminal terminal : terminals) {
            boolean flag = false;
            for (TerminalInfoDTO terminalInfoDTO : terminalInfo) {
                if (terminal.getTerminalSn().equalsIgnoreCase(terminalInfoDTO.getTerminalSn())) {
                    List<Connector> connectors = terminal.getConnectors();
                    List<Integer> connectorId = terminalInfoDTO.getConnectorId();
                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(connectors)
                            && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(connectorId)) {
                        flag = true;
                        break;
                    }

                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(connectors)
                            && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(connectorId)) {
                        Collections.sort(connectorId);
                        List<Integer> connectorIdList = connectors
                                .stream()
                                .map(Connector::getConnectorId)
                                .sorted(Integer::compareTo)
                                .collect(Collectors.toList());
                        if (connectorId.equals(connectorIdList)) {
                            flag = true;
                            break;
                        }
                    }
                }
            }

            if (!flag) {
                return 1;
            }
        }
        return 0;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void remove(RemoveChargePointDTO chargePointDTO, Long merchantId) {
        log.info("remove pile, pile: {}", chargePointDTO.getSn());
        if (Objects.isNull(chargePointDTO) || (Objects.isNull(chargePointDTO.getId()) && Objects.isNull(chargePointDTO.getSn()))) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED);
        }
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(Objects.nonNull(chargePointDTO.getId()), ChargePointMerchantRelationEntity::getId, chargePointDTO.getId());
        lambdaQueryWrapper.eq(Objects.nonNull(chargePointDTO.getSn()), ChargePointMerchantRelationEntity::getSn, chargePointDTO.getSn());
        lambdaQueryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        ChargePointMerchantRelationEntity chargePointEntity = chargePointMerchantRelationMapper.selectOne(lambdaQueryWrapper);
        if (Objects.isNull(chargePointEntity)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }
        //根据pileSN校验桩是否已存在别的场站下
        LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
        queryWrapper.eq(OpLocationPileEvseEntity::getPileSn, Optional.ofNullable(chargePointDTO.getSn()).orElseGet(chargePointEntity::getSn));
        queryWrapper.inSql(OpLocationPileEvseEntity::getLocationId, "select id from op_location  where deleted = 0 and operator_id = " + merchantId);
        Integer integer = opLocationPileEvseMapper.selectCount(queryWrapper);
        if (integer > 0) {
            List<OpLocationPileEvseEntity> evseEntityList = opLocationPileEvseMapper.selectList(queryWrapper);
            for (OpLocationPileEvseEntity pileEvse : evseEntityList) {
                List<OpLocationEvseElasticDTO> allByLocationIdAndPileSn = opLocationEvseElastic.findAllByLocationIdAndPileSn(pileEvse.getLocationId(), pileEvse.getPileSn());
                if (CollectionUtils.isEmpty(allByLocationIdAndPileSn)) {
                    log.warn("mysql es 数据不同步 直接删除mysql 桩数据: {}", JSON.toJSONString(pileEvse));
                    OpLocationPileEvseEntity entity = opLocationPileEvseMapper.selectById(pileEvse.getId());
                    if (Objects.isNull(entity)) {
                        continue;
                    }
                    entity.setUpdatedAt(System.currentTimeMillis());
                    entity.setDeleted(1);
                    opLocationPileEvseMapper.updateById(entity);
                    String evseList = pileEvse.getEvseList();
                    if (org.springframework.util.StringUtils.hasText(evseList)) {
                        List<Long> evseIds = JSON.parseArray(evseList, Long.class);
                        for (Long evseId : evseIds) {
                            log.warn("mysql es 数据不同步 直接删除mysql 枪数据: {}", JSON.toJSONString(pileEvse));
                            opLocationEvseMapper.deleteByEvseId(evseId);
                            opLocationPileEvseElastic.deleteById(evseId);
                        }
                    }
                } else {
                    log.warn("桩已被添加到场站，请先移除: {}", JSON.toJSONString(pileEvse));
                    throw new MessageCodeException(PileBaseEnum.CHARGE_POINT_USED_BY_LOCATION);
                }
            }
        }

        // 查询终端
        LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda().eq(ChargePointMerchantTerminalEntity::getMerchantId, merchantId).eq(ChargePointMerchantTerminalEntity::getHostSn, chargePointEntity.getSn());
        List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
        List<String> terminalSnList = chargePointMerchantTerminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());

        List<String> finalSnList = new ArrayList<>(Collections.singletonList(chargePointEntity.getSn()));
        if (CollUtil.isNotEmpty(terminalSnList)) {
            finalSnList.addAll(terminalSnList);
        }

        if (!chargePointDTO.isConfirmed()) {
            LambdaQueryWrapper<TbLenBindRelationEntity> tbLenBindRelationEntityLambdaQueryWrapper = Wrappers.lambdaQuery();
            tbLenBindRelationEntityLambdaQueryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
            tbLenBindRelationEntityLambdaQueryWrapper.in(TbLenBindRelationEntity::getPileSn, finalSnList);
            tbLenBindRelationEntityLambdaQueryWrapper.eq(TbLenBindRelationEntity::getStatus, 1);

            List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(tbLenBindRelationEntityLambdaQueryWrapper);
            if (CollUtil.isNotEmpty(tbLenBindRelationEntities)) {
                throw new MessageCodeException(PileBaseEnum.DELETE_EFFECTIVE_LICENSE_CONFIRM);
            }
        }

       /* SubStatus subStatus = SubStatus.statusOf(chargePointEntity.getSubStatus());
        ImminentExpireChargePointDTO subscriptionStatus = updateSubStatus(chargePointEntity, merchantId);
        if (!chargePointDTO.isConfirmed() && (SubStatus.EFFECTIVE.equals(subStatus) || SubStatus.SOON_TO_EXPIRE.equals(subStatus))) {
            throw new MessageCodeException(PileBaseEnum.DELETE_EFFECTIVE_LICENSE_CONFIRM);
        }*/
        chargePointEntity.setUpdateTime(System.currentTimeMillis());
        // 2.8 优化 改成已过期 不删除
        // 查询终端的sn
        LambdaUpdateWrapper<TbLenBindRelationEntity> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
        updateWrapper.in(TbLenBindRelationEntity::getPileSn, finalSnList);

        TbLenBindRelationEntity updateEntity = new TbLenBindRelationEntity();
        updateEntity.setStatus(2);
        updateEntity.setUpdateTime(System.currentTimeMillis());
        updateEntity.setUnavailableTime(updateEntity.getUpdateTime());
        tbLenBindRelationMapper.update(updateEntity, updateWrapper);

        ChargePointNoticeEvent chargePointNoticeEvent = ChargePointMerchantRelationTypeMapper.INSTANCE.chargePointMerchantRelationEntityToChargePointNoticeEvent(chargePointEntity);

        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointEntity.getOverchargingPileFlag())) {
            chargePointNoticeEvent.setDeviceType(DeviceTypeEnum.OVERCHARGING_THE_HOST.getCode());
        } else {
            chargePointNoticeEvent.setDeviceType(DeviceTypeEnum.OTHER_DEVICE.getCode());
        }

        chargePointNoticeEvent.setZoneId(chargePointEntity.getZoneId());
        chargePointNoticeEvent.setEvent(ChargePointNoticeEvent.Event.DELETE);

        ChargePointMerchantRelationEntity relationOwner = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.OWNER);
        if (Objects.isNull(relationOwner)) {
            chargePointNoticeEvent.setOwner(null);
            chargePointNoticeEvent.setOwnerName(null);
            chargePointNoticeEvent.setOwnerBindTime(null);
        } else if (merchantId.equals(relationOwner.getMerchantId())) {
            chargePointNoticeEvent.setOwner(null);
            chargePointNoticeEvent.setOwnerName(null);
            chargePointNoticeEvent.setOwnerBindTime(null);
        } else {
            Optional.of(relationOwner).ifPresent(target -> chargePointNoticeEvent.setOwner(target.getMerchantId()));
        }

        ChargePointMerchantRelationEntity relationMaintenance = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.MAINTENANCE);
        if (Objects.isNull(relationMaintenance)) {
            chargePointNoticeEvent.setMaintenance(null);
            chargePointNoticeEvent.setMaintenanceName(null);
            chargePointNoticeEvent.setMaintenanceBindTime(null);
        } else if (merchantId.equals(relationMaintenance.getMerchantId())) {
            chargePointNoticeEvent.setMaintenance(null);
            chargePointNoticeEvent.setMaintenanceName(null);
            chargePointNoticeEvent.setMaintenanceBindTime(null);
        } else {
            Optional.of(relationMaintenance).ifPresent(target -> chargePointNoticeEvent.setMaintenance(target.getMerchantId()));
        }
        Optional.ofNullable(chargePointNoticeEvent.getOwner()).ifPresent(id -> chargePointNoticeEvent.setOwnerName(pileUserServiceAdapter.findMerchantById(id).getName()));
        Optional.ofNullable(chargePointNoticeEvent.getMaintenance()).ifPresent(id -> chargePointNoticeEvent.setMaintenanceName(pileUserServiceAdapter.findMerchantById(id).getName()));
        log.info("delete Relation {}", JSON.toJSONString(chargePointEntity));
        chargePointMerchantRelationMapper.delete(lambdaQueryWrapper);

        Result<Boolean> booleanResult = edgeFeignClient.delPileLinkedEdgeGatWay(chargePointEntity.getSn());
        log.info("删除桩通知边缘云_resp:{}",booleanResult);
        if(booleanResult==null || booleanResult.getData()==null || !booleanResult.getData()){
            //删除失败
            log.error("删除桩通知边缘云报错,pilesn:{}",chargePointEntity.getSn());
            throw new MessageCodeException("_ktqTsQx1XEKL");
        }

        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointEntity.getOverchargingPileFlag())) {
            List<ChargePointMerchantTerminalEntity> terminalEntityList = chargePointMerchantTerminalService.getTerminalEntityList(chargePointEntity.getSn(), merchantId);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalEntityList)) {
                chargePointMerchantTerminalService.batchDelete(chargePointEntity.getSn(), merchantId);
                terminalEntityList.forEach(val -> rabbitTemplate.convertAndSend(
                            TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX,
                            ChargePointNoticeEvent.class.getSimpleName(),
                            JSON.toJSONString(this.buildTerminalEvent(val,
                                    ChargePointNoticeEvent.Event.DELETE,
                                    null,
                                    null,
                                    null))));
            }
        }
        redis2Util.delete(SubscribePileRightsService.HAVE_CHARGE_RIGHTS_KEY + chargePointNoticeEvent.getSn());
        log.info("ChargePointNoticeEvent={}", JSON.toJSONString(chargePointNoticeEvent));
        rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, ChargePointNoticeEvent.class.getSimpleName(), JSON.toJSONString(chargePointNoticeEvent));
    }

    private ImminentExpireChargePointDTO updateSubStatus(ChargePointMerchantRelationEntity chargePointEntity, Long merchantId, String commodityCode) {
        return updateSubStatus(chargePointEntity, merchantId, Collections.emptyList(), commodityCode);
    }

    private ImminentExpireChargePointDTO updateSubStatus(ChargePointMerchantRelationEntity chargePointEntity, Long merchantId, Collection<String> serviceIds, String commodityCode) {
        ImminentExpireChargePointDTO.ImminentExpireChargePointDTOBuilder builder = ImminentExpireChargePointDTO.builder();
        builder.merchantId(merchantId);
        builder.sn(chargePointEntity.getSn());

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
        queryWrapper.eq(TbLenBindRelationEntity::getPileSn, chargePointEntity.getSn());
        queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, 0);
        queryWrapper.eq(StringUtils.isNotBlank(commodityCode), TbLenBindRelationEntity::getGoodsId, commodityCode);
        queryWrapper.in(org.apache.commons.collections4.CollectionUtils.isNotEmpty(serviceIds), TbLenBindRelationEntity::getServiceId, serviceIds);
        queryWrapper.orderByDesc(TbLenBindRelationEntity::getUnavailableTime).last("limit 1");
        TbLenBindRelationEntity entity = tbLenBindRelationMapper.selectOne(queryWrapper);
        if (Objects.nonNull(entity)) {
            builder.goodsName(entity.getGoodsName());
            long effectiveDays = (BigDecimal.valueOf(entity.getUnavailableTime() - System.currentTimeMillis())).divide(BigDecimal.valueOf(ONE_DAY_MILLIS), RoundingMode.UP).longValue();
            builder.remainDays(effectiveDays);
            builder.expireEpochMilli(entity.getUnavailableTime());
            builder.expireDate(LocalDateTime.ofInstant(Instant.ofEpochMilli(entity.getUnavailableTime()), ZoneId.systemDefault()).format(dateFormatter));
            if (effectiveDays <= 0) {
                builder.subStatus(SubStatus.INVALIDITY);
            } else if (effectiveDays <= countDownDays) {
                builder.subStatus(SubStatus.SOON_TO_EXPIRE);
            } else {
                builder.subStatus(SubStatus.EFFECTIVE);
            }
        } else {
            builder.subStatus(SubStatus.INACTIVITY);
        }
        ImminentExpireChargePointDTO build = builder.build();
        if (org.springframework.util.CollectionUtils.isEmpty(serviceIds)) {  // 根据所有的权益采取更新否则只是查询 指定的权益
            updateSubStatus(build.getExpireEpochMilli(), build.getSubStatus().getStatus(), merchantId, chargePointEntity);
        }
        return build;
    }

    private ImminentExpireChargePointDTO updateSubStatus(ChargePointMerchantTerminalEntity chargePointEntity, Long merchantId) {
        return updateSubStatus(chargePointEntity, merchantId, Collections.emptyList());
    }

    private ImminentExpireChargePointDTO updateSubStatus(ChargePointMerchantTerminalEntity chargePointEntity, Long merchantId, Collection<String> serviceIds) {
        ImminentExpireChargePointDTO.ImminentExpireChargePointDTOBuilder builder = ImminentExpireChargePointDTO.builder();
        builder.merchantId(merchantId);
        builder.sn(chargePointEntity.getTerminalSn());

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
        queryWrapper.eq(TbLenBindRelationEntity::getPileSn, chargePointEntity.getTerminalSn());
        queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, 0);
        queryWrapper.in(org.apache.commons.collections4.CollectionUtils.isNotEmpty(serviceIds), TbLenBindRelationEntity::getServiceId, serviceIds);
        queryWrapper.orderByDesc(TbLenBindRelationEntity::getUnavailableTime).last("limit 1");
        TbLenBindRelationEntity entity = tbLenBindRelationMapper.selectOne(queryWrapper);
        if (Objects.nonNull(entity)) {
            builder.goodsName(entity.getGoodsName());
            long effectiveDays = (BigDecimal.valueOf(entity.getUnavailableTime() - System.currentTimeMillis())).divide(BigDecimal.valueOf(ONE_DAY_MILLIS), RoundingMode.UP).longValue();
            builder.remainDays(effectiveDays);
            builder.expireEpochMilli(entity.getUnavailableTime());
            builder.expireDate(LocalDateTime.ofInstant(Instant.ofEpochMilli(entity.getUnavailableTime()), ZoneId.systemDefault()).format(dateFormatter));
            if (effectiveDays <= 0) {
                builder.subStatus(SubStatus.INVALIDITY);
            } else if (effectiveDays <= countDownDays) {
                builder.subStatus(SubStatus.SOON_TO_EXPIRE);
            } else {
                builder.subStatus(SubStatus.EFFECTIVE);
            }
        } else {
            log.warn("merchantId {} chargePointEntity.sn {}  serviceIds{}", merchantId, chargePointEntity.getTerminalSn(), JSON.toJSONString(serviceIds));
            builder.subStatus(SubStatus.INACTIVITY);
        }
        ImminentExpireChargePointDTO build = builder.build();
        if (org.springframework.util.CollectionUtils.isEmpty(serviceIds)) {  // 根据所有的权益采取更新否则只是查询 指定的权益
            updateSubStatus(build.getExpireEpochMilli(), build.getSubStatus().getStatus(), merchantId, chargePointEntity);
        }
        return build;
    }

    private ImminentExpireChargePointDTO getAndUpdateTerminalInfo(ChargePointMerchantTerminalEntity terminal, Long merchantId, String commodityCode) {
        ImminentExpireChargePointDTO.ImminentExpireChargePointDTOBuilder builder = ImminentExpireChargePointDTO.builder();
        builder.merchantId(merchantId);
        builder.sn(terminal.getTerminalSn());

        LambdaQueryWrapper<TbLenBindRelationEntity> queryWrapper = Wrappers.lambdaQuery(TbLenBindRelationEntity.class);
        queryWrapper.eq(TbLenBindRelationEntity::getTenantId, merchantId);
        queryWrapper.eq(TbLenBindRelationEntity::getPileSn, terminal.getTerminalSn());
        queryWrapper.eq(TbLenBindRelationEntity::getGoodsId, commodityCode);
        queryWrapper.gt(TbLenBindRelationEntity::getUnavailableTime, 0);
        queryWrapper.orderByDesc(TbLenBindRelationEntity::getUnavailableTime).last("limit 1");
        TbLenBindRelationEntity entity = tbLenBindRelationMapper.selectOne(queryWrapper);
        if (Objects.nonNull(entity)) {
            builder.goodsName(entity.getGoodsName());
            long effectiveDays = (BigDecimal.valueOf(entity.getUnavailableTime() - System.currentTimeMillis())).divide(BigDecimal.valueOf(ONE_DAY_MILLIS), RoundingMode.UP).longValue();
            builder.remainDays(effectiveDays);
            builder.expireEpochMilli(entity.getUnavailableTime());
            builder.expireDate(LocalDateTime.ofInstant(Instant.ofEpochMilli(entity.getUnavailableTime()), ZoneId.systemDefault()).format(dateFormatter));
            builder.expireEpochMilli(entity.getUnavailableTime());
            if (effectiveDays <= 0) {
                builder.subStatus(SubStatus.INVALIDITY);
            } else if (effectiveDays <= countDownDays) {
                builder.subStatus(SubStatus.SOON_TO_EXPIRE);
            } else {
                builder.subStatus(SubStatus.EFFECTIVE);
            }
        } else {
            builder.subStatus(SubStatus.INACTIVITY);
        }
        ImminentExpireChargePointDTO build = builder.build();


        terminal.setUpdateTime(System.currentTimeMillis());
        terminal.setSubStatus(build.getSubStatus().getStatus());
        chargePointMerchantTerminalMapper.updateById(terminal);
        return build;
    }

    private Boolean updateSubStatus(Long expire, int subStatus, Long merchantId, ChargePointMerchantRelationEntity chargePointEntity) {
        if (Objects.isNull(chargePointEntity) || Objects.isNull(merchantId)) {
            log.warn("Objects.isNull(chargePointEntity) {}", JSON.toJSONString(chargePointEntity));
            return Boolean.FALSE;
        }
        if (subStatus == chargePointEntity.getSubStatus()) {
            log.warn("no change Sub {}", JSON.toJSONString(chargePointEntity));
            return Boolean.FALSE;
        } else {
            chargePointEntity.setSubStatus(subStatus);
            chargePointEntity.setSubExpire(expire);
        }
        chargePointEntity.setUpdateTime(System.currentTimeMillis());
        chargePointMerchantRelationMapper.updateById(chargePointEntity);
        notice(Objects.nonNull(expire) && expire.compareTo(System.currentTimeMillis()) > 0, chargePointEntity);
        return Boolean.TRUE;
    }
    private Boolean updateSubStatus(Long expire, int subStatus, Long merchantId, ChargePointMerchantTerminalEntity chargePointEntity) {
        if (Objects.isNull(chargePointEntity) || Objects.isNull(merchantId)) {
            log.warn("Objects.isNull(chargePointEntity) {}", JSON.toJSONString(chargePointEntity));
            return Boolean.FALSE;
        }
        if (Objects.isNull(expire)) {
            log.warn("Objects.isNull(expire) {}", JSON.toJSONString(chargePointEntity));
            //notice(Boolean.FALSE, chargePointEntity);
            return Boolean.FALSE;
        }
        if (subStatus == chargePointEntity.getSubStatus()) {
            log.warn("no change Sub {}", JSON.toJSONString(chargePointEntity));
            return Boolean.FALSE;
        } else {
            chargePointEntity.setSubStatus(subStatus);
        }
        chargePointEntity.setUpdateTime(System.currentTimeMillis());
        chargePointMerchantTerminalMapper.updateById(chargePointEntity);
        //notice(expire.compareTo(System.currentTimeMillis()) > 0, chargePointEntity);
        return Boolean.TRUE;
    }

    private void notice(Boolean status, ChargePointMerchantRelationEntity... chargePointEntity) {
        doExecuteTask(() -> {
            for (ChargePointMerchantRelationEntity chargePoint : chargePointEntity) {
                try {
                    ChargePointNoticeEvent chargePointNoticeEvent = ChargePointMerchantRelationTypeMapper.INSTANCE.chargePointMerchantRelationEntityToChargePointNoticeEvent(chargePoint);
                    chargePointNoticeEvent.setEvent(ChargePointNoticeEvent.Event.UPDATE);
                    if (Objects.isNull(chargePointNoticeEvent.getOwner())) {
                        ChargePointMerchantRelationEntity relation = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.OWNER);
                        Optional.ofNullable(relation).ifPresent(target -> chargePointNoticeEvent.setOwner(target.getMerchantId()));
                    }
                    if (Objects.isNull(chargePointNoticeEvent.getMaintenance())) {
                        ChargePointMerchantRelationEntity relation = findRelation(chargePointNoticeEvent.getSn(), MerchantChargePointRelationEnum.MAINTENANCE);
                        Optional.ofNullable(relation).ifPresent(target -> chargePointNoticeEvent.setMaintenance(target.getMerchantId()));
                    }
                    Optional.ofNullable(chargePointNoticeEvent.getOwner()).ifPresent(id -> chargePointNoticeEvent.setOwnerName(pileUserServiceAdapter.findMerchantById(id).getName()));
                    Optional.ofNullable(chargePointNoticeEvent.getMaintenance()).ifPresent(id -> chargePointNoticeEvent.setMaintenanceName(pileUserServiceAdapter.findMerchantById(id).getName()));
                    rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX, ChargePointNoticeEvent.class.getSimpleName(), JSON.toJSONString(chargePointNoticeEvent));

                    /*UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO = new UpdateEsSubscriptionStatusDTO();
                    updateEsSubscriptionStatusDTO.setPileSnList(Collections.singletonList(chargePoint.getSn()));
                    updateEsSubscriptionStatusDTO.setStatus(status);
                    opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);*/
                } catch (Exception e) {
                    log.error(String.format(" %s %s", chargePoint.getSn(), status.toString()), e);
                }
            }
        });
    }

    private void doExecuteTask(Runnable command) {
        pileBaseTaskExecutor.execute(command);
    }

    /**
     * 新增保存 使用
     *
     * @param chargePointDTO 入参
     * @param merchantId     商家ID
     * @return 保存之后的关系
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ChargePointMerchantRelationEntity save(ChargePointDTO chargePointDTO, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.save chargePointDTO : {} and merchantId : {}",
                JSON.toJSONString(chargePointDTO),
                JSON.toJSONString(merchantId));

        if (merchantId == null) {
            throw new MessageCodeException(PileUserEnum.MERCHANT_NOT_EXIST);
        }

        if (org.apache.commons.lang3.StringUtils.isBlank(chargePointDTO.getSn())) {
            throw new MessageCodeException(PileBaseEnum.THE_DEVICE_SN_OR_PIN_IS_INCORRECT);
        }

        this.checkConnectors(chargePointDTO);

        String sn = chargePointDTO.getSn();
        GetDeviceTypeDTO getDeviceTypeDTO = new GetDeviceTypeDTO();

        getDeviceTypeDTO.setSn(sn);
        getDeviceTypeDTO.setBrandId(chargePointDTO.getBrandId());

        if (Long.valueOf(BrandEnum.AUTEL.getCode()).equals(chargePointDTO.getBrandId())
                && !this.checkSn(getDeviceTypeDTO, ChargePointMerchantRelationServiceImpl.HOST)) {
            throw new MessageCodeException(PileBaseEnum.CANNOT_BE_USED_AS_AN_OVERCHARGED_HOST, new Object[]{sn});
        }

        if (!this.checkDeviceUnique(sn, merchantId)) {
            throw new MessageCodeException(PileBaseEnum.DO_NOT_ADD_DUPLICATES, new Object[]{sn});
        }

        List<String> terminalSnList = new ArrayList<>();
        Set<String> terminalSnSet = new HashSet<>();
        List<Terminal> terminals = chargePointDTO.getTerminals();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminals)) {
            for (Terminal terminal : terminals) {
                String terminalSn = terminal.getTerminalSn();
                terminalSnList.add(terminalSn);
                terminalSnSet.add(terminalSn);

                GetDeviceTypeDTO deviceTypeDTO = new GetDeviceTypeDTO();
                deviceTypeDTO.setSn(terminalSn);
                deviceTypeDTO.setBrandId(chargePointDTO.getBrandId());

                if (!this.checkDeviceUnique(terminalSn, merchantId)) {
                    throw new MessageCodeException(PileBaseEnum.DO_NOT_ADD_DUPLICATES, new Object[]{sn});
                }

                if (!this.checkSn(deviceTypeDTO, ChargePointMerchantRelationServiceImpl.TERMINAL)) {
                    throw new MessageCodeException(PileBaseEnum.THE_DEVICE_CANNOT_BE_USED_AS_A_TERMINAL, new Object[]{terminalSn});
                }

                List<String> terminalSns = new ArrayList<>();
                terminalSns.add(terminalSn);
                List<ChargePointMerchantTerminalEntity> terminalEntitys = chargePointMerchantTerminalService.getTerminalList(terminalSns, null);
                if (ObjectUtils.isNotEmpty(terminalEntitys)) {
                    for (ChargePointMerchantTerminalEntity terminalEntity : terminalEntitys) {
                        if (merchantId.equals(terminalEntity.getMerchantId())) {
                            throw new MessageCodeException(PileBaseEnum.DO_NOT_ADD_DUPLICATES, new Object[]{terminalSn});
                        }
                    }

                    if (terminalEntitys.size() >= 2) {
                        throw new MessageCodeException(PileBaseEnum.HAS_BEEN_ADDED_BY_OTHER_MERCHANTS, new Object[]{terminalSn});
                    }
                }
            }

            if (terminalSnList.size() != terminalSnSet.size()) {
                throw new MessageCodeException(PileBaseEnum.THE_DEVICE_SN_IS_DUPLICATED_PLEASE_CONFIRM);
            }
        }

        if (chargePointDTO.getOverchargingPileFlag() == null) {
            chargePointDTO.setOverchargingPileFlag(this.judgeOverchargingPile(getDeviceTypeDTO));
        }

        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointDTO.getOverchargingPileFlag())) {
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePointDTO.getConnectors())) {
                chargePointDTO.setConnectors(this.buildConnectors(terminals));
            }
        } else {
            chargePointDTO.setTerminals(null);
        }

        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ChargePointMerchantRelationEntity::getSn, sn);
        List<ChargePointMerchantRelationEntity> relationEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);

        if (CollectionUtils.isEmpty(relationEntities)) {
            // 第一个商家绑定该桩
            log.info("current merchant first try bind {}", MerchantChargePointRelationEnum.MAINTENANCE.getKey());
            return bindRelation(chargePointDTO, merchantId, MerchantChargePointRelationEnum.MAINTENANCE);
        } else {
            List<Integer> existRelations = new ArrayList<>();
            for (ChargePointMerchantRelationEntity relationEntity : relationEntities) {
                if (merchantId.equals(relationEntity.getMerchantId())) {

                    log.warn("merchantId={} already bound sn: {}", merchantId, sn);

                    throw new MessageCodeException(PileBaseEnum.CHARGE_POINT_BOUND_BY_MYSELF);

                }
                log.warn("merchantId={} already bound sn: {}  remove Relation {}", merchantId, sn, relationEntity.getRelation());
                existRelations.add(relationEntity.getRelation());
            }
            List<MerchantChargePointRelationEnum> remainingRelations = MerchantChargePointRelationEnum.remainingRelations(existRelations);
            if (CollectionUtils.isEmpty(remainingRelations)) {
                log.warn("merchantId={}  sn: {} already bound by other merchants: {}", merchantId, sn, JSON.toJSONString(relationEntities));
                throw new MessageCodeException(PileBaseEnum.SN_BOUND_BY_OTHER_MERCHANT);
            }
            log.info("current merchant try bind {}", remainingRelations.get(0));
            return bindRelation(chargePointDTO, merchantId, remainingRelations.get(0));
        }
    }

    private void checkConnectors(ChargePointDTO chargePointDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.checkConnectors chargePointDTO : {}",
                JSON.toJSONString(chargePointDTO));

        List<Connector> connectors = chargePointDTO.getConnectors();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(connectors)) {
            List<Integer> connectorIdList = new ArrayList<>();
            for (Connector connector : connectors) {
                if (connector == null
                        || connector.getConnectorId() == null) {
                    throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
                }
                connectorIdList.add(connector.getConnectorId());
            }

            if (connectorIdList.isEmpty()) {
                throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
            }

            List<Integer> orderedConnectorIdList = connectorIdList.stream().sorted(Integer::compareTo).collect(Collectors.toList());
            for (int i = 0; i < orderedConnectorIdList.size(); i++) {
                if (orderedConnectorIdList.get(i) != i + 1) {
                    throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
                }
            }
        }

        List<Terminal> terminals = chargePointDTO.getTerminals();
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminals)) {
            List<Integer> connectorIdList = new ArrayList<>();
            for (Terminal terminal : terminals) {
                if (terminal == null
                        || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(terminal.getConnectors())) {
                    throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
                }
                List<Connector> connectorList = terminal.getConnectors();
                for (Connector connector : connectorList) {
                    if (connector == null
                            || connector.getConnectorId() == null) {
                        throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
                    }
                    connectorIdList.add(connector.getConnectorId());
                }
            }

            if (connectorIdList.isEmpty()) {
                throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
            }

            List<Integer> orderedConnectorIdList = connectorIdList.stream().sorted(Integer::compareTo).collect(Collectors.toList());
            for (int i = 0; i < orderedConnectorIdList.size(); i++) {
                if (orderedConnectorIdList.get(i) != i + 1) {
                    throw new MessageCodeException(PileBaseEnum.SYSTEM_BUSY);
                }
            }
        }
    }

    private boolean checkDeviceUnique(String sn, Long merchantId) {

        if (org.apache.commons.lang3.StringUtils.isBlank(sn)
                || merchantId == null) {
            return false;
        }

        List<String> pileSnList = new ArrayList<>();
        pileSnList.add(sn);

        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntityList = this.getPileList(pileSnList, merchantId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantRelationEntityList)) {
            return false;
        }

        List<String> terminalSnList = new ArrayList<>();
        terminalSnList.add(sn);
        List<ChargePointMerchantTerminalEntity> chargePointMerchantTerminalEntityList = chargePointMerchantTerminalService.getTerminalList(terminalSnList, merchantId);
        return !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointMerchantTerminalEntityList);
    }

    private boolean checkSn(GetDeviceTypeDTO getDeviceTypeDTO, String deviceTypeName) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.checkSn getDeviceTypeDTO : {} and deviceTypeName : {}",
                JSON.toJSONString(getDeviceTypeDTO),
                JSON.toJSONString(deviceTypeName));

        Integer deviceType = pileDeviceServiceAdapter.getDeviceType(getDeviceTypeDTO);
        if (ChargePointMerchantRelationServiceImpl.HOST.equalsIgnoreCase(deviceTypeName)) {
            return !DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceType);
        } else {
            return DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceType);
        }
    }

    private List<Connector> buildConnectors(List<Terminal> terminals) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.buildConnectors terminals : {}",
                JSON.toJSONString(terminals));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(terminals)) {
            return null;
        }

        List<Connector> connectors = new ArrayList<>();
        terminals.forEach(val -> connectors.addAll(val.getConnectors()));
        return connectors;
    }

    /**
     * @see com.autel.cloud.pile.base.enums.chargepoint.OverchargingPileFlagEnum
     * @param getDeviceTypeDTO
     * @return
     */
    private Integer judgeOverchargingPile(GetDeviceTypeDTO getDeviceTypeDTO) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.judgeOverchargingPile getDeviceTypeDTO : {}",
                JSON.toJSONString(getDeviceTypeDTO));

        if (getDeviceTypeDTO.getBrandId() == null
                || !getDeviceTypeDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
            return OverchargingPileFlagEnum.NON_OVERCHARGING_PILE.getCode();
        }

        Integer deviceType = pileDeviceServiceAdapter.getDeviceType(getDeviceTypeDTO);
        if (DeviceTypeEnum.OVERCHARGING_THE_HOST.getCode().equals(deviceType)) {
            return OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode();
        }
        return OverchargingPileFlagEnum.NON_OVERCHARGING_PILE.getCode();
    }

    private ChargePointMerchantRelationEntity bindRelation(ChargePointDTO chargePointDTO, Long merchantId, MerchantChargePointRelationEnum merchantChargePointRelationEnum) {
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = ChargePointMerchantRelationTypeMapper.INSTANCE.dto2Entity(chargePointDTO, merchantChargePointRelationEnum);
        chargePointMerchantRelationEntity.setMerchantId(merchantId);
        chargePointMerchantRelationEntity.setBindTime(System.currentTimeMillis());

        if (chargePointDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {

            log.info("道通品牌 {}", chargePointDTO.getSn());

            List<VerifyDTO> verifyDTOList = new ArrayList<>();
            VerifyDTO verifyDTO = new VerifyDTO();
            verifyDTO.setPileSn(chargePointDTO.getSn());
            verifyDTO.setPin(chargePointDTO.getPin());
            verifyDTOList.add(verifyDTO);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(chargePointDTO.getTerminals())) {
                chargePointDTO.getTerminals().forEach(val -> {
                    VerifyDTO item = new VerifyDTO();
                    item.setPileSn(val.getTerminalSn());
                    item.setPin(val.getPin());
                    verifyDTOList.add(item);
                });
            }
            for (VerifyDTO dto : verifyDTOList) {
                Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(dto);
                if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.SN_AND_PIN_CODES_DO_NOT_MATCH);
                }
            }

            // 道通的桩查询一下尺寸
            com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO chargePileDTO = pileDeviceServiceAdapter.getChargePileInfo(chargePointDTO.getSn());
            chargePointMerchantRelationEntity.setCategory(chargePileDTO.getCategory());
            chargePointMerchantRelationEntity.setScreen1Pixel(chargePileDTO.getScreen1Pixel());
            chargePointMerchantRelationEntity.setScreen1Size(chargePileDTO.getScreen1Size());
            chargePointMerchantRelationEntity.setMac(pileDeviceServiceAdapter.getDeviceMac(chargePileDTO.getSn()));
            if (Objects.nonNull(chargePileDTO.getPartProduct())) {
                chargePointMerchantRelationEntity.setPartProduct(chargePileDTO.getPartProduct());
            }
            if (Objects.nonNull(chargePileDTO.getProductNamePdm())) {
                chargePointMerchantRelationEntity.setProductNamePdm(chargePileDTO.getProductNamePdm());
            }
            if (Objects.nonNull(chargePileDTO.getPhase())) {
                String phaseConstant = "PHASE";
                int category = chargePileDTO.getCategory() == null ? CategoryEnum.AC.getCode() : chargePileDTO.getCategory();
                String type = CategoryEnum.getEnumByCode(category).getDesc();
                int phase = chargePileDTO.getPhase() == null ? 3 : chargePileDTO.getPhase();
                chargePointMerchantRelationEntity.setPhases(type + "_" + phase + "_" + phaseConstant);
            }


            //判断是否美标极简版充电桩  美标极简版充电桩不能添加到平台作为商桩(要排在校验桩是否被绑定前，不然桩被改成商桩后，即不能添加在平台作为商桩，也不能添加到app作为家桩)
            Result<ChargePileDTO> chargePileDetailResult = null;
            try {
                chargePileDetailResult = deviceServiceFeign.query(chargePointDTO.getSn());
            } catch (Exception e) {
                log.error("OpLocationEvseRepositoryImpl validEvse", e);
            }
            if (chargePileDetailResult != null && chargePileDetailResult.getData() != null) {
                String productModel = chargePileDetailResult.getData().getProductModel();
                if (StringUtils.isNotBlank(productModel)) {
                    for (AmericaPileProductModelEnum americaPileProductModelEnum : AmericaPileProductModelEnum.values()) {
                        if (Objects.equals(productModel, americaPileProductModelEnum.getValue())) {
                            throw new MessageCodeException(PileBaseEnum.AMERICA_PILE_NOT_ALLOW_ADD);
                        }
                    }
                }
            }

            /*
            //校验pileSN和pin
            com.autel.cloud.device.dto.VerifyDTO verifyDTO = new com.autel.cloud.device.dto.VerifyDTO();
            verifyDTO.setPin(chargePointDTO.getPin());
            verifyDTO.setPileSn(chargePointDTO.getSn());
            Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(verifyDTO);
            if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
            }
            */

        } else {   //三方桩的校验
            log.info("三方桩 {}", chargePointDTO.getSn());
            OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
            opLocationEvseDTO.setEvseSn(chargePointDTO.getSn());
            opLocationEvseDTO.setPileSN(chargePointDTO.getSn());
            opLocationEvseDTO.setPower(chargePointDTO.getRatedPower().doubleValue());
            opLocationEvseDTO.setThirdPart(1);
            opLocationEvseDTO.setPileName(chargePointDTO.getName());
            opLocationEvseDTO.setPowerType(chargePointDTO.getPowerType());
            opLocationEvseDTO.setPinCode(chargePointDTO.getPin());
            opLocationEvseDTO.setBrandId(chargePointDTO.getBrandId());
            opLocationEvseDTO.setBrandName(chargePointDTO.getBrandName());
            List<OpLocationConnectorDTO> list = new ArrayList<>();
            for (Connector connector : chargePointDTO.getConnectors()) {
                OpLocationConnectorDTO connectorDTO = new OpLocationConnectorDTO();
                connectorDTO.setGunType(connector.getConnectorType());
                connectorDTO.setConnectorId(connector.getConnectorId().toString());
                list.add(connectorDTO);
            }
            opLocationEvseDTO.setProductModel(chargePointDTO.getPartProduct());
            opLocationEvseDTO.setProductNamePdm(chargePointDTO.getProductNamePdm());
            if (Objects.nonNull(chargePointDTO.getPhases())) {
                String phase = chargePointDTO.getPhases().contains("3") ? "3" : "1";
                opLocationEvseDTO.setPhase(phase);
            }
            opLocationEvseDTO.setOpLocationConnectorDTOs(list);
            opLocationEvseDTO.setCategory("DC".equalsIgnoreCase(chargePointDTO.getPowerType()) ? 2 : 1);
            pileDeviceServiceAdapter.saveThirdPile(opLocationEvseDTO);

        }
        log.info("chargePointMerchantRelationEntity=  {}", JSON.toJSONString(chargePointMerchantRelationEntity));
        chargePointMerchantRelationEntity.setCreateTime(System.currentTimeMillis());
        chargePointMerchantRelationMapper.insert(chargePointMerchantRelationEntity);

        Map<ChargePointMerchantTerminalEntity, ChargePointNoticeEvent.Event> chargePointMerchantTerminalEntityAndEventMap = new HashMap<>();
        if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(chargePointMerchantRelationEntity.getOverchargingPileFlag())) {
            chargePointMerchantTerminalEntityAndEventMap = this.processingTerminal(chargePointDTO.getTerminals(), chargePointMerchantRelationEntity);
        }

        this.syncBusinessMessage(chargePointMerchantRelationEntity, ChargePointNoticeEvent.Event.INSERT, chargePointMerchantTerminalEntityAndEventMap);
        return chargePointMerchantRelationEntity;
    }

    private List<ChargePointMerchantTerminalEntity> addTerminals(List<Terminal> terminals, ChargePointMerchantRelationEntity chargePointMerchantRelationEntity) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.addTerminals terminals : {} and chargePointMerchantRelationEntity : {}",
                JSON.toJSONString(terminals),
                JSON.toJSONString(chargePointMerchantRelationEntity));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(terminals)
                || chargePointMerchantRelationEntity == null) {
            return null;
        }

        List<ChargePointMerchantTerminalEntity> terminalEntityList = new ArrayList<>();
        terminals.forEach(val -> {
            ChargePointMerchantTerminalEntity terminalEntity = new ChargePointMerchantTerminalEntity();
            terminalEntity.setId(IdWorker.getId());
            terminalEntity.setTerminalSn(val.getTerminalSn());
            terminalEntity.setTerminalName(val.getTerminalName());
            terminalEntity.setTerminalPin(val.getPin());
            terminalEntity.setJsonFormatConnectors(val.getConnectors());
            terminalEntity.setHostSn(chargePointMerchantRelationEntity.getSn());
            terminalEntity.setMerchantId(chargePointMerchantRelationEntity.getMerchantId());
            terminalEntity.setSubStatus(SubStatus.INACTIVITY.getStatus());
            terminalEntity.setCreateTime(chargePointMerchantRelationEntity.getCreateTime());
            terminalEntity.setUpdateTime(chargePointMerchantRelationEntity.getUpdateTime());
            terminalEntityList.add(terminalEntity);
        });
        return chargePointMerchantTerminalService.batchAdd(terminalEntityList) ? terminalEntityList : null;
    }

    private ChargePointMerchantRelationEntity findRelation(String sn, MerchantChargePointRelationEnum merchantChargePointRelationEnum) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class)
                .eq(ChargePointMerchantRelationEntity::getSn, sn).eq(ChargePointMerchantRelationEntity::getRelation, merchantChargePointRelationEnum.getKey());
        ChargePointMerchantRelationEntity chargePointMerchantRelationEntity = chargePointMerchantRelationMapper.selectOne(queryWrapper);
        return chargePointMerchantRelationEntity;
    }

    @Override
    public List<ImminentExpireChargePointDTO> findSubRequiredPile(Long merchantId, String zoneId) {
        List<ImminentExpireChargePointDTO> subRequiredPiles = new ArrayList<>();
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class)
                .eq(ChargePointMerchantRelationEntity::getMerchantId, merchantId);
        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
        for (ChargePointMerchantRelationEntity chargePointEntity : chargePointEntities) {
            ImminentExpireChargePointDTO imminentExpireChargePointDTO = updateSubStatus(chargePointEntity, merchantId, PileChargingRights.SERVICE_ID_LIST, null);
            if (SUB_REQUIRED_SUBSCRIPTION_STATUS.contains(imminentExpireChargePointDTO.getSubStatus())) {
                subRequiredPiles.add(imminentExpireChargePointDTO);
            }
        }
        log.info("ImminentExpireChargePointDTO {}", JSON.toJSONString(subRequiredPiles));
        return subRequiredPiles;
    }

    private List<ConnectorDTO> getConnectorDTOS(String sn) {
        GunTypeAndNumVO gunTypeAndNumVO = pileDeviceServiceAdapter.getGunTypeAndNumVO(sn);
        log.info("gunTypeAndNumVO {}", JSON.toJSONString(gunTypeAndNumVO));
        List<ConnectorDTO> connectorList = new ArrayList<>();

        if (ObjectUtils.isEmpty(gunTypeAndNumVO.getGunNum())) {
            gunTypeAndNumVO.setGunNum("1");
        }
        if (ObjectUtils.isEmpty(gunTypeAndNumVO.getGunTypeMap())) {
            Map<String, String> gunTypeMap = new HashMap<>();
            gunTypeMap.put("1", "1");
            gunTypeAndNumVO.setGunTypeMap(gunTypeMap);
        }
        for (Map.Entry<String, String> next : gunTypeAndNumVO.getGunTypeMap().entrySet()) {
            ConnectorDTO connectorDTO = new ConnectorDTO();
            Integer connectorSequence = StringUtil.formatStringToInteger(next.getKey());
            Integer connectorType = StringUtil.formatStringToInteger(next.getValue());
            connectorDTO.setConnectorNo(connectorSequence);
            connectorDTO.setConnectorType(Objects.nonNull(connectorType) ? connectorType : 1);
            connectorList.add(connectorDTO);
        }
        return connectorList;
    }


    @Override
    public void downloadTempExcel(HttpServletResponse response, String language) throws IOException {
        response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        response.setCharacterEncoding(Constants.UTF_8);
        // 这里URLEncoder.encode可以防止中文乱码 当然和easyexcel没有关系
        String fileName = getI18n(PileExcelHeadConstant.EXPORT_MODULE_NAME);
        fileName = URLEncoder.encode(fileName, Constants.UTF_8).replace("+", "%20");
        fileName = fileName.replaceAll("Autel_","");
        fileName = fileName.replaceAll("Autel","");
        fileName = fileName.trim();
        fileName = this.upperCaseFirstLetter(fileName);

        response.setHeader(StringUtil.HEAD_NAME, StringUtil.HEAD_VALUE + fileName + "." + StringUtil.XLSX);


        ExcelWriterBuilder excelWriterBuilder;
        ExcelWriter excelWriter = null;
        try {
            excelWriterBuilder = EasyExcelFactory.write(response.getOutputStream());
            String sheetName = getI18n(PileExcelHeadConstant.EXPORT_MODULE_NAME);;
            List<List<String>> exportTemplateHeader = getExportTemplateHeader();
            sheetName = sheetName.replaceAll("Autel_","");
            sheetName = sheetName.replaceAll("Autel","");
            sheetName = sheetName.trim();
            log.info("sheetName {}", sheetName);
            WriteSheet sheet = EasyExcelFactory.writerSheet(1, sheetName).build();
            HashMap<Integer, String[]> dropDownMap = getDropDownCloumn();
            excelWriterBuilder.head(exportTemplateHeader)
                    .registerWriteHandler(new TitleHandler(dropDownMap))
                    .registerWriteHandler(new ImportTemplateCellStyleHandler())
                    .registerWriteHandler(new CustomSheetWriteHandler());
            excelWriter = excelWriterBuilder.build();
            excelWriter.write(new ArrayList<>(1), sheet);
        } catch (Exception e) {
            log.error("downloadTempExcel", e);
        } finally {
            if (excelWriter != null) {
                excelWriter.finish();
            }
        }
    }

    private String getI18n(String code) {
        return MessageSourceHolder.getMessage(code);
    }

    private List<GunTypeRespDTO> getGunType() {
        LambdaQueryWrapper<OpEvseTypeEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpEvseTypeEntity::getDeleted, 0);
        List<OpEvseTypeEntity> opEvseTypeEntities = opEvseTypeMapper.selectList(queryWrapper);
        //获取每个枪的枪类型
        List<Integer> gunTypes = opEvseTypeEntities.stream().map(OpEvseTypeEntity::getGunType).distinct().collect(Collectors.toList());
        Result<List<GunTypeRespDTO>> result = opEvseTypeService.getGunType();
        List<GunTypeRespDTO> gunTypeRespDTOS = new ArrayList<>();
        for (GunTypeRespDTO gunTypeRespDTO : result.getData()) {
            gunTypeRespDTO.setName(gunTypeRespDTO.getName().trim());
            if (gunTypes.contains(gunTypeRespDTO.getGunType())) {
                gunTypeRespDTOS.add(gunTypeRespDTO);
            }
        }
        return gunTypeRespDTOS;
    }

    private String upperCaseFirstLetter(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        char firstChar = str.charAt(0);
        if (Character.isUpperCase(firstChar)) {
            return str;
        } else {
            return Character.toUpperCase(firstChar) + str.substring(1);
        }
    }

    private HashMap<Integer, String[]> getDropDownCloumn() {
        HashMap<Integer, String[]> dropDownMap = new HashMap<>();
        String[] catagory = new String[]{"AC", "DC"};
        String[] brand = Arrays.asList(BrandEnum.values()).stream().map(BrandEnum::getName).collect(Collectors.toList()).toArray(new String[]{});
        List<String> gunTypeList = getGunType().stream().map(GunTypeRespDTO::getName).collect(Collectors.toList());
        String[] gunType = gunTypeList.toArray(new String[]{});
        dropDownMap.put(0, brand);
        dropDownMap.put(5, catagory);

        dropDownMap.put(7, gunType);
        dropDownMap.put(8, gunType);
        dropDownMap.put(9, gunType);
        dropDownMap.put(10, gunType);
        String[] phases = new String[]{AC_1_PHASE, AC_3_PHASE};
        dropDownMap.put(11, phases);// 电源相数
        return dropDownMap;
    }

    private List<List<String>> getExportTemplateHeader() {
        String tipsOne = getJoinAsterisk(String.valueOf(MgmtErrorCodeEnum.AUTEL_BRAND_TIPS.getCode()));
        String tipsTwo = getJoinAsterisk(String.valueOf(MgmtErrorCodeEnum.NO_AUTEL_BRAND_TIPS.getCode()));
        String tipsThree = getJoinAsterisk(String.valueOf(MgmtErrorCodeEnum.OVERCHARGING_IS_NOT_SUPPORTED.getCode()));
        List<List<String>> headerList = new ArrayList<>();
        List<String> head0 = new ArrayList<>();
        head0.add(tipsOne);
        head0.add(tipsTwo);
        head0.add(tipsThree);
        head0.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_BRAND));
        List<String> head1 = new ArrayList<>();
        head1.add(tipsOne);
        head1.add(tipsTwo);
        head1.add(tipsThree);
        head1.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_SN));

        List<String> head2 = new ArrayList<>();
        head2.add(tipsOne);
        head2.add(tipsTwo);
        head2.add(tipsThree);
        head2.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_PIN));

        List<String> head3 = new ArrayList<>();
        head3.add(tipsOne);
        head3.add(tipsTwo);
        head3.add(tipsThree);
        head3.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_PILE_NAME));


        List<String> head4 = new ArrayList<>();
        head4.add(tipsOne);
        head4.add(tipsTwo);
        head4.add(tipsThree);
        head4.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_CATAGORY));

        List<String> head5 = new ArrayList<>();
        head5.add(tipsOne);
        head5.add(tipsTwo);
        head5.add(tipsThree);
        head5.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_RATEDPOWER));

        List<String> head6 = new ArrayList<>();
        head6.add(tipsOne);
        head6.add(tipsTwo);
        head6.add(tipsThree);
        head6.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_GUN1));

        List<String> head7 = new ArrayList<>();
        head7.add(tipsOne);
        head7.add(tipsTwo);
        head7.add(tipsThree);
        head7.add(getI18n(PileExcelHeadConstant.EXPORT_HEADER_GUN2));

        List<String> head8 = new ArrayList<>();
        head8.add(tipsOne);
        head8.add(tipsTwo);
        head8.add(tipsThree);
        head8.add(getI18n(PileExcelHeadConstant.EXPORT_HEADER_GUN3));

        List<String> head9 = new ArrayList<>();
        head9.add(tipsOne);
        head9.add(tipsTwo);
        head9.add(tipsThree);
        head9.add(getI18n(PileExcelHeadConstant.EXPORT_HEADER_GUN4));

        List<String> head10 = new ArrayList<>();
        head10.add(tipsOne);
        head10.add(tipsTwo);
        head10.add(tipsThree);
        head10.add(getJoinAsterisk(PileExcelHeadConstant.EXPORT_HEADER_MODEL));

        List<String> head11 = new ArrayList<>();
        head11.add(tipsOne);
        head11.add(tipsTwo);
        head11.add(tipsThree);
        head11.add(getJoinAsterisk(String.valueOf(MgmtErrorCodeEnum.PHASES_TIPS.getCode())));

        headerList.add(head0);
        headerList.add(head1);
        headerList.add(head2);
        headerList.add(head3);
        headerList.add(head10);
        headerList.add(head4);
        headerList.add(head5);
        headerList.add(head6);
        headerList.add(head7);
        headerList.add(head8);
        headerList.add(head9);
        headerList.add(head11);

        return headerList;
    }

    private String getJoinAsterisk(String exportHeaderBrand) {
        return org.apache.commons.lang3.StringUtils.join(asterisk, getI18n(exportHeaderBrand));
    }


    @Override
    public Object newImport(String zoneId, Long merchantId, MultipartFile multipartFile) {
        long startTime = System.currentTimeMillis();
        String originalFilename = multipartFile.getOriginalFilename();
        log.info("newImport,originalFilename:{}", originalFilename);
        //校验文件后缀
        String extension = Objects.requireNonNull(FilenameUtils.getExtension(multipartFile.getOriginalFilename())).toLowerCase();
        if (!Objects.equals(extension, FileExtensionEnum.XLSX.getName())
                && !Objects.equals(extension, FileExtensionEnum.XLS.getName())
                && !Objects.equals(extension, FileExtensionEnum.CSV.getName())) {
            throw new MessageCodeException(PileBaseEnum.FILE_EXTENSION_WRONG);
        }
        List<PileImportNewV2DTO> pileList = Collections.synchronizedList(new ArrayList<>());
        File file = multipartFile2File(multipartFile);

        try {
            EasyExcelFactory.read(file, PileImportNewV2DTO.class, new BatchPageReadListener<PileImportNewV2DTO>(pileList::addAll))
                    .headRowNumber(4).sheet().autoTrim(true).doRead();
        } catch (Exception e) {
            log.info("newImport,Exception:", e);
            throw new MessageCodeException(PileBaseEnum.FILE_EXTENSION_WRONG);
        }
        log.info("originalCellValues:{}", JSON.toJSONString(pileList));
        List<String> snList = pileList.stream().map(PileImportNewV2DTO::getSn).distinct().collect(Collectors.toList());
        for (PileImportNewV2DTO pileImportNewDTO : pileList) {
            String brand = pileImportNewDTO.getBrand();
            String sn = pileImportNewDTO.getSn();
            //道通桩SN转大写
            if (StringUtils.isNotEmpty(brand) && brand.equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
                if (StringUtils.isNotEmpty(sn)) {
                    pileImportNewDTO.setSn(sn.toUpperCase());
                }
            }
        }

        List<ChargePileDTO> chargePileDTOS = pileDeviceServiceAdapter.queryPileList(snList);
        Map<String, ChargePileDTO> chargePileMap = chargePileDTOS.stream().collect(Collectors.toMap(ChargePileDTO::getSn, Function.identity(), (e1, e2) -> e1));
        Result<List<GunTypeRespDTO>> listResult = opEvseTypeServiceImpl.getGunType();
        List<GunTypeRespDTO> gunType = new ArrayList<>();
        if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(listResult) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(listResult.getData())) {
            gunType = listResult.getData();
        }
        List<String> fileSnRepeatList = pileList.stream().filter(e->StringUtils.isNotEmpty(e.getSn())).collect(Collectors.toMap(PileImportNewV2DTO::getSn, e -> 1, Integer::sum)).entrySet().stream().filter(entry -> entry.getValue() > 1).map(Map.Entry::getKey).collect(Collectors.toList());
        Map<String, Integer> gunTypeMap = new HashMap<>();
        if (!org.apache.commons.collections4.CollectionUtils.isEmpty(gunType)) {
            log.info("GunTypeRespDTO:{}", JSON.toJSONString(gunType));
            gunTypeMap.putAll(gunType.stream().collect(Collectors.toMap(GunTypeRespDTO::getName, GunTypeRespDTO::getGunType, (e1, e2) -> e1)));
            log.info("gunTypeMap:{}", JSON.toJSONString(gunTypeMap));
        }
        log.info("解析前,pileList:{}   fileSnRepeatList:{} chargePileMap:{} gunTypeMap:{}", JSON.toJSONString(pileList), JSON.toJSONString(fileSnRepeatList), JSON.toJSONString(chargePileMap), JSON.toJSONString(gunTypeMap));
        checkDataV2(pileList, chargePileMap, fileSnRepeatList, gunTypeMap);
        log.info("解析完成,pileList:{} fileSnRepeatList:{}  chargePileMap:{} gunTypeMap:{}", JSON.toJSONString(pileList), JSON.toJSONString(fileSnRepeatList), JSON.toJSONString(chargePileMap), JSON.toJSONString(gunTypeMap));
        List<PileImportNewV2DTO> insertImportList = pileList.stream().filter(p -> org.apache.commons.lang3.StringUtils.isBlank(p.getResult())).collect(Collectors.toList());

        log.info("savePileListV2 {}", JSON.toJSONString(insertImportList));
        List<PileImportNewV2DTO> pileImportNewV2DTOS = savePileListV2(insertImportList, merchantId, zoneId);//
//不符合条件的桩,先保存到缓存，输出excel
        List<PileImportNewV2DTO> errorImportList = pileList.stream().filter(p -> org.apache.commons.lang3.StringUtils.isNotBlank(p.getResult())).collect(Collectors.toList());
        Map<String, Object> resultMap = new HashMap<>();
        Integer total = pileList.size();
        Integer numberOfFailures = CollUtil.isNotEmpty(errorImportList) ? errorImportList.size() : 0;
        if (CollUtil.isNotEmpty(errorImportList)) {
            //错误列表加入缓存
            setErrorImportCache(errorImportList, FilenameUtils.getBaseName(originalFilename));
            resultMap.put("hasErrorFile", Boolean.TRUE);
        } else {
            resultMap.put("hasErrorFile", Boolean.FALSE);
        }
        resultMap.put("numberOfSuccesses", (total - numberOfFailures));
        resultMap.put("numberOfFailures", numberOfFailures);
        resultMap.put("status", "success");
        log.info("assert pileImportNewV2DTOS.size() + numberOfFailures == total ?, {}", pileImportNewV2DTOS.size() + numberOfFailures == total);
        log.info("importPile,文件名:{}导入成功;总条数:{};成功{}条;失败{}条;总耗时:{} ms;", total, (total - numberOfFailures), numberOfFailures, originalFilename, (System.currentTimeMillis() - startTime));
        return resultMap;
    }

    private void verifySn(PileImportNewV2DTO pile, ChargePileDTO chargePileDTO) {
        if (org.apache.commons.lang3.StringUtils.isBlank(pile.getSn())) {
            setResult(pile, ImportErrorEnum.IMPORT_SN_REQUIRED);
        } else if ((com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePileDTO) || com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePileDTO.getSn())) && pile.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
            setResult(pile, ImportErrorEnum.SN_NOT_FOUND);
        }
    }

    private void verifyAutelPin(PileImportNewV2DTO pile, ChargePileDTO chargePileDTO) {
        if (org.apache.commons.lang3.StringUtils.isBlank(pile.getPin())) {
            setResult(pile, ImportErrorEnum.IMPORT_PIN_REQUIRED);
        } else if (Objects.nonNull(chargePileDTO) && !chargePileDTO.getPin().equals(pile.getPin())) {
            setResult(pile, ImportErrorEnum.PIN_SN_NOT_MAPPING);
        }
    }

    /**
     * 校验并且保存桩数据
     */
    private void checkDataV2(List<PileImportNewV2DTO> pileImportNewDTOList, Map<String, ChargePileDTO> chargePileMap, List<String> fileSnRepeatList, Map<String, Integer> gunTypeMap) {
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(pileImportNewDTOList)) {
            throw new MessageCodeException(PileBaseEnum.EMPTY_FILE);
        }
        pileImportNewDTOList.forEach(pile -> {
            if (org.apache.commons.lang3.StringUtils.isBlank(pile.getPileName())) {
                pile.setPileName(pile.getSn());
            }
            if (org.apache.commons.lang3.StringUtils.isBlank(pile.getBrand())) {
                setResult(pile, ImportErrorEnum.IMPORT_BRAND_REQUIRED);
            } else if (!BrandEnum.findBrand(pile.getBrand())) {
                setResult(pile, ImportErrorEnum.BRAND_ERROR);
            }
            Set<String> fileSnSet = new HashSet<>();
            if (fileSnSet.contains(pile.getSn())) {
                setResult(pile, ImportErrorEnum.FILE_SN_IS_REPEAT);
            }
            if (CollUtil.isNotEmpty(fileSnRepeatList) && fileSnRepeatList.contains(pile.getSn())) {
                fileSnSet.add(pile.getSn());
            }
            ChargePileDTO chargePileDTO = chargePileMap.get(pile.getSn());
            verifySn(pile, chargePileDTO);
            if (BrandEnum.AUTEL.getName().equalsIgnoreCase(pile.getBrand())) {
                verifyAutelPin(pile, chargePileDTO);
            } else {
                log.info("批量导入充电桩校验,gunTypeMap={}", gunTypeMap);
                verifyThiredPile(gunTypeMap, pile);
            }

            //校验是否美标极简版桩

            Result<ChargePileDTO> chargePileDetailResult = null;
            try {
                chargePileDetailResult = deviceServiceFeign.query(pile.getSn());
            } catch (Exception e) {
                log.error("OpLocationEvseRepositoryImpl validEvse" + e);
            }
            if (chargePileDetailResult != null && chargePileDetailResult.getData() != null) {
                String productModel = chargePileDetailResult.getData().getProductModel();
                if (org.apache.commons.lang3.StringUtils.isNotBlank(productModel)) {
                    for (AmericaPileProductModelEnum americaPileProductModelEnum : AmericaPileProductModelEnum.values()) {
                        if (Objects.equals(productModel, americaPileProductModelEnum.getValue())) {
                            setResult(pile, ImportErrorEnum.AMERICA_PILE_ARE_NOT_ALLOW_ADD);
                        }
                    }
                }
            }
            String sn = pile.getSn();
            if (StringUtils.isEmpty(sn)) {
                return;
            }

            Integer deviceType = this.getDeviceType(GetDeviceTypeDTO
                    .builder()
                    .sn(pile.getSn())
                    .brandId(BrandEnum.findCode(pile.getBrand()) == null ? null : Long.valueOf(BrandEnum.findCode(pile.getBrand())))
                    .build());
            if (DeviceTypeEnum.OVERCHARGING_THE_HOST.getCode().equals(deviceType)) {
                this.setResult(pile, ImportErrorEnum.PLEASE_ADD_THE_OVERCHARGED_HOST_MANUALLY);
            } else if (DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode().equals(deviceType)) {
                this.setResult(pile, ImportErrorEnum.PLEASE_ADD_OVERCHARGING_TERMINAL_MANUALLY);
            }

//            //5、校验桩是否被绑定
//            if (org.apache.commons.lang3.StringUtils.isNotBlank(pile.getSn()) && org.apache.commons.lang3.StringUtils.isNotBlank(pile.getPin()) && BrandEnum.AUTEL.getName().equals(pile.getBrand()) && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pile.getResult())) {
//                Result<Object> result = dataServiceFeign.addPile(pile.getSn(), pile.getPin());
//                log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
//                if (result != null && Objects.equals(result.getCode(), org.springframework.http.HttpStatus.OK.value()) && !Objects.equals(result.getData(), Boolean.TRUE)) {
//                    setResult(pile, ImportErrorEnum.THE_PILE_HAS_BEEN_BOUND_AND_CANNOT_BE_ADDED);
//                }
//            }
//            //4、data-service是否已被绑定为家桩
//            if (!pile.getBrand().equals(BrandEnum.AUTEL.getName()) && com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pile.getResult())) {
//                Result<Boolean> restResult = homePileFeignClient.queryBind(pile.getSn());
//                if (restResult != null && restResult.getCode().equals(org.springframework.http.HttpStatus.OK.value()) && restResult.getData().equals(Boolean.TRUE)) {
//                    setResult(pile, ImportErrorEnum.THIS_PILE_HAS_BEEN_BOUND_AS_A_HOME_PILE);
//                }
//            }
        });
    }

    private void verifyThiredPile(Map<String, Integer> gunTypeMap, PileImportNewV2DTO pile) {
        log.info("批量导入桩第三方桩校验,gunTypeMap={},pile={}", gunTypeMap, pile);
        if (org.apache.commons.lang3.StringUtils.isBlank(pile.getChargingType())) {
            setResult(pile, ImportErrorEnum.IMPORT_CHARGE_TYPE_REQUIRED);
        } else if (!(AC_1_PHASE.equals(pile.getChargingType())
                || AC_3_PHASE.equals(pile.getChargingType())
                || DC.equals(pile.getChargingType()) || AC.equals(pile.getChargingType()))) {
            setResult(pile, ImportErrorEnum.CHARGE_TYPE_ERROR);
        }
        if (org.apache.commons.lang3.StringUtils.isBlank(pile.getChargingPower())) {
            setResult(pile, ImportErrorEnum.CHARGING_POWER_REQUIRED);
        } else {
            try {
                Double.valueOf(pile.getChargingPower());
            } catch (Exception e) {
                setResult(pile, ImportErrorEnum.CHARGE_POWER_ERROR);
            }
        }
        if (org.apache.commons.lang3.StringUtils.isBlank(pile.getGun1Type())) {
            setResult(pile, ImportErrorEnum.IMPORT_GUN1_REQUIRED);
        }
//        else if (!gunTypeMap.containsKey(pile.getGun1Type())) {
//            setResult(pile, ImportErrorEnum.GUN_TYPE_ERROR);
//        }

        if (org.apache.commons.lang3.StringUtils.isBlank(pile.getModel())) {
            setResult(pile, ImportErrorEnum.PRODUCT_MODEL_CANNOT_BE_EMPTY);
        } else if (org.apache.commons.lang3.StringUtils.isNotBlank(pile.getBrand())
                && !this.checkBrandModel(pile.getBrand(), pile.getModel())) {
            setResult(pile, ImportErrorEnum.PRODUCT_MODEL_ENTERED_INCORRECTLY);
        }
    }

    private boolean checkBrandModel(String brandName, String brandModel) {

        if (org.apache.commons.lang3.StringUtils.isBlank(brandName)
                || org.apache.commons.lang3.StringUtils.isBlank(brandModel)) {
            return false;
        }

        List<String> modelList = opEvseBrandModelRepository.getModelListByBrandName(brandName);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(modelList)) {
            return false;
        }

        for (String model : modelList) {
            if (brandModel.equalsIgnoreCase(model)) {
                return true;
            }
        }
        return false;
    }

    private void setErrorImportCache(List<PileImportNewV2DTO> errorImportList, String fileName) {
        try {
            JwtInfo loginUser = LoginUserHolder.getLoginUser();
            Payload payload = loginUser.getPayload();
            Long userId = Optional.ofNullable(payload).map(Payload::getUserId).orElse(0L);
            String item = RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE + userId;
            log.info("setErrorImportCacheData key={} data = {}", item, JSON.toJSONString(errorImportList));
            redis2Util.hset(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, item, errorImportList, 10 * 60000);
            String item1 = RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE_NAME + userId;
            log.info("setErrorImportCacheFileName key = {}  fileName = {}", item1, fileName);
            redis2Util.hset(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, item1, fileName, 10 * 60000);
        } catch (Exception e) {
            log.error("charge_pile_import_error_file,failed...", e);
        }
    }

    private File multipartFile2File(MultipartFile multipartFile) {
        File file = null;
        try {
            //获得原始文件名
            String fileName = multipartFile.getOriginalFilename();
            assert fileName != null;
            file = File.createTempFile(fileName.substring(0, fileName.lastIndexOf(".")), fileName.substring(fileName.lastIndexOf(".")));
            multipartFile.transferTo(file);
        } catch (IOException e) {
            log.error("multipartFile2File:", e);
        }
        return file;
    }

    private List<PileImportNewV2DTO> savePileListV2(List<PileImportNewV2DTO> saveList, Long merchantId, String zoneId) {
        log.info("要保存的数据：{}", JSON.toJSONString(saveList));
        List<ChargePointMerchantRelationEntity> createList = Lists.newArrayList();
        List<PileImportNewV2DTO> successList = Lists.newArrayList();
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(saveList)) {
            //循环每一个桩
            for (PileImportNewV2DTO pileImportNewDTO : saveList) {
                //构建createEVSE的参数
                OpLocationEvseDTO opLocationEvseDTO = buildCreateEVSEDTOV2(pileImportNewDTO);
                log.info("opLocationEvseDTO {}", JSON.toJSONString(opLocationEvseDTO));
                ChargePointDTO chargePointDTO = convert(opLocationEvseDTO);
                chargePointDTO.setZoneId(zoneId);
                try {
                    ChargePointMerchantRelationEntity chargePointEntity = save(chargePointDTO, merchantId);
                    createList.add(chargePointEntity);
                    successList.add(pileImportNewDTO);
                } catch (MessageCodeException e) {
                    pileImportNewDTO.setResult(MessageSourceHolder.getMessage(String.valueOf(e.getCode()), e.getParams()));
                    log.error("valid Failed", e);
                    log.info("chargePointDTO = {} ;  merchantId = {}", JSON.toJSONString(chargePointDTO), merchantId);
                } catch (Exception e) {
                    log.error("saveFailed", e);
                    log.info("chargePointDTO = {} ;  merchantId = {}", JSON.toJSONString(chargePointDTO), merchantId);
                }
            }
        }
        log.info("saveList = {}", JSON.toJSONString(createList));
        log.info("importSuccessList = {}", JSON.toJSONString(successList));
        return successList;
    }


    private OpLocationEvseDTO buildCreateEVSEDTOV2(PileImportNewV2DTO pileImportNewDTO) {
        if (org.apache.commons.lang3.StringUtils.isBlank(pileImportNewDTO.getBrand())) {
            setResult(pileImportNewDTO, ImportErrorEnum.IMPORT_BRAND_REQUIRED);
        }

        ChargePileDTO chargePileDTO = getChargePileDTO(pileImportNewDTO);
        //获取枪类型和数量
        Result<GunTypeAndNumVO> result = deviceServiceFeign.getGunTypeAndNum(pileImportNewDTO.getSn());
        //转成createEVSE的参数格式
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        //根据品牌名称查询品牌
        Long brandId = 2L;
        LambdaQueryWrapper<OpEvseBrandEntity> brandEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpEvseBrandEntity.class)
                .eq(OpEvseBrandEntity::getName, pileImportNewDTO.getBrand()).eq(OpEvseBrandEntity::getDeleted, 0);
        List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectList(brandEntityLambdaQueryWrapper);
        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opEvseBrandEntities)) {
            brandId = opEvseBrandEntities.get(0).getId();
        }
        opLocationEvseDTO.setBrandId(brandId);
        opLocationEvseDTO.setBrandName(pileImportNewDTO.getBrand());
        opLocationEvseDTO.setPileName(org.apache.commons.lang3.StringUtils.isBlank(pileImportNewDTO.getPileName()) ? pileImportNewDTO.getSn() : pileImportNewDTO.getPileName());
        opLocationEvseDTO.setPileSN(pileImportNewDTO.getSn());
        opLocationEvseDTO.setPinCode(pileImportNewDTO.getPin());
        opLocationEvseDTO.setProductModel(pileImportNewDTO.getModel());
        opLocationEvseDTO.setProductNamePdm(chargePileDTO.getProductNamePdm());
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileImportNewDTO.getChargingPower())) {
            opLocationEvseDTO.setPower(Double.valueOf(pileImportNewDTO.getChargingPower()));
        }
        int category = chargePileDTO.getCategory() == null ? CategoryEnum.AC.getCode() : chargePileDTO.getCategory();
        opLocationEvseDTO.setCategory(category);
        String type = CategoryEnum.getEnumByCode(category).getDesc();
        int phase = chargePileDTO.getPhase() == null ? 3 : chargePileDTO.getPhase();
        opLocationEvseDTO.setPowerType(type);
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileImportNewDTO.getPhase())) {
            opLocationEvseDTO.setPhase(pileImportNewDTO.getPhase());
        } else {
            opLocationEvseDTO.setPhase(type + "_" + phase + "_" + phaseConstant);
        }
        opLocationEvseDTO.setThirdPart(pileImportNewDTO.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName()) ? 0 : 1);
        //连接器
        List<OpLocationConnectorDTO> oplocationConnectorScanDTOS = getOpLocationConnectorDTOS(pileImportNewDTO, result);
        opLocationEvseDTO.setOpLocationConnectorDTOs(oplocationConnectorScanDTOS);
        if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePileDTO) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(chargePileDTO.getOutputPower())) {
            opLocationEvseDTO.setPower(chargePileDTO.getOutputPower());
        }
        return opLocationEvseDTO;
    }

    private ChargePileDTO getChargePileDTO(PileImportNewV2DTO pileImportNewDTO) {
        ChargePileDTO chargePileDTO = null;
        //根据SN获取桩详细信息
        if (pileImportNewDTO.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
            Result<ChargePileDTO> chargePileDTOResult = deviceServiceFeign.query(pileImportNewDTO.getSn());
            if (chargePileDTOResult == null || chargePileDTOResult.getCode() != org.springframework.http.HttpStatus.OK.value()) {
                setResult(pileImportNewDTO, ImportErrorEnum.SN_NOT_FOUND);
            }
            chargePileDTO = chargePileDTOResult.getData();
            log.info("feign调用device服务，根据SN获取桩详细信息结果：{}", JSON.toJSONString(chargePileDTO));
        }
        if (chargePileDTO == null) {
            chargePileDTO = new ChargePileDTO();
        }
        return chargePileDTO;
    }

    private void setResult(PileImportNewV2DTO pile, PileBaseEnum pileBaseEnum) {
        pile.setResult(MessageSourceHolder.getMessage(String.valueOf(pileBaseEnum.getCode())));
    }
    private void setResult(PileImportNewV2DTO pile, ImportErrorEnum importBrandIsRequired) {
        pile.setResult(MessageSourceHolder.getMessage(String.valueOf(importBrandIsRequired.getCode())));
    }

    private List<OpLocationConnectorDTO> getOpLocationConnectorDTOS(PileImportNewV2DTO pileImportNewDTO, Result<GunTypeAndNumVO> result) {
        List<OpLocationConnectorDTO> oplocationConnectorScanDTOS = new ArrayList<>();
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileImportNewDTO.getGun1Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(1));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun1Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        } else if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result) && result.getCode() == 200 && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("1"))) {
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(1));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("1")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileImportNewDTO.getGun2Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(2));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun2Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        } else if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result) && result.getCode() == 200 && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("2"))) {
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(2));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("2")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileImportNewDTO.getGun3Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(3));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun3Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        } else if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result) && result.getCode() == 200 && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("3"))) {
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(3));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("3")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileImportNewDTO.getGun4Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(4));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun4Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        } else if (!com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result) && result.getCode() == 200 && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("4"))) {
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(4));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("4")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        return oplocationConnectorScanDTOS;
    }

    private ChargePointDTO convert(OpLocationEvseDTO opLocationEvseDTO) {
        ChargePointDTO chargePointDTO = new ChargePointDTO();
        chargePointDTO.setSn(opLocationEvseDTO.getPileSN());
        chargePointDTO.setPin(opLocationEvseDTO.getPinCode());
        chargePointDTO.setBrandId(opLocationEvseDTO.getBrandId());
        chargePointDTO.setBrandName(opLocationEvseDTO.getBrandName());
        chargePointDTO.setName(opLocationEvseDTO.getPileName());
        chargePointDTO.setPartProduct(opLocationEvseDTO.getProductModel());
        chargePointDTO.setProductNamePdm(opLocationEvseDTO.getProductNamePdm());
        if (Objects.nonNull(opLocationEvseDTO.getPower())) {
            chargePointDTO.setRatedPower(BigDecimal.valueOf(opLocationEvseDTO.getPower()));
        }
        chargePointDTO.setPowerType(opLocationEvseDTO.getPowerType());
        chargePointDTO.setPhases(opLocationEvseDTO.getPhase());
        chargePointDTO.setConnectors(opLocationEvseDTO.getOpLocationConnectorDTOs().stream().map((t) -> {
            Connector connector = new Connector();
            connector.setConnectorId(Integer.valueOf(t.getConnectorId()));
            connector.setConnectorType(t.getGunType());
            connector.setPower(BigDecimal.valueOf(t.getPower()));
            connector.setConnectorName(ConnectorGunTypeEnum.getEnumByCode(t.getGunType()).getName());
            return connector;
        }).collect(Collectors.toList()));

        return chargePointDTO;
    }


    @Override
    public void downloadImportErrorFile(String language, HttpServletResponse response) {
        log.info("downloadImportErrorFile:{}", language);
        String fileName = "";
        List<PileImportNewV2DTO> errorList = new ArrayList<>();
        try {
            JwtInfo loginUser = LoginUserHolder.getLoginUser();
            Payload payload = loginUser.getPayload();
            Long userId = Optional.ofNullable(payload).map(Payload::getUserId).orElse(0L);
            String item = RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE + userId;
            errorList = (List<PileImportNewV2DTO>) redis2Util.hget(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, item);
            log.info("getErrorImportCacheData key={} data = {}", item, JSON.toJSONString(errorList));
            String item1 = RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE_NAME + userId;
            fileName = (String) redis2Util.hget(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, item1);
            log.info("getErrorImportCacheFileName key = {}  fileName = {}", item1, fileName);
        } catch (Exception e) {
            log.error("downloadImportErrorFile error:", e);
        }
        log.info("errorList:{}", JSON.toJSONString(errorList));
        if (CollUtil.isEmpty(errorList)) {
            throw new BusinessException(ImportErrorEnum.FILE_HAS_EXPIRED);
        }
        if (org.apache.commons.lang3.StringUtils.isBlank(fileName)) {
            fileName = "import_error";
        } else {
            fileName = fileName + "_import_error";
        }
        exportErrorList(language, response, errorList, fileName);
    }

    @Override
    public Set<String> findSellerByCount(RangeConditionDto rangeConditionDto) {
        if (ObjectUtils.isEmpty(rangeConditionDto) || ObjectUtils.isEmpty(rangeConditionDto.getRange())) {
            return null;
        }
        //查找商家
        Set<String> result = selectOwner(rangeConditionDto);
        //过滤道通端
        result.remove("0");
        return result;
    }

    private Set<String> selectOwner(RangeConditionDto rangeConditionDto) {
        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(ChargePointMerchantRelationEntity::getMerchantId);
        queryWrapper.groupBy(ChargePointMerchantRelationEntity::getMerchantId);
        if (rangeConditionDto.getRange()) {
            if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())) {
                queryWrapper.last("HAVING (COUNT(merchant_id) >=" + rangeConditionDto.getLowerLimit() + ")");
            } else if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && !ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())) {
                queryWrapper.last("HAVING (COUNT(merchant_id) >=" + rangeConditionDto.getLowerLimit() + " and count(merchant_id) <=" + rangeConditionDto.getUpperLimit() + ")");
            }
        } else {
            if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())) {
                queryWrapper.last("HAVING (COUNT(merchant_id) <" + rangeConditionDto.getLowerLimit() + ")");
            } else if (!ObjectUtils.isEmpty(rangeConditionDto.getLowerLimit()) && !ObjectUtils.isEmpty(rangeConditionDto.getUpperLimit())) {
                queryWrapper.last("HAVING (COUNT(merchant_id) <" + rangeConditionDto.getLowerLimit() + " or count(merchant_id) >" + rangeConditionDto.getUpperLimit() + ")");
            }
        }
        List<ChargePointMerchantRelationEntity> chargePointEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(chargePointEntities)) {
            return new HashSet<>();
        }
        return chargePointEntities.stream().filter(k -> !ObjectUtils.isEmpty(k) && !ObjectUtils.isEmpty(k.getMerchantId())).map(m -> String.valueOf(m.getMerchantId())).collect(Collectors.toSet());
    }

    private void exportErrorList(String language, HttpServletResponse response, List<PileImportNewV2DTO> errorImportList, String fileName) {
        long startTime = System.currentTimeMillis();
        List<PileImportErrorDTO> dataList = new ArrayList<>();
        errorImportList.forEach(pile -> {
            PileImportErrorDTO pileImportErrorDTO = new PileImportErrorDTO();
            BeanUtils.copyProperties(pile, pileImportErrorDTO);
            dataList.add(pileImportErrorDTO);
        });
        exportErrorPileExcelFile(dataList, language, response, fileName);
        log.info("错误文件导出总耗时:{} ms", (System.currentTimeMillis() - startTime));
    }

    private void exportErrorPileExcelFile(List<PileImportErrorDTO> dataList, String language, HttpServletResponse response, String importFileName) {
        log.info("language:{};exportErrorPileExcelFile:{}", language, JSON.toJSONString(dataList));
        ExcelWriter excelWriter = null;
        AtomicInteger sheetNo = new AtomicInteger(0);
        String fileName = importFileName + ".xlsx";
        try {
            fileName = URLEncoder.encode(fileName, UTF_8);
        } catch (Exception e) {
            log.error("导出文件名转换有误", e);
        }
        response.setContentType(BaseConstant.CONTENT_TYPE);
        response.setCharacterEncoding(UTF_8);
        response.setHeader(BaseConstant.HEAD_NAME, BaseConstant.HEAD_VALUE + fileName);
        try {
            excelWriter = EasyExcelFactory.write(response.getOutputStream()).build();
            int num = sheetNo.get();
            List<CellStyleModel> styleModelList = new ArrayList<>();
            CellStyleModel styleModel = new CellStyleModel();
            styleModel.setColIndex(13);
            styleModel.setFontColor(IndexedColors.RED.getIndex());
            styleModelList.add(styleModel);
            CellStyleModel styleModel2 = new CellStyleModel();
            styleModel2.setRowIndex(2);
            styleModel2.setFontColor(IndexedColors.RED.getIndex());
            styleModel2.setBackgroundColor(IndexedColors.BLUE.getIndex());
            styleModelList.add(styleModel2);
            WriteSheet writeSheet = EasyExcelFactory.writerSheet(num, fileName)
                    .head(PileImportErrorDTO.class)
                    .registerWriteHandler(new SimpleColumnWidthStyleStrategy(15))
                    .registerWriteHandler(new CustomCellStyleHandler(styleModelList)).build();
            excelWriter.write(dataList, writeSheet);
            sheetNo.incrementAndGet();
        } catch (Exception e) {
            log.error("exportErrorPileExcelFileFailed", e);
        } finally {
            if (excelWriter != null) {
                excelWriter.finish();
            }
        }
    }

    /**
     * @see com.autel.cloud.pile.base.enums.chargepoint.DeviceTypeEnum
     */
    private Map<String, Integer> getDeviceSnAndDeviceTypeEnumMap(Set<String> deviceSnSet, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getDeviceType deviceSnSet : {} and merchantId : {}",
                JSON.toJSONString(deviceSnSet),
                JSON.toJSONString(merchantId));

        Map<String, Integer> deviceSnAndDeviceTypeEnumMap = new HashMap<>();

        List<ChargePointMerchantRelationEntity> relationEntitys = this.getPileList(new ArrayList<>(deviceSnSet), merchantId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(relationEntitys)) {
            relationEntitys.forEach(val -> {
                if (OverchargingPileFlagEnum.OVERCHARGING_PILE.getCode().equals(val.getOverchargingPileFlag())) {
                    deviceSnAndDeviceTypeEnumMap.put(val.getSn(), DeviceTypeEnum.OVERCHARGING_THE_HOST.getCode());
                } else {
                    deviceSnAndDeviceTypeEnumMap.put(val.getSn(), DeviceTypeEnum.OTHER_DEVICE.getCode());
                }
            });
        }

        List<ChargePointMerchantTerminalEntity> terminalEntitys = chargePointMerchantTerminalService.getTerminalList(new ArrayList<>(deviceSnSet), merchantId);
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(terminalEntitys)) {
            terminalEntitys.forEach(val -> deviceSnAndDeviceTypeEnumMap.put(val.getTerminalSn(), DeviceTypeEnum.OVERCHARGING_TERMINAL.getCode()));
        }
        return deviceSnAndDeviceTypeEnumMap;
    }

    private List<ChargePointMerchantRelationEntity> getPileList(List<String> pileSnList, Long merchantId) {

        log.info("===>>> ChargePointMerchantRelationServiceImpl.getPileList pileSnList : {} and merchantId : {}",
                JSON.toJSONString(pileSnList),
                JSON.toJSONString(merchantId));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileSnList)
                && merchantId == null) {
            return null;
        }

        return chargePointMerchantRelationMapper.selectList(
                new LambdaQueryWrapper<ChargePointMerchantRelationEntity>()
                        .in(com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(pileSnList), ChargePointMerchantRelationEntity::getSn, pileSnList)
                        .eq(merchantId != null, ChargePointMerchantRelationEntity::getMerchantId, merchantId));
    }

    @Override
    public void exportExcel(HttpServletRequest request, HttpServletResponse response) throws IOException {
        log.info("download start.");

        Long sellerId = LoginUserUtil.getSellerId();
        //Long sellerId = 1530749400295227393L;

        LambdaQueryWrapper<ChargePointMerchantRelationEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantRelationEntity.class);
        queryWrapper.eq(ChargePointMerchantRelationEntity::getMerchantId, sellerId);

        List<ChargePointMerchantRelationEntity> chargePointMerchantRelationEntities = chargePointMerchantRelationMapper.selectList(queryWrapper);
        List<String> sns = chargePointMerchantRelationEntities.stream().map(ChargePointMerchantRelationEntity::getSn).collect(Collectors.toList());

        // 查询终端
        LambdaQueryWrapper<ChargePointMerchantTerminalEntity> terminalQueryWrapper = new QueryWrapper<ChargePointMerchantTerminalEntity>().lambda().eq(ChargePointMerchantTerminalEntity::getMerchantId, sellerId);

        List<ChargePointMerchantTerminalEntity> terminalEntities = chargePointMerchantTerminalMapper.selectList(terminalQueryWrapper);
        List<String> terminalSnList = terminalEntities.stream().map(ChargePointMerchantTerminalEntity::getTerminalSn).collect(Collectors.toList());

        List<OpLocationPileEvseElasticDTO> locationPiles = opLocationPileEvseElastic.findByPileSnIn(sns);
        Map<String, String> pileLocationMap = new HashMap<>();
        if (Objects.nonNull(locationPiles) && !CollectionUtils.isEmpty(locationPiles)) {
            for (OpLocationPileEvseElasticDTO pileEvseElasticDTO : locationPiles) {
                if (pileLocationMap.containsKey(pileEvseElasticDTO.getPileSn())) {
                    log.warn(" Duplicate key {}", pileEvseElasticDTO.getPileSn());
                }
                if (LoginUserUtil.getSellerId().equals(pileEvseElasticDTO.getOperatorId())) { // AB商家同时添加了同一个桩到资产，A添加该桩到场站，在B商家的账号能看到A商家的场站名称
                    pileLocationMap.put(pileEvseElasticDTO.getPileSn(), pileEvseElasticDTO.getLocationName());
                }
            }
        }

        // 导出集合
        List<ChargePointSubViewVO> exportList = new ArrayList<>();

        // 设置订阅 明细
        if (CollUtil.isNotEmpty(terminalSnList)) {
            sns.addAll(terminalSnList);
        }
        LambdaQueryWrapper<TbLenBindRelationEntity> lenQueryWrapper = Wrappers.lambdaQuery();
        lenQueryWrapper.in(TbLenBindRelationEntity::getPileSn, sns);
        lenQueryWrapper.eq(TbLenBindRelationEntity::getTenantId, sellerId);
        List<TbLenBindRelationEntity> tbLenBindRelationEntities = tbLenBindRelationMapper.selectList(lenQueryWrapper);

        String zoneId = request.getHeader("x-timezone") == null ? ZoneId.systemDefault().getId() : request.getHeader("x-timezone");

        chargePointMerchantRelationEntities.forEach(point -> {
            ChargePointSubViewVO subViewVO = new ChargePointSubViewVO();
            subViewVO.setSn(point.getSn());
            subViewVO.setName(point.getName());
            subViewVO.setLocationName(Optional.ofNullable(pileLocationMap.get(point.getSn())).orElse("--"));

            // 过滤sn
            List<TbLenBindRelationEntity> snCollect = tbLenBindRelationEntities.stream().filter(item -> point.getSn().equalsIgnoreCase(item.getPileSn())).collect(Collectors.toList());
            Map<String, TbLenBindRelationEntity> maxUnavailableTimeByGoodsId = snCollect.stream().collect(Collectors.toMap(TbLenBindRelationEntity::getServiceId, Function.identity(), BinaryOperator.maxBy(Comparator.comparing(TbLenBindRelationEntity::getUnavailableTime))));

            handSubscribeDetail(maxUnavailableTimeByGoodsId, subViewVO, zoneId);

            exportList.add(subViewVO);

            // 判断是否有终端
            List<ChargePointMerchantTerminalEntity> result = terminalEntities.stream().filter(entity -> point.getSn().equals(entity.getHostSn())).collect(Collectors.toList());
            result.forEach(terminal -> {
                ChargePointSubViewVO subViewTerminalVO = new ChargePointSubViewVO();
                subViewTerminalVO.setSn(terminal.getTerminalSn());
                subViewTerminalVO.setName(terminal.getTerminalName());
                subViewTerminalVO.setLocationName("--");

                // 过滤sn
                List<TbLenBindRelationEntity> snCollectTerminal = tbLenBindRelationEntities.stream().filter(item -> terminal.getTerminalSn().equalsIgnoreCase(item.getPileSn())).collect(Collectors.toList());
                Map<String, TbLenBindRelationEntity> maxUnavailableTimeByGoodsIdTerminal = snCollectTerminal.stream().collect(Collectors.toMap(TbLenBindRelationEntity::getServiceId, Function.identity(), BinaryOperator.maxBy(Comparator.comparing(TbLenBindRelationEntity::getUnavailableTime))));

                handSubscribeDetail(maxUnavailableTimeByGoodsIdTerminal, subViewTerminalVO, zoneId);

                exportList.add(subViewTerminalVO);
            });
        });

        // 处理完数据，导出
        log.info("export list size : {}", exportList.size());

        String excelType = "xlsx";

        LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(System.currentTimeMillis()), ZoneId.of(zoneId));
        // 定义日期格式化器
        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyyMMdd");
        // 格式化日期
        String formattedDate = dateTime.format(dateFormatter);
        String fileName = messageSource.getMessage("_mhfuyCerlA4r", null, LocaleContextHolder.getLocale());
        ExcelResponseUtil.setResponseHeader(request, response, fileName + formattedDate, excelType);
        ServletOutputStream outputStream = response.getOutputStream();

        EasyExcelFactory.write(outputStream)
                .head(ChargePointSubViewVO.class)
                .registerWriteHandler(new I18nCellWriteHandler(messageSource, LocaleContextHolder.getLocale()))
                .locale(LocaleContextHolder.getLocale())
                .excelType(ExcelTypeEnum.XLSX)
                .sheet(fileName)
                .doWrite(exportList);

    }

    @Override
    public void exportChargePoint(ChargePointExportDTO chargePointExportDTO, HttpServletRequest request, HttpServletResponse response, Long merchantId, String zoneId) throws IOException {
        log.info("===========> exportChargePoint chargePointExportDTO:{}",chargePointExportDTO);
        ChargePointFuzzQueryDTO chargePointFuzzQueryDTO = new ChargePointFuzzQueryDTO();
        chargePointFuzzQueryDTO.setKeyword(chargePointExportDTO.getKeyword());
        chargePointFuzzQueryDTO.setOrderBy(chargePointExportDTO.getOrderBy());
        chargePointFuzzQueryDTO.setOrderType(chargePointExportDTO.getOrderType());
        int pageNum = 1;
        chargePointFuzzQueryDTO.setPage(pageNum);
        chargePointFuzzQueryDTO.setPageSize(MYBATIS_PLUS_PAGE_MAX_LIMIT);
        List<ChargePointAssetVO> chargePointsRecords = new ArrayList<>();
        IPage<ChargePointAssetVO> chargePointsByPage = findChargePointsByPage(chargePointFuzzQueryDTO, merchantId, zoneId);
        while (!CollectionUtils.isEmpty(chargePointsByPage.getRecords())){
            chargePointsRecords.addAll(chargePointsByPage.getRecords());
            pageNum += 1;
            chargePointFuzzQueryDTO.setPage(pageNum);
            chargePointsByPage = findChargePointsByPage(chargePointFuzzQueryDTO, merchantId, zoneId);
        }
        log.info("===========> exportChargePoint export pageNum:{}",pageNum-1);
        List<ChargePointExportVO> chargePointExportVOs = new ArrayList<>();
        for(ChargePointAssetVO chargePointAssetVO:chargePointsRecords){
            chargePointExportVOs.add(new ChargePointExportVO(
                    chargePointAssetVO.getSn(),
                    null,
                    chargePointAssetVO.getName(),
                    chargePointAssetVO.getBrandName(),
                    chargePointAssetVO.getPin(),
                    chargePointAssetVO.getLocationName()
            ));
            if(!CollectionUtils.isEmpty(chargePointAssetVO.getTerminals())){
                List<Terminal> terminals = chargePointAssetVO.getTerminals();
                for (Terminal terminal :terminals){
                    chargePointExportVOs.add(new ChargePointExportVO(
                            chargePointAssetVO.getSn(),
                            terminal.getTerminalSn(),
                            terminal.getTerminalName(),
                            chargePointAssetVO.getBrandName(),
                            terminal.getPin(),
                            chargePointAssetVO.getLocationName()
                    ));
                }
            }
        }
        /* 导出数据 */
        response.setCharacterEncoding("utf-8");
        String excelType = chargePointExportDTO.getExcelType();
        String fileName = messageSource.getMessage("_FnowIiZRxptC", null, LocaleContextHolder.getLocale());
        ExcelResponseUtil.setResponseHeader(request, response, fileName , excelType );
        /* 解决火狐文件名乱码以及文件名 + */
        if("csv".equalsIgnoreCase(excelType)){
            response.setHeader("Content-disposition","attachment;filename="+ UriUtils.encode(fileName, Constants.UTF_8) + ".csv");
        }else{
            response.setHeader("Content-disposition","attachment;filename*=utf-8''"+UriUtils.encode(fileName, Constants.UTF_8)+".xlsx");
        }
        OutputStream outputStream = response.getOutputStream();
        if (ExcelTypeEnum.CSV.name().equalsIgnoreCase(excelType)) {
            outputStream.write(new byte[]{(byte) 0xEF, (byte) 0xBB, (byte) 0xBF});
        }
        EasyExcelFactory.write(outputStream)
                .head(ChargePointExportVO.class)
                .registerWriteHandler(new I18nCellWriteHandler(messageSource, LocaleContextHolder.getLocale()))
                .registerWriteHandler(new EmptyCellWriteHandler(EMPTY_CELL_DEFAULT_VALUE))
                .locale(LocaleContextHolder.getLocale())
                .excelType(ExcelTypeEnum.CSV.name().equalsIgnoreCase(excelType) ? ExcelTypeEnum.CSV : ExcelTypeEnum.XLSX)
                .sheet(fileName)
                .doWrite(chargePointExportVOs);
    }


    private void handSubscribeDetail(Map<String, TbLenBindRelationEntity> maxUnavailableTimeByGoodsId, ChargePointSubViewVO subViewVO, String zoneId) {
        AtomicBoolean pro = new AtomicBoolean(false);
        AtomicBoolean lite = new AtomicBoolean(false);
        AtomicBoolean cms = new AtomicBoolean(false);
        AtomicBoolean mp = new AtomicBoolean(false);

        maxUnavailableTimeByGoodsId.forEach((key, item) -> {
            if (PileChargingRights.SERVICE_ID_ALL.contains(key)) {
                int status;
                Long expireTime = item.getUnavailableTime();

                if (!Integer.valueOf(2).equals(item.getStatus())) {
                    status = System.currentTimeMillis() > Optional.ofNullable(item.getUnavailableTime()).orElse(0L) ? 0 : 1;
                } else {
                    status = 0;
                    // 包含正常过期、直接删除桩的过期
                    if (System.currentTimeMillis() < Optional.ofNullable(item.getUnavailableTime()).orElse(0L)) {
                        // 直接删除桩的过期
                        expireTime = null;
                    }
                }

                if (key.endsWith("Pro")) {
                    pro.set(true);
                    // status=0, name = 未订阅
                    // status=1, **到期   INACTIVITY  expiration_date
                    //  orderData.setPayMethod(messageSource.getMessage("Prepaid", null, "Prepaid", LocaleContextHolder.getLocale()));
                    if (status == 0) {
                        subViewVO.setProName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
                    } else {
                        LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(expireTime), ZoneId.of(zoneId));
                        // 定义日期格式化器
                        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                        // 格式化日期
                        String date = dateTime.format(dateFormatter);
                        //MessageSourceHolder.getMessage("subscribe_expired", new Object[]{ startDateTime.format(formatter0), endDateTime.format(formatter0)}, locale);
                        String subscribeExpired = MessageSourceHolder.getMessage("subscribe_expired", new Object[]{date}, LocaleContextHolder.getLocale());
                        subViewVO.setProName(subscribeExpired);
                    }
                } else if (key.endsWith("Lite")) {
                    lite.set(true);
                    if (status == 0) {
                        subViewVO.setLiteName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
                    } else {
                        LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(expireTime), ZoneId.of(zoneId));
                        // 定义日期格式化器
                        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                        // 格式化日期
                        String date = dateTime.format(dateFormatter);
                        subViewVO.setLiteName(messageSource.getMessage("subscribe_expired", new Object[]{date}, date, LocaleContextHolder.getLocale()));
                    }
                } else if (key.endsWith("ads")) {
                    cms.set(true);
                    if (status == 0) {
                        subViewVO.setAdsName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
                    } else {
                        LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(expireTime), ZoneId.of(zoneId));
                        // 定义日期格式化器
                        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                        // 格式化日期
                        String date = dateTime.format(dateFormatter);
                        subViewVO.setAdsName(messageSource.getMessage("subscribe_expired", new Object[]{date}, date, LocaleContextHolder.getLocale()));
                    }
                } else if (key.endsWith("ops")) {
                    mp.set(true);
                    if (status == 0) {
                        subViewVO.setMpName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
                    } else {
                        LocalDateTime dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(expireTime), ZoneId.of(zoneId));
                        // 定义日期格式化器
                        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
                        // 格式化日期
                        String date = dateTime.format(dateFormatter);
                        subViewVO.setMpName(messageSource.getMessage("subscribe_expired", new Object[]{date}, date, LocaleContextHolder.getLocale()));
                    }
                }
            }
        });

        // 补齐未订阅的
        if (!pro.get()) {
            subViewVO.setProName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
        }
        if (!lite.get()) {
            subViewVO.setLiteName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
        }
        if (!cms.get()) {
            subViewVO.setAdsName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
        }
        if (!mp.get()) {
            subViewVO.setMpName(messageSource.getMessage("INACTIVITY", null, "INACTIVITY", LocaleContextHolder.getLocale()));
        }
    }

    @Override
    public Page<PileBaseVO> queryPagePileByPrivilege(QueryPilePageDTO paramDTO) {

        Page<PileBaseVO> page=new Page<>(paramDTO.getPage(),paramDTO.getPageSize());
        List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
        if(CollectionUtils.isEmpty(locationIdList)){
            page.setTotal(0);
            return page;
        }
        return opLocationPileEvseElasticService.findByPage(paramDTO,locationIdList);
    }

    @Override
    public List<PileBaseVO> queryPileBySn(QueryPileListDTO paramDTO) {
        List<PileBaseVO> pileBaseVOS = chargePointMerchantRelationMapper.queryPileBySn(paramDTO);

        List<String> sns = pileBaseVOS.stream().map(PileBaseVO::getSn).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> locationPiles = opLocationPileEvseElastic.findByPileSnIn(sns);
        if(org.apache.commons.collections4.CollectionUtils.isNotEmpty(locationPiles)){
            Map<String, OpLocationPileEvseElasticDTO> map = locationPiles.stream().collect(Collectors
                    .toMap(OpLocationPileEvseElasticDTO::getPileSn, Function.identity(), (v1, v2) -> v2));

            pileBaseVOS.forEach(e->{
                OpLocationPileEvseElasticDTO op = map.get(e.getSn());
                if (op != null) {
                    e.setLocationId(op.getLocationId());
                    e.setLocationName(op.getLocationName());
                }
            });
        }

        return  pileBaseVOS;
    }
}
