package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.StrUtil;
import com.alibaba.excel.EasyExcelFactory;
import com.alibaba.excel.ExcelWriter;
import com.alibaba.excel.read.listener.PageReadListener;
import com.alibaba.excel.util.FileUtils;
import com.alibaba.excel.util.ListUtils;
import com.alibaba.excel.write.metadata.WriteSheet;
import com.alibaba.excel.write.style.column.SimpleColumnWidthStyleStrategy;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.TypeReference;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.exception.BusinessException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.bo.EasyExcelSheetWriteHandlerForPile;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.I18nConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.DozerConvert;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileEvseConvert;
import com.autel.cloud.pile.base.domain.hanlder.CustomCellStyleHandler;
import com.autel.cloud.pile.base.domain.listener.BatchPageReadListener;
import com.autel.cloud.pile.base.domain.model.CellStyleModel;
import com.autel.cloud.pile.base.domain.model.PileQrCodeDTO;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantRelationService;
import com.autel.cloud.pile.base.domain.service.CommonUtilService;
import com.autel.cloud.pile.base.domain.service.OpCountryService;
import com.autel.cloud.pile.base.domain.service.impl.OpEvseTypeServiceImpl;
import com.autel.cloud.pile.base.domain.utils.Redis2Util;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.config.PileFreeVendInfoDTO;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.dto.pile.CheckPileNameDTO;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.enums.*;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.exception.MessageSourceUtil;
import com.autel.cloud.pile.base.infrastructure.feign.*;
import com.autel.cloud.pile.base.infrastructure.feign.dto.*;
import com.autel.cloud.pile.base.infrastructure.feign.vo.GunTypeAndNumVO;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.EasyExcelUtil;
import com.autel.cloud.pile.base.util.ListSortUtil;
import com.autel.cloud.pile.base.util.StringUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.GunTypeIconRespDTO;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;
import com.autel.cloud.pile.base.vo.britishAct.DefaultChargingTimeVO;
import com.autel.cloud.pile.base.vo.britishAct.ThatDayDefaultChargingTimeVO;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellAddress;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.elasticsearch.index.query.*;
import org.elasticsearch.action.search.ClearScrollRequest;
import org.elasticsearch.action.search.SearchRequest;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.action.search.SearchScrollRequest;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.core.TimeValue;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.TermsQueryBuilder;
import org.elasticsearch.index.query.WildcardQueryBuilder;
import org.elasticsearch.search.Scroll;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.Aggregations;
import org.elasticsearch.search.aggregations.bucket.terms.LongTerms;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.elasticsearch.search.sort.SortBuilder;
import org.elasticsearch.index.query.*;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.SearchHitsIterator;
import org.springframework.data.elasticsearch.core.query.*;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.IndexBoost;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.SearchHitsIterator;
import org.springframework.data.elasticsearch.core.query.*;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Repository;
import org.springframework.util.StopWatch;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.net.URLEncoder;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.AmqpConstant.PILE_BASE_GUN_DELETE_EXCHANGE;
import static com.autel.cloud.pile.base.constant.AmqpConstant.PILE_BASE_GUN_DELETE_ROUTE;
import static org.elasticsearch.index.query.QueryBuilders.matchAllQuery;

/**
 * <p>
 * 充电设备组合（桩） 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-05-10
 */
@Repository
@Slf4j
public class OpLocationPileEvseRepositoryImpl extends ServiceImpl<OpLocationPileEvseMapper, OpLocationPileEvseEntity> implements OpLocationPileEvseRepository {

    @Autowired
    private OpEvseBrandMapper opEvseBrandMapper;
    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;
    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;
    @Autowired
    private OpLocationPileEvseMapper opLocationPileEvseMapper;
    @Autowired
    private MessageSourceUtil messageSourceUtil;
    @Autowired
    private DeviceServiceFeign deviceServiceFeign;
    @Autowired
    private OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    private OpEvseBrandModelMapper opEvseBrandModelMapper;
    @Autowired
    private OpLocationMapper opLocationMapper;
    @Autowired
    private DataServiceFeign dataServiceFeign;
    @Resource
    private MonitorFeign monitorFeign;
    @Resource
    private TariffFeignClient tariffFeignClient;
    @Resource
    private HomePileFeignClient homePileFeignClient;
    @Resource
    private OpsMgmtClient opsMgmtClient;
    @Resource
    private OpLocationElastic opLocationElastic;
    @Resource
    private OpCountryService opCountryService;
    @Resource
    private OpLocationOperationMapper opLocationOperationMapper;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private RabbitTemplate rabbitTemplate;
    @Resource
    private OpLocationEvseExpandElastic opLocationEvseExpandElastic;
    @Resource
    private MonitorFeignClient monitorFeignClient;
    @Resource
    private OpEvseTypeServiceImpl opEvseTypeServiceImpl;
    @Resource
    @Lazy
    private OpLocationRepository opLocationRepository;

    @Autowired
    private CommonUtilService commonUtilService;

    @Resource
    private RestHighLevelClient restHighLevelClient;

    @Resource
    @Lazy
    private ChargePointMerchantRelationService chargePointMerchantRelationService;

    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String,Object> redisTemplate;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    private static final String PARTTERREG = "^[0-9a-zA-Z]+$";
    private static final String RANDOMDELAYTIME = "RandomDelayTime";
    private static final String OFFPEAKHOURPARAM = "OffPeakHourParam";
    public static final String AC_1_PHASE = "AC_1_PHASE";
    public static final String AC_3_PHASE = "AC_3_PHASE";
    public static final String DC = "DC";
    public static final String AC = "AC";
    public static final Long DAY = 86400000L;
    public static final String UTF_8 = "UTF-8";
    @Resource
    private Redis2Util redis2Util;

    private long timeoutRedisTtl = 10;

    @Value("${remoteStartPhoneNumber:00000000000}")
    private String remoteStartPhoneNumber;

    @PostConstruct
    public void evictCache(){
        evictCache("*");
    }

    @XxlJob("evictCacheOpLocationPileEvseElasticDTO")
    public ReturnT<String> evictCache(String param){
        String[] split = param.split(",");
        String opLocationPileGroupEntityKey = String.join(":", OpLocationPileEvseElasticDTO.class.getName(), split[0].trim());
        stringRedisTemplate.delete(opLocationPileGroupEntityKey);
        try {
            timeoutRedisTtl = Long.parseLong(split[1]);
        } catch (Exception e) {
            return ReturnT.FAIL;
        }
        return ReturnT.SUCCESS;
    }

    public void evictCache(Long id) {
        Collection<String> deleteKeys = new HashSet<>();
        String findByLocationIdKey = genRedisKey("findByLocationId", id.toString());
        deleteKeys.add(findByLocationIdKey);
        stringRedisTemplate.delete(deleteKeys);
    }

    public void evictCache(Collection<Long> ids){
        Collection<String> deleteKeys = new HashSet<>();
        for (Long id : ids) {
            String findByLocationIdKey = genRedisKey("findByLocationId", id.toString());
            deleteKeys.add(findByLocationIdKey);
        }
        stringRedisTemplate.delete(deleteKeys);
    }

    public static String genRedisKey(String method, String param) {
        return String.join(":", OpLocationPileEvseElasticDTO.class.getName(), method, param);
    }


    @Override
    public OpLocationPileEvseDTO queryByPileSn(String pileSn) {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.PILESN, pileSn));
//        Iterable<OpLocationPileEvseElasticDTO> locationPileEvseElasticDTOList = opLocationPileEvseElastic.search(boolQueryBuilder);
        ArrayList<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toCollection(Lists::newArrayList));
        if (CollectionUtils.isEmpty(opLocationPileEvseElasticDTOList)) {
            return null;
        }
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticDTOList.get(0);
        return OpLocationPileEvseConvert.toOpLocationPileEvseDTO(opLocationPileEvseElasticDTO);
    }

    @Override
    public List<OpLocationPileEvseDTO> queryByPileSnList(List<String> pileSnList) {
        List<OpLocationPileEvseDTO> list = new ArrayList<>();
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(pileSnList);
        if (opLocationPileEvseElasticDTOList == null) {
            return Collections.emptyList();
        }
        for (OpLocationPileEvseElasticDTO item : opLocationPileEvseElasticDTOList) {
            list.add(OpLocationPileEvseConvert.toOpLocationPileEvseDTO(item));
        }
        return list;
    }

    @Override
    public OpLocationPileEvseDTO queryByPileId(Long pileId) {
        Optional<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOOptional =
                opLocationPileEvseElastic.findById(pileId);
        return opLocationPileEvseElasticDTOOptional
                .map(OpLocationPileEvseConvert::toOpLocationPileEvseDTO).orElse(null);
    }

    /**
     * 场站详情桩分页查询
     *
     * @param pilePageDTO 场站id
     * @return 桩分页
     */
    @Override
    public Page<PilePageVO> stationPilePage(PilePageDTO pilePageDTO) {

        Long stationId = pilePageDTO.getStationId();
        String pileNameOrPileSN = pilePageDTO.getPileNameOrPileSN();
        Integer pileStatusCode = pilePageDTO.getEvseStatusCode();
        Integer page = pilePageDTO.getPage();
        Integer pageSize = pilePageDTO.getPageSize();
        String orderBy = pilePageDTO.getOrderBy();
        String orderType = pilePageDTO.getOrderType();


        //根据枪状态找出满足状态条件的桩
        Set<String> pileSNSet = new HashSet<>();
        // todo 当前前端不会传该字段可以注释
        if (pileStatusCode != null) {
            //枪的业务状态转为实时状态
            List<String> realTimeStatusNameList = LocationEVSEStatusEnum.businessStatus2RealTimeStatusName(pileStatusCode);
            BoolQueryBuilder boolQueryBuilder = new BoolQueryBuilder();
            if (!pileStatusCode.equals(LocationEVSEStatusEnum.UNAVAILABLE.getCode())) {
                boolQueryBuilder.must(QueryBuilders.termsQuery("state", realTimeStatusNameList));
            } else {
                //查询不可用，还要添加状态为空的数据
                boolQueryBuilder.must(QueryBuilders.boolQuery()
                        .should(QueryBuilders.termsQuery("state", realTimeStatusNameList))
                        .should(QueryBuilders.termQuery("state", ""))
                        .should(QueryBuilders.boolQuery().mustNot(QueryBuilders.existsQuery("state"))));
            }
            boolQueryBuilder.must(QueryBuilders.termQuery("locationId", stationId));

            // 只查询 在 数据库里面未删除的记录id
            LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationEvseEntity.class);
            queryWrapper.select(OpLocationEvseEntity::getId, OpLocationEvseEntity::getEvseSn);
            queryWrapper.eq(OpLocationEvseEntity::getLocationId, stationId);
            queryWrapper.eq(OpLocationEvseEntity::getDeleted, 0);
            List<OpLocationEvseEntity> evseEntityList = opLocationEvseMapper.selectList(queryWrapper);
            if (CollectionUtils.isNotEmpty(evseEntityList)) {// 场站里面有枪信息
                List<Long> collect = evseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toList());
                syncLocationEvse(stationId, collect);
                boolQueryBuilder.must(QueryBuilders.termsQuery(BaseConstant.ID, collect));
            } else {
                syncLocationEvse(stationId, Collections.emptyList());
                boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.ID, BaseConstant.NOT_EXIST_RECORD_ID));
            }

            log.info("状态满足条件的枪数据ES查询语言：{}", boolQueryBuilder);

//            Iterable<OpLocationEvseElasticDTO> opLocationEvseElasticDTOIterable = opLocationEvseElastic.search(boolQueryBuilder);
            ArrayList<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toCollection(Lists::newArrayList));

            log.info("状态满足条件的枪数据：{}", JSON.toJSONString(opLocationEvseElasticDTOList));
            if (CollectionUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                pileSNSet = opLocationEvseElasticDTOList.stream().map(OpLocationEvseElasticDTO::getPileSn).filter(Objects::nonNull).collect(Collectors.toSet());
                log.info("根据枪的业务状态，只要这些sn的桩能查出来：{}", JSON.toJSONString(pileSNSet));
            }
            if (CollectionUtils.isEmpty(pileSNSet)) {
                Page<PilePageVO> resultPage = new Page<>(pilePageDTO.getPage(), pilePageDTO.getPageSize());
                resultPage.setRecords(new ArrayList<>());
                resultPage.setTotal(0);
                return resultPage;
            }
        }

        //ES分页查询桩
        BoolQueryBuilder boolQueryBuilder1 = QueryBuilders.boolQuery();
        //站点id
        boolQueryBuilder1.must(QueryBuilders.termQuery("locationId", stationId));
        //桩SN
        if (CollectionUtils.isNotEmpty(pileSNSet)) {
            boolQueryBuilder1.must(QueryBuilders.termsQuery(BaseConstant.PILESN, pileSNSet));
        }
        // 只查询 在 数据库里面未删除的记录id
        LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
        queryWrapper.select(OpLocationPileEvseEntity::getId, OpLocationPileEvseEntity::getPileSn);
        queryWrapper.eq(OpLocationPileEvseEntity::getLocationId, stationId);
        queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> evseEntityList = opLocationPileEvseMapper.selectList(queryWrapper);
        if (CollectionUtils.isNotEmpty(evseEntityList)) { // stationId场站有桩信息
            List<Long> collect = evseEntityList.stream().map(OpLocationPileEvseEntity::getId).collect(Collectors.toList());
            syncLocationPileEvse(stationId, collect);
            boolQueryBuilder1.must(QueryBuilders.termsQuery(BaseConstant.ID, collect));
        } else {
            syncLocationPileEvse(stationId, Collections.emptyList()); // stationId场站里面没有一个桩信息
            boolQueryBuilder1.must(QueryBuilders.termQuery(BaseConstant.ID, BaseConstant.NOT_EXIST_RECORD_ID));
        }
        //搜索值
        BoolQueryBuilder boolQueryBuilder2 = QueryBuilders.boolQuery();
        if (StringUtils.isNotBlank(pileNameOrPileSN)) {
            //特殊字符转义
            String tmp = QueryParserBase.escape(pileNameOrPileSN);
            boolQueryBuilder2.should(QueryBuilders.wildcardQuery("name", "*" + tmp + "*"));
            boolQueryBuilder2.should(QueryBuilders.wildcardQuery(BaseConstant.PILESN, "*" + tmp + "*"));
        }
        //查询对象
        NativeSearchQuery searchQuery = new NativeSearchQuery(boolQueryBuilder1, boolQueryBuilder2);
        //分页
        Pageable pageable = PageRequest.of(page - 1, pageSize);
        searchQuery.setPageable(pageable);
        //排序
        Sort sort = Sort.by(Sort.Direction.DESC, "updatedAt", "id");
        if (pilePageDTO.getAscArr() != null && pilePageDTO.getAscArr().length > 0) {
            Boolean powerAsc = pilePageDTO.getAscArr()[0];
            Sort.Direction direction = Sort.Direction.DESC;
            if (powerAsc) {
                direction = Sort.Direction.ASC;
            }
            sort = Sort.by(direction, "power");
        }
        //按名称或添加时间排序
        if (StringUtils.isNotEmpty(orderBy) && StringUtils.isNotEmpty(orderType)) {
            if ("createdAt".equals(orderBy) || "name".equals(orderBy)) {
                Sort.Direction direction = Sort.Direction.fromString(orderType);
                sort = Sort.by(direction, orderBy);
            }
        }
        searchQuery.addSort(sort);
        SearchHits<OpLocationPileEvseElasticDTO> esPileEVSEPage =
                elasticsearchRestTemplate.search(searchQuery, OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = esPileEVSEPage.stream().map(SearchHit::getContent).collect(Collectors.toList());

        Map<String, OpLocationPileEvseElasticDTO> pileSnAndOpLocationPileEvseElasticDTOMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
            opLocationPileEvseElasticDTOList.forEach(val -> pileSnAndOpLocationPileEvseElasticDTOMap.put(val.getPileSn(), val));
        }
        List<PilePageVO> pilePageVOList = DozerConvert.mapList(opLocationPileEvseElasticDTOList, PilePageVO.class);

        //封装桩其他信息
        if (CollectionUtils.isNotEmpty(pilePageVOList)) {
            //查询品牌map
            Set<Long> brandIdSet = opLocationPileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getBrandId).collect(Collectors.toSet());
            List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectBatchIds(brandIdSet);
            log.info("查询品牌id集合{}  实体数据：{}", JSON.toJSONString(brandIdSet), JSON.toJSONString(opEvseBrandEntities));
            Map<Long, OpEvseBrandEntity> brandIdEntityMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(opEvseBrandEntities)) {
                opEvseBrandEntities.forEach(brandEntity -> brandIdEntityMap.put(brandEntity.getId(), brandEntity));
            }

            //查询es枪设备map
            List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByLocationId(stationId);
            log.info("站点枪集合：{}", JSON.toJSONString(esEVSEList));
            Map<String, List<OpLocationEvseElasticDTO>> pileSNGunListMap = new HashMap<>();
            Map<Long, OpLocationEvseElasticDTO> evseIdEVSEMap = new HashMap<>();
            if (CollectionUtils.isNotEmpty(esEVSEList)) {
                esEVSEList.forEach(esEVSE -> {
                    String pileSN = esEVSE.getPileSn();
                    List<OpLocationEvseElasticDTO> pileGunList = pileSNGunListMap.get(pileSN);
                    if (pileGunList == null) {
                        pileGunList = new ArrayList<>();
                    }
                    pileGunList.add(esEVSE);
                    pileSNGunListMap.put(pileSN, pileGunList);
                    evseIdEVSEMap.put(esEVSE.getId(), esEVSE);
                });
            }
            //封装桩列表的其他信息
            for (PilePageVO pilePageVO : pilePageVOList) {
                OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileSnAndOpLocationPileEvseElasticDTOMap.get(pilePageVO.getPileSn());
                pilePageVO.setEroamingEnable((opLocationPileEvseElasticDTO == null || opLocationPileEvseElasticDTO.getEroamingEnable() == null)
                        ? 0
                        : opLocationPileEvseElasticDTO.getEroamingEnable());

                //品牌
                if (StringUtils.isBlank(pilePageVO.getBrandName()) && pilePageVO.getBrandId() != null) {
                    OpEvseBrandEntity opEvseBrandEntity = brandIdEntityMap.get(Long.valueOf(pilePageVO.getBrandId()));
                    if (opEvseBrandEntity != null) {
                        pilePageVO.setBrandName(opEvseBrandEntity.getName());
                    }
                }

                String evseList = pilePageVO.getEvseList();
                log.info("桩：{} 的枪集合:{}", pilePageVO.getId(), evseList);
                if (StringUtils.isNotBlank(evseList)) {
                    //桩的枪设备
                    JSONArray evseArray = JSON.parseArray(evseList);
                    //各个枪设备的状态和类型
                    List<GunStatusVO> gunStatusVOList = Lists.newArrayList();
                    for (Object evseId : evseArray) {
                        GunStatusVO gunStatusVO = new GunStatusVO();
                        gunStatusVO.setId(Long.valueOf(evseId.toString()));
                        OpLocationEvseElasticDTO esEVSE = evseIdEVSEMap.get(Long.valueOf(evseId.toString()));
                        if (esEVSE != null) {
                            String state = esEVSE.getState();
                            state = StringUtils.isBlank(state) ? String.valueOf(EvseDeviceStatusEnum.DEFAULT.getName()) : state;
                            LocationEVSEStatusEnum locationEVSEStatusEnum = LocationEVSEStatusEnum
                                    .realTimeStatus2BusinessStatus(EvseDeviceStatusEnum.getEnumByName(state).getName());
                            gunStatusVO.setEvseStatusCode(locationEVSEStatusEnum.getCode());
                            if (esEVSE.getGunType() != null) {
                                gunStatusVO.setGunTypeCode(esEVSE.getGunType());
                            }
                            gunStatusVO.setTariffId(esEVSE.getTariffId());
                            gunStatusVO.setBatterySoc(monitorFeign.getEvseMonitorSoc(gunStatusVO.getId()));
                            String evseSn = esEVSE.getEvseSn();
                            gunStatusVO.setEvseSn(evseSn);
                            Integer gunNo = CommonUtil.getGunNo(evseSn);
                            gunStatusVO.setEvseNo(gunNo == null ? null : String.valueOf(gunNo));
                        } else {
                            gunStatusVO.setEvseStatusCode(LocationEVSEStatusEnum.UNAVAILABLE.getCode());
                        }
                        if (ObjectUtils.isEmpty(gunStatusVO.getTariffId()) || gunStatusVO.getTariffId() == null) {
                            gunStatusVO.setTag(messageSourceUtil.getMessage("IS_NOT_ASSOCIATED_WITH_TARIFF_RULES"));
                        }
                        gunStatusVOList.add(gunStatusVO);
                    }
                    pilePageVO.setGunStatusVOS(gunStatusVOList);
                    //是否能重启  可用/不可用/故障
                    AtomicReference<Boolean> restart = new AtomicReference<>(true);
                    for (Object evseId : evseArray) {
                        OpLocationEvseElasticDTO esEVSE = evseIdEVSEMap.get(Long.valueOf(evseId.toString()));
                        if (esEVSE != null) {
                            String state = esEVSE.getState();
                            if (StringUtils.isNotBlank(state)) {
                                if (!EvseDeviceStatusEnum.getEnumByName(state).getCode().equals(EvseDeviceStatusEnum.AVAILABLE.getCode()) &&
                                        !EvseDeviceStatusEnum.getEnumByName(state).getCode().equals(EvseDeviceStatusEnum.UNAVAILABLE.getCode()) &&
                                        !EvseDeviceStatusEnum.getEnumByName(state).getCode().equals(EvseDeviceStatusEnum.FAULTED.getCode())) {
                                    restart.set(false);
                                    break;
                                }
                            }
                        }
                    }
                    pilePageVO.setRestart(restart.get());
                    //是否能停止充电，充电中/车辆暂停/充电中暂停
                    AtomicBoolean stopCharge = new AtomicBoolean(false);
                    for (Object evseId : evseArray) {
                        OpLocationEvseElasticDTO esEVSE = evseIdEVSEMap.get(Long.valueOf(evseId.toString()));
                        if (esEVSE != null) {
                            String state = esEVSE.getState();
                            if (StringUtils.isNotBlank(state)) {
                                if (EvseDeviceStatusEnum.getEnumByName(state).getCode().equals(EvseDeviceStatusEnum.CHARGING.getCode()) ||
                                        EvseDeviceStatusEnum.getEnumByName(state).getCode().equals(EvseDeviceStatusEnum.SUSPENDED_EV.getCode()) ||
                                        EvseDeviceStatusEnum.getEnumByName(state).getCode().equals(EvseDeviceStatusEnum.SUSPENDED_EVSE.getCode())) {
                                    stopCharge.set(true);
                                    break;
                                }
                            }
                        }
                    }
                    pilePageVO.setStopCharge(stopCharge.get());
                }
            }
        }
        //封装成新的分页对象
        Page<PilePageVO> resultPage = new Page<>(page, pageSize);
        resultPage.setRecords(pilePageVOList);
        resultPage.setTotal(esPileEVSEPage.getTotalHits());

        return resultPage;
    }

    private void syncLocationPileEvse(Long locationId, Collection<Long> ids){
        if (ids.isEmpty()) {
            List<OpLocationPileEvseElasticDTO> result = opLocationPileEvseElastic.findAllByLocationId(locationId);
            log.info("sync findAllByLocationId OpLocationPileEvseElasticDTO {}", JSON.toJSONString(result));
            if (CollectionUtils.isNotEmpty(result)) { //  整个场站ID 下面没有 一个桩 但是在es里面查询到有值那么直接删除es 里的数据
                List<Long> docIds = result.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationPileEvseElastic.deleteAllById(docIds);
            }
        } else {
            List<OpLocationPileEvseElasticDTO> result = opLocationPileEvseElastic.findByLocationIdAndIdNotIn(locationId, ids);
            log.info("sync findByLocationIdAndIdNotIn OpLocationPileEvseElasticDTO {}", JSON.toJSONString(result));
            if (CollectionUtils.isNotEmpty(result)) { // 在es  没有在数据库中的数据 es 直接删除
                List<Long> docIds = result.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationPileEvseElastic.deleteAllById(docIds);
            }
        }
    }

    private void syncLocationEvse(Long locationId, Collection<Long> ids){
        if (ids.isEmpty()) {
            List<OpLocationEvseElasticDTO> result = opLocationEvseElastic.findAllByLocationId(locationId);
            log.info("sync findAllByLocationId OpLocationEvseElasticDTO {}", JSON.toJSONString(result));
            if (CollectionUtils.isNotEmpty(result)) {// 场站里面没有一个枪信息 所以直接将es里 场站下的枪信息删除
                List<Long> docIds = result.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationEvseElastic.deleteAllById(docIds);
                opLocationEvseExpandElastic.deleteAllById(docIds);
            }
        } else {
            List<OpLocationEvseElasticDTO> result = opLocationEvseElastic.findByLocationIdAndIdNotIn(locationId, ids);
            log.info("sync findByLocationIdAndIdNotIn OpLocationEvseElasticDTO {}", JSON.toJSONString(result));
            if (CollectionUtils.isNotEmpty(result)) {
                List<Long> docIds = result.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
                opLocationEvseElastic.deleteAllById(docIds); // 将那些不存在 mysql 里面的 数据从es 中删除
                opLocationEvseExpandElastic.deleteAllById(docIds);
            }
        }

    }

    /**
     * 站点桩分页查询（场站列表用 ）
     *
     * @param pileByStationIdAndPileSNPageDTO 场站id
     * @param pageIndex                       当前页
     * @param pageSize                        页大小
     * @return 桩分院
     */
    @Override
    public Page<PilePageVO> stationPilePageByStationIdAndPileSN(PileByStationIdAndPileSNPageDTO pileByStationIdAndPileSNPageDTO, int pageIndex, int pageSize) {

        Long stationId = pileByStationIdAndPileSNPageDTO.getStationId();
        String pileSN = pileByStationIdAndPileSNPageDTO.getPileSN();


        //ES分页查询桩
        org.springframework.data.domain.Page<OpLocationPileEvseElasticDTO> pileEVSEPage;
        Pageable pageable = PageRequest.of(pageIndex - 1, pageSize);
        if (StringUtils.isNotBlank(pileSN)) {
            pileEVSEPage = opLocationPileEvseElastic.findByLocationIdAndPileSnLike(stationId, pileSN, pageable);
        } else {
            pileEVSEPage = opLocationPileEvseElastic.findByLocationIdOrderByCreatedAtDesc(stationId, pageable);
        }
        log.info("站点:{}  桩分页：{}", stationId, JSON.toJSONString(pileEVSEPage.getContent()));
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = pileEVSEPage.getContent();
        List<PilePageVO> pilePageVOList = DozerConvert.mapList(opLocationPileEvseElasticDTOList, PilePageVO.class);

        //数据库分页查询桩
        if (CollectionUtils.isEmpty(opLocationPileEvseElasticDTOList)) {
            QueryWrapper<OpLocationPileEvseEntity> opLocationPileEvseEntityQueryWrapper = new QueryWrapper<>();
            opLocationPileEvseEntityQueryWrapper.eq("location_id", stationId);
            opLocationPileEvseEntityQueryWrapper.eq("pile_sn", pileSN);
            Page<OpLocationPileEvseEntity> searchPage = new Page<>(pageIndex, pageSize);
            Page<OpLocationPileEvseEntity> opLocationPileEvseEntityPage = this.baseMapper.selectPage(searchPage, opLocationPileEvseEntityQueryWrapper);
            List<OpLocationPileEvseEntity> records = opLocationPileEvseEntityPage.getRecords();
            pilePageVOList = DozerConvert.mapList(records, PilePageVO.class);
        }

        //封装桩其他信息
        if (CollectionUtils.isNotEmpty(pilePageVOList)) {
            //品牌
            Map<Long, OpEvseBrandEntity> brandIdBrandEntityMap = null;
            Set<String> brandIds = pilePageVOList.stream().map(PilePageVO::getBrandId).filter(Objects::nonNull).collect(Collectors.toSet());
            if (CollectionUtils.isNotEmpty(brandIds)) {
                List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectBatchIds(brandIds);
                brandIdBrandEntityMap = buildBrandIdBrandEntityMap(opEvseBrandEntities);
            }
            if (brandIdBrandEntityMap != null) {
                for (PilePageVO pilePageVO : pilePageVOList) {
                    if (pilePageVO.getBrandId() != null) {
                        try {
                            OpEvseBrandEntity opEvseBrandEntity = brandIdBrandEntityMap.get(Long.valueOf(pilePageVO.getBrandId()));
                            if (opEvseBrandEntity != null) {
                                pilePageVO.setBrandName(opEvseBrandEntity.getName());
                            }
                        } catch (Exception ignore) {
                        }
                    }
                }
            }

            //根据充电枪集合封装两个map
            Map<String, List<OpLocationEvseRealTimeDTO>> pileSNGunListMap = new HashMap<>();
            Map<Long, OpLocationEvseRealTimeDTO> evseIdEVSEMap = new HashMap<>();
            List<OpLocationEvseRealTimeDTO> stationEVSEList = opLocationEvseRepository.getEvseByLocationId(stationId);
            for (OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO : stationEVSEList) {
                try {
                    String evseSN = opLocationEvseRealTimeDTO.getEvseSn();
                    pileSN = evseSN.split("_")[0];
                } catch (Exception e) {
                    pileSN = opLocationEvseRealTimeDTO.getEvseId();
                }
                List<OpLocationEvseRealTimeDTO> pileGunList = pileSNGunListMap.get(pileSN);
                if (pileGunList == null) {
                    pileGunList = new ArrayList<>();
                }
                pileGunList.add(opLocationEvseRealTimeDTO);
                pileSNGunListMap.put(pileSN, pileGunList);
                evseIdEVSEMap.put(opLocationEvseRealTimeDTO.getId(), opLocationEvseRealTimeDTO);
            }
            for (PilePageVO pilePageVO : pilePageVOList) {
                pileSN = pilePageVO.getPileSn();
                List<OpLocationEvseRealTimeDTO> opLocationEvseRealTimeDTOS = pileSNGunListMap.get(pileSN);
                log.info("桩：{} 的枪集合信息：{}", pileSN, JSON.toJSONString(opLocationEvseRealTimeDTOS));
                if (CollectionUtils.isNotEmpty(opLocationEvseRealTimeDTOS)) {
                    OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = opLocationEvseRealTimeDTOS.get(0);
                    //功率
                    pilePageVO.setPower(opLocationEvseRealTimeDTO.getPower());
                    if (CollectionUtils.isNotEmpty(opLocationEvseRealTimeDTO.getOpLocationConnectorDTOs())) {
                        //功率类型
                        pilePageVO.setPowerType(opLocationEvseRealTimeDTO.getOpLocationConnectorDTOs().get(0).getPowerType());
                    }
                    //各枪状态
                    List<GunStatusVO> gunStatusVOList = Lists.newArrayList();
                    String evseList = pilePageVO.getEvseList();
                    JSONArray evseArray = JSON.parseArray(evseList);
                    for (Object evseId : evseArray) {
                        GunStatusVO gunStatusVO = new GunStatusVO();
                        gunStatusVO.setId(Long.valueOf(evseId.toString()));
                        OpLocationEvseRealTimeDTO evseRealTimeDTO = evseIdEVSEMap.get(Long.valueOf(evseId.toString()));
                        if (evseRealTimeDTO != null) {
                            LocationEVSEStatusEnum locationEVSEStatusEnum = LocationEVSEStatusEnum.realTimeStatus2BusinessStatus(evseRealTimeDTO.getState());
                            gunStatusVO.setEvseStatusCode(locationEVSEStatusEnum.getCode());
                        } else {
                            gunStatusVO.setEvseStatusCode(LocationEVSEStatusEnum.UNAVAILABLE.getCode());
                        }
                        gunStatusVOList.add(gunStatusVO);
                    }
                    pilePageVO.setGunStatusVOS(gunStatusVOList);
                    //重启和停止充电
                    if (CollectionUtils.isNotEmpty(gunStatusVOList)) {
                        //根据各枪状态判断是否能重启(只有全部枪都非充电中、非离线的充电桩可以重启，不可用时置灰)   所有枪都是可用才可以置灰
                        AtomicReference<Boolean> restart = new AtomicReference<>(true);
                        //根据各枪状态判断是否能停止充电（只要有一个枪在充电中，则此按钮可用）
                        AtomicBoolean stopCharge = new AtomicBoolean(false);
                        gunStatusVOList.forEach(gunStatusVO -> {
                            int evseStatusCode = gunStatusVO.getEvseStatusCode();
                            if (evseStatusCode != LocationEVSEStatusEnum.AVAILABLE.getCode()) {
                                restart.set(false);
                            }
                            if (evseStatusCode == LocationEVSEStatusEnum.CHARGING.getCode()) {
                                stopCharge.set(true);
                            }
                        });
                        pilePageVO.setRestart(restart.get());
                        pilePageVO.setStopCharge(stopCharge.get());
                    }
                }
            }
        }

        //封装成新的分页对象
        Page<PilePageVO> resultPage = new Page<>(pageIndex, pageSize);
        resultPage.setRecords(pilePageVOList);
        resultPage.setTotal(pileEVSEPage.getTotalElements());
        return resultPage;
    }

    /**
     * 站点桩集合搜索(场站列表用)
     *
     * @param stationIds 站点id集合
     * @param pileSN     桩SN
     * @return 桩集合
     */
    @Override
    public Map<Long, List<PilePageVO>> stationPileListByStationIdsAndPileSN(Set<Long> stationIds, String pileSN) {
        Map<Long, List<PilePageVO>> resultMap = new HashMap<>();
        if (CollectionUtils.isEmpty(stationIds)) {
            return resultMap;
        }

        //1、查询站点桩封装成map
        List<OpLocationPileEvseElasticDTO> pileList;
        List<PilePageVO> pilePageVOList;
        if (StringUtils.isNotBlank(pileSN)) {
            BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
            if (CollectionUtils.isNotEmpty(stationIds)) {
                TermsQueryBuilder idTermsQueryBuilder = QueryBuilders.termsQuery("locationId", stationIds);
                boolQueryBuilder.must(idTermsQueryBuilder);
            }
            WildcardQueryBuilder nameQueryBuilder = QueryBuilders.wildcardQuery(BaseConstant.PILESN, "*" + pileSN + "*");
            boolQueryBuilder.must(nameQueryBuilder);

//            Iterable<OpLocationPileEvseElasticDTO> pileElasticDTOIterable = opLocationPileEvseElastic.search(boolQueryBuilder);
            pileList = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());
        } else {
            pileList = opLocationPileEvseElastic.findAllByLocationIdInOrderByCreatedAtAsc(stationIds);
        }
        pilePageVOList = DozerConvert.mapList(pileList, PilePageVO.class);
        if (CollectionUtils.isEmpty(pilePageVOList)) {
            return resultMap;
        }

        //将桩封装成站点id-站点桩map
        Map<Long, List<PilePageVO>> stationIdStationPileListMap = new HashMap<>();
        for (PilePageVO pilePageVO : pilePageVOList) {
            List<PilePageVO> stationPilePageVOList = stationIdStationPileListMap.get(pilePageVO.getLocationId());
            if (stationPilePageVOList == null) {
                stationPilePageVOList = new ArrayList<>();
            }
            stationPilePageVOList.add(pilePageVO);
            stationIdStationPileListMap.put(pilePageVO.getLocationId(), stationPilePageVOList);
        }
        //2、查询站点枪数据封装成站点id-枪数据map
        Map<Long, List<OpLocationEvseRealTimeDTO>> stationIdStationEVSEListMap = opLocationEvseRepository.getEvseByLocationIds(stationIds);
        //3、查询品牌封装成map
        Map<Long, OpEvseBrandEntity> brandIdBrandEntityMap = null;
        Set<String> brandIds = pilePageVOList.stream().map(PilePageVO::getBrandId).filter(Objects::nonNull).collect(Collectors.toSet());
        if (CollectionUtils.isNotEmpty(brandIds)) {
            List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectBatchIds(brandIds);
            brandIdBrandEntityMap = buildBrandIdBrandEntityMap(opEvseBrandEntities);
        }

        //4.给每个站点的每个桩封装品牌信息和枪信息
        for (Map.Entry<Long, List<PilePageVO>> entry : stationIdStationPileListMap.entrySet()) {
            Long stationId = entry.getKey();
            List<PilePageVO> stationPileList = entry.getValue();
            List<OpLocationEvseRealTimeDTO> stationEVSEList = stationIdStationEVSEListMap.get(stationId);
            List<PilePageVO> newStationPileList = new ArrayList<>();
            if (CollectionUtils.isNotEmpty(stationPileList)) {
                //根据站点充电枪集合封装两个map
                Map<String, List<OpLocationEvseRealTimeDTO>> stationPileSNGunListMap = new HashMap<>();
                Map<Long, OpLocationEvseRealTimeDTO> stationEvseIdEVSEMap = new HashMap<>();
                if (CollectionUtils.isNotEmpty(stationEVSEList)) {
                    for (OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO : stationEVSEList) {
                        try {
                            String evseSN = opLocationEvseRealTimeDTO.getEvseSn();
                            pileSN = evseSN.split("_")[0];
                        } catch (Exception e) {
                            pileSN = opLocationEvseRealTimeDTO.getEvseId();
                        }
                        List<OpLocationEvseRealTimeDTO> pileGunList = stationPileSNGunListMap.get(pileSN);
                        if (pileGunList == null) {
                            pileGunList = new ArrayList<>();
                        }
                        pileGunList.add(opLocationEvseRealTimeDTO);
                        stationPileSNGunListMap.put(pileSN, pileGunList);
                        stationEvseIdEVSEMap.put(opLocationEvseRealTimeDTO.getId(), opLocationEvseRealTimeDTO);
                    }
                }
                for (PilePageVO pilePageVO : stationPileList) {
                    //品牌
                    if (brandIdBrandEntityMap != null && pilePageVO.getBrandId() != null) {
                        try {
                            OpEvseBrandEntity opEvseBrandEntity = brandIdBrandEntityMap.get(Long.valueOf(pilePageVO.getBrandId()));
                            if (opEvseBrandEntity != null) {
                                pilePageVO.setBrandName(opEvseBrandEntity.getName());
                            }
                        } catch (Exception ignore) {
                        }
                    }
                    pileSN = pilePageVO.getPileSn();
                    List<OpLocationEvseRealTimeDTO> opLocationEvseRealTimeDTOS = stationPileSNGunListMap.get(pileSN);
                    log.info("桩：{} 的枪集合信息：{}", pileSN, JSON.toJSONString(opLocationEvseRealTimeDTOS));
                    pilePageVO.setOpLocationEvseRealTimeDTOS(opLocationEvseRealTimeDTOS);
                    if (CollectionUtils.isNotEmpty(opLocationEvseRealTimeDTOS)) {
                        //各枪状态
                        List<GunStatusVO> gunStatusVOList = Lists.newArrayList();
                        String evseList = pilePageVO.getEvseList();
                        JSONArray evseArray = JSON.parseArray(evseList);
                        for (Object evseId : evseArray) {
                            GunStatusVO gunStatusVO = new GunStatusVO();
                            gunStatusVO.setId(Long.valueOf(evseId.toString()));
                            OpLocationEvseRealTimeDTO evseRealTimeDTO = stationEvseIdEVSEMap.get(Long.valueOf(evseId.toString()));
                            if (evseRealTimeDTO != null) {
                                LocationEVSEStatusEnum locationEVSEStatusEnum = LocationEVSEStatusEnum.realTimeStatus2BusinessStatus(evseRealTimeDTO.getState());
                                gunStatusVO.setEvseStatusCode(locationEVSEStatusEnum.getCode());
                                gunStatusVO.setEvseSn(evseRealTimeDTO.getEvseSn().split("_")[0]);
                            } else {
                                gunStatusVO.setEvseStatusCode(LocationEVSEStatusEnum.UNAVAILABLE.getCode());
                            }
                            gunStatusVOList.add(gunStatusVO);
                        }
                        pilePageVO.setGunStatusVOS(gunStatusVOList);
                        //重启和停止充电
                        if (CollectionUtils.isNotEmpty(gunStatusVOList)) {
                            //根据各枪状态判断是否能重启(只有全部枪都非充电中、非离线的充电桩可以重启，不可用时置灰)   所有枪都是可用才可以置灰
                            AtomicReference<Boolean> restart = new AtomicReference<>(true);
                            //根据各枪状态判断是否能停止充电（只要有一个枪在充电中，则此按钮可用）
                            AtomicBoolean stopCharge = new AtomicBoolean(false);
                            gunStatusVOList.forEach(gunStatusVO -> {
                                int evseStatusCode = gunStatusVO.getEvseStatusCode();
                                if (evseStatusCode != LocationEVSEStatusEnum.AVAILABLE.getCode()) {
                                    restart.set(false);
                                }
                                if (evseStatusCode == LocationEVSEStatusEnum.CHARGING.getCode()) {
                                    stopCharge.set(true);
                                }
                            });
                            pilePageVO.setRestart(restart.get());
                            pilePageVO.setStopCharge(stopCharge.get());
                        }
                    }
                    newStationPileList.add(pilePageVO);
                }
            }
            resultMap.put(stationId, newStationPileList);
        }
        return resultMap;
    }

    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    @Override
    public Void downModuleGenerateXls(HttpServletRequest request, HttpServletResponse response) {
        //1、获取文件名称、sheet名称、表头数据
        String downLoadFileName = messageSourceUtil.getMessage(I18nConstant.PileImportModule.FILE_NAME);
        String sheetName = messageSourceUtil.getMessage(I18nConstant.PileImportModule.SHEET_NAME);
        String brandNameTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.BRAND_Name);
        String SNTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.SN);
        String PINTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.PIN);
        String pileNameTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.PILE_NAME);
        String chargeType = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CHARGE_TYPE);
        String chargePower = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CHARGE_POWER);
        String connectorTitle1 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR1);
        String connectorTitle2 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR2);
        String connectorTitle3 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR3);
        String connectorTitle4 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR4);
        String[] titleArray = {brandNameTitle, SNTitle, PINTitle, pileNameTitle, chargeType, chargePower, connectorTitle1, connectorTitle2, connectorTitle3, connectorTitle4};
        log.info("下载文件名称：{}   sheet名称：{}     表头数组：{}", downLoadFileName, sheetName, JSON.toJSONString(titleArray));
        //2、下载
        try {
            EasyExcelUtil.downloadWithRegisterWriteHandler(response, new EasyExcelSheetWriteHandlerForPile(), PileImportDTO.class, downLoadFileName, sheetName, titleArray, pileImportDTOList());
        } catch (IOException e) {
            log.error("downModuleGenerateXls:" + e);
        }

        return null;
    }

    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    @Override
    public Result<Void> downModuleResourceXls(HttpServletRequest request, HttpServletResponse response) {
        String downLoadFileName = messageSourceUtil.getMessage(I18nConstant.PileImportModule.FILE_NAME);
        String sheetName = messageSourceUtil.getMessage(I18nConstant.PileImportModule.SHEET_NAME);
        String brandNameTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.BRAND_Name);
        String SNTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.SN);
        String PINTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.PIN);
        String pileNameTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.PILE_NAME);
        String chargeType = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CHARGE_TYPE);
        String chargePower = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CHARGE_POWER);
        String connectorTitle1 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR1);
        String connectorTitle2 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR2);
        String connectorTitle3 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR3);
        String connectorTitle4 = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.CONNECTOR4);
        String productModelTitle = messageSourceUtil.getMessage(I18nConstant.PileImportModule.title.PRODUCT_MODEL);
        String[] titleArray = {brandNameTitle, SNTitle, PINTitle, pileNameTitle, chargeType, chargePower, connectorTitle1, connectorTitle2, connectorTitle3, connectorTitle4, productModelTitle};
        log.info("下载文件名称：{}   sheet名称：{}     表头数组：{}", downLoadFileName, sheetName, JSON.toJSONString(titleArray));

        FileInputStream fileInputStream = null;
        Workbook workbook = null;
        OutputStream outputStream = null;
        try {
            String resourceFileName = "Autel_桩_导入_模板.xlsx";
            String resourceFilePath = "xls/" + resourceFileName;
            InputStream inputStream = ChargeCardExcelEntity.class.getClassLoader().getResourceAsStream(resourceFilePath);
            File tempFile = File.createTempFile("test", ".xlsx");
            FileUtils.writeToFile(tempFile, inputStream);
            //sheet名称、标题国际化
            fileInputStream = new FileInputStream(tempFile.getPath());
            workbook = new XSSFWorkbook(fileInputStream);
            outputStream = new FileOutputStream(tempFile.getPath());
            Sheet sheet = workbook.getSheetAt(0);
            workbook.setSheetName(0, sheetName);
            sheet.setActiveCell(new CellAddress(0, 0));
            Row row = sheet.getRow(0);
            for (int i = 0; i < titleArray.length; i++) {
                Cell cell = row.getCell(i);
                cell.setCellValue(titleArray[i]);
            }
            workbook.write(outputStream);
            //下载
            response.setContentType("application/vnd.ms-excel");
            response.setCharacterEncoding("utf-8");
            downLoadFileName = URLEncoder.encode(downLoadFileName, "UTF-8").replaceAll("\\+", "%20");
            response.setHeader("Content-disposition", "attachment;filename*=utf-8''" + downLoadFileName + ".xlsx");
            byte[] fileToByteArray = FileUtils.readFileToByteArray(tempFile);
            response.getOutputStream().write(fileToByteArray);
        } catch (IOException e) {
            log.error("downModuleResourceXls:" + e);
        } finally {
            if (workbook != null) {
                try {
                    workbook.close();
                } catch (IOException e) {
                    log.error("OpEvseBrandModelRepositoryImpl.downModuleResourceXls close workbook exception = ", e);
                }
            }
            if (fileInputStream != null) {
                try {
                    fileInputStream.close();
                } catch (IOException e) {
                    log.error("OpEvseBrandModelRepositoryImpl.downModuleResourceXls close fileInputStream exception = ", e);
                }
            }
            if (outputStream != null) {
                try {
                    outputStream.close();
                } catch (IOException e) {
                    log.error("OpEvseBrandModelRepositoryImpl.downModuleResourceXls close outputStream exception = ", e);
                }
            }
        }

        return null;
    }

    /**
     * 批量导入桩
     *
     * @param multipartFile 文件
     * @return Result
     */
    @Override
    public PileUploadCheckVO uploadModuleXls(String locationId, MultipartFile multipartFile) {
        //校验文件后缀
        String extension = Objects.requireNonNull(FilenameUtils.getExtension(multipartFile.getOriginalFilename())).toLowerCase();
        if (!Objects.equals(extension, FileExtensionEnum.XLSX.getName())
                && !Objects.equals(extension, FileExtensionEnum.XLS.getName())
                && !Objects.equals(extension, FileExtensionEnum.CSV.getName())) {
            throw new MessageCodeException(PileBaseEnum.FILE_EXTENSION_WRONG);
        }

        //解析excel数据
        List<PileUploadDTO> pileUploadDTOList = new ArrayList<>();
        File file = multipartFile2File(multipartFile);
        EasyExcelFactory.read(file, PileUploadDTO.class, new PageReadListener<PileUploadDTO>(pileUploadDTOList::addAll)).sheet().doRead();
        log.info("解析导入桩excel数据：{}", JSON.toJSONString(pileUploadDTOList));

        //SN码转大写
        try {
            pileUploadDTOList.forEach(pileUploadDTO -> pileUploadDTO.setSN(pileUploadDTO.getSN().toUpperCase()));
        } catch (Exception e) {
            log.error("Conversion failed");
        }
        //校验数据
        checkData(pileUploadDTOList, locationId);

        //找出SN与库重复并且站点一样的数据
        List<String> uploadSNList = pileUploadDTOList.stream().map(PileUploadDTO::getSN).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> dbPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnInAndLocationId(uploadSNList, Long.valueOf(locationId));
        List<String> dbSNList = dbPileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
        List<PileUploadCheckDetailVO> repeatPileUploadList = Lists.newArrayList();
        List<PileUploadCheckDetailVO> validPileUploadList = Lists.newArrayList();
        for (PileUploadDTO pileUploadDTO : pileUploadDTOList) {
            if (dbSNList.contains(pileUploadDTO.getSN())) {
                repeatPileUploadList.add(DozerConvert.map(pileUploadDTO, PileUploadCheckDetailVO.class));
            } else {
                validPileUploadList.add(DozerConvert.map(pileUploadDTO, PileUploadCheckDetailVO.class));
            }
        }
        PileUploadCheckVO pileUploadCheckVO = new PileUploadCheckVO();
        pileUploadCheckVO.setRepeatPileUploadList(repeatPileUploadList);
        pileUploadCheckVO.setValidPileUploadList(validPileUploadList);
        return pileUploadCheckVO;
    }

    @Override
    public Object newImport(String locationId, MultipartFile multipartFile) {
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
        List<PileImportNewDTO> pileList = Collections.synchronizedList(new ArrayList<>());
        File file = multipartFile2File(multipartFile);

            EasyExcelFactory.read(file, PileImportNewDTO.class, new BatchPageReadListener<PileImportNewDTO>(pileList::addAll))
                    .headRowNumber(3).sheet().autoTrim(true).doRead();
        List<String> snList = pileList.stream().map(PileImportNewDTO::getSn).distinct().collect(Collectors.toList());
        for (PileImportNewDTO pileImportNewDTO : pileList) {
            if (!ObjectUtils.isEmpty(pileImportNewDTO.getBrand()) && pileImportNewDTO.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
                pileImportNewDTO.setSn(pileImportNewDTO.getSn().toUpperCase());
            }
        }
        Result<List<ChargePileDTO>> result = deviceServiceFeign.queryPileList(snList);
        Map<String, ChargePileDTO> chargePileMap = new HashMap<>();
        if (!ObjectUtils.isEmpty(result) && !ObjectUtils.isEmpty(result.getData()) && !ObjectUtils.isEmpty(result.getCode()) && result.getCode() == cn.hutool.http.HttpStatus.HTTP_OK) {
            chargePileMap = result.getData().stream().collect(Collectors.toMap(ChargePileDTO::getSn, Function.identity(), (e1, e2) -> e1));
        }
        Result<List<GunTypeRespDTO>> listResult = opEvseTypeServiceImpl.getGunType();
        List<GunTypeRespDTO> gunType = new ArrayList<>();
        if (!ObjectUtils.isEmpty(listResult) && !ObjectUtils.isEmpty(listResult.getData())) {
             gunType = listResult.getData();
        }
        List<String> fileSnRepeatList = pileList.stream().collect(Collectors.toMap(PileImportNewDTO::getSn, e -> 1, (a, b) -> a + b)).entrySet().stream().filter(entry -> entry.getValue() > 1).map(Map.Entry::getKey).collect(Collectors.toList());
        Map<String, Integer> gunTypeMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(gunType)) {
            gunTypeMap = gunType.stream().collect(Collectors.toMap(GunTypeRespDTO::getName, GunTypeRespDTO::getGunType, (e1, e2) -> e1));
        }
        checkDataV2(pileList,locationId,chargePileMap,fileSnRepeatList,gunTypeMap);
        log.info("解析完成,pileList:{}", JSON.toJSONString(pileList));
        List<PileImportNewDTO> insertImportList = pileList.stream().filter(p ->StringUtils.isBlank(p.getResult())).collect(Collectors.toList());
        savePileListV2(insertImportList,locationId);
        //不符合条件的桩,先保存到缓存，输出excel
        List<PileImportNewDTO> errorImportList = pileList.stream().filter(p -> StringUtils.isNotBlank(p.getResult())).collect(Collectors.toList());
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
        List<PileImportNewDTO> successList = insertImportList;
        while (successList.iterator().hasNext()) {
            PileImportNewDTO next = successList.iterator().next();
            if (errorImportList.contains(next)) {
                successList.remove(next);
            }
        }
        resultMap.put("successList",successList);
        if (!CollectionUtils.isEmpty(successList)) {
            List<String> pileSnList = successList.stream().map(PileImportNewDTO::getSn).collect(Collectors.toList());
            Result<List<GunTypeAndNumVO>> listResult1 = deviceServiceFeign.batchGetGunTypeAndNum(pileSnList);
            log.info("批量导入充电桩,listResult1={}",JSON.toJSONString(listResult1));
            if (!ObjectUtils.isEmpty(listResult1) && listResult1.getCode() == 200) {
                List<GunTypeAndNumVO> data = listResult1.getData();
                for (PileImportNewDTO pileImportNewDTO : successList) {
                    for (GunTypeAndNumVO datum : data) {
                        if (pileImportNewDTO.getSn().equals(datum.getPileSn()) && !ObjectUtils.isEmpty(datum.getGunTypeMap())) {
                            setGun(pileImportNewDTO,datum);
                        }
                    }
                }
            }
        }
        log.info("importPile,文件名:{}导入成功;总条数:{};成功{}条;失败{}条;总耗时:{} ms;", total, (total - numberOfFailures), numberOfFailures, originalFilename, (System.currentTimeMillis() - startTime));
        return resultMap;
    }

    private void setGun(PileImportNewDTO pileImportNewDTO, GunTypeAndNumVO datum) {
        String gunNum = datum.getGunNum();
        log.info("设枪类型");
        for (int i=1;i<=Integer.valueOf(gunNum);i++) {
            if (i==1 && StringUtils.isBlank(pileImportNewDTO.getGun1Type())) {
                pileImportNewDTO.setGun1Type(datum.getGunTypeMap().get("1"));
            }
            if (i==2 && StringUtils.isBlank(pileImportNewDTO.getGun2Type())) {
                pileImportNewDTO.setGun1Type(datum.getGunTypeMap().get("2"));
            }
            if (i==3 && StringUtils.isBlank(pileImportNewDTO.getGun3Type())) {
                pileImportNewDTO.setGun1Type(datum.getGunTypeMap().get("3"));
            }
            if (i==4 && StringUtils.isBlank(pileImportNewDTO.getGun4Type())) {
                pileImportNewDTO.setGun1Type(datum.getGunTypeMap().get("4"));
            }
        }
    }

    @Override
    public void downloadImportErrorFile(String language, HttpServletResponse response) {
        log.info("downloadImportErrorFile:{}", language);
        String fileName = "";
        List<PileImportNewDTO> errorList = new ArrayList<>();
        try {
            errorList = (List<PileImportNewDTO>) redis2Util.hget(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE + LoginUserHolder.getLoginUser().getPayload().getUserId());
            fileName = (String) redis2Util.hget(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE_NAME + LoginUserHolder.getLoginUser().getPayload().getUserId());
        } catch (Exception e) {
            log.error("downloadImportErrorFile error:", e);
        }
        log.info("errorList:{}", JSON.toJSONString(errorList));
        if (CollUtil.isEmpty(errorList)) {
            throw new BusinessException(ImportErrorEnum.FILE_HAS_EXPIRED);
        }
        if (StringUtils.isBlank(fileName)) {
            fileName = "import_error";
        } else {
            fileName = fileName + "_import_error";
        }
        exportErrorList(language, response, errorList, fileName);
    }

    private void exportErrorList(String language, HttpServletResponse response, List<PileImportNewDTO> errorImportList, String fileName) {
        long startTime = System.currentTimeMillis();
        List<PileImportErrorDTO> dataList = new ArrayList<>();
        errorImportList.stream().forEach(pile -> {
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
            log.error("导出文件名转换有误");
        }
        response.setContentType(BaseConstant.CONTENT_TYPE);
        response.setCharacterEncoding(UTF_8);
        response.setHeader(BaseConstant.HEAD_NAME, BaseConstant.HEAD_VALUE + fileName);
        OutputStream out = null;
        try {
            out = response.getOutputStream();
            excelWriter = EasyExcelFactory.write(out).build();
            int num = sheetNo.get();
            List<CellStyleModel> styleModelList = new ArrayList<>();
            CellStyleModel styleModel = new CellStyleModel();
            styleModel.setColIndex(12);
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
            log.error(e.getMessage(), e);
        } finally {
            if (excelWriter != null) {
                excelWriter.finish();
            }

            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    log.error("关闭输出流异常！", e);
                }
            }
        }
    }

    private void setErrorImportCache(List<PileImportNewDTO> errorImportList, String fileName) {
        try {
            redis2Util.hset(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE + LoginUserHolder.getLoginUser().getPayload().getUserId(), errorImportList, DAY);
            redis2Util.hset(RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE, RedisKeyConstant.PILE_CHARGE_PILE_IMPORT_ERROR_FILE_NAME + LoginUserHolder.getLoginUser().getPayload().getUserId(), fileName, DAY);
        } catch (Exception e) {
            log.error("charge_pile_import_error_file,failed...", e);
        }
    }

    private void savePileListV2(List<PileImportNewDTO> insertImportList,String locationId) {
        List<PileImportNewDTO> saveList = Lists.newArrayList();
        //查询站点
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.STATION_NOT_EXIT);
        }

        //保存
        if (CollectionUtils.isNotEmpty(insertImportList)) {
            saveList.addAll(insertImportList);
        }
        List<PileInfoDTO> pileInfoDTOList = new ArrayList<>();
        log.info("要保存的数据：{}", JSON.toJSONString(saveList));
        List<PileUploadSaveDetailVO> createList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(saveList)) {
            //循环每一个桩
            for (PileImportNewDTO pileImportNewDTO : saveList) {
                //构建createEVSE的参数
                OpLocationEvseDTO opLocationEvseDTO = buildCreateEVSEDTOV2(pileImportNewDTO, locationId);
                opLocationEvseDTO.setPublicMark(1);
                PileVO addPileVO = opLocationEvseRepository.createEvse(opLocationEvseDTO, opLocationEntity);
                Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
                try {
                    stringRedisTemplate.delete(RedisKeyConstant.getStringAddPileException(userId));
                } catch (Exception e) {
                    log.info("添加桩删除es异常,e={}",e);
                }
                PileUploadSaveDetailVO pileUploadSaveDetailVO = new PileUploadSaveDetailVO();
                if (addPileVO != null) {
                    pileUploadSaveDetailVO.setPileId(addPileVO.getPileId());
                    pileUploadSaveDetailVO.setPileName(addPileVO.getPileName());
                    pileUploadSaveDetailVO.setSN(addPileVO.getPileSN());
                    List<PileEvseVO> pileEvseVOS = addPileVO.getPileEvseVOS();
                    //根据一个桩，构建枪id和枪号，一个桩下的枪存到同一个evseAndTarifVOList
                    List<EvseAndTarifVO> evseAndTarifVOList = new ArrayList<>();
                    if (CollUtil.isNotEmpty(pileEvseVOS)) {
                        pileEvseVOS.forEach(pileEvseVO -> {
                            EvseAndTarifVO evseAndTarifVO = new EvseAndTarifVO();
                            evseAndTarifVO.setEvseId(pileEvseVO.getEvseId());
                            evseAndTarifVO.setConnector(pileEvseVO.getConnector());
                            evseAndTarifVOList.add(evseAndTarifVO);
                        });
                    }
                    pileUploadSaveDetailVO.setList(evseAndTarifVOList);
                    // todo 需求改动，这个字段已经没有用了，为适配前端，故设为false
                    pileUploadSaveDetailVO.setHubjectCheck(false);
                }
                createList.add(pileUploadSaveDetailVO);
                log.info("调用createEvse接口：{}", JSON.toJSONString(opLocationEvseDTO));
                try {
                    pileInfoDTOList.add(formatPileInfoDTO(opLocationEvseDTO, opLocationEntity));
                } catch (Exception e) {
                    log.error("数据转换失败，桩批量同步运维失败.", e.getMessage());
                }
            }
        }
        log.info("OpLocationPileEvseRepositoryImpl.savePileList.createList = {}", JSON.toJSONString(createList));
        //调用新运维平台同步接口，只保存成功的
        try {
            if (CollectionUtils.isNotEmpty(pileInfoDTOList)) {
                opsMgmtClient.batchSaveOpsPile(pileInfoDTOList);
            }
        } catch (Exception e) {
            log.error("桩批量同步运维失败.", e.getMessage());
        }

        try {
            // 批量导入充电桩成功之后，需要发送MQ消息到车队那边
            if (ObjectUtils.isNotEmpty(pileInfoDTOList)) {
                Set<String> pileSnSet = pileInfoDTOList
                        .stream()
                        .filter(var -> (var != null && StringUtils.isNotBlank(var.getSn())))
                        .map(PileInfoDTO::getSn)
                        .collect(Collectors.toSet());
                // 查询充电枪信息
                List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseElastic.findAllByPileSnIn(pileSnSet);
                if (ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                    // 获取userId
                    Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
                    // 构造入参对象
                    List<EvseInfoModifyDTO> evseInfoModifyDTOList = new ArrayList<>();
                    for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                        if (StringUtils.isNotBlank(opLocationEvseElasticDTO.getEvseSn())) {
                            EvseInfoModifyDTO evseInfoModifyDTO = new EvseInfoModifyDTO();
                            evseInfoModifyDTO.setEvseSn(opLocationEvseElasticDTO.getEvseSn());
                            evseInfoModifyDTO.setOperationType(EvseOperationTypeEnum.ADD.getCode());
                            evseInfoModifyDTOList.add(evseInfoModifyDTO);
                        }
                    }

                    // 异步推送
                    ThreadPoolUtil.getExecutor().execute(() -> opLocationEvseRepository.sendEvseInfoMQToFleet(evseInfoModifyDTOList, Long.valueOf(locationId), userId));
                }
            }
        } catch (Exception e) {
            log.error("批量导入充电桩成功之后，推送充电枪信息给车队那边出现异常 : {}", e);
        }
    }
    /**
     * 模拟批量导入桩
     *
     * @param locationId        站点id
     * @param pileUploadDTOList 文件对象
     * @return 校验结果
     */
    @Override
    public PileUploadCheckVO simulateUploadModuleXls(String locationId, List<PileUploadDTO> pileUploadDTOList) {

        //SN码转大写
        try {
            pileUploadDTOList.forEach(pileUploadDTO -> pileUploadDTO.setSN(pileUploadDTO.getSN().toUpperCase()));
        } catch (Exception e) {
            log.error("Conversion failed");
        }
        //校验数据
        checkData(pileUploadDTOList, locationId);

        //找出SN与库重复并且站点一样的数据
        List<String> uploadSNList = pileUploadDTOList.stream().map(PileUploadDTO::getSN).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> dbPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnInAndLocationId(uploadSNList, Long.valueOf(locationId));
        List<String> dbSNList = dbPileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
        List<PileUploadCheckDetailVO> repeatPileUploadList = Lists.newArrayList();
        List<PileUploadCheckDetailVO> validPileUploadList = Lists.newArrayList();
        for (PileUploadDTO pileUploadDTO : pileUploadDTOList) {
            if (dbSNList.contains(pileUploadDTO.getSN())) {
                repeatPileUploadList.add(DozerConvert.map(pileUploadDTO, PileUploadCheckDetailVO.class));
            } else {
                validPileUploadList.add(DozerConvert.map(pileUploadDTO, PileUploadCheckDetailVO.class));
            }
        }
        PileUploadCheckVO pileUploadCheckVO = new PileUploadCheckVO();
        pileUploadCheckVO.setRepeatPileUploadList(repeatPileUploadList);
        pileUploadCheckVO.setValidPileUploadList(validPileUploadList);
        return pileUploadCheckVO;
    }

    /**
     * 批量保存桩
     *
     * @param pileUploadSaveDTO 保存数据
     * @return 保存结果
     */
    @Override
    public PileUploadSaveVO savePileList(PileUploadSaveDTO pileUploadSaveDTO) {
        Long stationId = pileUploadSaveDTO.getStationId();
        List<PileUploadSaveDetailDTO> validList = pileUploadSaveDTO.getValidPileUploadList();
        List<PileUploadSaveDetailDTO> repeatList = pileUploadSaveDTO.getRepeatPileUploadList();
        List<PileUploadSaveDetailDTO> saveList = Lists.newArrayList();
        //查询站点
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(stationId);
        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.STATION_NOT_EXIT);
        }

        //把要替换的数据从数据库删除
        if (CollectionUtils.isNotEmpty(repeatList) && !pileUploadSaveDTO.isSkip()) {
            log.info("要删除的数据：{}", JSON.toJSONString(repeatList));
            for (PileUploadSaveDetailDTO pileUploadSaveDetailDTO : repeatList) {
                //ES桩
                List<OpLocationPileEvseElasticDTO> esPileList = opLocationPileEvseElastic.findAllByLocationIdAndPileSn(stationId, pileUploadSaveDetailDTO.getSN());
                if (CollectionUtils.isNotEmpty(esPileList)) {
                    opLocationPileEvseElastic.deleteAll(esPileList);
                    //数据库桩
                    Set<Long> pileIdSet = esPileList.stream().map(OpLocationPileEvseElasticDTO::getId).collect(Collectors.toSet());
                    List<OpLocationPileEvseEntity> dbPileList = opLocationPileEvseMapper.selectBatchIds(pileIdSet);
                    if (CollectionUtils.isNotEmpty(dbPileList)) {
                        for (OpLocationPileEvseEntity dbPile : dbPileList) {
                            dbPile.setDeleted(1);
                            opLocationPileEvseMapper.updateById(dbPile);
                        }
                    }

                }
                //ES枪
                List<OpLocationEvseElasticDTO> evseList = opLocationEvseElastic.findAllByLocationIdAndPileSn(stationId, pileUploadSaveDetailDTO.getSN());
                if (CollectionUtils.isNotEmpty(evseList)) {
                    opLocationEvseElastic.deleteAll(evseList);
                    evseList.forEach(dto -> {
                        rabbitTemplate.convertAndSend(PILE_BASE_GUN_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_GUN_DELETE_ROUTE, JSON.toJSONString(dto.getId()));
                    });
                    //数据库枪
                    Set<Long> evseIdSet = evseList.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toSet());
                    List<OpLocationEvseEntity> dbEvseEntities = opLocationEvseMapper.selectBatchIds(evseIdSet);
                    if (CollectionUtils.isNotEmpty(dbEvseEntities)) {
                        for (OpLocationEvseEntity dbEVSEEntity : dbEvseEntities) {
                            dbEVSEEntity.setDeleted(1);
                            opLocationEvseMapper.updateById(dbEVSEEntity);
                        }
                    }
                }

            }
            saveList.addAll(repeatList);
        }

        //保存
        if (CollectionUtils.isNotEmpty(validList)) {
            saveList.addAll(validList);
        }
        List<PileInfoDTO> pileInfoDTOList = new ArrayList<>();
        log.info("要保存的数据：{}", JSON.toJSONString(saveList));
        List<PileUploadSaveDetailVO> createList = Lists.newArrayList();
        if (CollectionUtils.isNotEmpty(saveList)) {
            //循环每一个桩
            for (PileUploadSaveDetailDTO pileUploadSaveDetailDTO : saveList) {
                //构建createEVSE的参数
                OpLocationEvseDTO opLocationEvseDTO = buildCreateEVSEDTO(pileUploadSaveDetailDTO, stationId);
                opLocationEvseDTO.setPublicMark(1);
                PileVO addPileVO = opLocationEvseRepository.createEvse(opLocationEvseDTO, opLocationEntity);
                Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
                try {
                    stringRedisTemplate.delete(RedisKeyConstant.getStringAddPileException(userId));
                } catch (Exception e) {
                    log.info("添加桩删除es异常,e={}",e);
                }
                PileUploadSaveDetailVO pileUploadSaveDetailVO = new PileUploadSaveDetailVO();
                if (addPileVO != null) {
                    pileUploadSaveDetailVO.setPileId(addPileVO.getPileId());
                    pileUploadSaveDetailVO.setPileName(addPileVO.getPileName());
                    pileUploadSaveDetailVO.setSN(addPileVO.getPileSN());
                    List<PileEvseVO> pileEvseVOS = addPileVO.getPileEvseVOS();
                    //根据一个桩，构建枪id和枪号，一个桩下的枪存到同一个evseAndTarifVOList
                    List<EvseAndTarifVO> evseAndTarifVOList = new ArrayList<>();
                    if (CollUtil.isNotEmpty(pileEvseVOS)) {
                        pileEvseVOS.forEach(pileEvseVO -> {
                            EvseAndTarifVO evseAndTarifVO = new EvseAndTarifVO();
                            evseAndTarifVO.setEvseId(pileEvseVO.getEvseId());
                            evseAndTarifVO.setConnector(pileEvseVO.getConnector());
                            evseAndTarifVOList.add(evseAndTarifVO);
                        });
                    }
                    pileUploadSaveDetailVO.setList(evseAndTarifVOList);
                    // todo 需求改动，这个字段已经没有用了，为适配前端，故设为false
                    pileUploadSaveDetailVO.setHubjectCheck(false);
                }
                createList.add(pileUploadSaveDetailVO);
                log.info("调用createEvse接口：{}", JSON.toJSONString(opLocationEvseDTO));
                try {
                    pileInfoDTOList.add(formatPileInfoDTO(opLocationEvseDTO, opLocationEntity));
                } catch (Exception e) {
                    log.error("数据转换失败，桩批量同步运维失败.", e.getMessage());
                }
            }
        }
        log.info("OpLocationPileEvseRepositoryImpl.savePileList.createList = {}", JSON.toJSONString(createList));
        PileUploadSaveVO pileUploadSaveVO = buildPileUploadSaveVO(pileUploadSaveDTO);
        pileUploadSaveVO.setSaveSuccessDetailVOList(createList);
        //调用新运维平台同步接口，只保存成功的
        try {
            if (CollectionUtils.isNotEmpty(pileInfoDTOList)) {
                opsMgmtClient.batchSaveOpsPile(pileInfoDTOList);
            }
        } catch (Exception e) {
            log.error("桩批量同步运维失败.", e.getMessage());
        }
        return pileUploadSaveVO;
    }


    /**
     * 根据桩id查询枪列表（为了给每一个枪关联计费规则）
     *
     * @param pileUploadSaveVO
     * @return
     */
    @Override
    public List<EvseAndTariffIdVO> getEvseByPileId(PileUploadSaveVO pileUploadSaveVO) {

        log.info("OpLocationPileEvseRepositoryImpl.getEvseByPileId.pileUploadSaveVO = {}", pileUploadSaveVO);

        if (pileUploadSaveVO == null || CollUtil.isEmpty(pileUploadSaveVO.getSaveSuccessDetailVOList())) {
            return null;
        }
        List<EvseAndTariffIdVO> evseAndTariffIdVOList = new ArrayList<>();
        EvseAndTariffIdVO evseAndTariffIdVO = new EvseAndTariffIdVO();
        List<EvseAndTarifVO> evseAndTarifVOList = new ArrayList<>();
        EvseAndTarifVO evseAndTarifVO = new EvseAndTarifVO();
        log.info("OpLocationPileEvseRepositoryImpl.getEvseByPileId.SaveSuccessDetailVOList = {}", pileUploadSaveVO.getSaveSuccessDetailVOList());
        for (PileUploadSaveDetailVO pileUploadSaveDetailVO : pileUploadSaveVO.getSaveSuccessDetailVOList()) {
            Long pileId = pileUploadSaveDetailVO.getPileId();
            if (pileId != null) {
                Optional<OpLocationPileEvseElasticDTO> pileEvseElasticDTOOptional = opLocationPileEvseElastic.findById(pileId);
                if (pileEvseElasticDTOOptional.isPresent()) {
                    OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileEvseElasticDTOOptional.get();
                    if (opLocationPileEvseElasticDTO.getLocationId() != null) {
                        Optional<OpLocationElasticDTO> locationElasticDTOOptional =
                                opLocationElastic.findById(opLocationPileEvseElasticDTO.getLocationId());
                        if (locationElasticDTOOptional.isPresent()) {
                            OpLocationElasticDTO opLocationElasticDTO = locationElasticDTOOptional.get();
                            List<Long> evseIdList = Optional.ofNullable(JSON.parseArray(opLocationPileEvseElasticDTO.getEvseList()))
                                    .orElse(JSON.parseArray("[]")).toJavaList(Long.class);
                            if (CollUtil.isNotEmpty(evseIdList)) {
                                evseIdList.forEach(id -> {
                                    evseAndTarifVO.setEvseId(id);
                                    Optional<OpLocationEvseElasticDTO> evseElasticDTOOptional = opLocationEvseElastic.findById(id);
                                    if (evseElasticDTOOptional.isPresent()) {
                                        OpLocationEvseElasticDTO opLocationEvseElasticDTO = evseElasticDTOOptional.get();
                                        if (opLocationEvseElasticDTO.getEvseSn() != null) {
                                            try {
                                                evseAndTarifVO.setConnector(opLocationEvseElasticDTO.getEvseSn().split("_")[1]);
                                                evseAndTarifVOList.add(evseAndTarifVO);
                                            } catch (Exception e) {
                                                log.error("getEvseByPileId.e", e);
                                            }
                                        }
                                    }
                                });
                            }

                            evseAndTariffIdVO.setHubjectCheck(opLocationElasticDTO.getHubjectCheck());
                        }
                    }
                }
            }
            evseAndTariffIdVO.setPileId(pileUploadSaveDetailVO.getPileId());
            evseAndTariffIdVO.setPileName(pileUploadSaveDetailVO.getPileName());
            evseAndTariffIdVO.setPileSn(pileUploadSaveDetailVO.getSN());
            evseAndTariffIdVO.setList(evseAndTarifVOList);
            evseAndTariffIdVOList.add(evseAndTariffIdVO);
        }
        return evseAndTariffIdVOList;
    }

    private PileInfoDTO formatPileInfoDTO(OpLocationEvseDTO evse, OpLocationEntity opLocationEntity) {
        PileInfoDTO pileInfoDTO = new PileInfoDTO();
        pileInfoDTO.setSn(evse.getPileSN());
        pileInfoDTO.setPin(evse.getPinCode());
        pileInfoDTO.setAddress(opLocationEntity.getAddress());
        pileInfoDTO.setLongitude(evse.getLongitude());
        pileInfoDTO.setLatitude(evse.getLatitude());
        pileInfoDTO.setBrand(evse.getBrandName());
        pileInfoDTO.setCategory(2);
        pileInfoDTO.setRatedPower(String.valueOf(evse.getPower()));
        if (StringUtils.isEmpty(evse.getPowerType())) {
            if ("AC_1_PHASE".equals(evse.getPowerType())) {
                pileInfoDTO.setPowerSupplyPhases(1);
            } else if ("AC_3_PHASE".equals(evse.getPowerType())) {
                pileInfoDTO.setPowerSupplyPhases(2);
            } else if ("DC".equals(evse.getPowerType())) {
                pileInfoDTO.setPowerSupplyPhases(3);
            }
        }
        pileInfoDTO.setPileName(evse.getPileName());
        pileInfoDTO.setCountry(opLocationEntity.getCountry());
        pileInfoDTO.setPileState(opLocationEntity.getProvince());
        pileInfoDTO.setCity(opLocationEntity.getCity());
        pileInfoDTO.setZipCode(opLocationEntity.getPostalCode());
        pileInfoDTO.setCustomer(evse.getVendor());
        pileInfoDTO.setModel(evse.getProductModel());
        pileInfoDTO.setVendor(evse.getVendor());
        pileInfoDTO.setMerchantId(String.valueOf(LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        pileInfoDTO.setPileGroupId(String.valueOf(opLocationEntity.getId()));
        pileInfoDTO.setGroupId(String.valueOf(opLocationEntity.getId()));
        //查询组织id
        QueryWrapper<OpLocationOperationEntity> opLocationOperationEntityQueryWrapper = new QueryWrapper<>();
        opLocationOperationEntityQueryWrapper.eq("location_id", opLocationEntity.getId());
        opLocationOperationEntityQueryWrapper.eq("deleted", 0);
        List<OpLocationOperationEntity> opLocationOperationEntities = opLocationOperationMapper.selectList(opLocationOperationEntityQueryWrapper);
        if (CollUtil.isNotEmpty(opLocationOperationEntities) && Objects.nonNull(opLocationOperationEntities.get(0))) {
            pileInfoDTO.setGroupId(String.valueOf(opLocationOperationEntities.get(0).getGroupId()));
        }

        List<Integer> opLocationConnectorDTOs = evse.getOpLocationConnectorDTOs().stream().map(OpLocationConnectorDTO::getGunType).collect(Collectors.toList());
        if (!opLocationConnectorDTOs.isEmpty()) {
            List<ConnectorDTO> gunTypeList = new ArrayList<>();
            for (int i = 0; i < opLocationConnectorDTOs.size(); ++i) {
                gunTypeList.add(ConnectorDTO.builder().connectorNo(i + 1).connectorType(opLocationConnectorDTOs.get(i)).build());
            }
            pileInfoDTO.setConnectorList(gunTypeList);
        }
        return pileInfoDTO;
    }


    @Override
    public Result<List<CostModelRuleDTO>> queryTariffByPileSN(String pileSN) {
        Long tariffId = null;
        //根据桩SN码查询枪
        LambdaQueryWrapper<OpLocationEvseEntity> evseEntityLambdaQueryWrapper = new LambdaQueryWrapper<>();
        evseEntityLambdaQueryWrapper.like(OpLocationEvseEntity::getEvseSn, pileSN);
        List<OpLocationEvseEntity> evseEntities = opLocationEvseMapper.selectList(evseEntityLambdaQueryWrapper);
        log.info("根据桩SN码查询枪集合：{}", JSON.toJSONString(evseEntities));
        if (CollectionUtils.isNotEmpty(evseEntities)) {
            for (OpLocationEvseEntity opLocationEvseEntity : evseEntities) {
                if (opLocationEvseEntity.getTariffId() != null) {
                    tariffId = opLocationEvseEntity.getTariffId();
                    break;
                }
            }
        }
        log.info("桩：{} 的计费规则id:{}", pileSN, tariffId);
        if (tariffId != null) {
            CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
            costModelRuleDTO.setRuleIds(Lists.newArrayList(tariffId));
            Result<List<CostModelRuleDTO>> tariffAPPFeignResult = tariffFeignClient.queryListByIds(costModelRuleDTO);
            log.info("feign调用tariffAPPFeign：{}", JSON.toJSONString(tariffAPPFeignResult));
            return tariffAPPFeignResult;
        }
        return Result.ofSucceed(Lists.newArrayList());
    }

    @Override
    public Result<Map<String, CostModelRuleDTO>> queryTariffByPileSN(String... pileSN) {
        if (pileSN == null || pileSN.length == 0) {
            return Result.ofSucceed();
        }
        //根据桩SN码查询枪
        List<OpLocationEvseElasticDTO> esEVSEList = opLocationEvseElastic.findAllByPileSnIn(new HashSet<>(Arrays.asList(pileSN)));
        log.info("根据桩SN码查询枪集合：{}", JSON.toJSONString(esEVSEList));
        if (CollectionUtils.isEmpty(esEVSEList)) {
            return Result.ofSucceed();
        }
        //封装桩SN-桩对象的map
        Map<String, OpLocationEvseElasticDTO> evseMap = new HashMap<>();
        esEVSEList.forEach(esEVSE -> evseMap.put(esEVSE.getPileSn(), esEVSE));
        //获取枪的计费规则id
        Set<Long> tariffIdSet = esEVSEList.stream().map(OpLocationEvseElasticDTO::getTariffId).filter(Objects::nonNull).collect(Collectors.toSet());

        //根据计费规则id远程调用计费规则封装成计费规则id-实体类map
        Map<Long, CostModelRuleDTO> tariffMap = new HashMap<>();
        if (CollectionUtils.isNotEmpty(tariffIdSet)) {
            CostModelRuleDTO costModelRuleSearchDTO = new CostModelRuleDTO();
            costModelRuleSearchDTO.setRuleIds(Lists.newArrayList(tariffIdSet));
            Result<List<CostModelRuleDTO>> tariffAPPFeignResult = tariffFeignClient.queryListByIds(costModelRuleSearchDTO);
            log.info("feign调用tariffAPPFeign：{}", JSON.toJSONString(tariffAPPFeignResult));
            if (tariffAPPFeignResult != null && tariffAPPFeignResult.getCode() == HttpStatus.OK.value()) {
                List<CostModelRuleDTO> tariffList = tariffAPPFeignResult.getData();
                if (CollectionUtils.isNotEmpty(tariffList)) {
                    tariffList.forEach(costModelRuleDTO -> tariffMap.put(costModelRuleDTO.getId(), costModelRuleDTO));
                }
            }
        }
        //封装桩SN-计费规则的map
        Map<String, CostModelRuleDTO> pileSNTariffMap = new HashMap<>();
        Arrays.asList(pileSN).forEach(sn -> {
            OpLocationEvseElasticDTO esEVSE = evseMap.get(sn);
            if (esEVSE != null && esEVSE.getTariffId() != null) {
                CostModelRuleDTO costModelRuleDTO = tariffMap.get(esEVSE.getTariffId());
                if (costModelRuleDTO != null) {
                    pileSNTariffMap.put(sn, costModelRuleDTO);
                }
            }
        });
        return Result.ofSucceed(pileSNTariffMap);
    }

    @Override
    public OpLocationPileEvseElasticDTO findByPileSn(String pileSn) {
        log.info("findByPileSn,pileSn={}", pileSn);
        OpLocationPileEvseElasticDTO pileDto = opLocationPileEvseElastic.findByPileSn(pileSn);
        log.info("findByPileSn,pileDto={}", JSON.toJSONString(pileDto));
        return pileDto;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId,String keyword) {
        log.info("findByLocationId,locationId={},keyword={}", locationId, keyword);
        List<OpLocationPileEvseElasticDTO> pileDtoList = new ArrayList<>();
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("locationId", locationId));
        if (StringUtils.isNotBlank(keyword)) {
            keyword = QueryParserBase.escape(keyword);
            BoolQueryBuilder fuzzyQuery = QueryBuilders.boolQuery();
            fuzzyQuery.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyword)));
            fuzzyQuery.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
            queryBuilder.must(fuzzyQuery);
        }
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(0, 100))
                .build();
        log.info("searchForStream: {}", searchQuery.getQuery());
        Iterator<OpLocationPileEvseElasticDTO> stream = elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationPileEvseElasticDTO.class)
                .stream().map(SearchHit::getContent).iterator();

        while (stream.hasNext()) {
            OpLocationPileEvseElasticDTO dto = stream.next();
            pileDtoList.add(dto);
        }
        log.info("findByLocationId,pileDtoList={}", JSON.toJSONString(pileDtoList));
        return pileDtoList;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId) {
//        String findByLocationIdKey = genRedisKey("findByLocationId" , locationId.toString());
//        String o = stringRedisTemplate.opsForValue().get(findByLocationIdKey);
//        if (org.springframework.util.StringUtils.hasText(o)) {
//            return JSON.parseArray(o, OpLocationPileEvseElasticDTO.class);
//        }
        List<OpLocationPileEvseElasticDTO> list = findByLocationId(locationId, null);
//        stringRedisTemplate.opsForValue().set(findByLocationIdKey, JSON.toJSONString(list), timeoutRedisTtl, TimeUnit.MINUTES);
        return list;
    }

    @Override
    public boolean updateRuleId(RelatePileDTO relatePileDTO) {
        log.info("updateRuleId,relatePileDTO={}", JSON.toJSONString(relatePileDTO));
        List<PileDTO> piles = relatePileDTO.getPiles();
        if (piles == null || piles.isEmpty()) {
            return true;
        }
        List<Long> pileIds = piles.stream().map(PileDTO::getPileId).collect(Collectors.toList());
        Iterable<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseElastic.findAllById(pileIds);
        log.info("updateRuleId,pileDtoList={}", JSON.toJSONString(pileDtoList));
        Long ruleId = relatePileDTO.getRuleId();
        String ruleName = relatePileDTO.getRuleName();
        pileDtoList.forEach(dto -> {
            dto.setRuleId(ruleId);
            dto.setRuleName(ruleName);
            //-1删除
            if (ruleId == -1L) {
                dto.setRuleId(null);
                dto.setRuleName(null);
            }
        });
        log.info("更新进场控制id的桩信息：{}", JSON.toJSONString(pileDtoList));
        Iterable<OpLocationPileEvseElasticDTO> resultDtoList = opLocationPileEvseElastic.saveAll(pileDtoList);
        log.info("updateRuleId,resultDtoList={}", JSON.toJSONString(resultDtoList));
        return true;
    }

    @Override
    public boolean syncRuleName(Long ruleId, String name) {
        log.info("syncRuleName,ruleId={},name={}", ruleId, name);
        List<OpLocationPileEvseElasticDTO> pileList = opLocationPileEvseElastic.findAllByRuleId(ruleId);
        log.info("syncRuleName,pileList={}", JSON.toJSONString(pileList));
        if (!org.springframework.util.CollectionUtils.isEmpty(pileList)) {
            pileList.forEach(p -> p.setRuleName(name));
            opLocationPileEvseElastic.saveAll(pileList);
        }
        return true;
    }

    private OpLocationEvseDTO buildCreateEVSEDTOV2(PileImportNewDTO pileImportNewDTO,String locationId) {
        if (StringUtils.isBlank(pileImportNewDTO.getBrand())) {
            setResult(pileImportNewDTO,ImportErrorEnum.IMPORT_BRAND_REQUIRED);
        }

        ChargePileDTO chargePileDTO = null;
        //根据SN获取桩详细信息
        if (pileImportNewDTO.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
            Result<ChargePileDTO> chargePileDTOResult = deviceServiceFeign.pileDetail(pileImportNewDTO.getSn());
            if (chargePileDTOResult == null || chargePileDTOResult.getCode() != HttpStatus.OK.value()) {
                setResult(pileImportNewDTO,ImportErrorEnum.SN_NOT_FOUND);
            }
            chargePileDTO = chargePileDTOResult.getData();
            log.info("feign调用device服务，根据SN获取桩详细信息结果：{}", JSON.toJSONString(chargePileDTO));
        }
        if (chargePileDTO == null) {
            chargePileDTO = new ChargePileDTO();
        }

        //获取枪类型和数量
        Result<GunTypeAndNumVO> result = deviceServiceFeign.getGunTypeAndNum(pileImportNewDTO.getSn());
        //转成createEVSE的参数格式
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        //根据品牌名称查询品牌
        Long brandId = 2L;
        LambdaQueryWrapper<OpEvseBrandEntity> brandEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpEvseBrandEntity.class).eq(OpEvseBrandEntity::getName, pileImportNewDTO.getBrand()).eq(OpEvseBrandEntity::getDeleted, 0);
        List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectList(brandEntityLambdaQueryWrapper);
        if (CollectionUtils.isNotEmpty(opEvseBrandEntities)) {
            brandId = opEvseBrandEntities.get(0).getId();
        }
        opLocationEvseDTO.setBrandId(brandId);
        opLocationEvseDTO.setBrandName(pileImportNewDTO.getBrand());
        opLocationEvseDTO.setLocationId(Long.valueOf(locationId));
        opLocationEvseDTO.setPileName(StringUtils.isBlank(pileImportNewDTO.getPileName()) ? pileImportNewDTO.getSn() : pileImportNewDTO.getPileName());
        opLocationEvseDTO.setPileSN(pileImportNewDTO.getSn());
        opLocationEvseDTO.setPinCode(pileImportNewDTO.getPin());
        opLocationEvseDTO.setProductModel(pileImportNewDTO.getModel());
        if (StringUtils.isNotBlank(pileImportNewDTO.getChargingPower())) {
            opLocationEvseDTO.setPower(Double.valueOf(pileImportNewDTO.getChargingPower()));
        }
        String phaseConstant = "PHASE";
        int category = chargePileDTO.getCategory() == null ? CategoryEnum.AC.getCode() : chargePileDTO.getCategory();
        String type = CategoryEnum.getEnumByCode(category).getDesc();
        int phase = chargePileDTO.getPhase() == null ? 3 : chargePileDTO.getPhase();
        opLocationEvseDTO.setPowerType(type + "_" + phase + "_" + phaseConstant);
        opLocationEvseDTO.setThirdPart(pileImportNewDTO.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName()) ? 0 : 1);
        //连接器
        List<OpLocationConnectorDTO> oplocationConnectorScanDTOS = new ArrayList<>();
        if (StringUtils.isNotBlank(pileImportNewDTO.getGun1Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(1));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun1Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }else if (!ObjectUtils.isEmpty(result) && result.getCode() == 200 && !ObjectUtils.isEmpty(result.getData()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("1"))){
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(1));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("1")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (StringUtils.isNotBlank(pileImportNewDTO.getGun2Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(2));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun2Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }else if (!ObjectUtils.isEmpty(result) && result.getCode() == 200 && !ObjectUtils.isEmpty(result.getData()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("2"))){
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(2));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("2")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (StringUtils.isNotBlank(pileImportNewDTO.getGun3Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(3));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun3Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }else if (!ObjectUtils.isEmpty(result) && result.getCode() == 200 && !ObjectUtils.isEmpty(result.getData()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("3"))){
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(3));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("3")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (StringUtils.isNotBlank(pileImportNewDTO.getGun4Type())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(4));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileImportNewDTO.getGun4Type()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }else if (!ObjectUtils.isEmpty(result) && result.getCode() == 200 && !ObjectUtils.isEmpty(result.getData()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap()) && !ObjectUtils.isEmpty(result.getData().getGunTypeMap().get("4"))){
            Map<String, String> gunTypeMap = result.getData().getGunTypeMap();
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(4));
            opLocationConnectorDTO.setGunType(Integer.valueOf(gunTypeMap.get("4")));
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        opLocationEvseDTO.setOpLocationConnectorDTOs(oplocationConnectorScanDTOS);
         if (!ObjectUtils.isEmpty(chargePileDTO) && !ObjectUtils.isEmpty(chargePileDTO.getOutputPower())){
            opLocationEvseDTO.setPower(chargePileDTO.getOutputPower());
        }
        return opLocationEvseDTO;
    }

    private OpLocationEvseDTO buildCreateEVSEDTO(PileUploadSaveDetailDTO pileUploadSaveDetailDTO, Long stationId) {
        if (StringUtils.isBlank(pileUploadSaveDetailDTO.getBrandName())) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED);
        }

        ChargePileDTO chargePileDTO = null;
        //根据SN获取桩详细信息
        if (pileUploadSaveDetailDTO.getBrandName().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
            Result<ChargePileDTO> chargePileDTOResult = deviceServiceFeign.pileDetail(pileUploadSaveDetailDTO.getSN());
            if (chargePileDTOResult == null || chargePileDTOResult.getCode() != HttpStatus.OK.value()) {
                throw new MessageCodeException(PileBaseEnum.CAN_NOT_FIND_SN_INFO, new Object[]{pileUploadSaveDetailDTO.getSN()});
            }
            chargePileDTO = chargePileDTOResult.getData();
            log.info("feign调用device服务，根据SN获取桩详细信息结果：{}", JSON.toJSONString(chargePileDTO));
        }
        if (chargePileDTO == null) {
            chargePileDTO = new ChargePileDTO();
        }

        //转成createEVSE的参数格式
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        //根据品牌名称查询品牌
        Long brandId = 2L;
        LambdaQueryWrapper<OpEvseBrandEntity> brandEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpEvseBrandEntity.class).eq(OpEvseBrandEntity::getName, pileUploadSaveDetailDTO.getBrandName()).eq(OpEvseBrandEntity::getDeleted, 0);
        List<OpEvseBrandEntity> opEvseBrandEntities = opEvseBrandMapper.selectList(brandEntityLambdaQueryWrapper);
        if (CollectionUtils.isNotEmpty(opEvseBrandEntities)) {
            brandId = opEvseBrandEntities.get(0).getId();
        }
        opLocationEvseDTO.setBrandId(brandId);
        opLocationEvseDTO.setBrandName(pileUploadSaveDetailDTO.getBrandName());
        opLocationEvseDTO.setLocationId(stationId);
        opLocationEvseDTO.setPileName(pileUploadSaveDetailDTO.getPileName());
        opLocationEvseDTO.setPileSN(pileUploadSaveDetailDTO.getSN());
        opLocationEvseDTO.setPinCode(pileUploadSaveDetailDTO.getPin());
        opLocationEvseDTO.setProductModel(pileUploadSaveDetailDTO.getProductModel());
        opLocationEvseDTO.setPower(Double.valueOf(pileUploadSaveDetailDTO.getChargePower()));
        String phaseConstant = "PHASE";
        int category = chargePileDTO.getCategory() == null ? CategoryEnum.AC.getCode() : chargePileDTO.getCategory();
        String type = CategoryEnum.getEnumByCode(category).getDesc();
        int phase = chargePileDTO.getPhase() == null ? 3 : chargePileDTO.getPhase();
        opLocationEvseDTO.setPowerType(type + "_" + phase + "_" + phaseConstant);
        opLocationEvseDTO.setThirdPart(pileUploadSaveDetailDTO.getBrandName().equalsIgnoreCase(BrandEnum.AUTEL.getName()) ? 0 : 1);
        //连接器
        List<OpLocationConnectorDTO> oplocationConnectorScanDTOS = new ArrayList<>();
        if (StringUtils.isNotBlank(pileUploadSaveDetailDTO.getConnector1())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(1));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileUploadSaveDetailDTO.getConnector1()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (StringUtils.isNotBlank(pileUploadSaveDetailDTO.getConnector2())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(2));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileUploadSaveDetailDTO.getConnector2()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (StringUtils.isNotBlank(pileUploadSaveDetailDTO.getConnector3())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(3));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileUploadSaveDetailDTO.getConnector3()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        if (StringUtils.isNotBlank(pileUploadSaveDetailDTO.getConnector4())) {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setConnectorId(String.valueOf(4));
            opLocationConnectorDTO.setGunType(ConnectorGunTypeEnum.getEnumByName(pileUploadSaveDetailDTO.getConnector4()).getCode());
            oplocationConnectorScanDTOS.add(opLocationConnectorDTO);
        }
        opLocationEvseDTO.setOpLocationConnectorDTOs(oplocationConnectorScanDTOS);

        return opLocationEvseDTO;
    }

    /**
     * 封装保存结果
     *
     * @param pileUploadSaveDTO 保存对象
     * @return 保存结果
     */
    private PileUploadSaveVO buildPileUploadSaveVO(PileUploadSaveDTO pileUploadSaveDTO) {
        List<PileUploadSaveDetailDTO> repeatList = pileUploadSaveDTO.getRepeatPileUploadList();
        List<PileUploadSaveDetailDTO> errorList = pileUploadSaveDTO.getErrorPileUploadList();
        List<PileUploadSaveDetailDTO> validList = pileUploadSaveDTO.getValidPileUploadList();

        int successNum = 0;
        int failureNum = 0;
        List<PileUploadSaveDetailVO> pileUploadSaveErrorList = Lists.newArrayList();
        PileUploadSaveVO pileUploadSaveVO = new PileUploadSaveVO();

        //重复的数据
        if (CollectionUtils.isNotEmpty(repeatList)) {
            if (pileUploadSaveDTO.isSkip()) {
                //跳过
                failureNum += repeatList.size();
                pileUploadSaveErrorList.addAll(DozerConvert.mapList(repeatList, PileUploadSaveDetailVO.class));
                pileUploadSaveErrorList.forEach(pileUploadSaveDetailVO -> pileUploadSaveDetailVO.setFailureReason("SN Repeat,Skip"));
            } else {
                //替换
                successNum += repeatList.size();
            }
        }

        //失败的数据
        if (CollectionUtils.isNotEmpty(errorList)) {
            failureNum += errorList.size();
            pileUploadSaveErrorList.addAll(DozerConvert.mapList(errorList, PileUploadSaveDetailVO.class));
        }

        //有效的数据
        if (CollectionUtils.isNotEmpty(validList)) {
            successNum += validList.size();
        }

        pileUploadSaveVO.setSuccessNum(successNum);
        pileUploadSaveVO.setFailureNum(failureNum);
        pileUploadSaveVO.setSaveFailureDetailVOList(pileUploadSaveErrorList);
        return pileUploadSaveVO;
    }

    /**
     * 校验并且保存桩数据
     *
     * @param pileUploadDTOList 上传的桩数据
     */
    private void checkData(List<PileUploadDTO> pileUploadDTOList, String locationId) {

        //校验是否上传空数据
        if (CollectionUtils.isEmpty(pileUploadDTOList)) {
            throw new MessageCodeException(PileBaseEnum.EMPTY_FILE);
        }
        //删除全部空白数据
        for (int i = pileUploadDTOList.size() - 1; i >= 0; i--) {
            PileUploadDTO pileUploadDTO = pileUploadDTOList.get(i);
            if (StringUtils.isBlank(pileUploadDTO.getBrandName()) && StringUtils.isBlank(pileUploadDTO.getSN()) && StringUtils.isBlank(pileUploadDTO.getChargeType()) && StringUtils.isBlank(pileUploadDTO.getChargePower())) {
                pileUploadDTOList.remove(i);
            }
        }
        //校验是否上传空数据
        if (CollectionUtils.isEmpty(pileUploadDTOList)) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED);
        }
        // 校验充电桩名称
        List<String> pileNameList = pileUploadDTOList
                .stream()
                .map(PileUploadDTO::getPileName)
                .collect(Collectors.toList());
        this.checkPileName(pileNameList);
        Map<String, Integer> SNToSNCountMap = buildSNToSNCountMap(pileUploadDTOList);
        for (int i = 0; i <= pileUploadDTOList.size() - 1; i++) {
            PileUploadDTO pileUploadDTO = pileUploadDTOList.get(i);

            //1、必填字段校验 品牌/桩SN/PIN/桩类型/额定功率/枪类型必填
            if (StringUtils.isBlank(pileUploadDTO.getBrandName()) ||
                    StringUtils.isBlank(pileUploadDTO.getSN()) ||
                    StringUtils.isBlank(pileUploadDTO.getChargePower()) ||
                    (StringUtils.isBlank(pileUploadDTO.getConnector1()) &&
                            StringUtils.isBlank(pileUploadDTO.getConnector2()) &&
                            StringUtils.isBlank(pileUploadDTO.getConnector3())) &&
                            StringUtils.isBlank(pileUploadDTO.getConnector4())
            ) {
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED, new Object[]{i + 1, pileUploadDTO.getSN()});
            }
            //2、SN是否和文档本身重复
            Integer integer = SNToSNCountMap.get(pileUploadDTO.getSN());
            if (integer > 1) {
                throw new MessageCodeException(PileBaseEnum.SN_REPEAT_WITH_FILE, new Object[]{i + 1, pileUploadDTO.getSN()});
            }

            //3、道通品牌的校验
            if (pileUploadDTO.getBrandName().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
                //1、pin必填校验
                if (StringUtils.isBlank(pileUploadDTO.getPIN()) ||
                        StringUtils.isBlank(pileUploadDTO.getChargeType())) {
                    throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED, new Object[]{i + 1, pileUploadDTO.getSN()});
                }
                //2、校验SN码是否合法注册过(energy_device库的charge_pile表
                List<String> snList = Collections.singletonList(pileUploadDTO.getSN());
                Result<List<ChargePileDTO>> pileListResult = deviceServiceFeign.queryPileList(snList);
                log.info("查询device服务的queryPileList：{}", JSON.toJSONString(pileListResult));
                if (pileListResult != null && pileListResult.getCode() == HttpStatus.OK.value() && CollectionUtils.isEmpty(pileListResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.SN_NOT_REGISTER, new Object[]{i + 1, pileUploadDTO.getSN()});
                }
                //3、校验是否美标极简版桩
                Result<ChargePileDTO> chargePileDetailResult = null;
                try {
                    chargePileDetailResult = deviceServiceFeign.pileDetail(pileUploadDTO.getSN());
                } catch (Exception e) {
                    log.error("buildPileUploadSaveVO:" + e);
                }
                if (chargePileDetailResult != null && chargePileDetailResult.getData() != null) {
                    String productModel = chargePileDetailResult.getData().getProductModel();
                    if (StringUtils.isNotBlank(productModel)) {
                        for (AmericaPileProductModelEnum americaPileProductModelEnum : AmericaPileProductModelEnum.values()) {
                            if (Objects.equals(productModel, americaPileProductModelEnum.getValue())) {
                                throw new MessageCodeException(PileBaseEnum.AMERICA_PILE_ARE_NOT_ALLOW_ADD, new Object[]{i + 1, pileUploadDTO.getSN()});
                            }
                        }
                    }
                }
                //4、校验pileSN和pin是否有误——道通品牌桩SN和PIN码是否匹配
                VerifyDTO verifyDTO = new VerifyDTO();
                verifyDTO.setPin(pileUploadDTO.getPIN());
                verifyDTO.setPileSn(pileUploadDTO.getSN());
                Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(verifyDTO);
                log.info("查询device服务的verifyPilet：{}", JSON.toJSONString(pileListResult));
                if (booleanResult != null && booleanResult.getCode() == HttpStatus.OK.value() && booleanResult.getData() != null && !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.SN_NOT_MATCH_PIN, new Object[]{i + 1, pileUploadDTO.getSN()});
                }
                //5、校验桩是否被绑定
                if (StringUtils.isNotBlank(pileUploadDTO.getSN()) && StringUtils.isNotBlank(pileUploadDTO.getPIN())) {
                    Result<Object> result = dataServiceFeign.addPile(pileUploadDTO.getSN(), pileUploadDTO.getPIN());
                    log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
                    if (result != null && Objects.equals(result.getCode(), HttpStatus.OK.value()) && !Objects.equals(result.getData(), Boolean.TRUE)) {
                        throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND, new Object[]{i + 1, pileUploadDTO.getSN()});
                    }
                }
                //三方品牌的校验
            } else {
                //1、产品型号必填校验
                if (StringUtils.isBlank(pileUploadDTO.getProductModel())) {
                    throw new MessageCodeException(PileBaseEnum.DATA_NOT_COMPLETED, new Object[]{i + 1, pileUploadDTO.getSN()});
                }
                //2、品牌和型号是否匹配
                LambdaQueryWrapper<OpEvseBrandModelEntity> evseBrandModelEntityLambdaQueryWrapper = Wrappers.lambdaQuery(OpEvseBrandModelEntity.class)
                        .eq(OpEvseBrandModelEntity::getBrandName, pileUploadDTO.getBrandName())
                        .eq(OpEvseBrandModelEntity::getProductModel, pileUploadDTO.getProductModel());
                List<OpEvseBrandModelEntity> opEvseBrandModelEntities = opEvseBrandModelMapper.selectList(evseBrandModelEntityLambdaQueryWrapper);
                if (CollectionUtils.isEmpty(opEvseBrandModelEntities)) {
                    throw new MessageCodeException(PileBaseEnum.BRAND_ARE_NOT_MATCH_PRODUCT_MODEL);
                }
                //3、三方桩SN是否含有（^&*!()）特殊字符
                for (int z = 0; z < pileUploadDTO.getSN().length(); z++) {
                    char c = pileUploadDTO.getSN().charAt(z);
                    if (c == '^' || c == '&' || c == '*' || c == '!' || c == '(' || c == ')') {
                        throw new MessageCodeException(PileBaseEnum.SN_CODE_CONTAINS_ERROR_SPECIAL_CHARACTERS);
                    }
                }
                //4、data-service是否已被绑定为家桩
                Result<Boolean> restResult = homePileFeignClient.queryBind(pileUploadDTO.getSN());
                if (restResult != null && restResult.getCode().equals(HttpStatus.OK.value()) && restResult.getData().equals(Boolean.TRUE)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND);
                }
            }
        }

        //校验场站是否存在
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        //根据pileSN校验桩是否已存在在别的场站下
        List<String> uploadSNList = pileUploadDTOList.stream().map(PileUploadDTO::getSN).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> dbPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnInAndLocationIdNot(uploadSNList, Long.valueOf(locationId));
        List<String> dbSNList = dbPileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
        for (int i = 0; i <= pileUploadDTOList.size() - 1; i++) {
            PileUploadDTO pileUploadDTO = pileUploadDTOList.get(i);
            if (dbSNList.contains(pileUploadDTO.getSN())) {
                throw new MessageCodeException(PileBaseEnum.SN_USED_BY_OTHER_LOCATION, new Object[]{i + 1, pileUploadDTO.getSN()});
            }
        }
    }

    private void setResult(PileImportNewDTO pile, ImportErrorEnum importBrandIsRequired) {
        pile.setResult(messageSourceUtil.getMessage(String.valueOf(importBrandIsRequired.getCode())));
    }

    private void verifySn(PileImportNewDTO pile, ChargePileDTO chargePileDTO) {
        if (StringUtils.isBlank(pile.getSn())) {
            setResult(pile, ImportErrorEnum.IMPORT_SN_REQUIRED);
        } else if ((ObjectUtils.isEmpty(chargePileDTO) || ObjectUtils.isEmpty(chargePileDTO.getSn())) && pile.getBrand().equalsIgnoreCase(BrandEnum.AUTEL.getName())) {
            setResult(pile, ImportErrorEnum.SN_NOT_FOUND);
        }
    }

    private void verifyAutelPin(PileImportNewDTO pile, ChargePileDTO chargePileDTO) {
        if (StringUtils.isBlank(pile.getPin())) {
            setResult(pile, ImportErrorEnum.IMPORT_PIN_REQUIRED);
        } else if (Objects.nonNull(chargePileDTO) && !chargePileDTO.getPin().equals(pile.getPin())) {
            setResult(pile, ImportErrorEnum.PIN_SN_NOT_MAPPING);
        }
    }

    private void verifyThiredPile(Map<String, Integer> gunTypeMap, PileImportNewDTO pile) {
        log.info("批量导入桩第三方桩校验,gunTypeMap={},pile={}",gunTypeMap,pile);
        if (StringUtils.isBlank(pile.getChargingType())) {
            setResult(pile, ImportErrorEnum.IMPORT_CHARGE_TYPE_REQUIRED);
        } else if (!(AC_1_PHASE.equals(pile.getChargingType())
                || AC_3_PHASE.equals(pile.getChargingType())
                || DC.equals(pile.getChargingType()) || AC.equals(pile.getChargingType()))) {
            setResult(pile, ImportErrorEnum.CHARGE_TYPE_ERROR);
        }
        if (StringUtils.isBlank(pile.getChargingPower())) {
            setResult(pile, ImportErrorEnum.CHARGING_POWER_REQUIRED);
        } else {
            try {
                Double.valueOf(pile.getChargingPower());
            } catch (Exception e) {
                setResult(pile, ImportErrorEnum.CHARGE_POWER_ERROR);
            }
        }
        if (StringUtils.isBlank(pile.getGun1Type())) {
            setResult(pile, ImportErrorEnum.IMPORT_GUN1_REQUIRED);
        }
//        else if (!gunTypeMap.containsKey(pile.getGun1Type())) {
//            setResult(pile, ImportErrorEnum.GUN_TYPE_ERROR);
//        }
    }

    /**
     * 校验并且保存桩数据
     */
    private void checkDataV2(List<PileImportNewDTO> pileImportNewDTOList, String locationId,Map<String, ChargePileDTO> chargePileMap, List<String> fileSnRepeatList,Map<String, Integer> gunTypeMap) {
        if (CollectionUtils.isEmpty(pileImportNewDTOList)) {
            throw new MessageCodeException(PileBaseEnum.EMPTY_FILE);
        }
        pileImportNewDTOList.forEach(pile-> {
            if (StringUtils.isBlank(pile.getPileName())) {
                pile.setPileName(pile.getSn());
            }
            if (StringUtils.isBlank(pile.getBrand())) {
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
                log.info("批量导入充电桩校验,gunTypeMap={}",gunTypeMap);
                verifyThiredPile(gunTypeMap, pile);
            }

            //校验是否美标极简版桩
            Result<ChargePileDTO> chargePileDetailResult = null;
            try {
                chargePileDetailResult = deviceServiceFeign.pileDetail(pile.getSn());
            } catch (Exception e) {
                log.error("buildPileUploadSaveVO:" + e);
            }
            if (chargePileDetailResult != null && chargePileDetailResult.getData() != null) {
                String productModel = chargePileDetailResult.getData().getProductModel();
                if (StringUtils.isNotBlank(productModel)) {
                    for (AmericaPileProductModelEnum americaPileProductModelEnum : AmericaPileProductModelEnum.values()) {
                        if (Objects.equals(productModel, americaPileProductModelEnum.getValue())) {
                            setResult(pile, ImportErrorEnum.AMERICA_PILE_ARE_NOT_ALLOW_ADD);
                        }
                    }
                }
            }

            //5、校验桩是否被绑定
            if (StringUtils.isNotBlank(pile.getSn()) && StringUtils.isNotBlank(pile.getPin()) && BrandEnum.AUTEL.getName().equals(pile.getBrand()) && ObjectUtils.isEmpty(pile.getResult())) {
                Result<Object> result = dataServiceFeign.addPile(pile.getSn(), pile.getPin());
                log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
                if (result != null && Objects.equals(result.getCode(), HttpStatus.OK.value()) && !Objects.equals(result.getData(), Boolean.TRUE)) {
                    setResult(pile,ImportErrorEnum.THE_PILE_HAS_BEEN_BOUND_AND_CANNOT_BE_ADDED);
                }
            }
            //4、data-service是否已被绑定为家桩
            if (!pile.getBrand().equals(BrandEnum.AUTEL.getName()) && ObjectUtils.isEmpty(pile.getResult())) {
                Result<Boolean> restResult = homePileFeignClient.queryBind(pile.getSn());
                if (restResult != null && restResult.getCode().equals(HttpStatus.OK.value()) && restResult.getData().equals(Boolean.TRUE)) {
                    setResult(pile,ImportErrorEnum.THIS_PILE_HAS_BEEN_BOUND_AS_A_HOME_PILE);
                }
            }
        });


        //校验场站是否存在
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if (opLocationEntity == null) {
            throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
        }
        //根据pileSN校验桩是否已存在在场站下
        List<String> uploadSNList = pileImportNewDTOList.stream().filter(m->ObjectUtils.isEmpty(m.getResult())).map(PileImportNewDTO::getSn).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> dbPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(uploadSNList);
        List<String> dbSNList = dbPileEvseElasticDTOList.stream().map(OpLocationPileEvseElasticDTO::getPileSn).collect(Collectors.toList());
        for (int i = 0; i < pileImportNewDTOList.size(); i++) {
            PileImportNewDTO pileImportNewDTO = pileImportNewDTOList.get(i);
            if (dbSNList.contains(pileImportNewDTO.getSn())) {
                setResult(pileImportNewDTO,ImportErrorEnum.SN_USED_BY_OTHER_LOCATION);
            }
        }

        Map<String, List<PileImportNewDTO>> pileNameAndPileImportNewDTOListMap = pileImportNewDTOList
                .stream()
                .filter(var -> StringUtils.isBlank(var.getResult()))
                .collect(Collectors.groupingBy(PileImportNewDTO::getPileName));

        pileNameAndPileImportNewDTOListMap.forEach((key, value) -> {
            value.forEach(var -> {
                // 校验同一场站下的充电桩名称不能相同
                if (StringUtils.isNotBlank(locationId)) {
                    CheckPileNameDTO checkPileNameDTO = new CheckPileNameDTO();
                    checkPileNameDTO.setPileName(key);
                    checkPileNameDTO.setLocationId(Long.valueOf(locationId));
                    if(!opLocationEvseRepository.checkPileNameInLocationUnique(checkPileNameDTO)) {
                        this.setResult(var, ImportErrorEnum.NAME_ALREADY_USED);
                    }
                }
            });
            if (value.size() > 1) {
                for (int i = 0; i < value.size(); i++) {
                    if (i > 0) {
                        this.setResult(value.get(i), ImportErrorEnum.NAME_ALREADY_USED);
                    }
                }
            }
        });
    }

    /**
     * @param pileNameList
     * @function 校验充电桩名称
     */
    private void checkPileName(List<String> pileNameList) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.checkPileName pileNameList : {}", JSON.toJSONString(pileNameList));

        // 校验充电桩名称是否为空
        if (org.apache.commons.lang3.ObjectUtils.isEmpty(pileNameList)) {
            throw new MessageCodeException(PileBaseEnum.PLEASE_ENTER_THE_CHARGING_PILE_NAME);
        }

        // 充电桩名称是否为空的标志
        boolean pileNameIsBlankMark = false;
        Set<String> pileNameSet = new HashSet<>();
        for (String pileName : pileNameList) {
            pileNameSet.add(pileName);
            if (StringUtils.isBlank(pileName)) {
                pileNameIsBlankMark = true;
                break;
            }
        }
        if (pileNameIsBlankMark) {
            throw new MessageCodeException(PileBaseEnum.PLEASE_ENTER_THE_CHARGING_PILE_NAME);
        }
        // 校验充电桩名称是否重复
        if (pileNameList.size() != pileNameSet.size()) {
            throw new MessageCodeException(PileBaseEnum.NAME_ALREADY_USED);
        }
    }

    private Map<String, Integer> buildSNToSNCountMap(List<PileUploadDTO> pileUploadDTOList) {
        Map<String, Integer> SNToSNCountMap = new HashMap<>();
        for (PileUploadDTO pileUploadDTO : pileUploadDTOList) {
            if (StringUtils.isNotBlank(pileUploadDTO.getSN())) {
                String SN = pileUploadDTO.getSN();
                Integer SNCount = SNToSNCountMap.get(SN);
                if (SNCount == null) {
                    SNCount = 0;
                }
                SNCount++;
                SNToSNCountMap.put(SN, SNCount);
            }
        }
        return SNToSNCountMap;
    }

    private Map<String, Integer> buildSNToSNCountMapV2(List<PileImportNewDTO> pileImportNewDTOList) {
        Map<String, Integer> SNToSNCountMap = new HashMap<>();
        for (PileImportNewDTO pileImportNewDTO : pileImportNewDTOList) {
            if (StringUtils.isNotBlank(pileImportNewDTO.getSn())) {
                String SN = pileImportNewDTO.getSn();
                Integer SNCount = SNToSNCountMap.get(SN);
                if (SNCount == null) {
                    SNCount = 0;
                }
                SNCount++;
                SNToSNCountMap.put(SN, SNCount);
            }
        }
        return SNToSNCountMap;
    }

    /**
     * 构建品牌实体类map
     *
     * @param opEvseBrandEntities 品牌集合
     * @return 品牌实体类map
     */
    private Map<Long, OpEvseBrandEntity> buildBrandIdBrandEntityMap(List<OpEvseBrandEntity> opEvseBrandEntities) {
        Map<Long, OpEvseBrandEntity> brandIdBrandEntityMap = new HashMap<>();
        opEvseBrandEntities.forEach(opEvseBrandEntity -> brandIdBrandEntityMap.put(opEvseBrandEntity.getId(), opEvseBrandEntity));
        return brandIdBrandEntityMap;
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
            log.error("multipartFile2File:" + e);
        }
        return file;
    }

    private List<PileImportDTO> pileImportDTOList() {
        List<PileImportDTO> list = ListUtils.newArrayList();
        PileImportDTO pileImportDTO = new PileImportDTO();
        for (int i = 0; i < 1; i++) {
            list.add(pileImportDTO);
        }
        return list;
    }

    @Override
    public Page<LocationForAdvertiseDTO> syncToAdvertise(PileSyncForAdvertiseParamDTO paramDTO) {
        Page<LocationForAdvertiseDTO> resultPage = new Page<>();
        //根据groupId查询
        BoolQueryBuilder locationQueryBuilder = new BoolQueryBuilder();
        if (paramDTO.getOrgId() != null) {
            locationQueryBuilder.must(QueryBuilders.termQuery("groupId", paramDTO.getOrgId()));
        }
        if (paramDTO.getLocationId() != null) {
            locationQueryBuilder.must(QueryBuilders.termQuery("id", paramDTO.getLocationId()));
        }
        locationQueryBuilder.mustNot(QueryBuilders.termQuery("platform", 2));
        PageRequest pageRequest = PageRequest.of(paramDTO.getPage() < 1 ? 1 : paramDTO.getPage() - 1, paramDTO.getPageSize());

//        org.springframework.data.domain.Page<OpLocationElasticDTO> locationPage =
//                opLocationElastic.search(locationQueryBuilder, pageRequest);
        SearchHits<OpLocationElasticDTO> locationPage =
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                        .withQuery(locationQueryBuilder)
                        .withTrackTotalHits(true)
                        .withPageable(pageRequest)
                        .build(), OpLocationElasticDTO.class);
        List<OpLocationElasticDTO> list = locationPage.stream().map(SearchHit::getContent).collect(Collectors.toList());

        log.info("OpLocationPileEvseRepositoryImpl.syncToAdvertise locationPage = " + JSON.toJSONString(locationPage));
        List<LocationForAdvertiseDTO> locationList = new ArrayList<>();
        list.forEach(location -> {
            if (location == null || location.getId() == null) {
                return;
            }
            LocationForAdvertiseDTO locationForAdvertiseDTO = new LocationForAdvertiseDTO();
            BeanUtils.copyProperties(location, locationForAdvertiseDTO);
            BoolQueryBuilder pileQueryBuilder = new BoolQueryBuilder();
            pileQueryBuilder.must(QueryBuilders.termQuery("locationId", location.getId()));
            Iterable<OpLocationPileEvseElasticDTO> pileIterable =
//                    opLocationPileEvseElastic.search(pileQueryBuilder);
            elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(pileQueryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());

            log.info("OpLocationPileEvseRepositoryImpl.syncToAdvertise pileIterable = " + JSON.toJSONString(pileIterable));
            pileIterable.forEach(pile -> {
                LocationForAdvertiseDTO.PileDTO pileDTO = new LocationForAdvertiseDTO.PileDTO();
                BeanUtils.copyProperties(pile, pileDTO);
                Result<ChargePileDTO> chargePileDTOResult = deviceServiceFeign.pileDetail(pile.getPileSn());
                if (chargePileDTOResult == null || chargePileDTOResult.getData() == null) {
                    return;
                }
                LocationForAdvertiseDTO.ChargePile chargePile = new LocationForAdvertiseDTO.ChargePile();
                BeanUtils.copyProperties(chargePileDTOResult.getData(), chargePile);
                pileDTO.setChargePile(chargePile);
                BoolQueryBuilder evseQueryBuilder = new BoolQueryBuilder();
                evseQueryBuilder.must(QueryBuilders.termQuery("pileSn", pile.getPileSn()));
                Iterable<OpLocationEvseElasticDTO> evseIterable =
//                        opLocationEvseElastic.search(evseQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(evseQueryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

                log.info("OpLocationPileEvseRepositoryImpl.syncToAdvertise evseIterable = " + JSON.toJSONString(evseIterable));
                evseIterable.forEach(evse -> {
                    LocationForAdvertiseDTO.EvseDTO evseDTO = new LocationForAdvertiseDTO.EvseDTO();
                    BeanUtils.copyProperties(evse, evseDTO);
                    pileDTO.addEvseDTO(evseDTO);
                });
                locationForAdvertiseDTO.addPileDTO(pileDTO);

            });
            locationList.add(locationForAdvertiseDTO);
        });
        resultPage.setCurrent(paramDTO.getPage());
        resultPage.setSize(paramDTO.getPageSize());
        resultPage.setRecords(locationList);

        PageImpl<OpLocationElasticDTO> pageImpl = new PageImpl<>(list, pageRequest, locationPage.getTotalHits());
        resultPage.setPages(pageImpl.getTotalPages());
        resultPage.setTotal(pageImpl.getTotalElements());
        return resultPage;
    }

    @Override
    public Boolean syncPileOperatorIdCache(String sn) {
        if (StringUtils.isNotBlank(sn)) {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(sn);
            if (opLocationPileEvseElasticDTO != null && opLocationPileEvseElasticDTO.getOperatorId() != null) {
                // 缓存桩运营商用户ID
                String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(sn);
                stringRedisTemplate.opsForValue().set(snOperatorIdKey, opLocationPileEvseElasticDTO.getOperatorId().toString());
            }
        } else {
            Iterable<OpLocationPileEvseElasticDTO> iterator = opLocationPileEvseElastic.findAll();
            ArrayList<OpLocationPileEvseElasticDTO> esPileList = Lists.newArrayList(iterator);
            if (CollectionUtils.isNotEmpty(esPileList)) {
                esPileList.forEach(p -> {
                    if (p.getOperatorId() != null) {
                        // 缓存桩运营商用户ID
                        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(p.getPileSn());
                        stringRedisTemplate.opsForValue().set(snOperatorIdKey, p.getOperatorId().toString());
                    }
                });
            }
        }

        return true;
    }

    @Override
    public List<OpLocationPileEvseVO> getListByLocationIds(List<Long> locationIds) {
        List<OpLocationPileEvseVO> resultList = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(locationIds)) {
            List<OpLocationPileEvseElasticDTO> pileDtoList = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                    .withPageable(PageRequest.of(0, 100))
                    .withSourceFilter(new FetchSourceFilter(new String[]{"id", "name", "locationId", "pileSn", "status"}, null))
                    .withQuery(QueryBuilders.termsQuery("locationId", locationIds))
                    .build(), OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(pileDtoList)) {
                return resultList;
            }
            pileDtoList.forEach(dto -> {
                OpLocationPileEvseVO vo = new OpLocationPileEvseVO();
                vo.setId(dto.getId());
                vo.setName(dto.getName());
                vo.setLocationId(dto.getLocationId());
                vo.setPileSn(dto.getPileSn());
                vo.setStatus(dto.getStatus());
                resultList.add(vo);
            });
        } else {
            log.info("getListByLocationIds,locationIds={}", locationIds);
        }
        return resultList;
    }

    /**
     * 落地页设备信息展示
     *
     * @param scanPileEvseDTO
     * @return
     */
    @Override
    public Result<List<OpLocationPileEvseLandingPageVO>> scanPileEvceDetail(ScanPileEvseDTO scanPileEvseDTO) {
        String scanContent = scanPileEvseDTO.getContent().toUpperCase();
        if (StringUtils.isEmpty(scanContent)) {
            return Result.ofSucceed(null);
        }
        int length = scanContent.length();
        if (StringUtils.containsIgnoreCase(scanContent, "https://") || StringUtils.containsIgnoreCase(scanContent, "http://")) {
            //直流桩的SN获取
            scanContent = scanContent.substring(scanContent.lastIndexOf("/") + 1);
            length = scanContent.length();
        }
        //校验传参是否符合规范
        if (scanSpecialChar(scanContent, length)) {
            log.info("扫码非法字符串返回: {}", scanContent);
            return Result.ofSucceed(null);
        }
        if (length == 18) {
            //传参桩序列号
            return verifyPile(scanContent);
        } else if (length == 20) {
            //传参枪号
            return verifyGun(scanContent);
        }

        return Result.ofSucceed(null);
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findList(List<String> pileSnList) {
        List<OpLocationPileEvseElasticDTO> resultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(pileSnList)) return resultList;
        StopWatch stopWatch = new StopWatch("按桩SN查询桩信息");
        stopWatch.start("查询桩列表");
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("pileSn", pileSnList));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(0, 100))
                .build();
        Iterator<OpLocationPileEvseElasticDTO> stream =
//                elasticsearchRestTemplate.stream(searchQuery, OpLocationPileEvseElasticDTO.class);
        elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationPileEvseElasticDTO.class)
                .stream().map(SearchHit::getContent).iterator();

        while (stream.hasNext()) {
            OpLocationPileEvseElasticDTO dto = stream.next();
            resultList.add(dto);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return resultList;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findList(List<Long> locationIds, String keyword) {
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        query.must(QueryBuilders.termsQuery("locationId", locationIds));
        if (StringUtils.isNotEmpty(keyword)) {
            keyword = QueryParserBase.escape(keyword);
            BoolQueryBuilder fuzzy = QueryBuilders.boolQuery();
            fuzzy.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", keyword)));
            fuzzy.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyword)));
            query.must(fuzzy);
        }
        NativeSearchQuery build = new NativeSearchQueryBuilder()
                .withQuery(query)
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "locationId"}, null))
                .withPageable(PageRequest.of(0, 10000))
                .build();
        SearchHitsIterator<OpLocationPileEvseElasticDTO> iterator = this.elasticsearchRestTemplate.searchForStream(build, OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> resultList = new ArrayList<>();
        while (iterator.hasNext()) {
            OpLocationPileEvseElasticDTO dto = iterator.next().getContent();
            resultList.add(dto);
        }
        return resultList;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findListByIds(List<Long> pileIds) {
        List<OpLocationPileEvseElasticDTO> resultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(pileIds)) return resultList;
        StopWatch stopWatch = new StopWatch("按桩ID查询桩信息");
        stopWatch.start("查询桩列表");
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("id", pileIds));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(0, 100))
                .build();
        Iterator<OpLocationPileEvseElasticDTO> stream =
//                elasticsearchRestTemplate.stream(searchQuery, OpLocationPileEvseElasticDTO.class);
        elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationPileEvseElasticDTO.class)
                .stream().map(SearchHit::getContent).iterator();
        while (stream.hasNext()) {
            OpLocationPileEvseElasticDTO dto = stream.next();
            resultList.add(dto);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return resultList;
    }

    /**
     * @param sellerId    商家id
     * @param currentPage 当前页码
     * @param pageSize    每页数据量
     * @param keyword     关键词（仅支持桩SN的模糊搜索）
     * @return 英标桩列表
     * @function 分页查询英标桩列表
     */
    @Override
    public PageVO<OpLocationPileEvseElasticDTO> getBritishActPileListPage(Long sellerId, int currentPage, int pageSize, String keyword) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.getBritishActPileListPage sellerId : {} and currentPage : {} and pageSize : {} and keyword : {}", JSON.toJSONString(sellerId), JSON.toJSONString(currentPage), JSON.toJSONString(pageSize), JSON.toJSONString(keyword));

        // 从ES中查询英标桩列表
        // 分页条件
        PageRequest pageRequest = PageRequest.of(currentPage - 1, pageSize);

        // 查询条件
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId))
                .must(QueryBuilders.termQuery(BaseConstant.BRITAIN_STAND_PILE_MARK, Boolean.TRUE));
        if (StrUtil.isNotBlank(keyword)) {
            // 处理特殊字符
            String keywordString = StringUtil.escapeChar(keyword);
            boolQueryBuilder.must(QueryBuilders.wildcardQuery(BaseConstant.PILE_SN, "*" + keywordString + "*"));
        }

        log.info("===>>>OpLocationPileEvseRepositoryImpl.getBritishActPileListPage boolQueryBuilder : {}", JSON.toJSONString(boolQueryBuilder));

        // ES查询语句
        NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
                .withIndicesBoost(new IndexBoost(OpLocationPileEvseElasticDTO.class.getAnnotation(Document.class).indexName(), 1.0f))
                .withQuery(boolQueryBuilder)
                .withSort(SortBuilders.fieldSort(BaseConstant.CREATED_AT).order(SortOrder.DESC))
                .withPageable(pageRequest)
                .build();

//        AggregatedPage<OpLocationPileEvseElasticDTO> page = elasticsearchRestTemplate.queryForPage(nativeSearchQuery, OpLocationPileEvseElasticDTO.class);
        SearchHits<OpLocationPileEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> list = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());

        // 构造返回对象
        PageVO<OpLocationPileEvseElasticDTO> response = new PageVO<>();
        response.setPage(currentPage);
        response.setPageSize(pageSize);
        if (searchHits != null && searchHits.getTotalHits() > 0) {
            response.setContent(list);

            PageImpl<OpLocationPileEvseElasticDTO> pageImpl = new PageImpl<>(list, pageRequest, searchHits.getTotalHits());
            response.setTotalPages(pageImpl.getTotalPages());
            response.setTotalRows(pageImpl.getTotalElements());
        }
        return response;
    }

    /**
     * @param opLocationPileEvseEntity op_location_pile_evse表实体类
     * @return 是否编辑成功
     * @function 英国法案认证：编辑默认充电时间
     */
    @Override
    public Boolean updateBritishActPileDefaultChargingTime(OpLocationPileEvseEntity opLocationPileEvseEntity,Boolean britainApproveSwitch) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.updateBritishActPileDefaultChargingTime opLocationPileEvseEntity : {}", JSON.toJSONString(opLocationPileEvseEntity));

        if (opLocationPileEvseEntity == null) {
            return Boolean.FALSE;
        }
        ThreadPoolUtil.getExecutor().execute(()->{
            log.info("开始异步查询默认充电时间");
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            log.info("睡眠时间结束，开始查询");
            Result<PileOffPeakHourParamDTO> result = opsMgmtClient.queryConfigBySnforDefaultCharging(opLocationPileEvseEntity.getPileSn(), OFFPEAKHOURPARAM);
            log.info("异步处理默认充电时间查询结果,result={}",JSON.toJSONString(result));
            if (!ObjectUtils.isEmpty(result) && !ObjectUtils.isEmpty(result.getData()) && Integer.valueOf(cn.hutool.http.HttpStatus.HTTP_OK).equals(result.getCode()) && !ObjectUtils.isEmpty(result.getData().getOffPeakHours())) {
                log.info("处理默认充电时间查询结果");
                List<OffPeakHourPeriodDTO> offPeakHours = result.getData().getOffPeakHours();
                Integer alertTimeout = result.getData().getAlertTimeout();
                Map<List<TimePeriodDTO>, List<OffPeakHourPeriodDTO>> offPeakHoursMap = offPeakHours.stream().collect(Collectors.groupingBy(OffPeakHourPeriodDTO::getPeriods));
                List<DefaultChargingTimeVO> defaultChargingTimeVOList = new ArrayList<>();
                offPeakHoursMap.forEach((k,v)-> {
                    DefaultChargingTimeVO defaultChargingTimeVO = new DefaultChargingTimeVO();
                    List<Integer> weeks = offPeakHoursMap.get(k).stream().map(OffPeakHourPeriodDTO::getWeekday).collect(Collectors.toList());
                    List<ThatDayDefaultChargingTimeVO> weeksRules = new ArrayList<>();
                    for (TimePeriodDTO timePeriodDTO : k) {
                        ThatDayDefaultChargingTimeVO thatDayDefaultChargingTimeVO = new ThatDayDefaultChargingTimeVO();
                        thatDayDefaultChargingTimeVO.setBeginHour((int) Math.floor(timePeriodDTO.getStartPeriod()/60/60));
                        thatDayDefaultChargingTimeVO.setBeginMinute(timePeriodDTO.getStartPeriod()/60%60);
                        thatDayDefaultChargingTimeVO.setEndHour((int) Math.floor((timePeriodDTO.getStartPeriod() + timePeriodDTO.getDuration())/60/60));
                        thatDayDefaultChargingTimeVO.setEndMinute((timePeriodDTO.getStartPeriod() + timePeriodDTO.getDuration())/60%60);
                        weeksRules.add(thatDayDefaultChargingTimeVO);
                    }
                    for (Integer week : weeks) {
                        if (week.equals(0)) {
                            weeks.remove(week);
                            weeks.add(7);
                        }
                    }
                    defaultChargingTimeVO.setWeeksRules(weeksRules);
                    defaultChargingTimeVO.setWeeks(weeks);
                    defaultChargingTimeVO.setAlertTimeout(alertTimeout);
                    defaultChargingTimeVOList.add(defaultChargingTimeVO);
                });
                try {
                    log.info("updateDefalutCharging,defaultChargingTimeVOList={}",JSON.toJSONString(defaultChargingTimeVOList));
                    LambdaUpdateWrapper<OpLocationPileEvseEntity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();
                    lambdaUpdateWrapper
                            .set(OpLocationPileEvseEntity::getDefaultChargingTime, JSON.toJSONString(defaultChargingTimeVOList))
                            .set(OpLocationPileEvseEntity::getUpdatedAt, opLocationPileEvseEntity.getUpdatedAt())
                            .eq(OpLocationPileEvseEntity::getId, opLocationPileEvseEntity.getId());
                    // 先更新数据库
                    if (this.update(lambdaUpdateWrapper)) {
                        // 同步更新ES
                        Optional<OpLocationPileEvseElasticDTO> searchById = opLocationPileEvseElastic.findById(opLocationPileEvseEntity.getId());
                        if (searchById.isPresent()) {
                            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = searchById.get();
                            opLocationPileEvseElasticDTO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
                            opLocationPileEvseElasticDTO.setDefaultChargingTime(JSON.toJSONString(defaultChargingTimeVOList));
                            opLocationPileEvseElasticDTO.setBritainStandPileMark(Boolean.TRUE);
                            opLocationPileEvseElasticDTO.setBritainApproveSwitch(britainApproveSwitch);
                            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
                        }
                    }
                } catch (Exception e) {
                    log.error("===>>>英国法案认证：编辑默认充电时间出现了错误！", e);
                }
            }
            });
        return Boolean.TRUE;
    }


    @Override
    public Boolean synchronousBritishActPileDefaultChargingTime(OpLocationPileEvseEntity opLocationPileEvseEntity,Boolean britainApproveSwitch) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.updateBritishActPileDefaultChargingTime opLocationPileEvseEntity : {}", JSON.toJSONString(opLocationPileEvseEntity));

        if (opLocationPileEvseEntity == null) {
            return Boolean.FALSE;
        }
                try {
                    LambdaUpdateWrapper<OpLocationPileEvseEntity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();
                    lambdaUpdateWrapper
                            .set(OpLocationPileEvseEntity::getDefaultChargingTime, opLocationPileEvseEntity.getDefaultChargingTime())
                            .set(OpLocationPileEvseEntity::getUpdatedAt, opLocationPileEvseEntity.getUpdatedAt())
                            .eq(OpLocationPileEvseEntity::getId, opLocationPileEvseEntity.getId());
                    // 先更新数据库
                    if (this.update(lambdaUpdateWrapper)) {
                        // 同步更新ES
                        Optional<OpLocationPileEvseElasticDTO> searchById = opLocationPileEvseElastic.findById(opLocationPileEvseEntity.getId());
                        if (searchById.isPresent()) {
                            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = searchById.get();
                            opLocationPileEvseElasticDTO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
                            opLocationPileEvseElasticDTO.setDefaultChargingTime(opLocationPileEvseEntity.getDefaultChargingTime());
                            opLocationPileEvseElasticDTO.setBritainStandPileMark(Boolean.TRUE);
                            opLocationPileEvseElasticDTO.setBritainApproveSwitch(britainApproveSwitch);
                            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
                        }
                    }
                } catch (Exception e) {
                    log.error("===>>>英国法案认证：编辑默认充电时间出现了错误！", e);
                }
        return Boolean.TRUE;
    }


    /**
     * 随机延迟开关
     */
    @Override
    public Boolean randomDelay(RandomDelayDTO randomDelayDTO) {
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(randomDelayDTO.getPileSn());
        if (opLocationPileEvseElasticDTO != null) {
            opLocationPileEvseElasticDTO.setRandomDelaySwitch(randomDelayDTO.getRandomDelaySwitch());
            if (randomDelayDTO.getRandomDelayTime() != null) {
                opLocationPileEvseElasticDTO.setRandomDelayTime(randomDelayDTO.getRandomDelayTime());
            } else {
                opLocationPileEvseElasticDTO.setRandomDelayTime(null);
            }
            if (!ObjectUtils.isEmpty(randomDelayDTO.getConfigBritishActVO())) {
                Result<Boolean> sendBritishAct = opsMgmtClient.sendBritishAct(randomDelayDTO.getConfigBritishActVO());
                log.info("randomDelay,sendBritishAct={}",sendBritishAct.getData());
                if (sendBritishAct.getData() == false) {
                    return false;
                }
            }
            ThreadPoolUtil.getExecutor().execute(()->{
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                Result<PileRandomDelayTimeDTO> result = opsMgmtClient.queryConfigBySn(opLocationPileEvseElasticDTO.getPileSn(), RANDOMDELAYTIME);
                log.info("randomDelay,result={}",result);
                if (!ObjectUtils.isEmpty(result) && !ObjectUtils.isEmpty(result.getData())) {
                    PileRandomDelayTimeDTO pileRandomDelayTimeDTO = result.getData();
                    opLocationPileEvseElasticDTO.setRandomDelayTime(pileRandomDelayTimeDTO.getRandomDelayMax());
                    opLocationPileEvseElasticDTO.setRandomDelaySwitch(pileRandomDelayTimeDTO.getRandomDelayEnabled());
                    opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
                    log.info("randomDelay,opLocationPileEvseElasticDTO={}",opLocationPileEvseElasticDTO);
                }
            });
        }
        return true;
    }

    /**
     * @param locationIdList 场站id集合
     * @return 充电桩信息集合
     * @function 根据场站id集合查询充电桩信息集合
     */
    @Override
    public List<OpLocationPileEvseEntity> getPileInfoListByLocationIdList(List<Long> locationIdList) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.getPileInfoListByLocationIdList locationIdList : {}", JSON.toJSONString(locationIdList));

        if (ObjectUtils.isEmpty(locationIdList)) {
            return null;
        }
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .in(OpLocationPileEvseEntity::getLocationId, locationIdList)
                .eq(OpLocationPileEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationPileEvseEntity::getCreatedAt, OpLocationPileEvseEntity::getId);
        return opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param pileSn 充电桩序列号
     * @return 充电桩信息
     * @function 根据充电桩序列号查询充电桩信息
     */
    @Override
    public OpLocationPileEvseEntity getPileInfoByPileSn(String pileSn) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.getPileInfoByPileSn pileSn : {}", JSON.toJSONString(pileSn));

        if (StrUtil.isBlank(pileSn)) {
            return null;
        }
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpLocationPileEvseEntity::getPileSn, pileSn)
                .eq(OpLocationPileEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationPileEvseEntity::getCreatedAt, OpLocationPileEvseEntity::getId)
                .last("limit 1");
        return opLocationPileEvseMapper.selectOne(lambdaQueryWrapper);
    }

    private Result<List<OpLocationPileEvseLandingPageVO>> verifyPile(String scanContent) {
        List<OpLocationPileEvseLandingPageVO> resultPileEvse = new ArrayList<>();
        String sn = scanContent.length() == 20 ? scanContent.substring(0, scanContent.length() - 2) : scanContent;
        Result<PileStandInfoDTO> pileStandInfoDTOResult = deviceServiceFeign.queryStandInfo(sn);
        if (pileStandInfoDTOResult == null || ObjectUtils.isEmpty(pileStandInfoDTOResult.getData())) {
            return Result.ofSucceed(null);
        }
        Integer usage = pileStandInfoDTOResult.getData().getUsage();
        if (usage != 2) {
            //是商桩或者私桩共享的桩
            log.info("桩类型查询结果：{}", JSON.toJSONString(pileStandInfoDTOResult));
            //根据桩序列号查询枪信息
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
            queryBuilder.must(QueryBuilders.termsQuery("pileSn", sn));
            //若扫码内容是枪号，增加查询条件
            if (scanContent.length() == 20) {
                //将枪号的"0"替换为"_"
                String evseSn = sn + "_" + scanContent.substring(scanContent.length() - 1);
                queryBuilder.must(QueryBuilders.termsQuery("evseSn", evseSn));
            }
            Iterable<OpLocationEvseElasticDTO> search =
//                    opLocationEvseElastic.search(queryBuilder);
            elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());

            log.info("枪信息查询结果：{}", JSON.toJSONString(search));
            List<OpLocationEvseElasticDTO> evseElasticDTOList = Lists.newArrayList(search);
            //枪号，枪对象Map
            List<String> evseSnList = evseElasticDTOList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(evseSnList)) {
                return Result.ofSucceed(null);
            }
            Set<Long> tariffIdSet = evseElasticDTOList.stream().map(OpLocationEvseElasticDTO::getTariffId).collect(Collectors.toSet());
            List<CostModelRuleDTO> listResult = tariffFeignClient.queryTariffByIds(new ArrayList<>(tariffIdSet)).getData();
            Map<Long, CostModelRuleDTO> tariffIdAndCostModelRuleDTOMap = listResult.stream().collect(Collectors.toMap(CostModelRuleDTO::getId, CostModelRuleDTO -> CostModelRuleDTO));

            Map<String, String> snStatusMap = gunStatus(evseSnList);
            //整合信息
            evseElasticDTOList.forEach(evseDTO -> {
                OpLocationPileEvseLandingPageVO opLocationPileEvseLandingPageVO = new OpLocationPileEvseLandingPageVO();
                String evseSn = evseDTO.getEvseSn();
                Integer gunNo = Integer.valueOf(evseSn.substring(evseSn.length() - 1));
                opLocationPileEvseLandingPageVO.setGunNo(gunNo);
                opLocationPileEvseLandingPageVO.setConnectorDisplayName(commonUtilService.getConnectorDisplayName(sn, String.valueOf(gunNo)));
                opLocationPileEvseLandingPageVO.setPileSn(evseDTO.getPileSn());
                opLocationPileEvseLandingPageVO.setGunType(evseDTO.getGunType());
                opLocationPileEvseLandingPageVO.setGunTypeName(ConnectorGunTypeEnum.getEnumByCode(evseDTO.getGunType()).getName());
                opLocationPileEvseLandingPageVO.setPower(evseDTO.getPower());
                opLocationPileEvseLandingPageVO.setStationId(evseDTO.getLocationId());
                opLocationPileEvseLandingPageVO.setIcon(getGunTypeIconList(evseDTO.getGunType()));
                //枪状态
                if (snStatusMap != null) {
                    opLocationPileEvseLandingPageVO.setState(snStatusMap.get(evseDTO.getEvseSn()));
                    LocationEVSESAPPStatusEnum locationEVSESAPPStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(snStatusMap.get(evseDTO.getEvseSn()));
                    opLocationPileEvseLandingPageVO.setAppEVSEStateCode(locationEVSESAPPStatusEnum.getCode());
                }

                if (usage == 1) {
                    //商桩
                    opLocationPileEvseLandingPageVO.setStatus(EVSETypeEnum.BUSINESS_PILE.getCode());
                } else if (usage == 3) {
                    //家桩共享
                    opLocationPileEvseLandingPageVO.setStatus(EVSETypeEnum.SHARE_PILE.getCode());
                }
                CostModelRuleDTO costModelRuleDTO = tariffIdAndCostModelRuleDTOMap.get(evseDTO.getTariffId());
                opLocationPileEvseLandingPageVO.setCostModelRuleDTO(costModelRuleDTO);
                resultPileEvse.add(opLocationPileEvseLandingPageVO);
            });
        } else {

            //是否被用户绑定
            Result<Boolean> queryBindResult = homePileFeignClient.queryBind(sn);
            if (ObjectUtils.isNotEmpty(queryBindResult) && ObjectUtils.isNotEmpty(queryBindResult.getData()) && !queryBindResult.getData()) {
                throw new MessageCodeException(PileBaseEnum.CHARGE_PILE_PROPERTY_UNKNOWN);
            }

            //是家桩(家桩只有一个枪!!!)
            HomePileMapCardDTO homePileMapCardDTO = new HomePileMapCardDTO();
            homePileMapCardDTO.setSn(sn);
            Result<HomePileVO> homePileMapCardResult = homePileFeignClient.getHomePileMapCard(homePileMapCardDTO);
            if (homePileMapCardResult == null || homePileMapCardResult.getData() == null) {
                return Result.ofSucceed(null);
            }
            HomePileVO homePileVO = homePileMapCardResult.getData();
            List<String> evseSnList = new ArrayList<>();
            evseSnList.add(sn + "_1");
            Map<String, String> snStatusMap = gunStatus(evseSnList);
            OpLocationPileEvseLandingPageVO opLocationPileEvseLandingPageVO = new OpLocationPileEvseLandingPageVO();
            opLocationPileEvseLandingPageVO.setGunNo(1);
            opLocationPileEvseLandingPageVO.setConnectorDisplayName(commonUtilService.getConnectorDisplayName(sn, "1"));
            opLocationPileEvseLandingPageVO.setPileSn(homePileVO.getSn());
            opLocationPileEvseLandingPageVO.setGunType(homePileVO.getGunType());
            opLocationPileEvseLandingPageVO.setGunTypeName(ConnectorGunTypeEnum.getEnumByCode(homePileVO.getGunType()).getName());
            opLocationPileEvseLandingPageVO.setPower(Double.valueOf(homePileVO.getPower()));
            opLocationPileEvseLandingPageVO.setIcon(getGunTypeIconList(homePileVO.getGunType()));
            //枪状态
            if (snStatusMap != null) {
                opLocationPileEvseLandingPageVO.setState(snStatusMap.get(sn + "_1"));
                LocationEVSESAPPStatusEnum locationEVSESAPPStatusEnum = LocationEVSESAPPStatusEnum.realTimeStatus2APPStatus(snStatusMap.get(sn + "_1"));
                opLocationPileEvseLandingPageVO.setAppEVSEStateCode(locationEVSESAPPStatusEnum.getCode());
            }
            opLocationPileEvseLandingPageVO.setStatus(EVSETypeEnum.HOME_PILE.getCode());
            resultPileEvse.add(opLocationPileEvseLandingPageVO);
        }
        log.info("落地页设备信息：{}", JSON.toJSONString(resultPileEvse));
        Collections.sort(resultPileEvse);
        return Result.ofSucceed(resultPileEvse);
    }

    private List<GunTypeIconRespDTO> getGunTypeIconList(Integer gunType) {

        Map<Long, OpImageEntity> imageEntityMap = getImageEntityMap();

        List<OpEvseTypeEntity> evseTypeEntityList = getEvseTypeList().stream().filter(t -> t.getGunType().equals(gunType)).collect(Collectors.toList());

        List<GunTypeIconRespDTO> gunTypeIconList = new ArrayList<>();
        evseTypeEntityList.forEach(a->{
            gunTypeIconList.add( GunTypeIconRespDTO.builder()
                    .type(Convert.toInt(a.getType()))
                    .imageId(a.getImageId())
                    .imageUrl(imageEntityMap.get(a.getImageId()).getUrl()).build());
        });
        return gunTypeIconList;
    }

    private List<OpEvseTypeEntity> getEvseTypeList() {
        List<OpEvseTypeEntity> evseTypeList = (List<OpEvseTypeEntity>) redisTemplate.opsForValue().get(RedisKeyConstant.getStringEvseTypeList());
        return evseTypeList;
    }

    private Map<Long, OpImageEntity> getImageEntityMap() {
        Map<Object, Object> entries = redisTemplate.opsForHash().entries(RedisKeyConstant.getHashImageMap());
        if (!org.springframework.util.CollectionUtils.isEmpty(entries)) {
            Map<Long, OpImageEntity> imageEntityMap = new HashMap<>(entries.size());
            entries.forEach((k, v) -> {
                Long id = Long.parseLong(k.toString());
                OpImageEntity entity = (OpImageEntity) v;
                imageEntityMap.put(id, entity);
            });
            return imageEntityMap;
        }
        return new HashMap<>();
    }

    @Override
    public int getGunCountBySn(String sn) {
        QueryWrapper<OpLocationPileEvseEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("deleted", 0);
        queryWrapper.eq("pile_sn", sn);
        queryWrapper.select("evse_list");
        queryWrapper.last("limit 1");
        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseMapper.selectOne(queryWrapper);
        if(opLocationPileEvseEntity != null && StringUtils.isNotBlank(opLocationPileEvseEntity.getEvseList())){
            List<String> list = JSONArray.parseArray(opLocationPileEvseEntity.getEvseList(), String.class);
            return list.size();
        }
        return 0;
    }

    /**
     * @param setPileEroamingDTO 设置充电桩的互联互通开关 入参模型
     * @return 操作结果
     * @function 设置充电桩的互联互通开关
     */
    @Override
    public boolean setPileEroaming(SetPileEroamingDTO setPileEroamingDTO) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.setPileEroaming setPileEroamingDTO : {}", JSON.toJSONString(setPileEroamingDTO));

        Long currentTimeMillis = System.currentTimeMillis();
        boolean mysqlUpdateMark = false;
        boolean esUpdateMark = false;
        // 根据充电桩序列号查询数据库
        OpLocationPileEvseEntity opLocationPileEvseEntity = this.getPileInfoByPileSn(setPileEroamingDTO.getPileSn());
        if (opLocationPileEvseEntity != null) {
            opLocationPileEvseEntity.setUpdatedAt(currentTimeMillis);
            opLocationPileEvseEntity.setEroamingEnable(setPileEroamingDTO.getPileEroamingOperateType());
            opLocationPileEvseEntity.setPublicMark(setPileEroamingDTO.getPileEroamingOperateType());
            // 先更新数据库
            mysqlUpdateMark = opLocationPileEvseMapper.updateById(opLocationPileEvseEntity) > 0;
            if (mysqlUpdateMark
                    && Integer.valueOf(1).equals(setPileEroamingDTO.getPileEroamingOperateType())
                    && opLocationPileEvseEntity.getLocationId() != null) {
                // 维护这个充电桩所在场站的是否启动OCPI字段
                opLocationRepository.updateLocationOcpiEnalbed(opLocationPileEvseEntity.getLocationId(), true);
            }
        }

        // 根据充电桩序列号查询ES
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = this.findByPileSn(setPileEroamingDTO.getPileSn());
        if (mysqlUpdateMark && opLocationPileEvseElasticDTO != null) {
            opLocationPileEvseElasticDTO.setUpdatedAt(currentTimeMillis);
            opLocationPileEvseElasticDTO.setEroamingEnable(opLocationPileEvseEntity.getEroamingEnable());
            opLocationPileEvseElasticDTO.setPublicMark(opLocationPileEvseEntity.getPublicMark());
            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
            esUpdateMark = true;
        }
        return mysqlUpdateMark && esUpdateMark;
    }

    /**
     * @return
     * @function 查询出所有的开启了互联互通功能的充电桩信息
     */
    @Override
    public List<OpLocationPileEvseElasticDTO> findAllEroamingPile() {
        return opLocationPileEvseElastic.findAllByEroamingEnable(1);
    }

    /**
     * @param locationId
     * @return
     * @function 查询出当前场站下所有的需要开启互联互通属性的充电桩集合
     */
    @Override
    public List<OpLocationPileEvseEntity> findAllNeedToOpenEroamingEnablePileList(Long locationId) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.findAllNeedToOpenEroamingEnablePileList locationId : {}",
                JSON.toJSONString(locationId));

        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpLocationPileEvseEntity::getLocationId, locationId)
                .eq(OpLocationPileEvseEntity::getPublicMark, 1)
                .and(var -> var.isNull(OpLocationPileEvseEntity::getEroamingEnable)
                        .or()
                        .ne(OpLocationPileEvseEntity::getEroamingEnable, 1))
                .eq(OpLocationPileEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationPileEvseEntity::getUpdatedAt, OpLocationPileEvseEntity::getId);
        return opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param opLocationPileEvseEntity 充电桩实体信息
     * @return 同步结果
     * @function 根据MySQL数据库中的数据同步ES数据
     */
    @Override
    public boolean syncPilePublicPropertyInES(OpLocationPileEvseEntity opLocationPileEvseEntity) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.syncPilePublicPropertyInES opLocationPileEvseEntity : {}", JSON.toJSONString(opLocationPileEvseEntity));

        if (opLocationPileEvseEntity == null
                || opLocationPileEvseEntity.getId() == null) {
            return false;
        }

        // 根据充电桩id查询充电桩信息
        Optional<OpLocationPileEvseElasticDTO> byId = opLocationPileEvseElastic.findById(opLocationPileEvseEntity.getId());
        if (byId.isPresent()) {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = byId.get();
            opLocationPileEvseElasticDTO.setPublicMark(opLocationPileEvseEntity.getPublicMark());
            opLocationPileEvseElasticDTO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
            return true;
        }
        return false;
    }

    @Override
    public OpLocationPileEvseEntity findLast(String pileSn) {
        OpLocationPileEvseEntity entity = this.getOne(new LambdaQueryWrapper<>(OpLocationPileEvseEntity.class)
                .eq(OpLocationPileEvseEntity::getPileSn, pileSn)
                .orderByDesc(OpLocationPileEvseEntity::getId)
                .last("limit 1"));
        return entity;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> queryBetaTestPileInfo(List<Long> userIdList) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.queryBetaTestPileInfo userIdList : {}",
                JSON.toJSONString(userIdList));

        return opLocationPileEvseElastic.findByOperatorIdIn(userIdList);
    }

    @Override
    public Boolean updatePileFreeVendInfo(PileFreeVendInfoDTO pileFreeVendInfoDTO) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.updatePileFreeVendInfo pileFreeVendInfoDTO : {}",
                JSON.toJSONString(pileFreeVendInfoDTO));

        boolean mysqlUpdateMark = false;
        boolean esUpdateMark = false;
        // 根据充电桩序列号查询数据库
        OpLocationPileEvseEntity opLocationPileEvseEntity = this.getPileInfoByPileSn(pileFreeVendInfoDTO.getPileSn());
        if (opLocationPileEvseEntity != null) {
            opLocationPileEvseEntity.setFreeVendEnable(pileFreeVendInfoDTO.getFreeVendEnable());
            opLocationPileEvseEntity.setFreeVendIdTag(pileFreeVendInfoDTO.getFreeVendIdTag());
            opLocationPileEvseEntity.setUpdatedAt(pileFreeVendInfoDTO.getUpdateTime());
            mysqlUpdateMark = opLocationPileEvseMapper.updateById(opLocationPileEvseEntity) > 0;
        }

        // 根据充电桩序列号查询ES
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = this.findByPileSn(pileFreeVendInfoDTO.getPileSn());
        if (opLocationPileEvseElasticDTO != null) {
            opLocationPileEvseElasticDTO.setFreeVendEnable(pileFreeVendInfoDTO.getFreeVendEnable());
            opLocationPileEvseElasticDTO.setFreeVendIdTag(pileFreeVendInfoDTO.getFreeVendIdTag());
            opLocationPileEvseElasticDTO.setUpdatedAt(pileFreeVendInfoDTO.getUpdateTime());
            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
            esUpdateMark = true;
        }
        return mysqlUpdateMark && esUpdateMark;
    }

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的充电桩数据
     */
    @Override
    public List<OpLocationPileEvseElasticDTO> getAllPileInfoBySellerId(Long sellerId) {
        return opLocationPileEvseElastic.findByOperatorId(sellerId);
    }

    /**
     * @param pileSnList
     * @return
     * @function 根据充电桩序列号查询充电桩信息
     */
    @Override
    public List<OpLocationPileEvseEntity> findPileInfoInPileSnList(List<String> pileSnList) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.findPileInfoInPileSnList pileSnList : {}", JSON.toJSONString(pileSnList));

        if (ObjectUtils.isNotEmpty(pileSnList)) {
            LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
            lambdaQueryWrapper
                    .in(OpLocationPileEvseEntity::getPileSn, pileSnList)
                    .eq(OpLocationPileEvseEntity::getDeleted, 0)
                    .orderByDesc(OpLocationPileEvseEntity::getUpdatedAt, OpLocationPileEvseEntity::getId);
            return opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
        }
        return null;
    }

    @Override
    public Result<Boolean> updateEsSubscriptionStatusByPileSnList(UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO) {
        if (CollUtil.isEmpty(updateEsSubscriptionStatusDTO.getPileSnList())) {
            return Result.ofSucceed(false);
        }
        List<String> pileSnList = updateEsSubscriptionStatusDTO.getPileSnList();
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElastic.findByPileSnIn(pileSnList);
        log.info("updateEsSubscriptionStatusByPileSnList.opLocationPileEvseElasticDTOList:{}", JSON.toJSONString(opLocationPileEvseElasticDTOList));
        if (opLocationPileEvseElasticDTOList == null) {
            return Result.ofSucceed(false);
        }
        List<Long> evseIdList = new ArrayList<>();
        for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
            List<Long> evseIds = Optional.ofNullable(JSON.parseArray(opLocationPileEvseElasticDTO.getEvseList()))
                    .orElse(JSON.parseArray("[]")).toJavaList(Long.class);
            evseIdList.addAll(evseIds);
        }

        Boolean status = updateEsSubscriptionStatusDTO.getStatus();
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("id", evseIdList));

        Iterable<OpLocationEvseExpandElasticDTO> search =
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseExpandElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        Iterator<OpLocationEvseExpandElasticDTO> it = search.iterator();
        List<OpLocationEvseExpandElasticDTO> result = new ArrayList<>();
        while (it.hasNext()) {
            OpLocationEvseExpandElasticDTO next = it.next();
            next.setSubscriptionCheck(status);
            result.add(next);
        }
        log.info("updateEsSubscriptionStatusByPileSnList.result:{}", JSON.toJSONString(result));
        if (result.size() > 0) {
            opLocationEvseExpandElastic.saveAll(result);
        }
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = opLocationEvseElastic.findAllByPileSnIn(new HashSet<>(pileSnList));
        if (CollUtil.isNotEmpty(opLocationEvseElasticDTOS)) {
            for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOS) {
                opLocationEvseElasticDTO.setSubscriptionCheck(status);
            }
            opLocationEvseElastic.saveAll(opLocationEvseElasticDTOS);
        }

        return Result.ofSucceed(true);
    }

    @Override
    public OpLocationPileEvseElasticDTO getOpLocationPileEvseElasticDTOByPileSnFromES(String pileSn) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.getOpLocationPileEvseElasticDTOByPileSnFromES pileSn : {}",
                JSON.toJSONString(pileSn));

        return StringUtils.isBlank(pileSn) ? null : opLocationPileEvseElastic.findByPileSn(pileSn);
    }

    @Override
    public List<Long> findLocationIds(Long sellerId, List<String> pileSnList) {
        if (CollectionUtils.isEmpty(pileSnList)) {
            log.info("findLocationIds,pileSnList is empty.");
            return null;
        }
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        sourceBuilder.size(0);
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        if (sellerId != null) {
            query.must(QueryBuilders.termQuery("operatorId", sellerId));
        }
        query.must(QueryBuilders.termsQuery("pileSn", pileSnList));
        sourceBuilder.query(query);
        sourceBuilder.aggregation(AggregationBuilders.terms("groupByLocationId").field("locationId").size(pileSnList.size()));
        SearchRequest request = new SearchRequest(OpLocationPileEvseElasticDTO.class.getAnnotation(Document.class).indexName());
        request.source(sourceBuilder);
        Aggregations aggregations = null;
        try {
            aggregations = this.restHighLevelClient.search(request, RequestOptions.DEFAULT).getAggregations();
        } catch (IOException e) {
            log.error("findLocationIds,IOException:", e);
        }
        if (aggregations == null) {
            log.info("findLocationIds,aggregations is null.");
            return null;
        }
        LongTerms groupByLocationId = aggregations.get("groupByLocationId");
        List<LongTerms.Bucket> buckets = groupByLocationId.getBuckets();
        if (CollectionUtils.isEmpty(buckets)) {
            log.info("findLocationIds,buckets is empty.");
            return null;
        }
        List<Long> locationIds = buckets.stream().map(b -> b.getKeyAsNumber().longValue()).collect(Collectors.toList());
        return locationIds;
    }

    /**
     * @param sellerId
     * @param pileSnSearch
     * @return
     * @function 按照商家id和关键字查询充电桩列表
     */
    @Override
    public List<OpLocationPileEvseElasticDTO> findByCondition(Long sellerId, String pileSnSearch) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.findByCondition sellerId : {} and pileSnSearch : {}", JSON.toJSONString(sellerId), JSON.toJSONString(pileSnSearch));

        // 查询条件
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, sellerId));
        if (StrUtil.isNotBlank(pileSnSearch)) {
            // 处理特殊字符
            String keywordString = StringUtil.escapeChar(pileSnSearch);
            boolQueryBuilder.must(QueryBuilders.wildcardQuery(BaseConstant.PILE_SN, "*" + keywordString + "*"));
        }

        log.info("===>>>OpLocationPileEvseRepositoryImpl.findByCondition boolQueryBuilder : {}", JSON.toJSONString(boolQueryBuilder));

        // ES查询语句
        NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
                // .withIndices(OpLocationPileEvseElasticDTO.class.getAnnotation(Document.class).indexName())
                .withIndicesBoost(new IndexBoost(OpLocationPileEvseElasticDTO.class.getAnnotation(Document.class).indexName(), 1.0f))
                .withQuery(boolQueryBuilder)
                .withSort(SortBuilders.fieldSort(BaseConstant.UPDATED_AT).order(SortOrder.DESC))
                .build();
        // return elasticsearchRestTemplate.queryForList(nativeSearchQuery, OpLocationPileEvseElasticDTO.class);
        return elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationPileEvseElasticDTO.class)
                .stream().map(SearchHit::getContent).collect(Collectors.toList());
    }

    @Override
    public Boolean checkExist(String pileSn) {
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(QueryBuilders.boolQuery().must(QueryBuilders.termQuery(BaseConstant.PILE_SN, pileSn)))
                .build();
        long count = elasticsearchRestTemplate.count(searchQuery, OpLocationPileEvseElasticDTO.class);
        if (count > 0L) {
            return true;
        }
        return false;
    }

    /**
     * @return
     * @function 根据充电桩id查询充电桩信息
     */
    @Override
    public OpLocationPileEvseElasticDTO getPileInfoByPileId(Long pileId) {

        log.info("===>>>OpLocationPileEvseRepositoryImpl.getPileInfoByPileId pileId : {}", JSON.toJSONString(pileId));

        if (pileId == null) {
            return null;
        }
        Optional<OpLocationPileEvseElasticDTO> optional = opLocationPileEvseElastic.findById(pileId);
        return optional.orElse(null);
    }

    /**
     * @param opLocationPileEvseElasticDTO
     * @return
     * @function 保存充电桩信息
     */
    @Override
    public boolean updatePileInfo(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO) {
        OpLocationPileEvseElasticDTO dto = opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
        return dto != null;
    }

    private Map<String, String> gunStatus(List<String> evseSnList) {
        //获取枪状态
        Map<String, String> snStatusMap;
        Result<Map<String, String>> snStatusMapResult = monitorFeignClient.queryStatusByEvseSnList(evseSnList);
        if (snStatusMapResult == null || snStatusMapResult.getData() == null) {
            return null;
        }
        log.info("枪状态查询结果：{}", JSON.toJSONString(snStatusMapResult));
        snStatusMap = snStatusMapResult.getData();
        return snStatusMap;
    }


    private Result<List<OpLocationPileEvseLandingPageVO>> verifyGun(String scanContent) {
        return verifyPile(scanContent);
    }

    private static boolean scanSpecialChar(String scanContent, int length) {
        if (length != 11 && length != 14 && length != 18 && length != 25 && length != 20 && length != 27) {
            return true;
        }
        if (scanContent.matches(PARTTERREG) && length != 25 && length != 27) {
            return false;
        }
        return getSpecialPinScan(scanContent, length);
    }

    private static boolean getSpecialPinScan(String scanContent, int length) {
        log.info("scanContent:{}", scanContent);
        if (length == 25 && scanContent.contains("|")) {
            String[] pins = scanContent.split("\\|");
            if (pins[0].length() != 18 || !pins[0].matches(PARTTERREG) || pins[1].length() != 6 || !pins[1].matches(PARTTERREG)) {
                return true;
            }
            return false;
        } else if (length == 27 && scanContent.contains("|")) {
            String[] pins = scanContent.split("\\|");
            if (pins[0].length() != 18 || !pins[0].matches(PARTTERREG) || pins[1].length() != 8 || !pins[1].matches(PARTTERREG)) {
                return true;
            }
            return false;
        }
        return true;
    }

    @Override
    public List<OpLocationPileEvseEntity> selectPlieList() {
        List<OpLocationPileEvseEntity> opLocationPileEvseEntities = opLocationPileEvseMapper
                .selectList(new LambdaQueryWrapper<OpLocationPileEvseEntity>()
                        .eq(OpLocationPileEvseEntity::getDeleted, 0));
        return opLocationPileEvseEntities;
    }

    @Override
    public List<OpLocationEvseElasticDTO> listEvse(OpLocationLiveEvseViewDTO opLocationLiveEvseViewDTO) {
        Long locationId = opLocationLiveEvseViewDTO.getLocationId();
        Integer evseState = opLocationLiveEvseViewDTO.getEvseState();
        String keywords = opLocationLiveEvseViewDTO.getKeywords();

        BoolQueryBuilder queryBuilder = getEvseQueryBuilder(Arrays.asList(locationId), evseState, keywords);
        //查询对象
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(
                        SortBuilders.fieldSort("pileSn").order(SortOrder.ASC),
                        SortBuilders.fieldSort("evseSn").order(SortOrder.ASC)
                ).build();
        SearchHits<OpLocationEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        return searchHits.stream()
                .map(SearchHit::getContent)
                .collect(Collectors.toList());
    }

    @Override
    public List<OpLocationEvseElasticDTO> listEvse(OpLocationEvseStateCountDTO opLocationEvseStateCountDTO) {
        List<Long> locationIds = opLocationEvseStateCountDTO.getLocationIds();
        String keywords = opLocationEvseStateCountDTO.getKeywords();

        BoolQueryBuilder queryBuilder = getEvseQueryBuilder(locationIds, null, keywords);
        //查询对象
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder).build();
        SearchHits<OpLocationEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        return searchHits.stream()
                .map(SearchHit::getContent)
                .collect(Collectors.toList());
    }

    @Override
    public PageVO<OpLocationEvseElasticDTO> listEvse(OpLocationLiveEvsePageDTO opLocationLiveEvsePageDTO) {
        List<Long> locationIds = opLocationLiveEvsePageDTO.getLocationIds();
        Integer evseState = opLocationLiveEvsePageDTO.getEvseState();
        String keywords = opLocationLiveEvsePageDTO.getKeywords();
        Integer page = opLocationLiveEvsePageDTO.getPage();
        Integer pageSize = opLocationLiveEvsePageDTO.getPageSize();
        BoolQueryBuilder queryBuilder = getEvseQueryBuilder(locationIds, evseState, keywords);
        //查询对象
        //按桩名称或最新订单启动时间排序
        List<SortBuilder<?>> sortList = new ArrayList<>();
        String orderBy = opLocationLiveEvsePageDTO.getOrderBy();
        String orderType = opLocationLiveEvsePageDTO.getOrderType();
        if (StringUtils.isNotEmpty(orderBy) && !"lastOrderSeq".equals(orderBy) && !"pileName".equals(orderBy) && !"createdAt".equals(orderBy)) {
            throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
        }
        if (StringUtils.isEmpty(orderBy)) {
            orderBy = "createdAt";
        }
        if (StringUtils.isEmpty(orderType)) {
            orderType = "asc";
        }
        //默认添加时间排序
        sortList.add(SortBuilders.fieldSort(orderBy).order(SortOrder.fromString(orderType)));
        sortList.add(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(sortList)
                .withPageable(PageRequest.of(page-1, pageSize)).build();
        log.info("list evse OpLocationLiveEvsePageDTO: query={}", searchQuery.getQuery().toString());
        SearchHits<OpLocationEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        PageVO<OpLocationEvseElasticDTO> pageVO = new PageVO<>();
        pageVO.setPage(opLocationLiveEvsePageDTO.getPage());
        pageVO.setPageSize(opLocationLiveEvsePageDTO.getPageSize());
        pageVO.setContent(searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList()));
        // 获取搜索结果的总命中数
        long totalHits = searchHits.getTotalHits();
        // 计算总页数
        int totalPages = (int) Math.ceil((double) totalHits / pageSize);
        pageVO.setTotalRows(totalHits);
        pageVO.setTotalPages(totalPages);
        return pageVO;
    }

    @Override
    public Page<PileQrCodeDTO> getPileQrCodePage(String keyWord, String locationId, int page, int pageSize) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        if (StringUtils.isNotBlank(locationId)){
            queryBuilder.must(QueryBuilders.termQuery(BaseConstant.LOCATIONID, locationId));
        }
        queryBuilder.must(QueryBuilders.termQuery(BaseConstant.OPERATOR_ID, LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        //搜索值
        if (StringUtils.isNotBlank(keyWord)) {
            BoolQueryBuilder keywordsQueryBuilder = QueryBuilders.boolQuery();
            keywordsQueryBuilder.should(QueryBuilders.wildcardQuery("name", String.format("%s%s%s", "*", keyWord, "*")));
            keywordsQueryBuilder.should(QueryBuilders.wildcardQuery(BaseConstant.PILESN, String.format("%s%s%s", "*", keyWord, "*")));
            queryBuilder.must(keywordsQueryBuilder);
        }
        //查询对象
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(
                        SortBuilders.fieldSort("pileSn").order(SortOrder.ASC)
                ).withPageable(PageRequest.of(page-1, pageSize)).build();
        log.info("list evse OpLocationLiveEvsePageDTO: query={}", searchQuery.getQuery().toString());
        SearchHits<OpLocationPileEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(searchQuery, OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOS = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<PileQrCodeDTO> pileQrCodeDTOs = new ArrayList<>();
        opLocationPileEvseElasticDTOS.forEach(opLocationPileEvseElasticDTO -> {
            PileQrCodeDTO pileQrCodeDTO = new PileQrCodeDTO();
            pileQrCodeDTO.setIdentificationCode(String.valueOf(opLocationPileEvseElasticDTO.getId()));
            pileQrCodeDTO.setLocationName(opLocationPileEvseElasticDTO.getLocationName());
            pileQrCodeDTO.setName(opLocationPileEvseElasticDTO.getName());
            pileQrCodeDTO.setPileSn(opLocationPileEvseElasticDTO.getPileSn());
            pileQrCodeDTO.setRemoteStartPhoneNumber(remoteStartPhoneNumber);
            //枪类型列表
            final String evseIdListString = opLocationPileEvseElasticDTO.getEvseList();
            if (StringUtils.isBlank(evseIdListString)) {
                return;
            }
            TypeReference<List<Long>> typeReference = new TypeReference<List<Long>>(){};
            final List<Long> evseIdList = JSON.parseObject(evseIdListString, typeReference);
            final List<OpLocationEvseElasticDTO> evseList = opLocationEvseElastic.findAllByIdInOrderByEvseSn(evseIdList);
            List<Integer> gunTypeCode = evseList.stream().map(OpLocationEvseElasticDTO::getGunType).collect(Collectors.toList());
            pileQrCodeDTO.setGunTypeCode(gunTypeCode);
            pileQrCodeDTOs.add(pileQrCodeDTO);
        });
        Page<PileQrCodeDTO> qrCodeDTOPage = new Page<>();
        qrCodeDTOPage.setCurrent(page);
        qrCodeDTOPage.setSize(pageSize);
        qrCodeDTOPage.setRecords(pileQrCodeDTOs);
        // 获取搜索结果的总命中数
        long totalHits = searchHits.getTotalHits();
        // 计算总页数
        int totalPages = (int) Math.ceil((double) totalHits / pageSize);
        qrCodeDTOPage.setTotal(totalHits);
        qrCodeDTOPage.setPages(totalPages);
        return qrCodeDTOPage;
    }

    /**
     * 根据场站/状态/关键词（桩SN/桩名称）构建搜索语句
     * @param locationIds
     * @param evseState
     * @param keywords
     * @return
     */
    private BoolQueryBuilder getEvseQueryBuilder(List<Long> locationIds, Integer evseState, String keywords) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        if (CollectionUtils.isNotEmpty(locationIds)) {
            queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));
        }
        queryBuilder.must(QueryBuilders.termQuery("operatorId", LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        if (Objects.nonNull(evseState)) {
            if (LocationEvseStatusV2Enum.AVAILABLE.getCode().equals(evseState)) {
                List<String> list = new ArrayList<>();
                for (LocationEvseStatusV2Enum value : LocationEvseStatusV2Enum.values()) {
                    if (LocationEvseStatusV2Enum.AVAILABLE.getCode().equals(value.getCode())) {
                        continue;
                    }
                    list.addAll(value.getRelatedOccpStatus());
                }
                queryBuilder.mustNot(QueryBuilders.termsQuery("state", list));
            } else {
                LocationEvseStatusV2Enum locationEvseStatusV2Enum = LocationEvseStatusV2Enum.getLocationEvseStatusV2Enum(evseState);
                if (locationEvseStatusV2Enum != null) {
                    queryBuilder.must(QueryBuilders.termsQuery("state", locationEvseStatusV2Enum.getRelatedOccpStatus()));
                }
            }
        }

        //搜索值
        if (StringUtils.isNotBlank(keywords)) {
            BoolQueryBuilder keywordsQueryBuilder = QueryBuilders.boolQuery();
            //特殊字符转义
            keywords = QueryParserBase.escape(keywords);
            keywordsQueryBuilder.should(QueryBuilders.wildcardQuery("pileName", String.format("%s%s%s", "*", keywords, "*")));
            keywordsQueryBuilder.should(QueryBuilders.wildcardQuery(BaseConstant.PILESN, String.format("%s%s%s", "*", keywords, "*")));
            queryBuilder.must(keywordsQueryBuilder);
        }
        return queryBuilder;
    }

    public Integer repairRedisOfOperatorIdByPileSNByPage(int pageSize, long page) {
        int repairedCount = 0;
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        // Scroll查询请求
        //滚动查询的Scroll
        Scroll scroll = new Scroll(TimeValue.timeValueMillis(180000));
        //构建searchRequest
        SearchRequest request = new SearchRequest(OpLocationPileEvseElasticDTO.class.getAnnotation(Document.class).indexName());
        SearchSourceBuilder sourceBuilder = new SearchSourceBuilder();
        //构造器加入需要查找的字段,query,每次滚动大小
        sourceBuilder.fetchSource(new String[]{"pileSn", "operatorId"}, null)// 把超级长的字段去掉 列表接口用不到
//                fetchSource(true)
                .query(queryBuilder)
                .size(pageSize);
        //加入scroll和构造器
        request.scroll(scroll).source(sourceBuilder);
        //存储scroll的list
        List<String> scrollIdList = new ArrayList<>();

        //滚动查询返回的结果
        SearchResponse searchScrollResponse;
        SearchScrollRequest searchScrollRequest = new SearchScrollRequest();
        try {
            searchScrollResponse = restHighLevelClient.search(request, RequestOptions.DEFAULT);
            // 获取查询总数量
            long totalCount = searchScrollResponse.getHits().getTotalHits().value;
            log.info("查询总命中数 in repairRedisOfOperatorIdByPileSN：{}", totalCount);
            //拿到第一个ScrollId（游标）
            String scrollId = searchScrollResponse.getScrollId();
            //保存scrollId
            scrollIdList.add(scrollId);
            // 查询总数量为0，直接返回
            if (totalCount == 0) {
                return 0;
            }
            // 计算总页数
            long pageCount = totalCount % pageSize == 0L ? totalCount / pageSize : totalCount / pageSize + 1L;
            log.info("pageCount={}", pageCount);
            // 重新设置当前页数
            if (page > pageCount) {
                page = pageCount;
                log.info("page={}", page);
            }
            searchScrollRequest.scroll(scroll);
            // 当前页大于1时继续滚动查询
            for (int i = 2; i <= page; i++) {
                //继续滚动，根据上一个游标，得到这次开始查询位置
                searchScrollRequest.scrollId(scrollId);
                searchScrollResponse = restHighLevelClient.scroll(searchScrollRequest, RequestOptions.DEFAULT);
                //定位游标
                scrollId = searchScrollResponse.getScrollId();
                scrollIdList.add(scrollId);
            }
            org.elasticsearch.search.SearchHits hits = searchScrollResponse.getHits();
            for (org.elasticsearch.search.SearchHit searchHit : hits) {
                OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = JSON.parseObject(searchHit.getSourceAsString(), OpLocationPileEvseElasticDTO.class);
                if (Objects.nonNull(opLocationPileEvseElasticDTO) && StrUtil.isNotBlank(opLocationPileEvseElasticDTO.getPileSn()) && null != opLocationPileEvseElasticDTO.getOperatorId()) {
                    // 缓存桩运营商用户
                    String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(opLocationPileEvseElasticDTO.getPileSn());
                    stringRedisTemplate.opsForValue().set(snOperatorIdKey, opLocationPileEvseElasticDTO.getOperatorId().toString());
                    repairedCount++;
                }
            }
        } catch (IOException e) {
            log.error("scrollPageFailed {}", searchScrollRequest.toString());
            log.error("scrollPageFailed", e);
        } finally {
            final long start = System.currentTimeMillis();
            log.info("清理滚动请求开始:{}", start);
            if (!org.springframework.util.CollectionUtils.isEmpty(scrollIdList)) {
                //清理scroll,释放资源
                ClearScrollRequest clearScrollRequest = new ClearScrollRequest();
                clearScrollRequest.setScrollIds(scrollIdList);
                try {
                    restHighLevelClient.clearScroll(clearScrollRequest, RequestOptions.DEFAULT);
                } catch (IOException e) {
                    log.error("clearScroll in method ScrollPage(KeywordPeriodSearchDTO  cdrKeywordDto) failed", e);
                }
                final long end = System.currentTimeMillis();
                long total = (end - start) / 1000 / 60;
                log.info("清理滚动请求结束:{},总耗时:{}s", end, total);
            }
        }
        return repairedCount;
    }

    @Override
    public Integer repairRedisOfOperatorIdByPileSN() {
        Query searchQuery = new NativeSearchQueryBuilder()
                .withQuery(matchAllQuery())  // 匹配所有文档
                .build();
        long totalCount = elasticsearchRestTemplate.count(searchQuery, OpLocationPileEvseElasticDTO.class);
        log.info("========== the totalCount in repairRedisOfOperatorIdByPileSN: {}", totalCount);
        if (totalCount <= 0) {
            return 0;
        }
        int pageSize = 100;
        long page = 1L;
        // 计算总页数
        long pageCount = totalCount % pageSize == 0L ? totalCount / pageSize : totalCount / pageSize + 1L;
        int repairedCount = 0;
        while (page <= pageCount) {
            log.info("========== loop in repairRedisOfOperatorIdByPileSN times: {}", page);
            repairedCount += repairRedisOfOperatorIdByPileSNByPage(pageSize, page);
            page++;
        }
        return repairedCount;
    }
    @Override
    public Map<String, List<CommonVO>> getPileLocationMap() {
        QueryBuilder queryBuilder = QueryBuilders.matchAllQuery();
        String[] includes = {"id","pileSn","name","locationId"};
        FetchSourceFilter fetchSourceFilter = new FetchSourceFilter(includes,null);
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withTrackTotalHits(true)
                .withSourceFilter(fetchSourceFilter).build();
        Map<Long, List<OpLocationPileEvseElasticDTO>> pileMap = elasticsearchRestTemplate.search(searchQuery, OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).sorted(Comparator.comparing(OpLocationPileEvseElasticDTO::getId).reversed()).filter(ListSortUtil.distinctByKey(OpLocationPileEvseElasticDTO::getPileSn)).collect(Collectors.groupingBy(OpLocationPileEvseElasticDTO::getLocationId));
        if (com.baomidou.mybatisplus.core.toolkit.CollectionUtils.isEmpty(pileMap)) {
            return new HashMap<>();
        }
        Map<String, List<CommonVO>> resultMap = new HashMap<>();
        pileMap.forEach((k,v)->{
            List<CommonVO> commonVOS = new ArrayList<>();
            for (OpLocationPileEvseElasticDTO elasticDTO : v) {
                CommonVO vo = new CommonVO();
                vo.setCode(elasticDTO.getPileSn());
                vo.setName(elasticDTO.getName());
                commonVOS.add(vo);
            }
            resultMap.put(k.toString(),commonVOS);
        });
        return resultMap;
    }
}
