package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.util.StrUtil;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.constant.PileChargingRights;
import com.autel.cloud.pile.base.domain.convert.OpLocationConnectorConvert;
import com.autel.cloud.pile.base.domain.convert.OpLocationEvseConvert;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileEvseConvert;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.SubscribePileRightsService;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.app.GunListDto;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.dto.pile.CheckPileNameDTO;
import com.autel.cloud.pile.base.dto.pile.EvscpSettingDTO;
import com.autel.cloud.pile.base.dto.rabbitTemplateDTO.EvseInfoModifyDTO;
import com.autel.cloud.pile.base.dto.subscribe.BenefitFunctionReqDto;
import com.autel.cloud.pile.base.dto.subscribe.PileBenefitFunctionRespDto;
import com.autel.cloud.pile.base.enums.AmericaPileProductModelEnum;
import com.autel.cloud.pile.base.enums.BrandEnum;
import com.autel.cloud.pile.base.enums.LocationEVSEStatusEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.amqp.MQSender;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.*;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseExpandElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.*;
import com.autel.cloud.pile.base.infrastructure.feign.dto.ChargePileDTO;
import com.autel.cloud.pile.base.infrastructure.feign.dto.VerifyDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.app.AppGunStateEnum;
import com.autel.cloud.pile.base.vo.app.GunListPageDto;
import com.autel.cloud.pile.base.vo.app.PageDTO;
import com.autel.cloud.pile.bill.dto.TimezoneUserDTO;
import com.autel.cloud.pile.bill.enums.DeviceTypeEnum;
import com.autel.cloud.pile.bill.vo.UserChargePileVO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.SyncUserLocationVO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.autel.cloud.tariff.dto.CostRuleWeeksDTO;
import com.autel.cloud.tariff.dto.CostRulesDTO;
import com.autel.cloud.tariff.dto.TariffRuleOfPileDTO;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpStatus;
import org.apache.http.util.Asserts;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.RestHighLevelClient;
import org.elasticsearch.index.query.*;
import org.elasticsearch.index.reindex.BulkByScrollResponse;
import org.elasticsearch.index.reindex.UpdateByQueryRequest;
import org.elasticsearch.script.Script;
import org.elasticsearch.script.ScriptType;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.context.config.annotation.RefreshScope;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.IndexBoost;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StopWatch;

import javax.annotation.Resource;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.concurrent.locks.LockSupport;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * <p>
 * 充电设备 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Service
@Slf4j
@RefreshScope
public class OpLocationEvseRepositoryImpl extends ServiceImpl<OpLocationEvseMapper, OpLocationEvseEntity> implements OpLocationEvseRepository {

    private final MonitorFeign monitorFeign;

    private final OpLocationEvseElastic opLocationEvseElastic;

    private final OpEvseChargingThresholdMapper opEvseChargingThresholdMapper;

    private final OpEvseDirectionsMapper opEvseDirectionsMapper;

    private final OpEvseImageMapper opEvseImageMapper;

    private final OpEvseSocThresholdMapper opEvseSocThresholdMapper;

    private final OpLocationConnectorMapper opLocationConnectorMapper;

    private final OpLocationMapper opLocationMapper;

    private final OpLocationOperationMapper opLocationOperationMapper;

    private final OpLocationPileEvseMapper opLocationPileEvseMapper;

    @Autowired
    private DeviceServiceFeign deviceServiceFeign;
    @Autowired
    private TariffFeignClient tariffFeignClient;
    @Autowired
    private OpLocationEvseMapper opLocationEvseMapper;
    @Autowired
    private OpEvseBrandModelMapper opEvseBrandModelMapper;
    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Autowired
    private OpLocationEvseExpandElastic opLocationEvseExpandElastic;
    @Autowired
    private OpLocationElastic opLocationElastic;
    @Autowired
    private RestHighLevelClient restHighLevelClient;

    @Autowired
    private DataServiceFeign dataServiceFeign;

    @Autowired
    private PileUserFeign pileUserFeign;

    @Autowired
    private MQSender mqSender;

    @Resource
    private OicpFeignClient oicpFeignClient;
    @Resource
    private HomePileFeignClient homePileClient;
    @Resource
    private RabbitTemplate rabbitTemplate;

    @Autowired
    private OpCostRuleDistributeMapper opCostRuleDistributeMapper;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Resource
    private OpLocationEvseExpandElasticService opLocationEvseExpandElasticService;
    @Resource
    private RedisUtil redisUtil;
    @Resource
    private TariffAPPFeign tariffAPPFeign;

    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;

    @Resource
    private SubscribePileRightsService subscribePileRightsService;
    @Resource
    private OpLocationRepository opLocationRepository;

    @Value("${pile.subscribe.enable:false}")
    private boolean subscribeEnable;

    public OpLocationEvseRepositoryImpl(MonitorFeign monitorFeign,
                                        OpLocationEvseElastic opLocationEvseElastic,
                                        OpEvseChargingThresholdMapper opEvseChargingThresholdMapper,
                                        OpEvseDirectionsMapper opEvseDirectionsMapper,
                                        OpEvseImageMapper opEvseImageMapper,
                                        OpEvseSocThresholdMapper opEvseSocThresholdMapper,
                                        OpLocationConnectorMapper opLocationConnectorMapper,
                                        OpLocationMapper opLocationMapper,
                                        OpLocationOperationMapper opLocationOperationMapper,
                                        OpLocationPileEvseMapper opLocationPileEvseMapper) {
        this.monitorFeign = monitorFeign;
        this.opLocationEvseElastic = opLocationEvseElastic;
        this.opEvseChargingThresholdMapper = opEvseChargingThresholdMapper;
        this.opEvseDirectionsMapper = opEvseDirectionsMapper;
        this.opEvseImageMapper = opEvseImageMapper;
        this.opEvseSocThresholdMapper = opEvseSocThresholdMapper;
        this.opLocationConnectorMapper = opLocationConnectorMapper;
        this.opLocationMapper = opLocationMapper;
        this.opLocationOperationMapper = opLocationOperationMapper;
        this.opLocationPileEvseMapper = opLocationPileEvseMapper;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean delete(Long id) {
        this.getBaseMapper().deleteByEvseId(id);
        opEvseChargingThresholdMapper.deleteByEvseId(id);
        opEvseSocThresholdMapper.deleteByEvseId(id);
        opLocationConnectorMapper.deleteByLocationEvseId(id);
        opEvseImageMapper.deleteByLocationEvseId(id);
        opEvseDirectionsMapper.deleteByLocationEvseId(id);
        // 物理刪除es中的数据
        opLocationEvseElastic.deleteById(id);
        return Boolean.TRUE;
    }

    @Override
    @Deprecated
    @Transactional(rollbackFor = Exception.class)
    public Boolean associatedBillingRules(OpLocationEvseEntity opLocationEvseEntity, OpLocationPileEvseDTO opLocationPileEvseDTO) {
        boolean ret = this.saveOrUpdate(opLocationEvseEntity);

        Optional<OpLocationEvseElasticDTO> optional = opLocationEvseElastic.findById(opLocationEvseEntity.getId());
        log.info("============= update tariff of OpLocationEvseElasticDTO in the ES begin =======================");
        if (optional.isPresent()) {
            OpLocationEvseElasticDTO evseElasticDTO = optional.get();
            evseElasticDTO.setTariffId(opLocationEvseEntity.getTariffId());
            evseElasticDTO.setUpdatedAt(opLocationEvseEntity.getUpdatedAt());
            opLocationEvseElastic.save(evseElasticDTO);
            log.info("============= update tariff of OpLocationEvseElasticDTO in the ES end =======================");
        }

        Optional<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOOptional = opLocationPileEvseElastic
                .findById(opLocationPileEvseDTO.getId());
        log.info("============= update tariff of opLocationPileEvseElasticDTOOptional in the ES begin =======================");
        if (opLocationPileEvseElasticDTOOptional.isPresent()) {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElasticDTOOptional.get();
            opLocationPileEvseElasticDTO.setTariffId(opLocationEvseEntity.getTariffId());
            opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
            log.info("============= update tariff of opLocationPileEvseElasticDTOOptional in the ES end =======================");
        }
        return ret;
    }

    @Override
    public OpEvseInfoDTO getEvseByEvseSn(String evseSn) {
        // 先从es查询
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.filter(QueryBuilders.termsQuery("evseSn", evseSn));
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpEvseInfoDTO> opEvseInfoDTOList = new LinkedList<>();
        iterable.forEach(elasticDTO -> {
            log.info("OpLocationEvseRepositoryImpl.getEvseByEvseSn elasticDTO = " + JSON.toJSONString(elasticDTO));
            OpEvseInfoDTO opEvseInfoDTO = OpLocationEvseConvert.toOpEvseInfoDTO(elasticDTO);
            log.info("OpLocationEvseRepositoryImpl.getEvseByEvseSn opEvseInfoDTO = " + JSON.toJSONString(opEvseInfoDTO));
            opEvseInfoDTOList.add(opEvseInfoDTO);
        });
        if (!opEvseInfoDTOList.isEmpty()) {
            OpEvseInfoDTO opEvseInfoDTO = opEvseInfoDTOList.get(0);
            Long locationEvseId = opEvseInfoDTO.getId();
            List<OpLocationConnectorDTO> op = opLocationConnectorMapper.selectByLocationEvseSn(locationEvseId);
            opEvseInfoDTO.setOpLocationConnectorDTOList(op);
            return opEvseInfoDTO;
        }
        return null;
    }

    @Override
    public List<OpLocationEvseRealTimeDTO> getEvseByLocationId(Long locationId) {
        // 先从es查询
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        QueryBuilder locationQueryBuild = QueryBuilders.termQuery(BaseConstant.LOCATIONID, locationId);
        boolQueryBuilder.must(locationQueryBuild);
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(boolQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationEvseRealTimeDTO> opLocationEvseDTOList = new ArrayList<>();
        iterable.forEach(elasticDTO -> {
            OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = OpLocationEvseConvert.toOpLocationEvseRealTimeDTO(elasticDTO);
            opLocationEvseRealTimeDTO.setState(monitorFeign.getEvseStatus(elasticDTO.getId()).getName());
            Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(elasticDTO.getId());
            OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);
            opLocationEvseDTOList.add(opLocationEvseRealTimeDTO);
        });
        if (!CollectionUtils.isEmpty(opLocationEvseDTOList)) {
            return opLocationEvseDTOList;
        }
        // es查询不到，从mysql查询
        LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE).eq(OpLocationEvseEntity::getLocationId, locationId);
        List<OpLocationEvseEntity> evseEntityList = this.list(queryWrapper);
        return evseEntityList.stream().map(entity -> {
            OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = OpLocationEvseConvert.toOpLocationEvseRealTimeDTO(entity);
            opLocationEvseRealTimeDTO.setState(monitorFeign.getEvseStatus(entity.getId()).getName());
            Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(entity.getId());
            OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);
            return opLocationEvseRealTimeDTO;
        }).collect(Collectors.toList());
    }

    @Override
    public Map<Long, List<OpLocationEvseRealTimeDTO>> getEvseByLocationIds(Set<Long> locationIds) {
        if (org.apache.commons.collections4.CollectionUtils.isEmpty(locationIds)) {
            return new HashMap<>();
        }

        //结果集
        Map<Long, List<OpLocationEvseRealTimeDTO>> stationIdStationEVSEListMap = new HashMap<>();

        //查询es
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = opLocationEvseElastic.findAllByLocationIdIn(locationIds);

        if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationEvseElasticDTOS)) {
            //构建每个站点的设备集合map
            Map<Long, List<OpLocationEvseElasticDTO>> esStationIdEVSEListMap = buildESStationIdEVSEListMap(opLocationEvseElasticDTOS);
            //封装每个站点的设备集合
            locationIds.forEach(stationId -> {
                //这个站点的所有设备
                List<OpLocationEvseElasticDTO> esStationEVSEList = esStationIdEVSEListMap.get(stationId);
                if (esStationEVSEList != null) {
                    //封装这个站点的所有设备
                    List<OpLocationEvseRealTimeDTO> opLocationEvseDTOList = new ArrayList<>();
                    esStationEVSEList.forEach(opLocationEvseElasticDTO -> {
                        OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = OpLocationEvseConvert.toOpLocationEvseRealTimeDTO(opLocationEvseElasticDTO);
                        opLocationEvseRealTimeDTO.setState(monitorFeign.getEvseStatus(opLocationEvseElasticDTO.getId()).getName());
                        Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(opLocationEvseElasticDTO.getId());
                        OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);
                        opLocationEvseDTOList.add(opLocationEvseRealTimeDTO);
                    });
                    if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(opLocationEvseDTOList)) {
                        stationIdStationEVSEListMap.put(stationId, opLocationEvseDTOList);
                    }
                }
            });
        }
        return stationIdStationEVSEListMap;
    }

    @Override
    public OcppLocationEVSEVO getLocationEvseVOBySnAndGunNo(String evseSn) {
        return opLocationConnectorMapper.getLocationEvseVOBySnAndGunNo(evseSn);
    }

    @Override
    public OcppLocationEVSEVO getLocationByPileSn(String pileSn) {
        return opLocationConnectorMapper.getLocationByPileSn(pileSn);
    }

    @Override
    public List<OplocationConnectorScanDTO> getLocationConnectorListByPileSn(String pileSn) {
        List<OplocationConnectorScanDTO> oplocationConnectorList = null;
        LambdaQueryWrapper<OpLocationPileEvseEntity> opLocationPileEvseEntityQueryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                .eq(OpLocationPileEvseEntity::getPileSn, pileSn)
                .eq(OpLocationPileEvseEntity::getDeleted, 0);
        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseMapper.selectOne(opLocationPileEvseEntityQueryWrapper);
        Asserts.notNull(opLocationPileEvseEntity, HttpCodeEnum.NOT_FOUND.getMessage());
        if (StringUtils.isBlank(opLocationPileEvseEntity.getEvseList())) {
            return Collections.emptyList();
        }
        List<Long> evseIdList = JSON.parseArray(opLocationPileEvseEntity.getEvseList(), Long.class);
        LambdaQueryWrapper<OpLocationEvseEntity> evseQueryWrapper = Wrappers.lambdaQuery(OpLocationEvseEntity.class).in(OpLocationEvseEntity::getId, evseIdList)
                .eq(OpLocationEvseEntity::getDeleted, 0);
        List<OpLocationEvseEntity> opLocationEvseList = this.baseMapper.selectList(evseQueryWrapper);
        if (org.apache.commons.collections.CollectionUtils.isEmpty(opLocationEvseList)) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<OpLocationConnectorEntity> connectorQueryWrapper = Wrappers.lambdaQuery(OpLocationConnectorEntity.class).in(OpLocationConnectorEntity::getLocationEvseId, evseIdList)
                .eq(OpLocationConnectorEntity::getDeleted, 0);
        List<OpLocationConnectorEntity> opLocationConnetorEntityList = opLocationConnectorMapper.selectList(connectorQueryWrapper);
        if (org.apache.commons.collections.CollectionUtils.isEmpty(opLocationConnetorEntityList)) {
            return Collections.emptyList();
        }
        Map<Long, String> evseMap = opLocationEvseList.stream().collect(Collectors.toMap(OpLocationEvseEntity::getId, OpLocationEvseEntity::getEvseSn));
        Map<Long, Integer> connectorMap = opLocationConnetorEntityList.stream().collect(Collectors.toMap(OpLocationConnectorEntity::getLocationEvseId, OpLocationConnectorEntity::getGunType));
        oplocationConnectorList = Lists.newArrayList();
        for (Map.Entry<Long, String> evseEntry : evseMap.entrySet()) {
            Long evseId = evseEntry.getKey();
            String evseSn = evseEntry.getValue();
            String[] veses = evseSn.split("_");
            Integer connectorNum = Integer.parseInt(veses[1]);
            String connectorSn = veses[0];
            Integer connectorType = connectorMap.get(evseId);
            if (null == connectorType) {
                continue;
            }
            OplocationConnectorScanDTO connectorScanDTO = new OplocationConnectorScanDTO();
            connectorScanDTO.setConnectorNumber(connectorNum);
            connectorScanDTO.setConnectorType(connectorType);
            connectorScanDTO.setPileSn(connectorSn);
            oplocationConnectorList.add(connectorScanDTO);
        }
        return oplocationConnectorList;
    }

    @Override
    public OpLocationEvseDTO details(Long id) {
        OpLocationEvseEntity opLocationEvseEntity = this.baseMapper.selectById(id);
        OpLocationEvseDTO opLocationEvseDTO = null;
        if (opLocationEvseEntity != null) {
            opLocationEvseDTO = OpLocationEvseConvert.toOpLocationEvseDTO(opLocationEvseEntity);
        }

        LambdaQueryWrapper<OpEvseChargingThresholdEntity> queryWrapper = Wrappers.lambdaQuery(OpEvseChargingThresholdEntity.class)
                .eq(OpEvseChargingThresholdEntity::getDeleted, Boolean.FALSE).eq(OpEvseChargingThresholdEntity::getEvseId, id);
        List<OpEvseChargingThresholdEntity> evseEntityList = opEvseChargingThresholdMapper.selectList(queryWrapper);
        if (!evseEntityList.isEmpty() && opLocationEvseDTO != null) {
            OpEvseChargingThresholdEntity opEvseChargingThresholdEntity = evseEntityList.get(0);
            OpEvseChargingThresholdDTO opEvseChargingThresholdDTO = OpLocationEvseConvert
                    .toOpEvseChargingThresholdDTO(opEvseChargingThresholdEntity);
            opLocationEvseDTO.setOpEvseChargingThresholdDTO(opEvseChargingThresholdDTO);
        }

        LambdaQueryWrapper<OpEvseSocThresholdEntity> queryWrapper1 = Wrappers.lambdaQuery(OpEvseSocThresholdEntity.class)
                .eq(OpEvseSocThresholdEntity::getDeleted, Boolean.FALSE).eq(OpEvseSocThresholdEntity::getEvseId, id);
        List<OpEvseSocThresholdEntity> opEvseSocThresholdEntityList = opEvseSocThresholdMapper.selectList(queryWrapper1);
        if (!opEvseSocThresholdEntityList.isEmpty() && opLocationEvseDTO != null) {
            OpEvseSocThresholdEntity opEvseSocThresholdEntity = opEvseSocThresholdEntityList.get(0);
            OpEvseSocThresholdDTO opEvseSocThresholdDTO = OpLocationEvseConvert.toOpEvseSocThresholdDTO(opEvseSocThresholdEntity);
            opLocationEvseDTO.setOpEvseSocThresholdDTO(opEvseSocThresholdDTO);
        }

        LambdaQueryWrapper<OpEvseDirectionsEntity> queryWrapper2 = Wrappers.lambdaQuery(OpEvseDirectionsEntity.class)
                .eq(OpEvseDirectionsEntity::getDeleted, Boolean.FALSE).eq(OpEvseDirectionsEntity::getLocationEvseId, id);
        List<OpEvseDirectionsEntity> opEvseDirectionsEntityList = opEvseDirectionsMapper.selectList(queryWrapper2);
        List<OpEvseDirectionsDTO> opEvseDirectionsDTOList = new LinkedList<>();
        if (!opEvseDirectionsEntityList.isEmpty() && opLocationEvseDTO != null) {
            for (OpEvseDirectionsEntity op :
                    opEvseDirectionsEntityList) {
                OpEvseDirectionsDTO opEvseDirectionsDTO = OpLocationEvseConvert.toOpEvseDirectionsDTO(op);
                opEvseDirectionsDTOList.add(opEvseDirectionsDTO);
            }
            opLocationEvseDTO.setOpEvseDirectionsDTOList(opEvseDirectionsDTOList);
        }

        LambdaQueryWrapper<OpEvseImageEntity> queryWrapper3 = Wrappers.lambdaQuery(OpEvseImageEntity.class)
                .eq(OpEvseImageEntity::getDeleted, Boolean.FALSE).eq(OpEvseImageEntity::getLocationEvseId, id);
        List<OpEvseImageEntity> opEvseImageEntityList = opEvseImageMapper.selectList(queryWrapper3);
        List<OpEvseImageDTO> opEvseImageDTOList = new LinkedList<>();
        if (!opEvseImageEntityList.isEmpty() && opLocationEvseDTO != null) {
            for (OpEvseImageEntity op :
                    opEvseImageEntityList) {
                OpEvseImageDTO opEvseImageDTO = OpLocationEvseConvert.toOpEvseImageDTO(op);
                opEvseImageDTOList.add(opEvseImageDTO);
            }
            opLocationEvseDTO.setOpEvseImageDTOList(opEvseImageDTOList);
        }

        LambdaQueryWrapper<OpLocationConnectorEntity> queryWrapper4 = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                .eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE).eq(OpLocationConnectorEntity::getLocationEvseId, id);
        OpLocationConnectorEntity opLocationConnetorEntity = opLocationConnectorMapper.selectOne(queryWrapper4);

        OpLocationConnectorDTO opLocationConnectorDTO = OpLocationEvseConvert.toOpLocationConnectorDTO(opLocationConnetorEntity);
        opLocationEvseDTO.setOpLocationConnectorDTOList(opLocationConnectorDTO);

        return opLocationEvseDTO;
    }

    @Override
    public PageDTO<GunListPageDto> getGunListRulesByStationId(GunListDto gunListDto) {
        log.info("getGunListRulesByStationId.gunListDto={}", gunListDto);
        Page<OpLocationEvseEntity> page = new Page<>(gunListDto.getCurrent(), gunListDto.getPageSize());
        LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(gunListDto.getStationId() != null, OpLocationEvseEntity::getLocationId, gunListDto.getStationId());
        queryWrapper.eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE);
        page(page, queryWrapper);
        List<GunListPageDto> gunListPageDtos = new ArrayList<>();
        log.info("getGunListRulesByStationId.page={}", page);
        page.getRecords().forEach(opLocationEvseEntity -> {
            log.info("getGunListRulesByStationId.getGunListRulesByStationId={}", opLocationEvseEntity);
            GunListPageDto gunListPageDto = new GunListPageDto();
            String[] split = opLocationEvseEntity.getEvseSn().split("_");
            gunListPageDto.setGunNo(Integer.valueOf(split[1]));
            gunListPageDto.setPileSn(split[0]);
            //根据evseId获取连接器
            List<OpLocationConnectorDTO> opLocationConnectorDTOS = opLocationConnectorMapper.selectByLocationEvseSn(opLocationEvseEntity.getId());
            if (!CollectionUtils.isEmpty(opLocationConnectorDTOS)) {
                gunListPageDto.setGunType(opLocationConnectorDTOS.get(0).getGunType());
                gunListPageDto.setConnectorId(opLocationConnectorDTOS.get(0).getConnectorId());
            }
            //从redis获取枪的状态
            EvseDeviceStatusEnum evseStatus = monitorFeign.getEvseStatus(opLocationEvseEntity.getId());
            log.info("getGunListRulesByStationId.evseStatus={}", evseStatus);
            gunListPageDto.setStatus(AppGunStateEnum.gunRealState2AppState(LocationEVSEStatusEnum.realTimeStatus2BusinessStatus(evseStatus.getName()).getCode()));
            //从redis获取枪的实时数据
            Map<String, String> evseMonitorInfo = monitorFeign.getEvseMonitorInfo(opLocationEvseEntity.getId());
            log.info("EVSEId={},实时数据={}", opLocationEvseEntity.getId(), evseMonitorInfo);
            OpLocationEvseRealTimeDTO opLocationEvseRealTimeDTO = new OpLocationEvseRealTimeDTO();
            Map<String, String> monitorMap = monitorFeign.getEvseMonitorInfo(opLocationEvseEntity.getId());
            OpLocationEvseConvert.setDataFromMap(opLocationEvseRealTimeDTO, monitorMap);
            gunListPageDto.setPower(opLocationEvseRealTimeDTO.getPower());
            gunListPageDto.setRestChargeTime(opLocationEvseRealTimeDTO.getRdTimeLeft());
            // 处理阶段电价
            if (opLocationEvseEntity.getTariffId() != null) {
                gunListPageDto.setStationRuleId(opLocationEvseEntity.getTariffId());
                OpLocationEntity opLocationEntity = opLocationMapper.selectById(opLocationEvseEntity.getLocationId());
                log.info("getGunListRulesByStationId.opLocationEntity={}", opLocationEntity);
                String zone = opLocationEntity.getTimeZone() == null ? "GMT+8" : opLocationEntity.getTimeZone().replace("UTC", "GMT");
                LocalDateTime localDateTime = LocalDateTime.now().atZone(ZoneId.of(zone)).toLocalDateTime();
                //检索对象
                List<StagePirceDto> stagePirces = new ArrayList<>();
                CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
                costModelRuleDTO.setId(opLocationEvseEntity.getTariffId());
                costModelRuleDTO.setRuleIds(Lists.newArrayList(opLocationEvseEntity.getTariffId()));
                log.info("getGunListRulesByStationId.costModelRuleDTO={}", costModelRuleDTO);
                //查询计费规则
                Result<List<CostModelRuleDTO>> tariffResult = tariffFeignClient.queryListByIds(costModelRuleDTO);
                if (tariffResult != null && org.apache.commons.collections4.CollectionUtils.isNotEmpty(tariffResult.getData())) {
                    CostModelRuleDTO costModelRuleDTO1 = tariffResult.getData().get(0);
                    log.info("getGunListRulesByStationId.costModelRule={}", costModelRuleDTO1);
                    List<CostRuleWeeksDTO> rules = costModelRuleDTO1.getRules();
                    if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(rules)) {
                        rules.forEach(weekRule -> {
                            int weekDay = localDateTime.getDayOfWeek().getValue();
                            if (weekRule.getWeeks().contains(weekDay)) {
                                StagePirceDto stagePirceDto = new StagePirceDto();
                                List<CostRulesDTO> weeksRules = weekRule.getWeeksRules();
                                if (org.apache.commons.collections4.CollectionUtils.isNotEmpty(weeksRules)) {
                                    for (CostRulesDTO costRulesDTO : weeksRules) {
                                        stagePirceDto.setTimeStart(costRulesDTO.getBeginTime());
                                        stagePirceDto.setTimeEnd(costRulesDTO.getEndTime());
                                        stagePirceDto.setEnergyPrice(costRulesDTO.getUnitPrice().setScale(4, BigDecimal.ROUND_HALF_UP));
                                        stagePirceDto.setTimePrice(costRulesDTO.getTimePrice().setScale(4, BigDecimal.ROUND_HALF_UP));
                                        stagePirceDto.setStartPrice(costRulesDTO.getStartPrice().setScale(4, BigDecimal.ROUND_HALF_UP));
                                        stagePirces.add(stagePirceDto);
                                    }
                                }
                            }
                        });
                    }
                    log.info("getGunListRulesByStationId.stagePirces={}", stagePirces);
                    gunListPageDto.setStagePirce(stagePirces);
                }
            }
            gunListPageDtos.add(gunListPageDto);
        });
        PageDTO<GunListPageDto> response = new PageDTO<>();
        response.setData(gunListPageDtos);
        response.setPages(page.getPages());
        response.setTotalCount(page.getTotal());
        response.setPageNo(((int) page.getCurrent()));
        response.setPageSize((int) page.getSize());
        log.info("getGunListRulesByStationId.response={}", response);
        return response;
    }

    @Override
    public List<OpEvseAssociatedRuleDTO> queryEvseByTariffId(Long tariffId, Boolean isExcludedTariff) {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        if (Boolean.TRUE.equals(isExcludedTariff)) {
            boolQueryBuilder.mustNot(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        } else {
            boolQueryBuilder = boolQueryBuilder.must(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        }
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(boolQueryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpEvseAssociatedRuleDTO> retList = new ArrayList<>();
        iterable.forEach(item -> retList.add(OpLocationEvseConvert.toOpEvseAssociatedRuleDTO(item)));
        return retList;
    }

    @Override
    public List<OpPileAssociatedRuleDTO> queryPileByTariffId(Long tariffId, Long currentUserId, Boolean isExcludedTariff) {
        log.info("OpLocationEvseRepositoryImpl.queryPileByTariffId.tatiffId and isExcludedTariff = {}, {}", tariffId, isExcludedTariff);

        //由于一个桩的多枪绑定多个计费规则之后，桩的计费规则是其中的一个枪的计费规则。
        // 所以现在不能使用桩来匹配计费规则，否则计费设置页面只能在其中的一个计费规则找到枪1，另外一个计费规则找不到枪2的情况。

        //一、现在通过枪来找计费规则，从而带出桩
        BoolQueryBuilder evseQuery = QueryBuilders.boolQuery();
        //isExcludedTariff：false的时候匹配当前的计费规则，true的时候匹配非当前的计费规则
        boolean havePermission = true;
        if (Boolean.TRUE.equals(isExcludedTariff)) {
            evseQuery.mustNot(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        } else {
            //查询已关联的桩
            havePermission = false;
            evseQuery.must(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        }
        evseQuery.must(QueryBuilders.termQuery("operatorId", currentUserId));
        //是否是商家
        boolean sellerAdmin = LoginUserUtil.isSellerAdmin();
        if (sellerAdmin) {
            havePermission = true;
        }
        List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
        log.info("OpLocationEvseRepositoryImpl queryPileByTariffId locationIdList = "
                + JSON.toJSONString(locationIdList));
        if (CollectionUtils.isEmpty(locationIdList) && havePermission) {
            return Collections.emptyList();
        }
        if (havePermission) {
            evseQuery.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIdList));
        }
        log.info("OpLocationEvseRepositoryImpl queryPileByTariffId evseQuery = "
                + JSON.toJSONString(evseQuery));

        Set<Long> notAdvertisingStationList = opLocationRepository.getAllNotAdvertisingStation(LoginUserUtil.getSellerId());
        if (!CollectionUtils.isEmpty(notAdvertisingStationList)) {
            evseQuery.must(QueryBuilders.termsQuery("locationId",notAdvertisingStationList));
        }
        Iterable<OpLocationEvseElasticDTO> evseIterable =
//                opLocationEvseElastic.search(evseQuery);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(evseQuery).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        //找出绑定对应计费规则的枪的桩
        HashSet<String> pileSnSet = new HashSet<>();
        if (evseIterable != null) {
            evseIterable.forEach(evse -> {
                if (StringUtils.isNotBlank(evse.getEvseSn())) {
                    pileSnSet.add(evse.getEvseSn().split("_")[0]);
                }
            });
        }

        //二、此时再根据已查出的Sn来找桩
        BoolQueryBuilder pileQuery = QueryBuilders.boolQuery();
        pileQuery.must(QueryBuilders.termQuery("operatorId", currentUserId));
        if (havePermission) {
            pileQuery.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIdList));
        }
        pileQuery.must(QueryBuilders.termsQuery("pileSn", pileSnSet));
        if (!CollectionUtils.isEmpty(notAdvertisingStationList)) {
            pileQuery.must(QueryBuilders.termsQuery("locationId",notAdvertisingStationList));
        }
        Iterable<OpLocationPileEvseElasticDTO> pileIterable =
//                opLocationPileEvseElastic.search(pileQuery);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(pileQuery).build(), OpLocationPileEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpPileAssociatedRuleDTO> retList = new ArrayList<>();
        boolean finalHavePermission = havePermission;
        pileIterable.forEach(item -> {
            OpPileAssociatedRuleDTO ruleDTO = OpLocationPileEvseConvert.toOpPileAssociatedRuleDTO(item);
            if (CollectionUtils.isEmpty(locationIdList) || (!finalHavePermission && !locationIdList.contains(item.getLocationId()))) {
                ruleDTO.setHavePermission(false);
            }
            List<Long> evseIdList = Optional.ofNullable(JSON.parseArray(item.getEvseList()))
                    .orElse(JSON.parseArray("[]")).toJavaList(Long.class);
            if (!CollectionUtils.isEmpty(evseIdList)) {
                //此时，桩下的枪可能绑定的计费规则是不一样的，所以需要再构造一次条件
                BoolQueryBuilder evseQuery2 = QueryBuilders.boolQuery();
                if (Boolean.TRUE.equals(isExcludedTariff)) {
                    evseQuery2.mustNot(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
                } else {
                    evseQuery2.must(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
                }
                evseQuery2.must(QueryBuilders.termsQuery("id", evseIdList));
                if (!CollectionUtils.isEmpty(notAdvertisingStationList)) {
                    evseQuery2.must(QueryBuilders.termsQuery("locationId",notAdvertisingStationList));
                }
                Iterable<OpLocationEvseElasticDTO> evseIterable2 =
//                        opLocationEvseElastic.search(evseQuery2);
                        elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(evseQuery2).build(), OpLocationEvseElasticDTO.class)
                                .stream().map(SearchHit::getContent).collect(Collectors.toList());
                //此时查出了对应计费规则的桩下的枪
                List<OpEvseAssociatedRuleDTO> evseRuleList = new ArrayList<>();
                if (CollUtil.isNotEmpty(evseIterable2)) {
                    evseIterable2.forEach(evseItem -> {
                        if (evseItem != null) {
                            evseRuleList.add(OpLocationEvseConvert.toOpEvseAssociatedRuleDTO(evseItem));
                        }
                    });
                }
                ruleDTO.setOpEvseAssociatedRuleDTOList(evseRuleList);
            }
            retList.add(ruleDTO);
        });
        log.info("OpLocationEvseRepositoryImpl.queryPileByTariffId.retList = {}", retList);
        return retList;
    }

    @Override
    public Page<OpLocationsAndEvsesDTO> searchSitesOrEvses(OpLocationQueryDTO opLocationQueryDTO) {
        LambdaQueryWrapper<OpLocationEntity> query = Wrappers.lambdaQuery(OpLocationEntity.class)
                .eq(OpLocationEntity::getDeleted, Boolean.FALSE)
                .like(OpLocationEntity::getName, opLocationQueryDTO.getNameOrSN());
        // 模糊查询场站
        Page<OpLocationEntity> page = new Page<>(opLocationQueryDTO.getPage(), opLocationQueryDTO.getPageSize());
        Page<OpLocationEntity> opLocationEntityPage = opLocationMapper.selectPage(page, query);
        List<OpLocationEntity> opLocationEntityList = opLocationMapper.selectList(query);

        LambdaQueryWrapper<OpLocationPileEvseEntity> query0 = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                .eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE)
                .like(OpLocationPileEvseEntity::getPileSn, opLocationQueryDTO.getNameOrSN());
        // 模糊查询桩SN
        Page<OpLocationPileEvseEntity> page1 = new Page<>(opLocationQueryDTO.getPage(), opLocationQueryDTO.getPageSize());
        Page<OpLocationPileEvseEntity> opLocationPileEvseEntityPage = opLocationPileEvseMapper.selectPage(page1, query0);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(query0);
        //统计返回page的总条数
        Set<Long> locationId = new TreeSet<>();
        List<Long> pileEvseLocationIds = new ArrayList<>();
        if (!opLocationEntityList.isEmpty()) {
            opLocationEntityList.forEach(entity -> locationId.add(entity.getId()));
        }
        if (!locationId.isEmpty() && !opLocationPileEvseEntityList.isEmpty()) {
            opLocationPileEvseEntityList.forEach(entity -> {
                if (!locationId.contains(entity.getLocationId())) {
                    pileEvseLocationIds.add(entity.getLocationId());
                }
                locationId.add(entity.getLocationId());
            });
        }

        List<OpLocationsAndEvsesDTO> opLocationsAndEvsesDTOS = new LinkedList<>();

        if (!opLocationEntityPage.getRecords().isEmpty()) {
            for (OpLocationEntity op : opLocationEntityPage.getRecords()) {
                LambdaQueryWrapper<OpLocationOperationEntity> query2 = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                        .eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE)
                        .eq(OpLocationOperationEntity::getLocationId, op.getId());
                OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(query2);
                OpLocationsAndEvsesDTO opLocationsAndEvsesDTO = OpLocationEvseConvert
                        .toOpLocationsAndEvsesDTO(op, opLocationOperationEntity);
                statisticEvse(opLocationsAndEvsesDTO);
                List<EvseInfoDTO> evseInfoDTOS = new LinkedList<>();
                if (!opLocationPileEvseEntityPage.getRecords().isEmpty()) {
                    opLocationPileEvseEntityPage.getRecords().forEach(entity -> {
                        if (entity.getLocationId().equals(op.getId())) {
                            String evseList = entity.getEvseList();
                            JSONArray evseIdArray = JSON.parseArray(evseList);

                            if (!evseIdArray.isEmpty()) {
                                for (int i = 0; i < evseIdArray.size(); i++) {
                                    Long evseId = evseIdArray.getLong(i);  // 遍历 jsonarray 数组，把每一个对象转成Long类型的id

                                    LambdaQueryWrapper<OpLocationEvseEntity> query1 = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                                            .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE)
                                            .eq(OpLocationEvseEntity::getId, evseId);
                                    OpLocationEvseEntity opLocationEvseEntity = this.getBaseMapper().selectOne(query1);

                                    LambdaQueryWrapper<OpLocationConnectorEntity> query3 = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                                            .eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE)
                                            .eq(OpLocationConnectorEntity::getLocationEvseId, evseId);
                                    OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorMapper.selectOne(query3);
                                    EvseInfoDTO evseInfoDTO = OpLocationEvseConvert.toEvseInfoDTO(opLocationEvseEntity, opLocationConnectorEntity);
                                    evseInfoDTOS.add(evseInfoDTO);
                                }
                            }
                        }
                    });
                    opLocationsAndEvsesDTO.setEvseInfoDTOS(evseInfoDTOS);
                }
                opLocationsAndEvsesDTOS.add(opLocationsAndEvsesDTO);
            }
        }
        if (!opLocationPileEvseEntityPage.getRecords().isEmpty()) {
            opLocationPileEvseEntityPage.getRecords().forEach(entity -> {
                if (pileEvseLocationIds.contains(entity.getLocationId())) {
                    LambdaQueryWrapper<OpLocationEntity> query4 = Wrappers.lambdaQuery(OpLocationEntity.class)
                            .eq(OpLocationEntity::getDeleted, Boolean.FALSE)
                            .eq(OpLocationEntity::getId, entity.getLocationId());
                    OpLocationEntity opLocationEntity = opLocationMapper.selectOne(query4);
                    LambdaQueryWrapper<OpLocationOperationEntity> query5 = Wrappers.lambdaQuery(OpLocationOperationEntity.class)
                            .eq(OpLocationOperationEntity::getDeleted, Boolean.FALSE)
                            .eq(OpLocationOperationEntity::getId, entity.getLocationId());
                    OpLocationOperationEntity opLocationOperationEntity = opLocationOperationMapper.selectOne(query5);
                    OpLocationsAndEvsesDTO opLocationsAndEvsesDTO = OpLocationEvseConvert.toOpLocationsAndEvsesDTO(opLocationEntity, opLocationOperationEntity);
                    statisticEvse(opLocationsAndEvsesDTO);

                    String evseList = entity.getEvseList();
                    JSONArray evseIdArray = JSON.parseArray(evseList);
                    Long evseId = evseIdArray.getLong(0);

                    // 查出充电设备信息
                    LambdaQueryWrapper<OpLocationEvseEntity> query6 = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                            .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE)
                            .eq(OpLocationEvseEntity::getId, evseId);
                    List<OpLocationEvseEntity> opLocationEvseEntityList = this.getBaseMapper().selectList(query6);
                    OpLocationEvseEntity opLocationEvseEntity = opLocationEvseEntityList.get(0);

                    LambdaQueryWrapper<OpLocationConnectorEntity> query7 = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                            .eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE)
                            .eq(OpLocationConnectorEntity::getLocationEvseId, evseId);
                    OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorMapper.selectOne(query7);
                    EvseInfoDTO evseInfoDTO = OpLocationEvseConvert.toEvseInfoDTO(opLocationEvseEntity, opLocationConnectorEntity);
                    List<EvseInfoDTO> evseInfoDTOS = new ArrayList<>();
                    evseInfoDTOS.add(evseInfoDTO);
                    opLocationsAndEvsesDTO.setEvseInfoDTOS(evseInfoDTOS);
                    opLocationsAndEvsesDTOS.add(opLocationsAndEvsesDTO);
                }
            });
        }

        Page<OpLocationsAndEvsesDTO> opLocationsAndEvsesDTOPage = new Page<>();
        opLocationsAndEvsesDTOPage.setRecords(opLocationsAndEvsesDTOS);
        opLocationsAndEvsesDTOPage.setTotal(locationId.size());
        opLocationsAndEvsesDTOPage.setSize(opLocationQueryDTO.getPageSize());
        opLocationsAndEvsesDTOPage.setCurrent(opLocationQueryDTO.getPage());

        return opLocationsAndEvsesDTOPage;
    }

    /**
     * 统计桩信息
     *
     * @param opLocationsAndEvsesDTO
     * @return
     */
    private OpLocationsAndEvsesDTO statisticEvse(OpLocationsAndEvsesDTO opLocationsAndEvsesDTO) {
        Long locationId = opLocationsAndEvsesDTO.getId();
        LambdaQueryWrapper<OpLocationEvseEntity> query = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                .eq(OpLocationEvseEntity::getDeleted, Boolean.FALSE)
                .eq(OpLocationEvseEntity::getLocationId, locationId);
        List<OpLocationEvseEntity> opLocationEvseEntityList = this.list(query);
        List<Long> evseIds = opLocationEvseEntityList.stream().map(OpLocationEvseEntity::getId).collect(Collectors.toList());
        if (!opLocationEvseEntityList.isEmpty()) {
            opLocationEvseEntityList.forEach(entity -> calStatus(opLocationsAndEvsesDTO, entity));
            evseIds.forEach(evseId -> {
                LambdaQueryWrapper<OpLocationConnectorEntity> query1 = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                        .eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE)
                        .eq(OpLocationConnectorEntity::getLocationEvseId, evseId);
                OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorMapper.selectOne(query1);
                if (opLocationConnectorEntity != null) {
                    calPower(opLocationsAndEvsesDTO, opLocationConnectorEntity);
                    calGunType(opLocationsAndEvsesDTO, opLocationConnectorEntity);
                }
            });
        }
        return opLocationsAndEvsesDTO; 
    }

    private void calGunType(OpLocationsAndEvsesDTO opLocationsAndEvsesDTO, OpLocationConnectorEntity entity) {
        switch (entity.getGunType()) {
            case 1:
                opLocationsAndEvsesDTO.increaseCCS_COMBO_2_num();
                break;
            case 2:
                opLocationsAndEvsesDTO.increaseCHADEMO_num();
                break;
            case 3:
                opLocationsAndEvsesDTO.increaseTYPE_2_num();
                break;
            case 4:
                opLocationsAndEvsesDTO.increaseTYPE_3_num();
                break;
            case 5:
                opLocationsAndEvsesDTO.increaseTYPE_1_num();
                break;
            case 6:
                opLocationsAndEvsesDTO.increaseWALL_num();
                break;
            case 7:
                opLocationsAndEvsesDTO.increaseCSS_COMBO_1_num();
                break;
            case 8:
                opLocationsAndEvsesDTO.increaseGB_T_DC_num();
                break;
            case 9:
                opLocationsAndEvsesDTO.increaseGB_T_AC_num();
                break;
            default:
                break;
        }
    }

    private void calPower(OpLocationsAndEvsesDTO opLocationsAndEvsesDTO, OpLocationConnectorEntity entity) {
        if (entity.getPower() == 7) {
            opLocationsAndEvsesDTO.increaseEvseNum_7();
        } else if (entity.getPower() == 60) {
            opLocationsAndEvsesDTO.increaseEvseNum_60();
        }
    }

    private void calStatus(OpLocationsAndEvsesDTO opLocationsAndEvsesDTO, OpLocationEvseEntity entity) {
        switch (EvseDeviceStatusEnum.getEnumByName(entity.getState())) {
            case AVAILABLE:
                opLocationsAndEvsesDTO.increaseAvailable();
                break;
            case CHARGING:
                opLocationsAndEvsesDTO.increaseCharging();
                break;
            case UNAVAILABLE:
                opLocationsAndEvsesDTO.increaseUnavailable();
                break;
            case RESERVED:
                opLocationsAndEvsesDTO.increaseDetained();
                break;
            default:
                break;
        }
    }

    @Override
    public OpLocationEvseElasticDTO getDetailsFromEsById(Long id) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("id", id));

        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        AtomicReference<OpLocationEvseElasticDTO> opLocationEvseElasticDTO = new AtomicReference<>(new OpLocationEvseElasticDTO());
        iterable.forEach(opLocationEvseElasticDTO::set);

        return opLocationEvseElasticDTO.get();
    }


    @Override
    public OpEvseInfoDTO getEvseById(Long id) {
        Optional<OpLocationEvseElasticDTO> optional = opLocationEvseElastic.findById(id);
        if (!optional.isPresent()) {
            return null;
        }
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = optional.get();
        return OpLocationEvseConvert.toOpEvseInfoDTO(opLocationEvseElasticDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class) // 需要的是分布式事务  这里加事务没有啥用
    public List<PileVO> createEvse(List<OpLocationEvseDTO> opLocationEvseDTOs) {
        List<PileVO> pileVOS = new ArrayList<>();
        opLocationEvseDTOs.forEach(opLocationEvseDTO -> {
            //SN码转大写
            try {
                opLocationEvseDTO.setEvseSn(opLocationEvseDTO.getPileSN().toUpperCase());
            } catch (Exception e) {
                log.error("Conversion failed");
            }
            //道通品牌桩的校验
            log.info("品牌id:{}  品牌id和道通匹配id相等:{}", opLocationEvseDTO.getBrandId(), opLocationEvseDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode())));
            if (opLocationEvseDTO.getBrandId().equals(Long.valueOf(BrandEnum.AUTEL.getCode()))) {
                log.info("道通品牌");
                //判断是否美标极简版充电桩  美标极简版充电桩不能添加到平台作为商桩(要排在校验桩是否被绑定前，不然桩被改成商桩后，即不能添加在平台作为商桩，也不能添加到app作为家桩)
                Result<ChargePileDTO> chargePileDetailResult = null;
                try {
                    chargePileDetailResult = deviceServiceFeign.pileDetail(opLocationEvseDTO.getPileSN());
                } catch (Exception e) {
                    log.error("OpLocationEvseRepositoryImpl validEvse" + e);
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
                //校验pileSN和pin
                VerifyDTO verifyDTO = new VerifyDTO();
                verifyDTO.setPin(opLocationEvseDTO.getPinCode());
                verifyDTO.setPileSn(opLocationEvseDTO.getPileSN());
                Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(verifyDTO);
                if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
                }
                //校验桩是否被绑定
                Result<Object> result = dataServiceFeign.addPile(opLocationEvseDTO.getPileSN(), opLocationEvseDTO.getPinCode());
                log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
                if (result == null || !Objects.equals(result.getCode(), HttpCodeEnum.OK.getCode()) || !Objects.equals(result.getData(), Boolean.TRUE)) {
                    log.error("OpLocationEvseRepositoryImpl.createEvse failed {}", opLocationEvseDTO.getPileSN());
                    throw new MessageCodeException(PileBaseEnum.CHARGE_PILE_HAS_BIND_USER);
                }

                //三方桩的校验
            } else {
                log.info("三方桩");
                //data-service是否已被绑定为家桩
                Result<Boolean> restResult = homePileClient.queryBind(opLocationEvseDTO.getPileSN());
                if (restResult != null && restResult.getCode().equals(HttpStatus.SC_OK) && restResult.getData().equals(Boolean.TRUE)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND);
                }
            }
            //校验场站是否存在
            OpLocationEntity opLocationEntity = opLocationMapper.selectById(opLocationEvseDTO.getLocationId());
            if (opLocationEntity == null) {
                throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
            }
            //根据pileSN校验桩是否已存在别的场站下
            LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
            queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
            queryWrapper.eq(OpLocationPileEvseEntity::getPileSn, opLocationEvseDTO.getPileSN());
            Integer integer = opLocationPileEvseMapper.selectCount(queryWrapper);
            if (integer > 0) {
                throw new MessageCodeException(PileBaseEnum.PILE_EXIST_OTHER_LOCATION);
            }

            PileVO pileVO = createEvse(opLocationEvseDTO, opLocationEntity);
            pileVOS.add(pileVO);
        });
        Long userId = null;
        try {
            userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        } catch (Exception e) {
            userId = LoginUserHolder.getLoginUser().getId();
        }
        stringRedisTemplate.delete(RedisKeyConstant.getStringAddPileException(userId));
        return pileVOS;
    }

    @Override
    public List<PileVO> validEvse(List<OpLocationEvseDTO> opLocationEvseDTOs) {
        List<PileVO> pileVOS = new ArrayList<>();
        opLocationEvseDTOs.forEach(opLocationEvseDTO -> {
            //SN码转大写
            try {
                opLocationEvseDTO.setEvseSn(opLocationEvseDTO.getPileSN().toUpperCase());
            } catch (Exception e) {
                log.error("Conversion failed");
            }
            //道通品牌桩的校验
            if (Objects.equals(opLocationEvseDTO.getBrandId(), Long.valueOf(BrandEnum.AUTEL.getCode()))) {
                //判断是否美标极简版充电桩  美标极简版充电桩不能添加到平台作为商桩(要排在校验桩是否被绑定前，不然桩被改成商桩后，即不能添加在平台作为商桩，也不能添加到app作为家桩)
                Result<ChargePileDTO> chargePileDetailResult = null;
                try {
                    chargePileDetailResult = deviceServiceFeign.pileDetail(opLocationEvseDTO.getPileSN());
                } catch (Exception e) {
                    log.error("OpLocationEvseRepositoryImpl validEvse" + e);
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
                //校验pileSN和pin
                VerifyDTO verifyDTO = new VerifyDTO();
                verifyDTO.setPin(opLocationEvseDTO.getPinCode());
                verifyDTO.setPileSn(opLocationEvseDTO.getPileSN());
                Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(verifyDTO);
                if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
                }
                //校验桩是否被绑定
                Result<Object> result = dataServiceFeign.addPile(opLocationEvseDTO.getPileSN(), opLocationEvseDTO.getPinCode());
                log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
                if (result == null || !Objects.equals(result.getCode(), HttpCodeEnum.OK.getCode()) || !Objects.equals(result.getData(), Boolean.TRUE)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGE_PILE_HAS_BIND_USER);
                }

                //三方桩的校验
            } else {
                //data-service是否已被绑定为家桩
                Result<Boolean> restResult = homePileClient.queryBind(opLocationEvseDTO.getPileSN());
                if (restResult != null && restResult.getCode().equals(HttpStatus.SC_OK) && restResult.getData().equals(Boolean.TRUE)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND);
                }
            }
            //校验场站是否存在,不能校验，因为有可能新增场站
            OpLocationEntity opLocationEntity = opLocationMapper.selectById(opLocationEvseDTO.getLocationId());
            //根据pileSN校验桩是否已存在别的场站下
            LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
            queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
            queryWrapper.eq(OpLocationPileEvseEntity::getPileSn, opLocationEvseDTO.getPileSN());
            Integer integer = opLocationPileEvseMapper.selectCount(queryWrapper);
            if (integer > 0) {
                throw new MessageCodeException(PileBaseEnum.PILE_EXIST_OTHER_LOCATION);
            }

            PileVO pileVO = createEvse(opLocationEvseDTO, opLocationEntity);
            pileVOS.add(pileVO);
        });
        Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        try {
            stringRedisTemplate.delete(RedisKeyConstant.getStringAddPileException(userId));
        } catch (Exception e) {
            log.info("添加桩删除es异常,e={}", e);
        }
        return pileVOS;
    }

    @Override
    public List<PileVO> onlyValidEvse(List<OpLocationEvseDTO> opLocationEvseDTOs) {
        List<PileVO> pileVOS = new ArrayList<>();
        opLocationEvseDTOs.forEach(opLocationEvseDTO -> {
            if (StringUtils.isEmpty(opLocationEvseDTO.getPileSN())) {
                log.info("OpLocationEvseRepositoryImpl,pileSn is={}", opLocationEvseDTO.getPileSN());
                return;
            }
            //SN码转大写
            try {
                opLocationEvseDTO.setEvseSn(opLocationEvseDTO.getPileSN().toUpperCase());
            } catch (Exception e) {
                log.error("Conversion failed");
            }
            //道通品牌桩的校验
            if (Objects.equals(opLocationEvseDTO.getBrandId(), Long.valueOf(BrandEnum.AUTEL.getCode()))) {
                //判断是否美标极简版充电桩  美标极简版充电桩不能添加到平台作为商桩(要排在校验桩是否被绑定前，不然桩被改成商桩后，即不能添加在平台作为商桩，也不能添加到app作为家桩)
                Result<ChargePileDTO> chargePileDetailResult = null;
                try {
                    chargePileDetailResult = deviceServiceFeign.pileDetail(opLocationEvseDTO.getPileSN());
                } catch (Exception e) {
                    log.error("OpLocationEvseRepositoryImpl validEvse" + e);
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
                //校验pileSN和pin
                VerifyDTO verifyDTO = new VerifyDTO();
                verifyDTO.setPin(opLocationEvseDTO.getPinCode());
                verifyDTO.setPileSn(opLocationEvseDTO.getPileSN());
                Result<Boolean> booleanResult = deviceServiceFeign.verifyPile(verifyDTO);
                if (booleanResult.getCode() != 200 || !Objects.equals(Boolean.TRUE, booleanResult.getData())) {
                    throw new MessageCodeException(PileBaseEnum.PILESN_NOT_MATCH_PIN);
                }
                //校验桩是否被绑定
                Result<Object> result = dataServiceFeign.addPile(opLocationEvseDTO.getPileSN(), opLocationEvseDTO.getPinCode());
                log.info("OpLocationEvseRepositoryImpl.createEvse.result = {}", JSON.toJSONString(result));
                if (result == null || !Objects.equals(result.getCode(), HttpCodeEnum.OK.getCode()) || !Objects.equals(result.getData(), Boolean.TRUE)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGE_PILE_HAS_BIND_USER);
                }

                //三方桩的校验
            } else {
                //data-service是否已被绑定为家桩
                Result<Boolean> restResult = homePileClient.queryBind(opLocationEvseDTO.getPileSN());
                if (restResult != null && restResult.getCode().equals(HttpStatus.SC_OK) && restResult.getData().equals(Boolean.TRUE)) {
                    throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND);
                }
            }
            //根据pileSN校验桩是否已存在别的场站下
            LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
            queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
            queryWrapper.eq(OpLocationPileEvseEntity::getPileSn, opLocationEvseDTO.getPileSN());
            Integer integer = opLocationPileEvseMapper.selectCount(queryWrapper);
            log.info("OpLocationEvseRepositoryImpl.onlyValidEvse.start.integer = {}", integer);
            if (integer > 0) {
                throw new MessageCodeException(PileBaseEnum.PILE_EXIST_OTHER_LOCATION);
            }
            log.info("OpLocationEvseRepositoryImpl.onlyValidEvse.end.integer = {}", integer);
            PileVO pileVO = new PileVO();
            pileVOS.add(pileVO);
        });
        return pileVOS;
    }

    @Override
    public PileVO createEvse(OpLocationEvseDTO opLocationEvseDTO, OpLocationEntity opLocationEntity) {

        log.info("===>>>OpLocationEvseRepositoryImpl.createEvse opLocationEvseDTO : {} and opLocationEntity : {}",
                JSON.toJSONString(opLocationEvseDTO),
                JSON.toJSONString(opLocationEntity));

        // 校验充电桩名称必填
        if (org.apache.commons.lang3.StringUtils.isBlank(opLocationEvseDTO.getPileName())) {
            throw new MessageCodeException(PileBaseEnum.PLEASE_ENTER_THE_CHARGING_PILE_NAME);
        }

        // 校验同一场站下的充电桩名称不能相同
        if (opLocationEvseDTO.getLocationId() != null) {
            CheckPileNameDTO checkPileNameDTO = new CheckPileNameDTO();
            checkPileNameDTO.setPileName(opLocationEvseDTO.getPileName());
            checkPileNameDTO.setLocationId(opLocationEvseDTO.getLocationId());
            if (!this.checkPileNameInLocationUnique(checkPileNameDTO)) {
                throw new MessageCodeException(PileBaseEnum.NAME_ALREADY_USED);
            }
        }
        //查询桩是否订阅到期
        boolean subscriptionCheck = false;
        String evseSn = opLocationEvseDTO.getPileSN();

        BenefitFunctionReqDto agreementFunctionReqVo = new BenefitFunctionReqDto();
        agreementFunctionReqVo.setPileSn(Lists.newArrayList(evseSn));
        log.info("添加桩时,agreementFunctionReqVo:{}", JSON.toJSONString(agreementFunctionReqVo));

        ArrayList<Long> evseIds = new ArrayList<>();
        PileVO pileVO = new PileVO();
        List<PileEvseVO> pileEvseVOs = new ArrayList<>();
        List<String> gunTypes = new ArrayList<>();
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = new ArrayList<>();
        final OpLocationEvseEntity[] oneOpLocationEvseEntity = {null};


        //如果是三方桩，保存device库的三张表(存在更新，不存在新增)
        if (Integer.valueOf(1).equals(opLocationEvseDTO.getThirdPart()) || !Objects.equals(opLocationEvseDTO.getBrandName().toUpperCase(), "AUTEL")) {
            deviceServiceFeign.saveThirdPile(opLocationEvseDTO);
        }

        if (!CollectionUtils.isEmpty(opLocationEvseDTO.getOpLocationConnectorDTOs())) {
            //如果是Autel品牌,基础信息根据sn获取
            if (Integer.valueOf(0).equals(opLocationEvseDTO.getThirdPart())) {
                List<String> snList = Collections.singletonList(opLocationEvseDTO.getPileSN());
                List<ChargePileDTO> chargePileDTOS = deviceServiceFeign.queryPileList(snList).getData();
                if (CollectionUtils.isEmpty(chargePileDTOS)) {
                    return new PileVO();
                }
                ChargePileDTO chargePileDTO = chargePileDTOS.get(0);
                opLocationEvseDTO.setProductModel(chargePileDTO.getProductModel());
                opLocationEvseDTO.setPowerType(handlePowerType(chargePileDTO.getCategory(), chargePileDTO.getPhase()));
                opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(e -> {
                    e.setAmperage(new BigDecimal(String.valueOf(chargePileDTO.getElectricCurrent() == null ? 0.0 : chargePileDTO.getElectricCurrent())));
                    e.setVoltage(new BigDecimal(String.valueOf(chargePileDTO.getVoltage() == null ? 0.0 : chargePileDTO.getVoltage())));
                    e.setPower(chargePileDTO.getOutputPower() == null ? 0.0 : chargePileDTO.getOutputPower());
                    e.setPowerType(handlePowerType(chargePileDTO.getCategory(), chargePileDTO.getPhase()));
                });
                //判断是否是家桩共享的桩，如果是的话，需要把订阅开关设置为true
                if (DeviceTypeEnum.SHARED_HOME_PILE.getValue().equals(chargePileDTO.getUsageScenario())) {
                    subscriptionCheck = true;
                }else {
                    subscriptionCheck = getSubscribed(agreementFunctionReqVo);
                }
            } else {
                subscriptionCheck = getSubscribed(agreementFunctionReqVo);
                LambdaQueryWrapper<OpEvseBrandModelEntity> queryWrapper = new LambdaQueryWrapper<>();
                queryWrapper.eq(OpEvseBrandModelEntity::getBrandName, opLocationEvseDTO.getBrandName())
                        .eq(OpEvseBrandModelEntity::getProductModel, opLocationEvseDTO.getProductModel())
                        .eq(OpEvseBrandModelEntity::getDeleted, 0);
                OpEvseBrandModelEntity opEvseBrandModelEntity = opEvseBrandModelMapper.selectOne(queryWrapper);
                opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(e -> {
                    e.setPower(opLocationEvseDTO.getPower() == null ? 0.0 : opLocationEvseDTO.getPower());
                    e.setPowerType(opLocationEvseDTO.getPowerType());
                    if (opEvseBrandModelEntity != null) {
                        e.setAmperage(opEvseBrandModelEntity.getAmperage());
                        e.setVoltage(opEvseBrandModelEntity.getVoltage());
                    }
                });
            }
            boolean finalSubscriptionCheck = subscriptionCheck;
            opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(opLocationConnectorDTO -> {
                gunTypes.add(String.valueOf(opLocationConnectorDTO.getGunType()));
                //添加充电设备
                OpLocationEvseEntity opLocationEvseEntity = new OpLocationEvseEntity();
                opLocationEvseEntity.setLocationId(opLocationEvseDTO.getLocationId());
                opLocationEvseEntity.setPinCode(opLocationEvseDTO.getPinCode());
                opLocationEvseEntity.setEvseSn(handlerEvseSn(opLocationConnectorDTO.getConnectorId(), opLocationEvseDTO.getPileSN()));
                this.save(opLocationEvseEntity);
                evseIds.add(opLocationEvseEntity.getId());

                //添加连接器
                OpLocationConnectorEntity opLocationConnectorEntity = OpLocationConnectorConvert.toOpLocationConnectorEntity(opLocationConnectorDTO);
                if (opLocationEvseDTO.getPower() != null) {
                    opLocationConnectorEntity.setPower(opLocationEvseDTO.getPower());
                }
                opLocationConnectorEntity.setPowerType(opLocationEvseDTO.getPowerType());
                opLocationConnectorEntity.setLocationEvseId(opLocationEvseEntity.getId());

                opLocationConnectorMapper.insert(opLocationConnectorEntity);

                //封装数据返回
                PileEvseVO pileEvseVO = new PileEvseVO();
                pileEvseVO.setEvseId(opLocationEvseEntity.getId());
                pileEvseVO.setGunType(opLocationConnectorEntity.getGunType());
                if (StringUtils.isNotBlank(opLocationEvseEntity.getEvseSn())) {
                    pileEvseVO.setConnector(opLocationEvseEntity.getEvseSn().substring(opLocationEvseEntity.getEvseSn().lastIndexOf("_") + 1));
                }
                pileEvseVOs.add(pileEvseVO);
                if (oneOpLocationEvseEntity[0] == null) {
                    oneOpLocationEvseEntity[0] = opLocationEvseEntity;
                }
                //收集evse es数据
                OpLocationEvseElasticDTO evseElasticDTO = OpLocationEvseConvert.toOpLocationEvseElastic(null,opLocationEvseEntity,
                        opLocationEntity, opLocationConnectorEntity);
                evseElasticDTO.setBrandId(opLocationEvseDTO.getBrandId());
                evseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
                evseElasticDTO.setPileSn(opLocationEvseDTO.getPileSN());
                evseElasticDTO.setPileName(opLocationEvseDTO.getPileName());
                evseElasticDTO.setState(monitorFeign.getEvseStatus(opLocationEvseEntity.getEvseSn()).getName());
                evseElasticDTO.setSubscriptionCheck(finalSubscriptionCheck);
                log.info("OpLocationEvseRepositoryImpl.createEvse evseElasticDTO = " + JSON.toJSONString(evseElasticDTO));
                opLocationEvseElasticDTOS.add(evseElasticDTO);
            });
        }
        OpLocationPileEvseEntity opLocationPileEvseEntity = OpLocationPileEvseConvert.toOpLocationPileEvseEntity(opLocationEvseDTO);
        opLocationPileEvseEntity.setEvseList(JSON.toJSONString(evseIds));
        opLocationPileEvseEntity.setId(null);
        if (opLocationEntity.getHubjectCheck() != null
                && opLocationEntity.getHubjectCheck()
                && Integer.valueOf(1).equals(opLocationPileEvseEntity.getPublicMark())) {
            opLocationPileEvseEntity.setEroamingEnable(1);
        } else {
            opLocationPileEvseEntity.setEroamingEnable(0);
        }
        Boolean isBritainStandPileFlag = Boolean.FALSE;
        String pileSn = opLocationPileEvseEntity.getPileSn();
        List<String> pileSnList = new ArrayList<>();
        pileSnList.add(pileSn);
        Map<String, Boolean> stringBooleanMap = opLocationPileEvseService.judgeBritainStand(pileSnList);

        log.info("OpLocationEvseRepositoryImpl.createEvse.stringBooleanMap : {}", JSON.toJSONString(stringBooleanMap));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(stringBooleanMap)) {
            Boolean flag = stringBooleanMap.get(pileSn);
            if (flag != null && flag) {
                // 英标桩需要设置默认充电时间模板
                isBritainStandPileFlag = Boolean.TRUE;
                opLocationPileEvseEntity.setDefaultChargingTime(TariffUtil.defaultChargingTemplate());
            }
        }

        OpLocationPileEvseEntity lastOpLocationPileEvseEntity = opLocationPileEvseService.findLast(pileSn);
        if (lastOpLocationPileEvseEntity != null) {
            opLocationPileEvseEntity.setFreeVendEnable(lastOpLocationPileEvseEntity.getFreeVendEnable());
            opLocationPileEvseEntity.setFreeVendIdTag(lastOpLocationPileEvseEntity.getFreeVendIdTag());
        }

        log.info("===>>>OpLocationEvseRepositoryImpl.createEvse.opLocationPileEvseEntity : {}",
                JSON.toJSONString(opLocationPileEvseEntity));

        int insert = opLocationPileEvseMapper.insert(opLocationPileEvseEntity);
        Long userId = null;
        try {
            userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
        } catch (Exception e) {
            userId = LoginUserHolder.getLoginUser().getId();
        }
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getStringAddPileException(userId), opLocationPileEvseEntity.getId().toString(), 1L, TimeUnit.MINUTES);
        if (insert > 0) {
            Map<String, String> ocppParam = new HashMap<>();
            ocppParam.put("sn", pileSn);
            ocppParam.put("bindOperatorId", String.valueOf(LoginUserHolder.getLoginUser().getId()));
            ocppParam.put("locationId", opLocationPileEvseEntity.getLocationId().toString());
            rabbitTemplate.convertAndSend("DIRECT_EXCHANGE_PILE_ACTIVATION_BIND_AUTEL" + RabbitBean.RABBITMQ_VERSION_SUFFIX, "PILE.ACTIVATION.BIND_AUTEL", ocppParam);
        }

        //同步桩数据到es
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = OpLocationPileEvseConvert
                .toOpLocationPileEvseElastic(opLocationPileEvseEntity, oneOpLocationEvseEntity[0], opLocationEntity);
        opLocationPileEvseElasticDTO.setPower(opLocationEvseDTO.getPower());
        opLocationPileEvseElasticDTO.setPowerType(opLocationEvseDTO.getPowerType());
        opLocationPileEvseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
        opLocationPileEvseElasticDTO.setPublicMark(opLocationEvseDTO.getPublicMark());
        opLocationPileEvseElasticDTO.setEroamingEnable(opLocationPileEvseEntity.getEroamingEnable());
        if (isBritainStandPileFlag) {
            // 英标桩需要设置默认充电时间模板
            opLocationPileEvseElasticDTO.setBritainStandPileMark(Boolean.TRUE);
            opLocationPileEvseElasticDTO.setDefaultChargingTime(TariffUtil.defaultChargingTemplate());
        } else {
            opLocationPileEvseElasticDTO.setBritainStandPileMark(Boolean.FALSE);
        }
        // 处理桩侧上报的英国法案数据
        this.handleEvscpSettingDTO(opLocationPileEvseElasticDTO);
        opLocationPileEvseElasticDTO.setFreeVendEnable(opLocationPileEvseEntity.getFreeVendEnable());
        opLocationPileEvseElasticDTO.setFreeVendIdTag(opLocationPileEvseEntity.getFreeVendIdTag());
        opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);

        log.info("OpLocationEvseRepositoryImpl.createEvse.opLocationPileEvseElasticDTO : {}",
                JSON.toJSONString(opLocationPileEvseElasticDTO));

        if (Integer.valueOf(1).equals(opLocationPileEvseEntity.getEroamingEnable())) {
            ThreadPoolUtil.getExecutor().execute(() -> {
                // 异步推送
                SetPileEroamingDTO setPileEroamingDTO = new SetPileEroamingDTO();
                setPileEroamingDTO.setPileSn(opLocationPileEvseEntity.getPileSn());
                setPileEroamingDTO.setPileEroamingOperateType(1);
                opLocationPileEvseService.setPileEroaming(setPileEroamingDTO);
            });
        }

        // 缓存桩运营商用户
        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(opLocationEvseDTO.getPileSN());
        stringRedisTemplate.opsForValue().set(snOperatorIdKey, opLocationPileEvseElasticDTO.getOperatorId().toString());

        //同步evse数据到es
        opLocationEvseElasticDTOS.forEach(e -> {
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.save(e);
            log.info("OpLocationEvseRepositoryImpl.createEvse.opLocationEvseElasticDTO={}", JSON.toJSONString(opLocationEvseElasticDTO));
        });

        //同步到设备扩展类
        ThreadPoolUtil.getExecutor().execute(() -> {
            List<Long> pileIds = opLocationEvseElasticDTOS.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
            log.info("OpLocationEvseRepositoryImpl.createEvse.pileIds={}", JSON.toJSONString(pileIds));
            this.syncEvseExpand(EvseExpandDTO.builder().pileIds(pileIds).build());
        });
        //封装数据返回
        pileVO.setPileId(opLocationPileEvseEntity.getId());
        pileVO.setPileName(opLocationPileEvseEntity.getName());
        pileVO.setPower(opLocationEvseDTO.getPower() == null ? 0.0 : opLocationEvseDTO.getPower());
        pileVO.setPileSN(opLocationEvseDTO.getPileSN());
        pileVO.setPowerType(opLocationEvseDTO.getPowerType());
        pileVO.setPileEvseVOS(pileEvseVOs);
        return pileVO;
    }

    private Boolean getSubscribed(BenefitFunctionReqDto agreementFunctionReqVo){
        Result<List<PileBenefitFunctionRespDto>> haveRightsPileList = subscribePileRightsService.getPileFunction(agreementFunctionReqVo);
        log.info("syncPileSnSubscriptionStatus，haveRightsPileList：{}", JSON.toJSONString(haveRightsPileList));

        log.info("添加桩时，haveRightsPileList：{}", JSON.toJSONString(haveRightsPileList));
        if (CollUtil.isNotEmpty(haveRightsPileList.getData())) {
            for (PileBenefitFunctionRespDto agreementFunctionDetailDto : haveRightsPileList.getData()) {
                if (agreementFunctionReqVo.getPileSn().get(0).equals(agreementFunctionDetailDto.getPileSn()) && agreementFunctionDetailDto.getUnavailableTime() != null && agreementFunctionDetailDto.getUnavailableTime() > System.currentTimeMillis()
                        &&  CollUtil.isNotEmpty(agreementFunctionDetailDto.getFunctionIdList()) && agreementFunctionDetailDto.getFunctionIdList().contains(PileChargingRights.GUN_SEARCH)) {
                    return true;
                }
            }
        }
        return false;
    }

    private void handleEvscpSettingDTO(OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO) {

        log.info("===>>>OpLocationEvseRepositoryImpl.handleEvscpSettingDTO opLocationPileEvseElasticDTO : {}",
                JSON.toJSONString(opLocationPileEvseElasticDTO));

        if (opLocationPileEvseElasticDTO == null
                || org.apache.commons.lang3.StringUtils.isBlank(opLocationPileEvseElasticDTO.getPileSn())) {
            return;
        }

        String redisResult = stringRedisTemplate.opsForValue().get(RedisKeyConstant.getPileReportBritishActKey(opLocationPileEvseElasticDTO.getPileSn()));
        if (org.apache.commons.lang3.StringUtils.isBlank(redisResult)
                || JSON.parseObject(redisResult, EvscpSettingDTO.class) == null) {
            return;
        }
        EvscpSettingDTO evscpSettingDTO = JSON.parseObject(redisResult, EvscpSettingDTO.class);
        opLocationPileEvseElasticDTO.setBritainApproveSwitch(evscpSettingDTO.getEnableRegulation());
        opLocationPileEvseElasticDTO.setRandomDelaySwitch(evscpSettingDTO.getEnableRandomDelay());
    }

    /**
     * @param checkPileNameDTO
     * @return
     * @function 校验同一场站下的充电桩名称不能相同
     */
    @Override
    public boolean checkPileNameInLocationUnique(CheckPileNameDTO checkPileNameDTO) {

        log.info("===>>>OpLocationEvseRepositoryImpl.checkPileNameInLocationUnique checkPileNameDTO : {}", JSON.toJSONString(checkPileNameDTO));

        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getLocationId, checkPileNameDTO.getLocationId());
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getName, checkPileNameDTO.getPileName());
        lambdaQueryWrapper.ne(null != checkPileNameDTO.getPileId(), OpLocationPileEvseEntity::getId, checkPileNameDTO.getPileId());
        lambdaQueryWrapper.eq(OpLocationPileEvseEntity::getDeleted, 0);
        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(lambdaQueryWrapper);
        return CollUtil.isEmpty(opLocationPileEvseEntityList);
    }

    private String handlePowerType(Integer category, Integer phase) {
        //1：交流 2：直流 3 交直流'
        if (category == null) {
            category = 1;
        }
        if (phase == null) {
            phase = 3;
        }
        StringBuilder stringBuilder = new StringBuilder();
        switch (category) {
            case 1:
                stringBuilder.append("AC");
                stringBuilder.append("_").append(phase).append("_").append("PHASE");
                break;
            case 2:
                stringBuilder.append("DC");
                stringBuilder.append("_").append(phase).append("_").append("PHASE");
                break;
            default:
                break;
        }
        return stringBuilder.toString();
    }

    private String handlerEvseSn(String connectorId, String pileSN) {
        if (StringUtils.isBlank(connectorId) || StringUtils.isBlank(pileSN)) {
            return null;
        }
        String[] split = connectorId.split("");
        if ("0".equals(split[0])) {
            return pileSN + "_" + split[1];
        }
        return pileSN + "_" + connectorId;
    }

    @Override
    public List<OpLocationPileEvseEntity> getPileInfoByPileNameAndLocationId(String pileName, Long locationId) {
        return opLocationPileEvseMapper.findByLocationIdAndPileName(locationId, pileName);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public List<PileVO> updateEvse(List<OpLocationEvseDTO> opLocationEvseDTOs) {
        List<PileVO> pileVOS = new ArrayList<>();
        opLocationEvseDTOs.forEach(opLocationEvseDTO -> {
            OpLocationEntity opLocationEntity = opLocationMapper.selectById(opLocationEvseDTO.getLocationId());
            if (opLocationEntity == null) {
                throw new MessageCodeException(PileBaseEnum.LOCATION_NOT_EXIST);
            }
            //根据pileSN校验桩是否已存在在别的场站下
            LambdaQueryWrapper<OpLocationPileEvseEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class);
            queryWrapper.eq(OpLocationPileEvseEntity::getDeleted, Boolean.FALSE);
            queryWrapper.eq(OpLocationPileEvseEntity::getPileSn, opLocationEvseDTO.getPileSN());
            OpLocationPileEvseEntity pileEvseEntity = opLocationPileEvseMapper.selectOne(queryWrapper);
            if (!Objects.equals(opLocationEvseDTO.getId(), pileEvseEntity.getId())) {
                throw new MessageCodeException(PileBaseEnum.PILE_EXIST_OTHER_LOCATION);
            }
            PileVO pileVO = updateEvse(opLocationEvseDTO, opLocationEntity, pileEvseEntity);
            pileVOS.add(pileVO);
        });
        return pileVOS;

    }

    @Override
    public Result<Boolean> updatePublicMark(UpdatePublicMarkDTO updatePublicMarkDTO) {
        OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseMapper.selectById(updatePublicMarkDTO.getPileId());
        if (ObjectUtils.isEmpty(opLocationPileEvseEntity)) {
            log.info("updatePublicMark接口，DB查不到桩信息");
            return Result.ofSucceed(false);
        }
        if (Integer.valueOf(1).equals(opLocationPileEvseEntity.getEroamingEnable())
                && Integer.valueOf(0).equals(updatePublicMarkDTO.getPublicMark())) {
            throw new MessageCodeException(PileBaseEnum.PLEASE_CLOSE_THE_INTERCONNECTION_OF_THE_PILE_FIRST);
        }
        opLocationPileEvseEntity.setPublicMark(updatePublicMarkDTO.getPublicMark());
        opLocationPileEvseMapper.updateById(opLocationPileEvseEntity);
        //更新es
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(opLocationPileEvseEntity.getPileSn());
        if (ObjectUtils.isEmpty(opLocationPileEvseElasticDTO)) {
            log.info("updatePublicMark接口，ES查不到桩信息");
            return Result.ofSucceed(false);
        }
        opLocationPileEvseElasticDTO.setPublicMark(updatePublicMarkDTO.getPublicMark());
        opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
        return Result.ofSucceed(true);
    }

    private PileVO updateEvse(OpLocationEvseDTO opLocationEvseDTO, OpLocationEntity opLocationEntity, OpLocationPileEvseEntity pileEvseEntity) {

        log.info("===>>>OpLocationEvseRepositoryImpl.updateEvse opLocationEvseDTO : {} and opLocationEntity : {} and pileEvseEntity : {}",
                JSON.toJSONString(opLocationEvseDTO), JSON.toJSONString(opLocationEntity), JSON.toJSONString(pileEvseEntity));

        // 校验充电桩名称必填
        if (org.apache.commons.lang3.StringUtils.isBlank(opLocationEvseDTO.getPileName())) {
            throw new MessageCodeException(PileBaseEnum.PLEASE_ENTER_THE_CHARGING_PILE_NAME);
        }

        // 校验同一场站下的充电桩名称不能相同
        if (opLocationEvseDTO.getLocationId() != null) {
            CheckPileNameDTO checkPileNameDTO = new CheckPileNameDTO();
            checkPileNameDTO.setPileId(opLocationEvseDTO.getId());
            checkPileNameDTO.setPileName(opLocationEvseDTO.getPileName());
            checkPileNameDTO.setLocationId(opLocationEvseDTO.getLocationId());
            if (!this.checkPileNameInLocationUnique(checkPileNameDTO)) {
                throw new MessageCodeException(PileBaseEnum.NAME_ALREADY_USED);
            }
        }

        //获取并构建充电设备旧数据
        List<Long> tmpIds = JSON.parseArray(pileEvseEntity.getEvseList(), Long.class);
        List<OpLocationEvseEntity> opLocationEvseEntities = this.listByIds(tmpIds);
        List<OpLocationEvseElasticDTO> evseDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(QueryBuilders.termsQuery("id", tmpIds)).build(), OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        Map<Long, OpLocationEvseElasticDTO> evseDtoMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(evseDtoList)) {
            evseDtoList.stream().forEach(e -> evseDtoMap.put(e.getId(), e));
        }
        Map<String, OpLocationEvseEntity> collect = opLocationEvseEntities.stream().collect(Collectors.toMap(OpLocationEvseEntity::getEvseSn, Function.identity()));
        log.info("OpLocationEvseRepositoryImpl.updateEvse.collect = {}", collect);
        ArrayList<Long> evseIds = new ArrayList<>();
        PileVO pileVO = new PileVO();
        List<PileEvseVO> pileEvseVOs = new ArrayList<>();
        List<String> gunTypes = new ArrayList<>();
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = new ArrayList<>();
        final OpLocationEvseEntity[] oneOpLocationEvseEntity = {null};
        if (!CollectionUtils.isEmpty(opLocationEvseDTO.getOpLocationConnectorDTOs())) {
            //如果是Autel品牌,基础信息根据sn获取
            if (Integer.valueOf(0).equals(opLocationEvseDTO.getThirdPart())) {
                List<String> snList = Collections.singletonList(opLocationEvseDTO.getPileSN());
                List<ChargePileDTO> chargePileDTOS = deviceServiceFeign.queryPileList(snList).getData();
                if (CollectionUtils.isEmpty(chargePileDTOS)) {
                    throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
                }
                ChargePileDTO chargePileDTO = chargePileDTOS.get(0);
                opLocationEvseDTO.setProductModel(chargePileDTO.getProductModel());
                opLocationEvseDTO.setPowerType(handlePowerType(chargePileDTO.getCategory(), chargePileDTO.getPhase()));
                opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(e -> {
                    e.setAmperage(new BigDecimal(String.valueOf(chargePileDTO.getElectricCurrent() == null ? 0.0 : chargePileDTO.getElectricCurrent())));
                    e.setVoltage(new BigDecimal(String.valueOf(chargePileDTO.getVoltage() == null ? 0.0 : chargePileDTO.getVoltage())));
                    e.setPower(chargePileDTO.getOutputPower() == null ? 0.0 : chargePileDTO.getOutputPower());
                    e.setPowerType(handlePowerType(chargePileDTO.getCategory(), chargePileDTO.getPhase()));
                });
            } else {
                opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(e -> {
                    e.setPower(opLocationEvseDTO.getPower());
                    e.setPowerType(opLocationEvseDTO.getPowerType());
                });
            }
            opLocationEvseDTO.getOpLocationConnectorDTOs().forEach(opLocationConnectorDTO -> {
                gunTypes.add(String.valueOf(opLocationConnectorDTO.getGunType()));
                //处理充电设备
                String evseSn = handlerEvseSn(opLocationConnectorDTO.getConnectorId(), opLocationEvseDTO.getPileSN());
                OpLocationEvseEntity evseEntity = collect.get(evseSn);
                if (evseEntity == null) {
                    //新增evse
                    evseEntity = new OpLocationEvseEntity();
                    evseEntity.setLocationId(opLocationEvseDTO.getLocationId());
                    evseEntity.setPinCode(opLocationEvseDTO.getPinCode());
                    evseEntity.setEvseSn(handlerEvseSn(opLocationConnectorDTO.getConnectorId(), opLocationEvseDTO.getPileSN()));
                    // 编辑充电桩信息时，不允许更新充电枪所绑定的计费规则id
                    /*
                    OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(opLocationEvseDTO.getPileSN());
                    if (opLocationPileEvseElasticDTO != null && opLocationPileEvseElasticDTO.getTariffId() != null) {
                        evseEntity.setTariffId(opLocationPileEvseElasticDTO.getTariffId());
                    }
                    */
                    this.save(evseEntity);
                    evseIds.add(evseEntity.getId());
                    //添加连接器
                    OpLocationConnectorEntity opLocationConnectorEntity = OpLocationConnectorConvert.toOpLocationConnectorEntity(opLocationConnectorDTO);
                    opLocationConnectorEntity.setPower(opLocationEvseDTO.getPower());
                    opLocationConnectorEntity.setPowerType(opLocationEvseDTO.getPowerType());
                    opLocationConnectorEntity.setLocationEvseId(evseEntity.getId());
                    opLocationConnectorMapper.insert(opLocationConnectorEntity);
                    //封装数据返回
                    PileEvseVO pileEvseVO = new PileEvseVO();
                    pileEvseVO.setEvseId(evseEntity.getId());
                    pileEvseVO.setGunType(opLocationConnectorEntity.getGunType());
                    pileEvseVOs.add(pileEvseVO);
                    if (oneOpLocationEvseEntity[0] == null) {
                        oneOpLocationEvseEntity[0] = evseEntity;
                    }
                    //收集evse es数据
                    OpLocationEvseElasticDTO evseElasticDTO = OpLocationEvseConvert.toOpLocationEvseElastic(evseDtoMap.get(evseEntity.getId()),evseEntity,
                            opLocationEntity, opLocationConnectorEntity);
                    evseElasticDTO.setBrandId(opLocationEvseDTO.getBrandId());
                    evseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
                    evseElasticDTO.setPileSn(opLocationEvseDTO.getPileSN());
                    evseElasticDTO.setPileName(opLocationEvseDTO.getPileName());
                    evseElasticDTO.setState(monitorFeign.getEvseStatus(evseSn).getName());
                    opLocationEvseElasticDTOS.add(evseElasticDTO);
                } else {
                    //更新
                    evseEntity.setLocationId(opLocationEvseDTO.getLocationId());
                    evseEntity.setPinCode(opLocationEvseDTO.getPinCode());
                    evseEntity.setEvseSn(evseSn);
                    // 编辑充电桩信息时，不允许更新充电枪所绑定的计费规则id
                    // evseEntity.setTariffId(opLocationEvseDTO.getTariffId());
                    this.updateById(evseEntity);
                    evseIds.add(evseEntity.getId());
                    //更新连接器
                    OpLocationConnectorEntity opLocationConnectorEntity = OpLocationConnectorConvert.toOpLocationConnectorEntity(opLocationConnectorDTO);
                    opLocationConnectorEntity.setPower(opLocationEvseDTO.getPower());
                    opLocationConnectorEntity.setPowerType(opLocationEvseDTO.getPowerType());
                    opLocationConnectorEntity.setLocationEvseId(evseEntity.getId());
                    LambdaUpdateWrapper<OpLocationConnectorEntity> connectorUpdate = Wrappers.lambdaUpdate();
                    connectorUpdate.eq(OpLocationConnectorEntity::getDeleted, Boolean.FALSE);
                    connectorUpdate.eq(OpLocationConnectorEntity::getLocationEvseId, evseEntity.getId());
                    opLocationConnectorMapper.update(opLocationConnectorEntity, connectorUpdate);
                    //封装数据返回
                    PileEvseVO pileEvseVO = new PileEvseVO();
                    pileEvseVO.setEvseId(evseEntity.getId());
                    pileEvseVO.setGunType(opLocationConnectorEntity.getGunType());
                    pileEvseVOs.add(pileEvseVO);
                    if (oneOpLocationEvseEntity[0] == null) {
                        oneOpLocationEvseEntity[0] = evseEntity;
                    }
                    //收集evse es数据
                    OpLocationEvseElasticDTO evseElasticDTO = OpLocationEvseConvert.toOpLocationEvseElastic(evseDtoMap.get(evseEntity.getId()),evseEntity,
                            opLocationEntity, opLocationConnectorEntity);
                    evseElasticDTO.setBrandId(opLocationEvseDTO.getBrandId());
                    evseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
                    evseElasticDTO.setPileSn(opLocationEvseDTO.getPileSN());
                    evseElasticDTO.setPileName(opLocationEvseDTO.getPileName());
                    evseElasticDTO.setState(monitorFeign.getEvseStatus(evseEntity.getId()).getName());
                    opLocationEvseElasticDTOS.add(evseElasticDTO);
                    collect.remove(evseSn);
                }
            });
        }
        //删除db evse
        ArrayList<Long> deleteEvseIds = new ArrayList<>();
        log.info("删除的evse = {}", collect);
        collect.forEach((k, v) -> {
            deleteEvseIds.add(v.getId());
            this.delete(v.getId());
            opLocationConnectorMapper.deleteByLocationEvseId(v.getId());
        });
        OpLocationPileEvseEntity opLocationPileEvseEntity = OpLocationPileEvseConvert.toOpLocationPileEvseEntity(opLocationEvseDTO);
        opLocationPileEvseEntity.setEvseList(JSON.toJSONString(evseIds));
        log.info(" opLocationPileEvseMapper.updateById  {}", JSON.toJSONString(opLocationPileEvseEntity));
        opLocationPileEvseMapper.updateById(opLocationPileEvseEntity);

        log.info("===>>> OpLocationEvseRepositoryImpl.updateEvse.opLocationPileEvseEntity : {}",
                JSON.toJSONString(opLocationPileEvseEntity));

        //同步桩数据到es
        Optional<OpLocationPileEvseElasticDTO> pileOptional = opLocationPileEvseElastic.findById(pileEvseEntity.getId());
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO;
        opLocationPileEvseElasticDTO = pileOptional.map(pileEvseElasticDTO -> OpLocationPileEvseConvert
                .toOpLocationPileEvseElastic(pileEvseElasticDTO, opLocationPileEvseEntity, oneOpLocationEvseEntity[0], opLocationEntity))
                .orElseGet(() -> OpLocationPileEvseConvert.toOpLocationPileEvseElastic(opLocationPileEvseEntity, oneOpLocationEvseEntity[0], opLocationEntity));
        opLocationPileEvseElasticDTO.setPower(opLocationEvseDTO.getPower());
        opLocationPileEvseElasticDTO.setPowerType(opLocationEvseDTO.getPowerType());
        opLocationPileEvseElasticDTO.setBrandName(opLocationEvseDTO.getBrandName());
        opLocationPileEvseElasticDTO.setPublicMark(opLocationEvseDTO.getPublicMark());
        opLocationPileEvseElasticDTO.setCreatedAt(pileEvseEntity.getCreatedAt());
        opLocationPileEvseElasticDTO.setName(opLocationEvseDTO.getPileName());
        opLocationPileEvseElastic.save(opLocationPileEvseElasticDTO);
        log.info("OpLocationEvseRepositoryImpl.updateEvse.opLocationPileEvseElasticDTO={}", JSON.toJSONString(opLocationPileEvseElasticDTO));

        // 缓存桩运营商用户
        String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(opLocationEvseDTO.getPileSN());
        stringRedisTemplate.opsForValue().set(snOperatorIdKey, opLocationPileEvseElasticDTO.getOperatorId().toString());

        //同步evse数据到es
        opLocationEvseElasticDTOS.forEach(e -> {
            final OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findById(e.getId()).orElse(null);
            //忽略的字段不在DB里
            if (Objects.nonNull(opLocationEvseElasticDTO)) {
                BeanUtils.copyProperties(e, opLocationEvseElasticDTO, "stateUploadTime", "lastOrderSeq", "offlineUploadTime", "lastState");
                opLocationEvseElastic.save(opLocationEvseElasticDTO);
                log.info("OpLocationEvseRepositoryImpl.updateEvse.opLocationEvseElasticDTO={}", JSON.toJSONString(opLocationEvseElasticDTO));
            } else {
                opLocationEvseElastic.save(e);
                log.info("OpLocationEvseRepositoryImpl.updateEvse.opLocationEvseElasticDTO={}", JSON.toJSONString(opLocationEvseElasticDTO));
            }
        });
        log.info("删除es的evse = {}", deleteEvseIds);
        deleteEvseIds.forEach(opLocationEvseElastic::deleteById);
        pileVO.setPileId(opLocationPileEvseEntity.getId());
        pileVO.setPileName(opLocationPileEvseEntity.getName());
        pileVO.setPower(opLocationEvseDTO.getPower());
        pileVO.setPileSN(opLocationEvseDTO.getPileSN());
        pileVO.setPowerType(opLocationEvseDTO.getPowerType());
        pileVO.setBrandId(opLocationPileEvseElasticDTO.getBrandId());
        pileVO.setBrandName(opLocationPileEvseElasticDTO.getBrandName());
        pileVO.setLocationId(opLocationPileEvseElasticDTO.getLocationId());
        pileVO.setPileEvseVOS(pileEvseVOs);
        //同步到设备扩展类
        List<Long> pileIds = opLocationEvseElasticDTOS.stream().map(OpLocationEvseElasticDTO::getId).collect(Collectors.toList());
        this.syncEvseExpand(EvseExpandDTO.builder().pileIds(pileIds).build());

        //发送mq
        mqSender.send(AmqpConstant.PILE_NAME_UPDATE, pileVO);
        return pileVO;
    }

    /**
     * 构建(站点id，站点设备集合)map
     *
     * @param opLocationEvseElasticDTOS 护色剂集合
     * @return (站点id ， 站点设备)map
     */
    private Map<Long, List<OpLocationEvseElasticDTO>> buildESStationIdEVSEListMap(List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS) {
        //构建每个站点的设备map
        Map<Long, List<OpLocationEvseElasticDTO>> esStationEVSEListMap = new HashMap<>();
        opLocationEvseElasticDTOS.forEach(opLocationEvseElasticDTO -> {
            Long locationId = opLocationEvseElasticDTO.getLocationId();
            List<OpLocationEvseElasticDTO> stationEVSEList = esStationEVSEListMap.get(locationId);
            if (stationEVSEList == null) {
                stationEVSEList = new ArrayList<>();
            }
            stationEVSEList.add(opLocationEvseElasticDTO);
            esStationEVSEListMap.put(locationId, stationEVSEList);
        });
        return esStationEVSEListMap;
    }

    @Override
    public List<OpEvseInfoDTO> queryPileListByOperationId(Long operatorId, List<Long> locationIds) {
        List<OpLocationEvseEntity> opLocationEvseEntits = opLocationPileEvseMapper.queryPileListByOperationId(operatorId, locationIds);
        List<OpEvseInfoDTO> resultList = Lists.newArrayList();
        if (org.apache.commons.collections.CollectionUtils.isNotEmpty(opLocationEvseEntits)) {
            for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntits) {
                OpEvseInfoDTO opEvseInfoDTO = new OpEvseInfoDTO();
                opEvseInfoDTO.setEvseSn(opLocationEvseEntity.getEvseSn());
                opEvseInfoDTO.setLocationId(opLocationEvseEntity.getLocationId());
                resultList.add(opEvseInfoDTO);
            }
        }
        return resultList;
    }

    @Override
    public List<OpLocationEvseDTO> queryEvseByLocationId(Long locationId) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery(BaseConstant.LOCATIONID, locationId));
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationEvseDTO> evseDTOList = new ArrayList<>();
        iterable.forEach(opLocationEvseElasticDTO -> evseDTOList.add(OpLocationEvseConvert.toOpLocationEvseDTO(opLocationEvseElasticDTO)));
        return evseDTOList;
    }

    @Override
    public List<OpLocationEvseDTO> queryEvseIncludeDeletedByLocationId(Long locationId) {
        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQuery = new LambdaQueryWrapper<>();
        lambdaQuery.eq(OpLocationEvseEntity::getLocationId, locationId);
        List<OpLocationEvseEntity> opLocationEvseEntities = getBaseMapper().selectList(lambdaQuery);
        List<OpLocationEvseDTO> evseDTOList = new ArrayList<>();
        opLocationEvseEntities.forEach(item -> {
            OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
            opLocationEvseDTO.setId(item.getId());
            opLocationEvseDTO.setLocationId(item.getLocationId());
            opLocationEvseDTO.setPileSN(item.getEvseId());
            opLocationEvseDTO.setEvseSn(item.getEvseSn());
            evseDTOList.add(opLocationEvseDTO);
        });
        return evseDTOList;
    }

    @Override
    public List<LocationEvseInfoVO> findEvseList(List<Long> locationIds) {
        Set<Long> params = new HashSet<>(locationIds);
        List<OpLocationPileEvseElasticDTO> all = opLocationPileEvseElastic.findAllByLocationIdIn(params);
        log.info("findEvseList,all={}", JSON.toJSONString(all));
        if (CollectionUtils.isEmpty(all)) {
            return Lists.newArrayList();
        }
        List<LocationEvseInfoVO> result = new ArrayList<>();
        all.stream().filter(e -> !ObjectUtils.isEmpty(e.getPileSn())).forEach(k -> {
            LocationEvseInfoVO entity = new LocationEvseInfoVO();
            entity.setId(k.getId());
            entity.setPileSn(k.getPileSn());
            entity.setLocationId(k.getLocationId());
            if (org.springframework.util.StringUtils.hasText(k.getName())) {
                entity.setName(k.getName());
            }
            result.add(entity);
        });
        log.info("findEvseList,result={}", JSON.toJSONString(result));
        return result;
    }

    @Override
    public Page<OpPileAssociatedRuleDTO> queryPileByTariffIdForPage(OpPileAssociatedRuleParamDTO opPileAssociatedRuleParamDTO) {
        log.info("OpLocationEvseRepositoryImpl queryPileByTariffIdForPage.queryPileByTariffIdForPage = {}",
                JSON.toJSONString(opPileAssociatedRuleParamDTO));
        Long tariffId = opPileAssociatedRuleParamDTO.getTariffId();
        Boolean isExcludedTariff = opPileAssociatedRuleParamDTO.getIsExcludedTariff();
        if (tariffId == null) {
            return new Page<>();
        }
        if (opPileAssociatedRuleParamDTO.getPage() < 1) {
            opPileAssociatedRuleParamDTO.setPage(1);
        }
        Long currentUserId = UserUtil.getSellerId();

        //由于一个桩的多枪绑定多个计费规则之后，桩的计费规则是其中的一个枪的计费规则。
        // 所以现在不能使用桩来匹配计费规则，否则计费设置页面只能在其中的一个计费规则找到枪1，另外一个计费规则找不到枪2的情况。

        //一、现在通过枪来找计费规则，从而带出桩
        BoolQueryBuilder evseQuery = QueryBuilders.boolQuery();
        boolean havePermission = true;
        if (Boolean.TRUE.equals(isExcludedTariff)) {
            evseQuery.mustNot(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        } else {
            havePermission = false;
            evseQuery.must(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
        }
        evseQuery.must(QueryBuilders.termQuery("operatorId", currentUserId));
        boolean sellerAdmin = LoginUserUtil.isSellerAdmin();
        if (sellerAdmin) {
            havePermission = true;
        }
        List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
        log.info("OpLocationEvseRepositoryImpl queryPileByTariffId locationIdList = "
                + JSON.toJSONString(locationIdList));
        if (CollectionUtils.isEmpty(locationIdList) && havePermission) {
            return null;
        }
        if (havePermission) {
            evseQuery.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIdList));
        }
        log.info("OpLocationEvseRepositoryImpl queryPileByTariffId evseQuery = "
                + JSON.toJSONString(evseQuery));
        Iterable<OpLocationEvseElasticDTO> evseIterable =
//                opLocationEvseElastic.search(evseQuery);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(evseQuery).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        //找出绑定当前计费规则的枪的桩
        HashSet<String> pileSnSet = new HashSet<>();
        if (evseIterable != null) {
            evseIterable.forEach(evse -> {
                if (StringUtils.isNotBlank(evse.getEvseSn())) {
                    pileSnSet.add(evse.getEvseSn().split("_")[0]);
                }
            });
        }

        //二、根据已有的桩进行匹配
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        boolQueryBuilder.must(QueryBuilders.termsQuery("pileSn", pileSnSet));
        boolQueryBuilder.must(QueryBuilders.termQuery("operatorId", currentUserId));
        if (CollectionUtils.isEmpty(locationIdList)) {
            return new Page<>();
        }
        if (havePermission) {
            boolQueryBuilder.must(QueryBuilders.termsQuery(BaseConstant.LOCATIONID, locationIdList));
        }

        log.info("opPileAssociatedRuleParamDTO.getPileSnSearch() = {}", opPileAssociatedRuleParamDTO.getPileSnSearch());
        BoolQueryBuilder pileQuery = QueryBuilders.boolQuery();
        if (StringUtils.isNotBlank(opPileAssociatedRuleParamDTO.getPileSnSearch())) {
            pileQuery.should(QueryBuilders.wildcardQuery("pileSn",
                    "*" + QueryParserBase.escape(opPileAssociatedRuleParamDTO.getPileSnSearch()) + "*"));
        }
        //新版查询增加根据桩名模糊查询
        if (StringUtils.isNotBlank(opPileAssociatedRuleParamDTO.getPileSnSearch())) {
            pileQuery.should(QueryBuilders.wildcardQuery("name",
                    "*" + QueryParserBase.escape(opPileAssociatedRuleParamDTO.getPileSnSearch()) + "*"));
        }
        boolQueryBuilder.must(pileQuery);

        log.info("OpLocationEvseRepositoryImpl queryPileByTariffIdForPage boolQueryBuilder = {}", JSON.toJSONString(boolQueryBuilder));

        Pageable pageable = PageRequest.of(0,
                5000);

//        org.springframework.data.domain.Page<OpLocationPileEvseElasticDTO> iterable =
//                opLocationPileEvseElastic.search(boolQueryBuilder, pageable);
        SearchHits<OpLocationPileEvseElasticDTO> searchHits =
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(boolQueryBuilder).withPageable(pageable).build(), OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> iterable = searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());

        log.info("OpLocationEvseRepositoryImpl.queryPileByTariffIdForPage.iterable = {}", iterable);

        List<OpPileAssociatedRuleDTO> opLocationEvseDTOList = new LinkedList<>();

        //新版列表增枪号和枪类型，并且每一个枪展示一行。
        for (OpLocationPileEvseElasticDTO pileEvseElasticDTO : iterable) {
            List<Long> evseIdList = Optional.ofNullable(JSON.parseArray(pileEvseElasticDTO.getEvseList()))
                    .orElse(JSON.parseArray("[]")).toJavaList(Long.class);
            if (CollUtil.isNotEmpty(evseIdList)) {
                boolean finalHavePermission = havePermission;
                evseIdList.forEach(id -> {
                    //查出对应计费规则的枪
                    //此时，桩下的枪可能绑定的计费规则是不一样的，所以需要再构造一次条件
                    BoolQueryBuilder evseQuery2 = QueryBuilders.boolQuery();
                    evseQuery2.must(QueryBuilders.termQuery(BaseConstant.TARIFFID, tariffId));
                    evseQuery2.must(QueryBuilders.termQuery("id", id));
                    OpLocationEvseElasticDTO evseElasticDTO = opLocationEvseElastic.findByTariffIdAndId(tariffId, id);
                    if (evseElasticDTO != null) {
                        OpPileAssociatedRuleDTO opLocationEvseDTO = OpLocationPileEvseConvert.toOpPileAssociatedRuleDTOV2(pileEvseElasticDTO, evseElasticDTO);
                        if (CollectionUtils.isEmpty(locationIdList) || (!finalHavePermission && !locationIdList.contains(pileEvseElasticDTO.getLocationId()))) {
                            opLocationEvseDTO.setHavePermission(false);
                        }
                        opLocationEvseDTOList.add(opLocationEvseDTO);
                    }
                });
            }
        }

        log.info("OpLocationEvseRepositoryImpl.queryPileByTariffIdForPage.opLocationEvseDTOList = {}", JSON.toJSONString(opLocationEvseDTOList));

        Page<OpPileAssociatedRuleDTO> opLocationEvseDTOPage = new Page<>();
        if (CollUtil.isNotEmpty(opLocationEvseDTOList)) {
            opLocationEvseDTOPage
                    .setRecords(ListUtils.partition(opLocationEvseDTOList, opPileAssociatedRuleParamDTO.getPageSize()).get(opPileAssociatedRuleParamDTO.getPage() - 1))
                    .setTotal(opLocationEvseDTOList.size())
                    .setSize(opPileAssociatedRuleParamDTO.getPageSize())
                    .setCurrent(opPileAssociatedRuleParamDTO.getPage())
                    .setPages(ListUtils.partition(opLocationEvseDTOList, opPileAssociatedRuleParamDTO.getPageSize()).size());
            return opLocationEvseDTOPage;
        }
        return new Page<>();
    }

    @Override
    @Deprecated
    public Boolean updateEvseState(String evseSn, String state) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.filter(QueryBuilders.termsQuery("evseSn", evseSn));
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<OpLocationEvseElasticDTO> list = new ArrayList<>();
        iterable.forEach(list::add);
        if (CollectionUtils.isEmpty(list)) {
            return Boolean.FALSE;
        }
        OpLocationEvseElasticDTO opLocationEvseElasticDTO = list.get(0);
        opLocationEvseElasticDTO.setState(state);
        opLocationEvseElastic.save(opLocationEvseElasticDTO);
        //同步到设备扩展类
        this.syncEvseExpand(EvseExpandDTO.builder().pileIds(Arrays.asList(opLocationEvseElasticDTO.getId())).build());
        return Boolean.TRUE;
    }


    @Override
    public Boolean batchUpdateEvseStateByUpdateAt(Collection<EvseStateDTO> evseStateDTOs) {
        if (CollectionUtils.isEmpty(evseStateDTOs)) {
            return Boolean.TRUE;
        }
        Set<String> evseSnSet = evseStateDTOs.stream().map(EvseStateDTO::getEvseSn).collect(Collectors.toSet());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        TermsQueryBuilder termQueryBuilder = new TermsQueryBuilder("evseSn", evseSnSet);
        queryBuilder.filter(termQueryBuilder);//.must(rangeQueryBuilder);

        NativeSearchQuery nativeSearchQuery = new NativeSearchQueryBuilder()
                .withIndicesBoost(new IndexBoost(OpLocationEvseElasticDTO.class.getAnnotation(Document.class).indexName(), 1.0f))
                .withQuery(queryBuilder)
                .build();
        log.info(nativeSearchQuery.getQuery().toString());
        List<OpLocationEvseElasticDTO> list =
//                elasticsearchRestTemplate.queryForList(nativeSearchQuery, OpLocationEvseElasticDTO.class);
                elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        if (CollectionUtils.isEmpty(list)) {
            return Boolean.FALSE;
        }
        Map<String, OpLocationEvseElasticDTO> collect = list.stream().collect(Collectors.toMap(OpLocationEvseElasticDTO::getEvseSn, Function.identity()));
        for (EvseStateDTO evseStateDTO : evseStateDTOs) {
            if (collect.containsKey(evseStateDTO.getEvseSn())) {
                OpLocationEvseElasticDTO opLocationEvseElasticDTO = collect.get(evseStateDTO.getEvseSn());
                evseStateDTO.setId(opLocationEvseElasticDTO.getId());
                String currentState = evseStateDTO.getState();
                if (Objects.isNull(currentState)) {
                    return Boolean.FALSE;
                }
                Long updatedAt = evseStateDTO.getUpdatedAt();
                opLocationEvseElasticDTO.setUpdatedAt(updatedAt);
                if (EvseDeviceStatusEnum.DEFAULT.getName().equals(currentState)) {
                    //新状态是离线则更新离线时间
                    opLocationEvseElasticDTO.setOfflineUploadTime(updatedAt);
                }
                //相当于上次状态
                final String state = opLocationEvseElasticDTO.getState();
                //相当于上上次状态
                final String lastState = opLocationEvseElasticDTO.getLastState();
                //上次状态为离线并且存在上上次状态比，需要三个状态一起比较
                if (EvseDeviceStatusEnum.DEFAULT.getName().equals(state)) {
                    //满足2种情况之一不更新状态上报时间，1.当前状态=上上次状态 || 2.当前状态 = 相对意义相同状态,反之更新
                    if (Objects.nonNull(lastState) && !currentState.equals(lastState) && !currentState.equals(getRelativelySimilarInMeaningState(lastState))) {
                        opLocationEvseElasticDTO.setStateUploadTime(updatedAt);
                    }
                } else {
                    //上次不是离线只需要判断是否当前状态 = 相对意义相同状态,反之更新
                    if (Objects.nonNull(state) && !state.equals(getRelativelySimilarInMeaningState(currentState))) {
                        opLocationEvseElasticDTO.setStateUploadTime(updatedAt);
                    }
                }
                opLocationEvseElasticDTO.setLastState(state);
                opLocationEvseElasticDTO.setState(currentState);
            }
        }
        if (!CollectionUtils.isEmpty(list)) {
            opLocationEvseElastic.saveAll(list);
            opLocationEvseExpandElasticService.batchCheckUpdate(list);
        }
        return Boolean.TRUE;
    }

    @Override
    public Boolean updateEvseStateByUpdateAt(String evseSn, String currentState, Long updatedAt) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        TermQueryBuilder termQueryBuilder = new TermQueryBuilder("evseSn", evseSn);
        RangeQueryBuilder rangeQueryBuilder = new RangeQueryBuilder("updatedAt").lte(updatedAt);
        queryBuilder.filter(termQueryBuilder).filter(rangeQueryBuilder);//.must(rangeQueryBuilder);
        if (Objects.isNull(currentState)) {
            return Boolean.FALSE;
        }
        //查询索引
        final OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findByEvseSn(evseSn);
        if (Objects.isNull(opLocationEvseElasticDTO)) {
            return Boolean.FALSE;
        }
        log.info("updateEvseStateByUpdateAt opLocationEvseElasticDTO:{}", JSON.toJSONString(opLocationEvseElasticDTO));

        String index = OpLocationEvseElasticDTO.class.getAnnotation(Document.class).indexName();
        UpdateByQueryRequest updateRequest = new UpdateByQueryRequest(index);
        updateRequest.setQuery(queryBuilder);
        StringBuilder scriptBuilder = new StringBuilder();
        Map<String, Object> parameters = new HashMap<>();
        //1.更新更新时间
        combineParams(scriptBuilder, "updateAt", parameters, updatedAt);
        if (EvseDeviceStatusEnum.DEFAULT.getName().equals(currentState)) {
            //新状态是离线则更新离线时间
            combineParams(scriptBuilder, "offlineUploadTime", parameters, updatedAt);
        }
        //相当于上次状态
        final String state = opLocationEvseElasticDTO.getState();
        //相当于上上次状态
        final String lastState = opLocationEvseElasticDTO.getLastState();
        log.info("EvseSn:{},lastState:{},state:{},currentState:{}", evseSn, lastState, state, currentState);
        //上次状态为离线并且存在上上次状态比，需要三个状态一起比较
        if (EvseDeviceStatusEnum.DEFAULT.getName().equals(state)) {
            //满足2种情况之一不更新状态上报时间，1.当前状态=上上次状态 || 2.当前状态 = 相对意义相同状态,反之更新
            if (Objects.nonNull(lastState) && !currentState.equals(lastState) && !currentState.equals(getRelativelySimilarInMeaningState(lastState))) {
                combineParams(scriptBuilder, "stateUploadTime", parameters, updatedAt);
            }
        } else if (!EvseDeviceStatusEnum.DEFAULT.getName().equals(currentState)) {
            //上次不是离线且当前状态也不是离线 只需要判断是否当前状态 = 相对意义相同状态,反之更新
            if (Objects.nonNull(state) && !state.equals(getRelativelySimilarInMeaningState(currentState))) {
                combineParams(scriptBuilder, "stateUploadTime", parameters, updatedAt);
            }
        }
        combineParams(scriptBuilder, "lastState", parameters, state);
        combineParams(scriptBuilder, "state", parameters, currentState);
        String script = scriptBuilder.toString();
        log.info("updateEvseStateByUpdateAt script:{}", script);
        updateRequest.setScript(new Script(ScriptType.INLINE, "painless", script, parameters));
        updateRequest.setMaxRetries(3);
        BulkByScrollResponse bulkByScrollResponse;
        int retryCount = 30;
        OpLocationEvseElasticDTO opLocationEvseElasticByEvseSn;
        do {
            try {

                bulkByScrollResponse = restHighLevelClient.updateByQuery(updateRequest, RequestOptions.DEFAULT);
                log.info("updateEvseStateByUpdateAt result:{}", JSON.toJSONString(bulkByScrollResponse.getUpdated()));
                if (bulkByScrollResponse.getVersionConflicts() <= 0) {
                    log.info("updateEvseStateByUpdateAt success retryCount:{}", 300 - retryCount);
                    break;
                }
                retryCount--;
                if(retryCount <= 0){
                    break;
                }
                //修改睡眠50毫秒
                LockSupport.parkNanos(50_000000);
                opLocationEvseElasticByEvseSn = opLocationEvseElastic.findByEvseSn(evseSn);
                boolean isBreak = Objects.isNull(opLocationEvseElasticByEvseSn) || (opLocationEvseElasticByEvseSn.getStateUploadTime() >= updatedAt);
                if(isBreak){
                    break;
                }
            } catch (IOException e) {
                retryCount--;
                log.info("updateEvseStateByUpdateAt failed,", e);
            }
        } while (true);

        //更新扩展索引库的枪状态
        try {
            Optional<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOOptional = opLocationEvseExpandElastic.findById(opLocationEvseElasticDTO.getId());
            if (opLocationEvseExpandElasticDTOOptional.isPresent()){
                OpLocationEvseExpandElasticDTO opLocationEvseExpandElasticDTO = opLocationEvseExpandElasticDTOOptional.get();
                opLocationEvseExpandElasticDTO.setGunState(currentState);
                opLocationEvseExpandElastic.save(opLocationEvseExpandElasticDTO);
            }
        } catch (Exception e) {
            log.info("===>>>updateEvseStateByUpdateAt 更新扩展索引库的枪状态异常：",e);
        }
        return Boolean.TRUE;
    }

    private StringBuilder combineParams(StringBuilder sb, String param, Map<String, Object> params, Object value) {
        if (Objects.nonNull(param)) {
            String format = "ctx._source.%s=params.%s;";
            sb.append(String.format(format, param, param));
            params.put(param, value);
        }
        return sb;
    }

    /**
     * 获取意义相同的相对状态
     * CHARGING = SUSPENDED_EVSE ：充电中
     * FINISHING = SUSPENDED_EV ： 占用（停充占用）
     *
     * @param state 状态
     * @return {@link String}
     */
    private String getRelativelySimilarInMeaningState(String state) {
        if (EvseDeviceStatusEnum.CHARGING.getName().equals(state)) {
            return EvseDeviceStatusEnum.SUSPENDED_EVSE.getName();
        }
        if (EvseDeviceStatusEnum.SUSPENDED_EVSE.getName().equals(state)) {
            return EvseDeviceStatusEnum.CHARGING.getName();
        }
        if (EvseDeviceStatusEnum.SUSPENDED_EV.getName().equals(state)) {
            return EvseDeviceStatusEnum.FINISHING.getName();
        }
        if (EvseDeviceStatusEnum.FINISHING.getName().equals(state)) {
            return EvseDeviceStatusEnum.SUSPENDED_EV.getName();
        }
        return state;
    }

    @Override
    public Boolean verifyPileSn(String pileSn) {
        List<String> snList = Collections.singletonList(pileSn);
        List<ChargePileDTO> chargePileDTOS = deviceServiceFeign.queryPileList(snList).getData();
        log.info("feigh调用device服务:{}", JSON.toJSONString(chargePileDTOS));
        if (CollectionUtils.isEmpty(chargePileDTOS)) {
            return Boolean.FALSE;
        } else {
            return Boolean.TRUE;
        }
    }

    @Override
    public Boolean verifyThirdPileSn(String pileSn, Long brandId) {
        //data-service是否已被绑定为家桩
        Result<Boolean> restResult = homePileClient.queryBind(pileSn);
        if (restResult != null && restResult.getCode().equals(HttpStatus.SC_OK) && restResult.getData().equals(Boolean.TRUE)) {
            throw new MessageCodeException(PileBaseEnum.CHARGEPILE_HAS_BIND);
        }
        return Boolean.TRUE;
    }

    @Override
    public OpLocationEvseDetailVO getDetailsFromDbById(Long id) {
        OpLocationEvseEntity evseEntity = opLocationEvseMapper.selectById(id);
        OpLocationEvseDetailVO evseVO = new OpLocationEvseDetailVO();
        BeanUtils.copyProperties(evseEntity, evseVO);
        return evseVO;
    }

    @Override
    public List<HubPileDTO> getPileInfoByPileSn(List<String> pileSnList) {
        log.info("getPileInfoByPileSn 入参：{}", JSON.toJSONString(pileSnList));
        List<HubPileDTO> pileInfoByPileSn = opLocationEvseMapper.getPileInfoByPileSn(pileSnList);
        log.info("getPileInfoByPileSn result: {}", JSON.toJSONString(pileInfoByPileSn));
        return pileInfoByPileSn;
    }

    @Override
    public Boolean syncEvseExpand(EvseExpandDTO expandDTO) {
        log.info("syncEvseExpand,expandDTO={}", JSON.toJSONString(expandDTO));
        List<Long> locationIds = expandDTO.getLocationIds();
        List<Long> pileIds = expandDTO.getPileIds();
        //同步前是否删除已有数据
        if (expandDTO.getDelete() != null && expandDTO.getDelete()) {
            opLocationEvseExpandElastic.deleteAll();
        }
        //按桩ID同步
        if (CollectionUtil.isNotEmpty(pileIds)) {
            return this.syncEvseExpandByEvseIds(pileIds);
        }
        //按场站ID同步
        List<OpLocationElasticDTO> dtoList = this.getLocationListFromEs(locationIds);
        Map<Long, OpLocationElasticDTO> locationMap = dtoList.stream().collect(Collectors.toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
        log.info("syncEvseExpand,locationMap size={}", locationMap.size());
        if (locationMap.size() > 0) {
            locationMap.forEach((k, v) -> {
                List<OpLocationEvseElasticDTO> evseList = opLocationEvseElastic.findAllByLocationId(k);
                List<OpLocationEvseExpandElasticDTO> list = new ArrayList<>();
                if (!ObjectUtils.isEmpty(evseList)) {
                    evseList.forEach(evseDto -> {
                        OpLocationEvseExpandElasticDTO dto = new OpLocationEvseExpandElasticDTO();
                        BeanUtils.copyProperties(v, dto);
                        dto.setId(evseDto.getId());
                        dto.setLocationId(v.getId());
                        dto.setPower(evseDto.getPower());
                        dto.setGunState(evseDto.getState());
                        dto.setGunType(evseDto.getGunType());
                        dto.setTariffId(evseDto.getTariffId());
                        dto.setSubscriptionCheck(evseDto.getSubscriptionCheck());
                        list.add(dto);
                    });
                    if (list.size() > 0) {
                        opLocationEvseExpandElastic.saveAll(list);
                        log.info("syncEvseExpand,locationId={}, svseSize={}", k, list.size());
                    }
                }
            });
        }
        return true;
    }

    /**
     * @param id 计费规则模型的主键Id
     * @return 计费规则模型关联的充电设备数量
     * @function 查询计费规则模型关联的充电设备数量
     */
    @Override
    public Integer getEvseNumberByTariffId(Long id) {

        log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.getEvseNumberByTariffId id : {}", JSON.toJSONString(id));

        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(OpLocationEvseEntity::getTariffId, id)
                .eq(OpLocationEvseEntity::getDeleted, 0);

        return opLocationEvseMapper.selectCount(lambdaQueryWrapper);
    }

    /**
     * @param tariffIdList 计费规则模型的主键Id集合
     * @return 根据计费规则模型的主键Id集合查询绑定的枪的Sn结果
     * @function 根据计费规则模型的主键Id集合查询绑定的枪的Sn
     */
    @Override
    public List<OpLocationEvseEntity> getPileSnListByTariffIdList(List<Long> tariffIdList) {

        log.info("====>>>> OpLocationEvseRepositoryImpl.getPileSnListByTariffIdList tariffIdList : {}",
                JSON.toJSONString(tariffIdList));

        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .in(OpLocationEvseEntity::getTariffId, tariffIdList)
                .eq(OpLocationEvseEntity::getDeleted, 0);
        return opLocationEvseMapper.selectList(lambdaQueryWrapper);
    }

    @Override
    public List<OpLocationEvseElasticDTO> findList(List<String> pileSnList) {
        List<OpLocationEvseElasticDTO> resultList = new ArrayList<>();
        if (CollectionUtils.isEmpty(pileSnList)) return resultList;
        StopWatch stopWatch = new StopWatch("按桩SN查询枪信息");
        stopWatch.start("查询枪列表");
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("pileSn", pileSnList));
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(PageRequest.of(0, 100))
                .build();
        Iterator<OpLocationEvseElasticDTO> stream =
//                elasticsearchRestTemplate.stream(searchQuery, OpLocationEvseElasticDTO.class);
                elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).iterator();

        while (stream.hasNext()) {
            OpLocationEvseElasticDTO dto = stream.next();
            resultList.add(dto);
        }
        stopWatch.stop();
        log.info(stopWatch.prettyPrint());
        return resultList;
    }

    @Override
    public Boolean isHubject(Long locationId) {
        OpLocationEntity opLocationEntity = opLocationMapper.selectById(locationId);
        if (opLocationEntity != null && opLocationEntity.getHubjectCheck() != null && Boolean.TRUE.equals(opLocationEntity.getHubjectCheck())) {
            return true;
        }
        return false;
    }

    @Override
    public boolean savBatchEs(List<OpLocationEvseElasticDTO> evseDtoList) {
        opLocationEvseElastic.saveAll(evseDtoList);
        return true;
    }

    @Override
    public CostModelRuleDTO getTatiffRuleByTariffId(Long tariffId) {
        CostModelRuleDTO costModelRuleDTO = new CostModelRuleDTO();
        costModelRuleDTO.setId(tariffId);
        Result<CostModelRuleDTO> costModelRuleDTOResult = tariffFeignClient.queryDetail(costModelRuleDTO);
        if (!ObjectUtils.isEmpty(costModelRuleDTOResult) && !ObjectUtils.isEmpty(costModelRuleDTOResult.getData())) {
            return costModelRuleDTOResult.getData();
        }
        return costModelRuleDTO;
    }

    @Override
    public List<OpLocationEvseElasticDTO> findAll() {
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withPageable(PageRequest.of(0, 100))
                .build();
        Iterator<OpLocationEvseElasticDTO> evseResult =
//                elasticsearchRestTemplate.stream(searchQuery, OpLocationEvseElasticDTO.class);
                elasticsearchRestTemplate.searchForStream(searchQuery, OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).iterator();

        List<OpLocationEvseElasticDTO> resultList = new ArrayList<>();
        evseResult.forEachRemaining(resultList::add);
        return resultList;
    }

    @Override
    public List<TariffsEvseNumDTO> getEvseNumByTariffIdsV2(List<Long> tariffGroupIds) {

        log.info("===>>>OpLocationEvseRepositoryImpl.getEvseNumByTariffIdsV2 tariffGroupIds: {}",
                JSON.toJSONString(tariffGroupIds));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(tariffGroupIds)) {
            return null;
        }

        List<TariffsEvseNumDTO> tariffsEvseNumDTOList = new ArrayList<>();
        tariffGroupIds.forEach(val -> {

            if (val == null) {
                return;
            }

            TariffsEvseNumDTO tariffsEvseNumDTO = new TariffsEvseNumDTO();

            tariffsEvseNumDTO.setTariffId(val);
            tariffsEvseNumDTO.setEvseNum(0);
            tariffsEvseNumDTO.setNumberOfApplicationLocations(0);
            tariffsEvseNumDTO.setNumberOfApplicationPiles(0);
            tariffsEvseNumDTO.setNumberOfApplicationGuns(0);

            tariffsEvseNumDTOList.add(tariffsEvseNumDTO);
        });

        List<OpLocationEvseEntity> opLocationEvseEntityList = opLocationEvseMapper.selectList(new LambdaQueryWrapper<OpLocationEvseEntity>()
                .in(OpLocationEvseEntity::getTariffId, tariffGroupIds)
                .eq(OpLocationEvseEntity::getDeleted, 0));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseEntityList)) {
            return tariffsEvseNumDTOList;
        }

        Map<Long, List<OpLocationEvseEntity>> tariffIdOpLocationEvseEntityMap = opLocationEvseEntityList
                .stream()
                .collect(Collectors.groupingBy(OpLocationEvseEntity::getTariffId));

        tariffsEvseNumDTOList.forEach(val -> {
            List<OpLocationEvseEntity> opLocationEvseEntitys = tariffIdOpLocationEvseEntityMap.get(val.getTariffId());

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEvseEntitys)) {
                return;
            }

            Set<Long> locationIdSet = new HashSet<>();
            Set<String> pileSnSet = new HashSet<>();

            opLocationEvseEntitys.forEach(item -> {
                locationIdSet.add(item.getLocationId());
                pileSnSet.add(CommonUtil.getPileSn(item.getEvseSn()));
            });

            val.setEvseNum(opLocationEvseEntitys.size());
            val.setNumberOfApplicationLocations(locationIdSet.size());
            val.setNumberOfApplicationPiles(pileSnSet.size());
            val.setNumberOfApplicationGuns(opLocationEvseEntitys.size());
        });

        List<OpLocationEntity> opLocationEntityList = opLocationMapper.selectList(new LambdaQueryWrapper<OpLocationEntity>()
                        .eq(OpLocationEntity::getDeleted, 0)
                        .in(OpLocationEntity::getId, opLocationEvseEntityList.stream().filter(val -> val != null && val.getLocationId() != null).map(OpLocationEvseEntity::getLocationId).collect(Collectors.toSet())));

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationEntityList)) {
            return tariffsEvseNumDTOList;
        }

        tariffsEvseNumDTOList.forEach(val -> {
            List<String> stringList = new ArrayList<>();
            for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntityList) {
                for (OpLocationEntity opLocationEntity : opLocationEntityList) {
                    if (val.getTariffId().equals(opLocationEvseEntity.getTariffId())
                            && opLocationEvseEntity.getLocationId() != null
                            && opLocationEvseEntity.getLocationId().equals(opLocationEntity.getId())) {
                        String locationWith = opLocationEntity.getName() + "-" + opLocationEvseEntity.getEvseSn();
                        if (locationWith.contains("_")) {
                            int indexOfKey = locationWith.lastIndexOf("_");
                            String prefix = locationWith.substring(0, indexOfKey);
                            String connectorIdString = locationWith.substring(indexOfKey + 1);
                            locationWith = prefix + "-0" + connectorIdString;
                        }
                        stringList.add(locationWith);
                    }
                }
            }

            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(stringList)) {
                Collections.sort(stringList);
                val.setLocationWith(stringList);
            }
        });

        return tariffsEvseNumDTOList;
    }

    @Override
    public List<OcppLocationEVSETimeZoneVO> getOcppLocationEVSETimeZoneVOList(List<String> evseSnList) {
        return opLocationConnectorMapper.getOcppLocationEVSETimeZoneVOList(evseSnList);
    }

    /**
     * @param tariffId 计费规则id
     * @return 场站id集合
     * @function 根据计费规则id，查询充电设备表，获得该计费规则所使用的场站集合
     */
    @Override
    public List<OpLocationEvseEntity> getAllLocationIdByTariffId(Long tariffId) {
        log.info("===>>> OpLocationEvseRepositoryImpl.getOneRandomRecordByTariffId tariffId : {}", JSON.toJSONString(tariffId));

        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .select(OpLocationEvseEntity::getId, OpLocationEvseEntity::getLocationId)
                .isNotNull(OpLocationEvseEntity::getLocationId)
                .eq(OpLocationEvseEntity::getTariffId, tariffId)
                .eq(OpLocationEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationEvseEntity::getUpdatedAt, OpLocationEvseEntity::getId);
        return opLocationEvseMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param opLocationEvseEntityList     充电设备实体集合(MySQL)
     * @param opLocationEvseElasticDTOList 充电设备实体集合(ES)
     * @return 更新结果
     * @function 为充电枪(充电设备)批量绑定计费规则
     */
    @Override
    public boolean bindCostModelRuleGroupForGun(List<OpLocationEvseEntity> opLocationEvseEntityList, List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList) {

        log.info("===>>>OpLocationEvseRepositoryImpl.bindCostModelRuleGroupForGun opLocationEvseEntityList : {} and opLocationEvseElasticDTOList : {}",
                JSON.toJSONString(opLocationEvseEntityList), JSON.toJSONString(opLocationEvseElasticDTOList));

        Map<Long, OpLocationEvseElasticDTO> idAndOpLocationEvseElasticDTOMap = new HashMap<>();
        opLocationEvseElasticDTOList.forEach(var -> idAndOpLocationEvseElasticDTOMap.put(var.getId(), var));

        // 更新枪ES
        opLocationEvseElastic.saveAll(opLocationEvseElasticDTOList);

        List<OpLocationEvseExpandElasticDTO> opLocationEvseExpandElasticDTOList = opLocationEvseExpandElastic.findAllByIdIn(opLocationEvseElasticDTOList
                .stream()
                .map(OpLocationEvseElasticDTO::getId)
                .collect(Collectors.toList()));
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opLocationEvseExpandElasticDTOList)) {
            opLocationEvseExpandElasticDTOList.forEach(var -> {
                var.setTariffId(idAndOpLocationEvseElasticDTOMap.get(var.getId()).getTariffId());
                var.setUpdatedAt(idAndOpLocationEvseElasticDTOMap.get(var.getId()).getUpdatedAt());
            });
            // 更新枪扩展ES
            opLocationEvseExpandElastic.saveAll(opLocationEvseExpandElasticDTOList);
        }

        // 更新数据库
        return this.updateBatchById(opLocationEvseEntityList);
    }

    /**
     * @param gunIdList 充电枪id集合
     * @return 充电设备（充电枪）的信息
     * @function 从MySQL数据库查询出充电设备（充电枪）的信息
     */
    @Override
    public List<OpLocationEvseEntity> getEvseInfoFromMySQL(List<Long> gunIdList) {

        log.info("===>>>OpLocationEvseRepositoryImpl.getEvseInfoFromMySQL gunIdList: {}", JSON.toJSONString(gunIdList));

        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .in(OpLocationEvseEntity::getId, gunIdList)
                .eq(OpLocationEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationEvseEntity::getUpdatedAt, OpLocationEvseEntity::getId);
        return opLocationEvseMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param gunIdList 充电枪id集合
     * @return 充电设备（充电枪）的信息
     * @function 从ES中查询出充电设备（充电枪）的信息
     */
    @Override
    public List<OpLocationEvseElasticDTO> getEvseInfoFromES(List<Long> gunIdList) {

        log.info("===>>>OpLocationEvseRepositoryImpl.getEvseInfoFromES gunIdList: {}", JSON.toJSONString(gunIdList));

        return opLocationEvseElastic.findAllByIdIn(gunIdList);
    }

    @Override
    public List<OpLocationEvseEntity> findByIds(List<Long> ids) {
        return this.listByIds(ids);
    }

    /**
     * @param sellerId
     * @return
     * @function 获得商家下所有的充电枪数据
     */
    @Override
    public List<OpLocationEvseElasticDTO> getAllEvseInfoBySellerId(Long sellerId) {
        return opLocationEvseElastic.findAllByOperatorId(sellerId);
    }

    /**
     * @param pileSn
     * @return
     * @function 根据充电桩序列号查询充电设备信息
     */
    @Override
    public List<OpLocationEvseElasticDTO> findListByPileSn(String pileSn) {

        log.info("===>>>OpLocationEvseRepositoryImpl.findListByPileSn pileSn: {}", JSON.toJSONString(pileSn));

        if (org.apache.commons.lang3.StringUtils.isBlank(pileSn)) {
            return null;
        }
        return opLocationEvseElastic.findAllByPileSn(pileSn);
    }

    /**
     * @param tariffGroupIdList
     * @param evseIdList
     * @return
     * @function 根据条件筛选出充电枪信息
     */
    @Override
    public List<OpLocationEvseEntity> findEvseInfoList(List<Long> tariffGroupIdList, List<Long> evseIdList) {

        log.info("===>>>OpLocationEvseRepositoryImpl.findEvseInfoList tariffGroupIdList : {} and evseIdList : {}", JSON.toJSONString(tariffGroupIdList), JSON.toJSONString(evseIdList));

        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .in(com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(tariffGroupIdList), OpLocationEvseEntity::getTariffId, tariffGroupIdList)
                .in(com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(evseIdList), OpLocationEvseEntity::getId, evseIdList)
                .eq(OpLocationEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationEvseEntity::getUpdatedAt, OpLocationEvseEntity::getId);
        return opLocationEvseMapper.selectList(lambdaQueryWrapper);
    }

    @Override
    public List<String> getTariffIdListByEvseSn(List<String> evseSnList) {
        return opLocationEvseMapper.getTariffIdListByEvseSn(evseSnList);
    }


    public List<PileBindTimeVO> batchQueryPileBindTime(List<String> snList) {
        if (CollectionUtils.isEmpty(snList)) {
            return new ArrayList<>();
        }

        List evseSnList = new ArrayList();
        snList.stream().forEach(sn -> {
            evseSnList.add(sn + "_1");
        });
        log.info("batchQueryPileBindTime evseSnList :{}", JSON.toJSONString(evseSnList));
        List<PileBindTimeVO> result = opLocationEvseMapper.batchQueryPileBindTime(evseSnList);
        log.info("batchQueryPileBindTime result :{}", JSON.toJSONString(result));
        if (CollectionUtils.isEmpty(result)) {
            return new ArrayList<>();
        }

        result.stream().forEach(r -> {
            r.setSn(r.getSn().split("_")[0]);
        });
        return result;
    }

    @Override
    public List<OpEvseAssociatedRuleDTO> searchGunInformation(List<Long> evseIdList) {
        if (CollectionUtils.isEmpty(evseIdList)) {
            return null;
        }
        BoolQueryBuilder evseQuery = QueryBuilders.boolQuery();
        evseQuery.must(QueryBuilders.termsQuery("id", evseIdList));
        Iterable<OpLocationEvseElasticDTO> evseIterable =
//                        opLocationEvseElastic.search(evseQuery2);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(evseQuery).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpEvseAssociatedRuleDTO> evseRuleList = new ArrayList<>();
        if (CollUtil.isNotEmpty(evseIterable)) {
            evseIterable.forEach(evseItem -> {
                if (evseItem != null) {
                    evseRuleList.add(OpLocationEvseConvert.toOpEvseAssociatedRuleDTO(evseItem));
                }
            });
        }
        return evseRuleList;
    }

    @Override
    public OpLocationEvseElasticDTO findByEvseSn(String evseSn) {
        return opLocationEvseElastic.findByEvseSn(evseSn);
    }

    @Override
    public List<OpLocationEvseElasticDTO> getSellerEvseInfo(Long operatorId, List<String> pileSnList) {
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        Optional.ofNullable(operatorId).ifPresent(value -> queryBuilder.must(QueryBuilders.termQuery("operatorId", operatorId)));
        Optional.ofNullable(pileSnList).ifPresent(value -> queryBuilder.must(QueryBuilders.termsQuery("pileSn", pileSnList)));
        log.info(" getSellerEvseInfo,orderSearch \n {}", queryBuilder);
        final NativeSearchQueryBuilder nativeSearchQueryBuilder = new NativeSearchQueryBuilder();
        final NativeSearchQuery nativeSearchQuery = nativeSearchQueryBuilder.withQuery(queryBuilder).withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC)).build();
        return elasticsearchRestTemplate.search(nativeSearchQuery, OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
    }

    @Override
    public Boolean updateEvseLastOrderSeq(String evseSn, Long orderSeq) {
        log.info("updateEvseLastOrderSeq,evseSn:{},orderSeq:{}", evseSn, orderSeq);
        String key = OpLocationEvseElasticDTO.class.getAnnotation(Document.class).indexName() + ":" + evseSn;
        try {
            Boolean aBoolean;
            do {
                aBoolean = stringRedisTemplate.opsForValue().setIfAbsent(key, evseSn + ":" + orderSeq, 50, TimeUnit.SECONDS);
            } while (Boolean.FALSE.equals(aBoolean));
            String index = OpLocationEvseElasticDTO.class.getAnnotation(Document.class).indexName();
            UpdateByQueryRequest updateRequest = new UpdateByQueryRequest(index);
            updateRequest.setQuery(QueryBuilders.termQuery("evseSn", evseSn));
            Map<String, Object> parameters = new HashMap<>();
            String script = "ctx._source.lastOrderSeq=params.lastOrderSeq;";
            parameters.put("lastOrderSeq", orderSeq);
            updateRequest.setScript(new Script(ScriptType.INLINE, "painless", script, parameters));
            updateRequest.setMaxRetries(3);
            BulkByScrollResponse bulkByScrollResponse;
            int retryCount = 300;
            do {
                try {
                    bulkByScrollResponse = restHighLevelClient.updateByQuery(updateRequest, RequestOptions.DEFAULT);
                    log.info("updateEvseLastOrderSeq result:{}", JSON.toJSONString(bulkByScrollResponse.getUpdated()));
                    if (bulkByScrollResponse.getVersionConflicts() <= 0) {
                        log.info("success update retryCount {}", 300 - retryCount);
                        break;
                    }
                    retryCount--;
                    LockSupport.parkNanos(1000_000000);
                } catch (IOException e) {
                    retryCount--;
                    log.info("updateEvseLastOrderSeq failed,", e);
                }
            } while (retryCount > 0);
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = opLocationEvseElastic.findByEvseSn(evseSn);
            log.info("updateEvseLastOrderSeq opLocationEvseElasticDTO:{}", JSON.toJSONString(opLocationEvseElasticDTO));
        } catch (Exception e) {
            log.info("updateEvseLastOrderSeq fail", e);
        } finally {
            log.info("remove lock key :{} {}, {}", key, evseSn + ":" + orderSeq, stringRedisTemplate.delete(key));
        }
        return true;
    }

    @Override
    public OpLocationEvseEntity findOneByEvseSn(String evseSn) {

        LambdaQueryWrapper<OpLocationEvseEntity> lqw = new LambdaQueryWrapper<>();
        lqw.eq(OpLocationEvseEntity::getEvseSn, evseSn)
                .eq(OpLocationEvseEntity::getDeleted, 0);

        return opLocationEvseMapper.selectOne(lqw);
    }

    @Override
    public List<String> getAllEvseSnByLocationIds(List<Long> locationIds) {
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("getAllEvseSnByLocationIds,locationIds is empty.");
            return null;
        }
        List<OpLocationEvseElasticDTO> evseDtoList = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                .withQuery(QueryBuilders.termsQuery("locationId", locationIds))
                .withPageable(PageRequest.of(0, 1000))
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "evseSn"}, null))
                .build(), OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(evseDtoList)) {
            log.info("getAllEvseSnByLocationIds,evseDtoList is empty.");
            return null;
        }
        return evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
    }

    @Override
    public List<OpLocationEvseElasticDTO> getOpLocationEvseElasticDTOs(List<String> evseSns) {

        log.info("===>>> OpLocationEvseRepositoryImpl.getOpLocationEvseElasticDTOs evseSns : {}",
                JSON.toJSONString(evseSns));

        return com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(evseSns)
                ? null
                : opLocationEvseElastic.findAllByEvseSnIn(evseSns);
    }

    public Boolean syncEvseExpandByEvseIds(List<Long> pileIds) {
        if (CollectionUtil.isNotEmpty(pileIds)) {
            List<OpLocationEvseElasticDTO> evselist = opLocationEvseElastic.findAllByIdIn(pileIds);
            if (CollectionUtil.isNotEmpty(evselist)) {
                Set<Long> locationIds = evselist.stream().map(OpLocationEvseElasticDTO::getLocationId).collect(Collectors.toSet());
                List<OpLocationElasticDTO> locationList = opLocationElastic.findAllByIdIn(locationIds);
                Map<Long, OpLocationElasticDTO> locationMap = locationList.stream().collect(Collectors.toMap(OpLocationElasticDTO::getId, e -> e, (f, s) -> f));
                List<OpLocationEvseExpandElasticDTO> list = new ArrayList<>();
                evselist.stream().forEach(evseDto -> {
                    Long locationId = evseDto.getLocationId();
                    OpLocationElasticDTO v = locationMap.get(locationId);
                    OpLocationEvseExpandElasticDTO dto = new OpLocationEvseExpandElasticDTO();
                    BeanUtils.copyProperties(v, dto);
                    dto.setId(evseDto.getId());
                    dto.setLocationId(locationId);
                    dto.setPower(evseDto.getPower());
                    dto.setGunState(evseDto.getState());
                    dto.setGunType(evseDto.getGunType());
                    dto.setTariffId(evseDto.getTariffId());
                    dto.setSubscriptionCheck(evseDto.getSubscriptionCheck());
                    list.add(dto);
                });
                if (list.size() > 0) {
                    opLocationEvseExpandElastic.saveAll(list);
                    log.info("syncEvseExpand,svseSize={}", list.size());
                }
            }
        }
        return true;
    }

    public List<OpLocationElasticDTO> getLocationListFromEs(List<Long> ids) {
        Iterable<OpLocationElasticDTO> result;
        if (CollectionUtil.isEmpty(ids)) {
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
            //只查询Autel的场站
            queryBuilder.must(QueryBuilders.termQuery("platform", 1));
//            result = opLocationElastic.search(queryBuilder);
            result = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());
        } else {
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
            queryBuilder.must(QueryBuilders.termsQuery("id", ids));
//            result = opLocationElastic.search(queryBuilder);
            result = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationElasticDTO.class)
                    .stream().map(SearchHit::getContent).collect(Collectors.toList());
        }
        Iterator<OpLocationElasticDTO> it = result.iterator();
        List<OpLocationElasticDTO> list = new ArrayList<>();
        while (it.hasNext()) {
            OpLocationElasticDTO next = it.next();
            list.add(next);
        }
        return list;
    }

    @Override
    public TariffRuleOfPileDTO getZoneIdByPileAndEvse(TariffRuleOfPileDTO tariffRuleOfPileDTO) {

        log.info("OpLocationEvseRepositoryImpl.getZoneIdByPileAndEvse tariffRuleOfPileDTO : {}", JSON.toJSONString(tariffRuleOfPileDTO));

        if (tariffRuleOfPileDTO == null
                || StrUtil.isBlank(tariffRuleOfPileDTO.getPileNo())
                || StrUtil.isBlank(tariffRuleOfPileDTO.getEvseSn())) {
            // 必填参数为空，直接返回tariffRuleOfPileDTO
            return tariffRuleOfPileDTO;
        }

        //根据桩SN查询charge_pile表的使用场景
        Result<PileStandInfoDTO> dtoResult = deviceServiceFeign.queryStandInfo(tariffRuleOfPileDTO.getPileNo());

        log.info("OpLocationEvseRepositoryImpl.getZoneIdByPileAndEvse dtoResult : {}", JSON.toJSONString(dtoResult));

        Integer usage = null;
        if (dtoResult != null && dtoResult.getData() != null) {
            PileStandInfoDTO pileStandInfoDTO = dtoResult.getData();
            usage = pileStandInfoDTO.getUsage();
            tariffRuleOfPileDTO.setUsage(usage);
        }

        //使用场景分以下三种情况：使用场景 0：无属性桩  1：商桩  2：家桩  3：家桩共享
        if (usage == null || usage == 0) {
            //1. 当usage为null或者0的时候，直接返回tariffRuleOfPileDTO
            return tariffRuleOfPileDTO;
        } else if (usage == 1 || usage == 3) {
            //2. 当usage为 1或3 的时候，在pile_base数据库查询opLocation表的zoneId
            //根据evseSn查询枪的计费规则id
            LambdaQueryWrapper<OpLocationEvseEntity> evseQuery = Wrappers.lambdaQuery();
            evseQuery.select(OpLocationEvseEntity::getTariffId, OpLocationEvseEntity::getLocationId)
                    .eq(OpLocationEvseEntity::getDeleted, 0)
                    .eq(OpLocationEvseEntity::getEvseSn, tariffRuleOfPileDTO.getEvseSn())
                    .last("limit 1");

            OpLocationEvseEntity opLocationEvseEntity = opLocationEvseMapper.selectOne(evseQuery);

            log.info("OpLocationEvseRepositoryImpl.getZoneIdByPileAndEvse opLocationEvseEntity : {}", JSON.toJSONString(opLocationEvseEntity));

            Long locationId = null;
            if (opLocationEvseEntity != null) {
                Long tariffId = opLocationEvseEntity.getTariffId();
                // 设置计费规则id
                tariffRuleOfPileDTO.setTariffId(tariffId);
                locationId = opLocationEvseEntity.getLocationId();
            }

            if (locationId != null) {
                // 根据场站id查询场站表，获取时区
                LambdaQueryWrapper<OpLocationEntity> locationQuery = Wrappers.lambdaQuery();
                locationQuery.select(OpLocationEntity::getZoneId)
                        .eq(OpLocationEntity::getDeleted, 0)
                        .eq(OpLocationEntity::getId, locationId)
                        .last("limit 1");

                OpLocationEntity opLocationEntity = opLocationMapper.selectOne(locationQuery);

                log.info("OpLocationEvseRepositoryImpl.getZoneIdByPileAndEvse.opLocationEntity : {}", JSON.toJSONString(opLocationEntity));

                if (opLocationEntity != null) {
                    String zoneId = opLocationEntity.getZoneId();
                    // 设置时区
                    tariffRuleOfPileDTO.setStationTimezone(zoneId);
                }
            }
            return tariffRuleOfPileDTO;
        } else if (usage == 2) {
            //3. 当usage为 2 的时候，在data_service数据库查询user_charge_pile表的user_id，根据user_id查bill模块的缓存
            Result<UserChargePileVO> pileVOResult = homePileClient.getUserIdBySN(tariffRuleOfPileDTO.getPileNo());

            log.info("OpLocationEvseRepositoryImpl.getZoneIdByPileAndEvse pileVOResult : {}", JSON.toJSONString(pileVOResult));

            if (pileVOResult != null && pileVOResult.getData() != null) {
                UserChargePileVO userChargePileVO = pileVOResult.getData();
                String userId = userChargePileVO.getUserId();
                if (StringUtils.isBlank(userId)) {
                    // 家桩未绑桩主，直接返回
                    return tariffRuleOfPileDTO;
                }
                // 给家桩设置桩主
                tariffRuleOfPileDTO.setUserId(Long.valueOf(userId));

                //从取缓存中取时区
                String userKey = String.format(RedisKeyConstant.TIMEZONE_OF_USER, userId);
                Object timezoneObject = redisUtil.get(userKey);

                log.info("OpLocationEvseRepositoryImpl.getZoneIdByPileAndEvse timezoneObject : {}", timezoneObject);

                if (null != timezoneObject) {
                    TimezoneUserDTO timezoneUserDTO = JSON.parseObject(JSON.toJSONString(timezoneObject), TimezoneUserDTO.class);
                    if (timezoneUserDTO != null) {
                        String zoneId = timezoneUserDTO.getZoneId();
                        // 给家桩设置时区
                        tariffRuleOfPileDTO.setStationTimezone(zoneId);
                    }
                }
            }
            return tariffRuleOfPileDTO;
        } else {
            // 使用场景异常，直接返回tariffRuleOfPileDTO
            return tariffRuleOfPileDTO;
        }
    }

    @Override
    public boolean updateRuleId(AssociatedGunDTO associatedGunDTO) {
        if (ObjectUtils.isEmpty(associatedGunDTO)) {
            return false;
        }
        Iterable<OpLocationEvseElasticDTO> allByEvseSnIn = opLocationEvseElastic.findAllByEvseSnIn(associatedGunDTO.getPiles().stream().map(m -> {
            return m.getSn() + "_" + m.getGunNumber();
        }).collect(Collectors.toList()));
        for (OpLocationEvseElasticDTO dto : allByEvseSnIn) {
            dto.setRuleId(associatedGunDTO.getRuleId());
            dto.setRuleName(associatedGunDTO.getRuleName());
            //-1删除
            if (associatedGunDTO.getRuleId() == -1L) {
                dto.setRuleId(null);
                dto.setRuleName(null);
            }
        }
        log.info("进场控制关联枪,allByEvseSnIn={}", JSON.toJSONString(allByEvseSnIn));
        Iterable<OpLocationEvseElasticDTO> opLocationEvseElasticDTOS = opLocationEvseElastic.saveAll(allByEvseSnIn);
        log.info("进场控制关联枪,opLocationEvseElasticDTOS={}", JSON.toJSONString(opLocationEvseElasticDTOS));
        return true;
    }

    @Override
    public List<OpLocationEvseElasticDTO> findByLocationId(Long locationId) {
        return opLocationEvseElastic.findAllByLocationId(locationId);
    }

    @Override
    public boolean syncRuleName(Long ruleId, String name) {
        log.info("syncRuleName,ruleId={},name={}", ruleId, name);
        List<OpLocationEvseElasticDTO> evseList = opLocationEvseElastic.findAllByRuleId(ruleId);
        log.info("syncRuleName,evseList={}", JSON.toJSONString(evseList));
        if (!org.springframework.util.CollectionUtils.isEmpty(evseList)) {
            evseList.forEach(p -> p.setRuleName(name));
            opLocationEvseElastic.saveAll(evseList);
        }
        return true;
    }

    @Override
    public List<OpLocationEvseElasticDTO> finEvseListByOplocationIdAndSellerId(List<Long> locationIds, Long sellerId) {
        if (CollectionUtils.isEmpty(locationIds) || ObjectUtils.isEmpty(sellerId)) {
            return null;
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));
        queryBuilder.must(QueryBuilders.termQuery("operatorId", sellerId));
        SearchHits<OpLocationEvseElasticDTO> searchHits = elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).withTrackTotalHits(true)
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC)).build(), OpLocationEvseElasticDTO.class);
        return searchHits.stream().map(SearchHit::getContent).collect(Collectors.toList());
    }

    @Override
    public IPage<SerachBindEvseVO> getEvseInformationByLocationIds(SerachBindEvseDTO dto, Long sellerId) {
        Long locationId = dto.getLocationId();
        Integer page = dto.getPage();
        Integer pageSize = dto.getPageSize();
        String keyWord = dto.getKeyWord();

        IPage<SerachBindEvseVO> resultPage = new Page<>(page, pageSize);
        List<Long> locationIds;
        if (locationId != null) {
            locationIds = Arrays.asList(locationId);
        }else {
            locationIds = this.opLocationRepository.getLocationIdBySellerId(sellerId);
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("getEvseInformationByLocationIds,locationIds is empty.");
            return resultPage;
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", sellerId));

        if (!StringUtils.isBlank(keyWord)) {
            keyWord = QueryParserBase.escape(keyWord);
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", keyWord)));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyWord)));
            queryBuilder.must(queryBuilder2);
        }
        List<Long> tmpIds = new ArrayList<>();
        //员工校验权限
        if (!LoginUserUtil.isSellerAdmin()) {
            List<Long> data = this.pileUserFeign.getLocationIds().getData();
            if (!CollectionUtils.isEmpty(data)) {
                tmpIds.addAll(data);
            }
            locationIds = locationIds.stream().filter(tmpIds::contains).collect(Collectors.toList());
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("getEvseInformationByLocationIds,after locationIds is empty.");
            return resultPage;
        }
        queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));

        //Set<Long> notAdvertisingStationList = opLocationRepository.getAllNotAdvertisingStation(LoginUserUtil.getSellerId());

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withPageable(PageRequest.of(page - 1, pageSize)).build();
        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> pageSearch = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(list)) {
            log.info("getEvseInformationByLocationIds,list is empty.");
            return resultPage;
        }
        //查找桩id
        Set<String> pileSnList = list.stream().map(OpLocationEvseElasticDTO::getPileSn).collect(Collectors.toSet());
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseServiceList = opLocationPileEvseService.findList(new ArrayList<>(pileSnList));
        Map<String, Long> snIdMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(opLocationPileEvseServiceList)) {
            snIdMap = opLocationPileEvseServiceList.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, OpLocationPileEvseElasticDTO::getId, (e1, e2) -> e1));
        }
        List<SerachBindEvseVO> serachBindEvseVOS = new ArrayList<>();
        for (OpLocationEvseElasticDTO evseDto : list) {
            SerachBindEvseVO serachBindEvseVO = new SerachBindEvseVO();
            BeanUtils.copyProperties(evseDto, serachBindEvseVO);
            Integer gunNo = CommonUtil.getGunNo(evseDto.getEvseSn());
            serachBindEvseVO.setGunNumber(gunNo != null ? gunNo.toString() : null);
            if (!CollectionUtils.isEmpty(snIdMap)) {
                serachBindEvseVO.setPileId(snIdMap.get(serachBindEvseVO.getPileSn()));
            }
            serachBindEvseVOS.add(serachBindEvseVO);
        }
        resultPage.setRecords(serachBindEvseVOS);
        resultPage.setTotal(pageSearch.getTotalElements());
        resultPage.setPages(pageSearch.getTotalPages());
        return resultPage;
    }

    @Override
    public TariffCountVO getTariffCountByPileSn(PileEvseTariffDTO pileEvseTariffDTO) {
        TariffCountVO tariffCountVO = new TariffCountVO();
        String pileSn = pileEvseTariffDTO.getPileSn();
        List<Long> evseIdList = new ArrayList<>();
        if (CollUtil.isNotEmpty(pileEvseTariffDTO.getEvseSnList())) {
            List<Integer> evseList = pileEvseTariffDTO.getEvseSnList();
            List<String> evseSnList = new ArrayList<>();
            for (Integer integer : evseList) {
                evseSnList.add(pileSn + "_" + integer);
            }
            List<OpLocationEvseEntity> opLocationEvseEntities = opLocationEvseMapper.selectList(Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                    .in(OpLocationEvseEntity::getEvseSn, evseSnList)
                    .eq(OpLocationEvseEntity::getDeleted, 0));
            log.info("getTariffCountByPileSn.opLocationEvseEntities:{}", JSON.toJSONString(opLocationEvseEntities));
            if (CollUtil.isEmpty(opLocationEvseEntities)) {
                return tariffCountVO;
            }
            for (OpLocationEvseEntity opLocationEvseEntity : opLocationEvseEntities) {
                evseIdList.add(opLocationEvseEntity.getId());
            }
        } else {
            LambdaQueryWrapper<OpLocationPileEvseEntity> pileQueryWrapper = new LambdaQueryWrapper<>();
            pileQueryWrapper.eq(OpLocationPileEvseEntity::getPileSn, pileSn)
                    .eq(OpLocationPileEvseEntity::getDeleted, 0);
            OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseMapper.selectOne(pileQueryWrapper);
            log.info("getTariffCountByPileSn.opLocationPileEvseEntity:{}", JSON.toJSONString(opLocationPileEvseEntity));
            if (ObjectUtils.isEmpty(opLocationPileEvseEntity) || StringUtils.isBlank(opLocationPileEvseEntity.getEvseList())) {
                return tariffCountVO;
            }
            evseIdList = JSON.parseArray(opLocationPileEvseEntity.getEvseList(), Long.class);
        }
        log.info("getTariffCountByPileSn.evseIdList:{}", evseIdList);


        Boolean tariffRuleIsSame;
        List<Integer> gunNoList = new ArrayList<>();
        LambdaQueryWrapper<OpLocationEvseEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(OpLocationEvseEntity::getId, OpLocationEvseEntity::getEvseSn, OpLocationEvseEntity::getTariffId)
                .eq(OpLocationEvseEntity::getDeleted, 0)
                .in(OpLocationEvseEntity::getId, evseIdList);
        List<OpLocationEvseEntity> opLocationEvseEntities = opLocationEvseMapper.selectList(queryWrapper);
        Set<Long> gunTariffIdSet = new HashSet<>();
        opLocationEvseEntities.forEach(opLocationEvseEntity -> {
            if (!ObjectUtils.isEmpty(opLocationEvseEntity.getTariffId())) {
                gunTariffIdSet.add(opLocationEvseEntity.getTariffId());
            }
        });
        if (gunTariffIdSet.size() == 1) {
            //表示计费规则是一样的
            tariffRuleIsSame = true;
        } else {
            //表示计费规则是不一样的
            tariffRuleIsSame = false;
            for (int i = 1; i <= opLocationEvseEntities.size(); i++) {
                gunNoList.add(i);
            }
        }
        tariffCountVO.setTariffRuleIsSame(tariffRuleIsSame);
        tariffCountVO.setGunNoList(gunNoList);
        return tariffCountVO;
    }

    /**
     * @param evseSnList
     * @return
     * @function 查询计费规则集合
     */
    @Override
    public List<OpLocationEvseEntity> queryTariffIdListByEvseSnList(List<String> evseSnList) {

        log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.queryTariffIdListByEvseSnList evseSnList : {}", JSON.toJSONString(evseSnList));

        LambdaQueryWrapper<OpLocationEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .in(OpLocationEvseEntity::getEvseSn, evseSnList)
                .eq(OpLocationEvseEntity::getDeleted, 0);

        return opLocationEvseMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param evseInfoModifyDTO
     * @function 当充电设备（充电枪）的基本信息发生变化时，向外发出信息变动的通知，供车队项目使用！
     */
    @Override
    public void sendEvseInfoMQToFleet(EvseInfoModifyDTO evseInfoModifyDTO) {

        log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet evseInfoModifyDTO before : {}", JSON.toJSONString(evseInfoModifyDTO));

        if (evseInfoModifyDTO == null
                || evseInfoModifyDTO.getOperationType() == null
                || org.apache.commons.lang3.StringUtils.isBlank(evseInfoModifyDTO.getEvseSn())) {
            return;
        }

        // 为防止发送的DTO消息中某些属性为空，所以需要差缺补漏
        if (evseInfoModifyDTO.getUserId() == null) {
            JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
            evseInfoModifyDTO.setUserId(jwtInfo.getPayload().getUserId());
        }

        if (evseInfoModifyDTO.getSellerId() == null
                || org.apache.commons.lang3.StringUtils.isBlank(evseInfoModifyDTO.getZoneId())) {

            OpEvseInfoDTO opEvseInfoDTO = this.getEvseByEvseSn(evseInfoModifyDTO.getEvseSn());

            log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet evseByEvseSn : {}", JSON.toJSONString(opEvseInfoDTO));

            if (opEvseInfoDTO != null && opEvseInfoDTO.getLocationId() != null) {
                Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(opEvseInfoDTO.getLocationId());
                if (optionalOpLocationElasticDTO.isPresent()) {
                    OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();

                    log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet opLocationElasticDTO : {}", JSON.toJSONString(opLocationElasticDTO));

                    // 场站运营商id
                    Long operatorId = opLocationElasticDTO.getOperatorId();
                    // 场站时区id
                    String zoneId = opLocationElasticDTO.getZoneId();
                    if (operatorId != null) {
                        evseInfoModifyDTO.setSellerId(operatorId);
                    }
                    if (org.apache.commons.lang3.StringUtils.isNotBlank(zoneId)) {
                        evseInfoModifyDTO.setZoneId(zoneId);
                    }
                }
            }
        }

        log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet evseInfoModifyDTO after : {}", JSON.toJSONString(evseInfoModifyDTO));

        rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_EVSE_MODIFY_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, AmqpConstant.PILE_BASE_EVSE_MODIFY_ROUTE, JSON.toJSONString(evseInfoModifyDTO));
    }

    /**
     * @param evseInfoModifyDTOList
     * @param locationId
     * @param userId
     * @function 当充电设备（充电枪）的基本信息发生变化时，向外发出信息变动的通知，供车队项目使用！
     */
    @Override
    public void sendEvseInfoMQToFleet(List<EvseInfoModifyDTO> evseInfoModifyDTOList, Long locationId, Long userId) {

        log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet evseInfoModifyDTOList : {} and locationId : {} and userId : {}",
                JSON.toJSONString(evseInfoModifyDTOList),
                JSON.toJSONString(locationId),
                JSON.toJSONString(userId));

        // 场站运营商id
        Long operatorId = null;
        // 场站时区id
        String zoneId = null;
        if (locationId != null) {
            // 根据场站id查询场站信息
            Optional<OpLocationElasticDTO> optionalOpLocationElasticDTO = opLocationElastic.findById(locationId);
            if (optionalOpLocationElasticDTO.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optionalOpLocationElasticDTO.get();

                log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet opLocationElasticDTO : {}", JSON.toJSONString(opLocationElasticDTO));

                operatorId = opLocationElasticDTO.getOperatorId();
                zoneId = opLocationElasticDTO.getZoneId();
            }
        }

        for (EvseInfoModifyDTO evseInfoModifyDTO : evseInfoModifyDTOList) {
            evseInfoModifyDTO.setSellerId(operatorId);
            evseInfoModifyDTO.setUserId(userId);
            evseInfoModifyDTO.setZoneId(zoneId);
            if (org.apache.commons.lang3.StringUtils.isNotBlank(evseInfoModifyDTO.getEvseSn())
                    && evseInfoModifyDTO.getOperationType() != null) {

                log.info("========>>>>>>>>>>>> OpLocationEvseRepositoryImpl.sendEvseInfoMQToFleet evseInfoModifyDTO after : {}", JSON.toJSONString(evseInfoModifyDTO));

                rabbitTemplate.convertAndSend(AmqpConstant.PILE_BASE_EVSE_MODIFY_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, AmqpConstant.PILE_BASE_EVSE_MODIFY_ROUTE, JSON.toJSONString(evseInfoModifyDTO));
            }
        }
    }

    @Override
    public List<OpEvseInfoDTO> getEvseByIdList(List<Long> idList) {
        List<OpLocationEvseElasticDTO> locationEvseElasticDTOS = opLocationEvseElastic.findAllByIdIn(idList);
        if (CollectionUtils.isEmpty(locationEvseElasticDTOS)) {
            return Lists.newArrayList();
        }
        return locationEvseElasticDTOS.stream().map(OpLocationEvseConvert::toOpEvseInfoDTO).collect(Collectors.toList());
    }
}
