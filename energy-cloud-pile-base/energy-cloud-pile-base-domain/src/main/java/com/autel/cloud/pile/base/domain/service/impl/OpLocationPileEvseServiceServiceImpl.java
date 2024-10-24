package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.util.StrUtil;
import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.fleet.FleetFeignClient;
import com.autel.cloud.fleet.vo.smartChargingPlan.SmartChargingTimeSharingVo;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.constant.key.ConfigRedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileEvseConvert;
import com.autel.cloud.pile.base.domain.model.SelectDeviceInfoForPosDTO;
import com.autel.cloud.pile.base.domain.model.SelectDeviceInfoForPosVO;
import com.autel.cloud.pile.base.domain.model.dto.SetPileEroamingForPileDTO;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.repository.impl.LocalAuthListRepositoryImpl;
import com.autel.cloud.pile.base.domain.repository.impl.OpLocationEvseRepositoryImpl;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.RuleLocationPileService;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.domain.utils.TimeZoneUtil;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.dto.advertise.AdvertiseListReq;
import com.autel.cloud.pile.base.dto.advertise.OpPileAdvertiseReq;
import com.autel.cloud.pile.base.dto.beta.QueryBetaTestPileInfoDTO;
import com.autel.cloud.pile.base.dto.britishAct.QueryDTO;
import com.autel.cloud.pile.base.dto.common.SearchDTO;
import com.autel.cloud.pile.base.dto.config.ClearSmartChargingProfileDTO;
import com.autel.cloud.pile.base.dto.config.PileFreeVendInfoDTO;
import com.autel.cloud.pile.base.dto.config.SetPileFreeVendDTO;
import com.autel.cloud.pile.base.dto.config.UpdatePileFreeVendIdTagDTO;
import com.autel.cloud.pile.base.dto.eroaming.SetPileEroamingDTO;
import com.autel.cloud.pile.base.dto.fleet.SelectChargingInfoForFleetDTO;
import com.autel.cloud.pile.base.dto.oicp.ActionType;
import com.autel.cloud.pile.base.dto.pile.EvscpSettingDTO;
import com.autel.cloud.pile.base.dto.pile.PileSimpleInfoQueryDTO;
import com.autel.cloud.pile.base.dto.pile.QueryPileDTO;
import com.autel.cloud.pile.base.enums.EVSETypeEnum;
import com.autel.cloud.pile.base.enums.LocalAuthListEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.config.ConfigKeyEnum;
import com.autel.cloud.pile.base.feign.CreativePlanFeignClient;
import com.autel.cloud.pile.base.feign.CreativePlatformFeignClient;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.feign.OpsMgmtClient;
import com.autel.cloud.pile.base.infrastructure.feign.WxProxyClient;
import com.autel.cloud.pile.base.infrastructure.feign.dto.PileOffPeakHourParamDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.LocalAuthListMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseBaseInfoVO;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseDeviceInfoVO;
import com.autel.cloud.pile.base.vo.advertise.AdvertiseListVO;
import com.autel.cloud.pile.base.vo.beta.BetaTestPileInfoVO;
import com.autel.cloud.pile.base.vo.britishAct.BritishActVO;
import com.autel.cloud.pile.base.vo.britishAct.DefaultChargingTimeVO;
import com.autel.cloud.pile.base.vo.britishAct.ModifyCommonPileToBritainStandPileForTestVO;
import com.autel.cloud.pile.base.vo.britishAct.ThatDayDefaultChargingTimeVO;
import com.autel.cloud.pile.base.vo.britishAct.ops.EVSCPParamVO;
import com.autel.cloud.pile.base.vo.fleet.ChargingGunInfoVO;
import com.autel.cloud.pile.base.vo.fleet.ChargingPileInfoVO;
import com.autel.cloud.pile.base.vo.fleet.SelectChargingInfoForFleetVO;
import com.autel.cloud.pile.base.vo.fleet.VehicleChargingInfoVO;
import com.autel.cloud.pile.base.vo.location.LocationInfoVO;
import com.autel.cloud.pile.base.vo.pile.OcppConfigurationViewEnableVO;
import com.autel.cloud.pile.base.vo.pile.PileBasicInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileInfoVO;
import com.autel.cloud.pile.base.vo.pile.PileSimpleInfoQueryVO;
import com.autel.cloud.pile.bill.dto.BatchSendMsgV2DTO;
import com.autel.cloud.pile.bill.dto.SendMsgDto;
import com.autel.cloud.pile.bill.dto.fleet.CacheChargeInfoDTO;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.bill.ChargeBillInfoVO;
import com.autel.cloud.pile.user.api.enums.UserSourceEnum;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.CommonVO;
import com.autel.cloud.smart.charge.enums.OcppAction;
import com.autel.cloud.smart.charge.feign.SmartChargeFeign;
import com.autel.cloud.tariff.dto.*;
import com.autel.cloud.tariff.enums.RuleModelTypeEnum;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.rule.dto.*;
import com.autel.cloud.tariff.utils.redis.TariffRedisKeyUtils;
import com.autel.cloud.tariff.vo.AdvancedCostModelRuleVO;
import com.autel.cloud.tariff.vo.BasicCostModelRuleVO;
import com.autel.cloud.tariff.vo.SimpleInformationAboutCostModelRuleVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.IndexQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@Slf4j
public class OpLocationPileEvseServiceServiceImpl implements OpLocationPileEvseService {

    private final OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Resource
    private RuleLocationPileService ruleLocationPileService;
    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Resource
    private RuleRepository ruleRepository;
    @Autowired
    private TariffFeignClient tariffFeignClient;
    @Autowired
    MonitorFeignClient monitorFeignClient;
    @Autowired
    @Lazy
    private OpLocationEvseService opLocationEvseService;

    @Autowired
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private OpLocationElastic opLocationElastic;

    @Autowired
    private OpLocationRepository opLocationRepository;

    @Resource
    private HomePileShareRepository homePileShareRepository;

    @Lazy
    @Resource
    private PileBaseAsync pileBaseAsync;

    @Resource
    private OpsMgmtClient opsMgmtClient;
    @Resource
    private WxProxyClient wxProxyClient;
    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private LocalAuthListRepositoryImpl localAuthListRepositoryImpl;

    @Resource
    LocalAuthListMapper localAuthListMapper;

    @Resource
    private OpLocationEvseRepositoryImpl opLocationEvseRepository;

    @Autowired
    private PileUserFeign pileUserFeign;

    @Resource
    private OpLocationPileGroupRepository opLocationPileGroupRepository;

    @Resource
    private SmartChargeFeign smartChargeFeign;

    @Autowired
    private CreativePlatformFeignClient creativePlatformFeignClient;

    @Autowired
    private CreativePlanFeignClient creativePlanFeignClient;

    @Autowired
    private FleetFeignClient fleetFeignClient;

    @Resource
    public RedisTemplate redisTemplate;

    //随机延迟启动时间最小值
    private static final Integer randomDelayMin = 600;
    private static final Integer randomDelayMax = 1800;

    @Autowired
    private IBillFeignClient billFeignClient;

    public OpLocationPileEvseServiceServiceImpl(OpLocationPileEvseRepository opLocationPileEvseRepository) {
        this.opLocationPileEvseRepository = opLocationPileEvseRepository;
    }

    /**
     * 桩导入模板下载
     *
     * @param request  request
     * @param response response
     * @return Result
     */
    @Override
    public Result<Void> downModuleGenerateXls(HttpServletRequest request, HttpServletResponse response) {
        return Result.ofSucceed(opLocationPileEvseRepository.downModuleGenerateXls(request, response));
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
        return opLocationPileEvseRepository.downModuleResourceXls(request, response);
    }

    /**
     * 批量导入桩
     *
     * @param multipartFile 文件
     * @return Result
     */
    @Override
    public Result<PileUploadCheckVO> uploadModuleXls(String locationId, MultipartFile multipartFile) {
        return Result.ofSucceed(opLocationPileEvseRepository.uploadModuleXls(locationId, multipartFile));
    }

    @Override
    public Object newImport(String locationId, MultipartFile file) {
        return opLocationPileEvseRepository.newImport(locationId,file);
    }

    @Override
    public void downloadImportErrorFile(String language, HttpServletResponse response) {
        opLocationPileEvseRepository.downloadImportErrorFile(language,response);
    }
    /**
     * 模拟批量导入桩
     *
     * @param locationId        站点id
     * @param pileUploadDTOList 文件对象
     * @return 校验结果
     */
    @Override
    public Result<PileUploadCheckVO> simulateUploadModuleXls(String locationId, List<PileUploadDTO> pileUploadDTOList) {
        return Result.ofSucceed(opLocationPileEvseRepository.simulateUploadModuleXls(locationId, pileUploadDTOList));
    }

    /**
     * 批量保存桩
     *
     * @param pileUploadSaveDTO 保存数据
     * @return 保存结果
     */
    @Override
    public Result<PileUploadSaveVO> savePileList(PileUploadSaveDTO pileUploadSaveDTO) {
        return Result.ofSucceed(opLocationPileEvseRepository.savePileList(pileUploadSaveDTO));
    }

    @Override
    public Result<List<EvseAndTariffIdVO>> getEvseByPileId(PileUploadSaveVO pileUploadSaveVO) {
        return Result.ofSucceed(opLocationPileEvseRepository.getEvseByPileId(pileUploadSaveVO));
    }

    @Override
    public Result<List<CostModelRuleDTO>> queryTariffByPileSN(String pileSN) {
        return opLocationPileEvseRepository.queryTariffByPileSN(pileSN);
    }

    @Override
    public Result<Map<String, CostModelRuleDTO>> queryTariffByPileSN(String... pileSN) {
        return opLocationPileEvseRepository.queryTariffByPileSN(pileSN);
    }

    @Override
    public OpLocationPileEvseElasticDTO findByPileSn(String pileId) {
        return opLocationPileEvseRepository.findByPileSn(pileId);
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId) {
        return opLocationPileEvseRepository.findByLocationId(locationId, null);
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findByLocationId(Long locationId, String keyword) {
        if (StringUtils.isNotBlank(keyword)) {
            return opLocationPileEvseRepository.findByLocationId(locationId, keyword);
        }
        return opLocationPileEvseRepository.findByLocationId(locationId);
    }

    @Override
    public boolean updateRuleId(RelatePileDTO relatePileDTO) {
        return opLocationPileEvseRepository.updateRuleId(relatePileDTO);
    }

    @Override
    public boolean syncRuleName(Long ruleId, String name) {
        return opLocationPileEvseRepository.syncRuleName(ruleId, name);
    }

    @Override
    public Integer syncRule() {
        List<RuleLocationPileEntity> relateEntityList = ruleLocationPileService.findAll();
        if (CollectionUtils.isEmpty(relateEntityList)) {
            return 0;
        }
        Map<Long, RuleLocationPileEntity> relateEntityMap = relateEntityList.stream().collect(Collectors.toMap(RuleLocationPileEntity::getPileId,  Function.identity()));
        List<RuleVO> ruleVoList = ruleRepository.findAllByIds(new ArrayList<>(relateEntityList.stream().map(RuleLocationPileEntity::getRuleId).collect(Collectors.toSet())));
        Map<Long, RuleVO> ruleVoMap = ruleVoList.stream().collect(Collectors.toMap(RuleVO::getId, Function.identity()));
        List<OpLocationPileEvseElasticDTO> pileDtoList = opLocationPileEvseRepository.findListByIds(relateEntityMap.keySet().stream().collect(Collectors.toList()));
        List<IndexQuery> queries = new ArrayList<>(pileDtoList.size());
//        Document document = OpLocationPileEvseElasticDTO.class.getAnnotation(Document.class);
        pileDtoList.stream().forEach(pileDto -> {
            pileDto.setRuleId(relateEntityMap.get(pileDto.getId()).getRuleId());
            log.info("getRuleId {}", pileDto.getRuleId());
            log.info("getRule {}", ruleVoMap.get(pileDto.getRuleId()));
            pileDto.setRuleName(ruleVoMap.get(pileDto.getRuleId()).getName());
            IndexQuery indexQuery = new IndexQuery();
            indexQuery.setId(pileDto.getId().toString());
//            indexQuery.setIndexName(document.indexName());
            indexQuery.setObject(pileDto);
//            indexQuery.setType(document.type());
            queries.add(indexQuery);
        });
        if (queries.size()> 0){
            elasticsearchRestTemplate.bulkIndex(queries, OpLocationPileEvseElasticDTO.class);
//            elasticsearchRestTemplate.refresh(document.indexName());
        }
        return queries.size();
    }

    @Override
    public Result<Page<LocationForAdvertiseDTO>> syncToAdvertise(PileSyncForAdvertiseParamDTO paramDTO) {
        return Result.ofSucceed(opLocationPileEvseRepository.syncToAdvertise(paramDTO));
    }

    @Override
    public Result<Boolean> syncPileOperatorIdCache(String sn) {
        return Result.ofSucceed(opLocationPileEvseRepository.syncPileOperatorIdCache(sn));
    }

    @Override
    public Result<List<OpLocationPileEvseVO>> getListByLocationIds(List<Long> locationIds) {
        return Result.ofSucceed(opLocationPileEvseRepository.getListByLocationIds(locationIds));
    }

    @Override
    public Long updatePile(OpLocationPileEvseDTO dto) {
        if (dto.getPileSn() == null || dto.getOfflineChargeCurrent() == null) {
            log.info("updatePile,dto={}", JSON.toJSONString(dto));
            return 0L;
        }
        OpLocationPileEvseElasticDTO pileEvseDto = opLocationPileEvseRepository.findByPileSn(dto.getPileSn());
        if (pileEvseDto == null) {
            log.info("updatePile,pileEvseDto is null.");
            return 0L;
        }
        pileEvseDto.setOfflineChargeCurrent(dto.getOfflineChargeCurrent());
        opLocationPileEvseRepository.updatePileInfo(pileEvseDto);
        return pileEvseDto.getId();
    }

    /**
     * 落地页设备信息展示
     *
     * @param scanPileEvseDTO
     * @return
     */
    @Override
    public Result<List<OpLocationPileEvseLandingPageVO>> scanPileEvceDetail(ScanPileEvseDTO scanPileEvseDTO) {
        return opLocationPileEvseRepository.scanPileEvceDetail(scanPileEvseDTO);
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findList(List<String> pileSnList) {
        return opLocationPileEvseRepository.findList(pileSnList);
    }

    /**
     * @param currentPage 当前页码
     * @param pageSize    每页数据量
     * @param keyword     关键词（仅支持桩SN的模糊搜索）
     * @return 英标桩列表
     * @function 英国法案认证：查询英标桩列表
     */
    @Override
    public Page<BritishActVO> getBritishActPileListPage(int currentPage, int pageSize, String keyword) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.getBritishActPileListPage currentPage : {} and pageSize : {} and keyword : {}", JSON.toJSONString(currentPage), JSON.toJSONString(pageSize), JSON.toJSONString(keyword));

        Page<BritishActVO> response = new Page<>(currentPage, pageSize);
        List<BritishActVO> britishActVOList = new ArrayList<>();

        // 获取商家id
        Long sellerId = UserUtil.getSellerId();

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.getBritishActPileListPage sellerId : {}", JSON.toJSONString(sellerId));

        // 商家id为空，直接返回
        if (sellerId == null) {
            return response;
        }

        // 从ES中查询英标桩列表
        PageVO<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOPageVO = opLocationPileEvseRepository.getBritishActPileListPage(sellerId, currentPage, pageSize, keyword);
        if (opLocationPileEvseElasticDTOPageVO != null) {
            List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseElasticDTOPageVO.getContent();
            if (ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
                for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
                    britishActVOList.add(OpLocationPileEvseConvert.opLocationPileEvseElasticDTOToBritishActVO(opLocationPileEvseElasticDTO));
                }
            }
            response.setRecords(britishActVOList);
            response.setTotal(opLocationPileEvseElasticDTOPageVO.getTotalRows());
            response.setPages(opLocationPileEvseElasticDTOPageVO.getTotalPages());
        }
        return response;
    }

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 英标桩详情
     * @funtion 英国法案认证：查询英标桩详情
     */
    @Override
    public BritishActVO getBritishActPileDetail(BritishActVO britishActVO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.getBritishActPileDetail britishActVO : {}", JSON.toJSONString(britishActVO));

        String pileSn = britishActVO.getPileSn();
        // 通过英标桩序列号来查询英标桩详情
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileSn);

        if (opLocationPileEvseElasticDTO != null
                && StringUtils.isNotBlank(opLocationPileEvseElasticDTO.getPileSn())
                && LoginUserHolder.getLoginUser() != null
                && LoginUserHolder.getLoginUser().getPayload() != null
                && LoginUserHolder.getLoginUser().getPayload().getUserId() != null) {
            Long userId = LoginUserHolder.getLoginUser().getPayload().getUserId();
            // ThreadPoolUtil.getExecutor().execute(() -> this.queryEvscp(opLocationPileEvseElasticDTO.getPileSn(), userId));
            // ThreadPoolUtil.getExecutor().execute(() -> this.queryOffPeakHour(opLocationPileEvseElasticDTO.getPileSn(), userId));

            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> this.queryEvscp(opLocationPileEvseElasticDTO.getPileSn(), userId)));
            AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> this.queryOffPeakHour(opLocationPileEvseElasticDTO.getPileSn(), userId)));
        }

        return OpLocationPileEvseConvert.opLocationPileEvseElasticDTOToBritishActVO(opLocationPileEvseElasticDTO);
    }

    @Override
    public void queryEvscp(String pileSn, Long userId) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.queryEvscp pileSn : {} and userId : {}",
                JSON.toJSONString(pileSn),
                JSON.toJSONString(userId));

        QueryDTO queryDTO = new QueryDTO();
        queryDTO.setSn(pileSn);
        queryDTO.setUserId(userId);
        Result<Boolean> commandEvscpResult = opsMgmtClient.commandEvscp(queryDTO);

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.queryEvscp commandEvscpResult : {}",
                JSON.toJSONString(commandEvscpResult));

        if (commandEvscpResult != null
                && commandEvscpResult.getData() != null
                && commandEvscpResult.getData()
                && Integer.valueOf(HttpStatus.HTTP_OK).equals(commandEvscpResult.getCode())) {

            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }

            Result<EVSCPParamVO> evscpParamVOResult = opsMgmtClient.queryEvscp(pileSn);

            log.info("===>>> OpLocationPileEvseServiceServiceImpl.queryEvscp evscpParamVOResult : {}",
                    JSON.toJSONString(evscpParamVOResult));

            if (evscpParamVOResult != null
                    && evscpParamVOResult.getData() != null
                    && Integer.valueOf(HttpStatus.HTTP_OK).equals(evscpParamVOResult.getCode())) {
                EVSCPParamVO evscpParamVO = evscpParamVOResult.getData();
                OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileSn);
                if (opLocationPileEvseElasticDTO != null) {
                    opLocationPileEvseElasticDTO.setBritainStandPileMark(true);
                    opLocationPileEvseElasticDTO.setBritainApproveSwitch(evscpParamVO.getEvscpenabled());
                    opLocationPileEvseElasticDTO.setRandomDelaySwitch(evscpParamVO.getRandomDelayEnabled());
                    opLocationPileEvseElasticDTO.setRandomDelayTime(evscpParamVO.getRandomDelayMax());
                    opLocationPileEvseElasticDTO.setUpdatedAt(System.currentTimeMillis());
                    opLocationPileEvseRepository.updatePileInfo(opLocationPileEvseElasticDTO);
                }
            }
        }
    }

    @Override
    public void queryOffPeakHour(String pileSn, Long userId) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.queryOffPeakHour pileSn : {} and userId : {}",
                JSON.toJSONString(pileSn),
                JSON.toJSONString(userId));

        QueryDTO queryDTO = new QueryDTO();
        queryDTO.setSn(pileSn);
        queryDTO.setUserId(userId);
        Result<Boolean> commandOffPeakHourResult = opsMgmtClient.commandOffPeakHour(queryDTO);

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.queryOffPeakHour commandOffPeakHourResult : {}",
                JSON.toJSONString(commandOffPeakHourResult));

        if (commandOffPeakHourResult != null
                && commandOffPeakHourResult.getData() != null
                && commandOffPeakHourResult.getData()
                && Integer.valueOf(HttpStatus.HTTP_OK).equals(commandOffPeakHourResult.getCode())) {

            try {
                Thread.sleep(3000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }

            Result<PileOffPeakHourParamDTO> pileOffPeakHourParamDTOResult = opsMgmtClient.queryOffPeakHour(pileSn);

            log.info("===>>> OpLocationPileEvseServiceServiceImpl.queryOffPeakHour pileOffPeakHourParamDTOResult : {}",
                    JSON.toJSONString(pileOffPeakHourParamDTOResult));

            if (pileOffPeakHourParamDTOResult != null
                    && pileOffPeakHourParamDTOResult.getData() != null
                    && ObjectUtils.isNotEmpty(pileOffPeakHourParamDTOResult.getData().getOffPeakHours())
                    && Integer.valueOf(HttpStatus.HTTP_OK).equals(pileOffPeakHourParamDTOResult.getCode()))  {
                List<OffPeakHourPeriodDTO> offPeakHours = pileOffPeakHourParamDTOResult.getData().getOffPeakHours();
                Integer alertTimeout = pileOffPeakHourParamDTOResult.getData().getAlertTimeout();
                Map<List<TimePeriodDTO>, List<OffPeakHourPeriodDTO>> offPeakHoursMap = offPeakHours.stream().collect(Collectors.groupingBy(OffPeakHourPeriodDTO::getPeriods));
                List<DefaultChargingTimeVO> defaultChargingTimeVOList = new ArrayList<>();
                offPeakHoursMap.forEach((k,v)-> {
                    DefaultChargingTimeVO defaultChargingTimeVO = new DefaultChargingTimeVO();
                    List<Integer> weeks = offPeakHoursMap.get(k).stream().map(OffPeakHourPeriodDTO::getWeekday).collect(Collectors.toList());
                    List<ThatDayDefaultChargingTimeVO> weeksRules = new ArrayList<>();
                    for (TimePeriodDTO timePeriodDTO : k) {
                        ThatDayDefaultChargingTimeVO thatDayDefaultChargingTimeVO = new ThatDayDefaultChargingTimeVO();
                        thatDayDefaultChargingTimeVO.setBeginHour((int) Math.floor(timePeriodDTO.getStartPeriod()/60d/60));
                        thatDayDefaultChargingTimeVO.setBeginMinute(timePeriodDTO.getStartPeriod()/60%60);
                        thatDayDefaultChargingTimeVO.setEndHour((int) Math.floor((timePeriodDTO.getStartPeriod() + timePeriodDTO.getDuration())/60d/60));
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

                String jsonString = JSON.toJSONString(defaultChargingTimeVOList);
                Long currentTimeMillis = System.currentTimeMillis();

                log.info("===>>> OpLocationPileEvseServiceServiceImpl.queryOffPeakHour jsonString : {}",
                        jsonString);

                OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getPileInfoByPileSn(pileSn);
                if (opLocationPileEvseEntity != null) {
                    opLocationPileEvseEntity.setDefaultChargingTime(jsonString);
                    opLocationPileEvseEntity.setUpdatedAt(currentTimeMillis);
                    opLocationPileEvseRepository.updateById(opLocationPileEvseEntity);
                }

                OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileSn);
                if (opLocationPileEvseElasticDTO != null) {
                    opLocationPileEvseElasticDTO.setBritainStandPileMark(true);
                    opLocationPileEvseElasticDTO.setDefaultChargingTime(jsonString);
                    opLocationPileEvseElasticDTO.setUpdatedAt(currentTimeMillis);
                    opLocationPileEvseRepository.updatePileInfo(opLocationPileEvseElasticDTO);
                }
            }
        }
    }

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 英标桩详情
     * @funtion 英国法案认证：编辑默认充电时间
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public BritishActVO updateBritishActPileDefaultChargingTime(BritishActVO britishActVO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.updateBritishActPileDefaultChargingTime britishActVO : {}", JSON.toJSONString(britishActVO));

        // 默认是修改成功的
        britishActVO.setSuccessfullyModifiedFlag(Boolean.TRUE);

        // 先查询英标桩（根据英标桩序列号进行查询）
        String pileSn = britishActVO.getPileSn();
        Long id = britishActVO.getId();
        List<DefaultChargingTimeVO> defaultChargingTimeVO = britishActVO.getDefaultChargingTimeVO();

        OpLocationPileEvseElasticDTO searchByPileSn = opLocationPileEvseRepository.findByPileSn(pileSn);
        if (searchByPileSn == null
                || !id.equals(searchByPileSn.getId())
                || !Boolean.TRUE.equals(searchByPileSn.getBritainStandPileMark())) {
            britishActVO.setSuccessfullyModifiedFlag(Boolean.FALSE);

            log.info("===>>>英国法案认证：编辑默认充电时间失败！(不存在这样的英标桩)");

        } else {

            log.info("===>>>英国法案认证：编辑默认充电时间(开始进行修改操作!)");

            OpLocationPileEvseEntity opLocationPileEvseEntity = new OpLocationPileEvseEntity();
            opLocationPileEvseEntity.setId(id);
            opLocationPileEvseEntity.setUpdatedAt(System.currentTimeMillis());
            opLocationPileEvseEntity.setPileSn(pileSn);
            if (ObjectUtils.isNotEmpty(defaultChargingTimeVO)) {
                opLocationPileEvseEntity.setDefaultChargingTime(JSON.toJSONString(defaultChargingTimeVO));
            }

            List<OffPeakHourPeriodDTO> offPeakHours = new ArrayList<>();
            OffPeakHourParamDTO offPeakHourParamDTO = new OffPeakHourParamDTO();
            if (!ObjectUtils.isEmpty(britishActVO.getDefaultChargingTimeVO())) {
                for (DefaultChargingTimeVO chargingTimeVO : britishActVO.getDefaultChargingTimeVO()) {
                    for (Integer week : chargingTimeVO.getWeeks()) {
                        OffPeakHourPeriodDTO offPeakHourPeriodDTO = new OffPeakHourPeriodDTO();
                        offPeakHourPeriodDTO.setWeekday(week);
                        if (offPeakHourPeriodDTO.getWeekday().equals(7)) {
                            offPeakHourPeriodDTO.setWeekday(0);
                        }
                        if (!ObjectUtils.isEmpty(chargingTimeVO.getWeeksRules().get(0).getBeginTime())) {
                            List<TimePeriodDTO> periods = new ArrayList<>();
                            for (ThatDayDefaultChargingTimeVO weeksRule : chargingTimeVO.getWeeksRules()) {
                                int startTime = weeksRule.getBeginHour()*60*60 + weeksRule.getBeginMinute()*60;
                                int endTime = weeksRule.getEndHour()*60*60 + weeksRule.getEndMinute()*60;
                                TimePeriodDTO timePeriodDTO = new TimePeriodDTO();
                                timePeriodDTO.setStartPeriod(startTime);
                                timePeriodDTO.setDuration(endTime-startTime);
                                periods.add(timePeriodDTO);
                            }
                            offPeakHourPeriodDTO.setPeriods(periods);
                        }
                        offPeakHours.add(offPeakHourPeriodDTO);
                    }
                }
                offPeakHourParamDTO.setOffPeakHours(offPeakHours);
            }
            ConfigOffPeakVO configOffPeakVO = new ConfigOffPeakVO();
            configOffPeakVO.setSn(pileSn);
            configOffPeakVO.setUserId(LoginUserHolder.getLoginUser().getId());
            offPeakHourParamDTO.setAlertEnabled(britishActVO.getBritainApproveSwitch());
            offPeakHourParamDTO.setAlertTimeout(britishActVO.getAlertTimeout());
            configOffPeakVO.setOffPeakHourParamDTO(offPeakHourParamDTO);
            log.info("defaultCharging,OffPeakHourParamDTO={}",JSON.toJSONString(configOffPeakVO.getOffPeakHourParamDTO()));
            Result<Boolean> offPeakHourParam = opsMgmtClient.sendOffPeakHourParam(configOffPeakVO);
            log.info("defaultCharging,offPeakHourParam={}",offPeakHourParam.getData());
            if (offPeakHourParam.getData() == false) {
                britishActVO.setSuccessfullyModifiedFlag(false);
                return britishActVO;
            }
            if (opLocationPileEvseRepository.updateBritishActPileDefaultChargingTime(opLocationPileEvseEntity,britishActVO.getBritainApproveSwitch())) {
                // 修改操作成功，重新查询数据
                OpLocationPileEvseElasticDTO searchByUpdatedPileSn = opLocationPileEvseRepository.findByPileSn(pileSn);
                // 转化后重新赋值
                britishActVO = OpLocationPileEvseConvert.opLocationPileEvseElasticDTOToBritishActVO(searchByUpdatedPileSn);
                britishActVO.setSuccessfullyModifiedFlag(Boolean.TRUE);
            } else {
                britishActVO.setSuccessfullyModifiedFlag(Boolean.FALSE);
            }
        }
        return britishActVO;
    }

    /**
     * @param britishActVO 英标桩详情 入参对象
     * @return 是否需要提醒
     * @funtion 英国法案认证：高峰期充电增加提醒
     */
    @Override
    public Boolean isPublicPileStartDuringPeakElectricityConsumption(BritishActVO britishActVO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.isPublicPileStartDuringPeakElectricityConsumption britishActVO : {}", JSON.toJSONString(britishActVO));

        // 默认不需要提醒
        Boolean flag = Boolean.FALSE;

        @NotNull String pileSn = britishActVO.getPileSn();
        Long currentTimeTimestamp = britishActVO.getCurrentTimeTimestamp();
        // 查询充电桩使用场景
        //根据桩SN查询charge_pile表的使用场景
        Result<PileStandInfoDTO> dtoResult = deviceServiceFeign.queryStandInfo(pileSn);

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.isPublicPileStartDuringPeakElectricityConsumption dtoResult : {}", JSON.toJSONString(dtoResult));

        if (dtoResult != null && HttpStatus.HTTP_OK == dtoResult.getCode()) {
            PileStandInfoDTO data = dtoResult.getData();
            if (data != null) {
                Integer usage = data.getUsage();
                if (EVSETypeEnum.BUSINESS_PILE.getCode().equals(usage)) {
                    // 查询商桩时区（场站时区）
                    OpLocationPileEvseElasticDTO searchByPileSn = opLocationPileEvseRepository.findByPileSn(pileSn);
                    if (searchByPileSn != null) {
                        Long locationId = searchByPileSn.getLocationId();
                        String defaultChargingTime = searchByPileSn.getDefaultChargingTime();
                        Boolean britainStandPileMark = searchByPileSn.getBritainStandPileMark();
                        if (locationId != null
                                && britainStandPileMark != null
                                && britainStandPileMark
                                && !ObjectUtils.isEmpty(searchByPileSn.getBritainApproveSwitch())
                                && searchByPileSn.getBritainApproveSwitch()) {
                            Set<Long> locationIds = new HashSet<>();
                            locationIds.add(locationId);
                            List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByIdIn(locationIds);

                            log.info("===>>>OpLocationPileEvseServiceServiceImpl.isPublicPileStartDuringPeakElectricityConsumption opLocationElasticDTOList : {}", JSON.toJSONString(opLocationElasticDTOList));

                            if (ObjectUtils.isNotEmpty(opLocationElasticDTOList)) {
                                for (OpLocationElasticDTO opLocationElasticDTO : opLocationElasticDTOList) {
                                    Long id = opLocationElasticDTO.getId();
                                    String zoneId = opLocationElasticDTO.getZoneId();
                                    if (locationId.equals(id) && TariffUtil.isPublicPileStartDuringPeakElectricityConsumption(zoneId, defaultChargingTime, currentTimeTimestamp)) {
                                        flag = Boolean.TRUE;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        return flag;
    }

    /**
     * @param pileSnList 充电桩序列号集合
     * @return 判断结果
     * @function 判断充电桩是不是英标桩
     */
    @Override
    public Map<String, Boolean> judgeBritainStand(List<String> pileSnList) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.judgeBritainStand pileSnList : {}", JSON.toJSONString(pileSnList));

        if (ObjectUtils.isEmpty(pileSnList)) {
            return null;
        }
        Map<String, Boolean> map = new HashMap<>();
        for (String pileSn : pileSnList) {
            map.put(pileSn, Boolean.FALSE);
        }
        Result<Map<String, String>> result = deviceServiceFeign.judgeBritainStand(pileSnList);

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.judgeBritainStand result : {}", JSON.toJSONString(result));

        if (result != null && HttpStatus.HTTP_OK == result.getCode()) {
            Map<String, String> data = result.getData();
            if (ObjectUtils.isNotEmpty(data)) {
                Set<String> keySet = data.keySet();
                for (String needJudgePileSn : pileSnList) {
                    for (String pileSn : keySet) {
                        if (StrUtil.isNotBlank(needJudgePileSn) && needJudgePileSn.equals(pileSn)) {
                            String judgeFlag = data.get(needJudgePileSn);
                            if ("1".equals(judgeFlag)) {
                                map.put(needJudgePileSn, Boolean.TRUE);
                            }
                            break;
                        }
                    }
                }
            }
        }
        return map;
    }

    /**
     * @param modifyCommonPileToBritainStandPileForTestVO 为方便测试英国法案需求(把充电桩修改为英标桩) 出参模型
     * @param operationPassword                           为防止误操作，需要输入操作密令
     * @return 修改结果
     * @function 为方便测试英国法案需求(需要把充电桩修改为英标桩)
     */
    @Override
    public Map<String, Boolean> modifyCommonPileToBritainStandPileForTest(ModifyCommonPileToBritainStandPileForTestVO modifyCommonPileToBritainStandPileForTestVO, String operationPassword) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.modifyCommonPileToBritainStandPileForTest modifyCommonPileToBritainStandPileForTestVO : {} and operationPassword : {}", JSON.toJSONString(modifyCommonPileToBritainStandPileForTestVO), JSON.toJSONString(operationPassword));

        List<String> needToModifyCommonPileToBritainStandPileList = new ArrayList<>();
        Boolean isModifyAllCommonPileToBritainStandPileFlag = modifyCommonPileToBritainStandPileForTestVO.getIsModifyAllCommonPileToBritainStandPileFlag();
        List<String> pileSnList = modifyCommonPileToBritainStandPileForTestVO.getPileSnList();
        if (isModifyAllCommonPileToBritainStandPileFlag != null && isModifyAllCommonPileToBritainStandPileFlag) {
            // 获取商家id
            Long sellerId = UserUtil.getSellerId();

            log.info("===>>>OpLocationPileEvseServiceServiceImpl.modifyCommonPileToBritainStandPileForTest sellerId : {}", JSON.toJSONString(sellerId));

            if (sellerId != null) {
                // 查询场站表，获得所有的场站信息
                List<OpLocationEntity> locationInfoListBySellerId = opLocationRepository.getLocationInfoBySellerId(sellerId);

                log.info("===>>>OpLocationPileEvseServiceServiceImpl.modifyCommonPileToBritainStandPileForTest locationInfoListBySellerId : {}", JSON.toJSONString(locationInfoListBySellerId));

                if (ObjectUtils.isNotEmpty(locationInfoListBySellerId)) {
                    List<Long> locationIdList = locationInfoListBySellerId.stream().map(OpLocationEntity::getId).collect(Collectors.toList());
                    if (ObjectUtils.isNotEmpty(locationIdList)) {
                        // 查询充电桩表，获得所有的充电桩信息
                        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.getPileInfoListByLocationIdList(locationIdList);

                        log.info("===>>>OpLocationPileEvseServiceServiceImpl.modifyCommonPileToBritainStandPileForTest opLocationPileEvseEntityList : {}", JSON.toJSONString(opLocationPileEvseEntityList));

                        if (ObjectUtils.isNotEmpty(opLocationPileEvseEntityList)) {
                            needToModifyCommonPileToBritainStandPileList.addAll(opLocationPileEvseEntityList.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList()));
                        }
                    }
                }
            }
        }
        if (ObjectUtils.isNotEmpty(pileSnList)) {
            needToModifyCommonPileToBritainStandPileList.addAll(pileSnList);
        }
        Map<String, Boolean> map = new HashMap<>();
        if (ObjectUtils.isNotEmpty(needToModifyCommonPileToBritainStandPileList)) {
            for (String pileSn : needToModifyCommonPileToBritainStandPileList) {
                map.put(pileSn, Boolean.FALSE);
            }
        }
        if (BaseConstant.OPERATION_PASSWORD.equals(operationPassword) && ObjectUtils.isNotEmpty(needToModifyCommonPileToBritainStandPileList)) {
            for (String pileSn : needToModifyCommonPileToBritainStandPileList) {
                // 根据充电桩序列号查询充电桩信息
                OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getPileInfoByPileSn(pileSn);
                if (opLocationPileEvseEntity != null) {
                    opLocationPileEvseEntity.setDefaultChargingTime(TariffUtil.defaultChargingTemplate());
                    opLocationPileEvseEntity.setUpdatedAt(System.currentTimeMillis());
                    // 修改数据库和ES
                    Boolean flag = opLocationPileEvseRepository.synchronousBritishActPileDefaultChargingTime(opLocationPileEvseEntity,false);
                    if (flag) {
                        // 修改成功，需要同步修改device数据库中的判断标志
                        Result<Boolean> result = deviceServiceFeign.modifyDtChargerInfoTableForTest(pileSn, BaseConstant.OPERATION_PASSWORD);
                        if (result != null
                                && HttpStatus.HTTP_OK == result.getCode()
                                && result.getData() != null) {
                            map.put(pileSn, result.getData());
                        }
                    }
                }
            }
        }
        return map;
    }

    @Override
    public Boolean syncBritainPile() {
        List<OpLocationPileEvseEntity> opLocationPileEvseEntitys = opLocationPileEvseRepository.selectPlieList();
        List<String> pileSnList = opLocationPileEvseEntitys.stream().map(OpLocationPileEvseEntity::getPileSn).collect(Collectors.toList());
        Map<String, Boolean> britainPileMap = judgeBritainStand(pileSnList);
        List<OpLocationPileEvseEntity> britainPileList = new ArrayList<>();
        opLocationPileEvseEntitys.forEach( m -> {
            if (!ObjectUtils.isEmpty(britainPileMap) && britainPileMap.containsKey(m.getPileSn()) && britainPileMap.get(m.getPileSn())) {
                britainPileList.add(m);
            }
        });
        britainPileList.forEach(n-> {
            if (StringUtils.isBlank(n.getDefaultChargingTime())) {
                n.setUpdatedAt(System.currentTimeMillis());
                n.setDefaultChargingTime(TariffUtil.defaultChargingTemplate());
            }
            opLocationPileEvseRepository.synchronousBritishActPileDefaultChargingTime(n,false);
        });
        return true;
    }

    @Override
    public Result<Boolean> randomDelay(RandomDelayDTO randomDelayDTO) {

        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.getOpLocationPileEvseElasticDTOByPileSnFromES(randomDelayDTO.getPileSn());
        if (opLocationPileEvseElasticDTO == null) {
            throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
        }

        RandomDelayTimeDTO randomDelayTimeDTO = new RandomDelayTimeDTO();
        randomDelayTimeDTO.setRandomDelayEnabled(randomDelayDTO.getRandomDelaySwitch());
        randomDelayTimeDTO.setEVSCPEnabled(opLocationPileEvseElasticDTO.getBritainApproveSwitch());
        if (!ObjectUtils.isEmpty(randomDelayDTO.getRandomDelayTime()) && randomDelayDTO.getRandomDelayTime() < randomDelayMin) {
            randomDelayDTO.setRandomDelayTime(randomDelayMin);
            //运维要求最小值默认传0
            randomDelayTimeDTO.setRandomDelayMin(0);
            randomDelayTimeDTO.setRandomDelayMax(randomDelayMin);
        }else if (!ObjectUtils.isEmpty(randomDelayDTO.getRandomDelayTime()) && randomDelayDTO.getRandomDelayTime() > randomDelayMax) {
            randomDelayDTO.setRandomDelayTime(randomDelayMax);
            randomDelayTimeDTO.setRandomDelayMin(0);
            randomDelayTimeDTO.setRandomDelayMax(randomDelayMax);
        }else {
            randomDelayTimeDTO.setRandomDelayMin(0);
            randomDelayTimeDTO.setRandomDelayMax(randomDelayDTO.getRandomDelayTime());
        }

        ConfigBritishActVO configBritishActVO = new ConfigBritishActVO();
        configBritishActVO.setRandomDelayTimeDTO(randomDelayTimeDTO);
        configBritishActVO.setUserId(LoginUserHolder.getLoginUser().getId());
        configBritishActVO.setSn(randomDelayDTO.getPileSn());
        randomDelayDTO.setConfigBritishActVO(configBritishActVO);
        return Result.ofSucceed(opLocationPileEvseRepository.randomDelay(randomDelayDTO));
    }

    @Override
    public RemoteStartPileDataVO getRemoteStartPileData(String sn) {
        int size = opLocationPileEvseRepository.getGunCountBySn(sn);

        Result<List<CurrencyDTO>> currencyListResult = tariffFeignClient.getCurrencyList();
        RemoteStartPileDataVO remoteStartPileDataVO = new RemoteStartPileDataVO();
        if(currencyListResult != null && currencyListResult.getData() != null){
            remoteStartPileDataVO.setCurrency(currencyListResult.getData().get(0));
        }else{
            CurrencyDTO currencyDTO = new CurrencyDTO();
            currencyDTO.setId(1L);
            currencyDTO.setCurrencyType("USD");
            currencyDTO.setCurrencySign("$");
            currencyDTO.setCurrencyTypeCn("美元");
            currencyDTO.setCode(840);
            currencyDTO.setDeleted(0);
            remoteStartPileDataVO.setCurrency(currencyDTO);
        }

        List<RemoteStartEnabledGunVO> remoteStartEnabledGunList = new ArrayList<>();
        log.info("getRemoteStartPileData sn :{}, 枪列表size:{}", sn, size);
        if(size > 0){
            List<String> evseSnList = new ArrayList<>();
            for(int i=1; i<=size; i++){
                evseSnList.add(sn + "_" + i);
            }

            Map<String, String> snStatusMap = monitorFeignClient.queryStatusByEvseSnList(evseSnList).getData();
            Map<String, Long> snTariffMap = opLocationEvseService.queryTariffByEvseSnList(evseSnList);

            for(int i=1; i<=size; i++){
                String pileGunTransactionKey = RedisKeyConstant.getChargePileGunKey(sn, i+"");
                String transactionMsg = stringRedisTemplate.opsForValue().get(pileGunTransactionKey);
                boolean remoteStartEnabled = getRemoteStartEnabled(snStatusMap.get(sn + "_" + i)) && snTariffMap.get(sn + "_" + i) != null
                        && StringUtils.isBlank(transactionMsg);
                RemoteStartEnabledGunVO remoteStartEnabledGunVO = RemoteStartEnabledGunVO.builder()
                        .gunNo(String.format("%02d",i))
                        .remoteStartEnabled(remoteStartEnabled)
                        .freeEnabled(isFreePrice(sn, i))
                        .build();
                remoteStartEnabledGunList.add(remoteStartEnabledGunVO);
            }

            /*Comparator<RemoteStartEnabledGunVO> a = Comparator.comparing(RemoteStartEnabledGunVO::getGunNo);
            Comparator<RemoteStartEnabledGunVO> b = Comparator.comparing(RemoteStartEnabledGunVO::getRemoteStartEnabled).reversed();
            Comparator<RemoteStartEnabledGunVO> c = b.thenComparing(a);*/
            remoteStartEnabledGunList = remoteStartEnabledGunList.stream().sorted(Comparator.comparing(o-> new Integer((o.getGunNo())))).collect(Collectors.toList());
        }
        remoteStartPileDataVO.setRemoteStartEnabledGunList(remoteStartEnabledGunList);

        return remoteStartPileDataVO;
    }

    /**
     * @param setPileEroamingDTO 设置充电桩的互联互通开关 入参模型
     * @return 操作结果
     * @function 设置充电桩的互联互通开关
     */
    @Override
    public boolean setPileEroaming(SetPileEroamingDTO setPileEroamingDTO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.setPileEroaming setPileEroamingDTO : {}", JSON.toJSONString(setPileEroamingDTO));

        if (setPileEroamingDTO == null
                || StringUtils.isBlank(setPileEroamingDTO.getPileSn())
                || setPileEroamingDTO.getPileEroamingOperateType() == null) {
            return false;
        }

        // 设置充电桩的互联互通开关（将数据保存到数据库和ES）
        if (opLocationPileEvseRepository.setPileEroaming(setPileEroamingDTO) ) {
          if (Integer.valueOf(1).equals(setPileEroamingDTO.getPileEroamingOperateType())) {
              //充电桩开启互联互通开关后，需要将枪列表及其对应平台的计费推送至互联互通平台
              pileBaseAsync.pileEromingAsync(setPileEroamingDTO.getPileSn(), ActionType.insert.getCode());
              pileBaseAsync.pushEvseList(setPileEroamingDTO.getPileSn(), ActionType.insert.getCode());
          } else {
              pileBaseAsync.pileEromingAsync(setPileEroamingDTO.getPileSn(), ActionType.delete.getCode());
              pileBaseAsync.pushEvseList(setPileEroamingDTO.getPileSn(), ActionType.delete.getCode());
          }
        }
        return true;
    }

    /**
     * @return 同步条数
     * @function 同步充电桩的公开属性标志
     */
    @Override
    public int syncPilePublicProperty() {
        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.selectPlieList();
        int count = 0;
        // 循环更新
        if (ObjectUtils.isNotEmpty(opLocationPileEvseEntityList)) {
            for (OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntityList) {
                // 根据MySQL数据库中的数据同步ES数据
                boolean flag = opLocationPileEvseRepository.syncPilePublicPropertyInES(opLocationPileEvseEntity);
                if (flag) {
                    count++;
                }
            }
        }
        return count;
    }

    @Override
    public OpLocationPileEvseEntity findLast(String pileSn) {
        OpLocationPileEvseEntity entity = opLocationPileEvseRepository.findLast(pileSn);
        return entity;
    }

    @Override
    public List<OpLocationPileEvseElasticDTO> findList(Long operatorId, List<String> includeField) {
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        query.must(QueryBuilders.termQuery("operatorId", operatorId));
        NativeSearchQuery builder = new NativeSearchQueryBuilder()
                .withPageable(PageRequest.of(0, 100))
                .withQuery(query)
                .build();
        if (!CollectionUtils.isEmpty(includeField)) {
            builder.addSourceFilter(new FetchSourceFilter(includeField.toArray(new String[0]), null));
        }
        List<OpLocationPileEvseElasticDTO> pileDtoList = elasticsearchRestTemplate.searchForStream(builder, OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        return pileDtoList;
    }

    /**
     * @param pileSimpleInfoQueryDTOList
     * @return
     * @function 查询充电桩信息
     */
    @Override
    public List<PileInfoVO> queryPileInfo(List<PileSimpleInfoQueryDTO> pileSimpleInfoQueryDTOList) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.queryPileInfo pileSimpleInfoQueryDTOList : {}", JSON.toJSONString(pileSimpleInfoQueryDTOList));

        if (ObjectUtils.isNotEmpty(pileSimpleInfoQueryDTOList)) {
            List<String> pileSnList = pileSimpleInfoQueryDTOList
                    .stream()
                    .filter(var -> (var != null && StringUtils.isNotBlank(var.getPileSn())))
                    .map(PileSimpleInfoQueryDTO::getPileSn)
                    .collect(Collectors.toList());
            List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.findPileInfoInPileSnList(pileSnList);
            if (ObjectUtils.isNotEmpty(opLocationPileEvseEntityList)) {
                List<Long> locationIdList = opLocationPileEvseEntityList
                        .stream()
                        .filter(var -> var.getLocationId() != null)
                        .map(OpLocationPileEvseEntity::getLocationId)
                        .collect(Collectors.toList());
                List<OpLocationEntity> opLocationEntityList = opLocationRepository.findLocationInfoInLocationList(locationIdList);
                // 构建场站id与其对应的场站信息之间的映射关系
                Map<Long, OpLocationEntity> locationIdAndOpLocationEntityMap = new HashMap<>();
                if (ObjectUtils.isNotEmpty(opLocationEntityList)) {
                    opLocationEntityList.forEach(var -> locationIdAndOpLocationEntityMap.put(var.getId(), var));
                }
                List<PileInfoVO> pileInfoVOList = new ArrayList<>();
                for (OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntityList) {
                    PileInfoVO pileInfoVO = new PileInfoVO();
                    pileInfoVO.setId(opLocationPileEvseEntity.getId());
                    pileInfoVO.setPileSn(opLocationPileEvseEntity.getPileSn());
                    pileInfoVO.setPileName(opLocationPileEvseEntity.getName());
                    pileInfoVO.setUpdatedAt(opLocationPileEvseEntity.getUpdatedAt());
                    if (opLocationPileEvseEntity.getLocationId() != null
                            && locationIdAndOpLocationEntityMap.get(opLocationPileEvseEntity.getLocationId()) != null) {
                        OpLocationEntity opLocationEntity = locationIdAndOpLocationEntityMap.get(opLocationPileEvseEntity.getLocationId());
                        LocationInfoVO pileLocationInfoVO = new LocationInfoVO();
                        pileLocationInfoVO.setLocationId(opLocationEntity.getId());
                        pileLocationInfoVO.setLocationName(opLocationEntity.getName());
                        pileLocationInfoVO.setCountry(opLocationEntity.getCountry());
                        pileLocationInfoVO.setProvince(opLocationEntity.getProvince());
                        pileLocationInfoVO.setUpdatedAt(opLocationEntity.getUpdatedAt());
                        pileInfoVO.setPileLocationInfoVO(pileLocationInfoVO);
                    }
                    pileInfoVOList.add(pileInfoVO);
                }
                return pileInfoVOList;
            }
        }
        return null;
    }

    @Override
    public Result<Boolean> updateEsSubscriptionStatusByPileSnList(UpdateEsSubscriptionStatusDTO updateEsSubscriptionStatusDTO) {
        return opLocationPileEvseRepository.updateEsSubscriptionStatusByPileSnList(updateEsSubscriptionStatusDTO);

    }

    @Override
    public Boolean handleEvscpSettingDTO(EvscpSettingDTO evscpSettingDTO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.handleEvscpSettingDTO evscpSettingDTO : {}",
                JSON.toJSONString(evscpSettingDTO));

        if (evscpSettingDTO == null
                || StringUtils.isBlank(evscpSettingDTO.getPileSn())) {
            return false;
        }

        stringRedisTemplate.opsForValue().set(
                RedisKeyConstant.getPileReportBritishActKey(evscpSettingDTO.getPileSn()),
                JSON.toJSONString(evscpSettingDTO),
                180L,
                TimeUnit.DAYS);

        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.getOpLocationPileEvseElasticDTOByPileSnFromES(evscpSettingDTO.getPileSn());
        if (opLocationPileEvseElasticDTO != null
                && opLocationPileEvseElasticDTO.getId() != null) {
            opLocationPileEvseElasticDTO.setBritainApproveSwitch(evscpSettingDTO.getEnableRegulation());
            opLocationPileEvseElasticDTO.setRandomDelaySwitch(evscpSettingDTO.getEnableRandomDelay());
            opLocationPileEvseRepository.updatePileInfo(opLocationPileEvseElasticDTO);
        }
        return true;
    }


    @Override
    public Page<PileBasicInfoVO> getPileBasicInfoVOPage(SearchDTO pileSimpleInfoQueryDTO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.getPileBasicInfoVOPage getLocationBasicInfoVOPage : {}",
                JSON.toJSONString(pileSimpleInfoQueryDTO));

        Page<PileBasicInfoVO> pileBasicInfoVOPage = new Page<>();
        if (pileSimpleInfoQueryDTO == null
                || pileSimpleInfoQueryDTO.getLocationId() == null) {
            return pileBasicInfoVOPage;
        }

        if (org.apache.commons.lang3.StringUtils.isNotBlank(pileSimpleInfoQueryDTO.getSearchValue())) {
            LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
            lambdaQueryWrapper
                    .eq(OpLocationPileEvseEntity::getLocationId, pileSimpleInfoQueryDTO.getLocationId())
                    .like(OpLocationPileEvseEntity::getPileSn, StringUtil.escapeChar(pileSimpleInfoQueryDTO.getSearchValue()))
                    .eq(OpLocationPileEvseEntity::getDeleted, 0)
                    .orderByDesc(OpLocationPileEvseEntity::getUpdatedAt, OpLocationPileEvseEntity::getId);

            List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.getBaseMapper().selectList(lambdaQueryWrapper);
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(opLocationPileEvseEntityList)) {
                return pileBasicInfoVOPage;
            }

            List<PileBasicInfoVO> pileBasicInfoVOList = new ArrayList<>();
            opLocationPileEvseEntityList.forEach(var -> {
                PileBasicInfoVO pileBasicInfoVO = new PileBasicInfoVO();
                pileBasicInfoVO.setId(var.getId());
                pileBasicInfoVO.setPileSn(var.getPileSn());
                pileBasicInfoVO.setPileName(var.getName());
                pileBasicInfoVOList.add(pileBasicInfoVO);
            });

            pileBasicInfoVOPage.setCurrent(1L);
            pileBasicInfoVOPage.setTotal(pileBasicInfoVOList.size());
            pileBasicInfoVOPage.setPages(1L);
            pileBasicInfoVOPage.setSize(pileBasicInfoVOList.size());
            pileBasicInfoVOPage.setRecords(pileBasicInfoVOList);
            return pileBasicInfoVOPage;
        }

        Page<OpLocationPileEvseEntity> page = new Page<>(pileSimpleInfoQueryDTO.getPage(), pileSimpleInfoQueryDTO.getPageSize());
        LambdaQueryWrapper<OpLocationPileEvseEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpLocationPileEvseEntity::getLocationId, pileSimpleInfoQueryDTO.getLocationId())
                .eq(OpLocationPileEvseEntity::getDeleted, 0)
                .orderByDesc(OpLocationPileEvseEntity::getUpdatedAt, OpLocationPileEvseEntity::getId);
        opLocationPileEvseRepository.getBaseMapper().selectPage(page, lambdaQueryWrapper);

        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(page.getRecords())) {
            List<PileBasicInfoVO> pileBasicInfoVOList = new ArrayList<>();
            page.getRecords().forEach(var -> {
                PileBasicInfoVO pileBasicInfoVO = new PileBasicInfoVO();
                pileBasicInfoVO.setId(var.getId());
                pileBasicInfoVO.setPileSn(var.getPileSn());
                pileBasicInfoVO.setPileName(var.getName());
                pileBasicInfoVOList.add(pileBasicInfoVO);
            });

            pileBasicInfoVOPage.setCurrent(page.getCurrent());
            pileBasicInfoVOPage.setTotal(page.getTotal());
            pileBasicInfoVOPage.setPages(page.getPages());
            pileBasicInfoVOPage.setSize(page.getSize());
            pileBasicInfoVOPage.setRecords(pileBasicInfoVOList);
        }
        return pileBasicInfoVOPage;
    }

    @Override
    public OcppConfigurationViewEnableVO ocppConfigurationViewEnable(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.ocppConfigurationViewEnable pileSn : {}",
                JSON.toJSONString(pileSn));

        if (StringUtils.isBlank(pileSn)) {
            return null;
        }

        OcppConfigurationViewEnableVO ocppConfigurationViewEnableVO = new OcppConfigurationViewEnableVO();
        ocppConfigurationViewEnableVO.setPileSn(pileSn);
        ocppConfigurationViewEnableVO.setOfflineChargeCurrentViewEnable(this.judgePileOfflineChargeCurrentEnable(pileSn));
        ocppConfigurationViewEnableVO.setClearSmartChargingProfileViewEnable(this.judgeClearPileSmartChargingProfileEnable(pileSn));
        ocppConfigurationViewEnableVO.setFreeVendViewEnable(this.judgePileFreeVendViewEnable(pileSn));
        return ocppConfigurationViewEnableVO;
    }

    private boolean judgeClearPileSmartChargingProfileEnable(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgeClearPileSmartChargingProfileEnable pileSn : {}",
                JSON.toJSONString(pileSn));

        OpLocationPileGroupEntity opLocationPileGroupEntity = opLocationPileGroupRepository.findName(pileSn);

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgeClearPileSmartChargingProfileEnable opLocationPileGroupEntity : {}",
                JSON.toJSONString(opLocationPileGroupEntity));

        return opLocationPileGroupEntity == null;
    }

    private boolean judgePileOfflineChargeCurrentEnable(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileOfflineChargeCurrentEnable pileSn : {}",
                JSON.toJSONString(pileSn));

        boolean flag = false;
        List<String> pileSnList = new ArrayList<>();
        pileSnList.add(pileSn);
        try {
            Result<Map<String, Boolean>> result = deviceServiceFeign.batchJudgePileOfflineChargeCurrentEnable(pileSnList);

            log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileOfflineChargeCurrentEnable result : {}",
                    JSON.toJSONString(result));

            if (result != null
                    && Integer.valueOf(HttpStatus.HTTP_OK).equals(result.getCode())
                    && ObjectUtils.isNotEmpty(result.getData())
                    && result.getData().get(pileSn) != null
                    && result.getData().get(pileSn)) {
                flag = true;
            }
        } catch (Exception e) {

            log.error("===>>> OpLocationPileEvseServiceServiceImpl.judgePileOfflineChargeCurrentEnable pileSn : {}, 调用device服务出现了异常！", pileSn, e);

            flag = false;
        }
        return flag;
    }

    @Override
    public List<UnionPileTreeVO> searchPileBySellerId(Long operatorId, Long locationId) {
        //查询桩信息
        List<OpLocationPileEvseElasticDTO> pileInformationList = opLocationPileEvseRepository.getAllPileInfoBySellerId(operatorId);
        //查询枪信息
        List<OpPileAssociatedRuleDTO> resultList = new ArrayList<>();
        for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : pileInformationList) {
            OpPileAssociatedRuleDTO ruleDTO = OpLocationPileEvseConvert.toOpPileAssociatedRuleDTO(opLocationPileEvseElasticDTO);
            List<Long> evseIdList = Optional.ofNullable(JSON.parseArray(opLocationPileEvseElasticDTO.getEvseList()))
                    .orElse(JSON.parseArray("[]")).toJavaList(Long.class);
            if (!CollectionUtils.isEmpty(evseIdList)) {
            List<OpEvseAssociatedRuleDTO> evseRuleList = opLocationEvseRepository.searchGunInformation(evseIdList);
            ruleDTO.setOpEvseAssociatedRuleDTOList(evseRuleList);
            }
            resultList.add(ruleDTO);
        }
        List<UnionPileTreeVO> pileTree = getPileTree(resultList);
        this.gunNumberSorting(pileTree);
        return pileTree;
    }

    @Override
    public Boolean clearPileSmartChargingProfile(ClearSmartChargingProfileDTO clearSmartChargingProfileDTO) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.clearPileSmartChargingProfile clearSmartChargingProfileDTO : {}",
                JSON.toJSONString(clearSmartChargingProfileDTO));

        String evseSn = clearSmartChargingProfileDTO.getPileSn() + "_" + "1";
        Result<String> pileStatusResult = monitorFeignClient.queryStatusByEvseSn(evseSn);

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.clearPileSmartChargingProfile pileStatusResult : {}",
                JSON.toJSONString(pileStatusResult));

        if (LoginUserHolder.getLoginUser().getPayload().getUserId() == null
                || pileStatusResult == null
                || !Integer.valueOf(HttpStatus.HTTP_OK).equals(pileStatusResult.getCode())
                || EvseDeviceStatusEnum.DEFAULT.getName().equalsIgnoreCase(pileStatusResult.getData())
                || !this.judgeClearPileSmartChargingProfileEnable(clearSmartChargingProfileDTO.getPileSn())) {
            throw new MessageCodeException(PileBaseEnum.ADD_MEMBER_CUSTOMER_FAIL);
        }

        Result<String> sendResult = smartChargeFeign.clearAllProfile(clearSmartChargingProfileDTO.getPileSn());

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.clearPileSmartChargingProfile sendResult : {}",
                JSON.toJSONString(sendResult));

        if (StringUtils.isNotBlank(sendResult.getData())) {
            clearSmartChargingProfileDTO.setUserId(LoginUserHolder.getLoginUser().getPayload().getUserId());
            clearSmartChargingProfileDTO.setPushMark(true);
            stringRedisTemplate.opsForValue().set(RedisKeyConstant.getClearPileSmartChargingProfileKey(sendResult.getData()), JSON.toJSONString(clearSmartChargingProfileDTO), 2L, TimeUnit.MINUTES);
        }
        return StringUtils.isNotBlank(sendResult.getData());
    }

    @Override
    public IPage<SearchPileByNameOrSnVO> searchPileByNameOrSn(SearchPileByNameOrSnDTO searchEvseByNameOrSnDTO) {
        if (ObjectUtils.isEmpty(searchEvseByNameOrSnDTO)) {
            return null;
        }
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId",LoginUserHolder.getLoginUser().getPayload().getSellerId()));
        if (!ObjectUtils.isEmpty(searchEvseByNameOrSnDTO.getLocationId())) {
            queryBuilder.must(QueryBuilders.termQuery("locationId",searchEvseByNameOrSnDTO.getLocationId()));
        }
        if (!StringUtils.isBlank(searchEvseByNameOrSnDTO.getKeyWord())) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("name",searchEvseByNameOrSnDTO.getKeyWord()));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn",searchEvseByNameOrSnDTO.getKeyWord()));
            queryBuilder.must(queryBuilder2);
        }
        Pageable pageable = PageRequest.of(searchEvseByNameOrSnDTO.getPage()-1,
                searchEvseByNameOrSnDTO.getPageSize());
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withPageable(pageable)
                .withTrackTotalHits(true)
                .withSorts(SortBuilders.fieldSort("pileSn").order(SortOrder.ASC))
                .build();
        SearchHits<OpLocationPileEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationPileEvseElasticDTO.class);
        SearchPage<OpLocationPileEvseElasticDTO> searchHits = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<SearchPileByNameOrSnVO> resultList = new ArrayList<>();
        List<OpLocationPileEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        for (OpLocationPileEvseElasticDTO elasticDTO : list) {
            SearchPileByNameOrSnVO vo = new SearchPileByNameOrSnVO();
            BeanUtils.copyProperties(elasticDTO,vo);
            vo.setPileName(elasticDTO.getName());
            resultList.add(vo);
        }
        IPage<SearchPileByNameOrSnVO> resultPage = new Page<>();
        resultPage.setCurrent(searchEvseByNameOrSnDTO.getPage());
        resultPage.setPages(searchHits.getTotalPages());
        resultPage.setSize(searchHits.getSize());
        resultPage.setRecords(resultList);
        resultPage.setTotal(searchHits.getTotalElements());
        return resultPage;
    }

    @Override
    public List<BetaTestPileInfoVO> queryBetaTestPileInfo(QueryBetaTestPileInfoDTO queryBetaTestPileInfoDTO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.queryBetaTestPileInfo queryBetaTestPileInfoDTO : {}",
                JSON.toJSONString(queryBetaTestPileInfoDTO));

        if (queryBetaTestPileInfoDTO == null ||
                ObjectUtils.isEmpty(queryBetaTestPileInfoDTO.getUserIdList())) {
            return null;
        }

        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseRepository.queryBetaTestPileInfo(queryBetaTestPileInfoDTO.getUserIdList());
        if (ObjectUtils.isEmpty(opLocationPileEvseElasticDTOList)) {
            return null;
        }
        List<BetaTestPileInfoVO> betaTestPileInfoVOList = new ArrayList<>();
        opLocationPileEvseElasticDTOList.forEach(var -> {
            BetaTestPileInfoVO betaTestPileInfoVO = new BetaTestPileInfoVO();
            betaTestPileInfoVO.setPileSn(var.getPileSn());
            betaTestPileInfoVO.setOperatorId(var.getOperatorId());
            betaTestPileInfoVO.setUserSource(UserSourceEnum.WEB.getValue());
            betaTestPileInfoVOList.add(betaTestPileInfoVO);
        });
        return betaTestPileInfoVOList;
    }

    @Override
    public String getZoneIdByPileSn(String pileSn) {
        if (StringUtils.isEmpty(pileSn)) {
            return null;
        }
        List<OpLocationPileEvseElasticDTO> pileDtoList = this.elasticsearchRestTemplate.search(new NativeSearchQueryBuilder()
                .withQuery(QueryBuilders.termQuery("pileSn", pileSn))
                .withPageable(PageRequest.of(0, 1))
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "locationId"}, null))
                .build(), OpLocationPileEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(pileDtoList)) {
            return null;
        }
        OpLocationPileEvseElasticDTO pileDto = pileDtoList.get(0);
        Long locationId = pileDto.getLocationId();
        if (locationId == null) {
            return null;
        }
        return this.opLocationRepository.getZoneId(locationId);
    }

    private List<UnionPileTreeVO> getPileTree(List<OpPileAssociatedRuleDTO> pileList) {
        ArrayListMultimap<Long, OpPileAssociatedRuleDTO> evseInfoMultiMap = ArrayListMultimap.create();
        for (OpPileAssociatedRuleDTO datum : pileList) {
            evseInfoMultiMap.put(datum.getLocationId(), datum);
        }
        List<UnionPileTreeVO> pileTree = Lists.newArrayList();
        evseInfoMultiMap.asMap().forEach((locationId, pileColl) -> {
            // 创建桩节点
            UnionPileTreeVO treeNode = new UnionPileTreeVO();
            treeNode.setLocationId(locationId);
            // todo 需求已经改版，该字段已经没有用了，为兼容前端，故将该字段全部置为false
            treeNode.setHubjectCheck(false);
            // 获取场站名字
            String LocationName = getLocationName(pileColl);
            treeNode.setLocationName(LocationName);
            List<OpPileAssociatedRuleDTO> evseList = new ArrayList<>(pileColl);
            log.info("pileColl size:{}", evseList.size());
            treeNode.setPiles(evseList);
            pileTree.add(treeNode);
        });
        return pileTree;
    }

    private void gunNumberSorting(List<UnionPileTreeVO> pileTree) {
        if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isEmpty(pileTree)) {
            return;
        }
        for (UnionPileTreeVO unionPileTreeVO : pileTree) {
            List<OpPileAssociatedRuleDTO> piles = unionPileTreeVO.getPiles();
            if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(piles)) {
                for (OpPileAssociatedRuleDTO pile : piles) {
                    List<OpEvseAssociatedRuleDTO> opEvseAssociatedRuleDTOList = pile.getOpEvseAssociatedRuleDTOList();
                    if (com.baomidou.mybatisplus.core.toolkit.ObjectUtils.isNotEmpty(opEvseAssociatedRuleDTOList) && opEvseAssociatedRuleDTOList.size() > 1) {
                        Collections.sort(opEvseAssociatedRuleDTOList);
                    }
                }
            }
        }
    }

    private String getLocationName(Collection<OpPileAssociatedRuleDTO> pileColl) {
        String LocationName = "";
        List<OpPileAssociatedRuleDTO> collect = pileColl.stream()
                .filter(e1 -> StringUtils.isNotBlank(e1.getLocationName()))
                .collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(collect)) {
            LocationName = collect.get(0).getLocationName();
        }
        return LocationName;
    }

    @Override
    public List<String> getPileSnList() {
        List<Long> locationIdList = this.getLocationIdList();
        if (ObjectUtils.isEmpty(locationIdList)) {
            return Collections.emptyList();
        }
        List<OpLocationPileEvseEntity> pileInfoListByLocationIdList = opLocationPileEvseRepository.getPileInfoListByLocationIdList(locationIdList);
        if (ObjectUtils.isEmpty(pileInfoListByLocationIdList)) {
            return Collections.emptyList();
        }
        return pileInfoListByLocationIdList
                .stream()
                .map(OpLocationPileEvseEntity::getPileSn)
                .collect(Collectors.toList());
    }

    private List<Long> getLocationIdList() {
        try {
            List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
            log.info("OpLocationPileEvseServiceServiceImpl getLocationIdList locationIdList = "
                    + JSON.toJSONString(locationIdList));
            if (CollectionUtils.isEmpty(locationIdList)) {
                return Collections.emptyList();
            }
            return locationIdList;
        } catch (Exception e) {
            log.info("OpLocationPileEvseServiceServiceImpl getLocationIdList Exception = ", e);
            return Collections.emptyList();
        }
    }


    /**
     * @return
     * @function 同步充电桩名称
     */
    @Override
    public Integer syncPileName() {
        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.selectPlieList();
        int count = 0;
        // 筛选出充电桩名称为空的集合
        if (ObjectUtils.isNotEmpty(opLocationPileEvseEntityList)) {
            List<OpLocationPileEvseEntity> pileNameIsBlankEntityList = opLocationPileEvseEntityList
                    .stream()
                    .filter(var -> StringUtils.isBlank(var.getName()))
                    .collect(Collectors.toList());
            if (ObjectUtils.isNotEmpty(pileNameIsBlankEntityList)) {
                for (OpLocationPileEvseEntity opLocationPileEvseEntity : pileNameIsBlankEntityList) {
                    String pileSn = opLocationPileEvseEntity.getPileSn();
                    String name = opLocationPileEvseEntity.getName();
                    if (StringUtils.isNotBlank(pileSn)
                            && StringUtils.isBlank(name)) {
                        opLocationPileEvseEntity.setName(pileSn);
                    }
                }
                opLocationPileEvseRepository.updateBatchById(pileNameIsBlankEntityList, 50);
            }
        }
        // 数据库同步后，重新查询数据库，继而同步ES
        List<OpLocationPileEvseEntity> newOpLocationPileEvseEntityList = opLocationPileEvseRepository.selectPlieList();
        // 同步ES
        if (ObjectUtils.isNotEmpty(newOpLocationPileEvseEntityList)) {
            for (OpLocationPileEvseEntity opLocationPileEvseEntity : newOpLocationPileEvseEntityList) {
                // 充电桩id
                Long pileId = opLocationPileEvseEntity.getId();
                // 充电桩名称
                String pileName = opLocationPileEvseEntity.getName();
                // 充电桩序列号
                String pileSn = opLocationPileEvseEntity.getPileSn();
                // 根据充电桩id查询充电桩ES
                OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.getPileInfoByPileId(pileId);
                if (opLocationPileEvseElasticDTO != null
                        && StringUtils.isNotBlank(pileName)
                        && !pileName.equals(opLocationPileEvseElasticDTO.getName())) {
                    opLocationPileEvseElasticDTO.setName(pileName);
                    opLocationPileEvseRepository.updatePileInfo(opLocationPileEvseElasticDTO);
                    count++;
                }
                // 根据充电桩序列号查询充电枪ES
                List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseService.findListByPileSn(pileSn);
                if (ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
                    for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
                        if (StringUtils.isNotBlank(pileName)
                                && !pileName.equals(opLocationEvseElasticDTO.getPileName())) {
                            opLocationEvseElasticDTO.setPileName(pileName);
                            opLocationEvseService.updateEvseInfo(opLocationEvseElasticDTO);
                        }
                    }
                }
            }
        }
        return count;
    }

    /**
     * @param pileSnList
     * @return
     * @function 根据充电桩序列号批量查询充电桩基础信息
     */
    @Override
    public List<com.autel.cloud.pile.base.vo.pile.PileInfoVO> getPileInfoListByPileSnList(List<String> pileSnList) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.getPileInfoListByPileSnList pileSnList : {}", JSON.toJSONString(pileSnList));

        if (ObjectUtils.isEmpty(pileSnList)) {
            return null;
        }

        // 查询充电桩es
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = opLocationPileEvseRepository.findList(pileSnList);
        // 构建充电桩序列号与充电桩信息之间的映射关系
        Map<String, OpLocationPileEvseElasticDTO> pileSnAndOpLocationPileEvseElasticDTOMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
            for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
                String pileSn = opLocationPileEvseElasticDTO.getPileSn();
                pileSnAndOpLocationPileEvseElasticDTOMap.put(pileSn, opLocationPileEvseElasticDTO);
            }
        }

        // 构建场站id与场站信息之间的映射关系
        Map<Long, OpLocationElasticDTO> locationIdAndOpLocationElasticDTOMap = new HashMap<>();
        // 过滤出有效的场站id集合
        Set<Long> locationIdSet = opLocationPileEvseElasticDTOList
                .stream()
                .filter(var -> (var != null && var.getLocationId() != null))
                .map(OpLocationPileEvseElasticDTO::getLocationId)
                .collect(Collectors.toSet());
        if (ObjectUtils.isNotEmpty(locationIdSet)) {
            // 查询场站信息
            List<OpLocationElasticDTO> opLocationElasticDTOList = opLocationElastic.findAllByIdIn(locationIdSet);
            if (ObjectUtils.isNotEmpty(opLocationElasticDTOList)) {
                for (OpLocationElasticDTO opLocationElasticDTO : opLocationElasticDTOList) {
                    Long id = opLocationElasticDTO.getId();
                    locationIdAndOpLocationElasticDTOMap.put(id, opLocationElasticDTO);
                }
            }
        }

        // 转换返回对象
        List<com.autel.cloud.pile.base.vo.pile.PileInfoVO> pileInfoVOList = new ArrayList<>();
        for (String pileSn : pileSnList) {
            com.autel.cloud.pile.base.vo.pile.PileInfoVO pileInfoVO = new PileInfoVO();
            pileInfoVO.setPileSn(pileSn);
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = pileSnAndOpLocationPileEvseElasticDTOMap.get(pileSn);
            if (opLocationPileEvseElasticDTO != null) {
                pileInfoVO.setId(opLocationPileEvseElasticDTO.getId());
                pileInfoVO.setPileName(opLocationPileEvseElasticDTO.getName());
                // todo 对充电桩名称做出特殊处理
                if (StringUtils.isBlank(pileInfoVO.getPileName())) {
                    pileInfoVO.setPileName(pileInfoVO.getPileSn());
                }
                pileInfoVO.setUpdatedAt(opLocationPileEvseElasticDTO.getUpdatedAt());
                Long locationId = opLocationPileEvseElasticDTO.getLocationId();
                OpLocationElasticDTO opLocationElasticDTO = locationIdAndOpLocationElasticDTOMap.get(locationId);
                if (opLocationElasticDTO != null) {
                    LocationInfoVO pileLocationInfoVO = new LocationInfoVO();
                    pileLocationInfoVO.setLocationId(opLocationElasticDTO.getId());
                    pileLocationInfoVO.setLocationName(opLocationElasticDTO.getName());
                    pileLocationInfoVO.setCountry(opLocationElasticDTO.getCountry());
                    if (StringUtils.isNotBlank(opLocationElasticDTO.getCountry())) {
                        pileLocationInfoVO.setCountryName(opLocationRepository.changeCountry(opLocationElasticDTO.getCountry()));
                    }
                    pileLocationInfoVO.setProvince(opLocationElasticDTO.getProvince());
                    pileLocationInfoVO.setCity(opLocationElasticDTO.getCity());
                    pileLocationInfoVO.setAddress(opLocationElasticDTO.getAddress());
                    pileLocationInfoVO.setPostalCode(opLocationElasticDTO.getPostalCode());
                    pileLocationInfoVO.setLatitude(opLocationElasticDTO.getLatitude());
                    pileLocationInfoVO.setLongitude(opLocationElasticDTO.getLongitude());
                    pileLocationInfoVO.setZoneId(opLocationElasticDTO.getZoneId());
                    pileLocationInfoVO.setUpdatedAt(opLocationElasticDTO.getUpdatedAt());
                    pileInfoVO.setPileLocationInfoVO(pileLocationInfoVO);
                }
            }
            pileInfoVOList.add(pileInfoVO);
        }
        return pileInfoVOList;
    }

    /**
     * @return
     * @function 查询出所有的开启了互联互通功能的充电桩信息
     */
    @Override
    public List<OpLocationPileEvseElasticDTO> findAllEroamingPile() {
        return opLocationPileEvseRepository.findAllEroamingPile();
    }

    private String buildConfigurationMsg(String action, String data, String seq) {
        return String.format("[2,\"%s\",\"%s\",%s]", seq, action,data);
    }

    @Override
    public Result<Boolean> getConfigurations(String sn) {
        SendMsgDto sendMsgDto = new SendMsgDto();
        sendMsgDto.setReceiver("sn-"+sn);
        String seq = IdWorker.getIdStr();
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getConfigurationCacheKey(sn + "_" +seq), LoginUserHolder.getLoginUser().getId().toString(), 2, TimeUnit.MINUTES);
        sendMsgDto.setMsg(buildConfigurationMsg(OcppAction.GET_CONFIGURATION.getValue(), "{}", seq));
        return wxProxyClient.sendMsg(sendMsgDto);
    }

    @Override
    public Result<Boolean> changeConfiguration(ChangeConfigurationDTO changeConfigurationDTO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.changeConfiguration changeConfigurationDTO : {}",
                JSON.toJSONString(changeConfigurationDTO));

        SendMsgDto sendMsgDto = new SendMsgDto();
        sendMsgDto.setReceiver("sn-"+changeConfigurationDTO.getSn());
        String seq = IdWorker.getIdStr();
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.changeConfigurationCacheKey(changeConfigurationDTO.getSn() + "_" +seq), LoginUserHolder.getLoginUser().getId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListKey(changeConfigurationDTO.getSn() + "_" +seq), LoginUserHolder.getLoginUser().getPayload().getSellerId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.getPileChangeConfigurationDtoKey(changeConfigurationDTO.getSn() + "_" + seq), JSON.toJSONString(changeConfigurationDTO), 2, TimeUnit.MINUTES);

        if (ConfigKeyEnum.FREE_VEND.getKey().equalsIgnoreCase(changeConfigurationDTO.getKey())) {
            SetPileFreeVendDTO setPileFreeVendDTO = new SetPileFreeVendDTO();
            setPileFreeVendDTO.setPileSn(changeConfigurationDTO.getSn());
            setPileFreeVendDTO.setMsgId(seq);
            setPileFreeVendDTO.setFreeVendEnable("true".equalsIgnoreCase(changeConfigurationDTO.getValue()) ? 1 : 0);
            setPileFreeVendDTO.setUserId(LoginUserHolder.getLoginUser().getId());
            setPileFreeVendDTO.setPushMark(true);
            stringRedisTemplate.opsForValue().set(ConfigRedisKeyConstant.getSetPileFreeVendDtoKey(changeConfigurationDTO.getSn() + "_" + seq), JSON.toJSONString(setPileFreeVendDTO), 2, TimeUnit.MINUTES);
        }

        if (ConfigKeyEnum.FREE_VEND_ID_TAG.getKey().equalsIgnoreCase(changeConfigurationDTO.getKey())) {
            UpdatePileFreeVendIdTagDTO updatePileFreeVendIdTagDTO = new UpdatePileFreeVendIdTagDTO();
            updatePileFreeVendIdTagDTO.setPileSn(changeConfigurationDTO.getSn());
            updatePileFreeVendIdTagDTO.setMsgId(seq);
            updatePileFreeVendIdTagDTO.setFreeVendIdTag(changeConfigurationDTO.getValue());
            updatePileFreeVendIdTagDTO.setUserId(LoginUserHolder.getLoginUser().getId());
            updatePileFreeVendIdTagDTO.setPushMark(true);
            stringRedisTemplate.opsForValue().set(ConfigRedisKeyConstant.getUpdatePileFreeVendIdTagDtoKey(changeConfigurationDTO.getSn() + "_" + seq), JSON.toJSONString(updatePileFreeVendIdTagDTO), 2, TimeUnit.MINUTES);
        }

        ChangeConfigurationMsgDTO changeConfigurationMsgDTO = ChangeConfigurationMsgDTO.builder()
                .key(changeConfigurationDTO.getKey())
                .value(changeConfigurationDTO.getValue())
                .build();

        List<SendMsgDto> sendMsgDtoList = new ArrayList<>();
        if (!ObjectUtils.isEmpty(changeConfigurationDTO) && !ObjectUtils.isEmpty(changeConfigurationDTO.getKey()) && !ObjectUtils.isEmpty(changeConfigurationDTO.getValue())
           && changeConfigurationDTO.getKey().equals("LocalAuthListEnabled") && changeConfigurationDTO.getValue().equals("true")) {
            sendMsgDtoList = setMessage(changeConfigurationDTO);
        }
        log.info("changeConfiguration,sendMsgDtoList={}",JSON.toJSONString(sendMsgDtoList));
        sendMsgDto.setMsg(buildConfigurationMsg(OcppAction.CHANGE_CONFIGURATION.getValue(), changeConfigurationMsgDTO.toString(), seq));
        sendMsgDtoList.add(sendMsgDto);
        BatchSendMsgV2DTO batchSendMsgV2DTO = new BatchSendMsgV2DTO();
        batchSendMsgV2DTO.setSendMsgDtoList(sendMsgDtoList);
        return wxProxyClient.batchSendMsgV2(batchSendMsgV2DTO);
    }

    private List<SendMsgDto> setMessage(ChangeConfigurationDTO changeConfigurationDTO) {
        List<SendMsgDto> sendMsgDtoList = new ArrayList<>();
        ChangeConfigurationMsgDTO authorizationCacheEnabled = ChangeConfigurationMsgDTO.builder()
                .key("AuthorizationCacheEnabled")
                .value("true")
                .build();
        SendMsgDto sendAuthorizationCacheEnabledMsgDto = new SendMsgDto();
        sendAuthorizationCacheEnabledMsgDto.setReceiver("sn-"+changeConfigurationDTO.getSn());
        String seq = IdWorker.getIdStr();
        sendAuthorizationCacheEnabledMsgDto.setMsg(buildConfigurationMsg(OcppAction.CHANGE_CONFIGURATION.getValue(), authorizationCacheEnabled.toString(), seq));
        sendMsgDtoList.add(sendAuthorizationCacheEnabledMsgDto);

        ChangeConfigurationMsgDTO localAuthorizeOffline = ChangeConfigurationMsgDTO.builder()
                .key("LocalAuthorizeOffline")
                .value("true")
                .build();
        SendMsgDto sendLocalAuthorizeOfflineMsgDto = new SendMsgDto();
        sendLocalAuthorizeOfflineMsgDto.setReceiver("sn-"+changeConfigurationDTO.getSn());
        String seq1 = IdWorker.getIdStr();
        sendLocalAuthorizeOfflineMsgDto.setMsg(buildConfigurationMsg(OcppAction.CHANGE_CONFIGURATION.getValue(), localAuthorizeOffline.toString(), seq1));
        sendMsgDtoList.add(sendLocalAuthorizeOfflineMsgDto);

        ChangeConfigurationMsgDTO localPreAuthorize = ChangeConfigurationMsgDTO.builder()
                .key("LocalPreAuthorize")
                .value("true")
                .build();
        SendMsgDto sendLocalPreAuthorizeMsgDto = new SendMsgDto();
        sendLocalPreAuthorizeMsgDto.setReceiver("sn-"+changeConfigurationDTO.getSn());
        String seq2 = IdWorker.getIdStr();
        sendLocalPreAuthorizeMsgDto.setMsg(buildConfigurationMsg(OcppAction.CHANGE_CONFIGURATION.getValue(), localPreAuthorize.toString(), seq2));
        sendMsgDtoList.add(sendLocalPreAuthorizeMsgDto);
        return sendMsgDtoList;
    }

    @Override
    public Result<Boolean> sendLocalList(LocalListInformationDTO localListInformationDTO) {
        if (ObjectUtils.isEmpty(localListInformationDTO) || StringUtils.isBlank(localListInformationDTO.getSn())) {
            return Result.ofSucceed(false);
        }
        if (StringUtils.isBlank(localListInformationDTO.getCard())) {
            return Result.ofSucceed(true);
        }
        if (!CommonUtil.checkChargeCard(localListInformationDTO.getCard())) {
            throw new MessageCodeException(PileBaseEnum.CARD_NUMBER_ERROR);
        }
        //判断数据库中是否存在
        List<LocalAuthListEntity> localAuthListEntities = localAuthListMapper.selectList(new LambdaQueryWrapper<LocalAuthListEntity>()
                .eq(LocalAuthListEntity::getCard, localListInformationDTO.getCard())
                .eq(LocalAuthListEntity::getSn,localListInformationDTO.getSn()));
        if (!CollectionUtils.isEmpty(localAuthListEntities)) {
            throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_EXIST_AUTH_LIST);
        }

        //处理前端传入的新数据
        List<AuthorizationData> localAuthorizationList = new ArrayList<>();
        AuthorizationData authorizationData = AuthorizationData.builder()
                .idTag(localListInformationDTO.getCard())
                .idTagInfo(IdTagInfoVO.builder()
                        .status(LocalAuthListEnum.findValue(localListInformationDTO.getStatus())).build()).build();
        if (ObjectUtils.isNotEmpty(localListInformationDTO.getValidityPeriod()) && !localListInformationDTO.getValidityPeriod().equals(-1L)) {
            authorizationData.getIdTagInfo().setExpiryDate(DateTime.of(localListInformationDTO.getValidityPeriod()));
        }
        localAuthorizationList.add(authorizationData);
        //查询商家底下所有的白名单列表
        List<LocalAuthListEntity> allAuthListBySellerId = localAuthListRepositoryImpl.getAllAuthListBySellerId(localListInformationDTO.getSn(),LoginUserHolder.getLoginUser().getPayload().getSellerId());
        if (!CollectionUtils.isEmpty(allAuthListBySellerId) && allAuthListBySellerId.size() >= 10) {
            throw new MessageCodeException(PileBaseEnum.CARD_NUMBER_REACHED_UPPER_LIMIT);
        }
        if (!CollectionUtils.isEmpty(allAuthListBySellerId)) {
            for (LocalAuthListEntity localAuthListEntity : allAuthListBySellerId) {
                AuthorizationData data = AuthorizationData.builder()
                        .idTag(localAuthListEntity.getCard())
                        .idTagInfo(IdTagInfoVO.builder()
                                .status(LocalAuthListEnum.findValue(localAuthListEntity.getStatus())).build()).build();
                if (ObjectUtils.isNotEmpty(localAuthListEntity.getExpiredTime()) && !localAuthListEntity.getExpiredTime().equals(-1L)) {
                    data.getIdTagInfo().setExpiryDate(DateTime.of(localAuthListEntity.getExpiredTime()));
                }
                localAuthorizationList.add(data);
            }
        }
        SendLocalListDataDTO sendLocalListDataDTO = SendLocalListDataDTO.builder()
                .listVersion(4)
                .updateType("Full")
                .localAuthorizationList(localAuthorizationList).build();
        SendMsgDto sendMsgDto = new SendMsgDto();
        sendMsgDto.setReceiver("sn-"+localListInformationDTO.getSn());
        String seq = IdWorker.getIdStr();
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListKey(localListInformationDTO.getSn() + "_" +seq), LoginUserHolder.getLoginUser().getPayload().getSellerId().toString(), 2, TimeUnit.MINUTES);
        //保存全部白名单列表
        LocalAuthListEntity entity = new LocalAuthListEntity();
        entity.setCard(localListInformationDTO.getCard());
        entity.setExpiredTime(localListInformationDTO.getValidityPeriod());
        entity.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        entity.setStatus(localListInformationDTO.getStatus());
        entity.setSn(localListInformationDTO.getSn());
        allAuthListBySellerId.add(entity);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListUserIdKey(localListInformationDTO.getSn() + "_" +seq), LoginUserHolder.getLoginUser().getId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListDataKey(localListInformationDTO.getSn() + "_" +seq), JSON.toJSONString(allAuthListBySellerId), 2, TimeUnit.MINUTES);
        sendMsgDto.setMsg(buildConfigurationMsg(OcppAction.SEND_LOCAL_LIST.getValue(), JSON.toJSONString(sendLocalListDataDTO), seq));
        log.info("sendLocalList,sendMsgDto={}",JSON.toJSONString(sendMsgDto));
        return wxProxyClient.sendMsg(sendMsgDto);
    }

    /**
     * @param locationId
     * @return
     * @function 批量开启某个场站下所有的属性为公开的充电桩的互联互通属性
     */
    @Override
    public boolean batchSetPileEroaming(Long locationId) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.setPileEroaming locationId : {}",
                JSON.toJSONString(locationId));

        if (locationId == null) {
            return false;
        }

        List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseRepository.findAllNeedToOpenEroamingEnablePileList(locationId);
        if (ObjectUtils.isNotEmpty(opLocationPileEvseEntityList)) {
            for (OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntityList) {
                ThreadPoolUtil.getExecutor().execute(() -> {
                    String pileSn = opLocationPileEvseEntity.getPileSn();
                    if (StringUtils.isNotBlank(pileSn)) {
                        SetPileEroamingDTO setPileEroamingDTO = new SetPileEroamingDTO();
                        setPileEroamingDTO.setPileSn(pileSn);
                        setPileEroamingDTO.setPileEroamingOperateType(Integer.valueOf(1));
                        this.setPileEroaming(setPileEroamingDTO);
                    }
                });
            }
        }
        return true;
    }

    /**
     * @param pileSimpleInfoQueryDTO 充电桩信息的简单查询实体 入参模型
     * @return 充电桩信息
     * @function 充电桩信息的简单查询
     */
    @Override
    public List<PileSimpleInfoQueryVO> pileSimpleInfoQuery(PileSimpleInfoQueryDTO pileSimpleInfoQueryDTO) {

        log.info("===>>>OpLocationPileEvseServiceServiceImpl.pileSimpleInfoQuery pileSimpleInfoQueryDTO : {}", JSON.toJSONString(pileSimpleInfoQueryDTO));

        List<PileSimpleInfoQueryVO> pileSimpleInfoQueryVOList = new ArrayList<>();
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseElasticDTOList = null;
        if (pileSimpleInfoQueryDTO.getLocationId() != null) {
            // 优先按照场站id查询
            opLocationPileEvseElasticDTOList = opLocationPileEvseRepository.findByLocationId(pileSimpleInfoQueryDTO.getLocationId());
        } else {
            // 按照商家id和关键字查询充电桩列表
            opLocationPileEvseElasticDTOList = opLocationPileEvseRepository.findByCondition(pileSimpleInfoQueryDTO.getSellerId(), pileSimpleInfoQueryDTO.getPileSnSearch());
        }
        // 转化
        if (ObjectUtils.isNotEmpty(opLocationPileEvseElasticDTOList)) {
            for (OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO : opLocationPileEvseElasticDTOList) {
                if (opLocationPileEvseElasticDTO != null) {
                    PileSimpleInfoQueryVO pileSimpleInfoQueryVO = new PileSimpleInfoQueryVO();
                    pileSimpleInfoQueryVO.setId(opLocationPileEvseElasticDTO.getId());
                    pileSimpleInfoQueryVO.setPileSn(opLocationPileEvseElasticDTO.getPileSn());
                    pileSimpleInfoQueryVO.setLocationId(opLocationPileEvseElasticDTO.getLocationId());
                    pileSimpleInfoQueryVO.setLocationName(opLocationPileEvseElasticDTO.getLocationName());
                    pileSimpleInfoQueryVO.setPileName(opLocationPileEvseElasticDTO.getName());
                    String powerType = opLocationPileEvseElasticDTO.getPowerType();
                    if (StringUtils.isNotBlank(powerType) && powerType.contains("_")) {
                        pileSimpleInfoQueryVO.setPileType(powerType.split("_")[0]);
                    }
                    pileSimpleInfoQueryVO.setUpdatedAt(opLocationPileEvseElasticDTO.getUpdatedAt());
                    pileSimpleInfoQueryVOList.add(pileSimpleInfoQueryVO);
                }
            }
        }
        // 返回
        return pileSimpleInfoQueryVOList;
    }

    /**
     * 枪的计费规则是否免费
     * @param sn
     * @param gunNo
     * @return
     */
    private Boolean isFreePrice(String sn, Integer gunNo){
        PileEvseDTO pileEvseDTO = new PileEvseDTO();
        pileEvseDTO.setGunNo(gunNo);
        pileEvseDTO.setPileSn(sn);
        CostModelRuleDTO costModelRuleDTO = opLocationEvseService.getTatiffRuleByEvseSn(pileEvseDTO).getData();
        if(costModelRuleDTO == null){
            return false;
        }
        if(costModelRuleDTO.getAutoCharging() != null &&
                costModelRuleDTO.getRuleModelType() != null &&
                RuleModelTypeEnum.FREE.getCode().equals(costModelRuleDTO.getRuleModelType()) &&
                1 == costModelRuleDTO.getAutoCharging()){
            log.info("getRemoteStartPileData sn :{}, gunNo:{} 免费自动启动", sn, gunNo);
            return false;
        }
        return judgeFree(costModelRuleDTO);
    }


    /**
     * 是否可以启动充电
     * @param evseStatus
     * @return
     */
    private Boolean getRemoteStartEnabled(String evseStatus) {
        return EvseDeviceStatusEnum.AVAILABLE.getName().equals(evseStatus)
                || EvseDeviceStatusEnum.PREPARING.getName().equals(evseStatus)
                || EvseDeviceStatusEnum.FINISHING.getName().equals(evseStatus);
    }

    private Boolean judgeFree(CostModelRuleDTO costModelRuleDTO) {
        log.info("costModelRuleDTO : {}", JSON.toJSONString(costModelRuleDTO));
        List<CostRuleWeeksDTO> costRuleWeeksDTOList = costModelRuleDTO.getRules();
        if (!CollectionUtils.isEmpty(costRuleWeeksDTOList)) {
            for (CostRuleWeeksDTO costRuleWeeksDTO : costRuleWeeksDTOList) {
                List<CostRulesDTO> weeksRules = costRuleWeeksDTO.getWeeksRules();
                for (CostRulesDTO costRulesDTO : weeksRules) {
                    if (costRulesDTO.getUnitPrice() != null && costRulesDTO.getUnitPrice().doubleValue() > 0) {
                        return false;
                    }
                    if (costRulesDTO.getTimePrice() != null && costRulesDTO.getTimePrice().doubleValue() > 0) {
                        return false;
                    }
                    if (costRulesDTO.getParkingPrice() != null && costRulesDTO.getParkingPrice().doubleValue() > 0) {
                        return false;
                    }
                }
            }
        }
        List<PhaseIdlePriceRuleDTO> phaseIdlePriceRuleDTOList = costModelRuleDTO.getPhaseIdlePriceRuleDTOList();
        if (!CollectionUtils.isEmpty(phaseIdlePriceRuleDTOList)) {
            for (PhaseIdlePriceRuleDTO phaseIdlePriceRuleDTO : phaseIdlePriceRuleDTOList) {
                if (phaseIdlePriceRuleDTO.getIdlePrice() != null && phaseIdlePriceRuleDTO.getIdlePrice().doubleValue() > 0) {
                    return false;
                }
            }
        }

        ParkingFeeRuleDTO parkingFeeRuleDTO = costModelRuleDTO.getParkingFeeRuleDTO();
        if (parkingFeeRuleDTO != null) {
            List<PhaseParkingPriceRuleDTO> phaseParkingPriceRuleDTOList = parkingFeeRuleDTO.getPhaseParkingPriceRuleDTOList();
            if (!CollectionUtils.isEmpty(phaseParkingPriceRuleDTOList)) {
                for (PhaseParkingPriceRuleDTO phaseParkingPriceRuleDTO : phaseParkingPriceRuleDTOList) {
                    if (phaseParkingPriceRuleDTO.getParkingPrice() != null
                            && phaseParkingPriceRuleDTO.getParkingPrice().doubleValue() > 0) {
                        return false;
                    }
                }
            }
        }

        // 基础模式
        BasicCostModelRuleVO basicCostModelRuleVO = costModelRuleDTO.getBasicCostModelRuleVO();
        if (basicCostModelRuleVO != null) {
            if (basicCostModelRuleVO.getDurationPrice() != null && basicCostModelRuleVO.getDurationPrice().getTimePrice() != null &&
                    basicCostModelRuleVO.getDurationPrice().getTimePrice().doubleValue() > 0) {
                return false;
            }

            if (basicCostModelRuleVO.getEnergyPrice() != null && basicCostModelRuleVO.getEnergyPrice().doubleValue() > 0) {
                return false;
            }

            if (basicCostModelRuleVO.getStartPrice() != null && basicCostModelRuleVO.getStartPrice().doubleValue() > 0) {
                return false;
            }

            if (basicCostModelRuleVO.getIdlePrice() != null && basicCostModelRuleVO.getIdlePrice().getIdlePrice() != null
                    && basicCostModelRuleVO.getIdlePrice().getIdlePrice().doubleValue() > 0) {
                return false;
            }
        }

        // 进阶模式
        AdvancedCostModelRuleVO advancedCostModelRuleVO = costModelRuleDTO.getAdvancedCostModelRuleVO();
        if (advancedCostModelRuleVO != null) {
            if (advancedCostModelRuleVO.getDurationDTO() != null) {
                List<DurationRuleDTO> durationRuleList = advancedCostModelRuleVO.getDurationDTO().getDurationRuleList();
                List<DurationRuleByDurDTO> durationRuleByDurList = advancedCostModelRuleVO.getDurationDTO().getDurationRuleByDurList();
                if (!CollectionUtils.isEmpty(durationRuleList)) {
                    for (DurationRuleDTO durationRuleDTO : durationRuleList) {
                        List<DurationPriceDTO> list = durationRuleDTO.getDurationPriceList();
                        if (!CollectionUtils.isEmpty(list)) {
                            for (DurationPriceDTO durationPriceDTO : list) {
                                if (durationPriceDTO.getTimePrice() != null && durationPriceDTO.getTimePrice().doubleValue() > 0) {
                                    return false;
                                }
                            }
                        }
                    }
                } else if (!CollectionUtils.isEmpty(durationRuleByDurList)) {
                    for (DurationRuleByDurDTO durationRuleByDurDTO : durationRuleByDurList) {
                        List<BasicPriceByDurationDTO> list = durationRuleByDurDTO.getPriceList();
                        if (!CollectionUtils.isEmpty(list)) {
                            for (BasicPriceByDurationDTO basicPriceByDurationDTO : list) {
                                if (basicPriceByDurationDTO.getPrice() != null && basicPriceByDurationDTO.getPrice().doubleValue() > 0) {
                                    return false;
                                }
                            }
                        }
                    }
                }

            }

            IdleDTO idleDTO = advancedCostModelRuleVO.getIdleDTO();
            if (idleDTO != null) {
                List<IdleRuleByDurDTO> idleRuleByDurDTOList = idleDTO.getIdleRuleByDurList();
                if (!CollectionUtils.isEmpty(idleRuleByDurDTOList)) {
                    for (IdleRuleByDurDTO idleRuleByDurDTO : idleRuleByDurDTOList) {
                        List<PhaseIdlePriceRuleDTO> phaseIdlePriceRuleList = idleRuleByDurDTO.getDurationPriceList();
                        if (!CollectionUtils.isEmpty(phaseIdlePriceRuleList)) {
                            for (PhaseIdlePriceRuleDTO phaseIdlePriceRuleDTO : phaseIdlePriceRuleList) {
                                if (phaseIdlePriceRuleDTO.getIdlePrice() != null && phaseIdlePriceRuleDTO.getIdlePrice().doubleValue() > 0) {
                                    return false;
                                }
                            }
                        }
                    }
                }

                List<IdleRuleDTO> idleRuleList = idleDTO.getIdleRuleList();
                if (!CollectionUtils.isEmpty(idleRuleList)) {
                    for (IdleRuleDTO idleRuleDTO : idleRuleList) {
                        List<IdlePriceDTO> idleRuleDTOList = idleRuleDTO.getIdlePriceList();
                        if (!CollectionUtils.isEmpty(idleRuleDTOList)) {
                            for (IdlePriceDTO idlePriceDTO : idleRuleDTOList) {
                                if (idlePriceDTO.getPrice() != null && idlePriceDTO.getPrice().doubleValue() > 0) {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }

            List<CostRuleWeeksDTO> energyRuleList = advancedCostModelRuleVO.getRules();
            if (!CollectionUtils.isEmpty(energyRuleList)) {
                for (CostRuleWeeksDTO energyRule : energyRuleList) {
                    List<CostRulesDTO> weeksRules = energyRule.getWeeksRules();
                    for (CostRulesDTO costRulesDTO : weeksRules) {
                        if (costRulesDTO.getUnitPrice() != null && costRulesDTO.getUnitPrice().doubleValue() > 0) {
                            return false;
                        }
                        if (costRulesDTO.getTimePrice() != null && costRulesDTO.getTimePrice().doubleValue() > 0) {
                            return false;
                        }
                        if (costRulesDTO.getParkingPrice() != null && costRulesDTO.getParkingPrice().doubleValue() > 0) {
                            return false;
                        }
                    }
                }
            }

            List<StartDTO> startDTOList = advancedCostModelRuleVO.getStartDTOList();
            if (!CollectionUtils.isEmpty(startDTOList)) {
                for (StartDTO startDTO : startDTOList) {
                    List<StartRuleDTO> weeksRules = startDTO.getStartPriceRuleList();
                    for (StartRuleDTO startRuleDTO : weeksRules) {
                        if (startRuleDTO.getStartPrice() != null && startRuleDTO.getStartPrice().doubleValue() > 0) {
                            return false;
                        }
                    }
                }
            }
        }

        if (costModelRuleDTO.getStartPrice() != null && costModelRuleDTO.getStartPrice().doubleValue() > 0) {
            return false;
        }
        return true;
    }

    /**
     * @paraString pileSnm pileSn 商桩序列号
     * @return 商家id
     * @function 根据商家id获取商桩序列号
     */
    @Override
    public Long getSellerIdByPileSn(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.getSellerIdByPileSn pileSn : {}", JSON.toJSONString(pileSn));

        if (StringUtils.isBlank(pileSn)) {
            return null;
        }
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileSn);
        if (opLocationPileEvseElasticDTO == null) {
            return null;
        }
        return opLocationPileEvseElasticDTO.getOperatorId();
    }

    @Override
    public Result<AdvertiseBaseInfoVO> queryAdvertiseBaseInfo(String pileSn) {
        Result<AdvertiseDeviceInfoVO> deviceInfo = creativePlatformFeignClient.getDeviceInfoByPileSn(pileSn);
        log.info("{} queryAdvertiseBaseInfo--> {}", pileSn, JSON.toJSONString(deviceInfo));
        if (Objects.nonNull(deviceInfo) && deviceInfo.getCode().equals(HttpStatus.HTTP_OK) && Objects.nonNull(deviceInfo.getData())) {
            Result<AdvertiseBaseInfoVO> advertiseDeviceInfo = creativePlatformFeignClient.getAdvertiseDeviceInfo(deviceInfo.getData().getId());
            if (Objects.nonNull(advertiseDeviceInfo) && advertiseDeviceInfo.getCode().equals(HttpStatus.HTTP_OK) && Objects.nonNull(advertiseDeviceInfo.getData())) {
                AdvertiseBaseInfoVO data = advertiseDeviceInfo.getData();
                log.info("AdvertiseBaseInfoVO--->{}", JSON.toJSONString(data));
                return Result.ofSucceed(data);
            }
        }

        return Result.ofSucceed();
    }

    @Override
    public Result<Page<AdvertiseListVO>> queryAdvertiseList(OpPileAdvertiseReq advertiseReq) {
        Result<AdvertiseDeviceInfoVO> deviceInfo = creativePlatformFeignClient.getDeviceInfoByPileSn(advertiseReq.getPileSn());
        log.info("{} queryAdvertiseList--> {}", JSON.toJSONString(advertiseReq), JSON.toJSONString(deviceInfo));
        if (Objects.nonNull(deviceInfo) && deviceInfo.getCode().equals(HttpStatus.HTTP_OK) && Objects.nonNull(deviceInfo.getData())) {
            AdvertiseListReq req = new AdvertiseListReq();
            req.setDeviceId(deviceInfo.getData().getId());
            req.setDeviceSn(deviceInfo.getData().getPileSn());
            req.setPage(advertiseReq.getPage());
            req.setPageSize(advertiseReq.getPageSize());
            return creativePlanFeignClient.getAdvertiseList(req);
        }

        return Result.ofSucceed();
    }

    private boolean judgePileFreeVendFirmwareSupportMark(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendFirmwareSupportMark pileSn : {}",
                JSON.toJSONString(pileSn));

        boolean flag = false;
        List<String> pileSnList = new ArrayList<>();
        pileSnList.add(pileSn);
        try {
            Result<Map<String, Boolean>> result = deviceServiceFeign.batchJudgePileFreeVendEnable(pileSnList);

            log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendFirmwareSupportMark result : {}",
                    JSON.toJSONString(result));

            if (result != null
                    && Integer.valueOf(HttpStatus.HTTP_OK).equals(result.getCode())
                    && ObjectUtils.isNotEmpty(result.getData())
                    && result.getData().get(pileSn) != null
                    && result.getData().get(pileSn)) {
                flag = true;
            }
        } catch (Exception e) {

            log.error("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendFirmwareSupportMark pileSn : {}, 调用device服务出现了异常！", pileSn, e);

            flag = false;
        }
        return flag;
    }

    private boolean judgePileFreeVendCostModelRuleSupportMark(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendCostModelRuleSupportMark pileSn : {}",
                JSON.toJSONString(pileSn));

        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseRepository.findListByPileSn(pileSn);
        if (ObjectUtils.isEmpty(opLocationEvseElasticDTOList)) {
            return false;
        }

        List<Long> tariffGroupIdList = new ArrayList<>();
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : opLocationEvseElasticDTOList) {
            if (opLocationEvseElasticDTO.getTariffId() == null) {
                return false;
            }
            tariffGroupIdList.add(opLocationEvseElasticDTO.getTariffId());
        }

        try {
            Result<List<SimpleInformationAboutCostModelRuleVO>> result = tariffFeignClient.querySimpleInformationAboutCostModelRuleVOByIds(tariffGroupIdList);

            log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendCostModelRuleSupportMark result : {}",
                    JSON.toJSONString(result));

            if (result != null
                    && Integer.valueOf(HttpStatus.HTTP_OK).equals(result.getCode())
                    && ObjectUtils.isNotEmpty(result.getData())) {
                List<SimpleInformationAboutCostModelRuleVO> simpleInformationAboutCostModelRuleVOList = result.getData();
                boolean flag = true;
                for (SimpleInformationAboutCostModelRuleVO simpleInformationAboutCostModelRuleVO : simpleInformationAboutCostModelRuleVOList) {
                    if (!RuleModelTypeEnum.FREE.getCode().equals(simpleInformationAboutCostModelRuleVO.getRuleModelType())) {
                        flag = false;
                        break;
                    }
                }
                if (flag) {
                    return true;
                }
            }
        } catch (Exception e) {

            log.error("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendCostModelRuleSupportMark pileSn : {}, 调用tariff服务出现了异常！", pileSn, e);

        }
        return false;
    }

    public boolean judgePileFreeVendViewEnable(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.judgePileFreeVendViewEnable pileSn : {}",
                JSON.toJSONString(pileSn));

        if (StringUtils.isBlank(pileSn)) {
            return false;
        }
        return this.judgePileFreeVendCostModelRuleSupportMark(pileSn);
    }

    @Override
    public Boolean updatePileFreeVendInfo(PileFreeVendInfoDTO pileFreeVendInfoDTO) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.updatePileFreeVendInfo pileFreeVendInfoDTO : {}",
                JSON.toJSONString(pileFreeVendInfoDTO));

        return opLocationPileEvseRepository.updatePileFreeVendInfo(pileFreeVendInfoDTO);
    }

    @Override
    public PileInfoVO getPileBriefInformation(String pileSn) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.getPileBriefInformation pileSn : {}",
                JSON.toJSONString(pileSn));

        if (StringUtils.isBlank(pileSn)) {
            return null;
        }

        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileSn);
        if (opLocationPileEvseElasticDTO == null) {
            return null;
        }

        PileInfoVO pileInfoVO = new PileInfoVO();
        pileInfoVO.setId(opLocationPileEvseElasticDTO.getId());
        pileInfoVO.setPileSn(pileSn);
        pileInfoVO.setPileName(opLocationPileEvseElasticDTO.getName());
        pileInfoVO.setFreeVendEnable(opLocationPileEvseElasticDTO.getFreeVendEnable());
        pileInfoVO.setFreeVendIdTag(opLocationPileEvseElasticDTO.getFreeVendIdTag());
        pileInfoVO.setUpdatedAt(opLocationPileEvseElasticDTO.getUpdatedAt());
        pileInfoVO.setSellerId(opLocationPileEvseElasticDTO.getOperatorId());

        if (opLocationPileEvseElasticDTO.getLocationId() != null) {
            Long locationId = opLocationPileEvseElasticDTO.getLocationId();
            Optional<OpLocationElasticDTO> optional = opLocationElastic.findById(locationId);
            if (optional.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optional.get();
                pileInfoVO.setZoneId(opLocationElasticDTO.getZoneId());
                pileInfoVO.setTimeZone(opLocationElasticDTO.getTimeZone());
            }
        }

        return pileInfoVO;
    }

    @Override
    public OpLocationEvseElasticDTO getEvseSnByIdAndGunCode(String id, String gunCode) {
        log.info("getEvseSnByIdAndGunCode,id:{},gunCode:{}", id, gunCode);
        final OpLocationPileEvseEntity pile = opLocationPileEvseRepository.getById(id);
        if (pile == null || StringUtils.isBlank(pile.getPileSn())) {
            log.info("未查询到桩信息");
            return null;
        }
        final String pileSn = pile.getPileSn();
        String evseSn = pileSn + "_" + gunCode;
        log.info("getEvseSnByIdAndGunCode,evseSn={}", evseSn);
        return opLocationEvseRepository.findByEvseSn(evseSn);
    }

    @Override
    public List<OpLocationPileEvseDTO> searchPileListBySellerId(QueryPileDTO queryPileDTO) {
        log.info("======== searchPileListBySellerId invoked, the queryPileDTO: {}", JSON.toJSONString(queryPileDTO));
       return opLocationEvseService.searchPileListBySellerId(queryPileDTO);
    }

    @Override
    public List<SelectDeviceInfoForPosVO> selectDeviceInfoForPos(SelectDeviceInfoForPosDTO selectDeviceInfoForPosDTO) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.selectDeviceInfoForPos selectDeviceInfoForPosDTO : {}",
                JSON.toJSONString(selectDeviceInfoForPosDTO));

        if (selectDeviceInfoForPosDTO.getSellerId() == null) {
            selectDeviceInfoForPosDTO.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
        }

        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", selectDeviceInfoForPosDTO.getSellerId()));
        if (ObjectUtils.isNotEmpty(selectDeviceInfoForPosDTO.getLocationIdList())) {
            queryBuilder.must(QueryBuilders.termsQuery("locationId", selectDeviceInfoForPosDTO.getLocationIdList()));
        }

        if (StringUtils.isNotBlank(selectDeviceInfoForPosDTO.getSearchValue())) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            queryBuilder2.should(QueryBuilders.wildcardQuery("name", String.format("*%s*", QueryParserBase.escape(selectDeviceInfoForPosDTO.getSearchValue()))));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", QueryParserBase.escape(selectDeviceInfoForPosDTO.getSearchValue()))));
            queryBuilder.must(queryBuilder2);
        }

        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("id").order(SortOrder.ASC))
                .withTrackTotalHits(true)
                .build();

        SearchHits<OpLocationPileEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationPileEvseElasticDTO.class);
        List<OpLocationPileEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());

        if (ObjectUtils.isEmpty(list)) {
            return null;
        }

        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOList = opLocationEvseService.getSellerEvseInfo(
                list.stream()
                        .filter(val -> val != null && StringUtils.isNotBlank(val.getPileSn()))
                        .map(OpLocationPileEvseElasticDTO::getPileSn)
                        .distinct()
                        .collect(Collectors.toList()),
                selectDeviceInfoForPosDTO.getSellerId());

        Map<String, List<Integer>> pileSnAndConnectorListMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(opLocationEvseElasticDTOList)) {
            Map<String, List<OpLocationEvseElasticDTO>> pileSnAndOpLocationEvseElasticDTOListMap = opLocationEvseElasticDTOList
                    .stream()
                    .collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));

            pileSnAndOpLocationEvseElasticDTOListMap.forEach((key, value) -> {
                List<Integer> connectorList = new ArrayList<>();
                value.forEach(val -> connectorList.add(CommonUtil.getGunNo(val.getEvseSn())));
                pileSnAndConnectorListMap.put(key, connectorList
                        .stream()
                        .sorted(Integer::compareTo)
                        .distinct()
                        .collect(Collectors.toList()));
            });
        }

        Result<List<Long>> getLocationIdsResult = pileUserFeign.getLocationIds();

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.selectDeviceInfoForPos getLocationIdsResult : {}",
                JSON.toJSONString(getLocationIdsResult));

        Set<Long> havePermissionLocationIdSet = new HashSet<>();
        if (getLocationIdsResult != null
                && ObjectUtils.isNotEmpty(getLocationIdsResult.getData())) {
            havePermissionLocationIdSet.addAll(getLocationIdsResult.getData());
        }

        List<SelectDeviceInfoForPosVO> selectDeviceInfoForPosVOList = new ArrayList<>();
        list.forEach(val -> {
            SelectDeviceInfoForPosVO selectDeviceInfoForPosVO = new SelectDeviceInfoForPosVO();
            selectDeviceInfoForPosVO.setPileId(val.getId());
            selectDeviceInfoForPosVO.setPileSn(val.getPileSn());
            selectDeviceInfoForPosVO.setPileName(val.getName());
            selectDeviceInfoForPosVO.setConnectorList(pileSnAndConnectorListMap.get(val.getPileSn()));
            selectDeviceInfoForPosVO.setLocationId(val.getLocationId());
            selectDeviceInfoForPosVO.setLocationName(val.getLocationName());
            selectDeviceInfoForPosVO.setHavePermission(havePermissionLocationIdSet.contains(val.getLocationId()));
            selectDeviceInfoForPosVOList.add(selectDeviceInfoForPosVO);
        });
        return selectDeviceInfoForPosVOList;
    }

    @Override
    public Boolean setPileEroamingForPile(SetPileEroamingForPileDTO setPileEroamingForPileDTO) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.setPileEroamingForPile setPileEroamingForPileDTO : {}",
                JSON.toJSONString(setPileEroamingForPileDTO));

        SetPileEroamingDTO setPileEroamingDTO = new SetPileEroamingDTO();

        setPileEroamingDTO.setPileSn(setPileEroamingForPileDTO.getPileSn());
        setPileEroamingDTO.setPileEroamingOperateType(setPileEroamingForPileDTO.getOperateType());

        return this.setPileEroaming(setPileEroamingDTO);
    }

    @Override
    public Result<Boolean> batchSendLocalList(LocalListDTO localListDTO) {
        if (ObjectUtils.isEmpty(localListDTO) || StringUtils.isBlank(localListDTO.getSn())
                || CollectionUtils.isEmpty(localListDTO.getLocalCardInfoList())) {
            return Result.ofSucceed(false);
        }

        List<LocalCardInfoDTO> localCardInfoList = localListDTO.getLocalCardInfoList();
        List<String> cardList = localCardInfoList.stream().map(LocalCardInfoDTO::getCard).collect(Collectors.toList());

        //判断数据库中是否存在
        List<LocalAuthListEntity> localAuthListEntities = localAuthListMapper.selectList(new LambdaQueryWrapper<LocalAuthListEntity>()
                .in(LocalAuthListEntity::getCard, cardList)
                .eq(LocalAuthListEntity::getSn, localListDTO.getSn()));
        if (!CollectionUtils.isEmpty(localAuthListEntities)) {
            throw new MessageCodeException(PileBaseEnum.CARD_ALREADY_EXIST_AUTH_LIST);
        }

        //处理前端传入的新数据
        List<AuthorizationData> localAuthorizationList = new ArrayList<>();
        for (LocalCardInfoDTO cardInfoDTO : localCardInfoList) {
            AuthorizationData authorizationData = AuthorizationData.builder()
                    .idTag(cardInfoDTO.getCard())
                    .idTagInfo(IdTagInfoVO.builder()
                            .status(LocalAuthListEnum.findValue(cardInfoDTO.getStatus())).build()).build();
            if (ObjectUtils.isNotEmpty(cardInfoDTO.getValidityPeriod()) && !cardInfoDTO.getValidityPeriod().equals(-1L)) {
                authorizationData.getIdTagInfo().setExpiryDate(DateTime.of(cardInfoDTO.getValidityPeriod()));
            }
            localAuthorizationList.add(authorizationData);
        }

        //查询商家底下所有的白名单列表
        List<LocalAuthListEntity> allAuthListBySellerId = localAuthListRepositoryImpl.getAllAuthListBySellerId(localListDTO.getSn(), LoginUserHolder.getLoginUser().getPayload().getSellerId());
        if (!CollectionUtils.isEmpty(allAuthListBySellerId)) {
            for (LocalAuthListEntity localAuthListEntity : allAuthListBySellerId) {
                AuthorizationData data = AuthorizationData.builder()
                        .idTag(localAuthListEntity.getCard())
                        .idTagInfo(IdTagInfoVO.builder()
                                .status(LocalAuthListEnum.findValue(localAuthListEntity.getStatus())).build()).build();
                if (ObjectUtils.isNotEmpty(localAuthListEntity.getExpiredTime()) && !localAuthListEntity.getExpiredTime().equals(-1L)) {
                    data.getIdTagInfo().setExpiryDate(DateTime.of(localAuthListEntity.getExpiredTime()));
                }
                localAuthorizationList.add(data);
            }
        }

        if (!CollectionUtils.isEmpty(localAuthorizationList) && localAuthorizationList.size() > 10) {
            throw new MessageCodeException(PileBaseEnum.CARD_NUMBER_REACHED_UPPER_LIMIT);
        }
        SendLocalListDataDTO sendLocalListDataDTO = SendLocalListDataDTO.builder()
                .listVersion(4)
                .updateType("Full")
                .localAuthorizationList(localAuthorizationList).build();
        SendMsgDto sendMsgDto = new SendMsgDto();
        sendMsgDto.setReceiver("sn-" + localListDTO.getSn());
        String seq = IdWorker.getIdStr();
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListKey(localListDTO.getSn() + "_" + seq), LoginUserHolder.getLoginUser().getPayload().getSellerId().toString(), 2, TimeUnit.MINUTES);
        //保存全部白名单列表
        for (LocalCardInfoDTO cardInfoDTO : localCardInfoList) {
            LocalAuthListEntity entity = new LocalAuthListEntity();
            entity.setCard(cardInfoDTO.getCard());
            entity.setExpiredTime(cardInfoDTO.getValidityPeriod());
            entity.setSellerId(LoginUserHolder.getLoginUser().getPayload().getSellerId());
            entity.setStatus(cardInfoDTO.getStatus());
            entity.setSn(localListDTO.getSn());
            allAuthListBySellerId.add(entity);
        }

        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListUserIdKey(localListDTO.getSn() + "_" + seq), LoginUserHolder.getLoginUser().getId().toString(), 2, TimeUnit.MINUTES);
        stringRedisTemplate.opsForValue().set(RedisKeyConstant.sendLocalListDataKey(localListDTO.getSn() + "_" + seq), JSON.toJSONString(allAuthListBySellerId), 2, TimeUnit.MINUTES);
        sendMsgDto.setMsg(buildConfigurationMsg(OcppAction.SEND_LOCAL_LIST.getValue(), JSON.toJSONString(sendLocalListDataDTO), seq));
        log.info("sendLocalList,sendMsgDto={}", JSON.toJSONString(sendMsgDto));
        return wxProxyClient.sendMsg(sendMsgDto);
    }

    @Override
    public Map<String, List<CommonVO>> getPileLocationMap() {
        return opLocationPileEvseRepository.getPileLocationMap();
    }

    @Override
    public List<SelectChargingInfoForFleetVO> selectChargingInfoForFleet(SelectChargingInfoForFleetDTO selectChargingInfoForFleetDTO) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.selectChargingInfoForFleet selectChargingInfoForFleetDTO : {}",
                JSON.toJSONString(selectChargingInfoForFleetDTO));

        if (selectChargingInfoForFleetDTO == null
                || ObjectUtils.isEmpty(selectChargingInfoForFleetDTO.getVehicleMacList())) {
            return null;
        }

        List<String> carMacList = new ArrayList<>();

        List<String> orderNumberList = new ArrayList<>();
        Map<String, String> carMacAndOrderNumberMap = new HashMap<>();

        List<String> evseSnList = new ArrayList<>();
        Map<String, String> carMacAndEvseSnMap = new HashMap<>();

        List<String> orderSeqList = new ArrayList<>();
        List<String> vehicleMacList = selectChargingInfoForFleetDTO.getVehicleMacList();
        for (String carMac : vehicleMacList) {

            if (StringUtils.isBlank(carMac)) {
                continue;
            }

            String redisResult = stringRedisTemplate.opsForValue().get(ConfigRedisKeyConstant.getCacheCurrentChargeCarInfoDtoKey(carMac.toUpperCase()));
            if (StringUtils.isBlank(redisResult)) {
                continue;
            }

            CacheChargeInfoDTO cacheChargeInfoDTO = JSON.parseObject(redisResult, CacheChargeInfoDTO.class);
            if (cacheChargeInfoDTO != null
                    && StringUtils.isNotBlank(cacheChargeInfoDTO.getCarMac())) {

                carMacList.add(carMac);

                String orderNumber = cacheChargeInfoDTO.getOrderNumber();
                orderNumberList.add(orderNumber);
                carMacAndOrderNumberMap.put(carMac, orderNumber);

                String evseSn = cacheChargeInfoDTO.getEvseSn();
                evseSnList.add(evseSn);
                carMacAndEvseSnMap.put(carMac, evseSn);

                String orderSeq = cacheChargeInfoDTO.getOrderSeq();
                orderSeqList.add(orderSeq);
            }
        }

        if (ObjectUtils.isEmpty(carMacList)) {
            return null;
        }

        Map<String, OpLocationEvseElasticDTO> evseSnAndOpLocationEvseElasticDTOMap = new HashMap<>();
        List<OpLocationEvseElasticDTO> opLocationEvseElasticDTOs = opLocationEvseRepository.getOpLocationEvseElasticDTOs(evseSnList);
        if (ObjectUtils.isNotEmpty(opLocationEvseElasticDTOs)) {
            opLocationEvseElasticDTOs.forEach(val -> evseSnAndOpLocationEvseElasticDTOMap.put(val.getEvseSn(), val));
        }

        Map<String, List<SmartChargingTimeSharingVo>> evseSnAndSmartChargingTimeSharingVoListMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(opLocationEvseElasticDTOs)) {
            Optional<OpLocationElasticDTO> optional = opLocationElastic.findById(opLocationEvseElasticDTOs.get(0).getLocationId());
            if (optional.isPresent()) {
                OpLocationElasticDTO opLocationElasticDTO = optional.get();
                Result<List<SmartChargingTimeSharingVo>> querySmartChargingPlanByPileGunsResult = fleetFeignClient.querySmartChargingPlanByPileGuns(opLocationElasticDTO.getZoneId(), evseSnList);

                log.info("===>>> OpLocationPileEvseServiceServiceImpl.selectChargingInfoForFleet querySmartChargingPlanByPileGunsResult : {}",
                        JSON.toJSONString(querySmartChargingPlanByPileGunsResult));

                if (querySmartChargingPlanByPileGunsResult != null
                        && ObjectUtils.isNotEmpty(querySmartChargingPlanByPileGunsResult.getData())) {
                    List<SmartChargingTimeSharingVo> smartChargingTimeSharingVoList = querySmartChargingPlanByPileGunsResult.getData();
                    evseSnAndSmartChargingTimeSharingVoListMap.putAll(smartChargingTimeSharingVoList
                            .stream()
                            .collect(Collectors.groupingBy(SmartChargingTimeSharingVo::getChargingPointId)));
                }
            }
        }

        Map<String, TariffComputeDTO> orderSeqAndTariffComputeDTOMap = new HashMap<>();
        for (String orderSeq : orderSeqList) {
            String tariffComputeKey = TariffRedisKeyUtils.buildKey(TariffRedisKeyUtils.STRING_CHARGING_TARIFF_COMPUTE_INFO, orderSeq);
            Object feeObject = redisTemplate.opsForValue().get(tariffComputeKey);
            if (feeObject != null) {
                orderSeqAndTariffComputeDTOMap.put(orderSeq, (TariffComputeDTO) feeObject);
            }
        }

        Map<String, ChargeBillInfoVO> orderNumberAndChargeBillInfoVOMap = new HashMap<>();
        Result<List<ChargeBillInfoVO>> batchGetBillInfoResult = billFeignClient.batchGetBillInfo(orderNumberList);

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.selectChargingInfoForFleet batchGetBillInfoResult : {}",
                JSON.toJSONString(batchGetBillInfoResult));

        if (batchGetBillInfoResult != null
                && ObjectUtils.isNotEmpty(batchGetBillInfoResult.getData())) {
            batchGetBillInfoResult.getData().forEach(val -> orderNumberAndChargeBillInfoVOMap.put(val.getOrderNumber(), val));
        }

        List<SelectChargingInfoForFleetVO> selectChargingInfoForFleetVOList = new ArrayList<>();
        for (String carMac : carMacList) {
            SelectChargingInfoForFleetVO selectChargingInfoForFleetVO = new SelectChargingInfoForFleetVO();

            selectChargingInfoForFleetVO.setVehicleMac(carMac);
            OpLocationEvseElasticDTO opLocationEvseElasticDTO = evseSnAndOpLocationEvseElasticDTOMap.get(carMacAndEvseSnMap.get(carMac));

            if (opLocationEvseElasticDTO == null) {
                continue;
            }

            ChargingPileInfoVO chargingPileInfoVO = new ChargingPileInfoVO();

            chargingPileInfoVO.setPileSn(opLocationEvseElasticDTO.getPileSn());
            chargingPileInfoVO.setPileName(opLocationEvseElasticDTO.getPileName());

            ChargingGunInfoVO chargingGunInfoVO = new ChargingGunInfoVO();

            chargingGunInfoVO.setEvseSn(opLocationEvseElasticDTO.getEvseSn());
            Integer gunNo = CommonUtil.getGunNo(opLocationEvseElasticDTO.getEvseSn());
            if (gunNo != null) {
                chargingGunInfoVO.setConnectorNumber(String.valueOf(gunNo));
            }
            chargingGunInfoVO.setStatus(opLocationEvseElasticDTO.getState());

            chargingPileInfoVO.setChargingGunInfoVO(chargingGunInfoVO);

            selectChargingInfoForFleetVO.setChargingPileInfoVO(chargingPileInfoVO);

            ChargeBillInfoVO chargeBillInfoVO = orderNumberAndChargeBillInfoVOMap.get(carMacAndOrderNumberMap.get(carMac));
            if (chargeBillInfoVO == null) {
                continue;
            }

            String orderSeq = chargeBillInfoVO.getOrderSeq();
            VehicleChargingInfoVO vehicleChargingInfoVO = new VehicleChargingInfoVO();

            vehicleChargingInfoVO.setTimeZone(chargeBillInfoVO.getTimeZone());
            vehicleChargingInfoVO.setZoneId(chargeBillInfoVO.getZoneId());
            vehicleChargingInfoVO.setStartChargingTimestamp(chargeBillInfoVO.getStartChargingTimestamp());
            vehicleChargingInfoVO.setStartSoc(chargeBillInfoVO.getStartSoc());

            TariffComputeDTO tariffComputeDTO = orderSeqAndTariffComputeDTOMap.get(orderSeq);
            if (tariffComputeDTO != null) {
                Integer currentSoc = tariffComputeDTO.getCurrentSoc();
                if (currentSoc != null) {
                    vehicleChargingInfoVO.setCurrentSoc(new BigDecimal(String.valueOf(currentSoc)));
                }
                vehicleChargingInfoVO.setUpdateAt(tariffComputeDTO.getLastTimeLong());
                List<SmartChargingTimeSharingVo> smartChargingTimeSharingVoList = evseSnAndSmartChargingTimeSharingVoListMap.get(opLocationEvseElasticDTO.getEvseSn());
                vehicleChargingInfoVO.setRemainingChargingTime(this.calculateRemainingChargingTime(smartChargingTimeSharingVoList, tariffComputeDTO, chargeBillInfoVO.getZoneId()));
                if (vehicleChargingInfoVO.getRemainingChargingTime() != null) {
                    BigDecimal remainingChargingTime = vehicleChargingInfoVO.getRemainingChargingTime();
                    Long timestamp = Long.valueOf(remainingChargingTime.multiply(new BigDecimal("60000")).toPlainString());
                    vehicleChargingInfoVO.setExpectedToBeFullTimestamp(tariffComputeDTO.getStartTimeLong() + timestamp);
                }
            }

            selectChargingInfoForFleetVO.setVehicleChargingInfoVO(vehicleChargingInfoVO);

            selectChargingInfoForFleetVOList.add(selectChargingInfoForFleetVO);
        }

        return selectChargingInfoForFleetVOList;
    }

    private BigDecimal calculateRemainingChargingTime(List<SmartChargingTimeSharingVo> smartChargingTimeSharingVoList, TariffComputeDTO tariffComputeDTO, String zoneId) {

        log.info("===>>> OpLocationPileEvseServiceServiceImpl.calculateRemainingChargingTime smartChargingTimeSharingVoList : {} and tariffComputeDTO : {} and zoneId : {}",
                JSON.toJSONString(smartChargingTimeSharingVoList),
                JSON.toJSONString(tariffComputeDTO),
                JSON.toJSONString(zoneId));


        if (ObjectUtils.isEmpty(smartChargingTimeSharingVoList)
                || tariffComputeDTO == null) {
            return null;
        }

        BigDecimal chargingPower = tariffComputeDTO.getChargingPower();
        Long startTimeLong = tariffComputeDTO.getStartTimeLong();

        if (chargingPower == null
                || startTimeLong == null) {
            return null;
        }

        BigDecimal sumPower = tariffComputeDTO.getSumPower();
        BigDecimal chargedAmount;
        if (sumPower == null) {
            chargedAmount = BigDecimal.ZERO;
        } else {
            chargedAmount = sumPower.divide(new BigDecimal("1000"));
        }

        chargingPower = chargingPower.divide(new BigDecimal("1000"));

        for (SmartChargingTimeSharingVo smartChargingTimeSharingVo : smartChargingTimeSharingVoList) {
            String energy = smartChargingTimeSharingVo.getEnergy();

            if (StringUtils.isBlank(energy)) {
                continue;
            }

            String startTime = smartChargingTimeSharingVo.getStartTime();
            String endTime = smartChargingTimeSharingVo.getEndTime();
            if (StringUtils.isBlank(startTime)
                    || StringUtils.isBlank(endTime)) {
                continue;
            }

            Long startTimestamp = TimeZoneUtil.getTimestamp(startTime, zoneId);
            Long endTimestamp = TimeZoneUtil.getTimestamp(endTime, zoneId);

            if (startTimeLong >= startTimestamp
                    && startTimeLong <= endTimestamp) {
                BigDecimal totalPower = new BigDecimal(energy);
                if (totalPower.compareTo(chargedAmount) > -1) {
                   return totalPower
                           .subtract(chargedAmount)
                           .divide(chargingPower, 2, BigDecimal.ROUND_HALF_UP)
                           .multiply(new BigDecimal("60"))
                           .setScale(0, BigDecimal.ROUND_HALF_UP);
                }
            }
        }
        return null;
    }
}
