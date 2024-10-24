package com.autel.cloud.pile.base.domain.service.impl;

;import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.code.HttpCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.repository.RuleRepository;
import com.autel.cloud.pile.base.domain.service.EvseBatchConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.dto.AssociatedGunDTO;
import com.autel.cloud.pile.base.dto.GunInformationDTO;
import com.autel.cloud.pile.base.dto.RemoveGunDTO;
import com.autel.cloud.pile.base.dto.batch.EvseBatchConfigDTO;
import com.autel.cloud.pile.base.dto.batch.EvseBatchConfigResponseDTO;
import com.autel.cloud.pile.base.dto.batch.EvseConfigQueryDTO;
import com.autel.cloud.pile.base.dto.batch.EvseRuleConfigDTO;
import com.autel.cloud.pile.base.dto.tariff.BindCostModelRuleGroupForGunDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.RuleMapper;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.autel.cloud.pile.base.vo.batch.BindRuleEvseVo;
import com.autel.cloud.pile.base.vo.batch.EvseConfigVO;
import com.autel.cloud.pile.base.vo.batch.EvseRuleVO;
import com.autel.cloud.pile.user.api.dto.AddRelationDTO;
import com.autel.cloud.pile.user.api.dto.MarketingRuleSearchDTO;
import com.autel.cloud.pile.user.api.dto.batch.MarketingRuleResponseDTO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.MarketingRulePileVO;
import com.autel.cloud.tariff.dto.business.BusinessCostModelRuleHeadDTO;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.business.BusinessCostModelHeadVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import feign.FeignException;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.lucene.queryparser.classic.QueryParserBase;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 充电桩/枪批量配置服务实现类
 */
@Service
@Slf4j
public class EvseBatchConfigServiceImpl implements EvseBatchConfigService {

    @Autowired
    private OpLocationRepository opLocationRepository;
    @Autowired
    private PileUserFeign pileUserFeign;
    @Autowired
    private ElasticsearchRestTemplate elasticsearchRestTemplate;
    @Autowired
    private  TariffFeignClient tariffFeignClient;
    @Autowired
    private RuleMapper ruleMapper;

    @Autowired
    private RuleRepository ruleRepository;

    @Autowired
    private OpLocationEvseService opLocationEvseService;
    @Autowired
    private OpLocationPileEvseService opLocationPileEvseService;

    private static final Long NO_DEVICE_TO_CONFIG = -1L;
    private static final Long BIND_COST_MODEL_RULE_FAIL =  -2L;
    private static final Long BIND_ENTRY_CONTROL_RULE_FAIL =  -3L;
    private static final Long BIND_MARKETING_RULE_FAIL =  -4L;
    private static final Long BATCH_CONFIG_SUCCESS = 1L;

    @Override
    public IPage<EvseConfigVO> page(EvseConfigQueryDTO evseConfigQueryDTO) {
        Long sellerId = LoginUserUtil.getSellerId();

        String keyWord = evseConfigQueryDTO.getKeyWord();
        Integer page = evseConfigQueryDTO.getPage();
        Integer pageSize = evseConfigQueryDTO.getPageSize();

        IPage<EvseConfigVO> resultPage = new Page<>(page,pageSize);
        if(StringUtils.isEmpty(evseConfigQueryDTO.getOrderType())){
            evseConfigQueryDTO.setOrderType("asc");
        }
        List<Long> locationIds = evseConfigQueryDTO.getLocationIds();
        if(CollectionUtils.isEmpty(locationIds)){
            if (LoginUserUtil.isSellerAdmin()) {
                locationIds =  opLocationRepository.getLocationIdBySellerId(sellerId);
            } else {
                locationIds = this.pileUserFeign.getLocationIds().getData();
            }
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("========>EvseBatchConfigServiceImpl page locationIds is empty.");
            return resultPage;
        }
        log.info("========>EvseBatchConfigServiceImpl page locationIds:{}",locationIds);
        /*获取充电枪列表*/
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", sellerId));
        queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));
        if (StringUtils.isNotEmpty(keyWord)) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            keyWord = QueryParserBase.escape(keyWord);
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", keyWord)));
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileSn", String.format("*%s*", keyWord)));
            queryBuilder.must(queryBuilder2);
        }
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort(StringUtils.isEmpty(evseConfigQueryDTO.getOrderBy())? "pileName": evseConfigQueryDTO.getOrderBy())
                        .order( StringUtils.isEmpty(evseConfigQueryDTO.getOrderType()) || evseConfigQueryDTO.getOrderType().equals("asc") ? SortOrder.ASC : SortOrder.DESC))
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withPageable(PageRequest.of(page - 1, pageSize)).build();
        log.info("========>EvseBatchConfigServiceImpl page searchQuery:{}",searchQuery);
        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> searchPage = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("========>EvseBatchConfigServiceImpl page  OpLocationEvseElasticDTO  List :{}", JSON.toJSON(list));
        if (CollectionUtils.isEmpty(list)) {
            log.info("========>EvseBatchConfigServiceImpl page OpLocationEvseElasticDTO List is empty.");
            return resultPage;
        }
        resultPage.setRecords(getEvseConfigList(list, sellerId));
        resultPage.setTotal(searchPage.getTotalElements());
        resultPage.setPages(searchPage.getTotalPages());
        return resultPage;
    }


    @Override
    public IPage<EvseConfigVO> queryEvseList(Integer page,Integer pageSize,String keyWord) {
        Long sellerId = LoginUserUtil.getSellerId();
        List<Long> locationIds = null;
        IPage<EvseConfigVO> resultPage = new Page<>(page,pageSize);
        if (LoginUserUtil.isSellerAdmin()) {
            locationIds =  opLocationRepository.getLocationIdBySellerId(sellerId);
        } else {
            locationIds = this.pileUserFeign.getLocationIds().getData();
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("========>EvseBatchConfigServiceImpl queryEvseList locationIds is empty.");
            return resultPage;
        }
        log.info("========>EvseBatchConfigServiceImpl queryEvseList locationIds:{}",locationIds);
        /*获取充电枪列表*/
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.must(QueryBuilders.termQuery("operatorId", sellerId));
        queryBuilder.must(QueryBuilders.termsQuery("locationId", locationIds));
        if (StringUtils.isNotEmpty(keyWord)) {
            BoolQueryBuilder queryBuilder2 = QueryBuilders.boolQuery();
            keyWord = QueryParserBase.escape(keyWord);
            queryBuilder2.should(QueryBuilders.wildcardQuery("pileName", String.format("*%s*", keyWord)));
            queryBuilder2.should(QueryBuilders.wildcardQuery("evseSn", String.format("*%s*", keyWord)));
            queryBuilder.must(queryBuilder2);
        }
        NativeSearchQuery searchQuery = new NativeSearchQueryBuilder()
                .withQuery(queryBuilder)
                .withSorts(SortBuilders.fieldSort("pileName").order(SortOrder.ASC))
                .withSorts(SortBuilders.fieldSort("evseSn").order(SortOrder.ASC))
                .withPageable(PageRequest.of(page - 1, pageSize)).build();;
        SearchHits<OpLocationEvseElasticDTO> search = elasticsearchRestTemplate.search(searchQuery, OpLocationEvseElasticDTO.class);
        SearchPage<OpLocationEvseElasticDTO> searchPage = SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
        List<OpLocationEvseElasticDTO> list = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        log.info("========>EvseBatchConfigServiceImpl queryEvseList OpLocationEvseElasticDTO  List :{}", JSON.toJSON(list));
        if (CollectionUtils.isEmpty(list)) {
            log.info("========>EvseBatchConfigServiceImpl queryEvseList OpLocationEvseElasticDTO List is empty.");
            return resultPage;
        }
        resultPage.setRecords(getEvseConfigList(list, sellerId));
        resultPage.setTotal(searchPage.getTotalElements());
        resultPage.setPages(searchPage.getTotalPages());
        return resultPage;
    }

    /*
     *  获取商家对应计费规则下拉列表
     */

    @Override
    public List<EvseRuleVO> getCostRuleList() {
        BusinessCostModelRuleHeadDTO businessCostModelRuleHeadDTO = new BusinessCostModelRuleHeadDTO();
        businessCostModelRuleHeadDTO.setExcludeTariffGroupIdList(Collections.emptyList());
        Result<List<BusinessCostModelHeadVO>> costModelHeadlistResult = tariffFeignClient.queryBusinessCostModelRuleHead(businessCostModelRuleHeadDTO);
        if (CollectionUtils.isEmpty(costModelHeadlistResult.getData())) {
            return Collections.emptyList();
        }
        return costModelHeadlistResult.getData().stream()
                .map(r->new EvseRuleVO(r.getTariffGroupId(),r.getRuleName()))
                .collect(Collectors.toList());
    }

    /*
     *  获取商家对应营销规则下拉列表
     */

    @Override
    public List<EvseRuleVO> getMarketingRuleList() {
        Long sellerId = LoginUserUtil.getSellerId();
        Result<List<MarketingRuleResponseDTO>> marketingRuleListResult = pileUserFeign.queryMarketingRuleList(sellerId);
        if(CollectionUtils.isEmpty(marketingRuleListResult.getData())){
            return Collections.emptyList();
        }
        return marketingRuleListResult.getData().stream()
                .map(r->new EvseRuleVO(r.getId(),r.getName()))
                .collect(Collectors.toList());
    }

    /*
     *  获取商家对应进场控制下拉列表
     */
    @Override
    public List<EvseRuleVO> getEntryControlList() {
        return ruleMapper.findRulesList(LoginUserUtil.getSellerId());
    }

    /*
     *  获取充电枪配置
     */
    private List<EvseConfigVO> getEvseConfigList(List<OpLocationEvseElasticDTO> list,Long sellerId) {
        Map<String, List<EvseRuleVO>> evseSnToMarketingNameMap = getMarketingRuleListResult(sellerId);
        Map<Long, String> tariffIdToName = getTariffIdToName();
        Map<String, Long> snIdMap = getSnIdMap(list);
        List<EvseConfigVO> evseConfigVOS = new ArrayList<>();
        for (OpLocationEvseElasticDTO opLocationEvseElasticDTO : list) {
            List<EvseRuleVO> evseMarketingNameList = evseSnToMarketingNameMap.get(opLocationEvseElasticDTO.getEvseSn());
            evseConfigVOS.add(new EvseConfigVO(
                    opLocationEvseElasticDTO.getLocationName(),
                    opLocationEvseElasticDTO.getLocationId(),
                    snIdMap.get(opLocationEvseElasticDTO.getPileSn()),
                    opLocationEvseElasticDTO.getPileSn(),
                    opLocationEvseElasticDTO.getEvseSn(),
                    opLocationEvseElasticDTO.getPileName(),
                    CommonUtil.getGunNo(opLocationEvseElasticDTO.getEvseSn()).toString(),
                    opLocationEvseElasticDTO.getId(),
                    opLocationEvseElasticDTO.getGunType(),
                    getEvseName(opLocationEvseElasticDTO.getPileName(),opLocationEvseElasticDTO.getEvseSn()),
                    tariffIdToName.get(opLocationEvseElasticDTO.getTariffId()),
                    opLocationEvseElasticDTO.getTariffId()==null ? null : opLocationEvseElasticDTO.getTariffId().toString(),
                    evseMarketingNameList,
                    opLocationEvseElasticDTO.getRuleName(),
                    opLocationEvseElasticDTO.getRuleId()==null ? null : opLocationEvseElasticDTO.getRuleId().toString()
            ));
        }
        return evseConfigVOS;
    }


    /*
     * 获取充电枪对应营销规则列表
     *
     */

    private Map<String, List<EvseRuleVO>> getMarketingRuleListResult(Long sellerId) {
        Result<List<MarketingRuleResponseDTO>> marketingRuleListResult = pileUserFeign.queryEvseMarketingRuleList(sellerId);
        if (CollectionUtils.isEmpty(marketingRuleListResult.getData())){
            return Collections.emptyMap();
        }
        return marketingRuleListResult.getData().stream()
                .filter(r -> !StringUtils.isEmpty(r.getEveSn()))
                .collect(Collectors.groupingBy(
                        MarketingRuleResponseDTO::getEveSn,
                        Collectors.mapping(
                                r -> new EvseRuleVO(r.getId(),r.getName()),
                                Collectors.toList())));
    }

    /*
     * 获取充电枪对应计费规则
     *
     */
    private Map<Long, String> getTariffIdToName(){
        BusinessCostModelRuleHeadDTO businessCostModelRuleHeadDTO = new BusinessCostModelRuleHeadDTO();
        businessCostModelRuleHeadDTO.setExcludeTariffGroupIdList(Collections.emptyList());
        Result<List<BusinessCostModelHeadVO>> costModelHeadlistResult = tariffFeignClient.queryBusinessCostModelRuleHead(businessCostModelRuleHeadDTO);
        if (CollectionUtils.isEmpty(costModelHeadlistResult.getData())) {
            return Collections.emptyMap();
        }
        return  costModelHeadlistResult.getData().stream()
                .collect(Collectors.toMap(
                        BusinessCostModelHeadVO::getTariffGroupId,
                        BusinessCostModelHeadVO::getRuleName));
    }

    /*
     * 获取桩对应桩Id
     */
    private Map<String, Long> getSnIdMap(List<OpLocationEvseElasticDTO> list) {
        if(CollectionUtils.isEmpty(list)){
            return Collections.emptyMap();
        }
        Set<String> pileSnList = list.stream().map(OpLocationEvseElasticDTO::getPileSn).collect(Collectors.toSet());
        List<OpLocationPileEvseElasticDTO> opLocationPileEvseServiceList = opLocationPileEvseService.findList(new ArrayList<>(pileSnList));
        if (CollectionUtils.isEmpty(opLocationPileEvseServiceList)) {
            return Collections.emptyMap();
        }
        return opLocationPileEvseServiceList.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, OpLocationPileEvseElasticDTO::getId, (e1, e2) -> e1));
    }


    /*
     * 生成枪名称
     */

    private String getEvseName(String pileName,String evseSn){
        if(StringUtils.isEmpty(pileName)){
            return evseSn;
        }
        return String.format("%s_%s",pileName,evseSn);
    }

    @Override
    public EvseBatchConfigResponseDTO batchConfigEvse(EvseBatchConfigDTO evseBatchConfigDTO) {
        Long sellerId = LoginUserUtil.getSellerId();

        log.info("====> EvseBatchConfigServiceImpl batchConfigEvse EvseBatchConfigDTO :{} ", JSON.toJSONString(evseBatchConfigDTO));
        if(evseBatchConfigDTO == null
                || CollectionUtils.isEmpty(evseBatchConfigDTO.getPiles())
                || CollectionUtils.isEmpty(evseBatchConfigDTO.getEvseSn())
                || CollectionUtils.isEmpty(evseBatchConfigDTO.getGunId())
                || evseBatchConfigDTO.getTariffId() == null
        ){
            return new EvseBatchConfigResponseDTO(NO_DEVICE_TO_CONFIG);
        }
        List<EvseRuleConfigDTO> piles = evseBatchConfigDTO.getPiles();
        List<String> evseSn = evseBatchConfigDTO.getEvseSn();

        /* 批量绑定桩计费规则 */
        Long tariffId = evseBatchConfigDTO.getTariffId();
        List<Long> gunId = evseBatchConfigDTO.getGunId();
        List<BindCostModelRuleGroupForGunDTO> bindCostModelRuleGroupForGunDTOList  = gunId.stream()
                .map(i -> {
                    BindCostModelRuleGroupForGunDTO dto = new BindCostModelRuleGroupForGunDTO();
                    dto.setTariffGroupId(tariffId);
                    dto.setGunId(i);
                    return dto;
                })
                .collect(Collectors.toList());
        Result<Boolean> bindCostModelRuleGroupResult = null;
        try {
            bindCostModelRuleGroupResult = opLocationEvseService.bindCostModelRuleGroupForGun(bindCostModelRuleGroupForGunDTOList);
        }catch(MessageCodeException e){
            log.info("====> EvseBatchConfigServiceImpl batchConfigEvse evseBatchBindCostModule Failed : {}", e.getCode());
        }catch(Exception e){
            log.info("====>evseBatchBindCostModule Failed : {}", e.getMessage());
        }
        if(bindCostModelRuleGroupResult == null
                || bindCostModelRuleGroupResult.getCode() != HttpCodeEnum.OK.getCode()){
            return new EvseBatchConfigResponseDTO(BIND_COST_MODEL_RULE_FAIL);
        }


        /* 批量绑定入场规则 */
        Long ruleId = evseBatchConfigDTO.getRuleId();
        List<BindRuleEvseVo> bindVoList = ruleMapper.findBindEvseAndRule(evseSn, sellerId);
        /* 已绑定入场规枪解绑原入场规则 */
        if(!CollectionUtils.isEmpty(bindVoList)){
            Map<Long, List<GunInformationDTO>> bindRuleAndEvseMap = bindVoList.stream().collect(Collectors.groupingBy(
                    BindRuleEvseVo::getRuleId,
                    Collectors.mapping(v -> new GunInformationDTO(
                            v.getPileSn(),
                            CommonUtil.getGunNo(v.getEvseSn()).toString(),
                            v.getLocationId(),
                            v.getPileId(),
                            v.getPileName()), Collectors.toList())));
            Result<Boolean> removeRuleResult = null;
            try {
                for (Map.Entry<Long, List<GunInformationDTO>> it : bindRuleAndEvseMap.entrySet()) {
                    RemoveGunDTO removeGunDTO = new RemoveGunDTO();
                    removeGunDTO.setRuleId(it.getKey());
                    removeGunDTO.setPiles(it.getValue());
                    removeGunDTO.setReplaceRuleId(-1L);
                    removeRuleResult = ruleRepository.removeEvse(removeGunDTO);
                }
            }catch(MessageCodeException e){
                log.info("====> EvseBatchConfigServiceImpl batchConfigEvse removeRule Failed : {}", e.getCode());
            }catch(Exception e){
                log.info("====>removeRule Failed : {}",e.getMessage());
            }
            if( removeRuleResult == null
                    ||  removeRuleResult.getCode() != HttpCodeEnum.OK.getCode()){
                return new EvseBatchConfigResponseDTO(BIND_ENTRY_CONTROL_RULE_FAIL);
            }
        }

        /* 所有枪绑定新入场规则 */
        if( ruleId != null ){
            Result<Boolean> bindRuleResult = null;
            int retryRuleCount = 0;
            boolean success = false;
            while (retryRuleCount < 2 && !success) {  // 失败重试一次
                try {
                    AssociatedGunDTO associatedGunDTO = new AssociatedGunDTO();
                    List<GunInformationDTO> pileEntryList = piles.stream().map(p -> new GunInformationDTO(
                            p.getSn(),
                            p.getConnector(),
                            p.getLocationId(),
                            p.getPileId(),
                            p.getName()
                    )).collect(Collectors.toList());
                    associatedGunDTO.setPiles(pileEntryList);
                    associatedGunDTO.setRuleId(ruleId);
                    bindRuleResult = ruleRepository.associatedGun(associatedGunDTO);
                    success = (bindRuleResult != null && bindRuleResult.getCode() == HttpCodeEnum.OK.getCode());
                } catch (MessageCodeException e) {
                    log.info("====> EvseBatchConfigServiceImpl batchConfigEvse bindRule Failed : {}", e.getCode());
                } catch (Exception e) {
                    log.info("====>bindRule Failed : {}",e.getMessage());
                }
                retryRuleCount++;
            }
            if (!success) {
                return new EvseBatchConfigResponseDTO(BIND_ENTRY_CONTROL_RULE_FAIL);
            }
        }


        /* 批量绑定营销规则 */
        List<Long> marketingRuleIds = evseBatchConfigDTO.getMarketingRuleIds();
        /* 已绑定营销规则枪解绑原营销规则 */
        MarketingRuleSearchDTO marketingRuleSearchDTO= new MarketingRuleSearchDTO();
        marketingRuleSearchDTO.setSellerId(sellerId);
        marketingRuleSearchDTO.setMarketingRuleId(null);
        marketingRuleSearchDTO.setEveSnList(evseSn);
        Result<List<MarketingRulePileVO>> bindMarketingRuleList = pileUserFeign.findBindList(marketingRuleSearchDTO);
        if(bindMarketingRuleList != null && !CollectionUtils.isEmpty(bindMarketingRuleList.getData())){
            List<Long> bindMarketingRuleIdList = bindMarketingRuleList.getData().stream().map(MarketingRulePileVO::getId).collect(Collectors.toList());
            Result<Boolean> removeMarketingResult = null;
            try {
                removeMarketingResult = pileUserFeign.deletedRelation(bindMarketingRuleIdList);
            }catch (FeignException e){
                log.info("====> EvseBatchConfigServiceImpl batchConfigEvse removeMarketingRule Failed : {}", e.responseBody());
            }catch (Exception e){
                log.info("====>removeMarketingRule Failed : {}", e.getMessage());
            }
            if( removeMarketingResult == null || removeMarketingResult.getCode() != HttpCodeEnum.OK.getCode()){
                return new EvseBatchConfigResponseDTO(BIND_MARKETING_RULE_FAIL);
            }
        }
        /*批量绑定营销规则*/
        if(!CollectionUtils.isEmpty(marketingRuleIds)) {
            List<AddRelationDTO> pileMarketingList = piles.stream().map(p -> new AddRelationDTO(
                    null,
                    p.getSn(),
                    p.getConnector(),
                    null,
                    p.getGunType(),
                    p.getName(),
                    p.getLocationId(),
                    p.getLocationName()
            )).collect(Collectors.toList());
            for (Long marketingRuleId : marketingRuleIds) {
                pileMarketingList.forEach(dto -> dto.setMarketingRuleId(marketingRuleId));
                int retryMarketingCount = 0;
                boolean success = false;
                Result<Boolean> bindMarketingResult = null;
                while (retryMarketingCount < 2 && !success) {
                    try {
                        bindMarketingResult = pileUserFeign.addRelation(pileMarketingList);
                        success = (bindMarketingResult != null && bindMarketingResult.getCode() == HttpCodeEnum.OK.getCode());
                    } catch (FeignException e) {
                        log.info("====> EvseBatchConfigServiceImpl batchConfigEvse bindMarketing Failed : {}", e.responseBody());
                    } catch (Exception e) {
                        log.info("====>bindMarketing Failed : {}", e.getMessage());
                    }
                    retryMarketingCount++;
                }
                if (!success) {
                    return new EvseBatchConfigResponseDTO(BIND_MARKETING_RULE_FAIL);
                }
            }
        }

        /*批量绑定成功*/
        return new EvseBatchConfigResponseDTO(BATCH_CONFIG_SUCCESS);
    }

}
