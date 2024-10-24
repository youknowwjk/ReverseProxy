package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupAssociateRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.domain.service.HomeService;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.TimePowerDTO;
import com.autel.cloud.pile.base.enums.LocationEvseStatusV2Enum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.PileUserServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.vo.OpLocationEvseStateCountVO;
import com.autel.cloud.pile.bill.dto.ChargePowerDTO;
import com.autel.cloud.pile.bill.dto.EmsGroupPowerDTO;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.assertj.core.util.Lists;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @Author A22282
 * @Date 2024/1/23 10:44
 */
@Service
@Slf4j
public class HomeServiceImpl implements HomeService {

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private OpLocationService opLocationService;

    @Resource
    private PileUserServiceFeign pileUserServiceFeign;

    @Resource
    private MonitorFeignClient monitorFeignClient;

    @Resource
    private OpLocationPileGroupAssociateRepository opLocationPileGroupAssociateRepository;

    @Resource
    private OpLocationEvseService opLocationEvseService;

    @Resource
    private OpLocationPileGroupRepository opLocationPileGroupRepository;

    @Override
    public Page<ChargePowerDTO> queryTimePower(TimePowerDTO dto) {
        Page<ChargePowerDTO> pageResult = new Page<>(dto.getCurrent(), dto.getSize());
        log.info("queryTimePower,dto={}", JSON.toJSONString(dto));
        Long sellerId = dto.getSellerId();
        if (sellerId == null) {
            log.info("queryTimePower,sellerId is null.");
            return pageResult;
        }
        List<Long> locationIds = dto.getLocationIds();
        if(CollectionUtils.isEmpty(locationIds)) {
            //如果为商家不调用user
            if (LoginUserUtil.isSellerAdmin()) {
                locationIds = this.opLocationService.getLocationIdBySellerId().getData();
            } else {
                locationIds = this.pileUserServiceFeign.getLocationIds().getData();
            }
        }
        if (CollectionUtils.isEmpty(locationIds)) {
            log.info("queryTimePower,locationIds is null.");
            return pageResult;
        }
        //查询场站
        BoolQueryBuilder query = QueryBuilders.boolQuery();
        query.must(QueryBuilders.termsQuery("id", locationIds));
        query.must(QueryBuilders.termQuery("operatorId", sellerId));
        NativeSearchQuery build = new NativeSearchQueryBuilder()
                .withQuery(query)
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "name", "transPower"}, null))
                .withPageable(PageRequest.of(dto.getCurrent() - 1, dto.getSize()))
                .withSorts(SortBuilders.fieldSort("id").order(SortOrder.DESC))
                .build();
        SearchHits<OpLocationElasticDTO> search = this.elasticsearchRestTemplate.search(build, OpLocationElasticDTO.class);
        long total = search.getTotalHits();
        List<OpLocationElasticDTO> locationDtoList = search.stream().map(SearchHit::getContent).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(locationDtoList)) {
            log.info("queryTimePower,locationDtoList is null.");
            return pageResult;
        }
        pageResult = new Page<>(dto.getCurrent(), dto.getSize(), total);
        //查询场站下的枪
        List<OpLocationEvseElasticDTO> evseDtoList = this.elasticsearchRestTemplate.searchForStream(new NativeSearchQueryBuilder()
                .withPageable(PageRequest.of(0, 500))
                .withQuery(QueryBuilders.termsQuery("locationId", locationDtoList.stream().map(OpLocationElasticDTO::getId).collect(Collectors.toList())))
                .withSourceFilter(new FetchSourceFilter(new String[]{"id", "power", "locationId", "state", "evseSn", "pileSn"}, null))
                .build(), OpLocationEvseElasticDTO.class).stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<ChargePowerDTO> resultList = new ArrayList<>();
        locationDtoList.stream().forEach(e -> {
            ChargePowerDTO powerDto = new ChargePowerDTO();
            powerDto.setId(e.getId());
            powerDto.setStationName(e.getName());
            powerDto.setAllPower(Optional.ofNullable(e.getTransPower()).orElse(BigDecimal.ZERO));
            powerDto.setRealPower(BigDecimal.ZERO);
            resultList.add(powerDto);
        });
        pageResult.setRecords(resultList);
        if (CollectionUtils.isEmpty(evseDtoList)) {
            log.info("queryTimePower,evseDtoList is null.");
            return pageResult;
        }
        //如果没有设置场站变压器功率则查询桩的额定功率
        List<OpLocationElasticDTO> tmpListToUse = locationDtoList.stream().filter(e -> e.getTransPower() == null).collect(Collectors.toList());
        Map<Long, BigDecimal> calcMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(tmpListToUse)) {
            List<Long> tmpIds = tmpListToUse.stream().map(OpLocationElasticDTO::getId).collect(Collectors.toList());
            Map<String, List<OpLocationEvseElasticDTO>> groupByPileSnMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
            groupByPileSnMap.forEach((k, v) -> {
                //取其中一把枪功率
                OpLocationEvseElasticDTO evseDto = v.get(0);
                Long locationId = evseDto.getLocationId();
                if (tmpIds.contains(locationId)) {
                    BigDecimal totalPower = calcMap.get(locationId);
                    if (totalPower == null) {
                        totalPower = BigDecimal.ZERO;
                    }
                    totalPower = totalPower.add(BigDecimal.valueOf(Optional.ofNullable(evseDto.getPower()).orElse(0D)));
                    calcMap.put(locationId, totalPower);
                }
            });
        }
        log.info("queryTimePower,calcMap={}", JSON.toJSONString(calcMap));
        //查询充电中实时功率
        Map<String, OpEvseMeterUploadDTO> uploadMap = new HashMap<>();
        List<String> evseSnList = evseDtoList.stream().filter(e -> EvseDeviceStatusEnum.CHARGING.getName().equals(e.getState())).map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        if (!CollectionUtils.isEmpty(evseSnList)) {
            List<OpEvseMeterUploadDTO> uploadDtoList = this.monitorFeignClient.queryNewMeters(evseSnList).getData();
            if (!CollectionUtils.isEmpty(uploadDtoList)) {
                uploadMap.putAll(uploadDtoList.stream().collect(Collectors.toMap(OpEvseMeterUploadDTO::getEvseSn, e -> e, (f, s) -> f)));
            }
        }
        Map<Long, List<OpLocationEvseElasticDTO>> evseDtoMap = evseDtoList.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getLocationId));
        resultList.stream().forEach(e -> {
            Long id = e.getId();
            BigDecimal t = e.getAllPower();
            if (t == null || t.compareTo(BigDecimal.ZERO) == 0) {
                t = Optional.ofNullable(calcMap.get(id)).orElse(BigDecimal.ZERO).setScale(2, BigDecimal.ROUND_HALF_UP);
            }
            e.setAllPower(t);
            BigDecimal timeValue = BigDecimal.ZERO;
            List<OpLocationEvseElasticDTO> tmpList = evseDtoMap.get(id);
            if (!CollectionUtils.isEmpty(tmpList)) {
                for (OpLocationEvseElasticDTO evseDto : tmpList) {
                    String evseSn = evseDto.getEvseSn();
                    OpEvseMeterUploadDTO uploadDto = uploadMap.get(evseSn);
                    if (uploadDto != null) {
                        timeValue = timeValue.add(BigDecimal.valueOf(Optional.ofNullable(uploadDto.getPower()).orElse(0D)));
                    }
                }
            }
            timeValue = timeValue.divide(BigDecimal.valueOf(1000L));
            // 小于20时四舍五入保留一位 大于20向下取整
            if (timeValue.compareTo(BigDecimal.valueOf(20L)) > 0) {
                timeValue = timeValue.setScale(0, BigDecimal.ROUND_DOWN);
            } else {
                timeValue = timeValue.setScale(1, BigDecimal.ROUND_HALF_UP);
            }
            e.setRealPower(timeValue);
        });
        log.info("queryTimePower,pageResult={}", JSON.toJSONString(pageResult));
        return pageResult;
    }

    @Override
    public EmsGroupPowerDTO queryEmsTimePower(Long groupId) {
        //定义待返回的数据
        EmsGroupPowerDTO result = new EmsGroupPowerDTO();
        result.setGroupId(groupId);
        //获取桩跟群组的关联信息
        List<OpLocationPileGroupAssociateEntity> list =  queryByGroupIds(groupId);
        if (CollectionUtils.isEmpty(list)) {
            return result;
        }

        //查询充电中实时功率
        List<String> pileSnList = list.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        //获取关联桩的数据
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        List<String> evseSnList = evseDtoList.stream().map(OpLocationEvseElasticDTO::getEvseSn).collect(Collectors.toList());
        //处理数据
        if (!CollectionUtils.isEmpty(evseSnList)) {
            //获取原始数据
            List<OpEvseMeterUploadDTO> uploadDtoList = this.monitorFeignClient.queryNewMeters(evseSnList).getData();
            if (!CollectionUtils.isEmpty(uploadDtoList)) {
                //赋初始值
                result.setRealPower(BigDecimal.ZERO);
                result.setRealCurrent(BigDecimal.ZERO);
                //遍历数据
                uploadDtoList.forEach(e ->{
                    if(EvseDeviceStatusEnum.CHARGING.getName().equalsIgnoreCase(e.getEvseStatusName())){
                        //只取充电中的
                        result.setRealPower(result.getRealPower().add(BigDecimal.valueOf(e.getPower())).setScale(2,RoundingMode.HALF_UP));
                        result.setRealCurrent(result.getRealCurrent().add(BigDecimal.valueOf(e.getCurrent())).setScale(2,RoundingMode.HALF_UP));
                    }
                });
                log.info("queryEmsTimePower,uploadDtoList={}", JSON.toJSONString(uploadDtoList));
                log.info("queryEmsTimePower,result={}", JSON.toJSONString(result));
                result.setRealPower(result.getRealPower().divide(BigDecimal.valueOf(1000L), RoundingMode.CEILING));
            }
        }
        return result;
    }

    @Override
    public List<OpLocationEvseStateCountVO> queryEmsEvseStateCount(Long groupId) {
        //获取桩跟群组的关联信息
        List<OpLocationPileGroupAssociateEntity> list = queryByGroupIds(groupId);
        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        List<String> pileSnList = list.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        List<OpLocationEvseElasticDTO> evseDtoList = opLocationEvseService.findList(pileSnList);
        List<OpLocationEvseStateCountVO> countResult = getOpLocationEvseStateCountVOS(evseDtoList);
        //计算总和
        if(countResult != null && countResult.size() >0){
            int num = 0;
            for (OpLocationEvseStateCountVO item : countResult){
                num += item.getCount();
            }
            OpLocationEvseStateCountVO opLocationEvseStateCountVO = new OpLocationEvseStateCountVO();
            opLocationEvseStateCountVO.setCount(num);
            opLocationEvseStateCountVO.setState(-1);
            countResult.add(opLocationEvseStateCountVO);
        }
        return countResult;
    }

    /**
     * 获取桩跟群的关联信息
     * @param groupId
     * @return
     */
    private List<OpLocationPileGroupAssociateEntity> queryByGroupIds(Long groupId){
        OpLocationPileGroupEntity byId = opLocationPileGroupRepository.getById(groupId);
        if(byId == null){
            return new ArrayList<>();
        }
        List<Long> idList = new ArrayList<>();
        //添加待查上级的Id
        idList.add(byId.getId());
        //递归遍历
        queryPileGroupByPId(Collections.singletonList(byId.getId()),idList);
        //查群组与桩的数据
        QueryWrapper<OpLocationPileGroupAssociateEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.lambda()
                //只获取有效的行
                .select(OpLocationPileGroupAssociateEntity::getPileSn)
                .eq(OpLocationPileGroupAssociateEntity::getDeleted,0)
                .in(OpLocationPileGroupAssociateEntity::getGroupId,idList);
        return opLocationPileGroupAssociateRepository.list(queryWrapper);
    }

    /**
     * 递归遍历
     * @param pIds
     * @param idList
     */
    private void queryPileGroupByPId(List<Long> pIds,List<Long> idList){
        //只查有效的行数据
        QueryWrapper<OpLocationPileGroupEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.lambda().select(OpLocationPileGroupEntity::getId)
                .in(OpLocationPileGroupEntity::getPid,pIds)
                .eq(OpLocationPileGroupEntity::getDeleted,0);
        List<OpLocationPileGroupEntity> entityList = opLocationPileGroupRepository.getBaseMapper().selectList(queryWrapper);
        if(entityList != null && entityList.size() >0){
            List<Long> collect = entityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toList());
            idList.addAll(collect);
            //递归遍历
            queryPileGroupByPId(collect,idList);
        }
    }

    /**
     * 获取枪状态统计List
     * @param list
     * @return
     */
    private List<OpLocationEvseStateCountVO> getOpLocationEvseStateCountVOS(List<OpLocationEvseElasticDTO> list) {
        Map<Integer, List<OpLocationEvseElasticDTO>> stateEvseListMap = list.stream().collect(Collectors.groupingBy(o -> LocationEvseStatusV2Enum.getEvseStatusEnumByOccpState(o.getState()).getCode()));
        List<OpLocationEvseStateCountVO> countResult = Lists.newArrayList();
        for (LocationEvseStatusV2Enum stateEnum : LocationEvseStatusV2Enum.values()) {
            OpLocationEvseStateCountVO vo = new OpLocationEvseStateCountVO();
            vo.setState(stateEnum.getCode());
            if (stateEvseListMap.containsKey(stateEnum.getCode())) {
                vo.setCount(stateEvseListMap.get(stateEnum.getCode()).size());
            } else {
                vo.setCount(0);
            }
            countResult.add(vo);
        }
        return countResult;
    }

}
