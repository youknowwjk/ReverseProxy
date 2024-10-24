package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.http.HttpStatus;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.edge.EdgeFeignClient;
import com.autel.cloud.edge.vo.edge.MeterBindVO;
import com.autel.cloud.meter.data.api.feign.MeterDataClient;
import com.autel.cloud.meter.data.api.vo.MeterDataRecordVO;
import com.autel.cloud.pile.base.domain.convert.LocationMeterConvert;
import com.autel.cloud.pile.base.domain.repository.LocationMeterRepository;
import com.autel.cloud.pile.base.domain.service.LocationMeterService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.LocationMeterDTO;
import com.autel.cloud.pile.base.dto.group.MeterDataRecordDTO;
import com.autel.cloud.pile.base.dto.meter.MeterIdsQueryDTO;
import com.autel.cloud.pile.base.dto.meter.MeterQueryDTO;
import com.autel.cloud.pile.base.dto.meter.PageParamDTO;
import com.autel.cloud.pile.base.dto.meter.PageQueryDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.enums.meter.MeterConnectionStatusEnum;
import com.autel.cloud.pile.base.enums.meter.MeterbrandEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.vo.LocationMeterVO;
import com.autel.cloud.pile.base.vo.MeterVO;
import com.autel.cloud.pile.base.vo.group.PileGroupBasicInfoVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @author temp
 * @description 针对表【op_location_meter(电表)】的数据库操作Service实现
 * @createDate 2023-01-29 17:26:48
 */
@Service
@Slf4j
public class LocationMeterServiceImpl implements LocationMeterService {

    public LocationMeterServiceImpl(LocationMeterRepository locationMeterRepository) {
        this.locationMeterRepository = locationMeterRepository;
    }

    private final LocationMeterRepository locationMeterRepository;

    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @Autowired
    private MeterDataClient meterDataClient;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private LocationMeterConvert convert;
    @Autowired
    private EdgeFeignClient edgeFeignClient;
    /**
     * 十分钟（单位毫秒）
     */
    private static final Long _10_MINUTE = 10 * 60 * 1000L;

    /**
     * 十五秒（单位毫秒）
     */
    private static final Long _15_SECOND = 15 * 1000L;

    /**
     * @param locationMeterDTO
     * @return
     * @function 添加电表
     */
    @Override
    @Transactional
    public Long add(LocationMeterDTO locationMeterDTO) {

        log.info("===>>>LocationMeterServiceImpl.add locationMeterDTO : {}",
                JSON.toJSONString(locationMeterDTO));

        if (locationMeterDTO.getId() != null) {
            locationMeterDTO.setId(null);
        }

        this.checkLocationMeterDTO(locationMeterDTO);

        Long id = IdWorker.getId();
        Long currentTimeMillis = System.currentTimeMillis();
        Long operatorId = null;
        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        if (jwtInfo != null
                && jwtInfo.getPayload() != null
                && jwtInfo.getPayload().getSellerId() != null) {
            operatorId = jwtInfo.getPayload().getSellerId();
        }

        LocationMeterEntity locationMeterEntity = new LocationMeterEntity();

        locationMeterEntity.setId(id);
        locationMeterEntity.setName(locationMeterDTO.getName());
        locationMeterEntity.setBrandEnum(locationMeterDTO.getBrandEnum());
        locationMeterEntity.setOperatorId(operatorId);
        locationMeterEntity.setCreatedAt(currentTimeMillis);
        locationMeterEntity.setUpdatedAt(currentTimeMillis);
        locationMeterEntity.setDeleted(0);
        locationMeterEntity.setPin(locationMeterDTO.getPIN());
        locationMeterEntity.setSn(locationMeterDTO.getSN());

        //边缘云需求  新增字段
        locationMeterEntity.setCategory(locationMeterDTO.getCategory());
        locationMeterEntity.setCallMode(locationMeterDTO.getCallMode());
        locationMeterEntity.setCallAddress(locationMeterDTO.getCallAddress());
        locationMeterEntity.setElecMode(locationMeterDTO.getElecMode());
        locationMeterEntity.setCtRange(locationMeterDTO.getCtRange());
        locationMeterEntity.setCtRatio(locationMeterDTO.getCtRatio());
        locationMeterEntity.setCallPort(locationMeterDTO.getCallPort());

        boolean add = locationMeterRepository.add(locationMeterEntity);
        if (!add) {
            throw new MessageCodeException(PileBaseEnum.FAILED_TO_ADD_METER);
        }
        return id;
    }

    /**
     * @param locationMeterDTO
     * @return
     * @function 修改电表
     */
    @Override
    @Transactional
    public Boolean update(LocationMeterDTO locationMeterDTO) {

        log.info("===>>>LocationMeterServiceImpl.update locationMeterDTO : {}",
                JSON.toJSONString(locationMeterDTO));

        this.checkLocationMeterDTO(locationMeterDTO);

        LocationMeterEntity locationMeterEntity = locationMeterRepository.getById(locationMeterDTO.getId());

        Long currentTimeMillis = System.currentTimeMillis();

        locationMeterEntity.setName(locationMeterDTO.getName());
//        locationMeterEntity.setBrandEnum(locationMeterDTO.getBrandEnum());
        locationMeterEntity.setUpdatedAt(currentTimeMillis);
        locationMeterEntity.setPin(locationMeterDTO.getPIN());
//        locationMeterEntity.setSn(locationMeterDTO.getSN());

        //边缘云需求  新增字段，限制品牌、型号、电表SN的修改
//        locationMeterEntity.setCategory(locationMeterDTO.getCategory());
        locationMeterEntity.setCallMode(locationMeterDTO.getCallMode());
        locationMeterEntity.setCallAddress(locationMeterDTO.getCallAddress());
        locationMeterEntity.setCallPort(locationMeterDTO.getCallPort());
        locationMeterEntity.setElecMode(locationMeterDTO.getElecMode());
        locationMeterEntity.setCtRange(locationMeterDTO.getCtRange());
        locationMeterEntity.setCtRatio(locationMeterDTO.getCtRatio());

        boolean update = locationMeterRepository.updateById(locationMeterEntity);
        String sn = locationMeterEntity.getSn();
        if (StringUtils.isNotEmpty(sn)) {
            String key = "energy:pile:base:meter:relation:" + sn;
            this.stringRedisTemplate.opsForValue().set(key, JSON.toJSONString(locationMeterEntity), 24L, TimeUnit.HOURS);
        }

        if (!update) {
            throw new MessageCodeException(PileBaseEnum.FAILED_TO_MODIFY_METER_INFORMATION);
        }
        return true;
    }

    /**
     * @param id
     * @return
     * @function 删除电表
     */
    @Override
    @Transactional
    public Boolean delete(Long id) {

        log.info("===>>>LocationMeterServiceImpl.delete id : {}",
                JSON.toJSONString(id));

        Result<List<MeterBindVO>> listResult = edgeFeignClient.queryMeterLinkedEdgeGateway(Arrays.asList(id));
        log.info("queryMeterLinkedEdgeGateway_resp:{}",listResult);
        if(!CollectionUtils.isEmpty(listResult.getData()) && listResult.getData().get(0).getLinekEd()){//关联了边缘网关，不允许删除
            throw new MessageCodeException(PileBaseEnum.LINKED_TO_EDGE_GATEWAY);
        }

        LocationMeterDTO locationMeterDTO = new LocationMeterDTO();
        locationMeterDTO.setId(id);
//        this.checkLocationMeterDTO(locationMeterDTO);

        List<Long> meterIdList = new ArrayList<>();
        meterIdList.add(id);
        Map<Long, List<OpLocationPileGroupEntity>> meterIdAndOpLocationPileGroupEntityListMap = opLocationPileGroupService.getMeterIdAndOpLocationPileGroupEntityListMap(meterIdList);
        if (ObjectUtils.isNotEmpty(meterIdAndOpLocationPileGroupEntityListMap)) {
            throw new MessageCodeException(PileBaseEnum.LINKED_TO_THE_SMART_CHARGING_PACK);
        }

        boolean reg = locationMeterRepository.logicDelete(id);
        if (!reg) {
            throw new MessageCodeException(PileBaseEnum.FAILED_TO_DELETE);
        }
        return true;
    }

    /**
     * @return
     * @function 查询商家可以给智能充电群组绑定的电表列表(过滤掉已经被绑定过的)
     */
    @Override
    public List<LocationMeterVO> getList(MeterQueryDTO meterQueryDTO) {

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        if (jwtInfo == null
                || jwtInfo.getPayload() == null
                || jwtInfo.getPayload().getSellerId() == null) {
            return Collections.emptyList();
        }

        Long sellerId = jwtInfo.getPayload().getSellerId();
        List<LocationMeterEntity> locationMeterEntityList = locationMeterRepository.getList(sellerId, null, meterQueryDTO.getSearchValue());
        if (ObjectUtils.isEmpty(locationMeterEntityList)) {
            return Collections.emptyList();
        }

        List<Long> meterIdList = locationMeterEntityList
                .stream()
                .map(LocationMeterEntity::getId)
                .collect(Collectors.toList());
        Map<Long, List<OpLocationPileGroupEntity>> meterIdAndOpLocationPileGroupEntityListMap = opLocationPileGroupService.getMeterIdAndOpLocationPileGroupEntityListMap(meterIdList);

        List<LocationMeterVO> locationMeterVOList = new ArrayList<>();
        if (ObjectUtils.isEmpty(meterIdAndOpLocationPileGroupEntityListMap)) {
            Map<Long, Integer> meterIdAndMeterConnectionStatusMap = this.getMeterConnectionStatus(locationMeterEntityList);
            this.buildLocationMeterVOList(locationMeterVOList, locationMeterEntityList, meterIdAndMeterConnectionStatusMap);
        } else {
            Set<Long> meterIdSet = meterIdAndOpLocationPileGroupEntityListMap.keySet();
            if (ObjectUtils.isNotEmpty(meterQueryDTO.getGroupIdList())) {
                List<OpLocationPileGroupEntity> opLocationPileGroupEntityList = opLocationPileGroupService.findAllByIdIn(meterQueryDTO.getGroupIdList());
                if (ObjectUtils.isNotEmpty(opLocationPileGroupEntityList)) {
                    Set<Long> needRemovedMeterIdSet = opLocationPileGroupEntityList
                            .stream()
                            .filter(var -> var.getMeterId() != null)
                            .map(OpLocationPileGroupEntity::getMeterId)
                            .collect(Collectors.toSet());
                    if (ObjectUtils.isNotEmpty(needRemovedMeterIdSet)) {
                        meterIdSet.removeAll(needRemovedMeterIdSet);
                    }
                }
            }

            List<LocationMeterEntity> entityList = locationMeterRepository.getList(sellerId, new ArrayList<>(meterIdSet), null);
            if (ObjectUtils.isNotEmpty(entityList)) {
                Map<Long, Integer> meterIdAndMeterConnectionStatusMap = this.getMeterConnectionStatus(entityList);
               this.buildLocationMeterVOList(locationMeterVOList, entityList, meterIdAndMeterConnectionStatusMap);
            }
        }
        return locationMeterVOList;
    }

    private void buildLocationMeterVOList(List<LocationMeterVO> locationMeterVOList, List<LocationMeterEntity> locationMeterEntityList, Map<Long, Integer> meterIdAndMeterConnectionStatusMap) {
        locationMeterEntityList
                .stream()
                .filter(Objects::nonNull)
                .forEach(var -> {
            LocationMeterVO locationMeterVO = new LocationMeterVO();
            locationMeterVO.setId(var.getId());
            locationMeterVO.setName(var.getName());
            locationMeterVO.setBrandEnum(var.getBrandEnum());
            locationMeterVO.setSn(var.getSn());
            locationMeterVO.setConnectionStatus(meterIdAndMeterConnectionStatusMap.get(var.getId()));
            locationMeterVOList.add(locationMeterVO);
        });
    }

    /**
     * @param id
     * @return
     * @function 查询电表信息详情
     */
    @Override
    public LocationMeterVO detail(Long id) {

        log.info("===>>>LocationMeterServiceImpl.detail id : {}",
                JSON.toJSONString(id));

        LocationMeterEntity entity = locationMeterRepository.getById(id);
        if (entity == null) {
            return null;
        }
        LocationMeterVO vo = new LocationMeterVO();
        BeanUtils.copyProperties(entity, vo);
        return vo;
    }

    /**
     * @param pageQueryDTO
     * @return
     * @function 分页查询商家下的电表数据
     */
    @Override
    public Page<LocationMeterVO> queryPages(PageQueryDTO pageQueryDTO) {

        log.info("===>>>LocationMeterServiceImpl.queryPages pageQueryDTO : {}",
                JSON.toJSONString(pageQueryDTO));

        final Integer page = pageQueryDTO.getPage();
        final Integer pageSize = pageQueryDTO.getPageSize();

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();

        if (jwtInfo == null
                || jwtInfo.getPayload() == null
                || jwtInfo.getPayload().getSellerId() == null) {
            return new Page<>(page, pageSize);
        }
        pageQueryDTO.setOperatorId(jwtInfo.getPayload().getSellerId());
        List<LocationMeterEntity> locationMeterEntityList = locationMeterRepository.queryPages(pageQueryDTO);
        if (ObjectUtils.isEmpty(locationMeterEntityList)) {
            return new Page<>(page, pageSize);
        }

        Map<Long, Integer> meterIdAndMeterConnectionStatusMap = this.getMeterConnectionStatus(locationMeterEntityList);

        List<LocationMeterEntity> finalResultSet = new ArrayList<>();
        if (pageQueryDTO.getConnectionStatus() != null) {
            locationMeterEntityList.forEach(var -> {
                if (pageQueryDTO.getConnectionStatus().equals(meterIdAndMeterConnectionStatusMap.get(var.getId()))) {
                    finalResultSet.add(var);
                }
            });
        } else {
            locationMeterEntityList.forEach(var -> finalResultSet.add(var));
        }

        if (ObjectUtils.isEmpty(finalResultSet)
                || finalResultSet.size() <= (page - 1) * pageSize) {
            return new Page<>(page, pageSize);
        }

        List<LocationMeterEntity> currentPageRecordList = ListUtils.partition(finalResultSet, pageSize).get(page - 1);
        List<Long> currIdList = currentPageRecordList
                .stream()
                .map(LocationMeterEntity::getId)
                .collect(Collectors.toList());
        Map<Long, List<OpLocationPileGroupEntity>> meterIdAndOpLocationPileGroupEntityListMap = opLocationPileGroupService.getMeterIdAndOpLocationPileGroupEntityListMap(currIdList);

        Result<List<MeterBindVO>> listResult = edgeFeignClient.queryMeterLinkedEdgeGateway(currIdList);
        Map<Long,MeterBindVO> linkedMap=new HashMap<>();
        if(!CollectionUtils.isEmpty(listResult.getData())){
            listResult.getData().forEach(e->linkedMap.put(e.getMeterId(),e));
        }

        List<LocationMeterVO> locationMeterVOList = new ArrayList<>();
        currentPageRecordList.forEach(var -> {
            LocationMeterVO locationMeterVO = new LocationMeterVO();
            BeanUtils.copyProperties(var, locationMeterVO);
            locationMeterVO.setConnectionStatus(meterIdAndMeterConnectionStatusMap.get(var.getId()));
            if (ObjectUtils.isNotEmpty(meterIdAndOpLocationPileGroupEntityListMap)
                    && ObjectUtils.isNotEmpty(meterIdAndOpLocationPileGroupEntityListMap.get(var.getId()))) {
                OpLocationPileGroupEntity opLocationPileGroupEntity = meterIdAndOpLocationPileGroupEntityListMap.get(var.getId()).get(0);
                PileGroupBasicInfoVO pileGroupBasicInfoVO = new PileGroupBasicInfoVO();
                pileGroupBasicInfoVO.setId(opLocationPileGroupEntity.getId());
                pileGroupBasicInfoVO.setName(opLocationPileGroupEntity.getName());
                pileGroupBasicInfoVO.setRootId(opLocationPileGroupService.getRootId(opLocationPileGroupEntity.getId()));
                pileGroupBasicInfoVO.setLocationId(opLocationPileGroupEntity.getLocationId());
                locationMeterVO.setPileGroupBasicInfoVO(pileGroupBasicInfoVO);
                locationMeterVO.setDeletedEnable(0);
            } else {
                locationMeterVO.setDeletedEnable(1);
            }
            MeterBindVO meterBindVO = linkedMap.get(var.getId());
            locationMeterVO.setGwLinked(0);
            if (meterBindVO != null) {
                locationMeterVO.setGwLinked(meterBindVO.getLinekEd()?1:0);
                locationMeterVO.setEdgeGateName(meterBindVO.getEdgeGateName());
            }
            locationMeterVOList.add(locationMeterVO);
        });
        Page<LocationMeterVO> locationMeterVOPage = new Page<>();
        locationMeterVOPage
                .setRecords(locationMeterVOList)
                .setTotal(finalResultSet.size())
                .setSize(pageSize)
                .setCurrent(page)
                .setPages(ListUtils.partition(finalResultSet, pageSize).size());
        return locationMeterVOPage;
    }

    @Override
    public List<Long> selectIdListBySellerId(Long sellerId) {
        if (null == sellerId) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<LocationMeterEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(LocationMeterEntity::getId);
        queryWrapper.eq(LocationMeterEntity::getOperatorId, sellerId);
        queryWrapper.eq(LocationMeterEntity::getDeleted, 0);
        List<LocationMeterEntity> meterEntityList = locationMeterRepository.list(queryWrapper);
        if (CollUtil.isNotEmpty(meterEntityList)) {
            return meterEntityList.stream().map(LocationMeterEntity::getId).collect(Collectors.toList());
        }
        return Collections.emptyList();
    }

    @Override
    public String generateDefaultMeterName(Integer brandEnum) {

        log.info("===>>>LocationMeterServiceImpl.generateDefaultMeterName brandEnum : {}",
                brandEnum);

        if (brandEnum == null) {
            return null;
        }

        List<Integer> brandCodeList = Arrays
                .stream(MeterbrandEnum.values())
                .map(MeterbrandEnum::getBrandCode)
                .collect(Collectors.toList());
        if (ObjectUtils.isEmpty(brandCodeList)
                || !brandCodeList.contains(brandEnum)) {
            return null;
        }

        final String defaultMeterNamePrefix = MeterbrandEnum.getBrandNameByBrandCode(brandEnum);
        if (StringUtils.isBlank(defaultMeterNamePrefix)) {
            return null;
        }

        final Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
        LambdaQueryWrapper<LocationMeterEntity> lqw = new LambdaQueryWrapper<>();
        lqw.select(LocationMeterEntity::getId)
                .eq(LocationMeterEntity::getOperatorId, sellerId)
                .eq(LocationMeterEntity::getBrandEnum, brandEnum)
                .eq(LocationMeterEntity::getDeleted, 0);
        List<LocationMeterEntity> locationMeterEntityList = locationMeterRepository.getBaseMapper().selectList(lqw);

        int count = 1;
        if (ObjectUtils.isNotEmpty(locationMeterEntityList)) {
            count = locationMeterEntityList.size() + 1;
        }

        String defaultMeterName = defaultMeterNamePrefix + "_" + (count);
        LocationMeterEntity locationMeterEntity = new LocationMeterEntity();
        locationMeterEntity.setOperatorId(sellerId);

        int maxIterations = 3;
        int iterations = 0;
        do {
            locationMeterEntity.setName(defaultMeterName);
            boolean flag = locationMeterRepository.checkMeterNameUnique(locationMeterEntity);
            if (flag) {
                return defaultMeterName;
            }
            defaultMeterName = defaultMeterNamePrefix + "_" + (++count);
            iterations++;
        } while (iterations < maxIterations);
        return null;
    }

    @Override
    public List<LocationMeterVO> getIds(List<String> sns) {
        if (CollectionUtils.isEmpty(sns)) {
            log.info("getIds,sn is empty.");
            return null;
        }
        List<String> findList = new ArrayList<>();
        List<LocationMeterVO> resultList = new ArrayList<>(sns.size());
        String prefix = "energy:pile:base:meter:relation:%s";
        List<String> notExistList = new ArrayList<>(sns);
        sns.stream().forEach(sn -> {
            String key = String.format(prefix, sn);
            String redisValue = this.stringRedisTemplate.opsForValue().get(key);
            if (StringUtils.isNotEmpty(redisValue)) {
                resultList.add(this.convert.toVo(JSON.parseObject(redisValue, LocationMeterEntity.class)));
                notExistList.remove(sn);
            } else {
                findList.add(sn);
            }
        });
        if (CollectionUtils.isEmpty(findList)) {
            return resultList;
        }
        List<LocationMeterEntity> list = this.findListBySn(findList);
        if (!CollectionUtils.isEmpty(list)) {
            list.stream().forEach(e -> {
                LocationMeterVO vo = this.convert.toVo(e);
                String sn = e.getSn();
                notExistList.remove(sn);
                if (StringUtils.isNotEmpty(sn)) {
                    this.stringRedisTemplate.opsForValue().set(String.format(prefix, sn), JSON.toJSONString(e), 24L, TimeUnit.HOURS);
                }
                resultList.add(vo);
            });
        }
        if (!CollectionUtils.isEmpty(notExistList)) {
            notExistList.stream().forEach(sn -> {
                String existKey = "energy:pile:base:meter:relation:not:exist:" + sn;
                this.stringRedisTemplate.opsForValue().set(existKey, sn, 24L, TimeUnit.HOURS);
            });
        }
        return resultList;
    }

    @Override
    public List<LocationMeterEntity> findListBySn(List<String> sns) {
        return this.locationMeterRepository.findListBySn(sns);
    }

    @Override
    public List<LocationMeterEntity> getByMeterIds(List<Long> meterIds) {
        if (CollectionUtils.isEmpty(meterIds)) {
            return new ArrayList<>();
        }
        return this.locationMeterRepository.getByMeterIds(meterIds);
    }

    /**
     * @param locationMeterDTO
     * @function 校验入参对象
     */
    private void checkLocationMeterDTO(LocationMeterDTO locationMeterDTO) {

        log.info("===>>>LocationMeterServiceImpl.checkLocationMeterDTO locationMeterDTO : {}",
                JSON.toJSONString(locationMeterDTO));

        if (locationMeterDTO.getId() != null) {
            LocationMeterEntity entity = locationMeterRepository.getById(locationMeterDTO.getId());
            if (entity == null) {
                throw new MessageCodeException(PileBaseEnum.DATA_NOT_EXIST);
            }
        }

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        if (StringUtils.isNotBlank(locationMeterDTO.getName())
                && jwtInfo != null
                && jwtInfo.getPayload() != null
                && jwtInfo.getPayload().getSellerId() != null) {
            LocationMeterEntity locationMeterEntity = new LocationMeterEntity();
            locationMeterEntity.setName(locationMeterDTO.getName());
            locationMeterEntity.setOperatorId(jwtInfo.getPayload().getSellerId());
            locationMeterEntity.setId(locationMeterDTO.getId());
            locationMeterEntity.setSn(locationMeterDTO.getSN());
            if (!locationMeterRepository.checkMeterNameUnique(locationMeterEntity)) {
                throw new MessageCodeException(PileBaseEnum.NAME_ALREADY_USED);
            }
            if(locationMeterDTO.getBrandEnum().intValue()==MeterbrandEnum.EASTRON.getBrandCode()
                    || locationMeterDTO.getBrandEnum().intValue()==MeterbrandEnum.AUTEL.getBrandCode()) {
                if (!locationMeterRepository.checkSnUnique(locationMeterEntity)) {
                    throw new MessageCodeException(PileBaseEnum.SN_ALREADY_USED);
                }
            }

        }

        //边缘云需求   autel、东鸿  必填字段校验
        if(locationMeterDTO.getBrandEnum().intValue()==MeterbrandEnum.EASTRON.getBrandCode()
                || locationMeterDTO.getBrandEnum().intValue()==MeterbrandEnum.AUTEL.getBrandCode()){
            if(StringUtils.isAnyBlank(
                    locationMeterDTO.getName(),
                    locationMeterDTO.getCallMode(),
                    locationMeterDTO.getCallAddress(),
                    locationMeterDTO.getCallPort(),
                    locationMeterDTO.getElecMode(),
                    locationMeterDTO.getCtRange(),
                    locationMeterDTO.getCtRatio())){
                throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
            }

            if(locationMeterDTO.getId()==null){
                if(StringUtils.isAnyBlank(locationMeterDTO.getCategory(),locationMeterDTO.getSN())){
                    throw new MessageCodeException(PileBaseEnum.PARAMETER_NOT_ILLEGAL);
                }
            }
        }
    }

    /**
     * @param meterIdList
     * @return
     * @function 批量获取电表的连接状态
     */
    private Map<Long, Integer> getMeterConnectionStatus(List<LocationMeterEntity> locationMeterEntityList) {

        log.info("===>>>LocationMeterServiceImpl.getMeterConnectionStatus meterIdList : {}",
                JSON.toJSONString(locationMeterEntityList));
        //根据品牌区分电表列表
        List<Long> enegicList = locationMeterEntityList
                .stream().filter(var -> MeterbrandEnum.ENEGIC.getBrandCode().equals(var.getBrandEnum())).map(LocationMeterEntity::getId)
                .collect(Collectors.toList());

        List<Long> autelList = locationMeterEntityList
                .stream().filter(var -> MeterbrandEnum.AUTEL.getBrandCode().equals(var.getBrandEnum())).map(LocationMeterEntity::getId)
                .collect(Collectors.toList());

        Set<Long> eastronList = locationMeterEntityList
                .stream().filter(var -> MeterbrandEnum.EASTRON.getBrandCode().equals(var.getBrandEnum())).map(LocationMeterEntity::getId)
                .collect(Collectors.toSet());

        List<Long> meterIdList = locationMeterEntityList
                .stream()
                .map(LocationMeterEntity::getId)
                .collect(Collectors.toList());
        Map<Long, Integer> meterIdAndMeterConnectionStatusMap = new HashMap<>();
        if (ObjectUtils.isNotEmpty(meterIdList)) {
            meterIdList.forEach(var -> meterIdAndMeterConnectionStatusMap.put(var, MeterConnectionStatusEnum.METER_NOT_CONNECTED.getConnectionCode()));
        }

        MeterDataRecordDTO meterDataRecordDTO = new MeterDataRecordDTO();
        Long currentTimeMillis = System.currentTimeMillis();
        meterDataRecordDTO.setTimeDimension(currentTimeMillis - LocationMeterServiceImpl._10_MINUTE);
        meterDataRecordDTO.setMeterIdList(meterIdList);
        Result<List<MeterDataRecordVO>> result = meterDataClient.getMeterDataRecordVOList(meterDataRecordDTO);

        log.info("===>>>LocationMeterServiceImpl.getMeterConnectionStatus result : {}",
                JSON.toJSONString(result));

        if (result != null
                && Integer.valueOf(HttpStatus.HTTP_OK).equals(result.getCode())
                && ObjectUtils.isNotEmpty(result.getData())) {
            result.getData().forEach(var -> {
                String externalId = var.getExternalId();
                if (StringUtils.isBlank(externalId)){
                       return;
                }
                if (currentTimeMillis-var.getUpdateTime()<_15_SECOND&&(autelList.contains(Long.valueOf(externalId))
                        ||eastronList.contains(Long.valueOf(externalId)))) {
                    meterIdAndMeterConnectionStatusMap.put(Long.valueOf(externalId), MeterConnectionStatusEnum.METER_IS_CONNECTED.getConnectionCode());
                }
                if (currentTimeMillis-var.getUpdateTime()<_10_MINUTE&&enegicList.contains(Long.valueOf(externalId))) {
                    meterIdAndMeterConnectionStatusMap.put(Long.valueOf(externalId), MeterConnectionStatusEnum.METER_IS_CONNECTED.getConnectionCode());
                }
            });
        }
        return meterIdAndMeterConnectionStatusMap;
    }


    @Override
    public Page<MeterVO> queryByPage(PageParamDTO paramDTO) {
        Page<LocationMeterEntity> page=new Page<>(paramDTO.getPage(),paramDTO.getPageSize());
        LambdaQueryWrapper<LocationMeterEntity> wp = new LambdaQueryWrapper<>();
        wp.eq(LocationMeterEntity::getOperatorId, paramDTO.getOperatorId())
                .in(!CollectionUtils.isEmpty(paramDTO.getBrandList()), LocationMeterEntity::getBrandEnum, paramDTO.getBrandList())
                .like(StringUtils.isNotBlank(paramDTO.getMeterName()), LocationMeterEntity::getName, StringUtil.escapeChar(paramDTO.getMeterName()))
                .eq(LocationMeterEntity::getDeleted, 0)
                .orderByDesc(LocationMeterEntity::getId);

        locationMeterRepository.page(page,wp);

        Page<MeterVO> ret=new Page<>(paramDTO.getPage(),paramDTO.getPageSize());
        ret.setTotal(page.getTotal());
        ret.setPages(page.getPages());
        List<MeterVO> records = page.getRecords().stream().map(c -> {
            MeterVO v = new MeterVO();
            v.setBrandEnum(c.getBrandEnum());
            v.setCategory(c.getCategory());
            v.setId(c.getId());
            v.setName(c.getName());
            v.setOperatorId(c.getOperatorId());
            v.setSn(c.getSn());
            return v;
        }).collect(Collectors.toList());
        ret.setRecords(records);
        return ret;
    }

    @Override
    public List<MeterVO> queryMetetByMetetIds(MeterIdsQueryDTO paramDTO) {
        LambdaQueryWrapper<LocationMeterEntity> wp = new LambdaQueryWrapper<>();
        wp.eq(LocationMeterEntity::getOperatorId, paramDTO.getOperatorId())
                .in(LocationMeterEntity::getId,paramDTO.getMeterIds())
                .orderByDesc(LocationMeterEntity::getId);
        List<LocationMeterEntity> list = locationMeterRepository.list(wp);

        List<MeterVO> ret = list.stream().map(c -> {
            MeterVO v = new MeterVO();
            v.setBrandEnum(c.getBrandEnum());
            v.setCategory(c.getCategory());
            v.setId(c.getId());
            v.setName(c.getName());
            v.setOperatorId(c.getOperatorId());
            v.setSn(c.getSn());
            v.setCallAddress(c.getCallAddress());
            v.setCallPort(c.getCallPort());
            v.setElecMode(c.getElecMode());
            v.setCallMode(c.getCallMode());
            v.setCtRange(c.getCtRange());
            v.setCtRatio(c.getCtRatio());
            return v;
        }).collect(Collectors.toList());

        return  ret;
    }
}




