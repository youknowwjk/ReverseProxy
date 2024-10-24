package com.autel.cloud.pile.base.domain.context;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.meter.data.api.feign.MeterDataClient;
import com.autel.cloud.meter.data.api.vo.MeterDataRecordVO;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupAssociateRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.DeliveryGroupDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileGroupStrategyDTO;
import com.autel.cloud.pile.base.dto.TimeSettingDetailDTO;
import com.autel.cloud.pile.base.enums.NumberPhasesEnum;
import com.autel.cloud.pile.base.enums.UnitEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.util.DlbUtil;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryListVO;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.autel.cloud.tariff.vo.CostModelRuleEntityVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.http.HttpStatus;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2023/2/24 16:21
 */
@Slf4j
public class PileGroupContext {
    private final OpLocationPileGroupRepository opLocationPileGroupRepository;
    private final OpLocationPileGroupAssociateRepository opLocationPileGroupAssociateRepository;
    private final OpLocationPileEvseService opLocationPileEvseService;
    private final OpLocationEvseService opLocationEvseService;
    private final TariffFeignClient tariffFeignClient;
    private final Long rootId;
    private final OpLocationPileGroupStrategyDTO strategyDto;
    private final String chargingUpUnit;
    private final Integer minReserve;
    private final StringRedisTemplate stringRedisTemplate;
    private final OpLocationPileGroupService opLocationPileGroupService;
    private final MeterDataClient meterDataClient;
    private List<OpLocationPileGroupEntity> childrenEntityList;
    private List<OpLocationPileGroupAssociateEntity> associateEntityList;
    private List<OpLocationPileEvseElasticDTO> pileDtoList;
    private List<String> pileSnList;
    private List<OpLocationEvseElasticDTO> evseDtoList;

    private List<OpLocationPileGroupDeliveryListVO> deliveryListVo;

    private Map<String, BigDecimal> specifyMap = new HashMap<>();

    public String getChargingUpUnit() {
        return chargingUpUnit;
    }

    public void addSpecifyValue(String evseSn, BigDecimal value) {
        specifyMap.put(evseSn, value);
    }

    public BigDecimal getSpecifyValue(String evseSn) {
        return specifyMap.get(evseSn);
    }

    public Integer getMinReserve() {
        return minReserve;
    }

    public PileGroupContext(OpLocationPileGroupRepository opLocationPileGroupRepository,
                            OpLocationPileGroupAssociateRepository opLocationPileGroupAssociateRepository,
                            OpLocationPileEvseService opLocationPileEvseService,
                            OpLocationEvseService opLocationEvseService,
                            TariffFeignClient tariffFeignClient,
                            StringRedisTemplate stringRedisTemplate,
                            OpLocationPileGroupService opLocationPileGroupService,
                            MeterDataClient meterDataClient,
                            OpLocationPileGroupStrategyDTO strategyDto) {
        this.opLocationPileGroupRepository = opLocationPileGroupRepository;
        this.opLocationPileGroupAssociateRepository = opLocationPileGroupAssociateRepository;
        this.opLocationPileEvseService = opLocationPileEvseService;
        this.opLocationEvseService = opLocationEvseService;
        this.tariffFeignClient = tariffFeignClient;
        this.strategyDto = strategyDto;
        this.rootId = strategyDto.getRootId();
        this.chargingUpUnit = strategyDto.getChargingUpUnit();
        this.stringRedisTemplate = stringRedisTemplate;
        this.minReserve = strategyDto.getMinReserve();
        this.opLocationPileGroupService = opLocationPileGroupService;
        this.meterDataClient = meterDataClient;
        this.setDeliveryListVo();
    }

    public List<OpLocationPileGroupEntity> getChildrenEntityList() {
        if (CollectionUtils.isEmpty(childrenEntityList)) {
            childrenEntityList = opLocationPileGroupRepository.findChildren(rootId, 1);
            long now = System.currentTimeMillis();
            OpLocationPileGroupEntity parentEntity = childrenEntityList.get(0);
            Long locationId = parentEntity.getLocationId();
            String zoneId = opLocationPileGroupService.getZoneId(locationId);
            Integer timeSettingMode = parentEntity.getTimeSettingMode();
            if (timeSettingMode == 1 && StringUtils.hasText(parentEntity.getTimeSettingDetail())) {
                this.setChargingUp(now, zoneId,null, parentEntity);
            }
            this.reduceSetChargingUp(childrenEntityList, now, zoneId, parentEntity.getChargingUp(), parentEntity.getId());
        }
        return childrenEntityList;
    }

    private void reduceSetChargingUp(List<OpLocationPileGroupEntity> childrenEntityList, long now, String zoneId, BigDecimal chargingUp, Long parentId) {
        if (!CollectionUtils.isEmpty(childrenEntityList)) {
            for (OpLocationPileGroupEntity entity : childrenEntityList) {
                if (entity.getPid().longValue() == parentId.longValue()) {
                    Integer timeSettingMode = entity.getTimeSettingMode();
                    if (timeSettingMode == 1 && StringUtils.hasText(entity.getTimeSettingDetail())) {
                        setChargingUp(now, zoneId, chargingUp, entity);
                    }
                    this.reduceSetChargingUp(childrenEntityList, now, zoneId, entity.getChargingUp(), entity.getId());
                }
            }
        }
    }

    private void setChargingUp(long now, String zoneId, BigDecimal chargingUp, OpLocationPileGroupEntity entity) {
        String detail = entity.getTimeSettingDetail();
        List<TimeSettingDetailDTO> timeSettingDetailDTOS = JSON.parseArray(detail, TimeSettingDetailDTO.class);
        LocalDateTime localDateTime = LocalDateTime.now(ZoneId.of(zoneId));
        LocalDate localDate = localDateTime.toLocalDate();
        int day = localDateTime.getDayOfWeek().getValue();
        TimeSettingDetailDTO detailDTO = timeSettingDetailDTOS.stream().filter(d -> d.getDays().contains(day)).findFirst().get();
        if (detailDTO != null) {
            List<TimeSettingDetailDTO.DetailDTO> details = detailDTO.getDetails().stream().sorted(Comparator.comparing(TimeSettingDetailDTO.DetailDTO::getStartTime)).collect(Collectors.toList());
            for (TimeSettingDetailDTO.DetailDTO dto : details) {
                long startTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getStartTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (dto.getEndTime().equals("24:00")) {
                    dto.setEndTime("23:59");
                }
                long endTime = LocalDateTime.of(localDate, LocalTime.parse(dto.getEndTime())).atZone(ZoneId.of(zoneId)).toInstant().toEpochMilli();
                if (dto.getEndTime().equals("23:59")) {
                    endTime = endTime + TimeUnit.SECONDS.toMillis(60) - 1;
                }
                if (now >= startTime && now < endTime) {
                    entity.setChargingUp(chargingUp != null ? dto.getChargingUp().min(chargingUp) : dto.getChargingUp());
                    break;
                }
            }
        }
    }

    public Long getRootId() {
        return rootId;
    }

    public List<OpLocationPileGroupAssociateEntity> getAssociateEntityList() {
        if (CollectionUtils.isEmpty(associateEntityList)) {
            List<OpLocationPileGroupEntity> childrenList = this.getChildrenEntityList();
            Set<Long> groupIds = childrenList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
            associateEntityList = opLocationPileGroupAssociateRepository.findList(groupIds);
        }
        return associateEntityList;
    }

    public List<OpLocationPileEvseElasticDTO> getPileDtoList() {
        if (CollectionUtils.isEmpty(pileDtoList)) {
            List<String> pileSnList = this.getPileSnList();
            pileDtoList = opLocationPileEvseService.findList(pileSnList);
        }
        return pileDtoList;
    }

    public List<OpLocationEvseElasticDTO> getEvseDtoList() {
        if (CollectionUtils.isEmpty(evseDtoList)) {
            List<String> pileSnList = this.getPileSnList();
            evseDtoList = opLocationEvseService.findList(pileSnList);
        }
        return evseDtoList;
    }

    public Map<String, OpLocationPileEvseElasticDTO> getPileDtoMap() {
        List<OpLocationPileEvseElasticDTO> tmp = this.getPileDtoList();
        return tmp.stream().collect(Collectors.toMap(OpLocationPileEvseElasticDTO::getPileSn, e -> e, (f, s) -> f));
    }

    public Map<String, List<OpLocationEvseElasticDTO>> getEvseDtoMap() {
        List<OpLocationEvseElasticDTO> tmp = this.getEvseDtoList();
        return tmp.stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
    }

    public List<String> getPileSnList() {
        if (CollectionUtils.isEmpty(pileSnList)) {
            List<OpLocationPileGroupAssociateEntity> tmp = this.getAssociateEntityList();
            pileSnList = tmp.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        }
        return pileSnList;
    }

    public void setDeliveryListVo(){
        List<OpLocationPileGroupDeliveryListVO> tmpList = new ArrayList<>();
        long now = System.currentTimeMillis();
        List<OpLocationPileGroupAssociateEntity> associateEntityList = this.getAssociateEntityList();
        Map<Long, List<OpLocationPileGroupAssociateEntity>> associateEntityMap = associateEntityList.stream().collect(Collectors.groupingBy(OpLocationPileGroupAssociateEntity::getGroupId));
        //查询桩信息
        Map<String, OpLocationPileEvseElasticDTO> pileDtoMap = this.getPileDtoMap();
        //查询枪信息
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = this.getEvseDtoMap();
        //查询计费规则
        Set<Long> ids = this.getEvseDtoList().stream().filter(vo -> vo.getTariffId() != null).map(OpLocationEvseElasticDTO::getTariffId).collect(Collectors.toSet());
        Map<Long, CostModelRuleEntityVO> costMap = null;
        if (!CollectionUtils.isEmpty(ids)) {
            Result<List<CostModelRuleEntityVO>> costRuleList = tariffFeignClient.getCostRuleList(new ArrayList<>(ids));
            if (costRuleList != null && costRuleList.getCode() == HttpStatus.OK.value() && !CollectionUtils.isEmpty(costRuleList.getData())) {
                List<CostModelRuleEntityVO> listData = costRuleList.getData();
                costMap = listData.stream().collect(Collectors.toMap(CostModelRuleEntityVO::getId, e -> e, (f, s) -> f));
                log.info("getDeliveryList,costMap={}",JSON.toJSONString(costMap));
            }
        }
        List<OpLocationPileGroupEntity> childrenEntityList = this.getChildrenEntityList();
        OpLocationPileGroupEntity rootEntity = childrenEntityList.get(0);
        Map<Long, CostModelRuleEntityVO> finalCostMap = costMap;
        childrenEntityList.stream().forEach(entity -> {
            OpLocationPileGroupDeliveryListVO vo = new OpLocationPileGroupDeliveryListVO();
            BeanUtils.copyProperties(entity, vo);
            vo.setUpdateType(strategyDto.getUpdateType());
            //ALM电表适配
            Long meterId = entity.getMeterId();
            BigDecimal chargingUp = entity.getChargingUp();
            DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
            groupDTO.setChargingUpUnit(chargingUpUnit);
            groupDTO.setMinReserve(entity.getMinReserve());
            groupDTO.setLoadType(entity.getLoadType());
            groupDTO.setPowerEquipmentEnabled(entity.getPowerEquipmentEnabled());
            BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
            Map<String, BigDecimal> almDeliveryMap = null;
            if (entity.getLoadType() == 1 && meterId != null) {
                MeterDataRecordVO last = meterDataClient.getLast(meterId.toString()).getData();
                if (last != null) {
                    Long updateTime = last.getUpdateTime();
                    MeterDataRecordVO.Meter meter = last.getMeters().get(0);
                    //电表离线
                    if (!"online".equals(last.getMeterStatus()) || (now - updateTime.longValue()) >= DlbUtil.getMeterOfflineOffset() || !meter.getOnline()) {
                        almDeliveryMap = this.getAlmEvseSn(entity.getId(), minValue);
                        log.info("almStart,meter is offline meterId={}", meterId);
                    } else {
                        BigDecimal meterValue = opLocationPileGroupService.getByMeterId(meterId).max(BigDecimal.ZERO);
                        BigDecimal other;
                        String whiteKey = "energy:pile:base:alm:meter:white:future:" + meterId;
                        String white = stringRedisTemplate.opsForValue().get(whiteKey);
                        if (entity.getMeterLocation() == 0 || (StringUtils.hasText(white) && entity.getMeterLocation() == 1)) {
                            other = DlbUtil.getMaxL(meter.getL1(), meter.getL2(), meter.getL3());
                            log.info("almStart,other={},meterLocation={}", other, entity.getMeterLocation());
                        } else {
                            other = DlbUtil.getMaxL(meter.getL1(), meter.getL2(), meter.getL3()).subtract(meterValue);
                        }
                        BigDecimal future = meterDataClient.getFuture(meterId.toString()).getData();
                        log.info("almStart,future={},meterValue={},other={}", future, meterValue, other);
                        BigDecimal calculate;
                        if (other.compareTo(BigDecimal.ZERO) < 0) {
                            other = DlbUtil.getMaxL(meter.getL1(), meter.getL2(), meter.getL3());
                        }
                        other = other.max(future);
                        calculate = chargingUp.subtract(other).min(chargingUp).min(rootEntity.getChargingUp());
                        if (calculate.compareTo(minValue) < 0) {
                            almDeliveryMap = this.getAlmEvseSn(entity.getId(), BigDecimal.ZERO);
                        } else {
                            vo.setChargingUp(calculate);
                        }
                        log.info("almStart,calculate={},almDeliveryMap={}", calculate, JSON.toJSONString(almDeliveryMap));
                    }
                    if (!CollectionUtils.isEmpty(almDeliveryMap)) {
                        almDeliveryMap.forEach((k, v) -> {
                            this.addSpecifyValue(k,v);
                        });
                    }
                }
            }
            vo.setUsableChargingUp(vo.getChargingUp());
            Long id = entity.getId();
            if (!CollectionUtils.isEmpty(associateEntityMap.get(id))) {
                List<OpLocationPileGroupDeliveryListVO.PileInfo> pileInfoList = new ArrayList<>();
                List<OpLocationPileGroupAssociateEntity> associateList = associateEntityMap.get(id);
                associateList.stream().forEach(associateEntity->{
                    OpLocationPileGroupDeliveryListVO.PileInfo pileInfo = new OpLocationPileGroupDeliveryListVO.PileInfo();
                    String pileSn = associateEntity.getPileSn();
                    OpLocationPileEvseElasticDTO pileDto = pileDtoMap.get(pileSn);
                    pileInfo.setPileId(pileDto.getId());
                    pileInfo.setAssociateId(associateEntity.getId());
                    pileInfo.setPileSn(pileSn);
                    pileInfo.setGroupId(associateEntity.getGroupId());
                    //桩的上限=枪上限
                    BigDecimal pileLimit = this.getPileLimit(evseDtoMap.get(pileSn).get(0), this.getChargingUpUnit());
                    pileInfo.setPileLimit(pileLimit);
                    pileInfo.setUsablePileLimit(pileLimit);
                    List<OpLocationPileGroupDeliveryListVO.EvseInfo> evseInfoList = new ArrayList<>();
                    List<OpLocationEvseElasticDTO> evseList = evseDtoMap.get(pileSn);
                    if (!CollectionUtils.isEmpty(evseList)) {
                        evseList.stream().forEach(evse->{
                            OpLocationPileGroupDeliveryListVO.EvseInfo evseInfo = new OpLocationPileGroupDeliveryListVO.EvseInfo();
                            BeanUtils.copyProperties(evse, evseInfo);
                            evseInfo.setPhaseNum(this.getPhaseNum(evseInfo.getPowerType()));
                            Long tariffId = evse.getTariffId();
                            if (tariffId != null && !CollectionUtils.isEmpty(finalCostMap)) {
                                CostModelRuleEntityVO costDetail = finalCostMap.get(tariffId);
                                if (costDetail != null) {
                                    CostModelRuleEntityVO cd = JSON.parseObject(JSON.toJSONString(costDetail), CostModelRuleEntityVO.class);
                                    evseInfo.setCostDetail(cd);
                                }
                            }
                            if (this.getSpecifyValue(evse.getEvseSn()) != null) {
                                evseInfo.setDeliveryValue(this.getSpecifyValue(evse.getEvseSn()));
                            }
                            evseInfoList.add(evseInfo);
                        });
                    }
                    pileInfo.setEvseInfoList(evseInfoList);
                    pileInfoList.add(pileInfo);
                });
                vo.setPileInfoList(pileInfoList);
            }
            tmpList.add(vo);
        });
        this.deliveryListVo = tmpList;
    }

    public List<OpLocationPileGroupDeliveryListVO> getDeliveryList() {
        if (CollectionUtils.isEmpty(deliveryListVo)) {
            this.setDeliveryListVo();
        }
        return deliveryListVo;
    }

    private Map<String, BigDecimal> getAlmEvseSn(Long groupId, BigDecimal delivery) {
        Map<String, BigDecimal> resultMap = new HashMap<>();
        List<OpLocationPileGroupEntity> children = opLocationPileGroupRepository.findChildren(groupId);
        if (!CollectionUtils.isEmpty(children)) {
            Set<Long> ids = children.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet());
            List<OpLocationPileGroupAssociateEntity> entityList = opLocationPileGroupAssociateRepository.findList(ids);
            if (!CollectionUtils.isEmpty(entityList)) {
                List<OpLocationEvseElasticDTO> list = opLocationEvseService.findList(entityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList()));
                list.stream().forEach(dto -> {
                    String evseSn = dto.getEvseSn();
                    resultMap.put(evseSn, delivery);
                });
            }
        }
        return resultMap;
    }

    private String getPhaseNum(String powerType) {
        return NumberPhasesEnum.getPhaseNum(powerType);
    }

    public BigDecimal getPileLimit(OpLocationEvseElasticDTO evseDto, String chargingUpUnit) {
        if (UnitEnum.POWER.getCode().equalsIgnoreCase(chargingUpUnit)) {
            return BigDecimal.valueOf(evseDto.getPower());
        } else {
            return evseDto.getAmperage();
        }
    }

}
