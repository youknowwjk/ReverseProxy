package com.autel.cloud.pile.base.domain.strategy;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.EvseDeviceStatusEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.context.DeliveryContext;
import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;
import com.autel.cloud.pile.base.domain.convert.OncePerRequestConvert;
import com.autel.cloud.pile.base.domain.service.OpLocationPileEvseService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.DeliveryEvseInfoDTO;
import com.autel.cloud.pile.base.dto.DeliveryGroupDTO;
import com.autel.cloud.pile.base.dto.DeliveryPileInfoDTO;
import com.autel.cloud.pile.base.dto.SmartChargeCalculateDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.AiSmartChargingClient;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.util.DlbUtil;
import com.autel.cloud.pile.base.vo.*;
import com.autel.cloud.smart.monitor.dto.EvseMonitorMistakeDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2023/1/30 9:16
 */
@Slf4j
public abstract class AbstractDistributeStrategy implements DistributeStrategy {
    @Resource
    private DeliveryContext deliveryContext;
    @Resource
    private OpLocationService opLocationService;
    @Resource
    private OpLocationPileEvseService opLocationPileEvseService;
    @Resource
    private AiSmartChargingClient aiSmartChargingClient;
    @Resource
    private OncePerRequestConvert oncePerRequestConvert;

    /**
     * 在策略执行之前被调用。
     * 根据不同的能量使用策略，处理充电桩数据。
     * 如果选择了成本最优策略，会通过调用外部算法服务来获取计算结果，然后基于这些结果设置 evseInfo（充电桩的信息），如类型、值等。
     * 如果是成本最优，就调用算法的拿到计算结果，走下发逻辑，就不在本地计算了
     * monitorDtoMap 监控指定值
     * @Date 2023/1/30 9:16
     */
    @Override
    public void handleUseStrategy(OncePerRequestContext context) {
        DeliveryGroupDTO rootDto = context.getRootDto();
        Integer energyUseStrategy = rootDto.getEnergyUseStrategy();
        if (energyUseStrategy != null && energyUseStrategy == 1) {
            Map<String, BigDecimal> designatedMap = context.getDesignatedMap();
            Map<String, EvseMonitorMistakeDTO> monitorDtoMap = context.getMonitorDtoMap();
            List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
            deliveryGroupList.stream().forEach(dto -> {
                List<DeliveryPileInfoDTO> pileInfoList = dto.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileInfo -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseInfo -> {
                        String evseSn = evseInfo.getEvseSn();
                        if (monitorDtoMap.containsKey(evseSn)) {
                            EvseMonitorMistakeDTO mistakeDTO = monitorDtoMap.get(evseSn);
                            evseInfo.setType(mistakeDTO.getType());
                            evseInfo.setDeliveryValue(mistakeDTO.getDeliveryValue());
                            evseInfo.setTimeValue(mistakeDTO.getTimeValue());
                            evseInfo.setLast(mistakeDTO.getLast());
                        } else if (designatedMap.containsKey(evseSn)) {
                            evseInfo.setType(4);
                            evseInfo.setTimeValue(designatedMap.get(evseSn));
                            evseInfo.setLast(true);
                        }
                    });
                });
            });
            this.priceMin(context);
        }
    }

    /**
     * 在策略执行之后被调用。
     *  根据能量使用策略（延迟或立即），设置下发请求的策略类型，并调用 doDelivery 方法执行实际的下发操作。
     * @Date 2023/1/30 9:16
     */
    @Override
    public void handleDelivery(OncePerRequestContext context) {
        //开始下发
        DeliveryGroupDTO rootDto = context.getRootDto();
        Integer energyUseStrategy = rootDto.getEnergyUseStrategy();
        DeliveryRequest request = new DeliveryRequest();
        request.setGroupId(rootDto.getId());
        List<OpLocationPileGroupDeliveryVO> deliveryList = context.getDeliveryList();
        if (!CollectionUtils.isEmpty(deliveryList)) {
            deliveryList.stream().forEach(vo -> {
                vo.setUpdateType(context.getSource());
                vo.setZoneId(context.getZoneId());
            });
        }
        request.setDeliveryVoList(deliveryList);
        if (energyUseStrategy != null && energyUseStrategy == 1) {
            request.setStrategy(BaseConstant.DELAY_STRATEGY);
        } else {
            request.setStrategy(BaseConstant.IMMEDIATELY_STRATEGY);
        }
        this.doDelivery(request);
    }

    private OpLocationPileGroupDeliveryTreeVO getOpLocationPileGroupDeliveryTreeVO(
            List<OpLocationPileGroupEntity> childrenEntityList,
            Map<Long, OpLocationPileGroupDeliveryListVO> deliverListVoMap,
            Long groupId
    ) {
        OpLocationPileGroupDeliveryListVO groupVo = deliverListVoMap.get(groupId);
        OpLocationPileGroupDeliveryTreeVO vo = new OpLocationPileGroupDeliveryTreeVO();
        BeanUtils.copyProperties(groupVo, vo);
        vo.setPileVoList(this.getPileVoList(groupVo.getPileInfoList()));
        vo.setChildrenList(this.generateVoTree(childrenEntityList, groupId, deliverListVoMap));
        BigDecimal total = BigDecimal.ZERO;
        Integer pileNumber = 0;
        //最小分配6A
        if (!CollectionUtils.isEmpty(vo.getPileVoList())) {
            total = total.add(vo.getPileVoList().stream().map(OpLocationPileGroupDeliveryTreeVO.PileInfoVO::getPileLimit).reduce((f, s) -> f.add(s)).get());
            pileNumber += vo.getPileVoList().size();
        }
        if (!CollectionUtils.isEmpty(vo.getChildrenList())) {
            total = total.add(vo.getChildrenList().stream().map(OpLocationPileGroupDeliveryTreeVO::getTotal).reduce((f, s) -> f.add(s)).get());
            pileNumber += vo.getChildrenList().stream().map(OpLocationPileGroupDeliveryTreeVO::getPileNumber).reduce((f, s) -> f + s).get();
        }
        vo.setTotal(total);
        vo.setPileNumber(pileNumber);
        return vo;
    }

    private List<OpLocationPileGroupDeliveryTreeVO.PileInfoVO> getPileVoList(List<OpLocationPileGroupDeliveryListVO.PileInfo> pileInfoList) {
        List<OpLocationPileGroupDeliveryTreeVO.PileInfoVO> resultList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(pileInfoList)) {
            pileInfoList.stream().forEach(entity -> {
                OpLocationPileGroupDeliveryTreeVO.PileInfoVO pileInfoVO = new OpLocationPileGroupDeliveryTreeVO.PileInfoVO();
                BeanUtils.copyProperties(entity, pileInfoVO);
                pileInfoVO.setEvseVoList(
                        entity.getEvseInfoList().stream().map(evseInfo -> {
                            OpLocationPileGroupDeliveryTreeVO.EvseInfoVO evseInfoVO = new OpLocationPileGroupDeliveryTreeVO.EvseInfoVO();
                            BeanUtils.copyProperties(evseInfo, evseInfoVO);
                            return evseInfoVO;
                        }).collect(Collectors.toList())
                );
                resultList.add(pileInfoVO);
            });
        }
        return resultList;
    }

    private List<OpLocationPileGroupDeliveryTreeVO> generateVoTree(
            List<OpLocationPileGroupEntity> childrenEntityList,
            Long parentId,
            Map<Long, OpLocationPileGroupDeliveryListVO> deliverListVoMap
    ) {
        List<OpLocationPileGroupDeliveryTreeVO> resultVoList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(childrenEntityList)) {
            childrenEntityList.stream().forEach(entity -> {
                Long pid = entity.getPid();
                Long groupId = entity.getId();
                if (parentId.longValue() == pid.longValue()) {
                    OpLocationPileGroupDeliveryTreeVO vo = getOpLocationPileGroupDeliveryTreeVO(childrenEntityList, deliverListVoMap, groupId);
                    if (vo != null) {
                        resultVoList.add(vo);
                    }
                }
            });
        }
        return resultVoList;
    }

    protected void priceMin(OncePerRequestContext context) {
        String zoneId = context.getZoneId();
        SmartChargeCalculateDTO paramDTO = new SmartChargeCalculateDTO();
        SmartChargeCalculateDTO.ParamDTO dto = new SmartChargeCalculateDTO.ParamDTO();
        //dto.setPrice(costRuleList);
        dto.setTimezone(zoneId);
        dto.setData(context.getDeliveryGroupList());
        paramDTO.setParams(dto);
        Result<List<OpLocationPileGroupDeliveryVO>> responseData = aiSmartChargingClient.arithmetic(paramDTO);
        log.info("AbstractDistributeStrategy,paramDTO={}", JSON.toJSONString(paramDTO));
        log.info("AbstractDistributeStrategy,调用算法返回结果,responseData={}", JSON.toJSONString(responseData));
        if (responseData != null && Objects.equals(responseData.getCode(), 200) && Objects.nonNull(responseData.getData())) {
            List<OpLocationPileGroupDeliveryVO> deliveryVoList = responseData.getData();
            List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
            //设置上报值
            Map<String, BigDecimal> uploadMap = new HashMap<>();
            deliveryGroupList.stream().forEach(d -> {
                List<DeliveryPileInfoDTO> pileInfoList = d.getPileInfoList();
                if (CollectionUtils.isEmpty(pileInfoList)) {
                    return;
                }
                pileInfoList.stream().forEach(pileDto -> {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileDto.getEvseInfoList();
                    if (CollectionUtils.isEmpty(evseInfoList)) {
                        return;
                    }
                    evseInfoList.stream().forEach(evseDto -> {
                        String evseSn = evseDto.getEvseSn();
                        BigDecimal timeValue = evseDto.getTimeValue();
                        if (EvseDeviceStatusEnum.CHARGING.getName().equals(evseDto.getState())) {
                            uploadMap.put(evseSn, timeValue);
                        }
                    });
                });
            });
            if (!CollectionUtils.isEmpty(deliveryVoList)) {
                deliveryVoList.stream().forEach(vo -> {
                    vo.setZoneId(zoneId);
                    vo.setUpdateType(context.getSource());
                    if (EvseDeviceStatusEnum.CHARGING.getName().equals(vo.getState())) {
                        vo.setUpload(uploadMap.get(vo.getEvseSn()));
                    }
                    vo.setEnergyUseStrategy(1);
                });
            }
            log.info("AbstractDistributeStrategy,计算结果,deliveryVoList={}", JSON.toJSONString(deliveryVoList));
            this.doDelivery(DeliveryRequest.builder().groupId(context.getRootId()).deliveryVoList(deliveryVoList).strategy(BaseConstant.DELAY_STRATEGY).build());
        }
    }

    public String getZoneId(Long locationId) {
        String zoneId;
        zoneId = opLocationService.getZoneId(locationId);
        log.info("getZoneId,zoneId={}", zoneId);
        if (StringUtils.isEmpty(zoneId)) {
            zoneId = BaseConstant.UTC_8.substring(3);
        }
        return zoneId;
    }


    public void doDelivery(DeliveryRequest request) {
        deliveryContext.getStrategy(request.getStrategy()).delivery(request.getGroupId(), request.getDeliveryVoList());
    }

    /**
     * 递归处理离线桩的预留功率。
     * 对于每一层的群组，遍历所有充电桩，判断是否在线（充电中），并根据状态调整可用的充电功率。
     * 如果是离线的桩，则会进行预留功率     * 如果是离线的桩，则会进行预留功率的递减操作。的递减操作。
     */
    public BigDecimal handleOffline(List<OpLocationPileGroupDeliveryVO> resultVoList, OncePerRequestContext context,DeliveryGroupTreeVO treeVO) {
        List<DeliveryPileInfoTreeVO> pileVoList = treeVO.getPileInfoList();
        List<DeliveryPileInfoTreeVO> pileVoListTemp = new ArrayList<>(pileVoList);
        List<DeliveryGroupTreeVO> childrenList = treeVO.getChildrenList();
        BigDecimal usableChargingUp = treeVO.getUsableChargingUp();
        DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
        groupDTO.setChargingUpUnit(treeVO.getChargingUpUnit());
        groupDTO.setMinReserve(treeVO.getMinReserve());
        groupDTO.setLoadType(treeVO.getLoadType());
        groupDTO.setPowerEquipmentEnabled(treeVO.getPowerEquipmentEnabled());
        //拿到最小值，8A或者4.2KW
        BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
        Integer source = context.getSource();
        Map<String, BigDecimal> designatedMap = context.getDesignatedMap();
        Map<String, EvseMonitorMistakeDTO> monitorDtoMap = context.getMonitorDtoMap();
        //每一层充电中的桩数量
        int pileNumber = 0;
        //每一层充电中的枪数量
        int evseNumber = 0;
        if (!CollectionUtils.isEmpty(pileVoList)) {
            int offline = 0;
            for (DeliveryPileInfoTreeVO pileInfoVO : pileVoList) {
                List<DeliveryEvseInfoTreeVO> evseVoList = pileInfoVO.getEvseInfoList();
                List<DeliveryEvseInfoTreeVO> evseVoListTemp = new ArrayList<>(evseVoList);
                BigDecimal usablePileLimit = pileInfoVO.getUsablePileLimit();
                for (DeliveryEvseInfoTreeVO evseInfoVO : evseVoList) {
                    String evseSn = evseInfoVO.getEvseSn();
                    //判断是否是充电中
                    if (!DlbUtil.isCharging(evseInfoVO.getState())) {
                        //第一次加入群组下发默认配置
                        if (DlbUtil.deliveryDefault(source)) {
                            resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, minValue));
                        }
                        evseVoListTemp.remove(evseInfoVO);
                        //单枪预留或者不预留都在此处理
                        if (treeVO.getMinReserve() != 2) {
                            //递归计算预留之后充电上限
                            usableChargingUp = usableChargingUp.subtract(minValue);
                            usablePileLimit = usablePileLimit.subtract(minValue);
                        } else {
                            //群组预留
                            if (offline == 0) {
                                usableChargingUp = usableChargingUp.subtract(minValue);
                                usablePileLimit = usablePileLimit.subtract(minValue);
                            }
                        }
                        offline++;
                    } else {
                        //在线的桩如果有指定值，直接给指定值
                        if (monitorDtoMap.containsKey(evseSn)) {
                            EvseMonitorMistakeDTO md = monitorDtoMap.get(evseSn);
                            BigDecimal tmp = md.getTimeValue().min(usablePileLimit);
                            if (md.getType() == 3) {
                                tmp = tmp.add(tmp.multiply(new BigDecimal("0.08")));
                            }
                            usableChargingUp = usableChargingUp.subtract(tmp);
                            usablePileLimit = usablePileLimit.subtract(tmp);
                            evseVoListTemp.remove(evseInfoVO);
                            resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, tmp));
                        }else if (designatedMap.containsKey(evseSn)){
                            BigDecimal tmp = designatedMap.get(evseSn).min(usablePileLimit);
                            usableChargingUp = usableChargingUp.subtract(tmp);
                            usablePileLimit = usablePileLimit.subtract(tmp);
                            evseVoListTemp.remove(evseInfoVO);
                            resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, tmp));
                        }else {
                            evseNumber +=1;
                        }
                    }
                }
                pileInfoVO.setUsablePileLimit(usablePileLimit);
                pileInfoVO.setEvseInfoList(evseVoListTemp);
                if (pileInfoVO.getEvseInfoList().isEmpty()) {
                    pileVoListTemp.remove(pileInfoVO);
                }else {
                    pileNumber += 1;
                }
            }
        }
        if (!CollectionUtils.isEmpty(childrenList)) {
            for (DeliveryGroupTreeVO child : childrenList) {
                //递归调用
                BigDecimal temp = this.handleOffline(resultVoList, context,child);
                usableChargingUp = usableChargingUp.subtract(child.getChargingUp().subtract(temp));
                pileNumber += child.getPileNumber();
                evseNumber += child.getEvseNumber();
            }
        }
        treeVO.setUsableChargingUp(usableChargingUp);
        treeVO.setPileNumber(pileNumber);
        treeVO.setEvseNumber(evseNumber);
        treeVO.setChargingUpVip(usableChargingUp);
        treeVO.setPileInfoList(pileVoListTemp);
        return treeVO.getUsableChargingUp();
    }

    /**
     * 递归处理 VIP 优先的预留功率。
     * 按照 VIP 优先策略，对可用的充电功率进行分配。
     * 采用加权平均的方法重新计算权重，以分配每个充电桩的可用功率上限。
     * 公式：vip=总电流-枪数*8A，算完之后进行策略计算，设置每台桩的可用功率上限
     */
    public BigDecimal handleVip(List<OpLocationPileGroupDeliveryVO> resultVoList,
                                DeliveryGroupTreeVO treeVO, BigDecimal vipUse, AtomicInteger online) {
        List<DeliveryPileInfoTreeVO> pileVoList = treeVO.getPileInfoList();
        List<DeliveryPileInfoTreeVO> pileVoListTemp = new ArrayList<>(pileVoList);
        List<DeliveryGroupTreeVO> childrenList = treeVO.getChildrenList();
        Integer priority = treeVO.getPriority();
        BigDecimal total = BigDecimal.ZERO;
        Integer pileNumber = 0;
        Integer evseNumber = 0;
        BigDecimal usableChargingUp = treeVO.getUsableChargingUp();
        DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
        groupDTO.setChargingUpUnit(treeVO.getChargingUpUnit());
        groupDTO.setMinReserve(treeVO.getMinReserve());
        groupDTO.setLoadType(treeVO.getLoadType());
        groupDTO.setPowerEquipmentEnabled(treeVO.getPowerEquipmentEnabled());
        BigDecimal minValue = DlbUtil.getMinValue(groupDTO);

        if (!CollectionUtils.isEmpty(childrenList)) {
            for (DeliveryGroupTreeVO child : childrenList) {
                BigDecimal useBefore = child.getUsableChargingUp();
                //递归调用
                vipUse = this.handleVip(resultVoList, child, vipUse, online);
                if (!child.getPileInfoList().isEmpty() || !child.getChildrenList().isEmpty()){
                    total = total.add(child.getTotal());
                    pileNumber += child.getPileNumber();
                    evseNumber += child.getEvseNumber();
                    usableChargingUp = usableChargingUp.subtract(useBefore.subtract(child.getUsableChargingUp()));
                }
            }
        }
        BigDecimal pileTotal = BigDecimal.ZERO;
        if (!CollectionUtils.isEmpty(pileVoList)) {
            //vip=总电流-枪数*8A
            BigDecimal vipUseTmp = vipUse.subtract(minValue.multiply(BigDecimal.valueOf(online.get())));
            int size = pileVoList.stream().filter(v -> !CollectionUtils.isEmpty(v.getEvseInfoList())).collect(Collectors.toList()).size() + evseNumber;
            for (DeliveryPileInfoTreeVO pileInfoVO : pileVoList) {
                List<DeliveryEvseInfoTreeVO> evseVoList = pileInfoVO.getEvseInfoList();
                List<DeliveryEvseInfoTreeVO> evseVoListTemp = new ArrayList<>(evseVoList);
                BigDecimal usablePileLimit = pileInfoVO.getUsablePileLimit();
                //VIP可用的
                BigDecimal usableChargingUpVip = usableChargingUp.subtract(minValue.multiply(BigDecimal.valueOf(size)));
                BigDecimal usablePileLimitVip = usablePileLimit.subtract(minValue.multiply(BigDecimal.valueOf(evseVoList.size())));
                for (DeliveryEvseInfoTreeVO evseInfoVO : evseVoList) {
                    //VIP优先
                    if (priority == 1 && evseInfoVO.getIsVip()) {
                        String state = evseInfoVO.getState();
                        BigDecimal current = minValue;
                        if (vipUseTmp.compareTo(BigDecimal.ZERO) > 0) {
                            BigDecimal tmp = vipCalc(state, vipUseTmp, usablePileLimitVip).min(usableChargingUpVip);
                            current = minValue.add(tmp);
                            vipUseTmp = vipUseTmp.subtract(tmp);
                            usableChargingUpVip = usableChargingUpVip.subtract(tmp);
                            usablePileLimitVip = usablePileLimitVip.subtract(tmp);
                        }
                        //
                        usablePileLimit = usablePileLimit.subtract(current);
                        usableChargingUp = usableChargingUp.subtract(current);
                        vipUse = vipUse.subtract(current);
                        resultVoList.add(this.buildOpLocationPileGroupDeliveryVO(pileInfoVO, evseInfoVO, treeVO, current));
                        evseVoListTemp.remove(evseInfoVO);
                        online.decrementAndGet();
                    }else {
                        evseNumber +=1;
                    }
                }
                pileInfoVO.setUsablePileLimit(usablePileLimit);
                pileInfoVO.setEvseInfoList(evseVoListTemp);
                if (pileInfoVO.getEvseInfoList().isEmpty()) {
                    pileVoListTemp.remove(pileInfoVO);
                } else {
                    pileNumber += 1;
                    pileTotal = pileTotal.add(usablePileLimit);
                }
            }
        }
        //加权平均重新计算权重，计算桩可用上限
        if (treeVO.getAllocationStrategy().equals(BaseConstant.DISTRIBUTE_EVENLY)) {
            if (total.add(pileTotal).compareTo(usableChargingUp) > 0 && !CollectionUtils.isEmpty(pileVoList)) {
                BigDecimal pileTotalTmp = BigDecimal.ZERO;
                for (DeliveryPileInfoTreeVO pileInfoVO : pileVoList) {
                    pileInfoVO.setUsablePileLimit(pileInfoVO.getUsablePileLimit().multiply(usableChargingUp).divide(total.add(pileTotal),1,BigDecimal.ROUND_DOWN));
                    pileTotalTmp = pileTotalTmp.add(pileInfoVO.getUsablePileLimit());
                }
                pileTotal = pileTotalTmp;
            }
        }
        total = total.add(pileTotal);
        treeVO.setUsableChargingUp(usableChargingUp);
        treeVO.setTotal(total);
        treeVO.setUsableTotal(total.min(usableChargingUp));
        treeVO.setPileNumber(pileNumber);
        treeVO.setEvseNumber(evseNumber);
        treeVO.setPileInfoList(pileVoListTemp);
        return vipUse;
    }

    public Long getSellerId(String pileSn) {
        OpLocationPileEvseElasticDTO pileEvseElasticDTO = opLocationPileEvseService.findByPileSn(pileSn);
        //String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
        //String operatorId = stringRedisTemplate.opsForValue().get(snOperatorIdKey);
        if (pileEvseElasticDTO != null) {
            return pileEvseElasticDTO.getOperatorId();
        }
        return null;
    }

    private BigDecimal vipCalc(String state, BigDecimal usable, BigDecimal pileLimit) {
        return DlbUtil.isCharging(state) ? usable.min(pileLimit) : BigDecimal.ZERO;
    }

    public OpLocationPileGroupDeliveryVO buildOpLocationPileGroupDeliveryVO(
            DeliveryPileInfoTreeVO pileInfoVO,
            DeliveryEvseInfoTreeVO evseInfoVO,
            DeliveryGroupTreeVO treeVO,
            BigDecimal current) {
        OpLocationPileGroupDeliveryVO vo = oncePerRequestConvert.toDeliveryVo(treeVO,pileInfoVO,evseInfoVO);
        vo.setChargingUp(current);
        return vo;
    }

    public BigDecimal getCurrent(String state, BigDecimal amperage) {
        Objects.requireNonNull(amperage);
        return DlbUtil.isCharging(state) ? amperage.max(BaseConstant.MIN_CURRENT) : BaseConstant.MIN_CURRENT;
    }


    /**
     *
     * 逻辑步骤解析
     * 初始化变量和处理指定值、监控值
     *
     * 根群组信息: 获取根群组的 minReserve（最小预留）、chargingUp（充电上限）、chargingUpUnit（充电单位）等信息。
     * 指定值和监控值: 从 context 中获取指定值 (designatedMap) 和监控值 (monitorDtoMap) 并对 chargingUp 进行扣减。这些指定值和监控值表示某些桩必须要分配的电流，优先保证这些桩的电流分配。
     * 计算临界值
     *
     * 计算充电桩的数量: 使用 getSize 方法，计算出需要参与分配的充电桩的数量。这个数量基于 minReserve 参数来确定（0表示不预留，1表示单枪预留，2表示群组预留）。
     * 获取最小分配电流: 调用 DlbUtil.getMinValue(groupDTO) 来计算每个充电桩的最小分配电流。
     * 计算剩余可分配电流的限制: 如果 chargingUp 小于计算得到的临界电流（chargingUp < size * minValue），则处理不同的负载类型和功率设备状态，设置电流分配的限制 limit。
     * 临界值判断
     *
     * 如果剩余的 chargingUp 小于计算得到的临界电流，并且 loadType 和 powerEquipmentEnabled 满足条件，则进入具体的电流分配逻辑。
     * 调用 doFiFo 方法，使用 FIFO（先进先出）策略分配电流。
     * 电流分配逻辑
     *
     * 处理每个群组的充电桩: 遍历 deliveryGroupList，计算并处理每个充电桩的电流分配，考虑其指定值、监控值，并基于此计算出最终要下发的电流。
     * VIP 桩的优先处理: 通过 handleVip 方法，优先处理 VIP 桩的电流分配，这些 VIP 桩会优先分配较高的电流。
     * FIFO 策略: 对于普通桩，按照 FIFO 策略分配电流，先到先得，直到用完可分配的电流或者达到限制。
     * 最终结果
     *
     * 返回一个 List<OpLocationPileGroupDeliveryVO>，这个列表包含了所有需要下发的充电桩的电流分配结果，每个 VO 表示一个充电桩的电流分配情况。
     * @return
     */
    public List<OpLocationPileGroupDeliveryVO> getMinDelivery(OncePerRequestContext context) {
        DeliveryGroupDTO rootDto = context.getRootDto();
        Integer minReserve = rootDto.getMinReserve();
        BigDecimal chargingUp = rootDto.getChargingUp();
        String chargingUpUnit = rootDto.getChargingUpUnit();
        Integer loadType = rootDto.getLoadType();
        Boolean powerEquipmentEnabled = rootDto.getPowerEquipmentEnabled();
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        //指定值是否超上限
        Map<String, BigDecimal> designatedMap = context.getDesignatedMap();
        Map<String, EvseMonitorMistakeDTO> monitorDtoMap = context.getMonitorDtoMap();
        if (!CollectionUtils.isEmpty(designatedMap)) {
            for (BigDecimal value : designatedMap.values()) {
                chargingUp = chargingUp.subtract(value);
            }
        }
        if (!CollectionUtils.isEmpty(monitorDtoMap)) {
            for (EvseMonitorMistakeDTO value : monitorDtoMap.values()) {
                chargingUp = chargingUp.subtract(value.getTimeValue());
            }
        }
        //临界值计算
        int size = getSize(minReserve, deliveryGroupList);
        DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
        groupDTO.setChargingUpUnit(chargingUpUnit);
        groupDTO.setMinReserve(1);
        groupDTO.setLoadType(loadType);
        groupDTO.setPowerEquipmentEnabled(powerEquipmentEnabled);
        BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
        int limit = DlbUtil.getLimit(chargingUp, minValue);
        Integer source = context.getSource();
        if (chargingUp.compareTo(BigDecimal.valueOf(size).multiply(minValue)) < 0) {
            if (loadType != null && loadType == 1 && powerEquipmentEnabled != null && powerEquipmentEnabled) {
                Map<String, BigDecimal> pileLimitMap = new HashMap<>();
                List<OpLocationPileGroupDeliveryVO> resultList = new ArrayList<>();
                deliveryGroupList.stream().forEach(d -> {
                    List<DeliveryPileInfoDTO> pileInfoList = d.getPileInfoList();
                    if (!CollectionUtils.isEmpty(pileInfoList)) {
                        pileInfoList.stream().forEach(pileInfo -> pileLimitMap.put(pileInfo.getPileSn(), pileInfo.getPileLimit()));
                    }
                });
                BigDecimal finalMinValue = minValue;
                deliveryGroupList.stream().forEach(g -> {
                    List<DeliveryPileInfoDTO> pileInfoList = g.getPileInfoList();
                    if (CollectionUtils.isEmpty(pileInfoList)) {
                        return;
                    }
                    pileInfoList.stream().forEach(pileInfo -> {
                        List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                        if (CollectionUtils.isEmpty(evseInfoList)) {
                            return;
                        }
                        evseInfoList.stream().forEach(evseInfo -> {
                            OpLocationPileGroupDeliveryVO vo = oncePerRequestConvert.toDeliveryVo(g, pileInfo, evseInfo);
                            vo.setUpdateType(context.getSource());
                            if (!DlbUtil.isCharging(evseInfo.getState())) {
                                //第一次加入群组下发默认配置
                                if (DlbUtil.deliveryDefault(source)) {
                                    vo.setChargingUp(finalMinValue);
                                    resultList.add(vo);
                                    return;
                                }
                            }
                            BigDecimal tmp = finalMinValue.min(pileLimitMap.get(pileInfo.getPileSn()));
                            vo.setChargingUp(tmp);
                            pileLimitMap.computeIfPresent(vo.getPileSn(), (k, v) -> v.subtract(tmp));
                            resultList.add(vo);
                        });
                    });
                });
                return resultList;
            }
            List<OpLocationPileGroupDeliveryVO> resultList = this.doFiFo(context, limit);
            return resultList;
        }
        return null;
    }

    private int getSize(Integer minReserve, List<DeliveryGroupDTO> deliveryGroupList) {
        int size0 = 0;
        int size1 = 0;
        int size2 = 0;
        for (DeliveryGroupDTO dto : deliveryGroupList) {
            List<DeliveryPileInfoDTO> pileInfoList = dto.getPileInfoList();
            int time = 0;
            int s = 0;
            if (!CollectionUtils.isEmpty(pileInfoList)) {
                for (DeliveryPileInfoDTO pileInfo : pileInfoList) {
                    List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                    if (!CollectionUtils.isEmpty(evseInfoList)) {
                        size1 += evseInfoList.size();
                        for (DeliveryEvseInfoDTO evseInfo : evseInfoList) {
                            if (DlbUtil.isCharging(evseInfo.getState())) {
                                size0 += 1;
                                time += 1;
                            }
                            s += 1;
                        }
                    }
                }
            }
            size2 += Math.min(time + 1, s);
        }
        int size = size1;
        if (minReserve == 0) {
            size = size0;
        }
        if (minReserve == 2) {
            size = size2;
        }
        return size;
    }

    /**
     * 1. 初始化与输入检查
     * 方法首先对输入的参数（如充电桩的群组、可分配电流等）进行初始化，确保数据的有效性。
     * 特别需要检查 usable（可用电流）和 pileLimit（充电桩的限制电流），确保其大于 0，才能进入下一步电流分配逻辑。
     * 2. VIP 桩的优先分配
     * 在分配普通桩电流之前，首先处理 VIP 桩。这些桩具有较高优先级，通过 handleVip 方法优先分配较高的电流。
     * VIP 桩的电流分配基于其专有逻辑，系统会为其保留和优先处理足够的电流，这一步旨在提高 VIP 桩的充电速度和效率。
     * 3. FIFO 分配策略
     * 对于普通桩，采用 FIFO（先进先出）的顺序进行处理，先到的桩优先获取电流分配。
     * 每个桩的状态决定了它是否需要电流。如果桩处于充电状态 (state)，则会进入分配流程。
     * 4. 计算可分配电流
     * fifoCalc 方法：
     * 调用 fifoCalc(state, usable, pileLimit) 来计算当前桩可分配的电流。其逻辑是基于充电状态和限制条件来分配最小的可用电流。
     * 如果桩处于充电状态 (DlbUtil.isCharging(state) 返回 true)，那么可分配电流的值为 usable.min(pileLimit)，即取当前剩余可用电流和桩的限制电流中的较小值。
     * 如果桩不需要充电，返回的电流值为 BigDecimal.ZERO，表示不分配电流。
     * 5. 更新限制和电流值
     * 在分配电流之后，系统会更新 groupLimitMap（群组电流限制）和 pileLimitMap（单个桩的电流限制），并将已经分配的电流从这些限制中扣减。
     * 这一步确保每个群组和每个充电桩的电流分配不会超过其最大可承受的限制，防止过载。
     * 6. 剩余可用电流处理
     * 在每次分配之后，系统会动态减少 usable（剩余可分配电流）的值，确保在电流全部分配完毕后停止分配。
     * 如果 usable 减少到 0 或小于 0，则停止后续桩的电流分配，表示当前所有电流已经完全用完。
     * 7. 循环处理所有桩
     * 系统会通过遍历所有充电桩，依次按照 FIFO 策略进行电流分配，直到所有桩的需求都被满足或剩余的可用电流耗尽。
     * 每个桩在被处理时，都会基于其状态、电流需求和限制条件进行计算，确保每个桩能够公平、合理地获得电流资源。
     * 8. 返回结果
     * 最后，doFiFo 方法会返回分配后的充电桩状态信息，包含每个桩的分配电流。
     * 结果集会记录每个桩的 chargingUp 值（实际分配的电流），供后续的操作或监控系统使用。
     * 核心逻辑总结
     * doFiFo 方法的核心就是基于 FIFO 策略，通过动态计算和条件判断，在电流资源有限的情况下，确保 VIP 桩优先获取电流，并公平地分配剩余电流给普通充电桩。
     */
    public List<OpLocationPileGroupDeliveryVO> doFiFo(OncePerRequestContext context,Integer limit) {
        log.info("doFiFo,limit={}",limit);
        DeliveryGroupDTO rootDto = context.getRootDto();
        List<DeliveryGroupDTO> deliveryGroupList = context.getDeliveryGroupList();
        List<OpLocationPileGroupDeliveryVO> deliveryVOList = new ArrayList<>();
        List<OpLocationPileGroupDeliveryVO> offlineList = new ArrayList<>();
        List<OpLocationPileGroupDeliveryVO> onlineList = new ArrayList<>();
        //可分配的电流等于根群组上限
        BigDecimal usable = rootDto.getChargingUp();
        Integer minReserve = rootDto.getMinReserve();
        DeliveryGroupDTO groupDTO = new DeliveryGroupDTO();
        groupDTO.setChargingUpUnit(rootDto.getChargingUpUnit());
        groupDTO.setMinReserve(rootDto.getMinReserve());
        groupDTO.setLoadType(rootDto.getLoadType());
        groupDTO.setPowerEquipmentEnabled(rootDto.getPowerEquipmentEnabled());
        BigDecimal minValue = DlbUtil.getMinValue(groupDTO);
        Map<Long, BigDecimal> groupLimitMap = deliveryGroupList.stream().collect(Collectors.toMap(DeliveryGroupDTO::getId,DeliveryGroupDTO::getChargingUp));
        Map<String, BigDecimal> pileLimitMap = new HashMap<>();
        deliveryGroupList.stream().forEach(d -> {
            List<DeliveryPileInfoDTO> pileInfoList = d.getPileInfoList();
            if (!CollectionUtils.isEmpty(pileInfoList)) {
                pileInfoList.stream().forEach(pileInfo -> pileLimitMap.put(pileInfo.getPileSn(), pileInfo.getPileLimit()));
            }
        });
        Map<Long,Integer> firstMap = new HashMap<>();
        BigDecimal allDelivery = BigDecimal.ZERO;
        Map<String, EvseMonitorMistakeDTO> monitorDtoMap = context.getMonitorDtoMap();
        Map<String, BigDecimal> designatedMap = context.getDesignatedMap();
        //计算分配电流并过滤离线桩
        for (DeliveryGroupDTO dto : deliveryGroupList) {
            List<DeliveryPileInfoDTO> pileInfoList = dto.getPileInfoList();
            if (CollectionUtils.isEmpty(pileInfoList)) {
                continue;
            }
            for (DeliveryPileInfoDTO pileInfo : pileInfoList) {
                String pileSn = pileInfo.getPileSn();
                BigDecimal pileLimit = pileLimitMap.get(pileSn);
                List<DeliveryEvseInfoDTO> evseInfoList = pileInfo.getEvseInfoList();
                if (CollectionUtils.isEmpty(evseInfoList)) {
                    continue;
                }
                for (DeliveryEvseInfoDTO evseInfo : evseInfoList) {
                    String evseSn = evseInfo.getEvseSn();
                    OpLocationPileGroupDeliveryVO vo = oncePerRequestConvert.toDeliveryVo(dto,pileInfo,evseInfo);
                    vo.setUpdateType(context.getSource());
                    BigDecimal tmp = minValue;
                    boolean putGroup = true;
                    boolean putPile = true;
                    //桩离线直接下发最小6A
                    if (!DlbUtil.isCharging(evseInfo.getState())) {
                        vo.setChargingUp(tmp);
                        //第一次加入群组下发默认配置
                        /*if (DlbUtil.deliveryDefault(context.getSource()) || (limit != null)) {
                        offlineList.add(vo);
                        }*/
                        if (minReserve == 2 && limit == null) {
                            if (!firstMap.containsKey(dto.getId())) {
                                firstMap.put(dto.getId(),1);
                            }else {
                                putGroup = false;
                            }
                            putPile = false;
                        }
                    } else {
                        //按指定值下发
                        if (monitorDtoMap.containsKey(evseSn)) {
                            EvseMonitorMistakeDTO md = monitorDtoMap.get(evseSn);
                            tmp = md.getTimeValue().min(pileLimit);
                            BigDecimal t = tmp;
                            pileLimitMap.computeIfPresent(vo.getPileSn(), (k, v) -> v.subtract(t));
                            if (md.getType() == 3) {
                                tmp = tmp.add(tmp.multiply(new BigDecimal("0.08")));
                            }
                            allDelivery = allDelivery.add(tmp);
                            if (limit != null) {
                                limit -= 1;
                            }
                            vo.setChargingUp(tmp);
                            deliveryVOList.add(vo);
                        }else if (designatedMap.containsKey(evseSn)){
                            tmp = designatedMap.get(evseSn).min(pileLimit);
                            BigDecimal t = tmp;
                            pileLimitMap.computeIfPresent(vo.getPileSn(), (k, v) -> v.subtract(t));
                            allDelivery = allDelivery.add(tmp);
                            if (limit != null) {
                                limit -= 1;
                            }
                            vo.setChargingUp(tmp);
                            deliveryVOList.add(vo);
                        }else {
                            onlineList.add(vo);
                            if (limit != null){
                                putGroup = false;
                                putPile = false;
                            }
                        }
                    }
                    BigDecimal finalTmp = tmp;
                    if (putGroup) {
                        groupLimitMap.computeIfPresent(dto.getId(), (k, v) -> {
                            BigDecimal n = v.subtract(finalTmp);
                            if (n.compareTo(BigDecimal.ZERO) > 0) {
                                return n;
                            }else {
                                return v;
                            }
                        });
                    }
                    if (putPile) {
                        pileLimitMap.computeIfPresent(pileSn, (k, v) -> v.subtract(finalTmp));
                    }
                }
            }
        }
        if (!offlineList.isEmpty()) {
            deliveryVOList.addAll(offlineList);
        }
        //是否预留单枪
        int count = offlineList.size() + onlineList.size();
        if (firstMap.size() > 0 && minReserve == 2) {
            count = firstMap.size() + onlineList.size();
        }
        //不预留不足以分配时，优先最小值分配
        if (minReserve == 0 && limit != null) {
            count = onlineList.size();
            DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
            tmpDto.setChargingUpUnit(rootDto.getChargingUpUnit());
            tmpDto.setMinReserve(1);
            tmpDto.setLoadType(rootDto.getLoadType());
            tmpDto.setPowerEquipmentEnabled(rootDto.getPowerEquipmentEnabled());
            minValue = DlbUtil.getMinValue(tmpDto);
        }
        //不足以按最小值分配
        if (limit != null && limit > 0) {
            if (minReserve == 1) {
                count = onlineList.size();
                DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                tmpDto.setChargingUpUnit(rootDto.getChargingUpUnit());
                tmpDto.setMinReserve(0);
                tmpDto.setLoadType(rootDto.getLoadType());
                tmpDto.setPowerEquipmentEnabled(rootDto.getPowerEquipmentEnabled());
                deliveryVOList.stream().forEach(e -> e.setChargingUp(DlbUtil.getMinValue(tmpDto)));
            }
            count = Math.min(count, limit);
        }
        //群组可分配电流
        usable = usable.subtract(allDelivery).subtract(minValue.multiply(BigDecimal.valueOf(count)));
        //过滤VIP优先
        if (!onlineList.isEmpty()) {
            List<OpLocationPileGroupDeliveryVO> tempList = new ArrayList<>();
            for (OpLocationPileGroupDeliveryVO vo : onlineList) {
                if (vo.getPriority() == 1 && vo.getIsVip()) {
                    Long entityId = vo.getGroupId();
                    String state = vo.getState();
                    BigDecimal current = minValue;
                    //当前群组上限
                    BigDecimal groupLimit = groupLimitMap.get(entityId);
                    BigDecimal pileLimit = pileLimitMap.get(vo.getPileSn());
                    if (usable.compareTo(BigDecimal.ZERO) > 0) {
                        if (groupLimit.compareTo(BigDecimal.ZERO) > 0 && pileLimit.compareTo(BigDecimal.ZERO) > 0) {
                            BigDecimal tmp = fifoCalc(state, usable, pileLimit).min(groupLimit);
                            current = tmp.add(minValue);
                            usable = usable.subtract(tmp);
                            groupLimitMap.computeIfPresent(entityId, (k, v) -> v.subtract(tmp));
                            pileLimitMap.computeIfPresent(vo.getPileSn(), (k, v) -> v.subtract(tmp));
                        }
                    }
                    //不足以分配按0A下发
                    if (limit != null && limit <= 0) {
                        DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                        tmpDto.setChargingUpUnit(rootDto.getChargingUpUnit());
                        tmpDto.setMinReserve(0);
                        tmpDto.setLoadType(rootDto.getLoadType());
                        tmpDto.setPowerEquipmentEnabled(rootDto.getPowerEquipmentEnabled());
                        current = DlbUtil.getMinValue(tmpDto);
                    }
                    vo.setChargingUp(current);
                    deliveryVOList.add(vo);
                    if (limit != null) {
                        limit -=1;
                    }
                } else {
                    tempList.add(vo);
                }
            }
            //按先进先出分配电流
            if (!tempList.isEmpty()) {
                tempList = tempList.stream().sorted(Comparator.comparing(OpLocationPileGroupDeliveryVO::getAttachTime)).collect(Collectors.toList());
                for (OpLocationPileGroupDeliveryVO deliveryVo : tempList) {
                    Long entityId = deliveryVo.getGroupId();
                    String state = deliveryVo.getState();
                    BigDecimal current = minValue;
                    //当前群组上限
                    BigDecimal groupLimit = groupLimitMap.get(entityId);
                    BigDecimal pileLimit = pileLimitMap.get(deliveryVo.getPileSn());
                    if (usable.compareTo(BigDecimal.ZERO) > 0) {
                        if (groupLimit.compareTo(BigDecimal.ZERO) > 0 && pileLimit.compareTo(BigDecimal.ZERO) > 0) {
                            BigDecimal tmp = fifoCalc(state, usable, pileLimit).min(groupLimit);
                            current = tmp.add(minValue);
                            usable = usable.subtract(tmp);
                            groupLimitMap.computeIfPresent(entityId, (k, v) -> v.subtract(tmp));
                            pileLimitMap.computeIfPresent(deliveryVo.getPileSn(), (k, v) -> v.subtract(tmp));
                        }
                    }
                    if (limit != null && limit <= 0) {
                        DeliveryGroupDTO tmpDto = new DeliveryGroupDTO();
                        tmpDto.setChargingUpUnit(rootDto.getChargingUpUnit());
                        tmpDto.setMinReserve(0);
                        tmpDto.setLoadType(rootDto.getLoadType());
                        tmpDto.setPowerEquipmentEnabled(rootDto.getPowerEquipmentEnabled());
                        current = DlbUtil.getMinValue(tmpDto);
                    }
                    deliveryVo.setChargingUp(current);
                    if (limit != null) {
                        limit -=1;
                    }
                }
                deliveryVOList.addAll(tempList);
            }
        }
        return deliveryVOList;
    }


    protected BigDecimal fifoCalc(String state, BigDecimal usable, BigDecimal pileLimit) {
        return DlbUtil.isCharging(state) ? usable.min(pileLimit) : BigDecimal.ZERO;
    }
}
