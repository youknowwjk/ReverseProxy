package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.excel.util.StringUtils;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.autel.cloud.ocpi.vo.location.EVSEVO;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileEvseConvert;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.OcpiLocationService;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.dto.oicp.ActionType;
import com.autel.cloud.pile.base.dto.oicp.EroamingEvseData;
import com.autel.cloud.pile.base.dto.oicp.TariffEvse;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.impl.OicpFeignClientProxy;
import com.autel.cloud.pile.base.infrastructure.feign.ocpi.OcpiClient;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileEvseMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.EroamingPileVO;
import com.autel.cloud.pile.base.vo.OcpiEvseVO;
import com.autel.cloud.pile.base.vo.OcpiLocationVO;
import com.autel.cloud.pile.base.vo.OpPileEvseInfoVO;
import com.autel.cloud.pile.bill.feign.IBillFeignClient;
import com.autel.cloud.pile.bill.vo.RoamingRulesVO;
import com.autel.cloud.tariff.feign.OcpiTariffFeignClient;
import com.autel.cloud.tariff.feign.TariffFeignClient;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * Author:   A19011
 * Description: PileBaseAsync
 * Date:     2023/3/20 10:00
 *
 * @Version 0.0.1-SNAPSHOT
 */
@Slf4j
@Service
public class PileBaseAsync {
    @Autowired
    private OpLocationPileEvseMapper opLocationPileEvseMapper;
    @Autowired
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Value("${ocpp.ocpi.enabled:false}")
    private Boolean ocpiEnabled;
    @Autowired
    private OcpiLocationService ocpiLocationService;
    @Autowired
    private OcpiClient ocpiClient;
    @Autowired
    private OcpiTariffFeignClient ocpiTariffFeignClient;
    @Autowired
    private TariffFeignClient tariffFeignClient;
    @Value("${hubject.enable:false}")
    private String hubjectEnable;
    @Resource
    private OicpFeignClientProxy oicpFeignClient;
    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;
    @Resource
    private OpLocationRepository opLocationRepository;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;
    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private IBillFeignClient billFeignClient;

    @Async("pileBaseTaskExecutor")
    public void pileEromingAsync(String pileSn, int actionType) {
        try {
            if (ocpiEnabled) {
                log.info("pileEromingAsync OCPI");
                OpLocationPileEvseDTO opLocationPileEvseDTO = opLocationPileEvseRepository.queryByPileSn(pileSn);
                log.info("pileEromingAsync opLocationPileEvseDTO :{}", JSON.toJSONString(opLocationPileEvseDTO));
                if (opLocationPileEvseDTO != null && StringUtils.isNotBlank(opLocationPileEvseDTO.getEvseList())) {
                    List<String> evseList = JSONArray.parseArray(opLocationPileEvseDTO.getEvseList(), String.class);
                    log.info("pileEromingAsync evseList :{}", JSON.toJSONString(evseList));
                    if (CollectionUtils.isEmpty(evseList)) {
                        return;
                    }

                    String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
                    String operationId = stringRedisTemplate.opsForValue().get(snOperatorIdKey);
                    log.info("pileEromingAsync operationId :{}", operationId);
                    if(StringUtils.isBlank(operationId)){
                        return;
                    }

                    Boolean cpoEoamingEnabled = billFeignClient.getCpoEoamingEnabled(Long.parseLong(operationId), "OCPI").getData();
                    if(!cpoEoamingEnabled){
                        return;
                    }

                    RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(Long.parseLong(operationId)).getData();
                    log.info("pileEromingAsync roamingRulesVO :{}", JSON.toJSONString(roamingRulesVO));
                    String cpoId = "";
                    if(roamingRulesVO != null){
                        cpoId = roamingRulesVO.getOcpiId();
                    }

                    String finalCpoId = cpoId;
                    if(ActionType.delete.getCode() == actionType){
                        List<OcpiEvseVO> evseVOList = new ArrayList<>();
                        evseList.forEach(evseId -> {
                            try{
                                OcpiEvseVO evseVO = new OcpiEvseVO();
                                evseVO.setUid(evseId);
                                evseVO.setStatus("REMOVED");
                                evseVOList.add(evseVO);
                            }catch (Exception e1){
                                log.error("pileEromingAsync evseList 异常", e1);
                            }
                        });

                        OcpiLocationVO locationVO = OcpiLocationVO.builder().evses(evseVOList).build();
                        ocpiClient.patchLocation(opLocationPileEvseDTO.getLocationId().toString(), finalCpoId, locationVO);
                        log.info("pileEromingAsync 去除互联互通成功");
                    }else{
                        evseList.forEach(evseId -> {
                            try{
                                OcpiLocationVO ocpiLocation = ocpiLocationService.getEvseLocation(opLocationPileEvseDTO.getLocationId().toString(), evseId);
                                ocpiClient.patchLocation(opLocationPileEvseDTO.getLocationId().toString(), finalCpoId, ocpiLocation);
                            }catch (Exception e1){
                                log.error("pileEromingAsync evseList 异常", e1);
                            }
                        });
                    }
                }
            }
        } catch (Exception e) {
            log.error("pileEromingAsync 异常", e);
        }
    }

    @Async("pileBaseTaskExecutor")
    public void pushEvseList(String pileSn, Integer action) {
        try {
            if (Boolean.parseBoolean(hubjectEnable) && oicpFeignClient.checkUserIsGray()) {
                log.info("hubject action:{}, pileSn: {}", action, pileSn);
                LambdaQueryWrapper<OpLocationPileEvseEntity> eq = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                        .eq(OpLocationPileEvseEntity::getPileSn, pileSn)
                        .eq(OpLocationPileEvseEntity::getDeleted, false);

                OpLocationPileEvseEntity opLocationPileEvseEntity = opLocationPileEvseRepository.getOne(eq);
                if (opLocationPileEvseEntity == null) {
                    log.info("pileSn: {} 不支持eroaming， eroamingEnable：{}", pileSn, JSON.toJSONString(opLocationPileEvseEntity));
                    return;
                }

                String snOperatorIdKey = RedisKeyConstant.getStringSnOperatorIdKey(pileSn);
                String operationId = stringRedisTemplate.opsForValue().get(snOperatorIdKey);
                log.info("pushEvseList operationId :{}", operationId);
                if(org.apache.commons.lang.StringUtils.isBlank(operationId)){
                    return;
                }

                Boolean cpoEoamingEnabled = billFeignClient.getCpoEoamingEnabled(Long.parseLong(operationId), "OICP").getData();
                if(!cpoEoamingEnabled){
                    return;
                }

                OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseEntity);

                String evseList = opLocationPileEvseEntity.getEvseList();
                List<Long> evseIds = JSON.parseArray(evseList, Long.class);
                List<OpLocationEvseDTO> opLocationEvseDTOS = Lists.newArrayList();
                List<TariffEvse> collect = Lists.newArrayList();
                evseIds.forEach(e -> {
                    OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(e);
                    OpLocationEvseElasticDTO byEvseSn = opLocationEvseElastic.findByEvseSn(opLocationEvseDTO.getEvseSn());
                    opLocationEvseDTOS.add(opLocationEvseDTO);
                    TariffEvse build = TariffEvse.builder()
                            .evseSn(opLocationEvseDTO.getEvseSn())
                            .tariffId(byEvseSn.getTariffId())
                            .build();
                    collect.add(build);
                });
                opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
                OpLocationDTO locationByPileSn = opLocationRepository.getLocationByPileSn(opPileEvseInfoVO.getPileSn());
                EroamingEvseData eroamingEvseData = EroamingEvseData.builder()
                        .locationDTO(locationByPileSn)
                        .opPileEvseInfoVO(opPileEvseInfoVO)
                        .build();
                if (opLocationPileEvseEntity.getEroamingEnable() == 1
                        || (opLocationPileEvseEntity.getEroamingEnable() != 1 && action == 3)) {
                    oicpFeignClient.pushEvseData(eroamingEvseData, action);
                    oicpFeignClient.cpoPushPricingProductData(collect, action);
                }

            }
        } catch (Exception e) {
            log.error("推送hubject平台失败", e);
        }
    }

    @Async("pileBaseTaskExecutor")
    public void syncEroamingPileList(List<EroamingPileVO> eroamingPileVOList) {
        try{
            if (ocpiEnabled) {
                log.info("syncEroamingPileList互联互通的枪列表：{}", JSON.toJSONString(eroamingPileVOList));
                eroamingPileVOList.forEach(evse -> {
                    try{
                        Boolean cpoEoamingEnabled = billFeignClient.getCpoEoamingEnabled(Long.parseLong(evse.getOperatorId()), "OCPI").getData();
                        if(cpoEoamingEnabled){
                            OcpiLocationVO ocpiLocationVO = ocpiLocationService.getEvseLocation(evse.getLocationId(), evse.getEvseId());
                            RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(Long.parseLong(evse.getOperatorId())).getData();
                            String ocpiId = "";
                            if(roamingRulesVO != null){
                                ocpiId = roamingRulesVO.getOcpiId();
                            }
                            ocpiClient.patchLocation(evse.getLocationId(), ocpiId, ocpiLocationVO);
                        }
                    }catch (Exception e1){
                        log.error("syncEroamingPileListByTariff evseList 异常", e1);
                    }
                });
            }
        }catch (Exception e){
            log.error("syncEroamingPileList互联互通的枪数据到EMSP异常");
        }
    }


    @Async("pileBaseTaskExecutor")
    public void syncEroamingLocationType(OpLocationEntity opLocationEntity) {
        try{
            if (ocpiEnabled) {
                Boolean cpoEoamingEnabled = billFeignClient.getCpoEoamingEnabled(opLocationEntity.getOperatorId(), "OCPI").getData();
                if(cpoEoamingEnabled){
                    RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(opLocationEntity.getOperatorId()).getData();
                    String ocpiId = "";
                    if(roamingRulesVO != null){
                        ocpiId = roamingRulesVO.getOcpiId();
                    }
                    OcpiLocationVO ocpiLocationVO = new OcpiLocationVO();

                    Integer locationType = opLocationEntity.getLocationType();
                    String type = "ON_STREET";
                    switch (locationType) {
                        case 1:
                            type = "ON_STREET";
                            break;
                        case 2:
                            type = "PARKING_GARAGE";
                            break;
                        case 3:
                            type = "UNDERGROUND_GARAGE";
                            break;
                        case 4:
                            type = "PARKING_LOT";
                            break;
                        case 5:
                            type = "PARKING_LOT";
                            break;
                        case 6:
                            type = "PARKING_LOT";
                            break;
                        case 7:
                            type = "PARKING_LOT";
                            break;
                        case 8:
                            type = "PARKING_LOT";
                            break;
                    }
                    ocpiLocationVO.setType(type);
                    ocpiClient.patchLocation(opLocationEntity.getId().toString(), ocpiId, ocpiLocationVO);
                }
            }
        }catch (Exception e){
            log.error("syncEroamingLocationType到EMSP异常");
        }
    }


    @Async("pileBaseTaskExecutor")
    public void syncEroamingLocation(OpLocationEntity opLocationEntity, Integer action) {
        try{
            if (ocpiEnabled) {
                Boolean cpoEoamingEnabled = billFeignClient.getCpoEoamingEnabled(opLocationEntity.getOperatorId(), "OCPI").getData();
                if(cpoEoamingEnabled){
                    RoamingRulesVO roamingRulesVO = billFeignClient.getIndependentCpoIdBySeller(opLocationEntity.getOperatorId()).getData();
                    String ocpiId = "";
                    if(roamingRulesVO != null){
                        ocpiId = roamingRulesVO.getOcpiId();
                    }
                    if(ActionType.delete.getCode() == action){
                        LambdaQueryWrapper<OpLocationEvseEntity> eq = Wrappers.lambdaQuery(OpLocationEvseEntity.class)
                                .select(OpLocationEvseEntity::getId)
                                .eq(OpLocationEvseEntity::getLocationId, opLocationEntity.getId())
                                .eq(OpLocationEvseEntity::getDeleted, false);

                        List<OpLocationEvseEntity> opLocationEvseList = opLocationEvseRepository.list(eq);
                        if (CollectionUtils.isEmpty(opLocationEvseList)) {
                            return;
                        }

                        for(OpLocationEvseEntity  opLocationEvseEntity :  opLocationEvseList){
                            EVSEVO evseVO = new EVSEVO();
                            evseVO.setStatus("REMOVED");
                            ocpiClient.patchToIOPEvse(opLocationEntity.getId().toString(), opLocationEvseEntity.getEvseId(), ocpiId, evseVO);
                        }
                    }else{
                        OcpiLocationVO ocpiLocationVO = ocpiLocationService.getEvseLocation(opLocationEntity.getId().toString(), null);
                        if(ocpiLocationVO != null){
                            ocpiClient.patchLocation(opLocationEntity.getId().toString(), ocpiId, ocpiLocationVO);
                        }
                    }
                }
            }
        }catch (Exception e){
            log.error("syncEroamingLocation到EMSP异常");
        }
    }

    @Async("pileBaseTaskExecutor")
    public void pushLocationEvseList(OpLocationEntity opLocationEntity, Integer action) {
        try {
            if (Boolean.parseBoolean(hubjectEnable) && oicpFeignClient.checkUserIsGray()) {
                log.info("hubject action:{}, pileSn: {}", action, JSON.toJSON(opLocationEntity));

                Boolean cpoEoamingEnabled = billFeignClient.getCpoEoamingEnabled(opLocationEntity.getOperatorId(), "OICP").getData();
                if(cpoEoamingEnabled){
                    LambdaQueryWrapper<OpLocationPileEvseEntity> eq = Wrappers.lambdaQuery(OpLocationPileEvseEntity.class)
                            .eq(OpLocationPileEvseEntity::getLocationId, opLocationEntity.getId())
                            .eq(OpLocationPileEvseEntity::getDeleted, false);

                    List<OpLocationPileEvseEntity> opLocationPileEvseEntityList = opLocationPileEvseMapper.selectList(eq);
                    if (CollectionUtils.isEmpty(opLocationPileEvseEntityList)) {
                        return;
                    }

                    for(OpLocationPileEvseEntity opLocationPileEvseEntity : opLocationPileEvseEntityList){
                        OpPileEvseInfoVO opPileEvseInfoVO = OpLocationPileEvseConvert.toOpPileEvseInfoVO(opLocationPileEvseEntity);

                        String evseList = opLocationPileEvseEntity.getEvseList();
                        List<Long> evseIds = JSON.parseArray(evseList, Long.class);
                        List<OpLocationEvseDTO> opLocationEvseDTOS = Lists.newArrayList();
                        List<TariffEvse> collect = Lists.newArrayList();
                        evseIds.forEach(e -> {
                            OpLocationEvseDTO opLocationEvseDTO = opLocationEvseRepository.details(e);
                            OpLocationEvseElasticDTO byEvseSn = opLocationEvseElastic.findByEvseSn(opLocationEvseDTO.getEvseSn());
                            opLocationEvseDTOS.add(opLocationEvseDTO);
                            TariffEvse build = TariffEvse.builder()
                                    .evseSn(opLocationEvseDTO.getEvseSn())
                                    .tariffId(byEvseSn.getTariffId())
                                    .build();
                            collect.add(build);
                        });
                        opPileEvseInfoVO.setOpLocationEvseDTOS(opLocationEvseDTOS);
                        OpLocationDTO locationByPileSn = opLocationRepository.getLocationByPileSn(opPileEvseInfoVO.getPileSn());
                        EroamingEvseData eroamingEvseData = EroamingEvseData.builder()
                                .locationDTO(locationByPileSn)
                                .opPileEvseInfoVO(opPileEvseInfoVO)
                                .build();
                        if (opLocationPileEvseEntity.getEroamingEnable() == 1
                                || (opLocationPileEvseEntity.getEroamingEnable() != 1 && action == 3)) {
                            oicpFeignClient.pushEvseData(eroamingEvseData, action);
                            oicpFeignClient.cpoPushPricingProductData(collect, action);
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("推送hubject平台失败", e);
        }
    }
}
