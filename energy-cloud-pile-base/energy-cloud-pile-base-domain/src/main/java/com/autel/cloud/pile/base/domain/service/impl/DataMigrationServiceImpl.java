package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.util.DateUtil;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.convert.OpLocationImageConvert;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.DataMigrationService;
import com.autel.cloud.pile.base.domain.service.OpLocationService;
import com.autel.cloud.pile.base.dto.OpLocationConnectorDTO;
import com.autel.cloud.pile.base.dto.OpLocationDTO;
import com.autel.cloud.pile.base.dto.OpLocationEvseDTO;
import com.autel.cloud.pile.base.dto.OpLocationPileEvseDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.feign.ConfigServiceFeignClient;
import com.autel.cloud.pile.base.infrastructure.feign.DeviceServiceFeign;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @ClassName DataMigrationServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/6/8 15:32
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Slf4j
public class DataMigrationServiceImpl implements DataMigrationService {

    @Autowired
    private OpLocationService opLocationService;

    @Autowired
    private OpLocationRepository opLocationRepository;

    @Autowired
    private OpCountryRepository opCountryRepository;

    @Autowired
    private OpImageRepository opImageRepository;

    @Autowired
    private OpLocationImageRepository opLocationImageRepository;

    @Autowired
    private OpLocationEvseRepository opLocationEvseRepository;

    @Autowired
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private DeviceServiceFeign deviceServiceFeign;

    @Autowired
    private OpLocationElastic opLocationElastic;

    @Autowired
    private OpLocationEvseElastic opLocationEvseElastic;

    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;

    private final ConfigServiceFeignClient configServiceFeignClient;

    public DataMigrationServiceImpl(ConfigServiceFeignClient configServiceFeignClient) {
        this.configServiceFeignClient = configServiceFeignClient;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Result<JSONObject> trigger() {
//        int total = 0;
//        int current = 1;
//        int size = 100;
//        List<OpLocationEvseDTO> opLocationEvseAddDTOList = new ArrayList<>();
//        List<OpLocationEvseDTO> opLocationEvseUpdateDTOList = new ArrayList<>();
//        do {
//            Map<String, Object> param = new HashMap<>();
//            param.put("current", current);
//            param.put("size", size);
//            String str = JSONObject.toJSONString(configServiceFeignClient.stationPage(param));
//            JSONObject ret = JSONObject.parseObject(str);
//            JSONObject data = ret.getJSONObject("data");
//            JSONArray records = data.getJSONArray("records");
//            total = records.size();
//            log.info("DataMigrationServiceImpl trigger total = " + total);
//            log.info("DataMigrationServiceImpl trigger records = " + JSONObject.toJSONString(records));
//            List<JSONObject> list = records.toJavaList(JSONObject.class);
//            list.forEach(jsonObject -> {
//                log.info("DataMigrationServiceImpl trigger records foreach = " + JSONObject.toJSONString(jsonObject));
//                OpLocationDTO opLocationDTO = toOpLocationDTO(jsonObject);
//                log.info("DataMigrationServiceImpl opLocationDTO = " + JSONObject.toJSONString(opLocationDTO));
//
//                // TODO 判断是否重复，根据名称查询场站
//                LambdaQueryWrapper<OpLocationEntity> query = Wrappers.lambdaQuery(OpLocationEntity.class)
//                        .eq(OpLocationEntity::getName, opLocationDTO.getName())
//                        .eq(OpLocationEntity::getOperatorId, opLocationDTO.getOperatorId())
//                        .eq(OpLocationEntity::getDeleted, Boolean.FALSE);
//                List<OpLocationEntity> oldList = opLocationRepository.list(query);
//                try {
//                    if (CollectionUtils.isEmpty(oldList)) {
//                        Result<Long> addRet = opLocationService.add(opLocationDTO);
//                        opLocationDTO.setId(addRet.getData());
//                        log.info("DataMigrationServiceImpl opLocationDTO is exist and name = " + opLocationDTO.getName());
//                    } else {
//                        opLocationDTO.setId(oldList.get(0).getId());
//                    }
//                } catch (Exception e) {
//                    log.error("DataMigrationServiceImpl jsonObject = " + JSONObject.toJSONString(jsonObject));
//                    return;
//                }
//
//                // 转换图片
//                List<OpImageEntity> imageEntityList = OpLocationImageConvert.toOpImageEntityList(jsonObject);
//                imageEntityList.forEach(image -> {
//                    LambdaQueryWrapper<OpImageEntity> queryWrapper = Wrappers.lambdaQuery(OpImageEntity.class);
//                    queryWrapper.eq(OpImageEntity::getDeleted, Boolean.FALSE);
//                    queryWrapper.eq(OpImageEntity::getUrl, image.getUrl());
//                    List<OpImageEntity> opImageEntityList = opImageRepository.list(queryWrapper);
//                    if (CollectionUtils.isEmpty(opImageEntityList)) {
//                        opImageRepository.saveOrUpdate(image);
//                    } else {
//                        image.setId(opImageEntityList.get(0).getId());
//                    }
//                    OpLocationImageEntity opLocationImageEntity = OpLocationImageConvert.buildOpLocationImageEntity(image, opLocationDTO);
//                    LambdaQueryWrapper<OpLocationImageEntity> LocationImageQueryWrapper = Wrappers.lambdaQuery(OpLocationImageEntity.class);
//                    LocationImageQueryWrapper.eq(OpLocationImageEntity::getDeleted, Boolean.FALSE);
//                    LocationImageQueryWrapper.eq(OpLocationImageEntity::getLocationId, opLocationImageEntity.getLocationId());
//                    LocationImageQueryWrapper.eq(OpLocationImageEntity::getImageId, opLocationImageEntity.getImageId());
//                    List<OpLocationImageEntity> opLocationImageEntityList = opLocationImageRepository.list(LocationImageQueryWrapper);
//                    if (CollectionUtils.isEmpty(opLocationImageEntityList)) {
//                        opLocationImageRepository.saveOrUpdate(opLocationImageEntity);
//                    } else {
//                        image.setId(opLocationImageEntityList.get(0).getId());
//                    }
//                });
//
//                // 根据场站获取桩枪
//                String pileListStr = JSONObject.toJSONString(configServiceFeignClient.getPileDTOByStationId(jsonObject.getLong("id")));
//                JSONObject pileListRet = JSONObject.parseObject(pileListStr);
//                log.info("DataMigrationServiceImpl pileRet = " + JSONObject.toJSONString(pileListRet));
//                if (pileListRet.getInteger("code").equals(0)) {
//                    JSONArray pileRetJSONArray = pileListRet.getJSONArray("data");
//                    List<JSONObject> pileRecord = pileRetJSONArray.toJavaList(JSONObject.class);
//                    log.info("DataMigrationServiceImpl pileRecord = " + JSONObject.toJSONString(pileRecord));
//                    pileRecord.forEach(pileDTOJSON -> {
//                        log.info("DataMigrationServiceImpl pileDTOJSON = " + JSONObject.toJSONString(pileDTOJSON));
//                        OpLocationEvseDTO opLocationEvseDTO = toOpLocationEvseDTO(pileDTOJSON, opLocationDTO);
//                        // 判断是否已经添加
//                        OpLocationPileEvseDTO opLocationPileEvseDTO = opLocationPileEvseRepository
//                                .queryByPileSn(opLocationEvseDTO.getPileSN());
//                        if (opLocationPileEvseDTO == null) {
//                            opLocationEvseAddDTOList.add(opLocationEvseDTO);
//                        } else {
//                            opLocationEvseDTO.setId(opLocationPileEvseDTO.getId());
//                            opLocationEvseUpdateDTOList.add(opLocationEvseDTO);
//                        }
//                    });
//
//                }
//            });
//            current++;
//        } while (total >= size);
//        log.info("DataMigrationServiceImpl opLocationEvseAddDTOList = " + JSONObject.toJSONString(opLocationEvseAddDTOList));
//        log.info("DataMigrationServiceImpl opLocationEvseUpdateDTOList = " + JSONObject.toJSONString(opLocationEvseUpdateDTOList));
//        List<OpLocationEvseDTO> pileAddList = distinctList(opLocationEvseAddDTOList);
//        List<OpLocationEvseDTO> pileUpdateList = distinctList(opLocationEvseUpdateDTOList);
//        log.info("DataMigrationServiceImpl pileAddList = " + JSONObject.toJSONString(pileAddList));
//        log.info("DataMigrationServiceImpl pileUpdateList = " + JSONObject.toJSONString(pileUpdateList));
//        opLocationEvseRepository.createEvse(pileAddList);
//        opLocationEvseRepository.updateEvse(pileUpdateList);
        return Result.ofSucceed();
    }

    private List<OpLocationEvseDTO> distinctList(List<OpLocationEvseDTO> dtoList) {
        // 去重
        return dtoList.stream().collect(
                Collectors.collectingAndThen(Collectors.toCollection(() ->
                        new TreeSet<>(Comparator.comparing(OpLocationEvseDTO::getPileSN))), ArrayList::new)
        );
    }

    /**
     * 构建
     *
     * @param pileDTOJSON
     * @param opLocationDTO
     * @return
     */
    private OpLocationEvseDTO toOpLocationEvseDTO(JSONObject pileDTOJSON, OpLocationDTO opLocationDTO) {
        String pileSn = pileDTOJSON.getString("pileNum");
        OpLocationEvseDTO opLocationEvseDTO = new OpLocationEvseDTO();
        deviceServiceFeign.queryPileList(Collections.singletonList(pileSn));
        opLocationEvseDTO.setLocationId(opLocationDTO.getId());
        opLocationEvseDTO.setBrandName("Autel");
        opLocationEvseDTO.setBrandId(1L);
        opLocationEvseDTO.setPileSN(pileSn);
        opLocationEvseDTO.setPileName(pileDTOJSON.getString("name"));
        opLocationEvseDTO.setThirdPart(0);
        opLocationEvseDTO.setTariffId(pileDTOJSON.getLong("stationRuleId"));
        opLocationEvseDTO.setPinCode(pileDTOJSON.getString("password"));
        JSONObject pileType = pileDTOJSON.getJSONObject("pileType");
        opLocationEvseDTO.setPower(pileType.getDouble("power"));
        List<OpLocationConnectorDTO> opLocationConnectorDTOS = new ArrayList<>();
        pileDTOJSON.getJSONArray("guns").toJavaList(JSONObject.class).forEach(jsonObject -> {
            OpLocationConnectorDTO opLocationConnectorDTO = new OpLocationConnectorDTO();
            opLocationConnectorDTO.setGunType(jsonObject.getInteger("gunTypeId"));
            opLocationConnectorDTO.setConnectorId(jsonObject.getString("number"));
            opLocationConnectorDTOS.add(opLocationConnectorDTO);
        });
        opLocationEvseDTO.setOpLocationConnectorDTOs(opLocationConnectorDTOS);
        return opLocationEvseDTO;
    }

    /**
     * 转换
     *
     * @param json
     * @return
     */
    private OpLocationDTO toOpLocationDTO(JSONObject json) {
        OpLocationDTO opLocationDTO = new OpLocationDTO();
        opLocationDTO.setName(json.getString("name"));
        opLocationDTO.setTimeZone(json.getString("timeZone"));
        opLocationDTO.setOperatorId(json.getLong("sellerId"));
        opLocationDTO.setSubOperatorId(null);
        opLocationDTO.setGroupId(0L);
        opLocationDTO.setOwnerId(null);
        opLocationDTO.setAppShow(json.getBoolean("appShow"));
        opLocationDTO.setProvince("");
        opLocationDTO.setId(null);
        opLocationDTO.setStatus(0);
        opLocationDTO.setLatitude(json.getString("latitude"));
        opLocationDTO.setLongitude(json.getString("longitude"));
        opLocationDTO.setOpenType(json.getInteger("openType"));
        opLocationDTO.setDeleted(0);
        opLocationDTO.setOperationType(json.getString("runType"));
        opLocationDTO.setState(null);
        OpCountryEntity opCountryEntity = getCountry(json.getString("countryCode"));
        log.info("DataMigrationServiceImpl toOpLocationDTO opCountryEntity = " + JSON.toJSONString(opCountryEntity));
        if (opCountryEntity != null) {
            opLocationDTO.setCountry(opCountryEntity.getCode());
            opLocationDTO.setCountryName(opCountryEntity.getName());
        }
        opLocationDTO.setAnnouncement(json.getString("announcement"));
        opLocationDTO.setCity("");
        opLocationDTO.setType(json.getString("stationType"));
        opLocationDTO.setTransPower(json.getBigDecimal("transPower"));
        opLocationDTO.setOperationDate(DateUtil.parseTimestampForDate("2022-06-01"));
        opLocationDTO.setBillingRule(json.getString("billingRule"));
        opLocationDTO.setServiceTel(json.getString("serviceTel"));
        opLocationDTO.setPostalCode(json.getString("postCode"));
        opLocationDTO.setAddress(json.getString("street"));
        opLocationDTO.setChargingWhenClosed(1);
        return opLocationDTO;
    }

    /**
     * 计算国家代码
     *
     * @param code
     * @return
     */
    private OpCountryEntity getCountry(String code) {
        if (StringUtils.isBlank(code)) {
            code = "+86";
        }
        String countryCode = "";
        if (code.startsWith("+")) {
            countryCode = code;
        } else {
            code = code.replace("（", "(").replace("）", ")");
            countryCode = code.substring(code.indexOf("(") + 1, code.length() - 1);
        }
        LambdaQueryWrapper<OpCountryEntity> queryWrapper = Wrappers.lambdaQuery(OpCountryEntity.class)
                .eq(OpCountryEntity::getCode, countryCode)
                .eq(OpCountryEntity::getDeleted, Boolean.FALSE);
        List<OpCountryEntity> list = opCountryRepository.list(queryWrapper);
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        return list.get(0);
    }

}
