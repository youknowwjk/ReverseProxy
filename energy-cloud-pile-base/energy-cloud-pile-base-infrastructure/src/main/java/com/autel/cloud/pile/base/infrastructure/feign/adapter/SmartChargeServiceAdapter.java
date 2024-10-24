package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.PowerLimitParamDTO;
import com.autel.cloud.pile.base.vo.ChargingDetailVO;
import com.autel.cloud.smart.charge.dto.ProfileParamDTO;
import com.autel.cloud.smart.charge.feign.SmartChargeFeign;
import com.autel.cloud.smart.charge.vo.ChargingGraphVO;
import com.autel.cloud.smart.charge.vo.ChargingProfileSettingVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestParam;

import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Component
public class SmartChargeServiceAdapter extends AbstractFeignServiceAdapter {

    @Resource
    private SmartChargeFeign smartChargeFeign;

    @Value("${webhook.wechat.key.pile-user:66ae591a-4e15-4b57-9a1c-56978ba6152b}")
    protected String webhookWechatKey;


    public Boolean delivery(PowerLimitParamDTO powerLimitParamDTO) {
        log.info("SetChargingProfile {}", JSON.toJSONString(powerLimitParamDTO));
        Boolean handle = nullableHandle(smartChargeFeign.delivery(powerLimitParamDTO));
        log.info("SetChargingProfile {}", handle);
        return handle;
    }

    public Boolean clear(PowerLimitParamDTO powerLimitParamDTO) {
        log.info("ClearChargingProfile {}", JSON.toJSONString(powerLimitParamDTO));
        Boolean handle = nullableHandle(smartChargeFeign.clear(powerLimitParamDTO));
        log.info("ClearChargingProfile {}", handle);
        return handle;
    }

    public Boolean get(PowerLimitParamDTO powerLimitParamDTO) {
        log.info("GetChargingProfile {}", JSON.toJSONString(powerLimitParamDTO));
        Boolean handle = nullableHandle(smartChargeFeign.get(powerLimitParamDTO));
        log.info("GetChargingProfile {}", handle);
        return handle;
    }


    public List<ChargingDetailVO> findProfileList(String evseSn, String transactionId) {
        Result<List<ChargingProfileSettingVO>> listResult = smartChargeFeign.findProfileList(evseSn, transactionId);
        List<ChargingProfileSettingVO> profileSettingVOList = nullableHandle(listResult);
        List<ChargingDetailVO> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(profileSettingVOList)) {
            if (profileSettingVOList.size() == 1) {
                result.addAll(profileSettingVOList.get(0).getSocList());
                return result;
            }
            for (int i = 0; i < profileSettingVOList.size() - 1; i++) {
                ChargingProfileSettingVO pre = profileSettingVOList.get(i);
                ChargingProfileSettingVO next = profileSettingVOList.get(i + 1);
                Long createTime = next.getCreateTime();
                List<ChargingDetailVO> socList = pre.getSocList();
                if (!CollectionUtils.isEmpty(socList)) {
                    for (ChargingDetailVO chargingDetailVO : socList) {
                        if (chargingDetailVO.getStartPeriod().compareTo(createTime) < 0) {
                            result.add(chargingDetailVO);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        return result;
    }

    public ChargingDetailVO findLastChargingDetailVO(@RequestParam("evseSn") String evseSn, @RequestParam("transactionId") String transactionId) {
        ProfileParamDTO profileParamDTO = new ProfileParamDTO();
        profileParamDTO.setEvseSn(evseSn);
        profileParamDTO.setTransactionId(transactionId);
        ChargingDetailVO lastChargingDetailVO = findCurrentChargingDetailVO(profileParamDTO);
        log.info("lastChargingDetailVO {}  evseSn= {} transactionId={}", JSON.toJSONString(lastChargingDetailVO), evseSn, transactionId);
        return lastChargingDetailVO;
    }

//    public ChargingProfileSettingVO findLastProfileList(String evseSn, String transactionId) {
//        log.info("findLastProfileList evseSn= {}  transactionId={}", evseSn, transactionId);
//        ProfileParamDTO profileParamDTO = new ProfileParamDTO();
//        profileParamDTO.setEvseSn(evseSn);
//        profileParamDTO.setTransactionId(transactionId);
//        return findLastProfileList(profileParamDTO);
//    }

    public ChargingGraphVO graphList(String evseSn, String transactionId) {
        log.info("graphList evseSn= {}  transactionId={}", evseSn, transactionId);
        ProfileParamDTO profileParamDTO = new ProfileParamDTO();
        profileParamDTO.setEvseSn(evseSn);
        profileParamDTO.setTransactionId(transactionId);
        return graphList(profileParamDTO);
    }

//    public ChargingProfileSettingVO findLastProfileList(ProfileParamDTO paramDto) {
//        log.info("findLastProfileList  {}", JSON.toJSONString(paramDto));
//        Result<List<ChargingProfileSettingVO>> responseResult = smartChargeFeign.lastProfileList(Collections.singletonList(paramDto));
//        log.info("findLastProfileList result {}", JSON.toJSONString(responseResult));
//        List<ChargingProfileSettingVO> chargingProfileSettingVOS = nullableHandle(responseResult);
//        if (CollectionUtils.isEmpty(chargingProfileSettingVOS)) {
//            return null;
//        }
//        return chargingProfileSettingVOS.get(0);
//    }

    public ChargingGraphVO graphList(ProfileParamDTO paramDto) {
        log.info("graphList  {}", JSON.toJSONString(paramDto));
        Result<List<ChargingGraphVO>> responseResult = smartChargeFeign.graphList(Collections.singletonList(paramDto));
        log.info("graphList result {}", JSON.toJSONString(responseResult));
        List<ChargingGraphVO> chargingProfileSettingVOS = nullableHandle(responseResult);
        if (CollectionUtils.isEmpty(chargingProfileSettingVOS)) {
            return null;
        }
        return chargingProfileSettingVOS.get(0);
    }

    public ChargingDetailVO findCurrentChargingDetailVO(ProfileParamDTO paramDto) {
        log.info("findCurrentChargingDetailVO {}", JSON.toJSONString(paramDto));
        ChargingGraphVO lastChargingProfileSettingVO = graphList(paramDto);
        if (Objects.isNull(lastChargingProfileSettingVO)) {
            log.info("findLastChargingDetailVO 1 {}", JSON.toJSONString(paramDto));
            return null;
        }
        List<ChargingDetailVO> socList = lastChargingProfileSettingVO.getSocList();
        if (CollectionUtils.isEmpty(socList)) {
            log.info("findLastChargingDetailVO 2 {}", JSON.toJSONString(paramDto));
            return null;
        }
        List<ChargingDetailVO> collect = socList.stream().sorted(Comparator.comparing(ChargingDetailVO::getStartPeriod)).collect(Collectors.toList());
        if (collect.size() == 1) {
            log.info("findLastChargingDetailVO 1 {} {}", JSON.toJSONString(paramDto), collect.get(0));
            return collect.get(0);
        }
        for (int i = 0; i < collect.size() - 1; i++) {
            ChargingDetailVO left = collect.get(i);
            ChargingDetailVO right = collect.get(i + 1);
            long millis = System.currentTimeMillis();
            if (millis >= left.getStartPeriod() && millis < right.getStartPeriod()) {
                log.info("findLastChargingDetailVO 2 {} {}", JSON.toJSONString(paramDto), left);
                return left;
            }
        }
        ChargingDetailVO result = collect.get(collect.size() - 1);
        log.info("findLastChargingDetailVO 3 {} {}", JSON.toJSONString(paramDto), result);
        return result;
    }
}
