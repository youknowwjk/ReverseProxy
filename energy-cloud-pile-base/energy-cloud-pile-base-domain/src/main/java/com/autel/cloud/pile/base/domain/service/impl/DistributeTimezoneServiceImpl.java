package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileEvseRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationRepository;
import com.autel.cloud.pile.base.domain.service.DistributeTimezoneService;
import com.autel.cloud.pile.base.dto.PileTimezoneDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.ProtocolFeignClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class DistributeTimezoneServiceImpl implements DistributeTimezoneService {

    @Autowired
    private ProtocolFeignClient protocolFeignClient;

    @Autowired
    private OpLocationRepository opLocationRepository;

    @Autowired
    @Lazy
    private OpLocationPileEvseRepository opLocationPileEvseRepository;

    @Autowired
    private UserTimezoneServiceImpl userTimezoneService;

    @Override
    public Result<Boolean> distributePublicPileTimezone(String pileSn) {
        log.info("======== the distributePublicPileTimezone function invoked, the pileSn:{}", pileSn);
        OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseRepository.findByPileSn(pileSn);
        if (null == opLocationPileEvseElasticDTO) {
            log.error("======= the pileSn is not existed, return. pileSn:{}", pileSn);
            return Result.ofSucceed(Boolean.FALSE);
        }
        OpLocationElasticDTO opLocationElasticDTO = opLocationRepository.getDetailsFromEsById(opLocationPileEvseElasticDTO.getLocationId());
        if (null == opLocationElasticDTO) {
            log.error("======= the location of pileSn is not existed, return. pileSn:{}", pileSn);
            return Result.ofSucceed(Boolean.FALSE);
        }
        PileTimezoneDTO pileTimezoneDTO = PileTimezoneDTO.builder()
                .pileSn(pileSn)
                .timezone(opLocationElasticDTO.getTimeZone())
                .zoneId(opLocationElasticDTO.getZoneId())
                .build();
        Result<Boolean> dispatchResult = protocolFeignClient.dispatchTimezone(pileTimezoneDTO);
        log.info("======== the pileSn:{} in the distributePublicPileTimezone function dispatchResult:{}", pileSn, dispatchResult);
        return dispatchResult;
    }

    @Override
    public Result<Boolean> distributeHomePileTimezone(String pileSn, Long userId) {
        log.info("======== the distributeHomePileTimezone function invoked, the pileSn:{}, userId:{}", pileSn, userId);
        if (null == userId) {
            log.error("======== because userId is null in distributeHomePileTimezone function, return.");
            return Result.ofSucceed(Boolean.FALSE);
        }
        com.autel.cloud.pile.bill.dto.TimezoneUserDTO timezoneUserDTO = userTimezoneService.getTimeZoneInfoOfUser(userId);
        String timezone = timezoneUserDTO.getTimezone();
        String zoneId = timezoneUserDTO.getZoneId();
        PileTimezoneDTO pileTimezoneDTO = PileTimezoneDTO.builder()
                .pileSn(pileSn)
                .timezone(timezone)
                .zoneId(zoneId)
                .build();
        Result<Boolean> dispatchResult = protocolFeignClient.dispatchTimezone(pileTimezoneDTO);
        log.info("======== the pileSn:{} in the distributeHomePileTimezone function dispatchResult:{}", pileSn, dispatchResult);
        return dispatchResult;
    }
}
