package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationFacilityRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationFacilityService;
import com.autel.cloud.pile.base.dto.OpLocationFacilityDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationFacilityEntity;
import groovy.util.logging.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class OpLocationFacilityServiceImpl implements OpLocationFacilityService {

    @Autowired
    private OpLocationFacilityRepository opLocationFacilityRepository;

    @Override
    public Result<Long> addOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO) {
        Long facilityId = opLocationFacilityRepository.addOpLocationFacility(opLocationFacilityDTO);
        return Result.ofSucceed(facilityId);
    }

    @Override
    public Result<List<OpLocationFacilityEntity>> addOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList) {
        List<OpLocationFacilityEntity> result = opLocationFacilityRepository.addOpLocationFacilityByLocationId(locationId, opLocationFacilityDTOList);
        return Result.ofSucceed(result);
    }

    @Override
    public Result<Boolean> updateOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO) {
        Boolean result = opLocationFacilityRepository.updateOpLocationFacility(opLocationFacilityDTO);
        return Result.ofSucceed(result);
    }

    @Override
    public Result<List<OpLocationFacilityEntity>> updateOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList) {
        List<OpLocationFacilityEntity> result = opLocationFacilityRepository.updateOpLocationFacilityByLocationId(locationId, opLocationFacilityDTOList);
        return Result.ofSucceed(result);
    }

    @Override
    public Result<Boolean> delOpLocationFacility(Long id) {
        Boolean result = opLocationFacilityRepository.delOpLocationFacility(id);
        return Result.ofSucceed(result);
    }

    @Override
    public Result<Boolean> delOpLocationFacilityByLocationId(Long locationId) {
        Boolean result = opLocationFacilityRepository.delOpLocationFacilityByLocationId(locationId);
        return Result.ofSucceed(result);
    }

    @Override
    public Result<List<OpLocationFacilityDTO>> selectOpLocationFacilityListByLocationId(Long locationId) {
        List<OpLocationFacilityDTO> opLocationFacilityDTOList = opLocationFacilityRepository.selectOpLocationFacilityListByLocationId(locationId);
        return Result.ofSucceed(opLocationFacilityDTOList);
    }
}
