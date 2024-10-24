package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.repository.OpLocationFacilityRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationIconRepository;
import com.autel.cloud.pile.base.dto.OpLocationFacilityDTO;
import com.autel.cloud.pile.base.dto.OpLocationIconDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationFacilityMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationFacilityEntity;
import com.autel.cloud.pile.base.vo.OpLocationIconVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * <p>
 * 场站设施 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Service
@Slf4j
public class OpLocationFacilityRepositoryImpl extends ServiceImpl<OpLocationFacilityMapper, OpLocationFacilityEntity> implements OpLocationFacilityRepository {

    @Autowired
    private OpLocationIconRepository opLocationIconRepository;

    @Override
    public Long addOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO) {
        log.info("======== addOpLocationFacility in OpLocationFacilityRepositoryImpl faction params:{}", JSON.toJSONString(opLocationFacilityDTO));
        OpLocationFacilityEntity opLocationFacilityEntity = new OpLocationFacilityEntity();
        opLocationFacilityEntity.setDeleted(0);
        opLocationFacilityEntity.setFacilityCode(opLocationFacilityDTO.getCode());
        opLocationFacilityEntity.setFacilityStatus(0);
        opLocationFacilityEntity.setLocationId(opLocationFacilityDTO.getLocationId());
        opLocationFacilityEntity.setCreatedAt(System.currentTimeMillis());
        opLocationFacilityEntity.setUpdatedAt(System.currentTimeMillis());
        save(opLocationFacilityEntity);
        return opLocationFacilityEntity.getId();
    }

    @Override
    public List<OpLocationFacilityEntity> addOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList) {
        log.info("======== addOpLocationFacilityByLocationId in OpLocationFacilityRepositoryImpl faction, the locationId:{},  params:{}", locationId, JSON.toJSONString(opLocationFacilityDTOList));
        if (CollUtil.isEmpty(opLocationFacilityDTOList) || null == locationId) {
            return Collections.emptyList();
        }
        List<OpLocationFacilityEntity> opLocationFacilityEntityList = new ArrayList<>();
        Long now = System.currentTimeMillis();
        opLocationFacilityDTOList.forEach(opLocationFacilityDTO -> {
            OpLocationFacilityEntity opLocationFacilityEntity = new OpLocationFacilityEntity();
            opLocationFacilityEntity.setDeleted(0);
            opLocationFacilityEntity.setFacilityCode(opLocationFacilityDTO.getCode());
            opLocationFacilityEntity.setFacilityStatus(0);
            opLocationFacilityEntity.setLocationId(locationId);
            opLocationFacilityEntity.setCreatedAt(now);
            opLocationFacilityEntity.setUpdatedAt(now);
            opLocationFacilityEntityList.add(opLocationFacilityEntity);
        });
        boolean result = saveBatch(opLocationFacilityEntityList);
        if (result) {
            return opLocationFacilityEntityList;
        }
        return Collections.emptyList();
    }

    @Override
    public Boolean updateOpLocationFacility(OpLocationFacilityDTO opLocationFacilityDTO) {
        log.info("======== updateOpLocationFacility in OpLocationFacilityRepositoryImpl faction params:{}", JSON.toJSONString(opLocationFacilityDTO));
        OpLocationFacilityEntity opLocationFacilityEntity = new OpLocationFacilityEntity();
        opLocationFacilityEntity.setId(opLocationFacilityDTO.getId());
        opLocationFacilityEntity.setFacilityCode(opLocationFacilityDTO.getCode());
        opLocationFacilityEntity.setFacilityStatus(opLocationFacilityDTO.getStatus());
        opLocationFacilityEntity.setCreatedAt(opLocationFacilityDTO.getCreatedAt());
        opLocationFacilityEntity.setLocationId(opLocationFacilityDTO.getLocationId());
        opLocationFacilityEntity.setUpdatedAt(System.currentTimeMillis());
        return updateById(opLocationFacilityEntity);
    }

    @Override
    public List<OpLocationFacilityEntity> updateOpLocationFacilityByLocationId(Long locationId, List<OpLocationFacilityDTO> opLocationFacilityDTOList) {
        log.info("======== updateOpLocationFacility in OpLocationFacilityRepositoryImpl faction, the locationId:{},  params:{}", locationId, JSON.toJSONString(opLocationFacilityDTOList));
        if (null == opLocationFacilityDTOList || null == locationId) {
            return Collections.emptyList();
        }
        // 查询场站已有的周边设施
        LambdaQueryWrapper<OpLocationFacilityEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLocationFacilityEntity::getDeleted, 0);
        queryWrapper.eq(OpLocationFacilityEntity::getLocationId, locationId);
        List<OpLocationFacilityEntity> existedFacilityEntityList = list(queryWrapper);
        log.info("============ the existedFacilityEntityList:{}", existedFacilityEntityList);
        List<Long> existedIdList = existedFacilityEntityList.stream().map(OpLocationFacilityEntity::getId).collect(Collectors.toList());
        // 参数中的周边设施Id
        List<Long> paramsIdList = opLocationFacilityDTOList.stream().filter(opLocationFacilityDTO -> null != opLocationFacilityDTO.getId()).map(OpLocationFacilityDTO::getId).collect(Collectors.toList());
        //取差集
        List<Long> differenceSet = existedIdList.stream().filter(existId ->
                !paramsIdList.contains(existId)
        ).collect(Collectors.toList());
        log.info("========= the differenceSet:{}", differenceSet);
        List<OpLocationFacilityEntity> opLocationFacilityEntityList = new ArrayList<>();
        opLocationFacilityDTOList.forEach(opLocationFacilityDTO -> {
            OpLocationFacilityEntity opLocationFacilityEntity = new OpLocationFacilityEntity();
            if (null == opLocationFacilityDTO.getId()) {
                opLocationFacilityEntity.setCreatedAt(System.currentTimeMillis());
            } else {
                opLocationFacilityEntity.setId(opLocationFacilityDTO.getId());
            }
            opLocationFacilityEntity.setDeleted(0);
            opLocationFacilityEntity.setFacilityCode(opLocationFacilityDTO.getCode());
            opLocationFacilityEntity.setFacilityStatus(0);
            opLocationFacilityEntity.setUpdatedAt(System.currentTimeMillis());
            opLocationFacilityEntity.setLocationId(locationId);
            opLocationFacilityEntityList.add(opLocationFacilityEntity);
        });
        // 删除本次更新中不存在的周边设施
        if (CollUtil.isNotEmpty(differenceSet)) {
            batchDelOpLocationFacility(differenceSet);
        }
        boolean result = saveOrUpdateBatch(opLocationFacilityEntityList);
        if (result) {
            return opLocationFacilityEntityList;
        }
        return null;
    }

    @Override
    public Boolean delOpLocationFacility(Long id) {
        log.info("======== delOpLocationFacility in OpLocationFacilityRepositoryImpl faction params:{}", id);
        if (id == null) {
            return false;
        }
        LambdaUpdateWrapper<OpLocationFacilityEntity> updateQuery = new LambdaUpdateWrapper<>();
        updateQuery.set(OpLocationFacilityEntity::getDeleted, 1);
        updateQuery.eq(OpLocationFacilityEntity::getId, id);
        return update(updateQuery);
    }

    public Boolean batchDelOpLocationFacility(List<Long> idList) {
        log.info("======== batchDelOpLocationFacility in OpLocationFacilityRepositoryImpl faction params:{}", idList);
        if (CollUtil.isEmpty(idList)) {
            return false;
        }
        LambdaUpdateWrapper<OpLocationFacilityEntity> updateQuery = new LambdaUpdateWrapper<>();
        updateQuery.set(OpLocationFacilityEntity::getDeleted, 1);
        updateQuery.in(OpLocationFacilityEntity::getId, idList);
        return update(updateQuery);
    }

    @Override
    public Boolean delOpLocationFacilityByLocationId(Long locationId) {
        log.info("======== delOpLocationFacilityByLocationId in OpLocationFacilityRepositoryImpl faction params:{}", locationId);
        if (locationId == null) {
            return false;
        }
        LambdaUpdateWrapper<OpLocationFacilityEntity> updateQuery = new LambdaUpdateWrapper<>();
        updateQuery.set(OpLocationFacilityEntity::getDeleted, 1);
        updateQuery.eq(OpLocationFacilityEntity::getLocationId, locationId);
        return update(updateQuery);
    }

    @Override
    public List<OpLocationFacilityDTO> selectOpLocationFacilityListByLocationId(Long locationId) {
        log.info("======== selectOpLocationFacilityListByLocationId in OpLocationFacilityRepositoryImpl faction params:{}", locationId);
        List<OpLocationFacilityDTO> opLocationFacilityDTOList = new ArrayList<>();
        if (locationId == null) {
            return opLocationFacilityDTOList;
        }
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String language = request.getHeader("accept-language");
        OpLocationIconDTO opLocationIconDTO = new OpLocationIconDTO();
        opLocationIconDTO.setLanguage(language);
        opLocationIconDTO.setCategory("1");
        List<OpLocationIconVO> opLocationIconVOList = opLocationIconRepository.getAllIcon(opLocationIconDTO);
        LambdaQueryWrapper<OpLocationFacilityEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLocationFacilityEntity::getDeleted, 0);
        queryWrapper.eq(OpLocationFacilityEntity::getLocationId, locationId);
        List<OpLocationFacilityEntity> opLocationFacilityEntityList = list(queryWrapper);
        opLocationFacilityEntityList.forEach(opLocationFacilityEntity -> {
            OpLocationIconVO opLocationIconVO = opLocationIconVOList.stream()
                    .filter(iconVo -> iconVo.getCode().equals(opLocationFacilityEntity.getFacilityCode())).findFirst().orElse(null);
            OpLocationFacilityDTO temp = new OpLocationFacilityDTO();
            temp.setDeleted(opLocationFacilityEntity.getDeleted());
            temp.setCode(opLocationFacilityEntity.getFacilityCode());
            temp.setStatus(opLocationFacilityEntity.getFacilityStatus());
            temp.setId(opLocationFacilityEntity.getId());
            temp.setLocationId(opLocationFacilityEntity.getLocationId());
            temp.setCreatedAt(opLocationFacilityEntity.getCreatedAt());
            temp.setUpdatedAt(opLocationFacilityEntity.getUpdatedAt());
            if (null != opLocationIconVO) {
                temp.setDesc(opLocationIconVO.getDesc());
                temp.setName(opLocationIconVO.getName());
                temp.setIconUrl(opLocationIconVO.getIconUrl());
            }
            opLocationFacilityDTOList.add(temp);
        });
        Collections.sort(opLocationFacilityDTOList);
        return opLocationFacilityDTOList;
    }

    @Override
    public List<OpLocationFacilityDTO> selectOpLocationFacilityListByLocationIdList(List<Long> locationIdList) {
        log.info("======== selectOpLocationFacilityListByLocationIdList in OpLocationFacilityRepositoryImpl faction params:{}", locationIdList);
        List<OpLocationFacilityDTO> opLocationFacilityDTOList = new ArrayList<>();
        if (CollUtil.isEmpty(locationIdList)) {
            return opLocationFacilityDTOList;
        }
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String language = request.getHeader("accept-language");
        OpLocationIconDTO opLocationIconDTO = new OpLocationIconDTO();
        opLocationIconDTO.setLanguage(language);
        opLocationIconDTO.setCategory("1");
        List<OpLocationIconVO> opLocationIconVOList = opLocationIconRepository.getAllIcon(opLocationIconDTO);
        LambdaQueryWrapper<OpLocationFacilityEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(OpLocationFacilityEntity::getDeleted, 0);
        queryWrapper.in(OpLocationFacilityEntity::getLocationId, locationIdList);
        List<OpLocationFacilityEntity> opLocationFacilityEntityList = list(queryWrapper);
        opLocationFacilityEntityList.forEach(opLocationFacilityEntity -> {
            OpLocationIconVO opLocationIconVO = opLocationIconVOList.stream()
                    .filter(iconVo -> iconVo.getCode().equals(opLocationFacilityEntity.getFacilityCode())).findFirst().orElse(null);
            OpLocationFacilityDTO temp = new OpLocationFacilityDTO();
            temp.setDeleted(opLocationFacilityEntity.getDeleted());
            temp.setCode(opLocationFacilityEntity.getFacilityCode());
            temp.setStatus(opLocationFacilityEntity.getFacilityStatus());
            temp.setId(opLocationFacilityEntity.getId());
            temp.setLocationId(opLocationFacilityEntity.getLocationId());
            temp.setCreatedAt(opLocationFacilityEntity.getCreatedAt());
            temp.setUpdatedAt(opLocationFacilityEntity.getUpdatedAt());
            if (null != opLocationIconVO) {
                temp.setDesc(opLocationIconVO.getDesc());
                temp.setName(opLocationIconVO.getName());
                temp.setIconUrl(opLocationIconVO.getIconUrl());
            }
            opLocationFacilityDTOList.add(temp);
        });
        return opLocationFacilityDTOList;
    }
}
