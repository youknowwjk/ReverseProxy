package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.AlarmLevelTypeRelationRepository;
import com.autel.cloud.pile.base.dto.AlarmLevelTypeRelationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.TbAlarmLevelTypeRelationMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmLevelTypeRelationEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Service
@Log4j2
public class AlarmLevelTypeRelationRepositoryImpl extends ServiceImpl<TbAlarmLevelTypeRelationMapper, TbAlarmLevelTypeRelationEntity> implements AlarmLevelTypeRelationRepository {

    @Override
    public Boolean addAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        if (Boolean.TRUE.equals(isExistedAlarmTypeInLevel(alarmLevelTypeRelationDTO))) {
            throw new RuntimeException("该告警类型已设置级别");
        }
        TbAlarmLevelTypeRelationEntity alarmLevelTypeRelationEntity = new TbAlarmLevelTypeRelationEntity();
        BeanUtil.copyProperties(alarmLevelTypeRelationDTO, alarmLevelTypeRelationEntity);
        alarmLevelTypeRelationEntity.setId(IdWorker.getId(TbAlarmLevelTypeRelationEntity.class));
        alarmLevelTypeRelationEntity.setDeleted(0);
        alarmLevelTypeRelationEntity.setCreateId(0L);
        alarmLevelTypeRelationEntity.setCreateTime(new Date());
        return this.save(alarmLevelTypeRelationEntity);
    }

    @Override
    public Boolean updateAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        if (Boolean.TRUE.equals(isExistedAlarmTypeInLevel(alarmLevelTypeRelationDTO))) {
            throw new RuntimeException("该告警类型已设置级别");
        }
        TbAlarmLevelTypeRelationEntity alarmLevelTypeRelationEntity = new TbAlarmLevelTypeRelationEntity();
        BeanUtil.copyProperties(alarmLevelTypeRelationDTO, alarmLevelTypeRelationEntity);
        return this.updateById(alarmLevelTypeRelationEntity);
    }

    @Override
    public Boolean delAlarmLevelTypeRelation(Long id) {
        return this.removeById(id);
    }

    @Override
    public PageVO<AlarmLevelTypeRelationDTO> selectAlarmLevelTypeRelationPage(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        PageVO<AlarmLevelTypeRelationDTO> response = new PageVO<>();
        com.baomidou.mybatisplus.extension.plugins.pagination.Page<TbAlarmLevelTypeRelationEntity> page = new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(alarmLevelTypeRelationDTO.getPage(), alarmLevelTypeRelationDTO.getPageSize());
        LambdaQueryWrapper<TbAlarmLevelTypeRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StrUtil.isNotBlank(alarmLevelTypeRelationDTO.getAlarmType()), TbAlarmLevelTypeRelationEntity::getAlarmType, alarmLevelTypeRelationDTO.getAlarmType());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getOperatorId, alarmLevelTypeRelationDTO.getOperatorId());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getDeleted, Boolean.FALSE);
        page(page, queryWrapper);
        List<AlarmLevelTypeRelationDTO> alarmLevelTypeRelationDTOList = new ArrayList<>();
        page.getRecords().forEach(alarmLevelTypeRelationEntity -> {
            AlarmLevelTypeRelationDTO temp = new AlarmLevelTypeRelationDTO();
            BeanUtil.copyProperties(alarmLevelTypeRelationEntity, temp);
            alarmLevelTypeRelationDTOList.add(temp);
        });
        response.setPageSize(alarmLevelTypeRelationDTO.getPageSize());
        response.setPage(alarmLevelTypeRelationDTO.getPage());
        response.setContent(alarmLevelTypeRelationDTOList);
        return response;
    }

    @Override
    public AlarmLevelTypeRelationDTO selectAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        LambdaQueryWrapper<TbAlarmLevelTypeRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StrUtil.isNotBlank(alarmLevelTypeRelationDTO.getAlarmType()), TbAlarmLevelTypeRelationEntity::getAlarmType, alarmLevelTypeRelationDTO.getAlarmType());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getOperatorId, alarmLevelTypeRelationDTO.getOperatorId());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getDeleted, Boolean.FALSE);
        AlarmLevelTypeRelationDTO response = new AlarmLevelTypeRelationDTO();
        TbAlarmLevelTypeRelationEntity alarmLevelTypeRelationEntity = this.getOne(queryWrapper);
        BeanUtil.copyProperties(alarmLevelTypeRelationEntity, response);
        return response;
    }

    public List<TbAlarmLevelTypeRelationEntity> selectAlarmLevelTypeRelationList(TbAlarmLevelTypeRelationEntity alarmLevelTypeRelationEntity) {
        LambdaQueryWrapper<TbAlarmLevelTypeRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StrUtil.isNotBlank(alarmLevelTypeRelationEntity.getAlarmType()), TbAlarmLevelTypeRelationEntity::getAlarmType, alarmLevelTypeRelationEntity.getAlarmType());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getOperatorId, alarmLevelTypeRelationEntity.getOperatorId());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getDeleted, Boolean.FALSE);
        return list(queryWrapper);
    }

    private Boolean isExistedAlarmTypeInLevel(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        LambdaQueryWrapper<TbAlarmLevelTypeRelationEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StrUtil.isNotBlank(alarmLevelTypeRelationDTO.getAlarmType()), TbAlarmLevelTypeRelationEntity::getAlarmType, alarmLevelTypeRelationDTO.getAlarmType());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getOperatorId, alarmLevelTypeRelationDTO.getOperatorId());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getDeleted, 0);
        queryWrapper.ne(null != alarmLevelTypeRelationDTO.getId(), TbAlarmLevelTypeRelationEntity::getId, alarmLevelTypeRelationDTO.getId());
        queryWrapper.eq(TbAlarmLevelTypeRelationEntity::getDeleted, Boolean.FALSE);
        return CollUtil.isNotEmpty(list(queryWrapper));
    }
}
