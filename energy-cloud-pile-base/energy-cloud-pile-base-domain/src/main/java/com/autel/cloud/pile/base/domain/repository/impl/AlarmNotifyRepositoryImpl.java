package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.AlarmNotifyRepository;
import com.autel.cloud.pile.base.dto.AlarmNotifyDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.TbAlarmNotifyMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmNotifyEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Service
@Log4j2
public class AlarmNotifyRepositoryImpl extends ServiceImpl<TbAlarmNotifyMapper, TbAlarmNotifyEntity> implements AlarmNotifyRepository {
    @Override
    public Boolean addAlarmNotify(AlarmNotifyDTO alarmNotifyDTO) {
        if (Boolean.TRUE.equals(isExistedAlarmNotify(alarmNotifyDTO))) {
            throw new RuntimeException("该告警等级的推送方式已存在");
        }
        TbAlarmNotifyEntity alarmNotifyEntity = new TbAlarmNotifyEntity();
        BeanUtil.copyProperties(alarmNotifyDTO, alarmNotifyEntity);
        alarmNotifyEntity.setCreateId(0L);
        alarmNotifyEntity.setCreateTime(new Date());
        return this.save(alarmNotifyEntity);
    }

    @Override
    public Boolean updateAlarmNotify(AlarmNotifyDTO alarmNotifyDTO) {
        if (Boolean.TRUE.equals(isExistedAlarmNotify(alarmNotifyDTO))) {
            throw new RuntimeException("该告警等级的推送方式已存在");
        }
        TbAlarmNotifyEntity alarmNotifyEntity = new TbAlarmNotifyEntity();
        BeanUtil.copyProperties(alarmNotifyDTO, alarmNotifyEntity);
        return this.updateById(alarmNotifyEntity);
    }

    @Override
    public Boolean delAlarmNotify(Long id) {
        return removeById(id);
    }

    @Override
    public PageVO<AlarmNotifyDTO> selectAlarmNotifyPage(AlarmNotifyDTO alarmNotifyDTO) {
        PageVO<AlarmNotifyDTO> response = new PageVO<>();
        com.baomidou.mybatisplus.extension.plugins.pagination.Page<TbAlarmNotifyEntity> page = new com.baomidou.mybatisplus.extension.plugins.pagination.Page(alarmNotifyDTO.getPage(), alarmNotifyDTO.getPageSize());
        LambdaQueryWrapper<TbAlarmNotifyEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbAlarmNotifyEntity::getAlarmLevel, alarmNotifyDTO.getAlarmLevel());
        queryWrapper.eq(TbAlarmNotifyEntity::getPushType, alarmNotifyDTO.getPushType());
        queryWrapper.ne(null != alarmNotifyDTO.getId(), TbAlarmNotifyEntity::getId, alarmNotifyDTO.getId());
        queryWrapper.eq(TbAlarmNotifyEntity::getOperatorId, alarmNotifyDTO.getOperatorId());
        queryWrapper.eq(TbAlarmNotifyEntity::getDeleted, Boolean.FALSE);
        page(page, queryWrapper);
        List<AlarmNotifyDTO> alarmNotifyDTOList = new ArrayList<>();
        page.getRecords().stream().forEach(alarmNotifyEntity -> {
            AlarmNotifyDTO temp = new AlarmNotifyDTO();
            BeanUtil.copyProperties(alarmNotifyEntity, temp);
            alarmNotifyDTOList.add(temp);
        });
        response.setPageSize(alarmNotifyDTO.getPageSize());
        response.setPage(alarmNotifyDTO.getPage());
        response.setTotalRows(page.getTotal());
        response.setTotalPages(page.getPages());
        response.setContent(alarmNotifyDTOList);
        return response;
    }

    @Override
    public List<TbAlarmNotifyEntity> selectAlarmNotify(AlarmNotifyDTO alarmNotifyDTO) {
        LambdaQueryWrapper<TbAlarmNotifyEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(null != alarmNotifyDTO.getAlarmLevel(), TbAlarmNotifyEntity::getAlarmLevel, alarmNotifyDTO.getAlarmLevel());
        queryWrapper.eq(null != alarmNotifyDTO.getPushType(), TbAlarmNotifyEntity::getPushType, alarmNotifyDTO.getPushType());
        queryWrapper.ne(null != alarmNotifyDTO.getId(), TbAlarmNotifyEntity::getId, alarmNotifyDTO.getId());
        queryWrapper.eq(TbAlarmNotifyEntity::getOperatorId, alarmNotifyDTO.getOperatorId());
        queryWrapper.eq(TbAlarmNotifyEntity::getDeleted, Boolean.FALSE);
        return list(queryWrapper);
    }

    /**
     * 同一告警等级的推送方式是否重复
     *
     * @param alarmNotifyDTO
     * @return
     */
    private Boolean isExistedAlarmNotify(AlarmNotifyDTO alarmNotifyDTO) {
        LambdaQueryWrapper<TbAlarmNotifyEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbAlarmNotifyEntity::getAlarmLevel, alarmNotifyDTO.getAlarmLevel());
        queryWrapper.eq(TbAlarmNotifyEntity::getPushType, alarmNotifyDTO.getPushType());
        queryWrapper.ne(null != alarmNotifyDTO.getId(), TbAlarmNotifyEntity::getId, alarmNotifyDTO.getId());
        queryWrapper.eq(TbAlarmNotifyEntity::getOperatorId, alarmNotifyDTO.getOperatorId());
        queryWrapper.eq(TbAlarmNotifyEntity::getDeleted, Boolean.FALSE);
        if (CollUtil.isNotEmpty(list(queryWrapper))) {
            return true;
        }
        return false;
    }
}
