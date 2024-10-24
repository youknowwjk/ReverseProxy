package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.AlarmRepository;
import com.autel.cloud.pile.base.dto.AlarmDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.TbAlarmMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
@Log4j2
public class AlarmRepositoryImpl extends ServiceImpl<TbAlarmMapper, TbAlarmEntity> implements AlarmRepository {
    @Override
    public Boolean addAlarm(AlarmDTO alarmDTO) {
        TbAlarmEntity alarmEntity = new TbAlarmEntity();
        BeanUtil.copyProperties(alarmDTO, alarmEntity);
        return save(alarmEntity);
    }

    @Override
    public PageVO<AlarmDTO> selectAlarmPage(AlarmDTO alarmDTO) {
        PageVO<AlarmDTO> response = new PageVO<>();
        com.baomidou.mybatisplus.extension.plugins.pagination.Page<TbAlarmEntity> page = new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(alarmDTO.getPage(), alarmDTO.getPageSize());
        LambdaQueryWrapper<TbAlarmEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(null != alarmDTO.getAlarmLevel(), TbAlarmEntity::getAlarmLevel, alarmDTO.getAlarmLevel());
        queryWrapper.eq(StrUtil.isNotBlank(alarmDTO.getAlarmType()), TbAlarmEntity::getAlarmType, alarmDTO.getAlarmType());
        page(page, queryWrapper);
        List<AlarmDTO> alarmDTOList = new ArrayList<>();
        page.getRecords().forEach(tbAlarmEntity -> {
            AlarmDTO temp = new AlarmDTO();
            BeanUtil.copyProperties(tbAlarmEntity, temp);
            alarmDTOList.add(temp);
        });
        response.setContent(alarmDTOList);
        response.setPage(alarmDTO.getPage());
        response.setPageSize(alarmDTO.getPageSize());
        return response;
    }

    @Override
    public List<TbAlarmEntity> selectAlarmList(AlarmDTO alarmDTO) {
        LambdaQueryWrapper<TbAlarmEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(null != alarmDTO.getAlarmLevel(), TbAlarmEntity::getAlarmLevel, alarmDTO.getAlarmLevel());
        queryWrapper.eq(StrUtil.isNotBlank(alarmDTO.getAlarmType()), TbAlarmEntity::getAlarmType, alarmDTO.getAlarmType());
        queryWrapper.eq(StrUtil.isNotBlank(alarmDTO.getEvseSn()), TbAlarmEntity::getEvseSn, alarmDTO.getEvseSn());
        queryWrapper.eq(null != alarmDTO.getOperatorId(), TbAlarmEntity::getOperatorId, alarmDTO.getOperatorId());
        queryWrapper.eq(TbAlarmEntity::getDeleted, Boolean.FALSE);
        return list(queryWrapper);
    }

    @Override
    public Boolean delAlarmByEvseSn(String evseSn) {
        LambdaQueryWrapper<TbAlarmEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbAlarmEntity::getEvseSn, evseSn);
        return remove(queryWrapper);
    }
}
