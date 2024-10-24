package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.AlarmLevelTypeRelationRepository;
import com.autel.cloud.pile.base.domain.service.AlarmLevelTypeRelationService;
import com.autel.cloud.pile.base.dto.AlarmLevelTypeRelationDTO;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class AlarmLevelTypeRelationServiceImpl implements AlarmLevelTypeRelationService {

    private final AlarmLevelTypeRelationRepository alarmLevelTypeRelationRepository;

    public AlarmLevelTypeRelationServiceImpl(AlarmLevelTypeRelationRepository alarmLevelTypeRelationRepository) {
        this.alarmLevelTypeRelationRepository = alarmLevelTypeRelationRepository;
    }

    @Override
    public Boolean addAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        return alarmLevelTypeRelationRepository.addAlarmLevelTypeRelation(alarmLevelTypeRelationDTO);
    }

    @Override
    public Boolean updateAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        return alarmLevelTypeRelationRepository.updateAlarmLevelTypeRelation(alarmLevelTypeRelationDTO);
    }

    @Override
    public Boolean delAlarmLevelTypeRelation(Long id) {
        return alarmLevelTypeRelationRepository.delAlarmLevelTypeRelation(id);
    }

    @Override
    public PageVO<AlarmLevelTypeRelationDTO> selectAlarmLevelTypeRelationPage(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO) {
        return alarmLevelTypeRelationRepository.selectAlarmLevelTypeRelationPage(alarmLevelTypeRelationDTO);
    }
}
