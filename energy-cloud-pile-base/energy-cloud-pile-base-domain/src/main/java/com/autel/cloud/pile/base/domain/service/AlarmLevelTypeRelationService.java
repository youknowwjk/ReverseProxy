package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.AlarmLevelTypeRelationDTO;

public interface AlarmLevelTypeRelationService {

    Boolean addAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);

    Boolean updateAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);

    Boolean delAlarmLevelTypeRelation(Long id);

    PageVO<AlarmLevelTypeRelationDTO> selectAlarmLevelTypeRelationPage(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);
}
