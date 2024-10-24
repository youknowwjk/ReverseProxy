package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.AlarmLevelTypeRelationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmLevelTypeRelationEntity;
import com.baomidou.mybatisplus.extension.service.IService;

public interface AlarmLevelTypeRelationRepository extends IService<TbAlarmLevelTypeRelationEntity> {

    Boolean addAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);

    Boolean updateAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);

    Boolean delAlarmLevelTypeRelation(Long id);

    PageVO<AlarmLevelTypeRelationDTO> selectAlarmLevelTypeRelationPage(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);

    AlarmLevelTypeRelationDTO selectAlarmLevelTypeRelation(AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO);
}
