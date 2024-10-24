package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.AlarmNotifyDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmNotifyEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface AlarmNotifyRepository extends IService<TbAlarmNotifyEntity> {
    Boolean addAlarmNotify(AlarmNotifyDTO alarmNotifyDTO);

    Boolean updateAlarmNotify(AlarmNotifyDTO alarmNotifyDTO);

    Boolean delAlarmNotify(Long id);

    PageVO<AlarmNotifyDTO> selectAlarmNotifyPage(AlarmNotifyDTO alarmNotifyDTO);


    List<TbAlarmNotifyEntity> selectAlarmNotify(AlarmNotifyDTO alarmNotifyDTO);
}
