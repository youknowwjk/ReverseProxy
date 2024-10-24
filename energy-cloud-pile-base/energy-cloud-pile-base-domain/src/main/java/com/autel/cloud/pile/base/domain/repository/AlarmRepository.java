package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.AlarmDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmEntity;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface AlarmRepository extends IService<TbAlarmEntity> {
    Boolean addAlarm(AlarmDTO alarmDTO);

    PageVO<AlarmDTO> selectAlarmPage(AlarmDTO alarmDTO);

    List<TbAlarmEntity> selectAlarmList(AlarmDTO alarmDTO);

    Boolean delAlarmByEvseSn(String evseSn);
}
