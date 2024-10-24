package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.AlarmDTO;

import java.util.List;

public interface AlarmService {

    Boolean addAlarm(AlarmDTO alarmDTO);

    PageVO<AlarmDTO> selectAlarmPage(AlarmDTO alarmDTO);

    List<AlarmDTO> selectAlarmList(AlarmDTO alarmDTO);

    Boolean delAlarmByEvseSn(String evseSn);
}
