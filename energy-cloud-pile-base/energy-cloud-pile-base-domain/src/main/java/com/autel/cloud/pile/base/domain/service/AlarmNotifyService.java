package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.dto.AlarmNotifyDTO;

public interface AlarmNotifyService {

    Boolean addAlarmNotify(AlarmNotifyDTO alarmNotifyDTO);

    Boolean updateAlarmNotify(AlarmNotifyDTO alarmNotifyDTO);

    Boolean delAlarmNotify(Long id);

    PageVO<AlarmNotifyDTO> selectAlarmNotifyPage(AlarmNotifyDTO alarmNotifyDTO);
}
