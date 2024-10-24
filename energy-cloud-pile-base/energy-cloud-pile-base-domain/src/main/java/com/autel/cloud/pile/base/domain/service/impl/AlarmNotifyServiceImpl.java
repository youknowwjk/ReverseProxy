package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.AlarmNotifyRepository;
import com.autel.cloud.pile.base.domain.service.AlarmNotifyService;
import com.autel.cloud.pile.base.dto.AlarmNotifyDTO;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class AlarmNotifyServiceImpl implements AlarmNotifyService {
    private final AlarmNotifyRepository alarmNotifyRepository;

    public AlarmNotifyServiceImpl(AlarmNotifyRepository alarmNotifyRepository) {
        this.alarmNotifyRepository = alarmNotifyRepository;
    }


    @Override
    public Boolean addAlarmNotify(AlarmNotifyDTO alarmNotifyDTO) {
        return alarmNotifyRepository.addAlarmNotify(alarmNotifyDTO);
    }

    @Override
    public Boolean updateAlarmNotify(AlarmNotifyDTO alarmNotifyDTO) {
        return alarmNotifyRepository.updateAlarmNotify(alarmNotifyDTO);
    }

    @Override
    public Boolean delAlarmNotify(Long id) {
        return alarmNotifyRepository.delAlarmNotify(id);
    }

    @Override
    public PageVO<AlarmNotifyDTO> selectAlarmNotifyPage(AlarmNotifyDTO alarmNotifyDTO) {
        return alarmNotifyRepository.selectAlarmNotifyPage(alarmNotifyDTO);
    }
}
