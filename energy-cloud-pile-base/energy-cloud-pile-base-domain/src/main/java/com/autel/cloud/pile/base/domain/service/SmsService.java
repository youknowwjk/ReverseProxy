package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.pile.base.dto.SmsSendDTO;

public interface SmsService {

    void sendSms(SmsSendDTO smsSendDTO);
}
