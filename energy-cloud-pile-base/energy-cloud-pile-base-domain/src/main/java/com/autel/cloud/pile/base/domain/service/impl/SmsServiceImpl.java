package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.http.HttpUtil;
import com.autel.cloud.pile.base.domain.service.SmsService;
import com.autel.cloud.pile.base.dto.SmsSendDTO;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

/**
 * 短信发送
 */
@Service
@Log4j2
public class SmsServiceImpl implements SmsService {
    @Value(value = "${smsUrl}")
    private String smsUrl;

    @Value(value = "${smsSign}")
    private String smsSign;

    @Value(value = "${smsTemplateCode}")
    private String smsTemplateCode;

    @Override
    public void sendSms(SmsSendDTO smsSendDTO) {
        smsSendDTO.setSmsTemplate(smsTemplateCode);
        smsSendDTO.setSignName(smsSign);
        log.info("****** sms send info:{}", smsSendDTO.toString());
        HttpUtil.post(smsUrl, smsSendDTO.toString());
    }
}
