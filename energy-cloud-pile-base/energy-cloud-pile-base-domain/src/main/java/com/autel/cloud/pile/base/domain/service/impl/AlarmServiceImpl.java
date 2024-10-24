package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import com.autel.cloud.base.data.domain.pagination.PageVO;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.AlarmService;
import com.autel.cloud.pile.base.domain.service.SmsService;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.AlarmLevelEnum;
import com.autel.cloud.pile.base.enums.AlarmPushTypeEnum;
import com.autel.cloud.pile.base.enums.SwitchEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbAlarmNotifyEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbMailboxConfigEntity;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.sun.mail.util.MailSSLSocketFactory;
import lombok.SneakyThrows;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.util.*;

@Service
@Log4j2
public class AlarmServiceImpl implements AlarmService {

    /**
     * 邮件主题模板.
     */
    private final static String EMAIL_SUBJECT_TEMPLATE = "Email Title: [%s] charging station alarm notification: [%s]";

    /**
     * 邮件正文模板.
     */
    private final static String EMAIL_CONTEXT_TEMPLATE = "Email Body: \n" +
            "Dear [%s]：\n" +
            "Your [%s] charging station, [%s] plug had a [%s] condition at [%s] with an alarm level of [%s]. Please deal with it. \n" +
            "Best regards" +
            "\n" +
            "[%s]\n" +
            "Sent on:[%s]\n" +
            "System email. Please do not reply it.";

    private final AlarmRepository alarmRepository;

    private final OpLocationRepository opLocationRepository;

    private final OpLocationEvseRepository opLocationEvseRepository;

    private final AlarmNotifyRepository alarmNotifyRepository;

    private final AlarmLevelTypeRelationRepository alarmLevelTypeRelationRepository;

    private final MailboxConfigRepository mailboxConfigRepository;

    private final SmsService smsServiceImpl;

    public AlarmServiceImpl(AlarmRepository alarmRepository,
                            OpLocationRepository opLocationRepository,
                            OpLocationEvseRepository opLocationEvseRepository,
                            AlarmNotifyRepository alarmNotifyRepository,
                            AlarmLevelTypeRelationRepository alarmLevelTypeRelationRepository,
                            MailboxConfigRepository mailboxConfigRepository,
                            SmsService smsServiceImpl) {
        this.alarmRepository = alarmRepository;
        this.opLocationRepository = opLocationRepository;
        this.opLocationEvseRepository = opLocationEvseRepository;
        this.alarmNotifyRepository = alarmNotifyRepository;
        this.alarmLevelTypeRelationRepository = alarmLevelTypeRelationRepository;
        this.mailboxConfigRepository = mailboxConfigRepository;
        this.smsServiceImpl = smsServiceImpl;
    }

    @Override
    public Boolean addAlarm(AlarmDTO alarmDTO) {
        OpEvseInfoDTO opEvseInfoDTO = opLocationEvseRepository.getEvseByEvseSn(alarmDTO.getEvseSn());
        if (null == opEvseInfoDTO) {
            log.error("******** addAlarm failed in AlarmServiceImpl, the opEvseInfoDTO is null, the alarmDTO:{}", alarmDTO);
            return Boolean.FALSE;
        }

        OpLocationEntity opLocationEntity = opLocationRepository.selectOpLocationEntityById(opEvseInfoDTO.getLocationId());
        if (null == opEvseInfoDTO) {
            log.error("******** addAlarm failed in AlarmServiceImpl, the opLocationEntity is null, the alarmDTO:{}", alarmDTO);
            return Boolean.FALSE;
        }

        // 告警等级
        AlarmLevelTypeRelationDTO alarmLevelTypeRelationQuery = new AlarmLevelTypeRelationDTO();
        alarmLevelTypeRelationQuery.setAlarmType(alarmDTO.getAlarmType());
        alarmLevelTypeRelationQuery.setOperatorId(opLocationEntity.getOperatorId());
        AlarmLevelTypeRelationDTO alarmLevelTypeRelationDTO = alarmLevelTypeRelationRepository.selectAlarmLevelTypeRelation(alarmLevelTypeRelationQuery);
        if (null == alarmLevelTypeRelationDTO) {
            log.error("******** addAlarm failed in AlarmServiceImpl, the alarmLevelTypeRelationDTO is null, the alarmDTO:{}", alarmDTO);
            return Boolean.FALSE;
        }

        AlarmNotifyDTO queryAlarmNotify = new AlarmNotifyDTO();
        queryAlarmNotify.setAlarmLevel(alarmLevelTypeRelationDTO.getAlarmLevel());
        queryAlarmNotify.setOperatorId(alarmLevelTypeRelationDTO.getOperatorId());
        List<TbAlarmNotifyEntity> alarmNotifyEntityList = alarmNotifyRepository.selectAlarmNotify(queryAlarmNotify);
        //todo 消息推送
        alarmNotifyEntityList.forEach(alarmNotifyEntity -> {
            if (SwitchEnum.OPEN.getCode().equals(alarmNotifyEntity.getPushSwitch())) {
                String alarmLevelName = AlarmLevelEnum.getAlarmLevelEnum(alarmLevelTypeRelationDTO.getAlarmLevel()).getName();
                if (AlarmPushTypeEnum.EMAIL.getCode().equals(alarmNotifyEntity.getPushType())) {
                    TbMailboxConfigEntity tbMailboxConfigEntity = mailboxConfigRepository.selectMailboxByOperatorId(alarmNotifyEntity.getOperatorId());
                    if (SwitchEnum.OPEN.getCode().equals(tbMailboxConfigEntity.getActiveSwitch())) {
                        String mailSubject = String.format(EMAIL_SUBJECT_TEMPLATE, opLocationEntity.getName(), alarmLevelName);
                        String mailContext = String.format(EMAIL_CONTEXT_TEMPLATE,
                                alarmNotifyEntity.getContactName(),
                                opLocationEntity.getName(),
                                alarmDTO.getEvseSn(),
                                alarmLevelName,
                                DateUtil.formatDateTime(alarmDTO.getCreateTime()),
                                alarmDTO.getAlarmType(),
                                "",
                                DateUtil.date()
                        );
                        sendEmail(tbMailboxConfigEntity, alarmNotifyEntity.getContactInfo(), mailSubject, mailContext);
                    }
                } else {
                    SmsSendDTO smsSendDTO = new SmsSendDTO();
                    //todo 手机号
                    smsSendDTO.setPhoneNumber(alarmNotifyEntity.getContactInfo());
                    // 短信body
                    Map<String, String> map = new HashMap<>(8);
                    map.put("shopName", alarmNotifyEntity.getOperatorId() + "");
                    map.put("stationName", opLocationEntity.getName());
                    map.put("pileName", alarmDTO.getEvseSn());
                    map.put("sn", alarmDTO.getEvseSn());
                    map.put("gunNum", alarmDTO.getEvseSn());
                    map.put("time", DateUtil.formatDateTime(alarmDTO.getCreateTime()));
                    map.put("warningType", alarmDTO.getAlarmType());
                    map.put("warningLevel", alarmLevelName);
                    smsSendDTO.setParam(map);
                    smsServiceImpl.sendSms(smsSendDTO);
                }
            }
        });

        alarmDTO.setLocationId(opEvseInfoDTO.getLocationId());
        alarmDTO.setEvseId(opEvseInfoDTO.getId());
        alarmDTO.setAlarmLevel(alarmLevelTypeRelationDTO.getAlarmLevel());
        alarmDTO.setId(IdWorker.getId(TbAlarmEntity.class));
        alarmDTO.setLocationName(opLocationEntity.getName());
        alarmDTO.setTimeZone(opLocationEntity.getTimeZone());
        alarmDTO.setLocationTime(alarmDTO.getCreateTime());
        alarmDTO.setOperatorId(opLocationEntity.getOperatorId());
        alarmDTO.setCreateId(0L);
        return alarmRepository.addAlarm(alarmDTO);
    }

    @Override
    public PageVO<AlarmDTO> selectAlarmPage(AlarmDTO alarmDTO) {
        return alarmRepository.selectAlarmPage(alarmDTO);
    }

    @Override
    public List<AlarmDTO> selectAlarmList(AlarmDTO alarmDTO) {
        List<TbAlarmEntity> alarmEntityList = alarmRepository.selectAlarmList(alarmDTO);
        List<AlarmDTO> alarmDTOList = new ArrayList<>(alarmEntityList.size());
        alarmEntityList.stream().forEach(tbAlarmEntity -> {
            AlarmDTO temp = new AlarmDTO();
            BeanUtil.copyProperties(tbAlarmEntity, temp);
            alarmDTOList.add(temp);
        });
        return alarmDTOList;
    }

    @Override
    public Boolean delAlarmByEvseSn(String evseSn) {
        return alarmRepository.delAlarmByEvseSn(evseSn);
    }

    @SneakyThrows
    private Session buildMailSession(String host, String port, String protocol, String email, String authCode) {
        // 跟smtp服务器建立一个连接
        Properties properties = new Properties();
        properties.setProperty("mail.debug", "true");
        // 发送服务器需要身份验证,要采用指定用户名密码的方式去认证
        properties.setProperty("mail.smtp.auth", "true");
        // 设置邮件服务器主机名
        properties.setProperty("mail.host", host);//"smtp.qq.com"
        // 发送邮件协议名称
        properties.setProperty("mail.transport.protocol", protocol);//"smtp"
        // 开启SSL加密，否则会失败
        MailSSLSocketFactory sslSocketFactory = new MailSSLSocketFactory();
        sslSocketFactory.setTrustAllHosts(true);
        properties.put("mail.smtp.ssl.enable", "true");
        properties.put("mail.smtp.ssl.socketFactory", sslSocketFactory);

        // 创建session
        Session session =
                Session.getDefaultInstance(
                        properties,
                        new Authenticator() {
                            @Override
                            protected PasswordAuthentication getPasswordAuthentication() {
                                // 用户名可以用QQ账号也可以用邮箱的别名:第一个参数为邮箱账号,第二个为授权码
                                PasswordAuthentication passwordAuthentication =
                                        new PasswordAuthentication(email, authCode);//"185758815@qq.com" //shmvbhazwokrbjha
                                return passwordAuthentication;
                            }
                        });
        // 设置打开调试状态
        session.setDebug(true);
        log.info("******** successful build email session, the sender : {}", email);
        return session;
    }

    @SneakyThrows
    private void sendEmail(TbMailboxConfigEntity tbMailboxConfigEntity, String toMail, String subject, String content) {
        Session session = this.buildMailSession(tbMailboxConfigEntity.getHost(),
                tbMailboxConfigEntity.getPort(),
                tbMailboxConfigEntity.getProtocol(),
                tbMailboxConfigEntity.getEmail(),
                tbMailboxConfigEntity.getAuthCode());
        // 声明一个 Message 对象(代表一封邮件),从session中创建
        MimeMessage msg = new MimeMessage(session);
        // 邮件信息封装
        msg.setFrom(new InternetAddress(tbMailboxConfigEntity.getEmail()));
        msg.setRecipient(Message.RecipientType.TO, new InternetAddress(toMail)); //user "c472279240@gmail.com"

        msg.setSubject(subject);
        msg.setText(content);
        // 保存并生成最终的邮件内容
        msg.saveChanges();
        try {
            // 发送动作
            Transport.send(msg);
        } catch (Exception e) {
            log.error("******** send email failure, the email from:{}, to:{}, email content:{}", tbMailboxConfigEntity.getEmail(), toMail, content);
        }
    }
}
