package com.autel.cloud.pile.base.infrastructure.feign.adapter;


import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.webhook.dto.Markdown;
import com.autel.cloud.base.webhook.message.MarkdownMessage;
import com.autel.cloud.home.feign.HomePileClient;
import com.autel.cloud.pile.base.vo.UserVehicle;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class WebHookServiceAdapter extends AbstractFeignServiceAdapter {

    // https://qyapi.weixin.qq.com/cgi-bin/webhook/send?key=6a8280ed-b1e0-4b0e-ba75-429e766c9176
    @Value("${webhook.wechat.ics.key:6a8280ed-b1e0-4b0e-ba75-429e766c9176}")
    protected String webhookWechatKey;


    @Resource
    private HomePileClient homePileClient;

    private static final String MSG = " [ %s ] [ %s ] %s";

    private static final String USER_VEHICLE_TEMP = "%s %s %s";

    private static final String CONTENT_TEMP = "智能充电监控\n" +
            "            >环境: <font color=\"comment\"> %s</font>\n" +
            "            >事务: <font color=\"warning\"> %s</font>\n" +
            "            >SN:<font color=\"comment\">%s</font>\n" +
            "            >订单: <font color=\"info\"> %s</font>\n" +
            "            >消息: <font color=\"comment\"> %s</font>";


    private static final String IDENTIFIED_CONTENT_TEMP = "智能充电监控\n" +
            "            >环境: <font color=\"comment\"> %s</font>\n" +
            "            >事务: <font color=\"warning\"> %s</font>\n" +
            "            >SN:<font color=\"comment\">%s</font>\n" +
            "            >订单: <font color=\"info\"> %s</font>\n" +
            "            >车辆: <font color=\"comment\"> %s</font>\n" +
            "            >消息: <font color=\"comment\"> %s</font>";

    /**
     * 信息
     */
    private final LoadingCache<Long, Optional<String>> userVehicleNameCache = CacheBuilder.newBuilder()
            .expireAfterWrite(8, TimeUnit.HOURS)
            .initialCapacity(10)
            .maximumSize(100)
            .build(new CacheLoader<Long, Optional<String>>() {
                @Override
                public Optional<String> load(Long id) {
                    UserVehicle userVehicle = selectById(id);
                    return Optional.ofNullable(String.format(USER_VEHICLE_TEMP, userVehicle.getMake(), userVehicle.getModel(), userVehicle.getYear()));
                }
            });


    public UserVehicle selectById(Long userVehicleId) {
        log.info("selectById {}", userVehicleId);
        Result<UserVehicle> result = homePileClient.queryUserVehicle(userVehicleId);
        log.info("selectById result {}", JSON.toJSONString(result));
        return nullableHandle(result);
    }

    public String getUserVehicleName(Long userVehicleId) {
        try {
            if (Objects.isNull(userVehicleId)) {
                return "未知";
            }
            Optional<String> userVehicle = userVehicleNameCache.get(userVehicleId);
            return userVehicle.orElse("未知");
        } catch (ExecutionException e) {
            log.error("getUserVehicleName:" + userVehicleId, e);
        }
        return Optional.ofNullable(selectById(userVehicleId)).map(UserVehicle::getName).orElse("未知");
    }

    public void sendMessage(String transactionId, String evseSn, String orderNumber, String msg) {
        weChatClient.sendMessage(buildMarkdownMessage(transactionId, evseSn, orderNumber, msg), webhookWechatKey);
    }

    public void sendMessage(String transactionId, String evseSn, String orderNumber, Long vehicleId, String msg) {
        sendMessage(transactionId, evseSn, orderNumber, getUserVehicleName(vehicleId), msg);
    }

    public void sendMessage(String transactionId, String evseSn, String orderNumber, String vehicle, String msg) {
        weChatClient.sendMessage(buildMarkdownMessage(transactionId, evseSn, orderNumber, vehicle, msg), webhookWechatKey);
    }

    protected MarkdownMessage buildMarkdownMessage(String transactionId, String evseSN, String orderNumber, String message) {
        Markdown text = new Markdown();
        text.setContent(String.format(CONTENT_TEMP, getEnvironment(), transactionId, evseSN, orderNumber, message));
        return new MarkdownMessage(text);
    }

    protected MarkdownMessage buildMarkdownMessage(String transactionId, String evseSN, String orderNumber, String vehicle, String message) {
        Markdown text = new Markdown();
        text.setContent(String.format(IDENTIFIED_CONTENT_TEMP, getEnvironment(), transactionId, evseSN, orderNumber, vehicle, message));
        return new MarkdownMessage(text);
    }

    /**
     * 信息
     */
    public void sendMessage(String content) {
        weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
    }

}
