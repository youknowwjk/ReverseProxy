package com.autel.cloud.pile.base.domain.utils;

import com.alibaba.fastjson.JSON;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@Component
@Slf4j
public class FeiShuRobotUtil {

    public static final String WEB_HOOK = "https://open.feishu.cn/open-apis/bot/v2/hook/443c7e9c-2d25-46a3-9d51-f2df03eee584";

    @Value("${fei.shu.robot.flag:false}")
    private Boolean flag;

    public static final String OFFLINE_MSG_MSG = "调用离线策略(消息延迟调用),ENV:%s,SN:%s,GroupId:%s,时间差:%s";

    public static final String ONLINE_MSG_MSG = "调用在线策略(中断后首次调用在线策略),ENV:%s,SN:%s,GroupId:%s,时间差:%s";

    public static final String OFFLINE_MSG_REDIS = "调用离线策略(检测调用),ENV:%s,SN:%s,GroupId:%s,时间差:%s";

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    //缓存监测key
    private static final String EXPIRE_KEY = "ems:fei:shu:cache:%s";

    //飞书告警缓存过期时间
    private static final Long REDIS_FEI_SHU_ALARM_EXPIRE_TIME = 20L;

    //单位为毫秒
    private static final Long ABS_TIME = 15 * 1000L;

    /**
     * 发送飞书机器人消息
     * @param msg
     * @return
     */
    public void sendMsgToFeiShu(String msg,String sn){
        //加锁
        synchronized (this) {
            if (flag) {
                try {
                    String key = String.format(EXPIRE_KEY, sn);
                    String s = stringRedisTemplate.opsForValue().get(key);
                    if (StringUtils.isNotBlank(s) && System.currentTimeMillis() - Long.valueOf(s) < ABS_TIME) {
                        return;
                    }
                    //发送飞书消息
                    sendMsg(msg);
                    //缓存值
                    stringRedisTemplate.opsForValue().set(key, System.currentTimeMillis() + "", REDIS_FEI_SHU_ALARM_EXPIRE_TIME, TimeUnit.SECONDS);
                }catch (Exception e){
                    log.error("发送飞书消息常:{}",e);
                }
            }
        }
    }

    public void sendMsg(String msg) {
        try {
            OkHttpClient client = new OkHttpClient();
            Map<String,String> textMap = new HashMap<>();
            textMap.put("text",msg);

            RequestBody requestBody = new FormBody.Builder()
                    .add("msg_type","text")
                    .add("content", JSON.toJSONString(textMap)).build();

            Request request = new Request.Builder()
                    .url(WEB_HOOK)
                    .post(requestBody)
                    .build();
            //执行请求
            Response response = client.newCall(request).execute();
            log.info("飞书机器人消息发送成功,消息内容:{},响应内容:{}",JSON.toJSONString(msg),response);
        }catch (Exception e){
            log.error("发送消息至飞书发生异常:{}",e);
        }
    }
}
