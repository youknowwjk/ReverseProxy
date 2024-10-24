package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.ems.constant.RedisConstant;
import com.autel.cloud.pile.base.domain.constant.EmsConstant;
import com.autel.cloud.pile.base.domain.constant.RabbitMQConstant;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.domain.service.SellerAccountService;
import com.autel.cloud.pile.base.domain.utils.FeiShuRobotUtil;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.rabbitmq.client.Channel;
import lombok.extern.slf4j.Slf4j;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import java.util.List;
import java.util.UUID;

@Slf4j
@Component
public class EmsDelayMessageListener {

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @Resource
    private RedisUtil redisUtil;

    @Resource
    private FeiShuRobotUtil feiShuRobotUtil;

    @Resource
    private SellerAccountService sellerAccountService;

    /**
     * 消息流 mqtt --> kafka KAFKA_TRANSFER_TOPIC --> rabbit EMS_POWER_DELAY_QUEUE_NAME
     * @param message  接收的信息
     * @param channel 通道
     */
    @RabbitListener(queues = RabbitMQConstant.EMS_POWER_DELAY_QUEUE_NAME + "#{rabbitBean.rabbitmqVersionSuffix}")
    public void emsDelayMessage(Message message, Channel channel) throws Exception {
        String sn = new String(message.getBody());
        // 记录日志
        log.info("当前时间：{}，EMS延时队列收到的sn：{}",  System.currentTimeMillis(), sn);
        // 获取redis key值
        String key = String.format(EmsKafkaListener.REDIS_SN_DELAY_KEY, sn);
        // 获取value
        String s = stringRedisTemplate.opsForValue().get(key);
        long currentTimeMillis = System.currentTimeMillis();
        String uuid = UUID.randomUUID().toString();
        //分析策略的准确性
        log.info("增加策略调度分析2,sn:{},系统时间：{},消息时间:{},uuid:{}",sn,currentTimeMillis,s,uuid);
        //改判断时间（可以由Iot判断）
        if(Math.abs(currentTimeMillis - Long.valueOf(s)) > EmsConstant.ABS_EXPIRE_TIME){
            long sb = currentTimeMillis - Long.valueOf(s);
            log.info("增加策略调度分析2,执行离线策略（检测延时）,sn:{},系统时间：{},消息时间:{},时间差:{}",sn,currentTimeMillis,s,sb);

            Object groupId = redisUtil.hget(RedisConstant.REDIS_EMS_SN_GROUP_RELATION,sn);

            List<Long> groupList = (List<Long>) groupId;
            String env = sellerAccountService.queryCurrentEnv().getData();
            if(env == null){
                env = "未知环境";
            }
            //发送飞书机器人消息
            String msg = String.format(FeiShuRobotUtil.OFFLINE_MSG_REDIS, env, sn, JSON.toJSON(groupList),sb + "");
            feiShuRobotUtil.sendMsgToFeiShu(msg,sn);
            // 记录日志
            log.info("ems调用业务功率离线分配策略：{}",  sn);
            for(Long gi : groupList){
                //直接调离线处理
                opLocationPileGroupService.handleEmsGroupOffLine(gi);
            }
            String firstKey = String.format(EmsKafkaListener.REDIS_SN_ONLINE_FIRST_CHECK_KEY, sn);
            redisUtil.del(firstKey);
        }
        channel.basicAck(message.getMessageProperties().getDeliveryTag(), Boolean.FALSE);
    }

}
