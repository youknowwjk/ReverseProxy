package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.ems.constant.EmsMqTopicConstant;
import com.autel.cloud.ems.constant.RedisConstant;
import com.autel.cloud.ems.dto.PhotovoltaicStoragePowerSupplyDTO;
import com.autel.cloud.pile.base.domain.constant.EmsConstant;
import com.autel.cloud.pile.base.domain.service.DelayService;
import com.autel.cloud.pile.base.domain.service.HomeService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.domain.service.SellerAccountService;
import com.autel.cloud.pile.base.domain.utils.FeiShuRobotUtil;
import com.autel.cloud.pile.base.infrastructure.redis.RedisUtil;
import com.autel.cloud.pile.base.vo.EnergyEmsPowerInfoVO;
import com.autel.cloud.pile.bill.dto.EmsGroupPowerDTO;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.kafka.annotation.EnableKafka;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import javax.annotation.Resource;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

@Component
@EnableKafka
@Slf4j
public class EmsKafkaListener {

    @Resource
    private DelayService delayService;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    private OpLocationPileGroupService opLocationPileGroupService;

    @Resource
    private RedisUtil redisUtil;

    @Resource
    private FeiShuRobotUtil feiShuRobotUtil;

    @Resource
    private HomeService homeService;

    @Resource
    private SellerAccountService sellerAccountService;

    @Resource(name = "emsTaskExecutor")
    private Executor emsTaskExecutor;

    //延时key
    public static final String REDIS_SN_DELAY_KEY = "ems:sn:delay:%s";

    //首次在线检测
    public static final String REDIS_SN_ONLINE_FIRST_CHECK_KEY = "ems:sn:first:check:%s";

    /***
     * 数据入库、转发 mqtt --> kafka KAFKA_TRANSFER_TOPIC --> rabbit EMS_POWER_DELAY_QUEUE_NAME
     * @param messageRecord 入参
     */
    @KafkaListener(topics = EmsMqTopicConstant.KAFKA_TRANSFER_TOPIC + "#{kafkaUtil.kafkaVersionSuffix}",
            groupId = EmsMqTopicConstant.KAFKA_TRANSFER_TOPIC + "#{kafkaUtil.kafkaVersionSuffix}")
    public  void supplyTopicConsumer(ConsumerRecord<Object,Object> messageRecord){
        try{
            String message = messageRecord.value().toString();
            log.info("光储市电接收到的kafka信息：{}",message);
            //只提供SN
            PhotovoltaicStoragePowerSupplyDTO photovoltaicStoragePowerSupplyDTO = JSON.parseObject(message, PhotovoltaicStoragePowerSupplyDTO.class);
            long currentTimeMillis = System.currentTimeMillis();
            //获取groupId
            Object groupId = redisUtil.hget(RedisConstant.REDIS_EMS_SN_GROUP_RELATION,photovoltaicStoragePowerSupplyDTO.getSn());
            if (ObjectUtils.isEmpty(groupId)){
                return;
            }
            log.info("光储市电,groupId数据:{}",groupId);
            // 时间加大
            boolean isExpire = Math.abs(currentTimeMillis - photovoltaicStoragePowerSupplyDTO.getCreateTime()) > EmsConstant.ABS_EXPIRE_TIME;
            boolean isLegal = validParam(photovoltaicStoragePowerSupplyDTO);
            long sb = currentTimeMillis - photovoltaicStoragePowerSupplyDTO.getCreateTime();
            //分析策略的准确性
            log.info("增加策略调度分析1,sn:{},系统时间：{},消息时间:{},时间差:{}",photovoltaicStoragePowerSupplyDTO.getSn(),currentTimeMillis,photovoltaicStoragePowerSupplyDTO.getCreateTime(),sb);
            if(isExpire||isLegal){
                log.info("增加策略调度分析1,执行离线策略（消息延时）,sn:{},系统时间：{},消息时间:{},时间差:{}",photovoltaicStoragePowerSupplyDTO.getSn(),currentTimeMillis,
                        photovoltaicStoragePowerSupplyDTO.getCreateTime(),sb);
                List<Long> groupList = (List<Long>) groupId;
                //发送飞书机器人消息
                String env = sellerAccountService.queryCurrentEnv().getData();
                if(env == null){
                    env = "未知环境";
                }
                String msg = String.format(FeiShuRobotUtil.OFFLINE_MSG_MSG, env, photovoltaicStoragePowerSupplyDTO.getSn(), JSON.toJSON(groupList),sb +"");
                feiShuRobotUtil.sendMsgToFeiShu(msg,photovoltaicStoragePowerSupplyDTO.getSn());
                for(Long gi : groupList){
                    //直接调离线处理
                    log.info("supplyTopicConsumer isExpire is {} isLegal is {}",isExpire,isLegal);
                    String firstKey = String.format(EmsKafkaListener.REDIS_SN_ONLINE_FIRST_CHECK_KEY, photovoltaicStoragePowerSupplyDTO.getSn());
                    redisUtil.del(firstKey);
                    opLocationPileGroupService.handleEmsGroupOffLine(gi);
                }
            }else{
                //发送消息到rabbit生成延时消息检测
                String key = String.format(REDIS_SN_DELAY_KEY, photovoltaicStoragePowerSupplyDTO.getSn());
                //记录到redis,DELAY_REDIS_EXPIRE_TIME秒后过期
                stringRedisTemplate.opsForValue().set(key,System.currentTimeMillis()+"", EmsConstant.DELAY_REDIS_EXPIRE_TIME, TimeUnit.SECONDS);
                //发送消息至队列，产生延时消息
                delayService.sendDelayMessage(photovoltaicStoragePowerSupplyDTO.getSn());
                //异步执行在线策略
                emsTaskExecutor.execute(()->{
                    String firstKey = String.format(EmsKafkaListener.REDIS_SN_ONLINE_FIRST_CHECK_KEY, photovoltaicStoragePowerSupplyDTO.getSn());
                    if(!redisUtil.hasKey(firstKey)){
                        redisUtil.set(firstKey,String.valueOf(System.currentTimeMillis()));
                        String env = sellerAccountService.queryCurrentEnv().getData();
                        if(env == null){
                            env = "未知环境";
                        }
                        //发送消息至飞书
                        String msg = String.format(FeiShuRobotUtil.ONLINE_MSG_MSG, env, photovoltaicStoragePowerSupplyDTO.getSn(), groupId,sb +"");
                        feiShuRobotUtil.sendMsg(msg);
                    }
                    //执行在线策略
                    handleEmsGroupOnLine(photovoltaicStoragePowerSupplyDTO,currentTimeMillis,sb,groupId);
                });
            }
        }catch (Exception e){
            log.error("光储市电接收到的kafka信息，处理异常",e);
        }
    }

    /**
     * 参数校验
     * @param photovoltaicStoragePowerSupplyDTO
     * @return
     */
    private boolean validParam(PhotovoltaicStoragePowerSupplyDTO photovoltaicStoragePowerSupplyDTO) {
        if (ObjectUtils.isEmpty(photovoltaicStoragePowerSupplyDTO.getUtilityMeterMaxPower())){
            return true;
        }
        return false;
    }

    /***
     * 处理在线策略
     * @param photovoltaicStoragePowerSupplyDTO
     * @param currentTimeMillis
     * @param sb
     * @param groupId
     */
    private void handleEmsGroupOnLine(PhotovoltaicStoragePowerSupplyDTO photovoltaicStoragePowerSupplyDTO,long currentTimeMillis,long sb,
                                      Object groupId){
        long foreTime = System.currentTimeMillis();
        log.info("增加策略调度分析1,执行在线策略,sn:{},系统时间：{},消息时间:{},时间差:{}",photovoltaicStoragePowerSupplyDTO.getSn(),currentTimeMillis,photovoltaicStoragePowerSupplyDTO.getCreateTime(),sb);
        List<Long> gList = (List<Long>) groupId;

        EmsGroupPowerDTO emsGroupPowerDTO = homeService.queryEmsTimePower(gList.get(0));
        if (emsGroupPowerDTO==null){
            emsGroupPowerDTO=new EmsGroupPowerDTO();
            emsGroupPowerDTO.setGroupId(gList.get(0));
            emsGroupPowerDTO.setRealPower(BigDecimal.ZERO);
            emsGroupPowerDTO.setRealCurrent(BigDecimal.ZERO);
        }
        log.info("群组实时功率: {}",emsGroupPowerDTO);

        //调在线处理
        EnergyEmsPowerInfoVO energyEmsPowerInfoVO = new EnergyEmsPowerInfoVO();
        energyEmsPowerInfoVO.setGroupId(gList.get(0));
        energyEmsPowerInfoVO.setCreateTime(photovoltaicStoragePowerSupplyDTO.getCreateTime());

        //储能功率为正放负充
        boolean storageCharge=photovoltaicStoragePowerSupplyDTO.getStorageMeterTotalActivePower()>=0;

        //电流可用之和
        BigDecimal useableCurrent = BigDecimal.ZERO;

        BigDecimal utilityMeterMaxCurrent = BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getUtilityMeterMaxPower() * 1000)
                .divide(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getUtilityMeterAPhaseVoltage()), RoundingMode.DOWN)
                .divide(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getUtilityMeterAPhaseFactor()), RoundingMode.DOWN);

        log.info("电网可用电流=电网可提供功率：{}除以市电A相电压：{}除以功率因数A相：{}",photovoltaicStoragePowerSupplyDTO.getUtilityMeterMaxPower(),
                photovoltaicStoragePowerSupplyDTO.getUtilityMeterAPhaseVoltage(),photovoltaicStoragePowerSupplyDTO.getUtilityMeterAPhaseFactor());

        useableCurrent = useableCurrent.add(utilityMeterMaxCurrent);
        useableCurrent = useableCurrent.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getStorageMeterMaxCurrent()));
        useableCurrent = useableCurrent.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getPvMeterAPhaseCurrent()));

        log.info("电网可用电流:{}+储能可用电流:{}+光伏实时电流:{}=电流可用之和:{}",utilityMeterMaxCurrent,
                photovoltaicStoragePowerSupplyDTO.getStorageMeterMaxCurrent(),photovoltaicStoragePowerSupplyDTO.getPvMeterAPhaseCurrent(),useableCurrent);

        //功率可用之和
        BigDecimal useablePower = BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getUtilityMeterMaxPower());
        useablePower = useablePower.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getStorageMeterMaxPower()));
        useablePower = useablePower.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getPvMeterTotalActivePower()));

        log.info("电网可用功率:{}+储能可用功率:{}+光伏实时功率:{}=功率可用之和:{}",photovoltaicStoragePowerSupplyDTO.getUtilityMeterMaxPower(),
                photovoltaicStoragePowerSupplyDTO.getStorageMeterMaxPower(),photovoltaicStoragePowerSupplyDTO.getPvMeterTotalActivePower(),useablePower);


        //电流实际之和
        BigDecimal realCurrent = BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getUtilityMeterAPhaseCurrent());
        realCurrent = storageCharge?realCurrent.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getStorageMeterAPhaseCurrent())):realCurrent.subtract(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getStorageMeterAPhaseCurrent()));
        realCurrent = realCurrent.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getPvMeterAPhaseCurrent()));

        log.info("电网实际电流:{}+储能实际电流:{}+光伏实际电流:{}=电流实际之和:{}",photovoltaicStoragePowerSupplyDTO.getUtilityMeterAPhaseCurrent(),
                photovoltaicStoragePowerSupplyDTO.getStorageMeterAPhaseCurrent(),photovoltaicStoragePowerSupplyDTO.getPvMeterAPhaseCurrent(),realCurrent);


        //功率实际之和
        BigDecimal realPower = BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getUtilityMeterTotalActivePower());
        realPower = realPower.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getStorageMeterTotalActivePower()));
        realPower = realPower.add(BigDecimal.valueOf(photovoltaicStoragePowerSupplyDTO.getPvMeterTotalActivePower()));

        log.info("电网实际功率:{}+储能实际功率:{}+光伏实际功率:{}=功率实际之和:{}",photovoltaicStoragePowerSupplyDTO.getUtilityMeterTotalActivePower(),
                photovoltaicStoragePowerSupplyDTO.getStorageMeterTotalActivePower(),photovoltaicStoragePowerSupplyDTO.getPvMeterTotalActivePower(),realPower);

        //计算其他负载之和
        BigDecimal otherPower;
        if(emsGroupPowerDTO.getRealPower() != null){
            otherPower = realPower.subtract(emsGroupPowerDTO.getRealPower());
        }else{
            otherPower = realPower;
        }
        BigDecimal otherCurrent = realCurrent.subtract(emsGroupPowerDTO.getRealCurrent());
        log.info("其他负载功率之和：{}，其他负载电流之和：{}",otherPower,otherCurrent);

        BigDecimal maxPower = useablePower.subtract(otherPower);
        //计算可用电流上限
        BigDecimal maxCurrent = useableCurrent.subtract(otherCurrent);
        log.info("可分配功率上限：{}，可分配电流上限：{}",maxPower,maxCurrent);


        energyEmsPowerInfoVO.setCurrentTotal(maxCurrent);
        energyEmsPowerInfoVO.setPowerTotal(maxPower);
        energyEmsPowerInfoVO.setMessageId(photovoltaicStoragePowerSupplyDTO.getId());
        //调用业务功率分配策略
        opLocationPileGroupService.handleEmsGroupOnLine(energyEmsPowerInfoVO);
        log.info("在线策略执行耗时：{},消息时间:{}",System.currentTimeMillis() - foreTime,photovoltaicStoragePowerSupplyDTO.getCreateTime());
    }
}
