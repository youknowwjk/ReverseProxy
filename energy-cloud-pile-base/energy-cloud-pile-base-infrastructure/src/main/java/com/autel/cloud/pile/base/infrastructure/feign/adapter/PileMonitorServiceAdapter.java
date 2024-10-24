package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.monitor.dto.OpEvseMeterUploadDTO;
import com.autel.cloud.monitor.dto.OpEvseStatusUploadDTO;
import com.autel.cloud.monitor.feign.MonitorFeignClient;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.IntelligentChargeScheduleJob;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.compress.utils.Lists;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Component
public class PileMonitorServiceAdapter extends AbstractFeignServiceAdapter {


    @Resource
    private MonitorFeignClient monitorFeignClient;

    @Resource
    private StringRedisTemplate stringRedisTemplate;


    @Value("${webhook.wechat.key.pile-user:66ae591a-4e15-4b57-9a1c-56978ba6152b}")
    protected String webhookWechatKey;


    public BigDecimal readSOCFromMonitorCache(IntelligentChargeScheduleJob intelligentChargeScheduleJob) {
        String evseRealTimeKey = RedisKeyConstant.getHashPileBaseEvseRealTime(intelligentChargeScheduleJob.getEvseSn());
        Map<Object, Object> evseRealTimeRet = stringRedisTemplate.opsForHash().entries(evseRealTimeKey);
        log.info("OpEvseMonitorRedis getMeter evseRealTimeRet = {}", JSON.toJSONString(evseRealTimeRet));
        if (!MapUtils.isEmpty(evseRealTimeRet)) {
            OpEvseMeterUploadDTO opEvseMeterUploadDTO = JSON.parseObject(JSON.toJSONString(evseRealTimeRet).replace("\\\"", ""), OpEvseMeterUploadDTO.class);
            Long createTime = opEvseMeterUploadDTO.getCreateTime();
            String busId = opEvseMeterUploadDTO.getBusId();
            BigDecimal batterySoc = opEvseMeterUploadDTO.getBatterySoc();
            if (intelligentChargeScheduleJob.getOrderSeq().equalsIgnoreCase(busId) && Objects.nonNull(batterySoc) && batterySoc.compareTo(BigDecimal.ZERO) > 0) {
                log.info("直接从上报到monitor的读取 {} soc= {}", JSON.toJSONString(intelligentChargeScheduleJob), batterySoc);
                return batterySoc;
            }
        }
        return null;
    }

    public BigDecimal readSOCFromMonitorCache(String evseSn, String orderSeq) {
        String evseRealTimeKey = RedisKeyConstant.getHashPileBaseEvseRealTime(evseSn);
        Map<Object, Object> evseRealTimeRet = stringRedisTemplate.opsForHash().entries(evseRealTimeKey);
        log.info("OpEvseMonitorRedis getMeter evseRealTimeRet = {}", JSON.toJSONString(evseRealTimeRet));
        if (!MapUtils.isEmpty(evseRealTimeRet)) {
            OpEvseMeterUploadDTO opEvseMeterUploadDTO = JSON.parseObject(JSON.toJSONString(evseRealTimeRet).replace("\\\"", ""), OpEvseMeterUploadDTO.class);
            Long createTime = opEvseMeterUploadDTO.getCreateTime();
            String busId = opEvseMeterUploadDTO.getBusId();
            BigDecimal batterySoc = opEvseMeterUploadDTO.getBatterySoc();
            if (orderSeq.equalsIgnoreCase(busId) && batterySoc.compareTo(BigDecimal.ZERO) > 0) {
                log.info("直接从上报到monitor的读取 {} {} soc= {}", orderSeq, evseSn, batterySoc);
                return batterySoc;
            }
        }
        return null;
    }

    public List<OpEvseMeterUploadDTO> queryNewMeters(List<String> evseSnList) {
        if (CollectionUtils.isEmpty(evseSnList)) {
            return Collections.emptyList();
        }
        log.info("queryNewMeters: {}", evseSnList);
        Result<List<OpEvseMeterUploadDTO>> listResult = monitorFeignClient.queryNewMeters(evseSnList);
        log.info("queryNewMeters result: {}", JSON.toJSONString(listResult));
        List<OpEvseMeterUploadDTO> list = nullableHandle(listResult);
        return Optional.ofNullable(list).orElse(Collections.emptyList());
    }

    /**
     * 查询最新充电上报信息
     */
    public OpEvseStatusUploadDTO queryStatusInfoByEvseSn(String evseSn) {
        if (Objects.isNull(evseSn)) {
            return null;
        }
        log.info("queryStatusInfoByEvseSn: {}", JSON.toJSONString(evseSn));
        Result<OpEvseStatusUploadDTO> result = monitorFeignClient.queryStatusInfoByEvseSn(evseSn);
        log.info("queryStatusInfoByEvseSn result: {}", JSON.toJSONString(result));
        OpEvseStatusUploadDTO measureDTOList = nullableHandle(result);
        if (Objects.isNull(measureDTOList)) {
            String content = String.format("调用服务(pile-monitor/opEvseMonitor/queryStatusInfoByEvseSn?evseSn={}) 没有查询到桩 evseSn=%s  ", evseSn);
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return null;
        }
        return measureDTOList;
    }

    /**
     * 根据busID 查询监控
     *
     * @param orderSeq  号
     * @param beginTime 号
     * @return
     */
    public List<OpEvseMeterUploadDTO> queryMeterByBusId(@NotNull String orderSeq, @NotNull Long beginTime) {
        if (StringUtils.isBlank(orderSeq)) {
            return Collections.emptyList();
        }
        OpEvseMeterUploadDTO opEvseMeterUploadDTO = new OpEvseMeterUploadDTO();
        opEvseMeterUploadDTO.setBusId(orderSeq);
        opEvseMeterUploadDTO.setCreateTime(beginTime);// 那边查询的时候只需要按照这个时间以后的数据
        Result<List<OpEvseMeterUploadDTO>> result = monitorFeignClient.selectMeterByBusId(opEvseMeterUploadDTO);
        log.info("selectMeterByBusId  code: {}  msg:{}", result.getCode(), result.getMessage());
        List<OpEvseMeterUploadDTO> measureDTOList = nullableHandle(result);
        if (Objects.isNull(measureDTOList)) {
            String content = String.format("调用服务(pile-monitor/opEvseMonitor/selectMeterByBusId?busId={}) 没有查询到桩 busId(orderSeq)=%s  ", orderSeq);
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return Collections.emptyList();
        }
        return measureDTOList;
    }

    public List<OpEvseMeterUploadDTO> queryNewMeterByEvseSnList(List<String> evseSnList) {
        log.info("queryNewMeterByEvseSnList  param is {}", JSON.toJSONString(evseSnList));

        if (CollectionUtils.isEmpty(evseSnList)) {
            return Collections.emptyList();
        }
        List<OpEvseMeterUploadDTO> opEvseMeterUploadDTOS = Lists.newArrayList();
        try {
            Result<List<OpEvseMeterUploadDTO>> listResult = monitorFeignClient.queryNewMeterByEvseSnList(evseSnList);
            log.info("queryNewMeterByEvseSnList  result  is {}", JSON.toJSONString(listResult));
            opEvseMeterUploadDTOS = listResult.getData();
        }catch (Exception e){
            log.error("queryNewMeterByEvseSnList  error  is {}", e.getMessage());
            e.printStackTrace();

        }

        if(CollectionUtils.isEmpty(opEvseMeterUploadDTOS)){
            return Lists.newArrayList();
        }
        // 过滤 null情况
        List<OpEvseMeterUploadDTO> collect = opEvseMeterUploadDTOS.stream().filter(item -> null != item && StringUtils.isNotBlank(item.getEvseSn())).collect(Collectors.toList());
        return collect;
    }

}
