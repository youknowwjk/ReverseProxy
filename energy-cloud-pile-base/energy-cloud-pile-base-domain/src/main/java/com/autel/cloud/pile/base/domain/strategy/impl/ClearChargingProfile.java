package com.autel.cloud.pile.base.domain.strategy.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.context.OncePerRequestContext;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.strategy.DistributeStrategy;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupDeliveryVO;
import com.autel.cloud.smart.charge.dto.DeliveryParamDTO;
import com.autel.cloud.smart.charge.feign.SmartChargeFeign;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @Author temp
 * @Date 2022/12/16 14:59
 */
@Component(value = BaseConstant.CLEAR_CHARGING_PROFILE)
@Slf4j
public class ClearChargingProfile implements DistributeStrategy {

    @Resource
    private StringRedisTemplate stringRedisTemplate;
    @Resource
    private SmartChargeFeign smartChargeFeign;
    @Resource
    private OpLocationEvseService opLocationEvseService;

    @Override
    public void handleUseStrategy(OncePerRequestContext context) {

    }

    @Override
    public void execute(OncePerRequestContext context) {
        List<OpLocationPileGroupAssociateEntity> deleteEntityList = context.getDeleteList();
        if (CollectionUtils.isEmpty(deleteEntityList)) {
            log.error("ClearChargingProfile,deleteEntityList={}", deleteEntityList);
            return;
        }
        List<String> snList = deleteEntityList.stream().map(OpLocationPileGroupAssociateEntity::getPileSn).collect(Collectors.toList());
        Map<String, List<OpLocationEvseElasticDTO>> evseDtoMap = opLocationEvseService.findList(snList).stream().collect(Collectors.groupingBy(OpLocationEvseElasticDTO::getPileSn));
        List<OpLocationPileGroupDeliveryVO> deleteList = new ArrayList<>();
        deleteEntityList.stream().forEach(entity -> {
            List<OpLocationEvseElasticDTO> evseDtoList = evseDtoMap.get(entity.getPileSn());
            evseDtoList.stream().forEach(evseDto -> {
                OpLocationPileGroupDeliveryVO vo = new OpLocationPileGroupDeliveryVO();
                vo.setPileSn(entity.getPileSn());
                vo.setEvseSn(evseDto.getEvseSn());
                vo.setAssociateId(entity.getId());
                vo.setGroupId(entity.getGroupId());
                deleteList.add(vo);
            });
        });
        log.info("ClearChargingProfile,计算结果,deleteList={}", JSON.toJSONString(deleteList));
        //删除下发缓存
        if (!CollectionUtils.isEmpty(deleteList)) {
            deleteList.stream().forEach(vo -> {
                String evseSn = vo.getEvseSn();
                String key = RedisKeyConstant.getChargeLastDeliveryKey(evseSn);
                if (Optional.ofNullable(stringRedisTemplate.hasKey(key)).orElse(false)) {
                    stringRedisTemplate.delete(key);
                }
            });
        }
        DeliveryParamDTO paramDTO = new DeliveryParamDTO();
        paramDTO.setDeliveryVoList(deleteList);
        paramDTO.setUsageScenario(1);
        Result<Boolean> booleanResult = smartChargeFeign.clearChargeProfile(paramDTO);
        log.info("ClearChargingProfile,booleanResult={}", booleanResult);
    }

    @Override
    public void handleDelivery(OncePerRequestContext context) {

    }
}
