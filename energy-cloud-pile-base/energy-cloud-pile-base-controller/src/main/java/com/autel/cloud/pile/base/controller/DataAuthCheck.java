package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargeCardMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.RuleMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleEntity;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import lombok.extern.log4j.Log4j2;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @ClassName PageAuthCheckServiceImpl
 * @Author A22121
 * @Description
 * @Date 2022/6/23 20:22
 * @Version 0.0.1-SNAPSHOT
 */
@Service
@Aspect
@Log4j2
public class DataAuthCheck {

    private static final String OP_LOCATION_CONTROLLER = "OpLocationController";

    private static final String OP_LOCATION_PILE_EVSE_CONTROLLER = "OpLocationPileEvseController";

    private static final String OP_LOCATION_EVSE_CONTROLLER = "OpLocationEvseController";

    private static final String OP_LOCATION_PILE_GROUP_CONTROLLER = "OpLocationPileGroupController";

    private static final String RULE_CONTROLLER = "RuleController";

    private static final String OP_POWER_LIMIT_CONTROLLER = "OpPowerLimitController";

    private static final String CHARGE_CARD_BUSINESS_CONTROLLER = "ChargeCardBusinessController";

    @Autowired
    private PileUserFeign pileUserFeign;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Autowired
    private OpLocationPileEvseElastic opLocationPileEvseElastic;

    @Autowired
    private OpLocationPileGroupRepository opLocationPileGroupRepository;

    @Autowired
    private RuleMapper ruleMapper;

    @Autowired
    private ChargeCardMapper chargeCardMapper;

    @Pointcut("execution(* com.autel.cloud.pile.base.controller.OpLocationController.detail(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationController.update(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationController.updateAnnouncement(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationController.stationGunStatusGroupCount(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationController.pileAndEvse(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationController.delete(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.getPileInfoByPileSn(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.getEvseListByPileSn(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.detail(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.stationPilePage(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.deleteByPileId(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.getEvseListByPileSnv2(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.getPileInfoByPileSnv2(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileEvseController.getPileInfoByPileSnV3(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationEvseController.updateEvse(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationEvseOperationController.backendStop(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationEvseOperationController.disable(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationEvseOperationController.able(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationEvseOperationController.reset(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationEvseOperationController.unlock(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.RuleController.getAllRuleByLocationId(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.RuleController.relatePile(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.RuleController.detail(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.RuleController.editRule(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.RuleController.removePile(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.RuleController.deleteRule(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.isAssociatePileGroup(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.queryV2(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.status(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.queryPileInLocation(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.update(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.delete(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpLocationPileGroupController.dataAuthCheckDetail(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpPowerLimitController.getTurnOn(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpPowerLimitController.queryTimeSetting(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.OpPowerLimitController.queryPowerLimit(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.ChargeCardBusinessController.enableDisableChargingCard(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.ChargeCardBusinessController.update(..))" +
            "|| execution(* com.autel.cloud.pile.base.controller.ChargeCardBusinessController.batchRemovalOfChargingCards(..))" +
            "")
    public void point() {
    }

    @Around("point()")
    public Object around(ProceedingJoinPoint proceedingJoinPoint) {

        String methodName = proceedingJoinPoint.getSignature().getName();
        String className = proceedingJoinPoint.getSignature().getDeclaringTypeName();
        // 如果是oicp服务Feign调用，跳过校验
        HttpServletRequest request = ((ServletRequestAttributes) Objects.requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String oicpRequest = "oicpRequest";
        String oicpRequest1 = request.getHeader("oicpRequest");

        boolean isCheckAuth = Boolean.FALSE;
        if (className.contains(DataAuthCheck.OP_LOCATION_CONTROLLER) && "detail".equals(methodName)) {
            long locationId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_CONTROLLER) && "update".equals(methodName)) {
            JSONObject update = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long locationId = update.getLong("id");
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_CONTROLLER) && "updateAnnouncement".equals(methodName)) {
            JSONObject update = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long locationId = update.getLong("id");
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_CONTROLLER) && "stationGunStatusGroupCount".equals(methodName)) {
            long locationId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_CONTROLLER) && "pileAndEvse".equals(methodName)) {
            long locationId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_CONTROLLER) && "delete".equals(methodName)) {
            long locationId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && "getPileInfoByPileSn".equals(methodName)) {
            String pileSn = proceedingJoinPoint.getArgs()[0].toString();
            isCheckAuth = checkAuthPileSn(pileSn);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && "getEvseListByPileSn".equals(methodName)) {
            String pileSn = proceedingJoinPoint.getArgs()[0].toString();
            isCheckAuth = checkAuthPileSn(pileSn);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && "detail".equals(methodName)) {
            long pileId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthPileId(pileId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && "stationPilePage".equals(methodName)) {
            JSONObject update = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long locationId = update.getLong("stationId");
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && "deleteByPileId".equals(methodName)) {
            long pileId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthPileId(pileId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && "getEvseListByPileSnv2".equals(methodName)) {
            JSONObject opPileEvseDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            String pileSn = opPileEvseDTO.getString("pileSn");
            isCheckAuth = checkAuthPileSn(pileSn);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_EVSE_CONTROLLER) && ("getPileInfoByPileSnv2".equals(methodName) || "getPileInfoByPileSnV3".equals(methodName))) {
            JSONObject opPileEvseDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            String pileSn = opPileEvseDTO.getString("pileSn");
            isCheckAuth = checkAuthPileSn(pileSn);
        }
        else if (className.contains(DataAuthCheck.OP_LOCATION_EVSE_CONTROLLER) && "updateEvse".equals(methodName)) {
            JSONArray updateArray = JSON.parseArray(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            boolean flag = true;
            for (int i = 0; i < updateArray.size(); i++) {
                JSONObject update = updateArray.getJSONObject(i);
                Long pileId = update.getLong("id");
                if (!this.checkAuthPileId(pileId)) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                isCheckAuth = true;
            }
        } else if (className.contains("OpLocationEvseOperationController")) {
            String evseSn = proceedingJoinPoint.getArgs()[0].toString();
            isCheckAuth = checkAuthPileSn(evseSn.split("_")[0]);
        } else if (className.contains(DataAuthCheck.RULE_CONTROLLER) && "getAllRuleByLocationId".equals(methodName)) {
            long locationId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthLocationId(locationId);
        } else if (className.contains(DataAuthCheck.RULE_CONTROLLER) && "relatePile".equals(methodName)) {
            JSONObject relatePileDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long ruleId = relatePileDTO.getLong("ruleId");
            isCheckAuth = this.checkAuthRuleId(ruleId);
            if (isCheckAuth) {
                JSONArray piles = relatePileDTO.getJSONArray("piles");
                for (int i = 0; i < piles.size(); i++) {
                    JSONObject jsonObject = piles.getJSONObject(i);
                    Long locationId = jsonObject.getLong("locationId");
                    Long pileId = jsonObject.getLong("pileId");
                    if (!this.checkAuthPileId(pileId)
                            || !this.checkAuthLocationId(locationId)) {
                        isCheckAuth = false;
                        break;
                    }
                }
            }
        } else if (className.contains(DataAuthCheck.RULE_CONTROLLER) && "detail".equals(methodName)) {
            long ruleId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthRuleId(ruleId);
        } else if (className.contains(DataAuthCheck.RULE_CONTROLLER) && "editRule".equals(methodName)) {
            JSONObject opLocationPileGroupParamDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long ruleId = opLocationPileGroupParamDTO.getLong("id");
            isCheckAuth = this.checkAuthRuleId(ruleId);
        } else if (className.contains(DataAuthCheck.RULE_CONTROLLER) && "removePile".equals(methodName)) {
            JSONObject removePileDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long ruleId = removePileDTO.getLong("ruleId");
            isCheckAuth = this.checkAuthRuleId(ruleId);
        } else if (className.contains(DataAuthCheck.RULE_CONTROLLER) && "deleteRule".equals(methodName)) {
            long ruleId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthRuleId(ruleId);
        }
        else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "isAssociatePileGroup".equals(methodName)) {
            String pileSn = proceedingJoinPoint.getArgs()[0].toString();
            // isCheckAuth = checkAuthPileSn(pileSn);
            isCheckAuth = true;
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "queryV2".equals(methodName)) {
            JSONObject opLocationPileGroupParamDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long locationId = opLocationPileGroupParamDTO.getLong("locationId");
            // isCheckAuth = checkAuthLocationId(locationId);
            isCheckAuth = true;
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "status".equals(methodName)) {
            long groupId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthgroupId(groupId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "queryPileInLocation".equals(methodName)) {
            JSONObject opLocationPileEvseGroupParamDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long locationId = opLocationPileEvseGroupParamDTO.getLong("locationId");
            // isCheckAuth = checkAuthLocationId(locationId);
            isCheckAuth = true;
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "update".equals(methodName)) {
            JSONObject opLocationPileGroupDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long id = opLocationPileGroupDTO.getLong("id");
            isCheckAuth = this.checkAuthgroupId(id);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "delete".equals(methodName)) {
            long groupId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthgroupId(groupId);
        } else if (className.contains(DataAuthCheck.OP_LOCATION_PILE_GROUP_CONTROLLER) && "dataAuthCheckDetail".equals(methodName)) {
            long groupId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = this.checkAuthgroupId(groupId);
        } else if (className.contains(DataAuthCheck.OP_POWER_LIMIT_CONTROLLER) && "getTurnOn".equals(methodName)) {
            long pileId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthPileId(pileId);
        } else if (className.contains(DataAuthCheck.OP_POWER_LIMIT_CONTROLLER) && "queryTimeSetting".equals(methodName)) {
            long pileId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthPileId(pileId);
        } else if (className.contains(DataAuthCheck.OP_POWER_LIMIT_CONTROLLER) && "queryPowerLimit".equals(methodName)) {
            long pileId = Long.parseLong(proceedingJoinPoint.getArgs()[0].toString());
            isCheckAuth = checkAuthPileId(pileId);
        } else if (className.contains(DataAuthCheck.CHARGE_CARD_BUSINESS_CONTROLLER) && "enableDisableChargingCard".equals(methodName)) {
            JSONObject enableDisableChargingCardDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            String cardNumber = enableDisableChargingCardDTO.getString("cardNumber");
            isCheckAuth = this.checkCardNumber(cardNumber);
        } else if (className.contains(DataAuthCheck.CHARGE_CARD_BUSINESS_CONTROLLER) && "update".equals(methodName)) {
            JSONObject cardBusinessDTO = JSON.parseObject(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            Long id = cardBusinessDTO.getLong("id");
            isCheckAuth = this.checkCardId(id);
        } else if (className.contains(DataAuthCheck.CHARGE_CARD_BUSINESS_CONTROLLER) && "batchRemovalOfChargingCards".equals(methodName)) {
            JSONArray ids = JSON.parseArray(JSON.toJSONString(proceedingJoinPoint.getArgs()[0]));
            boolean flag = true;
            for (int i = 0; i < ids.size(); i++) {
                Long cardId = ids.getLong(i);
                if (!this.checkCardId(cardId)) {
                    flag = false;
                    break;
                }
            }
            if (flag) {
                isCheckAuth = true;
            }
        }
        // 放行oicp feign 接口调用
        if (StringUtils.endsWithIgnoreCase(oicpRequest, oicpRequest1)) {
            isCheckAuth = Boolean.TRUE;
        }

        // 商家id为0则全部放开
        if (!isCheckAuth) {
            try {
                if (LoginUserHolder.getLoginUser().getPayload().getSellerId().longValue() == 0L) {
                    isCheckAuth = Boolean.TRUE;
                }
            } catch (Exception e) {
                isCheckAuth = Boolean.FALSE;
            }
        }

        // 判断用户是否有权限
        if (!isCheckAuth) {
            throw new MessageCodeException(PileBaseEnum.NO_DATA_ACCESS);
        }

        try {
            return proceedingJoinPoint.proceed();
        } catch (MessageCodeException e) {
            log.error("DataAuthCheck.around proceed failed and MessageCodeException = ", e);
            throw e;
        } catch (Throwable throwable) {
            log.error("DataAuthCheck.around proceed failed and throwable = ", throwable);
            throw new MessageCodeException(throwable.getMessage());
        }
    }

    private List<String> getCardNumberList() {
        try {
            Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
            if (sellerId == null) {
                return Collections.emptyList();
            }
            LambdaQueryWrapper<ChargeCardEntity> lqw = new LambdaQueryWrapper<>();
            lqw.select(ChargeCardEntity::getId, ChargeCardEntity::getCardNumber)
                    .eq(ChargeCardEntity::getOperatorId, sellerId)
                    .eq(ChargeCardEntity::getDeleted, false);
            List<ChargeCardEntity> chargeCardEntityList = chargeCardMapper.selectList(lqw);
            if (ObjectUtils.isEmpty(chargeCardEntityList)) {
                return Collections.emptyList();
            }
            return chargeCardEntityList.stream().map(ChargeCardEntity::getCardNumber).collect(Collectors.toList());
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private List<Long> getRuleList() {
        try {
            Long sellerId = LoginUserHolder.getLoginUser().getPayload().getSellerId();
            if (sellerId == null) {
                return Collections.emptyList();
            }
            LambdaQueryWrapper<RuleEntity> lqw = new LambdaQueryWrapper<>();
            lqw.select(RuleEntity::getId)
                    .eq(RuleEntity::getSellerId, sellerId)
                    .eq(RuleEntity::getDeleted, 0);
            List<RuleEntity> ruleEntityList = ruleMapper.selectList(lqw);
            if (ObjectUtils.isEmpty(ruleEntityList)) {
                return Collections.emptyList();
            }
            return ruleEntityList.stream().map(RuleEntity::getId).collect(Collectors.toList());
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private List<Long> getLocationIdList() {
        try {
            List<Long> locationIdList = pileUserFeign.getLocationIds().getData();
            log.info("PageAuthCheckServiceImpl getLocationIdList locationIdList = "
                    + JSON.toJSONString(locationIdList));
            if (CollectionUtils.isEmpty(locationIdList)) {
                return Collections.emptyList();
            }
            return locationIdList;
        } catch (Exception e) {
            log.info("PageAuthCheckServiceImpl getLocationIdList Exception = ", e);
            return Collections.emptyList();
        }
    }

    private boolean checkCardId(Long id) {
        try {
            if (id == null) {
                return false;
            }
            ChargeCardEntity chargeCardEntity = chargeCardMapper.selectById(id);
            if (chargeCardEntity == null
                    || org.apache.commons.lang3.StringUtils.isEmpty(chargeCardEntity.getCardNumber())) {
                return false;
            }
            return this.checkCardNumber(chargeCardEntity.getCardNumber());
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    private boolean checkCardNumber(String cardNumber) {
        try {
            return this.getCardNumberList().contains(cardNumber);
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    private boolean checkAuthRuleId(Long ruleId) {
        try {
            return this.getRuleList().contains(ruleId);
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    private boolean checkAuthgroupId(Long groupId) {
        try {
            OpLocationPileGroupEntity entity = opLocationPileGroupRepository.findOne(groupId);
            if (entity == null
                    || entity.getMerchantId() == null) {
                return false;
            }
            return LoginUserHolder.getLoginUser().getPayload().getSellerId().equals(entity.getMerchantId());
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    private boolean checkAuthLocationId(Long locationId) {
        try {
            List<Long> locationIdList = getLocationIdList();
            return locationIdList.contains(locationId);
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    private boolean checkAuthPileId(Long pileId) {
        try {
            Optional<OpLocationPileEvseElasticDTO> optional = opLocationPileEvseElastic.findById(pileId);
            if (!optional.isPresent()) {
                return Boolean.FALSE;
            }
            OpLocationPileEvseElasticDTO elasticDTO = optional.get();
            List<Long> locationIdList = getLocationIdList();
            return locationIdList.contains(elasticDTO.getLocationId());
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    private boolean checkAuthPileSn(String pileSn) {
        try {
            OpLocationPileEvseElasticDTO opLocationPileEvseElasticDTO = opLocationPileEvseElastic.findByPileSn(pileSn);
            if (opLocationPileEvseElasticDTO == null || opLocationPileEvseElasticDTO.getLocationId() == null) {
                return Boolean.FALSE;
            }
            return this.getLocationIdList().contains(opLocationPileEvseElasticDTO.getLocationId());
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }
}
