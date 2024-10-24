package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.AppRuleRepository;
import com.autel.cloud.pile.base.domain.service.RuleDetailService;
import com.autel.cloud.pile.base.dto.app.BusinessDetailDTO;
import com.autel.cloud.pile.base.dto.app.BusinessTimeDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.elastic.RuleElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.RuleElasticDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.RuleMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleDetailEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleEntity;
import com.autel.cloud.pile.base.util.CommonUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.IdWorker;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Repository
@Slf4j
public class AppRuleRepositoryImpl extends ServiceImpl<RuleMapper,RuleEntity> implements AppRuleRepository {

    @Autowired
    private RuleDetailService ruleDetailService;
    @Autowired
    private RuleElastic ruleElastic;

    @Override
    @Transactional
    public Result<Boolean> addTime(BusinessTimeDTO businessTimeDTO) {
        log.info("addRule,input ruleDTO={}", JSON.toJSONString(businessTimeDTO));
        RuleDetailEntity ruleDetailEntity = new RuleDetailEntity();

        //数据校验
        if (CollectionUtils.isEmpty(businessTimeDTO.getDetails()) || businessTimeDTO.getDetails().size() > 7) {
            throw new MessageCodeException(PileBaseEnum.RULE_DETAIL_EMPTY);
        }

        log.info("getDetails().size()====="+businessTimeDTO.getDetails().size());
        List<BusinessDetailDTO> details = businessTimeDTO.getDetails();
        details.forEach(time -> {
            if (CollectionUtils.isEmpty(time.getDays()) || time.getDays().size() > 7) {
                throw new MessageCodeException(PileBaseEnum.RULE_DAYS_EMPTY);
            }
            if (StringUtils.hasText(time.getMobile()) && !CommonUtil.checkPhone(time.getMobile())) {
                throw new MessageCodeException(PileBaseEnum.MOBILE_FORMAT_ERROR);
            }
            if (StringUtils.hasText(time.getEmail()) && !CommonUtil.checkEmail(time.getEmail())) {
                throw new MessageCodeException(PileBaseEnum.EMAIL_FORMAT_ERROR);
            }
        });
        RuleEntity entity = new RuleEntity();
        entity.setId(IdWorker.getId());
        entity.setSellerId(businessTimeDTO.getSellerId());
        entity.setCreateBy(businessTimeDTO.getUserId());
        log.info("addTime,entity={}", JSON.toJSONString(entity));
        boolean save = this.save(entity);
        log.info("addTime,save={}", save);
        if (!save) {
            throw new RuntimeException("addTime fail.");
        }
        Long ruleId = entity.getId();
        List<RuleDetailEntity> ruleDetailEntities = new ArrayList<>();
        businessTimeDTO.getDetails().forEach(detail -> {
            BeanUtils.copyProperties(detail, ruleDetailEntity);
            ruleDetailEntity.setId(IdWorker.getId());
            ruleDetailEntity.setRuleId(ruleId);
            ruleDetailEntity.setDays(detail.getDays().stream().map(String::valueOf).collect(Collectors.joining(",")));
            ruleDetailEntity.setCreatTime(System.currentTimeMillis());
            ruleDetailEntities.add(ruleDetailEntity);
        });
        log.info("addRule,ruleDetailEntities={}", JSON.toJSONString(ruleDetailEntities));
        boolean saveBatch = ruleDetailService.saveBatch(ruleDetailEntities);
        log.info("addRule,saveBatch={}", saveBatch);
        if (!saveBatch) {
            throw new RuntimeException("addTime fail.");
        }
        //同步到ES
        syncRuleToES(entity, ruleDetailEntities);
        return Result.ofSucceed(true);
    }

    private void syncRuleToES(RuleEntity entity, List<RuleDetailEntity> ruleDetailEntities) {
        RuleElasticDTO dto = new RuleElasticDTO();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setSellerId(entity.getSellerId());
        dto.setCreateBy(entity.getCreateBy());
        dto.setDetails(JSON.toJSONString(ruleDetailEntities));
        RuleElasticDTO resultDto = ruleElastic.save(dto);
        log.info("syncRuleToES,resultDto={}", JSON.toJSONString(resultDto));
    }
}
