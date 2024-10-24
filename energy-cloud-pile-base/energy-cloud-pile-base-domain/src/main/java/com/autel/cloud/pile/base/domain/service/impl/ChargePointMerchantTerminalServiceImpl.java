package com.autel.cloud.pile.base.domain.service.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.service.ChargePointMerchantTerminalService;
import com.autel.cloud.pile.base.infrastructure.mapper.ChargePointMerchantTerminalMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantRelationEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargePointMerchantTerminalEntity;
import com.autel.cloud.pile.base.vo.chargepoint.TerminalBriefInfoVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
public class ChargePointMerchantTerminalServiceImpl implements ChargePointMerchantTerminalService {

    @Resource
    private ChargePointMerchantTerminalMapper chargePointMerchantTerminalMapper;

    @Override
    public List<ChargePointMerchantTerminalEntity> getTerminalList(List<String> terminalSnList, Long merchantId) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.getTerminalList terminalSnList : {} and merchantId : {}",
                JSON.toJSONString(terminalSnList),
                JSON.toJSONString(merchantId));

        if (ObjectUtils.isEmpty(terminalSnList)
                && merchantId == null) {
            return null;
        }

        return chargePointMerchantTerminalMapper
                .selectList(new LambdaQueryWrapper<ChargePointMerchantTerminalEntity>()
                        .eq(merchantId != null, ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
                        .in(ObjectUtils.isNotEmpty(terminalSnList), ChargePointMerchantTerminalEntity::getTerminalSn, terminalSnList));
    }

    @Override
    public ChargePointMerchantTerminalEntity queryBySN(String sn, Long sellerId) {
        LambdaQueryWrapper<ChargePointMerchantTerminalEntity> queryWrapper = Wrappers.lambdaQuery(ChargePointMerchantTerminalEntity.class);
        queryWrapper.eq(ChargePointMerchantTerminalEntity::getTerminalSn, sn).eq(ChargePointMerchantTerminalEntity::getMerchantId, sellerId);
        return chargePointMerchantTerminalMapper.selectOne(queryWrapper);
    }

    @Override
    public List<TerminalBriefInfoVO> getTerminalBriefInfoVOList(String hostSn, Long merchantId) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.getTerminalBriefInfoVOList hostSn : {} and merchantId : {}",
                JSON.toJSONString(hostSn),
                JSON.toJSONString(merchantId));

        List<ChargePointMerchantTerminalEntity> entities = this.getTerminalEntityList(hostSn, merchantId);

        if (ObjectUtils.isEmpty(entities)) {
            return null;
        }

        List<TerminalBriefInfoVO> terminalBriefInfoVOs = new ArrayList<>();
        entities.forEach(val -> {
            TerminalBriefInfoVO terminalBriefInfoVO = new TerminalBriefInfoVO();
            terminalBriefInfoVO.setTerminalSn(val.getTerminalSn());
            terminalBriefInfoVO.setTerminalName(val.getTerminalName());
            terminalBriefInfoVOs.add(terminalBriefInfoVO);
        });
        return terminalBriefInfoVOs;
    }

    @Override
    public List<ChargePointMerchantTerminalEntity> getTerminalEntityList(String hostSn, Long merchantId) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.getTerminalEntityList hostSn : {} and merchantId : {}",
                JSON.toJSONString(hostSn),
                JSON.toJSONString(merchantId));

        if (StringUtils.isBlank(hostSn)
                && merchantId == null) {
            return null;
        }

        return chargePointMerchantTerminalMapper
                .selectList(new LambdaQueryWrapper<ChargePointMerchantTerminalEntity>()
                        .eq(merchantId != null ,ChargePointMerchantTerminalEntity::getMerchantId, merchantId)
                        .eq(StringUtils.isNotBlank(hostSn) ,ChargePointMerchantTerminalEntity::getHostSn, hostSn));
    }

    @Override
    public boolean batchAdd(List<ChargePointMerchantTerminalEntity> terminalEntityList) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.batchAdd terminalEntityList : {}",
                JSON.toJSONString(terminalEntityList));

        terminalEntityList.forEach(val -> chargePointMerchantTerminalMapper.insert(val));
        return true;
    }

    @Override
    public boolean updateTerminal(ChargePointMerchantTerminalEntity chargePointMerchantTerminalEntity) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.updateTerminal chargePointMerchantTerminalEntity : {}",
                JSON.toJSONString(chargePointMerchantTerminalEntity));

        chargePointMerchantTerminalMapper.updateById(chargePointMerchantTerminalEntity);
        return true;
    }

    @Override
    public boolean batchDelete(String hostSn, Long merchantId) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.bachDelete hostSn : {} and merchantId : {}",
                JSON.toJSONString(hostSn),
                JSON.toJSONString(merchantId));

        if (StringUtils.isBlank(hostSn)
                && merchantId == null) {
            return false;
        }

        chargePointMerchantTerminalMapper.delete(new LambdaQueryWrapper<ChargePointMerchantTerminalEntity>()
                .eq(StringUtils.isNotBlank(hostSn), ChargePointMerchantTerminalEntity::getHostSn, hostSn)
                .eq(merchantId != null, ChargePointMerchantTerminalEntity::getMerchantId, merchantId));
        return true;
    }

    @Override
    public boolean deleteByCondition(List<String> terminalSnList, Long merchantId) {

        log.info("===>>> ChargePointMerchantTerminalServiceImpl.deleteByCondition terminalSnList : {} and merchantId : {}",
                JSON.toJSONString(terminalSnList),
                JSON.toJSONString(merchantId));

        if (ObjectUtils.isEmpty(terminalSnList)
                && merchantId == null) {
            return false;
        }

        chargePointMerchantTerminalMapper.delete(new LambdaQueryWrapper<ChargePointMerchantTerminalEntity>()
                .in(ObjectUtils.isNotEmpty(terminalSnList), ChargePointMerchantTerminalEntity::getTerminalSn, terminalSnList)
                .eq(merchantId != null, ChargePointMerchantTerminalEntity::getMerchantId, merchantId));
        return true;
    }
}
