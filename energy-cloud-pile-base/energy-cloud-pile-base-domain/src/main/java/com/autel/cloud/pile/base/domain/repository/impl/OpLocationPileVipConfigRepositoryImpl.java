package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.pile.base.VipPolicy;
import com.autel.cloud.pile.base.domain.convert.OpLocationPileVipConfigTypeMapper;
import com.autel.cloud.pile.base.domain.convert.VipPolicyTypeMapper;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileVipConfigRepository;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileVipConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileVipConfigEntity;
import com.autel.cloud.pile.user.api.constant.Constant;
import com.autel.cloud.pile.user.api.dto.MemberCleanDTO;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.vo.MemberGroupVO;
import com.autel.cloud.pile.user.api.vo.MemberVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.time.ZoneOffset;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Slf4j
public class OpLocationPileVipConfigRepositoryImpl extends ServiceImpl<OpLocationPileVipConfigMapper, OpLocationPileVipConfigEntity> implements OpLocationPileVipConfigRepository {
    @Resource
    private OpLocationPileVipConfigMapper opLocationPileVipConfigMapper;

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    @Resource
    private PileUserFeign pileUserFeign;

    @Resource
    private OpLocationPileGroupRepository opLocationPileGroupRepository;

    @Override
    public void save(VipPolicy dto) {
        OpLocationPileVipConfigEntity entity = OpLocationPileVipConfigTypeMapper.INSTANCE.map(dto);

        opLocationPileVipConfigMapper.insert(entity);
    }

    @Override
    public void save(List<VipPolicy> list) {
        List<OpLocationPileVipConfigEntity> entities = OpLocationPileVipConfigTypeMapper.INSTANCE.map(list);
        saveBatch(entities);
    }

    @Override
    public void batchAddOrUpdate(List<VipPolicy> list) {
        List<OpLocationPileVipConfigEntity> entities = OpLocationPileVipConfigTypeMapper.INSTANCE.map(list);
        opLocationPileVipConfigMapper.batchAddOrUpdate(entities);
    }

    @Override
    public List<VipPolicy> findByGroupId(Long groupId) {
        List<VipPolicy> list = this.findList(groupId);

        Set<Long> collect1 = list.stream().filter(t -> t.getType() == 1).map(VipPolicy::getPrincipalId).collect(Collectors.toSet());
        List<MemberGroupVO> memberGroupByIds = pileUserServiceAdapter.findMemberGroupByIds(collect1);
        Map<Long, String> memberGroupMap = memberGroupByIds.stream().collect(Collectors.toMap(MemberGroupVO::getId, MemberGroupVO::getName));
        Set<Long> collect2 = list.stream().filter(t -> t.getType() == 2).map(VipPolicy::getPrincipalId).collect(Collectors.toSet());
        List<MemberVO> memberByIds = pileUserServiceAdapter.findMemberByIds(collect2);

        Map<Long, MemberVO> memberMap = new HashMap<>();
        if (!CollectionUtils.isEmpty(memberByIds)) {
            memberMap.putAll(memberByIds.stream().collect(Collectors.toMap(MemberVO::getId, e -> e, (f, s) -> f)));
        }
        if (!CollectionUtils.isEmpty(list)) {
            list.forEach(vipPolicy -> {
                // build long time
                longTimeBuild(vipPolicy);

                if (1 == vipPolicy.getType() && vipPolicy.getPrincipalId() == -1) {
                    vipPolicy.setName(MessageSourceHolder.getMessage(Constant.ALL_MEMBERS_NAME));
                }
                if (1 == vipPolicy.getType() && memberGroupMap.containsKey(vipPolicy.getPrincipalId())) {
                    String anotherString = memberGroupMap.get(vipPolicy.getPrincipalId());
                    String name = Constant.DEFAULT_DISCOUNT.equalsIgnoreCase(anotherString) ? MessageSourceHolder.getMessage(Constant.DEFAULT_DISCOUNT) : anotherString;
                    vipPolicy.setName(name);
                }
                if (2 == vipPolicy.getType() && memberMap.containsKey(vipPolicy.getPrincipalId())) {
                    MemberVO memberVO = memberMap.get(vipPolicy.getPrincipalId());
                    if (memberVO == null) {
                        return;
                    }
                    String userName = memberVO.getName();
                    String userEmail = memberVO.getEmail();
                    String userTelephone = memberVO.getTelephone();
                    String name = StringUtils.hasText(userName) ? userName : StringUtils.hasText(userEmail) ? userEmail : userTelephone;
                    vipPolicy.setName(name);
                }
                vipPolicy.setLongTerm(LONG_TERM_EXPIRE_DATE.isEqual(vipPolicy.getExpirationDate()));
            });
        }
        return list;
    }

    private void longTimeBuild(VipPolicy vipPolicy) {
        if (vipPolicy.getEffectiveDate() != null) {
            vipPolicy.setEffectiveLongDate(vipPolicy.getEffectiveDate().toInstant(ZoneOffset.UTC).toEpochMilli());
        }
        if (vipPolicy.getExpirationDate() != null) {
            vipPolicy.setExpirationLongDate(vipPolicy.getExpirationDate().toInstant(ZoneOffset.UTC).toEpochMilli());
        }
    }

    @Override
    public List<VipPolicy> findList(Long groupId) {
        LambdaQueryWrapper<OpLocationPileVipConfigEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationPileVipConfigEntity.class);
        lambdaQuery.eq(OpLocationPileVipConfigEntity::getGroupId, groupId);
        List<OpLocationPileVipConfigEntity> opLocationPileVipConfigEntities = opLocationPileVipConfigMapper.selectList(lambdaQuery);
        List<VipPolicy> list = VipPolicyTypeMapper.INSTANCE.entity2DTO(opLocationPileVipConfigEntities);
        return list;
    }

    @Override
    public List<Long> findMemberIdsByGroupId(Long groupId) {
        return null;
    }

    @Override
    public List<OpLocationPileVipConfigEntity> findByTypeAndPrincipalId(int type, Long principalId) {
        LambdaQueryWrapper<OpLocationPileVipConfigEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationPileVipConfigEntity.class);
        lambdaQuery.eq(OpLocationPileVipConfigEntity::getPrincipalId, principalId).eq(OpLocationPileVipConfigEntity::getType, type);
        return opLocationPileVipConfigMapper.selectList(lambdaQuery);
    }

    @Override
    public Integer removeMembers(MemberCleanDTO dto) {
        List<Long> ids = dto.getUserId();
        if (CollectionUtils.isEmpty(ids)) {
            return 0;
        }
        LambdaQueryWrapper<OpLocationPileVipConfigEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationPileVipConfigEntity.class);
        lambdaQuery.eq(OpLocationPileVipConfigEntity::getType, 2);
        lambdaQuery.in(OpLocationPileVipConfigEntity::getPrincipalId, ids);
        List<OpLocationPileVipConfigEntity> existEntityList = this.opLocationPileVipConfigMapper.selectList(lambdaQuery);
        if (CollectionUtils.isEmpty(existEntityList)) {
            return 0;
        }
        int count = this.opLocationPileVipConfigMapper.deleteBatchIds(existEntityList.stream().map(OpLocationPileVipConfigEntity::getId).collect(Collectors.toList()));
        return count;
    }

    @Override
    public Integer syncMemberGroupId() {
        LambdaQueryWrapper<OpLocationPileVipConfigEntity> query = Wrappers.lambdaQuery();
        query.select(OpLocationPileVipConfigEntity::getPkId, OpLocationPileVipConfigEntity::getGroupId, OpLocationPileVipConfigEntity::getPrincipalId);
        query.eq(OpLocationPileVipConfigEntity::getType, 1);
        query.eq(OpLocationPileVipConfigEntity::getPrincipalId, 0);
        List<OpLocationPileVipConfigEntity> entityList = this.list(query);
        if (CollectionUtils.isEmpty(entityList)) {
            log.info("syncMemberGroupId,entityList is empty.");
            return 0;
        }
        List<Long> groupIds = entityList.stream().map(OpLocationPileVipConfigEntity::getGroupId).distinct().collect(Collectors.toList());
        LambdaQueryWrapper<OpLocationPileGroupEntity> queryGroup = Wrappers.lambdaQuery();
        queryGroup.select(OpLocationPileGroupEntity::getId, OpLocationPileGroupEntity::getMerchantId);
        queryGroup.in(OpLocationPileGroupEntity::getId, groupIds);
        List<OpLocationPileGroupEntity> list = this.opLocationPileGroupRepository.list(queryGroup);
        if (CollectionUtils.isEmpty(list)) {
            log.info("syncMemberGroupId,list is empty.");
            return 0;
        }
        Map<Long, Long> map = list.stream().collect(Collectors.toMap(OpLocationPileGroupEntity::getId, OpLocationPileGroupEntity::getMerchantId, (f, s) -> f));
        List<Long> sellerIds = list.stream().map(OpLocationPileGroupEntity::getMerchantId).distinct().collect(Collectors.toList());
        List<MemberGroupVO> memberGroupVoList = this.pileUserFeign.findDefaultList(sellerIds).getData();
        if (CollectionUtils.isEmpty(memberGroupVoList)) {
            log.info("syncMemberGroupId,memberGroupVoList is empty.");
            return 0;
        }
        Map<Long, Long> mapToUse = memberGroupVoList.stream().collect(Collectors.toMap(MemberGroupVO::getSellerId, MemberGroupVO::getId, (f, s) -> f));
        List<OpLocationPileVipConfigEntity> updateList = new ArrayList<>();
        entityList.stream().forEach(e -> {
            Long groupId = e.getGroupId();
            Long sellerId = map.get(groupId);
            if (sellerId == null) {
                return;
            }
            Long memberGroupId = mapToUse.get(sellerId);
            if (memberGroupId == null) {
                return;
            }
            e.setPrincipalId(memberGroupId);
            updateList.add(e);
        });
        if (CollectionUtils.isEmpty(updateList)) {
            log.info("syncMemberGroupId,updateList is empty.");
            return 0;
        }
        updateList.stream().forEach(e -> {
            LambdaUpdateWrapper<OpLocationPileVipConfigEntity> updateWrapper = Wrappers.lambdaUpdate();
            updateWrapper.set(OpLocationPileVipConfigEntity::getPrincipalId, e.getPrincipalId());
            updateWrapper.eq(OpLocationPileVipConfigEntity::getPkId, e.getPkId());
            this.update(updateWrapper);
        });
        return updateList.size();
    }

    @Override
    public List<OpLocationPileVipConfigEntity> findUse() {
        return this.opLocationPileVipConfigMapper.findUse();
    }

    @Override
    public int deleteByGroupId(Long groupId) {
        LambdaQueryWrapper<OpLocationPileVipConfigEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationPileVipConfigEntity.class);
        lambdaQuery.eq(OpLocationPileVipConfigEntity::getGroupId, groupId);
        return opLocationPileVipConfigMapper.delete(lambdaQuery);
    }

    @Override
    public int deleteByGroupIds(Collection<Long> groupIds) {
        if(CollectionUtils.isEmpty(groupIds)) {
            return 0;
        }
        LambdaQueryWrapper<OpLocationPileVipConfigEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationPileVipConfigEntity.class);
        lambdaQuery.in(OpLocationPileVipConfigEntity::getGroupId, groupIds);
        return opLocationPileVipConfigMapper.delete(lambdaQuery);
    }
}
