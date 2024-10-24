package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.constant.RedisKeyConstant;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupAssociateRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileGroupAssociateMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupAssociateEntity;
import com.autel.cloud.pile.base.util.ThreadPoolUtil;
import com.autel.cloud.pile.base.vo.OpLocationPileGroupAssociateVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * <p>
 * 桩和桩组关系表 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@Service
public class OpLocationPileGroupAssociateRepositoryImpl
        extends ServiceImpl<OpLocationPileGroupAssociateMapper, OpLocationPileGroupAssociateEntity>
        implements OpLocationPileGroupAssociateRepository {

    @Resource
    private OpLocationPileGroupAssociateMapper opLocationPileGroupAssociateMapper;
    @Autowired
    @Qualifier("redisTemplates")
    private RedisTemplate<String, Object> redisTemplate;

    @Override
    public List<Long> findGroupId(Long locationId, String pileSn) {
        return opLocationPileGroupAssociateMapper.findGroupId(locationId, pileSn);
    }

    @Override
    public List<Long> findGroupIdByMerchantIdAndPileSn(Long merchantId, String pileSn) {
        return opLocationPileGroupAssociateMapper.findGroupIdByMerchantIdAndPileSn(merchantId, pileSn);
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findList(Set<Long> groupIds) {
        return opLocationPileGroupAssociateMapper.findList(groupIds);
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findList(Long groupId) {
        return this.list(new LambdaQueryWrapper<OpLocationPileGroupAssociateEntity>()
                .eq(OpLocationPileGroupAssociateEntity::getGroupId, groupId)
                .eq(OpLocationPileGroupAssociateEntity::getDeleted, 0));
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findAll() {
        return this.list(new LambdaQueryWrapper<OpLocationPileGroupAssociateEntity>().eq(OpLocationPileGroupAssociateEntity::getDeleted, 0));
    }

    @Override
    public boolean updateBatch(List<OpLocationPileGroupAssociateEntity> list) {
        return this.updateBatchById(list);
    }

    @Override
    public List<OpLocationPileGroupAssociateVO> findDetailList(List<String> pileSnList) {
        return opLocationPileGroupAssociateMapper.findDetailList(pileSnList);
    }

    @Override
    public boolean insertBatch(List<OpLocationPileGroupAssociateEntity> associateEntityList) {
        return this.saveBatch(associateEntityList);
    }

    @Override
    public boolean deleteByGroupIds(Collection<Long> deleteIds) {
        if (CollectionUtils.isEmpty(deleteIds)) return true;

        Wrapper<OpLocationPileGroupAssociateEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileGroupAssociateEntity.class)
                .in(OpLocationPileGroupAssociateEntity::getGroupId, deleteIds);
        return super.remove(queryWrapper);
    }

    @Override
    public boolean deleteBatch(List<OpLocationPileGroupAssociateEntity> associateEntityList) {
        if (CollectionUtils.isEmpty(associateEntityList)) {
            return true;
        }
        List<OpLocationPileGroupAssociateEntity> list = associateEntityList.stream().map(entity -> {
            entity.setDeleted(Boolean.TRUE);
            entity.setUpdatedAt(System.currentTimeMillis());
            return entity;
        }).collect(Collectors.toList());
        return this.updateBatch(list);
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findListDeleted(List<String> pileSnList) {
        return opLocationPileGroupAssociateMapper.findListDeleted(pileSnList,1);
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findList(List<String> pileSnList) {
        return opLocationPileGroupAssociateMapper.findListDeleted(pileSnList,0);
    }

    @Override
    public OpLocationPileGroupAssociateEntity findOne(String pileSn) {
        return this.getOne(new LambdaQueryWrapper<OpLocationPileGroupAssociateEntity>()
                .eq(OpLocationPileGroupAssociateEntity::getPileSn, pileSn)
                .eq(OpLocationPileGroupAssociateEntity::getDeleted, 0));
    }

    @Override
    public OpLocationPileGroupAssociateEntity findOne(Long pileId) {
        return this.getOne(new LambdaQueryWrapper<OpLocationPileGroupAssociateEntity>()
                .eq(OpLocationPileGroupAssociateEntity::getPileId, pileId)
                .eq(OpLocationPileGroupAssociateEntity::getDeleted, 0));
    }

    @Override
    public Page<OpLocationPileGroupAssociateEntity> findPageList(Long groupId, int page, int pageSize) {
        QueryWrapper<OpLocationPileGroupAssociateEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("group_id", groupId);
        queryWrapper.eq("deleted", 0);
        queryWrapper.orderByAsc("pile_sn").orderByDesc("created_at").orderByDesc("id");
        Page<OpLocationPileGroupAssociateEntity> pageObject = new Page<>(page, pageSize);
        return this.page(pageObject, queryWrapper);
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findPageListV3(Long groupId) {
        QueryWrapper<OpLocationPileGroupAssociateEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("group_id", groupId);
        queryWrapper.eq("deleted", 0);
        queryWrapper.orderByAsc("pile_sn").orderByDesc("created_at").orderByDesc("id");
        return opLocationPileGroupAssociateMapper.selectList(queryWrapper);
    }

    @Override
    public List<OpLocationPileGroupAssociateEntity> findAssociateList(List<Long> groupIdList, int pageSize) {
        return opLocationPileGroupAssociateMapper.findAssociateList(groupIdList, pageSize);
    }

    @Override
    public List<Long> findGroupIdByMerchantIdAndPileSnAndGroupName(long merchantId, String pileSN, String groupName) {
        return opLocationPileGroupAssociateMapper.findGroupIdByMerchantIdAndPileSnAndGroupName(merchantId, pileSN, groupName);
    }

    @Override
    public List<Long> findDemandGroupIds(long merchantId) {
        return opLocationPileGroupAssociateMapper.findDemandGroupIds(merchantId);
    }
}