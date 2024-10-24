package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.pile.base.domain.model.DemandControlAddConfigDTO;
import com.autel.cloud.pile.base.domain.service.DemandControlConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.infrastructure.mapper.DemandControlConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.DemandControlConfigEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import groovy.lang.Lazy;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;
import java.util.Set;

/**
* @author wang
* @description 针对表【demand_control_config(需求费控制器表)】的数据库操作Service实现
* @createDate 2024-04-30 10:16:41
*/
@Service
public class DemandControlConfigServiceImpl extends ServiceImpl<DemandControlConfigMapper, DemandControlConfigEntity>
    implements DemandControlConfigService {
    @Resource
    private DemandControlConfigMapper demandControlConfigMapper;
    @Resource
    @Lazy
    private OpLocationPileGroupService opLocationPileGroupService;


    @Override
    public String findContentByGroupId(Long id) {
        return demandControlConfigMapper.findContentByGroupId(id);
    }

    @Override
    public List<DemandControlConfigEntity> listDemandControlConfig(DemandControlAddConfigDTO dto) {
        //根据商家id获取需求费控制器配置列表
        LambdaQueryWrapper<DemandControlConfigEntity> queryWrapper = Wrappers.lambdaQuery(DemandControlConfigEntity.class);
        queryWrapper.eq(DemandControlConfigEntity::getMerchantId, dto.getMerchantId());
        queryWrapper.eq(DemandControlConfigEntity::getDeleted, 0);
        queryWrapper.orderByDesc(DemandControlConfigEntity::getCreatedAt);
        List<DemandControlConfigEntity> list = demandControlConfigMapper.selectList(queryWrapper);
        return list;
    }

    @Override
    public List<DemandControlConfigEntity> listAllDemandControlConfig(DemandControlAddConfigDTO dto) {
        //根据商家id获取需求费控制器配置列表
        LambdaQueryWrapper<DemandControlConfigEntity> queryWrapper = Wrappers.lambdaQuery(DemandControlConfigEntity.class);
        queryWrapper.eq(DemandControlConfigEntity::getMerchantId, dto.getMerchantId());
        List<DemandControlConfigEntity> list = demandControlConfigMapper.selectList(queryWrapper);
        return list;
    }

    @Override
    public Boolean updateDemandControlStatus(Integer status, Integer deleted, Long id) {
        //更新需求费控制器状态
        DemandControlConfigEntity entity = new DemandControlConfigEntity();
        //根据入参设置需求费控制器状态
        if (status != null) {
            entity.setStatus(status);
            DemandControlConfigEntity demandControlConfigEntity = demandControlConfigMapper.selectById(id);
            if (status == 0) {
                //根据id获取需求费控制器配置
                opLocationPileGroupService.clearSettings(demandControlConfigEntity.getGroupId());
            }else {
                opLocationPileGroupService.distributeSettings(demandControlConfigEntity.getGroupId());
            }
        }
        //根据入参设置需求费控制器删除状态
        if (deleted != null) {
            entity.setDeleted(deleted);
            if (deleted == 1) {
                //根据id获取需求费控制器配置
                DemandControlConfigEntity demandControlConfigEntity = demandControlConfigMapper.selectById(id);
                opLocationPileGroupService.deleteV3(demandControlConfigEntity.getGroupId());
                entity.setGroupId(null);
            }
        }
        LambdaQueryWrapper<DemandControlConfigEntity> queryWrapper = Wrappers.lambdaQuery(DemandControlConfigEntity.class);
        queryWrapper.eq(DemandControlConfigEntity::getId, id);
        int update = demandControlConfigMapper.update(entity, queryWrapper);
        return update > 0;
    }

    @Override
    public int countDemandControlConfig(DemandControlAddConfigDTO dto) {
        LambdaQueryWrapper<DemandControlConfigEntity> queryWrapper = Wrappers.lambdaQuery(DemandControlConfigEntity.class);
        queryWrapper.eq(DemandControlConfigEntity::getMerchantId, dto.getMerchantId());
        queryWrapper.eq(DemandControlConfigEntity::getDeleted, 0);
        queryWrapper.orderByDesc(DemandControlConfigEntity::getCreatedAt);
        return demandControlConfigMapper.selectCount(queryWrapper);
    }

    @Override
    public void deleteDemandControlById(Long rootId) {
        LambdaQueryWrapper<DemandControlConfigEntity> queryWrapper = Wrappers.lambdaQuery(DemandControlConfigEntity.class);
        queryWrapper.eq(DemandControlConfigEntity::getGroupId, rootId);
        demandControlConfigMapper.delete(queryWrapper);
    }

    @Override
    public void deleteByGroupIds(Set<Long> deleteIds) {
        LambdaQueryWrapper<DemandControlConfigEntity> queryWrapper = Wrappers.lambdaQuery(DemandControlConfigEntity.class);
        queryWrapper.in(DemandControlConfigEntity::getGroupId, deleteIds);
        demandControlConfigMapper.delete(queryWrapper);
    }
}




