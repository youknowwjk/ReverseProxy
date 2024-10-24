package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.repository.OpLocationPileGroupRepository;
import com.autel.cloud.pile.base.domain.service.DemandControlConfigService;
import com.autel.cloud.pile.base.domain.service.OpLocationPileGroupService;
import com.autel.cloud.pile.base.dto.OpLocationPileGroupParamDTO;
import com.autel.cloud.pile.base.dto.PileGroupParamDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileGroupAssociateMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileGroupMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileGroupEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.annotation.XxlJob;
import groovy.lang.Lazy;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import javax.annotation.PostConstruct;
import javax.annotation.Resource;
import java.util.*;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.constant.BaseConstant.OP_LOCATION_PILE_GROUP_ROOT_ID;

/**
 * <p>
 * 充电桩群组 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-07-13
 */
@Service
@Slf4j
public class OpLocationPileGroupRepositoryImpl
        extends ServiceImpl<OpLocationPileGroupMapper, OpLocationPileGroupEntity>
        implements OpLocationPileGroupRepository {

    @Resource
    private OpLocationPileGroupAssociateMapper opLocationPileGroupAssociateMapper;

    @Resource
    private OpLocationPileGroupMapper opLocationPileGroupMapper;

    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Resource
    @Lazy
    private DemandControlConfigService demandControlConfigService;


    @PostConstruct
    public void evictCache(){
        evictCache("*");
    }

    @XxlJob("evictCacheOpLocationPileGroupEntity")
    public ReturnT<String> evictCache(String match){
        String opLocationPileGroupEntityKey = String.join(":", OpLocationPileGroupEntity.class.getName(), match);
        stringRedisTemplate.delete(opLocationPileGroupEntityKey);
        return ReturnT.SUCCESS;
    }

    public void evictCache(Long id) {
        Collection<String> deleteKeys = new HashSet<>();
        String opLocationPileGroupEntityKey = genRedisKey("getById", id.toString());;
        deleteKeys.add(opLocationPileGroupEntityKey);
        String findChildrenKey = genRedisKey("findChildren", id.toString());
        deleteKeys.add(findChildrenKey);
        stringRedisTemplate.delete(deleteKeys);
    }

    public void evictCache(Collection<Long> ids){
        Collection<String> deleteKeys = new HashSet<>();
        for (Long id : ids) {
            String opLocationPileGroupEntityKey = genRedisKey("getById", id.toString());;
            deleteKeys.add(opLocationPileGroupEntityKey);
            String findChildrenKey = genRedisKey("findChildren", id.toString());
            deleteKeys.add(findChildrenKey);
        }
        stringRedisTemplate.delete(deleteKeys);
    }

    public static String genRedisKey(String method, String param) {
       return String.join(":", OpLocationPileGroupEntity.class.getName(), method, param);
    }

    @Override
    public OpLocationPileGroupEntity getById(Long id) {
        /*String opLocationPileGroupEntityKey = genRedisKey("getById", id.toString());
        String o = stringRedisTemplate.opsForValue().get(opLocationPileGroupEntityKey);
        if (StringUtils.hasText(o)) {
            return JSON.parseObject(o, OpLocationPileGroupEntity.class);
        }*/
        OpLocationPileGroupEntity opLocationPileGroupEntity = getBaseMapper().selectById(id);
        //stringRedisTemplate.opsForValue().set(opLocationPileGroupEntityKey, JSON.toJSONString(opLocationPileGroupEntity),10, TimeUnit.MINUTES);
        return opLocationPileGroupEntity;
    }

    @Override
    public List<Long> findGroupId(OpLocationPileGroupParamDTO dto) {
        return opLocationPileGroupMapper.findGroupId(dto);
    }

    @Override
    public List<Long> findGroupIdV3(PileGroupParamDTO dto) {
        return opLocationPileGroupMapper.findGroupIdV3(dto);
    }


    @Override
    public List<Long> findGroupId(Long locationId, Long pid, Long self) {
        return opLocationPileGroupMapper.findGroupIdWithPid(locationId, pid, self);
    }

    @Override
    public Set<String> findRemovePileSns(Long merchantId, Long id, Set<String> includeSubPileSns) {
        return opLocationPileGroupMapper.findRemovePileSns(merchantId, id, includeSubPileSns);
    }

    @Override
    public Set<Long> findRemoveSubGroupIds(Long merchantId, Long id, Set<Long> includeSubGroupIds) {
        return opLocationPileGroupMapper.findRemoveSubGroupIds(merchantId, id, includeSubGroupIds);
    }

    @Override
    public int countByMerchantId(Long merchantId) {
        return this.count(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(OpLocationPileGroupEntity::getMerchantId, merchantId)
                .eq(OpLocationPileGroupEntity::getDeleted, 0));
    }

    @Override
    public void updateBatchGroupType(Integer groupType, Set<Long> updateIds) {
        if (CollectionUtils.isEmpty(updateIds)) {
            return;
        }
        opLocationPileGroupMapper.updateBatchGroupType(groupType, updateIds);
    }


    @Override
    public int updatePidByIds(Set<Long> removeGroupIds) {
        if (CollectionUtils.isEmpty(removeGroupIds)) {
            return 0;
        }
        evictCache(removeGroupIds);
        return opLocationPileGroupMapper.updatePidByIds(removeGroupIds, OP_LOCATION_PILE_GROUP_ROOT_ID,BaseConstant.NORMAL_GROUP_TYPE);
    }

    @Override
    public Page<OpLocationPileGroupEntity> findRootGroup(PileGroupParamDTO pileGroupParamDTO) {
        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class);
        queryWrapper.eq(OpLocationPileGroupEntity::getMerchantId, pileGroupParamDTO.getMerchantId())
                .eq(OpLocationPileGroupEntity::getDeleted, 0)
                .eq(OpLocationPileGroupEntity::getPid, 0)
                .eq(Objects.nonNull(pileGroupParamDTO.getLoadType()), OpLocationPileGroupEntity::getLoadType, pileGroupParamDTO.getLoadType())
                .in(!CollectionUtils.isEmpty(pileGroupParamDTO.getRootIds()), OpLocationPileGroupEntity::getId, pileGroupParamDTO.getRootIds())
                .inSql(OpLocationPileGroupService.MULTI_LAYER.equals(pileGroupParamDTO.getPid()), OpLocationPileGroupEntity::getId, "SELECT pid FROM  op_location_pile_group where  deleted = 0  and merchant_id = " + pileGroupParamDTO.getMerchantId())
                .notInSql(OpLocationPileGroupService.SINGLE_LAYER.equals(pileGroupParamDTO.getPid()), OpLocationPileGroupEntity::getId, "SELECT pid FROM op_location_pile_group ")
                .orderByDesc(OpLocationPileGroupEntity::getId);
        Page<OpLocationPileGroupEntity> page = new Page<>(pileGroupParamDTO.getPage(), pileGroupParamDTO.getPageSize());
        return opLocationPileGroupMapper.selectPage(page, queryWrapper);
    }

    @Override
    public List<OpLocationPileGroupEntity> findByPidAndMerchantId(Long merchantId, Long pid, Long self) {
        LambdaQueryWrapper<OpLocationPileGroupEntity> lambdaQuery = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class);
        lambdaQuery
                .eq(Objects.nonNull(self), OpLocationPileGroupEntity::getId, self)
                .eq(OpLocationPileGroupEntity::getMerchantId, merchantId).eq(OpLocationPileGroupEntity::getPid, pid)
                .eq(OpLocationPileGroupEntity::getDeleted, 0);
        return opLocationPileGroupMapper.selectList(lambdaQuery);
    }

    @Override
    public List<Long> findGroupIdByPidAndMerchantId(Long merchantId, Long pid, Long self) {
        return opLocationPileGroupMapper.findGroupIdByPidAndMerchantId(merchantId, pid, self);
    }

    @Override
    public Long findOrdinaryRoot(Long id) {
        List<Long> rootList = opLocationPileGroupMapper.findOrdinaryRoot(id);
        if (!CollectionUtils.isEmpty(rootList)) {
            return rootList.get(0);
        }
        return null;
    }

    @Override
    public Long findRootByPileSn(String pileSn) {
        List<Long> groupIds = opLocationPileGroupAssociateMapper.findGroupIdByPileSn(pileSn);
        log.info("pileSn: {} belong: {}", pileSn, JSON.toJSONString(groupIds));
        if (CollectionUtils.isEmpty(groupIds)) {
            return null;
        }
        Assert.state(groupIds.size() == 1, pileSn + "relation multi group" + JSON.toJSONString(groupIds));
        List<Long> rootList = opLocationPileGroupMapper.findOrdinaryRoot(groupIds.get(0));
        if (!CollectionUtils.isEmpty(rootList)) {
            return rootList.get(0);
        }
        return null;
    }

    @Override
    public List<OpLocationPileGroupEntity> findChildren(Long id) {
        /*String findChildrenKey = genRedisKey("findChildren" , id.toString());
        String o = stringRedisTemplate.opsForValue().get(findChildrenKey);
        if (StringUtils.hasText(o)) {
            return JSON.parseArray(o, OpLocationPileGroupEntity.class);
        }*/
        List<OpLocationPileGroupEntity> list = opLocationPileGroupMapper.findChildren(id, null);
        //stringRedisTemplate.opsForValue().set(findChildrenKey, JSON.toJSONString(list),10, TimeUnit.MINUTES);
        return list;

    }

    @Override
    public List<OpLocationPileGroupEntity> findChildren(Long id, Integer status) {
        return opLocationPileGroupMapper.findChildren(id, status);
    }

    @Override
    public List<OpLocationPileGroupEntity> findChildren(Long id, boolean includeParent) {
        List<OpLocationPileGroupEntity> childrenList = this.findChildren(id);
        if (!includeParent && !CollectionUtils.isEmpty(childrenList)) {
            List<OpLocationPileGroupEntity> newList = new ArrayList<>(childrenList);
            newList.remove(childrenList.get(0));
            return newList;
        }
        return childrenList;
    }

    @Override
    public boolean updateBatch(List<OpLocationPileGroupEntity> pileGroupEntityList) {
        if (CollectionUtils.isEmpty(pileGroupEntityList)) {
            return false;
        }
        evictCache(pileGroupEntityList.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toSet()));
        return opLocationPileGroupMapper.updateBatch(pileGroupEntityList);
    }

    @Override
    public boolean insert(OpLocationPileGroupEntity pileGroupEntity) {
        return this.save(pileGroupEntity);
    }

    @Override
    public List<OpLocationPileGroupEntity> findDetails(Set<Long> rootIds) {
        return this.listByIds(rootIds);
    }

    @Override
    public boolean check(Long id) {
        return this.count(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(OpLocationPileGroupEntity::getId, id)
                .eq(OpLocationPileGroupEntity::getDeleted, 0)) > 0;
    }

    @Override
    public List<Long> queryEmsGroupId() {
        List<OpLocationPileGroupEntity> list = this.lambdaQuery().select(OpLocationPileGroupEntity::getId)
                .eq(OpLocationPileGroupEntity::getGroupType, 3)
                .eq(OpLocationPileGroupEntity::getDeleted, 0).list();
        if(list != null && list.size() >0){
            return list.stream().map(OpLocationPileGroupEntity::getId).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    @Override
    public boolean updateEntity(OpLocationPileGroupEntity pileGroupEntity) {
        evictCache(pileGroupEntity.getId());
        return this.update(new LambdaUpdateWrapper<OpLocationPileGroupEntity>()
                .set(OpLocationPileGroupEntity::getCreatedAt, pileGroupEntity.getCreatedAt())
                .set(OpLocationPileGroupEntity::getUpdatedAt, pileGroupEntity.getUpdatedAt())
                .set(OpLocationPileGroupEntity::getDeleted, pileGroupEntity.getDeleted())
                .set(OpLocationPileGroupEntity::getStatus, pileGroupEntity.getStatus())
                .set(OpLocationPileGroupEntity::getNote, pileGroupEntity.getNote())
                .set(OpLocationPileGroupEntity::getName, pileGroupEntity.getName())
                .set(OpLocationPileGroupEntity::getUseSmartCharging, pileGroupEntity.getUseSmartCharging())
                .set(OpLocationPileGroupEntity::getChargingUp, pileGroupEntity.getChargingUp())
                .set(OpLocationPileGroupEntity::getChargingUpType, pileGroupEntity.getChargingUpType())
                .set(OpLocationPileGroupEntity::getChargingUpUnit, pileGroupEntity.getChargingUpUnit())
                .set(OpLocationPileGroupEntity::getSmartChargingMode, pileGroupEntity.getSmartChargingMode())
                .set(OpLocationPileGroupEntity::getTimeSettingMode, pileGroupEntity.getTimeSettingMode())
                .set(OpLocationPileGroupEntity::getLocationId, pileGroupEntity.getLocationId())
                .set(OpLocationPileGroupEntity::getPhaseNum, pileGroupEntity.getPhaseNum())
                .set(OpLocationPileGroupEntity::getMeterLocation, pileGroupEntity.getMeterLocation())
                .set(OpLocationPileGroupEntity::getAllocationStrategy, pileGroupEntity.getAllocationStrategy())
                .set(OpLocationPileGroupEntity::getPid, pileGroupEntity.getPid())
                .set(OpLocationPileGroupEntity::getLoadType, pileGroupEntity.getLoadType())
                .set(OpLocationPileGroupEntity::getEnergyUseStrategy, pileGroupEntity.getEnergyUseStrategy())
                .set(OpLocationPileGroupEntity::getTariffId, pileGroupEntity.getTariffId())
                .set(OpLocationPileGroupEntity::getFavor, pileGroupEntity.getFavor())
                .set(OpLocationPileGroupEntity::getPlanTime, pileGroupEntity.getPlanTime())
                .set(OpLocationPileGroupEntity::getMeterId, pileGroupEntity.getMeterId())
                .set(OpLocationPileGroupEntity::getPriority, pileGroupEntity.getPriority())
                .set(OpLocationPileGroupEntity::getSecurityEnabled, pileGroupEntity.getSecurityEnabled())
                .set(OpLocationPileGroupEntity::getOfflineValue, pileGroupEntity.getOfflineValue())
                .set(OpLocationPileGroupEntity::getMinReserve, pileGroupEntity.getMinReserve())
                .set(OpLocationPileGroupEntity::getPowerEquipmentEnabled, pileGroupEntity.getPowerEquipmentEnabled())
                .set(OpLocationPileGroupEntity::getPowerEquipmentStartTime, pileGroupEntity.getPowerEquipmentStartTime())
                .set(OpLocationPileGroupEntity::getPowerEquipmentUp, pileGroupEntity.getPowerEquipmentUp())
                .set(OpLocationPileGroupEntity::getElectricUp, pileGroupEntity.getElectricUp())
                .set(OpLocationPileGroupEntity::getMeterVoltage, pileGroupEntity.getElectricUp())
                .set(OpLocationPileGroupEntity::getTimeSettingDetail, pileGroupEntity.getTimeSettingDetail())
                .eq(OpLocationPileGroupEntity::getId, pileGroupEntity.getId())
        );
    }

    @Override
    public OpLocationPileGroupEntity findOne(Long id) {
        return this.getById(id);
    }

    @Override
    public OpLocationPileGroupEntity findOne(Long locationId, Long meterId) {
        return this.getOne(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(locationId != null, OpLocationPileGroupEntity::getLocationId, locationId)
                .eq(OpLocationPileGroupEntity::getMeterId, meterId)
                .eq(OpLocationPileGroupEntity::getLoadType, 1)
                .eq(OpLocationPileGroupEntity::getStatus, 1)
                .eq(OpLocationPileGroupEntity::getDeleted, 0));
    }

    @Override
    public List<OpLocationPileGroupEntity> findList(Long locationId, Long priceId) {
        return this.list(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(OpLocationPileGroupEntity::getLocationId, locationId)
                .eq(OpLocationPileGroupEntity::getTariffId, priceId)
                .eq(OpLocationPileGroupEntity::getDeleted, 0));
    }

    @Override
    public boolean deleteBatch(Set<Long> deleteIds) {
        if (CollectionUtils.isEmpty(deleteIds)) return true;
        this.updateBatchById(deleteIds.stream().map(id -> {
            OpLocationPileGroupEntity entity = new OpLocationPileGroupEntity();
            entity.setId(id);
            entity.setDeleted(1);
            entity.setUpdatedAt(System.currentTimeMillis());
            evictCache(id);
            return entity;
        }).collect(Collectors.toList()));
        evictCache(deleteIds);
        //按照groupId删除需求控制器
        demandControlConfigService.deleteByGroupIds(deleteIds);
        return true;
    }


    @Override
    public boolean deleteByIds(Collection<Long> deleteIds) {
        if (CollectionUtils.isEmpty(deleteIds)) {
            return true;
        }
        evictCache(deleteIds);
        return removeByIds(deleteIds);
    }


    @Override
    public OpLocationPileGroupEntity findName(String pileSn) {
        return opLocationPileGroupMapper.findName(pileSn);
    }

    @Override
    public List<OpLocationPileGroupEntity> findAllRoot(Long rootId) {
        return this.list(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(OpLocationPileGroupEntity::getDeleted, 0)
                .eq(rootId != null, OpLocationPileGroupEntity::getId, rootId)
                .eq(OpLocationPileGroupEntity::getPid, OP_LOCATION_PILE_GROUP_ROOT_ID)
                .eq(OpLocationPileGroupEntity::getStatus, 1));
    }

    @Override
    public List<OpLocationPileGroupEntity> findByIds(List<Long> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return new ArrayList<>();
        }
        return this.listByIds(ids);
    }

    @Override
    public List<OpLocationPileGroupEntity> findAll() {
        return this.list(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(OpLocationPileGroupEntity::getDeleted, 0));
    }

    @Override
    public List<OpLocationPileGroupEntity> findByLocationId(Long locationId) {
        return this.list(new LambdaQueryWrapper<OpLocationPileGroupEntity>()
                .eq(OpLocationPileGroupEntity::getLocationId, locationId)
                .eq(OpLocationPileGroupEntity::getDeleted, 0));
    }

    @Override
    public Page<OpLocationPileGroupEntity> findChildrenPageList(Long groupId, Integer page, Integer pageSize) {
        QueryWrapper<OpLocationPileGroupEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("pid", groupId);
        queryWrapper.eq("deleted", 0);
        queryWrapper.orderByDesc("created_at").orderByDesc("id");
        Page<OpLocationPileGroupEntity> pageObject = new Page<>(page, pageSize);
        return this.page(pageObject, queryWrapper);
    }

    @Override
    public List<OpLocationPileGroupEntity> findChildrenPageListV3(Long groupId) {
        QueryWrapper<OpLocationPileGroupEntity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("pid", groupId);
        queryWrapper.eq("deleted", 0);
        queryWrapper.orderByDesc("created_at").orderByDesc("id");

        return opLocationPileGroupMapper.selectList(queryWrapper);
    }


    /**
     * @param meterIdList
     * @return
     * @function 根据电表id集合查询充电桩群组信息
     */
    @Override
    public List<OpLocationPileGroupEntity> findAllByMeterIdList(List<Long> meterIdList) {

        log.info("===>>>OpLocationPileGroupRepositoryImpl.findAllByMeterIdList meterIdList : {}",
                JSON.toJSONString(meterIdList));

        if (ObjectUtils.isEmpty(meterIdList)) {
            return null;
        }

        LambdaQueryWrapper<OpLocationPileGroupEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .in(OpLocationPileGroupEntity::getMeterId, meterIdList)
                .eq(OpLocationPileGroupEntity::getDeleted, 0)
                .orderByDesc(OpLocationPileGroupEntity::getUpdatedAt, OpLocationPileGroupEntity::getId);
        return opLocationPileGroupMapper.selectList(lambdaQueryWrapper);
    }

    @Override
    public boolean jugeEdgeGroupId(Long groupId) {
        return opLocationPileGroupMapper.countEdgeGroupId(groupId)==0;
    }

    @Override
    public void updateSubGroupType(Long rootId) {
        //所有子群组id
        List<Long> childIds = opLocationPileGroupMapper.selectChildIds(rootId,Arrays.asList(2),false);
        if (CollectionUtils.isEmpty(childIds)) {
            return;
        }
        LambdaUpdateWrapper<OpLocationPileGroupEntity> wp=new LambdaUpdateWrapper<>();
        wp.in(OpLocationPileGroupEntity::getId,childIds);
        wp.set(OpLocationPileGroupEntity::getGroupType,0);
        int update = opLocationPileGroupMapper.update(null, wp);
        log.info("清除子组边缘云标记,rows:{},ids:{}",update,JSON.toJSONString(childIds));
    }
    //子级是否含边缘云、ems
    public boolean hasEdgeEMs(Long rootId){
        List<Long> childIds = opLocationPileGroupMapper.selectChildIds(rootId, Arrays.asList(2,3),true);
        if (CollectionUtils.isEmpty(childIds)) {
            return false;
        }
        LambdaQueryWrapper<OpLocationPileGroupEntity> queryWrapper=new LambdaQueryWrapper<>();
        queryWrapper.in(OpLocationPileGroupEntity::getId,childIds);
        queryWrapper.in(OpLocationPileGroupEntity::getGroupType, Arrays.asList(3,2));
        int edgeCount = this.count(queryWrapper);
        return edgeCount>0;
    }
}