package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.domain.repository.LocationMeterRepository;
import com.autel.cloud.pile.base.dto.meter.PageQueryDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.LocationMeterMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.LocationMeterEntity;
import com.autel.cloud.pile.base.infrastructure.util.StringUtil;
import com.autel.cloud.pile.base.vo.LocationMeterVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.ObjectUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.Collections;
import java.util.List;

/**
 * @author temp
 * @description 针对表【op_location_meter(电表)】的数据库操作Service实现
 * @createDate 2023-01-29 17:26:48
 */
@Service
@Slf4j
public class LocationMeterRepositoryImpl extends ServiceImpl<LocationMeterMapper, LocationMeterEntity>
        implements LocationMeterRepository {

    @Autowired
    private LocationMeterMapper locationMeterMapper;
    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    /**
     * @param sellerId
     * @param meterIdList
     * @param searchValue
     * @return
     * @function 根据条件获取商家下的电表信息
     */
    @Override
    public List<LocationMeterEntity> getList(Long sellerId, List<Long> meterIdList, String searchValue) {

        log.info("===>>>LocationMeterRepositoryImpl.getList sellerId : {} and meterIdList : {} and searchValue : {}",
                JSON.toJSONString(sellerId),
                JSON.toJSONString(meterIdList),
                JSON.toJSONString(searchValue));

        if (sellerId == null) {
            return null;
        }

        LambdaQueryWrapper<LocationMeterEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(LocationMeterEntity::getOperatorId, sellerId)
                .like(StringUtils.isNotBlank(searchValue), LocationMeterEntity::getName, StringUtil.escapeChar(searchValue))
                .notIn(!CollectionUtils.isEmpty(meterIdList), LocationMeterEntity::getId, meterIdList)
                .eq(LocationMeterEntity::getDeleted, 0)
                .orderByDesc(LocationMeterEntity::getUpdatedAt, LocationMeterEntity::getId);
        return locationMeterMapper.selectList(lambdaQueryWrapper);
    }

    /**
     * @param locationMeterEntity
     * @return
     * @function 添加电表
     */
    @Override
    public boolean add(LocationMeterEntity locationMeterEntity) {

        log.info("===>>>LocationMeterRepositoryImpl.add locationMeterEntity : {}",
                JSON.toJSONString(locationMeterEntity));
        String sn = locationMeterEntity.getSn();
        if (StringUtils.isNotEmpty(sn)) {
            String existKey = "energy:pile:base:meter:relation:not:exist:" + sn;
            this.stringRedisTemplate.delete(existKey);
        }

        return locationMeterMapper.insert(locationMeterEntity) > 0;
    }

    /**
     * @param locationMeterEntity
     * @return
     * @function 电表名称校验
     */
    @Override
    public boolean checkMeterNameUnique(LocationMeterEntity locationMeterEntity) {

        log.info("===>>>LocationMeterRepositoryImpl.checkMeterNameUnique locationMeterEntity : {}",
                JSON.toJSONString(locationMeterEntity));

        LambdaQueryWrapper<LocationMeterEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(LocationMeterEntity::getOperatorId, locationMeterEntity.getOperatorId())
                .ne(null != locationMeterEntity.getId(), LocationMeterEntity::getId, locationMeterEntity.getId())
                .eq(LocationMeterEntity::getName, locationMeterEntity.getName())
                .eq(LocationMeterEntity::getDeleted, 0);
        return ObjectUtils.isEmpty(locationMeterMapper.selectList(lambdaQueryWrapper));
    }

    @Override
    public boolean checkSnUnique(LocationMeterEntity locationMeterEntity) {
        log.info("===>>>LocationMeterRepositoryImpl.checkSnUnique locationMeterEntity : {}",
                JSON.toJSONString(locationMeterEntity));

        LambdaQueryWrapper<LocationMeterEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .ne(null != locationMeterEntity.getId(), LocationMeterEntity::getId, locationMeterEntity.getId())
                .eq(LocationMeterEntity::getSn, locationMeterEntity.getSn())
                .eq(LocationMeterEntity::getDeleted, 0);
        return ObjectUtils.isEmpty(locationMeterMapper.selectList(lambdaQueryWrapper));
    }

    @Override
    public List<LocationMeterEntity> findListBySn(List<String> sns) {
        LambdaQueryWrapper<LocationMeterEntity> query = Wrappers.lambdaQuery();
        query.in(LocationMeterEntity::getSn, sns);
        query.eq(LocationMeterEntity::getDeleted, 0);
        return this.list(query);
    }

    @Override
    public List<LocationMeterEntity> getByMeterIds(List<Long> meterIds) {
        LambdaQueryWrapper<LocationMeterEntity> query = Wrappers.lambdaQuery();
        query.in(LocationMeterEntity::getId,meterIds);
        query.eq(LocationMeterEntity::getDeleted, 0);
        return this.list(query);
    }

    /**
     * @return
     * @function 逻辑删除电表
     */
    @Override
    public boolean logicDelete(Long id) {

        log.info("===>>>LocationMeterRepositoryImpl.logicDelete id : {}",
                JSON.toJSONString(id));

        if (id == null) {
            return false;
        }

        LocationMeterEntity locationMeterEntity = locationMeterMapper.selectById(id);
        if (locationMeterEntity == null) {
            return false;
        }

        String sn = locationMeterEntity.getSn();
        if (StringUtils.isNotEmpty(sn)) {
            String key = "energy:pile:base:meter:relation:" + sn;
            this.stringRedisTemplate.delete(key);
        }

        Long currentTimeMillis = System.currentTimeMillis();
        locationMeterEntity.setDeleted(1);
        locationMeterEntity.setUpdatedAt(currentTimeMillis);
        return locationMeterMapper.updateById(locationMeterEntity) > 0;
    }

    /**
     * @param pageQueryDTO
     * @return
     * @function 根据条件查询电表信息
     */
    @Override
    public List<LocationMeterEntity> queryPages(PageQueryDTO pageQueryDTO) {

        log.info("===>>>LocationMeterRepositoryImpl.queryPages pageQueryDTO : {}",
                JSON.toJSONString(pageQueryDTO));

        LambdaQueryWrapper<LocationMeterEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(LocationMeterEntity::getOperatorId, pageQueryDTO.getOperatorId())
                .eq(pageQueryDTO.getBrandEnum() != null, LocationMeterEntity::getBrandEnum, pageQueryDTO.getBrandEnum())
                .like(StringUtils.isNotBlank(pageQueryDTO.getSearchValue()), LocationMeterEntity::getName, StringUtil.escapeChar(pageQueryDTO.getSearchValue()))
                .eq(LocationMeterEntity::getDeleted, 0);

        String OrderBy = pageQueryDTO.getOrderBy();
        String OrderType = pageQueryDTO.getOrderType();
        // 传进来为空的话就给默认值
        if(StringUtils.isEmpty(OrderBy)){
            OrderBy = "name";
        }
        if(StringUtils.isEmpty(OrderType)){
            OrderType = "ASC";
        }
        Boolean isASC = "ASC".equalsIgnoreCase(OrderType) ? true : false;
        if("name".equalsIgnoreCase(OrderBy)){
            lambdaQueryWrapper.last("order by CONVERT(name USING gbk ) COLLATE GBK_CHINESE_CI " + OrderType);
        } else{
            lambdaQueryWrapper.orderBy(true, isASC, LocationMeterEntity::getCreatedAt);
        }
        return locationMeterMapper.selectList(lambdaQueryWrapper);
    }


}




