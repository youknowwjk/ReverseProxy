package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.pile.base.domain.convert.OpPileShareWhiteListConvert;
import com.autel.cloud.pile.base.domain.repository.OpPileShareWhiteListRepository;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListDTO;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListPageDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpPileShareWhiteListMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileShareWhiteListEntity;
import com.autel.cloud.pile.base.vo.OpPileShareWhiteListPageVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Log4j2
@Service
public class OpPileShareWhiteListRepositoryImpl extends ServiceImpl<OpPileShareWhiteListMapper, OpPileShareWhiteListEntity> implements OpPileShareWhiteListRepository {


    @Value("${customs.config.isStartHomeShared:false}")
    private Boolean isStartHomeShared = Boolean.FALSE;

    private final OpPileShareWhiteListMapper opPileShareWhiteListMapper;

    public OpPileShareWhiteListRepositoryImpl(OpPileShareWhiteListMapper opPileShareWhiteListMapper) {
        this.opPileShareWhiteListMapper = opPileShareWhiteListMapper;
    }

    @Override
    public Boolean isExistByPileSn(String pileSn) {
        if (Boolean.FALSE.equals(isStartHomeShared)) {
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }

    @Override
    public Page<OpPileShareWhiteListPageVO> getWhiteListPage(OpPileShareWhiteListPageDTO opPileShareWhiteListPageDTO) {
        //搜索值
        LambdaQueryWrapper<OpPileShareWhiteListEntity> queryWrapper = Wrappers.lambdaQuery(OpPileShareWhiteListEntity.class)
                .select(OpPileShareWhiteListEntity::getId, OpPileShareWhiteListEntity::getCreatedAt, OpPileShareWhiteListEntity::getUpdatedAt, OpPileShareWhiteListEntity::getCreatedBy, OpPileShareWhiteListEntity::getPileSn);
        Page<OpPileShareWhiteListEntity> page = new Page<>(opPileShareWhiteListPageDTO.getPage(), opPileShareWhiteListPageDTO.getPageSize());
        Page<OpPileShareWhiteListEntity> listEntityPage = opPileShareWhiteListMapper.selectPage(page, queryWrapper);
        List<OpPileShareWhiteListEntity> listEntities = listEntityPage.getRecords();
        List<OpPileShareWhiteListPageVO> records = new ArrayList<>();
        listEntities.forEach(entity -> records.add(OpPileShareWhiteListConvert.toOpPileShareWhiteListPageVO(entity)));
        //分页
        Page<OpPileShareWhiteListPageVO> voPage = new Page<>();
        voPage.setRecords(records);
        voPage.setTotal(records.size());
        voPage.setSize(opPileShareWhiteListPageDTO.getPageSize());
        voPage.setCurrent(opPileShareWhiteListPageDTO.getPage());
        return voPage;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public OpPileShareWhiteListEntity create(OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        log.info("OpPileShareWhiteListRepositoryImpl -> create{}", JSON.toJSONString(opPileShareWhiteListDTO));
        OpPileShareWhiteListEntity opPileShareWhiteListEntity = OpPileShareWhiteListConvert.toPileShareWhiteEntity(opPileShareWhiteListDTO);
        opPileShareWhiteListEntity.setCreatedBy(UserUtil.getUserId());
        log.info("当前用户userId={}", UserUtil.getUserId());
        this.save(opPileShareWhiteListEntity);
        return opPileShareWhiteListEntity;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean deleteByPileSn(OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        log.info("OpPileShareWhiteListRepositoryImpl -> deleteByPileSn{}", JSON.toJSONString(opPileShareWhiteListDTO));
        LambdaQueryWrapper<OpPileShareWhiteListEntity> queryWrapper = Wrappers.lambdaQuery(OpPileShareWhiteListEntity.class)
                .eq(OpPileShareWhiteListEntity::getPileSn, opPileShareWhiteListDTO.getPileSn());
        return this.remove(queryWrapper);
    }
}
