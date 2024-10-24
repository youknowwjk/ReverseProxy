package com.autel.cloud.pile.base.domain.repository.impl;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.pile.base.domain.repository.OpPosAuthorizedAmountDistributeRepositoty;
import com.autel.cloud.pile.base.infrastructure.mapper.OpPosAuthorizedAmountDistributeMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPosAuthorizedAmountDistributeEntity;
import com.autel.cloud.pile.base.util.StringUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 提供数据服务 实现类
 */
@Service
@Slf4j
public class OpPosAuthorizedAmountDistributeRepositotyImpl extends ServiceImpl<OpPosAuthorizedAmountDistributeMapper, OpPosAuthorizedAmountDistributeEntity> implements OpPosAuthorizedAmountDistributeRepositoty {

    @Autowired
    private OpPosAuthorizedAmountDistributeMapper opPosAuthorizedAmountDistributeMapper;

    /**
     * @param pageDTO
     * @return
     * @function 分页查询本地POS预授权金额修改工具设置记录
     */
    @Override
    public Page<OpPosAuthorizedAmountDistributeEntity> queryPages(PageDTO pageDTO) {

        log.info("===>>>OpPosAuthorizedAmountDistributeRepositotyImpl.queryPages pageDTO : {}",
                JSON.toJSONString(pageDTO));

        Page<OpPosAuthorizedAmountDistributeEntity> page = new Page<>(pageDTO.getPage(), pageDTO.getPageSize());
        LambdaQueryWrapper<OpPosAuthorizedAmountDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotBlank(pageDTO.getSearchValue())) {
            lambdaQueryWrapper.like(OpPosAuthorizedAmountDistributeEntity::getPileSn, StringUtil.escapeChar(pageDTO.getSearchValue()));
        }

        JwtInfo jwtInfo = LoginUserHolder.getLoginUser();
        if (jwtInfo != null
                && jwtInfo.getPayload() != null
                && jwtInfo.getPayload().getSellerId() != null) {
            lambdaQueryWrapper.eq(OpPosAuthorizedAmountDistributeEntity::getSellerId, jwtInfo.getPayload().getSellerId());
        }
        lambdaQueryWrapper.eq(OpPosAuthorizedAmountDistributeEntity::getDeleted, 0);
        lambdaQueryWrapper.orderByDesc(OpPosAuthorizedAmountDistributeEntity::getUpdateTime, OpPosAuthorizedAmountDistributeEntity::getId);
        return opPosAuthorizedAmountDistributeMapper.selectPage(page, lambdaQueryWrapper);
    }

    /**
     * @param opPosAuthorizedAmountDistributeEntity
     * @return
     * @function 新增或者修改一条本地POS预授权金额修改工具设置记录
     */
    @Override
    public boolean saveOrUpdatePosAuthorizedAmountDistribute(OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity) {

        log.info("===>>>OpPosAuthorizedAmountDistributeRepositotyImpl.saveOrUpdatePosAuthorizedAmountDistribute opPosAuthorizedAmountDistributeEntity : {}",
                JSON.toJSONString(opPosAuthorizedAmountDistributeEntity));

        OpPosAuthorizedAmountDistributeEntity existedOpPosAuthorizedAmountDistributeEntity = this.findOnePosAuthorizedAmountDistribute(opPosAuthorizedAmountDistributeEntity);
        if (existedOpPosAuthorizedAmountDistributeEntity == null) {
            return opPosAuthorizedAmountDistributeMapper.insert(opPosAuthorizedAmountDistributeEntity) > 0;
        } else {
            existedOpPosAuthorizedAmountDistributeEntity.setStatus(opPosAuthorizedAmountDistributeEntity.getStatus());
            if (opPosAuthorizedAmountDistributeEntity.getUpdateTime() != null) {
                existedOpPosAuthorizedAmountDistributeEntity.setUpdateTime(opPosAuthorizedAmountDistributeEntity.getUpdateTime());
            }
            return opPosAuthorizedAmountDistributeMapper.updateById(existedOpPosAuthorizedAmountDistributeEntity) > 0;
        }
    }

    /**
     * @param opPosAuthorizedAmountDistributeEntity
     * @return
     * @function 根据条件查询一条本地POS预授权金额修改工具设置记录
     */
    private OpPosAuthorizedAmountDistributeEntity findOnePosAuthorizedAmountDistribute(OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity) {

        log.info("===>>>OpPosAuthorizedAmountDistributeRepositotyImpl.findOnePosAuthorizedAmountDistribute opPosAuthorizedAmountDistributeEntity : {}",
                JSON.toJSONString(opPosAuthorizedAmountDistributeEntity));

        if (opPosAuthorizedAmountDistributeEntity == null
                || (opPosAuthorizedAmountDistributeEntity.getId() == null
                && (StringUtils.isBlank(opPosAuthorizedAmountDistributeEntity.getPileSn())))) {
            return null;
        }

        if (opPosAuthorizedAmountDistributeEntity.getId() != null) {
            return opPosAuthorizedAmountDistributeMapper.selectById(opPosAuthorizedAmountDistributeEntity.getId());
        }

        LambdaQueryWrapper<OpPosAuthorizedAmountDistributeEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper
                .eq(OpPosAuthorizedAmountDistributeEntity::getPileSn, opPosAuthorizedAmountDistributeEntity.getPileSn())
                .eq(OpPosAuthorizedAmountDistributeEntity::getDeleted, 0)
                .orderByDesc(OpPosAuthorizedAmountDistributeEntity::getUpdateTime, OpPosAuthorizedAmountDistributeEntity::getId)
                .last("limit 1");
        return opPosAuthorizedAmountDistributeMapper.selectOne(lambdaQueryWrapper);
    }
}
