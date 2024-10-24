package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpLocationConnectorRepository;
import com.autel.cloud.pile.base.dto.OpLocationConnectorPagingDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationConnectorMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.vo.OpLocationConnectorPagingVo;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * <p>
 * 充电设备连接器 服务实现类
 * </p>
 *
 * @author A22121
 * @since 2022-04-15
 */
@Service
@Slf4j
public class OpLocationConnectorRepositoryImpl extends ServiceImpl<OpLocationConnectorMapper, OpLocationConnectorEntity> implements OpLocationConnectorRepository {

    @Autowired
    private OpLocationConnectorMapper opLocationConnectorMapper;

    @Override
    public Page<OpLocationConnectorPagingDTO> paging(OpLocationConnectorPagingVo opLocationConnectorPagingVo) {
        Page<OpLocationConnectorPagingDTO> page = new Page<>(opLocationConnectorPagingVo.getStart(), opLocationConnectorPagingVo.getPageSize());
        return opLocationConnectorMapper.paging(page, opLocationConnectorPagingVo);
    }

    @Override
    public OpLocationConnectorPagingDTO getOpLocationConnectorDetail(Long connectorId) {
        LambdaQueryWrapper<OpLocationConnectorEntity>  connectorWrapper = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                .eq(OpLocationConnectorEntity::getLocationEvseId,connectorId).eq(OpLocationConnectorEntity::getDeleted,Boolean.FALSE);
        OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorMapper.selectOne(connectorWrapper);
        OpLocationConnectorPagingDTO opLocationConnectorPagingDTO = new OpLocationConnectorPagingDTO();
        if(null ==opLocationConnectorEntity){
            return  null;
        }
        opLocationConnectorPagingDTO.setGunType(opLocationConnectorEntity.getGunType());
        opLocationConnectorPagingDTO.setConnectorId(String.valueOf(opLocationConnectorEntity.getId()));
        opLocationConnectorPagingDTO.setLocationEvseId(connectorId);
        opLocationConnectorPagingDTO.setVoltage(opLocationConnectorEntity.getVoltage());
        opLocationConnectorPagingDTO.setPowerType(opLocationConnectorEntity.getPowerType());
        opLocationConnectorPagingDTO.setAmperage(opLocationConnectorEntity.getAmperage());
        opLocationConnectorPagingDTO.setPower(opLocationConnectorEntity.getPower());
        return opLocationConnectorPagingDTO;
    }

    @Override
    public OpLocationConnectorPagingDTO getOpLocationConnectorDetailDeleted(Long connectorId) {
        LambdaQueryWrapper<OpLocationConnectorEntity>  connectorWrapper = Wrappers.lambdaQuery(OpLocationConnectorEntity.class)
                .eq(OpLocationConnectorEntity::getLocationEvseId,connectorId);
        OpLocationConnectorEntity opLocationConnectorEntity = opLocationConnectorMapper.selectOne(connectorWrapper);
        OpLocationConnectorPagingDTO opLocationConnectorPagingDTO = new OpLocationConnectorPagingDTO();
        if(null ==opLocationConnectorEntity){
            return  null;
        }
        opLocationConnectorPagingDTO.setGunType(opLocationConnectorEntity.getGunType());
        opLocationConnectorPagingDTO.setConnectorId(String.valueOf(opLocationConnectorEntity.getId()));
        opLocationConnectorPagingDTO.setLocationEvseId(connectorId);
        opLocationConnectorPagingDTO.setVoltage(opLocationConnectorEntity.getVoltage());
        opLocationConnectorPagingDTO.setPowerType(opLocationConnectorEntity.getPowerType());
        opLocationConnectorPagingDTO.setAmperage(opLocationConnectorEntity.getAmperage());
        opLocationConnectorPagingDTO.setPower(opLocationConnectorEntity.getPower());
        return opLocationConnectorPagingDTO;
    }

    @Override
    public Boolean updateEvsePowerBySn(String evseSn,Double power){
        List<OpLocationConnectorEntity> connectorListBysn = opLocationConnectorMapper.getConnectorListBysn(evseSn);
        log.info("通过sn查询出的连接器信息为：{}",connectorListBysn);

        for (OpLocationConnectorEntity opLocationConnectorEntity:connectorListBysn){
            opLocationConnectorEntity.setPower(power);
            int i = opLocationConnectorMapper.updateById(opLocationConnectorEntity);
        }
        return true;
    }
}
