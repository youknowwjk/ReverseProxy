package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationConnectorRepository;
import com.autel.cloud.pile.base.domain.service.OpEvseTypeService;
import com.autel.cloud.pile.base.domain.service.OpLocationConnectorService;
import com.autel.cloud.pile.base.dto.OpLocationConnectorPagingDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseTypeMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationConnectorMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseTypeEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationConnectorEntity;
import com.autel.cloud.pile.base.vo.OpLocationConnectorPagingVo;
import com.autel.cloud.pile.base.vo.app.GunTypeRespDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @Author: A22327
 * @CreateTime: 2022/4/24 20:29
 * @Description: 连接器实现类
 */
@Service
@Slf4j
public class OpLocationConnectorServiceImpl implements OpLocationConnectorService {

    private final OpLocationConnectorRepository opLocationConnectorRepository;

    @Resource
    private OpLocationConnectorMapper opLocationConnectorMapper;

    @Resource
    private OpEvseTypeService opEvseTypeService;

    @Resource
    private OpEvseTypeMapper opEvseTypeMapper;
    public OpLocationConnectorServiceImpl(OpLocationConnectorRepository opLocationConnectorRepository) {
        this.opLocationConnectorRepository = opLocationConnectorRepository;
    }

    @Override
    public Result<Page<OpLocationConnectorPagingDTO>> paging(OpLocationConnectorPagingVo opLocationConnectorPagingVo) {
        // 去除mybatis-plus 最大分页默认500
        if (opLocationConnectorPagingVo.getPageSize() > 500) {
            opLocationConnectorPagingVo.setPageSize(-1);
        }
        //todo 处理阶段电价
        Page<OpLocationConnectorPagingDTO> pagingDTOPage = opLocationConnectorRepository.paging(opLocationConnectorPagingVo);
        if (pagingDTOPage == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage("connector doesn't exist!"));
        }
        return Result.ofSucceed(pagingDTOPage);
    }

    @Override
    public Result<OpLocationConnectorPagingDTO> getOpLocationConnectorDetail(Long connectorId) {
        return Result.ofSucceed(opLocationConnectorRepository.getOpLocationConnectorDetail(connectorId));
    }

    @Override

    public List<Double> getPowerByGunType(int gunType) {
        LambdaQueryWrapper<OpLocationConnectorEntity> queryWrapper = new LambdaQueryWrapper<>();
        if (gunType == 3){

            queryWrapper.eq(OpLocationConnectorEntity::getPowerType,"DC");
            List<OpLocationConnectorEntity> opLocationConnectorEntities = opLocationConnectorMapper.selectList(queryWrapper);
            return   opLocationConnectorEntities.stream().map(opLocationConnectorEntity -> opLocationConnectorEntity.getPower()).distinct().collect(Collectors.toList());
        }else {
            queryWrapper.in(OpLocationConnectorEntity::getPowerType,"AC_3_PHASE","AC");
            List<OpLocationConnectorEntity> opLocationConnectorEntities = opLocationConnectorMapper.selectList(queryWrapper);
            return   opLocationConnectorEntities.stream().map(opLocationConnectorEntity -> opLocationConnectorEntity.getPower()).distinct().collect(Collectors.toList());

        }

    }

    @Override
    public List<GunTypeRespDTO> getGunTypeByType(String type) {
        LambdaQueryWrapper<OpEvseTypeEntity> queryWrapper = new LambdaQueryWrapper<>();

        if ("DC".equalsIgnoreCase(type)) {
            queryWrapper.like(OpEvseTypeEntity::getADType, "D");
        }else {
            queryWrapper.like(OpEvseTypeEntity::getADType, "A");
        }
        List<OpEvseTypeEntity> opEvseTypeEntities = opEvseTypeMapper.selectList(queryWrapper);

            //获取每个枪的枪类型
            List<Integer> gunTypes = opEvseTypeEntities.stream().map(OpEvseTypeEntity::getGunType).distinct().collect(Collectors.toList());
            Result<List<GunTypeRespDTO>> result = opEvseTypeService.getGunType();
            List<GunTypeRespDTO> gunTypeRespDTOS = new ArrayList<>();
            for (GunTypeRespDTO gunTypeRespDTO:result.getData()) {
                gunTypeRespDTO.setName(gunTypeRespDTO.getName().trim());
                if (gunTypes.contains(gunTypeRespDTO.getGunType())){
                    gunTypeRespDTOS.add(gunTypeRespDTO);
                }
            }
            return gunTypeRespDTOS;
      }

   @Override
    public Result<OpLocationConnectorPagingDTO> getOpLocationConnectorDetailDeleted(Long connectorId) {
        return Result.ofSucceed(opLocationConnectorRepository.getOpLocationConnectorDetailDeleted(connectorId));
    }

    @Override
    public Result<Boolean> updateEvsePowerBySn(String evseSn,Double power){
        return Result.ofSucceed(opLocationConnectorRepository.updateEvsePowerBySn(evseSn,power));
    }

    @Override
    public List<OpLocationConnectorEntity> findByEvseId(List<Long> ids) {
        return opLocationConnectorRepository.list(new LambdaQueryWrapper<OpLocationConnectorEntity>()
                .in(OpLocationConnectorEntity::getLocationEvseId, ids)
                .eq(OpLocationConnectorEntity::getDeleted, 0));
    }

    @Override
    public boolean updateBatch(List<OpLocationConnectorEntity> connectorEntityList) {
        return opLocationConnectorRepository.updateBatchById(connectorEntityList);
    }
}
