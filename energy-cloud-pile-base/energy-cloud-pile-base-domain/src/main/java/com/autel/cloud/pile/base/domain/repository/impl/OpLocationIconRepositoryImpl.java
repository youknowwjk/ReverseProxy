package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.collection.CollUtil;
import com.autel.cloud.pile.base.domain.repository.OpLocationIconRepository;
import com.autel.cloud.pile.base.dto.OpLocationIconDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationIconMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationIconEntity;
import com.autel.cloud.pile.base.vo.OpLocationIconVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * <p>
 * 图标信息（icon） 服务实现类（数据库存储服务）
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
@Service
public class OpLocationIconRepositoryImpl extends ServiceImpl<OpLocationIconMapper, OpLocationIconEntity> implements OpLocationIconRepository {

    @Autowired
    private OpLocationIconMapper opLocationIconMapper;

    /**
     * 获得所有图标信息（icon）条件查询
     *
     * @return
     */
    @Override
    public List<OpLocationIconVO> getAllIcon(OpLocationIconDTO opLocationIconDTO) {
        LambdaQueryWrapper<OpLocationIconEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(OpLocationIconEntity::getDeleted, 0)
                .eq(opLocationIconDTO.getId() != null, OpLocationIconEntity::getId, opLocationIconDTO.getId())
                .eq(opLocationIconDTO.getCategory() != null, OpLocationIconEntity::getCategory, opLocationIconDTO.getCategory())
                .eq(opLocationIconDTO.getCode() != null, OpLocationIconEntity::getIconCode, opLocationIconDTO.getCode())
                .like(opLocationIconDTO.getName() != null, OpLocationIconEntity::getIconName, opLocationIconDTO.getName())
                .like(opLocationIconDTO.getDesc() != null, OpLocationIconEntity::getIconDescribe, opLocationIconDTO.getDesc())
                .eq(opLocationIconDTO.getIconUrl() != null, OpLocationIconEntity::getIconUrl, opLocationIconDTO.getIconUrl())
                .eq(OpLocationIconEntity::getLanguage, opLocationIconDTO.getLanguage());
        List<OpLocationIconEntity> opLocationIconEntities = opLocationIconMapper.selectList(lambdaQueryWrapper);
        List<OpLocationIconVO> opLocationIconVOS = new ArrayList<>();
        if (opLocationIconEntities != null) {
            for (OpLocationIconEntity opLocationIconEntity : opLocationIconEntities) {
                OpLocationIconVO opLocationIconVO = new OpLocationIconVO();
                BeanUtils.copyProperties(opLocationIconEntity, opLocationIconVO);
                opLocationIconVO.setCode(opLocationIconEntity.getIconCode());
                opLocationIconVO.setName(opLocationIconEntity.getIconName());
                opLocationIconVO.setDesc(opLocationIconEntity.getIconDescribe());
                opLocationIconVOS.add(opLocationIconVO);
            }
        }
        Collections.sort(opLocationIconVOS);
        return opLocationIconVOS;
    }

    /**
     * 根据主键查询图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    @Override
    public OpLocationIconVO getIconById(OpLocationIconDTO opLocationIconDTO) {
        LambdaQueryWrapper<OpLocationIconEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        OpLocationIconEntity opLocationIconEntity = new OpLocationIconEntity();
        if (opLocationIconDTO.getId() != null) {
            lambdaQueryWrapper.eq(OpLocationIconEntity::getDeleted, 0);
            lambdaQueryWrapper.eq(OpLocationIconEntity::getId, opLocationIconDTO.getId());
            lambdaQueryWrapper.eq(OpLocationIconEntity::getLanguage, opLocationIconDTO.getLanguage());
            opLocationIconEntity = opLocationIconMapper.selectOne(lambdaQueryWrapper);
        }
        OpLocationIconVO opLocationIconVO = new OpLocationIconVO();
        BeanUtils.copyProperties(opLocationIconEntity, opLocationIconVO);
        opLocationIconVO.setCode(opLocationIconEntity.getIconCode());
        opLocationIconVO.setName(opLocationIconEntity.getIconName());
        opLocationIconVO.setDesc(opLocationIconEntity.getIconDescribe());
        return opLocationIconVO;
    }

    /**
     * 单个添加图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    @Override
    public Integer addIcon(OpLocationIconDTO opLocationIconDTO) {
        OpLocationIconEntity opLocationIconEntity = new OpLocationIconEntity();
        BeanUtils.copyProperties(opLocationIconDTO, opLocationIconEntity);
        opLocationIconEntity.setIconCode(opLocationIconDTO.getCode());
        opLocationIconEntity.setIconName(opLocationIconDTO.getName());
        opLocationIconEntity.setIconDescribe(opLocationIconDTO.getDesc());
        return opLocationIconMapper.insert(opLocationIconEntity);
    }

    /**
     * 单个修改图标信息（icon）根据主键（id）修改
     *
     * @param opLocationIconDTO
     * @return
     */
    @Override
    public Integer editIcon(OpLocationIconDTO opLocationIconDTO) {
        OpLocationIconEntity opLocationIconEntity = new OpLocationIconEntity();
        BeanUtils.copyProperties(opLocationIconDTO, opLocationIconEntity);
        opLocationIconEntity.setIconCode(opLocationIconDTO.getCode());
        opLocationIconEntity.setIconName(opLocationIconDTO.getName());
        opLocationIconEntity.setIconDescribe(opLocationIconDTO.getDesc());
        return opLocationIconMapper.updateById(opLocationIconEntity);
    }

    /**
     * 根据编码和分类查询图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    @Override
    public Boolean getIconByCodeAndCategory(OpLocationIconDTO opLocationIconDTO) {
        LambdaQueryWrapper<OpLocationIconEntity> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(OpLocationIconEntity::getIconCode, opLocationIconDTO.getCode())
                .eq(OpLocationIconEntity::getCategory, opLocationIconDTO.getCategory())
                .eq(OpLocationIconEntity::getDeleted, 0)
                .eq(OpLocationIconEntity::getLanguage, opLocationIconDTO.getLanguage())
                .ne(null != opLocationIconDTO.getId(), OpLocationIconEntity::getId, opLocationIconDTO.getId());
        List<OpLocationIconEntity> opLocationIconEntityList = opLocationIconMapper.selectList(lambdaQueryWrapper);
        return CollUtil.isNotEmpty(opLocationIconEntityList);
    }

    /**
     * 查询所有图标信息（icon）的主键
     *
     * @return
     */
    @Override
    public List<Long> selectAllIconId() {
        List<OpLocationIconEntity> opLocationIconEntities = opLocationIconMapper.selectList(null);
        List<Long> opLocationIconIdList = new ArrayList<>();
        if (CollUtil.isNotEmpty(opLocationIconEntities)) {
            for (OpLocationIconEntity opLocationIconEntity : opLocationIconEntities) {
                opLocationIconIdList.add(opLocationIconEntity.getId());
            }
        }
        return opLocationIconIdList;
    }

    /**
     * 根据图标信息的主键集合对图标信息进行批量删除（icon）
     *
     * @param opLocationIconIdList
     * @return
     */
    @Override
    public Integer deleteIconByIdList(List<Long> opLocationIconIdList) {
        return opLocationIconMapper.deleteBatchIds(opLocationIconIdList);
    }
}
