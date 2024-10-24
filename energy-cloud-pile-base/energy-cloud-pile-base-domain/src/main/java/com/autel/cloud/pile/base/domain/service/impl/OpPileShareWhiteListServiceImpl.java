package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpPileShareWhiteListRepository;
import com.autel.cloud.pile.base.domain.service.OpPileShareWhiteListService;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListDTO;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListPageDTO;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.mapper.OpPileShareWhiteListMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileShareWhiteListEntity;
import com.autel.cloud.pile.base.vo.OpPileShareWhiteListPageVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class OpPileShareWhiteListServiceImpl implements OpPileShareWhiteListService {

    private final OpPileShareWhiteListRepository opPileShareWhiteListRepository;

    private final OpPileShareWhiteListMapper opPileShareWhiteListMapper;

    public OpPileShareWhiteListServiceImpl(OpPileShareWhiteListRepository opPileShareWhiteListRepository, OpPileShareWhiteListMapper opPileShareWhiteListMapper) {
        this.opPileShareWhiteListRepository = opPileShareWhiteListRepository;
        this.opPileShareWhiteListMapper = opPileShareWhiteListMapper;
    }

    @Override
    public Result<Boolean> isExistByPileSn(String pileSn) {
        if (StringUtils.isBlank(pileSn)) {
            return Result.ofFailed(ResultCodeEnum.PARAM_ERROR);
        }
        return Result.ofSucceed(opPileShareWhiteListRepository.isExistByPileSn(pileSn));
    }

    @Override
    public Result<Page<OpPileShareWhiteListPageVO>> getWhiteListPage(OpPileShareWhiteListPageDTO opPileShareWhiteListPageDTO) {
        return Result.ofSucceed(opPileShareWhiteListRepository.getWhiteListPage(opPileShareWhiteListPageDTO));
    }

    @Override
    public Result<Long> create(OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        LambdaQueryWrapper<OpPileShareWhiteListEntity> queryWrapper = Wrappers.lambdaQuery(OpPileShareWhiteListEntity.class)
                .eq(OpPileShareWhiteListEntity::getPileSn, opPileShareWhiteListDTO.getPileSn());
        List<OpPileShareWhiteListEntity> listEntities = opPileShareWhiteListMapper.selectList(queryWrapper);
        if (CollectionUtils.isNotEmpty(listEntities)) {
            throw new MessageCodeException(PileBaseEnum.DATA_ALREADY_EXIST);
        }
        OpPileShareWhiteListEntity opPileShareWhiteListEntity;
        if (StringUtils.isBlank(opPileShareWhiteListDTO.getPileSn())) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        opPileShareWhiteListEntity = opPileShareWhiteListRepository.create(opPileShareWhiteListDTO);
        return Result.ofSucceed(opPileShareWhiteListEntity.getId());
    }

    @Override
    public Result<Boolean> deleteByPileSn(OpPileShareWhiteListDTO opPileShareWhiteListDTO) {
        if (StringUtils.isBlank(opPileShareWhiteListDTO.getPileSn())) {
            return Result.ofFailed(ResultCodeEnum.NOT_FOUND);
        }
        Boolean bo = opPileShareWhiteListRepository.deleteByPileSn(opPileShareWhiteListDTO);
        return Result.ofSucceed(bo);
    }
}
