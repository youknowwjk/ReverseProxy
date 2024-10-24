package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpLocationEvaluationDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvaluationEntity;
import com.autel.cloud.pile.base.vo.OpLocationEvaluationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

public interface OpLocationEvaluationRepository extends IService<OpLocationEvaluationEntity> {

    /**
     * 场站评价分页
     *
     * @param opLocationEvaluationDTO 场站id
     * @return 场站评价分页
     */
    Page<OpLocationEvaluationVO> page(OpLocationEvaluationDTO opLocationEvaluationDTO);
}