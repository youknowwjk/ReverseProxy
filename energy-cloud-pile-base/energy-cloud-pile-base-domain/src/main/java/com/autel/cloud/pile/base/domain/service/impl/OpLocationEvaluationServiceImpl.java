package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.OpLocationEvaluationRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationEvaluationService;
import com.autel.cloud.pile.base.dto.OpLocationEvaluationDTO;
import com.autel.cloud.pile.base.vo.OpLocationEvaluationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class OpLocationEvaluationServiceImpl implements OpLocationEvaluationService {

    @Autowired
    private OpLocationEvaluationRepository opLocationEvaluationRepository;

    /**
     * 场站评价分页
     *
     * @param opLocationEvaluationDTO 场站id
     * @return 场站评价分页
     */
    @Override
    public Result<Page<OpLocationEvaluationVO>> page(OpLocationEvaluationDTO opLocationEvaluationDTO) {
        return Result.ofSucceed(opLocationEvaluationRepository.page(opLocationEvaluationDTO));
    }
}
