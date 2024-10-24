package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpLocationEvaluationDTO;
import com.autel.cloud.pile.base.vo.OpLocationEvaluationVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

public interface OpLocationEvaluationService {
    /**
     * 场站评价分页
     *
     * @param opLocationEvaluationDTO 场站id
     * @return 场站评价分页
     */
    Result<Page<OpLocationEvaluationVO>> page(OpLocationEvaluationDTO opLocationEvaluationDTO);
}
