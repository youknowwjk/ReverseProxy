package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.pile.base.dto.pos.SetPosAuthorizedAmountDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPosAuthorizedAmountDistributeEntity;
import com.autel.cloud.pile.base.vo.pos.PileVO;
import com.autel.cloud.pile.base.vo.pos.PosAuthorizedAmountDistributeVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 业务逻辑层 接口
 */
public interface OpPosAuthorizedAmountDistributeService {

    /**
     * @param pageDTO
     * @return
     * @function 分页查询本地POS预授权金额修改工具设置记录
     */
    Page<PosAuthorizedAmountDistributeVO> queryPages(PageDTO pageDTO);

    /**
     * @param setPosAuthorizedAmountDTO
     * @return
     * @function 修改本地POS预授权金额
     */
    Boolean setPosAuthorizedAmount(SetPosAuthorizedAmountDTO setPosAuthorizedAmountDTO);

    /**
     * @param opPosAuthorizedAmountDistributeEntity
     * @return
     * @function 新增或者修改一条本地POS预授权金额修改工具设置记录
     */
    Boolean saveOrUpdatePosAuthorizedAmountDistribute(OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity);

    /**
     * @param pageDTO
     * @return
     * @function 分页查询充电桩序列号下拉列表
     */
    Page<PileVO> getPileDropDownPageList(PageDTO pageDTO);
}
