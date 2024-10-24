package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.base.common.page.PageDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPosAuthorizedAmountDistributeEntity;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * @Author A22599
 * @Date 2023/06/02
 * @Function 本地POS预授权金额修改工具设置 提供数据服务 接口
 */
public interface OpPosAuthorizedAmountDistributeRepositoty extends IService<OpPosAuthorizedAmountDistributeEntity> {

    /**
     * @param pageDTO
     * @return
     * @function 分页查询本地POS预授权金额修改工具设置记录
     */
    Page<OpPosAuthorizedAmountDistributeEntity> queryPages(PageDTO pageDTO);

    /**
     * @param opPosAuthorizedAmountDistributeEntity
     * @return
     * @function 新增或者修改一条本地POS预授权金额修改工具设置记录
     */
    boolean saveOrUpdatePosAuthorizedAmountDistribute(OpPosAuthorizedAmountDistributeEntity opPosAuthorizedAmountDistributeEntity);
}
