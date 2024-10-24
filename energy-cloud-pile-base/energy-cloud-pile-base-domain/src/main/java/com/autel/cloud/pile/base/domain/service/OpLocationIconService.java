package com.autel.cloud.pile.base.domain.service;


import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpLocationIconDTO;
import com.autel.cloud.pile.base.vo.OpLocationIconVO;

import java.util.List;

/**
 * <p>
 * 图标信息（icon） 业务逻辑接口
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
public interface OpLocationIconService {
    /**
     * 获得所有图标信息（icon）条件查询
     *
     * @return
     */
    Result<List<OpLocationIconVO>> getAllIcon(OpLocationIconDTO opLocationIconDTO);

    /**
     * 单个添加图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    Result<Boolean> addIcon(OpLocationIconDTO opLocationIconDTO);

    /**
     * 单个修改图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    Result<Boolean> editIcon(OpLocationIconDTO opLocationIconDTO);

    /**
     * 初始化图标信息（icon）
     *
     * @return
     */
    Result<Boolean> initializeIconInformation();
}
