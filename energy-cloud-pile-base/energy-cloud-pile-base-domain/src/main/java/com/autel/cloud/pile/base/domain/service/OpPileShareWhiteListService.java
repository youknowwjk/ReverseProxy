package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListDTO;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListPageDTO;
import com.autel.cloud.pile.base.vo.OpPileShareWhiteListPageVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

/**
 * @ClassName OpPileShareWhiteListService
 * @Author A22591
 * @Data 2022/8/31
 */
public interface OpPileShareWhiteListService {

    /**
     * 根据pileSn查询桩是否在白名单
     *
     * @param pileSn
     * @return
     */
    Result<Boolean> isExistByPileSn(String pileSn);

    /**
     * 分页查询白名单
     *
     * @param opPileShareWhiteListPageDTO
     * @return
     */
    Result<Page<OpPileShareWhiteListPageVO>> getWhiteListPage(OpPileShareWhiteListPageDTO opPileShareWhiteListPageDTO);

    /**
     * 新增白名单
     *
     * @param opPileShareWhiteListDTO
     * @return
     */
    Result<Long> create(OpPileShareWhiteListDTO opPileShareWhiteListDTO);

    /**
     * 根据pileSn删除白名单
     *
     * @param opPileShareWhiteListDTO
     * @return
     */
    Result<Boolean> deleteByPileSn(OpPileShareWhiteListDTO opPileShareWhiteListDTO);
}
