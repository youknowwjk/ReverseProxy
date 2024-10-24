package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpPileShareWhiteListDTO;
import com.autel.cloud.pile.base.dto.OpPileShareWhiteListPageDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpPileShareWhiteListEntity;
import com.autel.cloud.pile.base.vo.OpPileShareWhiteListPageVO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 * 家桩共享白名单表
 * </p>
 *
 * @author A22591
 * @since 2022/8/31
 */
public interface OpPileShareWhiteListRepository extends IService<OpPileShareWhiteListEntity> {

    /**
     * 根据pileSn查询桩是否在白名单
     *
     * @param pileSn
     * @return
     */
    Boolean isExistByPileSn(String pileSn);

    /**
     * 分页查询白名单
     *
     * @param opPileShareWhiteListPageDTO
     * @return
     */
    Page<OpPileShareWhiteListPageVO> getWhiteListPage(OpPileShareWhiteListPageDTO opPileShareWhiteListPageDTO);

    /**
     * 新增白名单
     *
     * @param opPileShareWhiteListDTO
     * @return
     */
    OpPileShareWhiteListEntity create(OpPileShareWhiteListDTO opPileShareWhiteListDTO);


    /**
     * 根据pileSn删除白名单
     *
     * @param opPileShareWhiteListDTO
     * @return
     */
    Boolean deleteByPileSn(OpPileShareWhiteListDTO opPileShareWhiteListDTO);
}
