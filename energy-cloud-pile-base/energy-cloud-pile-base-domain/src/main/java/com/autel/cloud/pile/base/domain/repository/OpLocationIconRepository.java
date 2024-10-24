package com.autel.cloud.pile.base.domain.repository;

import com.autel.cloud.pile.base.dto.OpLocationIconDTO;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationIconEntity;
import com.autel.cloud.pile.base.vo.OpLocationIconVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * <p>
 * 图标信息（icon） 服务类（数据库存储服务）
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
public interface OpLocationIconRepository extends IService<OpLocationIconEntity> {
    /**
     * 获得所有图标信息（icon）条件查询
     *
     * @return
     */
    List<OpLocationIconVO> getAllIcon(OpLocationIconDTO opLocationIconDTO);

    /**
     * 根据主键查询图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    OpLocationIconVO getIconById(OpLocationIconDTO opLocationIconDTO);

    /**
     * 单个添加图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    Integer addIcon(OpLocationIconDTO opLocationIconDTO);

    /**
     * 单个修改图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    Integer editIcon(OpLocationIconDTO opLocationIconDTO);

    /**
     * 根据编码和分类查询图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    Boolean getIconByCodeAndCategory(OpLocationIconDTO opLocationIconDTO);


    /**
     * 查询所有图标信息（icon）的主键
     *
     * @return
     */
    List<Long> selectAllIconId();

    /**
     * 根据图标信息的主键集合批量删除图标信息（icon）
     *
     * @param opLocationIconIdList
     * @return
     */
    Integer deleteIconByIdList(List<Long> opLocationIconIdList);
}
