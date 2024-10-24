package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationImageEntity;
import com.autel.cloud.pile.base.vo.OpLocationDetailImageVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 场站图片 Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-04-14
 */
@Mapper
@Repository
public interface OpLocationImageMapper extends BaseMapper<OpLocationImageEntity> {
    /**
     * 根据场站id删除记录
     *
     * @param locationId
     */
    void deleteByLocationId(Long locationId);

    /**
     * 根据场站id更新记录
     *
     * @param opLocationImageEntity
     */
    void updateByLocationId(OpLocationImageEntity opLocationImageEntity);

    /**
     * 根据场站id查询
     *
     * @param locationId
     * @return
     */
    List<Long> selectByLocationId(Long locationId);

    /**
     * 查询场站图片
     *
     * @param locationIdList
     * @return
     */
    List<OpLocationDetailImageVO> selectImageByLocationIdList(@Param(value = "locationIdList") List<Long> locationIdList);
}
