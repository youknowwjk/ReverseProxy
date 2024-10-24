package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpImageEntity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.MapKey;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Map;

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
public interface OpImageMapper extends BaseMapper<OpImageEntity> {

    List<OpImageEntity> getImagesByIds(@Param(value = "imageIds") List<Long> imageIds);

    /**
     *  根据locationId删除
     */
    void deleteByLocationId(Long id);

    @MapKey("id")
    List<Map> selectEvseImgs(@Param(value = "evseUidList") List<Long> evseUidList);
}
