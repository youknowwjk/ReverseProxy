package com.autel.cloud.pile.base.infrastructure.mapper;

import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationEvseEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpLocationPileEvseEntity;
import com.autel.cloud.pile.base.vo.PileDetailVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * <p>
 * 充电设备组合（桩） Mapper 接口
 * </p>
 *
 * @author A22121
 * @since 2022-05-10
 */
@Mapper
@Repository
public interface OpLocationPileEvseMapper extends BaseMapper<OpLocationPileEvseEntity> {

    int batchSave(@Param("insertList") List<OpLocationPileEvseEntity> insertList);

    List<OpLocationPileEvseEntity> findByLocationIdAndPileName(@Param("locationId") Long locationId, @Param("name") String name);

    List<OpLocationEvseEntity> queryPileListByOperationId(@Param("operatorId") Long operatorId,@Param("list") List<Long> locationIds);

    List<PileDetailVO> getLocationPileList(@Param("locationIds") List<Long> locationIds,@Param("keyword") String keyword);

    /**
     * 根据id逻辑删除充电设备
     *
     * @param pileSN
     */
    void deleteByPileSN(String pileSN);

    OpLocationPileEvseEntity findOne(@Param("pileSn") String pileSn);
}
